#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <limits.h>

#define NOB_STRIP_PREFIX
#define NOB_IMPLEMENTATION
#include "nob.h"

#ifdef __APPLE__
#define IS_APPLE 1
#define HOME     "/Users/guychouk"
#else
#define IS_APPLE 0
#define HOME     "/home/guychouk"
#endif
#define XDG_CONFIG         HOME "/.config"
#define DOTSDIR            HOME "/dotfiles"
#define LAUNCH_AGENTS_DIR  HOME "/Library/LaunchAgents"

typedef struct {
    char *src;
    char *dst;
} Link;

const Link links[] = {
    {DOTSDIR "/fish",                 XDG_CONFIG "/fish" },
    {DOTSDIR "/git",                  XDG_CONFIG "/git"},
    {DOTSDIR "/kitty",                XDG_CONFIG "/kitty"},
    {DOTSDIR "/yazi",                 XDG_CONFIG "/yazi"},
    {DOTSDIR "/zsh",                  XDG_CONFIG "/zsh"},
    {DOTSDIR "/mise",                 XDG_CONFIG "/mise"},
    {DOTSDIR "/newsboat",             XDG_CONFIG "/newsboat"},
    {DOTSDIR "/vim",                  HOME "/.vim"},
    {DOTSDIR "/ctags",                HOME "/.ctags.d"},
    {DOTSDIR "/zsh/.zshrc",           HOME "/.zshrc"},
    {DOTSDIR "/zsh/.zshenv",          HOME "/.zshenv"},
    {DOTSDIR "/gnupg/gpg-agent.conf", HOME "/.gnupg/gpg-agent.conf"},
    {DOTSDIR "/scripts/pinentry",     "/usr/local/bin/pinentry"},
    {DOTSDIR "/curl/curlrc",          HOME "/.curlrc"},
    {DOTSDIR "/emacs",                HOME "/.emacs.d"},
};

// (Re)load a launchd service, replacing any running instance. bootout is
// asynchronous, so a bootstrap fired right after it can lose the race with the
// teardown and fail with a generic I/O error; retry until the job drains. Logs
// and launchctl's own stderr are silenced during the dance since the transient
// failures are expected; only a final, persistent failure is reported.
void reload_service(const char *domain, const char *label, const char *plist) {
    Cmd cmd = {0};
    Nob_Log_Level prev = minimal_log_level;
    minimal_log_level = NO_LOGS;
    cmd_append(&cmd, "launchctl", "bootout", temp_sprintf("%s/%s", domain, label));
    cmd_run(&cmd, .stderr_path = "/dev/null");
    for (int i = 0; i < 15; i++) {
        cmd_append(&cmd, "launchctl", "bootstrap", domain, plist);
        if (cmd_run(&cmd, .stderr_path = "/dev/null")) {
            minimal_log_level = prev;
            cmd_free(cmd);
            return;
        }
        usleep(200 * 1000);
    }
    minimal_log_level = prev;
    cmd_free(cmd);
    nob_log(ERROR, "could not load %s; try: launchctl bootstrap %s %s", label, domain, plist);
}

// Symlink src -> dst. Paths under $HOME are created directly as the invoking
// user (the common case); anything outside it (eg. /usr/local/bin) needs root,
// so shell out to sudo for just that entry rather than running all of links as
// root and littering $HOME with root-owned symlinks.
void link_path(Cmd *cmd, const char *src, const char *dst) {
    if (strncmp(dst, HOME, strlen(HOME)) == 0) {
        if (symlink(src, dst) < 0 && errno != EEXIST) perror(dst);
    } else {
        const char *slash = strrchr(dst, '/');
        if (slash && slash != dst) {
            cmd_append(cmd, "sudo", "mkdir", "-p",
                       temp_sprintf("%.*s", (int)(slash - dst), dst));
            cmd_run_sync_and_reset(cmd);
        }
        cmd_append(cmd, "sudo", "ln", "-sf", src, dst);
        cmd_run_sync_and_reset(cmd);
    }
    printf("%s -> %s\n", src, dst);
}

void usage(void) {
    printf("usage: ./nob <links|launchd>\n");
}

int main (int argc, char **argv) {
    GO_REBUILD_URSELF(argc, argv);
    const char *program = shift(argv, argc);
    if (argc < 1) {
        usage();
        return 1;
    }
    const char *command = shift(argv, argc);
    if (strcmp(command, "links") == 0) {
        Cmd cmd = {0};
        for (size_t i = 0; i < ARRAY_LEN(links); i++) {
            link_path(&cmd, links[i].src, links[i].dst);
        }
        nob_mkdir_if_not_exists(HOME "/.local/bin");
        File_Paths binaries = {0};
        if (!read_entire_dir(DOTSDIR "/scripts", &binaries)) return 1;
        for (size_t i = 0; i < binaries.count; i++) {
            const char *name = binaries.items[i];
            if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0) continue;
            link_path(&cmd, temp_sprintf(DOTSDIR "/scripts/%s", name),
                      temp_sprintf(HOME "/.local/bin/%s", name));
        }
        nob_da_free(binaries);
        cmd_free(cmd);
        File_Paths existing = {0};
        if (read_entire_dir(HOME "/.local/bin", &existing)) {
            for (size_t i = 0; i < existing.count; i++) {
                const char *name = existing.items[i];
                if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0) continue;
                const char *path = temp_sprintf(HOME "/.local/bin/%s", name);
                char target[PATH_MAX];
                ssize_t n = readlink(path, target, sizeof(target) - 1);
                if (n < 0) continue;            // not a symlink
                target[n] = '\0';
                if (strncmp(target, DOTSDIR, strlen(DOTSDIR)) == 0 &&
                    access(target, F_OK) != 0) {
                    unlink(path);
                    printf("pruned %s -> %s\n", path, target);
                }
            }
            nob_da_free(existing);
        }
    } else if (strcmp(command, "launchd") == 0) {
        if (!IS_APPLE) {
            nob_log(ERROR, "launchd is macOS-only; use systemd user units on Linux");
            return 1;
        }
        Cmd cmd = {0};
        const char *domain = temp_sprintf("gui/%d", getuid());
        File_Paths plists = {0};
        if (!read_entire_dir(DOTSDIR "/launchd", &plists)) return 1;
        for (size_t i = 0; i < plists.count; i++) {
            const char *name = plists.items[i];
            size_t name_len = strlen(name);
            const char *suffix = ".plist";
            size_t suffix_len = strlen(suffix);
            if (name_len < suffix_len || strcmp(name + name_len - suffix_len, suffix) != 0) continue;
            const char *src = temp_sprintf(DOTSDIR "/launchd/%s", name);
            const char *dst = temp_sprintf(LAUNCH_AGENTS_DIR "/%s", name);
            link_path(&cmd, src, dst);
            const char *label = temp_sprintf("%.*s", (int)(name_len - suffix_len), name);
            reload_service(domain, label, dst);
        }
        nob_da_free(plists);
        cmd_free(cmd);
    } else {
        usage();
        return 1;
    }
    return 0;
}
