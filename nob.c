#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#define NOB_STRIP_PREFIX
#define NOB_IMPLEMENTATION
#include "nob.h"

#ifdef __APPLE__
#define HOME               "/Users/guychouk"
#else
#define HOME               "/home/guychouk"
#endif
#define XDG_CONFIG         HOME"/.config"
#define DOTSDIR            HOME"/dotfiles"
#define LAUNCH_AGENTS_DIR  HOME"/Library/LaunchAgents"

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
    {DOTSDIR "/direnv",               XDG_CONFIG "/direnv"},
    {DOTSDIR "/newsboat",             XDG_CONFIG "/newsboat"},
    {DOTSDIR "/gh-dash",              XDG_CONFIG "/gh-dash"},
    {DOTSDIR "/vim",                  HOME "/.vim"},
    {DOTSDIR "/ctags",                HOME "/.ctags.d"},
    {DOTSDIR "/zsh/.zshrc",           HOME "/.zshrc"},
    {DOTSDIR "/zsh/.zshenv",          HOME "/.zshenv"},
    {DOTSDIR "/gnupg/gpg-agent.conf", HOME "/.gnupg/gpg-agent.conf"},
    {DOTSDIR "/bin/pinentry",         "/usr/local/bin/pinentry-wrapper"},
    {DOTSDIR "/curl/curlrc",          HOME "/.curlrc"},
    {DOTSDIR "/tmux/tmux.conf",       HOME "/.tmux.conf"},
    {DOTSDIR "/emacs",                HOME "/.emacs.d"},
};

typedef struct {
    const char *label;
    const char *args[10];   // NULL-terminated
    const char *env[6];     // NULL-terminated "KEY=VALUE" pairs
    const char *workdir;    // optional, NULL to skip
    const char *log;        // optional, NULL to skip (used for both stdout+stderr)
    bool keep_alive;
    bool run_at_load;
    bool scheduled;         // run on a daily StartCalendarInterval at hour:minute
    int hour;
    int minute;
} Service;

const Service services[] = {
    {
        .label = "org.gnupg.gpg-agent",
        .args = {"/opt/homebrew/bin/gpg-agent", "--supervised"},
        .log = "/tmp/gpg-agent.log",
        .keep_alive = true, .run_at_load = true,
    },
    {
        .label = "local.gpg-kill-on-lock",
        .args = {HOME "/bin/gpg-kill-on-lock"},
        .log = "/tmp/gpg-kill-on-lock.log",
        .keep_alive = true, .run_at_load = true,
    },
    {
        .label = "local.clippy",
        .args = {HOME "/bin/clippy", "--daemon"},
        .log = "/tmp/clippy-daemon.log",
        .keep_alive = true, .run_at_load = true,
    },
    {
        .label = "local.butler-hotkey",
        .args = {HOME "/bin/hotkey", "11", "768", HOME "/bin/butler"},
        .keep_alive = true, .run_at_load = true,
    },
    {
        .label = "org.gnu.emacs.daemon",
        .args = {"/opt/homebrew/bin/emacs", "--fg-daemon"},
        .env = {"LANG=en_US.UTF-8", "LC_ALL=en_US.UTF-8"},
        .log = "/tmp/emacs-daemon.log",
        .keep_alive = true, .run_at_load = true,
    },
    {
        .label = "local.backup-documents",
        .args = {HOME "/bin/backup-documents"},
        .log = "/tmp/backup-documents.log",
        .scheduled = true, .hour = 12, .minute = 0,
    },
};

const Link swift_builds[] = {
    {DOTSDIR "/src/gpg-kill-on-lock.swift", HOME "/bin/gpg-kill-on-lock"},
    {DOTSDIR "/src/pbpass.swift",           HOME "/bin/pbpass"},
    {DOTSDIR "/src/clippy.swift",           HOME "/bin/clippy"},
    {DOTSDIR "/src/picker.swift",           HOME "/bin/picker"},
    {DOTSDIR "/src/hotkey.swift",           HOME "/bin/hotkey"},
};

void gen_plist(String_Builder *sb, const Service *s) {
    sb->count = 0;
    sb_appendf(sb, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    sb_appendf(sb, "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n");
    sb_appendf(sb, "<plist version=\"1.0\">\n<dict>\n");
    sb_appendf(sb, "    <key>Label</key>\n    <string>%s</string>\n", s->label);
    sb_appendf(sb, "    <key>ProgramArguments</key>\n    <array>\n");
    for (size_t i = 0; i < ARRAY_LEN(s->args) && s->args[i]; i++) {
        sb_appendf(sb, "        <string>%s</string>\n", s->args[i]);
    }
    sb_appendf(sb, "    </array>\n");
    if (s->env[0]) {
        sb_appendf(sb, "    <key>EnvironmentVariables</key>\n    <dict>\n");
        for (size_t i = 0; i < ARRAY_LEN(s->env) && s->env[i]; i++) {
            const char *eq = strchr(s->env[i], '=');
            sb_appendf(sb, "        <key>%.*s</key>\n        <string>%s</string>\n",
                       (int)(eq - s->env[i]), s->env[i], eq + 1);
        }
        sb_appendf(sb, "    </dict>\n");
    }
    if (s->workdir)
        sb_appendf(sb, "    <key>WorkingDirectory</key>\n    <string>%s</string>\n", s->workdir);
    if (s->run_at_load)
        sb_appendf(sb, "    <key>RunAtLoad</key>\n    <true/>\n");
    if (s->keep_alive)
        sb_appendf(sb, "    <key>KeepAlive</key>\n    <true/>\n");
    if (s->scheduled) {
        sb_appendf(sb, "    <key>StartCalendarInterval</key>\n    <dict>\n");
        sb_appendf(sb, "        <key>Hour</key>\n        <integer>%d</integer>\n", s->hour);
        sb_appendf(sb, "        <key>Minute</key>\n        <integer>%d</integer>\n", s->minute);
        sb_appendf(sb, "    </dict>\n");
    }
    if (s->log) {
        sb_appendf(sb, "    <key>StandardOutPath</key>\n    <string>%s</string>\n", s->log);
        sb_appendf(sb, "    <key>StandardErrorPath</key>\n    <string>%s</string>\n", s->log);
    }
    sb_appendf(sb, "</dict>\n</plist>\n");
}

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
    printf("usage: ./nob <links|launchd|swiftc>\n");
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
        nob_mkdir_if_not_exists(HOME "/bin");
        File_Paths binaries = {0};
        if (!read_entire_dir(DOTSDIR "/bin", &binaries)) return 1;
        for (size_t i = 0; i < binaries.count; i++) {
            const char *name = binaries.items[i];
            if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0) continue;
            link_path(&cmd, temp_sprintf(DOTSDIR "/bin/%s", name),
                      temp_sprintf(HOME "/bin/%s", name));
        }
        nob_da_free(binaries);
        cmd_free(cmd);
    } else if (strcmp(command, "swiftc") == 0) {
#ifndef __APPLE__
        nob_log(ERROR, "swiftc is macOS-only (Swift GUI helpers)");
        return 1;
#endif
        Cmd cmd = {0};
        for (size_t i = 0; i < ARRAY_LEN(swift_builds); i++) {
            cmd_append(&cmd, "swiftc", swift_builds[i].src, "-o", swift_builds[i].dst);
            cmd_run_sync_and_reset(&cmd);
        }
    } else if (strcmp(command, "launchd") == 0) {
#ifndef __APPLE__
        nob_log(ERROR, "launchd is macOS-only; use systemd user units on Linux");
        return 1;
#endif
        String_Builder sb = {0};
        const char *domain = temp_sprintf("gui/%d", getuid());
        for (size_t i = 0; i < ARRAY_LEN(services); i++) {
            const Service *s = &services[i];
            const char *dst = temp_sprintf(LAUNCH_AGENTS_DIR "/%s.plist", s->label);
            gen_plist(&sb, s);
            if (!write_entire_file(dst, sb.items, sb.count)) return 1;
            printf("%s\n", dst);
            reload_service(domain, s->label, dst);
        }
        nob_sb_free(sb);
    } else {
        usage();
        return 1;
    }
    return 0;
}
