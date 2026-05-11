#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#define NOB_STRIP_PREFIX
#define NOB_IMPLEMENTATION
#include "nob.h"

#define HOME               "/Users/guychouk"
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
    {DOTSDIR "/skhd",                 XDG_CONFIG "/skhd"},
    {DOTSDIR "/newsboat",             XDG_CONFIG "/newsboat"},
    {DOTSDIR "/vim",                  HOME "/.vim"},
    {DOTSDIR "/ctags",                HOME "/.ctags.d"},
    {DOTSDIR "/zsh/.zshrc",           HOME "/.zshrc"},
    {DOTSDIR "/zsh/.zshenv",          HOME "/.zshenv"},
    {DOTSDIR "/gnupg/gpg-agent.conf", HOME "/.gnupg/gpg-agent.conf"},
    {DOTSDIR "/curl/curlrc",          HOME "/.curlrc"},
    {DOTSDIR "/emacs",                HOME "/.emacs.d"},
};

const Link launchd_links[] = {
    {DOTSDIR "/launchd/org.gnupg.gpg-agent.plist",    LAUNCH_AGENTS_DIR "/org.gnupg.gpg-agent.plist"},
    {DOTSDIR "/launchd/local.gpg-kill-on-lock.plist", LAUNCH_AGENTS_DIR "/local.gpg-kill-on-lock.plist"},
    {DOTSDIR "/launchd/com.koekeishiya.skhd.plist",   LAUNCH_AGENTS_DIR "/com.koekeishiya.skhd.plist"},
    {DOTSDIR "/launchd/local.clippy-daemon.plist",    LAUNCH_AGENTS_DIR "/local.clippy-daemon.plist"},
};

const Link swift_builds[] = {
    {DOTSDIR "/src/gpg-kill-on-lock.swift", HOME "/bin/gpg-kill-on-lock"},
    {DOTSDIR "/src/pbpass.swift",           HOME "/bin/pbpass"},
    {DOTSDIR "/src/clippy.swift",           HOME "/bin/clippy"},
};

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
        for (size_t i = 0; i < ARRAY_LEN(links); i++) {
            if (symlink(links[i].src, links[i].dst) < 0) {
                if (errno != EEXIST) perror(links[i].dst);
            }
            printf("%s -> %s\n", links[i].src, links[i].dst);
        }
    } else if (strcmp(command, "swiftc") == 0) {
        Cmd cmd = {0};
        for (size_t i = 0; i < ARRAY_LEN(swift_builds); i++) {
            cmd_append(&cmd, "swiftc", swift_builds[i].src, "-o", swift_builds[i].dst);
            cmd_run_sync_and_reset(&cmd);
        }
    } else if (strcmp(command, "launchd") == 0) {
        Cmd cmd = {0};
        for (size_t i = 0; i < ARRAY_LEN(launchd_links); i++) {
            if (symlink(launchd_links[i].src, launchd_links[i].dst) < 0) {
                if (errno != EEXIST) perror(links[i].dst);
            }
            printf("%s -> %s\n", launchd_links[i].src, launchd_links[i].dst);
            cmd_append(&cmd, "launchctl", "load", launchd_links[i].dst);
            cmd_run_sync_and_reset(&cmd);
        }
    } else {
        usage();
        return 1;
    }
    return 0;
}
