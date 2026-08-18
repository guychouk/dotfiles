# Intentionally does not hook direnv.
#
# mise now provides per-directory env and tools (see each mono worktree's
# mise.local.toml). Every .envrc left on this machine is nix/devbox-backed and
# therefore dead. This file shadows homebrew's vendor_conf.d/direnv.fish, which
# would otherwise install the hook regardless.
#
# To re-enable: replace the body with `direnv hook fish | source`.
