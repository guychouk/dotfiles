export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export SHELL_SESSIONS_DISABLE=1

export EDITOR=vim
export VISUAL=vim
export PAGER=less
export MANPAGER="vim +MANPAGER --not-a-term -"

export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"

export ZDOTDIR="${XDG_CONFIG_HOME}/zsh"
export ZSH_CACHE_DIR="${XDG_CACHE_HOME}/zsh"

# Homebrew (needed for fzf and other tools)
if [[ "$(uname)" == "Darwin" && -f /opt/homebrew/bin/brew ]]; then
  export HOMEBREW_CASK_OPTS="--appdir=/Applications"
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# PATH
export GEM_HOME="${XDG_DATA_HOME}/gem"
export PATH="$GEM_HOME:$HOME/.local/bin:$PATH"

# gpg-agent as the SSH agent: unconditional and in .zshenv (not .zshrc) so it
# also applies to non-interactive zsh, e.g. Claude Code's Bash tool. Mirrors
# what fish/conf.d/02_gpg.fish already does unconditionally for every fish
# shell; zsh only had this in .zshrc, Linux-gated, i.e. never on this Mac
# outside an interactive shell.
if command -v gpgconf > /dev/null 2>&1; then
  export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
fi
