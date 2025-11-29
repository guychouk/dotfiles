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
export OLLAMA_API_BASE="http://127.0.0.1:11434"

# Homebrew (needed for fzf and other tools)
if [[ "$(uname)" == "Darwin" && -f /opt/homebrew/bin/brew ]]; then
  export HOMEBREW_CASK_OPTS="--appdir=/Applications"
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# asdf-vm
export ASDF_DIR="${XDG_DATA_HOME}/asdf"
export ASDF_DATA_DIR="${ASDF_DIR}"
export ASDF_CONFIG_FILE="${XDG_CONFIG_HOME}/asdf/.asdfrc"
export ASDF_GOLANG_MOD_VERSION_ENABLED=true
if [[ -f "$ASDF_DIR/asdf.sh" ]]; then
  source "$ASDF_DIR/asdf.sh"
fi

# PATH
export GEM_HOME="${XDG_DATA_HOME}/gem"
export PATH="$ASDF_DIR/shims:$ASDF_DIR/completions:$GEM_HOME:$HOME/bin:$HOME/scripts:$HOME/.local/bin:$PATH"
