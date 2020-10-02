export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export ZDOTDIR="${XDG_CONFIG_HOME:-$HOME/.config}/zsh"

export VISUAL=nvim
export LESSHISTFILE="-"
export EDITOR="$VISUAL"

export HISTSIZE=10000
export SAVEHIST=10000
export HISTFILE="${XDG_DATA_HOME:-$HOME/.local/share}/history"
