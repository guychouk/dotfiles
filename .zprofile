export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"

export VISUAL=nvim
export LESSHISTFILE="-"
export EDITOR="$VISUAL"
export ZDOTDIR="${XDG_CONFIG_HOME:-$HOME/.config}/zsh"
export _FASD_DATA="${XDG_CACHE_HOME:-$HOME/.cache}/.fasd"
export HISTFILE="${XDG_DATA_HOME:-$HOME/.local/share}/history"
