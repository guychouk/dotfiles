## Setup

_has() {
  return $( whence $1 &>/dev/null )
}

export ZSH_CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/zsh"
[[ ! -d "$ZSH_CACHE_DIR" ]] && mkdir "$ZSH_CACHE_DIR"

## Colors

autoload -U colors
colors

## Prompt

parse_git_branch() {
	case "$PWD" in
		/net/*|/Volumes/*) return ;;
	esac
	git_branch=$(git symbolic-ref --short HEAD 2> /dev/null)
	if [ ! "$git_branch" ]; then printf ""; else printf " [$git_branch]"; fi
}

parse_kubectl_current_context() {
	kube_context=$(kubectl config current-context 2> /dev/null)
	if [ ! "$kube_context" ]; then printf ""; else printf " [$kube_context]"; fi
}

setopt PROMPT_SUBST
PROMPT=$'%{\e[3 q%}%F{38}%1~%F{208}$(parse_git_branch) λ %f'

## Variables

HISTSIZE=50000
SAVEHIST=50000
HISTFILE="$ZSH_CACHE_DIR/history"

export GPG_TTY=$(tty)
export KEYTIMEOUT=1
export SNIPPET_DIR="$HOME"
export KUBECONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/kube/config"
export GOPATH="${XDG_DATA_HOME:-$HOME/.local/share}/go"
export CARGO_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/cargo"
export PARALLEL_HOME="${XDG_CACHE_HOME:-$HOME/.cache}/parallel"
export GEM_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/gem"
export GEM_PATH="$GEM_HOME"
export DOCKER_CONFIG="${XDG_DATA_HOME:-$HOME/.local/share}/docker"
export NPM_CONFIG_USERCONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/.npmrc"
export NPM_CONFIG_CACHE="${XDG_CACHE_HOME:-$HOME/.cache}/npm"
export AMMONITE_HOME="${XDG_CONFIG_HOME:-$HOME/.config}/ammonite"
export NODE_REPL_HISTORY="${XDG_CACHE_HOME:-$HOME/.cache}/.node_repl_history"
export TS_NODE_HISTORY="${XDG_CACHE_HOME:-$HOME/.cache}/.ts_node_repl_history"
export WGET_HSTS_FILE="${XDG_CONFIG_HOME:-$HOME/.cache}/.wget-hsts"
export PYTHONHISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/.python_history"
export LESSHISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/.lesshst"

## Aliases

alias \
	g=git \
	d=docker \
	k=kubectl \
	v=$EDITOR \
	dcc='docker compose' \
	godot=/Applications/Godot.app/Contents/MacOS/Godot \

# setup ll alias by which ls program is installed and if on macOS
if [[ $(uname) = "Darwin" ]]; then
	list_cmd='ls'
	if command -v gls >/dev/null 2>&1; then
		list_cmd='gls -lah --group-directories-first'
	fi
	if command -v eza >/dev/null 2>&1; then
		list_cmd='eza -la --icons --time-style=long-iso --group-directories-first'
	fi
	alias ll="$list_cmd --color=always"
	export HOMEBREW_CASK_OPTS="--appdir=/Applications"
	ulimit -n 10240
else
	alias ll='ls -lah --color=always'
fi

## General

bindkey -e                          # selects keymap `emacs` & set as main keymap
bindkey "^U" backward-kill-line     # kill backwards from cursor to the beginning of the line

# Let ^W delete to slashes (taken from statico's dotfiles)
# https://github.com/statico/dotfiles/blob/633664e2c59a7bbac0d42f8bbec0cb1843e18dea/.zshrc#L617
backward-delete-to-slash() {
  local WORDCHARS=${WORDCHARS//\//}
  zle .backward-delete-word
}
zle -N backward-delete-to-slash
bindkey "^W" backward-delete-to-slash

unsetopt auto_cd              # disable jump to path without using `cd`
setopt hist_reduce_blanks     # remove superfluous blanks from history items
setopt hist_ignore_all_dups   # remove older duplicate entries from history
setopt inc_append_history     # save commands to history as soon as they are entered
setopt share_history          # share history between different instances of the shell
setopt extended_history       # add timestamps to history
setopt interactive_comments   # enable entering comments as commands that do nothing
setopt hist_expire_dups_first # expire duplicate entries first when trimming history
setopt hist_ignore_space      # ignore entries that start with space (for sensitive commands)
setopt globdots               # autocomplete hidden files and folders (dotfiles)
setopt auto_param_slash       # automatically adds a trailing slash to directory names during completion
setopt complete_in_word       # allow completion within a word
setopt glob_complete          # completes based on glob patterns
setopt list_rows_first        # lists completion options row-wise instead of column-wise
setopt no_beep                # disable beeping for errors and completion

function update_path_for_node_modules() {
  local node_bin="$PWD/node_modules/.bin"
  # Remove any existing node_modules/.bin from PATH
  PATH=$(echo "$PATH" | awk -v RS=: -v ORS=: '/node_modules\/.bin/ {next} {print}' | sed 's/:$//')
  # If node_modules/.bin exists in the current directory, prepend it to PATH
  if [[ -d "$node_bin" ]]; then
    export PATH="$node_bin:$PATH"
  fi
}

# This function is called whenever the current directory changes
chpwd() {
    update_path_for_node_modules
}

# Initialize the function for the current directory
update_path_for_node_modules

path=("$GEM_HOME" $path "$GOPATH/bin" "$HOME/scripts")

# removes duplicate entries from $PATH
typeset -aU path

# edit command line with $EDITOR
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey -M emacs '^v' edit-command-line

## ASDF

export ASDF_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/asdf"
export ASDF_DATA_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/asdf"
export ASDF_CONFIG_FILE="${XDG_CONFIG_HOME:-$HOME/.config}/asdf/.asdfrc"

[ -d "${XDG_CONFIG_HOME:-$HOME/.config}/asdf" ] || mkdir -p "${XDG_CONFIG_HOME:-$HOME/.config}/asdf"
[ -d "$ASDF_DIR" ] || git clone https://github.com/asdf-vm/asdf.git $ASDF_DIR --branch v0.10.0

source "$ASDF_DIR/asdf.sh"
fpath=(${ASDF_DIR}/completions $fpath)

## Completion

zmodload -i zsh/complist
# The following lines were added by compinstall

zstyle ':completion:*' completer _complete _ignored
zstyle ':completion:*' list-dots yes
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list ''
zstyle ':completion:*' menu select=2
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle :compinstall filename "${XDG_CONFIG_HOME:-$HOME/.config}/zsh/.zshrc"

autoload -Uz compinit
compinit -d "$ZSH_CACHE_DIR/compdump"
# End of lines added by compinstall

# select option from completion menu using h,j,k,l
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

# import useful functions
source "$ZDOTDIR/functions/time"
source "$ZDOTDIR/functions/git"

## Syntax highlighting

function () {
  local fsh_cache_dir="$ZSH_CACHE_DIR/zsh-syntax-highlighting"
  local fsh_plugin="$fsh_cache_dir/zsh-syntax-highlighting.zsh"
  [ -d "$fsh_cache_dir" ] || git clone https://github.com/zsh-users/zsh-syntax-highlighting.git "$fsh_cache_dir"
  [ -f "$fsh_plugin" ] && source "$fsh_plugin"
}

## FZF

if _has fzf; then
  export FZF_DEFAULT_OPTS='--height 50%'
  if _has rg; then
    export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --glob "!.git/*"'
  fi
  [[ $- == *i* ]] && source "/usr/local/opt/fzf/shell/completion.zsh" 2> /dev/null
  [[ $- == *i* ]] && source "/usr/local/opt/fzf/shell/key-bindings.zsh"

  # useful fzf + git functions
  source "$ZDOTDIR/functions/fzf-git"
fi

## Direnv

if _has direnv; then
	export DIRENV_LOG_FORMAT=
	# eval "$(direnv hook zsh)"
fi

## Zoxide

if _has zoxide; then
  eval "$(zoxide init --cmd j zsh)"
fi

## Environment

if [ -f ~/.env ]; then
    source ~/.env
fi
