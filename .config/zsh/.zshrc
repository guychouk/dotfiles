## Setup

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

# Remove annoying percent sign
unsetopt PROMPT_SP

PROMPT=$'%{\e[3 q%}%F{38}%1~%F{208}$(parse_git_branch) λ %f'
setopt PROMPT_SUBST

## Variables

HISTSIZE=50000
SAVEHIST=50000
HISTFILE="$ZSH_CACHE_DIR/history"

export GPG_TTY=$(tty)
export KEYTIMEOUT=1
export KUBECONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/kube/config"
export GOPATH="${XDG_DATA_HOME:-$HOME/.local/share}/go"
export CARGO_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/cargo"
export GNUPGHOME="${XDG_CACHE_HOME:-$HOME/.cache}/gnupg"
export NODE_REPL_HISTORY="${XDG_CACHE_HOME:-$HOME/.cache}/.node_repl_history"

## Aliases

alias \
	g=git \
	d=docker \
	k=kubectl \
	nv=nvim \
	dcc='docker compose' \
	dfm='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME' \
	godot=/Applications/Godot.app/Contents/MacOS/Godot \
	tmux='tmux -f "${XDG_CONFIG_HOME:-$HOME/.config}/tmux/tmux.config"'

## General

bindkey -e                          # selects keymap `emacs` & set as main keymap
bindkey "^U" backward-kill-line     # kill backwards from cursor to the beginning of the line

setopt AUTO_CD                      # jump to path without using `cd`
setopt HIST_REDUCE_BLANKS           # remove superfluous blanks from history items
setopt HIST_IGNORE_ALL_DUPS         # remove older duplicate entries from history
setopt INC_APPEND_HISTORY           # save commands to history as soon as they are entered
setopt SHARE_HISTORY                # share history between different instances of the shell
setopt EXTENDED_HISTORY             # add timestamps to history
setopt INTERACTIVE_COMMENTS         # enable entering comments as commands that do nothing
setopt HIST_EXPIRE_DUPS_FIRST       # expire duplicate entries first when trimming history
setopt HIST_IGNORE_SPACE            # ignore entries that start with space (for sensitive commands)

path=($path "$GOPATH/bin" "$HOME/bin")

# removes duplicate entries from $PATH
typeset -aU path

if [[ $(uname) = "Darwin" ]]; then
	list_cmd='ls'
	if command -v gls >/dev/null 2>&1; then
		list_cmd='gls -lah --group-directories-first'
	fi
	if command -v exa >/dev/null 2>&1; then
		list_cmd='exa -la --icons --time-style=long-iso --group-directories-first'
	fi
	alias ll="$list_cmd --color=always"
	export HOMEBREW_CASK_OPTS="--appdir=/Applications"
	ulimit -n 10240
else
	alias ll='ls -lah --color=always'
fi

# edit command line with $EDITOR
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey -M emacs '^v' edit-command-line

## Completion

zmodload -i zsh/complist
# The following lines were added by compinstall

zstyle ':completion:*' completer _complete _ignored
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

## Syntax highlighting

function () {
  local fsh_cache_dir="$ZSH_CACHE_DIR/zsh-syntax-highlighting"
  local fsh_plugin="$fsh_cache_dir/zsh-syntax-highlighting.zsh"

  [ -d "$fsh_cache_dir" ] || git clone https://github.com/zsh-users/zsh-syntax-highlighting.git "$fsh_cache_dir"
  [ -f "$fsh_plugin" ] && source "$fsh_plugin"
}

## FASD

if command -v fasd 1>/dev/null 2>&1; then
	fasd_cd() {
		if [ $# -le 1 ]; then
			fasd "$@"
		else
			local _fasd_ret="$(fasd -e 'printf %s' "$@")"
			[ -z "$_fasd_ret" ] && return
			[ -d "$_fasd_ret" ] && cd "$_fasd_ret" || printf %s\\n "$_fasd_ret"
		fi
	}

	fasd_fzf_edit() {
		local _files=`fasd -fs "$@" | awk '{print $2}' | fzf -m`
		[ ! -z "$_files" ] && $EDITOR $(echo $_files)
	}

	alias \
		f='fasd -f' \
		j='fasd_cd -d' \
		v='fasd -f -e nvim' \
		vf='fasd_fzf_edit'

	export _FASD_DATA="${XDG_CACHE_HOME:-$HOME/.cache}/.fasd"
	eval "$(fasd --init zsh-hook zsh-wcomp-install zsh-wcomp)"
fi

## FZF

if command -v fzf 1>/dev/null 2>&1; then
	export FZF_DEFAULT_OPTS='--height 50% --layout=reverse --border=none'

	[[ $- == *i* ]] && source "/usr/local/opt/fzf/shell/completion.zsh" 2> /dev/null
	source "/usr/local/opt/fzf/shell/key-bindings.zsh"

	source "$ZDOTDIR/functions/fzf-git"
	source "$ZDOTDIR/functions/fzf-tmux"
fi

## Direnv

if command -v direnv 1>/dev/null 2>&1; then
	export DIRENV_LOG_FORMAT=
	eval "$(direnv hook zsh)"
fi

## ASDF

export ASDF_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/asdf"
export ASDF_CONFIG_FILE="${XDG_CONFIG_HOME:-$HOME/.config}/asdf/.asdfrc"

[ -d "${XDG_CONFIG_HOME:-$HOME/.config}/asdf" ] || mkdir -p "${XDG_CONFIG_HOME:-$HOME/.config}/asdf"
[ -d "$ASDF_DIR" ] || git clone https://github.com/asdf-vm/asdf.git $ASDF_DIR --branch v0.10.0

source "$ASDF_DIR/asdf.sh"

## General Functions

source "$ZDOTDIR/functions/time"

## Git scripts

source "$ZDOTDIR/functions/git"
