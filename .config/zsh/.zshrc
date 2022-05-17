[[ ! -d "${XDG_CACHE_HOME:-$HOME/.cache}/zsh" ]] && mkdir "${XDG_CACHE_HOME:-$HOME/.cache}/zsh" 

# Colors
# ------------------------

autoload -U colors
colors

# Prompt
# ------------------------

parse_git_branch() {
	git_branch=$(git symbolic-ref --short HEAD 2> /dev/null)
	if [ ! $git_branch ]; then printf ""; else printf " [${git_branch}]"; fi
}
parse_kubectl_current_context() {
	kube_context=$(kubectl config current-context 2> /dev/null)
	if [ ! $kube_context ]; then printf ""; else printf " [${kube_context}]"; fi
}
unsetopt PROMPT_SP
setopt PROMPT_SUBST
PROMPT='%F{38}%1~%F{208}$(parse_git_branch)$(parse_kubectl_current_context) λ %f'

# Completion
# ------------------------

autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit -d "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/compdump"
_comp_options+=(globdots)

# ENV variables
# ------------------------

HISTSIZE=10000
SAVEHIST=10000
HISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/history"

export KUBECONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/kube/config"

export GOPATH="${XDG_DATA_HOME:-$HOME/.local/share}/go"
export CARGO_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/cargo"

export GNUPGHOME="${XDG_CACHE_HOME:-$HOME/.cache}/gnupg"
export NODE_REPL_HISTORY="${XDG_CACHE_HOME:-$HOME/.cache}/.node_repl_history"

bindkey -e
export KEYTIMEOUT=1

bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

setopt auto_cd                      # jump to path without using `cd`
setopt hist_reduce_blanks           # remove superfluous blanks from history items
setopt hist_ignore_all_dups         # remove older duplicate entries from history
setopt append_history               # append history instead of overwriting
setopt inc_append_history           # save commands to history as soon as they are entered
setopt share_history                # share history between different instances of the shell
setopt extended_history             # add timestamps to history
setopt interactivecomments          # enable entering comments as commands that do nothing
setopt hist_expire_dups_first       # expire duplicate entries first when trimming history

alias \
	g=git \
	n=nnn \
	k=kubectl \
	nv=nvim \
	dfm='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME' \
	tmux='tmux -f "${XDG_CONFIG_HOME:-$HOME/.config}/tmux/tmux.config"'

# append to path
path=($path "$HOME/bin" "$GOPATH/bin")

# removes duplicate entries
typeset -aU path

if [[ $(uname) = "Darwin" ]]; then
	alias ll='ls -laG'

	export TMUX_SESSION='Work'
	export LDFLAGS=-L/usr/local/opt/openssl/lib
	export CPPFLAGS=-I/usr/local/opt/openssl/include
	export HOMEBREW_CASK_OPTS="--appdir=/Applications"

	## set max open file descriptors
	ulimit -n 10240

	## asdf
	export ASDF_CONFIG_FILE="${XDG_CONFIG_HOME:-$HOME/.config}/asdf/.asdfrc"
	[ -d "${XDG_CONFIG_HOME:-$HOME/.config}/asdf" ] || mkdir "${XDG_CONFIG_HOME:-$HOME/.config}/asdf"
	source $(brew --prefix asdf)/libexec/asdf.sh
else
	# Placeholder
fi

# fasd
# ------------------------

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

	alias \
		f='fasd -f' \
		s='cmd-split' \
		j='fasd_cd -d' \
		v='fasd -f -e nvim'

	export _FASD_DATA="${XDG_CACHE_HOME:-$HOME/.cache}/.fasd"
	eval "$(fasd --init zsh-hook zsh-wcomp-install zsh-wcomp)"
fi

# FZF
# ------------------------

export FZF_DEFAULT_OPTS='--height 50% --layout=reverse'

if command -v fzf 1>/dev/null; then
	fzf_config="${XDG_CONFIG_HOME:-$HOME/.config}/fzf/fzf.zsh"
	fzf_git_functions="${XDG_CONFIG_HOME:-$HOME/.config}/fzf/fzf-git.zsh"
	[ -f "$fzf_config" ] && source "$fzf_config"
	[ -f "$fzf_git_functions" ] && source "$fzf_git_functions"
fi

# Syntax highlighting
# ------------------------

fsh_cache_dir="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/zsh-syntax-highlighting"
fsh_plugin="$fsh_cache_dir/zsh-syntax-highlighting.zsh"
[ -d "$fsh_cache_dir" ] || git clone https://github.com/zsh-users/zsh-syntax-highlighting.git "$fsh_cache_dir"
[ -f "$fsh_plugin" ] && source "$fsh_plugin"

# Direnv
# ------------------------

if which direnv &> /dev/null; then
  eval "$(direnv hook zsh)"

  # This silences direnv
  # Worth noting that this is dangerous since you might forget it exists.
  # But I'm ok with that :)
  _direnv_hook() {
    eval "$(direnv export zsh 2> >( egrep -v -e '^direnv: (loading|export|unloading)' ))"
  };
fi
