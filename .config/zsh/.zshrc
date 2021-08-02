[[ ! -d "${XDG_CACHE_HOME:-$HOME/.cache}/zsh" ]] && mkdir "${XDG_CACHE_HOME:-$HOME/.cache}/zsh" 

unsetopt PROMPT_SP                  # fix percent sign on initialization
autoload -U colors && colors
export PS1="%F{38}%1~%F{208} λ %f"

HISTSIZE=10000
SAVEHIST=10000
HISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/history"

export ZETZ_PATH="${HOME}/Projects/personal/zetz"
export FZF_DEFAULT_OPTS='--height 50% --layout=reverse'
export GOPATH="${XDG_DATA_HOME:-$HOME/.local/share}/go"
export _FASD_DATA="${XDG_CACHE_HOME:-$HOME/.cache}/.fasd"
export NVM_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/nvm"
export CARGO_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/cargo"
export YARN_CACHE_FOLDER="${XDG_CACHE_HOME:-$HOME/.cache}/yarn"
export NODE_REPL_HISTORY="${XDG_CACHE_HOME:-$HOME/.cache}/.node_repl_history"

autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit -d "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/compdump"
_comp_options+=(globdots)

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
    k=kubectl \
    nv=nvim \
    v='f -e nvim' \
    vf='nv `fzf`' \
    zetz='${ZETZ_PATH}/bin/zetz' \
    dfm='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME' \
    tmux='tmux -f "${XDG_CONFIG_HOME:-$HOME/.config}/tmux/tmux.config"'

path=($path "$HOME/bin" "$GOPATH/bin")

typeset -aU path                    # removes duplicates from $PATH

if [[ $(uname) = "Darwin" ]]; then
  source "${XDG_CONFIG_HOME:-$HOME/.config}/zsh/.zshrc-macos"
else 
  source "${XDG_CONFIG_HOME:-$HOME/.config}/zsh/.zshrc-arch"
fi

# Check that tmux exists, that we're in an interactive shell and not already within tmux.
if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
  # If the session with the name $TMUX_SESSION does not exist - create it.
  if [ "$(tmux list-sessions 2> /dev/null | grep -o $TMUX_SESSION)" != "$TMUX_SESSION" ]; then
    tmux new-session -s "$TMUX_SESSION"
  # Otherwise just reattach.
  else
    tmux attach -t "$TMUX_SESSION"
  fi
fi

if command -v fasd 1>/dev/null 2>&1; then
  eval "$(fasd --init posix-alias zsh-hook zsh-wcomp-install zsh-wcomp)"
fi

if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/fzf/fzf.zsh" ] \
  && source "${XDG_CONFIG_HOME:-$HOME/.config}/fzf/fzf.zsh"

[ -d "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/fsh" ] \
  || git clone https://github.com/zdharma/fast-syntax-highlighting "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/fsh"

[ -f "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/fsh/fast-syntax-highlighting.plugin.zsh" ] \
  && source "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/fsh/fast-syntax-highlighting.plugin.zsh"


# GIT <3 FZF
# -------------

if command -v fzf 1>/dev/null; then

  is_in_git_repo() {
    git rev-parse HEAD > /dev/null 2>&1
  }

  fzf-down() {
    fzf --height 50% "$@"
  }

  # Changed files
  fgf() {
    is_in_git_repo || return
    git -c color.status=always status --short |
    fzf-down -m --ansi --nth 2..,.. \
      --preview '(git diff --color=always -- {-1} | sed 1,4d; cat {-1}) | head -500' |
    cut -c4- | sed 's/.* -> //'
  }

  # Branches
  fgb() {
    is_in_git_repo || return
    git branch -a --color=always | grep -v '/HEAD\s' | sort |
    fzf-down --ansi --multi --tac --preview-window right:70% \
      --preview 'git log --oneline --graph --date=short --color=always --pretty="format:%C(auto)%cd %h%d %s" $(sed s/^..// <<< {} | cut -d" " -f1) | head -'$LINES |
    sed 's/^..//' | cut -d' ' -f1 |
    sed 's#^remotes/##'
  }

  # Tags
  fgt() {
    is_in_git_repo || return
    git tag --sort -version:refname |
    fzf-down --multi --preview-window right:70% \
      --preview 'git show --color=always {} | head -'$LINES
  }

  # Commits
  fgc() {
    is_in_git_repo || return
    git log --date=short --format="%C(green)%C(bold)%cd %C(auto)%h%d %s (%an)" --graph --color=always |
    fzf-down --ansi --no-sort --reverse --multi --bind 'ctrl-s:toggle-sort' \
      --header 'Press CTRL-S to toggle sort' \
      --preview 'grep -o "[a-f0-9]\{7,\}" <<< {} | xargs git show --color=always | head -'$LINES |
    grep -o "[a-f0-9]\{7,\}"
  }

  # Remotes
  fgr() {
    is_in_git_repo || return
    git remote -v | awk '{print $1 "\t" $2}' | uniq |
    fzf-down --tac \
      --preview 'git log --oneline --graph --date=short --pretty="format:%C(auto)%cd %h%d %s" {1} | head -200' |
    cut -d$'\t' -f1
  }

fi
