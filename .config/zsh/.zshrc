[[ ! -d "${XDG_CACHE_HOME:-$HOME/.cache}/zsh" ]] && mkdir "${XDG_CACHE_HOME:-$HOME/.cache}/zsh" 

autoload -U colors && colors
export PS1="%F{38}%1~%F{208} λ %f"

HISTSIZE=10000
SAVEHIST=10000
HISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/history"

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

bindkey -v
export KEYTIMEOUT=1

bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

setopt auto_cd                      # jump to path without using `cd`
setopt hist_reduce_blanks           # remove superfluous blanks from history items
setopt hist_ignore_all_dups         # remove older duplicate entries from history
setopt appendhistory                # append history instead of overwriting
setopt inc_append_history           # save commands to history as soon as they are entered
setopt share_history                # share history between different instances of the shell
setopt interactivecomments          # enable entering comments as commands that do nothing
setopt hist_expire_dups_first       # expire duplicate entries first when trimming history
typeset -aU path                    # removes duplicates from $PATH

alias \
    g=git \
    pip=pip3 \
    vim=nvim \
    nv=nvim \
    python=python3 \
    v='f -e nvim' \
    fh='find ~/ | fzf' \
    fb='find ~/.local/bin | fzf' \
    dcl='dcc logs -f' \
    dcr='dcc restart' \
    dcc='docker-compose -f ~/Projects/box/docker-compose.yml' \
    dfm='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME' \
    tmux='tmux -f "${XDG_CONFIG_HOME:-$HOME/.config}/tmux/tmux.config"'

path=($path "$HOME/.local/bin" "$GOPATH/bin")

if [[ $(uname) = "Darwin" ]]; then
    alias ll='ls -laG'

    export TMUX_SESSION='Work'
    export CLOUDSDK_PYTHON="/usr/local/opt/python@3.8/libexec/bin/python"
    export GCLOUD_SDK="/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk"
    export ZETZ_PATH="${HOME}/Projects/personal/zetz"

    [ -f ${GCLOUD_SDK}/path.zsh.inc ] && source "${GCLOUD_SDK}/path.zsh.inc"
    [ -f ${GCLOUD_SDK}/completion.zsh.inc ] && source "${GCLOUD_SDK}/completion.zsh.inc"

    [ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"
else 
    alias \
        ll='ls -la --color=auto' \
        list_packages='comm -23 <(pacman -Qqett | sort) <(pacman -Qqg base-devel | sort | uniq)'

    export TMUX_SESSION='Lab'
    export GNUPGHOME="${XDG_CONFIG_HOME:-$HOME/.config}/gnupg"
    export PYENV_ROOT="${XDG_DATA_HOME:-$HOME/.local/share}/pyenv"
    export NPM_CONFIG_USERCONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/npm/.npmrc"

    eval $(keychain --dir "${XDG_DATA_HOME:-$HOME/.local/share}/keychain" --eval --quiet id_rsa)

    [ -s "/usr/share/nvm/nvm.sh" ] && . "/usr/share/nvm/nvm.sh"
    eval "$(pyenv init -)"
    eval "$(rbenv init -)"
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

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -d ~/.config/zsh/fsh ] || git clone https://github.com/zdharma/fast-syntax-highlighting ~/.config/zsh/fsh
[ -f ~/.config/zsh/fsh/fast-syntax-highlighting.plugin.zsh ] && source ~/.config/zsh/fsh/fast-syntax-highlighting.plugin.zsh

eval "$(fasd --init posix-alias zsh-hook zsh-wcomp-install zsh-wcomp)"
