# Basic auto/tab complete:
autoload -U compinit

# Enable colors and change prompt:
autoload -U colors && colors
export PS1="%F{38}%1~%F{208} λ %f"

#########################
#     ENV variables     #
#########################

export HISTSIZE=10000
export SAVEHIST=10000
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export HISTFILE="${XDG_DATA_HOME:-$HOME/.local/share}/history"

export ANDROID_AVD_HOME="$HOME/.android/avd"
export ANDROID_SDK_ROOT=/usr/local/Caskroom/android-sdk/4333796
export NODE_REPL_HISTORY="${XDG_CACHE_HOME:-$HOME/.cache}/.node_repl_history"

#########################
#      Completion       #
#########################

zmodload zsh/complist
zstyle ':completion:*' menu select
compinit -d ~/.cache/.zcompdump
_comp_options+=(globdots)

#########################
#         FASD          #
#########################

eval "$(fasd --init auto)"

#########################
#       Bindkeys        #
#########################

# vi mode
bindkey -v
export KEYTIMEOUT=1

bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

# Set Ctrl-R as incremental history search
bindkey '^R' history-incremental-search-backward

#########################
#        Options        #
#########################

setopt auto_cd              # CD to path without using `cd`
setopt hist_reduce_blanks   # remove superfluous blanks from history items
setopt hist_ignore_all_dups # remove older duplicate entries from history
setopt appendhistory        # Append history to the history file (no overwriting)
setopt inc_append_history   # save history entries as soon as they are entered
setopt share_history        # share history between different instances of the shell
setopt interactivecomments  # Turn on comments interactive comments
unsetopt PROMPT_SP          # Fix percent sign on initialization
typeset -aU path            # Removes duplicates from $PATH

#########################
#        Aliases        #
#########################

alias g=git
alias pip=pip3
alias python=python3
alias v='f -e nvim'
alias dcl='dcc logs -f'
alias dcr='dcc restart'
alias dcc='docker-compose -f ~/Projects/box/docker-compose.yml'
alias ffc='ffmpeg -i "`ls -t1 | head -n 1`" ../output.gif'
alias dfm='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

#########################
#        Setup          #
#########################

path=($path "$HOME/bin")

if [[ $(uname) = "Darwin" ]]; then
    source "${XDG_CONFIG_HOME:-$HOME/.config}/.osxenv"
else 
    source "${XDG_CONFIG_HOME:-$HOME/.config}/.archenv"
fi

# Check that tmux exists, that we're in an interactive shell and not already within tmux.
# Taken from here: https://unix.stackexchange.com/a/113768/312299
if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
  # Tests to see if the session already exists. If it does, just reattach.
  if [ "$(tmux list-sessions 2> /dev/null | grep -o $TMUX_SESSION)" != "$TMUX_SESSION" ]; then
    tmux new-session -s "$TMUX_SESSION"
  else
    tmux attach -t "$TMUX_SESSION"
  fi
fi

