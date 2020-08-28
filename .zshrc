#########################
#       Autoloads       #
#########################

autoload -U compinit
autoload -U colors && colors

#########################
#     ENV variables     #
#########################

export VISUAL=nvim
export EDITOR="$VISUAL"
export KEYTIMEOUT=1
export HISTSIZ=E10000
export SAVEHIST=10000
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export DISPLAY=localhost:0.0
export HISTFILE=~/.cache/.zsh_history
export ANDROID_AVD_HOME="$HOME/.android/avd"
export ANDROID_SDK_ROOT=/usr/local/Caskroom/android-sdk/4333796
export PS1="%{$fg[cyan]%}[ %{$reset_color%}%{$fg[yellow]%}%~%{$reset_color%}%{$fg[cyan]%} ]%{$reset_color%}%{$fg[yellow]%} λ "

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

# Set Vim keys on TAB complete menu & Emacs keys in general
bindkey -e
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

# Removes duplicates from $PATH
typeset -aU path
path=($path
        "$HOME/bin"
        "$HOME/go/bin"
        "$HOME/.yarn/bin"
        "$HOME/.config/yarn/global/node_modules/.bin")

if [[ ! -f /proc/sys/kernel/hostname ]]; then
    # We're on macOS
    alias ll='ls -laG'
    alias labo='ssh gv@local.lab.com'

    export TMUX_SESSION='Work'
    export NVM_DIR="$HOME/.nvm"
    [ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"
    [ -s "/usr/local/opt/nvm/etc/bash_completion" ] && . "/usr/local/opt/nvm/etc/bash_completion"

    source '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc'
    source '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc'
    source /usr/local/opt/zsh-syntax-highlighting/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
else 
    # We're on Arch
    alias ll='ls -la --color=auto'
    alias list_packages='comm -23 <(pacman -Qqett | sort) <(pacman -Qqg base-devel | sort | uniq)'

    export TMUX_SESSION='Lab'
    source /usr/share/nvm/init-nvm.sh
    source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
    source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
    eval $(keychain --eval --quiet id_rsa)
fi

export NODE_PATH=`npm config get prefix`/lib/node_modules/

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

