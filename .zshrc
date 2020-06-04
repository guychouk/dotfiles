#########################
#       Autoloads       #
#########################

autoload -U compinit
autoload -U colors && colors

#########################
#      Completion       #
#########################

zmodload zsh/complist
zstyle ':completion:*' menu select
compinit
_comp_options+=(globdots) # Include hidden files.

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
alias ll='ls -la'
alias v='f -e nvim'
alias dcc='docker-compose -f ~/Projects/box/docker-compose.yml'
alias dcl='dc logs -f'
alias req='http --verify=no'
alias ffc='ffmpeg -i "`ls -t1 | head -n 1`" ../output.gif'
alias dfm='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

#########################
#     ENV Variables     #
#########################

# Stylize prompt:
PS1="%{$fg[cyan]%}[ %{$reset_color%}%{$fg[yellow]%}%~%{$reset_color%}%{$fg[cyan]%} ]%{$reset_color%}%{$fg[yellow]%} λ "

TMUX_SESSION='Main'

HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/.zsh_history

export LANG=en_US.UTF-8                                                                      # Locale
export LC_ALL=en_US.UTF-8                                                                    # Locale
export LANGUAGE=en_US.UTF-8                                                                  # Locale
export NVM_DIR="$HOME/.nvm"                                                                  # NVM directory
export VISUAL=nvim                                                                           # Set NeoVim as visual editor
export EDITOR="$VISUAL"                                                                      # Set $EDITOR to the same editor in $VISUAL
export KEYTIMEOUT=1                                                                          # How long to wait for additional keys in key sequences (10ms)
export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$HOME/go/bin:$PATH" # Add yarn, npm & go "bin" directories to path
export PATH=$(echo $PATH | tr ':' '\n' | grep -v /mnt/ | tr -s '\n' ':')                     # Remove Windows paths from PATH

#########################
#        Setup          #
#########################

# Setup NVM if installed
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"                           # This loads nvm
[ -s "/usr/local/opt/nvm/etc/bash_completion" ] && . "/usr/local/opt/nvm/etc/bash_completion" # This loads nvm bash_completion

# Setup Google cloud SDK completions
if [[ -a /usr/local/Caskroom ]]; then
  source '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc'
  source '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc'
fi

#########################
#       Functions       #
#########################

function dcr() {
  dc stop "$1" && dc rm -f "$1" && dc up -d "$1" && dc logs -f "$1"
}

# For running Docker commands on WSL
function wslsetup() {
  export DOCKER_HOST=tcp://localhost:2375 
}

function ape-dev() {
  dc up -d sdk campaign player editor console
}

function ape-logs() {
  tmux \
    new-window -n Logs \; send-keys -t Main:Logs "dcc up -d editor; dcc logs -f editor" C-m \; \
    split-window \; send-keys "dcc up -d users; dcc logs -f users" C-m \; \
    split-window \; send-keys "dcc up -d match; dcc logs -f match" C-m \; \
    split-window \; send-keys "dcc up -d player; dcc logs -f player" C-m \; \
    select-layout tiled
}

# Setup zsh-syntax-highlighting (should be last)
source /usr/local/opt/zsh-syntax-highlighting/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null

# Check that tmux exists, that we're in an interactive shell and not already within tmux.
# Taken from here: https://unix.stackexchange.com/a/113768/312299
if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
  # Tests to see if the session already exists. If it does, just reattach.
  if [ "$(tmux list-sessions 2> /dev/null | grep -o $TMUX_SESSION)" != "$TMUX_SESSION" ]; then
    exec tmux new-session -s $TMUX_SESSION
  else
    exec tmux attach -t $TMUX_SESSION
  fi
fi
