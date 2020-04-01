# Autoload completion initializor
autoload -U compinit
# Autoload colors
autoload -U colors && colors
# Stylize prompt:
PS1="%B%{$fg[red]%}[ %{$fg[yellow]%}%n%{$fg[green]%} %{$fg[magenta]%}%~%{$fg[red]%} ]%{$reset_color%}%{$fg[yellow]%} λ%b "

# History in cache directory:
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/.zsh_history

# Setup completion
zmodload zsh/complist
zstyle ':completion:*' menu select
compinit
# Include hidden files.
_comp_options+=(globdots)

# vi mode
bindkey -v
export KEYTIMEOUT=1

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char
bindkey '^R' history-incremental-search-backward

# Options
setopt auto_cd # CD to path without using `cd`
setopt hist_reduce_blanks # remove superfluous blanks from history items
setopt hist_ignore_all_dups # remove older duplicate entries from history
setopt appendhistory #Append history to the history file (no overwriting)
setopt inc_append_history # save history entries as soon as they are entered
setopt share_history # share history between different instances of the shell
setopt interactivecomments # Turn on comments interactive comments
unsetopt PROMPT_SP # Fix percent sign on initialization

# Aliases
alias em='emacsclient -n'
alias dc='docker-compose -f ~/Projects/box/docker-compose.yml'
alias pip=pip3
alias req='http --verify=no'
alias dfm='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias ffc='ffmpeg -i "`ls -t1 | head -n 1`" ../output.gif'
alias python=python3

# Some Locale configuration
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
# Set Emacs as default terminal editor
export EDITOR="emacs -nw"
# Add yarn, npm & go bin directories to path
export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$HOME/go/bin:$PATH"
# Setup Google cloud SDK completions
source '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc'
source '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc'
# Setup NVM
export NVM_DIR="$HOME/.nvm"
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/usr/local/opt/nvm/etc/bash_completion" ] && . "/usr/local/opt/nvm/etc/bash_completion"  # This loads nvm bash_completion
# Load zsh-syntax-highlighting; should be last.
source /usr/local/opt/zsh-syntax-highlighting/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
