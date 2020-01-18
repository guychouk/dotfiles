# Enable colors and change prompt:
autoload -U colors && colors
PS1="%B%{$fg[red]%}[ %{$fg[yellow]%}%n%{$fg[green]%} %{$fg[magenta]%}%~%{$fg[red]%} ]%{$reset_color%}%{$fg[yellow]%} λ%b "

# History in cache directory:
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/.zsh_history

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

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

setopt auto_cd # CD to path without using `cd`
setopt hist_reduce_blanks # remove superfluous blanks from history items
setopt hist_ignore_all_dups # remove older duplicate entries from history
setopt appendhistory #Append history to the history file (no overwriting)
setopt inc_append_history # save history entries as soon as they are entered
setopt share_history # share history between different instances of the shell
unsetopt PROMPT_SP # Fix percent sign on initialization

# Aliases
alias em='emacsclient -n'
alias dc='docker-compose -f ~/Projects/box/docker-compose.yml'
alias init_dev='dc up -d campaign-api sdk users interaction editor console strip-pwa player'
alias users_groups='http-prompt --env ~/Projects/iab-sellers/groups.req'
alias users_publishers='http-prompt --env ~/Projects/iab-sellers/publisher.req'
alias dfm='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

# Setup NVM
export NVM_DIR="$HOME/.nvm"
  [ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
  [ -s "/usr/local/opt/nvm/etc/bash_completion" ] && . "/usr/local/opt/nvm/etc/bash_completion"  # This loads nvm bash_completion

# Set Emacs as default terminal editor
export EDITOR="emacs -nw"

# Setup Google cloud SDK completions
source '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc'
    source '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc'

# Load zsh-syntax-highlighting; should be last.
source /usr/local/opt/zsh-syntax-highlighting/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
