# Setup completion system and automatic re-compiling on updates
autoload -Uz compinit
typeset -i updated_at=$(date +'%j' -r ~/.zcompdump 2>/dev/null || stat -f '%Sm' -t '%j' ~/.zcompdump 2>/dev/null)
if [ $(date +'%j') != $updated_at ]; then
  compinit -i
else
  compinit -C -i
fi
zmodload -i zsh/complist

# General settings
HISTSIZE=100000
HISTFILE=$HOME/.zsh_history
SAVEHIST=$HISTSIZE

setopt auto_cd # CD to path without using `cd`
setopt hist_reduce_blanks # remove superfluous blanks from history items
setopt hist_ignore_all_dups # remove older duplicate entries from history
setopt inc_append_history # save history entries as soon as they are entered
setopt share_history # share history between different instances of the shell

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

# Fix weird locale issue
export LC_ALL=en_US.UTF-8

# Fix for GPG
GPG_TTY=$(tty)
export GPG_TTY
