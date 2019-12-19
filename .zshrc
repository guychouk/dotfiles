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
