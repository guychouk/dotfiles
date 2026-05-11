set -gx LANG en_US.UTF-8
set -gx LC_ALL en_US.UTF-8
set -gx LC_CTYPE en_US.UTF-8
set -gx SHELL_SESSIONS_DISABLE 1

set -gx EDITOR vim
set -gx VISUAL vim
set -gx PAGER less
set -gx MANPAGER "vim +MANPAGER --not-a-term -"

set -gx SNIPPETS_FILE         "$HOME/dotfiles/SNIPPETS"

set -gx XDG_CACHE_HOME        "$HOME/.cache"
set -gx XDG_CONFIG_HOME       "$HOME/.config"
set -gx XDG_DATA_HOME         "$HOME/.local/share"
set -gx XDG_STATE_HOME        "$HOME/.local/state"

set -gx DOCKER_CONFIG         "$XDG_DATA_HOME/docker"
set -gx GEM_HOME              "$XDG_DATA_HOME/gem"
set -gx LESSHISTFILE          "$XDG_CACHE_HOME/.lesshst"
set -gx NODE_REPL_HISTORY     "$XDG_CACHE_HOME/.node_repl_history"
set -gx NPM_CONFIG_CACHE      "$XDG_CACHE_HOME/npm"
set -gx NPM_CONFIG_USERCONFIG "$XDG_CONFIG_HOME/npm/npmrc"
set -gx PARALLEL_HOME         "$XDG_CACHE_HOME/parallel"
set -gx PYTHONHISTFILE        "$XDG_CACHE_HOME/.python_history"
set -gx TS_NODE_HISTORY       "$XDG_CACHE_HOME/.ts_node_repl_history"
set -gx WGET_HSTS_FILE        "$XDG_CONFIG_HOME/.wget-hsts"

set -gx COLORTERM truecolor
set -gx LS_COLORS "di=34:ln=36:ex=01;32:or=91:mi=91:pi=93:so=35:bd=94:cd=93:su=37;41:sg=37;42:tw=37;46:ow=34;40:st=37;44"
set -gx EZA_COLORS "ur=37:uw=37:ux=32:ue=32:gr=37:gw=37:gx=32:tr=37:tw=37:tx=32:xa=90:sn=37:sb=37:uu=37:un=37:gu=95:gn=95:da=37:im=37"
