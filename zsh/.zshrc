export VISUAL=vim
export EDITOR="${VISUAL}"
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export SHELL_SESSIONS_DISABLE=1
export MANPAGER="vim +MANPAGER --not-a-term -"

export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"
export ZDOTDIR="${XDG_CONFIG_HOME}/zsh"
export ZSH_CACHE_DIR="${XDG_CACHE_HOME}/zsh"

export GPG_TTY=$(tty)
export HISTFILE="${ZSH_CACHE_DIR}/history"
export KUBECONFIG="${XDG_CONFIG_HOME}/kube/config"
export GOPATH="${XDG_DATA_HOME}/go"
export CARGO_HOME="${XDG_DATA_HOME}/cargo"
export PARALLEL_HOME="${XDG_CACHE_HOME}/parallel"
export GEM_HOME="${XDG_DATA_HOME}/gem"
export GEM_PATH="${GEM_HOME}"
export DOCKER_CONFIG="${XDG_DATA_HOME}/docker"
export NPM_CONFIG_USERCONFIG="${XDG_CONFIG_HOME}/.npmrc"
export NPM_CONFIG_CACHE="${XDG_CACHE_HOME}/npm"
export AMMONITE_HOME="${XDG_CONFIG_HOME}/ammonite"
export NODE_REPL_HISTORY="${XDG_CACHE_HOME}/.node_repl_history"
export TS_NODE_HISTORY="${XDG_CACHE_HOME}/.ts_node_repl_history"
export WGET_HSTS_FILE="${XDG_CONFIG_HOME}/.wget-hsts"
export PYTHONHISTFILE="${XDG_CACHE_HOME}/.python_history"
export LESSHISTFILE="${XDG_CACHE_HOME}/.lesshst"

if [[ "$(uname)" == "Linux" ]]; then
  export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
fi

# Override any of the above with .env
if [ -f ~/.env ]; then
  source ~/.env
fi

## ZSH Setup

[[ ! -d "${ZSH_CACHE_DIR}" ]] && mkdir "${ZSH_CACHE_DIR}"

HISTSIZE=50000
SAVEHIST=50000

unsetopt auto_cd              # disable jump to path without using `cd`
setopt hist_reduce_blanks     # remove superfluous blanks from history items
setopt hist_ignore_all_dups   # remove older duplicate entries from history
setopt inc_append_history     # save commands to history as soon as they are entered
setopt share_history          # share history between different instances of the shell
setopt extended_history       # add timestamps to history
setopt interactive_comments   # enable entering comments as commands that do nothing
setopt hist_expire_dups_first # expire duplicate entries first when trimming history
setopt hist_ignore_space      # ignore entries that start with space (for sensitive commands)
setopt globdots               # autocomplete hidden files and folders (dotfiles)
setopt auto_param_slash       # automatically adds a trailing slash to directory names during completion
setopt complete_in_word       # allow completion within a word
setopt glob_complete          # completes based on glob patterns
setopt list_rows_first        # lists completion options row-wise instead of column-wise
setopt no_beep                # disable beeping for errors and completion

# Completion setup

zmodload -i zsh/complist
# The following lines were added by compinstall
zstyle ':completion:*' completer _complete _ignored
zstyle ':completion:*' list-dots yes
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list ''
zstyle ':completion:*' menu select=2
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle :compinstall filename "${ZDOTDIR}/.zshrc"
autoload -Uz compinit
compinit -d "${ZSH_CACHE_DIR}/compdump"
# End of lines added by compinstall

# select option from completion menu using h,j,k,l
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

bindkey -e                          # selects keymap `emacs` & set as main keymap
bindkey "^U" backward-kill-line     # kill backwards from cursor to the beginning of the line

# Let ^W delete to slashes (taken from statico's dotfiles)
function backward-delete-to-slash() {
  local WORDCHARS=${WORDCHARS//\//}
  zle .backward-delete-word
}
zle -N backward-delete-to-slash
bindkey "^W" backward-delete-to-slash

# edit command line with $EDITOR
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey -M emacs '^v' edit-command-line

## Colors

autoload -U colors
colors
export LS_COLORS="di=36:fi=37:ln=34:ex=32:pi=33:so=35:bd=46:cd=43"

## Prompt

parse_git_branch() {
  case "${PWD}" in
    /net/*|/Volumes/*) return ;;
  esac
  git_branch=$(git symbolic-ref --short HEAD 2> /dev/null)
  if [ ! "${git_branch}" ]; then printf ""; else printf " [${git_branch}]"; fi
}

parse_kubectl_current_context() {
  kube_context=$(kubectl config current-context 2> /dev/null)
  if [ ! "${kube_context}" ]; then printf ""; else printf " [${kube_context}]"; fi
}

setopt PROMPT_SUBST
PROMPT=$'%{\e[3 q%}%F{8}[%m] %f%F{4}%1~%F{16}$(parse_git_branch) λ %f'

## Syntax highlighting

function () {
  local fsh_cache_dir="${ZSH_CACHE_DIR}/zsh-syntax-highlighting"
  local fsh_plugin="${fsh_cache_dir}/zsh-syntax-highlighting.zsh"
  [ -d "${fsh_cache_dir}" ] || git clone https://github.com/zsh-users/zsh-syntax-highlighting.git "${fsh_cache_dir}"
  [ -f "${fsh_plugin}" ] && source "${fsh_plugin}"
}

## ZSH Abbreviations

function () {
  local zsh_abbr_cache_dir="${ZSH_CACHE_DIR}/zsh-abbr"
  local zsh_abbr_plugin="${zsh_abbr_cache_dir}/zsh-abbr.zsh"
  [ -d "${zsh_abbr_cache_dir}" ] || git clone https://github.com/olets/zsh-abbr --single-branch --branch v5 --depth 1 "${zsh_abbr_cache_dir}"
  [ -f "${zsh_abbr_plugin}" ] && source "${zsh_abbr_plugin}"
}

## PATH

export ASDF_DIR="${XDG_DATA_HOME}/asdf"
export ASDF_DATA_DIR="${ASDF_DIR}"
export ASDF_CONFIG_FILE="${XDG_CONFIG_HOME}/asdf/.asdfrc"
export ASDF_GOLANG_MOD_VERSION_ENABLED=true
mkdir -p "$XDG_CONFIG_HOME/asdf"
[ -d "$ASDF_DIR" ] || git clone https://github.com/asdf-vm/asdf.git "$ASDF_DIR" --branch v0.10.0
source "$ASDF_DIR/asdf.sh"

if [[ "$(uname)" == "Darwin" && -f /opt/homebrew/bin/brew ]]; then
  export HOMEBREW_CASK_OPTS="--appdir=/Applications"
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

export PATH="$ASDF_DIR/shims:$ASDF_DIR/completions:$GEM_HOME:$(go env GOBIN):$HOME/bin:$HOME/scripts:$HOME/.local/bin:$PATH"

## Programs

_has() {
  return $(whence $1 &>/dev/null)
}

if _has eza; then
  alias ll='eza -lag --time-style=long-iso --group-directories-first --color=always'
elif _has gls; then
  alias ll='gls -lah --group-directories-first --color=always'
else
  alias ll='ls -lah --color=always'
fi

if _has fzf; then
  source "${ZDOTDIR}/functions/fzf-git"
  export FZF_DEFAULT_OPTS="--prompt='λ ' --margin 2%,2% --height 65% --info=inline-right:'🔍 ' --reverse --no-separator --no-scrollbar"

  if _has rg; then
    export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --glob "!.git/*"'
  fi

  if [[ "$(uname)" == "Darwin" ]]; then
    [[ $- == *i* ]] && source "$(brew --prefix)/opt/fzf/shell/completion.zsh" 2> /dev/null
    [[ $- == *i* ]] && source "$(brew --prefix)/opt/fzf/shell/key-bindings.zsh"
  elif [[ "$(uname)" == "Linux" ]]; then
    [ -f /usr/share/fzf/completion.zsh ] && source /usr/share/fzf/completion.zsh 2> /dev/null
    [ -f /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh
  fi
fi

if _has direnv; then
  export DIRENV_LOG_FORMAT=
  eval "$(direnv hook zsh)"
fi

if _has zoxide; then
  eval "$(zoxide init --cmd j zsh)"
fi

## Hooks

function update_path_for_node_modules() {
  PATH=$(awk -v RS=: -v ORS=: '!/node_modules\/.bin/' <<< "$PATH" | sed 's/:$//')
  [[ -d "$PWD/node_modules/.bin" ]] && PATH="$PWD/node_modules/.bin:$PATH"
}
function auto_virtualenv() {
  # If already in the desired virtualenv, do nothing
  if [[ -n "$VIRTUAL_ENV" && "$PWD" == "$VIRTUAL_ENV"* ]]; then
    return
  fi
  # Deactivate if leaving the virtualenv directory
  if [[ -n "$VIRTUAL_ENV" && "$PWD" != "$VIRTUAL_ENV_WORKDIR"* ]]; then
    deactivate
    unset VIRTUAL_ENV_WORKDIR
  fi
  # Activate if .venv exists in the current directory
  if [[ -e ".venv/bin/activate" ]]; then
    source .venv/bin/activate
    export VIRTUAL_ENV_WORKDIR="$PWD"
  fi
}
autoload -U add-zsh-hook
add-zsh-hook chpwd auto_virtualenv
add-zsh-hook chpwd update_path_for_node_modules
auto_virtualenv
update_path_for_node_modules
