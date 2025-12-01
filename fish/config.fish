# Environment Variables
set -gx LANG en_US.UTF-8
set -gx LC_ALL en_US.UTF-8
set -gx LC_CTYPE en_US.UTF-8
set -gx SHELL_SESSIONS_DISABLE 1

set -gx EDITOR vim
set -gx VISUAL vim
set -gx PAGER less
set -gx MANPAGER "vim +MANPAGER --not-a-term -"

set -gx XDG_CACHE_HOME "$HOME/.cache"
set -gx XDG_CONFIG_HOME "$HOME/.config"
set -gx XDG_DATA_HOME "$HOME/.local/share"
set -gx XDG_STATE_HOME "$HOME/.local/state"

set -gx OLLAMA_API_BASE "http://127.0.0.1:11434"

# XDG compliance for various tools
set -gx GEM_HOME "$XDG_DATA_HOME/gem"
set -gx GEM_PATH "$GEM_HOME"
set -gx DOCKER_CONFIG "$XDG_DATA_HOME/docker"
set -gx PARALLEL_HOME "$XDG_CACHE_HOME/parallel"
set -gx NPM_CONFIG_USERCONFIG "$XDG_CONFIG_HOME/.npmrc"
set -gx NPM_CONFIG_CACHE "$XDG_CACHE_HOME/npm"
set -gx NODE_REPL_HISTORY "$XDG_CACHE_HOME/.node_repl_history"
set -gx TS_NODE_HISTORY "$XDG_CACHE_HOME/.ts_node_repl_history"
set -gx WGET_HSTS_FILE "$XDG_CONFIG_HOME/.wget-hsts"
set -gx PYTHONHISTFILE "$XDG_CACHE_HOME/.python_history"
set -gx LESSHISTFILE "$XDG_CACHE_HOME/.lesshst"

# Homebrew
if test (uname) = "Darwin" -a -f /opt/homebrew/bin/brew
    set -gx HOMEBREW_CASK_OPTS "--appdir=/Applications"
    eval (/opt/homebrew/bin/brew shellenv)
end

# asdf-vm
set -gx ASDF_DIR "$XDG_DATA_HOME/asdf"
set -gx ASDF_DATA_DIR "$ASDF_DIR"
set -gx ASDF_CONFIG_FILE "$XDG_CONFIG_HOME/asdf/.asdfrc"
set -gx ASDF_GOLANG_MOD_VERSION_ENABLED true

if test -d "$ASDF_DIR"
    or git clone https://github.com/asdf-vm/asdf.git "$ASDF_DIR" --branch v0.10.0
end

if test -f "$ASDF_DIR/asdf.fish"
    source "$ASDF_DIR/asdf.fish"
end

# PATH
fish_add_path -p "$ASDF_DIR/shims"
fish_add_path -p "$ASDF_DIR/completions"
fish_add_path -p "$GEM_HOME"
fish_add_path -p "$HOME/bin"
fish_add_path -p "$HOME/scripts"
fish_add_path -p "$HOME/.local/bin"

# Colors
set -gx LS_COLORS "di=36:fi=37:ln=34:ex=32:pi=33:so=35:bd=46:cd=43"
set -gx COLORTERM truecolor

# History
set -g fish_history_max 50000

# Save history immediately after each command
function save_history --on-event fish_postexec
    history save
end

# Aliases
if command -q eza
    alias ll='eza -lag --time-style=long-iso --group-directories-first --color=always'
else if command -q gls
    alias ll='gls -lah --group-directories-first --color=always'
else
    alias ll='ls -lah --color=always'
end

# Tool integrations

# direnv
if command -q direnv
    set -gx DIRENV_LOG_FORMAT ""
    direnv hook fish | source
end

# zoxide
if command -q zoxide
    zoxide init --cmd j fish | source
end

# fzf
if command -q fzf
    set -gx FZF_DEFAULT_OPTS "--prompt='λ ' --margin 2%,2% --height 65% --info=inline-right:'🔍 ' --reverse --no-separator --no-scrollbar"
    source /opt/homebrew/opt/fzf/shell/key-bindings.fish
    set -gx FZF_ALT_C_COMMAND ""
    fzf_key_bindings
end
