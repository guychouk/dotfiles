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

# Nix
if test -e /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish
    source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish
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
fish_add_path -p "$HOME/.nix-profile/bin"
fish_add_path -p "/nix/var/nix/profiles/default/bin"
fish_add_path -p "$ASDF_DIR/shims"
fish_add_path -p "$ASDF_DIR/completions"
fish_add_path -p "$GEM_HOME"
fish_add_path -p "$HOME/bin"
fish_add_path -p "$HOME/scripts"
fish_add_path -p "$HOME/.local/bin"

# Colors - gman theme (ANSI 16-color)
set -gx COLORTERM truecolor

# di = directories
# ln = symlinks
# ex = executables
# or = orphaned symlinks
# mi = missing files
# pi = pipes (FIFO)
# so = sockets
# bd = block devices
# cd = character devices
# su = setuid files
# sg = setgid files
# tw = sticky + writable
# ow = other writeable
# st = sticky files
set -gx LS_COLORS "di=34:ln=36:ex=01;32:or=91:mi=91:pi=93:so=35:bd=94:cd=93:su=37;41:sg=37;42:tw=37;46:ow=34;40:st=37;44"

# eza colors for metadata fields (sizes, user, group, dates)
# ur/uw/ux = user file permissions
# gr/gw/gx = group file permissions
# tr/tw/tx = others file permissions
# sn = size number
# sb = size bytes
# uu = user owner (truncated)
# un = user owner (normal)
# gu = group owner (truncated)
# gn = group owner (normal)
# da = date (timestamp)
# im = important files (README, Makefile, package.json, etc.)
# xa = extended attributes (@)
set -gx EZA_COLORS "ur=37:uw=37:ux=32:ue=32:gr=37:gw=37:gx=32:tr=37:tw=37:tx=32:xa=90:sn=37:sb=37:uu=37:un=37:gu=95:gn=95:da=37:im=37"

set -g fish_color_command green           # Commands
set -g fish_color_param normal            # Parameters
set -g fish_color_error red               # Errors
set -g fish_color_quote green             # Quotes
set -g fish_color_valid_path --underline  # Underline valid paths

# History
set -g fish_history_max 50000

## Save history immediately after each command
function save_history --on-event fish_postexec
    history save
end

# Keybindings

# Listing files
if command -q eza
    alias ll='eza -la --time-style=long-iso --group-directories-first --color=always'
else if command -q gls
    alias ll='gls -lah --group-directories-first --color=always'
else
    alias ll='ls -lah --color=always'
end

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
    set -gx FZF_ALT_C_COMMAND ""
    set -gx FZF_DEFAULT_OPTS "--prompt='' --margin=2%,2% --height 65% --info=inline-right --layout=reverse --no-scrollbar --tiebreak=length"
    if status is-interactive
        fzf --fish | source
    end
end
