# first entry in path is homebrew

# second is nix
fish_add_path -p "$HOME/.nix-profile/bin"
fish_add_path -p "/nix/var/nix/profiles/default/bin"

# asdf is third
fish_add_path -p "$ASDF_DATA_DIR/shims"
fish_add_path -p "$ASDF_DATA_DIR/completions"

# last are my local binaries and scripts
fish_add_path -p "$HOME/bin"
fish_add_path -p "$HOME/scripts"
fish_add_path -p "$HOME/.local/bin"
