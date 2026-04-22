# first entry in path is homebrew

# second is nix
fish_add_path -p "$HOME/.nix-profile/bin"
fish_add_path -p "/nix/var/nix/profiles/default/bin"

# last are my scripts and binaries in my $HOME directory
fish_add_path -p "$HOME/bin"
fish_add_path -p "$HOME/.local/bin"
