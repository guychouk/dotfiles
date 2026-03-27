set -l nix_profiles /nix/var/nix/profiles/default
if test -e $nix_profiles/etc/profile.d/nix-daemon.fish
  source $nix_profiles/etc/profile.d/nix-daemon.fish
end
