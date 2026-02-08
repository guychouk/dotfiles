#!/usr/bin/env bash

cd "$(dirname "$0")" || exit

# XDG configs (links to ~/.config/)
xdg_configs=(
  fish
  git
  kitty
  yazi
  zsh
  asdf
  direnv
  skhd
  newsboat
)

# Other locations - "source:destination"
other_links=(
  "vim:$HOME/.vim"
  "ctags:$HOME/.ctags.d"
  "zsh/.zshrc:$HOME/.zshrc"
  "zsh/.zshenv:$HOME/.zshenv"
  "asdf/.tool-versions:$HOME/.tool-versions"
  "gnupg/gpg-agent.conf:$HOME/.gnupg/gpg-agent.conf"
  "curl/.curlrc:$HOME/.config/.curlrc"
  "scripts:$HOME/scripts"
)

ensure_link() {
  local dest=$1 src=$2
  if [[ -L "$dest" ]]; then
    local target
    target=$(readlink "$dest")
    if [[ "$target" == "$src" ]]; then
      printf "  [OK] %s\n" "$dest"
    else
      printf "  [ERROR] %s -> %s (expected %s)\n" "$dest" "$target" "$src"
    fi
  elif [[ -e "$dest" ]]; then
    printf "  [ERROR] %s already exists, resolve manually\n" "$dest"
  else
    ln -s "$src" "$dest"
    printf "  [OK] %s -> %s\n" "$dest" "$src"
  fi
}

if [[ "$1" == "unlink" ]]; then
  for dir in "${xdg_configs[@]}"; do
    rm -f "$HOME/.config/$dir"
    echo "  removed ~/.config/$dir"
  done
  for link in "${other_links[@]}"; do
    dest="${link#*:}"
    rm -f "$dest"
    echo "  removed $dest"
  done
else
  for dir in "${xdg_configs[@]}"; do
    ensure_link "$HOME/.config/$dir" "$PWD/$dir"
  done
  for link in "${other_links[@]}"; do
    src="${link%%:*}"
    dest="${link#*:}"
    ensure_link "$dest" "$PWD/$src"
  done
fi
