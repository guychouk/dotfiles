#!/usr/bin/env bash

cd "$(dirname "$0")"

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

if [[ "$1" == "unlink" ]]; then
  for dir in "${xdg_configs[@]}"; do
    rm -f ~/.config/$dir
  done
  for link in "${other_links[@]}"; do
    dest="${link#*:}"
    rm -f "$dest"
  done
else
  for dir in "${xdg_configs[@]}"; do
    rm -f ~/.config/$dir
    ln -s "$PWD/$dir" ~/.config/$dir
  done
  for link in "${other_links[@]}"; do
    src="${link%%:*}"
    dest="${link#*:}"
    rm -f "$dest"
    ln -s "$PWD/$src" "$dest"
  done
fi
