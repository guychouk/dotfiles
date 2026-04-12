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

# Swift sources to compile - "source:binary"
swift_builds=(
  "scripts/gpg-kill-on-lock.swift:$HOME/bin/gpg-kill-on-lock"
  "scripts/pbcopy-concealed.swift:$HOME/bin/pbcopy-concealed"
)

# Other locations - "source:destination"
other_links=(
  "vim:$HOME/.vim"
  "ctags:$HOME/.ctags.d"
  "zsh/.zshrc:$HOME/.zshrc"
  "zsh/.zshenv:$HOME/.zshenv"
  "asdf/.tool-versions:$HOME/.tool-versions"
  "gnupg/gpg-agent.conf:$HOME/.gnupg/gpg-agent.conf"
  "gnupg/org.gnupg.gpg-agent.plist:$HOME/Library/LaunchAgents/org.gnupg.gpg-agent.plist"
  "gnupg/local.gpg-kill-on-lock.plist:$HOME/Library/LaunchAgents/local.gpg-kill-on-lock.plist"
  "curl/curlrc:$HOME/.curlrc"
  "scripts:$HOME/scripts"
  "emacs:$HOME/.emacs.d"
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

build_swift() {
  local src=$1 dest=$2
  if ! command -v swiftc &>/dev/null; then
    return
  fi
  mkdir -p "$(dirname "$dest")"
  if swiftc "$PWD/$src" -o "$dest" 2>/dev/null; then
    printf "  [OK] %s -> %s\n" "$src" "$dest"
  else
    printf "  [ERROR] failed to compile %s\n" "$src"
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
  for build in "${swift_builds[@]}"; do
    src="${build%%:*}"
    dest="${build#*:}"
    build_swift "$src" "$dest"
  done
fi
