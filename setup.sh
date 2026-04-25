#!/usr/bin/env bash

cd "$(dirname "$0")" || exit

# XDG configs (links to ~/.config/)
xdg_configs=(
  fish
  git
  kitty
  yazi
  zsh
  mise
  direnv
  skhd
  newsboat
)

# Swift sources to compile - "source:binary"
swift_builds=(
  "bin/gpg-kill-on-lock.swift:$HOME/bin/gpg-kill-on-lock"
  "bin/pbcopy-concealed.swift:$HOME/bin/pbcopy-concealed"
)

# Other locations - "source:destination"
other_links=(
  "vim:$HOME/.vim"
  "ctags:$HOME/.ctags.d"
  "zsh/.zshrc:$HOME/.zshrc"
  "zsh/.zshenv:$HOME/.zshenv"
  "gnupg/gpg-agent.conf:$HOME/.gnupg/gpg-agent.conf"
  "curl/curlrc:$HOME/.curlrc"
  "emacs:$HOME/.emacs.d"
)

# LaunchAgents - "source:destination"
launchagents=(
  "launchd/org.gnupg.gpg-agent.plist:$HOME/Library/LaunchAgents/org.gnupg.gpg-agent.plist"
  "launchd/local.gpg-kill-on-lock.plist:$HOME/Library/LaunchAgents/local.gpg-kill-on-lock.plist"
  "launchd/com.koekeishiya.skhd.plist:$HOME/Library/LaunchAgents/com.koekeishiya.skhd.plist"
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
  if [[ "$OSTYPE" == "darwin"* ]]; then
    for link in "${launchagents[@]}"; do
      dest="${link#*:}"
      launchctl unload "$dest" 2>/dev/null || true
      rm -f "$dest"
      echo "  removed $dest"
    done
  fi
  for f in bin/*; do
    case "$f" in *.swift) continue ;; esac
    rm -f "$HOME/bin/$(basename "$f")"
    echo "  removed $HOME/bin/$(basename "$f")"
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
  if [[ "$OSTYPE" == "darwin"* ]]; then
    mkdir -p "$HOME/Library/LaunchAgents"
    for link in "${launchagents[@]}"; do
      src="${link%%:*}"
      dest="${link#*:}"
      ensure_link "$dest" "$PWD/$src"
      launchctl load "$dest" 2>/dev/null || launchctl bootstrap gui/$(id -u) "$dest" 2>/dev/null || true
    done
  fi
  mkdir -p "$HOME/bin"
  for f in bin/*; do
    case "$f" in *.swift) continue ;; esac
    ensure_link "$HOME/bin/$(basename "$f")" "$PWD/$f"
  done
  for build in "${swift_builds[@]}"; do
    src="${build%%:*}"
    dest="${build#*:}"
    build_swift "$src" "$dest"
  done
fi
