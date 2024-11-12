#!/usr/bin/env zsh

LINKS_FILE="./LINKS"

while IFS= read -r line || [[ -n "$line" ]]; do
  # Ignore comments and empty lines
  [[ "$line" =~ ^#.*$ ]] && continue
  [[ -z "$line" ]] && continue

  src=$(echo $line | awk '{print $1}')
  dest=$(echo $line | awk '{print $2}')

  src=$(realpath "$src")
  dest=$(eval echo "$dest")

  if [ -e "$dest" ] || [ -L "$dest" ]; then
    echo "Skipping $dest, file or link already exists."
  else
    echo "Linking $src to $dest"
    ln -s "$src" "$dest"
  fi
done < "$LINKS_FILE"
