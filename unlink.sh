#!/usr/bin/env zsh

LINKS_FILE="./LINKS"

while IFS= read -r line || [[ -n "$line" ]]; do
  # Ignore comments and empty lines
  [[ "$line" =~ ^#.*$ ]] && continue
  [[ -z "$line" ]] && continue

  dest=$(echo $line | awk '{print $2}')

  if [ -L "$dest" ]; then
    echo "Removing symlink $dest"
    rm "$dest"
  else
    echo "Skipping $dest, not a symlink."
  fi
done < "$LINKS_FILE"
