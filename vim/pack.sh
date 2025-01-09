#!/usr/bin/env zsh

mkdir -p tmp/undo
mkdir -p pack/bundle/start

while IFS=' ' read -r repo dir; do
  [ -z "$dir" ] && dir=$(basename "$repo")
  if [ ! -d "pack/bundle/start/$dir" ]; then
    git -C "pack/bundle/start" clone "git@github.com:$repo.git" "$dir"
  fi
done < PACKAGES

for d in pack/bundle/start/*; do
  if ! grep -q "${d##*/}" PACKAGES; then
    echo "Removing package: $d"
    rm -rf "$d"
  fi
done
