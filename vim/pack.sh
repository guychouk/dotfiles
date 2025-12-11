#!/usr/bin/env bash

# This script reads the PACKAGES file and clones all of the packages from
# github to pack/bundle/start. It then proceeds to remove packages that DON'T
# exist in the same PACKAGES file. This is a layman's solution to package
# managers in vim which utilizes vim's built-in packages system (:h packages)

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
