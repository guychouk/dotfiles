#!/bin/bash

mkdir -p pack/myplugins/start

while IFS=' ' read -r repo dir; do
  [ -z "$dir" ] && dir=$(basename "$repo")
  git -C "pack/myplugins/start" clone "git@github.com:${repo}.git" "$dir"
done < packages.txt
