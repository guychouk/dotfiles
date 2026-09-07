#!/bin/bash
PATH="/opt/homebrew/bin:/usr/bin:/bin:$PATH"
items="[]"
for d in "$HOME"/src/*/ "$HOME/dotfiles"; do
  [ -d "$d/.git" ] || continue
  git -C "$d" remote get-url gitea >/dev/null 2>&1 || continue
  branch=$(git -C "$d" symbolic-ref --short -q HEAD)
  name=$(basename "$d")
  items=$(jq -c --arg name "$name" --arg path "$d" --arg branch "${branch:-detached HEAD}" \
    '. + [{"uid":$name,"title":$name,"subtitle":("push " + $branch + " to gitea"),"arg":$path,"icon":{"path":"icon.png"}}]' \
    <<<"$items")
done
jq -n --argjson items "$items" '{items: ($items | sort_by(.title))}'
