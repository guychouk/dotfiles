#!/usr/bin/env bash

# A snippet selector that inserts into readline, bound to C-s. Depends on fzf.
_snip_widget() {
  local selected
  selected=$(
    awk '/^#/ {desc=substr($0, 3); getline; cmd=$0; print desc "\t" cmd}' "$SNIPPETS_FILE" | \
      fzf --delimiter='\t' --with-nth=1 \
      --preview="echo {2..} | bat -l bash -p --color always" \
      --preview-window=up:3:wrap
    )
    if [ -n "$selected" ]; then
      local cmd
      cmd=$(echo "$selected" | cut -f2-)
      READLINE_LINE="$cmd"
      READLINE_POINT=${#READLINE_LINE}
    fi
}

bind -x '"\C-s": _snip_widget'
