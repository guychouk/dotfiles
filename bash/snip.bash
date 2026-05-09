#!/usr/bin/env bash

# This is an fzf-based snippet selector that inserts into readline, bound to C-s.
_snip_widget() {
    local snippets_file=~/dotfiles/SNIPPETS
    local selected=$(
        awk '/^#/ {desc=substr($0, 3); getline; cmd=$0; print desc "\t" cmd}' "$snippets_file" | \
        fzf --delimiter='\t' --with-nth=1 \
            --preview="echo {2..} | bat -l bash -p --color always" \
            --preview-window=up:3:wrap
    )
    if [ -n "$selected" ]; then
        local cmd=$(echo "$selected" | cut -f2-)
        READLINE_LINE="$cmd"
        READLINE_POINT=${#READLINE_LINE}
    fi
}

bind -x '"\C-s": _snip_widget'
