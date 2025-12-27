#!/usr/bin/env bash

# Snippet selector that inserts into readline
_snip_widget() {
    local snippets_file=~/dotfiles/SNIPPETS

    # Use fzf to select a snippet
    local selected=$(
        awk '/^#/ {desc=substr($0, 3); getline; cmd=$0; print desc "\t" cmd}' "$snippets_file" | \
        fzf --delimiter='\t' --with-nth=1 \
            --preview="echo {2..} | bat -l bash -p --color always" \
            --preview-window=up:3:wrap
    )

    # Extract command and insert into readline
    if [ -n "$selected" ]; then
        local cmd=$(echo "$selected" | cut -f2-)
        READLINE_LINE="$cmd"
        READLINE_POINT=${#READLINE_LINE}
    fi
}

# Bind ctrl-s to the widget
bind -x '"\C-s": _snip_widget'
