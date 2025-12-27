#!/usr/bin/env zsh

# Snippet selector that inserts into zle
_snip_widget() {
    local snippets_file=~/dotfiles/SNIPPETS

    # Use fzf to select a snippet
    local selected=$(
        awk '/^#/ {desc=substr($0, 3); getline; cmd=$0; print desc "\t" cmd}' "$snippets_file" | \
        fzf --delimiter='\t' --with-nth=1 \
            --preview="echo {2..} | bat -l bash -p --color always" \
            --preview-window=up:3:wrap \
            --bind='ctrl-y:preview-up,ctrl-e:preview-down'
    )

    # Extract command and insert into zle buffer
    if [ -n "$selected" ]; then
        local cmd=$(echo "$selected" | cut -f2-)
        BUFFER="$cmd"
        CURSOR=${#BUFFER}
    fi

    zle reset-prompt
}

# Register as zle widget and bind to ctrl-s
zle -N _snip_widget
bindkey '^s' _snip_widget
