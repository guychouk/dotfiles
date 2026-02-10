function yank_docs_file
    find ~/Documents -type f -not -path '*/.git/*' | fzf --pointer="▶" | tr -d '\n' | pbcopy
end
