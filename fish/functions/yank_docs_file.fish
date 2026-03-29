function yank_docs_file --description "Yank path to specific document in ~/Documents"
    find ~/Documents -type f -not -path '*/.git/*' | fzf --pointer="▶" | tr -d '\n' | pbcopy
end
