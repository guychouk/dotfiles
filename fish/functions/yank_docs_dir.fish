function yank_docs_dir --description "Yank path to specific directory in ~/Documents"
    find ~/Documents/ -type d -maxdepth 3 -not -path '*/.git/*' -not -name '.git' | fzf --pointer="▶" | tr -d '\n' | pbcopy
end
