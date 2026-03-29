function yank_project_dir --description "Yank path to specific directory in ~/projects"
    find ~/projects/ -type d -maxdepth 3 -not -path '*/.git/*' -not -name '.git' | fzf --pointer="▶" | tr -d '\n' | pbcopy
end
