function yank_project_dir
    find ~/Projects/ -type d -maxdepth 3 -not -path '*/.git/*' -not -name '.git' | fzf --pointer="▶" | tr -d '\n' | pbcopy
end
