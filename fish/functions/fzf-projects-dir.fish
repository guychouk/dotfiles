function fzf-projects-dir
    find ~/Projects/ -type d -not -path '*/.git/*' -not -name '.git' | fzf --pointer="▶" | tr -d '\n' | pbcopy
end
