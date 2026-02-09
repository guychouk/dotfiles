function fzf-projects-file
    find ~/Projects -type f -not -path '*/.git/*' | fzf --pointer="▶" | tr -d '\n' | pbcopy
end
