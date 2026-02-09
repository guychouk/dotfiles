function fzf-docs-dir
    find ~/Documents/ -type d -not -path '*/.git/*' -not -name '.git' | fzf --pointer="▶" | tr -d '\n' | pbcopy
end
