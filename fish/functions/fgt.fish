# Tags
function fgt
    if not git rev-parse HEAD >/dev/null 2>&1
        return
    end
    git tag --sort -version:refname \
        | fzf --height 50% --multi --preview-window right:70% \
            --preview "git show --color=always {} | head -$LINES"
end
