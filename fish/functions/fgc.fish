# Commits
function fgc
    if not git rev-parse HEAD >/dev/null 2>&1
        return
    end
    git log --date=short --format="%C(green)%C(bold)%cd %C(auto)%h%d %s (%an)" --graph --color=always \
        | fzf --height 50% --ansi --no-sort --reverse --multi --bind 'ctrl-s:toggle-sort' \
            --header 'Press CTRL-S to toggle sort' \
            --preview "grep -o '[a-f0-9]\\{7,\\}' | head -1 | xargs git show --color=always | head -$LINES" \
        | grep -o '[a-f0-9]\{7,\}'
end
