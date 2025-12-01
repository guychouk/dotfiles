# Changed files
function fgf
    if not git rev-parse HEAD >/dev/null 2>&1
        return
    end
    git -c color.status=always status --short \
        | fzf --height 50% -m --ansi --nth 2..,.. \
            --preview '(git diff --color=always -- {-1} | sed 1,4d; cat {-1}) | head -500' \
        | cut -c4- | sed 's/.* -> //'
end
