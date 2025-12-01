# Branches
function fgb
    if not git rev-parse HEAD >/dev/null 2>&1
        return
    end
    git branch -a --color=always | grep -v '/HEAD\s' | sort \
        | fzf --height 50% --ansi --multi --tac --preview-window right:70% \
            --preview "git log --oneline --graph --date=short --color=always --pretty=\"format:%C(auto)%cd %h%d %s\" (sed 's/^..//' | cut -d' ' -f1 | echo) | head -$LINES" \
        | sed 's/^..//' | cut -d' ' -f1 \
        | sed 's#^remotes/##'
end
