# Remotes
function fgr
    if not git rev-parse HEAD >/dev/null 2>&1
        return
    end
    git remote -v | awk '{print $1 "\t" $2}' | uniq \
        | fzf --height 50% --tac \
            --preview "git log --oneline --graph --date=short --pretty=\"format:%C(auto)%cd %h%d %s\" {1} | head -200" \
        | cut -d\t -f1
end
