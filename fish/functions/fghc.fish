# Commits + Open in browser
function fghc
    if not git rev-parse HEAD >/dev/null 2>&1
        return
    end
    set repo (git config --get remote.origin.url | sed 's/[^:]*://' | sed 's/\.git//')
    open "https://github.com/$repo/commit/"(fgc)
end
