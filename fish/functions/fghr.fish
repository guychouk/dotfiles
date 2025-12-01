# Remote + Open in browser
function fghr
    pushd ~/Projects >/dev/null; or return
    find . -type d -maxdepth 1 \
        | parallel 'cd {}; git remote get-url origin 2>/dev/null' \
        | awk '{match($0, /:(.*)/, a); if(a[1]) printf "%s\n", a[1]}' \
        | fzf --height 50% --bind 'enter:abort+execute(open http://github.com/{})'
    popd >/dev/null; or return
end
