function gcub --description "Fetch, prune remotes and worktrees, delete gone branches"
    git fetch --prune
    git worktree prune
    for branch in (git for-each-ref --format '%(refname:short) %(upstream:track)' refs/heads | string match -r '.+ \[gone\]' | string replace -r ' \[gone\]' '')
        git branch -D $branch
    end
end
