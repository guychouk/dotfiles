function pass_entries --description "List all pass store entries"
    set -l prefix (set -q PASSWORD_STORE_DIR; and echo $PASSWORD_STORE_DIR; or echo $HOME/.password-store)
    for f in $prefix/**/*.gpg
        string replace -r '^'"$prefix"'/' '' -- $f | string replace -r '\.gpg$' '' --
    end
end
