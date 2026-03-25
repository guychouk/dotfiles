complete -c pcp -f -a "(find (set -q PASSWORD_STORE_DIR; and echo $PASSWORD_STORE_DIR; or echo $HOME/.password-store) -name '*.gpg' | sed 's|.*/\.password-store/||; s|\.gpg||')"
