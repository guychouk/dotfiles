#!/bin/zsh

if [[ $(uname) == "Darwin" ]]; then
    /opt/homebrew/bin/pinentry-mac "$@"
else
    /usr/bin/pinentry-tty "$@"
fi
