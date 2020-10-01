# Yet another dotfiles repository

A repo used for keeping track of my configuration files as proposed by DistroTube in [this](https://www.youtube.com/watch?v=tBoLDpTWVOM) video.

Use it in the following manner:
1. Clone it using `git clone --bare git@github.com:cudacoder/dotfiles.git $HOME/.dotfiles`.
1. Set the following alias: `alias dfm='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'`
1. Run `dfm pull` to pull all of the configuration files.
