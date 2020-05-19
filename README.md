# My dotfiles

A repo used for keeping track of my configuration files as proposed by DistroTube in [this](https://www.youtube.com/watch?v=tBoLDpTWVOM) video.

To use it:
1. Clone it using `git clone --bare <repo-url> ~/.dotfiles`.
1. Add this alias to .bashrc: `alias dfm='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'`
1. Run `dfm pull` to pull all of the configuration files.
