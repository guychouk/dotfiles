# 🏠 dotfiles

Personal configuration for the machines I use daily: macOS primarily, a Linux
box on the side. No framework, no installer wizard.

## Layout

One directory per tool, mirroring the config layout that tool expects under
`~/.config` or `$HOME`. [`nob.c`](nob.c) is the only piece of glue: a small C
program (built with [nob.h](https://github.com/tsoding/nob.h)) that symlinks
each directory into place. Run `./nob` to (re)link everything on a given
machine.

`SNIPPETS` is a flat text file of commands I'd otherwise forget - fzf+bat
picker bound to a shell abbreviation, not a script, because these are rare
and fiddly rather than frequent.

## Notes

There's a longer-form writeup of how and why this is put together over at
[notes.guycho.uk/home-sweet-home](https://notes.guycho.uk/home-sweet-home).
This README is just a quick summary while that page is the tour.

## Using this

Not meant to be installed wholesale on someone else's machine - paths and
tool choices are mine. I hope something here will be of interest to you.
