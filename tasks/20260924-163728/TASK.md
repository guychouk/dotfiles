# GMAN palette drift between vim, kitty and emacs

- STATUS: OPEN
- PRIORITY: 60
- TAGS: theme

The three theme files are no longer the same theme (found 2026-09-21, still true
2026-09-24): kitty/gman-theme.conf uses green `#86df8d`, white `#f5f1e3` and
magenta `#e67eb3` where vim/colors/gman.vim has `#7ed68a`, `#e8e1cf` and
`#d97aad` (vim uses `#e67eb3` as its cyan); emacs/themes/gman-theme.el shares
kitty's green and foreground. vim/colors/gman.vim is hand-maintained, not
generated (no gman tool exists; that idea is a task in ~/src/diane/tasks), and
coop's TUI already treats the vim file as the source. Pick the source, bring the
others in line, and record the palette and its contrast rule (QuickFixLine's
plum was chosen at 9.67:1 on 2026-09-21) somewhere all three can be checked
against.
