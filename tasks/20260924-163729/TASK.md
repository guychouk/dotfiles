# FZF_DEFAULT_OPTS differs between fish and zsh

- STATUS: OPEN
- PRIORITY: 80
- TAGS: shell

fish/conf.d/fzf.fish exports
`--margin=2%,0% --height 70% --info=hidden --layout=reverse --no-scrollbar`;
zsh/.zshrc exports
`--prompt='λ ' --margin 2%,2% --height 65% --info=inline-right:'🔍 ' --reverse --no-separator --no-scrollbar`.
The same finder looks different depending on which shell launched it, and vim's
fzf inherits whichever. Noted 2026-08-27, not fixed. Make them one, or decide
zsh is no longer maintained in parallel (the 2026-08-24 audit's open question
about zsh's role).
