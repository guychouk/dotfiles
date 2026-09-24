# Spellcheck for text typed in kitty

- STATUS: OPEN
- PRIORITY: 90
- TAGS: kitty

kitty has no spellcheck, so neither does anything typed into a terminal program,
Claude Code's and tape's prompts included (noted 2026-08-06, coming from vim's
`:set spell`). The workaround is looking words up in a browser. Look for an
inline or terminal-level option before building anything; the prompt's own
external-editor key (`/edit` in tape, Ctrl-G in Claude Code) into vim with
`spell` on may already be enough.
