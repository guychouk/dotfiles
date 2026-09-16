#!/bin/bash
PATH="$HOME/.local/bin:/opt/homebrew/bin:/usr/bin:/bin:$PATH"
NAME="$1"
KITTY=/Applications/kitty.app/Contents/MacOS/kitty

SOCK=""
for f in /tmp/kitty-*; do
	[ -S "$f" ] || continue
	if "$KITTY" @ --to "unix:$f" ls >/dev/null 2>&1; then
		SOCK="$f"
		break
	fi
done

if [ -n "$SOCK" ]; then
	"$KITTY" @ --to "unix:$SOCK" launch --type=tab --cwd="$HOME" -- abduco -a "$NAME"
else
	nohup "$KITTY" -d "$HOME" -- abduco -a "$NAME" >/dev/null 2>&1 &
	disown
fi
