#!/bin/bash
PATH="$HOME/.local/bin:/opt/homebrew/bin:/usr/bin:/bin:$PATH"
NAME="$1"
TITLE="${NAME#agent-}"

SOCK=""
for f in /tmp/kitty-*; do
	[ -S "$f" ] || continue
	if kitty @ --to "unix:$f" ls >/dev/null 2>&1; then
		SOCK="$f"
		break
	fi
done

if [ -n "$SOCK" ]; then
	kitty @ --to "unix:$SOCK" launch --type=tab --tab-title="$TITLE" --cwd="$HOME" -- abduco -a "$NAME"
else
	nohup kitty -d "$HOME" -o "tab_title_template=$TITLE" -- abduco -a "$NAME" >/dev/null 2>&1 &
	disown
fi
