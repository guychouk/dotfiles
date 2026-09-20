#!/usr/bin/env bash
# Screenshot the debug Chrome tab straight to the clipboard via cdp shotclip.
PATH="/opt/homebrew/bin:/Users/guychouk/.local/share/mise/shims:/usr/bin:/bin:$PATH"

cd "$HOME/src/cdp" || exit 1
if out=$(mise exec -- bun cli.ts shotclip "$1" 2>&1); then
  osascript -e 'display notification "Copied to clipboard" with title "shotclip"'
else
  err=$(echo "$out" | tail -1 | sed 's/"/\\"/g')
  osascript -e "display notification \"$err\" with title \"shotclip failed\""
fi
