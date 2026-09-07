#!/bin/bash
PATH="$HOME/.local/bin:/opt/homebrew/bin:/usr/bin:/bin:$PATH"
NAME="$1"
PORT=$(paddock ls --json | jq -r --arg n "$NAME" '
  .targets[] | select(.name == $n) |
  if (.ports // []) != [] then (.ports[0]) elif .localPort then .localPort else empty end
')
if [ -z "$PORT" ]; then
  osascript -e "display notification \"$NAME has no known port\" with title \"paddock\" subtitle \"Error\""
  exit 0
fi
open "http://localhost:$PORT"
