#!/bin/bash
PATH="$HOME/.local/bin:/opt/homebrew/bin:/usr/bin:/bin:$PATH"
NAME="$1"
PORT=$(paddock ls --json | jq -r --arg n "$NAME" '.targets[] | select(.name == $n) | (.ports // [])[0] // empty')
if [ -z "$PORT" ]; then
  osascript -e "display notification \"$NAME has no known port\" with title \"paddock cmd\" subtitle \"Error\""
  exit 0
fi
open "http://localhost:$PORT"
