#!/bin/bash
PATH="$HOME/.local/bin:/opt/homebrew/bin:/usr/bin:/bin:$PATH"
NAME="$1"
RUNNING=$(coop ls --json | jq -r --arg n "$NAME" '.targets[] | select(.name == $n) | .running')
if [ "$RUNNING" = "true" ]; then
  ACTION="stop"
else
  ACTION="start"
fi
OUT=$(coop "$ACTION" "$NAME" 2>&1)
if [ $? -eq 0 ]; then
  osascript -e "display notification \"$OUT\" with title \"coop pf\" subtitle \"$NAME\""
else
  osascript -e "display notification \"$OUT\" with title \"coop pf\" subtitle \"$NAME — error\""
fi
