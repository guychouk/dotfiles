#!/bin/bash
PATH="$HOME/.local/bin:/opt/homebrew/bin:/usr/bin:/bin:$PATH"
NAME="$1"
RUNNING=$(paddock ls --json | jq -r --arg n "$NAME" '.targets[] | select(.name == $n) | .running')
if [ "$RUNNING" = "true" ]; then
  ACTION="stop"
else
  ACTION="start"
fi
OUT=$(paddock "$ACTION" "$NAME" 2>&1)
if [ $? -eq 0 ]; then
  osascript -e "display notification \"$OUT\" with title \"paddock cmd\" subtitle \"$NAME\""
else
  osascript -e "display notification \"$OUT\" with title \"paddock cmd\" subtitle \"$NAME — error\""
fi
