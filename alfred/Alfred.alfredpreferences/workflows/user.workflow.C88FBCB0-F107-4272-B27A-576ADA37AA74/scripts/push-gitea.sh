#!/bin/bash
PATH="/opt/homebrew/bin:/usr/bin:/bin:$PATH"
REPO="$1"
NAME=$(basename "$REPO")
BRANCH=$(git -C "$REPO" symbolic-ref --short -q HEAD)
if [ -z "$BRANCH" ]; then
  osascript -e "display notification \"cannot push a detached HEAD\" with title \"$NAME\" subtitle \"Error\""
  exit 0
fi
osascript -e "display notification \"Pushing $BRANCH to gitea...\" with title \"$NAME\""
if git -C "$REPO" push gitea "$BRANCH" >/tmp/gitea_push.log 2>&1; then
  osascript -e "display notification \"Push complete\" with title \"$NAME\" subtitle \"Success\""
else
  osascript -e "display notification \"Push failed — see /tmp/gitea_push.log\" with title \"$NAME\" subtitle \"Error\""
fi
