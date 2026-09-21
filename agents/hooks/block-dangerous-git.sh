#!/bin/bash
# PreToolUse hook on Bash. Hard-blocks the git operations AGENTS.md already
# says need explicit confirmation, mechanically rather than by memory.
# Adapted from mattpocock/skills' git-guardrails-claude-code, with plain
# `git push` and `git branch -D` left alone (the normal PR workflow pushes
# constantly, and named branch deletes are routine); only the actually
# destructive/hard-to-reverse shapes stay blocked.

INPUT=$(cat)

# `.tool_input.command` by parameter expansion rather than a `jq -r` fork,
# which is 3.8ms of this hook's 8.4ms. The value runs from `"command":"` to
# the first quote not preceded by a backslash, so an escaped `\"` is swapped
# for an escape byte (JSON can never carry one raw) before the cut and back to
# a quote after it. Both harnesses put the command in that `tool_input` object.
COMMAND=${INPUT#*'"command":"'}
if [ "$COMMAND" = "$INPUT" ]; then
  exit 0
fi
ESCAPED_QUOTE='\\"'
COMMAND=${COMMAND//$ESCAPED_QUOTE/$'\001'}
COMMAND=${COMMAND%%'"'*}
COMMAND=${COMMAND//$'\001'/\"}

if [ -z "$COMMAND" ]; then
  exit 0
fi

DANGEROUS_PATTERNS=(
  "git push[^&|;]*(--force|--force-with-lease|-f([[:space:]]|$))"
  "git reset[^&|;]*--hard"
  "git clean[^&|;]*-[a-zA-Z]*f"
  "git (checkout|restore)[^&|;]*[[:space:]]\.([[:space:]]|$)"
  "\-\-no-verify"
  "\-\-no-gpg-sign"
  "commit\.gpgsign=false"
)

for pattern in "${DANGEROUS_PATTERNS[@]}"; do
  if [[ $COMMAND =~ $pattern ]]; then
    echo "BLOCKED: '$COMMAND' matches guarded pattern '$pattern'. This needs guychouk running it himself, not Claude." >&2
    exit 2
  fi
done

exit 0