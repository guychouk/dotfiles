#!/bin/bash
# PreToolUse hook on Bash. Hard-blocks the git operations AGENTS.md already
# says need explicit confirmation, mechanically rather than by memory.
# Adapted from mattpocock/skills' git-guardrails-claude-code, with plain
# `git push` and `git branch -D` left alone (the normal PR workflow pushes
# constantly, and named branch deletes are routine); only the actually
# destructive/hard-to-reverse shapes stay blocked.

INPUT=$(cat)
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command // empty')

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
  if echo "$COMMAND" | grep -qE "$pattern"; then
    echo "BLOCKED: '$COMMAND' matches guarded pattern '$pattern'. This needs guychouk running it himself, not Claude." >&2
    exit 2
  fi
done

exit 0
