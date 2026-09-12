#!/bin/bash
PATH="$HOME/.local/bin:/opt/homebrew/bin:/usr/bin:/bin:$PATH"
paddock ls --json | jq '
  [.targets[] | select(.kind == "agent")]
  | sort_by(.name)
  | map({
      uid: .name,
      title: (.name | ltrimstr("agent-")),
      subtitle: (
        (if .running then "● running" + (if .pid then " (pid \(.pid))" else "" end) else "○ stopped" end)
        + "   " + (.cwd // "")
      ),
      arg: .name,
      icon: {path: "icon.png"},
      mods: {
        cmd: {
          subtitle: (if .running then "Attach abduco session in kitty" else "Not running — nothing to attach" end),
          arg: .name,
          valid: .running
        }
      }
    })
  | {items: .}
'
