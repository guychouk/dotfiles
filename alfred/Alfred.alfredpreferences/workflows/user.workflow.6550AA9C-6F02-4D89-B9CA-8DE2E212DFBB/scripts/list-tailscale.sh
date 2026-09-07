#!/bin/bash
PATH="$HOME/.local/bin:/opt/homebrew/bin:/usr/bin:/bin:$PATH"
paddock ls --json | jq '
  [.targets[] | select(.kind == "tailscale-serve")]
  | sort_by(.name)
  | map({
      uid: .name,
      title: .name,
      subtitle: (
        (if .running then "● exposed" else "○ not exposed" end)
        + (if .localPort and .tailnetPort then "   :" + (.localPort|tostring) + " → :" + (.tailnetPort|tostring) else "" end)
      ),
      arg: .name,
      icon: {path: "icon.png"}
    })
  | {items: .}
'
