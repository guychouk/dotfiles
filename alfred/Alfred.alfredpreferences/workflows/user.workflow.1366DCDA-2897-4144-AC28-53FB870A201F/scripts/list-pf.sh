#!/bin/bash
PATH="$HOME/.local/bin:/opt/homebrew/bin:/usr/bin:/bin:$PATH"
paddock ls --json | jq '
  [.targets[] | select(.kind == "portforward")]
  | sort_by(.name)
  | map({
      uid: .name,
      title: .name,
      subtitle: (
        (if .running then "● running" + (if .pid then " (pid \(.pid))" else "" end) else "○ stopped" end)
        + "   :" + (.localPort|tostring) + " → " + (.resource // "?") + " :" + (.remotePort|tostring)
        + (if .namespace then "   (" + .namespace + ")" else "" end)
      ),
      arg: .name,
      icon: {path: "icon.png"}
    })
  | {items: .}
'
