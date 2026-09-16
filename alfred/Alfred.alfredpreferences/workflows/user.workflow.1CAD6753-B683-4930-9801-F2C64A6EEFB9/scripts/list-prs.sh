#!/bin/bash
PATH="/opt/homebrew/bin:/Users/guychouk/.local/share/mise/shims:/usr/bin:/bin:$PATH"
set -uo pipefail

FACTIFY="$HOME/src/factify"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

i=0
# every repo dir (main worktree or linked worktree), one gh call each, all in
# parallel: single flat list, no chained steps, no query-matching tricks.
for dir in "$FACTIFY"/*/; do
	dir="${dir%/}"
	[ -e "$dir/.git" ] || continue
	git -C "$dir" remote get-url origin >/dev/null 2>&1 || continue
	i=$((i + 1))
	(
		name="$(basename "$dir")"
		pr_json="$(cd "$dir" && gh pr view --json url,title,number 2>/dev/null)"
		[ -n "$pr_json" ] && jq --arg name "$name" '. + {project: $name}' <<<"$pr_json" >"$tmp/$i.json"
	) &
done
wait

results="$(jq -s '[.[] | select(length > 0)] | unique_by(.number, .project)' "$tmp"/*.json 2>/dev/null)"
[ -n "$results" ] && [ "$results" != "null" ] || results="[]"

jq '{
	items: [.[] | {
		title: ("#" + (.number|tostring) + " " + .title),
		subtitle: (.project + " — " + .url),
		arg: .url
	}]
}' <<<"$results"
