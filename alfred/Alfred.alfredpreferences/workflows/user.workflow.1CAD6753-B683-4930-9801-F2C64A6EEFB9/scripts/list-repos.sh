#!/bin/bash
PATH="/opt/homebrew/bin:/Users/guychouk/.local/share/mise/shims:/usr/bin:/bin:$PATH"
set -uo pipefail

FACTIFY="$HOME/src/factify"
items="[]"

to_https() {
	sed -E 's#^git@github\.com:#https://github.com/#; s#\.git$##' <<<"$1"
}

# main worktrees only (.git is a directory): one repo-homepage entry each, no
# network calls, so this stays instant no matter how many linked worktrees exist.
for dir in "$FACTIFY"/*/; do
	dir="${dir%/}"
	name="$(basename "$dir")"
	[ -d "$dir/.git" ] || continue
	remote="$(git -C "$dir" remote get-url origin 2>/dev/null)" || continue
	url="$(to_https "$remote")"
	items="$(jq --arg title "$name" --arg subtitle "$url" --arg arg "$url" \
		'. + [{title:$title, subtitle:$subtitle, arg:$arg}]' <<<"$items")"
done

jq -n --argjson items "$items" '{items:$items}'
