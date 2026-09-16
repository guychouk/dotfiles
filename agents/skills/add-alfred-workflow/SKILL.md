---
name: add-alfred-workflow
description: >-
  Hand-authors an Alfred workflow (info.plist + scripts + icon) directly in
  ~/dotfiles/alfred/Alfred.alfredpreferences/workflows, matching the format and
  visual conventions of guychouk's existing workflows. TRIGGER when asked to
  add, extend, or speed up an Alfred workflow, keyword, or script filter. SKIP
  when the ask is about Alfred's own settings (file search scope, appearance,
  hotkeys) rather than a workflow — that's GUI-only, see wiki/dotfiles.md.
user-invocable: true
---

# Add an Alfred workflow

Alfred syncs `~/dotfiles/alfred/Alfred.alfredpreferences` live (see
`~/Library/Application Support/Alfred/prefs.json`'s `syncfolders`). Editing
files there while Alfred.app is running is normal and expected — Alfred watches
the folder and picks up changes; no need to quit it first.

## Layout

```
alfred/Alfred.alfredpreferences/workflows/user.workflow.<UUID>/
  info.plist   # objects, connections, keywords — the whole workflow definition
  icon.png     # 256x256, see Icon below
  scripts/     # bash scripts the plist's script-filter/action objects call
alfred/icons/<name>.svg   # source of the icon, committed alongside
```

One UUID per workflow directory (`uuidgen`), one more per plist object (`uid`
field). Bundle id convention: `com.guychouk.<slug>`.

Before writing anything, read one existing workflow's `info.plist` as a worked
template — `Pass` (`user.workflow.D4DB7121-03D3-4DC2-92D3-818A8431248C`) covers
script filter, script action, clipboard output, and multi-modifier connections
in one file.

## Object types used so far

- `alfred.workflow.input.scriptfilter` — a keyword input. Set
  `alfredfiltersresults: true` so Alfred runs the script once (per distinct
  query) and filters the returned list client-side against each item's title,
  instead of re-running the script on every keystroke. Script must print
  `{"items":[{"title":...,"subtitle":...,"arg":...}]}` (add `"valid": false` for
  a disabled/informational row).
- **Do not chain a Script Filter directly into another Script Filter**
  (`keyword: ""` on the destination, fed by a connection) expecting the picked
  item's `arg` to arrive as `"{query}"` in the downstream script. Tried this for
  a two-step "pick a project, then fetch its PRs" flow; Alfred visibly echoes
  the picked text into the search bar, but the workflow debugger showed the
  actual invocation as `Queuing argument '(null)'` — the downstream script never
  receives it, so anything depending on that arg breaks silently (wrong path,
  empty variable, filtered-to-nothing results). The bar text and the real script
  argv are not the same thing for this connection type. If a two-stage lookup is
  genuinely needed, chain into an `alfred.workflow.action.script` node instead —
  that type's `"{query}"` substitution is confirmed working (see `Pass`'s
  `pass show "{query}"` action) — or better, just make the root filter's single
  script do everything up front (parallelize the slow part; see Keep it fast
  below).
- `alfred.workflow.output.clipboard` — `clipboardtext: "{query}"`,
  `transient: true` for a one-off copy that doesn't pollute clipboard history
  forever.
- `alfred.workflow.action.openurl` — `url: "{query}"`, `browser: ""` (default
  browser). This is a lightly-verified object type — after wiring it, open the
  workflow in Alfred's GUI once and confirm the URL field shows `{query}` before
  trusting the cmd path.

`connections` is keyed by source `uid`, each entry an array of
`{destinationuid, modifiers, modifiersubtext, vitoclose}`. Modifier bitmask: `0`
= plain Enter, `1048576` = ⌘, `524288` = ⌥, `131072` = ⇧, `262144` = ⌃.

Validate every edit: `plutil -lint path/to/info.plist`. To see what a running
workflow actually did (its real script invocations and raw output, not just what
the search bar shows) open Alfred Preferences → Workflows → select the workflow
→ the bug icon in the toolbar to start the debugger, run the keyword, read the
log. Use this before guessing at a second fix — it's what caught the null-argv
chaining issue above on the first try.

## Keep it fast

`alfredfiltersresults: true` only avoids re-running the script per keystroke —
it still runs once, synchronously, before you see anything, so any network call
(`gh`, curl, etc.) in that run is felt as input lag. Since chaining into a
second Script Filter to defer the slow part doesn't work (see above), the
options are: keep everything in one script and parallelize the slow part
(background each network call with `&`, `wait`, then combine — see
`Factify Links`'s `ghp` keyword, `scripts/list-prs.sh` in
`user.workflow.1CAD6753-B683-4930-9801-F2C64A6EEFB9`, which fires one
`gh pr view` per repo/worktree in parallel and returns in ~1-2s instead of
running them serially), or accept the flat list and don't try to defer.

Scripts run with Alfred's minimal PATH — start every script with:

```bash
PATH="/opt/homebrew/bin:/Users/guychouk/.local/share/mise/shims:/usr/bin:/bin:$PATH"
```

Test each script directly from the shell with realistic args before wiring it
into the plist; a script bug is much easier to see as raw JSON on stdout than
inside Alfred's UI.

## Icon

Every existing workflow icon is 256x256 PNG, rendered from a 512x512 source SVG
in `alfred/icons/`, in one fixed style: a `#1c2733` rounded-square background
(`rx="104"`, i.e. an app-icon squircle) with a simple geometric glyph drawn in a
single accent color, `stroke-width` around 28-32, `stroke-linecap="round"`. Pick
an accent color not already used by another icon in that directory
(`grep -h fill\\\|stroke alfred/icons/*.svg` to check the palette in use) —
that's the only per-icon design decision; everything else (bg color, corner
radius, line weight) is fixed convention, copy it exactly.

Render with the repo's pinned `resvg` (never a system/homebrew SVG tool):

```bash
cd ~/dotfiles
mise exec -- resvg --width 256 --height 256 \
  alfred/icons/<name>.svg \
  "alfred/Alfred.alfredpreferences/workflows/user.workflow.<UUID>/icon.png"
```

Confirm with `sips -g pixelWidth -g pixelHeight icon.png` — must read 256x256.

## Done when

`plutil -lint` passes, every script runs clean from the shell with a realistic
arg, the icon is 256x256 and committed alongside its source SVG, and — since
Alfred is live — you've confirmed in the running app that the keyword actually
appears and returns results (ask guychouk to check the cmd path on any
`openurl`/rarely-used object type you hand-wrote from memory).
