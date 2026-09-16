---
name: code-review
description: >-
  Review a branch or PR for things that actually matter (security, correctness,
  efficiency, backwards compatibility) and skip the rest. Wraps the reviewer
  agent for the core review, then layers Factify's Linear spec-conformance check
  and multi-PR fanout on top. Trigger when asked to review a branch, review a
  PR, run code review, or fan out reviews across many open PRs.
user-invocable: true
---

# Code Review

Everything about the actual review bar, voice, and posting mechanics lives in
the `reviewer` agent (`agents/agents/reviewer.md`) - this skill spawns it and
adds two things it doesn't do: checking the diff against its Linear ticket, and
fanning a review out across many open PRs at once. Read `reviewer.md` for the
bar and voice; don't restate them here.

## When to use

- User asks to review a branch, review a PR, run code review, review staged
  changes.
- User asks to fan out reviews across many open PRs (each gets its own worktree
  and its own review).
- Before approving a teammate's PR.

Do not use for: scaffolding, fixing the code, or writing it. This skill produces
a review, nothing else.

## Process

1. **Spawn the reviewer agent** (Agent tool, `subagent_type: reviewer`) with the
   worktree path and PR number or branch. It writes `PR-REVIEW.md` and, when a
   verdict needs posting, `PR-COMMENT.md` at the worktree root per its own
   process.

2. **Layer on spec conformance.** A fifth axis, alongside the reviewer agent's
   four, checking whether the diff delivers what the ticket asked for. Every
   Factify PR has a `Closes ENG-XXX` line; fetch it
   (`linear issue view ENG-XXX`) and read its `## Acceptance` section.

   Check three things: acceptance criteria the diff doesn't touch at all,
   behavior the ticket never asked for (scope creep), and criteria that look
   implemented but land wrong. Quote the acceptance line for each finding, same
   discipline as the reviewer agent: a location and a concrete gap, not "doesn't
   fully match the ticket."

   Append this as its own `## Spec Conformance` section in `PR-REVIEW.md`, after
   the agent's own Verdict section, and fold anything that changes the verdict
   into `PR-COMMENT.md` before posting. If there's no `Closes` line and no other
   way to find the spec, say so and skip the axis rather than guess at intent.

3. **Post if asked**, following the reviewer agent's own posting steps (Process,
   step 4, in `reviewer.md`).

## Fanning out across many PRs

When the user wants every open PR by some author reviewed in parallel:

```bash
# list open PRs
gh pr list --repo <owner/repo> --assignee <handle> --state open \
  --json number,title,headRefName,baseRefName

# create a worktree per PR
git -C <repo> worktree add ../<repo>-pr-<N> origin/<headRefName>
```

Spawn one reviewer agent per PR (single message, multiple tool calls, so they
run in parallel). Each agent gets the worktree path and PR number. Do not use
`claude --print --permission-mode bypassPermissions` for the fanout - the
harness classifier blocks it as an unsafe autonomous agent loop. Use Agent
subagents instead, they inherit the session's permission scope.

When all agents finish, layer spec conformance onto each `PR-REVIEW.md` per step
2, then post the matching `gh pr review` action per PR (`--request-changes` for
NEEDS WORK, `--approve` for APPROVED and APPROVED WITH CHANGES).
