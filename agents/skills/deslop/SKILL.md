---
name: deslop
description: >-
  Reviews a branch diff against main and removes AI-generated slop: comments
  that narrate the agent's reasoning (decision history, rejected alternatives,
  ticket/PR refs, "observed every run"), comments that restate the code,
  over-defensive blocks, redundant casts, and style inconsistencies. Also cleans
  AI tells out of prose (journal entries, knowledge articles, tickets, PR
  bodies, reports). Use when reviewing a branch before committing or creating a
  PR, when cleaning a written draft, or when the user says deslop, clean up
  slop, or remove AI junk.
user-invocable: true
---

# Deslop

Remove the deliberation trail an AI leaves behind, without changing behavior.
Slop is implementation-time reasoning that leaked into the artifact. Maintainers
care what the code does and which lines are load-bearing, not how it came to be.

## When to use

Before commit or PR, or when the user explicitly asks to clean AI-generated
noise.

## The test for every comment

> Would someone maintaining this code, who never saw the diff, the PR, or the
> chat, need this line to avoid breaking something?

No → cut it. Yes → keep it, in the fewest words.

## Remove: agent thinking noise

This is the highest-value cut and the easiest to miss, because each line reads
as "helpful context." It is not; it is a work log.

- **Decision history / comparison to what it replaced:**
  `# deterministic, no fixed sleep, no re-mint churn`, `# instead of X`,
  `# simpler than …`, `# the old approach guessed`. The rejected alternative is
  not in the code; do not document it.
- **Observational narration:** `# observed every run`, `# this was consistent`,
  `# turns out …`, `# after much debugging`.
- **Ticket / PR / reviewer refs as inline justification:** `(ENG-1234)`,
  `(PR #101 review)`, `(Codex P1)`. Traceability lives in git blame and the PR.
- **Meta-commentary:** `# same idiom as X`, `# SC2016 is the intent`,
  `# note that`, `# important:`, `# elegantly handles`.
- **Restating the code:** `i++  # increment i`, `# loop over files` above a
  plain loop.

## Remove: structural slop

- Over-defensive try/catch or nil checks that don't match surrounding code
  style.
- Redundant type casts or assertions added "just in case."
- Inconsistent naming, formatting, or patterns versus the rest of the file.
- Hedging / self-narration in PR and commit bodies ("I decided to…", "As you can
  see…", long recaps of the journey).

## Keep: but terse

Deslop is not "delete all comments." These carry signal a maintainer cannot
recover from the code alone:

- Non-obvious load-bearing invariants, especially security/safety
  (`# root-only`, `# fail closed on fork PRs`). One line, so nobody deletes the
  guard by accident.
- Real gotchas the code can't express
  (`# piping to head SIGPIPEs gh under pipefail`).
- Usage / contract of a script or public function (args, env, exit codes).
- A magic number's meaning (`exp=$((now + 540))  # 9-min JWT (10 max)`).

## Prose

Everything above is about code. The same deliberation trail shows up in writing,
as a set of rhetorical tics that make any two AI-written paragraphs sound like
each other. Diane writes a lot of prose (journal entries, knowledge articles,
Linear tickets, PR bodies, `~/Documents` reports), so this half fires as often
as the code half.

The test: **would a competent writer with something to say have reached for this
construction, or is it filling the space where an argument goes?**

Cut these:

- **Binary contrast.** "It's not X, it's Y." "This isn't just a refactor, it's a
  rethink." Says one thing while sounding like two. State Y.
- **Throat-clearing openers.** "Here's the thing." "Let's be clear." "The
  reality is." Delete the sentence; the next one was the point.
- **False-insight setup.** "What nobody tells you." "The part everyone misses."
  A claim to secret knowledge standing in for the knowledge.
- **The colon reveal.** "The problem: nobody owns the schema." A drumroll before
  a short clause. Write it as a sentence.
- **Dramatic fragments.** "Every time." "That's it." "Not anymore." Emphasis
  borrowed from rhythm rather than earned by content.
- **Importance inflation.** "Crucial", "pivotal", "game-changing", "at its
  core", "fundamentally". If the thing matters, the facts show it.
- **Buzzword filler.** "Seamless", "robust", "leverage", "commitment to
  quality", "best-in-class". Name the actual property instead.
- **Weasel attribution.** "Studies show", "it is widely known", "experts agree",
  and the passive "it has been observed". Cite the source or drop the claim.
- **Synonym churn and the rule of three.** Three near-synonyms where one word
  works ("clear, concise, and readable"). Pick the one that carries the meaning.
- **Fake-profound conclusions.** A closing paragraph that restates the piece in
  a graver register and adds nothing. End at the last real sentence.

Also cut, per the standing voice rules: em dashes anywhere, "great question" and
its relatives, restating the question before answering it, and bullet lists
standing in for paragraphs that should be prose.

Keep the voice. Slop removal is not homogenization: a writer's own vocabulary,
sentence rhythm, dry asides, and preference for a blunt word over a polite one
all stay. Cut the constructions any model would have produced, not the ones only
this author would have.

Procedure: read the draft top to bottom flagging each hit, cut rather than
reword where the sentence carried nothing, and reread the result once for
rhythm, since removing a fragment sometimes leaves two long sentences colliding.
Never change a factual claim, a number, a command, or a quoted string while
cleaning.

## How (code)

1. Diff against `main` (or the target base branch). When asked to deslop a
   _feature_, scope the whole surface (sibling scripts, Dockerfiles, charts,
   tests), not one file.
2. Triage each hunk: keep behavior, delete noise. Prefer deletion over
   shortening; collapse a multi-line rationale to one clause or nothing; align
   style with the file's authors.
3. Never change logic, control flow, or contract strings (log markers, status
   text a test greps for, user-facing messages).
4. Re-validate with the file's cheapest proof: `bash -n` + `shellcheck` for
   shell, `go build`/`vet`/tests for Go, the package's suite otherwise. Done
   only when the checks are as green as before.
5. Keep the diff minimal, no unrelated refactors.

## Example

Before:

```sh
# A freshly-minted GitHub App token is not usable by GitHub's git-over-HTTPS endpoint
# for a few seconds — the first push 404s ("Repository not found") until it propagates.
# This was consistent every run (ENG-1234 e2e on pr-101 + pr-102): attempt 1 failed,
# then the retry succeeded. Poll until it authenticates, THEN act once — the same
# "wait for an observed signal" idiom as readiness_gate.sh, not a fixed sleep.
```

After:

```sh
# A fresh token 404s at the git endpoint for a few seconds; poll until it authenticates.
```
