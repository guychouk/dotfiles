---
name: writing-for-agents
description: >-
  Reference for writing or editing any document an agent consumes, a skill,
  AGENTS.md, CLAUDE.md, a repo's wiki/index.md pointer. TRIGGER before creating
  a new skill, or editing AGENTS.md, CLAUDE.md, or an existing SKILL.md.
user-invocable: true
---

# Writing for Agents

Reference for writing any document an agent consumes: a skill,
`AGENTS.md`/`CLAUDE.md`, a wiki/index.md pointer. The packaging differs, the
writing doesn't: the same levers make each one predictable, since the agent
takes the same process every run rather than producing the same output.

## Context pointers

A context pointer is a reference held in context that names out-of-context
material and the condition for reaching it: a skill's description, a line in
`AGENTS.md` naming an article. The pointer's wording, not its target, decides
when and how reliably it's reached. A must-have target behind a weak pointer is
a variance bug, sharpen the wording first, inline only if sharpening fails.

A pointer states what the material is and lists the branches that should trigger
reaching it. Every word of an always-loaded pointer costs on every turn:

- front-load the leading word
- one trigger per branch, collapse synonyms
- cut identity the body already carries

## Context load vs. cognitive load

Every document spends one of two budgets. Context load is the cost of
always-loaded material (an `AGENTS.md` line, a skill description) spent every
turn whether or not it fires. Cognitive load is the cost on guychouk: which
documents exist, when to reach for each. Not a cost to minimize, it's the price
of his own judgment; spend it where his judgment matters, remove it where it
doesn't. Material behind a pointer escapes context load at the price of the
pointer's own line; unpointed material rides entirely on cognitive load.

## Information hierarchy

Steps (ordered actions) and reference (facts consulted on demand) mix freely.
Rank each piece by how immediately it's needed:

1. In-file step, the primary tier.
2. In-file reference, a flat peer-set consulted on demand, fine as-is.
3. Disclosed reference, pushed to a separate file reached by a pointer, loaded
   only when it fires.

Push too little down and the top bloats, this is exactly why `AGENTS.md` has a
150-line budget and the corpus exists; push too much down and material that's
actually needed gets hidden. Branching is the test: inline what every branch
needs, push behind a pointer what only some branches reach.

## Word choice

A compact concept the model already holds from pretraining (tight, red,
frontier) recruits priors for free; a coined word has to be defined from scratch
and costs more. Hunt for restated triads or sentence-long gestures that collapse
into one existing word.

Avoid steering by negation: "don't do X" pulls X into context and makes it more
available, not less. State the positive target instead; a prohibition earns its
place only as a hard guardrail, and even then pair it with the positive form.

## Pruning

- One meaning, one place. Duplication costs tokens and inflates a meaning's rank
  past what it deserves.
- The environment is a source of truth too (a repo's own config, `--help`
  output, its file layout). A document that restates it is a cache, worth its
  cost only when the lookup itself is expensive. Cache the unwritten convention
  and the gotcha, not what a one-file lookup already answers.
- Hunt no-ops: an instruction the model already follows by default pays load to
  say nothing. Test it by running the document, not by debate. A leading word
  too weak to beat the default is a no-op too, fix it with a stronger word, not
  a different technique.
- Without this discipline the default fate is sediment: stale layers nobody
  removes because adding feels safe and removing feels risky.
