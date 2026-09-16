---
name: diagnosing-bugs
description: >-
  Structured diagnosis loop for a hard bug or performance regression, building a
  tight reproducible feedback loop before touching a hypothesis. TRIGGER when
  the user reports something broken, throwing, failing, or slow, or says
  "diagnose" or "debug this". SKIP a trivial one-line fix where the cause is
  already obvious from the error message or stack trace alone, just fix it.
user-invocable: true
---

# Diagnosing Bugs

A discipline for hard bugs and performance regressions. Skip phases only when
explicitly justified.

Before exploring the codebase, check the target repo's wiki/index.md (or
AGENTS.md) for domain vocabulary and known gotchas.

## Redact

Show commands, outputs, and captured artifacts freely, but redact every secret
first: write `<REDACTED>` in its place. Build loops against env vars so
credentials stay in the environment rather than in what gets shown. Captured
artifacts often carry auth headers; quote only the lines that carry the signal.

## Phase 1: build a feedback loop

This is the skill. Everything else is mechanical. A tight loop that goes red on
this exact bug finds the cause; bisection, hypothesis testing, and
instrumentation all just consume it. No loop, no amount of reading code helps.

Ways to build one, roughly in this order:

1. Failing test at whatever seam reaches the bug.
2. Curl or an HTTP script against a running dev server.
3. CLI invocation with a fixture input, diffed against a known-good snapshot.
4. Headless browser script (`cdp`, per this machine's tooling) driving the UI
   and asserting on DOM, console, network.
5. Replay a captured trace: save the real request, payload, or event log to
   disk, replay it in isolation.
6. Throwaway harness: a minimal subset of the system exercising the bug path
   with one function call.
7. Property or fuzz loop for "sometimes wrong output": many random inputs, look
   for the failure mode.
8. Bisection harness for a bug that appeared between two known states: script
   "boot at state X, check, repeat" so it's `git bisect run`-able.
9. Differential loop: same input through old vs new, diff the outputs.
10. Last resort, human in the loop: drive the human with a short structured
    script rather than a freeform back and forth.

### Tighten the loop

Once there's a loop, tighten it: faster (skip unrelated init, narrow scope),
sharper (assert the specific symptom, not "didn't crash"), more deterministic
(pin time, seed RNG, isolate filesystem, freeze network). A 30-second flaky loop
is barely better than none; a 2-second deterministic one is a debugging
superpower.

### Non-deterministic bugs

Aim for a higher reproduction rate, not a clean repro. Loop the trigger a
hundred times, parallelize, add stress, narrow timing windows. A 50%-flake bug
is debuggable; a 1% one is not, so keep raising the rate until it is.

### When no loop is possible

Stop and say so. List what was tried. Ask for: access to the environment that
reproduces it, a redacted captured artifact (HAR file, log dump, core dump,
timestamped recording), or permission to add temporary instrumentation. Do not
hypothesize without a loop.

### Done when

Name one command, already run at least once (show the invocation and output,
redacted), that is red-capable (drives the real bug path, asserts the user's
exact symptom), deterministic, fast, and runnable unattended.

## Phase 2: reproduce and minimize

Run the loop, confirm it produces the user's actual symptom, not a different
nearby failure, reproducible across runs. Then shrink to the smallest scenario
that still goes red: cut inputs, callers, config, data one at a time, rerunning
after each cut. Done when every remaining element is load-bearing.

## Phase 3: hypothesize

Generate 3 to 5 ranked, falsifiable hypotheses before testing any of them: "if X
is the cause, changing Y makes the bug disappear, changing Z makes it worse." No
prediction, no hypothesis; discard or sharpen it. Show the ranked list before
testing, domain knowledge often re-ranks it instantly. Don't block on a response
if nobody's watching.

## Phase 4: instrument

One probe per prediction, one variable at a time. Prefer a debugger or REPL
breakpoint over logs; never "log everything and grep." Tag every debug log with
a unique prefix so cleanup is one grep. For performance regressions, measure
first (timing harness, profiler, query plan), then bisect; logs are usually the
wrong tool there.

## Phase 5: fix and regression test

Write the regression test before the fix, but only at a seam that exercises the
real bug pattern as it occurs at the call site. If no correct seam exists,
that's itself the finding, flag it rather than write a test that gives false
confidence. Otherwise: failing test at that seam, watch it fail, apply the fix,
watch it pass, rerun the original unminimized repro.

## Phase 6: cleanup

Before calling it done: original repro no longer reproduces, regression test
passes (or the missing seam is documented), every debug-log prefix is grepped
out, throwaway prototypes are deleted, and the confirmed hypothesis is stated in
the commit message so the next person doesn't re-derive it.
