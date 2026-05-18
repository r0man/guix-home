---
name: shiny
description: Engineer in a Box — the disciplined way to ship a non-trivial code change: design before you code, review before you ship, test before you submit. Use this skill whenever the user asks to implement or build a feature, fix a non-trivial bug, or make a multi-file change and wants it done "properly", "the right way", "carefully", "don't cut corners", or "design it first" — even when they don't name a workflow. Also trigger when a change is risky, touches several files, or the user has been burned before by rushed work. Use the shiny-enterprise skill instead when the work is high-stakes/critical and warrants iterative refinement; use the code-review skill for review-only requests and the design skill for design-only requests.
---

# Shiny — Engineer in a Box

The canonical right way to ship a change: **design before you code, review before you ship, test before you submit.** Five phases, each with an acceptance gate you must clear before moving on.

The whole point of this skill is to fight the default failure mode of jumping straight to code. Code written before the approach is thought through is code you rewrite. A review you skip is a bug you ship. Tests you defer are regressions you discover in production. The gates exist so that each phase's work is *real* — not a box you tick on the way to the next one.

Walk the phases in order. Don't start a phase until the previous gate is genuinely met. If a phase reveals the previous one was wrong (design doesn't survive contact with implementation), go back and fix it — that's the process working, not failing.

## Phase 1 — Design

Think about architecture *before* writing code. Genuinely consider:

- How does this fit into the existing system? What patterns/utilities already exist that you should reuse?
- What are the edge cases and failure modes?
- What could go wrong? What's the simplest approach that actually works?

Write a short design doc covering the approach, the trade-offs you weighed, and the concrete list of files you'll change or create. Keep it proportional — a few paragraphs for a normal feature, not a treatise. Commit it as a `DESIGN.md` — *unless you're working on a beads issue*, in which case record it on the bead instead and do **not** also add a `DESIGN.md` (see "Working on a beads issue" below); one home for the design, not two.

For genuinely hard or open-ended design problems, escalate to the **design** skill, which explores the problem across several dimensions in parallel and synthesizes a design doc.

**Gate:** a design doc is committed covering approach, trade-offs, and the files to change. Don't implement until this exists — writing it down is what forces the thinking.

## Phase 2 — Implement

Write the code for the feature. Follow the design you just committed. Keep it simple. Don't gold-plate — build what the design says, not what you imagine someone might want later. Speculative generality is debt you take on with no proven need.

**Gate:** every file named in the design doc is created/modified and committed. If you find you need files the design didn't mention, that's a signal the design was incomplete — update it, don't silently drift.

For test-critical work — anything where a silent regression would be expensive, or where the spec is precise enough to pin down with tests — use the test-first variant instead of plain implement: see `references/tdd-cycle.md`. It commits failing tests *before* implementation so the tests can't later be quietly weakened to make a bad implementation pass.

For security-sensitive code (auth, crypto, input handling, anything touching secrets or untrusted data), wrap implement and submit with the security scans in `references/security-audit.md`.

## Phase 3 — Review

Review your own implementation before anyone else sees it. Check:

- Does it actually match the design? Where it diverged, was that deliberate and justified?
- Are there obvious bugs, unhandled edge cases, off-by-ones, nil/error paths?
- Is it readable and maintainable — would a new teammate understand it?
- Are there security concerns introduced by these changes?

**Escalation:** for a large or risky change (many files, core code paths, security surface), a single self-review pass isn't enough — invoke the **code-review** skill, which runs the diff through multiple specialized lenses in parallel and synthesizes a prioritized review.

**Gate:** self-review complete, no obvious bugs remain, code is readable and secure.

## Phase 4 — Test

Write and run tests. Unit tests for new logic, integration tests where behavior crosses boundaries. Run the *full* suite, not just your new tests — your change's blast radius includes code you didn't touch. Fix every regression you introduced.

**Gate:** all tests pass, no regressions, new code has meaningful test coverage (tests that would actually fail if the behavior broke — not assertions that can't fail).

## Phase 5 — Submit

Final check before merge:

- `git status` and `git diff` — know exactly what you're shipping. No stray debug code, no unrelated changes.
- Commit with a clear message explaining *why*, not just *what*.
- Push to the feature branch and follow the repo's landing workflow (PR, merge queue, etc.).

**Gate:** clean git status, clear commit message, code pushed to the feature branch.

## Working on a beads issue

This skill is often pointed at an existing beads issue ("work on `proj-7f` with shiny"). When the work corresponds to a bead, keep that bead's **notes** current as you go: a short note at each gate — design done, implemented, reviewed, tested, submitted — so the issue reflects reality for whoever looks next, and close it once the work is pushed. Recording the design in the bead also satisfies Phase 1's design gate without a separate `DESIGN.md`.

That's the only beads-specific behavior here. This skill doesn't manage beads itself — a separate skill covers `bd` usage — and doesn't require it: with no bead in play, follow the workflow normally and use the committed design-doc file for the Phase 1 gate.

## References

- `references/tdd-cycle.md` — red→green→refactor variant of Phase 2; commit failing tests before implementation. Read when the work is test-critical.
- `references/security-audit.md` — pre/post security scans wrapping Phase 2 and Phase 5. Read when the code is security-sensitive.
