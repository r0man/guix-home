---
name: shiny-enterprise
description: Enterprise-grade engineering workflow — the shiny discipline (design → implement → review → test → submit) with the implementation phase expanded into the Rule of Five iterative refinement. Use this skill whenever the work is high-stakes, critical, or production-load-bearing and the user wants the absolute best result over speed: phrases like "do the best possible job", "this is critical/important, get it right", "iterate until it's excellent", "enterprise-grade", "polish this until it shines", or any change where correctness and quality clearly outweigh turnaround time. Also trigger when the user explicitly asks for the "rule of five" or iterative-refinement passes. Prefer the plain shiny skill for ordinary careful work where a single solid implementation pass is enough.
---

# Shiny Enterprise — Shiny + Rule of Five

Same disciplined spine as the **shiny** skill — design before you code, review before you ship, test before you submit — but the Implement phase is not a single pass. It's the **Rule of Five**: a draft followed by four focused refinement passes.

The reason: LLM agents (Jeffrey Emanuel's observation, which this formalizes) produce their best work not in one shot but through four to five iterative refinements — breadth-first to get the shape, then successive editorial passes each with a single focus. A first attempt that tries to be simultaneously correct, clear, complete, and excellent ends up mediocre at all four. Separating the concerns into ordered passes lets each one go deep without the others diluting it. Use this when the cost of "good enough" is high enough to justify the extra passes.

Walk the phases in order, clearing each gate before the next.

## Phase 1 — Design

Identical to shiny's design phase: think about how this fits the existing
system, edge cases, failure modes, the simplest approach that works. Reuse
existing patterns and utilities. Commit a short design doc (approach,
trade-offs, files to change).

For genuinely hard design problems, the Rule of Five applies here too — and the
**design** skill explores the problem across several dimensions in parallel and
synthesizes a design doc. Use it when the design itself is the hard part.

**Gate:** design doc committed covering approach, trade-offs, and files to change.

## Phase 2 — Implement (Rule of Five)

Five ordered passes over the implementation. Each pass has one job. Commit
between passes so the progression is reviewable and revertible.

1. **Draft** — initial attempt. Don't aim for perfection. Get the shape right.
   Breadth over depth: cover the whole surface roughly rather than perfecting
   one corner. This pass exists to make the problem concrete, not to finish it.

2. **Refine 1 — Correctness.** Fix errors, bugs, mistakes. Is the logic sound?
   Walk the real and edge-case inputs. Nothing else matters if it's wrong, so
   this comes first.

3. **Refine 2 — Clarity.** Can someone else understand this? Simplify. Remove
   jargon and incidental complexity. Improve naming and structure. Correct-but-
   unreadable code is a future bug, so clarity is its own pass.

4. **Refine 3 — Edge cases.** What could go wrong? What's missing? Handle the
   unusual inputs, the failure paths, the boundaries — the cases the draft
   skipped because it was chasing the happy path.

5. **Refine 4 — Excellence.** The last pass. Make it shine. Is this something
   you'd be proud to ship and put your name on? Polish the rough edges that are
   individually minor but collectively the difference between adequate and
   excellent.

Don't collapse the passes into one "I'll just do it well" pass — the ordering is
the technique. Don't pad them either: if a pass genuinely finds nothing to
improve, say so briefly and move on rather than inventing churn.

For test-critical work, the Rule of Five composes with test-first development —
write and commit the tests before the Draft, then keep them green through every
refinement pass (they make the refinement safe). For security-sensitive code,
run a security scan after the Draft and again before Submit.

**Gate:** all five passes done; every file from the design doc is created/
modified and committed.

## Phase 3 — Review

Self-review the final implementation against the design: bugs, readability,
security, and whether the divergences from the design were deliberate. For a
large or risky change, escalate to the **code-review** skill, which runs the
diff through multiple specialized lenses in parallel and synthesizes a
prioritized review — enterprise work usually warrants this.

**Gate:** review complete, no obvious bugs, code readable and secure.

## Phase 4 — Test

Write and run tests; run the *full* suite; fix every regression. New code needs
meaningful coverage — tests that would actually fail if the behavior broke.

**Gate:** all tests pass, no regressions, meaningful coverage on new code.

## Phase 5 — Submit

`git status` / `git diff` (know exactly what ships, no stray changes), clear
commit message explaining *why*, push to the feature branch, follow the repo's
landing workflow.

**Gate:** clean git status, clear commit message, code pushed.

## Working on a beads issue

This skill is often pointed at an existing beads issue ("work on `proj-7f` with shiny-enterprise"). When the work corresponds to a bead, keep that bead's **notes** current as you go: a short note at each gate — design done, then one line per Rule-of-Five pass as you complete it, reviewed, tested, submitted — so the issue reflects reality for whoever looks next, and close it once the work is pushed. The five passes are progress notes on the *one* bead, not five issues. Recording the design in the bead also satisfies the Phase 1 design gate without a separate `DESIGN.md`.

That's the only beads-specific behavior here. This skill doesn't manage beads itself — a separate skill covers `bd` usage — and doesn't require it: with no bead in play, follow the workflow normally and use the committed design-doc file for the Phase 1 gate.
