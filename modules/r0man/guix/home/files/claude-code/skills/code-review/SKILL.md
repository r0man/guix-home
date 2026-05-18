---
name: code-review
description: Comprehensive multi-lens code review with presets and a synthesized, prioritized verdict (correctness, performance, security, resilience, elegance, style, smells, wiring, commit discipline, test quality). Use this skill proactively whenever the user wants existing changes assessed — "review PR #142", "review the diff on branch feature/sso vs main", "go through my last 3 commits on the billing branch", "is this ready / mergeable?", "what's blocking the merge?", "do a thorough review of the changes in src/auth/*.ts before I open the PR", "security review of the file-upload handler change", "I think this refactor is behavior-preserving but I'm not sure — review it". Trigger even when the user just says "review this", "look this over", "any red flags?", or "is it safe to merge" without saying how, and whenever a PR, branch, diff, or set of changed files is the subject — strongly prefer this over a quick single-pass read. Do NOT use it to implement or fix code (use shiny), to critique a not-yet-built design or architecture proposal (use design), or to tidy/clean up code you just wrote (the simplify skill).
---

# Code Review — multi-lens convoy

A single read-through finds whatever the reader happened to be thinking about. Real review coverage comes from looking at the same diff through *several independent lenses*, then merging the findings. This skill runs that convoy and synthesizes one prioritized review.

## Step 1 — Resolve the target and scope

Figure out what's under review:

- **PR number** → `gh pr view <n>` and `gh pr diff <n>` for the diff and changed files.
- **Branch** → diff against the base branch (usually `main`): `git diff main...<branch>`.
- **File glob** → the matching files / their working-tree diff.

Note the scope: how many files, how many lines, what kind of code (core path? security surface? pure refactor?). Scope drives both the preset and the execution mode below.

## Step 2 — Pick a preset

Presets select which lenses run. Each lens is fully specified in
`references/review-lenses.md` — read that file before reviewing.

| Preset | Lenses | When |
|---|---|---|
| `gate` | wiring, security, smells, test-quality | Fast pre-merge gate; light automatic flow |
| `full` | correctness, performance, security, elegance, resilience, style, smells, wiring, commit-discipline, test-quality | Major feature; thorough review (default for large changes) |
| `security-focused` | security, resilience, correctness, wiring | Auth, crypto, input handling, sensitive changes |
| `refactor` | elegance, smells, style, commit-discipline | Reviewing a refactor (behavior unchanged) |
| custom | user-specified subset | User names the lenses they want |

If the user didn't specify, infer: small/quick ask → `gate`; "thorough"/large
feature → `full`; security-sensitive → `security-focused`; pure refactor →
`refactor`. State which preset you chose and why.

## Step 3 — Execute (adaptive)

Match effort to scope — running ten subagents on a three-line diff wastes the
user's time and tokens; reading a 4000-line diff single-threaded misses things.

- **Small scope** (a few files, modest diff) — run the selected lenses yourself,
  sequentially, in one pass. You hold the whole diff in context; spawning
  subagents would only add latency.
- **Large scope** (many files / large diff, or the `full` preset on a
  substantial change) — fan out: spawn one subagent per lens **in parallel**
  (one message, multiple Task calls), each given that lens's prompt from
  `references/review-lenses.md` plus the diff/target. Each subagent writes its
  findings to a file (e.g. `.reviews/<id>/<lens>-findings.md`). Parallelism is
  the point — the lenses are independent, so they should run concurrently.

Each lens produces findings structured as: Summary, Critical Issues (P0, must
fix before merge), Major Issues (P1, should fix), Minor Issues (P2, nice to
fix), Observations (non-blocking). Every issue carries a specific `file:line`
reference and a suggested fix. Findings without a location aren't actionable.

## Step 4 — Synthesize

Combine all lens findings into one review. Do not just concatenate them:

- **Deduplicate** — issues found by multiple lenses are reported once, noting
  which lenses flagged them (that's a strength signal, not redundancy).
- **Prioritize** — by impact then effort. P0s first, regardless of which lens
  found them.
- **Decide** — give an explicit merge recommendation.

Output (always use this structure):

```markdown
# Code Review: <target>

## Executive Summary
Overall assessment and explicit merge recommendation (merge / merge after P0s / do not merge).

## Critical Issues (P0 — must fix before merge)
- `file:line` — issue, impact, suggested fix. (lenses: …)

## Major Issues (P1 — should fix before merge)
- grouped by theme

## Minor Issues (P2 — nice to fix)
- briefly listed

## Wiring Gaps
Dependencies/config added but not actually used (from the wiring lens).

## Commit Quality
Notes on commit atomicity and messages.

## Test Quality
Whether the tests meaningfully verify behavior (coverage numbers lie).

## Positive Observations
What's done well — worth saying, and it calibrates the rest.

## Recommendations
Concrete, ordered next steps.
```

## Working on a beads issue

When the change under review is tracked by a beads issue (the user names one — "review `proj-7f`" — or the PR is bead-linked), record the review on that bead: put the verdict and the blocking items in its **notes** so the outcome lives with the work, not just in a chat reply.

That's the only beads-specific behavior here. This skill doesn't manage beads itself — a separate skill covers `bd` usage and any follow-up issue filing — and doesn't require it: with no bead in play, the Step 4 synthesized markdown is the deliverable.

## Reference

- `references/review-lenses.md` — the full prompt for each of the 10 lenses and
  the preset definitions. Read it before Step 3.
