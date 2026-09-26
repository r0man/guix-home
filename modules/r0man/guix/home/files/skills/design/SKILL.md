---
name: design
description: Structured multi-dimension design exploration that produces a synthesized design doc with options, trade-offs, risks, and a phased plan. Use this skill when you want something thought through before it is built - for API design, data modeling, UX, scalability, security, and integration concerns. Explores problems across independent dimensions and reconciles them.
---

# Design — multi-dimension exploration

A design proposed from one angle is a design with blind spots. The API looks clean until the data model can't support it; the data model is elegant until it doesn't scale; it scales until the security model breaks. Real designs come from examining the problem across independent dimensions and then reconciling them. This skill runs that exploration and synthesizes one design doc.

## Step 1 — Frame the problem

Capture three things explicitly before exploring:

- **Problem statement** — what are we actually solving? Restate it crisply; a vague problem yields a vague design.
- **Context** — existing code, constraints, prior decisions the design must respect.
- **Scope** — `small` (≈1 file), `medium` (a package), or `large` (a system). Ask the user if it's unclear; scope drives execution mode below.

## Step 2 — Explore the dimensions (adaptive)

Six dimensions, each fully specified in `references/design-dimensions.md` (read
it before exploring):

- **api** — interface design, ergonomics, developer experience
- **data** — data model, storage, schema, migrations
- **ux** — user experience, CLI ergonomics, discoverability
- **scale** — performance at scale, bottlenecks, limits
- **security** — threat model, attack surface, trust boundaries
- **integration** — fit with the existing system, compatibility, rollout

Match effort to scope:

- **small** — work the dimensions yourself in one pass. Not every dimension is
  load-bearing for a one-file change; cover the relevant ones and note which you
  judged not applicable and why.
- **medium / large** — fan out: spawn one subagent per relevant dimension **in
  parallel** (one message, multiple Task calls), each given that dimension's
  prompt from the reference plus the problem/context. Each writes its analysis
  to a file (e.g. `.designs/<id>/<dimension>.md`). The dimensions are
  independent analyses, so they run concurrently.

Each dimension produces: Summary, Key Considerations, Options Explored (each
with description / pros / cons / effort), a Recommendation, Constraints
Identified, Open Questions, and Integration Points. Flag decisions that need
human input rather than silently picking.

## Step 3 — Synthesize

Combine the dimension analyses into one design document. The job here is
reconciliation, not concatenation: surface where dimensions *conflict* (the
API the UX wants vs. the one the data model affords) and resolve or escalate
each conflict explicitly.

Output (always use this structure):

```markdown
# Design: <problem>

## Executive Summary
2–3 paragraph overview of the proposed design.

## Problem Statement
Clear statement of what we're solving.

## Proposed Design
### Overview
High-level approach.
### Key Components
Main pieces and how they fit together.
### Interface
CLI/API summary (from the api dimension).
### Data Model
Schema summary (from the data dimension).

## Trade-offs and Decisions
### Decisions Made
Key choices and their rationale.
### Open Questions
Decisions needing human input — highlight these, don't bury them.
### Trade-offs
What we're trading off and why.

## Risks and Mitigations
From the security and scale dimensions.

## Implementation Plan
From the integration dimension.
### Phase 1: MVP
### Phase 2: Polish
### Phase 3: Future

## Appendix: Dimension Analyses
Links to the full per-dimension documents (large scope) or brief inline
summaries (small scope).
```

Write the synthesized doc to `.designs/<id>/design-doc.md` for medium/large
scope, or a single `design-doc.md` for small scope. Be concrete and actionable —
a design doc that doesn't let someone start building is unfinished.

## Working on a beads issue

When the design work is tracked by a beads issue (the user names one — "design `proj-7f`"), record the resulting design and the key decisions in that bead's **notes** so the design lives with the work, not just in a loose doc.

That's the only beads-specific behavior here. This skill doesn't manage beads itself — a separate skill covers `bd` usage and any breakdown of the design into an epic/tasks — and doesn't require it: with no bead in play, the Step 3 synthesized design doc is the deliverable.

## Reference

- `references/design-dimensions.md` — the full prompt for each of the 6
  dimensions and the synthesis template. Read it before Step 2.
