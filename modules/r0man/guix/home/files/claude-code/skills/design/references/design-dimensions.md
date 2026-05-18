# Design dimensions

Each dimension is an independent analyst examining the problem from one angle.
Give the analyst its description below plus the problem statement, context, and
scope. Each produces: Summary, Key Considerations, Options Explored (each with
description / pros / cons / effort = Low|Medium|High), a Recommendation,
Constraints Identified, Open Questions, and Integration Points. Be thorough but
actionable; flag decisions needing human input.

## Contents

- [api](#api) — interface design and developer ergonomics
- [data](#data) — data model, storage, migrations
- [ux](#ux) — user experience and CLI ergonomics
- [scale](#scale) — performance at scale and bottlenecks
- [security](#security) — threat model and attack surface
- [integration](#integration) — fit with the existing system
- [synthesis](#synthesis) — combining the dimensions

---

## api
**Focus:** interface design and developer ergonomics.

Analyze the interface design for this feature.

Explore:
- Command-line interface: flags, subcommands, ergonomics
- Programmatic API: function signatures, return types
- Configuration interface: files, environment variables
- Error messages and help text
- Naming conventions and discoverability
- Consistency with existing interfaces

Questions to answer:
- How will users discover and learn this feature?
- What's the happy path vs edge cases?
- Does it follow existing CLI/API patterns?
- What would make this a joy to use?

Deliverable: interface proposals.

## data
**Focus:** data model, storage, and migrations.

Analyze the data model requirements for this feature.

Explore:
- Data structures: types, relationships, constraints
- Storage format: JSON, TOML, SQLite, in-memory
- Schema design: fields, indices, normalization
- Migration strategy: versioning, backwards compatibility
- Data lifecycle: creation, updates, deletion
- Persistence vs ephemeral considerations

Questions to answer:
- What data needs to persist vs be computed?
- How will the data grow over time?
- What queries/access patterns are needed?
- How do we handle schema evolution?

Deliverable: schema proposals.

## ux
**Focus:** user experience and CLI ergonomics.

Analyze the user experience implications of this feature.

Explore:
- Mental model: how users think about this
- Workflow integration: where does this fit in daily use?
- Learning curve: progressive disclosure
- Error experience: what happens when things go wrong?
- Feedback: how does the user know it's working?
- Discoverability: --help, docs, examples

Questions to answer:
- What's the user's goal when using this?
- What's the minimum viable interaction?
- How do we handle power users vs beginners?
- What would surprise or confuse users?

Deliverable: UX recommendations.

## scale
**Focus:** performance at scale and bottlenecks.

Analyze the scalability implications of this feature.

Explore:
- Scale dimensions: data size, request rate, user count
- Resource usage: memory, CPU, disk, network
- Bottlenecks: what limits growth?
- Complexity: algorithmic, space, time
- Caching opportunities
- Degradation modes: what happens at limits?

Questions to answer:
- What happens at 10x, 100x, 1000x current scale?
- What are the hard limits?
- Where should we optimize vs keep simple?
- What needs to be lazy vs eager?

Deliverable: performance analysis.

## security
**Focus:** threat model and attack surface.

Analyze the security implications of this feature.

Explore:
- Trust boundaries: what trusts what?
- Attack surface: new inputs, outputs, permissions
- Threat model: who might attack this and how?
- Sensitive data: what's exposed or stored?
- Authentication/authorization implications
- Failure modes: what if security fails?

Questions to answer:
- What's the worst case if this is exploited?
- What new permissions or access does this need?
- How do we validate/sanitize inputs?
- Are there defense-in-depth opportunities?

Deliverable: threat analysis.

## integration
**Focus:** how it fits the existing system.

Analyze how this feature integrates with the existing system.

Explore:
- Existing components: what does this touch?
- Dependencies: what does this need from others?
- Dependents: what will depend on this?
- Migration path: how do we get from here to there?
- Backwards compatibility: what might break?
- Testing strategy: how do we verify integration?

Questions to answer:
- Where does this code live?
- How does it affect existing workflows?
- What needs to change in dependent code?
- Can we feature-flag or gradually roll out?

Deliverable: integration plan.

---

## synthesis

Combine all dimension analyses into a unified design document.

Identify conflicts between dimensions and resolve or escalate each one
explicitly — that reconciliation is the value of synthesis. Flag decisions
needing human input; don't bury them. Be concrete and actionable.

Structure:

```markdown
# Design: <problem>

## Executive Summary
2–3 paragraph overview of the proposed design.

## Problem Statement
Clear statement of what we're solving.

## Proposed Design
### Overview
### Key Components
### Interface          (from api)
### Data Model         (from data)

## Trade-offs and Decisions
### Decisions Made
### Open Questions      (highlight — needs human input)
### Trade-offs

## Risks and Mitigations  (from security and scale)

## Implementation Plan    (from integration)
### Phase 1: MVP
### Phase 2: Polish
### Phase 3: Future

## Appendix: Dimension Analyses
```
