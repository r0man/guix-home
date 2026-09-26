# Code review lenses

Each lens is an independent reviewer with one focus. Give the lens its
description below plus the diff/target. A lens writes findings as: Summary,
Critical Issues (P0), Major Issues (P1), Minor Issues (P2), Observations — every
issue with a specific `file:line` reference and a suggested fix.

## Contents

**Analysis lenses** (read and analyze the code)
- [correctness](#correctness) — logic errors, bugs, edge cases
- [performance](#performance) — bottlenecks, efficiency
- [security](#security) — vulnerabilities, OWASP
- [elegance](#elegance) — design clarity, abstraction quality
- [resilience](#resilience) — error handling, failure modes
- [style](#style) — convention compliance, consistency
- [smells](#smells) — anti-patterns, technical debt

**Verification lenses** (check implementation quality, not just code)
- [wiring](#wiring) — installed-but-not-wired gaps
- [commit-discipline](#commit-discipline) — commit quality and atomicity
- [test-quality](#test-quality) — test meaningfulness, not just coverage

[Presets](#presets) at the bottom.

---

## correctness
**Focus:** logical correctness and edge case handling.

Review the code for logical errors and edge case handling.

Look for:
- Logic errors and bugs
- Off-by-one errors
- Null/nil/undefined handling
- Unhandled edge cases
- Race conditions in concurrent code
- Dead code or unreachable branches
- Incorrect assumptions in comments vs code
- Integer overflow/underflow potential
- Floating point comparison issues

Questions to answer:
- Does the code do what it claims to do?
- What inputs could cause unexpected behavior?
- Are all code paths tested or obviously correct?

## performance
**Focus:** performance bottlenecks and efficiency.

Review the code for performance issues.

Look for:
- O(n²) or worse algorithms where O(n) is possible
- Unnecessary allocations in hot paths
- Missing caching opportunities
- N+1 query patterns (database or API)
- Blocking operations in async contexts
- Memory leaks or unbounded growth
- Excessive string concatenation
- Unoptimized regex or parsing

Questions to answer:
- What happens at 10x, 100x, 1000x scale?
- Are there obvious optimizations being missed?
- Is performance being traded for readability appropriately?

## security
**Focus:** security vulnerabilities and attack surface.

Review the code for security vulnerabilities.

Look for:
- Input validation gaps
- Authentication/authorization bypasses
- Injection vulnerabilities (SQL, XSS, command, LDAP)
- Sensitive data exposure (logs, errors, responses)
- Hardcoded secrets or credentials
- Insecure cryptographic usage
- Path traversal vulnerabilities
- SSRF (Server-Side Request Forgery)
- Deserialization vulnerabilities
- OWASP Top 10 concerns

Questions to answer:
- What can a malicious user do with this code?
- What data could be exposed if this fails?
- Are there defense-in-depth gaps?

## elegance
**Focus:** design clarity and abstraction quality.

Review the code for design quality.

Look for:
- Unclear abstractions or naming
- Functions doing too many things
- Missing or over-engineered abstractions
- Coupling that should be loose
- Dependencies that flow the wrong direction
- Unclear data flow or control flow
- Magic numbers/strings without explanation
- Inconsistent design patterns
- Violation of SOLID principles
- Reinventing existing utilities

Questions to answer:
- Would a new team member understand this?
- Does the structure match the problem domain?
- Is the complexity justified?

## resilience
**Focus:** error handling and failure modes.

Review the code for resilience and error handling.

Look for:
- Swallowed errors or empty catch blocks
- Missing error propagation
- Unclear error messages
- Insufficient retry/backoff logic
- Missing timeout handling
- Resource cleanup on failure (files, connections)
- Partial failure states
- Missing circuit breakers for external calls
- Unhelpful panic/crash behavior
- Recovery path gaps

Questions to answer:
- What happens when external services fail?
- Can the system recover from partial failures?
- Are errors actionable for operators?

## style
**Focus:** convention compliance and consistency.

Review the code for style and convention compliance.

Look for:
- Naming convention violations
- Formatting inconsistencies
- Import organization issues
- Comment quality (missing, outdated, or obvious)
- Documentation gaps for public APIs
- Inconsistent patterns within the codebase
- Lint/format violations
- Test naming and organization
- Log message quality and levels

Questions to answer:
- Does this match the rest of the codebase?
- Would the style guide approve?
- Is the code self-documenting where possible?

## smells
**Focus:** anti-patterns and technical debt.

Review the code for code smells and anti-patterns.

Look for:
- Long methods (>50 lines is suspicious)
- Deep nesting (>3 levels)
- Shotgun surgery patterns
- Feature envy
- Data clumps
- Primitive obsession
- Temporary fields
- Refused bequest
- Speculative generality
- God classes/functions
- Copy-paste code (DRY violations)
- TODO/FIXME accumulation

Questions to answer:
- What will cause pain during the next change?
- What would you refactor if you owned this code?
- Is technical debt being added or paid down?

---

## wiring
**Focus:** installed-but-not-wired gaps.

Detect dependencies, configs, or libraries that were added but not actually
used. This catches subtle bugs where the implementer THINKS they integrated
something, but the old implementation is still being used.

Look for:
- New dependency in manifest but never imported
  - Go: module in go.mod but no import
  - Rust: crate in Cargo.toml but no `use`
  - Node: package in package.json but no import/require
- SDK added but old implementation remains
  - Added Sentry but still using console.error for errors
  - Added Zod but still using manual typeof validation
- Config/env var defined but never loaded
  - New .env var that isn't accessed in code

Questions to answer:
- Is every new dependency actually used?
- Are there old patterns that should have been replaced?
- Is there dead config that suggests incomplete migration?

## commit-discipline
**Focus:** commit quality and atomicity.

Review commit history for good practices. Good commits make the codebase easier
to understand, bisect, and revert.

Look for:
- Giant "WIP" or "fix" commits
  - Multiple unrelated changes in one commit
  - Commits that touch 20+ files across different features
- Poor commit messages
  - "stuff", "update", "asdf", "fix"
  - No context about WHY the change was made
- Unatomic commits
  - Feature + refactor + bugfix in same commit
  - Should be separable logical units
- Missing type prefixes (if project uses conventional commits)
  - feat:, fix:, refactor:, test:, docs:, chore:

Questions to answer:
- Could this history be bisected effectively?
- Would a reviewer understand the progression?
- Are commits atomic (one logical change each)?

## test-quality
**Focus:** test meaningfulness, not just coverage.

Verify tests are actually testing something meaningful. Coverage numbers lie. A
test that can't fail provides no value.

Look for:
- Weak assertions
  - Only checking != nil / !== null / is not None
  - Using .is_ok() without checking the value
  - assertTrue(true) or equivalent
- Missing negative test cases
  - Happy path only, no error cases
  - No boundary testing
  - No invalid input testing
- Tests that can't fail
  - Mocked so heavily the test is meaningless
  - Testing implementation details, not behavior
- Flaky test indicators
  - Sleep/delay in tests
  - Time-dependent assertions

Questions to answer:
- Do these tests actually verify behavior?
- Would a bug in the implementation cause a test failure?
- Are edge cases and error paths tested?

---

## Presets

| Preset | Lenses |
|---|---|
| `gate` | wiring, security, smells, test-quality — light review for automatic flow; fast, focused on blockers |
| `full` | correctness, performance, security, elegance, resilience, style, smells, wiring, commit-discipline, test-quality — comprehensive, for major features |
| `security-focused` | security, resilience, correctness, wiring — security-heavy review for sensitive changes |
| `refactor` | elegance, smells, style, commit-discipline — quality review during refactoring |
| custom | a user-specified subset of the lenses above |
