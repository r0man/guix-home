# TDD cycle — test-first variant of the Implement phase

Use this instead of plain "implement" when the work is test-critical: a silent
regression would be expensive, or the spec is precise enough to pin down with
tests. It replaces Phase 2 with a five-step red→green→refactor cycle.

The defining rule: **commit the tests before you write any implementation.**
Tests locked into version control before the implementation exists cannot later
be silently weakened to make a bad implementation pass. That commit-first
pattern is the entire defense against test manipulation — without it, "the tests
pass" means nothing, because the tests and the code were written together to
agree with each other.

## Step 1 — Write failing tests

Read the requirements and acceptance criteria. Identify the testable behaviors.
Write tests that assert the expected outcomes.

Then **commit the tests before writing any implementation code**:

```
git add <test files>
git commit -m "test: add failing tests for <target>"
```

Detect the right test command for the affected code (e.g. `go test ./...`,
`npx jest` in a TS service, `poetry run pytest` in a Python service). Do **not**
write any implementation in this step.

**Acceptance:** failing tests committed in a separate commit before any
implementation code.

## Step 2 — Verify red

Run the suite. The tests you just wrote **must fail**.

Hard gate: if everything passes, your tests aren't testing new behavior — they
assert things that are already true. Rewrite them to actually exercise the
unimplemented functionality. Do not proceed until you see genuine failures, and
confirm they fail for the *expected* reason (missing implementation) — not a
broken test harness — and that no pre-existing tests are broken.

**Acceptance:** the suite ran and produced at least one failure for the right
reason.

## Step 3 — Implement to green

Write the minimal code to make all tests pass. Rules:

- Do **not** modify the test files from Step 1.
- Follow existing codebase conventions.
- Keep it minimal — just enough to pass the tests, no gold-plating.
- Commit: `git add <impl files> && git commit -m "feat: implement <target>"`.

**Acceptance:** implementation committed; the Step 1 test files are unchanged.

## Step 4 — Verify green

Run the *full* suite. Every test must pass — both the new tests and all
pre-existing ones.

Hard gate: if any test fails, fix the *implementation*, not the tests. Do not
proceed to refactor until the suite is fully green with no regressions and the
new tests pass for the right reasons.

**Acceptance:** full suite passes with zero failures.

## Step 5 — Refactor

Clean up the implementation: improve naming, reduce duplication, simplify logic.
After refactoring, run the full suite again to confirm nothing broke.

If no meaningful refactor is needed, skip it — don't refactor for its own sake.
If you changed things: `git commit -m "refactor: clean up <target>"`.

**Acceptance:** code cleaned up and suite still green, or no refactor needed.

After the cycle completes, return to Phase 3 (Review) of the shiny workflow.
