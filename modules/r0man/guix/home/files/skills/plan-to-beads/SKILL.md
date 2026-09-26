---
name: plan-to-beads
description: Convert a plan file into a beads epic plus sequential tasks for cross-session tracking. Use when asked to turn a plan into beads issues.
---

# Convert Plan to Beads Tasks

Input: an optional path to a plan file given by the user. Without one,
use the most recent plan (`ls -t ~/.claude/plans/*.md | head -1`).

If your agent supports subagents, delegate the conversion to one so the
bd output stays out of the main context. Otherwise do it directly.

## Steps

1. **Find the plan file**
   - If a path was given, use it
   - Otherwise: `ls -t ~/.claude/plans/*.md | head -1`

2. **Parse the plan structure**
   - Title: First `# Plan:` or `#` heading
   - Description: Content under `## Summary` or `## Context`
   - Tasks: Each `### Phase N:` or `### N.` section
   - File list: Include in epic description

3. **Create the epic**
   - Make sure all information from the plan is captured in the epic on a high level.
   ```bash
   bd create "[Plan Title]" -t epic -p 1 -d "[summary]. Files: N to modify." --json
   ```

4. **Create tasks from phases**
   - Each phase becomes a task
   - Use first paragraph of phase content as description
   - Make sure all information from the plan is captured by the beads issues in detail.
   ```bash
   bd create "[Phase title]" -t task -p 2 -d "[description]" --json
   ```

5. **Add sequential dependencies**
   - Phases are sequential: `bd dep add <phase2> <phase1>`

6. **Link tasks to epic**
   - `bd dep add <epic> <task>` for each task

7. **Return a concise summary** (not raw output):
   ```
   Created from: [filename]

   Epic: [title] ([epic-id])
     ├── [Phase 1] ([id]) - ready
     ├── [Phase 2] ([id]) - blocked by [prev]
     └── [Phase 3] ([id]) - blocked by [prev]

   Total: [N] tasks
   Run `bd ready` to start.
   ```

## Notes

- Original plan file is preserved for reference
- Task descriptions use first paragraph only (keeps them scannable)
- Sequential phases get automatic dependencies
