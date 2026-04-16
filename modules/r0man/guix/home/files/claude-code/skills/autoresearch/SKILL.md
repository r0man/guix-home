---
name: autoresearch
description: >
  Create and refine autonomous research workflows where an AI agent iteratively
  experiments on code, measures results, and keeps or discards changes. Use this
  skill whenever the user wants to set up an autoresearch loop, create a
  program.md, design an autonomous experimentation workflow, optimize
  hyperparameters autonomously, run overnight experiments, or adapt an existing
  autoresearch setup to new metrics, hardware, or goals. Also trigger when the
  user mentions "autoresearch", "experiment loop", "autonomous optimization",
  "program.md", or wants an AI agent to try ideas and keep what works.
---

# Autoresearch

Autoresearch is a workflow pattern where an AI coding agent (Claude Code)
autonomously runs experiments in a loop: modify code, run it, measure results,
keep improvements, discard regressions, and repeat indefinitely. The human
"programs the researcher" by writing a `program.md` file that tells the agent
what to do, rather than doing the experiments manually.

This skill helps you create, refine, and adapt autoresearch workflows for any
project where improvement can be measured.

## When Autoresearch Works Well

The pattern fits any project where:

- There is a **measurable metric** (loss, latency, throughput, accuracy, binary
  size, test pass rate, benchmark score, etc.)
- Experiments are **fast enough** to run many times (ideally 1-10 minutes each)
- Changes are **contained** to a small surface area (one or a few files)
- Results are **deterministic enough** that improvements are real, not noise

Examples beyond ML training: compiler flag tuning, database query optimization,
algorithm selection, prompt engineering, configuration optimization, game AI
parameter tuning, image compression quality/size tradeoffs.

## Creating an Autoresearch Workflow

When a user wants to set up autoresearch for their project, walk through these
stages:

### 1. Understand the Project

Ask about (and investigate the codebase to answer where possible):

- **What to optimize**: What metric matters? Lower or higher is better?
- **How to measure**: What command produces the metric? How to extract it from
  output?
- **What to change**: Which file(s) should the agent modify? What's off-limits?
- **Time budget**: How long should each experiment take? (Default: 5 minutes)
- **Hardware constraints**: What GPU/CPU/RAM is available? This affects
  batch sizes, model sizes, and what's feasible per experiment.

### 2. Design the Evaluation

The evaluation must be:

- **Reproducible**: Same code should produce similar results across runs
- **Fast**: Fits within the time budget
- **Meaningful**: Actually correlates with the goal (not a proxy that can be
  gamed)
- **Extractable**: The agent can parse the metric from stdout/stderr via grep
  or similar

Help the user create or identify their evaluation script. If one doesn't exist,
help write it. The evaluation should print results in a parseable format:

```
metric_name: 0.1234
secondary_metric: 56.78
```

### 3. Set Up the File Structure

A minimal autoresearch project needs:

```
project/
  program.md          # Agent instructions (human writes/edits this)
  <run-script>        # The thing that runs and prints metrics
  <editable-files>    # What the agent is allowed to modify
  results.tsv         # Experiment log (agent creates and appends)
```

### 4. Write the program.md

This is the core deliverable. Structure it as follows:

```markdown
# Autonomous Research Program

## Goal
[One sentence: what are we optimizing and why]

## Setup
1. Create a git branch `autoresearch/<descriptive-tag>`
2. Read all source files to understand the codebase
3. Verify prerequisites (data, dependencies, hardware)
4. Run baseline experiment and record initial metrics
5. Initialize results.tsv with headers

## Files
- **Read-only**: [files the agent must not modify]
- **Editable**: [files the agent can modify]
- **Run command**: `<command that runs one experiment>`
- **Metric extraction**: `grep "^metric_name:" run.log`

## Experiment Loop
1. Think of an idea to try (informed by past results)
2. Modify only the editable files
3. `git add -A && git commit -m "<short description of change>"`
4. Run: `<run command> > run.log 2>&1`
5. Extract metrics: `grep "^metric_name:" run.log`
6. **Decision**:
   - If metric improved: keep the commit, update branch pointer
   - If metric same or worse: `git reset --hard HEAD~1`
   - If crashed: check if it's a simple bug (fix and retry) or a
     bad idea (discard and move on)
7. Log to results.tsv:
   `<commit-hash>\t<metric>\t<secondary-metrics>\t<kept|discarded|crashed>\t<description>`
8. Go to step 1. NEVER STOP.

## Constraints
- [Time budget per experiment]
- [Memory/hardware limits]
- [Dependencies that must not change]
- [Any domain-specific rules]

## Strategy Guidance
- [What kinds of changes are promising in this domain]
- [What has been tried before and didn't work]
- [Complexity budget: small improvements from simple changes are
  preferred over large improvements from fragile hacks]
```

### 5. Hardware-Aware Adaptation

Adapt the workflow to the user's hardware:

**GPU memory limited** (< 16GB):
- Reduce batch sizes, model dimensions, sequence lengths
- Suggest gradient checkpointing as a baseline change
- Shorter time budgets (2-3 min) to get more experiments in

**CPU only**:
- Much longer time budgets (10-30 min) or smaller problem sizes
- Focus on algorithmic changes rather than scale
- Consider using a subset of data for faster iteration

**Multi-GPU**:
- Can run larger experiments or parallelize evaluation
- Consider running multiple experiment branches simultaneously

**Cloud/remote**:
- Add checkpointing to survive preemption
- Log results to a persistent location
- Consider cost per experiment in the strategy

## Refining an Existing program.md

When the user has an existing autoresearch setup and wants to improve it:

### Diagnose Issues

Read the existing `program.md` and `results.tsv` (if available). Look for:

- **High crash rate** (>30%): The agent is trying changes that break things.
  Add more constraints or guardrails to the program.md.
- **No improvements after many experiments**: The search space may be exhausted,
  the metric may have plateaued, or the agent needs better strategy guidance.
  Suggest new directions or a different metric.
- **Agent stuck in a loop**: Repeating similar ideas. Add a "diversity" nudge
  or explicit "do not retry" list.
- **Improvements are noise**: The evaluation isn't stable enough. Increase eval
  data, add multiple runs, or use a more robust metric.
- **Agent modifying wrong files**: Tighten the file constraints.
- **Experiments too slow**: Reduce problem size, add early stopping, or use a
  faster proxy metric.

### Analyze Experiment History

If `results.tsv` exists, analyze it:

```bash
# Summary statistics
awk -F'\t' '{print $4}' results.tsv | sort | uniq -c | sort -rn
# Show kept experiments
grep 'kept' results.tsv
# Show progression of best metric
grep 'kept' results.tsv | awk -F'\t' '{print NR, $2}'
```

Help the user understand:
- What fraction of experiments succeeded vs crashed vs were discarded
- What kinds of changes produced the biggest improvements
- Whether the metric is still improving or has plateaued
- What the agent should try next

### Update the program.md

Based on the analysis, suggest specific changes:

- Add strategy hints based on what worked ("changes to the learning rate
  schedule have been most productive")
- Add constraints based on what failed ("do not modify the data loading
  pipeline, it causes OOM crashes")
- Adjust the time budget if experiments are consistently finishing early or
  timing out
- Update the complexity budget if the agent is making changes that are too
  complex or too trivial
- Add new metrics to track if the user wants to monitor additional aspects

## Tips for Effective program.md Files

**Be specific about the metric**. "Make it faster" is vague. "Reduce
`wall_clock_seconds` as printed by the benchmark script, while keeping
`accuracy` above 0.95" is actionable.

**Explain the domain**. The agent is smart but doesn't know your specific
codebase. A paragraph of context about what the code does and why certain
approaches might work goes a long way. Think of it as onboarding a new
researcher.

**Set a complexity budget**. Without this, agents tend toward increasingly
baroque solutions. Explicitly state that simple changes are preferred and that
complexity must be justified by proportional improvement.

**Include a "do not" list**. If there are changes that seem tempting but won't
work (or are dangerous), say so upfront. This saves experiment cycles.

**Version the program.md**. As you learn what works, update it. The program.md
is living documentation of your research strategy.

## Reference: results.tsv Format

Tab-separated with these columns:

| Column | Description |
|--------|-------------|
| commit | Git commit hash (short) |
| metric | Primary metric value |
| secondary | Other metrics (comma-separated or single value) |
| status | `kept`, `discarded`, or `crashed` |
| description | What the agent changed |

The agent creates this file with a header row during setup and appends one row
per experiment.

## Reference: Git Workflow

The agent works on a dedicated branch (`autoresearch/<tag>`) and uses git
as an undo mechanism:

- **Keep**: Commit stays, branch advances
- **Discard**: `git reset --hard HEAD~1` removes the failed attempt
- **Crash**: Same as discard, but logged differently for analysis

This means `git log` on the autoresearch branch shows only the successful
experiments — a clean chain of improvements.
