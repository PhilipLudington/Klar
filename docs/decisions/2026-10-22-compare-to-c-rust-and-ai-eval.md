# Compare Klar to C and Rust, for speed and for AI use (2026-10-22)

**Decided by:** Philip, 2026-10-22 · **Status:** Decided
**Led to:** PLAN.md Phase 1 task "C and Rust comparison" and new Phase 2 "AI-use
comparison — Klar against C and Rust"

## What prompted it
Reviewing the Goose language video (notes in `docs/design/goose-inspiration.md`), the
lesson was that speed claims without a fair, checkable comparison get dismissed. Philip:
*"Let's also plan on comparing to C and Rust. I also want some way of seeing which
language is faster for AI to use and results in a high quality code."*

## What we weighed
- **Runtime comparison inside v0.6.0 or after it.** Inside: the trial party gets numbers
  against languages they know, at the cost of roughly a day's work before the tag. After:
  the release ships sooner, but its benchmark page compares Klar only to itself (VM
  against native). Placed inside v0.6.0 on Claude's recommendation, since the benchmark
  page is the trial's reference either way; Philip has not ruled on the placement.
- **AI-use comparison as a quick test or a measured study.** A few hand-run prompts would
  be cheap, but would prove nothing to an outside reader. A fixed method with hidden tests,
  repeats and a cost cap is slower but gives numbers that hold up. Chosen: the measured
  study, as its own phase after the tag.

## What we decided, and why
Compare Klar's runtime benchmarks to C and Rust in the v0.6.0 baseline, and build a
repeatable study of how quickly an AI gets to working code in each language and how good
that code is. Klar's whole claim is that it is the language AI writes best, so it should
be measured, not asserted.

## What it does not settle
The exact metrics, task count, repeat count, model and cost cap; those go in
`docs/design/ai-eval.md` for Philip to approve before any scored run. Whether results are
published if Klar does badly: the plan says they are, but the method document is where
that gets confirmed.
