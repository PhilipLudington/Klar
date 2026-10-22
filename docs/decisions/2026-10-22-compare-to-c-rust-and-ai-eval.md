# Compare Klar to other languages, for speed and for AI use (2026-10-22)

**Decided by:** Philip, 2026-10-22 · **Status:** Decided
**Led to:** PLAN.md Phase 1 task "Other-language comparison" and new Phase 2 "AI-use
comparison — Klar against C, Rust, Go, C#, Python and TypeScript"

## What prompted it
Reviewing the Goose language video (notes in `docs/design/goose-inspiration.md`), the
lesson was that speed claims without a fair, checkable comparison get dismissed. Philip:
*"Let's also plan on comparing to C and Rust. I also want some way of seeing which
language is faster for AI to use and results in a high quality code."* He then asked
whether to include C#, Zig, Python, Go or others.

## What we weighed
- **Which languages, for run speed.** C and Rust show the ceiling. Go and C# are Klar's
  real peers, since Klar is an application language "like C#/Go". Python is the floor.
  Zig was left out: it targets the same low-level work as C and would repeat that column.
  Swift was added on Philip's say-so: like Klar it manages memory by reference counting
  with no lifetime annotations, so it shows whether Klar's `Rc` costs are competitive.
  Other languages in the same space (Lobster, Vale, Hylo, Koka, Roc, Gleam, Mojo) were
  captured in `docs/design/goose-inspiration.md` as reading for after the tag.
- **Which languages, for AI use.** The same five plus TypeScript. Python and TypeScript
  are what AI writes most, so they are the bar for development speed. MoonBit and
  Nanolang, which make AI-first claims like Klar's, were noted as a possible later
  addition (young toolchains, not installed).
- **Run-speed comparison inside v0.6.0 or after it.** Inside: the trial party gets numbers
  against the languages it would otherwise use, at the cost of about two days before the
  tag. After: the release ships sooner but compares Klar only to itself. Chosen: inside.
- **AI-use comparison as a quick test or a measured study.** A few hand-run prompts would
  prove nothing to an outside reader. Chosen: a fixed method with hidden tests, repeats
  and a cost cap, as its own phase after the tag.

## What we decided, and why
The v0.6.0 benchmark page compares Klar with C, Rust, Go, C#, Swift and Python, and Phase 2
measures how quickly an AI gets to working code in Klar and in C, Rust, Go, C#, Python and
TypeScript, and how good that code is. Klar's claim is that it is the language AI writes
best, so it should be measured, not asserted. Python matters most for development speed,
in Philip's words: *"that's the main reason not to use a more performant language in many
situations."* The result that matters is the trade-off between development speed and the
quality and speed of the code.

## What it does not settle
The exact metrics, task count, repeat count, model and cost cap; those go in
`docs/design/ai-eval.md` for Philip to approve before any scored run, including which
language is dropped first if the cost cap is hit. Whether MoonBit or Nanolang join later.
