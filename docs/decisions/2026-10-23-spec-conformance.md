# Settled design is checked against the compiler, not assumed (2026-10-23)

**Decided by:** Philip, 2026-10-23 · **Status:** Decided
**Led to:** PLAN.md Next Up "Docs examples as tests" and "Spec conformance sweep"

## What prompted it
Bug 63: `let x = 1; x = 2` compiled. The January checker was written to refuse any write
rooted in a `let`, but the check sat on a path only the REPL used, so files were never
checked. The docs' own example (`x = 20  // Error`) had been wrong since it was written,
and stdlib code grew to rely on the gap. Philip: "this makes me worried about what of
design has been settled but isn't being enforce correctly? Do we need somesort of
specialized qa reviewer that understand the intents and checks for them being followed
correctly?"

## What we weighed
- **A specialised reviewer on each PR** — reviews read only the lines a branch changes,
  and the broken check had not been touched since January, so no review would have seen
  it. A whole-tree audit (`/qa-audit`, 2026-09-05) is what found it.
- **Run the docs' examples as tests** — every rule the docs show by example is checked on
  every test run, permanently. Cheap.
- **A one-time conformance sweep** of the rules the docs state in words, with the probe
  programs kept as tests, so a gap found once stays found.
- **A pipeline-wide "spec conformance" audit mode** for every project with a spec.

## What we decided, and why
The docs-as-tests item and the conformance sweep, both at the top of Next Up. A test keeps
catching a gap forever, while a reviewer finds it once. Whether to build the pipeline-wide
audit mode waits on what the sweep turns up.

## What it does not settle
Which backend's behaviour counts as right when they disagree with each other and the docs
are silent; each such case is its own decision.
