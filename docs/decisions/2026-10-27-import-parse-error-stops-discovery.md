# A parse error in an imported module stops module discovery (2026-10-27)

**Decided by:** Philip, 2026-10-27 · **Status:** Decided
**Led to:** Bug 51 (PLAN.md, v0.6.0 cut set)

## What prompted it
Bug 51 was filed because a parse error in an imported module was skipped and the build
reported success. When the Builder came to fix it on 2026-10-27, the bug no longer
happened as filed: since Bug 49, the skipped module turns into a type error, "module
'<name>' not found", and `check`, `build` and `run` all exit 1. The question was whether
that was good enough.

## What we weighed
- **Close Bug 51 as fixed by Bug 49, keep a regression test** — the exit code is right,
  but the user sees a second, wrong error: the module exists, it just failed to parse.
- **Still make discovery stop at the parse error** — the user sees only the real error.
  Chosen.

## What we decided, and why
Module discovery treats an imported module's parse error as fatal, as the test runner
already does, so the misleading "module not found" line never prints. Under
`check --partial` the parse error is a diagnostic like the entry file's and the run exits 1
at the end. Philip chose this option ("1) b"), the one the Builder recommended. It follows
"No surprises": an error message should name the real cause.

## What it does not settle
Nothing open.
