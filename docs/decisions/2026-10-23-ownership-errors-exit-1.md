# A failed ownership check makes `klar check` exit 1, like every other compile failure (2026-10-23)

**Decided by:** Philip, 2026-10-23 · **Status:** Decided
**Led to:** Bug 49 and Bug 77 fixed on `fix/bug-49-exit-status`; Bug 132 filed

## What prompted it
Bug 49: `klar check` and `klar build` printed their errors and still exited 0, so no script
or CI job could tell a failed compile from a good one. The fix makes every failure exit 1.
Once a failed ownership check exited 1 too, the selfhost test suite went red: four of the
compiler's own Klar sources (`checker_decl`, `checker`, `module_resolver`, `parser`) fail
the ownership check with 127, 12, 12 and 3 errors. They are not real errors. The checker
counts a value as moved after a branch that already returned, and after a loop that
reassigns the name (Bug 132). Before the fix nobody saw them, because the exit code was
always 0.

## What we weighed
- **A: exit 1 on ownership errors, and fix the false checks first.** The right end state,
  but it holds a release item behind a change to the ownership checker of unknown size.
- **B: exit 1 on parse, type and file errors, and keep exit 0 for ownership errors until the
  false checks are fixed.** It lands now, but it keeps a wrong exit 0 for one kind of
  failure, the very thing Bug 49 is about.
- **C: fix the false checks on the same branch.** It mixes two unrelated fixes in one
  change.
- **D (chosen): exit 1 for every compile failure, ownership included, and mark the four
  selfhost files as known failures in the test suite, pointing at Bug 132.**

## What we decided, and why
D. Exiting 0 when compilation fails breaks the exit-code convention, and that convention is
the point of Bug 49. Option B would have kept a wrong exit 0 for one kind of error. D keeps
the exit code right everywhere and confines the workaround to the test suite. A marked file
must still fail with ownership errors and nothing else. When it starts passing, the suite
fails and says to remove the mark, so a fix to Bug 132 cannot slip by unnoticed.

## What it does not settle
- Fixing the false checks (Bug 132). The marks come off when it lands.
- Exit codes outside `check`, `build` and a failed build under `run`: other commands'
  argument errors still exit 0.
