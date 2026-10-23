# Bug 132 stays out of the v0.6.0 cut set and is listed as a known issue (2026-10-23)

**Decided by:** Philip, 2026-10-23 · **Status:** Decided
**Led to:** PLAN.md Phase 1 Known issues task names Bug 132

## What prompted it
Option D (`2026-10-23-ownership-errors-exit-1.md`) made a failed ownership check exit 1.
A side agent noted that this reaches users too: "Any Klar program with the common 'return
early' pattern can now fail `klar check`, even when the code is correct." The ownership
checker's false use-after-move reports (Bug 132) used to print with exit 0, and now they
also fail the check.

## What we weighed
- **1: leave Bug 132 out of the cut set and list it in the release's Known issues.** The
  tag doesn't wait. The trial builds natively with `-O2`, and `klar build` and `klar run`
  skip the ownership pass, so the trial only meets the bug if it runs `klar check`.
- **2: add Bug 132 to the cut set.** The release waits on an ownership-checker fix of
  unknown size.

## What was decided, and why
Option 1. By the plan's own cut-set test, Bug 132 doesn't affect the native build the trial
runs. The false errors already printed before option D; only the exit code changed.

## What it does not settle
- If the trial project will run `klar check` (in an editor or a CI step), revisit this and
  add Bug 132 to the cut set.
- When Bug 132 itself is fixed. It stays deferred until after the milestone.
