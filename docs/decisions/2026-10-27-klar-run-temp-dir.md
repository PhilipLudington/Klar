# `klar run` puts its private temp folder under the user's own temp directory (2026-10-27)

**Decided by:** Philip, 2026-10-27 · **Status:** Decided
**Led to:** Bug 56 (PLAN.md, v0.6.0 cut set)

## What prompted it
Bug 56: `klar run` writes its compiled program to a guessable `/tmp/klar-run-<seconds>`,
where another user on the same machine could plant or swap the file first. The fix makes
a unique folder only the user can open. The plan review on 2026-10-27 asked two things the
plan did not say: where that folder goes on macOS and Linux, and what "only the user can
open it" means on Windows, which has no 0700 folder mode.

## What we weighed
- **macOS/Linux, under `$TMPDIR` with `/tmp` as fallback** — `$TMPDIR` is already a
  private per-user folder on macOS, so the program never sits in a shared place. Chosen.
- **macOS/Linux, always `/tmp`** — what happens today; relies only on the 0700 folder.
- **Windows, a uniquely named folder under `%TEMP%`** — `%TEMP%` sits in the user's own
  profile, which other users cannot read by default. Chosen.
- **Windows, plus an explicit owner-only permission list** — more code for protection the
  profile folder already gives.

## What we decided, and why
On macOS and Linux the folder goes under `$TMPDIR` when it is set and non-empty, otherwise
`/tmp`, and is made 0700. On Windows a uniquely named folder under `%TEMP%` is enough.
Both follow the operating system's own idea of a private temp place rather than adding
protection on top of it. Philip answered "2) a 3) a" to the plan review's options.

## What it does not settle
- The `.o` file next to the binary can also be overwritten; that is outside Bug 56 and not
  yet filed.
- A Windows setup where `%TEMP%` points somewhere shared would reopen question 3.
