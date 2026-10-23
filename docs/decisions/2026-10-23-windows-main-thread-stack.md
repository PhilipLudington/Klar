# Windows Klar programs get an 8M main-thread stack (2026-10-23)

**Decided by:** Philip, 2026-10-23 · **Status:** Decided
**Led to:** PLAN.md Next Up "Nightly CI red at 25bfe92 (Windows (full suite))"; Debt 57

## What prompted it
The nightly CI run on 2026-10-23 failed on Windows only. The selfhost type checker,
compiled by Klar, crashed with no output while checking `test/native/many_params.kl`.
The crash was the program running out of stack. Since the Bug 5 fix (f7cccf6), which
reserves every local variable's space when a function starts instead of when the code
reaches it, each function call takes more stack: about 128K per level of nested `+` in
the checker, up from about 80K. An 8-term sum now needs about 1M. Windows gives a
program's main thread 1M of stack, while macOS and Linux give 8M, so only Windows ran out.

## What we weighed
- **Give Windows programs an 8M stack when they are linked.** Small and contained, and
  it makes Windows match macOS and Linux, in line with "no surprises". It does not undo
  the extra stack use, which every platform still pays.
- **Make functions use less stack again**, by telling LLVM when each local's life starts
  and ends so it can reuse the same space (`llvm.lifetime` markers). This fixes the cause,
  but it is a bigger compiler change, and it only helps optimised builds.

## What we decided, and why
The 8M Windows stack is the fix, and the lifetime markers are written down as owed work
(Debt 57). Claude recommended this, and Philip agreed: *"Let's follow your
recommendation."* A program should not crash on one platform when it runs on the others,
and the stack-size difference was the only reason it did.

## What it does not settle
Programs still use more stack than they did before Bug 5, on every platform. If a
program overflows the 8M stack anywhere, or the compiler code that reserves stack space
is opened again, Debt 57 comes due. The exact linker option and how it is tested belong
to the fix itself.
