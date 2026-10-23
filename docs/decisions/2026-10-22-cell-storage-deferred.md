# Cell storage stays on the stack until after v0.6.0 (2026-10-22)

**Decided by:** Philip, 2026-10-22 · **Status:** Decided
**Led to:** Bug 129 widened and kept deferred; PR 74 merged with Cell as its one exception

## What prompted it
PR 74 (Bug 5) moved every temporary stack slot in native code to the top of its function,
so loops stop growing the stack. Cells could not move with them: a Cell *is* the address
of its slot, so one shared slot made every Cell created in a loop the same Cell. A side
review of PR 74 put it this way: *"So a loop that makes very many Cells can still crash …
Decide whether Cells should get proper heap storage before you call Bug 5 fully closed."*

## What we weighed
- **Widen Bug 129 and defer it until after v0.6.0** — Bug 129 already covered a Cell
  returned from the function that made it, which has the same cure: keep Cell values on
  the heap, with code that frees them. One later branch fixes both.
- **Do the heap change next, before v0.6.0** — closes both problems now, but needs a way
  to free a Cell that does not exist yet, which is design work ahead of the release.
- **Leave Bug 129 as it was** — the loop crash would be written down only in a review
  report, where nobody would find it.

## What we decided, and why
The first option (Philip answered "1"). PR 74 makes nothing worse: Cells behave exactly as
they did before it, and every other stack slot is fixed. A loop of 2,000,000 `Cell.new`
calls built natively still crashes (checked 2026-10-22), and Bug 129 now says so.

## What it does not settle
How a heap-stored Cell is freed: Cell is not registered as droppable today (`emit.zig`,
around line 2700). Nor whether Cell gets reference counting.
