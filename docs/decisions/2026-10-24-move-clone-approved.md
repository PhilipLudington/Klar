# The explicit move/clone design is approved, receiver marker `(move b)` (2026-10-24)

**Decided by:** Philip, 2026-10-24 · **Status:** Decided
**Led to:** `docs/design/explicit-move-clone.md` status Approved; § 8 Q5 decided; PLAN.md Next Up line's WAITING ON PHILIP cleared

## What prompted it
Answering § 8 Q3 ("a by-value `self` receiver moves the variable") left the design
needing a way to write the marker on a method call. The revision (PR 84) proposed one as
Q5 and asked Philip to approve the revised design as a whole. He asked for "more context
and details about the design choice", got a plain-words walk-through, and replied
"Approve".

## What we weighed
- **`(move b).finish()` / `(clone b).finish()`**: the existing prefix operator,
  parenthesized. No new syntax (the parser already accepts a parenthesized expression as
  a method-call object), one meaning for `move`, only the first link of a builder chain
  marked. Cost: parentheses, but only on methods that really consume `self`, since
  § 7 step 2¾ converts read-only methods to `self: ref`.
- **`move b.finish()` with `move` binding to the receiver**: rejected. The operator would
  mean two things depending on what follows it.
- **A marker at the call (`b.finish(move)`, `b move.finish()`)**: rejected. New grammar
  for a case the operator already covers.

## What we decided, and why
The recommended spelling, and the revised design as a whole, including what Q3 commits
it to: step 2¾'s rewrite of read-only methods and the core traits (`Eq`, `Ordered`,
`Clone`, `Hash`, `Printable`) to `self: ref`, and Bug 136's receiver half landing after
it. Philip: "Approve".

## What it does not settle
- Each rollout step's details, which its own PR decides.
- Which by-value methods stay consuming; step 2¾'s PR names each one kept.
