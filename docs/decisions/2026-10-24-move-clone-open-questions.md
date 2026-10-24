# The move/clone design's four open questions, answered (2026-10-24)

**Decided by:** Philip, 2026-10-24 · **Status:** Decided
**Led to:** revision of `docs/design/explicit-move-clone.md` § 8; Bug 45 resolution direction

## What prompted it
The explicit move/clone design (PR 83) left four questions for Philip, each with a
recommendation. He asked to be walked through each "with some commentary so I better
understand the context and the choice."

## What we weighed
- **Q1, is the lowercase `string` freely copyable?** Copy: no markers, and since a `string`
  can't be changed in place, two names sharing its text can never surprise anyone (the
  reason Go, C# and Python get away with it). Owned: one rule for all text, but markers
  that guard nothing, on a type used everywhere. Copy's cost is that a `string` can then
  own no memory, so text built at run time must come back as an owned `String` (Bug 45).
- **Q2, does `return s` need `move`?** No marker: nothing can use `s` after the return, so
  copy and move look the same. Always a marker: no exception to remember, but noise at
  the end of nearly every function that builds a value.
- **Q3, does calling a method move the variable it's called on?** Leave it alive: keeps the
  design small, but leaves a hole where a method that keeps `self` shares one value
  between two names. Move it: no hole, but 167 plain-`self` methods must first be
  rewritten to borrow, and the marker needs a spelling on a receiver.
- **Q4, may your own structs be freely copyable?** Opt in with `impl X: Copy` when every
  field is copyable, or never.

## What we decided, and why
Q1: `string` is freely copyable. Q2: returning a whole variable needs no marker; returning
a field does. Q3: a method taking `self` by value moves the variable, against the
recommendation, so the ownership rule has no exception at method calls. Q4: a struct may
opt in to being copyable with `impl X: Copy`, only when all its fields are copyable;
with Q3's answer, this is what keeps small number-only structs free of markers.

## What it does not settle
- How the marker is spelled on a method receiver (Q3's consequence); the revised design
  proposes one for approval.
- Approval of the design as a whole, which waits on that revision.
