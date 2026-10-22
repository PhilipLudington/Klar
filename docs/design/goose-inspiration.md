# Goose-Inspired Ideas for Klar

> **Source:** Code to the Moon, "New Language Claims to Be as Safe as Rust, but Faster and
> Simpler" (YouTube, 2026-10-21), an overview of Wouter van Oortmerssen's Goose language,
> and the video's comment section. Reviewed 2026-10-22.
>
> **Status: captured, not decided.** Nothing here is planned work. Each idea records what
> it is, how it fits Klar's principles, and what would have to be true before it is
> worth taking up. Deciding on any of them gets its own record in `docs/decisions/`.

---

## Ideas not to adopt

These are written down so a later session does not re-propose them without the reasons.

- **Returning from a caller further up the stack.** Goose lets a function return a value
  on behalf of an enclosing caller (a compile error if that caller is not on the stack).
  This conflicts with "No ambiguity. No surprises.": a reader can no longer tell from a
  call whether it can end the current function. It was the most criticised feature in the
  video's comments ("a worse goto"). Klar's `?` gives early exit with the exit point
  marked at the call.
- **Errors as a trailing boolean return value.** Goose has no `Result`; by convention the
  last of several return values says whether the call succeeded. Klar's `Result#[T, E]`
  with `?` cannot be ignored by accident and carries the error.
- **Many array flavors** (compile-time sized, runtime-fixed, grow-only, grow-and-shrink).
  Each is defensible alone; together they drew the comment "Now I have to hold like 20
  more contexts in my head just to use an array." Any new collection type in Klar has to
  pass "Explicitness earns its characters."

## Possible future features

### 1. A grow-only list with stable element references

**Idea:** a list that can be appended to but never shrinks, so a reference to one of its
elements cannot dangle while the list grows. Goose uses this to allow references into a
list that is still being modified.

**Fit:** matches "Ownership without complexity": a safety guarantee from the type, with no
lifetime annotations.

**Open questions:** Klar's `List` is a header pointer to a heap `{ptr, len, cap}`, so a
push that reallocates moves the elements; a stable-reference list would need chunked
storage or a different growth strategy. How references into a list are checked today
would need confirming first.

**Worth taking up when:** real Klar programs need to hold references into a growing list
and are working around it.

### 2. Typed narrow indexes into a list ("links")

**Idea:** Goose's links are pointers stored as a 32-, 16- or 8-bit offset from the start
of one contiguous array instead of a full address, which saves memory in pointer-heavy
data.

**Fit:** Klar code already does this by hand. The self-hosted compiler uses
`Map#[string, i32]` plus a parallel `List` for indirection (the workaround for large-struct
map values). A typed index such as `Index#[T]` (optionally `Index#[T, u16]`) would make
the pattern explicit and checked, so an index into one list cannot be used on another
by mistake.

**Open questions:** how the index names the list it belongs to without lifetimes; what
happens on an index into a list that has shrunk (bounds check, as today).

**Worth taking up when:** the selfhost parallel-list pattern, or a user program, shows
index mix-ups as a real bug class.

### 3. Enums sized to the stored variant

**Idea:** Goose has two enum kinds: fixed-size (every value takes the largest variant's
space, like Rust and Klar today) and variable-size (each value takes only its own
variant's space), with different restrictions on each.

**Fit:** a memory optimisation with a real cost in rules a user must learn. Today a large
variant can be put behind an `Rc` to keep the enum small.

**Precondition:** the fixed layout must be correct first. Bug 34 (payload slots sized by
the data layout) is fixed; Bug 126 (payload starts at offset 1, misaligned) and Bug 127
(generic enum over an enum gets too small a payload slot) were open on 2026-10-22.

**Worth taking up when:** the fixed layout bugs are closed and a measured program shows
enum size as a real memory cost.

### 4. Share-nothing threads with typed queues

**Idea:** Goose threads share no memory; data moves between threads only through
message queues, one per data type. No locks or mutexes.

**Fit:** likely easier for AI-generated code to get right than shared state with `Arc`
and locks, which suits Klar's AI-native goal. Klar currently offers `Arc#[T]` for sharing
across threads; `docs/design/async-runtime-internal.md` covers the async runtime.

**Open questions:** whether this replaces or sits beside `Arc`; how values are moved
(copied versus ownership transfer) into a queue; how it relates to async.

**Worth taking up when:** Klar's concurrency model is next revisited.

## Benchmark lessons

Goose's headline speed figures (3.3x idiomatic C++, 1.12x the best safe Rust) were met
with doubt, not interest. The presenter himself said he had not checked whether the
comparison programs were fair, and "entirely vibe coded" became the comment section's
reason to dismiss it. Klar's v0.6.0 performance baseline (`PLAN.md` Phase 1) will be read
by an outside party judging Klar on performance, so four rules are proposed for
`docs/benchmarks/v0.6.0.md`. They are written into that `PLAN.md` task as proposals.

1. **Reproducible:** each result links the exact source and command. Any comparison
   program in another language is written the way a competent user of that language
   would write it, or a single checked unfair comparison discredits every number.
2. **Honest:** results where Klar is slower are reported, with a reason when known. A
   table Klar wins everywhere reads as marketing.
3. **Evidence first:** the page opens with what the tests show (self-host fixed point,
   suite counts) before any speed figure.
4. **Limits stated:** the page links the release's Known issues list. Goose's own
   "downsides" section was one of the few things commenters credited.

The video's other point, that wordy languages cost more in AI tokens, is the trade-off
Klar's third principle ("Explicitness earns its characters") already makes on purpose.
