# Bug 136's argument half waits for Copy structs and a selfhost `ref` migration (2026-10-24)

**Decided by:** Philip, 2026-10-24 · **Status:** Decided
**Led to:** PLAN.md Next Up "Ownership move tracking" (now Bugs 132, 135, 137); BUG.md
Bug 136 `**Deferred:**`, Bug 138; `docs/design/explicit-move-clone.md` § 7 step 1

## What prompted it

Rollout step 1 of the explicit move/clone design was meant to fix four move-checker bugs
together: 132, 135, 136 (its argument half) and 137. With all four built, the ownership
check reported 253 `use_after_move` errors across 10 of the 21 selfhost source files.
Every one of them is correct under the approved rules. For example, `tk(p2, "eof")`
hands a `Parser` to a by-value parameter, and `insert(k, struct_type)` hands over a
`KlarType`, which only becomes Copy once step 2½ (`impl X: Copy`) exists. With 136's
argument half left out, all 21 selfhost files passed, including the four that were on
the selfhost suite's expected-failure list for Bug 132.

## What we weighed

- **(a) Land it now** and put the 10 files on the selfhost expected-failure list until
  the selfhost is migrated. This closes Bug 136 sooner, but the selfhost check would stop
  guarding ownership in those 10 files.
- **(b) Move it later in the rollout**, after step 2½ and a selfhost migration to `ref`
  parameters. Bugs 132, 135 and 137 land now, and the expected-failure list empties.

## What we decided, and why

(b), the recommendation. Bug 136 only means some real errors go unreported. Option (a)
would stop ownership checking on 10 files that are fully checked today. Philip took the
recommendation.

For whoever picks it up: the argument half that was built moves each bare non-Copy
variable passed to a function the module declares or imports by name (a call to a
builtin such as `println` moves nothing, and neither does a local variable that shadows
the function's name), and each bare non-Copy variable passed as a method argument. In
both cases this goes through the pass's one `moveValue` path. Call-site borrows are
spelled out (`f(ref a)`), so a variable passed bare goes to a by-value parameter.

## What it does not settle

- The exact rollout slot: after step 2½, but not decided whether before or after step 2¾
  (borrowing receivers), or as part of the step 3 migration.
- How the selfhost gets to `ref` parameters: by hand, or with the step 3 migration tool.
- Bug 138 (parameters are always treated as Copy) has the same spread and is deferred
  with it.
