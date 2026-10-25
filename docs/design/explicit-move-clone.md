# Explicit Move and Clone: `move a` and `clone a`

> **Written for Claude.** Precise for a later session, not easy reading. To review
> it, ask Claude to explain it in plain words and judge the explanation.

> **Status:** Approved, 2026-10-24 (Philip); proposed and revised with his answers to
> § 8 Q1–Q4 the same day, then approved with Q5 as recommended.
> **Decisions:** [2026-10-18-explicit-move-clone](../decisions/2026-10-18-explicit-move-clone.md),
> [2026-10-24-move-clone-open-questions](../decisions/2026-10-24-move-clone-open-questions.md),
> [2026-10-24-move-clone-approved](../decisions/2026-10-24-move-clone-approved.md)

---

## 1. Summary

A named variable holding a non-Copy value (a `String`, a `List`, a struct) cannot be used
bare where the use would hand the value to a new owner. The program says which it means:
`move a` hands it over and `a` is unusable afterwards; `clone a` hands over a copy made by
the type's `Clone` impl and `a` stays usable. A bare `a` there is a compile error that
names both spellings. Copy values (integers, floats, `bool`, `char`, the primitive
`string`, and a struct that opts in with `impl X: Copy`) keep the bare form.
`a.clone()` called directly becomes an error, so a copy has one spelling.

```klar
let a: String = String.from("abc")
let b: String = move a          // a is moved; using a again is use_after_move
let c: String = clone b         // b stays usable; c is an independent String
let d: String = b               // error: 'b' is a String; write `move b` or `clone b`
let n: i32 = 5
let m: i32 = n                  // fine: i32 is Copy
```

Both markers compile to what the program compiles to today: `move a` to the bare `a` (a
bit copy plus the ownership checker's move mark), `clone a` to the `Clone::clone` call
that `a.clone()` makes today. No backend gains runtime machinery.

## 2. Which values need a marker

A value needs a marker when its type is **not Copy**. Copy is decided by the type
checker on the resolved type, never on the spelling, so a type alias is as Copy as the
type it names.

| Copy (bare use) | Not Copy (marker required) |
|---|---|
| `i8`…`i128`, `u8`…`u128`, `isize`, `usize` | `String` |
| `f32`, `f64` | `List#[T]`, `Map#[K, V]`, `Set#[T]` |
| `bool`, `char`, `void`, `string` | `Rc#[T]`, `Arc#[T]` |
| references (`ref T`) | closures (`fn(…) -> …` values) |
| arrays, tuples and optionals whose elements are all Copy | slices, `Result#[T, E]` |
| a user struct with `impl X: Copy` (§ 8 Q4) | user structs without it, and user enums |
| a type alias of any of the above | a type variable `T` with no `Copy` bound |

**`string` is Copy** (§ 8 Q1). It cannot be changed in place, so two names sharing its
text can never observe each other. The price: a `string` must never own heap memory, so
every builtin that builds text at run time returns an owned `String`, never `string`
(Bug 45's `env_get` is the known case; § 6).

**`impl X: Copy`** (§ 8 Q4) uses the `Copy` trait that `stdlib/core.kl` already declares.
The type checker accepts it only when every field of `X` is Copy by this table, and
otherwise reports the first non-Copy field by name (`'Point' cannot be Copy: field
'label' is a String`). It is opt-in, never inferred: adding a `String` field to a Copy
struct later turns its `impl` into an error instead of silently changing every hand-off.
User enums have no opt-in in this design.

A generic body that moves a `T` writes `move t`, since `T` may be bound to a String. A
`T: Copy` bound lets it write `t`, and Q4's opt-in makes `Copy` a usable bound.

## 3. Which uses need a marker

The rule covers **named places**: a local, a parameter, or a field path rooted at one
(`p.name`). A temporary (a call result, a literal, a struct literal) is already a fresh
value with no other name, so it is used bare: `let s: String = make()` stays as it is.

A named non-Copy place needs `move` or `clone` in each **consuming position**, where the
value goes to a new owner:

- a `let` or `var` initializer: `let b: String = move a`
- the right side of an assignment: `b = move a`
- an argument to a by-value parameter, functions and methods alike:
  `consume(move a)`, `list.push(clone s)`, `tx.send(move msg)`
- **the receiver of a method whose `self` is by value** (§ 8 Q3): `fn finish(self) ->
  Report` called on a named non-Copy place is spelled `(move b).finish()`, and `b` is
  moved (§ 4, § 8 Q5 for the spelling)
- a struct literal field value, an array or tuple literal element, an enum payload,
  `Some(move a)`, `Ok(move a)`, `Err(move e)`
- a `return` of a field path: `return move self.name` or `return clone self.name`, since
  the struct lives on in the caller's view

**`return s` of a whole local or parameter stays bare** (§ 8 Q2). Nothing can use `s`
after the return, so whether it was copied or moved can never be observed, and the
marker would earn no characters. `return move s` there is a `needless_marker` error.

Uses that do not consume stay bare, since nothing changes owner:

- the receiver of a method whose `self` is `ref` or `inout`: `a.len()`, `a.push(x)`, and
  any receiver of a Copy type, by-value `self` included (`p.distance()` on a Copy `Point`)
- a borrow: `ref a`, and an argument to an `inout` or `ref` parameter
- an operator operand: `a == b`, `a < b`, `a + b` on strings
- string interpolation `"{a}"`, a `match a` scrutinee, an `if` or `while` condition
- a `for x in a` iterable (it iterates today without moving `a`; unchanged)
- a field read of a Copy field: `p.age`

A marker where none is needed is an error too, so each use has exactly one spelling:
`move n` on an `i32` reports "`n` is Copy; write `n`", and `clone make()` reports "a
temporary is already a fresh value; write `make()`".

## 4. Syntax

`move` and `clone` are prefix operators on an operand, parsed at the `.unary` precedence
`ref` and `not` use (`src/parser.zig` `parseRefExpr`):
postfix operations bind first, so `move p.name` moves the field `p.name` (a partial move,
as `docs/memory/ownership.md` § Moving Out of Structs describes) and `clone a.items`
clones the field. `move` takes only a place. `clone` takes a place; a temporary is the
error in § 3.

```klar
let name: String = move person.name   // person.name is moved, person.age still readable
let copy: List#[i32] = clone self.items
```

**On a method receiver** (§ 3; spelling approved 2026-10-24, § 8 Q5) the marker
is the same prefix operator, parenthesized: `(move b).finish()` moves `b` into the
by-value `self`; `(clone b).finish()` hands `finish` a copy and keeps `b`. Since postfix
operations bind first, the unparenthesized `move b.finish()` means "move the result of
`b.finish()`", a temporary, and is the `needless_marker` error, whose message names the
parenthesized form. The proposal adds no syntax: the parser already accepts a
parenthesized unary operand as a method-call object. A builder chain marks only its
first link, since each later receiver is the previous call's temporary:

```klar
let r: Report = (move b).title("Q3").rows(rows_for(q)).finish()
let s: Report = Builder.new().title("Q3").finish()   // all temporaries: no marker
```

Rejected spellings, kept here so they are not re-proposed: `move b.finish()` with a
special case making `move` bind to the receiver (two meanings for one operator,
depending on what follows), and a receiver annotation at the call (`b.finish(move)`,
`b move.finish()`), new grammar for a case the operator already covers.

**Keyword status (the decision's open question).** Both become reserved keywords in the
lexer table (`src/lexer.zig`). `move` is already listed as reserved in
`docs/appendix/keywords.md` § Reserved for Future Use but is not in the lexer, so today
it lexes as an identifier. The one place `clone` must still be a name is the trait
method itself: `fn clone(self) -> Self` in `trait Clone` (`stdlib/core.kl:89`) and in each
`impl X: Clone`. The parser accepts the `clone` keyword token as a method name after `fn`
inside a trait or impl body, and nowhere else. The alternative, renaming the trait
method, breaks every existing `Clone` impl for no gain, since no `.kl` file in the repo
uses `move` or `clone` as an identifier (measured 2026-10-24: the only hit is the trait
method declaration).

**`a.clone()` becomes an error**, `"write `clone a`"`, at every call written as a
method call on a value, including `Rc` and `Arc`, whose `clone a` keeps today's shallow
meaning (the count goes up, the data is shared). `Clone::clone` stays callable from the
trait's own default methods, if any are ever written.

## 5. Where each check lives

- **The marker rule lives in the type checker** (`src/checker/`). It has the resolved
  type of every expression, sees aliases through, runs on every command (`check`,
  `build`, `run`, `test`), and runs on every module. It also knows each method's
  receiver mode, which is what decides whether a receiver is a consuming position, and
  it checks each `impl X: Copy` against X's fields. New error kind `implicit_move` for a
  bare non-Copy place in a consuming position, and `needless_marker` for § 3's
  marker-where-none-is-needed. Both report the line and column of the operand.
- **Move tracking stays in the ownership pass** (`src/ownership/checker.zig`), which
  already marks a `let`/`var` initializer and a `return` value moved and reports
  `use_after_move`. With markers, it reads the move off the `move` node instead of
  inferring it from the binding's declared type. `clone a` marks nothing. A by-value
  receiver written `(move b)` marks `b` moved the same way (`analyzeMethodCall` marks
  no receiver today; Bug 136).
- **The ownership pass must run on `build` and `run`** (Bug 52). Without that, `move a`
  followed by a use of `a` compiles to native code that shares memory behind the
  moved-from name, which is the gap the decision record set out to close.
- **The selfhost checker** (`selfhost/checker*.kl`) gains the same two error kinds, so
  the diagnostic parity suites keep matching.

### Representation

The parser lowers `clone a` to the method call `a.clone()` with a flag
`from_keyword = true` on the `MethodCall` node, and `move a` to a `Unary` with a new op
`.move_`. The interpreter, VM and native backends lower `.move_` as its operand and need
nothing for `clone`, because they already handle the method call. The type checker
rejects a `clone` method call without the flag. `dump-ast` and the typed AST
(`docs/design/typed-ast-format.md`) gain the `"op":"move"` unary and the `from_keyword`
field, and the selfhost parser emits both, so `--ast-input` and `--typed-ast-input` keep
parity.

## 6. Prerequisites in the move checker

The decision record says the move-checking bugs come first, since explicit syntax still
needs the checker to reject a use after `move`. Open at 2026-10-24:

- **Bug 52** — the ownership pass runs only in `klar check`, entry module only.
- **Bug 135** — a plain `b = a` assignment never moves `a`.
- **Bug 132** — false `use_after_move` after an early return and after a loop
  reassignment (the selfhost suite carries known failures until it is fixed).
- **Bug 136** — an argument to a by-value parameter never moves: `consume(a)` then
  `a.as_str()` passes `klar check` (found while writing this design). The same gap holds
  for a by-value `self` receiver, which § 8 Q3 makes a consuming position: `analyzeCall`
  and `analyzeMethodCall` (`src/ownership/checker.zig`) are one system, but the
  receiver half lands in § 7 step 2¾, after the receiver conversion: marking by-value
  receivers moved while `Clone::clone` still takes `self` by value would make every
  `s.clone()` today a move of `s`.
- **Bug 137** — the ownership pass decides Copy from the spelled type, so a variable of
  `type Id = i32` is treated as moved by `let b: Id = a` and a later read of `a` is a
  false `use_after_move` (found while writing this design). § 5's move-off-the-node rule
  removes this guess for moves, but the bug ships today.
- **Bug 45** — `env_get` returns runtime-built text as the primitive `string`, so it
  leaks. With `string` Copy (§ 8 Q1), the fix direction is decided: it returns an owned
  `String`, as must any other builtin found doing the same.

## 7. Rollout

One PR per step, in order. Each step leaves `./run-tests.sh` green.

1. **The prerequisite bugs** of § 6, Bug 136's receiver half excepted (step 2¾).
   Bug 136's argument half is excepted too: it alone made 10 selfhost files fail
   `klar check`, so it lands after step 2½ and a selfhost migration to `ref`
   parameters, its exact slot not yet set
   (`docs/decisions/2026-10-24-bug136-argument-half-deferred.md`).
2. **Accept both markers, optional.** Lexer, both parsers, `dump-ast`, typed AST, the
   type checker (`move` on a place, `clone` needs `Clone`), the ownership pass (a `move`
   node marks its place moved), and the three backends. A bare use still compiles. The
   reserved-keyword change lands here: it breaks nothing measured, but any user program
   naming a variable `move` or `clone` stops parsing, which goes in the changelog.
   2½. **`impl X: Copy`** (§ 8 Q4): the type checker accepts it for an all-Copy struct
   and refuses it naming the field otherwise; a Copy struct joins § 2's Copy column in
   both checkers and the ownership pass. Lands before step 3 so the migration tool
   does not mark hand-offs of structs that will be Copy.
   2¾. **Borrowing receivers** (§ 8 Q3). Every method that only reads `self` and is
   declared on a type that is not Copy after step 2½ is changed to `self: ref X`, or
   once markers are required each call to it kills its caller's variable. This includes
   the receivers and `other` parameters of the builtin traits in `stdlib/core.kl`
   (`Eq`, `Ordered`, `Clone`, `Hash`, `Printable`), which take `self` by value today;
   `clone a` lowers to a `Clone::clone` call and must not move `a`. Measured 2026-10-24
   over the repo's `.kl` files: 148 methods take `self` by value against 9 `self: ref`
   and 13 `self: inout` (counting receivers inside `impl` and `trait` bodies only; the
   walk-through's 167 against 9, quoted in the decision record, was a looser grep). Of
   the 148, 37 are trait declarations, 53 are impl methods on
   structs whose fields are all Copy (with `string` Copy), which step 2½'s opt-in
   covers without a receiver change, and 58 are impl methods on structs with a non-Copy
   field: those 58 and the 37 trait declarations are this step's scope. Methods that
   genuinely consume `self` (a builder's `finish`) keep it by value; the step's PR names
   each one kept. Bug 136's receiver half (§ 6) lands at the end of this step.
3. **Migration tool:** `klar fix --explicit-moves <path>…` rewrites a file in place:
   inserts `move` before every bare non-Copy place in a consuming position (where the
   checker would report `implicit_move`), a by-value receiver included as `(move b)`,
   and rewrites every `a.clone()` to `clone a`. It leaves every `return` of a whole
   local or parameter bare (§ 8 Q2).
   It runs over `test/`, `stdlib/`, `examples/`, `selfhost/`, `tools/` and the code
   blocks of `docs/`. Measured scale, 2026-10-24: 223 `let`/`var` initializers that are a
   bare name of a capitalised type (208 in `selfhost/`, most of them
   `var c: TypeChecker = tc`), 42 `.clone()` calls, and by-value call arguments, which a
   grep cannot count and the tool's dry run will. The selfhost sources move only after
   the selfhost parser learns the syntax in step 2, so the bootstrap keeps building.
4. **Require the markers.** Turn on `implicit_move`, `needless_marker` and the
   direct-`.clone()` error in both checkers. The parity suites and the bootstrap fixed
   point are re-measured in this PR.
5. **Docs.** `docs/memory/ownership.md` (every example), `docs/advanced/builtin-traits.md`
   § Clone, `docs/language/traits.md`, `docs/appendix/keywords.md` (move `move` out of
   "Reserved for Future Use", add `clone`), and `CLAUDE.md`'s syntax quick reference.

## 8. Questions for Philip

Q1–Q4 decided by Philip on 2026-10-24
([decision record](../decisions/2026-10-24-move-clone-open-questions.md)), and Q5 the
same day with his approval of the design
([decision record](../decisions/2026-10-24-move-clone-approved.md)); each is kept below
as asked, with its answer.

- **Q1. Is the primitive `string` Copy?** **Decided: yes, Copy** (as recommended). Every
  `let b: string = a` stays bare, and runtime-built text is returned as an owned
  `String` (§ 2, Bug 45). The type checker says yes (`Type.isCopyType`
  treats every primitive as Copy, and Bug 45 calls `string` "the Copy primitive"), but
  the ownership pass and `docs/memory/ownership.md` say no: `let b: string = a` marks `a`
  moved today. *Recommendation: Copy.* `string` cannot be changed in place, so sharing
  its text is invisible, which is the reason the decision gave for rejecting implicit
  copies of `String` only. Every `let b: string = a` then stays bare.
- **Q2. Does `return s` need `move`?** **Decided: no, for a whole local or parameter**
  (as recommended); a field return needs a marker (§ 3). Rejected: `return move s`
  always, which is noise at the end of nearly every function that builds a value. *Recommendation: no, for a whole local or
  parameter*: nothing can use `s` after the return, so the answer to "copy or move?" can
  never be observed and the marker earns no characters. A field (`return self.name`)
  still needs `move` or `clone`, since the struct lives on in the caller's view.
- **Q3. A by-value `self` receiver.** `fn consume(self) -> T` called as `a.consume()`
  does not move `a` today. *Recommendation: leave receivers bare and unmoved in this
  design*, and file the by-value-receiver move as its own bug if Philip wants it moved.
  Marking receivers would put `(move a).consume()` in every builder chain.
  **Decided: a by-value receiver moves the variable**, against the recommendation, so the
  ownership rule has no exception at method calls and a method that keeps `self` cannot
  share one value between two names. What it adds: the receiver as a consuming position
  (§ 3), its spelling (§ 4, Q5), the receiver move in the ownership pass (§ 6, Bug 136),
  and step 2¾'s conversion of read-only methods to `self: ref X` (§ 7).
- **Q4. User Copy structs.** No user struct can be Copy today, so a `Point { x: f64,
  y: f64 }` would need `move p` or `clone p` at every hand-off. *Recommendation: allow
  `impl Point: Copy` (the trait already exists in `stdlib/core.kl`) for a struct whose
  fields are all Copy, checked by the type checker, as step 2½ of § 7.* The alternative
  is the noise, which the decision named as the reason Copy types keep the bare form.
  **Decided: allow `impl X: Copy`**, opt-in and checked field by field (§ 2, step 2½).
  With Q3's answer it is also what keeps `p.distance()` on a number-only struct from
  moving `p`. Rejected: no user Copy structs.
- **Q5. How is the marker spelled on a method receiver?** (Q3's consequence)
  **Decided: the parenthesized prefix operator** (as recommended), `(move b).finish()`
  and `(clone b).finish()`.
  *Recommendation: the existing prefix operator, parenthesized: `(move b).finish()` and
  `(clone b).finish()`* (§ 4). No new syntax, one meaning for `move`, and only the first
  link of a builder chain is marked. Its cost is the parentheses, on calls that, after
  step 2¾, are only the genuinely consuming ones. The alternatives (a `move` that binds
  to the receiver when a call follows, or a marker inside the call's argument list) are
  in § 4 with why each is rejected.

## 9. What this design does not settle

- Closure captures: whether a closure capturing a non-Copy local needs `move a` in its
  body or a capture list. Captures do not move today; this design leaves them as they are.
- Moves out of a collection element (`move list[0]`): refused by § 4 (an index is not a
  place `move` takes) until a design for partial collection moves exists.
- Whether `klar fmt` learns the markers' spacing beyond one space after the keyword.
