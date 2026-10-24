# Explicit Move and Clone: `move a` and `clone a`

> **Written for Claude.** Precise for a later session, not easy reading. To review
> it, ask Claude to explain it in plain words and judge the explanation.

> **Status:** Proposed, 2026-10-24. Waiting on Philip's approval. No code is written
> against this design until he approves it, and the open questions in § 8 are his.
> **Decision:** [2026-10-18-explicit-move-clone](../decisions/2026-10-18-explicit-move-clone.md)

---

## 1. Summary

A named variable holding a non-Copy value (a `String`, a `List`, a struct) cannot be used
bare where the use would hand the value to a new owner. The program says which it means:
`move a` hands it over and `a` is unusable afterwards; `clone a` hands over a copy made by
the type's `Clone` impl and `a` stays usable. A bare `a` there is a compile error that
names both spellings. Copy values (integers, floats, `bool`, `char`) keep the bare form.
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
| `bool`, `char`, `void` | `Rc#[T]`, `Arc#[T]` |
| references (`ref T`) | closures (`fn(…) -> …` values) |
| arrays, tuples and optionals whose elements are all Copy | slices, `Result#[T, E]` |
| a type alias of any of the above | user structs and enums (§ 8, Q4) |
| | a type variable `T` with no `Copy` bound |
| | `string` (§ 8, Q1 — recommendation: Copy) |

A generic body that moves a `T` writes `move t`, since `T` may be bound to a String. A
`T: Copy` bound lets it write `t`; this needs the `Copy` trait of `stdlib/core.kl` to be
a usable bound, which § 8 Q4 settles.

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
- a struct literal field value, an array or tuple literal element, an enum payload,
  `Some(move a)`, `Ok(move a)`, `Err(move e)`
- a `return` value, unless § 8 Q2 exempts it (recommendation: exempt a whole local)

Uses that do not consume stay bare, since nothing changes owner:

- a method receiver: `a.len()`, `a.push(x)` (§ 8 Q3 covers a by-value `self`)
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
  `build`, `run`, `test`), and runs on every module. New error kind `implicit_move` for a
  bare non-Copy place in a consuming position, and `needless_marker` for § 3's
  marker-where-none-is-needed. Both report the line and column of the operand.
- **Move tracking stays in the ownership pass** (`src/ownership/checker.zig`), which
  already marks a `let`/`var` initializer and a `return` value moved and reports
  `use_after_move`. With markers, it reads the move off the `move` node instead of
  inferring it from the binding's declared type. `clone a` marks nothing.
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
  `a.as_str()` passes `klar check` (found while writing this design).
- **Bug 137** — the ownership pass decides Copy from the spelled type, so a variable of
  `type Id = i32` is treated as moved by `let b: Id = a` and a later read of `a` is a
  false `use_after_move` (found while writing this design). § 5's move-off-the-node rule
  removes this guess for moves, but the bug ships today.

## 7. Rollout

One PR per step, in order. Each step leaves `./run-tests.sh` green.

1. **The prerequisite bugs** of § 6.
2. **Accept both markers, optional.** Lexer, both parsers, `dump-ast`, typed AST, the
   type checker (`move` on a place, `clone` needs `Clone`), the ownership pass (a `move`
   node marks its place moved), and the three backends. A bare use still compiles. The
   reserved-keyword change lands here: it breaks nothing measured, but any user program
   naming a variable `move` or `clone` stops parsing, which goes in the changelog.
3. **Migration tool:** `klar fix --explicit-moves <path>…` rewrites a file in place:
   inserts `move` before every bare non-Copy place in a consuming position (where the
   checker would report `implicit_move`) and rewrites every `a.clone()` to `clone a`.
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

## 8. Open questions for Philip

Each has a recommendation; none is decided.

- **Q1. Is the primitive `string` Copy?** The type checker says yes (`Type.isCopyType`
  treats every primitive as Copy, and Bug 45 calls `string` "the Copy primitive"), but
  the ownership pass and `docs/memory/ownership.md` say no: `let b: string = a` marks `a`
  moved today. *Recommendation: Copy.* `string` cannot be changed in place, so sharing
  its text is invisible, which is the reason the decision gave for rejecting implicit
  copies of `String` only. Every `let b: string = a` then stays bare.
- **Q2. Does `return s` need `move`?** *Recommendation: no, for a whole local or
  parameter*: nothing can use `s` after the return, so the answer to "copy or move?" can
  never be observed and the marker earns no characters. A field (`return self.name`)
  still needs `move` or `clone`, since the struct lives on in the caller's view.
- **Q3. A by-value `self` receiver.** `fn consume(self) -> T` called as `a.consume()`
  does not move `a` today. *Recommendation: leave receivers bare and unmoved in this
  design*, and file the by-value-receiver move as its own bug if Philip wants it moved.
  Marking receivers would put `(move a).consume()` in every builder chain.
- **Q4. User Copy structs.** No user struct can be Copy today, so a `Point { x: f64,
  y: f64 }` would need `move p` or `clone p` at every hand-off. *Recommendation: allow
  `impl Point: Copy` (the trait already exists in `stdlib/core.kl`) for a struct whose
  fields are all Copy, checked by the type checker, as step 2½ of § 7.* The alternative
  is the noise, which the decision named as the reason Copy types keep the bare form.

## 9. What this design does not settle

- Closure captures: whether a closure capturing a non-Copy local needs `move a` in its
  body or a capture list. Captures do not move today; this design leaves them as they are.
- Moves out of a collection element (`move list[0]`): refused by § 4 (an index is not a
  place `move` takes) until a design for partial collection moves exists.
- Whether `klar fmt` learns the markers' spacing beyond one space after the keyword.
