# Moves and copies of non-Copy values become explicit: `move a` and `clone a` (2026-10-18)

**Decided by:** Philip, 2026-10-18 · **Status:** Decided (design to be written; no code yet)
**Led to:** Bugs 52, 115, 49 (move checking; 52 and 49 were already filed and gained the
new reproductions, 115 is new); PLAN.md Next Up "Design — explicit move/clone"

## What prompted it

PR 65 fixed Bug 108: using a `String` after `.drop()` crashed native programs. The fix
makes `drop()` reset the String to empty instead of freeing it. A side agent pointed out
the consequence: Klar String copies share one stored text, so after
`let b: String = a; a.drop()`, `b` is silently empty too. "That turns a crash into
quietly lost text, which sits uneasily with Klar's 'No surprises' principle."

Testing showed the copy should never have existed. Klar's ownership docs say assignment
*moves* a String, and `klar check` rejects that program (`use of moved variable 'a'`).
But `klar build` compiles it anyway, a move into a `var` isn't tracked at all, and
`klar check` exits 0 even when it fails. Those three gaps are what let two names share
one String.

## What we weighed

- **Independent copies on `=`** (as in Go, C# or Python). The most natural at first
  glance. Rejected: those languages' strings can't be changed in place, so sharing is
  invisible there. Klar's String can be, so every assignment would hide an allocation.
- **Shared text, changeable through either name** (what native Klar does today when a
  move goes unchecked). Rejected: no mainstream language works this way, and it is the
  surprise the side agent found.
- **Plain move, as the spec already says** (as in Rust). Sound, and the compiler catches
  mistakes. But whether `let b = a` copies or moves depends on `a`'s type, declared
  elsewhere, so the line can't be read at a glance.
- **Explicit `move a` / `a.clone()`.** Fixes the at-a-glance problem, but puts the two
  answers to one question in two different shapes.
- **Explicit `move a` / `clone a` keywords.** Chosen.

## What we decided, and why

For a non-Copy value (String, List, structs) held in a named variable, using it as a
value must say which you mean: `move a` or `clone a`. A bare `a` is a compile error
that suggests both. Copy types (`i32`, `f64`, `bool`, `char`) keep the bare form,
because for them copy and move are the same and a marker would be noise. `clone a`
calls the `Clone` trait the way `==` calls `Eq`, so types still define their own
copy, and calling `a.clone()` directly becomes an error so there is one spelling.

Philip: "in our language, we like to be explicit, so maybe we should deal with the
problem by being more explicit and not make any assumptions about what equal means,
because in a way, it is ambiguous." And: "If we are adding a move keyword, doesn't it
seem more consistent to have a clone keyword instead of a .clone() function?"

PR 65 stays as it is. Under the move rules, the program the side agent described is
invalid, so a dropped String emptying a "copy" only happens in programs the compiler
should already reject.

## What it does not settle

- Whether `return s` needs `move` (nothing can use `s` afterwards).
- Whether `clone` is a contextual keyword or the trait method is renamed.
- The migration: every `.kl` file that moves a value changes (tests, standard library,
  the self-hosted compiler). A tool that adds `move` where the checker sees a move
  today would do most of it.
- The three move-checking bugs come first either way: explicit syntax still needs the
  checker to reject a use after `move`, and the build to stop on it.
