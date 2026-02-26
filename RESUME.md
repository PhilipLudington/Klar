# Milestone 9.10 Phase A — QA Issues

## Issue 1: Method arguments never type-checked (Moderate)

**Location:** `selfhost/checker_expr.kl:528-573`

`check_method_call` checks the receiver but never calls `check_expr` on the arguments. This means:
- `"hello".contains(undefined_var)` — undefined variable not caught
- `list.push(bad_expr + oops)` — errors inside arg expressions silently swallowed

Pre-existing gap from 9.9 (struct methods didn't check args either), but Phase A significantly expands the surface area.

**Fix:** Add a `check_method_args` helper that walks `data.args_start..args_start+args_count` calling `check_expr` on each arg (without type matching — just to catch expression-level errors). Wire it before the dispatch.

## Issue 2: Stale comments in checker.kl (Minor)

**Line ~630-632:**
> `// Deferred to 9.10 for full builtin support. Stub for now.`

No longer a stub — 18 builtins are registered. Update to say "Phase A" or similar.

**Line ~592-593:**
> `// For 9.9, this is strict equality (no widening, no coercion).`

With `is_unknown_type` wildcards, this is no longer "strict equality."

## Issue 3: `Option#[T]` generic form not handled (Minor)

**Location:** `selfhost/checker.kl`, `resolve_generic_apply`

`List`, `Map`, `Set`, `Rc`, `Arc`, `Result` are handled. But `Option#[T]` (the explicit generic apply syntax) falls through to `type_unknown()`. Klar typically uses `?T`, but `Option#[T]` may appear in some files.

**Fix:** Add an `Option` case with `args_count == 1` that calls `make_optional_type`.

## Issue 4: No direct unit test for unknown type compatibility (Nit)

`checker.kl` main tests `type_compatible` for i32/bool/never/error but not the new `unknown` wildcard. It's tested indirectly via `test_expanded_builtins`, but a direct test in checker.kl's main would be more robust.
