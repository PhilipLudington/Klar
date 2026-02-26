# RESUME.md — Milestone 9.9: Self-Hosted Type Checker (Foundation)

## Status: UNBLOCKED — All 5 codegen fixes applied

### Phase A Progress (Compiler Fixes) — COMPLETE

All five codegen bugs in `src/codegen/emit.zig` have been fixed:

| Fix | Status | Description |
|-----|--------|-------------|
| Map/Set in function signatures | **DONE** | `typeExprToLLVM()` + `requiresSretForTypeExpr()` now handle `Map#[K,V]` and `Set#[T]` |
| `list[i]` for all element types | **DONE** | `emitIndexAccess()` now handles List indexing with bounds checking |
| Collection methods on struct fields | **DONE** | List/Map/Set method dispatch uses temp alloca for non-identifier objects (like `h.items.len()`) |
| Collection metadata for function params | **DONE** | All 4 param sites (method, function, mono function, mono method) now set list/map/set/string_data metadata |
| Data enum variant destructuring | **DONE** | `lookupVariantIndex()` now handles `.enum_` subjects for shorthand patterns |

All 1469 tests pass (232 unit + 307 native + 10 app + 7 module + 61 args + 3 freestanding + 22 check + 37 fmt + 790 selfhost).

### Verification Results

- `List#[MyType]` parameter with `.get()` → prints `42` ✓
- `Shape::Circle(42)` shorthand destructuring → exit code 42 ✓
- Data enum from List + destructuring → exit code 42 ✓
- `selfhost/checker_test.kl` → prints "OK: found", exit code 0 ✓

### What's Done
- Read and understood all selfhost files (types.kl, ast.kl, lexer.kl, parser*.kl)
- Wrote initial checker.kl (~500 lines) but hit Klar codegen constraints
- Fixed all 5 codegen bugs (Map/Set signatures, list[i], field method dispatch, param metadata, enum destructuring)
- checker_test.kl in selfhost/ passes

### Constraint Summary (Post-Fixes)

**Now works:**
- Maps/Sets in function signatures (`fn f(m: Map#[K,V]) -> Map#[K,V]`)
- `list[i]` for primitives, simple enums, and structs
- `h.items.len()`, `h.items.is_empty()` etc. on struct fields (read-only)
- Lists as `var` locals: push, pop, get, len, set, first, last, clear
- `.get()` / `.push()` / other element-aware methods on List function parameters
- Returning `List#[T]` from functions
- `?KlarType` returns
- Data enum variant destructuring in match (`Circle(r) => ...`)

**Still has limitations:**
- `list[i]` for data enums (`List#[enum_with_data]`) — use `.get(i)` instead
- `h.items.push(x)` mutating original field (temp alloca = copy semantics) — extract to local, mutate, reassign

### Next Steps

#### Phase B: Self-Hosted Type Checker (Milestone 9.9)
1. Rewrite checker.kl with proper architecture
2. Write checker_expr.kl — expression type checking
3. Write checker_stmt.kl — statement checking
4. Write checker_decl.kl — declaration checking (two-pass)
5. Write checker_main.kl — test harness + smoke tests

### PLAN.md Checklist (9.9)

- [ ] 9.9.1 — Scope management
- [ ] 9.9.2 — Expression typing
- [ ] 9.9.3 — Function call type checking
- [ ] 9.9.4 — Declaration checking
- [ ] 9.9.5 — Control flow checking
- [ ] 9.9.6 — Struct and method resolution
- [ ] 9.9.7 — Error reporting with source spans
