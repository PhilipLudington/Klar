# Resume: Milestone 9.6 — Self-Hosted Parser

## Current State

All compiler and self-hosted parser bugs from QA review are fixed. The two-phase module checking now has proper circular import warnings, robust error handling, and no duplicate diagnostics. The self-hosted parser has fixes for impl method modifiers, negative float patterns, JSON escaping, brace handling, and integer overflow.

**Test results:** 694 passed, 1 failed (pre-existing `ast` selfhost inline test `NotImplemented`).

## What Was Done

### Session 1: Circular Import Support + Parser Declaration Loss (COMPLETE)

**Circular Import Support:**
- `src/module_resolver.zig` — `topoSort` returns on cycles instead of erroring
- `src/checker/checker.zig` — Two-phase compilation: `checkModuleDeclarations` (Phase 1) + `checkModuleBodies` (Phase 2)
- `src/main.zig` — All 5 compilation loops updated to use two-phase approach

**Parser Declaration Loss — String Interpolation Bug:**
- `src/parser.zig` — `parseExpressionFromString` returns `?ast.Expr` instead of `ParseError!ast.Expr`; falls back to literal string on parse failure

### Session 2: QA Review Fixes (COMPLETE)

Comprehensive QA review found 10 issues across the changes. All 10 fixed:

#### Zig Compiler Fixes

1. **Circular import warning restored** (`module_resolver.zig`, `main.zig`)
   - Added `warnCircularImports()` helper that runs `detectCycle()` after topo sort
   - Emits "Warning: circular import detected involving '...'" to stderr
   - Called at all 5 compilation entry points (interpreter, build, check, test, VM)
   - Compilation proceeds (two-phase handles the cycles), but user is informed

2. **`.?` panic in `registerModuleExports` fixed** (`checker.zig`)
   - Function and const_decl lookups used `.?` which panics on null
   - Replaced with `orelse continue` — skips export if symbol not in scope (OOM recovery)

3. **`saveModuleScope` error logging** (`checker.zig`)
   - `catch {}` → `catch |err| log.err(...)` on save
   - Added `log.warn` when `restoreModuleScope` finds no saved entry

4. **Duplicate error suppression in Phase 2** (`checker.zig`)
   - `importSpecificSymbol`: guarded "symbol not found" and "not public" errors with `skip_existing_imports`
   - `processImport`: guarded namespace re-registration with `lookupLocal` check
   - Prevents doubled diagnostics when imports are re-processed in Phase 2

5. **`async_function_names` staleness** (`checker.zig`)
   - Added `clearRetainingCapacity` + re-population at start of `checkModuleBodies`
   - Prevents cross-module leakage of async function name sets

#### Self-Hosted Parser Fixes

6. **`pub`/`unsafe` on impl methods** (`parser_decl.kl`)
   - `parse_impl_stub` body loop now checks for `pub_` and `unsafe_` tokens before `fn_`
   - Passes modifiers through to `parse_function_decl`

7. **Negative float patterns** (`parser_stmt.kl`)
   - Added `float_literal` handling after minus token in `parse_pattern`
   - Added explicit error message for non-numeric token after `-` in patterns

8. **JSON control character escaping** (`parser.kl`)
   - `json_str` now escapes `\b` (backspace), `\f` (formfeed)
   - All other control chars below 0x20 escaped as `\u00XX`
   - Added `hex_digit` helper function

9. **`parse_impl_stub` nested brace handling** (`parser_decl.kl`)
   - Non-fn items with `{` blocks now use `skip_braces` (depth-aware)
   - Previously single-token `advance` could eat the outer impl `}`

10. **`parse_int_value` overflow** (`parser_expr.kl`)
    - Base-10 literals now strip underscores/suffixes directly from text (no numeric computation)
    - Non-decimal bases still compute via i64 (sufficient for practical cases)

## Architecture

### Two-Phase Module Checking

**How it works:**
1. Phase 1: For each module in topo order, register type declarations + function signatures (with `skip_missing_modules=true` for imports not yet registered). Export symbols. Save module scope.
2. Phase 2: For each module, restore saved scope, re-process imports (with `skip_existing_imports=true` to avoid conflicts), check function bodies.

**TypeChecker fields:**
- `skip_missing_modules: bool` — Phase 1 lenient import mode
- `skip_existing_imports: bool` — Phase 2 re-import mode
- `module_scope_map: AutoHashMap(*const ModuleInfo, *Scope)` — saved scopes for Phase 2 restore

**TypeChecker methods:**
- `checkModuleDeclarations(module)` — Phase 1 (imports + types + signatures)
- `checkModuleBodies(module)` — Phase 2 (re-imports + bodies)
- `saveModuleScope(mod)` / `restoreModuleScope(mod)` — scope save/restore

### File Split Architecture

```
selfhost/parser.kl       — Core infrastructure (21 fns)
selfhost/parser_expr.kl  — Expression parsing (34 fns)
selfhost/parser_type.kl  — Type parsing (7 fns)
selfhost/parser_stmt.kl  — Statements, blocks, patterns (21 fns)
selfhost/parser_decl.kl  — Declarations + parse_module (13 fns)
selfhost/parser_main.kl  — Entry point: main, smoke tests (2 fns + 1 test)
```

### Dependency Graph (with circular imports)

```
parser.kl (core)  ← imported by all others
     ↑
parser_type.kl ←→ parser_expr.kl ←→ parser_stmt.kl
     ↑                  ↑                  ↑
     └──────────── parser_decl.kl ─────────┘
                        ↑
                  parser_main.kl
```

Circular imports: `parser_expr ↔ parser_type`, `parser_expr ↔ parser_stmt`.

### Topological Order (DFS)

With cycle-tolerant topoSort, the compilation order is:
1. lexer → 2. parser → 3. parser_type → 4. parser_stmt → 5. parser_expr → 6. parser_decl → 7. parser_main

In Phase 1, `parser_type` skips `import parser_expr` (not yet registered). In Phase 2, the import resolves.

## Remaining Tasks

### Immediate: Runtime Execution
1. **Fix interpreter `UndefinedVariable`** — `klar run selfhost/parser_main.kl --interpret` fails at runtime even though type checking passes. The interpreter can't find `run_smoke_tests` at runtime. This is likely a scope/binding issue in the interpreter for multi-module programs.
2. **Fix codegen `UnsupportedFeature`** — `klar run selfhost/parser_main.kl` (native) fails because the LLVM codegen doesn't support some feature used by the selfhost parser (likely cross-module tuple types).
3. **Fix test block error** — `klar check` reports `test 'smoke' references missing function 'smoke'` for `parser_main.kl`. The test block syntax may need investigation.

### After Runtime Works
4. **Run split parser:** `./zig-out/bin/klar run selfhost/parser_main.kl` → should print "parser.kl: all smoke tests passed"
5. **Parity testing:** compare `parser_main.kl` output against `klar dump-ast` for test corpus
6. **Add Phase 3** to `scripts/run-selfhost-tests.sh`

### Known Limitations (from QA review, deferred)
- `parse_int_value`: hex/binary/octal literals still use i64 computation (overflow possible for values > i64 max)
- `is_type_args_context`: uppercase identifier heuristic can misidentify `foo[Bar]` as type args (fundamental ambiguity)
- `process_string_escapes`: `\u` and `\x` escape sequences not handled (falls through to unknown escape)
- `final_expr` detection in `parse_block` uses fragile JSON prefix string matching (acknowledged by COUPLING comment)

## Key Files

| File | Status |
|------|--------|
| `src/parser.zig` | Modified — fixed string interpolation false positive |
| `src/module_resolver.zig` | Modified — cycle-tolerant topoSort |
| `src/checker/checker.zig` | Modified — two-phase compilation, error handling, duplicate suppression |
| `src/main.zig` | Modified — two-phase loops + `warnCircularImports` at all 5 entry points |
| `selfhost/parser.kl` | Modified — json_str control char escaping |
| `selfhost/parser_decl.kl` | Modified — impl pub/unsafe + brace depth handling |
| `selfhost/parser_stmt.kl` | Modified — negative float patterns |
| `selfhost/parser_expr.kl` | Modified — base-10 int literal text passthrough |
| `selfhost/parser_main.kl` | Type check PASSES, runtime blocked by interpreter/codegen bugs |
