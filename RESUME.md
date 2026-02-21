# Resume: Milestone 9.6 — Self-Hosted Parser

## Current State

All compiler and self-hosted parser bugs from QA review are fixed. The two-phase module checking now has proper circular import warnings, robust error handling, and no duplicate diagnostics. The self-hosted parser has fixes for impl method modifiers, negative float patterns, JSON escaping, brace handling, and integer overflow. Native codegen now supports multi-module programs with cross-module struct field access and proper function declaration ordering.

**Test results:** 695 passed, 0 failed.

**Self-hosted parser runs natively:** `klar run selfhost/parser_main.kl` → "parser.kl: all smoke tests passed"

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

### Session 3: Native Codegen for Multi-Module Programs (COMPLETE)

Fixed three bugs preventing native compilation of multi-module Klar programs:

1. **Cross-module struct field resolution** (`checker.zig`, `emit.zig`)
   - Root cause: `lookupFieldStructTypeName`, `lookupFieldType`, `getFieldType` used `tc.global_scope.lookup()` which only searches the current module's scope
   - Fix: Added `lookupSymbolAcrossModules()` to TypeChecker that searches current scope + all saved module scopes. Updated all three emitter functions to use it.
   - Symptom: `UnsupportedFeature` error on `p.current.kind` (chained field access through cross-module struct types)

2. **Cross-module function declaration ordering** (`emit.zig`, `main.zig`)
   - Root cause: `emitModule` bundles function declaration + body emission per-module. With circular imports, module A's body can call module B's function before B is emitted.
   - Fix: Split into `declareModuleFunctions()` + `emitModuleBodies()`. Multi-module compilation now declares ALL functions across ALL modules first, then emits all bodies.
   - Symptom: LLVM verification error — cross-module calls dispatched as closures (wrong calling convention)

3. **Multi-module main conflict** (`emit.zig`, `main.zig`)
   - Root cause: Multiple modules can define `main()` — codegen declared all of them, linker picked the wrong one
   - Fix: Added `skip_main` flag to emitter. Non-entry modules (all except the last in compilation order) skip `main` declaration and emission.
   - Symptom: Lexer's `main` ran instead of `parser_main`'s

### Session 4: QA Review of Session 3 Changes (COMPLETE)

QA review of the multi-module codegen changes found 5 issues. All addressed:

1. **`emitMainArgsWrapper` called N times in multi-module path** (`emit.zig`, `main.zig`)
   - `emitModuleBodies()` contained a `emitMainArgsWrapper()` call inherited from the `emitModule()` extraction
   - In multi-module path, `emitModuleBodies` is called once per module, so the wrapper would be emitted N times
   - Fix: Removed wrapper call from `emitModuleBodies` (now a pure body-emission function). Multi-module path in `main.zig` calls `emitMainArgsWrapper()` once after all modules are emitted.

2. **`registerStructDecl` skipped in multi-module path** (`main.zig`)
   - Multi-module path called `declareModuleFunctions` + `emitModuleBodies` but never registered struct declarations
   - `emitModule()` has an explicit first pass for struct registration, but multi-module path bypassed it
   - Fix: Added Phase 1 struct registration loop in `main.zig` before function declarations. Multi-module codegen now has 3 phases: struct registration, function declaration, body emission.

3. **`lookupSymbolAcrossModules` non-deterministic on name collisions** (`checker.zig`)
   - `module_scope_map.valueIterator()` order is non-deterministic (hash map)
   - Safe in practice: current scope is checked first (deterministic priority), fallback is for unimported type references
   - Fix: Added doc comment explaining the safety rationale and limitation.

4. **`skip_main` state between loops** (`main.zig`)
   - Informational only — per-iteration assignment in both loops is already correct
   - No code change needed.

5. **`registerStructDecl` and `emitMainArgsWrapper` visibility** (`emit.zig`)
   - Both methods were private (`fn`) but now called from `main.zig` in the multi-module path
   - Fix: Made both `pub` with doc comments explaining why.

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

### Completed
1. ~~**Fix interpreter `UndefinedVariable`**~~ — FIXED. `klar run selfhost/parser_main.kl --interpret` runs and prints "parser.kl: all smoke tests passed".
2. ~~**Fix codegen `UnsupportedFeature`**~~ — FIXED. Three bugs fixed:
   - Cross-module struct field resolution: `lookupFieldStructTypeName`, `lookupFieldType`, `getFieldType` now use `lookupSymbolAcrossModules` instead of `global_scope.lookup`
   - Cross-module function declaration ordering: multi-module codegen now declares ALL functions before emitting ANY bodies (split into `declareModuleFunctions` + `emitModuleBodies`)
   - Multi-module main conflict: `skip_main` flag prevents non-entry modules from declaring/emitting their `main` functions
3. ~~**Run split parser natively**~~ — DONE. `klar run selfhost/parser_main.kl` prints "parser.kl: all smoke tests passed"

4. ~~**Fix test block error**~~ — FIXED. `test smoke` renamed to `test run_smoke_tests` in `parser_main.kl`. Klar's `test <name>` syntax requires the name to reference an existing function; `smoke` didn't exist but `run_smoke_tests` does.

### Remaining
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
| `src/checker/checker.zig` | Modified — two-phase compilation, error handling, duplicate suppression, `lookupSymbolAcrossModules` |
| `src/main.zig` | Modified — two-phase loops, `warnCircularImports`, 3-phase multi-module codegen (structs, declarations, bodies + wrapper) |
| `src/codegen/emit.zig` | Modified — cross-module type resolution, split declaration/body emission, `skip_main`, `emitModuleBodies` no longer emits wrapper |
| `selfhost/parser.kl` | Modified — json_str control char escaping |
| `selfhost/parser_decl.kl` | Modified — impl pub/unsafe + brace depth handling |
| `selfhost/parser_stmt.kl` | Modified — negative float patterns |
| `selfhost/parser_expr.kl` | Modified — base-10 int literal text passthrough |
| `selfhost/parser_main.kl` | FULLY WORKING — interpreter and native compilation both succeed |
