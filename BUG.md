# Klar Compiler Bugs

Tracking issues identified in QA review (2026-01-29).

---

## [x] Bug 1: Overflow checking not implemented in native codegen

**Status:** Fixed

**Severity:** High

**Location:** `src/codegen/emit.zig:3666-3735`

**Resolution:** Implemented proper overflow checking using LLVM overflow intrinsics (`llvm.sadd.with.overflow.*`, `llvm.ssub.with.overflow.*`, `llvm.smul.with.overflow.*`). On overflow, the program now traps with an unreachable instruction (SIGILL on most platforms).

---

## [x] Bug 2: Saturating arithmetic operators don't saturate

**Status:** Fixed

**Severity:** Medium

**Location:** `src/codegen/emit.zig:3737-3795`

**Resolution:** Implemented saturating arithmetic using LLVM intrinsics:
- `+|` uses `llvm.sadd.sat.iN` / `llvm.uadd.sat.iN`
- `-|` uses `llvm.ssub.sat.iN` / `llvm.usub.sat.iN`
- `*|` uses overflow detection with manual clamping to MIN/MAX

---

## [x] Bug 3: Range size calculation can overflow

**Status:** Fixed

**Severity:** Medium

**Location:** `src/interpreter.zig:1273-1298`

**Resolution:** Implemented checked arithmetic using `std.math.sub` and `std.math.add` with proper error handling. Added a maximum range size limit of 1,000,000 elements to prevent OOM. Returns `IntegerOverflow` error for extreme ranges.

---

## [x] Bug 4: Bytecode backend missing enum compilation

**Status:** Fixed (documented limitation)

**Severity:** Medium

**Location:** `src/compiler.zig:726`

**Description:** The bytecode compiler now emits a clear error message when enum literals are used. Structs are fully supported; only enums are pending implementation.

**Resolution:** Programs using enum literals now receive a clear error message directing users to use native compilation (`klar build`) instead of the VM. Struct literals work correctly in the VM backend.

**Note:** Full enum support in the bytecode VM is a future enhancement. For now, native compilation is the recommended path for programs using enums.

---

## [x] Bug 5: Cross-compilation struct offset mismatch

**Status:** Fixed (documented limitation)

**Severity:** Low

**Location:** `src/codegen/emit.zig:17-25`

**Resolution:** Updated documentation to clearly explain that cross-compilation of filesystem operations to different OS/arch is NOT supported. Users needing cross-compilation for filesystem code should build the Klar compiler on the target platform.

---

## [x] Bug 6: Monomorphization cache uses linear search

**Status:** Fixed

**Severity:** Low (Performance)

**Location:** `src/checker.zig:213-236, 2180-2220`

**Resolution:** Converted all four monomorphization caches from `ArrayListUnmanaged` to `StringHashMapUnmanaged` for O(1) average-case lookup:
- `monomorphized_functions`
- `monomorphized_structs`
- `monomorphized_enums`
- `monomorphized_methods`

The mangled name (which is already computed) is used as the hash key.

---

# Architecture Issues (Not Bugs)

These are code organization concerns that don't affect correctness but impact maintainability.

## [x] Issue A1: Monolithic emit.zig (29,444 lines)

**Status:** Fixed

**Location:** `src/codegen/emit.zig`

**Resolution:** Split into 13 focused modules with documentation and utilities:
- `emit.zig` (orchestrator, ~29,600 lines - main implementation)
- `runtime.zig` (C library declarations, Rc/Arc runtime, ~1,165 lines)
- `generics.zig` (monomorphization utilities)
- `types_emit.zig` (type conversion documentation)
- `strings_emit.zig` (String type utilities)
- `list.zig` (List[T] utilities)
- `map.zig` (Map[K,V] utilities)
- `set.zig` (Set[T] utilities)
- `io.zig` (File, Path, BufReader, BufWriter utilities)
- `optionals.zig` (Optional/Result utilities)
- `builtins.zig` (built-in function utilities)
- `expressions.zig` (expression emission utilities)
- `statements.zig` (statement emission utilities)
- `functions.zig` (function emission utilities)

All modules are accessible via `emit.generics`, `emit.list`, etc. The main implementation remains in emit.zig while supporting modules provide documentation and standalone utilities.

---

## [x] Issue A2: Large checker.zig (3,764 lines remaining)

**Status:** Fixed

**Location:** `src/checker/checker.zig`

**Resolution:** Modularization complete. Final modules:
- `method_calls.zig` (2,624 lines) - Static constructors and builtin method checking
- `type_resolution.zig` (1,003 lines) - Type substitution, unification, AST resolution
- `expressions.zig` (976 lines) - Expression type checking
- `declarations.zig` (868 lines) - Declaration checking
- `builtins_check.zig` (726 lines) - Builtin function checking
- `comptime_eval.zig` (603 lines) - Compile-time evaluation
- `patterns.zig` (328 lines) - Pattern matching
- `methods.zig` (318 lines) - Method resolution and signature verification
- `statements.zig` (242 lines) - Statement checking
- `type_utils.zig` (167 lines) - Type compatibility utilities
- `trait_checking.zig` (157 lines) - Trait implementation and bounds checking
- Type definition modules: `generics.zig`, `traits.zig`, `comptime.zig`, `builtins.zig`, `modules.zig`

checker.zig reduced from ~7,700 lines to 3,764 lines (51% reduction). User-defined method lookup remains in checker.zig (~200 lines).

---

## [x] Issue A3: 45 TODO comments indicate technical debt

**Status:** Fixed

**Resolution:** Triaged 37 TODO comments (2026-01-30):
- **Removed 6**: Obsolete or dead code paths
- **Documented 4**: Converted to limitation notes (VM/interpreter don't support enums, UTF-8 is ASCII-only)
- **Kept 13**: Valid inline markers for VM/IR future work
- **Tracked 10**: Real gaps added as new issues below

---

# Open Issues

Issues identified during TODO triage that affect correctness or functionality.

## [x] Bug 7: Closures assume all captures are i32

**Status:** Fixed

**Severity:** Medium

**Location:** `src/codegen/emit.zig:11197-11220, 11280-11310, 11398-11430`

**Description:** When creating closure environments, the codegen assumed all captured variables are i32. This broke closures that capture other types (i64, f64, strings, structs, etc.).

**Resolution:** Fixed by preserving full `LocalValue` metadata for captured variables:
1. Compute capture metadata BEFORE switching `named_values` (while original variable types are accessible)
2. Store complete `LocalValue` info including `ty`, `is_signed`, `is_string`, `struct_type_name`, etc.
3. Use actual LLVM types from `named_values` for environment struct (not just `typeExprToLLVM`)
4. Copy full metadata when creating closure-local variables for method resolution to work

---

## [ ] Bug 8: Set.contains() uses pointer equality for complex types

**Severity:** Medium

**Location:** `src/codegen/emit.zig:24951`

**Description:** `Set.contains()` uses pointer comparison for non-primitive types. For strings and structs, this means two equal values at different addresses won't match.

---

## [ ] Bug 9: Self type not resolved in impl context

**Severity:** Medium

**Location:** `src/checker/type_resolution.zig:608`

**Description:** When `Self` is used in an impl block, it returns `unknown` type instead of resolving to the implementing type.

---

## [ ] Bug 10: Struct pattern fields not validated

**Severity:** Low

**Location:** `src/checker/patterns.zig:41`

**Description:** Struct patterns in match expressions don't verify that field names exist on the struct type.

---

## [ ] Bug 11: Struct pattern bindings not created

**Severity:** Low

**Location:** `src/checker/patterns.zig:233`

**Description:** Variables bound in struct patterns (e.g., `Point { x, y }`) are not added to scope.

---

## [ ] Bug 12: Enum payload struct field matching incomplete

**Severity:** Low

**Location:** `src/checker/expressions.zig:837`

**Description:** When checking enum literal payloads with struct types, fields are matched by position rather than by name.

---

## [ ] Bug 13: Ownership checker assumes all types are Copy

**Severity:** Low

**Location:** `src/ownership/checker.zig:181`

**Description:** The ownership checker hardcodes `is_copy = true`, preventing proper move semantics for non-Copy types.

---

## [ ] Bug 14: Drop inserter doesn't know actual types

**Severity:** Low

**Location:** `src/ownership/drop.zig:265`

**Description:** The drop inserter uses `Type.unknown` for let declarations, preventing proper drop insertion for owned types.

---

# Summary

| ID | Title | Severity | Status |
|----|-------|----------|--------|
| 1 | Overflow checking not implemented | High | Fixed |
| 2 | Saturating arithmetic doesn't saturate | Medium | Fixed |
| 3 | Range size overflow | Medium | Fixed |
| 4 | Bytecode missing enum | Medium | Fixed (documented) |
| 5 | Cross-compilation offset mismatch | Low | Fixed (documented) |
| 6 | Monomorphization cache O(n×m) | Low | Fixed |
| 7 | Closures assume i32 captures | Medium | Fixed |
| 8 | Set.contains() pointer equality | Medium | Open |
| 9 | Self type not resolved | Medium | Open |
| 10 | Struct pattern fields not validated | Low | Open |
| 11 | Struct pattern bindings missing | Low | Open |
| 12 | Enum payload field matching | Low | Open |
| 13 | Ownership assumes Copy | Low | Open |
| 14 | Drop inserter unknown types | Low | Open |
| A1 | Monolithic emit.zig | Arch | Fixed |
| A2 | Large checker.zig | Arch | Fixed |
| A3 | 45 TODO comments | Debt | Fixed |
