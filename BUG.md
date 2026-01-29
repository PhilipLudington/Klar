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

## [ ] Issue A1: Monolithic emit.zig (29,444 lines)

**Location:** `src/codegen/emit.zig`

**Recommendation:** Split into ~8-10 focused modules:
- `emit.zig` (orchestrator)
- `expressions.zig`
- `statements.zig`
- `types.zig`
- `functions.zig`
- `generics.zig`
- `builtins.zig`
- `ffi.zig`

---

## [ ] Issue A2: Large checker.zig (11,424 lines)

**Location:** `src/checker.zig`

**Recommendation:** Split into focused modules:
- `checker_core.zig`
- `checker_traits.zig`
- `checker_generics.zig`
- `checker_comptime.zig`
- `checker_modules.zig`

---

## [ ] Issue A3: 45 TODO comments indicate technical debt

**Locations:** Various files

**Recommendation:** Resolve or convert to tracked issues.

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
| A1 | Monolithic emit.zig | Arch | Open |
| A2 | Large checker.zig | Arch | Open |
| A3 | 45 TODO comments | Debt | Open |
