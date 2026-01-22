# Bug Report: Klar 0.3.0-dev VM Runtime Issues

## Summary

~~Multiple VM runtime bugs prevent execution of complex programs.~~ Two critical bugs have been fixed. One medium-severity bug remains.

## Environment

- **Klar Version**: 0.3.1-dev
- **Platform**: macOS (Darwin 24.6.0)

---

## Bug 1: Struct Field Corruption Across Loop Iterations

### Status: FIXED

### Severity: Critical

### Description

Struct fields became corrupted after approximately 11-12 iterations of a loop. Fields that should contain `f64` values instead returned `<object>`.

### Root Cause

The VM was creating `ObjOptional` objects using the raw allocator instead of the GC allocator. This caused memory leaks and potential corruption when leaked memory was reused.

### Fix

Changed all `ObjOptional.createNone()` and `ObjOptional.createSome()` calls in `src/vm.zig` to use `ObjOptional.createNoneGC()` and `ObjOptional.createSomeGC()` respectively. This ensures optionals are properly tracked and freed by the garbage collector.

---

## Bug 2: Optional Type Methods Cause TypeError

### Status: FIXED

### Severity: High

### Description

Calling `.is_some()` on optional types (`?T`) returned from user-defined functions caused a TypeError at runtime.

### Root Cause

The bytecode compiler was not wrapping return values in `Some` when returning from functions with optional return types. The value was returned as a raw value instead of an `ObjOptional`, so `.is_some()` failed because the value wasn't recognized as an optional type.

### Fix

Modified `src/compiler.zig` to:
1. Track whether the current function has an optional return type (`returns_optional` field)
2. Emit `op_some` to wrap return values in functions with optional return types
3. Emit `op_none` for empty returns in optional-returning functions

---

## Bug 3: Native Codegen UnsupportedFeature

### Status: OPEN

### Severity: Medium (VM workaround available)

### Description

Native compilation (`klar run` without `--vm`) fails with "UnsupportedFeature" for fixed-size arrays.

### Reproduction

```
$ klar run src/main.kl
Codegen error: UnsupportedFeature
```

### Workaround

Use VM mode: `klar run src/main.kl --vm`

---

## Test Results

All 443 tests pass after fixes:
- Unit Tests: 220 passed
- Native Tests: 207 passed
- App Tests: 10 passed
- Module Tests: 6 passed

---

**Reported**: 2026-01-22
**Fixed**: 2026-01-22 (Bug 1, Bug 2)
**Klar Version**: 0.3.1-dev
