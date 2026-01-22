# Bug Report: Klar 0.3.0-dev VM Runtime Issues

## Summary

~~Multiple VM runtime bugs prevent execution of complex programs.~~ All four bugs have been fixed.

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

## Bug 3: Native Codegen UnsupportedFeature for Field Access

### Status: FIXED

### Severity: Medium

### Description

Native compilation failed with "UnsupportedFeature" when accessing struct fields on non-identifier expressions (e.g., function call results).

### Reproduction (before fix)

```klar
struct Point { x: i32, y: i32 }

fn make_point(x: i32, y: i32) -> Point {
    return Point { x: x, y: y }
}

fn main() -> i32 {
    // This failed with UnsupportedFeature
    let x: i32 = make_point(10, 20).x
    return x
}
```

### Root Cause

The native codegen's `emitFieldAccess` function only supported field access on identifier expressions (variables). For non-identifier expressions like function calls, it returned `UnsupportedFeature`.

Additionally, `inferExprType` for struct literals was creating anonymous LLVM struct types instead of using registered named types, causing type mismatches.

### Fix

Modified `src/codegen/emit.zig`:
1. Added `getStructTypeNameFromExpr()` to extract struct type names from call expressions by looking up the function's return type
2. Extended `getStructTypeName()` to handle identifier expressions by looking up the variable's struct_type_name
3. Updated `inferExprType` for struct literals to return registered struct types
4. Updated `inferExprType` for field access to look up field types from struct_types cache

---

# Pre-existing Issues (Now Fixed)

The following issues were discovered during Bug 3 investigation. They were pre-existing limitations in the native codegen that have now been fixed.

---

## Issue 1: Nested Struct Field Access on Function Call Results

### Status: FIXED

### Severity: Low

### Description

Accessing a nested struct field directly from a function call result crashed during native codegen. When the field being accessed was itself a struct type (not a primitive), the generated LLVM IR caused a crash.

### Reproduction (before fix)

```klar
struct Point { x: i32, y: i32 }
struct Rectangle { origin: Point, width: i32, height: i32 }

fn make_rect() -> Rectangle {
    return Rectangle { origin: Point { x: 5, y: 6 }, width: 100, height: 200 }
}

fn main() -> i32 {
    // This crashed in native codegen - nested struct field access on function call
    let origin: Point = make_rect().origin
    return origin.x
}
```

### Root Cause

In `emitFieldAccess()`, the field type was extracted using `LLVMStructGetTypeAtIndex()`. For primitive types like `i32`, all instances are identical in LLVM. However, for struct types, `LLVMStructGetTypeAtIndex` may return a type reference that differs from the registered struct type in `struct_types`. LLVM struct types have identity semantics - the load failed due to type mismatch.

### Fix

Modified `src/codegen/emit.zig`:
1. Added `lookupFieldType()` function that uses the type checker to get the field's semantic type, then converts via `typeToLLVM()` to properly look up registered struct types
2. Added `lookupFieldStructTypeName()` to get a field's struct type name when the field is a struct type
3. Extended `getStructTypeNameFromExpr()` to handle field expressions by recursively looking up field types (enables chained access like `make_rect().origin.x`)
4. Updated `inferExprType()` for field expressions to use `getStructTypeNameFromExpr()` and `lookupFieldType()`
5. Updated both field access code paths in `emitFieldAccess()` to use `lookupFieldType()` with fallback to `LLVMStructGetTypeAtIndex()`

---

## Test Results

All 444 tests pass after fixes:
- Unit Tests: 220 passed
- Native Tests: 208 passed
- App Tests: 10 passed
- Module Tests: 6 passed

---

**Reported**: 2026-01-22
**Fixed**: 2026-01-22 (Bug 1, Bug 2, Bug 3, Issue 1)
**Open Issues**: None
**Klar Version**: 0.3.1-dev
