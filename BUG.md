# Klar Language Bugs

Bugs encountered while implementing various features in Klar.

---

## [x] Bug 1: Import path resolution from subdirectories

**Status:** Fixed
**Severity:** High
**Discovered:** 2026-01-27

Tests in a `tests/` subdirectory cannot import modules from sibling directories like `lib/`. Fixed by improving module resolution.

---

## [x] Bug 2: String interpolation triggers on brace literals

**Status:** Fixed
**Severity:** Medium
**Discovered:** 2026-01-27

String literals containing `{` were incorrectly parsed as string interpolation. Fixed by validating interpolation content.

---

## [x] Bug 3: LLVM codegen error with string variable comparisons

**Status:** Fixed
**Severity:** High
**Discovered:** 2026-01-27

String equality comparisons involving variables built through concatenation caused LLVM verification failures. Fixed by properly handling string struct types vs pointer types in codegen.

---

## [x] Bug 4: Parse error with certain if/else patterns in functions

**Status:** Fixed
**Severity:** Medium
**Discovered:** 2026-01-27

Certain combinations of if/else statements with brace character literals caused parse errors. Fixed by improved brace handling in string interpolation validation.

---

## [x] Bug 5: Parse error with `.to[T]` generic method syntax

**Status:** Fixed
**Severity:** High
**Discovered:** 2026-01-27
**Fixed:** 2026-01-27

### Description

The `.to[T]` generic method syntax caused a parse error (`error.UnexpectedToken`). This affected any use of the `.to[T]` conversion method, regardless of the source or target type.

### Root Cause

The parser handled `.as[T]` and `.trunc[T]` specially (without requiring parentheses), but `.to[T]` was not included. When the parser encountered `.to[T]`, it fell through to generic method parsing which required `method[T]()` with parentheses, causing the `UnexpectedToken` error.

Additionally, the type checker returned `T` for all conversion methods, but `.to[T]` should return `?T` (optional) since it's a fallible conversion.

### Fix

Three changes were required:

1. **Parser** (`src/parser.zig`): Added special handling for `.to[T]` in `parseFieldOrMethod`, similar to `.as[T]` and `.trunc[T]`. The new `parseFallibleConversion` function parses `.to[T]` and creates a MethodCall node with type_args and empty args.

2. **Type Checker** (`src/checker.zig`): Modified `checkMethodCall` to return `?T` (optional type) for `.to` method calls, instead of returning `T` directly like `.as` and `.trunc`.

3. **Code Generator** (`src/codegen/emit.zig`): Added `emitStringToNumeric` function to handle string-to-numeric conversions. This function:
   - Uses `strtol` for integer conversions
   - Uses `strtod` for float conversions
   - Checks if the entire string was consumed (valid parse)
   - Returns `Some(value)` on success, `None` on failure

### Test Cases

```klar
// Valid integer conversion
let n: ?i32 = "42".to[i32]     // Some(42)

// Invalid conversion returns None
let bad: ?i32 = "abc".to[i32]  // None

// Negative numbers work
let neg: ?i32 = "-100".to[i32] // Some(-100)

// Float conversion
let f: ?f64 = "3.14".to[f64]   // Some(3.14)
```

---

## [x] Bug 6: Cannot access array elements via struct field

**Status:** Fixed
**Discovered:** Phase 8 FFI integration testing
**Category:** Parser

The parser now correctly handles `struct.array_field[index]` syntax.

```klar
pub struct Message {
    data: [i32; 4],
}

fn main() -> i32 {
    let m: Message = Message { data: [10, 20, 30, 40] }
    let x: i32 = m.data[0]  // Works correctly, returns 10
    return x
}
```

---

## [x] Bug 7: Type::method() syntax not working for structs

**Status:** Fixed
**Discovered:** Phase 8 FFI integration testing
**Category:** Type Checker

The `Type::method()` syntax now works for both enums and structs.

```klar
pub struct Message {
    label: u64,
}

impl Message {
    pub fn new(label: u64) -> Message {
        return Message { label: label }
    }
}

fn main() -> i32 {
    let msg: Message = Message::new(100.as[u64])  // Works correctly
    return 0
}
```

---

## [x] Bug 8: Sized extern types not fully supported in method parameters

**Status:** Fixed
**Discovered:** Phase 8 FFI integration testing
**Category:** Codegen

Sized extern types (`extern type(N)`) now work correctly in impl methods with `ref Self` parameters.

**Root cause:** Two issues in `namedTypeToLLVM`:
1. Extern types weren't being looked up in the type checker, causing fallback to `i32`
2. When computing `reference_inner_type` for self parameters, "Self" wasn't resolved to the actual struct type

**Fix:**
- Added extern type lookup via `type_checker.lookupType()` in `namedTypeToLLVM`
- For self parameters in impl methods, use the struct's LLVM type directly instead of trying to resolve "Self"

```klar
extern type SeL4MessageInfo(8)

struct Endpoint { cap: u64 }

impl Endpoint {
    pub fn send(self: ref Self, info: SeL4MessageInfo) -> void {
        unsafe {
            seL4_Send(self.cap, info)  // Now works correctly
        }
    }
}
```

---

## [x] Bug 9: Extern type allocations not freed

**Status:** Fixed
**Discovered:** Phase 8 FFI integration testing
**Category:** Memory

**Root cause:** `ExternType` objects created in `checkExternType` were allocated but not tracked for deallocation.

**Fix:**
- Added `extern_types: std.ArrayListUnmanaged(*types.ExternType)` field to TypeChecker
- Track allocations in `checkExternType` by appending to the list
- Free allocations in `TypeChecker.deinit()`

No more memory leak warnings in debug builds.

---

## Summary of Fixed Bugs

All bugs have been verified as fixed:

1. **Import path resolution** - Tests can now import from sibling directories
2. **String brace literals** - `"{"` and `"}"` now work in string literals
3. **String comparison codegen** - String equality after concatenation works
4. **If/else parse patterns** - Multiple if statements with char comparisons work
5. **`.to[T]` parse/type/codegen** - Fallible conversions now parse, type-check, and execute correctly
6. **Array element via struct field** - `struct.array[i]` syntax works
7. **Type::method() for structs** - Static method calls work for structs
8. **Extern types in methods** - Sized extern types work in impl methods
9. **Extern type memory** - No more memory leaks from extern type allocations
