# Klar Language Bugs

Bugs encountered while implementing the JSON parser for Klar.

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

## Previously Fixed Bugs

The following bugs were fixed in the Klar compiler:

1. **Import path resolution** - Tests can now import from sibling directories
2. **String brace literals** - `"{"` and `"}"` now work in string literals
3. **String comparison codegen** - String equality after concatenation works
4. **If/else parse patterns** - Multiple if statements with char comparisons work
5. **`.to[T]` parse/type/codegen** - Fallible conversions now parse, type-check, and execute correctly
