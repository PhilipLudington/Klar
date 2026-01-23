# Bug Report: Arrays of Structs Cause Codegen Error

**Status: FIXED** (2026-01-22)

## Summary

Arrays containing struct types pass type checking but fail at code generation with "UnsupportedFeature" error. This affects both fixed-size (`[T; N]`) and dynamic (`[T]`) array syntax.

## Root Cause

The `getStructTypeNameFromExpr` function in `src/codegen/emit.zig` didn't handle `.index` expressions. When accessing a field on an array element (e.g., `items[0].id`), the codegen couldn't determine the struct type name of the indexed element, causing it to fall through to `UnsupportedFeature`.

## Fix

Added handling for `.index` expressions in `getStructTypeNameFromExpr`. The fix:
1. Extracts the array variable name from the index expression
2. Looks up the variable's `array_element_type` in local values
3. Falls back to the type checker's global scope if needed
4. Returns the struct name if the element type is a struct

## Test Coverage

Added `test/native/array_of_structs.kl` covering:
- Dynamic arrays of structs
- Fixed-size arrays of structs
- Field access on array elements
