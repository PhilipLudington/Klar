# Klar Compiler Bugs

Bugs discovered while implementing the JSON parser. Klar version: **0.3.1-dev**

---

## ~~Bug 7: Associated Functions on Structs with String Fields - LLVM Error~~ FIXED

**Status:** ✅ Fixed

**Description:** Calling associated functions (static methods like `Lexer.new()`) on structs caused LLVM verification failures when the struct also had instance methods.

**Root cause:** The codegen's `inferExprType` and `emitMethodCall` didn't handle static method calls where the object is a type name (e.g., `Counter.new()`) - they only handled instance method calls where the object is a variable.

**Fix:** Added detection for static method calls in both `inferExprType` (line ~6303) and `emitMethodCall` (line ~8195) in `src/codegen/emit.zig`.

---

## ~~Bug 8: Arrays Cannot Be Stored in Struct Fields~~ FIXED

**Status:** ✅ Fixed

**Description:** Storing `[char]` (or other slice types) as struct fields worked at parse time, but calling methods on them (like `.len()`) or chaining method calls (like `.len().to_string()`) failed at codegen time.

**Root cause:** Multiple issues in `src/codegen/emit.zig`:
1. `isSliceExpr()` didn't handle field access expressions - it only checked identifiers
2. `isIntegerExpr()` didn't handle field access expressions, so chained calls like `lexer.pos.to_string()` failed
3. The array/slice method handling in `emitMethodCall()` didn't create temporary allocas for non-identifier expressions

**Fix:**
- Added `getFieldType()` helper to look up the semantic type of struct fields
- Added `.field` case handling in `isSliceExpr()` and `isIntegerExpr()`
- Added `.len()` to the list of methods recognized as returning integers in `isIntegerExpr()`
- Fixed `emitMethodCall()` to create temporary allocas for slice values from field accesses

---

## ~~Bug 9: Pattern Matching on Tuple Elements Directly~~ FIXED

**Status:** ✅ Fixed

**Description:** `match result.0` was not supported. Had to extract tuple element to a variable first.

**Root cause:** In `emitMatchStmt()`, the code to resolve the subject's semantic type only handled direct identifier lookups (like `match x`). For field access expressions like `match tuple.0`, the fallback to the type checker wasn't working correctly.

**Fix:** Added handling for field access expressions in the subject type resolution in `emitMatchStmt()` (line ~4341 in `src/codegen/emit.zig`). When the match subject is a field access like `tuple.0`:
1. Look up the object's semantic type from `named_values`
2. If it's a tuple type, parse the field name as an index
3. Return the tuple element's type for pattern matching

**Test:** Added `test/native/match_tuple_element.kl` to verify pattern matching on tuple elements works with Result, Option, and integer types.

---

## Summary Table

| Bug | Feature | Severity | Status |
|-----|---------|----------|--------|
| 7 | Associated fn on structs | Blocking | ✅ Fixed |
| 8 | Arrays in struct fields | Blocking | ✅ Fixed |
| 9 | Match on tuple element | Minor | ✅ Fixed |

---

## Impact on JSON Parser

All blocking bugs have been fixed:
- ~~Must pass `[char]` as parameter instead of storing in Lexer struct (Bug 8)~~ Fixed!
- ~~Must use free functions instead of `Lexer.new()` (Bug 7)~~ Fixed!
- ~~Must extract tuple elements before pattern matching (Bug 9)~~ Fixed!
