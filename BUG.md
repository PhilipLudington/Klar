# Klar Compiler Bugs

Bugs discovered while implementing the JSON parser. Klar version: **0.3.1-dev**

---

## ~~Bug 1: Result in Tuple Returns~~ - FIXED

**Status:** Fixed

**Description:** Returning a tuple containing a `Result[T, E]` and a struct caused a codegen error.

**Fix:** Propagate type context through tuple literals in both type checker (`checkTupleLiteralWithHint`) and codegen (`emitTupleLiteral`). This allows `Ok()`/`Err()` constructors inside tuples to infer the correct Result type.

---

## ~~Bug 2: Pattern Matching on Result/Option - Parse Error~~ - FIXED

**Status:** Fixed

**Description:** Pattern matching with `Ok(value) =>` or `Err(e) =>` now works correctly.

**Fix:**
1. Parser: Added support for shorthand variant patterns like `Ok(v)` without requiring explicit type prefix
2. Type Checker: Added handling for Result and Optional types in pattern matching
3. Codegen: Added proper handling for Result/Optional tag types (i1 instead of i8) and payload extraction

**Note:** `Some(v)` patterns work for pattern matching, but the `Some()` constructor (Bug 5) is still not implemented. Use implicit wrapping via function returns or `?` operator instead.

---

## ~~Bug 3: Struct with String Field - Method Returns Wrong `string.len()`~~ - FIXED

**Status:** Fixed

**Description:** When a struct contains a `string` field and is passed to a method or function, calling `.len()` on the string field returns 0 instead of the actual length.

**Fix:** The `isStringExpr` function in codegen wasn't recognizing field access expressions as string types. Added explicit handling for `.field` expressions that looks up the struct's field type from the type checker's registered struct types. Also extended to check monomorphized struct instances for generic structs (e.g., `Container[T]` instantiated as `Container$i32`).

---

## ~~Bug 4: Impl Methods on Structs - Wrong Parameter Type~~ - FIXED

**Status:** Fixed

**Description:** Calling impl methods on structs that contain non-primitive fields caused LLVM verification failure. The compiler was passing only the first field instead of the entire struct.

**Fix:** This was fixed as part of earlier codegen improvements. The method call emission now correctly loads and passes the entire struct value to impl methods.

---

## ~~Bug 5: Some() Constructor Not Recognized~~ - FIXED

**Status:** Fixed

**Description:** The `Some(value)` and `None()` constructors for optional types are now recognized.

**Usage:**
```klar
fn main() -> i32 {
    let opt1: ?i32 = Some(42)    // Works!
    let opt2: ?i32 = None()      // Works! (requires parentheses)
    let opt3: ?string = Some("hello")

    // Pattern matching works
    match opt1 {
        Some(v) => { println(v.to_string()) }
        None => { println("none") }
    }

    // Default operator works
    let val: i32 = opt1 ?? 0  // Returns 42
    return 0
}
```

**Fix:**
1. Type checker: Added `Some`/`None` as builtin functions in `initBuiltins()` and `checkSomeNoneCall()` for type inference
2. Codegen: Added `emitSomeCall()`/`emitNoneCall()` dispatchers and `getOptionalType()` helper
3. Type inference: Added handling for `Some`/`None` in `inferExprType()` to return proper `{ i1, T }` struct type

---

## ~~Bug 6: Tuple with Struct and Primitive~~ - FIXED

**Status:** Fixed (same fix as Bug 1)

**Description:** Returning a tuple containing a struct and a primitive type (like i32) caused a codegen error. This was fixed along with Bug 1 by propagating type context through tuple literals.

---

## Summary Table

| Bug | Feature | Severity | Status |
|-----|---------|----------|--------|
| 1 | Result in tuple return | Blocking | **FIXED** |
| 2 | Pattern matching Result/Option | Blocking | **FIXED** |
| 3 | String field in struct | Blocking | **FIXED** |
| 4 | Impl methods on mixed structs | Blocking | **FIXED** |
| 5 | Some()/None() constructors | Minor | **FIXED** |
| 6 | Tuple (Struct, primitive) | High | **FIXED** |

---

## Impact on JSON Parser

All blocking bugs are now fixed:
- ~~**Lexer:** Cannot return `(Result[Token, ParseError], LexerState)` (Bug 1)~~ **FIXED**
- ~~**Parser:** Same issue with parser state~~ **FIXED**
- ~~**Error handling:** Cannot pattern match on Result (Bug 2)~~ **FIXED**
- ~~**String processing:** Cannot use string fields in lexer struct (Bug 3)~~ **FIXED**
- ~~**Impl methods:** Cannot use impl methods on structs with string fields (Bug 4)~~ **FIXED**

The JSON parser implementation can now proceed with full language feature support.
