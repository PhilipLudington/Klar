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

**Fix:** The `isStringExpr` function in codegen wasn't recognizing field access expressions as string types. Added explicit handling for `.field` expressions that looks up the struct's field type from the type checker's registered struct types, rather than relying on re-running type checking (which failed because scope state differs during codegen).

---

## Bug 4: Impl Methods on Structs - Wrong Parameter Type

**Severity:** Blocking

**Description:** Calling impl methods on structs that contain non-primitive fields causes LLVM verification failure. The compiler passes only the first field instead of the entire struct.

**Reproduction:**
```klar
struct MyStruct {
    name: string,
    value: i32,
}

impl MyStruct {
    fn get_value(self: MyStruct) -> i32 {
        return self.value
    }
}

fn main() -> i32 {
    let s: MyStruct = MyStruct { name: "test", value: 42 }
    let v: i32 = s.get_value()  // CRASH
    return 0
}
```

**Error:**
```
LLVM Module verification failed: Call parameter type does not match function signature!
  %s1 = load i32, ptr %s, align 4
 { ptr, i32 }  %method.result = call i32 @MyStruct_get_value(i32 %s1)
```

**Workaround:** Use free functions instead of impl methods, or use only i32 fields in structs:
```klar
// Works: i32-only struct
struct Counter {
    value: i32,
}

impl Counter {
    fn get(self: Counter) -> i32 {
        return self.value  // OK
    }
}
```

---

## Bug 5: Some() Constructor Not Recognized

**Severity:** Minor

**Description:** The `Some(value)` constructor for optional types is not recognized as a function/constructor.

**Reproduction:**
```klar
fn main() -> i32 {
    let opt: ?i32 = Some(42)  // ERROR
    return 0
}
```

**Error:**
```
Type error(s):
  2:21: undefined variable 'Some'
  2:21: cannot call non-function type
```

**Workaround:** Return values directly from functions with optional return type (implicit Some):
```klar
fn get_value() -> ?i32 {
    return 42  // Implicitly wrapped as Some(42)
}
```

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
| 4 | Impl methods on mixed structs | Blocking | Use free functions |
| 5 | Some() constructor | Minor | Use implicit return |
| 6 | Tuple (Struct, primitive) | High | **FIXED** |

---

## Impact on JSON Parser

These bugs block the following:
- ~~**Lexer:** Cannot return `(Result[Token, ParseError], LexerState)` (Bug 1)~~ **FIXED**
- ~~**Parser:** Same issue with parser state~~ **FIXED**
- ~~**Error handling:** Cannot pattern match on Result (Bug 2)~~ **FIXED**
- ~~**String processing:** Cannot use string fields in lexer struct (Bug 3)~~ **FIXED**
- **Impl methods:** Cannot use impl methods on structs with string fields (Bug 4)

Current workaround approach:
1. ~~Use i32-only `LexerState` struct~~ String fields now work!
2. ~~Pass `[char]` slice separately~~ Can now use string fields directly!
3. ~~Use `.is_err()` + `!` instead of pattern matching~~ Pattern matching now works!
4. ~~Core logic works, but full Result integration is blocked~~ Result handling now complete!
5. Use free functions instead of impl methods for structs with non-primitive fields (Bug 4)
