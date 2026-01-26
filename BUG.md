# Klar Compiler Bugs

Bugs discovered while implementing the JSON parser. Klar version: **0.3.1-dev**

---

## ~~Bug 1: Result in Tuple Returns~~ - FIXED

**Status:** Fixed

**Description:** Returning a tuple containing a `Result[T, E]` and a struct caused a codegen error.

**Fix:** Propagate type context through tuple literals in both type checker (`checkTupleLiteralWithHint`) and codegen (`emitTupleLiteral`). This allows `Ok()`/`Err()` constructors inside tuples to infer the correct Result type.

---

## Bug 2: Pattern Matching on Result/Option - Parse Error

**Severity:** Blocking

**Description:** Pattern matching with `Ok(value) =>` or `Some(value) =>` causes a parse error, despite being documented syntax.

**Reproduction:**
```klar
fn main() -> i32 {
    let r: Result[i32, string] = Ok(42)
    match r {
        Ok(v) => { println("Value: {v}") }
        Err(e) => { println("Error: {e}") }
    }
    return 0
}
```

**Error:**
```
Parse error: error.UnexpectedToken
  4:11: expected '=>' after pattern
```

**Same issue with Option:**
```klar
fn main() -> i32 {
    let opt: ?i32 = Some(42)
    match opt {
        Some(v) => { println("Value: {v}") }
        None => { println("None") }
    }
    return 0
}
```

**Workaround:** Use `.is_ok()`, `.is_err()`, `.is_some()`, `.is_none()` with force unwrap `!`:
```klar
if r.is_ok() {
    let v: i32 = r!
    println("Value: {v}")
}
```

---

## Bug 3: Struct with String Field - Method Returns Wrong `string.len()`

**Severity:** Blocking

**Description:** When a struct contains a `string` field and is passed to a method or function, calling `.len()` on the string field returns 0 instead of the actual length.

**Reproduction:**
```klar
struct Lexer {
    source: string,
    pos: i32,
}

fn is_at_end(lex: Lexer) -> bool {
    println("source: {lex.source}")        // Prints: "hello"
    println("len: {lex.source.len()}")     // Prints: 0 (WRONG!)
    return lex.pos >= lex.source.len()
}

fn main() -> i32 {
    let lex: Lexer = Lexer { source: "hello", pos: 0 }
    println("Direct len: {lex.source.len()}")  // Prints: 5 (correct)

    let at_end: bool = is_at_end(lex)  // Returns true (WRONG!)
    return 0
}
```

**Output:**
```
Direct len: 5
source: hello
len: 0
```

**Workaround:** Store length separately in an i32 field and don't include strings in structs passed to functions:
```klar
struct LexerState {
    pos: i32,
    len: i32,  // Store length separately
}
```

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
| 2 | Pattern matching Result/Option | Blocking | Use is_ok()/is_err() + ! |
| 3 | String field in struct | Blocking | Store length separately |
| 4 | Impl methods on mixed structs | Blocking | Use free functions |
| 5 | Some() constructor | Minor | Use implicit return |
| 6 | Tuple (Struct, primitive) | High | **FIXED** |

---

## Impact on JSON Parser

These bugs block the following:
- ~~**Lexer:** Cannot return `(Result[Token, ParseError], LexerState)` (Bug 1)~~ **FIXED**
- ~~**Parser:** Same issue with parser state~~ **FIXED**
- **Error handling:** Cannot pattern match on Result (Bug 2)
- **String processing:** Cannot use string fields in lexer struct (Bug 3, 4)

Current workaround approach:
1. Use i32-only `LexerState` struct
2. Pass `[char]` slice separately
3. Use `.is_err()` + `!` instead of pattern matching
4. ~~Core logic works, but full Result integration is blocked~~ Result in tuples now works!
