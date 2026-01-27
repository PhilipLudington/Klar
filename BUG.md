# Klar Compiler Bugs

Bugs discovered while implementing the JSON parser. Klar version: **0.3.1-dev**

---

## [x] Bug 7: Associated Functions on Structs with String Fields

**Description:** Previously, calling associated functions on structs with string fields caused LLVM verification failures. This is now working.

```klar
struct Lexer {
    source: string,
    pos: i32,
}

impl Lexer {
    fn new(source: string) -> Lexer {
        return Lexer { source: source, pos: 0 }  // NOW WORKS
    }
}
```

---

## [x] Bug 8: Arrays in Struct Fields

**Description:** Storing `[char]` (slice types) in struct fields now works correctly. The fix ensures that when an array literal is assigned to a slice-typed variable or struct field, it is properly converted to a slice struct `{ ptr, len }`.

```klar
struct Foo {
    chars: [char],
}

fn main() -> i32 {
    let chars: [char] = ['a', 'b', 'c']
    let foo: Foo = Foo { chars: chars }
    let arr: [char] = foo.chars  // NOW WORKS
    println("{arr.len()}")  // Prints: 3
    return 0
}
```

---

## [x] Bug 9: Pattern Matching on Tuple Elements

**Description:** `match result.0` now works correctly without extracting to a variable first.

```klar
fn test() -> (Result[i32, string], i32) {
    return (Ok(42), 1)
}

fn main() -> i32 {
    let result: (Result[i32, string], i32) = test()
    match result.0 {  // NOW WORKS
        Ok(n) => { println("{n}") }
        Err(e) => { println("{e}") }
    }
    return 0
}
```

---

## [x] Bug 10: String Concatenation with `+` Operator

**Description:** String concatenation using `+` now works correctly. The type checker allows `string + string` operations, and all three backends (native LLVM, VM, interpreter) support it.

```klar
fn main() -> i32 {
    let a: string = "hello"
    let b: string = "world"
    let c: string = a + " " + b  // NOW WORKS
    println("{c}")  // Prints: hello world

    // Building strings in loops also works
    var result: string = ""
    for i: i32 in 0..5 {
        result = result + "x"
    }
    println("{result}")  // Prints: xxxxx

    return 0
}

---

## [x] Bug 11: Result with Struct Error Type

**Description:** Using `Result[T, E]` where `E` is a struct now works correctly. The fix ensures that when binding variables from pattern matching (e.g., `Err(e)`), the struct type name is properly recorded so that field access (e.g., `e.message`) works correctly.

```klar
pub struct ParseError {
    message: string,
}

fn make_result() -> Result[i32, ParseError] {
    return Ok(42)
}

fn main() -> i32 {
    let r: Result[i32, ParseError] = make_result()
    match r {
        Ok(n) => { println("Got: {n}") }
        Err(e) => { println("Error: {e.message}") }  // NOW WORKS
    }
    return 0
}

---

## [ ] Bug 12: Arrays as Function Parameters with Complex Return Types (BLOCKING)

**Description:** Passing `[char]` as a function parameter when the function also returns complex types (tuples with Result) causes LLVM type mismatch.

**Reproduction:**
```klar
pub enum Token { LBrace, Eof }
pub struct LexerState { pos: i32 }

fn next_token(chars: [char], state: LexerState) -> (Result[Token, string], LexerState) {
    let new_state: LexerState = LexerState { pos: state.pos + 1 }
    return (Ok(Token::LBrace), new_state)
}

fn main() -> i32 {
    let chars: [char] = ['a', 'b']
    let state: LexerState = LexerState { pos: 0 }
    let result: (Result[Token, string], LexerState) = next_token(chars, state)
    // LLVM ERROR
    return 0
}
```

**Error:**
```
LLVM Module verification failed: Call parameter type does not match function signature!
  %chars2 = load [2 x i32], ptr %chars, align 4
 { ptr, i64 }
```

**Workaround:** None found - this pattern is fundamental to lexer implementation.

---

## Summary

**Bug Status: 5/6 fixed**

| Bug | Feature | Status |
|-----|---------|--------|
| 7 | Associated fn on string structs | Fixed |
| 8 | Arrays in struct fields | Fixed |
| 9 | Match on tuple element | Fixed |
| 10 | String `+` concatenation | Fixed |
| 11 | Result with struct error | Fixed |
| 12 | Arrays + complex returns | Blocking |

---

## Impact on JSON Parser

The JSON lexer cannot be implemented due to bug 12:

1. **Bug 12** prevents the fundamental lexer pattern of passing char arrays to tokenization functions

The `value.kl` module (JsonValue types) works correctly. The lexer and parser are blocked until this compiler bug is fixed.
