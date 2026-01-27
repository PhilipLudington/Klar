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

## [x] Bug 12: Arrays as Function Parameters with Complex Return Types

**Description:** Passing `[char]` as a function parameter when the function also returns complex types (tuples with Result) now works correctly. The fix ensures that when passing arrays to functions, they are automatically converted to slice structs `{ ptr, len }` to match the expected parameter type.

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
    let result: (Result[Token, string], LexerState) = next_token(chars, state)  // NOW WORKS

    // Extract tuple elements first (direct field access on tuple elements is a separate issue)
    let token_result: Result[Token, string] = result.0
    let new_state: LexerState = result.1

    match token_result {
        Ok(token) => {
            match token {
                Token::LBrace => { println("Got LBrace!") }
                Token::Eof => { println("Got Eof") }
            }
        }
        Err(e) => { println("Error: {e}") }
    }

    println("New pos: {new_state.pos}")
    return 0
}
```

---

## Summary

**Bug Status: 6/6 fixed**

| Bug | Feature | Status |
|-----|---------|--------|
| 7 | Associated fn on string structs | Fixed |
| 8 | Arrays in struct fields | Fixed |
| 9 | Match on tuple element | Fixed |
| 10 | String `+` concatenation | Fixed |
| 11 | Result with struct error | Fixed |
| 12 | Arrays + complex returns | Fixed |

---

## Impact on JSON Parser

All blocking bugs have been fixed! The JSON lexer and parser can now be implemented using the fundamental patterns needed:

1. ✅ **Bug 12** - Array parameters with complex return types now work
2. ✅ All other bugs are fixed

**Note:** Direct field access through tuple indices (e.g., `result.1.pos`) is not yet supported. Use the workaround of extracting to a variable first:
```klar
let state: LexerState = result.1
println("{state.pos}")  // Works
// println("{result.1.pos}")  // Not yet supported
```
