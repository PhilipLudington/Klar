# Klar Language Bugs

Bugs discovered while implementing the JSON parser.

---

## [x] Bug 1: Cannot use `{` or `}` in string literals

**Severity:** Blocking → **FIXED**

**Solution:** Use `\{` and `\}` to escape braces in strings:
```klar
fn main() -> i32 {
    print("\{\}")           // Prints: {}
    print("\{key\}")        // Prints: {key}
    let x: i32 = 42
    print("\{value: {x}\}") // Prints: {value: 42}
    return 0
}
```

**Original issue:** Any string containing `{` or `}` caused a parse error because the parser interpreted these as string interpolation delimiters.

---

## [x] Bug 2: No escape sequence for braces in strings

**Severity:** Blocking → **FIXED**

**Solution:** Use `\{` to produce `{` and `\}` to produce `}`. See Bug 1 for examples.

**Original issue:** There was no documented or working way to escape `{` or `}` characters in string literals.

---

## [x] Bug 3: `char.to_string()` returns wrong type

**Severity:** High → **FIXED**

**Solution:** The type checker and code generator now correctly handle `char.to_string()`:
- Type checker returns primitive `string` type for `char.to_string()` (not heap-allocated `String`)
- Code generator emits a single-character null-terminated string
- Variable declarations use type annotations to determine the correct LLVM type

```klar
fn main() -> i32 {
    let c: char = 'a'
    let s: string = c.to_string()  // Now works correctly
    println(s)                      // Prints: a
    return 0
}
```

**Original issue:** The type checker was returning `String` (heap-allocated) instead of `string` (primitive) for `char.to_string()`, and the code generator was treating chars like integers for the `to_string` method.

---

## [x] Bug 4: Char interpolation prints code point, not character

**Severity:** Medium → **FIXED**

**Solution:** The LLVM codegen now uses semantic type information to detect char expressions and uses `%c` format specifier instead of `%d`:

```klar
fn main() -> i32 {
    let c: char = 'a'
    println("{c}")  // Now correctly prints "a"

    let x: char = 'X'
    println("char is: {x}")  // Prints "char is: X"

    // Mixed interpolation works too
    let num: i32 = 42
    let letter: char = 'Z'
    println("num={num}, char={letter}")  // Prints "num=42, char=Z"
    return 0
}
```

**Original issue:** The LLVM codegen treated char values (stored as i32) the same as regular integers, using `%d` format which printed the numeric code point instead of the character.

---

## [x] Bug 5: No string slicing method

**Severity:** High → **FIXED**

**Solution:** The `slice(start, end)` method is now implemented with clamping semantics:
```klar
fn main() -> i32 {
    let s: string = "hello world"
    let sub: string = s.slice(0, 5)   // "hello"
    let end: string = s.slice(6, 11)  // "world"
    println(sub)  // Prints: hello
    println(end)  // Prints: world
    return 0
}
```

**Behavior:**
- Indices outside valid range are clamped (like Python/Go)
- `s.slice(0, 100)` on "hello" → "hello" (clamps end to 5)
- `s.slice(10, 20)` on "hello" → "" (start >= len)
- `s.slice(3, 1)` → "" (start > end)
- Negative indices are clamped to 0

**Original issue:** The `slice(start, end)` method did not exist on the `string` type.

---

## Summary

| Bug | Severity | Status |
|-----|----------|--------|
| Braces in strings | Blocking | ✅ FIXED - use `\{` and `\}` |
| No brace escape | Blocking | ✅ FIXED - use `\{` and `\}` |
| char.to_string() type | High | ✅ FIXED - returns `string` now |
| Char interpolation | Medium | ✅ FIXED - uses `%c` format |
| No string slicing | High | ✅ FIXED - use `s.slice(start, end)` |

Remaining blockers for JSON parser:
1. ~~Cannot write test strings containing `{` or `}`~~ ✅ Fixed
2. ~~Cannot build strings from characters (needed for token construction)~~ ✅ Fixed
3. ~~Cannot extract substrings (needed for keyword/string token extraction)~~ ✅ Fixed

**All bugs resolved!** The JSON parser implementation can proceed.
