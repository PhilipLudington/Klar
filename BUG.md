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

## [ ] Bug 3: `char.to_string()` returns wrong type

**Severity:** High

**Description:** According to the documentation, `char.to_string()` should return `string`, but it appears to return a different type causing type mismatch errors.

**Reproduction:**
```klar
fn main() -> i32 {
    let c: char = 'a'
    let s: string = c.to_string()  // Type error
    println(s)
    return 0
}
```

**Error:**
```
Type error(s):
  3:5: initializer type doesn't match declared type
```

**Documentation claim (from docs/types/primitives.md:157):**
```klar
let as_str: string = c.to_string()   // "A"
```

**Impact:** Cannot build strings from individual characters, which is needed for the lexer to construct tokens.

---

## [ ] Bug 4: Char interpolation prints code point, not character

**Severity:** Medium

**Description:** When interpolating a `char` value in a string, it prints the numeric Unicode code point rather than the character itself.

**Reproduction:**
```klar
fn main() -> i32 {
    let c: char = 'a'
    println("{c}")  // Prints "97" instead of "a"
    return 0
}
```

**Output:** `97`

**Expected:** `a`

**Impact:** Cannot easily print char values for debugging. Must convert to string first, but that's blocked by bug #3.

---

## [ ] Bug 5: No string slicing method

**Severity:** High

**Description:** The `slice(start, end)` method referenced in design documents does not exist on the `string` type.

**Reproduction:**
```klar
fn main() -> i32 {
    let s: string = "hello world"
    let sub: string = s.slice(0, 5)  // Method not found
    println(sub)
    return 0
}
```

**Error:**
```
Type error(s):
  4:23: method 'slice' not found
```

**Also tried:**
- `s.substring(0, 5)` - method not found
- `s[0..5]` - cannot index this type

**Impact:** Cannot extract substrings, which is essential for lexer token extraction.

---

## Summary

| Bug | Severity | Status |
|-----|----------|--------|
| Braces in strings | Blocking | ✅ FIXED - use `\{` and `\}` |
| No brace escape | Blocking | ✅ FIXED - use `\{` and `\}` |
| char.to_string() type | High | ❌ Open |
| Char interpolation | Medium | ❌ Open |
| No string slicing | High | ❌ Open |

Remaining blockers for JSON parser:
1. ~~Cannot write test strings containing `{` or `}`~~ ✅ Fixed
2. Cannot build strings from characters (needed for token construction)
3. Cannot extract substrings (needed for keyword/string token extraction)
