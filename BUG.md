# Klar Language Bugs

Bugs encountered while implementing the JSON parser for Klar.

---

## [x] Bug 1: Import path resolution from subdirectories

**Status:** Fixed
**Severity:** High
**Discovered:** 2026-01-27

### Description

Tests in a `tests/` subdirectory cannot import modules from sibling directories like `lib/`. Klar's module resolution is relative to the source file location, with no way to reference parent or sibling directory trees.

### Steps to Reproduce

1. Create project structure:
   ```
   project/
   ├── lib/
   │   └── json/
   │       └── value.kl    # exports ParseError
   └── tests/
       └── lexer_test.kl   # wants to import from lib/json/value
   ```

2. In `tests/lexer_test.kl`, try:
   ```klar
   import lib.json.value.{ ParseError }
   ```

3. Run: `klar run tests/lexer_test.kl`

### Expected Behavior

Import resolves to `../lib/json/value.kl` relative to the test file.

### Actual Behavior

```
Module error: module 'lib.json.value' not found
```

### Workaround

Place test files in the project root directory, or create self-contained test files that duplicate type definitions.

---

## [x] Bug 2: String interpolation triggers on brace literals

**Status:** Fixed
**Severity:** Medium
**Discovered:** 2026-01-27

### Description

String literals containing `{` are incorrectly parsed as the start of string interpolation, even when there's no matching `}` or valid interpolation expression.

### Steps to Reproduce

```klar
fn main() -> i32 {
    let json: string = "{"  // This fails
    println(json)
    return 0
}
```

### Expected Behavior

The string `"{"` should be a valid string literal containing a single left brace character.

### Actual Behavior

```
Parse error: error.UnterminatedString
  X:Y: unmatched '{' in string interpolation
```

### Workaround

Use character-to-string conversion:

```klar
fn lbrace() -> string { return '{'.to_string() }

fn main() -> i32 {
    let json: string = lbrace()  // Works
    println(json)
    return 0
}
```

---

## [x] Bug 3: LLVM codegen error with string variable comparisons

**Status:** Fixed
**Severity:** High
**Discovered:** 2026-01-27

### Description

String equality comparisons involving variables built through concatenation cause LLVM verification failures. The generated IR has type mismatches between string struct types and pointer types.

### Steps to Reproduce

```klar
pub fn read_keyword(first: char) -> string {
    var keyword: string = first.to_string()
    keyword = keyword + "r"
    keyword = keyword + "u"
    keyword = keyword + "e"

    if keyword == "true" {  // This causes LLVM error
        return "matched"
    }
    return "no match"
}

fn main() -> i32 {
    let result: string = read_keyword('t')
    println(result)
    return 0
}
```

### Expected Behavior

The code compiles and runs, printing "matched".

### Actual Behavior

```
LLVM Module verification failed: Both operands to a binary operator are not of the same type!
  %addtmp = add nsw { ptr, i32, i32 } %keyword, ptr %chartostr.heap
Both operands to ICmp instruction are not of the same type!
  %eqtmp = icmp eq { ptr, i32, i32 } %keyword, ptr @str

LLVM verification failed: ModuleVerificationFailed
```

### Analysis

The LLVM IR shows:
- String variables are represented as `{ ptr, i32, i32 }` (pointer + length + capacity)
- String literals are represented as `ptr`
- Comparison and concatenation operations don't properly handle the type difference

### Workaround

None found. Avoid string equality comparisons with dynamically built strings. Use character-by-character comparison or restructure code to avoid this pattern.

---

## [x] Bug 4: Parse error with certain if/else patterns in functions

**Status:** Fixed
**Severity:** Medium
**Discovered:** 2026-01-27

### Description

Certain combinations of if/else statements within functions cause parse errors, even when the syntax appears correct. The error message indicates an unmatched brace, but the braces are properly balanced.

### Steps to Reproduce

```klar
pub struct LexerState {
    pos: i32,
    len: i32,
}

pub fn lexer_next_token(state: LexerState) -> i32 {
    if state.pos >= state.len { return 0 }

    let c: char = 'x'

    if c == '{' {
        return 1
    }
    if c == '}' {
        return 2
    }
    // ... more if statements

    return -1
}
```

When this function is part of a larger file with many other declarations, parse errors occur at seemingly random locations.

### Expected Behavior

The file parses successfully.

### Actual Behavior

```
Parse error: error.UnexpectedToken
  X:1: expected '}'
```

The line number often points to the start of a subsequent function, not the actual problem location.

### Workaround

- Isolate problematic code into smaller files
- Reduce the number of sequential if statements
- Use match expressions instead of multiple if statements where possible

---

## [x] Bug 5: Parse error in large files with multiple function definitions

**Status:** Fixed (resolved by Bug 4 fix)
**Severity:** High
**Discovered:** 2026-01-27

### Description

Large Klar files with multiple function definitions fail to parse with `error.UnexpectedToken`, even when the same code works in smaller files. The error occurs at function boundaries, suggesting an issue with how the parser handles sequences of function definitions.

### Resolution

This bug could not be reproduced after the Bug 4 fix was applied. The Bug 4 fix improved brace handling in string interpolation by adding validation to check if content between braces starts with a valid expression character. This fix also resolved the parsing issues that were manifesting as Bug 5.

Extensive testing was performed with:
- Files over 200 lines with 30+ function definitions
- Files with many brace character literals (`if c == '{'`)
- Files with sequential if statements checking brace characters
- The test suite (430 tests) passes completely

### Original Steps to Reproduce (no longer reproducible)

1. Create a file with multiple functions (e.g., `lib/json/lexer.kl` with ~280 lines)
2. The file parses successfully up to ~70 lines
3. Adding the next function definition causes a parse error

### Original Error (no longer occurs)

```
Parse error: error.UnexpectedToken
  72:1: expected '}'
```

---

## Impact on JSON Parser Project

All bugs have been verified as fixed in the Klar language:

1. ~~**BUG-001** prevents organizing tests in a `tests/` directory~~ ✓ Fixed
2. ~~**BUG-002** requires helper functions for any JSON syntax in test strings~~ ✓ Fixed
3. ~~**BUG-003** prevents testing keyword recognition (`true`, `false`, `null`)~~ ✓ Fixed
4. ~~**BUG-004** limits the complexity of test files~~ ✓ Fixed
5. ~~**BUG-005** blocks large lexer files~~ ✓ Fixed (resolved by Bug 4 fix)

The JSON parser project can now proceed without any known blocking issues.
