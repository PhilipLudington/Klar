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

### Solution

The module resolver now adds the **current working directory** as a search path in addition to the entry file's directory. This allows imports to resolve relative to where the command is run.

When running `klar run tests/lexer_test.kl` from the project root:
- The cwd (`project/`) is added as a search path
- `import lib.json.value` resolves to `project/lib/json/value.kl`

**Fix applied in:** `src/main.zig` - Added cwd to resolver search paths before processing imports.

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

### Solution

Added `hasValidInterpolation()` function in `src/parser.zig` that validates every unescaped `{` has a matching `}` before committing to interpolation parsing. If any unescaped `{` lacks a matching `}`, the string is treated as a regular literal (not interpolation).

**Fix applied in:** `src/parser.zig` - Added validation before calling `parseInterpolatedString()`.

Now these all work correctly:
- `"{"` → literal `{`
- `"}"` → literal `}`
- `"hello { world"` → literal with unmatched brace
- `"{x}"` → interpolation (when x is in scope)

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

### Actual Behavior (before fix)

```
LLVM Module verification failed: Both operands to a binary operator are not of the same type!
  %addtmp = add nsw { ptr, i32, i32 } %keyword, ptr %chartostr.heap
Both operands to ICmp instruction are not of the same type!
  %eqtmp = icmp eq { ptr, i32, i32 } %keyword, ptr @str

LLVM verification failed: ModuleVerificationFailed
```

### Solution

Multiple fixes in `src/codegen/emit.zig`:

1. **Binary operators**: Added `isStringValue()` and `extractStringPtr()` helper functions to detect and handle both `string` (ptr) and `string_data` (struct) types in comparisons and concatenation.

2. **Variable declarations**: When declaring a `string` variable with a `string_data` value (e.g., from `char.to_string()`), the pointer is now extracted from the struct during assignment.

3. **Function parameters**: Added `semantic_type` to parameter registration so `char.to_string()` works correctly for char parameters (not just char literals).

**Files modified:** `src/codegen/emit.zig`

---

## [ ] Bug 4: Parse error with certain if/else patterns in functions

**Status:** Open
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

## Impact on JSON Parser Project

These bugs collectively block running comprehensive lexer tests:

1. **BUG-001** prevents organizing tests in a `tests/` directory
2. **BUG-002** requires helper functions for any JSON syntax in test strings
3. **BUG-003** prevents testing keyword recognition (`true`, `false`, `null`)
4. **BUG-004** limits the complexity of test files

The lexer implementation itself works (verified through manual testing and partial test coverage), but automated test coverage is limited until these issues are resolved.
