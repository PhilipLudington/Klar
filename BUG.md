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

## [x] Bug 4: Parse error with empty braces in strings

**Status:** Fixed
**Severity:** Medium
**Discovered:** 2026-01-27

### Description

Strings containing empty braces `{}` or braces with only whitespace `{  }` were incorrectly parsed as string interpolation, causing parse errors. This affected code using JSON-like strings or any string with literal brace characters.

### Steps to Reproduce

```klar
fn main() -> i32 {
    let json: string = "{}"  // This failed with ExpectedExpression
    println(json)
    return 0
}
```

Or when mixing literal braces with interpolation:

```klar
fn main() -> i32 {
    let x: i32 = 42
    let s: string = "{} count: {x}"  // This also failed
    println(s)
    return 0
}
```

### Expected Behavior

Empty braces `{}` should be treated as literal characters, not as interpolation.

### Actual Behavior (before fix)

```
Parse error: error.ExpectedExpression
```

The parser attempted to parse the empty content between braces as an expression.

### Solution

Modified `src/parser.zig` to properly handle empty or invalid brace content:

1. Added `isValidExpressionContent()` helper function that checks if content between braces starts with a valid expression character (letter, digit, underscore, parenthesis, string/char literal).

2. Updated `hasValidInterpolation()` to only consider braces with valid expression content as interpolation candidates.

3. Updated `parseInterpolatedString()` to treat braces with invalid content (empty, whitespace-only, or starting with non-expression characters like `{` or `}`) as literal string segments instead of trying to parse them as expressions.

Now these all work correctly:
- `"{}"` → literal `{}`
- `"{  }"` → literal `{  }`
- `"{{}}"` → literal `{{}}`
- `"{} count: {x}"` → literal `{}` followed by interpolation

**Files modified:** `src/parser.zig`

---

## Impact on JSON Parser Project

All bugs have been fixed:

1. **BUG-001** ✅ Fixed - Tests can now be organized in `tests/` directory
2. **BUG-002** ✅ Fixed - String literals with unmatched braces work correctly
3. **BUG-003** ✅ Fixed - String comparisons with variables work correctly
4. **BUG-004** ✅ Fixed - Empty braces in strings are now treated as literals

The JSON parser project can now proceed with full test coverage.
