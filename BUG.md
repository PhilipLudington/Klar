# Klar Compiler Bugs

Bugs discovered while implementing the JSON parser for Klar.

---

## [ ] Bug 1: Cross-file imports not working

**Status:** Open
**Type:** Module System
**Severity:** High

### Description

Import statements between files in a project don't resolve correctly. The compiler cannot locate sibling modules regardless of the import syntax used.

### Project Structure

```
klar-json/
├── lib/
│   └── json/
│       ├── lexer.kl      # exports Token, LexerState, lexer_new, etc.
│       └── parser.kl     # wants to import from lexer.kl
└── tests/
    └── parser_test.kl    # wants to import from lib/json/
```

### Steps to Reproduce

1. Create `lib/json/lexer.kl` with public exports:
```klar
pub enum Token {
    Null,
    True,
    False,
    // ...
}

pub struct LexerState {
    source: string,
    pos: i32,
}

pub fn lexer_new(source: string) -> LexerState {
    return LexerState { source: source, pos: 0 }
}
```

2. Create `lib/json/parser.kl` attempting to import:
```klar
import lexer.{ Token, LexerState, lexer_new }
```

3. Run `klar check lib/json/parser.kl`

### Import Variations Tried

All of the following fail with `module not found`:

```klar
import lexer.{ Token }                    // module 'lexer' not found
import json.lexer.{ Token }               // module 'json' not found
import lib.json.lexer.{ Token }           // module 'lib' not found
import ./lexer.{ Token }                  // syntax error
import "lexer".{ Token }                  // syntax error
```

### Expected Behavior

Imports resolve relative to the current file's directory, or via a project-level module path configuration.

### Actual Behavior

```
[undefined_module]: module 'lexer' not found
```

### Questions for Compiler Team

1. **Module resolution**: How does Klar resolve module paths? Is there a `klar.toml` or similar project file that defines module roots?

2. **Relative vs absolute**: Should imports be relative to the importing file, or relative to a project root?

3. **File extension**: Does the module name include or exclude the `.kl` extension?

4. **pub keyword**: Is `pub` sufficient to export items, or is there an explicit `mod` declaration needed?

5. **Documentation**: The docs at `~/Fun/Klar/docs` mention imports but the examples don't show multi-file projects. Is there a working example of cross-file imports?

### Current Workaround

Duplicate all shared types and functions in each file that needs them. This works but defeats the purpose of modular code organization.

### Impact

This bug forces the JSON parser to be a single monolithic file (~400 lines) instead of cleanly separated lexer/parser modules. It also prevents writing test files that import the library.

---

## [x] Bug 2: String return corruption from match arms on recursive enums

**Status:** Fixed
**Type:** Codegen / Runtime
**Severity:** High

**Fix:** Changed `emitInterpolatedString()` and `emitExprAsString()` in `src/codegen/emit.zig` to allocate string buffers on the heap (via `malloc`) instead of the stack (via `buildAlloca`). Stack-allocated buffers became invalid when the match arm scope was popped, causing the returned pointer to be dangling.

### Description

When returning a dynamically-created string (e.g., from string interpolation) from a match arm that pattern-matches on a recursive enum, the returned string is corrupted or empty. Literal string returns work correctly.

### Minimal Reproduction

```klar
enum JsonValue {
    Null,
    Bool(bool),
    Number(f64),
    Str(string),
    Array(List[JsonValue]),    // recursive
    Object(Map[string, JsonValue]),  // recursive
}

impl JsonValue {
    fn stringify(self: JsonValue) -> string {
        match self {
            JsonValue.Null => { return "null" }  // literal - WORKS
            JsonValue.Bool(b) => {
                if b { return "true" }  // literal - WORKS
                return "false"
            }
            JsonValue.Number(n) => {
                // Inside the function, the value is correct:
                println("n = {n}")  // prints "42.5" correctly
                return "{n}"  // CORRUPTED - returns empty or garbage
            }
            _ => { return "other" }
        }
    }
}

fn main() -> i32 {
    let num_val: JsonValue = JsonValue::Number(42.5)
    let result: string = num_val.stringify()
    println("result: '{result}'")  // prints empty or corrupted
    return 0
}
```

### Steps to Reproduce

1. Save the above code to `test.kl`
2. Run `klar run test.kl`

### Expected Behavior

```
n = 42.5
result: '42.5'
```

### Actual Behavior

```
n = 42.5
result: ''
```

Or sometimes shows corrupted characters like `'���'` or `'{g]o'`.

### Investigation Notes

- The value is correct **inside** the match arm (verified with println)
- The corruption happens when the string is returned
- Literal string returns work fine from the same match arms
- Non-recursive enums don't have this issue
- The `List[JsonValue]` and `Map[string, JsonValue]` variants make it recursive

### Workarounds Attempted

1. Using a helper function to convert f64 → string: **still corrupted**
2. Assigning to a `var result` outside the match and returning after: **still corrupted**
3. Copying the bound value to a local variable: **still corrupted**

### Impact

Prevents serializing any JsonValue variant that requires dynamic string construction (Number, Str, Array, Object). Only primitives with literal returns (Null, Bool) work.

---

## [x] Bug 3: Segfault when pushing to List of recursive enum type

**Status:** Fixed
**Type:** Runtime / Memory
**Severity:** Critical

### Description

Calling `push()` on a `List[T]` where `T` is a recursive enum type causes a segmentation fault at runtime. The code type-checks correctly.

### Minimal Reproduction

```klar
enum JsonValue {
    Null,
    Bool(bool),
    Number(f64),
    Str(string),
    Array(List[JsonValue]),    // recursive reference
    Object(Map[string, JsonValue]),  // recursive reference
}

fn main() -> i32 {
    println("Creating list...")
    var arr: List[JsonValue] = List.new[JsonValue]()

    println("Pushing Null...")
    arr.push(JsonValue::Null)  // SEGFAULT HERE

    println("Done")
    return 0
}
```

### Steps to Reproduce

1. Save the above code to `test.kl`
2. Run `klar run test.kl`

### Expected Behavior

```
Creating list...
Pushing Null...
Done
```

### Actual Behavior

```
Creating list...
Pushing Null...
[Segmentation fault: 11]
```

### Investigation Notes

- `List.new[JsonValue]()` succeeds without crashing
- The crash happens on the first `push()` call
- Empty lists can be wrapped in `JsonValue::Array(empty_list)` without crashing
- The issue is specifically with mutating a List of recursive enum type
- `List[i32]`, `List[string]`, etc. work fine
- The recursive nature of `JsonValue` (containing `List[JsonValue]`) seems to be the trigger

### Workarounds

None found. This completely blocks working with arrays or objects that contain values.

### Impact

- Cannot create JSON arrays with content
- Cannot create JSON objects with content
- Parser cannot store parsed arrays/objects
- Serializer cannot test array/object output

This is a critical blocker for any recursive data structure in Klar.
