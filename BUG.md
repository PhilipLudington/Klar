# Klar Compiler Bugs

Active bugs discovered during development.

---

## [x] Bug 1: `ref arr[idx]` returns pointer to wrong memory

**Status:** Fixed
**Type:** Codegen / FFI
**Severity:** Medium

### Description

Taking a reference to an array element via `ref arr[idx]` and converting it to a C pointer with `ref_to_ptr` results in a pointer to the wrong memory location (reads as 0, not the actual element value).

### Steps to Reproduce

```klar
fn main() -> i32 {
    var arr: [i32; 5] = [5, 2, 8, 1, 9]

    // This works - single variable
    var x: i32 = 42
    let x_ptr: CPtr[i32] = unsafe { ref_to_ptr(ref x) }
    let x_val: i32 = unsafe { read(x_ptr) }
    // x_val == 42 ✓

    // This FAILS - array element
    let ptr0: CPtr[i32] = unsafe { ref_to_ptr(ref arr[0]) }
    let val0: i32 = unsafe { read(ptr0) }
    // val0 == 0 ✗ (expected 5)

    return 0
}
```

### Expected Behavior

`ref arr[0]` should return a reference to the first element of the array, and `ref_to_ptr` should convert that to a valid `CPtr[i32]` pointing to the element's memory location.

### Actual Behavior

The pointer returned by `ref_to_ptr(ref arr[0])` does not point to the array element. Reading through it returns 0 (or garbage) instead of the actual element value.

### Workaround

Get a pointer to the whole array, then cast to element type:

```klar
var arr: [i32; 5] = [5, 2, 8, 1, 9]

// Instead of: ref_to_ptr(ref arr[0])  -- BROKEN
// Use:
let arr_ptr: CPtr[[i32; 5]] = unsafe { ref_to_ptr(ref arr) }
let elem_ptr: CPtr[i32] = unsafe { ptr_cast[i32](arr_ptr) }
let val0: i32 = unsafe { read(elem_ptr) }
// val0 == 5 ✓

// For arr[n], use offset:
let elem_n: CPtr[i32] = unsafe { offset(elem_ptr, n.as[isize]) }
```

### Impact

Affects FFI code that needs pointers to array elements. The workaround is verbose but functional.

### Investigation Notes

The issue is likely in how `ref` handles indexed expressions in codegen. The reference to `arr[0]` may be:
1. Creating a temporary copy and returning a reference to that
2. Incorrectly computing the GEP (GetElementPtr) offset
3. Not properly handling the array indexing in the reference context

### Discovered

While implementing the `extern_fn_qsort.kl` test for FFI function pointers.

### Fix

Added `emitIndexAddressOf()` function in `src/codegen/emit.zig` that computes the element pointer via GEP without loading the value. Modified `.ref` and `.ref_mut` cases in `emitUnary()` to call this function when the operand is an `.index` expression.

The root cause was that `emitIndexAccess()` always loads the element value after computing its address. For `ref arr[idx]`, we need the address, not the value. The fix adds a separate code path that returns the GEP result directly.

---

## [x] Bug 2: String return corruption from match arms on recursive enums

**Status:** Fixed
**Type:** Codegen / Runtime
**Severity:** High

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

---

## [x] Bug 4: Codegen crash when importing modules with tuple-containing-array return types

**Status:** Fixed
**Type:** Codegen / Import
**Severity:** High

### Description

Importing from a module that contains a function returning a tuple with a `[char]` array element causes the compiler to crash (exit code 133) or fail with "UnsupportedFeature". The module compiles and runs correctly in isolation (with its own `main()`), but any import from it triggers the bug.

**Root Cause Identified:** Functions returning `(T, [char])` tuple types trigger codegen failures when the containing module is imported.

### Minimal Reproduction

**File: `bug4_repro/lib/tuple_char_array.kl`**
```klar
pub struct State {
    pos: i32,
    len: i32,
}

pub fn init(s: string) -> (State, [char]) {
    let chars: [char] = s.chars()
    let state: State = State { pos: 0, len: s.len() }
    return (state, chars)
}

fn main() -> i32 {
    let pair: (State, [char]) = init("hello")
    println("Len: {pair.0.len}")
    return 0
}
```

**File: `bug4_repro/test_tuple_char.kl`**
```klar
import lib.tuple_char_array.{ State, init }

fn main() -> i32 {
    let pair: (State, [char]) = init("test")
    println("Len: {pair.0.len}")
    return 0
}
```

### Steps to Reproduce

```bash
cd bug4_repro

# Type checking passes:
klar check lib/tuple_char_array.kl
# Output: All checks passed

# But codegen fails even in isolation:
klar run lib/tuple_char_array.kl
# Error: Codegen error: UnsupportedFeature

# Import also fails:
klar run test_tuple_char.kl
# Exit code 133 (crash) or "Codegen error: UnsupportedFeature"
```

**Note:** The real `lib/json/lexer.kl` uses this same pattern in `lexer_new()` but has additional complexity. The lexer type-checks successfully (`klar check lib/json/lexer.kl` passes), but importing from it crashes the compiler.

### Error Messages Observed

1. **Exit code 133** - Crash during codegen (most common)
2. **`Codegen error: UnsupportedFeature`** - When tuple type is in return position
3. Original LLVM error (may be related):
   ```
   LLVM Module verification failed: Both operands to a binary operator are not of the same type!
     %addtmp = add nsw ptr %num_str29, i32 0
   ```

### Investigation Notes

**What Works:**
- Returning `[char]` alone from an imported function ✅
- Returning `(State, string)` tuples from imports ✅
- Returning `Result[string, Error]` from imports ✅
- String concatenation in imported modules ✅
- Multiple functions with string operations in imports ✅

**What Fails:**
- Returning `(State, [char])` from any function in an imported module ❌
- Returning `(T, [char])` for any struct `T` ❌
- Even importing *other* symbols from a module that *contains* such a function ❌

### Real-World Impact

The JSON lexer (`lib/json/lexer.kl`) uses this pattern:
```klar
pub fn lexer_new(source: string) -> (LexerState, [char]) {
    let chars: [char] = source.chars()
    let state: LexerState = LexerState.new(source.len())
    return (state, chars)
}
```

This prevents:
- Importing the lexer module for use in parser
- Writing separate test files that import lexer functions
- Modular library design with character-based parsing

### Suggested Investigation Areas

1. **Codegen for tuple types containing arrays** - The LLVM IR generation for `(T, [array])` return types may have incorrect type handling when resolving imports
2. **Import symbol resolution** - The crash occurs even when importing unrelated symbols from a module containing such functions, suggesting the issue is in how the imported module is compiled, not the specific imported symbol
3. **Memory layout for mixed tuple types** - The `add nsw ptr, i32` error suggests a pointer/integer type confusion when computing struct field offsets

### Workaround

Avoid returning `[char]` in tuples. Instead, pass the array as an output parameter or restructure to avoid the pattern:

```klar
// Instead of returning (State, [char]), return just State
// and have caller create the char array separately
pub fn lexer_new_state(source_len: i32) -> LexerState {
    return LexerState.new(source_len)
}

// Caller does:
let chars: [char] = source.chars()
let state: LexerState = lexer_new_state(source.len())
```
