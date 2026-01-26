# Klar Compiler Bugs and Feature Requests

Discovered while implementing the JSON parser. Tested with Klar v0.3.1-dev.

---

## Bug: impl Methods Returning Optional Enum When Accessing Map Variant

**Status:** Fixed (verified 2026-01-25)

**Description:** `impl` methods on enums that return `?EnumType` and access a `Map` inside a variant fail LLVM code generation. Note that the equivalent code using `List` works correctly—only `Map` access fails.

**Reproduction:**

```klar
pub enum JsonValue {
    Null,
    Object(Map[string, JsonValue]),
}

impl JsonValue {
    pub fn get(self: JsonValue, key: string) -> ?JsonValue {
        match self {
            JsonValue.Object(obj) => { return obj.get(key) }
            _ => { }
        }
    }
}

fn main() {
    var entries: Map[string, JsonValue] = Map.new[string, JsonValue]()
    entries.insert("name", JsonValue::Str("Alice"))
    let obj: JsonValue = JsonValue::Object(entries)

    let name_val: ?JsonValue = obj.get("name")  // ERROR
}
```

**Error:**
```
LLVM Module verification failed: Invalid indices for GEP pointer type!
  %opt.tag.ptr = getelementptr i32, ptr %opt.is_some.tmp, i32 0, i32 0
```

**Expected:** impl methods should work uniformly regardless of whether the variant contains a `List` or `Map`.

**Workaround:** Use a free function instead:

```klar
fn json_get(val: JsonValue, key: string) -> ?JsonValue {
    match val {
        JsonValue.Object(obj) => { return obj.get(key) }
        _ => { }
    }
}
```

---

## Bug: Array Iteration Causes Compiler Segfault

**Status:** Fixed (verified 2026-01-25)

**Description:** Iterating over a slice (e.g., from `string.chars()`) caused the compiler to segfault during codegen. The issue was that `emitForLoopArray` assumed all array types were fixed-size LLVM arrays, but slices are represented as `{ ptr, len }` structs.

**Root Cause:** The `getArrayInfo()` function called `LLVMGetElementType()` on the slice struct type instead of the actual element type. Slices (dynamic arrays without fixed size) needed separate handling from fixed-size arrays.

**Fix:** Added `emitForLoopSlice()` function to handle slice iteration separately, extracting the data pointer and length from the slice struct fields and using them for iteration.

**Reproduction (now works):**

```klar
fn main() -> i32 {
    let s: string = "hello"
    let arr: [char] = s.chars()

    var count: i32 = 0
    for c: char in arr {
        count = count + 1
    }

    return count  // Returns 5
}
```

---

## Bug: Array Indexing Not Supported

**Status:** Fixed (verified 2026-01-25)

**Description:** Array indexing (`arr[i]`) on slices returned a codegen error, making it impossible to access individual characters from `string.chars()`.

**Root Cause:** The `emitIndexAccess` function only handled fixed-size LLVM arrays (`LLVMArrayTypeKind`), but slices are stored as `{ ptr: *T, len: i64 }` structs (`LLVMStructTypeKind`). The code fell through to an `UnsupportedFeature` error for struct types.

**Fix:** Added slice indexing support in `emitIndexAccess` by checking for struct-typed variables with `is_array = true` and `array_size = null`. The fix extracts the data pointer and length from the slice struct, performs bounds checking, and GEPs into the data pointer to access the element.

**Reproduction (now works):**

```klar
fn main() -> i32 {
    let s: string = "hello"
    let chars: [char] = s.chars()
    let first: char = chars[0]   // 'h'
    let second: char = chars[1]  // 'e'
    let last: char = chars[4]    // 'o'

    // Variable index also works
    var idx: i32 = 2
    let third: char = chars[idx]  // 'l'

    return 0
}
```

---

## Feature Requests

### 1. Method Chaining for Optional Returns

**Description:** Allow calling methods on optional values with automatic propagation, similar to Rust's `?` operator or Swift's optional chaining.

**Current code:**
```klar
let arr_opt: ?List[JsonValue] = val.as_array()
if arr_opt.is_some() {
    let arr: List[JsonValue] = arr_opt.unwrap()
    let first: ?JsonValue = arr.get(0)
    // ...
}
```

**Desired:**
```klar
let first: ?JsonValue = val.as_array()?.get(0)
```

### 2. Derive Debug/Display for Enums

**Description:** Automatically generate string representation for enum values for debugging purposes.

**Desired:**
```klar
#[derive(Debug)]
enum JsonValue {
    Null,
    Bool(bool),
    // ...
}

fn main() {
    let v: JsonValue = JsonValue::Bool(true)
    println(v.debug())  // prints "Bool(true)"
}
```

### 3. Match Expression (Not Statement)

**Description:** Allow `match` to be used as an expression that returns a value.

**Current code:**
```klar
fn type_name(val: JsonValue) -> string {
    match val {
        JsonValue.Null => { return "null" }
        JsonValue.Bool(_) => { return "boolean" }
        // ...
    }
}
```

**Desired:**
```klar
fn type_name(val: JsonValue) -> string {
    return match val {
        JsonValue.Null => "null",
        JsonValue.Bool(_) => "boolean",
        // ...
    }
}
```
