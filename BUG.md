# Klar Compiler Bugs and Feature Requests

Discovered while implementing the JSON parser. Tested with Klar v0.3.1-dev.

**Last verified:** 2026-01-25 — All blocking bugs are now fixed. JSON parser implementation can proceed.

---

## Bug: impl Methods Returning Optional Enum When Accessing Map Variant

**Status:** Fixed (verified 2026-01-25)

**Description:** ~~`impl` methods on enums that return `?EnumType` and access a `Map` inside a variant would compile but return incorrect values at runtime. The Map's `get` method would return None even when the key existed.~~

**Root Cause:** Two bugs in `src/codegen/emit.zig`:
1. `getTypeSize` and `getSizeOfKlarType` returned 8 bytes for Map/List/Set types, but these are actually larger structs (Map: 20 bytes, List: 16 bytes, Set: 20 bytes). This caused enum payloads to be undersized.
2. `bindPatternVariables` for variant patterns was passing a raw `i8*` pointer to the payload bytes instead of loading the actual payload value.

**Fix:** Updated collection type sizes and added proper payload loading in match pattern binding.

**Original Reproduction:**

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

    let name_val: ?JsonValue = obj.get("name")  // Now works correctly!
}
```

---

## Bug: Array Iteration Causes Compiler Segfault

**Status:** Fixed (verified 2026-01-25)

**Description:** ~~Iterating over an array (e.g., from `string.chars()`) causes the compiler to segfault during codegen. This blocks implementing a lexer since strings cannot be converted to an iterable form.~~ Now works correctly.

**Reproduction:**

```klar
fn main() -> i32 {
    let s: string = "hello"
    let arr: [char] = s.chars()

    var count: i32 = 0
    for c: char in arr {  // Segfault here
        count = count + 1
    }

    return count
}
```

**Error:**
```
Segmentation fault at address 0x8
...
in _codegen.emit.Emitter.emitForLoopArray
```

**Expected:** Should iterate over the array without error.

~~**Impact:** Without this, we cannot implement a JSON lexer because:
1. Cannot iterate over `string.chars()` directly
2. Cannot use array indexing (returns `Codegen error: UnsupportedFeature`)
3. Cannot convert string to `List[char]` (would require iteration)

**Workaround:** None for arbitrary strings. For testing, characters can be manually pushed to a `List[char]`.~~

**Update:** All three blockers are now resolved. JSON lexer implementation can proceed.

---

## Bug: Array Indexing Not Supported

**Status:** Fixed (verified 2026-01-25)

**Description:** ~~Array indexing (`arr[i]`) returns a codegen error, making it impossible to access individual characters from `string.chars()`.~~ Now works correctly.

**Reproduction:**

```klar
fn main() -> i32 {
    let s: string = "hello"
    let chars: [char] = s.chars()
    let first: char = chars[0]  // ERROR
    return 0
}
```

**Error:**
```
Codegen error: UnsupportedFeature
```

**Expected:** Should return the character at the given index.

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
