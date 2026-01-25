# Klar Compiler Bugs and Feature Requests

Discovered while implementing the JSON parser. Tested with Klar v0.3.1-dev.

---

## Bug: impl Methods Returning Optional Enum When Accessing Map Variant

**Status:** Fixed

**Description:** `impl` methods on enums that return `?EnumType` and access a `Map` inside a variant fail LLVM code generation. Note that the equivalent code using `List` works correctly—only `Map` access fails.

**Fix:** The issue was in `inferExprType` where Cell method inference (`get()`, `replace()`, `set()`) happened before user-defined struct/enum method lookup. When processing `obj.get("name")` where `obj: JsonValue`, the code incorrectly treated it as a Cell method call. Fixed by checking for user-defined methods first before falling back to Cell method inference.

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
