# Klar Compiler Bugs and Limitations

Discovered while implementing the JSON parser. Tested with Klar v0.3.1-dev.

---

## Bug 1: Recursive Types in Enums

**Status:** Fixed

**Description:** Enum variants cannot reference their containing type, even indirectly through generic containers.

**Reproduction:**

```klar
enum JsonValue {
    Null,
    Bool(bool),
    Array(List[JsonValue]),  // ERROR
}
```

**Error:**
```
Type error(s):
  8:16: undefined type 'JsonValue'
```

**Expected:** The compiler should recognize `JsonValue` as a forward reference within its own definition.

**Impact:** Cannot represent JSON arrays or objects, which are inherently recursive structures.

---

## Bug 2: impl Blocks on Enums

**Status:** Fixed

**Description:** Methods defined in `impl` blocks for enum types are not found when called.

**Reproduction:**

```klar
enum JsonValue {
    Null,
    Bool(bool),
}

impl JsonValue {
    fn is_null(self: JsonValue) -> bool {
        match self {
            JsonValue.Null => { return true }
            _ => { return false }
        }
    }
}

fn main() {
    let n: JsonValue = JsonValue::Null
    if n.is_null() {  // ERROR
        println("null")
    }
}
```

**Error:**
```
Type error(s):
  53:8: method 'is_null' not found
```

**Expected:** Methods in `impl EnumType` blocks should be callable on enum values.

**Workaround:** Use free functions instead of methods:

```klar
fn json_is_null(val: JsonValue) -> bool {
    match val {
        JsonValue.Null => { return true }
        _ => { return false }
    }
}
```

---

## Bug 3: impl Methods Returning Optional of Recursive Enum

**Status:** Fixed

**Description:** `impl` methods on enums that return `?EnumType` (optional of the enum itself or types containing it) fail LLVM code generation.

**Reproduction:**

```klar
pub enum JsonValue {
    Null,
    Array(List[JsonValue]),
}

impl JsonValue {
    pub fn at(self: JsonValue, index: i32) -> ?JsonValue {
        match self {
            JsonValue.Array(arr) => { return arr.get(index) }
            _ => { }  // implicit None
        }
    }
}
```

**Error:**
```
LLVM Module verification failed: Function return type does not match operand type of return inst!
  ret void
 { i1, { i8, [8 x i8] } }
LLVM verification failed: ModuleVerificationFailed
```

**Expected:** impl methods should be able to return optional types involving the enum.

**Workaround:** Use free functions instead:

```klar
fn json_at(val: JsonValue, index: i32) -> ?JsonValue {
    match val {
        JsonValue.Array(arr) => { return arr.get(index) }
        _ => { }
    }
}
```

---

## Bug 4: List.push with Enum Variant Containing Map

**Status:** Fixed

**Description:** Pushing an enum variant that contains a `Map` to a `List` fails LLVM code generation.

**Reproduction:**

```klar
pub enum JsonValue {
    Null,
    Object(Map[string, JsonValue]),
}

fn main() {
    var list: List[JsonValue] = List.new[JsonValue]()
    var m: Map[string, JsonValue] = Map.new[string, JsonValue]()
    m.insert("x", JsonValue::Null)
    list.push(JsonValue::Object(m))  // ERROR
}
```

**Error:**
```
LLVM Module verification failed: PHI node entries do not match predecessors!
  %push.data_ptr = phi ptr [ %push.current_ptr, %map.continue_probe ], ...
LLVM verification failed: ModuleVerificationFailed
```

**Expected:** Should be able to push any enum variant to a List.

**Workaround:** None found. Cannot build arrays of objects dynamically.

**Impact:** Cannot parse JSON arrays containing objects.

---

## Working Features

The following features work correctly:

- Enum definitions with data variants
- Recursive types in enums (`Array(List[JsonValue])`, `Object(Map[string, JsonValue])`)
- Pattern matching with `match` on enums
- Enum construction with `EnumName::Variant(value)`
- Pattern syntax with `EnumName.Variant` in match arms
- `impl` blocks on structs (methods work)
- `impl` blocks on enums for methods returning non-optional types
- Free functions accepting/returning enum types
- Free functions returning optional enum types
- `List.push` with simple enum variants (Null, Bool, Number, Str, Array)
- `Map.insert` with recursive enum values
- Creating standalone `JsonValue::Object(...)` values
