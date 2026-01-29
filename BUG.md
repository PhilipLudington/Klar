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

---

## [x] Bug 4: Imported enum types cannot be used for variant construction

**Status:** Fixed
**Type:** Type Checker / Module System
**Severity:** High

**Fix:** Modified `registerModuleExports()` in `src/checker.zig` to look up the actual type from the current scope for struct, enum, and type alias exports instead of storing `null`. The types are already registered in scope by `checkStruct()`, `checkEnum()`, and `checkTypeAlias()` before exports are registered, so the lookup succeeds and the proper type information is now preserved through the import system.

### Description

When importing an enum type from another module, the type checker fails to recognize it as an enum when constructing variants. The error `expected enum type` occurs even though the import succeeds and the module is found.

### Minimal Reproduction

**enum_lib.kl:**
```klar
pub enum Color {
    Red,
    Green,
    Blue,
}

pub fn get_red() -> Color {
    return Color::Red
}
```

**main.kl:**
```klar
import enum_lib.{ Color, get_red }

fn main() -> i32 {
    let c: Color = Color::Red   // ERROR: expected enum type
    let d: Color = get_red()    // This would work if we got past line above
    return 0
}
```

### Steps to Reproduce

1. Create the two files above in the same directory
2. Run `klar check main.kl`

### Expected Behavior

The code type-checks successfully. `Color::Red` should construct an enum variant of type `Color`.

### Actual Behavior

```
Type check failed with 1 error(s):
  4:20 [type_mismatch]: expected enum type
```

### Investigation Notes

The issue is in how imported enum types are registered in the scope. Looking at the relevant code:

1. In `registerModuleExports()` (checker.zig ~line 10842):
   ```zig
   .enum_decl => |e| {
       if (e.is_pub) {
           const sym = ModuleSymbol{
               .name = e.name,
               .kind = .enum_type,
               .type_ = null,  // <-- Problem: no type info stored
               .is_pub = true,
           };
       }
   }
   ```

2. In `importSpecificSymbol()` (checker.zig ~line 10987):
   ```zig
   self.current_scope.define(.{
       .name = local_name,
       .type_ = sym.type_ orelse self.type_builder.unknownType(),  // Results in unknownType()
       .kind = symbol_kind,  // This is .type_ for enums
       ...
   });
   ```

The enum is registered with `type_ = null` because the comment says "Types don't have a Type value." When imported, this becomes `unknownType()`. When the type checker later sees `Color::Red`, it cannot resolve `Color` as an actual enum type.

### Root Cause

The module export/import system doesn't preserve the actual enum type definition. It only records that a symbol named `Color` exists and is an enum, but doesn't store the information needed to:
1. Resolve `Color` as a valid type annotation
2. Look up variants like `Red`, `Green`, `Blue`
3. Validate variant construction expressions like `Color::Red`

### Potential Fix Approaches

1. **Store enum type reference**: Instead of `type_ = null`, store a reference to the actual enum type definition so it can be resolved in the importing module.

2. **Register enum in type namespace**: When importing an enum, register it in the importing module's type namespace (similar to how struct imports should work).

3. **Cross-module type resolution**: Add mechanism for the type checker to look up type definitions from imported modules when resolving type expressions.

### Workaround

Import only functions that return the enum type, not the enum type itself:

```klar
// Instead of: import enum_lib.{ Color }
// Use:        import enum_lib.{ get_red, get_green, get_blue }

import enum_lib.{ get_red }

fn main() -> i32 {
    let c = get_red()  // Type inferred, no explicit Color:: needed
    return 0
}
```

This is severely limiting as it requires wrapper functions for every variant.

### Impact

- Cannot import and use enum types across modules
- Cannot pattern match on imported enums
- Forces duplication of enum definitions or awkward wrapper functions
- Blocks modular code organization for any project using enums

### Related

This bug is independent of Bug 1 (module resolution). Bug 1 prevented finding modules; this bug occurs even when modules are found successfully. Likely also affects struct type imports (not yet verified).
