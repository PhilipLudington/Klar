# Known Bugs and Limitations

## [x] Bug 1: Cannot access array elements via struct field

**Status:** Fixed (verified after rebase from main)
**Discovered:** Phase 8 FFI integration testing
**Category:** Parser

The parser now correctly handles `struct.array_field[index]` syntax.

```klar
pub struct Message {
    data: [i32; 4],
}

fn main() -> i32 {
    let m: Message = Message { data: [10, 20, 30, 40] }
    let x: i32 = m.data[0]  // Works correctly, returns 10
    return x
}
```

---

## [x] Bug 2: Static method calls on structs fail type checking

**Status:** Fixed (verified after rebase from main)
**Discovered:** Phase 8 FFI integration testing
**Category:** Type Checker

The `Type::method()` syntax now works for both enums and structs.

```klar
pub struct Message {
    label: u64,
}

impl Message {
    pub fn new(label: u64) -> Message {
        return Message { label: label }
    }
}

fn main() -> i32 {
    let msg: Message = Message::new(100.as[u64])  // Works correctly
    return 0
}
```

---

## [x] Bug 3: Sized extern types not fully supported in method parameters

**Status:** Fixed
**Discovered:** Phase 8 FFI integration testing
**Category:** Codegen

Sized extern types (`extern type(N)`) now work correctly in impl methods with `ref Self` parameters.

**Root cause:** Two issues in `namedTypeToLLVM`:
1. Extern types weren't being looked up in the type checker, causing fallback to `i32`
2. When computing `reference_inner_type` for self parameters, "Self" wasn't resolved to the actual struct type

**Fix:**
- Added extern type lookup via `type_checker.lookupType()` in `namedTypeToLLVM`
- For self parameters in impl methods, use the struct's LLVM type directly instead of trying to resolve "Self"

```klar
extern type(8) SeL4CPtr

pub struct Endpoint {
    cap: SeL4CPtr,
}

impl Endpoint {
    pub fn send(self: ref Self, info: SeL4MessageInfo) -> void {
        unsafe {
            seL4_Send(self.cap, info)  // Now works correctly
        }
    }
}
```

---

## [x] Bug 4: Extern type allocations not freed

**Status:** Fixed
**Discovered:** Phase 8 FFI integration testing
**Category:** Memory

**Root cause:** `ExternType` objects created in `checkExternType` were allocated but not tracked for deallocation.

**Fix:**
- Added `extern_types: std.ArrayListUnmanaged(*types.ExternType)` field to TypeChecker
- Track allocations in `checkExternType` by appending to the list
- Free allocations in `TypeChecker.deinit()`

No more memory leak warnings in debug builds.
