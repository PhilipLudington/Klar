# Known Bugs and Limitations

## [x] Bug 1: Cannot access array elements via struct field

**Status:** Fixed
**Discovered:** Phase 8 FFI integration testing
**Category:** Parser
**Fixed:** Added lookahead in `parseFieldOrMethod` to distinguish type arguments from array subscripts

The parser did not handle `struct.array_field[index]` syntax correctly because it eagerly consumed `[` assuming type arguments for generic method calls.

**Fix:** In `src/parser.zig`, added `canStartType()` helper and modified `parseFieldOrMethod` to check if the token after `[` can start a type expression. If not (e.g., an integer literal like `0`), the `[` is left for the subscript parser to handle.

```klar
// This now works:
let m: Message = Message { data: [1, 2, 3, 4] }
let x: i32 = m.data[0]  // OK
```

---

## [x] Bug 2: Static method calls on structs fail type checking

**Status:** Fixed
**Discovered:** Phase 8 FFI integration testing
**Category:** Type Checker, Codegen
**Fixed:** Modified type checker and codegen to handle struct static methods via `Type::method()` syntax

The parser creates `EnumLiteral` AST nodes for all `Type::name()` syntax. The fix teaches both the type checker and code generator to handle struct static methods when the type resolves to a struct instead of an enum.

**Fix:**
- `src/checker.zig`: In `checkEnumLiteral`, added handling for struct types - looks up static methods and type-checks arguments
- `src/codegen/emit.zig`: In `emitEnumLiteral`, added `emitStructStaticMethodCall` to emit calls to struct static methods

```klar
// This now works:
let msg: Message = Message::new(100.as[u64])  // OK
let msg2: Message = Message::default()        // OK
```

---

## [ ] Bug 3: Sized extern types not fully supported in method parameters

**Status:** Open
**Discovered:** Phase 8 FFI integration testing
**Category:** Codegen

Sized extern types (`extern type(N)`) work in standalone functions but cause LLVM verification errors when used as struct fields accessed in impl methods.

```klar
extern type(8) SeL4CPtr

pub struct Endpoint {
    cap: SeL4CPtr,
}

impl Endpoint {
    pub fn send(self: ref Self, info: SeL4MessageInfo) -> void {
        unsafe {
            seL4_Send(self.cap, info)  // LLVM error: Invalid indices for GEP
        }
    }
}
```

**Workaround:** Use free functions instead of methods when working with sized extern types.

---

## [ ] Bug 4: Extern type allocations not freed

**Status:** Open (minor)
**Discovered:** Phase 8 FFI integration testing
**Category:** Memory

`ExternType` objects created in `checkExternType` are allocated but not tracked for deallocation. This causes memory leak warnings in debug builds but doesn't affect correctness.

**Location:** `src/checker.zig:8725`
