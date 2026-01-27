# Klar FFI Implementation Plan

**Status:** Complete (All Phases Done)
**Goal:** Implement Foreign Function Interface (FFI) for C interoperability

> **Previous plans:** [Phase 4: Language Completion](docs/history/phase4-language-completion.md) (generics, traits, modules, stdlib)

---

## Overview

This plan implements the FFI specification (`klar-ffi-spec.md`) to enable Klar programs to:
1. Call C functions
2. Use C-compatible type layouts
3. Work with raw pointers safely
4. Clearly mark unsafe operations

---

## Phase 1: Unsafe Blocks ✅

**Objective:** Add `unsafe` keyword and block syntax as the foundation for all FFI features.

### 1.1 Lexer Changes
- [x] Add `unsafe` keyword token (already existed)

### 1.2 Parser Changes
- [x] Parse `unsafe { ... }` blocks as expressions/statements
- [x] Parse `unsafe fn` declarations
- ~~Parse `unsafe trait` and `unsafe impl`~~ → moved to Phase 9

### 1.3 AST Changes
- [x] Add `UnsafeBlock` node wrapping inner statements
- [x] Add `is_unsafe` flag to `FunctionDecl`

### 1.4 Checker Changes
- [x] Track "unsafe context" during type checking (`in_unsafe_context` field)
- [x] Error when unsafe operations occur outside unsafe context
- [x] Propagate unsafe requirement through call chains (via `FunctionType.is_unsafe`)

### 1.5 Codegen
- [x] No special codegen needed - unsafe is a compile-time check only
- [x] All backends (codegen, compiler, interpreter, ownership) handle `unsafe_block`

### 1.6 Tests
- [x] `test/native/ffi/unsafe_block.kl` - basic unsafe blocks
- [x] `test/native/ffi/unsafe_fn.kl` - unsafe function declarations
- [x] `test/native/ffi/unsafe_error.kl` - verify errors outside unsafe

---

## Phase 2: External Type Declarations ✅

**Objective:** Support `extern type` for opaque and sized external types.

### 2.1 Lexer Changes
- [x] Add `extern` keyword token

### 2.2 Parser Changes
- [x] Parse `extern type Name` (opaque, unknown size)
- [x] Parse `extern type(N) Name` (sized, N bytes)

### 2.3 AST Changes
- [x] Add `ExternTypeDecl` node with optional size

### 2.4 Type System Changes
- [x] Add `ExternType` variant to type representation
- [x] Track whether extern type has known size
- [x] Prevent construction/field access of extern types (inherent - no constructors)

### 2.5 Checker Changes
- [x] Register extern types in scope
- ~~Validate extern types can only be used behind pointers (if unsized)~~ → moved to Phase 9
- ~~Allow sized extern types to be passed by value~~ → moved to Phase 9

### 2.6 Codegen
- [x] Generate LLVM pointer type for unsized extern types
- [x] Generate LLVM `[N x i8]` for sized extern types

### 2.7 Tests
- [x] `test/native/ffi/extern_type_opaque.kl`
- [x] `test/native/ffi/extern_type_sized.kl`

---

## Phase 3: Pointer Types ✅

**Objective:** Add `CPtr[T]`, `COptPtr[T]`, and `CStr` types.

### 3.1 Type System Changes
- [x] Add `CPtr` generic type (non-null raw pointer)
- [x] Add `COptPtr` generic type (nullable raw pointer)
- [x] Add `CStr` type (borrowed null-terminated string)

### 3.2 Builtin Functions
- [x] `is_null[T](ptr: COptPtr[T]) -> bool` - safe, no unsafe required
- [x] `unwrap_ptr[T](ptr: COptPtr[T]) -> CPtr[T]` - unsafe
- [x] `offset[T](ptr: CPtr[T], count: isize) -> CPtr[T]` - unsafe
- [x] `read[T](ptr: CPtr[T]) -> T` - unsafe
- [x] `write[T](ptr: CPtr[T], value: T) -> void` - unsafe
- [x] `ref_to_ptr[T](value: ref T) -> CPtr[T]` - unsafe
- ~~`ptr_cast[U](ptr: CPtr[T]) -> CPtr[U]`~~ → moved to Phase 9

### 3.3 Checker Changes
- [x] Type check pointer operations
- [x] Enforce unsafe requirement for dereference operations
- [x] Validate type parameters for pointer generics

### 3.4 Codegen
- [x] Map `CPtr[T]` to LLVM `ptr` (opaque pointer)
- [x] Map `COptPtr[T]` to LLVM `ptr`
- [x] Generate null checks for `is_null`
- [x] Generate `getelementptr` for `offset`
- [x] Generate `load`/`store` for `read`/`write`
- ~~Generate `bitcast` for `ptr_cast`~~ → moved to Phase 9

### 3.5 Tests
- [x] `test/native/ffi/cptr_basic.kl`
- [x] `test/native/ffi/ptr_type_inference.kl`
- [x] `test/native/ffi/ptr_functions.kl`
- [x] `test/native/ffi/ptr_unsafe_required.kl`

---

## Phase 4: External Function Declarations ✅

**Objective:** Support `extern { fn name(...) -> Type }` blocks.

### 4.1 Parser Changes
- [x] Parse `extern { ... }` blocks containing function declarations
- [x] Parse `out` parameter modifier (contextual keyword)
- [x] Parse variadic `...` in parameter lists

### 4.2 AST Changes
- [x] Add `ExternBlock` node containing list of extern fn declarations
- [x] Add `is_extern` flag to `FunctionDecl`
- [x] Add `is_out` flag to parameters
- [x] Add `is_variadic` flag to function signatures

### 4.3 Checker Changes
- [x] Validate extern function signatures use FFI-compatible types
- [x] Track which functions are extern (require unsafe to call)
- [x] Validate variadic functions have at least one non-variadic param
- ~~Handle `out` parameters (allocate stack space, pass pointer)~~ → moved to Phase 9

### 4.4 Codegen
- [x] Generate LLVM `declare` for extern functions
- [x] Use C calling convention (`ccc`)
- [x] Handle variadic calls with LLVM vararg support
- ~~For `out` params: alloca + pass pointer + mark initialized after call~~ → moved to Phase 9

### 4.5 Tests
- [x] `test/native/ffi/extern_fn_basic.kl` - basic extern fn declaration
- [x] `test/native/ffi/extern_fn_call.kl` - call libc `exit`
- [x] `test/native/ffi/extern_fn_out.kl` - out parameter syntax
- [x] `test/native/ffi/extern_fn_variadic.kl` - variadic function syntax

---

## Phase 5: C-Compatible Struct Layout ✅

**Objective:** Support `extern struct` with C ABI-compatible layout.

### 5.1 Parser Changes
- [x] Parse `extern struct Name { ... }`
- [x] Parse `extern struct packed Name { ... }`
- [x] Parse `pub extern struct` for visibility

### 5.2 AST Changes
- [x] Add `is_extern` flag to `StructDecl`
- [x] Add `is_packed` flag to `StructDecl`

### 5.3 Type System Changes
- [x] Track struct layout kind (Klar default vs C-compatible vs packed)

### 5.4 Checker Changes
- [x] Validate extern struct fields are FFI-compatible types
- [x] Prevent generic type parameters in extern structs

### 5.5 Codegen
- [x] For extern structs: use LLVM struct type with no packing (C layout)
- [x] For packed structs: use LLVM packed struct
- [x] Ensure field order matches declaration order
- [x] Use target DataLayout for alignment

### 5.6 Tests
- [x] `test/native/ffi/extern_struct.kl`
- [x] `test/native/ffi/extern_struct_packed.kl`

---

## Phase 6: C-Compatible Enum Layout ✅

**Objective:** Support `extern enum Name: IntType { ... }` with explicit repr.

### 6.1 Parser Changes
- [x] Parse `extern enum Name: Type { Variant = Value, ... }`
- [x] Require explicit integer type for extern enums
- [x] Require explicit values for all variants

### 6.2 AST Changes
- [x] Add `is_extern` flag to `EnumDecl`
- [x] Add `repr_type` field to `EnumDecl`
- [x] Add `value` field to `EnumVariant`

### 6.3 Type System Changes
- [x] Add `is_extern` and `repr_type` to `EnumType`
- [x] Add `value` field to `EnumVariant`

### 6.4 Checker Changes
- [x] Validate repr type is an integer type
- [x] Validate all variants have explicit values
- [x] Validate values fit in repr type
- [x] Prevent payload variants in extern enums
- [x] Prevent type parameters on extern enums
- [x] Update `isFfiCompatibleType` to accept extern enums

### 6.5 Codegen
- [x] Generate extern enum as the repr integer type
- [x] Use explicit discriminant values for extern enum literals

### 6.6 Tests
- [x] `test/native/ffi/extern_enum.kl`

---

## Phase 7: String Conversions ✅

**Objective:** Implement string conversion between Klar `string` and C `CStr`.

### 7.1 Type Additions
- ~~`CStrOwned` type (owned null-terminated string)~~ → moved to Phase 9

### 7.2 Built-in Methods on `string` and `String`
- [x] `fn as_cstr(self: ref Self) -> CStr` - borrow as C string (works on primitive string and String)
- ~~`fn to_cstr(self: ref Self) -> CStrOwned`~~ → moved to Phase 9

### 7.3 Built-in Methods on `CStr`
- [x] `unsafe fn to_string(self: Self) -> String` - copy to Klar String
- [x] `unsafe fn len(self: Self) -> usize` - get length
- [x] `fn from_ptr(ptr: CPtr[i8]) -> CStr` - construct from pointer

### 7.4 Codegen
- [x] `as_cstr`: return pointer to string data (string literals and String are null-terminated)
- [x] `to_string`: call strlen, allocate, memcpy
- [x] `len`: call strlen

### 7.5 Tests
- [x] `test/native/ffi/string_to_cstr.kl`
- [x] `test/native/ffi/cstr_to_string.kl`

---

## Phase 8: Integration & Linking ✅

**Objective:** Ensure Klar object files link with C object files.

### 8.1 Build System
- [x] Basic libc linking works (puts, strlen, malloc, free, etc.)
- [x] Document linking with C libraries (see docs/advanced/ffi.md)
- [x] Add `-l` flag for linking additional system libraries
- [x] Add `-L` flag for library search paths

### 8.2 ABI Compliance
- [x] Verify struct passing matches C ABI (by value vs by pointer)
- [x] Verify return value handling (small structs returned in registers)
- [x] Test on multiple platforms (macOS, Linux via Docker)

### 8.3 Integration Tests
- [x] FFI tests already link with libc successfully
- [x] `test/native/ffi/extern_fn_return_struct.kl` - C struct return values
- [x] `test/native/ffi/sel4_bindings.kl` - seL4-style FFI bindings
- ~~`test/native/ffi/call_c_function.kl` - call custom C code~~ → moved to Phase 9

---

## Phase 9: Deferred FFI Features ✅

**Status:** Complete
**Objective:** Complete remaining FFI features organized by implementation priority.

### Batch A: Quick Wins (Validation & Testing) ✅

- [x] `test/native/ffi/call_c_function.kl` - test linking with custom C code
- [x] Validate unsized extern types can only be used behind pointers (`CPtr[T]`, `COptPtr[T]`)
- [x] Allow sized extern types to be passed by value to C functions

### Batch B: Out Parameters ✅

- [x] Parser: Parse `out identifier` syntax at call sites
- [x] Checker: Validate `out` arguments match extern function's out parameters
- [x] Checker: Track out_params bitmask in FunctionType
- [x] Codegen: For `out` params - pass pointer to variable's alloca
- [x] Test: `test/native/ffi/extern_fn_out_call.kl` - end-to-end out parameter test

### Batch C: String Ownership ✅

- [x] Add `CStrOwned` type (owned null-terminated string)
- [x] Add `fn to_cstr(self: ref Self) -> CStrOwned` method to `string`/`String`
- [x] Add `as_cstr()`, `to_string()`, and `len()` methods to `CStrOwned`
- [x] Automatic memory deallocation (Drop) for `CStrOwned` at scope exit
- [x] Test: `test/native/ffi/cstr_owned.kl` - end-to-end CStrOwned test

### Batch D: Pointer Cast ✅

- [x] Add `ptr_cast[U](ptr: CPtr[T]) -> CPtr[U]` builtin (needs type arg syntax at call site)
- [x] Parser: Support `func[Type](args)` syntax for function calls with explicit type arguments
- [x] Codegen: Generate appropriate LLVM for `ptr_cast`
- [x] Test: `test/native/ffi/ptr_cast.kl` - end-to-end ptr_cast test

### Batch E: Unsafe Traits ✅

- [x] Parse `unsafe trait` declarations
- [x] Parse `unsafe impl` declarations
- [x] Add `is_unsafe` field to TraitDecl and ImplDecl AST nodes
- [x] Add `is_unsafe` field to TraitType in type system
- [x] Validate that unsafe traits require unsafe impl
- [x] Validate that unsafe impl can only be used with unsafe traits
- [x] Test: `test/native/ffi/unsafe_trait.kl` - end-to-end unsafe trait test
- [x] Test: `test/native/ffi/unsafe_trait_error.kl` - error when missing unsafe impl
- [x] Test: `test/native/ffi/unsafe_impl_error.kl` - error when unsafe impl on safe trait

---

## Implementation Order

The phases have dependencies:

```
Phase 1 (unsafe) ──┬──> Phase 3 (pointers)
                   │
                   ├──> Phase 4 (extern fn)
                   │
Phase 2 (extern type) ──> Phase 4 (extern fn)

Phase 3 (pointers) ──> Phase 7 (strings)

Phase 4 (extern fn) ──┬──> Phase 5 (extern struct)
                      │
                      └──> Phase 6 (extern enum)

Phase 5,6 ──> Phase 8 (integration)
```

**Recommended sequence:**
1. Phase 1: Unsafe blocks (foundation)
2. Phase 2: External types (needed for pointers)
3. Phase 3: Pointer types (needed for extern fn)
4. Phase 4: External functions (core FFI capability)
5. Phase 5: External structs (common FFI pattern)
6. Phase 6: External enums (common FFI pattern)
7. Phase 7: String conversions (convenience)
8. Phase 8: Integration testing

---

## File Changes Summary

### New Files
- `test/native/ffi/*.kl` - FFI test cases

### Modified Files
- `src/lexer.zig` - `unsafe`, `extern` keywords
- `src/parser.zig` - extern blocks, unsafe blocks, out params
- `src/ast.zig` - new node types
- `src/types.zig` - CPtr, COptPtr, CStr, ExternType
- `src/checker.zig` - unsafe context tracking, FFI validation
- `src/codegen/emit.zig` - LLVM IR for extern functions and types

---

## Known Limitations

All previously tracked bugs have been fixed. See `BUG.md` for history.

**Deferred features** are now tracked in Phase 9, organized by implementation batch.

---

## Future Considerations

These items may be addressed beyond Phase 9:

1. **Variadic type safety**: How strictly should we validate arguments to variadic functions? (Currently minimal validation)

2. **Platform-specific types**: Should we add `c_int`, `c_long`, etc. type aliases for portable FFI code?

---

## Success Criteria

- [x] Can call libc functions (puts, printf, malloc, free)
- [x] Can define C-compatible structs and pass to C functions
- [x] Can receive C structs from C functions
- [x] Can work with C strings safely
- [x] All unsafe operations require explicit unsafe blocks
- [x] Klar binaries link cleanly with C libraries
- [x] seL4 example from spec compiles and type-checks
