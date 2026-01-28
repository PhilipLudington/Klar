# Klar Phase 4: Language Completion

> **Goal:** Complete the Klar language with generics, traits, modules, standard library, and FFI.

## Current State

**Completed (Phases 1-3):**
- Full compilation pipeline (lexer → parser → checker → LLVM → native)
- Ownership-based memory management (Rc/Arc, automatic drop)
- Basic types, structs, enums, closures, optionals, results
- Parser supports generics/traits/modules syntax
- Three execution backends: interpreter, bytecode VM, native compilation
- 252x speedup for native vs VM

**Completed in Phase 4:**
- [x] **Milestone 1: Generics** - Full generic type checking with monomorphization
- [x] **Milestone 2: Traits** - Trait definitions, implementations, bounds, inheritance, associated types
- [x] **Milestone 3: Modules** - Multi-file compilation with imports and visibility
- [x] **Milestone 4: Stdlib Core** - Option, Result, List, String, Map, Set as builtins
- [x] **Milestone 6: Iterators** - For-loops, Range, collection adapters
- [x] **Milestone 7: Error Handling** - `?` operator, From/Into traits, error context
- [x] **Milestone 10: REPL** - Interactive exploration with interpreter backend
- [x] **Milestone 11: Comptime** - Compile-time evaluation, reflection, assertions
- [x] **Milestone 12: FFI** - Foreign Function Interface for C interoperability
- [x] **Milestone 5: Stdlib I/O** - File I/O, buffered I/O, Path type, filesystem operations

**In Progress:**
- **Milestone 13: FFI Function Pointers** - Phases 1-5 complete (Phase 6 deferred) ← **CURRENT**
- **Milestone 8: Package Manager** - Not started
- **Milestone 9: Tooling** - Not started

> **Previous plans archived:** [Phase 4 History](docs/history/phase4-language-completion.md)

---

## Milestone 1: Generic Type Checking ✅

**Status:** Complete. Generic functions, structs, enums, and struct methods all working.

See [Phase 4 History](docs/history/phase4-language-completion.md#milestone-1-generic-type-checking) for full details.

---

## Milestone 2: Trait System ✅

**Status:** Complete. Core trait infrastructure complete (trait registry, definition validation, impl checking, bounds parsing, method resolution through bounds, trait inheritance, associated types).

See [Phase 4 History](docs/history/phase4-language-completion.md#milestone-2-trait-system) for full details.

---

## Milestone 3: Module System ✅

**Status:** Complete. Multi-file compilation works with selective imports, visibility enforcement, and topological ordering.

See [Phase 4 History](docs/history/phase4-language-completion.md#milestone-3-module-system) for full details.

---

## Milestone 4: Standard Library - Core ✅

**Status:** Complete. Optional, Result, builtin type methods, List[T], String, Map[K,V], and Set[T] all implemented as builtin types.

See [Phase 4 History](docs/history/phase4-language-completion.md#milestone-4-standard-library---core) for full details.

---

## Milestone 5: Standard Library - I/O ✅

**Objective:** Implement file and console I/O.

**Status:** Complete. File I/O, buffered I/O, Path type, and filesystem operations all implemented.

### Completed
- [x] Mutable buffer allocation (`@repeat`, `ref T`, `inout T`, deref assignment)
- [x] Read trait with File:Read implementation
- [x] Write trait with File:Write, Stdout:Write, Stderr:Write implementations
- [x] IoError enum as builtin type
- [x] File type with open, read, write, close, flush, read_all, read_to_string
- [x] Standard I/O (stdin, stdout, stderr)
- [x] Platform-specific stdio access (macOS, Linux)
- [x] Buffered I/O (BufReader, BufWriter with automatic flush on drop)
- [x] Path type with path manipulation methods
- [x] Directory operations (fs_exists, fs_is_file, fs_is_dir, fs_create_dir, fs_create_dir_all, fs_remove_file, fs_remove_dir, fs_read_dir)
- [x] Convenience functions (fs_read_string, fs_write_string)

### Path Type Methods
- `Path.new(s: string) -> Path` - construct from string
- `.to_string() -> string` - convert to string
- `.join(other: string) -> Path` - join path components (handles trailing slashes)
- `.parent() -> ?Path` - get parent directory (returns `None` for paths without `/`)
- `.file_name() -> ?string` - get filename component
- `.extension() -> ?string` - get file extension
- `.exists() -> bool` - check if path exists
- `.is_file() -> bool` - check if path is a file
- `.is_dir() -> bool` - check if path is a directory

> **Note on `.parent()` behavior:**
> - `Path.new("/home/user").parent()` → `Some(Path("/home"))`
> - `Path.new("/").parent()` → `Some(Path("/"))` (root is its own parent)
> - `Path.new("file.txt").parent()` → `None` (no directory separator)
> - `Path.new("dir/file.txt").parent()` → `Some(Path("dir"))`

### Filesystem Functions
- `fs_exists(path: string) -> bool`
- `fs_is_file(path: string) -> bool`
- `fs_is_dir(path: string) -> bool`
- `fs_create_dir(path: string) -> Result[void, IoError]`
- `fs_create_dir_all(path: string) -> Result[void, IoError]`
- `fs_remove_file(path: string) -> Result[void, IoError]`
- `fs_remove_dir(path: string) -> Result[void, IoError]`
- `fs_read_string(path: string) -> Result[String, IoError]`
- `fs_write_string(path: string, content: string) -> Result[void, IoError]`
- `fs_read_dir(path: string) -> Result[List[String], IoError]`

### Tests
All filesystem tests in `test/native/fs/`:
- fs_exists.kl, fs_create_remove.kl, fs_read_write.kl, fs_read_dir.kl
- path_basic.kl, path_exists.kl

### Platform Support
> **Note:** File I/O and filesystem operations use POSIX syscalls (stat, mkdir, unlink, rmdir, opendir, readdir, etc.) and currently support **macOS and Linux only**. Windows is not implemented.

---

## Milestone 6: Iterator Protocol ✅

**Status:** Complete. For-loops work with Range[T], arrays, List[T], Set[T], and Map[K,V]. Iterator adapter methods implemented on collection types.

See [Phase 4 History](docs/history/phase4-language-completion.md#milestone-6-iterator-protocol) for full details.

---

## Milestone 7: Error Handling Improvements ✅

**Status:** Complete. `?` operator implemented for early return on Optional and Result types, with automatic error conversion via From trait. Error context via `.context()` method implemented.

See [Phase 4 History](docs/history/phase4-language-completion.md#milestone-7-error-handling-improvements) for full details.

---

## Milestone 8: Package Manager

**Objective:** Implement basic package management.

**Status:** Not started.

### Manifest Format
- [ ] Define klar.toml schema
- [ ] Parse [package] section (name, version, authors)
- [ ] Parse [dependencies] section
- [ ] Parse [dev-dependencies] section
- [ ] Support git dependencies with tag/branch/commit

### CLI Commands
- [ ] `klar init` - Create new project directory structure
- [ ] `klar build` - Read klar.toml, resolve dependencies, build all files
- [ ] `klar run` - Build if needed, execute binary
- [ ] `klar test` - Discover and run test functions
- [ ] `klar add` - Add dependency to klar.toml

### Dependency Resolution
- [ ] Build dependency graph
- [ ] Resolve version constraints
- [ ] Handle diamond dependencies
- [ ] Generate klar.lock for reproducible builds

---

## Milestone 9: Tooling

**Objective:** Developer tooling for productive Klar development.

**Status:** Not started.

### Code Formatter (klar fmt)
- [ ] Parse source file into AST
- [ ] Pretty-print AST with consistent formatting
- [ ] Preserve/normalize comments

### Documentation Generator (klar doc)
- [ ] Extract `///` documentation comments
- [ ] Generate HTML documentation
- [ ] Include type signatures and cross-references

### Language Server Protocol (LSP)
- [ ] Implement JSON-RPC server
- [ ] textDocument/hover, definition, references, completion, diagnostics

### VS Code Extension
- [ ] Syntax highlighting grammar
- [ ] LSP client integration

---

## Milestone 10: REPL ✅

**Status:** Complete. Basic REPL works with interpreter backend.

See [Phase 4 History](docs/history/phase4-language-completion.md#milestone-10-repl) for full details.

---

## Milestone 11: Comptime ✅

**Status:** Complete. Comptime blocks, functions, parameters, reflection, and assertions all working.

See [Phase 4 History](docs/history/phase4-language-completion.md#milestone-11-comptime) for full details.

---

## Milestone 12: FFI (Foreign Function Interface) ✅

**Objective:** Enable Klar programs to call C functions, use C-compatible type layouts, work with raw pointers, and clearly mark unsafe operations.

**Status:** Complete. All phases implemented.

> **Full specification:** [klar-ffi-spec.md](klar-ffi-spec.md)
> **Documentation:** [docs/advanced/ffi.md](docs/advanced/ffi.md)

### Phase 1: Unsafe Blocks ✅
- [x] `unsafe { ... }` blocks as expressions/statements
- [x] `unsafe fn` declarations
- [x] Track "unsafe context" during type checking
- [x] Error when unsafe operations occur outside unsafe context

### Phase 2: External Type Declarations ✅
- [x] `extern type Name` (opaque, unknown size)
- [x] `extern type(N) Name` (sized, N bytes)
- [x] Generate LLVM pointer type for unsized, `[N x i8]` for sized

### Phase 3: Pointer Types ✅
- [x] `CPtr[T]` (non-null raw pointer)
- [x] `COptPtr[T]` (nullable raw pointer)
- [x] `CStr` (borrowed null-terminated string)
- [x] Builtin functions: `is_null`, `unwrap_ptr`, `offset`, `read`, `write`, `ref_to_ptr`, `ptr_cast`

### Phase 4: External Function Declarations ✅
- [x] `extern { fn name(...) -> Type }` blocks
- [x] `out` parameter modifier
- [x] Variadic `...` in parameter lists
- [x] C calling convention

### Phase 5: C-Compatible Struct Layout ✅
- [x] `extern struct Name { ... }` with C ABI layout
- [x] `extern struct packed Name { ... }` for packed layout

### Phase 6: C-Compatible Enum Layout ✅
- [x] `extern enum Name: IntType { Variant = Value, ... }`
- [x] Explicit integer repr type and variant values

### Phase 7: String Conversions ✅
- [x] `string.as_cstr() -> CStr` (borrow as C string)
- [x] `CStr.to_string() -> String` (copy to Klar String)
- [x] `CStr.len() -> usize`, `CStr.from_ptr()`

### Phase 8: Integration & Linking ✅
- [x] `-l` flag for linking additional system libraries
- [x] `-L` flag for library search paths
- [x] ABI-compliant struct passing and returns
- [x] Tested on macOS and Linux

### Phase 9: Deferred FFI Features ✅
- [x] Extern type validation (unsized only behind pointers, sized by value)
- [x] Out parameters at call sites
- [x] `CStrOwned` type with automatic deallocation
- [x] `ptr_cast[U](ptr)` builtin with explicit type argument syntax
- [x] `unsafe trait` and `unsafe impl` declarations

### Tests
All FFI tests in `test/native/ffi/`:
- unsafe_block.kl, unsafe_fn.kl, unsafe_error.kl
- extern_type_opaque.kl, extern_type_sized.kl
- cptr_basic.kl, ptr_functions.kl, ptr_cast.kl
- extern_fn_*.kl, extern_struct*.kl, extern_enum.kl
- string_to_cstr.kl, cstr_to_string.kl, cstr_owned.kl
- unsafe_trait.kl, unsafe_trait_error.kl, unsafe_impl_error.kl
- call_c_function.kl, sel4_bindings.kl

---

## Milestone 13: FFI Function Pointers

**Objective:** Enable Klar to pass function pointers to C code (callbacks) and receive/call C function pointers, completing bidirectional FFI.

**Status:** Phases 1-5 complete. Phase 6 (Variadic) deferred.

> **Depends on:** Milestone 12 (FFI) ✅

### Background

Milestone 12 implemented calling C functions from Klar, but many C APIs require passing function pointers as callbacks (e.g., qsort, signal handlers, event callbacks, plugin systems). This milestone adds:
1. Passing Klar functions to C as callbacks
2. Receiving C function pointers in Klar
3. Calling C function pointers from Klar code

### Design Principles

1. **Safety First**: Function pointer operations require `unsafe` context
2. **Explicit Types**: `extern fn` clearly distinguishes C function pointers from Klar closures
3. **No Hidden Magic**: Closures with captures cannot become C callbacks (environment pointer incompatible)
4. **Consistent Syntax**: Uses existing `extern` keyword (like `extern type`, `extern struct`)
5. **Reuse Existing Features**: Nullable pointers use `?extern fn` (existing optional syntax)

### Phase 1: Function Pointer Type Syntax ✅

**Objective:** Add syntax for declaring function pointer types in FFI context.

- [x] Add `extern fn(Args) -> Ret` type for C function pointers
  - Represents a raw C function pointer (no environment)
  - Can be received from C code
  - Can be called from Klar (requires unsafe)
- [x] Parse `extern fn` as a type expression (distinct from `extern { fn }` declarations)
- [x] Add to type system (`extern_fn` variant in `Type` union)
- [x] LLVM codegen: map to LLVM function pointer type (8 bytes, not 16-byte closure struct)

**Example:**
```klar
// Receive a C function pointer
extern {
    fn get_callback() -> extern fn(i32) -> i32
    fn set_callback(cb: extern fn(i32, i32) -> void)
}
```

### Phase 2: Calling C Function Pointers ✅

**Objective:** Enable calling received C function pointers from Klar code.

- [x] Implement call syntax for `extern fn` values
- [x] Type check arguments and return type
- [x] Require unsafe context for calls (raw pointer dereference)
- [x] LLVM codegen: emit indirect call instruction
- [x] `extern fn` types accepted as FFI-compatible in extern function signatures

**Example:**
```klar
fn use_callback() {
    let cb: extern fn(i32) -> i32 = unsafe { get_callback() }

    // Call the C function pointer (requires unsafe)
    let result: i32 = unsafe { cb(42) }
    println(result.to_string())
}
```

### Phase 3: Creating C-Compatible Callbacks ✅

**Objective:** Convert Klar functions to C function pointers for passing to C APIs.

- [x] Add `@fn_ptr` builtin to get raw function pointer from:
  - Named functions (always works)
  - Closures with NO captures (stateless closures)
- [x] Compile-time error for closures with captures (incompatible with C ABI)
- [x] Return type is `extern fn(...) -> R`

**Example:**
```klar
// Named function - can always get pointer
fn my_compare(a: i32, b: i32) -> i32 {
    return a - b
}

fn use_qsort() {
    // Get function pointer from named function
    let cmp: extern fn(i32, i32) -> i32 = @fn_ptr(my_compare)

    unsafe {
        // Pass to C qsort
        c_qsort(arr_ptr, len, size, cmp)
    }
}

// Stateless closure - works (no captures)
let add_one: extern fn(i32) -> i32 = @fn_ptr(|x: i32| -> i32 { return x + 1 })

// Closure with captures - COMPILE ERROR
let offset: i32 = 10
let bad: extern fn(i32) -> i32 = @fn_ptr(|x: i32| -> i32 { return x + offset })
// Error: Cannot create C function pointer from closure with captures
```

### Phase 4: Function Pointer Parameters in Extern Functions ✅

**Objective:** Declare extern functions that accept function pointer parameters.

- [x] Support `extern fn` parameters in extern function declarations
- [x] Support `extern fn` return types in extern function declarations
- [x] Generate correct LLVM function signatures
- [x] Pass function pointers with C calling convention

**Example:**
```klar
extern {
    // qsort callback
    fn qsort(
        base: CPtr[void],
        nmemb: usize,
        size: usize,
        compar: extern fn(CPtr[void], CPtr[void]) -> i32
    )

    // Signal handler
    fn signal(
        signum: i32,
        handler: extern fn(i32) -> void
    ) -> extern fn(i32) -> void

    // Callback with user data (common pattern)
    fn register_callback(
        callback: extern fn(CPtr[void]) -> void,
        user_data: CPtr[void]
    )
}
```

### Phase 5: Optional Function Pointers ✅

**Objective:** Support nullable function pointers for optional callbacks.

- [x] Use `?extern fn(...) -> R` for nullable function pointers (existing optional syntax)
- [x] Return None via implicit return in function (matches Klar's optional semantics)
- [x] Unwrap with match pattern to extract function pointer
- [x] Handle C's common pattern of NULL callback = no-op

**Example:**
```klar
extern {
    fn set_optional_callback(cb: ?extern fn(i32) -> void)
}

fn setup() {
    // Pass None for no callback
    unsafe { set_optional_callback(None) }

    // Or pass a real callback wrapped in Some
    let cb: ?extern fn(i32) -> void = Some(@fn_ptr(my_handler))
    unsafe { set_optional_callback(cb) }
}
```

### Phase 6: Variadic C Function Pointers (Deferred)

**Objective:** Support function pointers to variadic C functions.

- [ ] Parse variadic function pointer types: `extern fn(i32, ...) -> void`
- [ ] Only usable with extern declarations (cannot create in Klar)
- [ ] Example: printf-style callbacks

**Note:** This phase may be deferred if use cases are rare.

### Implementation Tasks

#### Type System (src/types.zig)
- [x] Add `extern_fn: *ExternFnType` variant to Type union
- [x] `ExternFnType` contains function signature (params, return type)
- [x] Equality and hash for extern function pointer types
- [x] Optional extern fn uses existing optional type machinery
- [x] `isCopyType()` returns true for extern_fn (just an address)
- [x] `formatType()` formats as "extern fn(...) -> ..."
- [x] `TypeBuilder.externFnType()` constructor

#### Lexer/Parser (src/lexer.zig, src/parser.zig)
- [x] Parse `extern fn(Args) -> Ret` as a type expression
- [x] Distinguish from `extern { fn ... }` declaration blocks
- [x] Parse `@fn_ptr(expr)` builtin call

#### Type Checker (src/checker.zig)
- [x] Validate `extern fn` type structure
- [x] Resolve extern_function AST node to extern_fn Type
- [x] Type substitution for generics with extern_fn
- [x] containsTypeVar() for extern_fn
- [x] unifyTypes() for extern_fn
- [x] Type check extern function pointer calls (argument/return types)
- [x] Require unsafe context for extern function pointer calls
- [x] Accept extern_fn as FFI-compatible type
- [x] Check `@fn_ptr` argument is function or stateless closure
- [x] Error on closure with captures for `@fn_ptr`

#### LLVM Codegen (src/codegen/emit.zig)
- [x] Convert `extern fn` type to LLVM pointer type (ptr, not closure struct)
- [x] Handle `?extern fn` as `{ i1, ptr }` optional struct
- [x] Type mangling for extern_fn
- [x] getSizeOfKlarType() returns 8 for extern_fn
- [x] Emit indirect call for extern function pointer invocation
- [x] Emit `@fn_ptr` as address-of for named functions
- [x] Emit `@fn_ptr` for stateless closures (create C-compatible wrapper function)

### Tests

Create `test/native/ffi/fn_ptr/`:
- [x] extern_fn_type.kl - Basic `extern fn` type declarations
- [x] extern_fn_call.kl - Calling C function pointers
- [x] extern_fn_create.kl - Creating function pointers with `@fn_ptr`
- [x] extern_fn_closure.kl - Stateless closure to function pointer
- [x] extern_fn_capture_error.kl - Error on closure with captures (negative test)
- [x] extern_fn_param.kl - Function pointer parameters in extern functions (in scratch/)
- [x] extern_fn_optional.kl - Nullable function pointers with `?extern fn`
- [ ] extern_fn_qsort.kl - Real-world qsort callback example

### Documentation

- [ ] Add section to docs/advanced/ffi.md
- [ ] Update klar-ffi-spec.md with function pointer specification
- [ ] Add examples in docs/examples/

### Success Criteria

- [x] `extern fn` type parses and type-checks correctly
- [x] Can pass Klar functions to C APIs expecting callbacks via `@fn_ptr`
- [x] Can receive and call C function pointers (`extern fn` values)
- [x] Compile-time error prevents capturing closures from becoming C callbacks
- [x] `?extern fn` works for nullable function pointers
- [ ] qsort example works with custom comparator
- [ ] Signal handler example works
- [x] All tests pass

---

## Stretch Goals

These are valuable but not required for Phase 4 completion:

### Async/Await
- [ ] Design async runtime model
- [ ] Implement Future trait
- [ ] Implement async fn transformation
- [ ] Implement .await syntax

### Self-Hosting
- [ ] Port lexer to Klar
- [ ] Port parser to Klar
- [ ] Port checker to Klar
- [ ] Full self-hosted compiler

### WebAssembly Target
- [ ] Add WASM backend to codegen
- [ ] Handle WASM-specific ABI
- [ ] Test in browser/Node.js

### REPL Enhancements
- [ ] Multi-file import support
- [ ] Tab completion for identifiers
- [ ] History persistence across sessions

### Windows Support
- [ ] Windows target triple detection
- [ ] Windows-specific stdio access
- [ ] Windows CI testing

---

## Implementation Order

Based on dependencies:

1. **Milestone 1: Generics** (foundation for everything) ✅
2. **Milestone 2: Traits** (needs generics) ✅
3. **Milestone 3: Modules** (needed for stdlib) ✅
4. **Milestone 10: REPL** (uses interpreter, enables AI workflow) ✅
5. **Milestone 11: Comptime** (uses interpreter, enables metaprogramming) ✅
6. **Milestone 6: Iterators** (for-loops, collection adapters) ✅
7. **Milestone 4: Stdlib Core** (needs generics, traits, modules) ✅
8. **Milestone 7: Error Handling** (`?` operator, From/Into traits) ✅
9. **Milestone 12: FFI** (C interoperability) ✅
10. **Milestone 5: Stdlib I/O** (filesystem operations) ✅
11. **Milestone 13: FFI Function Pointers** (callbacks, completing FFI) ← **CURRENT**
12. **Milestone 8: Package Manager** (needs modules)
13. **Milestone 9: Tooling** (needs stable language)

---

## Success Criteria

Phase 4 is complete when:

**Language Completeness:**
- [x] Generic functions and types work correctly
- [x] Traits can be defined and implemented
- [x] Multi-file projects compile
- [x] Standard library provides core functionality (as builtins)
- [x] Comptime enables compile-time metaprogramming
- [x] For-loops work with Range, arrays, List, Set, Map
- [x] `?` operator for error propagation
- [x] FFI enables C interoperability
- [ ] FFI function pointers enable callbacks to C code

**AI-Native Development:**
- [x] REPL provides interactive code exploration
- [x] AI assistants can verify code before presenting to users
- [x] Fast feedback loop for iterative development

**Usability:**
- [x] Can write non-trivial programs (CLI tools, utilities) - filesystem I/O complete
- [ ] Error messages are helpful and actionable
- [ ] Documentation exists for language and stdlib

**Tooling:**
- [ ] Package manager works for dependencies
- [ ] IDE support via LSP
- [ ] Code formatter available

**Example Programs:**
- [ ] JSON parser using generics
- [x] File processing utility - now possible with fs_* functions
- [ ] HTTP client (stretch goal with async)
