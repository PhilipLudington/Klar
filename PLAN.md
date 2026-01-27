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

**In Progress:**
- **Milestone 5: Stdlib I/O** - Core I/O complete, filesystem operations pending
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

## Milestone 5: Standard Library - I/O

**Objective:** Implement file and console I/O.

**Status:** 🟡 Read/Write Traits Complete. Mutable buffer I/O with arrays working. Basic file I/O types and stdout/stderr implemented as builtins. Buffered I/O complete. Filesystem operations not yet started.

### Completed
- [x] Mutable buffer allocation (`@repeat`, `ref T`, `inout T`, deref assignment)
- [x] Read trait with File:Read implementation
- [x] Write trait with File:Write, Stdout:Write, Stderr:Write implementations
- [x] IoError enum as builtin type
- [x] File type with open, read, write, close, flush, read_all, read_to_string
- [x] Standard I/O (stdin, stdout, stderr)
- [x] Platform-specific stdio access (macOS, Linux)
- [x] Buffered I/O (BufReader, BufWriter with automatic flush on drop)

### Remaining
- [ ] Path type with path manipulation
- [ ] Directory operations (fs.exists, fs.create_dir, fs.remove, fs.read_dir)
- [ ] Convenience functions (fs.read, fs.read_string, fs.write, fs.write_string)
- [ ] errno mapping for IoError conversion

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
10. **Milestone 5: Stdlib I/O** (filesystem operations) ← **CURRENT**
11. **Milestone 8: Package Manager** (needs modules)
12. **Milestone 9: Tooling** (needs stable language)

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

**AI-Native Development:**
- [x] REPL provides interactive code exploration
- [x] AI assistants can verify code before presenting to users
- [x] Fast feedback loop for iterative development

**Usability:**
- [ ] Can write non-trivial programs (CLI tools, utilities)
- [ ] Error messages are helpful and actionable
- [ ] Documentation exists for language and stdlib

**Tooling:**
- [ ] Package manager works for dependencies
- [ ] IDE support via LSP
- [ ] Code formatter available

**Example Programs:**
- [ ] JSON parser using generics
- [ ] File processing utility
- [ ] HTTP client (stretch goal with async)
