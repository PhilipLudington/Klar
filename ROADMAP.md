# Klar — Roadmap

## Phase 0: Tree-walking Interpreter ✅
**Status:** Complete

## Phase 1: Bytecode VM ✅
**Status:** Complete

## Phase 2: Native Compiler (LLVM) ✅
**Status:** Complete

---

## Phase 3: Language Completion

### Generic Type Checking
- [x] Track type parameters in checker scope
- [x] Implement type parameter substitution
- [x] Implement monomorphization (generate concrete types at call sites)
- [x] Support generic structs
- [x] Support generic enums
- [x] Support generic functions
- [x] Implement type inference at call sites
- [x] Cache monomorphized instances to avoid duplication

### Trait System
- [x] Trait definition parsing and checking
- [x] Trait implementation
- [x] Trait bounds on generics
- [x] Multiple trait bounds
- [ ] Default method implementations
- [x] Associated types
- [x] Self type in trait methods
- [ ] Derive macro basics

### Module System
- [ ] Module declaration
- [x] Import resolution
- [x] Selective imports
- [ ] Glob imports
- [x] Relative imports
- [ ] Visibility modifiers
- [x] Module dependency graph construction
- [x] Cycle detection in imports
- [x] Compile multiple files into single binary

### Standard Library — Core

Core types (`List#[T]`, `String`, `Map#[K,V]`, `Set#[T]`) are currently compiler builtins. Implemented so far: `stdlib/toml.kl` (TOML parser), `stdlib/cli.kl` (CLI argument parser).

- [ ] `Option#[T]` — replace built-in `?T` with stdlib type
- [ ] `Result#[T, E]` — replace built-in with richer API
- [ ] `String` — owned, growable string type
- [ ] `List#[T]` — dynamic array
- [ ] `Map#[K, V]` — hash map
- [ ] `Set#[T]` — hash set
- [ ] `Range` — iteration support

### Standard Library — I/O
- [ ] `File` type with read/write
- [ ] `stdin`, `stdout`, `stderr` handles
- [ ] `Read` and `Write` traits
- [ ] `BufReader` and `BufWriter`
- [ ] Path manipulation
- [ ] Directory operations

### Iterator Protocol
- [ ] `Iterator` trait with `next()` method
- [ ] `IntoIterator` trait for for-loop support
- [ ] Iterator adapters: `map`, `filter`, `take`, `skip`
- [ ] `collect()` to gather into collections
- [ ] Range iterators (`0..10`, `0..=10`)

### Error Handling Improvements
- [ ] Full `?` operator implementation (early return on Err/None)
- [ ] `try` blocks for localized error handling
- [ ] Error conversion with `From` trait
- [ ] `anyhow`-style error boxing
- [ ] Stack traces in debug mode

### Package Manager
- [ ] `klar.toml` manifest format
- [ ] `klar init` — create new project
- [ ] `klar build` — enhance existing command
- [ ] `klar run` — build and run
- [ ] `klar test` — run tests
- [ ] `klar add <package>` — add dependency
- [ ] Dependency resolution
- [ ] Package registry integration

### Tooling
- [x] `klar fmt` — code formatter
- [x] `klar check` — type check without compiling
- [ ] `klar doc` — documentation generator
- [ ] Language Server Protocol (LSP) implementation
- [ ] Syntax highlighting definitions

---

## Phase 4: C Interoperability

See [DESIGN.md](DESIGN.md#c-interoperability) for detailed FFI design.

### Extern Functions
- [ ] `extern fn` declarations for C functions
- [ ] C calling convention support
- [ ] Symbol name specification with `@link_name`
- [ ] Variadic function support (`...`)
- [ ] ABI handling (System V AMD64, Win64, ARM64)

### C-Compatible Types
- [ ] Raw pointer types: `*T`, `*mut T`, `*const T`
- [ ] Void pointer: `*void`, `*mut void`
- [ ] C integer types: `c_int`, `c_long`, `c_size_t`, etc.
- [ ] Null pointer constant: `null`
- [ ] Pointer arithmetic in unsafe blocks
- [ ] Array-to-pointer decay for C interop

### C Structs and Unions
- [ ] `@repr(C)` attribute for C-compatible layout
- [ ] `@packed` attribute for packed structs
- [ ] Union types
- [ ] Opaque types for incomplete C types
- [ ] Bitfields (basic support)

### Library Linking
- [ ] `@link` attribute to specify libraries
- [ ] Static library linking (`.a`, `.lib`)
- [ ] Dynamic library linking (`.so`, `.dylib`, `.dll`)
- [ ] System library paths
- [ ] pkg-config integration
- [ ] Build system support in `klar.toml`

### Binding Generator
- [ ] `klar bindgen` command
- [ ] Parse C headers using libclang
- [ ] Generate `extern fn` declarations
- [ ] Generate `@repr(C)` struct definitions
- [ ] Generate type aliases
- [ ] Handle preprocessor macros (constants)
- [ ] Configurable naming conventions

### Safe Wrappers
- [ ] RAII wrappers for C resources
- [ ] Error handling integration
- [ ] Slice-to-pointer conversions
- [ ] String conversions (Klar string ↔ C string)
- [ ] Callback wrappers
- [ ] Standard wrapper patterns documentation

---

## Phase 5: Bootstrap and Self-Hosting

### Compiler Infrastructure in Klar
- [ ] String interning / symbol table
- [ ] Source location tracking
- [ ] Diagnostic/error reporting system
- [ ] Arena allocator for AST nodes
- [ ] File I/O for source reading
- [x] Command-line argument parsing (`stdlib/cli.kl`)

### Lexer in Klar
- [ ] Token definitions
- [ ] Lexer state machine
- [ ] String/number literal parsing
- [ ] Comment handling
- [ ] Error recovery and diagnostics

### AST and Parser in Klar
- [ ] Complete AST node definitions
- [ ] Recursive descent parser
- [ ] Operator precedence parsing (Pratt parser)
- [ ] Error recovery with synchronization
- [ ] Source location preservation

### Type Checker in Klar
- [ ] Type representation and equality
- [ ] Type inference engine
- [ ] Generic instantiation
- [ ] Trait resolution
- [ ] Borrow checking (if applicable)
- [ ] Semantic error reporting

### Code Generator in Klar
- [ ] LLVM C API bindings (via Phase 4 FFI)
- [ ] IR generation from typed AST
- [ ] Function codegen with proper ABI
- [ ] Struct layout and access
- [ ] Control flow (if, match, loops)
- [ ] Optimization pipeline integration

### Bootstrap Validation
- [ ] Stage 1 compiler (Klar compiler compiled by Zig compiler)
- [ ] Stage 2 compiler (Klar compiler compiled by Stage 1)
- [ ] Stage 3 compiler (Klar compiler compiled by Stage 2)
- [ ] Binary comparison: Stage 2 == Stage 3 (fixed point)
- [ ] Full test suite passes on all stages

### Transition to Self-Hosting
- [ ] CI/CD builds using Klar compiler
- [ ] Distribution packages built with Klar compiler
- [ ] Documentation updated for self-hosted workflow
- [ ] Zig implementation archived
- [ ] Contributing guide for Klar-based development
