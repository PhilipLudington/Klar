# Klar Implementation Roadmap

> Project roadmap for implementing the Klar programming language.

For the language specification and design philosophy, see [DESIGN.md](DESIGN.md).

---

## Table of Contents

1. [Implementation Strategy](#implementation-strategy)
2. [Phase 4: Language Completion](#phase-4-language-completion-plan)
3. [Phase 5: C Interoperability](#phase-5-c-interoperability)
4. [Phase 6: Bootstrap and Self-Hosting](#phase-6-bootstrap-and-self-hosting)

---

## Implementation Strategy

### Phased Approach

```
Phase 1: Tree-walking interpreter (validate design)     ✅ Complete
    ↓
Phase 2: Bytecode VM (practical performance)           ✅ Complete
    ↓
Phase 3: Native compiler (LLVM)                        ✅ Complete
    ↓
Phase 4: Language completion (generics, traits, etc.)  🔄 In Progress
    ↓
Phase 5: C Interoperability (FFI)                      ⏳ Planned
    ↓
Phase 6: Bootstrap and Self-Hosting                    ⏳ Planned
```

### Implementation Language: Zig

**Reasons:**
- Simple, fast, explicit
- Powerful comptime
- Great for systems work
- No hidden allocations
- Fast compilation

### Project Structure

```
klar/
├── build.zig
├── build.zig.zon
├── src/
│   ├── main.zig
│   ├── lexer.zig
│   ├── token.zig
│   ├── ast.zig
│   ├── parser.zig
│   ├── types.zig
│   ├── checker.zig
│   ├── interpreter.zig
│   ├── values.zig
│   ├── builtins.zig
│   └── errors.zig
├── std/
│   └── *.kl
└── test/
    └── *.zig
```

---

## Phase 4: Language Completion Plan

> **Goal:** Complete the Klar language with generics, traits, modules, and standard library.

### Executive Summary

Phase 4 transforms Klar from a working compiler into a complete, usable programming language. This phase implements the remaining language features specified above: **generics**, **traits**, **module system**, and **standard library**. The focus is on making Klar practical for real-world use.

**Current State (Post Phase 3):**
- ✅ Full compilation pipeline (lexer → parser → checker → LLVM → native)
- ✅ Ownership-based memory management (Rc/Arc, automatic drop)
- ✅ Basic types, structs, enums, closures, optionals, results
- ⚠️ Parser supports generics/traits/modules but checker doesn't fully implement them
- ⚠️ No standard library beyond builtins (print, panic, assert)
- ⚠️ Single-file compilation only

**Phase 4 Adds:**
- Generic functions and types with monomorphization
- Trait definitions, implementations, and bounds
- Multi-file compilation with module system
- Standard library (collections, I/O, strings)
- Package manager and tooling

**Design Philosophy Alignment:**
- `comptime` blocks (replacing C preprocessor) - stretch goal for Phase 4
- No macros - design uses `comptime` for metaprogramming instead
- Word operators (`and`, `or`, `not`) - already implemented ✅
- Explicit casts (`.as#[T]`, `.to#[T]`) - already implemented ✅

---

### Phase 4 Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         KLAR PHASE 4 ADDITIONS                          │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│   Source Files (.kl)                                                    │
│        │                                                                │
│        ▼                                                                │
│   ┌─────────────┐                                                       │
│   │   Module    │  ← NEW: Resolve imports, build dependency graph       │
│   │  Resolver   │                                                       │
│   └──────┬──────┘                                                       │
│          │ module tree                                                  │
│          ▼                                                              │
│   ┌─────────────┐                                                       │
│   │   Parser    │  ← existing (already parses generics/traits)          │
│   └──────┬──────┘                                                       │
│          │ AST with generics                                            │
│          ▼                                                              │
│   ┌─────────────┐                                                       │
│   │   Checker   │  ← ENHANCED: Generic instantiation, trait resolution  │
│   └──────┬──────┘                                                       │
│          │ monomorphized AST                                            │
│          ▼                                                              │
│   ┌─────────────┐                                                       │
│   │   Codegen   │  ← existing (works on monomorphized code)             │
│   └──────┬──────┘                                                       │
│          │                                                              │
│          ▼                                                              │
│   Native Binary + std library                                           │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

---

### Milestone 1: Generic Type Checking

**Objective:** Implement full generic type checking with monomorphization.

**Current State:** Parser accepts `fn foo#[T](x: T)` syntax, but checker treats type parameters as unknown types.

**Deliverables:**
- [x] Track type parameters in checker scope
- [x] Implement type parameter substitution
- [x] Implement monomorphization (generate concrete types at call sites)
- [x] Support generic structs: `struct Pair#[A, B] { first: A, second: B }`
- [x] Support generic enums: `enum Option#[T] { Some(T), None }`
- [x] Support generic functions: `fn swap#[T](inout a: T, inout b: T)`
- [x] Implement type inference at call sites
- [x] Cache monomorphized instances to avoid duplication

**Type Inference Example:**
```klar
fn identity#[T](x: T) -> T { x }

let a = identity(42)        // T inferred as i32
let b = identity("hello")   // T inferred as string
let c: f64 = identity(3.14) // T inferred as f64 from context
```

**Monomorphization Strategy:**
```
Source:
  fn swap#[T](inout a: T, inout b: T) { ... }
  swap(inout x, inout y)  // x, y are i32
  swap(inout p, inout q)  // p, q are f64

Generated:
  fn swap_i32(inout a: i32, inout b: i32) { ... }
  fn swap_f64(inout a: f64, inout b: f64) { ... }
```

**Files to Modify:**
```
src/checker.zig          # Add generic type checking, monomorphization
src/types.zig            # Enhance TypeVar, AppliedType handling
src/codegen/emit.zig     # Emit monomorphized functions
```

**Success Criteria:**
- Generic identity function works with multiple types
- Generic Pair struct can hold different types
- No code bloat from unused instantiations

---

### Milestone 2: Trait System

**Objective:** Implement trait definitions, implementations, and bounds.

**Deliverables:**
- [x] Trait definition parsing (already done) and checking
- [x] Trait implementation (`impl Type: Trait { ... }`)
- [x] Trait bounds on generics (`fn sort#[T: Ordered](list: List#[T])`)
- [x] Multiple trait bounds (`T: Ordered + Clone`)
- [ ] Default method implementations
- [x] Associated types (`type Item` in traits)
- [x] `Self` type in trait methods
- [ ] Derive macro basics (`#[derive(Eq, Clone)]`)

**Core Traits to Implement:**
```klar
trait Eq {
    fn eq(self, other: Self) -> bool
}

trait Ordered: Eq {
    fn compare(self, other: Self) -> Ordering
}

trait Clone {
    fn clone(self) -> Self
}

trait Drop {
    fn drop(inout self: Self) -> void
}

trait Default {
    fn default() -> Self
}

trait Hash {
    fn hash(self) -> u64
}
```

**Trait Resolution:**
```klar
trait Printable {
    fn to_string(self) -> String
}

impl i32: Printable {
    fn to_string(self) -> String {
        // integer to string conversion
    }
}

fn print_value#[T: Printable](value: T) {
    println(value.to_string())
}

print_value(42)  // Resolves to i32's Printable impl
```

**Files to Create/Modify:**
```
src/traits.zig           # NEW: Trait registry, resolution
src/checker.zig          # Add trait checking, impl validation
src/types.zig            # Add trait bounds to type system
```

**Success Criteria:**
- Can define and implement traits
- Trait bounds restrict generic parameters
- Method calls resolve to correct implementation

---

### Milestone 3: Module System

**Objective:** Implement multi-file compilation with imports and exports.

**Deliverables:**
- [ ] Module declaration (`module math.vector`)
- [x] Import resolution (`import stdlib.collections.List`)
- [x] Selective imports (`import stdlib.io.{ read, write }`)
- [ ] Glob imports (`import stdlib.collections.*`)
- [x] Relative imports (`import .sibling`, `import ..parent.child`)
- [ ] Visibility modifiers (`pub fn`, `pub struct`)
- [x] Module dependency graph construction
- [x] Cycle detection in imports
- [x] Compile multiple files into single binary

**Module Resolution Rules:**
```
import stdlib.collections.List
  → Look in: stdlib/collections.kl or stdlib/collections/mod.kl
  → Find: pub struct List#[T] { ... }

import .utils
  → Look in: ./utils.kl (relative to current file)

import ..parent.helper
  → Look in: ../parent/helper.kl
```

**Project Structure Example:**
```
myproject/
├── klar.toml           # Package manifest
├── src/
│   ├── main.kl         # module main
│   ├── utils.kl        # module utils
│   └── models/
│       ├── mod.kl      # module models
│       └── user.kl     # module models.user
└── std/                # Standard library (symlinked or copied)
```

**Files to Create:**
```
src/module_resolver.zig  # NEW: Module discovery, import resolution
src/project.zig          # NEW: Project/package handling
```

**Success Criteria:**
- Can compile multi-file projects
- Imports resolve correctly
- Circular imports detected and reported

---

### Milestone 4: Standard Library - Core

**Objective:** Implement core standard library types.

**Current State (builtin types):** `List#[T]`, `String`, `Map#[K,V]`, and `Set#[T]` are compiler builtins with heap-indirected shared-mutation semantics:
- **List** — `{ header_ptr }` (8 bytes) → heap `{ data_ptr, len, capacity }`. Copies share state.
- **String** — `{ header_ptr }` (8 bytes) → heap `{ data_ptr, len, capacity }`. Copies share state.
- **Map** — `{ header_ptr }` (8 bytes) → heap `{ entries_ptr, len, capacity, tombstones }`. Copies share state.
- **Set** — flat `{ entries_ptr, len, capacity, tombstones }` (20 bytes). Not yet heap-indirected.

The deliverables below track the eventual migration from builtins to user-defined stdlib types.

**Stdlib Libraries Implemented:**
- `stdlib/toml.kl` — Full TOML parser (~1500 lines, pure Klar)
- `stdlib/cli.kl` — CLI argument parsing library (builder-pattern API)

**Deliverables:**
- [ ] `Option#[T]` - replaces built-in `?T` with stdlib type
- [ ] `Result#[T, E]` - replaces built-in with richer API
- [ ] `String` - owned, growable string type
- [ ] `List#[T]` - dynamic array (like Vec in Rust)
- [ ] `Map#[K, V]` - hash map
- [ ] `Set#[T]` - hash set
- [ ] `Range` - iteration support

**String Type:**
```klar
// std/string.kl
pub struct String {
    data: Rc#[List#[u8]]
    len: usize
}

impl String {
    pub fn new() -> String
    pub fn from(ref s: string) -> String
    pub fn len(self) -> i32
    pub fn push(inout self, c: u8) -> void
    pub fn concat(self, ref other: String) -> String
    pub fn slice(self, start: i32, end: i32) -> string
    pub fn chars(self) -> Iterator#[u8]
}

impl String: Eq + Clone + Hash + Printable
```

**List Type:**
```klar
// std/collections/list.kl
pub struct List#[T] {
    data: CPtr#[T]
    len: i32
    capacity: i32
}

impl List#[T] {
    pub fn new() -> List#[T]
    pub fn with_capacity(cap: i32) -> List#[T]
    pub fn push(inout self, value: T) -> void
    pub fn pop(inout self) -> ?T
    pub fn get(ref self, index: i32) -> ?T
    pub fn len(ref self) -> i32
    pub fn iter(ref self) -> Iterator#[T]
}

impl List#[T]: Clone where T: Clone
impl List#[T]: Drop  // Drops all elements
```

**Files to Create:**
```
std/
├── core/
│   ├── mod.kl          # Re-exports
│   ├── option.kl       # Option#[T]
│   ├── result.kl       # Result#[T, E]
│   └── ordering.kl     # Ordering enum
├── string.kl           # String type
├── collections/
│   ├── mod.kl
│   ├── list.kl         # List#[T]
│   ├── map.kl          # Map#[K, V]
│   └── set.kl          # Set#[T]
└── prelude.kl          # Auto-imported types
```

**Success Criteria:**
- Can create and manipulate strings
- Can use List for dynamic collections
- Map and Set work with hashable keys

---

### Milestone 5: Standard Library - I/O

**Objective:** Implement file and console I/O.

**Deliverables:**
- [ ] `File` type with read/write
- [ ] `stdin`, `stdout`, `stderr` handles
- [ ] `Read` and `Write` traits
- [ ] `BufReader` and `BufWriter`
- [ ] Path manipulation
- [ ] Directory operations

**I/O Traits:**
```klar
// std/io/traits.kl
pub trait Read {
    fn read(inout self, buf: inout [u8]) -> Result#[i32, IoError]
}

pub trait Write {
    fn write(inout self, ref buf: [u8]) -> Result#[i32, IoError]
    fn flush(inout self) -> Result#[void, IoError]
}
```

**File API:**
```klar
// std/fs.kl
pub struct File { ... }

impl File {
    pub fn open(ref path: string) -> Result#[File, IoError]
    pub fn create(ref path: string) -> Result#[File, IoError]
}

impl File: Read + Write

// Usage
let file = File.open("data.txt")?
let contents = file.read_to_string()?
```

**Files to Create:**
```
std/
├── io/
│   ├── mod.kl
│   ├── traits.kl       # Read, Write
│   ├── stdio.kl        # stdin, stdout, stderr
│   └── buffer.kl       # BufReader, BufWriter
├── fs.kl               # File, directory operations
└── path.kl             # Path type
```

**Success Criteria:**
- Can read and write files
- Buffered I/O works correctly
- Proper error handling with Result

---

### Milestone 6: Iterator Protocol

**Objective:** Implement iterators and for-loop integration.

**Deliverables:**
- [ ] `Iterator` trait with `next()` method
- [ ] `IntoIterator` trait for for-loop support
- [ ] Iterator adapters: `map`, `filter`, `take`, `skip`
- [ ] `collect()` to gather into collections
- [ ] Range iterators (`0..10`, `0..=10`)

**Iterator Trait:**
```klar
pub trait Iterator {
    type Item
    fn next(inout self) -> ?Self.Item

    // Default implementations
    fn map#[B](self, f: fn(Self.Item) -> B) -> Map#[Self, B]
    fn filter(self, pred: fn(ref Self.Item) -> bool) -> Filter#[Self]
    fn collect#[C: FromIterator#[Self.Item]](self) -> C
}

pub trait IntoIterator {
    type Item
    type IntoIter: Iterator#[Item = Self.Item]
    fn into_iter(self) -> Self.IntoIter
}
```

**For Loop Desugaring:**
```klar
// Source
for x in collection {
    process(x)
}

// Desugared to
var iter: Iterator = collection.into_iter()
loop {
    let item: ?T = iter.next()
    match item {
        Some(x) => { process(x) }
        None => { break }
    }
}
```

**Success Criteria:**
- For loops work with any IntoIterator
- Iterator chains are lazy
- Can collect into List, Set, etc.

---

### Milestone 7: Error Handling Improvements

**Objective:** Complete the `?` operator and improve error handling.

**Deliverables:**
- [ ] Full `?` operator implementation (early return on Err/None)
- [ ] `try` blocks for localized error handling
- [ ] Error conversion with `From` trait
- [ ] `anyhow`-style error boxing
- [ ] Stack traces in debug mode

**Error Propagation:**
```klar
fn read_config() -> Result#[Config, Error] {
    let file = File.open("config.toml")?  // Returns Err if fails
    let contents = file.read_to_string()?
    let config = parse_toml(contents)?
    Ok(config)
}
```

**Error Conversion:**
```klar
trait From#[T] {
    fn from(value: T) -> Self
}

// Automatic conversion via ?
impl Error: From#[IoError] { ... }
impl Error: From#[ParseError] { ... }

fn example() -> Result#[void, Error] {
    let file = File.open("x")?  // IoError → Error via From
    Ok(())
}
```

**Success Criteria:**
- `?` operator properly propagates errors
- Error types can be converted automatically
- Clear error messages with context

---

### Milestone 8: Package Manager

**Objective:** Implement basic package management.

**Deliverables:**
- [ ] `klar.toml` manifest format
- [ ] `klar init` - create new project
- [ ] `klar build` - build project (already exists, enhance)
- [ ] `klar run` - build and run
- [ ] `klar test` - run tests
- [ ] `klar add <package>` - add dependency
- [ ] Dependency resolution
- [ ] Package registry integration (optional)

**Manifest Format:**
```toml
# klar.toml
[package]
name = "myapp"
version = "0.1.0"
authors = ["Alice <alice@example.com>"]

[dependencies]
json = "1.0"
http = { git = "https://github.com/klar/http" }

[dev-dependencies]
test-utils = "0.1"
```

**CLI Commands:**
```bash
klar init myproject          # Create new project
klar build                   # Build current project
klar build --release         # Build with optimizations
klar run                     # Build and run
klar test                    # Run tests
klar add json                # Add dependency
klar update                  # Update dependencies
```

**Files to Create:**
```
src/package/
├── mod.zig                  # Package management entry
├── manifest.zig             # klar.toml parsing
├── resolver.zig             # Dependency resolution
└── registry.zig             # Package registry client
```

**Success Criteria:**
- Can create and build projects
- Dependencies resolved correctly
- Reproducible builds with lockfile

---

### Milestone 9: Tooling

**Objective:** Developer tooling for productive Klar development.

**Deliverables:**
- [x] `klar fmt` - code formatter
- [x] `klar check` - type check without compiling
- [ ] `klar doc` - documentation generator
- [ ] Language Server Protocol (LSP) implementation
- [ ] Syntax highlighting definitions (VS Code, etc.)

**Language Server Features:**
- Go to definition
- Find references
- Hover for type info
- Autocomplete
- Diagnostics (errors/warnings)
- Code actions (quick fixes)

**Files to Create:**
```
src/lsp/
├── mod.zig                  # LSP entry point
├── server.zig               # JSON-RPC server
├── handlers.zig             # Request handlers
└── protocol.zig             # LSP protocol types

tools/
├── klar-fmt/                # Formatter
└── vscode-klar/             # VS Code extension
```

**Success Criteria:**
- Formatter produces consistent output
- LSP provides go-to-definition and autocomplete
- VS Code extension available

---

### Phase 4 Dependency Graph

```
Milestone 1: Generics
    │
    ├──► Milestone 2: Traits (needs generics for bounds)
    │         │
    │         ├──► Milestone 6: Iterators (needs Iterator trait)
    │         │
    │         └──► Milestone 7: Error Handling (needs From trait)
    │
    ├──► Milestone 3: Modules (parallel, foundation)
    │         │
    │         └──► Milestone 4: Stdlib Core (needs modules)
    │                   │
    │                   └──► Milestone 5: Stdlib I/O (needs core)
    │
    └──► Milestone 8: Package Manager (needs modules)
              │
              └──► Milestone 9: Tooling (needs stable language)
```

**Recommended Order:**
1. Generics (Milestone 1)
2. Modules (Milestone 3) - can start in parallel
3. Traits (Milestone 2)
4. Stdlib Core (Milestone 4)
5. Iterators (Milestone 6)
6. Error Handling (Milestone 7)
7. Stdlib I/O (Milestone 5)
8. Package Manager (Milestone 8)
9. Tooling (Milestone 9)

---

### Phase 4 Timeline Estimate

| Milestone | Effort | Dependencies |
|-----------|--------|--------------|
| 1. Generics | Large | None |
| 2. Traits | Large | Generics |
| 3. Modules | Medium | None |
| 4. Stdlib Core | Medium | Generics, Traits, Modules |
| 5. Stdlib I/O | Medium | Stdlib Core |
| 6. Iterators | Medium | Traits |
| 7. Error Handling | Small | Traits |
| 8. Package Manager | Medium | Modules |
| 9. Tooling | Large | All above |

---

### Phase 4 Success Metrics

**Phase 4 is complete when:**

1. **Language Completeness**
   - [x] Generic functions and types work
   - [x] Traits can be defined and implemented
   - [x] Multi-file projects compile
   - [ ] Standard library provides core functionality

2. **Usability**
   - [x] Can write non-trivial programs (TOML parser, CLI library)
   - [x] Error messages are helpful
   - [x] Documentation exists

3. **Tooling**
   - [ ] Package manager works
   - [ ] IDE support via LSP
   - [x] Code formatter available

4. **Example Programs**
   - [ ] JSON parser using generics
   - [ ] HTTP client using async (stretch)
   - [ ] File processing utility

---

### Stretch Goals

These are valuable but not required for Phase 4 completion:

1. **Async/Await** - Designed above but complex to implement
2. **Comptime** - Compile-time evaluation (replaces C preprocessor per design philosophy)
3. **Self-Hosting** - Compiler written in Klar
4. ~~**REPL** - Interactive Klar shell~~ ✅ Complete
5. **WebAssembly Target** - Compile to WASM

> **Note:** The design philosophy specifically calls for `comptime` blocks instead of macros.
> `macro` is a reserved keyword for potential future use, but `comptime` is the intended
> mechanism for metaprogramming and compile-time code generation.

---

## Phase 5: C Interoperability

> **Goal:** Enable Klar programs to call C libraries like SDL3, OpenGL, SQLite, and system APIs.

### Executive Summary

Phase 5 adds Foreign Function Interface (FFI) capabilities to Klar, allowing seamless interoperability with C libraries. This unlocks access to the vast ecosystem of existing C code including graphics (SDL3, OpenGL, Vulkan), databases (SQLite, PostgreSQL), compression (zlib), cryptography (OpenSSL), and operating system APIs.

**Why C Interop?**
- Graphics/multimedia libraries are written in C (SDL3, OpenGL, Vulkan)
- System APIs are C-based (POSIX, Win32)
- Performance-critical libraries exist in C
- Gradual migration path from C codebases

**Design Philosophy:**
- Explicit is better than implicit - C calls are clearly marked
- Safety at the boundary - validate C data entering Klar
- Zero-overhead when possible - direct calls, no wrappers for simple cases
- Ergonomic bindings - high-level Klar APIs wrap low-level C

---

### Phase 5 Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                      KLAR PHASE 5: C INTEROP                            │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│   Klar Source (.kl)              C Headers (.h)                         │
│        │                              │                                 │
│        ▼                              ▼                                 │
│   ┌─────────────┐              ┌─────────────┐                          │
│   │   Parser    │              │  Bindgen    │  ← NEW: Generate Klar    │
│   │             │              │   Tool      │    bindings from headers │
│   └──────┬──────┘              └──────┬──────┘                          │
│          │                            │                                 │
│          │ AST with extern fns        │ Generated .kl bindings          │
│          │                            │                                 │
│          └──────────┬─────────────────┘                                 │
│                     ▼                                                   │
│              ┌─────────────┐                                            │
│              │   Checker   │  ← ENHANCED: Validate C type mappings      │
│              └──────┬──────┘                                            │
│                     │                                                   │
│                     ▼                                                   │
│              ┌─────────────┐                                            │
│              │   Codegen   │  ← ENHANCED: C calling conventions,        │
│              └──────┬──────┘    symbol linkage, ABI compatibility       │
│                     │                                                   │
│                     ▼                                                   │
│              ┌─────────────┐                                            │
│              │   Linker    │  ← ENHANCED: Link C libraries              │
│              └──────┬──────┘    (.a, .so, .dylib, .dll)                 │
│                     │                                                   │
│                     ▼                                                   │
│              Native Binary + C libraries                                │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

---

### Milestone 1: Extern Functions

**Objective:** Allow Klar to declare and call C functions.

**Deliverables:**
- [ ] `extern fn` declarations for C functions
- [ ] C calling convention support
- [ ] Symbol name specification with `@link_name`
- [ ] Variadic function support (`...`)
- [ ] Proper ABI handling (System V AMD64, Win64, ARM64)

**Extern Function Syntax:**
```klar
// Declare C functions
extern fn printf(format: *const u8, ...) -> i32
extern fn malloc(size: usize) -> *mut void
extern fn free(ptr: *mut void)

// With explicit link name (for name mangling)
@link_name("SDL_Init")
extern fn sdl_init(flags: u32) -> i32

// In a block for organization
extern "C" {
    fn puts(s: *const u8) -> i32
    fn getenv(name: *const u8) -> *const u8
    fn exit(status: i32) -> !
}
```

**Success Criteria:**
- Can call `printf` from Klar
- Can call SDL_Init and other SDL functions
- Proper handling of different calling conventions

---

### Milestone 2: C-Compatible Types

**Objective:** Define Klar types that map directly to C types.

**Deliverables:**
- [ ] Raw pointer types: `*T`, `*mut T`, `*const T`
- [ ] Void pointer: `*void`, `*mut void`
- [ ] C integer types: `c_int`, `c_long`, `c_size_t`, etc.
- [ ] Null pointer constant: `null`
- [ ] Pointer arithmetic in unsafe blocks
- [ ] Array-to-pointer decay for C interop

**Pointer Types:**
```klar
// Immutable pointer (like const T* in C)
let ptr: *const i32 = &value

// Mutable pointer (like T* in C)
let ptr: *mut i32 = &mut value

// Void pointer (like void* in C)
let ptr: *void = raw_ptr
let ptr: *mut void = raw_mut_ptr

// Nullable pointer (optional pointer)
let maybe_ptr: ?*i32 = get_optional_ptr()

// Pointer to many (like T* array in C)
let arr_ptr: [*]i32 = get_array_ptr()
let arr_ptr: [*:0]u8 = get_null_terminated_string()  // Sentinel-terminated
```

**Success Criteria:**
- Can represent any C pointer type
- Pointer arithmetic works correctly
- Platform-specific C types have correct sizes

---

### Milestone 3: C Structs and Unions

**Objective:** Define structs and unions with C-compatible memory layout.

**Deliverables:**
- [ ] `@repr(C)` attribute for C-compatible layout
- [ ] `@packed` attribute for packed structs
- [ ] Union types
- [ ] Opaque types for incomplete C types
- [ ] Bitfields (basic support)

**C-Compatible Structs:**
```klar
// C-compatible layout (fields in declaration order, C alignment)
@repr(C)
struct SDL_Rect {
    x: c_int
    y: c_int
    w: c_int
    h: c_int
}

// Packed struct (no padding)
@repr(C)
@packed
struct PackedData {
    flag: u8
    value: u32  // No padding before this
}

// Opaque types
@opaque
struct SDL_Window

@opaque
struct SDL_Renderer
```

**Success Criteria:**
- SDL_Rect has same memory layout as C version
- Can pass structs to C functions
- Opaque types prevent invalid access

---

### Milestone 4: Library Linking

**Objective:** Link Klar programs with C libraries.

**Deliverables:**
- [ ] `@link` attribute to specify libraries
- [ ] Static library linking (`.a`, `.lib`)
- [ ] Dynamic library linking (`.so`, `.dylib`, `.dll`)
- [ ] System library paths
- [ ] pkg-config integration
- [ ] Build system support in `klar.toml`

**Link Attribute:**
```klar
// Link with SDL3
@link("SDL3")
extern fn SDL_Init(flags: u32) -> c_int

@link("SDL3")
extern fn SDL_Quit()

// Multiple libraries
@link("ssl")
@link("crypto")
extern fn SSL_library_init() -> c_int

// Framework on macOS
@link(framework: "Cocoa")
extern fn NSApplicationMain(argc: c_int, argv: *const *const u8) -> c_int
```

**Success Criteria:**
- Can link with SDL3 and create a window
- pkg-config integration works
- Cross-platform library resolution

---

### Milestone 5: Binding Generator

**Objective:** Automatically generate Klar bindings from C headers.

**Deliverables:**
- [ ] `klar bindgen` command
- [ ] Parse C headers using libclang
- [ ] Generate `extern fn` declarations
- [ ] Generate `@repr(C)` struct definitions
- [ ] Generate type aliases
- [ ] Handle preprocessor macros (constants)
- [ ] Configurable naming conventions

**Bindgen Usage:**
```bash
# Generate bindings for SDL3
klar bindgen SDL3/SDL.h -o src/sdl3.kl

# With configuration
klar bindgen SDL3/SDL.h \
    --config bindgen.toml \
    --include /usr/include/SDL3 \
    -o src/sdl3.kl
```

**Success Criteria:**
- Can generate SDL3 bindings automatically
- Generated code compiles and works
- Handles complex headers (nested structs, function pointers)

---

### Milestone 6: Safe Wrappers

**Objective:** Provide idiomatic Klar wrappers around C APIs.

**Deliverables:**
- [ ] RAII wrappers for C resources
- [ ] Error handling integration
- [ ] Slice-to-pointer conversions
- [ ] String conversions (Klar string ↔ C string)
- [ ] Callback wrappers
- [ ] Standard wrapper patterns documentation

**Resource Management:**
```klar
// Safe wrapper around SDL_Window
pub struct Window {
    ptr: *sdl.Window
}

impl Window {
    pub fn create(ref title: string, width: i32, height: i32) -> Result#[Window, SdlError] {
        let c_title = CString.from(title)
        let ptr = unsafe {
            sdl.CreateWindow(
                c_title.as_ptr(),
                sdl.WINDOWPOS_CENTERED,
                sdl.WINDOWPOS_CENTERED,
                width, height,
                sdl.WINDOW_SHOWN
            )
        }
        match ptr {
            Some(p) => Ok(Window { ptr: p })
            None => Err(SdlError.from_last())
        }
    }
}

impl Window: Drop {
    fn drop(inout self: Window) -> void {
        unsafe { sdl.DestroyWindow(self.ptr) }
    }
}

// Usage - automatically cleaned up
fn main() -> Result#[void, Error] {
    let window = Window.create("My Game", 800, 600)?
    // window.drop() called automatically at end of scope
}
```

**Success Criteria:**
- SDL3 wrapper provides safe, idiomatic API
- Resources are automatically cleaned up
- No memory leaks or dangling pointers in safe code

---

### Phase 5 Success Metrics

**Phase 5 is complete when:**

1. **C Interop Works**
   - [ ] Can call C functions with correct ABI
   - [ ] Pointers work correctly across boundary
   - [ ] Structs have C-compatible layout
   - [ ] Libraries link correctly on all platforms

2. **SDL3 Integration**
   - [ ] Can create SDL3 window from Klar
   - [ ] Can handle SDL3 events
   - [ ] Can render graphics with SDL3
   - [ ] Safe wrappers prevent common errors

3. **Developer Experience**
   - [ ] Bindgen generates usable bindings
   - [ ] Error messages help debug FFI issues
   - [ ] Documentation covers common patterns

4. **Example Programs**
   - [ ] SDL3 hello world (window + events)
   - [ ] SDL3 game loop with rendering
   - [ ] SQLite database access
   - [ ] System API usage (file dialogs, etc.)

---

## Phase 6: Bootstrap and Self-Hosting

> **Goal:** Rewrite the Klar compiler in Klar itself, achieving self-hosting.

### Executive Summary

Phase 6 marks the maturation of Klar from a language implemented in Zig to a fully self-hosting language. The Klar compiler will be rewritten in Klar, proving the language is capable of implementing complex systems software. This is both a practical milestone (the compiler becomes a first-class Klar project) and a symbolic one (the language can sustain itself).

**Why Self-Host?**
- Proves the language is production-ready for systems programming
- Dogfooding surfaces language design issues
- Compiler becomes the largest Klar codebase, driving improvements
- Reduces dependency on Zig toolchain for development
- Attracts contributors who prefer writing Klar over Zig

**Prerequisites:**
- Phase 4 complete (generics, traits, modules, std library)
- Phase 5 complete (C interop for LLVM/Cranelift, system APIs)

**Design Philosophy:**
- Incremental migration - component by component, not big-bang rewrite
- Zig version remains authoritative until Klar version passes all tests
- Maintain feature parity - no regressions during transition
- Use bootstrap as stress test for language ergonomics

---

### Phase 6 Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    KLAR PHASE 6: BOOTSTRAP STRATEGY                     │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│   Stage 0: Zig Compiler (current)                                       │
│   ┌─────────────────────────────────────────────────────────────────┐   │
│   │  Lexer → Parser → Checker → VM/Codegen  (all Zig)               │   │
│   └─────────────────────────────────────────────────────────────────┘   │
│                              │                                          │
│                              ▼                                          │
│   Stage 1: Partial Bootstrap                                            │
│   ┌─────────────────────────────────────────────────────────────────┐   │
│   │  Lexer.kl → Parser.kl → Checker.kl → Codegen (Zig+LLVM)         │   │
│   │       ▲                                    │                    │   │
│   │       │         Compiled by Stage 0        │                    │   │
│   │       └────────────────────────────────────┘                    │   │
│   └─────────────────────────────────────────────────────────────────┘   │
│                              │                                          │
│                              ▼                                          │
│   Stage 2: Full Bootstrap                                               │
│   ┌─────────────────────────────────────────────────────────────────┐   │
│   │  Lexer.kl → Parser.kl → Checker.kl → Codegen.kl (LLVM via FFI)  │   │
│   │       ▲                                    │                    │   │
│   │       │         Compiled by Stage 1        │                    │   │
│   │       └────────────────────────────────────┘                    │   │
│   └─────────────────────────────────────────────────────────────────┘   │
│                              │                                          │
│                              ▼                                          │
│   Stage 3: Self-Sustaining                                              │
│   ┌─────────────────────────────────────────────────────────────────┐   │
│   │  klarc (Klar) compiles itself                                   │   │
│   │  Zig compiler archived as historical reference                  │   │
│   └─────────────────────────────────────────────────────────────────┘   │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

---

### Milestone 1: Compiler Infrastructure in Klar

**Objective:** Port foundational data structures and utilities to Klar.

**Deliverables:**
- [ ] String interning / symbol table
- [ ] Source location tracking
- [ ] Diagnostic/error reporting system
- [ ] Arena allocator for AST nodes
- [ ] File I/O for source reading
- [x] Command-line argument parsing (`stdlib/cli.kl`)

---

### Milestone 2: Lexer in Klar

**Objective:** Rewrite the lexer in Klar.

**Deliverables:**
- [ ] Token definitions
- [ ] Lexer state machine
- [ ] String/number literal parsing
- [ ] Comment handling
- [ ] Error recovery and diagnostics

**Validation:**
- Lex entire Klar standard library
- Compare token output with Zig lexer (should be identical)
- Benchmark: within 2x of Zig lexer performance

---

### Milestone 3: AST and Parser in Klar

**Objective:** Rewrite the parser and AST definitions in Klar.

**Deliverables:**
- [ ] Complete AST node definitions
- [ ] Recursive descent parser
- [ ] Operator precedence parsing (Pratt parser)
- [ ] Error recovery with synchronization
- [ ] Source location preservation

**Validation:**
- Parse entire Klar standard library
- Round-trip test: parse → pretty-print → parse → compare AST
- Error messages match Zig parser quality

---

### Milestone 4: Type Checker in Klar

**Objective:** Rewrite the type checker and semantic analysis in Klar.

**Deliverables:**
- [ ] Type representation and equality
- [ ] Type inference engine
- [ ] Generic instantiation
- [ ] Trait resolution
- [ ] Borrow checking (if applicable)
- [ ] Semantic error reporting

**Validation:**
- Type-check entire Klar standard library
- All existing test cases pass
- Error messages are clear and actionable

---

### Milestone 5: Code Generator in Klar

**Objective:** Rewrite native code generation in Klar using LLVM via FFI.

**Deliverables:**
- [ ] LLVM C API bindings (via Phase 5 FFI)
- [ ] IR generation from typed AST
- [ ] Function codegen with proper ABI
- [ ] Struct layout and access
- [ ] Control flow (if, match, loops)
- [ ] Optimization pipeline integration

**Validation:**
- Compile and run all native test programs
- Generated code matches Zig compiler output quality
- Benchmark within 10% of Zig compiler performance

---

### Milestone 6: Bootstrap Validation

**Objective:** Prove the Klar compiler can compile itself.

**Deliverables:**
- [ ] Stage 1 compiler (Klar compiler compiled by Zig compiler)
- [ ] Stage 2 compiler (Klar compiler compiled by Stage 1)
- [ ] Stage 3 compiler (Klar compiler compiled by Stage 2)
- [ ] Binary comparison: Stage 2 == Stage 3 (fixed point)
- [ ] Full test suite passes on all stages

**Bootstrap Process:**
```
# Build Stage 1: Zig compiles Klar compiler
zig build -Doptimize=ReleaseFast
./zig-out/bin/klarc src/klar/main.kl -o stage1

# Build Stage 2: Stage 1 compiles Klar compiler
./stage1 src/klar/main.kl -o stage2

# Build Stage 3: Stage 2 compiles Klar compiler
./stage2 src/klar/main.kl -o stage3

# Verify fixed point (Stage 2 and Stage 3 should be identical)
diff stage2 stage3 && echo "Bootstrap successful!"

# Run full test suite with Stage 2
./stage2 test
```

**Validation Criteria:**
- All three stages produce working compilers
- Stage 2 and Stage 3 binaries are byte-identical
- Full test suite passes on Stage 2 compiler
- Compilation time reasonable (< 2x Zig compiler)

---

### Milestone 7: Transition to Self-Hosting

**Objective:** Make the Klar compiler the primary/official implementation.

**Deliverables:**
- [ ] CI/CD builds using Klar compiler
- [ ] Distribution packages built with Klar compiler
- [ ] Documentation updated for self-hosted workflow
- [ ] Zig implementation archived
- [ ] Contributing guide for Klar-based development

**Repository Structure (Post-Bootstrap):**
```
klar/
├── src/                    # Klar compiler source (in Klar)
│   ├── main.kl
│   ├── lexer.kl
│   ├── parser.kl
│   ├── checker.kl
│   └── codegen/
├── std/                    # Standard library (in Klar)
├── bootstrap/              # Prebuilt binaries for bootstrapping
│   ├── klarc-linux-x64
│   ├── klarc-macos-x64
│   ├── klarc-macos-arm64
│   └── klarc-windows-x64.exe
├── archive/                # Historical Zig implementation
│   └── zig-compiler/
└── test/
```

---

### Phase 6 Success Metrics

**Phase 6 is complete when:**

1. **Bootstrap Achieved**
   - [ ] Klar compiler compiles itself
   - [ ] Stage 2 == Stage 3 (fixed point reached)
   - [ ] All tests pass on self-compiled compiler

2. **Production Ready**
   - [ ] CI/CD uses self-hosted compiler
   - [ ] Release binaries built with Klar compiler
   - [ ] No regressions from Zig implementation

3. **Performance Acceptable**
   - [ ] Compilation speed within 2x of Zig implementation
   - [ ] Memory usage reasonable for large projects
   - [ ] Generated code quality unchanged

4. **Developer Experience**
   - [ ] Clear bootstrap instructions in README
   - [ ] Prebuilt binaries for major platforms
   - [ ] Contributing guide updated for Klar development

---

### Phase 6 Stretch Goals

1. **Incremental Compilation** - Only recompile changed modules
2. **Parallel Compilation** - Multi-threaded frontend and codegen
3. **Self-Hosted Standard Library** - Rewrite std in pure Klar where possible
4. **Debug Info** - DWARF/CodeView debug information generation
5. **Cross-Compilation** - Compile for other targets from Klar compiler
6. **Language Server** - LSP implementation in Klar

---

### Bootstrapping FAQ

**Q: Why not bootstrap earlier (before Phase 5)?**

A: The compiler needs C interop to use LLVM for native code generation. Without Phase 5, we'd either need to write a native backend from scratch or keep parts in Zig permanently.

**Q: What if we find language design issues during bootstrap?**

A: This is expected and valuable! Issues found will be fixed in both compilers (Zig and Klar) until the Klar version is authoritative.

**Q: How do new users build without a Klar compiler?**

A: Prebuilt bootstrap binaries are provided for major platforms. These are updated with each release.

**Q: Can we still use the Zig compiler after bootstrap?**

A: Yes, it's archived and can be used for debugging or if issues arise. However, the Klar compiler becomes the official implementation.

**Q: What about compile times?**

A: Initial Klar compiler may be slower than Zig version. Optimization is a stretch goal. The priority is correctness first.

---

*Document version: 2.3*
*Last updated: March 2026*
