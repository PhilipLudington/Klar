# Klar Phase 4: Language Completion

> **Goal:** Complete the Klar language with generics, traits, modules, and standard library.

## Current State

**Completed (Phases 1-3):**
- Full compilation pipeline (lexer → parser → checker → LLVM → native)
- Ownership-based memory management (Rc/Arc, automatic drop)
- Basic types, structs, enums, closures, optionals, results
- Parser supports generics/traits/modules syntax
- Three execution backends: interpreter, bytecode VM, native compilation
- 252x speedup for native vs VM

**Gaps to Address:**
- Generic functions, structs, and enums working (type checking + monomorphization); enum literal codegen pending
- Traits parsed but not semantically checked
- Single-file compilation only
- No standard library beyond builtins (print, panic, assert)

---

## Milestone 1: Generic Type Checking

**Objective:** Implement full generic type checking with monomorphization.

**Status:** Generic functions, structs, and enums complete (type checking, monomorphization, codegen registration, enum literal construction, match pattern parsing/checking). Generic struct methods pending; match expression codegen not yet implemented.

### Type Parameter Tracking
- [x] Add TypeParam scope tracking in checker.zig
- [x] Store type parameters in function/struct context
- [x] Implement type parameter lookup during type resolution
- [x] Handle nested generic scopes correctly

### Type Parameter Substitution
- [x] Implement substituteTypeParams() for replacing type vars with concrete types
- [x] Handle recursive type substitution in nested generics
- [ ] Create AppliedType representation for instantiated generic types
- [x] Support type parameter substitution in function signatures

### Monomorphization
- [x] Detect generic function call sites
- [x] Infer concrete types from arguments at call sites
- [x] Record monomorphized function instances (e.g., identity$i32)
- [x] Cache monomorphized instances to avoid duplication
- [x] Mangle names for uniqueness (e.g., `swap$i32` or `swap_i32`)
- [x] Generate monomorphized functions in codegen (emit.zig)

### Generic Structs
- [x] Support generic struct definitions: `struct Pair[A, B] { first: A, second: B }`
- [x] Instantiate generic structs at usage sites
- [x] Generate monomorphized struct types for codegen
- [ ] Handle generic struct methods

### Generic Enums
- [x] Support generic enum definitions: `enum Option[T] { Some(T), None }`
- [x] Instantiate generic enums at usage sites (type checker + monomorphization)
- [x] Generate monomorphized enum types for codegen
- [x] Parse and type-check generic enum variants in match patterns (Type[T]::Variant syntax)
- [x] Emit generic enum literal construction in codegen (EnumType[T]::Variant(payload) syntax)
- [ ] Emit match expression codegen for all patterns (blocking full match support)

### Type Inference at Call Sites
- [x] Infer type parameters from argument types
- [ ] Infer type parameters from return type context
- [ ] Support partial type inference with explicit annotations
- [ ] Report errors for ambiguous type inference

### Testing
- [x] Test: generic identity function works with multiple types
- [x] Test: generic Pair struct can hold different types
- [ ] Test: no code bloat from unused instantiations
- [x] Test: type inference selects correct instantiation

**Files Modified:**
- `src/checker.zig` - Generic type checking, monomorphization recording (functions + structs + enums) ✓
- `src/types.zig` - TypeVar handling ✓
- `src/codegen/emit.zig` - Monomorphized function/struct/enum emission ✓
- `src/parser.zig` - Generic struct literal parsing ✓
- `src/main.zig` - Integration of type checker with emitter ✓
- `test/native/generics_basic.kl` - Generic function test ✓
- `test/native/generic_struct.kl` - Generic struct test ✓

---

## Milestone 2: Trait System

**Objective:** Implement trait definitions, implementations, and bounds.

### Trait Registry
- [ ] Create traits.zig module for trait management
- [ ] Implement TraitRegistry to store all trait definitions
- [ ] Store trait method signatures and default implementations
- [ ] Handle trait inheritance (trait A: B syntax)

### Trait Definition Checking
- [ ] Validate trait definitions (unique method names, valid signatures)
- [ ] Check for conflicts in trait inheritance
- [ ] Store Self type placeholder for trait methods
- [ ] Handle associated types in traits

### Trait Implementation
- [ ] Parse and check `impl Type: Trait { ... }` blocks
- [ ] Verify all required methods are implemented
- [ ] Check method signatures match trait definition
- [ ] Allow default method overrides
- [ ] Handle impl blocks for generic types

### Trait Bounds
- [ ] Implement trait bounds on generics: `fn sort[T: Ordered](list: List[T])`
- [ ] Check that type arguments satisfy trait bounds
- [ ] Support multiple trait bounds: `T: Ordered + Clone`
- [ ] Handle where clauses for complex bounds

### Associated Types
- [ ] Support `type Item` declarations in traits
- [ ] Require associated type definitions in impl blocks
- [ ] Resolve `Self.Item` references in trait methods
- [ ] Handle associated types in generic contexts

### Method Resolution
- [ ] Resolve trait method calls on concrete types
- [ ] Look up correct implementation for method dispatch
- [ ] Handle method calls through trait bounds in generics
- [ ] Support UFCS (Uniform Function Call Syntax)

### Core Traits
- [ ] Implement `Eq` trait (equality comparison)
- [ ] Implement `Ordered` trait (comparison, ordering)
- [ ] Implement `Clone` trait (explicit cloning)
- [ ] Implement `Drop` trait (custom destructors)
- [ ] Implement `Default` trait (default values)
- [ ] Implement `Hash` trait (hashing for maps/sets)

### Derive Macro Basics
- [ ] Parse `#[derive(Eq, Clone)]` attributes
- [ ] Generate Eq implementation for structs
- [ ] Generate Clone implementation for structs
- [ ] Generate Hash implementation for structs

### Testing
- [ ] Test: can define and implement traits
- [ ] Test: trait bounds restrict generic parameters
- [ ] Test: method calls resolve to correct implementation
- [ ] Test: associated types work correctly

**Files to Create/Modify:**
- `src/traits.zig` - NEW: Trait registry, resolution
- `src/checker.zig` - Add trait checking, impl validation
- `src/types.zig` - Add trait bounds to type system

---

## Milestone 3: Module System

**Objective:** Implement multi-file compilation with imports and exports.

### Module Resolver
- [ ] Create module_resolver.zig for module discovery
- [ ] Implement file-to-module name mapping
- [ ] Build module dependency graph
- [ ] Detect and report circular imports

### Module Declaration
- [ ] Parse `module math.vector` declarations
- [ ] Validate module name matches file path
- [ ] Support implicit module names from file location
- [ ] Handle mod.kl for directory modules

### Import Resolution
- [ ] Parse `import std.collections.List` statements
- [ ] Resolve import paths to source files
- [ ] Search in: current dir, std library, dependencies
- [ ] Handle selective imports: `import std.io.{ read, write }`

### Import Variants
- [ ] Implement glob imports: `import std.collections.*`
- [ ] Implement aliased imports: `import std.collections.HashMap as Map`
- [ ] Implement relative imports: `import .sibling`, `import ..parent.child`
- [ ] Handle re-exports from modules

### Visibility System
- [ ] Implement `pub fn`, `pub struct` visibility
- [ ] Check visibility at import resolution
- [ ] Support `pub(package)` for internal APIs
- [ ] Report errors for private item access

### Multi-File Compilation
- [ ] Compile modules in dependency order
- [ ] Link multiple object files into single binary
- [ ] Share type information across modules
- [ ] Handle cross-module references

### Project Structure
- [ ] Support standard project layout (src/, tests/, std/)
- [ ] Locate standard library from compiler installation
- [ ] Handle package manifest (klar.toml)

### Testing
- [ ] Test: can compile multi-file projects
- [ ] Test: imports resolve correctly
- [ ] Test: circular imports detected and reported
- [ ] Test: visibility enforced correctly

**Files to Create:**
- `src/module_resolver.zig` - NEW: Module discovery, import resolution
- `src/project.zig` - NEW: Project/package handling

---

## Milestone 4: Standard Library - Core

**Objective:** Implement core standard library types.

### Option Type
- [ ] Implement `Option[T]` enum in std/core/option.kl
- [ ] Add `Some(T)` and `None` variants
- [ ] Implement `unwrap()`, `unwrap_or()`, `map()`, `and_then()`
- [ ] Implement `Eq`, `Clone` for Option

### Result Type
- [ ] Implement `Result[T, E]` enum in std/core/result.kl
- [ ] Add `Ok(T)` and `Err(E)` variants
- [ ] Implement `unwrap()`, `unwrap_err()`, `map()`, `map_err()`
- [ ] Implement `and_then()` for chaining
- [ ] Implement `Eq`, `Clone` for Result

### String Type
- [ ] Implement `String` struct in std/string.kl
- [ ] Use Rc for memory management
- [ ] Implement `new()`, `from()`, `len()`
- [ ] Implement `push()`, `concat()`, `slice()`
- [ ] Implement `chars()` iterator
- [ ] Implement `Eq`, `Clone`, `Hash` for String

### List Type
- [ ] Implement `List[T]` struct in std/collections/list.kl
- [ ] Manage heap allocation with capacity/length
- [ ] Implement `new()`, `with_capacity()`, `push()`, `pop()`
- [ ] Implement `get()`, `set()`, `len()`, `is_empty()`
- [ ] Implement `iter()` for iteration
- [ ] Implement `Clone` where T: Clone
- [ ] Implement `Drop` to free memory

### Map Type
- [ ] Implement `Map[K, V]` struct in std/collections/map.kl
- [ ] Use hash-based implementation
- [ ] Implement `new()`, `insert()`, `get()`, `remove()`
- [ ] Implement `contains_key()`, `keys()`, `values()`
- [ ] Require K: Hash + Eq bound
- [ ] Implement `Clone` where K: Clone, V: Clone

### Set Type
- [ ] Implement `Set[T]` struct in std/collections/set.kl
- [ ] Build on Map implementation
- [ ] Implement `new()`, `insert()`, `contains()`, `remove()`
- [ ] Implement `union()`, `intersection()`, `difference()`
- [ ] Require T: Hash + Eq bound

### Ordering Type
- [ ] Implement `Ordering` enum in std/core/ordering.kl
- [ ] Add `Less`, `Equal`, `Greater` variants
- [ ] Use in Ordered trait implementations

### Prelude
- [ ] Create std/prelude.kl with auto-imported types
- [ ] Include Option, Result, String, List
- [ ] Include core traits (Eq, Clone, etc.)
- [ ] Auto-import prelude in all modules

### Testing
- [ ] Test: can create and manipulate strings
- [ ] Test: List works for dynamic collections
- [ ] Test: Map and Set work with hashable keys
- [ ] Test: Option and Result methods work correctly

**Files to Create:**
```
std/
├── core/
│   ├── mod.kl          # Re-exports
│   ├── option.kl       # Option[T]
│   ├── result.kl       # Result[T, E]
│   └── ordering.kl     # Ordering enum
├── string.kl           # String type
├── collections/
│   ├── mod.kl
│   ├── list.kl         # List[T]
│   ├── map.kl          # Map[K, V]
│   └── set.kl          # Set[T]
└── prelude.kl          # Auto-imported types
```

---

## Milestone 5: Standard Library - I/O

**Objective:** Implement file and console I/O.

### Read Trait
- [ ] Define `Read` trait in std/io/traits.kl
- [ ] Add `read(buf: &mut [u8]) -> Result[usize, IoError]`
- [ ] Add default `read_all()`, `read_to_string()`

### Write Trait
- [ ] Define `Write` trait in std/io/traits.kl
- [ ] Add `write(buf: &[u8]) -> Result[usize, IoError]`
- [ ] Add `flush() -> Result[void, IoError]`
- [ ] Add default `write_all()`

### IoError Type
- [ ] Define `IoError` enum with common error variants
- [ ] Include NotFound, PermissionDenied, Timeout, etc.
- [ ] Implement conversion to/from system errors

### File Type
- [ ] Implement `File` struct in std/fs.kl
- [ ] Wrap OS file handles
- [ ] Implement `open()`, `create()` constructors
- [ ] Implement Read trait for File
- [ ] Implement Write trait for File
- [ ] Implement Drop to close file handle

### Standard I/O
- [ ] Implement `stdin()` function returning Stdin type
- [ ] Implement `stdout()` function returning Stdout type
- [ ] Implement `stderr()` function returning Stderr type
- [ ] Implement Read for Stdin
- [ ] Implement Write for Stdout, Stderr

### Buffered I/O
- [ ] Implement `BufReader[R: Read]` wrapper
- [ ] Implement `BufWriter[W: Write]` wrapper
- [ ] Add buffered read_line() method
- [ ] Handle buffer flushing on drop

### Path Type
- [ ] Implement `Path` struct in std/path.kl
- [ ] Support path parsing and manipulation
- [ ] Implement `join()`, `parent()`, `file_name()`
- [ ] Handle platform differences (/ vs \)

### Directory Operations
- [ ] Implement `fs.exists(path)` function
- [ ] Implement `fs.create_dir(path)` function
- [ ] Implement `fs.remove(path)` function
- [ ] Implement `fs.read_dir(path)` returning iterator

### Convenience Functions
- [ ] Implement `fs.read(path) -> Result[List[u8], IoError]`
- [ ] Implement `fs.read_string(path) -> Result[String, IoError]`
- [ ] Implement `fs.write(path, data) -> Result[void, IoError]`
- [ ] Implement `fs.write_string(path, s) -> Result[void, IoError]`

### Testing
- [ ] Test: can read and write files
- [ ] Test: buffered I/O works correctly
- [ ] Test: proper error handling with Result
- [ ] Test: directory operations work

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

---

## Milestone 6: Iterator Protocol

**Objective:** Implement iterators and for-loop integration.

### Iterator Trait
- [ ] Define `Iterator` trait in std/iter.kl
- [ ] Add associated type `type Item`
- [ ] Add `next(self: &mut Self) -> Option[Self.Item]`
- [ ] Add `size_hint() -> (usize, Option[usize])` with default

### IntoIterator Trait
- [ ] Define `IntoIterator` trait
- [ ] Add associated types `type Item`, `type IntoIter`
- [ ] Add `into_iter(self) -> Self.IntoIter`
- [ ] Implement for List, Set, Map, String

### For Loop Desugaring
- [ ] Transform `for x in collection { ... }` in parser/checker
- [ ] Call `into_iter()` on collection
- [ ] Generate while loop calling `next()`
- [ ] Handle loop variables correctly

### Range Iterators
- [ ] Implement Range struct for `start..end`
- [ ] Implement RangeInclusive for `start..=end`
- [ ] Implement Iterator for Range types
- [ ] Support integer ranges

### Iterator Adapters
- [ ] Implement `map[B](f: fn(Self.Item) -> B) -> Map[Self, B]`
- [ ] Implement `filter(pred: fn(&Self.Item) -> bool) -> Filter[Self]`
- [ ] Implement `take(n: usize) -> Take[Self]`
- [ ] Implement `skip(n: usize) -> Skip[Self]`
- [ ] Implement `enumerate() -> Enumerate[Self]`
- [ ] Implement `zip[U](other: U) -> Zip[Self, U.IntoIter]`

### Collect
- [ ] Define `FromIterator[T]` trait
- [ ] Implement `collect[C: FromIterator[Self.Item]](self) -> C`
- [ ] Implement FromIterator for List
- [ ] Implement FromIterator for Set
- [ ] Implement FromIterator for String (from chars)

### Testing
- [ ] Test: for loops work with any IntoIterator
- [ ] Test: iterator chains are lazy (not evaluated until needed)
- [ ] Test: can collect into List, Set
- [ ] Test: range iterators work correctly

**Files to Create:**
- `std/iter.kl` - Iterator, IntoIterator traits, adapters

---

## Milestone 7: Error Handling Improvements

**Objective:** Complete the `?` operator and improve error handling.

### Question Mark Operator
- [ ] Implement full `?` operator in checker
- [ ] For Result: return early on Err
- [ ] For Option: return early on None
- [ ] Generate proper early return code in codegen

### From Trait for Error Conversion
- [ ] Define `From[T]` trait in std/core
- [ ] Implement automatic error conversion in `?`
- [ ] Allow `IoError -> AppError` via `impl AppError: From[IoError]`
- [ ] Chain From implementations

### Try Blocks
- [ ] Parse `try { ... }` block expressions
- [ ] Evaluate to Result type
- [ ] Scope `?` operator within try block
- [ ] Support early return within try

### Error Context
- [ ] Add `.context(msg)` method to Result
- [ ] Wrap errors with additional context
- [ ] Support chained error messages
- [ ] Preserve original error for inspection

### Debug Mode Stack Traces
- [ ] Capture stack trace on error creation (debug mode only)
- [ ] Store trace in error metadata
- [ ] Display trace on unhandled errors
- [ ] Omit in release builds for performance

### Into Trait
- [ ] Define `Into[T]` trait (inverse of From)
- [ ] Blanket implement Into when From exists
- [ ] Use in error conversion chains

### Testing
- [ ] Test: `?` operator properly propagates errors
- [ ] Test: error types can be converted automatically
- [ ] Test: try blocks scope correctly
- [ ] Test: context messages preserved

**Files to Modify:**
- `src/checker.zig` - Enhance `?` operator checking
- `src/codegen/emit.zig` - Generate `?` early return code
- `std/core/result.kl` - Add context methods

---

## Milestone 8: Package Manager

**Objective:** Implement basic package management.

### Manifest Format
- [ ] Define klar.toml schema
- [ ] Parse [package] section (name, version, authors)
- [ ] Parse [dependencies] section
- [ ] Parse [dev-dependencies] section
- [ ] Support git dependencies with tag/branch/commit

### CLI: klar init
- [ ] Create new project directory structure
- [ ] Generate klar.toml with defaults
- [ ] Generate src/main.kl with hello world
- [ ] Set up .gitignore

### CLI: klar build (enhance)
- [ ] Read klar.toml for project info
- [ ] Resolve dependencies
- [ ] Build all source files
- [ ] Support --release flag for optimizations
- [ ] Support --target for cross-compilation

### CLI: klar run
- [ ] Build if needed
- [ ] Execute the binary
- [ ] Pass command line arguments

### CLI: klar test
- [ ] Discover test functions (with #[test] attribute)
- [ ] Build test binary
- [ ] Run tests and report results
- [ ] Support test filtering

### CLI: klar add
- [ ] Add dependency to klar.toml
- [ ] Resolve and download dependency
- [ ] Update lockfile

### Dependency Resolution
- [ ] Build dependency graph
- [ ] Resolve version constraints
- [ ] Handle diamond dependencies
- [ ] Detect version conflicts

### Lockfile
- [ ] Generate klar.lock with resolved versions
- [ ] Use lockfile for reproducible builds
- [ ] Update lockfile on dependency changes

### Testing
- [ ] Test: can create and build projects
- [ ] Test: dependencies resolved correctly
- [ ] Test: reproducible builds with lockfile

**Files to Create:**
```
src/package/
├── mod.zig              # Package management entry
├── manifest.zig         # klar.toml parsing
├── resolver.zig         # Dependency resolution
└── registry.zig         # Package registry client (future)
```

---

## Milestone 9: Tooling

**Objective:** Developer tooling for productive Klar development.

### Code Formatter (klar fmt)
- [ ] Parse source file into AST
- [ ] Pretty-print AST with consistent formatting
- [ ] Handle indentation (4 spaces)
- [ ] Format multi-line constructs
- [ ] Preserve/normalize comments
- [ ] Write formatted output back to file

### Enhanced klar check
- [ ] Full type checking without codegen
- [ ] Report all errors (not just first)
- [ ] Suggest fixes where possible
- [ ] Performance optimization for rapid feedback

### Documentation Generator (klar doc)
- [ ] Extract `///` documentation comments
- [ ] Generate HTML documentation
- [ ] Include type signatures
- [ ] Cross-reference links
- [ ] Generate index and navigation

### Language Server Protocol (LSP)
- [ ] Implement JSON-RPC server
- [ ] Handle initialize/shutdown lifecycle
- [ ] Implement textDocument/hover
- [ ] Implement textDocument/definition
- [ ] Implement textDocument/references
- [ ] Implement textDocument/completion
- [ ] Implement textDocument/diagnostics

### VS Code Extension
- [ ] Syntax highlighting grammar
- [ ] Language configuration (brackets, comments)
- [ ] LSP client integration
- [ ] Snippet support
- [ ] Build task integration

### Testing
- [ ] Test: formatter produces consistent output
- [ ] Test: LSP provides go-to-definition
- [ ] Test: LSP provides autocomplete
- [ ] Test: VS Code extension installs and works

**Files to Create:**
```
src/lsp/
├── mod.zig              # LSP entry point
├── server.zig           # JSON-RPC server
├── handlers.zig         # Request handlers
└── protocol.zig         # LSP protocol types

tools/
├── klar-fmt/            # Formatter (if separate)
└── vscode-klar/         # VS Code extension
    ├── package.json
    ├── syntaxes/klar.tmLanguage.json
    └── src/extension.ts
```

---

## Stretch Goals

These are valuable but not required for Phase 4 completion:

### Async/Await
- [ ] Design async runtime model
- [ ] Implement Future trait
- [ ] Implement async fn transformation
- [ ] Implement .await syntax
- [ ] Implement spawn for concurrent tasks

### Comptime
- [ ] Implement comptime block evaluation
- [ ] Support comptime function execution
- [ ] Enable compile-time code generation
- [ ] Replace need for macros

### Self-Hosting
- [ ] Port lexer to Klar
- [ ] Port parser to Klar
- [ ] Port checker to Klar
- [ ] Full self-hosted compiler

### REPL
- [ ] Implement read-eval-print loop
- [ ] Support incremental compilation
- [ ] Maintain REPL state between expressions

### WebAssembly Target
- [ ] Add WASM backend to codegen
- [ ] Handle WASM-specific ABI
- [ ] Test in browser/Node.js

---

## Implementation Order

Based on dependencies:

1. **Milestone 1: Generics** (foundation for everything)
2. **Milestone 3: Modules** (can start in parallel, needed for stdlib)
3. **Milestone 2: Traits** (needs generics)
4. **Milestone 4: Stdlib Core** (needs generics, traits, modules)
5. **Milestone 6: Iterators** (needs traits)
6. **Milestone 7: Error Handling** (needs traits)
7. **Milestone 5: Stdlib I/O** (needs core)
8. **Milestone 8: Package Manager** (needs modules)
9. **Milestone 9: Tooling** (needs stable language)

---

## Success Criteria

Phase 4 is complete when:

**Language Completeness:**
- [ ] Generic functions and types work correctly
- [ ] Traits can be defined and implemented
- [ ] Multi-file projects compile
- [ ] Standard library provides core functionality

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
