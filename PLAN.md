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
- Generic functions, structs, and enums fully working (type checking, monomorphization, codegen)
- Traits working (definitions, implementations, bounds, method resolution through bounds)
- Single-file compilation only
- No standard library beyond builtins (print, panic, assert)

**Near-term Fixes (discovered via TheNarrowWindow game):**
- [x] VM: struct field assignment fails with "TypeError" - fixed by swapping pop/peek order in op_set_field handler
- [x] VM: struct field access fails with "UndefinedField" for all structs - fixed by emitting StructDescriptor with field names to constant pool
- [x] Native codegen: struct parameters to functions (fixed - track struct_type_name for parameters)
- [x] Native codegen: mutable struct variables (`var s = struct_value`) - already working
- [x] VM: struct field mutation crashes (hash map panic in ObjStruct.setField) - fixed by making setField duplicate keys
- [x] VM: ownership checker crashes on complex if/else blocks (fixed in 4b75243)
- [x] String interpolation with struct field access (`"{game.turn}"`) - implemented in native codegen
- [x] VM: implicit optional return not wrapping as None - fixed by emitting op_none before op_return for optional return types

---

## Milestone 1: Generic Type Checking

**Objective:** Implement full generic type checking with monomorphization.

**Status:** ✅ Complete. Generic functions, structs, enums, and struct methods all working (type checking, monomorphization, codegen registration, enum literal construction, match pattern parsing/checking, match expression codegen, struct methods via impl blocks).

### Type Parameter Tracking
- [x] Add TypeParam scope tracking in checker.zig
- [x] Store type parameters in function/struct context
- [x] Implement type parameter lookup during type resolution
- [x] Handle nested generic scopes correctly

### Type Parameter Substitution
- [x] Implement substituteTypeParams() for replacing type vars with concrete types
- [x] Handle recursive type substitution in nested generics
- [x] Create AppliedType representation for instantiated generic types
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
- [x] Handle generic struct methods

### Generic Enums
- [x] Support generic enum definitions: `enum Option[T] { Some(T), None }`
- [x] Instantiate generic enums at usage sites (type checker + monomorphization)
- [x] Generate monomorphized enum types for codegen
- [x] Parse and type-check generic enum variants in match patterns (Type[T]::Variant syntax)
- [x] Emit generic enum literal construction in codegen (EnumType[T]::Variant(payload) syntax)
- [x] Emit match expression codegen for all patterns

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

**Status:** ✅ Complete. Core trait infrastructure complete (trait registry, definition validation, impl checking, bounds parsing, method resolution through bounds, trait inheritance, associated types including generic context resolution via `T.Item`).

### Trait Registry
- [x] Create traits.zig module for trait management (implemented in checker.zig)
- [x] Implement TraitRegistry to store all trait definitions
- [x] Store trait method signatures and default implementations
- [x] Handle trait inheritance (trait A: B syntax) - supports single and multiple inheritance (A: B + C)

### Trait Definition Checking
- [x] Validate trait definitions (unique method names, valid signatures)
- [x] Check for conflicts in trait inheritance (first definition wins)
- [x] Store Self type placeholder for trait methods
- [ ] Handle associated types in traits

### Trait Implementation
- [x] Parse and check `impl Type: Trait { ... }` blocks
- [x] Verify all required methods are implemented
- [x] Check method signatures match trait definition (parameter count, types, return type)
- [x] Allow default method overrides (detected, not yet used for defaults)
- [x] Handle impl blocks for generic types

### Trait Bounds
- [x] Implement trait bounds on generics: `fn sort[T: Ordered](list: List[T])`
- [x] Check that type arguments satisfy trait bounds
- [x] Support multiple trait bounds: `T: Ordered + Clone`
- [ ] Handle where clauses for complex bounds

### Associated Types
- [x] Support `type Item` declarations in traits
- [x] Require associated type definitions in impl blocks
- [x] Resolve `Self.Item` references in trait methods
- [x] Handle associated types in generic contexts (T.Item resolution via AssociatedTypeRef)

### Method Resolution
- [x] Resolve trait method calls on concrete types (via struct_methods)
- [x] Look up correct implementation for method dispatch
- [x] Handle method calls through trait bounds in generics (type_var handling in checkMethodCall)
- [ ] Support UFCS (Uniform Function Call Syntax)

### Core Traits
- [x] Implement `Eq` trait (equality comparison) - builtin for primitives (int, float, bool, string)
- [x] Implement `Ordered` trait (comparison, ordering) - builtin for primitives (int, float, string)
- [x] Implement `Clone` trait (explicit cloning) - builtin for primitives (int, float, bool, string)
- [x] Implement `Drop` trait (custom destructors) - structs can implement to run cleanup code
- [x] Implement `Default` trait (default values) - builtin for primitives (i32.default() -> 0, etc.)
- [x] Implement `Hash` trait (hashing for maps/sets) - builtin for primitives (int, float, bool, string)

### Testing
- [x] Test: can define and implement traits (trait_basic.kl)
- [x] Test: trait bounds restrict generic parameters (trait_bounds.kl)
- [x] Test: method calls resolve to correct implementation (trait_default.kl)
- [x] Test: trait method calls through generic bounds (trait_method_through_bounds.kl, trait_method_with_args.kl)
- [x] Test: trait inheritance works correctly (trait_inheritance.kl, trait_multi_inherit.kl)
- [x] Test: Eq trait works for primitives (eq_trait.kl, eq_trait_bool.kl, eq_trait_string.kl)
- [x] Test: Ordered trait works for primitives (ordered_trait.kl, ordered_trait_float.kl, ordered_trait_string.kl)
- [x] Test: Clone trait works for primitives (clone_trait_int.kl, clone_trait_float.kl, clone_trait_bool.kl, clone_trait_string.kl)
- [x] Test: Drop trait works for structs (drop_trait.kl)
- [x] Test: Default trait works for primitives (default_trait_int.kl, default_trait_float.kl, default_trait_bool.kl, default_trait_string.kl)
- [x] Test: Hash trait works for primitives (hash_trait_int.kl, hash_trait_float.kl, hash_trait_bool.kl, hash_trait_string.kl)
- [x] Test: associated types work correctly (assoc_type_basic.kl, assoc_type_with_bounds.kl, assoc_type_self_item.kl)

**Files Modified:**
- `src/ast.zig` - TraitDecl super_traits field, AssociatedTypeDecl, AssociatedTypeBinding ✓
- `src/parser.zig` - Parse trait inheritance (: B + C) syntax, associated type declarations and bindings ✓
- `src/checker.zig` - Trait checking, impl validation, trait registry, trait method resolution through bounds, trait inheritance, Eq, Ordered, Clone, Drop, Hash trait definitions and checking, associated type processing ✓
- `src/types.zig` - Trait bounds to type system, AssociatedType in TraitType ✓
- `src/codegen/emit.zig` - Early struct registration for monomorphized function signatures, Eq, Ordered, Clone, Drop, and Hash trait codegen ✓
- `src/main.zig` - Call struct registration before monomorphized function declarations ✓
- `test/native/eq_trait.kl` - Test Eq for integers ✓
- `test/native/eq_trait_bool.kl` - Test Eq for booleans ✓
- `test/native/eq_trait_string.kl` - Test Eq for strings ✓
- `test/native/ordered_trait.kl` - Test Ordered for integers ✓
- `test/native/ordered_trait_float.kl` - Test Ordered for floats ✓
- `test/native/ordered_trait_string.kl` - Test Ordered for strings ✓
- `test/native/clone_trait_int.kl` - Test Clone for integers ✓
- `test/native/clone_trait_float.kl` - Test Clone for floats ✓
- `test/native/clone_trait_bool.kl` - Test Clone for booleans ✓
- `test/native/clone_trait_string.kl` - Test Clone for strings ✓
- `test/native/drop_trait.kl` - Test Drop for structs ✓
- `test/native/hash_trait_int.kl` - Test Hash for integers ✓
- `test/native/hash_trait_float.kl` - Test Hash for floats ✓
- `test/native/hash_trait_bool.kl` - Test Hash for booleans ✓
- `test/native/hash_trait_string.kl` - Test Hash for strings ✓

---

## Milestone 3: Module System

**Objective:** Implement multi-file compilation with imports and exports.

**Status:** ✅ Core functionality complete. Multi-file compilation works with selective imports, visibility enforcement, and topological ordering.

### Module Resolver
- [x] Create module_resolver.zig for module discovery
- [x] Implement file-to-module name mapping
- [x] Build module dependency graph
- [x] Detect and report circular imports

### Module Declaration
- [ ] Parse `module math.vector` declarations
- [ ] Validate module name matches file path
- [x] Support implicit module names from file location
- [x] Handle mod.kl for directory modules

### Import Resolution
- [x] Parse `import std.collections.List` statements
- [x] Resolve import paths to source files
- [x] Search in: current dir, std library, dependencies
- [x] Handle selective imports: `import std.io.{ read, write }`

### Import Variants
- [x] Implement glob imports: `import std.collections.*`
- [x] Implement aliased imports: `import std.collections.HashMap as Map`
- [ ] Implement relative imports: `import .sibling`, `import ..parent.child`
- [ ] Handle re-exports from modules

### Visibility System
- [x] Implement `pub fn`, `pub struct` visibility
- [x] Check visibility at import resolution
- [ ] Support `pub(package)` for internal APIs
- [x] Report errors for private item access

### Multi-File Compilation
- [x] Compile modules in dependency order
- [x] Link multiple object files into single binary
- [x] Share type information across modules
- [x] Handle cross-module references

### Project Structure
- [ ] Support standard project layout (src/, tests/, std/)
- [ ] Locate standard library from compiler installation
- [ ] Handle package manifest (klar.toml)

### Testing
- [x] Test: can compile multi-file projects
- [x] Test: imports resolve correctly
- [x] Test: circular imports detected and reported
- [x] Test: visibility enforced correctly

**Files Created:**
- `src/module_resolver.zig` - Module discovery, import resolution ✓

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

## Milestone 10: REPL

**Objective:** Interactive Read-Eval-Print Loop for exploration and AI-assisted development.

**Status:** ✅ Core functionality complete. Basic REPL works with interpreter backend.

The REPL is central to Klar's AI-native philosophy. It enables AI assistants to verify generated code before presenting it to users, transforming AI from "code suggester" to "code author."

### Core REPL Loop
- [x] Implement `klar repl` command
- [x] Parse and evaluate expressions interactively
- [x] Display results with type information
- [ ] Handle multi-line input (detect incomplete expressions)
- [x] Graceful error recovery (don't exit on errors)

### Stateful Sessions
- [x] Persist variable bindings across inputs
- [x] Persist function definitions across inputs
- [x] Persist type/struct definitions across inputs
- [x] Support `:reset` command to clear state
- [x] Support `:load <file>` to load definitions from file (single-file only)

### Introspection Commands
- [x] `:type <expr>` - Show type of expression without evaluating
- [ ] `:info <name>` - Show definition/signature of binding
- [x] `:list` - Show all current bindings
- [x] `:help` - Show available commands

### AI Integration Features
- [ ] Fast startup (< 100ms target)
- [ ] Machine-readable output mode (JSON)
- [ ] Sandboxed execution (no filesystem/network by default)
- [ ] Timeout for runaway computations
- [ ] Memory limits for safety

### Implementation Notes
- Uses interpreter backend (simpler, adequate for interactive use)
- Source strings persisted for symbol name stability
- Multi-file imports deferred to stretch goals (requires ModuleResolver integration)

### Testing
- [x] Test: can define and call functions interactively
- [x] Test: state persists across inputs
- [x] Test: errors don't crash the session
- [x] Test: introspection commands work correctly

**Files Created:**
```
src/repl.zig             # REPL loop and command handling ✓
```

---

## Milestone 11: Comptime

**Objective:** Compile-time code evaluation and generation.

**Status:** 🟡 In Progress. Comptime blocks and comptime functions work with primitives and structs.

Comptime enables powerful metaprogramming without macros—code that runs at compile time to generate code, validate invariants, or compute constants.

### Comptime Blocks
- [x] Parse `@{ ... }` blocks (comptime block syntax)
- [x] Evaluate comptime blocks during type checking
- [x] Use interpreter for comptime evaluation
- [x] Comptime blocks can contain literals, arithmetic, and boolean operations
- [x] Results are stored and emitted as compile-time constants in codegen
- [x] Support accessing outer scope constants (populateInterpreterEnv copies constant_values to interpreter)
- [x] Support struct types in comptime blocks
- [x] Support arrays in comptime blocks

### Comptime Functions
- [x] Mark functions as `fn @name(...)` (@ prefix on function name)
- [x] Comptime functions can only call other comptime functions (at call site)
- [x] Comptime functions execute at compile time when called with comptime-known arguments
- [x] Return values become compile-time constants
- [x] Nested comptime function calls supported (e.g., `@add(@mul(2, 3), 4)`)
- [x] Support recursive comptime function calls (implemented via shared interpreter environment)

### Comptime Parameters
- [x] Support `@param: Type` syntax (@ prefix on parameter name)
- [x] `fn foo(@n: i32)` - n must be known at compile time
- [ ] Enable type-level computation based on values (array sizes from comptime params, etc.)

### Compile-Time Reflection
- [x] `@typeName(T)` - Get string name of type (already implemented via builtins)
- [x] `@typeInfo(T)` - Get type kind string ("primitive", "struct", "enum", etc.)
- [x] `@hasField(T, "name")` - Check if struct has field (already implemented via builtins)
- [x] `@fields(T)` - Get comma-separated field names (struct) or variant names (enum)

### Code Generation
- [ ] Comptime can generate struct fields
- [ ] Comptime can generate function bodies
- [ ] Comptime can generate match arms
- [ ] Enable derive-like functionality without macros

### Comptime Assertions
- [x] `@assert(condition)` - Compile error if false (supports optional message)
- [x] `@compileError("message")` - Emit custom compile error (already implemented via builtins)
- [x] Validate invariants at compile time

### Integration with Generics
- [ ] Comptime expressions in generic bounds
- [ ] Conditional compilation based on type properties
- [ ] Generate specialized implementations

### Implementation Strategy
- [x] Use interpreter for comptime (simpler integration with type checker)
- [x] Interpreter can directly work with AST during type checking phase
- [ ] VM could be used later if comptime performance becomes a bottleneck
- [x] Comptime runs once per compilation, so speed is less critical than REPL

### Testing
- [x] Test: comptime blocks evaluate correctly (test/native/comptime_basic.kl)
- [x] Test: comptime functions produce constants (test/native/comptime_fn.kl, test/native/comptime_fn_simple.kl)
- [x] Test: @typeInfo returns correct metadata (test/native/typeinfo_basic.kl, typeinfo_struct.kl, typeinfo_enum.kl)
- [x] Test: comptime parameters work correctly (test/native/comptime_param.kl)
- [x] Test: comptime assertions catch errors at compile time (test/native/comptime_assert.kl)
- [x] Test: comptime blocks can access outer scope constants (test/native/comptime_const_access.kl)
- [x] Test: recursive comptime functions work (test/native/comptime_recursive.kl, comptime_recursive_simple.kl)
- [x] Test: comptime struct values work (test/native/comptime_struct.kl)
- [x] Test: comptime array values work (test/native/comptime_array.kl)

**Files Modified:**
- `src/ast.zig` - Added is_comptime field to FunctionDecl ✓
- `src/parser.zig` - Parse `comptime fn` syntax ✓
- `src/checker.zig` - Comptime evaluation during type checking, comptime function call evaluation, struct and array support in ComptimeValue ✓
- `src/interpreter.zig` - Made evalBlock public for comptime ✓
- `src/codegen/emit.zig` - Emit comptime values as constants (including struct and array values) ✓
- `src/codegen/llvm.zig` - Added Const.namedStruct and Const.array for comptime struct/array emission ✓

---

## Stretch Goals

These are valuable but not required for Phase 4 completion:

### Async/Await
- [ ] Design async runtime model
- [ ] Implement Future trait
- [ ] Implement async fn transformation
- [ ] Implement .await syntax
- [ ] Implement spawn for concurrent tasks

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
- [ ] Add multi-file import support (integrate ModuleResolver)
- [ ] Add tab completion for identifiers
- [ ] Add history persistence across sessions
- [ ] Add multi-line input support for function/struct definitions

---

## Implementation Order

Based on dependencies:

1. **Milestone 1: Generics** (foundation for everything) ✅
2. **Milestone 2: Traits** (needs generics) ✅
3. **Milestone 3: Modules** (needed for stdlib) ✅
4. **Milestone 10: REPL** (uses interpreter, enables AI workflow) ✅
5. **Milestone 11: Comptime** (uses interpreter, enables metaprogramming)
6. **Milestone 4: Stdlib Core** (needs generics, traits, modules)
7. **Milestone 6: Iterators** (needs traits)
8. **Milestone 7: Error Handling** (needs traits)
9. **Milestone 5: Stdlib I/O** (needs core)
10. **Milestone 8: Package Manager** (needs modules)
11. **Milestone 9: Tooling** (needs stable language)

---

## Success Criteria

Phase 4 is complete when:

**Language Completeness:**
- [x] Generic functions and types work correctly
- [x] Traits can be defined and implemented
- [x] Multi-file projects compile
- [ ] Standard library provides core functionality
- [ ] Comptime enables compile-time metaprogramming

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
