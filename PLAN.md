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
- [x] Native codegen: array index assignment (`arr[i] = value`) and struct field assignment (`s.field = value`)
- [x] Interpreter: array index assignment and struct field assignment
- [x] Native codegen: struct method return type inference - fixed by looking up method function by mangled name instead of defaulting to i32

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
- [x] Handle associated types in traits

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
- [x] Test: trait method calls with ref Self through generic bounds (trait_method_ref_bounds.kl)
- [x] Test: trait method calls with inout Self through generic bounds (trait_method_inout_bounds.kl)
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

**Status:** ✅ Core Complete. Optional, Result, builtin type methods (integer, string, array), List[T], String, Map[K,V], and Set[T] are all implemented as builtin types. Ordering type and Prelude not yet started (deferred - may not be needed given builtin approach).

### Option Type (Built-in as `?T`)
- [x] Built-in `?T` syntax for Optional types
- [x] `Some(T)` created via function return, `None` via implicit
- [x] Implement `is_some()`, `is_none()`
- [x] Implement `unwrap()` - panics on None
- [x] Implement `unwrap_or(default)` - returns value or default
- [x] Implement `expect(msg)` - panics with message on None
- [x] Implement `map(f)` - applies function to inner value
- [x] Implement `and_then(f)` - applies function returning Optional
- [x] Implement `Eq`, `Clone` for Option

### Result Type (Built-in as `Result[T, E]`)
- [x] Built-in `Result[T, E]` type with `Ok(T)` and `Err(E)` variants
- [x] Implement `is_ok()`, `is_err()`
- [x] Implement `unwrap()` - panics on Err
- [x] Implement `unwrap_err()` - panics on Ok
- [x] Implement `unwrap_or(default)` - returns value or default
- [x] Implement `expect(msg)` - panics with message on Err
- [x] Implement `ok()` - converts to `?T`
- [x] Implement `err()` - converts to `?E`
- [x] Implement `map(f)` - applies function to ok value
- [x] Implement `map_err(f)` - applies function to err value
- [x] Implement `and_then(f)` for Optional chaining
- [x] Implement `and_then(f)` for Result chaining
- [x] Implement `Eq`, `Clone` for Result

### Builtin Type Methods
The following methods are built into the compiler for primitive types:

**Integer Methods:**
- [x] Implement `abs()` - returns absolute value
- [x] Implement `min(other)` - returns minimum of two values
- [x] Implement `max(other)` - returns maximum of two values

**String Methods:**
- [x] Implement `len()` - returns string length as i32
- [x] Implement `is_empty()` - returns true if length is 0
- [x] Implement `contains(pattern)` - check if pattern exists in string
- [x] Implement `starts_with(prefix)` - check prefix match
- [x] Implement `ends_with(suffix)` - check suffix match
- [x] Implement `trim()` - remove leading/trailing whitespace
- [x] Implement `to_uppercase()` - convert to uppercase
- [x] Implement `to_lowercase()` - convert to lowercase
- [x] Implement `bytes()` - returns `[u8]` slice of UTF-8 bytes
- [x] Implement `chars()` - returns `[char]` slice of unicode codepoints

**Array/Slice Methods:**
- [x] Implement `len()` - returns length as i32
- [x] Implement `is_empty()` - returns true if length is 0
- [x] Implement `first()` - returns `?T` (first element or None)
- [x] Implement `last()` - returns `?T` (last element or None)
- [x] Implement `get(index)` - returns `?T` (element at index or None)
- [x] Implement `contains(value)` - check if value exists in array

### String Type (Builtin Heap-Allocated)
String is implemented as a builtin type in the compiler with heap-allocated memory.

**Type Representation:** `{ ptr: *u8, len: i32, capacity: i32 }`

**Implemented:**
- [x] `String.new()` - Create an empty string
- [x] `String.from(s)` - Create from string literal
- [x] `String.with_capacity(n)` - Create with pre-allocated capacity
- [x] `len()` -> i32 - Current length in bytes
- [x] `is_empty()` -> bool - Check if empty
- [x] `capacity()` -> i32 - Allocated capacity
- [x] `push(char)` - Append a character (with automatic growth)

**Also Implemented:**
- [x] `concat(other)` - Concatenate two strings (returns new String, supports chaining)
- [x] `append(other)` - Append another string (mutates self)
- [x] `as_str()` -> string - Get as primitive string
- [x] `clear()` - Clear contents (keeps capacity)
- [x] `clone()` -> String - Deep copy
- [x] `drop()` - Free memory
- [x] `eq(other)` -> bool - Equality comparison
- [x] `hash()` -> i64 - Hash code (FNV-1a)

**Files Modified:**
- `src/types.zig` - StringDataType definition
- `src/checker.zig` - Type checking and method validation
- `src/codegen/emit.zig` - LLVM codegen for String operations
- `src/runtime/string_heap.zig` - Runtime functions (for reference, inline codegen used)
- `test/native/string_basic.kl` - Basic tests

### List Type (Builtin)
List[T] is implemented as a builtin type in the compiler, not in the standard library.

**Implemented:**
- [x] `List.new[T]()` - Create an empty list
- [x] `push(value)` - Add element to end
- [x] `pop()` -> ?T - Remove and return last element
- [x] `get(index)` -> ?T - Get element at index
- [x] `set(index, value)` - Set element at index
- [x] `len()` -> i32 - Current length
- [x] `is_empty()` -> bool - Check if empty
- [x] `first()` -> ?T - Get first element
- [x] `last()` -> ?T - Get last element
- [x] `clear()` - Remove all elements
- [x] `capacity()` -> i32 - Allocated capacity
- [x] For-loop iteration (`for x in list`)

**Not Yet Implemented:**
- [x] `with_capacity(n)` - Create with pre-allocated capacity
- [x] Clone trait - Clone list with cloneable elements
- [x] Drop trait - Proper memory cleanup

### Map Type (Builtin)
Map[K, V] is implemented as a builtin type in the compiler with hash-based implementation.

**Implemented:**
- [x] `Map.new[K, V]()` - Create an empty map
- [x] `insert(key, value)` - Insert or update key-value pair
- [x] `get(key)` -> ?V - Get value for key (optional)
- [x] `remove(key)` -> ?V - Remove and return value
- [x] `contains_key(key)` -> bool - Check if key exists
- [x] `len()` -> i32 - Current number of entries
- [x] `is_empty()` -> bool - Check if empty
- [x] `capacity()` -> i32 - Allocated capacity
- [x] `clear()` - Remove all entries
- [x] `clone()` -> Map[K, V] - Deep copy
- [x] `drop()` - Free memory
- [x] Automatic resizing on insert

**Files Modified:**
- `src/checker.zig` - Map type checking and method validation
- `src/codegen/emit.zig` - LLVM codegen for Map operations
- `test/native/map_basic.kl` - Basic tests
- `test/native/map_resize.kl` - Resize/capacity tests

### Set Type (Builtin)
Set[T] is implemented as a builtin type in the compiler with hash-based implementation.

**Implemented:**
- [x] `Set.new[T]()` - Create an empty set
- [x] `Set.with_capacity[T](n)` - Create with pre-allocated capacity
- [x] `insert(value)` -> bool - Insert element, returns true if new
- [x] `contains(value)` -> bool - Check membership
- [x] `remove(value)` -> bool - Remove element, returns true if existed
- [x] `len()` -> i32 - Current number of elements
- [x] `is_empty()` -> bool - Check if empty
- [x] `capacity()` -> i32 - Allocated capacity
- [x] `clear()` - Remove all elements
- [x] `clone()` -> Set[T] - Deep copy
- [x] `drop()` - Free memory
- [x] `union(other)` -> Set[T] - Set union
- [x] `intersection(other)` -> Set[T] - Set intersection
- [x] `difference(other)` -> Set[T] - Set difference

**Files Modified:**
- `src/checker.zig` - Set type checking and method validation
- `src/codegen/emit.zig` - LLVM codegen for Set operations
- `test/native/set_basic.kl` - Basic tests
- `test/native/set_operations.kl` - Set operation tests (union, intersection, difference)

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
- [x] Test: can create and manipulate strings (string_basic.kl, string_concat.kl, etc.)
- [x] Test: List works for dynamic collections (list_basic.kl, list_capacity.kl, list_iter.kl)
- [x] Test: Map works with hashable keys (map_basic.kl, map_resize.kl)
- [x] Test: Set works with hashable elements (set_basic.kl, set_operations.kl)
- [x] Test: Option methods work correctly (optional_*.kl tests)
- [x] Test: Result methods work correctly (result_*.kl tests)

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

**Status:** 🟡 Read/Write Traits Complete. Mutable buffer I/O with arrays working. Basic file I/O types and stdout/stderr implemented as builtins. Buffered I/O complete. Filesystem operations not yet started.

### Mutable Buffer Allocation
- [x] `@repeat(value, count)` builtin for array initialization: `var buf: [u8; 256] = @repeat(0.as[u8], 256)`
- [x] `ref T` syntax for immutable references (replaces `&T`)
- [x] `inout T` syntax for mutable references (replaces `&mut T`)
- [x] `ref x` expression for creating references (replaces `&x`, mutability determined by whether x is `var` or `let`)
- [x] Deref assignment support: `*ptr = value`
- [x] read()/write() accept both `[u8]` slices and `[u8; N]` arrays

### Read Trait
- [x] Define `Read` trait as builtin (in checker.zig)
- [x] Add `read(self: inout Self, buf: [u8]) -> Result[i32, IoError]` (slice by value)
- [x] Register `File:Read` implementation
- [x] Add `File.read_all(path)`, `File.read_to_string(path)` static convenience methods

### Write Trait
- [x] Define `Write` trait as builtin (in checker.zig)
- [x] Add `write(self: inout Self, buf: [u8]) -> Result[i32, IoError]` (slice by value)
- [x] Add `flush(self: inout Self) -> Result[void, IoError]`
- [x] Register `File:Write`, `Stdout:Write`, `Stderr:Write` implementations
- [ ] Add default `write_all()`

### IoError Type (Builtin)
- [x] Define `IoError` enum as builtin type
- [x] Include NotFound, PermissionDenied, AlreadyExists, InvalidInput, UnexpectedEof, Other(string) variants
- [ ] Implement conversion to/from system errors (errno mapping)

### File Type (Builtin)
- [x] Implement `File` as builtin type (opaque FILE* handle)
- [x] Implement `File.open(path, mode)` static constructor
- [x] Implement `File.read_to_string(path)` convenience method (opens, reads, closes)
- [x] Implement `File.read_all(path)` convenience method (opens, reads bytes, closes)
- [x] Implement `read(&mut self, buf)` method
- [x] Implement `write(&mut self, buf)` method
- [x] Implement `write_string(&mut self, s)` method
- [x] Implement `close(self)` method
- [x] Implement `flush(&mut self)` method
- [x] Full Result type integration (is_ok/is_err/unwrap on returned Results)
- [x] Implement Read/Write traits for File

### Standard I/O (Builtin)
- [x] Implement `stdin()` function returning Stdin type
- [x] Implement `stdout()` function returning Stdout type
- [x] Implement `stderr()` function returning Stderr type
- [x] Implement Read for Stdin (`read(&mut self, buf: &mut [u8]) -> Result[i32, IoError]`)
- [x] Implement `Stdout.write(buf)`, `Stdout.write_string(s)` and `Stdout.flush()`
- [x] Implement `Stderr.write(buf)`, `Stderr.write_string(s)` and `Stderr.flush()`
- [x] Implement Write trait for Stdout and Stderr
- [x] Platform-specific stdio access (macOS: `__stdoutp`/`__stderrp`/`__stdinp`, Linux: `stdout`/`stderr`/`stdin`)

### Buffered I/O
- [x] Implement `BufReader[R: Read]` wrapper
- [x] Implement `BufWriter[W: Write]` wrapper
- [x] Add buffered read_line() method
- [x] Handle buffer flushing on drop (BufWriter automatically flushes when going out of scope)

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
- [x] Test: stdout write_string and flush work (stdout_basic.kl)
- [x] Test: Write trait with write(buf) works (write_trait_basic.kl)
- [x] Test: Write trait on Stdout (io_generic.kl)
- [x] Test: file write and error handling (file_write.kl, file_error.kl)
- [x] Test: stdin type exists and can be obtained (stdin_basic.kl)
- [x] Test: file read with mutable array buffer (file_read_buffer.kl)
- [x] Test: buffered I/O works correctly (bufreader_basic.kl, bufwriter_basic.kl, bufwriter_writestr.kl)
- [x] Test: BufReader.read_line() works correctly (bufreader_readline.kl)
- [x] Test: BufWriter automatic flush on drop works correctly (bufwriter_drop.kl)
- [ ] Test: directory operations work

**Files Modified:**
- `src/types.zig` - Added file, io_error, stdout_handle, stderr_handle, stdin_handle types ✓
- `src/checker.zig` - Registered I/O types and methods, Read/Write traits, Stdin:Read, array buffer support ✓
- `src/codegen/emit.zig` - Implemented I/O codegen, fixed buffer reference handling, stdin support, extractBufferPtrAndLen helper ✓
- `test/native/stdout_basic.kl` - Stdout test ✓
- `test/native/stdin_basic.kl` - Stdin test ✓
- `test/native/file_write.kl` - File write test ✓
- `test/native/file_error.kl` - File error test ✓
- `test/native/write_trait_basic.kl` - Write trait test ✓
- `test/native/read_trait_basic.kl` - Read trait test (placeholder) ✓
- `test/native/io_generic.kl` - Generic I/O test ✓
- `test/native/file_read_buffer.kl` - File read with mutable array buffer ✓

**Files to Create (future):**
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

**Status:** ✅ Core Complete. For-loops work with Range[T], arrays, List[T], Set[T], and Map[K,V]. Iterator adapter methods implemented on collection types. Lazy iterator types and collect not yet started.

### Iterator Trait
- [x] Define `Iterator` trait as builtin (in checker.zig)
- [x] Add associated type `type Item`
- [x] Add `next(self: &mut Self) -> ?Self.Item`
- [ ] Add `size_hint() -> (usize, ?usize)` with default

### IntoIterator Trait
- [x] Define `IntoIterator` trait as builtin (in checker.zig)
- [x] Add associated types `type Item`, `type IntoIter`
- [x] Add `into_iter(self) -> Self.IntoIter`
- [x] For-loop iteration for List, Set, Map (via direct codegen, not trait dispatch)
- [ ] Implement for String

### For Loop Implementation
- [x] Parse `for x in collection { ... }` syntax
- [x] Support for-loops over Range literals (`for i in 0..10`)
- [x] Support for-loops over Range variables (`var r = 0..10; for i in r`)
- [x] Support for-loops over arrays (`for x in [1, 2, 3]`)
- [x] Support for-loops over List[T] (`for x in list`, test/native/list_for.kl)
- [x] Handle loop variables correctly with proper scoping
- [x] Support `break` and `continue` in for-loops
- [x] Support for-loops over Map[K,V] (`for (k, v) in map`, test/native/map_for.kl)
- [x] Support for-loops over Set[T] (`for x in set`, test/native/set_for.kl)
- [ ] Desugar via `into_iter()` for custom types (currently only builtin types supported)

### Range Iterators
- [x] Implement `Range[T]` builtin type with `{start, end, current, inclusive}` layout
- [x] Support exclusive ranges: `start..end`
- [x] Support inclusive ranges: `start..=end`
- [x] Implement `next()` method for Range iteration
- [x] Implement `reset()`, `is_empty()`, `len()`, `clone()` methods
- [x] Support integer element types

### Iterator Adapters (as collection methods)
Note: Implemented as eager methods on collection types, not lazy iterator types.
- [x] `take(n)` - List, Set, Map
- [x] `skip(n)` - List, Set, Map
- [x] `filter(pred)` - List, Set, Map (Map uses 2-arg predicate)
- [x] `map(f)` / `map_values(f)` - List, Set (returns List), Map (map_values)
- [x] `enumerate()` - List, Set (returns List of tuples)
- [x] `zip(other)` - List, Set (returns List of tuples)
- [ ] Lazy iterator types (Map[Self, B], Filter[Self], etc.) - deferred

### Collect
- [ ] Define `FromIterator[T]` trait
- [ ] Implement `collect[C: FromIterator[Self.Item]](self) -> C`
- [ ] Implement FromIterator for List
- [ ] Implement FromIterator for Set
- [ ] Implement FromIterator for String (from chars)

### Testing
- [x] Test: for loops work with Range literals (for_range.kl)
- [x] Test: for loops work with Range variables (for_range.kl)
- [x] Test: for loops work with arrays (for_array.kl)
- [x] Test: for loops work with List (list_for.kl)
- [x] Test: for loops work with Set (set_for.kl)
- [x] Test: for loops work with Map (map_for.kl)
- [x] Test: range iterators work correctly (range_basic.kl, range_inclusive.kl)
- [x] Test: Iterator trait can be used as bound (iter_trait_basic.kl)
- [x] Test: List adapters work (list_take.kl, list_skip.kl, list_filter.kl, list_map.kl, list_enumerate.kl, list_zip.kl)
- [x] Test: Set adapters work (set_take.kl, set_skip.kl, set_filter.kl, set_map.kl, set_enumerate.kl, set_zip.kl)
- [x] Test: Map adapters work (map_take.kl, map_skip.kl, map_filter.kl, map_values.kl)
- [ ] Test: iterator chains are lazy (not evaluated until needed) - N/A for eager methods
- [ ] Test: can collect into List, Set

**Files Modified:**
- `src/checker.zig` - Iterator/IntoIterator trait definitions, Range method checking, collection adapter methods ✓
- `src/codegen/emit.zig` - For-loop codegen for Range, arrays, List, Set, Map; iterator adapter emission ✓
- `src/types.zig` - RangeType definition ✓
- `test/native/for_range.kl` - Range for-loop tests ✓
- `test/native/for_array.kl` - Array for-loop tests ✓
- `test/native/list_for.kl` - List for-loop tests ✓
- `test/native/set_for.kl` - Set for-loop tests ✓
- `test/native/map_for.kl` - Map for-loop tests ✓
- `test/native/list_*.kl` - List adapter tests ✓
- `test/native/set_*.kl` - Set adapter tests ✓
- `test/native/map_*.kl` - Map adapter tests ✓

---

## Milestone 7: Error Handling Improvements

**Objective:** Complete the `?` operator and improve error handling.

**Status:** ✅ Core Complete. `?` operator implemented for early return on Optional and Result types, with automatic error conversion via From trait. Error context via `.context()` method implemented.

### Question Mark Operator
- [x] Implement full `?` operator in checker
- [x] For Result: return early on Err
- [x] For Option: return early on None
- [x] Generate proper early return code in codegen
- [x] Test: `?` on Optional propagates None (optional_propagate.kl)
- [x] Test: `?` on Result propagates Err (result_propagate.kl)

### From Trait for Error Conversion
- [x] Define `From[T]` trait as builtin (in checker.zig)
- [x] Implement automatic error conversion in `?`
- [x] Allow `IoError -> AppError` via `impl AppError: From[IoError]`
- [x] Test: error_from_conversion.kl verifies From::from() is called during `?` propagation
- [ ] Chain From implementations

### Error Context
- [x] Add `.context(msg)` method to Result
- [x] Add `ContextError[E]` builtin type with message and cause
- [x] `Result[T, E].context(msg) -> Result[T, ContextError[E]]`
- [x] `ContextError[E].message() -> string` returns context message
- [x] `ContextError[E].cause() -> E` returns original error
- [x] `ContextError[E].display_chain() -> string` formats full error chain
- [x] Support chained error messages (nested ContextError types)
- [x] Preserve original error for inspection
- [x] Test: result_context.kl verifies context wrapping, chaining, message/cause extraction
- [x] Test: result_context_display.kl verifies display_chain formatting

### Debug Mode Location Tracking
- [x] Capture file:line:column on `.context()` call (debug mode only)
- [x] Store location in ContextError metadata (5-field layout: message, cause, file, line, column)
- [x] Display location in `display_chain()` when file is non-null
- [x] Omit location in release builds (file=null, line=0, column=0)

### Into Trait
- [x] Define `Into[T]` trait (inverse of From)
- [x] Method signature: `fn into(self: Self) -> T`
- [x] Type variable substitution for return types in verifyMethodSignature()
- [ ] Blanket implement Into when From exists
- [ ] Use in error conversion chains

### Testing
- [x] Test: `?` operator properly propagates errors (optional_propagate.kl, result_propagate.kl)
- [x] Test: error types can be converted automatically (error_from_conversion.kl)
- [x] Test: context messages preserved (result_context.kl)
- [x] Test: Into trait works for type conversion (into_trait.kl)

**Files Modified:**
- `src/types.zig` - Added ContextErrorType to Type union ✓
- `src/checker.zig` - Enhanced `?` operator checking, expected_type context, checkOkErrCall, .context() method, ContextError methods (message, cause, display_chain), ContextError[E] type resolution ✓
- `src/codegen/emit.zig` - Generate `?` early return code, extended ReturnTypeInfo, expected_type propagation, inferExprType for unwrap_err/unwrap, ContextError codegen, display_chain implementation ✓
- `src/interpreter.zig` - Added ContextError support, Result methods, ContextError methods (message, cause, display_chain) ✓
- `src/values.zig` - Added ContextErrorValue ✓
- `test/native/optional_propagate.kl` - Test `?` on Optional ✓
- `test/native/result_propagate.kl` - Test `?` on Result ✓
- `test/native/result_propagate_simple.kl` - Simpler `?` on Result test ✓
- `test/native/error_from_conversion.kl` - Test From trait error conversion ✓
- `test/native/result_context.kl` - Test .context() method and ContextError ✓
- `test/native/result_context_display.kl` - Test display_chain() method ✓
- `test/native/into_trait.kl` - Test Into trait type conversion ✓

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

**Status:** ✅ Core Complete. Comptime blocks, functions, parameters, reflection, and assertions all working. Advanced features (code generation, dependent types) deferred.

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

### Windows Support
- [ ] Add Windows target triple detection in native codegen
- [ ] Handle Windows-specific stderr access (UCRT `__acrt_iob_func`)
- [ ] Handle Windows-specific stdout access
- [ ] Test libc function linking on Windows (MSVCRT/UCRT)
- [ ] Add Windows CI testing

---

## Implementation Order

Based on dependencies:

1. **Milestone 1: Generics** (foundation for everything) ✅
2. **Milestone 2: Traits** (needs generics) ✅
3. **Milestone 3: Modules** (needed for stdlib) ✅
4. **Milestone 10: REPL** (uses interpreter, enables AI workflow) ✅
5. **Milestone 11: Comptime** (uses interpreter, enables metaprogramming) ✅ Core Complete
6. **Milestone 6: Iterators** (for-loops, collection adapters) ✅ Core Complete
7. **Milestone 4: Stdlib Core** (needs generics, traits, modules) ✅ Core Complete
8. **Milestone 7: Error Handling** (`?` operator done, From/Into traits pending) ← **CURRENT**
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
- [x] Standard library provides core functionality (as builtins: Optional, Result, List, String, Map, Set)
- [x] Comptime enables compile-time metaprogramming (core features complete)
- [x] For-loops work with Range, arrays, List, Set, Map
- [x] `?` operator for error propagation

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
