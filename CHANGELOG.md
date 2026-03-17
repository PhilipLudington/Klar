# Changelog


Now I have enough context. Let me compose the changelog.

## [v0.5.0] - 2026-01-28

Klar v0.5.0 is a major release introducing the Meta Layer for code annotations, a WebAssembly compilation target, async/await across all backends, an LSP server, and significant self-hosting progress — the self-hosted lexer and parser now achieve full parity with the Zig compiler, and the self-hosted type checker covers generics, traits, closures, and module imports.

### Added

#### Meta Layer — Code Annotations for Developers and AI Agents

- Add `meta` keyword with 13 annotation kinds (`intent`, `decision`, `tag`, `hint`, `deprecated`, `pure`, `module`, `guide`, `related`, `group`, `in`, `define`, `custom`) attachable to all declarations including struct fields and enum variants
- Add `klar meta` CLI command for querying annotations across source files with `--tag`, `--module`, `--related`, `--deprecated`, and `--hints` modes (supports `--json` output for machine consumption)
- Add `meta define` for project-specific custom annotation vocabularies with compile-time validation of argument count, string constraints, and scope restrictions
- Add `meta pure` verification — compiler enforces that pure functions contain no I/O, no impure calls, and no non-pure method calls (local mutation is allowed)
- Add meta annotation validation during type checking: scope rules, group name resolution, deprecation warnings at call sites, and related-path resolution
- Add formatter support for preserving all 13 meta annotation kinds through round-trip formatting
- Add `meta module { ... }` and `meta guide { ... }` block annotations for file-level documentation and `meta group` for grouping related declarations

#### WebAssembly Compilation Target

- Add WebAssembly output via `klar build --target wasm` (freestanding wasm32) and `--target wasi` with LLVM wasm32 codegen, wasm-ld linking, and 32-bit pointer-aware `isize`/`usize`
- Unsupported features (filesystem, `readline`) trap at runtime with clear error messages on wasm targets

#### Async/Await Runtime

- Add `async`/`await` support across all three backends (native, VM, interpreter) with centralized `Future` state tags
- Add async keyword completion and hover docs in the LSP server
- Preserve `async` modifier in formatted output

#### LSP Server

- Add LSP server with stdio transport, diagnostics, completion, hover, and go-to-definition
- Add synced diagnostics and robust file URI handling

#### Testing Framework

- Add inline `test` block declarations with assertion helpers (`assert_ok`, `assert_err`, `assert_some`, `assert_none`) across all runtimes
- Add `klar test --fn` for running individual tests and directory mode for batch execution
- Add `--strict` and `--require` flags for enforcing test presence and coverage
- Add `--json` output for test results (including `--include-source`) with consistent error paths
- Add structured JSON compiler errors for tooling integration

#### Language Features

- **Breaking:** Generic type parameter syntax changed from `[T]` to `#[T]` — e.g. `fn max#[T: Ordered]`, `List#[i32]`, `Result#[T, E]`, `.as#[i32]`. This eliminates parsing ambiguity between generics and array indexing (Milestone 10)
  - **Before:** `fn max[T: Ordered](a: T, b: T) -> T`, `List[i32]`, `x.as[i64]`
  - **After:** `fn max#[T: Ordered](a: T, b: T) -> T`, `List#[i32]`, `x.as#[i64]`
  - **Migration:** Find and replace `[T]` with `#[T]` in generic positions across your codebase. `[` now always means array indexing; `#[` always means generics.
- Require explicit `shadow` keyword for variable shadowing — `shadow let x: i32 = ...` must be used when re-binding an outer variable, preventing accidental shadowing
- Add string methods (`byte_at`, `byte_len`, `substring`, `index_of`) and builtin functions (`from_byte`, `parse_int`, `parse_float`) across all backends; `len()` now returns UTF-8 codepoint count instead of byte count
- Add `List.last()`, `List.pop()`, index assignment (`list[i] = v`), and `List#[String].drop()` with string buffer cleanup in native codegen
- Add interpreter support for impl method dispatch, enum literals, and two-pass multi-module execution
- Add two-phase module compilation for circular import support
- Add recoverable module parsing API and partial recovery mode for error-tolerant tooling

#### Self-Hosting Progress

- Implement complete self-hosted lexer in Klar (~960 lines) with byte-for-byte JSON token stream parity against the Zig lexer
- Implement self-hosted parser achieving full parity (259/259 test files) including UTF-8 handling, string interpolation, and mandatory return type annotations
- Add full AST type hierarchy with flat arena pattern and typed indices for the self-hosted compiler
- Implement self-hosted type system definitions (44 `TypeKind` variants, 17 `PrimitiveKind` variants, 22 compound data structs)
- Implement self-hosted checker infrastructure: scope management, expression/statement/declaration checking, structured error kinds (23 variants), and generic monomorphization with caching
- Add trait infrastructure for self-hosted checker: builtin trait registration, `TypeVar` bound dispatch, and impl validation
- Add closure, if-expression, and match-expression type checking in self-hosted checker
- Add module import resolution with export registry and glob/selective imports
- Add assertion builtins (`assert_ok`, `assert_err`, `assert_some`, `assert_none`) with call-site validation matching Zig checker parity
- Add bootstrap architecture scaffolding with `dump-tokens`/`dump-ast` CLI commands for parity testing
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed

- **Breaking:** Generic type parameter syntax changed from `[T]` to `#[T]` — e.g. `fn max#[T: Ordered]`, `List#[i32]`, `Result#[T, E]`, `.as#[i32]`. This eliminates parsing ambiguity between generics and array indexing (Milestone 10)

## [v0.3.0] - 2026-01-22

### Added

#### Native Compilation (LLVM Backend) — Phase 3
- Complete LLVM native code generation with IR layer, ownership analysis, and drop insertion
- Implement functions, calling conventions, and control flow
- Implement composite types: tuples, arrays, structs with named field access and bounds checking
- Implement Optional and Result types for native compilation
- Implement Rc#[T], Arc#[T], and Cell#[T] for memory management
- Implement closures with variable capture and closure passing
- Implement runtime builtins and optimization passes
- Implement debug information, cross-compilation (`--target`), and `--emit-ir` flag

#### Generic Types and Traits — Phase 4
- Implement generic struct instantiation and monomorphization
- Implement generic enum pattern matching with `::` syntax
- Implement core trait system (definitions, implementations, bounds)
- Implement trait method resolution through generic bounds
- Implement builtin traits: Eq, Ordered, Clone, Drop, Default, Hash
- Implement trait inheritance and signature verification
- Implement associated types with `Self.Item` resolution

#### Comptime Evaluation
- Implement comptime blocks, functions, and recursive evaluation
- Implement `@typeInfo`, `@fields`, `@assert`, and `@repeat` builtins
- Implement comptime parameter modifier for compile-time argument validation

#### Standard Library I/O
- Implement stdout, stderr, and File I/O with Read/Write traits
- Implement BufReader and BufWriter buffered I/O types
- Implement Stdin support for standard input
- Implement `read_line()`, `read_to_string()`, and `read_all()` convenience methods

#### Builtin Collection Types
- Implement List#[T] with push, with_capacity, clone, drop, and iterator adapters (take, skip, filter, map, enumerate, zip)
- Implement String with push, concat, chars, bytes methods
- Implement Set#[T] with full hash set functionality
- Implement Map#[K,V] with full hash map functionality
- Implement Range#[T] as builtin iterator type with for-loop support

#### Module System
- Implement multi-file compilation with imports
- Implement aliased imports and circular import detection
- Implement REPL for interactive code exploration

#### Language Features
- Implement `?` operator for early return on Optional and Result types
- Implement From and Into traits for automatic type conversions
- Implement `.context()` method for Result error wrapping with ContextError
- Implement `ref`/`inout` reference syntax for parameters
- Implement sret calling convention for fixed-size array returns

### Fixed

- Fix VM struct field assignment, mutation crash, and UndefinedField bug
- Fix VM implicit optional return not wrapping as None
- Fix VM string interpolation memory leak
- Fix VM stack corruption with loop scoping and break cleanup
- Fix ownership checker segfault and type checker memory leaks
- Fix double-free in Ordered trait method params cleanup
- Fix struct method return type inference in codegen
- Fix File I/O Result type integration
- Fix native codegen for void if/else expressions
- Fix memory leaks in enum and struct monomorphization
- Fix struct parameters and string interpolation in native codegen
- Fix array mutation bug

### Changed

- Convert if/match from expressions to statements
- Require explicit type annotations on closures
- Change `.len()` to return i32 for ergonomic loop counters
- Unify comptime syntax to use `@` prefix consistently
- Reorganize test scripts into `scripts/` directory
- Add AirTower integration for build/test status tracking

## [v0.2.0] - 2026-01-15

### Added

- Complete VM integration and testing (Phase 2, Milestone 7)
- Implement debugging and tooling for bytecode VM (Phase 2, Milestone 6)
- Implement garbage collector and memory management (Phase 2, Milestone 5)
- Implement VM built-in functions and methods (Phase 2, Milestone 4)
- Implement bytecode VM core (Phase 2, Milestone 3)
- Implement bytecode compiler (Phase 2, Milestone 2)
- Implement bytecode instruction set for VM (Phase 2, Milestone 1)

## [v0.1.0] - 2026-01-14

### Added

- Initial release with tree-walking interpreter
- Core language: variables, functions, structs, enums, closures
- Type system with type inference
- Optional and Result types
- Pattern matching
- Ownership system foundation

[Unreleased]: https://github.com/PhilipLudington/Klar/compare/v0.3.0...HEAD
[v0.3.0]: https://github.com/PhilipLudington/Klar/compare/v0.2.0...v0.3.0
[v0.2.0]: https://github.com/PhilipLudington/Klar/compare/v0.1.0...v0.2.0
[v0.1.0]: https://github.com/PhilipLudington/Klar/releases/tag/v0.1.0
