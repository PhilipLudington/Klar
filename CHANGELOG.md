# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [v0.3.0] - 2026-01-22

### Added

#### Native Compilation (LLVM Backend) — Phase 3
- Complete LLVM native code generation with IR layer, ownership analysis, and drop insertion
- Implement functions, calling conventions, and control flow
- Implement composite types: tuples, arrays, structs with named field access and bounds checking
- Implement Optional and Result types for native compilation
- Implement Rc[T], Arc[T], and Cell[T] for memory management
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
- Implement List[T] with push, with_capacity, clone, drop, and iterator adapters (take, skip, filter, map, enumerate, zip)
- Implement String with push, concat, chars, bytes methods
- Implement Set[T] with full hash set functionality
- Implement Map[K,V] with full hash map functionality
- Implement Range[T] as builtin iterator type with for-loop support

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
- Add GitStat integration for build/test status tracking

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
