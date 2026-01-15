# Klar Implementation Plan

Implementation plan for the Klar programming language, based on DESIGN.md specification.

---

## Overview

**Goal**: Build a self-hosting Klar compiler, starting with a Zig implementation.

**Strategy**:
1. Phase 1: Tree-walking interpreter (validate language design) ✓
2. Phase 2: Bytecode VM (practical performance)
3. Phase 3: Native compiler (Cranelift/LLVM)
4. Phase 4: Self-hosting (Klar compiler written in Klar)

**Current**: Phase 1 complete. Ready for Phase 2.

---

## Phase 1: Tree-Walking Interpreter

### Milestone 1: Project Setup & Lexer ✓

#### Tasks

- [x] **1.1 Initialize project structure**
  - Create `build.zig` and `build.zig.zon`
  - Set up `src/` directory with module files
  - Configure test infrastructure

- [x] **1.2 Implement token types** (`src/token.zig`)
  - Define all token kinds from spec (keywords, operators, literals)
  - Add source location tracking (line, column, span)
  - Implement `lexeme()` helper for debugging

- [x] **1.3 Implement lexer** (`src/lexer.zig`)
  - Single-character tokens (delimiters, simple operators)
  - Multi-character operators (`==`, `!=`, `->`, `=>`, `??`, `..`, etc.)
  - Wrapping/saturating operators (`+%`, `-|`, `*%`, etc.)
  - Keywords vs identifiers lookup
  - Integer literals (decimal, hex `0x`, binary `0b`, octal `0o`, underscores)
  - Float literals (decimal, scientific notation, type suffixes)
  - String literals (regular, raw `r"..."`, multi-line `"""`)
  - Character literals (including unicode `\u{...}`)
  - Comments (line `//`, block `/* */`, doc `///`, module `//!`)
  - Newline handling (significant for statement termination)
  - Error recovery and invalid token reporting

- [x] **1.4 Lexer tests**
  - Test all token types
  - Test edge cases (unterminated strings, invalid escapes)
  - Test source locations are accurate

- [x] **1.5 Basic CLI** (`src/main.zig`)
  - `klar run <file>` - tokenize and display
  - `klar help` - usage information
  - `klar version` - version string
  - File reading with error handling

---

### Milestone 2: AST & Expression Parser ✓

#### Tasks

- [x] **2.1 Define AST nodes** (`src/ast.zig`)

  **Expressions**:
  - `Literal` - int, float, string, char, bool
  - `Identifier` - variable reference
  - `Binary` - arithmetic, comparison, logical (`and`, `or`)
  - `Unary` - prefix (`-`, `not`, `&`, `&mut`, `*`)
  - `Postfix` - `?`, `!`
  - `Call` - function call
  - `Index` - array/slice indexing `a[i]`
  - `Field` - field access `a.b`
  - `MethodCall` - `a.method(args)`
  - `If` - if expression (value-producing)
  - `Match` - pattern matching expression
  - `Block` - `{ ... }` expression
  - `Closure` - `|args| body` with optional type annotations
  - `Range` - `a..b`, `a..=b`
  - `Struct` - struct literal `Point { x: 1, y: 2 }`
  - `Array` - array literal `[1, 2, 3]`
  - `Tuple` - tuple literal `(a, b, c)`
  - `TypeCast` - `.as[T]`, `.to[T]`, `.trunc[T]`

  **Patterns** (for match/destructuring):
  - `Wildcard` - `_`
  - `Literal` - literal value pattern
  - `Binding` - variable binding
  - `Variant` - enum variant `Option.Some(x)`
  - `Struct` - struct destructure `Point { x, y }`
  - `Tuple` - tuple destructure `(a, b)`
  - `Or` - alternative patterns `1 | 2 | 3`
  - `Guard` - pattern with guard `n if n > 0`

- [x] **2.2 Implement Pratt parser foundation** (`src/parser.zig`)
  - Token stream management (peek, advance, expect)
  - Error reporting with source locations
  - Synchronization for error recovery
  - Precedence table from spec

- [x] **2.3 Parse literals**
  - Integer parsing (handle bases, underscores, suffixes)
  - Float parsing (handle scientific, suffixes)
  - String parsing (handle escapes, raw strings)
  - Character parsing (handle unicode escapes)
  - Boolean literals (`true`, `false`)

- [x] **2.4 Parse operators by precedence**

  | Prec | Operators |
  |------|-----------|
  | 1 | `.` `()` `[]` (postfix) |
  | 2 | `?` `!` (postfix) |
  | 3 | `-` `not` `&` `&mut` `*` (prefix) |
  | 4 | `*` `/` `%` `*%` `*|` |
  | 5 | `+` `-` `+%` `-%` `+|` `-|` |
  | 6 | `<<` `>>` |
  | 7 | `&` (bitwise) |
  | 8 | `^` |
  | 9 | `|` (bitwise) |
  | 10 | `..` `..=` |
  | 11 | `==` `!=` `<` `>` `<=` `>=` `is` |
  | 12 | `and` |
  | 13 | `or` |
  | 14 | `??` |
  | 15 | `=` `+=` `-=` etc. |

- [x] **2.5 Parse complex expressions**
  - If expressions: `if cond { a } else { b }`
  - Match expressions: `match value { ... }`
  - Block expressions: `{ stmt; stmt; expr }`
  - Closures: `|x, y| x + y`
  - Struct literals: `Point { x: 1, y: 2 }`
  - Array/tuple literals

- [x] **2.6 Expression parser tests**
  - Precedence correctness
  - Associativity correctness
  - All literal types
  - Complex nested expressions

---

### Milestone 3: Statement & Declaration Parser ✓

#### Tasks

- [x] **3.1 Define statement AST nodes**

  **Statements**:
  - `Let` - `let x: T = expr`
  - `Var` - `var x: T = expr`
  - `Assignment` - `x = expr`, `x += expr`
  - `Expression` - expression as statement
  - `Return` - `return expr`
  - `Break` - `break` or `break value`
  - `Continue` - `continue`
  - `For` - `for item in iter { ... }`
  - `While` - `while cond { ... }`
  - `Loop` - `loop { ... }`

  **Declarations**:
  - `Function` - `fn name[T](params) -> Ret { body }`
  - `Struct` - `struct Name { fields }`
  - `Enum` - `enum Name { variants }`
  - `Trait` - `trait Name { methods }`
  - `Impl` - `impl Type: Trait { methods }`
  - `TypeAlias` - `type Name = T`
  - `Const` - `const NAME = value`
  - `Import` - `import path.to.module`
  - `Module` - `module name`

- [x] **3.2 Parse variable declarations**
  - `let` (immutable) with optional type annotation
  - `var` (mutable) with optional type annotation
  - Type inference marker

- [x] **3.3 Parse control flow statements**
  - `for item in iterable { body }`
  - `for (index, item) in iter.enumerate() { body }`
  - `while condition { body }`
  - `loop { body }`
  - `break` and `break value`
  - `continue`
  - `return` and `return value`

- [x] **3.4 Parse function declarations**
  - Function signature: `fn name(params) -> ReturnType`
  - Generic parameters: `fn name[T, U](params)`
  - Trait bounds: `fn name[T: Trait](params)`
  - Where clauses: `where T: A + B`
  - Single-expression shorthand: `fn double(x: i32) -> i32 = x * 2`
  - Async functions: `async fn fetch()`
  - Public visibility: `pub fn`

- [x] **3.5 Parse type declarations**
  - Structs with fields and visibility
  - Enums with variants (unit, tuple, struct)
  - Trait declarations with method signatures
  - Impl blocks (inherent and trait)
  - Type aliases

- [x] **3.6 Parse imports and modules**
  - `import std.collections`
  - `import std.collections.{ List, Map }`
  - `import std.collections.*`
  - `import path as alias`
  - `module name.subname`

- [x] **3.7 Statement/declaration tests**
  - All statement types
  - All declaration types
  - Visibility modifiers
  - Generic parameters

---

### Milestone 4: Type System ✓

#### Tasks

- [x] **4.1 Define type representation** (`src/types.zig`)

  **Primitive types**:
  - `i8`, `i16`, `i32`, `i64`, `i128`, `isize`
  - `u8`, `u16`, `u32`, `u64`, `u128`, `usize`
  - `f32`, `f64`
  - `bool`, `char`, `string`
  - `void`, `never` (!)

  **Composite types**:
  - `Array` - `[T; N]`
  - `Slice` - `[T]`
  - `Tuple` - `(T1, T2, ...)`
  - `Option` - `?T`
  - `Result` - `Result[T, E]`
  - `Function` - `fn(Args) -> Ret`
  - `Reference` - `&T`, `&mut T`
  - `Pointer` - raw pointers (unsafe)

  **User-defined**:
  - `Struct` - named struct type
  - `Enum` - enum type with variants
  - `Trait` - trait type
  - `Generic` - type parameter `T`
  - `Applied` - `Container[T]`

- [x] **4.2 Implement type checker** (`src/checker.zig`)
  - Symbol table / scope management
  - Type environment
  - Type unification
  - Error accumulation

- [x] **4.3 Type inference**
  - Infer literal types
  - Infer from context (let binding)
  - Infer generic parameters
  - Bidirectional type checking

- [x] **4.4 Type checking rules**
  - No implicit numeric conversions
  - Require `.as[T]` for widening
  - Require `.to[T]` for narrowing (with trap)
  - Option type for nullable (`?T`)
  - Reference validity (borrows)

- [x] **4.5 Trait resolution**
  - Check trait implementations exist
  - Resolve method calls through traits
  - Check trait bounds on generics

- [x] **4.6 Type checker tests**
  - Primitive type checking
  - Struct/enum type checking
  - Generic instantiation
  - Error detection (type mismatches)

---

### Milestone 5: Interpreter ✓

#### Tasks

- [x] **5.1 Define runtime values** (`src/values.zig`)
  - Integer values (all sizes)
  - Float values
  - Boolean, char, string
  - Struct instances
  - Enum instances
  - Arrays, slices, tuples
  - Closures (captured environment)
  - References
  - Option/Result wrappers

- [x] **5.2 Implement environment**
  - Variable storage
  - Scope chain
  - Function definitions
  - Type definitions

- [x] **5.3 Evaluate expressions** (`src/interpreter.zig`)
  - Literals → values
  - Variables → lookup
  - Binary operations (with overflow behavior)
  - Unary operations
  - Function calls
  - Method calls
  - Field access
  - Indexing
  - If expressions
  - Match expressions
  - Block expressions

- [x] **5.4 Execute statements**
  - Let/var declarations
  - Assignments
  - Return statements
  - For loops (with iterators)
  - While loops
  - Loop with break value

- [x] **5.5 Implement arithmetic behavior**
  - Default: trap on overflow
  - `+%`, `-%`, `*%`: wrapping
  - `+|`, `-|`, `*|`: saturating
  - Division by zero: trap

- [x] **5.6 Pattern matching runtime**
  - Literal matching
  - Variable binding
  - Struct destructuring
  - Enum variant matching
  - Guard evaluation
  - Or-patterns

- [x] **5.7 Interpreter tests**
  - Arithmetic operations
  - Control flow
  - Functions and closures
  - Pattern matching
  - Error handling

---

### Milestone 6: Builtins & Standard Library ✓

#### Tasks

- [x] **6.1 Core builtins** (in `src/interpreter.zig`)
  - `print(msg: string)` - print without newline
  - `println(msg: string)` - print with newline
  - `assert(condition: bool)` - trap if false
  - `assert_eq[T: Eq](a: T, b: T)` - trap if not equal
  - `panic(msg: string) -> !` - halt with message
  - `len(collection)` - array/tuple/string length
  - `type_of(value)` - runtime type name

- [x] **6.2 Type conversion builtins**
  - `.as[T]` - safe widening conversion
  - `.to[T]` - checked narrowing (trap on overflow)
  - `.trunc[T]` - truncating conversion
  - `.to_string()` - convert to string

- [x] **6.3 String operations**
  - String concatenation (`+` operator)
  - String methods: len, is_empty, contains, starts_with, ends_with
  - String methods: trim, to_uppercase, to_lowercase, chars, bytes

- [x] **6.4 Collection builtins**
  - Array/slice length (`.len()`)
  - Array methods: is_empty, first, last, get, contains
  - Basic iteration (for loops)

- [x] **6.5 Option/Result operations**
  - `?` propagation operator
  - `??` null coalescing
  - `!` unwrap (trap on None/Err)
  - Methods: is_some, is_none, unwrap, unwrap_or, expect

- [x] **6.6 Standard library stubs** (`std/`)
  - `std/core.kl` - prelude (auto-imported)
  - `std/io.kl` - basic I/O
  - `std/string.kl` - string utilities

- [x] **6.7 Integration tests**
  - Hello World
  - FizzBuzz
  - Fibonacci

---

## File Structure

```
klar/
├── build.zig
├── build.zig.zon
├── src/
│   ├── main.zig          # CLI entry point
│   ├── token.zig         # Token types
│   ├── lexer.zig         # Tokenizer
│   ├── ast.zig           # AST node definitions
│   ├── parser.zig        # Recursive descent parser
│   ├── types.zig         # Type representations
│   ├── checker.zig       # Type checker
│   ├── interpreter.zig   # Tree-walking interpreter
│   ├── values.zig        # Runtime values
│   ├── builtins.zig      # Built-in functions
│   └── errors.zig        # Error types and reporting
├── std/
│   ├── core.kl           # Core types (auto-imported)
│   ├── io.kl             # I/O operations
│   └── string.kl         # String utilities
├── examples/
│   ├── hello.kl
│   ├── fizzbuzz.kl
│   └── fibonacci.kl
└── test/
    ├── lexer_test.zig
    ├── parser_test.zig
    ├── checker_test.zig
    └── interpreter_test.zig
```

---

## Implementation Order

1. **Project setup** - build.zig, directory structure
2. **Tokens** - complete token.zig
3. **Lexer** - complete lexer.zig with all token types
4. **AST** - define all node types
5. **Expression parser** - Pratt parser for expressions
6. **Statement parser** - statements and declarations
7. **Type representation** - types.zig
8. **Type checker** - basic type checking
9. **Values** - runtime value representation
10. **Interpreter** - expression evaluation
11. **Interpreter** - statement execution
12. **Builtins** - core functions
13. **Integration** - run example programs

---

## Success Criteria for Phase 1

- [x] Can tokenize any valid Klar source file
- [x] Can parse expressions with correct precedence
- [x] Can parse all statement and declaration types
- [x] Type checker catches basic type errors
- [x] Can run Hello World: `println("Hello, Klar!")`
- [x] Can run FizzBuzz with match expressions
- [x] Can run recursive Fibonacci
- [x] All arithmetic operators work (including wrapping/saturating)
- [x] Pattern matching works in match expressions
- [x] Functions with parameters and return values work
- [x] Basic structs and enums work

**All Phase 1 criteria met!**

---

## Current Status

**Milestone 1**: Complete ✓
- [x] Project structure created (build.zig, build.zig.zon, src/)
- [x] token.zig implemented with all token types
- [x] lexer.zig implemented with full lexer and tests
- [x] main.zig implemented with CLI (run, tokenize, parse, help, version)
- [x] All tests passing (26 tests)

**Milestone 2**: Complete ✓
- [x] ast.zig - Complete AST node definitions for:
  - Expressions: Literal, Identifier, Binary, Unary, Postfix, Call, Index, Field, MethodCall, IfExpr, MatchExpr, Block, Closure, Range, StructLiteral, ArrayLiteral, TupleLiteral, TypeCast, Grouped
  - Patterns: Wildcard, PatternLiteral, Binding, VariantPattern, StructPattern, TuplePattern, OrPattern, GuardedPattern
  - Statements: LetDecl, VarDecl, Assignment, ExprStmt, ReturnStmt, BreakStmt, ContinueStmt, ForLoop, WhileLoop, LoopStmt
  - Declarations: FunctionDecl, StructDecl, EnumDecl, TraitDecl, ImplDecl, TypeAlias, ConstDecl, ImportDecl, ModuleDecl
  - Types: NamedType, ArrayType, SliceType, TupleType, OptionalType, ResultType, FunctionType, ReferenceType, GenericApply
- [x] parser.zig - Pratt parser with:
  - Full precedence support (15 levels from spec)
  - All arithmetic operators (+, -, *, /, %, +%, -%, *%, +|, -|, *|)
  - Comparison operators (==, !=, <, >, <=, >=, is)
  - Logical operators (and, or, not)
  - Bitwise operators (&, |, ^, ~, <<, >>)
  - Range operators (.., ..=)
  - Assignment operators (=, +=, -=, *=, /=, %=)
  - Null coalescing (??)
  - Postfix operators (?, !)
  - Function calls, method calls, field access, indexing
  - If expressions with else branches
  - Match expressions with pattern matching
  - Block expressions
  - Closures (|args| body with optional types)
  - Array and tuple literals
  - Type parsing (named, optional, reference, slice, function, generic, Self)
- [x] Statement parsing (let, var, for, while, loop, return, break, continue)
- [x] Declaration parsing (fn, struct, enum, trait, impl, type, const, import, module)
- [x] Module parsing (complete file parsing with module declaration, imports, declarations)
- [x] 52 parser unit tests passing

**Milestone 4**: Complete ✓
- [x] types.zig - Type representation:
  - Primitive types: i8-i128, u8-u128, isize, usize, f32, f64, bool, char, string
  - Composite types: Array, Slice, Tuple, Optional, Result, Function, Reference
  - User-defined types: Struct, Enum, Trait
  - Generic types: TypeVar, AppliedType
  - TypeBuilder for arena-based type construction
  - Type equality and widening checks
- [x] checker.zig - Type checker:
  - Symbol table with scope management (global, function, block, loop)
  - Type environment for type variable substitution
  - Expression type checking (literals, identifiers, binary ops, calls, etc.)
  - Statement type checking (let, var, assignment, control flow)
  - Declaration type checking (functions, structs, enums)
  - Pattern matching type checking
  - Built-in types and functions (print, println, assert)
  - Error reporting with source locations
- [x] CLI 'check' command for type checking files
- [x] 62 total tests passing

**Milestone 5**: Complete ✓
- [x] values.zig - Runtime value representation:
  - Integer values with type info (i8-i128, u8-u128)
  - Float values (f32, f64)
  - Bool, char, string values
  - Composite values: Array, Tuple, Struct, Enum
  - Optional/Result wrappers
  - References
  - Callable values: Function, Closure, Builtin
  - Value equality, cloning, formatting
- [x] interpreter.zig - Tree-walking interpreter:
  - Environment with scope management
  - Expression evaluation (all expression types)
  - Statement execution (let, var, assignment, control flow)
  - Function calls (user functions and closures)
  - Pattern matching runtime
  - Arithmetic with overflow behavior (trap, wrap, saturate)
  - Built-in functions: print, println, assert, assert_eq
- [x] CLI 'run' command executes Klar programs
- [x] Can run Hello World, Fibonacci, and other examples
- [x] 78 total tests passing

**Milestone 6**: Complete ✓
- [x] Core builtins:
  - [x] panic(msg: string) -> ! (halt with message)
  - [x] len(collection) -> usize (array, tuple, string length)
  - [x] type_of(value) -> string (runtime type name)
- [x] Type conversion methods:
  - [x] .as[T] - safe widening conversion
  - [x] .to[T] - checked narrowing (trap on overflow)
  - [x] .trunc[T] - truncating conversion
- [x] String operations:
  - [x] .to_string() on all types
  - [x] String methods: len, is_empty, contains, starts_with, ends_with, trim
  - [x] String methods: to_uppercase, to_lowercase, chars, bytes
- [x] Array/collection methods:
  - [x] len, is_empty, first, last, get, contains
- [x] Integer methods:
  - [x] abs, min, max
- [x] Optional methods:
  - [x] is_some, is_none, unwrap, unwrap_or, expect
- [x] Standard library stubs:
  - [x] std/core.kl - Core types and traits
  - [x] std/io.kl - Basic I/O operations
  - [x] std/string.kl - String utilities
- [x] Integration tests:
  - [x] Hello World works
  - [x] FizzBuzz works (examples/fizzbuzz.kl)
  - [x] Fibonacci works
- [x] Fixed assignment expression evaluation in binary operations
- [x] Type checker recognizes built-in methods
- [x] Memory management: arena allocator for runtime strings (no leaks)

**Phase 1 Complete!** ✓

Ready for Phase 2 (Bytecode VM) or additional language features.

---

## Notes

### Phase 1 Lessons Learned
- Pratt parsing works well for operator precedence (15 levels implemented cleanly)
- Arena allocator pattern is essential for runtime string allocations to avoid leaks
- Assignment expressions need special handling in binary operator evaluation
- Type checker and interpreter can share AST nodes directly (no IR needed for Phase 1)
- Built-in methods on types (like `.to_string()`) are cleaner than standalone functions

### Architecture Decisions
- Tree-walking interpreter is sufficient for language validation
- Builtins integrated directly in interpreter (no separate builtins.zig needed)
- Standard library stubs in Klar itself (std/*.kl) for future native implementation

### Phase 2: Bytecode VM (Planned)

**Goal**: Practical performance for real programs.

- Bytecode instruction set design
- Compiler from AST to bytecode
- Stack-based or register-based VM
- Garbage collection strategy
- Consider adding:
  - ~~String interpolation (`"value: {x}"`)~~ ✓ Implemented
  - More collection methods (map, filter, reduce)
  - Import/module system implementation
  - REPL mode

---

### Phase 3: Native Compiler (Planned)

**Goal**: Production-ready performance via native code generation.

- Backend options: Cranelift (faster compile) or LLVM (better optimization)
- Full standard library with OS bindings
- FFI/C interop for system calls
- Debug info generation
- Cross-compilation support

---

### Phase 4: Self-Hosting (Planned)

**Goal**: Write the Klar compiler in Klar itself.

#### Prerequisites

| Feature | Required For | Phase |
|---------|--------------|-------|
| File I/O | Reading source files | 2-3 |
| HashMap | Symbol tables, scopes | 2-3 |
| ArrayList/Vec | AST nodes, tokens | 2-3 |
| String manipulation | Lexer, error messages | ✓ |
| Pattern matching | AST traversal | ✓ |
| Traits/generics | Visitor patterns | ✓ |
| Memory management | Compiler data structures | 3 |
| Native codegen | Producing executables | 3 |

#### Bootstrap Strategy

```
Step 1: Implement standard library
        └── Collections (HashMap, Vec, Set)
        └── File I/O (read, write, path handling)
        └── String utilities (StringBuilder, formatting)

Step 2: Write Klar compiler in Klar (klar-in-klar)
        └── Lexer (port from Zig)
        └── Parser (port from Zig)
        └── Type checker (port from Zig)
        └── Code generator (new, targets same backend)

Step 3: Bootstrap
        └── Compile klar-in-klar using Zig-based compiler
        └── Produces: klar1 executable

Step 4: Self-compile
        └── Compile klar-in-klar using klar1
        └── Produces: klar2 executable

Step 5: Verify
        └── klar1 and klar2 should produce identical output
        └── Run full test suite with klar2
```

#### Milestones

- [ ] **4.1** Standard library: collections (HashMap, Vec, Set)
- [ ] **4.2** Standard library: file I/O
- [ ] **4.3** Standard library: string builder
- [ ] **4.4** Port lexer to Klar
- [ ] **4.5** Port parser to Klar
- [ ] **4.6** Port type checker to Klar
- [ ] **4.7** Implement code generator in Klar
- [ ] **4.8** First successful bootstrap (klar1)
- [ ] **4.9** Self-compilation (klar2)
- [ ] **4.10** Bootstrap verification (klar1 == klar2 output)

#### Benefits of Self-Hosting

1. **Dogfooding** — Find language pain points by using it
2. **Single language** — Contributors only need to know Klar
3. **Proof of capability** — Demonstrates Klar can build real software
4. **Faster iteration** — Compiler improvements benefit themselves
