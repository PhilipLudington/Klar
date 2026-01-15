# Klar Implementation Plan

Implementation plan for the Klar programming language, based on DESIGN.md specification.

---

## Overview

**Goal**: Build a working Klar interpreter in Zig, following a phased approach.

**Strategy**:
1. Phase 1: Tree-walking interpreter (validate language design)
2. Phase 2: Bytecode VM (practical performance)
3. Phase 3: Native compiler (Cranelift/LLVM)

This plan focuses on **Phase 1** - getting a working interpreter that can run basic Klar programs.

---

## Phase 1: Tree-Walking Interpreter

### Milestone 1: Project Setup & Lexer

**Status**: Partially complete (starter code in DESIGN.md)

#### Tasks

- [ ] **1.1 Initialize project structure**
  - Create `build.zig` and `build.zig.zon`
  - Set up `src/` directory with module files
  - Configure test infrastructure

- [ ] **1.2 Implement token types** (`src/token.zig`)
  - Define all token kinds from spec (keywords, operators, literals)
  - Add source location tracking (line, column, span)
  - Implement `lexeme()` helper for debugging

- [ ] **1.3 Implement lexer** (`src/lexer.zig`)
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

- [ ] **1.4 Lexer tests**
  - Test all token types
  - Test edge cases (unterminated strings, invalid escapes)
  - Test source locations are accurate

- [ ] **1.5 Basic CLI** (`src/main.zig`)
  - `klar run <file>` - tokenize and display
  - `klar help` - usage information
  - `klar version` - version string
  - File reading with error handling

---

### Milestone 2: AST & Expression Parser

#### Tasks

- [ ] **2.1 Define AST nodes** (`src/ast.zig`)

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
  - `Closure` - `|args| body` or `fn(args) body`
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

- [ ] **2.2 Implement Pratt parser foundation** (`src/parser.zig`)
  - Token stream management (peek, advance, expect)
  - Error reporting with source locations
  - Synchronization for error recovery
  - Precedence table from spec

- [ ] **2.3 Parse literals**
  - Integer parsing (handle bases, underscores, suffixes)
  - Float parsing (handle scientific, suffixes)
  - String parsing (handle escapes, raw strings)
  - Character parsing (handle unicode escapes)
  - Boolean literals (`true`, `false`)

- [ ] **2.4 Parse operators by precedence**

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

- [ ] **2.5 Parse complex expressions**
  - If expressions: `if cond { a } else { b }`
  - Match expressions: `value match { ... }`
  - Block expressions: `{ stmt; stmt; expr }`
  - Closures: `|x, y| x + y`
  - Struct literals: `Point { x: 1, y: 2 }`
  - Array/tuple literals

- [ ] **2.6 Expression parser tests**
  - Precedence correctness
  - Associativity correctness
  - All literal types
  - Complex nested expressions

---

### Milestone 3: Statement & Declaration Parser

#### Tasks

- [ ] **3.1 Define statement AST nodes**

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

- [ ] **3.2 Parse variable declarations**
  - `let` (immutable) with optional type annotation
  - `var` (mutable) with optional type annotation
  - Type inference marker

- [ ] **3.3 Parse control flow statements**
  - `for item in iterable { body }`
  - `for (index, item) in iter.enumerate() { body }`
  - `while condition { body }`
  - `loop { body }`
  - `break` and `break value`
  - `continue`
  - `return` and `return value`

- [ ] **3.4 Parse function declarations**
  - Function signature: `fn name(params) -> ReturnType`
  - Generic parameters: `fn name[T, U](params)`
  - Trait bounds: `fn name[T: Trait](params)`
  - Where clauses: `where T: A + B`
  - Single-expression shorthand: `fn double(x: i32) -> i32 = x * 2`
  - Async functions: `async fn fetch()`
  - Public visibility: `pub fn`

- [ ] **3.5 Parse type declarations**
  - Structs with fields and visibility
  - Enums with variants (unit, tuple, struct)
  - Trait declarations with method signatures
  - Impl blocks (inherent and trait)
  - Type aliases

- [ ] **3.6 Parse imports and modules**
  - `import std.collections`
  - `import std.collections.{ List, Map }`
  - `import std.collections.*`
  - `import path as alias`
  - `module name.subname`

- [ ] **3.7 Statement/declaration tests**
  - All statement types
  - All declaration types
  - Visibility modifiers
  - Generic parameters

---

### Milestone 4: Type System

#### Tasks

- [ ] **4.1 Define type representation** (`src/types.zig`)

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

- [ ] **4.2 Implement type checker** (`src/checker.zig`)
  - Symbol table / scope management
  - Type environment
  - Type unification
  - Error accumulation

- [ ] **4.3 Type inference**
  - Infer literal types
  - Infer from context (let binding)
  - Infer generic parameters
  - Bidirectional type checking

- [ ] **4.4 Type checking rules**
  - No implicit numeric conversions
  - Require `.as[T]` for widening
  - Require `.to[T]` for narrowing (with trap)
  - Option type for nullable (`?T`)
  - Reference validity (borrows)

- [ ] **4.5 Trait resolution**
  - Check trait implementations exist
  - Resolve method calls through traits
  - Check trait bounds on generics

- [ ] **4.6 Type checker tests**
  - Primitive type checking
  - Struct/enum type checking
  - Generic instantiation
  - Error detection (type mismatches)

---

### Milestone 5: Interpreter

#### Tasks

- [ ] **5.1 Define runtime values** (`src/values.zig`)
  - Integer values (all sizes)
  - Float values
  - Boolean, char, string
  - Struct instances
  - Enum instances
  - Arrays, slices, tuples
  - Closures (captured environment)
  - References
  - Option/Result wrappers

- [ ] **5.2 Implement environment**
  - Variable storage
  - Scope chain
  - Function definitions
  - Type definitions

- [ ] **5.3 Evaluate expressions** (`src/interpreter.zig`)
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

- [ ] **5.4 Execute statements**
  - Let/var declarations
  - Assignments
  - Return statements
  - For loops (with iterators)
  - While loops
  - Loop with break value

- [ ] **5.5 Implement arithmetic behavior**
  - Default: trap on overflow
  - `+%`, `-%`, `*%`: wrapping
  - `+|`, `-|`, `*|`: saturating
  - Division by zero: trap

- [ ] **5.6 Pattern matching runtime**
  - Literal matching
  - Variable binding
  - Struct destructuring
  - Enum variant matching
  - Guard evaluation
  - Or-patterns

- [ ] **5.7 Interpreter tests**
  - Arithmetic operations
  - Control flow
  - Functions and closures
  - Pattern matching
  - Error handling

---

### Milestone 6: Builtins & Standard Library

#### Tasks

- [ ] **6.1 Core builtins** (`src/builtins.zig`)
  - `print(msg: string)` - print without newline
  - `println(msg: string)` - print with newline
  - `assert(condition: bool)` - trap if false
  - `assert_eq[T: Eq](a: T, b: T)` - trap if not equal
  - `panic(msg: string) -> !` - halt with message

- [ ] **6.2 Type conversion builtins**
  - `.as[T]` - safe widening conversion
  - `.to[T]` - checked narrowing (trap on overflow)
  - `.trunc[T]` - truncating conversion
  - `.to_string()` - convert to string

- [ ] **6.3 String operations**
  - String concatenation
  - String interpolation `"value: {x}"`
  - Basic string methods

- [ ] **6.4 Collection builtins**
  - Array/slice length
  - Array/slice indexing
  - Basic iteration

- [ ] **6.5 Option/Result operations**
  - `?` propagation operator
  - `??` null coalescing
  - `!` unwrap (trap on None/Err)

- [ ] **6.6 Standard library stubs** (`std/`)
  - `std/core.kl` - prelude (auto-imported)
  - `std/io.kl` - basic I/O
  - `std/string.kl` - string utilities

- [ ] **6.7 Integration tests**
  - Hello World
  - FizzBuzz
  - Fibonacci
  - Basic data structures

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

- [ ] Can tokenize any valid Klar source file
- [ ] Can parse expressions with correct precedence
- [ ] Can parse all statement and declaration types
- [ ] Type checker catches basic type errors
- [ ] Can run Hello World: `println("Hello, Klar!")`
- [ ] Can run FizzBuzz with match expressions
- [ ] Can run recursive Fibonacci
- [ ] All arithmetic operators work (including wrapping/saturating)
- [ ] Pattern matching works in match expressions
- [ ] Functions with parameters and return values work
- [ ] Basic structs and enums work

---

## Current Status

**Milestone 1**: Complete ✓
- [x] Project structure created (build.zig, build.zig.zon, src/)
- [x] token.zig implemented with all token types
- [x] lexer.zig implemented with full lexer and tests
- [x] main.zig implemented with CLI (run, tokenize, parse, help, version)
- [x] All tests passing (26 tests)

**Milestone 2**: In Progress
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
  - Closures (|args| body and fn(args) body)
  - Array and tuple literals
  - Type parsing (named, optional, reference, slice, function, generic)
- [x] 14 parser unit tests passing
- [ ] Statement parsing (let, var, for, while, loop, return, break, continue)
- [ ] Declaration parsing (fn, struct, enum, trait, impl, import, module)

**Next Step**: Add statement and declaration parsing to complete Milestone 2, then begin Milestone 3 (Type Checker)

---

## Notes

- The lexer starter code handles most operators but may need refinement
- Parser will use recursive descent with Pratt parsing for expressions
- Type checker starts simple (no full inference) and grows
- Interpreter is tree-walking for simplicity; optimize later
- Focus on correctness first, then add features incrementally
