# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Klar is an **AI-native application programming language** designed for AI code generation. The core philosophy is **"No ambiguity. No surprises."**

### Design Principles

1. **Parseable at a glance** — Syntax should be understandable without surrounding context. Every construct is self-describing. This benefits both humans reading code and AI generating it.
   - `[i32; 3]` not `i32[3]` — array type is self-contained, no lookahead needed
   - `and`, `or`, `not` not `&&`, `||`, `!` — keywords over symbols
   - Explicit type annotations — `let x: i32 = 5` not `let x = 5`

2. **No undefined behavior** — Every operation has defined semantics.
   - Explicit overflow: `+%` (wrapping), `+|` (saturating)
   - Bounds checking on array access
   - No null pointers (use `?T` optionals)

3. **No implicit conversions** — Type changes must be explicit.
   - `.as[T]` for safe conversions
   - `.to[T]` for fallible conversions
   - `.trunc[T]` for truncating conversions

4. **Ownership without complexity** — Memory safety via ownership, but simpler than Rust.
   - No lifetime annotations
   - Reference counting where needed (`Rc[T]`, `Arc[T]`)

Klar targets application-level programming (like C#/Go), not bare-metal systems (like C/Rust/Zig). This is why `.len()` returns `i32` for ergonomic loop counter usage.

## Build Commands

```bash
# Build the compiler (ALWAYS use wrapper script)
./build.sh

# Run ALL tests (ALWAYS use this for GitStat integration)
./run-tests.sh           # Runs all test suites (unit, native, app, module)

# Run individual test suites (in scripts/ directory)
./scripts/run-unit-tests.sh     # Zig unit tests
./scripts/run-native-tests.sh   # Native compilation tests (test/native/)
./scripts/run-app-tests.sh      # Reference application tests
./scripts/run-module-tests.sh   # Module system tests
./scripts/run-benchmarks.sh     # VM vs native benchmarks (not included in run-tests.sh)

# DO NOT run `zig build` or `zig build test` directly
```

## Klar Command Rules

**NEVER** use `./zig-out/bin/klar` directly in Bash commands. Always use the corresponding skill:

| Action | Use Skill |
|--------|-----------|
| Run a .kl file | `/klar-run` |
| Test a .kl file | `/klar-test` |
| Build an executable | `/klar-build` |
| Type-check a file | `/klar-check` |

If you find yourself typing `./zig-out/bin/klar` in a Bash command, **STOP** and use the skill instead.

## Running Klar Programs

```bash
# Run with bytecode VM (default)
./zig-out/bin/klar run program.kl

# Run with tree-walking interpreter
./zig-out/bin/klar run program.kl --interpret

# Compile to native executable
./zig-out/bin/klar build program.kl -o program
./zig-out/bin/klar build program.kl -o program -O2      # Optimized
./zig-out/bin/klar build program.kl -o program -g       # Debug info

# Test a single file (useful for debugging)
./zig-out/bin/klar build test/native/hello.kl -o /tmp/test && /tmp/test

# Debug output
./zig-out/bin/klar build program.kl --emit-llvm         # Output .ll file
./zig-out/bin/klar build program.kl --emit-asm          # Output .s file
./zig-out/bin/klar build program.kl --emit-ir           # Output internal IR
```

## Interactive REPL

The REPL (Read-Eval-Print Loop) enables interactive code exploration using the interpreter backend:

```bash
# Start the REPL
./zig-out/bin/klar repl
```

### REPL Commands

| Command | Description |
|---------|-------------|
| `:help` | Show available commands |
| `:type <expr>` | Show the type of an expression without evaluating |
| `:list` | Show all current bindings (variables, functions, types) |
| `:load <file>` | Load definitions from a Klar file |
| `:reset` | Clear all bindings and start fresh |
| `:quit` or `:q` | Exit the REPL |

### REPL Usage Examples

```
klar> let x: i32 = 42
klar> x + 10
52
klar> fn double(n: i32) -> i32 { return n * 2 }
klar> double(x)
84
klar> :type double(x)
i32
klar> :list
Variables:
  x: i32
Functions:
  double: fn(i32) -> i32
```

**Notes:**
- State persists across inputs (variables, functions, structs remain defined)
- Uses the interpreter backend (not VM or native compilation)
- Errors don't crash the session - you can continue after errors
- Multi-file imports not yet supported in REPL

## Compiler Architecture

The compiler is a multi-stage pipeline with three execution backends:

```
Source (.kl) → Lexer → Parser → AST → Type Checker → [Backend]
                                            ↓
              ┌─────────────────────────────┴─────────────────────────────┐
              │                             │                             │
         Interpreter              Bytecode Compiler              IR Builder
         (--interpret)               (default)                      ↓
              │                         │                      LLVM Codegen
              ↓                         ↓                           ↓
           Output                      VM                    Native Binary
```

### Key Source Files

| File | Purpose | Lines |
|------|---------|-------|
| `src/main.zig` | CLI entry point, command routing | 1,650 |
| `src/lexer.zig` | Tokenization | 595 |
| `src/parser.zig` | Recursive descent parser with generics/traits | 2,600+ |
| `src/checker.zig` | Type checking, trait resolution, monomorphization | **5,700+** |
| `src/codegen/emit.zig` | LLVM IR generation | **11,000+** |
| `src/vm.zig` | Bytecode virtual machine | 2,570 |
| `src/compiler.zig` | AST to bytecode compiler | 2,100 |
| `src/interpreter.zig` | Tree-walking interpreter | 2,500 |
| `src/types.zig` | Type representation and operations | 975 |

### Module Organization

- **Frontend**: `lexer.zig`, `parser.zig`, `ast.zig`, `token.zig`
- **Type System**: `checker.zig`, `types.zig`
- **Ownership**: `src/ownership/` - borrow checking, drop insertion, state tracking
- **Optimization**: `src/opt/` - constant folding, DCE, simplification
- **IR**: `src/ir/` - intermediate representation for native codegen
- **Codegen**: `src/codegen/` - LLVM backend
- **Runtime**: `src/runtime/` - Rc, Arc, allocator definitions

## Current Implementation Status

**Phase 4: Language Completion** (in progress) - See `PLAN.md` for full roadmap.

| Feature | Status |
|---------|--------|
| Generic functions/structs/enums | ✅ Complete |
| Monomorphization | ✅ Complete |
| Trait definitions/implementations | ✅ Complete |
| Trait bounds on generics | ✅ Complete |
| Trait inheritance | ✅ Complete |
| Builtin traits (Eq, Ordered, Clone, Drop) | ✅ Complete |
| Associated types | ❌ Not started |
| Module system (imports) | ❌ Not started |
| Standard library | ❌ Not started |

**Note:** `Option[T]` and `Result[T, E]` are built-in generic enums handled by the compiler, not yet user-definable types.

## Language Syntax Quick Reference

```klar
// Variables (explicit types required)
let x: i32 = 42         // immutable
let y: i64 = 100        // all types must be declared
var counter: i32 = 0    // mutable

// Functions (always use explicit return)
fn add(a: i32, b: i32) -> i32 {
    return a + b
}

fn max[T: Ordered](a: T, b: T) -> T {
    if a > b {
        return a
    }
    return b
}

// Closures (explicit types and return required)
let double: fn(i32) -> i32 = |x: i32| -> i32 { return x * 2 }

// Structs and methods
struct Point { x: f64, y: f64 }
impl Point {
    fn distance(self) -> f64 {
        return sqrt(self.x * self.x + self.y * self.y)
    }
}

// Enums with data
enum Option[T] { Some(T), None }
enum Result[T, E] { Ok(T), Err(E) }

// Traits
trait Drawable { fn draw(self) }
impl Circle: Drawable { fn draw(self) { ... } }

// Control flow (statement-based, not expressions)
var max: i32
if a > b {
    max = a
} else {
    max = b
}

for i: i32 in 0..10 { ... }

var result: string
match value {
    Pattern => { result = "matched" }
    _ => { result = "default" }
}

// Error handling
let maybe: ?i32 = Some(42)
let value: i32 = maybe ?? 0          // default if None

fn read_config() -> Result[Config, Error] {
    let content: string = read_file(path)?   // propagate error
    return Ok(parse(content))
}
```

## Development Guidelines

CarbideZig standards apply - see `carbide/CARBIDE.md` and `carbide/STANDARDS.md`.

### When Adding Language Features

1. **Parser** (`parser.zig`): Add AST nodes and parsing rules
2. **AST** (`ast.zig`): Define node structures
3. **Checker** (`checker.zig`): Implement type checking and validation
4. **All three backends**: Update interpreter, bytecode compiler, and native codegen

### When Debugging

- **Use `klar run` for quick testing** - Don't build to a temp file and execute separately. Use `./zig-out/bin/klar run file.kl` instead of `./zig-out/bin/klar build file.kl -o /tmp/test && /tmp/test`
- Use `--interpret` flag with `klar run` to use the tree-walking interpreter instead of the bytecode VM
- Use `--emit-llvm` to inspect generated LLVM IR
- Use `--emit-ir` to inspect internal IR before LLVM
- The VM has `--debug` flag for bytecode tracing
- Tests in `test/native/` cover most language features

### GitStat Integration

Wrapper scripts write results to JSON files read by GitStat:
- `.build-results.json` - Build status
- `.test-results.json` - Unit test results
- `.native-test-results.json` - Native compilation test results

## Scratch Directory

The `scratch/` directory is your sandbox. You have full freedom to:
- Create any files or directories
- Modify or delete anything
- Run experiments and tests
- Store working notes and analysis

Use it liberally for:
- Trying out Klar code implementations before proposing changes
- Creating test fixtures and sample `.kl` files
- Writing analysis and comparisons of approaches
- Any temporary work that doesn't belong in the main codebase

The scratch directory is gitignored and can be cleaned at any time.
