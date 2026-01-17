# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Klar is a systems programming language designed for AI code generation. The core philosophy is **"No ambiguity. No surprises."** Key design principles:

- Context-free grammar with keyword-driven operators (`and`, `or`, `not` instead of `&&`, `||`, `!`)
- No undefined behavior - explicit overflow operators (`+%` wrapping, `+|` saturating)
- Ownership-based memory safety without lifetime annotations
- No implicit type conversions (requires `.as[]`, `.to[]`, `.trunc[]`)

Klar targets application-level programming (like C#/Go), not bare-metal systems (like C/Rust/Zig). This is why `.len()` returns `i32` for ergonomic loop counter usage.

## Build Commands

```bash
# Build the compiler (ALWAYS use wrapper script)
./build.sh

# Run tests (ALWAYS use wrapper scripts for GitStat integration)
./run-tests.sh           # Zig unit tests
./run-native-tests.sh    # Native compilation tests (77 tests in test/native/)
./run-benchmarks.sh      # VM vs native benchmarks

# DO NOT run `zig build` or `zig build test` directly
```

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

# Debug output
./zig-out/bin/klar build program.kl --emit-llvm         # Output .ll file
./zig-out/bin/klar build program.kl --emit-asm          # Output .s file
./zig-out/bin/klar build program.kl --emit-ir           # Output internal IR
```

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

**Phase 4: Language Completion** (in progress)

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

## Language Syntax Quick Reference

```klar
// Variables
let x = 42              // immutable, type inferred
let y: i64 = 100        // explicit type
var counter = 0         // mutable

// Functions
fn add(a: i32, b: i32) -> i32 { a + b }
fn max[T: Ordered](a: T, b: T) -> T { if a > b { a } else { b } }

// Structs and methods
struct Point { x: f64, y: f64 }
impl Point {
    fn distance(self) -> f64 { ... }
}

// Enums with data
enum Option[T] { Some(T), None }
enum Result[T, E] { Ok(T), Err(E) }

// Traits
trait Drawable { fn draw(self) }
impl Circle: Drawable { fn draw(self) { ... } }

// Control flow
let max = if a > b { a } else { b }
for i in 0..10 { ... }
match value { Pattern => result, _ => default }

// Error handling
let maybe: ?i32 = Some(42)
let value = maybe ?? 0          // default if None
let result = read_file(path)?   // propagate error
```

## Development Guidelines

CarbideZig standards apply - see `carbide/CARBIDE.md` and `carbide/STANDARDS.md`.

### When Adding Language Features

1. **Parser** (`parser.zig`): Add AST nodes and parsing rules
2. **AST** (`ast.zig`): Define node structures
3. **Checker** (`checker.zig`): Implement type checking and validation
4. **All three backends**: Update interpreter, bytecode compiler, and native codegen

### When Debugging

- Use `--emit-llvm` to inspect generated LLVM IR
- Use `--emit-ir` to inspect internal IR before LLVM
- The VM has `--debug` flag for bytecode tracing
- Tests in `test/native/` cover most language features

### GitStat Integration

Wrapper scripts write results to JSON files read by GitStat:
- `.build-results.json` - Build status
- `.test-results.json` - Unit test results
- `.native-test-results.json` - Native compilation test results
