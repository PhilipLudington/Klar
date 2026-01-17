# Klar

> **"No ambiguity. No surprises."**

Klar is an application programming language designed for clarity, safety, and simplicity. It is optimized for AI code generation—specifically Claude Code—prioritizing unambiguous syntax and predictable semantics.

## Design Philosophy

### Why Klar?

Traditional languages present challenges for AI code generation:
- **C/C++**: Ambiguous syntax, undefined behavior, preprocessor complexity
- **Rust**: Steep learning curve, complex lifetime annotations, borrow checker friction
- **Go/C#**: Implicit behaviors, null references, verbose error handling

Klar takes a different approach: **maximize clarity, minimize surprises**.

### Core Principles

1. **Unambiguous syntax** — no context needed to parse
2. **No undefined behavior** — every operation has defined semantics
3. **Memory safe by default** — ownership without lifetime annotations
4. **Explicit over implicit** — no silent type conversions
5. **One obvious way** — minimize redundant syntax forms
6. **Readable operators** — `and`, `or`, `not` instead of `&&`, `||`, `!`

### Problems Solved

| C/C++ Problem | Klar Solution |
|---------------|---------------|
| Ambiguous syntax | Keyword-driven, context-free parsing |
| Preprocessor macros | `comptime` blocks |
| Undefined behavior | Defined semantics + explicit overflow operators |
| Null pointer danger | `?T` optional types |
| Memory unsafety | Ownership + borrows, no dangling references |
| Header/source split | Modules |
| Implicit conversions | Explicit `.as[T]`, `.to[T]`, `.trunc[T]` |
| Complex build systems | Convention-based builds |

## Quick Tour

### Hello World

```klar
fn main() {
    println("Hello, Klar!")
}
```

### Variables and Types

```klar
let x = 42              // immutable, type inferred (i32)
let y: i64 = 100        // explicit type
var counter = 0         // mutable

// No implicit conversions
let wide = x.as[i64]    // safe widening
let narrow = y.to[i32]  // checked narrowing (traps on overflow)
```

### String Interpolation

```klar
let name = "World"
let count = 42
println("Hello, {name}! Count: {count}")
```

### Control Flow

```klar
// If expressions (produce values)
let max = if a > b { a } else { b }

// Pattern matching
let result = match status {
    Status.Ok => "success"
    Status.Error(msg) => "failed: {msg}"
    _ => "unknown"
}

// Loops
for item in collection {
    process(item)
}

for i in 0..10 {
    println("{i}")
}

while condition {
    work()
}
```

### Functions

```klar
fn add(a: i32, b: i32) -> i32 {
    return a + b
}

fn greet(name: string) {
    println("Hello, {name}!")
}

// Generic functions
fn max[T: Ordered](a: T, b: T) -> T {
    if a > b {
        return a
    }
    return b
}
```

### Closures

```klar
let double = |x| x * 2
let add = |a: i32, b: i32| a + b
let process = |x: i32| -> i32 {
    let y = x * 2
    y + 1
}

// With capturing
let factor = 10
let scale = |x| x * factor
```

### Structs and Enums

```klar
struct Point {
    x: f64
    y: f64
}

enum Option[T] {
    Some(T)
    None
}

enum Message {
    Quit
    Move { x: i32, y: i32 }
    Write(string)
}
```

### Error Handling

```klar
// Optional types
let maybe: ?i32 = Some(42)
let value = maybe ?? 0          // default if None
let forced = maybe!             // trap if None

// Result types with propagation
fn read_config() -> Result[Config, Error] {
    let content = read_file(path)?   // propagate error
    let parsed = parse(content)?
    return Ok(parsed)
}
```

### Explicit Arithmetic

```klar
let a: i32 = 2_000_000_000
let b = a + a           // trap on overflow (default, safe)
let c = a +% a          // wrapping arithmetic
let d = a +| a          // saturating arithmetic
```

## Key Features

### For AI Code Generation

- **Context-free grammar**: Every construct is unambiguous without surrounding context
- **Keyword-driven**: `and`/`or`/`not` vs `&&`/`||`/`!` eliminates symbol confusion
- **Consistent patterns**: One way to do things, predictable structure
- **Explicit semantics**: No hidden behavior or implicit conversions

### For Safety

- **Ownership model**: Memory safety without garbage collection
- **No null**: Optional types (`?T`) for nullable values
- **Bounds checking**: Array access is checked by default
- **Defined overflow**: Choose trap, wrap, or saturate explicitly

### For Simplicity

- **No header files**: Single source of truth per module
- **No preprocessor**: `comptime` for compile-time computation
- **No function overloading**: Use generics instead
- **Minimal syntax**: Fewer ways to express the same thing

## Current Status

**Phase 4: Language Completion** — In Progress

| Phase | Status | Description |
|-------|--------|-------------|
| Phase 1: Tree-Walking Interpreter | ✅ Complete | Lexer, parser, type checker, interpreter |
| Phase 2: Bytecode VM | ✅ Complete | Bytecode compiler and virtual machine |
| Phase 3: Native Compiler | ✅ Complete | LLVM-based native code generation |
| Phase 4: Language Completion | 🚧 In Progress | Generics, traits, modules, standard library |

### Completed Features

- ✅ LLVM backend with optimization levels (-O0 to -O3)
- ✅ Ownership-based memory management (no GC)
- ✅ Rc/Arc reference counting with automatic drop
- ✅ Closures with capture analysis
- ✅ Optional and Result types
- ✅ Debug info generation (-g flag)
- ✅ Cross-compilation support (--target flag)
- ✅ 252x speedup over VM for compute-bound code
- ✅ Generic functions, structs, and enums with monomorphization
- ✅ Trait definitions, implementations, and bounds
- ✅ Builtin traits: Eq, Ordered, Clone, Drop

### In Progress

- 🚧 Associated types in traits
- 🚧 Module system and imports
- 🚧 Standard library

See [PLAN.md](PLAN.md) for implementation details and roadmap.

## Building

Requires [Zig](https://ziglang.org/) 0.15+ and LLVM 17+.

```bash
# Install LLVM (macOS)
brew install llvm

# Build the compiler
./build.sh

# Run tests
./run-tests.sh         # Unit tests
./run-native-tests.sh  # Native compilation tests
./run-benchmarks.sh    # VM vs Native benchmarks
```

## Usage

### Running with the VM (Bytecode)

```bash
# Run a Klar program using the bytecode VM
./zig-out/bin/klar run program.kl
```

### Native Compilation

```bash
# Compile to native executable
./zig-out/bin/klar build program.kl -o program

# Run the compiled binary
./program

# With optimizations
./zig-out/bin/klar build program.kl -o program -O2

# With debug info (for lldb/gdb)
./zig-out/bin/klar build program.kl -o program -g

# Cross-compile for different architectures
./zig-out/bin/klar build program.kl --target x86_64-apple-macosx
./zig-out/bin/klar build program.kl --target aarch64-linux-gnu

# Emit LLVM IR or assembly
./zig-out/bin/klar build program.kl --emit-llvm  # Outputs .ll file
./zig-out/bin/klar build program.kl --emit-asm   # Outputs .s file
```

### Optimization Levels

| Flag | Description |
|------|-------------|
| `-O0` | No optimizations (default, fastest compile) |
| `-O1` | Basic optimizations (constant folding, DCE) |
| `-O2` | Standard optimizations (recommended for release) |
| `-O3` | Aggressive optimizations (may increase code size) |

## Performance

The native compiler provides significant speedups over the bytecode VM:

| Benchmark | VM Time | Native Time | Speedup |
|-----------|---------|-------------|---------|
| fib(35) | ~14s | 0.05s | **259x** |
| matrix | 0.03s | 0.02s | 1.6x |
| sort | 0.03s | 0.02s | 1.6x |

Run `./run-benchmarks.sh` to see current benchmark results.

## Examples

See the `examples/` directory:
- `hello.kl` — Hello World
- `fibonacci.kl` — Recursive Fibonacci
- `fizzbuzz.kl` — Classic FizzBuzz

See the `benchmarks/` directory:
- `fib.kl` — Recursive Fibonacci (compute-bound)
- `matrix.kl` — Matrix multiplication
- `sort.kl` — Sorting simulation
- `rc_stress.kl` — Reference counting stress test

See the `examples/apps/` directory for larger reference applications:
- `mandelbrot.kl` — ASCII Mandelbrot set renderer
- `collatz.kl` — Collatz conjecture visualizer
- `fibonacci.kl` — Fibonacci sequence with bar chart
- `array_demo.kl` — Array features demo
- `struct_demo.kl` — Structs and tuples
- `closure_demo.kl` — Closures and higher-order functions
- `optional_demo.kl` — Optional types (?T)
- `result_demo.kl` — Result types and error handling
- `rc_demo.kl` — Reference counting (Rc/Arc)

## File Extension

`.kl`

## License

MIT

---

*Klar is designed by Claude Code for Claude Code.*
