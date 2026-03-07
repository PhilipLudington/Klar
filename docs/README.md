# Klar Programming Language

**No ambiguity. No surprises. The code explains itself.**

Klar is an AI-native application programming language designed for AI code generation. Every construct is self-describing, parseable at a glance, and has defined semantics.

## Quick Start

```klar
fn main() -> i32 {
    let message: string = "Hello, Klar!"
    println(message)
    return 0
}
```

Save as `hello.kl` and run:

```bash
klar run hello.kl
```

## Documentation

### Philosophy

- [Design Philosophy](PHILOSOPHY.md) - The three principles behind every design decision

### Getting Started

- [Installation](getting-started/installation.md) - Build the compiler from source
- [Hello World](getting-started/hello-world.md) - Your first Klar program
- [CLI Reference](getting-started/cli-reference.md) - Command-line interface
- [REPL Guide](getting-started/repl-guide.md) - Interactive development
- [AI Agent's Guide](AI-GUIDE.md) - Guide for AI agents generating Klar code

### Language Guide

- [Basics](language/basics.md) - Variables, types, expressions
- [Functions](language/functions.md) - Functions, closures, generics
- [Control Flow](language/control-flow.md) - if, match, for, while, loop
- [Structs](language/structs.md) - Struct definitions and methods
- [Enums](language/enums.md) - Enum definitions and pattern matching
- [Traits](language/traits.md) - Trait system
- [Generics](language/generics.md) - Generic types and functions
- [Error Handling](language/error-handling.md) - Optional, Result, and the `?` operator
- [Modules](language/modules.md) - Import system and visibility

### Type System

- [Primitives](types/primitives.md) - i32, f64, bool, char, string
- [Arrays](types/arrays.md) - Fixed-size arrays `[T; N]`
- [Tuples](types/tuples.md) - Tuple types `(T1, T2, ...)`
- [Optional](types/optional.md) - The `?T` optional type
- [Result](types/result.md) - The `Result#[T, E]` type
- [Collections](types/collections.md) - List, Map, Set, Range
- [Smart Pointers](types/smart-pointers.md) - Rc, Arc, Cell
- [I/O Types](types/io-types.md) - File, streams, readers/writers

### Memory Management

- [Ownership](memory/ownership.md) - Ownership model
- [References](memory/references.md) - `ref` and `inout` parameters
- [Reference Counting](memory/reference-counting.md) - Rc and Arc patterns

### Standard Library

- [Path Manipulation](stdlib/path.md) - `path_join`, `path_parent`, `path_extension`, `path_normalize`
- [Directory Walking](stdlib/dir.md) - `dir_list`, `dir_list_ext`, `dir_walk`, `dir_walk_ext`
- [File Utilities](stdlib/file.md) - `file_write`, `file_append`, `file_write_lines` *(planned)*

### Guides

- [WebAssembly](guides/wasm.md) - Compile to WebAssembly
- [Self-Hosting](guides/self-hosting.md) - Bootstrap architecture and compiler frontend port

### Advanced Topics

- [Operators](advanced/operators.md) - Complete operator reference
- [Comptime](advanced/comptime.md) - Compile-time programming
- [Builtin Traits](advanced/builtin-traits.md) - Eq, Ordered, Clone, Drop, etc.
- [FFI](advanced/ffi.md) - Foreign Function Interface for C interop

### Design

- [Meta Layer](design/meta-layer.md) - Self-describing code with `meta` annotations
- [DSPy Opportunities](design/dspy-opportunities.md) - DSPy-inspired language features
- [MoonBit Semantic Sampler](design/moonbit-semantic-sampler.md) - MoonBit-inspired design
- [Nanolang Inspiration](design/nanolang-inspiration.md) - Nanolang-inspired features

### Appendix

- [Builtins](appendix/builtins.md) - Built-in functions reference
- [Keywords](appendix/keywords.md) - Reserved keywords

## Design Philosophy

### Parseable at a Glance

Every construct is self-describing without surrounding context:

```klar
[i32; 3]       // Array type - self-contained, no lookahead needed
and, or, not   // Keywords over cryptic symbols
let x: i32 = 5 // Explicit type annotations
```

### No Undefined Behavior

Every operation has defined semantics:

```klar
let a: i32 = 100
let b: i32 = a +% 200  // Wrapping addition (explicit overflow handling)
let c: i32 = a +| 200  // Saturating addition (clamps at max)
```

### No Implicit Conversions

Type changes are always explicit:

```klar
let x: i64 = 42.as#[i64]     // Safe conversion
let y: ?i32 = "42".to#[i32]  // Fallible conversion
let z: i8 = x.trunc#[i8]     // Truncating conversion
```

### Ownership Without Complexity

Memory safety through ownership, but simpler than Rust:

```klar
let data: Rc#[Data] = Rc.new(Data { ... })  // Reference counted
let copy: Rc#[Data] = data.clone()          // Explicit clone
```

### C Interoperability (FFI)

Call C functions and use C types with explicit `unsafe` blocks:

```klar
extern { fn puts(s: CStr) -> i32 }

fn main() -> i32 {
    unsafe { puts("Hello from Klar FFI!".as_cstr()) }
    return 0
}
```

## Version

Current version: **0.4.0**

## License

See [LICENSE](../LICENSE) for details.
