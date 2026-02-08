# Klar Reference Apps

Demo programs that exercise and showcase Klar language features.

## Programs

### Visual Demos

#### Mandelbrot Set (`mandelbrot.kl`)
ASCII Mandelbrot set renderer mapping the complex plane to terminal characters.

**Features:** Type casts (`.as[f64]`), nested while loops, break, floating-point arithmetic

#### Collatz Conjecture (`collatz.kl`)
Visualizes the 3n+1 problem with step-by-step output.

**Features:** While loops, if/else chains, integer arithmetic, variable mutation

#### Fibonacci Sequence (`fibonacci.kl`)
Recursive Fibonacci with visual bar chart output.

**Features:** Recursive functions, return values, expression-based returns

#### Array Demo (`array_demo.kl`)
Demonstrates array literals, indexing, and iteration.

**Features:** Array literals, indexing (`arr[i]`), while loop iteration

### Feature Demos

#### Struct Demo (`struct_demo.kl`)
Demonstrates structs and tuples with a geometry example.

**Features:** Struct definitions, struct literals, field access, tuples, tuple indexing

#### Closure Demo (`closure_demo.kl`)
Higher-order functions and closures with capturing.

**Features:** Closure literals, typed params, variable capture, functions as params

#### Optional Demo (`optional_demo.kl`)
Optional types for nullable values.

**Features:** `?T` type, implicit Some/None, force unwrap (`!`), null coalescing (`??`)

#### Result Demo (`result_demo.kl`)
Result types for error handling.

**Features:** `Ok()`, `Err()`, `.is_ok()`, unwrap (`!`), `.unwrap_err()`

#### Rc Demo (`rc_demo.kl`)
Reference counting for shared ownership.

**Features:** `Rc.new()`, `Arc.new()`, `.clone()`, automatic drop

### Real-World Examples

#### JSON Parser (`json_parser.kl`)
A complete JSON parser demonstrating Klar's advanced features.

**Features:** Recursive enums (`JsonValue`), generic collections (`List[T]`, `Map[K,V]`), 
Result type error handling, `?` operator for error propagation, pattern matching, 
character-level string processing

This example parses JSON primitives (null, booleans, numbers, strings), arrays, 
objects, and nested structures—showcasing how Klar handles real-world parsing tasks.

### Work in Progress

#### Sort Visualization (`sort_viz.kl`)
Bubble sort with step-by-step visualization.

**Note:** Requires array mutation (`arr[i] = x`) which is not yet implemented.

## Running

```bash
# Bytecode VM
./zig-out/bin/klar run examples/apps/mandelbrot.kl

# Native compilation
./zig-out/bin/klar build examples/apps/mandelbrot.kl -o mandelbrot
./mandelbrot
```

## Native Compilation Status

| App | VM | Native | Notes |
|-----|:--:|:------:|-------|
| mandelbrot.kl | ✓ | ✓ | |
| collatz.kl | ✓ | ✓ | |
| fibonacci.kl | ✓ | ✓ | |
| array_demo.kl | ✓ | ✓ | |
| struct_demo.kl | ✓ | ✓ | |
| closure_demo.kl | ✓ | ✓ | |
| optional_demo.kl | ✓ | ✓ | |
| result_demo.kl | ✓ | ✓ | |
| rc_demo.kl | ✓ | ✓ | |
| json_parser.kl | ✓ | ✓ | Recursive enums, generics |
| sort_viz.kl | ✓ | ✗ | Needs array mutation |
