# Klar Reference Apps

Demo programs that exercise and showcase Klar language features.

## Programs

### Mandelbrot Set (`mandelbrot.kl`)

ASCII Mandelbrot set renderer mapping the complex plane to terminal characters.

**Features demonstrated:**
- Type casts with `.as[f64]` syntax
- Nested `while` loops with `break`
- Floating-point arithmetic

### Collatz Conjecture (`collatz.kl`)

Visualizes the 3n+1 problem: if even divide by 2, if odd multiply by 3 and add 1.

**Features demonstrated:**
- While loops with conditions
- If/else chains
- Integer arithmetic and variable mutation

### Fibonacci Sequence (`fibonacci.kl`)

Recursive Fibonacci with visual bar chart output.

**Features demonstrated:**
- Recursive functions
- Functions with parameters and return values
- Expression-based returns

### Array Demo (`array_demo.kl`)

Demonstrates array literals, indexing, and iteration.

**Features demonstrated:**
- Array literals: `[5, 2, 8, 1, 9]`
- Array indexing: `arr[i]`
- Iteration via while loop + index

### Sort Visualization (`sort_viz.kl`)

Bubble sort with step-by-step visualization.

**Note:** Requires array mutation (`arr[i] = x`) which is not yet implemented in native compilation.

## Running

```bash
# Bytecode VM
./zig-out/bin/klar run examples/apps/mandelbrot.kl

# Native compilation
./zig-out/bin/klar build examples/apps/mandelbrot.kl -o mandelbrot
./mandelbrot
```

## Native Compilation Status

| App | VM | Native |
|-----|----|----|
| mandelbrot.kl | Yes | Yes |
| collatz.kl | Yes | Yes |
| fibonacci.kl | Yes | Yes |
| array_demo.kl | Yes | Yes |
| sort_viz.kl | Yes | No (needs array mutation) |
