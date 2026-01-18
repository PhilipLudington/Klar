# Klar Build

**USE THIS SKILL** when building Klar executables. Always use this instead of manual `./zig-out/bin/klar build` bash commands when:
- Creating a persistent executable (`-o output`)
- Generating LLVM IR or assembly for debugging (`--emit-llvm`, `--emit-asm`)
- Building optimized or debug versions (`-O2`, `-g`)

For quick testing without persistent output, prefer `/klar-test` or `/klar-run` instead.

Compile a Klar program to a native executable.

## Arguments

- `$ARGUMENTS` - Required: path to .kl file and build options

## Instructions

Use `klar build` when you need a persistent executable or special output:

```bash
./zig-out/bin/klar build <file.kl> -o <output> [flags]
```

### Required Flags

- `-o <output>` - Output path for the executable (required)

### Optional Flags

- `-O2` - Enable optimizations
- `-g` - Include debug symbols
- `--emit-llvm` - Output LLVM IR (.ll file) for debugging
- `--emit-asm` - Output assembly (.s file)
- `--emit-ir` - Output internal IR representation

### Examples

```bash
# Basic build
./zig-out/bin/klar build program.kl -o program

# Optimized release build
./zig-out/bin/klar build program.kl -o program -O2

# Debug build with symbols
./zig-out/bin/klar build program.kl -o program -g

# Emit LLVM IR for inspection
./zig-out/bin/klar build program.kl -o program --emit-llvm
```

### When to Use

Use `klar build` instead of `klar run` when you need:
- A persistent executable to run multiple times
- To inspect generated LLVM IR or assembly
- Debug symbols for debugging tools
- Optimized builds for benchmarking

For quick testing, prefer `/klar-test` or `/klar-run` instead.

## Examples

```
/klar-build src/main.kl -o myprogram
/klar-build benchmark.kl -o bench -O2
/klar-build debug.kl -o debug -g --emit-llvm
```
