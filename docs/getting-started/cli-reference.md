# CLI Reference

The Klar compiler provides several commands for running, building, and analyzing programs.

## Usage

```
klar <command> [options]
```

## Commands

### run

Run a Klar program. By default, compiles to native code and executes.

```bash
klar run <file.kl> [options] [-- program_args...]
```

**Options:**

| Option | Description |
|--------|-------------|
| `--vm` | Use bytecode VM instead of native compilation |
| `--interpret` | Use tree-walking interpreter |
| `--debug` | Enable instruction tracing (VM only) |

**Examples:**

```bash
# Run with native compilation (default, fastest)
klar run hello.kl

# Run with bytecode VM
klar run hello.kl --vm

# Run with interpreter (slowest, for debugging)
klar run hello.kl --interpret

# Pass arguments to the program
klar run hello.kl arg1 arg2

# Use -- to pass flags to the program
klar run hello.kl -- --verbose --count=5
```

### build

Compile a Klar program to a native executable.

```bash
klar build <file.kl> [options]
```

**Options:**

| Option | Description |
|--------|-------------|
| `-o <name>` | Output file name (default: `build/<basename>`) |
| `-O0` | No optimizations (default) |
| `-O1` | Basic optimizations (constant folding, DCE) |
| `-O2` | Standard optimizations |
| `-O3` | Aggressive optimizations |
| `-g` | Generate debug information (DWARF) |
| `-l <lib>` | Link with library (e.g., `-lm`, `-lcurl`) |
| `-L <path>` | Add library search path |
| `--emit-llvm` | Output LLVM IR (.ll file) |
| `--emit-asm` | Output assembly (.s file) |
| `--emit-ir` | Output Klar IR (.ir file) |
| `--target <triple>` | Cross-compile for target |
| `--verbose-opt` | Show optimization statistics |

**Examples:**

```bash
# Basic build
klar build hello.kl              # Creates build/hello

# Custom output path
klar build hello.kl -o myapp     # Creates myapp

# Optimized build
klar build hello.kl -O2          # Standard optimizations

# Debug build
klar build hello.kl -g           # With debug symbols

# Link with external library
klar build math.kl -lm           # Link with libm

# Multiple libraries with search path
klar build app.kl -L/opt/lib -lmylib -lcurl

# Inspect generated code
klar build hello.kl --emit-llvm  # Creates hello.ll
klar build hello.kl --emit-asm   # Creates hello.s

# Cross-compilation
klar build hello.kl --target x86_64-linux-gnu
```

**Target Triples:**

| Triple | Description |
|--------|-------------|
| `x86_64-linux-gnu` | Linux on x86_64 |
| `aarch64-linux-gnu` | Linux on ARM64 |
| `x86_64-apple-macosx` | macOS on Intel |
| `arm64-apple-macosx` | macOS on Apple Silicon |
| `x86_64-windows-msvc` | Windows on x86_64 |

### check

Type-check a file without compiling or running.

```bash
klar check <file.kl> [options]
```

**Options:**

| Option | Description |
|--------|-------------|
| `--dump-ownership` | Show ownership analysis output |

**Examples:**

```bash
# Type check a file
klar check myprogram.kl

# Check with ownership analysis output
klar check myprogram.kl --dump-ownership
```

### repl

Start an interactive REPL (Read-Eval-Print Loop).

```bash
klar repl
```

See [REPL Guide](repl-guide.md) for detailed usage.

### Other Commands

| Command | Description |
|---------|-------------|
| `tokenize <file>` | Show lexer output (tokens) |
| `parse <file>` | Show parser output (AST) |
| `disasm <file>` | Disassemble bytecode |
| `help` | Show help message |
| `version` | Show version |

## Exit Codes

The `klar run` command returns the exit code from your program's `main` function:

```klar
fn main() -> i32 {
    return 0  // Exit code 0 = success
}
```

If compilation fails, `klar` returns a non-zero exit code.

## Environment

Klar does not currently use any environment variables for configuration.

## Next Steps

- [REPL Guide](repl-guide.md) - Interactive development
- [Hello World](hello-world.md) - Your first program
