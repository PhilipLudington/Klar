# WebAssembly Target

Klar can compile programs to WebAssembly (wasm32), enabling deployment in browsers, edge runtimes, and standalone wasm environments.

## Quick Start

```bash
# Compile to .wasm (freestanding)
klar build hello.kl --target wasm

# Compile to object file only
klar build hello.kl --target wasm -c

# Inspect generated LLVM IR
klar build hello.kl --target wasm --emit-llvm
```

## Target Variants

| Shorthand | Full Triple | Description |
|-----------|-------------|-------------|
| `wasm` | `wasm32-unknown-unknown` | Freestanding wasm (no OS) |
| `wasi` | `wasm32-unknown-wasi` | WASI target (future) |

The `wasm` shorthand is the recommended target. It produces a freestanding `.wasm` module where libc functions (`puts`, `printf`, `malloc`, `free`) become wasm imports that the host environment must provide.

## How It Works

Klar uses LLVM to generate WebAssembly:

1. **Codegen**: LLVM emits a wasm32 object file (`.o`)
2. **Linking**: `wasm-ld` (from LLVM/LLD) links it into a `.wasm` module
3. **Imports**: Unresolved symbols (libc functions) become wasm imports
4. **Exports**: All symbols are exported via `--export-all`

### Requirements

- **wasm-ld** must be installed (comes with LLVM). On macOS:
  ```bash
  brew install llvm
  ```

## Platform Differences

### Pointer Size

On wasm32, pointers are 32 bits (4 bytes). The `isize` and `usize` types are 32-bit integers instead of the usual 64-bit on x86_64/aarch64. This is handled automatically by the compiler.

### Unsupported Features

The following features are not available on WebAssembly targets and will trap at runtime if called:

- **Filesystem operations**: `fs_exists`, `fs_is_file`, `fs_is_dir`, `fs_create_dir`, `fs_create_dir_all`, `fs_remove_file`, `fs_remove_dir`, `fs_read_string`, `fs_write_string`, `fs_read_dir`
- **Standard input**: `readline`

Print functions (`print`, `println`) work by importing `puts`/`printf` from the host environment.

## Running Wasm Modules

### With wasmtime

```bash
klar build hello.kl --target wasm
wasmtime build/hello.wasm
```

Note: wasmtime must provide the required imports (puts, printf, malloc, free).

### In a Browser

```javascript
const imports = {
  env: {
    puts: (ptr) => { /* read string from wasm memory at ptr */ },
    printf: (fmt, ...args) => { /* format and print */ },
    malloc: (size) => { /* allocate in wasm memory */ },
    free: (ptr) => { /* free in wasm memory */ },
  }
};

const { instance } = await WebAssembly.instantiateStreaming(
  fetch("hello.wasm"),
  imports
);

instance.exports.main();
```

## Examples

### Pure Computation

Programs that do arithmetic and return results work directly:

```klar
fn fibonacci(n: i32) -> i32 {
    if n <= 1 {
        return n
    }
    return fibonacci(n - 1) + fibonacci(n - 2)
}

fn main() -> i32 {
    return fibonacci(10)
}
```

```bash
klar build fib.kl --target wasm
```

### Structs and Generics

Structs, methods, closures, and generics all work on wasm:

```klar
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn sum(self: Point) -> i32 {
        return self.x + self.y
    }
}

fn identity[T](x: T) -> T {
    return x
}

fn main() -> i32 {
    let p: Point = Point { x: 10, y: 20 }
    return identity(p.sum())
}
```

## Future Work

- **WASI support**: Full WASI sysroot for filesystem, environment variables, and clock access
- **WASI-P2 (Component Model)**: First-class component support
- **Browser integration**: Built-in JS binding generation
