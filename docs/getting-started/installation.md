# Installation

This guide covers building the Klar compiler from source.

## Prerequisites

- **Zig 0.15+** - The compiler is written in Zig
- **LLVM 17+** - Required for native code generation
- **Git** - To clone the repository

### Installing Zig

Download Zig from [ziglang.org/download](https://ziglang.org/download/) or use your package manager:

```bash
# macOS (Homebrew)
brew install zig

# Ubuntu/Debian
snap install zig --classic

# Windows (winget - recommended)
winget install zig.zig

# Windows (Chocolatey)
choco install zig
```

Verify the installation:

```bash
zig version
# Should show 0.15.0 or later
```

### Installing LLVM (Optional)

LLVM is required only for `klar build` (native compilation). Without LLVM, the compiler still fully supports `klar run` (bytecode VM), `klar test`, `klar check`, `klar fmt`, `klar lsp`, and `klar repl`.

```bash
# macOS (Homebrew) — includes development headers
brew install llvm@17

# Ubuntu/Debian — must install -dev package for headers
apt install llvm-17 llvm-17-dev

# Windows — see "Windows Setup" section below
```

The build system auto-detects LLVM in these locations:
- macOS: `/opt/homebrew/opt/llvm` (ARM64), `/usr/local/opt/llvm` (x86_64)
- Linux: `/usr/include/llvm-c/`, `/usr/local/include/llvm-c/`
- Windows: `C:\Program Files\LLVM`, `C:\ProgramData\chocolatey\lib\llvm`
- All platforms: custom path via the `LLVM_PREFIX` environment variable

## Building Klar

Clone the repository and build:

```bash
git clone https://github.com/yourusername/klar.git
cd klar
./run-build.sh
```

The build script compiles the Klar compiler to `./zig-out/bin/klar`.

## Windows Setup

### Prerequisites

1. **Git for Windows** with Git Bash (provides a Bash shell)
2. **Zig 0.15+**: `winget install zig.zig`

### Building on Windows

Use Git Bash or any Bash-compatible shell:

```bash
# Build the compiler
zig build

# Or use the wrapper script
./run-build.sh
```

The compiler builds in VM-only mode when LLVM development headers are not found. All features work except `klar build` (native compilation):

| Command | Without LLVM | With LLVM |
|---------|-------------|-----------|
| `klar run` | VM backend (default) | Native backend (default) |
| `klar test` | Works | Works |
| `klar check` | Works | Works |
| `klar fmt` | Works | Works |
| `klar lsp` | Works | Works |
| `klar repl` | Works | Works |
| `klar build` | Error with guidance | Works |

### LLVM on Windows (for native compilation)

The standard LLVM Windows installer (`winget install LLVM.LLVM`) does not include the C API development headers (`llvm-c/Core.h`) needed for native code generation. To enable `klar build` on Windows, you need LLVM built with development headers:

**Option 1: Set LLVM_PREFIX**
If you have a custom LLVM build with headers:
```bash
set LLVM_PREFIX=C:\path\to\llvm
zig build
```

**Option 2: Build from source**
Build LLVM from source with `-DLLVM_ENABLE_PROJECTS=clang` and install the development headers.

### Line Endings

The repository includes a `.gitattributes` file that enforces LF line endings for all text files. This prevents CRLF issues with test comparisons and shell scripts.

If you cloned before `.gitattributes` was added, re-normalize:
```bash
git add --renormalize .
git checkout -- .
```

### Known Windows Differences

- `klar run` uses the VM backend by default (LLVM builds use native compilation)
- `klar run` with native compilation shows temp binary path as `args[0]` instead of the source file path
- Test scripts (`run-tests.sh`, etc.) require Git Bash or WSL

## Verifying the Installation

Check that the compiler is working:

```bash
./zig-out/bin/klar --version
# Klar 0.4.0

./zig-out/bin/klar --help
# Shows usage information
```

## Adding to PATH

For convenience, add the Klar binary to your PATH:

```bash
# macOS/Linux: add to ~/.bashrc or ~/.zshrc
export PATH="$PATH:/path/to/klar/zig-out/bin"

# Windows (Git Bash): add to ~/.bashrc
export PATH="$PATH:/c/path/to/klar/zig-out/bin"
```

On macOS/Linux, you can also create a symlink:

```bash
sudo ln -s /path/to/klar/zig-out/bin/klar /usr/local/bin/klar
```

After adding to PATH:

```bash
klar --version
# Klar 0.4.0
```

## Running Tests

To verify the build, run the test suite:

```bash
./run-tests.sh
```

This runs unit tests, native compilation tests, and application tests. On Windows without LLVM, unit tests and check tests will pass; native/app/module tests require LLVM.

## Next Steps

- [Hello World](hello-world.md) - Write your first Klar program
- [CLI Reference](cli-reference.md) - Learn the command-line interface
