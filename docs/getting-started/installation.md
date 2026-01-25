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

# Windows (Chocolatey)
choco install zig
```

Verify the installation:

```bash
zig version
# Should show 0.15.0 or later
```

### Installing LLVM

```bash
# macOS (Homebrew)
brew install llvm@17

# Ubuntu/Debian
apt install llvm-17 llvm-17-dev

# Windows
# Download from releases.llvm.org
```

## Building Klar

Clone the repository and build:

```bash
git clone https://github.com/yourusername/klar.git
cd klar
./build.sh
```

The build script compiles the Klar compiler to `./zig-out/bin/klar`.

## Verifying the Installation

Check that the compiler is working:

```bash
./zig-out/bin/klar version
# Klar 0.3.1-dev

./zig-out/bin/klar help
# Shows usage information
```

## Adding to PATH

For convenience, add the Klar binary to your PATH:

```bash
# Add to ~/.bashrc or ~/.zshrc
export PATH="$PATH:/path/to/klar/zig-out/bin"
```

Or create a symlink:

```bash
sudo ln -s /path/to/klar/zig-out/bin/klar /usr/local/bin/klar
```

After adding to PATH:

```bash
klar version
# Klar 0.3.1-dev
```

## Running Tests

To verify the build, run the test suite:

```bash
./run-tests.sh
```

This runs unit tests, native compilation tests, and application tests.

## Next Steps

- [Hello World](hello-world.md) - Write your first Klar program
- [CLI Reference](cli-reference.md) - Learn the command-line interface
