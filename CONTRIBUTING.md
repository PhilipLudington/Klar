# Contributing to Klar

Thank you for your interest in contributing to Klar! This document provides guidelines and information for contributors.

## Getting Started

### Prerequisites

- **Zig 0.15+** - The compiler is written in Zig
- **LLVM 17+** - Required for native code generation
- **Git** - For version control

### Building from Source

```bash
git clone https://github.com/PhilipLudington/Klar.git
cd Klar
./build.sh
```

### Running Tests

```bash
# Run all tests
./run-tests.sh

# Run native compilation tests
./scripts/run-native-tests.sh

# Run benchmarks
./scripts/run-benchmarks.sh
```

## How to Contribute

### Reporting Bugs

Before submitting a bug report:

1. Check existing issues to avoid duplicates
2. Use the latest version from `main`
3. Create a minimal reproduction case

When reporting:

- Describe the expected vs actual behavior
- Include the Klar code that triggers the bug
- Provide compiler output/error messages
- List your environment (OS, Zig version, LLVM version)

See [BUG.md](BUG.md) for examples of well-documented bugs.

### Suggesting Features

Feature suggestions are welcome! Please:

1. Check the [ROADMAP.md](ROADMAP.md) to see if it's already planned
2. Explain the use case and motivation
3. Consider how it fits Klar's design philosophy (see [DESIGN.md](DESIGN.md))

### Code Contributions

1. **Fork** the repository
2. **Create a branch** for your changes:
   ```bash
   git checkout -b feature/your-feature-name
   ```
3. **Make your changes** following the code style below
4. **Test** your changes thoroughly
5. **Submit a pull request** against `main`

### Pull Request Guidelines

- **Never push directly to main** - always use pull requests
- Keep PRs focused on a single change
- Include tests for new functionality
- Update documentation as needed
- Describe your changes in the PR description

## Code Style

### Zig Code

- Follow Zig's official style guide
- Use meaningful variable and function names
- Add comments for complex logic
- Keep functions focused and reasonably sized

### Klar Code (examples, tests)

- Follow the patterns in existing examples
- Include type annotations (Klar requires explicit types)
- Add comments explaining what the example demonstrates

### Documentation

- Use Markdown for documentation
- Keep examples simple and focused
- Update the table of contents when adding sections

## Project Structure

```
Klar/
├── src/           # Compiler source code (Zig)
├── std/           # Standard library (Klar)
├── examples/      # Example programs
├── test/          # Test files
├── benchmarks/    # Performance benchmarks
├── docs/          # Documentation
├── scripts/       # Build and utility scripts
└── carbide/       # Carbide integration
```

## Development Workflow

### Understanding the Codebase

1. Read [DESIGN.md](DESIGN.md) for language philosophy
2. Read [PLAN.md](PLAN.md) for implementation details
3. Explore `src/` starting with `main.zig`

### Compilation Pipeline

```
Source (.kl) → Lexer → Parser → Type Checker → [Backend]
                                     ↓
              ┌──────────────────────┼──────────────────────┐
              │                      │                      │
         Interpreter          Bytecode VM            LLVM Codegen
         (--interpret)          (default)                  ↓
              │                    │                 Native Binary
              ↓                    ↓
           Output               Output
```

### Key Source Files

| File | Purpose |
|------|---------|
| `src/main.zig` | Entry point, CLI handling |
| `src/lexer.zig` | Tokenization |
| `src/parser.zig` | Syntax parsing |
| `src/checker.zig` | Type checking |
| `src/codegen/` | Code generation (LLVM) |
| `src/vm.zig` | Bytecode virtual machine |
| `src/interpreter.zig` | Tree-walking interpreter |

## Documentation Contributions

Documentation improvements are always welcome:

- Fix typos and clarify confusing sections
- Add examples for underdocumented features
- Translate documentation (coordinate with maintainers first)

## Questions?

If you have questions about contributing:

1. Check the existing documentation
2. Look at similar PRs for guidance
3. Open an issue for discussion

## Code of Conduct

Be respectful, constructive, and professional. We're all here to build something useful together.

## License

By contributing, you agree that your contributions will be licensed under the MIT License.
