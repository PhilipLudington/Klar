# Contributing to Klar

Thank you for your interest in contributing to Klar! This document provides guidelines and information for contributors.

## Philosophy

Klar is an **AI-native** programming language. Every design decision should support:

1. **Unambiguous syntax** — Code should be parseable without surrounding context
2. **No undefined behavior** — Every operation has defined semantics
3. **Explicit over implicit** — No hidden conversions or behaviors
4. **One obvious way** — Minimize redundant syntax forms

When contributing, ask: "Would this help an AI agent write correct code?"

## Getting Started

### Prerequisites

- **Zig 0.15+** — The compiler is written in Zig
- **LLVM 17+** — Required for native code generation
- **Git** — For version control

### Building from Source

```bash
git clone https://github.com/PhilipLudington/Klar.git
cd Klar
./build.sh
```

### Running Tests

```bash
# Run all tests (required before submitting PRs)
./run-tests.sh

# Run specific test suites
./scripts/run-unit-tests.sh      # Zig unit tests
./scripts/run-native-tests.sh    # Native compilation tests
./scripts/run-app-tests.sh       # Reference application tests
./scripts/run-module-tests.sh    # Module system tests

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

## Development Workflow

### Branch Naming

- `feature/description` — New features
- `fix/description` — Bug fixes
- `docs/description` — Documentation changes
- `refactor/description` — Code refactoring

### Commit Messages

Follow conventional commits:

```
type(scope): description

[optional body]

[optional footer]
```

Types: `feat`, `fix`, `docs`, `refactor`, `test`, `chore`

Examples:
```
feat(parser): add support for async functions
fix(codegen): correct tuple field access alignment
docs(readme): update installation instructions
```

### Pull Request Guidelines

- **Never push directly to main** — always use pull requests
- Keep PRs focused on a single change
- Include tests for new functionality
- Update documentation as needed
- PR titles should follow the same format as commit messages

### Understanding the Codebase

1. Read [DESIGN.md](DESIGN.md) for language philosophy
2. Read [PLAN.md](PLAN.md) for implementation details
3. Explore `src/` starting with `main.zig`

## Coding Standards

### Zig Code

The compiler uses CarbideZig standards (see `carbide/CARBIDE.md`):

- 4-space indentation
- Max line length: 120 characters
- Use `const` by default, `var` only when needed
- Handle all errors explicitly

### Klar Code (Examples/Tests)

- 4-space indentation
- Explicit type annotations (required by design)
- One statement per line
- Document non-obvious code with comments

### Test Files

Test files in `test/native/` follow a naming convention:
- `feature_basic.kl` — Basic functionality
- `feature_edge.kl` — Edge cases
- `feature_error.kl` — Expected error cases

Include an `EXPECTED:` comment at the top for output verification:

```klar
// EXPECTED: Hello, World!
fn main() -> i32 {
    println("Hello, World!")
    return 0
}
```

For error tests (expected to fail compilation):
```klar
// ERROR: expected error message substring

fn main() -> i32 {
    // Code that should fail
    return 0
}
```

## Architecture Overview

```
Source (.kl) → Lexer → Parser → AST → Type Checker → [Backend]
                                            ↓
              ┌─────────────────────────────┴─────────────────┐
              │                             │                 │
         Interpreter              Bytecode Compiler      IR Builder
         (--interpret)               (default)              ↓
              │                         │              LLVM Codegen
              ↓                         ↓                   ↓
           Output                      VM             Native Binary
```

### Key Source Files

| File | Purpose |
|------|---------|
| `src/main.zig` | CLI entry point |
| `src/lexer.zig` | Tokenization |
| `src/parser.zig` | Recursive descent parser |
| `src/checker.zig` | Type checking, trait resolution |
| `src/codegen/emit.zig` | LLVM IR generation |
| `src/vm.zig` | Bytecode virtual machine |
| `src/interpreter.zig` | Tree-walking interpreter |

### Adding Language Features

When adding a new language feature:

1. **Parser** (`parser.zig`): Add AST nodes and parsing rules
2. **AST** (`ast.zig`): Define node structures
3. **Checker** (`checker.zig`): Implement type checking
4. **All backends**: Update interpreter, VM, and native codegen
5. **Tests**: Add comprehensive tests in `test/native/`
6. **Documentation**: Update relevant docs in `docs/`

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

## Documentation Contributions

Documentation improvements are always welcome:

- Fix typos and clarify confusing sections
- Add examples for underdocumented features
- Keep examples simple and focused
- Update the table of contents when adding sections

### Where to Document

- **Language features**: `docs/language/`
- **Type system**: `docs/types/`
- **Advanced topics**: `docs/advanced/`
- **Getting started**: `docs/getting-started/`

## Questions?

If you have questions about contributing:

1. Check the existing documentation
2. Look at similar PRs for guidance
3. Open an issue for discussion

## Code of Conduct

Be respectful, constructive, and professional. We're all here to build something useful together.

## License

By contributing, you agree that your contributions will be licensed under the MIT License.
