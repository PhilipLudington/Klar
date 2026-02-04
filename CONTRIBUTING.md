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

### Building

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
```

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

### Pull Request Process

1. **Create a branch** from `main`
2. **Make changes** following the coding standards below
3. **Run all tests** with `./run-tests.sh`
4. **Update documentation** if adding/changing features
5. **Submit PR** with a clear description

**PR titles** should follow the same format as commit messages.

**Never push directly to main** — always use pull requests.

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

### Adding Language Features

When adding a new language feature:

1. **Parser** (`parser.zig`): Add AST nodes and parsing rules
2. **AST** (`ast.zig`): Define node structures
3. **Checker** (`checker.zig`): Implement type checking
4. **All backends**: Update interpreter, VM, and native codegen
5. **Tests**: Add comprehensive tests in `test/native/`
6. **Documentation**: Update relevant docs in `docs/`

## Documentation

### Where to Document

- **Language features**: `docs/language/`
- **Type system**: `docs/types/`
- **Advanced topics**: `docs/advanced/`
- **Getting started**: `docs/getting-started/`

### Documentation Style

- Use clear, concise language
- Include code examples for every concept
- Test all code examples to ensure they work
- Link to related documentation

## Testing

### Writing Tests

Tests go in `test/native/` organized by feature:

```
test/native/
├── arrays/
│   ├── basic.kl
│   └── bounds.kl
├── structs/
│   ├── basic.kl
│   └── methods.kl
└── ...
```

### Test Conventions

```klar
// EXPECTED: expected output
// or for multi-line:
// EXPECTED:
// line 1
// line 2
// END_EXPECTED

fn main() -> i32 {
    // Test code
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

## Reporting Issues

When reporting bugs, include:

1. **Minimal reproduction** — Smallest code that demonstrates the issue
2. **Expected behavior** — What should happen
3. **Actual behavior** — What actually happens
4. **Environment** — OS, Zig version, LLVM version

Use the template in BUG.md as a guide for formatting bug reports.

## Feature Requests

For feature requests:

1. **Check PLAN.md** — The feature might be planned already
2. **Explain the use case** — Why is this needed?
3. **Consider AI impact** — Does it help or hinder AI code generation?
4. **Propose syntax** — What would it look like?

## Community

- Be respectful and constructive
- Help others learn
- Ask questions if unsure
- Follow the code of conduct

## License

By contributing, you agree that your contributions will be licensed under the MIT License.

---

Thank you for contributing to Klar! 🎉
