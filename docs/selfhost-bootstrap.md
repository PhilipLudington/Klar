# Self-Hosted Compiler Bootstrap

## Overview

The Klar self-hosted compiler frontend (`selfhost/`) implements a complete lexer, parser, and type checker in Klar itself. The **typed AST pipeline** connects the selfhost frontend to the Zig LLVM backend:

```
Source (.kl) → [Selfhost Frontend] → Typed AST JSON → [Zig Backend] → Native Binary
                 lexer.kl                                 codegen/emit.zig
                 parser.kl                                typed_ast_loader.zig
                 checker.kl
                 emitter.kl
```

## Prerequisites

- Klar compiler built: `./run-build.sh`
- Selfhost binary built: `./zig-out/bin/klar build selfhost/main.kl -o build/selfhost_main`

## Quick Start

```bash
# Build the selfhost binary (Stage 1)
./zig-out/bin/klar build selfhost/main.kl -o build/selfhost_main

# Compile a program through the typed AST pipeline
export KLAR_BIN=./zig-out/bin/klar
build/selfhost_main emit-typed-ast program.kl > typed_ast.json
./zig-out/bin/klar build program.kl --typed-ast-input typed_ast.json -o program
```

## Pipeline Stages

### Stage 0: Zig Compiler (Baseline)

The Zig-based compiler compiles the selfhost source into a native binary:

```bash
./zig-out/bin/klar build selfhost/main.kl -o build/selfhost_main
```

This produces the Stage 1 binary.

### Stage 1: Selfhost Binary

The Stage 1 binary (`build/selfhost_main`) is the selfhost compiler frontend compiled to native code. It supports these commands:

| Command | Description |
|---------|-------------|
| `check <file>` | Type-check a file |
| `dump-tokens <file>` | Dump lexer tokens as JSON |
| `dump-ast <file>` | Dump parser AST as JSON |
| `emit-typed-ast <file>` | Emit typed AST JSON |
| `help` | Show usage |

### Typed AST Pipeline

For single-file programs:

1. **Emit typed AST**: `build/selfhost_main emit-typed-ast file.kl > typed.json`
2. **Build via Zig backend**: `klar build file.kl --typed-ast-input typed.json -o binary`
3. **Run**: `./binary`

The typed AST JSON includes:
- All declarations (functions, structs, enums, traits, impls)
- Resolved types on expressions
- Monomorphized functions, structs, and methods
- Import declarations

## Validation

The validation script tests the entire pipeline against the test suite:

```bash
bash scripts/validate-typed-ast-pipeline.sh
```

Current results: **311/317 match (98.1%)**

| Category | Count | Notes |
|----------|-------|-------|
| Match | 311 | Exit codes and stdout identical |
| Std crashes | 3 | Standard pipeline crashes (meta edge cases) |
| Associated types | 1 | T.Item resolution not yet implemented |
| Parser limitation | 1 | String interpolation with `{` in literals |
| Expected error | 1 | Build error detection not in selfhost checker |

### Zig Parser Fallback

When source files use features the selfhost parser doesn't support (meta annotations, `@pure`, `@deprecated`, etc.), the selfhost automatically falls back to the Zig parser via `KLAR_BIN`:

```bash
export KLAR_BIN=./zig-out/bin/klar
build/selfhost_main emit-typed-ast file_with_meta.kl
```

The fallback runs `$KLAR_BIN dump-ast <file>` and uses that AST for type checking.

## Selfhost Architecture

### Source Files

| File | Purpose |
|------|---------|
| `selfhost/main.kl` | CLI entry point |
| `selfhost/lexer.kl` | Tokenization |
| `selfhost/parser.kl` | Parser core + error handling |
| `selfhost/parser_decl.kl` | Declaration parsing |
| `selfhost/parser_expr.kl` | Expression parsing |
| `selfhost/parser_stmt.kl` | Statement parsing |
| `selfhost/parser_type.kl` | Type expression parsing |
| `selfhost/ast.kl` | AST node definitions |
| `selfhost/types.kl` | Type system definitions |
| `selfhost/checker.kl` | Type checker core |
| `selfhost/checker_decl.kl` | Declaration checking |
| `selfhost/checker_expr.kl` | Expression checking |
| `selfhost/checker_stmt.kl` | Statement checking |
| `selfhost/checker_builtins.kl` | Builtin type/function registration |
| `selfhost/json.kl` | JSON parser |
| `selfhost/ast_builder.kl` | JSON-to-AST arena builder |
| `selfhost/emitter.kl` | Typed AST JSON emitter |

### Key Design Decisions

1. **Arena-based AST**: All AST nodes stored in flat `List` arenas indexed by IDs. No pointers between nodes.

2. **Value-based TypeChecker**: The `TypeChecker` struct is passed by value and returned modified (move semantics). This avoids mutable reference issues.

3. **Map indirection for large structs**: Due to a codegen limitation, large structs use `Map#[string, i32]` + parallel `List#[StructType]` instead of `Map#[string, LargeStruct]`.

4. **Zig parser fallback**: The selfhost parser handles ~95% of Klar syntax. Files with meta annotations fall back to the Zig parser automatically.

## Known Limitations

- **Single-file only**: The typed AST pipeline processes one file at a time. Multi-module compilation uses the standard Zig compiler.
- **Associated types**: `T.Item` resolution not implemented in selfhost checker.
- **Async generics**: Future type support is basic.
- **String interpolation**: Strings containing literal `{` may be misinterpreted as interpolation by the selfhost parser.
