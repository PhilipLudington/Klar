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

**Bootstrap achieved (2026-03-10):** The selfhost compiler compiles itself with byte-identical fixed-point output. Stage 1 (compiled by Zig), Stage 2 (compiled by Stage 1), and Stage 3 (compiled by Stage 2) all produce identical typed AST JSON output.

## Prerequisites

- Klar compiler built: `./run-build.sh`

## Quick Start

```bash
# Full bootstrap (builds Stage 1, emits typed AST, builds Stage 2, validates)
./scripts/run-bootstrap-stage2.sh

# Or step by step:

# 1. Build Stage 1 binary (-O2 required for large struct ABI)
./zig-out/bin/klar build selfhost/main.kl -O2 -o build/stage1_main

# 2. Emit multi-module typed AST via Stage 1
build/stage1_main emit-typed-ast-multi selfhost/main.kl > stage2_ast.json

# 3. Build Stage 2 from the typed AST
./zig-out/bin/klar build selfhost/main.kl --typed-ast-input stage2_ast.json -o build/stage2_main

# 4. Verify fixed point: Stage 2 emits identical AST
build/stage2_main emit-typed-ast-multi selfhost/main.kl > stage3_ast.json
shasum -a 256 stage2_ast.json stage3_ast.json  # Should match
```

## Pipeline Stages

### Stage 0: Zig Compiler (Baseline)

The Zig-based compiler compiles the selfhost source into a native binary:

```bash
./zig-out/bin/klar build selfhost/main.kl -O2 -o build/stage1_main
```

This produces the Stage 1 binary. The `-O2` flag is required because LLVM optimization resolves a large struct parameter passing ABI issue.

### Stage 1: Selfhost Binary

The Stage 1 binary (`build/stage1_main`) is the selfhost compiler frontend compiled to native code. It supports these commands:

| Command | Description |
|---------|-------------|
| `check <file>` | Type-check a file |
| `dump-tokens <file>` | Dump lexer tokens as JSON |
| `dump-ast <file>` | Dump parser AST as JSON |
| `emit-typed-ast <file>` | Emit typed AST JSON (single file) |
| `emit-typed-ast-multi <file>` | Emit typed AST JSON (multi-module) |
| `help` | Show usage |

### Stage 2: Self-Compiled Binary

Stage 2 is built from Stage 1's multi-module typed AST emission:

1. Stage 1 discovers all 18 selfhost modules via BFS import traversal
2. Topologically sorts modules (dependencies before dependents)
3. Two-pass type checking: pre-register exports (for circular imports), then full check
4. Emits multi-module typed AST JSON envelope
5. Zig backend loads the JSON and generates native code via LLVM

### Fixed-Point Validation

Stage 2 produces **byte-identical** output to Stage 1:

```
Stage 1 emit-typed-ast-multi → 5,210,145 bytes → SHA-256: 1593eec5...
Stage 2 emit-typed-ast-multi → 5,210,145 bytes → SHA-256: 1593eec5...
Stage 3 emit-typed-ast-multi → 5,210,145 bytes → SHA-256: 1593eec5...
```

This proves the bootstrap is stable: the compiler compiles itself identically.

### Typed AST Format

#### Single-Module

```json
{
  "format": "typed-ast",
  "version": 1,
  "declarations": [...],
  "monomorphized_functions": [...],
  "monomorphized_structs": [...],
  "monomorphized_methods": [...]
}
```

#### Multi-Module

```json
{
  "format": "typed-ast-multi",
  "version": 1,
  "modules": [
    {"name": "lexer", "is_entry": false, "declarations": [...], "imports": [...]},
    {"name": "main", "is_entry": true, "declarations": [...], "imports": [...]}
  ],
  "monomorphized_functions": [...],
  "monomorphized_structs": [...],
  "monomorphized_methods": [...]
}
```

## Validation

### Single-File Pipeline

The validation script tests the typed AST pipeline against the test suite:

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

### Multi-Module Bootstrap

```bash
./scripts/run-bootstrap-stage2.sh --verbose
```

Validates:
1. Stage 1 builds successfully
2. Multi-module typed AST emission works (18 modules)
3. Stage 2 binary builds from typed AST
4. Single-file emission matches between Stage 1 and Stage 2
5. Multi-module emission matches (fixed-point validation)

### Zig Parser Fallback

When source files use features the selfhost parser doesn't support (meta annotations, `@pure`, `@deprecated`, etc.), the selfhost automatically falls back to the Zig parser via `KLAR_BIN`:

```bash
export KLAR_BIN=./zig-out/bin/klar
build/stage1_main emit-typed-ast file_with_meta.kl
```

The fallback runs `$KLAR_BIN dump-ast <file>` and uses that AST for type checking.

## Selfhost Architecture

### Source Files (18 modules)

| File | Purpose |
|------|---------|
| `selfhost/main.kl` | CLI entry point, multi-module orchestration |
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
| `selfhost/module_resolver.kl` | Module discovery + topological sort |

### Key Design Decisions

1. **Arena-based AST**: All AST nodes stored in flat `List` arenas indexed by IDs. No pointers between nodes.

2. **Value-based TypeChecker**: The `TypeChecker` struct is passed by value and returned modified (move semantics). This avoids mutable reference issues.

3. **Map indirection for large structs**: Due to a codegen limitation, large structs use `Map#[string, i32]` + parallel `List#[StructType]` instead of `Map#[string, LargeStruct]`.

4. **Zig parser fallback**: The selfhost parser handles ~95% of Klar syntax. Files with meta annotations fall back to the Zig parser automatically.

5. **Two-pass multi-module checking**: Pre-registers all module exports as `type=unknown` before any body checking, enabling circular imports (e.g., `parser_expr` ↔ `parser_stmt`).

6. **`-O2` required**: LLVM optimization resolves a large struct parameter passing ABI issue. Stage 1 must be built with `-O2`.

## Known Limitations

- **Associated types**: `T.Item` resolution not implemented in selfhost checker.
- **Async generics**: Future type support is basic.
- **9 type warnings**: Body check of the multi-module typed AST produces 9 non-fatal type warnings (codegen proceeds successfully).
