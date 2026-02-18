# Self-Hosting the Klar Compiler

This guide explains the bootstrap architecture for making the Klar compiler self-hosted — that is, writing the compiler frontend in Klar itself.

## Scope

Self-hosting covers the **frontend only**: lexer, parser, AST, type system, and type checker. The LLVM codegen backend (~11,000+ lines of Zig) stays in Zig. The self-hosted frontend serializes typed AST for the Zig backend to consume.

This is the standard bootstrap strategy used by Go, Rust, and other self-hosted languages.

## Bootstrap Stages

### Stage 0: Zig Compiler (current)

The full compiler is written in Zig. This is the reference implementation.

```
source.kl → [Zig Lexer → Zig Parser → Zig Checker → Zig Codegen] → binary
```

### Stage 1: Klar Frontend + Zig Backend

The frontend is rewritten in Klar and compiled by the Stage 0 compiler.

```
source.kl → [Klar Lexer → Klar Parser → Klar Checker] → AST JSON → [Zig Codegen] → binary
```

### Stage 2: Self-Compiled Frontend

The Stage 1 compiler compiles its own Klar frontend source. If Stage 1 and Stage 2 produce identical output, the bootstrap is validated.

```
selfhost/*.kl → [Stage 1 Frontend] → AST JSON → [Zig Codegen] → Stage 2 binary
```

**Validation:** Stage 2 binary must produce bit-identical AST output as Stage 1 on all test inputs.

## Directory Structure

```
selfhost/
├── lexer.kl       # Tokenizer (Milestone 9.4)
├── ast.kl         # AST type definitions (Milestone 9.5)
├── parser.kl      # Recursive descent parser (Milestones 9.6–9.7)
├── types.kl       # Type system definitions (Milestone 9.8)
├── checker.kl     # Type checker (Milestones 9.9–9.10)
└── main.kl        # Entry point, pipeline wiring (Milestone 9.11)
```

All files are currently stubs that pass `klar check`. They will be progressively implemented across milestones 9.4–9.11.

## Parity Testing Commands

Two diagnostic commands enable parity testing between the Zig and Klar frontends:

### `klar dump-tokens <file>`

Outputs the token stream as a JSON array:

```bash
klar dump-tokens hello.kl
```

```json
[
  {"kind":"fn_","text":"fn","line":1,"column":1,"start":0,"end":2},
  {"kind":"identifier","text":"main","line":1,"column":4,"start":3,"end":7},
  ...
]
```

Each token includes: `kind` (tag name), `text` (source slice), `line`, `column`, `start`, `end`. Newlines and EOF are included.

### `klar dump-ast <file>`

Outputs the full AST as a JSON object:

```bash
klar dump-ast hello.kl
```

```json
{
  "module_decl": null,
  "imports": [],
  "declarations": [
    {
      "kind": "function",
      "name": "main",
      "params": [],
      "return_type": {"kind": "named", "name": "i32"},
      "body": { ... }
    }
  ]
}
```

Every tagged union includes a `"kind"` discriminator. No span/location information is included — the focus is structural parity.

> **Note:** The example above is abbreviated. The full output includes additional fields such as `is_pub`, `type_params`, `where_clause`, `body`, etc. Run `klar dump-ast` on a sample file to see the complete schema.

## Estimated Sizes

| Component | Estimated Lines | Milestone |
|-----------|----------------|-----------|
| Lexer | 500–700 | 9.4 |
| AST Definitions | 800–1,000 | 9.5 |
| Parser (Core) | 1,500–2,000 | 9.6 |
| Parser (Full) | 1,500–2,000 | 9.7 |
| Type System | 600–800 | 9.8 |
| Type Checker (Foundation) | 2,000–3,000 | 9.9 |
| Type Checker (Advanced) | 2,000–3,000 | 9.10 |
| Integration | 500–1,000 | 9.11 |
| **Total** | **~11,000–16,000** | |

## Development Workflow

1. **Implement a component** (e.g., lexer) in `selfhost/lexer.kl`
2. **Run type checking:** `klar check selfhost/lexer.kl`
3. **Run inline tests:** `klar test selfhost/lexer.kl`
4. **Run parity tests:** Compare `klar dump-tokens` output from Zig lexer vs selfhost lexer on the test corpus
5. **Run the full selfhost test suite:** `./scripts/run-selfhost-tests.sh`

## Running Tests

```bash
# Run all selfhost tests (check + inline tests)
./scripts/run-selfhost-tests.sh

# Run as part of the full test suite
./run-tests.sh
```

Results are written to `.selfhost-test-results.json` for AirTower integration.

## Milestone Roadmap

| # | Milestone | Status |
|---|-----------|--------|
| 9.1 | String Primitives | Complete |
| 9.2 | Collection Foundations | Complete |
| 9.3 | Bootstrap Architecture | Complete |
| 9.4 | Self-Hosted Lexer | Planned |
| 9.5 | AST Definitions | Planned |
| 9.6 | Parser (Core Subset) | Planned |
| 9.7 | Parser (Full Language) | Planned |
| 9.8 | Type System Definitions | Planned |
| 9.9 | Type Checker (Foundation) | Planned |
| 9.10 | Type Checker (Advanced) | Planned |
| 9.11 | Frontend Integration | Planned |
| 9.12 | Bootstrap Validation | Planned |
| 9.13 | Tooling Self-Hosting | Stretch |
