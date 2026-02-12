# Klar Phase 5: AI-Native Language Improvements

> **Goal:** Strengthen Klar's AI-native story across documentation, language features, testing, and tooling.
>
> **Inspiration:** Three research sources, each contributing complementary ideas:
> - [Nanolang](docs/design/nanolang-inspiration.md) — Inline tests, checked arithmetic, FFI sandbox, formal verification
> - [MoonBit Semantic Sampler](docs/design/moonbit-semantic-sampler.md) — Mandatory return types, grammar spec, incremental checking, constrained decoding
> - [DSPy](docs/design/dspy-opportunities.md) — Pipeline operator, type schema export, AI test feedback, comptime examples, quality metrics

## Previous Phases

**Phase 4 (Language Completion):** All 13 milestones complete. Generics, traits, modules, stdlib (List, Map, Set, String, Option, Result), iterators, error handling (`?` operator), REPL, comptime, FFI (including function pointers), package manager, formatter, and doc generator all working.

> **Phase 4 archive:** [docs/history/phase4-language-completion.md](docs/history/phase4-language-completion.md)

---

## Implementation Order

5 milestones across 3 phases, ordered by effort, impact, and dependencies:

### Phase 5A: Foundation (no code changes)

| # | Milestone | Effort | Impact | Source |
|---|-----------|--------|--------|--------|
| 1 | [MEMORY.md](#milestone-1-llm-reference-file) | Low | **High** | Nanolang |

### Phase 5B: Small Language Features

| # | Milestone | Effort | Impact | Source |
|---|-----------|--------|--------|--------|
| 2 | ~~[Mandatory return types](#milestone-2-mandatory-function-return-types)~~ | Low | **High** | MoonBit | **Done** |

### Phase 5C: Testing Story

| # | Milestone | Effort | Impact | Source |
|---|-----------|--------|--------|--------|
| 3 | [Inline test blocks (`test`)](#milestone-3-inline-test-blocks) | Medium | **High** | Nanolang |
| 4 | [Structured test output (`--json`)](#milestone-4-structured-test-output) | Low | **High** | DSPy |

### Phase 5D: Tooling

| # | Milestone | Effort | Impact | Source |
|---|-----------|--------|--------|--------|
| 5 | [LSP and incremental type checking](#milestone-5-lsp-and-incremental-type-checking) | High | **High** | MoonBit |

---

## Completed Out-of-Band Improvements

- [x] **Explicit variable shadowing (`shadow`)**
  - Added `shadow` keyword with parser/AST/formatter support (`shadow let`, `shadow var`)
  - Type checker now rejects implicit shadowing and requires explicit `shadow`
  - Added `check` regression tests for:
    - explicit shadowing pass case
    - shadowing without `shadow` (error)
    - `shadow` with no outer binding (error)

---

## Milestone 1: LLM Reference File

**Objective:** Create a single consolidated reference document optimized for LLM consumption.

**Status:** Complete

**Effort:** Low | **Impact:** High | **Source:** Nanolang (MEMORY.md)

> **Deferred:** grammar.json and spec.json were originally planned but deferred. Constrained decoding is still niche — LLMs generate code from examples and docs, not formal grammars. The spec.json overlaps with MEMORY.md content. If the semantic sampler (Milestone 10) materializes, a machine-readable grammar can be revisited then.

### Tasks

- [x] **1.1** Create `MEMORY.md` at repo root (~1400 lines target)
  - Language version and feature list at top
  - Quick reference: all types, operators, keywords (table format)
  - Canonical patterns: one way to write each construct
  - Common errors and fixes (table format)
  - Type conversion cheat sheet
  - Standard library quick reference (all builtins with signatures)
  - Anti-patterns: what NOT to generate

### Success Criteria

- [x] An LLM given only MEMORY.md can generate syntactically valid Klar code
- [x] All language features documented with exactly one canonical example

---

## Milestone 2: Mandatory Function Return Types

**Objective:** Require explicit `-> T` on all function declarations, including `-> void`.

**Status:** Complete

**Effort:** Low | **Impact:** High | **Source:** MoonBit

### Rationale

Klar's return types were previously optional (`parser.zig:2446-2449`). MoonBit's paper shows mandatory top-level type signatures enable the semantic sampler to know expected types at every call site. This also aligns with Klar's "no ambiguity" philosophy — every function self-documents its contract.

```klar
// Explicit return type always required
fn greet(name: string) -> void { println("Hello " + name) }
```

### Tasks

- [x] **2.1** Change parser (`src/parser.zig`): require `-> Type` after parameter list
  - Error if `->` missing: "return type required for function (use '-> void' for functions that return nothing)"
  - Also updated `parseExternFnDecl` for extern functions
- [x] **2.2** Update test suite: add `-> void` to any functions missing return types
  - Fixed codegen bug: explicit `-> void` on methods with `ref`/`inout` self caused LLVM verification failure
  - Added negative test `test/native/missing_return_type.kl`
- [x] **2.3** Update MEMORY.md, docs/, and CLAUDE.md to reflect requirement

### Success Criteria

- [ ] Parser rejects functions without explicit return types
- [ ] All existing tests updated and passing
- [ ] Error message is clear and actionable

---

## Milestone 3: Inline Test Blocks

**Objective:** Add `test <name> { ... }` blocks for inline testing with `klar test` command.

**Status:** In progress (`klar test <file>` now executes only entry-module test blocks with module import support)

**Effort:** Medium | **Impact:** High | **Source:** Nanolang

```klar
fn gcd(a: i32, b: i32) -> i32 {
    if b == 0 { return a }
    return gcd(b, a % b)
}

test gcd {
    assert_eq(gcd(12, 8), 4)
    assert_eq(gcd(7, 0), 7)
    assert_eq(gcd(0, 5), 5)
}
```

### Tasks

- [x] **3.1** Add `test` keyword to lexer, `TestDecl` AST node, parser support
- [x] **3.2** Add assertion builtins: `assert_ne`, `assert_err`, `assert_ok`, `assert_some`, `assert_none`
- [x] **3.3** Type-check test blocks (validate referenced function exists, check body)
- [x] **3.4** Conditional compilation: `klar run`/`build` skip tests, `klar test` runs them
- [x] **3.5** All three backend support
- [x] **3.6** `klar test` command: file, `--fn`, directory modes; pass/fail reporting
- [x] **3.7** Enforcement flags: `--strict-tests` (warn), `--require-tests` (error)
- [x] **3.8** Formatter support, integration with AirTower
- [x] **3.9** Tests in `test/native/test_blocks/`

### Success Criteria

- [x] `test` keyword parses as top-level declaration
- [x] `klar test file.kl` discovers and runs all test blocks
- [x] `klar run` / `klar build` skip test blocks entirely
- [x] All assertion builtins work across all three backends
- [x] `--strict-tests` and `--require-tests` flags work

---

## Milestone 4: Structured Test Output

**Objective:** Add `--json` flag to `klar test` for machine-readable test results.

**Status:** Complete

**Effort:** Low | **Impact:** High | **Source:** DSPy | **Dependencies:** Milestone 3 (inline test blocks)

```bash
klar test math.kl --json
```

```json
{
  "file": "math.kl",
  "tests": [{
    "name": "gcd",
    "status": "FAIL",
    "assertions": [
      { "type": "assert_eq", "passed": false, "call": "gcd(12, 8)", "expected": 4, "actual": 6 }
    ]
  }]
}
```

### Use Cases

- **AI code generation:** Feed structured failures back to an LLM for automatic fix/retry loops
- **CI integration:** Machine-parseable results for build systems and dashboards
- **Tooling:** Any script or tool that needs to programmatically inspect test results

Optionally include function source with `--include-source` for richer AI context.

### Tasks

- [x] **4.1** Add `--json` flag to `klar test`
- [x] **4.2** JSON output: test name, assertion results, expected vs actual
- [x] **4.3** Include compiler errors in same structured format
- [x] **4.4** `--include-source` flag to embed function source in JSON output

### Success Criteria

- [x] `klar test --json` outputs valid JSON
- [x] JSON includes enough context for tooling to act on failures
- [x] Works with all assertion types

---

## Milestone 5: LSP and Incremental Type Checking

**Objective:** Build a Language Server Protocol implementation for Klar, powered by an error-recovering parser and incremental type checker.

**Status:** In progress

**Effort:** High | **Impact:** High | **Source:** MoonBit

### Tasks

**Compiler foundations:**
- [x] **5.1** Error-recovering parser: continue past first error, produce partial AST
- [ ] **5.2** Scope extraction at cursor position (all in-scope bindings with types)
- [ ] **5.3** Incremental type checking of partial declarations
- [ ] **5.4** Expected-type inference at cursor (what type does context demand?)

**LSP server:**
- [ ] **5.5** JSON-RPC transport layer (stdio)
- [ ] **5.6** `textDocument/diagnostic` — real-time error reporting
- [ ] **5.7** `textDocument/completion` — scope and type-aware completions
- [ ] **5.8** `textDocument/hover` — type information on hover
- [ ] **5.9** `textDocument/definition` — go to definition

**Editor integration:**
- [ ] **5.10** VS Code extension with syntax highlighting and LSP client

### Success Criteria

- [ ] Parser produces useful AST even with syntax errors
- [ ] Type checker reports errors found so far in partial code
- [ ] Scope query returns correct bindings at any cursor position
- [ ] LSP provides diagnostics, completions, hover, and go-to-definition
- [ ] VS Code extension works with the LSP

---

## Remaining Phase 4 Items

| Item | Status | Notes |
|------|--------|-------|
| Async/Await | Stretch goal | |
| Self-hosting | Stretch goal | |
| WebAssembly target | Stretch goal | |
| Windows support | Stretch goal | |

---

## References

**Nanolang:**
- [Nanolang GitHub](https://github.com/jordanhubbard/nanolang)
- [Design Analysis](docs/design/nanolang-inspiration.md)

**MoonBit:**
- [MoonBit Paper (IEEE)](https://ieeexplore.ieee.org/document/10734654/)
- [MoonBit Paper (ACM)](https://dl.acm.org/doi/10.1145/3643795.3648376)
- [Design Analysis](docs/design/moonbit-semantic-sampler.md)

**DSPy:**
- [DSPy Paper (arXiv)](https://arxiv.org/abs/2310.03714)
- [DSPy Paper (ICLR 2024)](https://openreview.net/pdf?id=sY5N0zY5Od)
- [Design Analysis](docs/design/dspy-opportunities.md)
