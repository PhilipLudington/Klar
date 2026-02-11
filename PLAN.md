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

14 milestones across 5 phases, ordered by effort, impact, and dependencies:

### Phase 5A: Foundation (no code changes)

| # | Milestone | Effort | Impact | Source |
|---|-----------|--------|--------|--------|
| 1 | [MEMORY.md + grammar.json + spec.json](#milestone-1-llm-reference-files) | Low | **High** | Nanolang + MoonBit |

### Phase 5B: Small Language Features

| # | Milestone | Effort | Impact | Source |
|---|-----------|--------|--------|--------|
| 2 | [Mandatory return types](#milestone-2-mandatory-function-return-types) | Low | **High** | MoonBit |
| 3 | [Checked arithmetic (`+?`)](#milestone-3-checked-arithmetic) | Low-Med | Medium | Nanolang |
| 4 | [Pipeline operator (`\|>`)](#milestone-4-pipeline-operator) | Low | Medium | DSPy |

### Phase 5C: Testing Story

| # | Milestone | Effort | Impact | Source |
|---|-----------|--------|--------|--------|
| 5 | [Inline test blocks (`test`)](#milestone-5-inline-test-blocks) | Medium | **High** | Nanolang |
| 6 | [Comptime examples (`@example`)](#milestone-6-comptime-examples) | Low | Medium | DSPy |
| 7 | [AI test feedback (`--ai-feedback`)](#milestone-7-ai-test-feedback) | Low | **High** | DSPy |
| 8 | [Quality metrics in tests](#milestone-8-quality-metrics-in-tests) | Low | Medium | DSPy |

### Phase 5D: Tooling

| # | Milestone | Effort | Impact | Source |
|---|-----------|--------|--------|--------|
| 9 | [Type schema export (`klar schema`)](#milestone-9-type-schema-export) | Low-Med | Medium | DSPy |
| 10 | [Semantic sampler (`klar assist`)](#milestone-10-semantic-sampler-server) | High | **High** | MoonBit |
| 11 | [Incremental type checking](#milestone-11-incremental-type-checking) | High | **High** | MoonBit |

### Phase 5E: Advanced (Long-Term)

| # | Milestone | Effort | Impact | Source |
|---|-----------|--------|--------|--------|
| 12 | [Function contracts (`@doc`)](#milestone-12-function-contracts) | Medium | Medium | DSPy |
| 13 | [FFI sandbox (`--sandbox`)](#milestone-13-ffi-sandbox) | High | Medium | Nanolang |
| 14 | [Formal verification (Lean 4)](#milestone-14-formal-verification) | Very High | Medium | Nanolang |

---

## Milestone 1: LLM Reference Files

**Objective:** Create documentation optimized for LLM consumption: a human-readable reference, a machine-readable grammar spec, and a structured language spec.

**Status:** Not started

**Effort:** Low | **Impact:** High | **Source:** Nanolang (MEMORY.md) + MoonBit (grammar.json)

### Tasks

- [ ] **1.1** Create `MEMORY.md` at repo root (~1400 lines target)
  - Language version and feature list at top
  - Quick reference: all types, operators, keywords (table format)
  - Canonical patterns: one way to write each construct
  - Common errors and fixes (table format)
  - Type conversion cheat sheet
  - Standard library quick reference (all builtins with signatures)
  - Anti-patterns: what NOT to generate
- [ ] **1.2** Create `grammar.json` — machine-readable grammar for constrained decoding tools
  - All keywords and operators with precedence
  - Declaration patterns in simplified BNF
  - Type syntax rules
  - Loadable by grammar-constrained decoding libraries (`outlines`, `guidance`)
- [ ] **1.3** Create `spec.json` — structured language specification
  - All keywords and their roles
  - Type system rules
  - Operator precedence table
  - Built-in functions with signatures

### Success Criteria

- [ ] An LLM given only MEMORY.md can generate syntactically valid Klar code
- [ ] grammar.json can be loaded by constrained decoding tools
- [ ] All language features documented with exactly one canonical example

---

## Milestone 2: Mandatory Function Return Types

**Objective:** Require explicit `-> T` on all function declarations, including `-> void`.

**Status:** Not started

**Effort:** Low | **Impact:** High | **Source:** MoonBit

### Rationale

Klar's return types are currently optional (`parser.zig:2446-2449`). MoonBit's paper shows mandatory top-level type signatures enable the semantic sampler to know expected types at every call site. This also aligns with Klar's "no ambiguity" philosophy — every function self-documents its contract.

```klar
// Before: both valid
fn greet(name: string) { println("Hello " + name) }
fn greet(name: string) -> void { println("Hello " + name) }

// After: explicit return type always required
fn greet(name: string) -> void { println("Hello " + name) }
```

### Tasks

- [ ] **2.1** Change parser (`src/parser.zig:2446`): require `-> Type` after parameter list
  - Error if `->` missing: "return type annotation required for function declaration"
- [ ] **2.2** Update test suite: add `-> void` to any functions missing return types
- [ ] **2.3** Update MEMORY.md and docs to reflect requirement

### Success Criteria

- [ ] Parser rejects functions without explicit return types
- [ ] All existing tests updated and passing
- [ ] Error message is clear and actionable

---

## Milestone 3: Checked Arithmetic

**Objective:** Add `+?`, `-?`, `*?`, `/?` operators returning `Result[T, OverflowError]`.

**Status:** Not started

**Effort:** Low-Medium | **Impact:** Medium | **Source:** Nanolang

Completes Klar's four-mode overflow story:

| Operator | Behavior | Return Type | Status |
|----------|----------|-------------|--------|
| `+` | Panic (debug), wrap (release) | `T` | Exists |
| `+%` | Wrapping (modular) | `T` | Exists |
| `+|` | Saturating (clamp) | `T` | Exists |
| `+?` | Checked (return Result) | `Result[T, OverflowError]` | **NEW** |

### Tasks

- [ ] **3.1** Add tokens (`.plus_checked` etc.) and lexer rules (`src/token.zig`, `src/lexer.zig`)
- [ ] **3.2** Add AST binary op variants (`add_checked` etc. in `src/ast.zig`)
- [ ] **3.3** Add `OverflowError` builtin enum (Overflow, Underflow, DivisionByZero)
- [ ] **3.4** Type-check: operands must be integer, result is `Result[T, OverflowError]`
- [ ] **3.5** LLVM codegen: use existing `llvm.sadd.with.overflow.*` intrinsics
- [ ] **3.6** VM and interpreter support
- [ ] **3.7** Tests in `test/native/checked_arithmetic/`
- [ ] **3.8** Update formatter

### Success Criteria

- [ ] All four checked operators work across all three backends
- [ ] Composes with `?` and `??` operators
- [ ] All tests pass

---

## Milestone 4: Pipeline Operator

**Objective:** Add `|>` operator for left-to-right function composition with compile-time type checking.

**Status:** Not started

**Effort:** Low | **Impact:** Medium | **Source:** DSPy

```klar
// Without pipeline: nested, read inside-out
let result: string = format(validate(parse(input)))

// With pipeline: linear, read left-to-right
let result: string = input |> parse |> validate |> format
```

### Semantics

`x |> f` is sugar for `f(x)`. Desugared before backends — no special codegen needed.

### Tasks

- [ ] **4.1** Add `|>` token to lexer (`src/token.zig`, `src/lexer.zig`)
- [ ] **4.2** Parse as binary operator with lowest precedence (`src/parser.zig`)
- [ ] **4.3** Desugar `x |> f` to `f(x)` in checker (`src/checker.zig`)
- [ ] **4.4** Tests in `test/native/pipeline/`
- [ ] **4.5** Update formatter

### Success Criteria

- [ ] `x |> f` type-checks as `f(x)`
- [ ] Chains work: `x |> f |> g |> h`
- [ ] Composes with `?`: `x |> parse? |> validate?`
- [ ] Clear error messages on type mismatches

---

## Milestone 5: Inline Test Blocks

**Objective:** Add `test <name> { ... }` blocks for inline testing with `klar test` command.

**Status:** Not started (`klar test` exists as stub in `src/main.zig`)

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

- [ ] **5.1** Add `test` keyword to lexer, `TestDecl` AST node, parser support
- [ ] **5.2** Add assertion builtins: `assert_ne`, `assert_err`, `assert_ok`, `assert_some`, `assert_none`
- [ ] **5.3** Type-check test blocks (validate referenced function exists, check body)
- [ ] **5.4** Conditional compilation: `klar run`/`build` skip tests, `klar test` runs them
- [ ] **5.5** All three backend support
- [ ] **5.6** `klar test` command: file, `--fn`, directory modes; pass/fail reporting
- [ ] **5.7** Enforcement flags: `--strict-tests` (warn), `--require-tests` (error)
- [ ] **5.8** Formatter support, integration with AirTower
- [ ] **5.9** Tests in `test/native/test_blocks/`

### Success Criteria

- [ ] `test` keyword parses as top-level declaration
- [ ] `klar test file.kl` discovers and runs all test blocks
- [ ] `klar run` / `klar build` skip test blocks entirely
- [ ] All assertion builtins work across all three backends
- [ ] `--strict-tests` and `--require-tests` flags work

---

## Milestone 6: Comptime Examples

**Objective:** Add `@example` annotations validated at compile time.

**Status:** Not started

**Effort:** Low | **Impact:** Medium | **Source:** DSPy

```klar
@example(celsius_to_fahrenheit(0.0) == 32.0)
@example(celsius_to_fahrenheit(100.0) == 212.0)
fn celsius_to_fahrenheit(c: f64) -> f64 {
    return c * 1.8 + 32.0
}
```

### Semantics

- Evaluated at compile time using existing comptime infrastructure
- Compilation fails if any example fails
- Zero runtime cost — serve as docs, tests, AND compile checks simultaneously
- Complementary to test blocks: `@example` for simple I/O, `test` for complex scenarios

### Tasks

- [ ] **6.1** Parse `@example(expr)` as function annotation
- [ ] **6.2** Evaluate example expressions at compile time via comptime
- [ ] **6.3** Report failing examples with expected vs actual
- [ ] **6.4** Tests and formatter support

### Success Criteria

- [ ] `@example` expressions evaluated at compile time
- [ ] Compilation fails with clear message on example failure
- [ ] Works for pure functions with comptime-evaluable arguments

---

## Milestone 7: AI Test Feedback

**Objective:** Structured JSON test output enabling a DSPy-style assert → feedback → regenerate loop.

**Status:** Not started

**Effort:** Low | **Impact:** High | **Source:** DSPy | **Dependencies:** Milestone 5 (test blocks)

```bash
klar test math.kl --ai-feedback
```

```json
{
  "file": "math.kl",
  "tests": [{
    "name": "gcd",
    "status": "FAIL",
    "assertions": [
      { "type": "assert_eq", "passed": false, "call": "gcd(12, 8)", "expected": 4, "actual": 6 }
    ],
    "function_source": "fn gcd(a: i32, b: i32) -> i32 { return a % b }"
  }]
}
```

### The AI Regeneration Loop

1. AI generates Klar code with test block
2. `klar test --ai-feedback` runs tests, outputs structured JSON
3. On failure: feed JSON back to AI as context
4. AI regenerates with failure feedback (DSPy's backtrack → retry pattern)
5. Repeat until all tests pass

### Tasks

- [ ] **7.1** Add `--ai-feedback` flag to `klar test`
- [ ] **7.2** JSON output: test name, assertion results, expected vs actual, function source
- [ ] **7.3** Include compiler errors in same structured format
- [ ] **7.4** Documentation for AI tool integration

### Success Criteria

- [ ] `klar test --ai-feedback` outputs valid JSON
- [ ] JSON includes enough context for an LLM to fix failing code
- [ ] Works with all assertion types

---

## Milestone 8: Quality Metrics in Tests

**Objective:** Allow test blocks to report scored metrics (not just pass/fail).

**Status:** Not started

**Effort:** Low | **Impact:** Medium | **Source:** DSPy | **Dependencies:** Milestone 5 (test blocks)

```klar
test sort {
    assert_eq(sort(List.from([3, 1, 2])), List.from([1, 2, 3]))

    let large: List[i32] = List.range(0, 10000).reverse()
    let start: i64 = time_ns()
    let _sorted: List[i32] = sort(large)
    let elapsed: i64 = time_ns() - start

    metric("sort_10k_time", 1.0 - (elapsed.to_f64() / 1000000000.0))
}
```

### Tasks

- [ ] **8.1** Add `metric(name: string, score: f64)` builtin (test scope only)
- [ ] **8.2** `klar test` collects and reports metrics alongside pass/fail
- [ ] **8.3** `--ai-feedback` includes metrics in JSON output
- [ ] **8.4** Metrics are informational — they don't cause test failure

### Success Criteria

- [ ] `metric()` reports scores in test output
- [ ] AI tools can optimize code quality via metric scores

---

## Milestone 9: Type Schema Export

**Objective:** Export Klar types as JSON Schema for LLM structured output.

**Status:** Not started

**Effort:** Low-Medium | **Impact:** Medium | **Source:** DSPy

```bash
klar schema types.kl              # Export all public types
klar schema types.kl --type Point  # Export specific type
```

Maps: `i32`/`i64` → integer, `f64` → number, `bool` → boolean, `string` → string, `?T` → nullable, `List[T]` → array, `enum` → string enum, `struct` → object.

### Tasks

- [ ] **9.1** New `klar schema` CLI command in `src/main.zig`
- [ ] **9.2** AST walker: extract public structs/enums, emit JSON Schema
- [ ] **9.3** Handle nested types, optionals, collections
- [ ] **9.4** Tests with representative Klar types

### Success Criteria

- [ ] LLMs with structured output (OpenAI, Anthropic) can use exported schemas
- [ ] Schema stays in sync with Klar types (single source of truth)

---

## Milestone 10: Semantic Sampler Server

**Objective:** JSON-RPC server exposing Klar's parser and type checker for real-time constrained decoding.

**Status:** Not started

**Effort:** High | **Impact:** High | **Source:** MoonBit | **Dependencies:** Milestone 11 (incremental checking)

```bash
klar assist --port 9100    # TCP server
klar assist --stdio        # Stdio for tool integration
```

### Protocol

- `validate_token` — Is this next token syntactically valid? (local sampling)
- `check_partial` — Type-check partial code up to cursor (global sampling)
- `completions_at` — Valid completions with types at cursor position

### Tasks

- [ ] **10.1** JSON-RPC server infrastructure (shares with LSP)
- [ ] **10.2** `validate_token`: expose parser state for syntactic validation
- [ ] **10.3** `check_partial`: partial type checking (depends on Milestone 11)
- [ ] **10.4** `completions_at`: scope + type-aware suggestions
- [ ] **10.5** AI tool integration (VS Code, Claude Code, Cursor)

### Success Criteria

- [ ] Local sampling validates tokens with <1ms latency
- [ ] Global sampling catches type errors in partial code
- [ ] Integrates with at least one AI coding tool

---

## Milestone 11: Incremental Type Checking

**Objective:** Enable type checking of partial/incomplete code for the semantic sampler and LSP.

**Status:** Not started

**Effort:** High | **Impact:** High | **Source:** MoonBit | **Shares:** LSP infrastructure (Phase 4 M9)

### Tasks

- [ ] **11.1** Error-recovering parser: continue past first error, produce partial AST
- [ ] **11.2** Scope extraction at cursor position (all in-scope bindings with types)
- [ ] **11.3** Incremental type checking of partial declarations
- [ ] **11.4** Expected-type inference at cursor (what type does context demand?)

### Success Criteria

- [ ] Parser produces useful AST even with syntax errors
- [ ] Type checker reports errors found so far in partial code
- [ ] Scope query returns correct bindings at any cursor position

---

## Milestone 12: Function Contracts

**Objective:** Machine-readable preconditions, postconditions, and examples on functions.

**Status:** Not started

**Effort:** Medium | **Impact:** Medium | **Source:** DSPy

```klar
/// Compute the greatest common divisor.
/// @pre a >= 0 and b >= 0
/// @post result divides a and result divides b
/// @example gcd(12, 8) == 4
fn gcd(a: i32, b: i32) -> i32 { ... }
```

### Tasks

- [ ] **12.1** Parse structured doc comment tags (`@pre`, `@post`, `@example`)
- [ ] **12.2** `klar doc` extracts contracts into documentation
- [ ] **12.3** `klar schema` includes descriptions in JSON Schema
- [ ] **12.4** Optional runtime checking of `@pre`/`@post` in debug mode

### Success Criteria

- [ ] Structured doc tags parsed and available to tooling
- [ ] AI tools can read contracts to understand function behavior

---

## Milestone 13: FFI Sandbox

**Objective:** `--sandbox` mode isolating FFI calls in a child process via IPC.

**Status:** Not started

**Effort:** High | **Impact:** Medium | **Source:** Nanolang

### Tasks

- [ ] **13.1** IPC protocol design (serialize function name, args, results via pipes)
- [ ] **13.2** FFI worker process (load libraries, execute C calls, return results)
- [ ] **13.3** `klar repl --sandbox` (Phase 1: highest value)
- [ ] **13.4** `klar run --sandbox` (Phase 2: runtime)
- [ ] **13.5** `FfiError` type (WorkerCrashed, SerializationError, Timeout)
- [ ] **13.6** Worker lifecycle (auto-restart on crash, timeout, graceful shutdown)

### Success Criteria

- [ ] `klar repl --sandbox` isolates FFI crashes
- [ ] Worker crash returns `FfiError` instead of killing session
- [ ] `klar run --sandbox` works for bytecode VM

---

## Milestone 14: Formal Verification

**Objective:** Formalize Klar's core type system in Lean 4 and prove type soundness.

**Status:** Not started

**Effort:** Very High | **Impact:** Medium | **Source:** Nanolang | **Dependencies:** Language stabilization

### Tasks

- [ ] **14.1** Define KlarCore syntax as inductive type in Lean 4
- [ ] **14.2** Define typing rules and operational semantics
- [ ] **14.3** Prove preservation and progress theorems
- [ ] **14.4** Prove determinism and match exhaustiveness
- [ ] **14.5** Extract test oracle, CI integration

### Deliverables

- `proofs/` directory with Lean 4 source
- `proofs/README.md` explaining what is proved
- CI step verifying proofs

### Success Criteria

- [ ] Core type system formalized
- [ ] Preservation and progress proved
- [ ] Proofs checked in CI

---

## Remaining Phase 4 Items

| Item | Status | Notes |
|------|--------|-------|
| LSP | Not started | Shares infrastructure with Milestones 10-11 |
| VS Code extension | Not started | Depends on LSP |
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
