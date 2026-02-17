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

- [x] Parser rejects functions without explicit return types
- [x] All existing tests updated and passing
- [x] Error message is clear and actionable

---

## Milestone 3: Inline Test Blocks

**Objective:** Add `test <name> { ... }` blocks for inline testing with `klar test` command.

**Status:** Complete

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

**Status:** Complete

**Effort:** High | **Impact:** High | **Source:** MoonBit

### Tasks

**Compiler foundations:**
- [x] **5.1** Error-recovering parser: continue past first error, produce partial AST
- [x] **5.2** Scope extraction at cursor position (all in-scope bindings with types)
  - Added checker API `extractScopeAtOffset(module, cursor_offset)` with local-scope traversal
  - Exposed via CLI: `klar check <file> --scope-at <line:col> [--scope-json]`
  - Added checker unit tests for nested scopes, declaration ordering, and explicit shadowing behavior
- [x] **5.3** Incremental type checking of partial declarations
  - Added `klar check --partial` mode using parser recovery (`parseModuleRecovering`)
  - Partial mode reports parse diagnostics and still type-checks recoverable declarations
  - Scope queries (`--scope-at`) now work with partial mode and type-error states for tooling workflows
- [x] **5.4** Expected-type inference at cursor (what type does context demand?)
  - Added checker expected-type inference for cursor-positioned expressions in declaration/function contexts
  - Exposed via CLI: `klar check <file> --expected-type-at <line:col>`
  - Added args regression coverage for expected type inference at cursor

**LSP server:**
- [x] **5.5** JSON-RPC transport layer (stdio)
  - Added `klar lsp` command with `Content-Length` framed JSON-RPC over stdio
  - Handles `initialize`, `shutdown`, and `exit` lifecycle messages
  - Added args smoke test for framed initialize/shutdown/exit handshake
- [x] **5.6** `textDocument/diagnostic` — real-time error reporting
  - Added pull diagnostics request handling for `textDocument/diagnostic`
  - Returns parser and checker diagnostics in LSP `full` report format
  - Advertises `diagnosticProvider` capability in initialize response
  - Added args regression coverage for diagnostic response payloads
- [x] **5.7** `textDocument/completion` — scope and type-aware completions
  - Added `textDocument/completion` request handling in `klar lsp`
  - Completion items are derived from checker scope extraction at cursor offset
  - Ranking is type-aware via expected-type matching (`sortText` prioritization)
  - Added args regression coverage for completion payload fields/ranking
- [x] **5.8** `textDocument/hover` — type information on hover
  - Added `textDocument/hover` request handling in `klar lsp`
  - Hover resolves identifier at cursor and returns symbol type/kind in markdown
  - Advertises `hoverProvider` capability in initialize response
  - Added args regression coverage for hover response payload fields
- [x] **5.9** `textDocument/definition` — go to definition
  - Added `textDocument/definition` request handling in `klar lsp`
  - Resolves symbol at cursor and returns same-file declaration location range
  - Advertises `definitionProvider` capability in initialize response
  - Added args regression coverage for definition response payload

**Editor integration:**
- [x] **5.10** VS Code extension with syntax highlighting and LSP client
  - Added `tools/vscode-klar` extension scaffold (TypeScript + `vscode-languageclient`)
  - Added Klar language registration (`.kl`), language configuration, and TextMate grammar
  - Added LSP client wiring to launch `klar lsp` with configurable executable path/args
  - Added extension README with local development instructions

### Success Criteria

- [x] Parser produces useful AST even with syntax errors
- [x] Type checker reports errors found so far in partial code
- [x] Scope query returns correct bindings at any cursor position
- [x] LSP provides diagnostics, completions, hover, and go-to-definition
- [x] VS Code extension works with the LSP

---

## Remaining Phase 4 Items

| Item | Status | Notes |
|------|--------|-------|
| Async/Await | **Done** (Milestone 6) | |
| Self-hosting | Stretch goal | |
| WebAssembly target | Stretch goal | |
| Windows support | Stretch goal | |

### Suggested Next Execution Order

1. ~~Async/Await~~ (Complete)
2. Windows support
3. WebAssembly target
4. Self-hosting

---

## Milestone 6: Async/Await

**Objective:** Add first-class async functions with explicit `async fn` declarations and `await` expressions.

**Status:** Complete

**Effort:** High | **Impact:** High | **Dependencies:** Milestones 2, 3

### Tasks

- [x] **6.1** Syntax and AST support
  - Add lexer/parser support for `async` and `await`
  - Extended AST unary ops with `await` and parser support for `await <expr>`
  - Added parser support for `async fn` declarations (top-level and trait/impl methods)
  - Added parser/lexer regression tests for `async`/`await` syntax
  - Enforce explicit return annotations for async functions (`-> Future[T]` or equivalent canonical form)
- [x] **6.2** Type-checking semantics
  - Validate await operand type constraints (`await` operand must be `Future[T]`)
  - Enforced await usage context with checker diagnostics (`await` only permitted in async function context)
  - Enforced explicit async contract: `async fn` must declare `-> Future[T]`
  - `await` now enforces `Future[T]` operands and yields `T`
  - Added explicit checker diagnostics for unsupported async declarations and methods (`async fn`, async trait/impl methods)
  - Added check-suite regression coverage for invalid async method declarations
  - Added checker unit coverage for await operand validation and async-call await behavior
- [x] **6.3** Runtime/backend execution model
  - Implement minimal task/future representation in VM and interpreter
  - Added runtime cooperative executor scaffold (`src/runtime/async_executor.zig`) with deterministic FIFO scheduling, cancellation, and failure isolation semantics
  - Wired interpreter runtime state to include cooperative executor scaffold for future async integration
  - Added minimal `Future` runtime value representation in interpreter (`src/values.zig`) and VM value model (`src/vm_value.zig`) with task ids and lifecycle states (`pending/completed/failed/cancelled`)
  - Added interpreter `await` runtime behavior for futures: completed futures yield values, pending/failed/cancelled futures return deterministic runtime errors
  - Removed checker/runtime backend gating for top-level `async fn` + `await` so async call flows execute end-to-end
  - Added VM bytecode opcode `op_await` and initial await runtime handling
  - Added native codegen lowering for `await` as synchronous value forwarding
  - Aligned runtime error termination semantics across backends (`exit(1)` on runtime errors in CLI paths)
  - Added parity coverage for non-completed Future states (`pending`/`failed`/`cancelled`) in runtime unit tests plus native raw-layout smoke coverage
  - **Follow-up required:** define long-term async execution semantics beyond current synchronous-completion model:
    - scheduler/task lifecycle semantics for real concurrent async execution
    - cancellation propagation policy for task trees
    - failure propagation model for awaited task failures
  - Ensure deterministic behavior and clear cancellation/error propagation semantics
- [x] **6.4** Tooling integration
  - Update formatter for async/await constructs
  - Added formatter emission for `async fn` modifiers across top-level, trait, and impl method declarations
  - Updated formatter regression fixture `test/fmt/async.kl` to use semantically valid async patterns (`Future[T]` returns)
  - Add LSP completion/hover/diagnostic support for new syntax
  - Added LSP keyword completions for `async`/`await` with async-aware detail text
  - Added LSP hover keyword docs for `async`/`await` when not resolving to symbols
  - Added args-suite LSP regression coverage for async completion, hover, and diagnostics payloads
- [x] **6.5** Tests and docs
  - Add parser/checker/runtime tests for success and failure paths
  - Added checker unit regressions for async `await` success cases and operand-validation failures
  - Added cross-backend args-suite regression (`native`/`vm`/`interpret`) for async/await execution parity
  - Added native regression fixture `test/native/async_await_basic.kl`
  - Add examples showing sequential async flows (concurrent execution deferred to future work)
  - Update MEMORY.md and docs language guide
  - Updated `MEMORY.md`, `docs/language/functions.md`, and `docs/appendix/keywords.md` for async/await language status and usage
  - Added canonical async patterns and anti-patterns to `MEMORY.md` (Future[T] return, await usage, common mistakes)
  - Renamed misleading check test `async_function_not_supported.kl` → `async_function_pass.kl`

### Success Criteria

- [x] Async functions parse/type-check with explicit, unambiguous signatures
- [x] Await works correctly across interpreter, VM, and native build paths
- [x] Async misuse surfaces actionable diagnostics
- [x] Test suite includes regression coverage for async/await semantics

---

## Milestone 7: Windows Support

**Objective:** Provide first-class Windows developer and runtime support across build, test, and tooling workflows.

**Status:** In Progress — compiler cross-platform changes complete, Windows testing pending

**Effort:** Medium-High | **Impact:** High | **Dependencies:** Milestones 4, 5

### Rationale for ordering

Windows before WebAssembly: lower effort builds momentum, fixes platform assumptions (paths, line endings, stdio) that would otherwise complicate the WebAssembly target, and expands the contributor base before tackling the harder milestone.

### Tasks

- [x] **7.1** Replace `std.posix` stdio handles with cross-platform IO helpers
  - All 7 files, 17 sites converted to `comptime if` Windows/POSIX branching
- [x] **7.2** Cross-platform temp directory and executable extension
  - `klar run` uses `%TEMP%` on Windows, `/tmp` on POSIX; appends `.exe` on Windows
- [x] **7.3** Platform-conditional process execution
  - Windows: `std.process.Child` path; POSIX: `fork`/`exec` path (unchanged)
  - Note: `args[0]` shows temp binary path on Windows (minor behavioral difference)
- [x] **7.4** Fix linker.zig comptime string concatenation bug
  - Replaced `"/OUT:" ++ output_file` with `std.fmt.allocPrint`
- [x] **7.5** Conditional `@cImport` in emit.zig
  - `sys/stat.h`, `dirent.h`, `errno.h` wrapped in platform conditional
  - Hardcoded fallback constants for Windows MSVC CRT
- [x] **7.6** Platform-aware errno accessor
  - `__error` (macOS), `_errno` (Windows), `__errno_location` (Linux)
- [x] **7.7** Platform-aware POSIX C function declarations
  - Simple renames: `_access`, `_stat64`, `_rmdir`, `_unlink`
  - `_mkdir` (1 param vs 2), callers updated
  - `_findfirst64`/`_findnext64`/`_findclose` with full Windows `emitFsReadDir` path
  - `emitFsCreateDirAll` handles both `/` and `\` separators on Windows
- [x] **7.8** Object file extension: `.obj` on Windows, `.o` on POSIX
- [x] **7.9** Build system: Windows LLVM detection paths added
- [x] **7.10** LSP: Windows drive letter handling in `uriToPath`
- [x] **7.11** Path separator: `shouldSkipPath` handles both `/` and `\`
- [ ] **7.12** Windows on-device testing (Parallels)
  - Verify `zig build`, `klar run`, `klar build`, `klar test`, `klar lsp`
- [ ] **7.13** CI: Add Windows matrix jobs for `./run-tests.sh`
- [ ] **7.14** Documentation: Windows setup/install guide

### Known Limitations

- Test scripts (`run-tests.sh`, etc.) require WSL or Git Bash on Windows
- `klar run` on Windows shows temp binary path as `args[0]` instead of source path
- Cross-compilation of filesystem operations to Windows from non-Windows is not supported (build on target platform)

### Success Criteria

- [x] Repository builds on macOS with zero regressions (667/667 tests pass)
- [ ] Repository builds and full test suite pass on supported Windows environments
- [ ] CLI and LSP workflows behave consistently with macOS/Linux
- [ ] Windows-specific path/stdio regressions are covered by tests
- [ ] Contributor docs include complete Windows development setup

---

## Milestone 8: WebAssembly Target

**Objective:** Add a WebAssembly compilation target for sandboxed execution and browser/edge integration.

**Status:** Planned

**Effort:** High | **Impact:** High | **Dependencies:** Milestone 5, Milestone 7 (platform abstractions)

### Tasks

- [ ] **8.1** Target and CLI surface
  - Add target selection for wasm output in `klar build`
  - Define output conventions (`.wasm`, optional host shim artifacts)
- [ ] **8.2** Codegen pipeline
  - Add/extend backend lowering for wasm-compatible IR
  - Map Klar primitives, control flow, and memory model to wasm constraints
  - Define ABI conventions for function exports/imports
- [ ] **8.3** Runtime and stdlib compatibility
  - Audit stdlib/runtime APIs for wasm-safe behavior
  - Gate unsupported APIs with clear compile-time errors
  - Provide minimal wasm runtime bindings for I/O-adjacent operations
- [ ] **8.4** Testing and fixtures
  - Add wasm smoke tests and golden fixtures
  - Add host-run integration tests (Node or wasmtime path)
- [ ] **8.5** Documentation and examples
  - Document wasm build/run workflows
  - Add browser and CLI host examples

### Success Criteria

- [ ] `klar build` can emit valid wasm modules for representative programs
- [ ] Core language features execute correctly under wasm target constraints
- [ ] Unsupported features fail with clear diagnostics
- [ ] CI includes wasm-target regression coverage

---

## Milestone 9: Self-hosting

**Objective:** Move Klar toward self-hosting by enabling the compiler front-end and selected tooling components to be implemented in Klar.

**Status:** Planned

**Effort:** Very High | **Impact:** Very High | **Dependencies:** Milestones 6, 7, 8

### Tasks

- [ ] **9.1** Self-hosting scope definition
  - Define phased target (bootstrap compiler subset vs full compiler)
  - Freeze minimal language/runtime requirements for compiler implementation in Klar
- [ ] **9.2** Bootstrap architecture
  - Design two-stage bootstrap flow (`zig-hosted klar` -> `klar-hosted klar`)
  - Define artifact compatibility and reproducibility checks between stages
- [ ] **9.3** Core compiler libraries in Klar
  - Port reusable front-end components (token stream utilities, AST helpers, diagnostics formatting)
  - Establish stable internal APIs for parser/checker/codegen layers
- [ ] **9.4** Incremental compiler porting
  - Port parser and semantic analysis modules in prioritized order
  - Keep feature parity gates and fallback paths during transition
- [ ] **9.5** Validation and parity testing
  - Add cross-compiler parity tests (output equivalence and diagnostic equivalence)
  - Add performance tracking for bootstrap stages
- [ ] **9.6** Tooling/docs/release strategy
  - Document bootstrap process and contributor workflow
  - Define transition criteria for when Klar-hosted compiler becomes default

### Success Criteria

- [ ] Klar can build a functional Klar compiler artifact through a documented bootstrap path
- [ ] Compiler behavior parity is validated against the Zig-hosted baseline
- [ ] Bootstrap process is reproducible in CI with deterministic outputs where feasible
- [ ] Documentation clearly defines maintenance model for self-hosted compiler evolution

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
