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

**Phase 5A–5D + Async/Await + WebAssembly:** Milestones 1-6 and 8 complete. LLM reference, mandatory return types, inline tests, structured test output, LSP server, async/await, and WebAssembly target all working.

> **Phase 5 archive:** [docs/history/phase5-milestones.md](docs/history/phase5-milestones.md)

---

## Completed Milestones Summary

| # | Milestone | Status | Source |
|---|-----------|--------|--------|
| 1 | LLM Reference File (MEMORY.md) | Complete | Nanolang |
| 2 | Mandatory Function Return Types | Complete | MoonBit |
| 3 | Inline Test Blocks (`test`) | Complete | Nanolang |
| 4 | Structured Test Output (`--json`) | Complete | DSPy |
| 5 | LSP and Incremental Type Checking | Complete | MoonBit |
| 6 | Async/Await | Complete | — |
| 8 | WebAssembly Target | Complete | — |

---

## Milestone 7: Windows Support

**Objective:** Provide first-class Windows developer and runtime support across build, test, and tooling workflows.

**Status:** In Progress — compiler cross-platform changes complete (tasks 7.1–7.11), Windows testing pending

**Effort:** Medium-High | **Impact:** High

### Rationale

Windows before WebAssembly: lower effort builds momentum, fixes platform assumptions (paths, line endings, stdio) that would otherwise complicate the WebAssembly target, and expands the contributor base before tackling the harder milestone.

### Remaining Tasks

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

## Milestone 8: WebAssembly Target (Complete)

WebAssembly compilation target is fully working for wasm32 freestanding. `klar build --target wasm` emits valid `.wasm` modules; 6 smoke tests cover hello, arithmetic, structs, closures, generics, and control flow. Filesystem/readline builtins are gated with runtime traps. See [archive](docs/history/phase5-milestones.md#milestone-8-webassembly-target) for full detail.

**Remaining:** CI wasm-target regression coverage (deferred to Milestone 7.13 CI work).

---

## Milestone 9: Self-Hosting

**Objective:** Implement the Klar compiler front-end (lexer through type checker) in Klar itself, enabling the language to compile its own compiler.

**Status:** In Progress — 9.1-9.8 complete (lexer, AST, parser at full parity: 259/259 files, 789/789 tests; type system definitions). 9.9+ (type checker port) not started.

**Effort:** Very High | **Impact:** Very High | **Dependencies:** Milestones 6, 7, 8

### Scope Boundary

Self-hosting means **frontend only** (lexer through type checker). The 33K-line LLVM codegen stays in Zig. The self-hosted frontend serializes AST/typed-AST for the Zig backend to consume. This is the standard bootstrap strategy (Go, Rust, etc.).

### Estimated Total: ~11,000–16,000 lines of Klar

### Dependency Chain

```
9.1 → 9.2 → 9.3 → 9.4 → 9.6 → 9.7 → 9.8 → 9.9 → 9.10 → 9.11 → 9.12
                    9.5 ↗      ↗                                      9.13 ↗
```

---

### Phase 9A: Language Prerequisites

Language features needed in the Zig compiler before porting can begin.

#### 9.1 — String and Collection Primitives

**Effort:** Medium

Wire up low-level string operations and numeric parsing needed by a lexer/parser.

- [x] **9.1.1** `string.byte_at(i) -> u8` — access individual byte by index
- [x] **9.1.2** `string.byte_len() -> i32` — byte length (distinct from `len()` if char-aware)
- [x] **9.1.3** `string.substring(start, end) -> string` — byte-range substring
- [x] **9.1.4** `string.index_of(sub) -> ?i32` — find first occurrence of substring
- [x] **9.1.5** `string.from_byte(b: u8) -> string` — single-byte string construction
- [x] **9.1.6** Wire `parse_int(s) -> ?i64` and `parse_float(s) -> ?f64` to native backend

**Success Criteria:**
- [x] All six string primitives work across native backend
- [x] `parse_int` / `parse_float` return `None` on invalid input (no panics)

#### 9.2 — Data Structure Foundations

**Effort:** Medium

Fix known collection gaps that would block self-hosting data structures.

- [x] **9.2.1** Fix `List#[String]` drop — free individual string buffers before freeing list storage
- [x] **9.2.2** `List.set(i, v)` / `list[i] = v` assignment in native codegen
- [x] **9.2.3** `List.last() -> ?T` and `List.pop() -> ?T` in native codegen
- [x] **9.2.4** Validate deeply nested structures: `List#[List#[i32]]` basic creation validated

**Known Limitation:** `List#[List#[T]].drop()` leaks inner list buffers. Fixing this requires a general recursive element destructor, deferred to a future milestone. The self-hosting compiler can work around this by manually dropping inner lists before the outer list.

**Success Criteria:**
- [x] `List#[String]` can be created, mutated, and dropped without leaks
- [x] Index assignment compiles and executes correctly
- [x] Nested generic structures validated (basic creation; nested drop is a known limitation)

---

### Phase 9B: Bootstrap Infrastructure

#### 9.3 — Bootstrap Architecture

**Effort:** Low-Medium

Set up the directory structure, testing harness, and diagnostic commands for parity testing.

- [x] **9.3.1** Create `selfhost/` directory with stub files (`lexer.kl`, `parser.kl`, `ast.kl`, `types.kl`, `checker.kl`, `main.kl`)
- [x] **9.3.2** Add `klar dump-tokens <file>` command — output token stream as JSON for parity testing
- [x] **9.3.3** Add `klar dump-ast <file>` command — output AST as JSON for parity testing
- [x] **9.3.4** Create `scripts/run-selfhost-tests.sh` — runs parity tests between Zig and Klar frontends
- [x] **9.3.5** Document bootstrap process (Stage 0/1/2) in `docs/guides/self-hosting.md`

**Success Criteria:**
- [x] `klar dump-tokens` produces deterministic JSON for any valid `.kl` file
- [x] `klar dump-ast` produces deterministic JSON for any valid `.kl` file
- [x] `selfhost/` directory compiles (even if stubs produce no useful output yet)

---

### Phase 9C: Compiler Frontend Port

#### 9.4 — Self-Hosted Lexer

**Effort:** Medium (~500–700 lines Klar)

Port token definitions and lexer logic from `src/token.zig` + `src/lexer.zig`.

- [x] **9.4.1** Define `TokenKind` enum with all token variants
- [x] **9.4.2** Define `Token` struct with kind, lexeme, line, column
- [x] **9.4.3** Implement `Lexer` struct with `next_token() -> Token` method
- [x] **9.4.4** Keyword lookup via `Map#[string, TokenKind]`
- [x] **9.4.5** All operators, string/number/char literals, comments, location tracking
- [x] **9.4.6** Inline `test` blocks for lexer edge cases
- [x] **9.4.7** Parity tests: `selfhost/lexer.kl` output matches `klar dump-tokens` on test corpus

**Known Limitation:** Keyword lookup uses chained `if`/`else` comparisons instead of `Map#[string, TokenKind]` because Klar's `Map` doesn't yet support string keys with proper hashing. Functionally equivalent.

**Success Criteria:**
- [x] Lexer tokenizes all files in `test/native/` identically to Zig lexer
- [x] Lexer can tokenize its own source file

#### 9.5 — AST Definitions

**Effort:** Medium (~800–1000 lines Klar)

Define the full AST type hierarchy needed by the parser.

- [x] **9.5.1** Define `Expr` enum (literal, binary, unary, call, field access, index, closure, etc.)
- [x] **9.5.2** Define `Stmt` enum (let/var, return, if/else, while, for, loop, match, assignment, expression)
- [x] **9.5.3** Define `Decl` enum (function, struct, enum, trait, impl, import, test)
- [x] **9.5.4** Define `TypeExpr` enum (named, generic, optional, array, function, reference)
- [x] **9.5.5** Use typed i32 indices into flat `List#[T]` pools for recursive indirection (arena pattern — same as Zig's AST)
- [x] **9.5.6** Debug functions (`expr_kind_name`, `binary_op_name`, `unary_op_name`) for diagnostics
- [x] **9.5.7** Inline tests for AST construction, sentinels, and operator names

**Known Limitation:** Klar's value semantics mean `List#[T]` fields inside structs lose data on move. Variable-length children use `(start, count)` i32 ranges into flat "extra" lists managed as direct `var` variables by the parser. This is the same flat-arena pattern used by production compilers (Zig, Rust HIR).

**Success Criteria:**
- [x] AST types can represent every construct in the Klar language
- [x] All recursive structures use typed indices into arena lists — no raw pointers

#### 9.6 — Parser (Core Subset)

**Effort:** High (~1500–2000 lines Klar)

Implement a recursive descent parser for the core language without generics or traits.

- [x] **9.6.1** Pratt precedence expression parsing (all operators, grouping, calls, field access)
- [x] **9.6.2** Statements: `let`/`var`, `return`, `if`/`else`, `while`, `for`, `loop`, `match`, assignment
- [x] **9.6.3** Function declarations (with parameter types and return types)
- [x] **9.6.4** Struct and enum declarations (no generics yet)
- [x] **9.6.5** Type annotation parsing (named types, optionals, arrays, function types)
- [x] **9.6.6** Parity tests: parser AST output matches `klar dump-ast` on non-generic test files

**Success Criteria:**
- [x] Parser handles all `test/native/` files that don't use generics, traits, or imports
- [x] Error recovery produces partial AST (continue past first error)

#### 9.7 — Parser (Full Language)

**Effort:** High (~1500–2000 lines Klar)

Extend the parser to cover the complete Klar language.

- [x] **9.7.1** Generic type parameters on functions, structs, enums (`#[T]`, `#[T: Bound]`)
- [x] **9.7.2** Trait definitions and impl blocks (including trait bounds, associated types)
- [x] **9.7.3** Closures with explicit types and return
- [x] **9.7.4** Full pattern matching (enum variants, wildcards, nested patterns)
- [x] **9.7.5** Comptime blocks, comptime functions, comptime parameters
- [x] **9.7.6** FFI syntax (`extern fn`), test blocks, `shadow` keyword
- [x] **9.7.7** Import statements (all variants: selective, glob, aliased)
- [x] **9.7.8** Full parity: identical AST output on entire `test/native/` suite (259/259 files)
- [x] **9.7.9** Self-parse: parser can parse its own source files

**Success Criteria:**
- [x] 100% parity with Zig parser on all test files
- [x] Parser can parse `selfhost/*.kl` — i.e., it can parse itself

---

### Phase 9D: Type System Port

#### 9.8 — Type System Definitions

**Effort:** Medium (~600–800 lines Klar)

Port the type representation from `src/types.zig`.

- [x] **9.8.1** Define `KlarType` struct with `TypeKind` enum (44 variants) + `PrimitiveKind` (17 variants) mirroring `src/types.zig`
- [x] **9.8.2** Type equality checks (`type_eq_simple` for primitives/singletons; compound equality via payload index)
- [x] **9.8.3** Type compatibility/coercion checks (`can_widen_to`, `is_copy_type`, classification: `is_integer`, `is_float`, `is_numeric`, `is_signed`)
- [x] **9.8.4** Type printing (`type_to_string_simple` for primitives/singletons; compound display deferred to 9.9)
- [ ] **9.8.5** Type substitution for generic instantiation (deferred to 9.10 — requires working type checker)

**Design Notes:**
- `KlarType` is a struct (`kind: TypeKind, payload: i32`) rather than a tagged union, since Klar enums with struct payloads have value-semantics limitations
- Compound type data (ArrayData, FunctionData, StructData, etc.) are flat structs stored in List#[T] arenas, referenced by i32 index — same pattern as ast.kl
- Arena list management deferred to type checker (9.9); this module defines shapes and pure functions only
- 24 compound data structs defined: ArrayData, SliceData, TupleData, ResultData, FunctionData, ReferenceData, StructData, StructFieldData, EnumData, EnumVariantData, TraitData, TraitMethodData, AssocTypeData, TypeVarData, AppliedData, AssocTypeRefData, WrapperData, RangeData, MapData, ExternTypeData, ExternFnData, TypeId

**Known Limitation:** `?Enum` optional returns are broken in the interpreter (test runner), so inline `test` blocks avoid functions that return `?PrimitiveKind`. Full coverage runs via `klar run` (native/VM backend).

**Success Criteria:**
- [x] `KlarType` can represent every type the Zig checker produces (44 TypeKind variants + 17 PrimitiveKind variants)
- [x] Type equality matches Zig checker behavior for primitives and singletons
- [x] All 1,468 tests pass (zero regressions)

#### 9.9 — Type Checker (Foundation)

**Effort:** Very High (~2000–3000 lines Klar)

Port the core type checking logic from `src/checker.zig`.

- [ ] **9.9.1** Scope management via `List#[Map#[string, KlarType]]` stack
- [ ] **9.9.2** Expression typing (literals, binary ops, unary ops, calls, field access, indexing)
- [ ] **9.9.3** Function call type checking (argument count, parameter types, return type)
- [ ] **9.9.4** Declaration checking (let/var type annotation matching)
- [ ] **9.9.5** Control flow checking (if/else arm types, loop/while body, match exhaustiveness)
- [ ] **9.9.6** Struct and method resolution (field access, method calls, `self` types)
- [ ] **9.9.7** Error reporting with source spans (file, line, column)

**Success Criteria:**
- [ ] Checker accepts all valid non-generic `test/native/` files
- [ ] Checker rejects all `test/check/` negative test files with correct error messages
- [ ] Diagnostics include file:line:column spans

#### 9.10 — Type Checker (Advanced)

**Effort:** Very High (~2000–3000 lines Klar)

Port the advanced type system features.

- [ ] **9.10.1** Generic monomorphization (type parameter inference, substitution, caching)
- [ ] **9.10.2** Trait resolution (trait bounds checking, method dispatch through bounds)
- [ ] **9.10.3** Optional/Result type checking, `?` propagation operator
- [ ] **9.10.4** Module import resolution (multi-file type checking)
- [ ] **9.10.5** Builtin function and method type checking (all 100+ builtins)
- [ ] **9.10.6** Full diagnostic parity with Zig checker

**Success Criteria:**
- [ ] Checker produces identical accept/reject decisions on entire test suite
- [ ] Error messages match Zig checker output (content, not necessarily formatting)
- [ ] Monomorphization produces identical instantiation sets

---

### Phase 9E: Integration and Bootstrap

#### 9.11 — Frontend Integration

**Effort:** Medium

Wire the self-hosted frontend components together and connect to the Zig backend.

- [ ] **9.11.1** Wire lexer → parser → checker pipeline in `selfhost/main.kl`
- [ ] **9.11.2** AST serialization to JSON for Zig backend consumption
- [ ] **9.11.3** Add `klar build --ast-input <file>` to Zig compiler (read serialized AST, skip Zig frontend)
- [ ] **9.11.4** End-to-end test: Klar frontend + Zig backend produces correct binaries
- [ ] **9.11.5** Performance comparison: Klar frontend vs Zig frontend on representative files

**Success Criteria:**
- [ ] `selfhost/main.kl` can process any `.kl` file and produce JSON AST
- [ ] Zig backend consumes serialized AST and produces identical binaries
- [ ] End-to-end pipeline passes full test suite

#### 9.12 — Bootstrap Validation (Stage 2)

**Effort:** High

Prove the self-hosted compiler can compile itself.

- [ ] **9.12.1** Stage 1: Zig-compiled Klar frontend compiles `selfhost/*.kl` source
- [ ] **9.12.2** Stage 2: Stage-1 binary compiles same `selfhost/*.kl` source again
- [ ] **9.12.3** Verify functional equivalence: Stage-1 and Stage-2 outputs are identical
- [ ] **9.12.4** Add CI job for bootstrap validation (Stage 0 → 1 → 2)
- [ ] **9.12.5** Document the bootstrap process and contributor workflow

**Success Criteria:**
- [ ] Stage-2 binary produces bit-identical AST output as Stage-1 binary
- [ ] Bootstrap is reproducible in CI with deterministic outputs
- [ ] Documentation clearly explains the three stages

#### 9.13 — Tooling Self-Hosting (Stretch)

**Effort:** Medium

Port selected tooling components to Klar.

- [ ] **9.13.1** Port formatter to Klar (AST pretty-printer)
- [ ] **9.13.2** Port diagnostics renderer to Klar (error message formatting)
- [ ] **9.13.3** Port test runner to Klar (test discovery and execution)
- [ ] **9.13.4** Parity tests against Zig implementations

**Success Criteria:**
- [ ] Self-hosted formatter produces identical output to Zig formatter
- [ ] Self-hosted test runner passes its own tests

---

## Milestone 10: Unambiguous Generic Syntax (`[T]` → `#[T]`)

**Objective:** Eliminate the syntactic ambiguity between generics (`[T]`) and array indexing (`[i]`). After this change, `[` is **always** arrays and `#[` is **always** generics — parseable at a glance with zero disambiguation heuristics.

**Status:** Complete

**Effort:** Medium | **Impact:** High (language design, philosophy alignment)

### Motivation

The current `[T]` syntax requires the parser to use uppercase heuristics and lookahead (`isTypeArgsFollowedByCall`) to disambiguate `foo[Bar]` (generics vs indexing). This violates Klar's core principle: **"No ambiguity. No surprises."**

```klar
// Before                              // After
fn max[T: Ordered](a: T, b: T)        fn max#[T: Ordered](a: T, b: T)
struct Pair[A, B] { ... }             struct Pair#[A, B] { ... }
let list: List[i32] = ...             let list: List#[i32] = ...
List.new[i32]()                       List.new#[i32]()
value.as[f64]                         value.as#[f64]

// Unchanged (arrays, not generics)
let arr: [i32; 3] = [1, 2, 3]
let x: i32 = arr[0]
```

### Design Decisions

1. **Two tokens (`hash` + `l_bracket`), not one compound token** — lexer stays simple
2. **`#[T]` everywhere** — declarations, type applications, method calls, casts
3. **`isTypeArgsFollowedByCall()` deleted** — the entire lookahead heuristic becomes unnecessary

### Tasks

#### Phase 1: Zig Compiler — Lexer

- [x] **10.1** Add `hash` token to `src/token.zig` (Kind enum + lexeme)
- [x] **10.2** Handle `#` in `src/lexer.zig` main switch → `self.makeToken(.hash)`
- [x] **10.3** Add lexer tests for `#` and `#[` sequences

#### Phase 2: Zig Compiler — Parser

- [x] **10.4** `parseTypeParams()` — match `hash` then `l_bracket` (was just `l_bracket`)
- [x] **10.5** `parseIndexOrTypeArgs()` — `l_bracket` is now ALWAYS indexing; remove type-args branch
- [x] **10.6** Add `hash` handling in infix expression parsing for `expr#[T](...)` calls
- [x] **10.7** Delete `isTypeArgsFollowedByCall()` and `canStartType()` — no longer needed
- [x] **10.8** `parseFieldOrMethod()` — `hash` instead of `l_bracket + canStartType` heuristic
- [x] **10.9** `parseTypeCast()` (`.as#[T]`) — expect `hash` before `l_bracket`
- [x] **10.10** `parseFallibleConversion()` (`.to#[T]`) — expect `hash` before `l_bracket`
- [x] **10.11** `parseType()` generic application — match `hash` then `l_bracket`
- [x] **10.12** Expression context generic types — check `hash` not `l_bracket`
- [x] **10.13** Pattern context generic types — check `hash` not `l_bracket`
- [x] **10.14** Extern fn/enum generic errors — check `hash` not `l_bracket`

#### Phase 3: Update All .kl Source Files (~139 files)

- [x] **10.15** Update `test/native/*.kl` files
- [x] **10.16** Update `selfhost/*.kl` files (generic usage, not parser logic)
- [x] **10.17** Update `std/*.kl`, `examples/*.kl`, `scratch/*.kl`
- [x] **10.18** Update `test/fmt/*.kl`, `test/module/*.kl`, `test/app/*.kl`, `test/check/*.kl`, `test/args/*.kl`, `test/wasm/*.kl`

#### Phase 4: Build + Verify

- [x] **10.19** Build compiler: `./run-build.sh`
- [x] **10.20** Run full test suite: `./run-tests.sh` (all non-selfhost tests pass)
- [x] **10.21** Fix any failures (formatter, types.zig display, hash precedence)

#### Phase 5: Selfhost Parser

- [x] **10.22** Update `selfhost/lexer.kl` — add `Hash` token kind and `#` handler
- [x] **10.23** Update `selfhost/parser_decl.kl` — fn/struct/enum/impl type params expect `hash`
- [x] **10.24** Update `selfhost/parser_expr.kl` — delete `is_type_args_context()` and `parse_type_args_and_call()`; add `parse_generic_call()`
- [x] **10.25** Update `selfhost/parser_expr.kl` — `parse_field_or_method()` uses `hash` for method generics
- [x] **10.26** Update `selfhost/parser_type.kl` — `parse_named_type()` expects `hash`
- [x] **10.27** Update `selfhost/parser_expr.kl` — cast syntax (`.as#[T]`, `.to#[T]`, `.trunc#[T]`)

#### Phase 6: Selfhost Verify

- [x] **10.28** Run selfhost tests: `./scripts/run-selfhost-tests.sh` — 789/789 passed
- [x] **10.29** Fix any selfhost test failures — none needed

#### Phase 7: Documentation

- [x] **10.30** Update `CLAUDE.md` — Language Syntax Quick Reference
- [x] **10.31** Update `docs/` — all files referencing generic syntax
- [x] **10.32** Update project documentation

### Success Criteria

- [x] All test suites pass (native, unit, app, module, selfhost)
- [x] `isTypeArgsFollowedByCall()` and `canStartType` heuristic are deleted
- [x] `[` in expression context is ALWAYS parsed as indexing — no disambiguation
- [x] `#[` is ALWAYS parsed as generics — unambiguous at the token level
- [x] Documentation reflects new syntax throughout

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

---

## Backlog: Selfhost Parser Known Limitations

Deferred tech debt from the Milestone 9.6 selfhost parser work.

- [ ] `parse_int_value`: hex/binary/octal literals use i64 computation (overflow possible for values > i64 max)
- [ ] `process_string_escapes`: `\u` and `\x` escape sequences not handled (falls through to unknown escape)
- [ ] `final_expr` detection in `parse_block` uses fragile JSON prefix string matching (acknowledged by COUPLING comment)
- [ ] `is_extern` exemption in mandatory return type check is moot: Zig parser rejects standalone `extern fn`, so the selfhost exemption has no test coverage
