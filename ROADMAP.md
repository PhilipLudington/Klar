# Klar — Project Roadmap

> **"No ambiguity. No surprises."** — An AI-native application programming language.

For the language specification and design philosophy, see [DESIGN.md](DESIGN.md).
For the active milestone plan, see [PLAN.md](PLAN.md).

---

Klar is a compiled language targeting application-level programming (like C#/Go) with ownership-based memory safety, explicit types, and AI-optimized syntax. The compiler is implemented in Zig with LLVM codegen, a bytecode VM, and a tree-walking interpreter.

Current status: **Phase 7 in progress.** All milestones done (1–10, M, Phase 6).

---

## Phase 1–3: Compiler Foundation ✅

**Status:** Complete

- [x] Lexer → Parser → AST → Type Checker pipeline
- [x] Three backends: interpreter, bytecode VM, LLVM native
- [x] Ownership-based memory management (Rc/Arc, automatic drop)
- [x] Basic types, structs, enums, closures, optionals, results
- [x] 252x speedup for native vs VM

### Implementation Language: Zig

**Reasons:** Simple, fast, explicit. Powerful comptime. No hidden allocations. Fast compilation.

---

## Phase 4: Language Completion ✅

**Status:** Complete (13 milestones)

> **Goal:** Complete the Klar language with generics, traits, modules, and standard library.
>
> **Archive:** [docs/history/phase4-language-completion.md](docs/history/phase4-language-completion.md)

- [x] Generic type checking
- [x] Generic structs and enums
- [x] Trait definitions and implementations
- [x] Trait bounds and method dispatch
- [x] Trait inheritance
- [x] Builtin traits (Eq, Ordered, Clone, Drop)
- [x] Associated types
- [x] Module system (imports)
- [x] Standard library (List, Map, Set, String)
- [x] Iterators and error handling (`?` operator)
- [x] REPL and comptime
- [x] FFI (including function pointers)
- [x] Package manager, formatter, doc generator

---

## Phase 5: AI-Native Language Improvements ✅

> **Goal:** Strengthen Klar's AI-native story across documentation, language features, testing, and tooling.
>
> **Inspiration:** [Nanolang](docs/design/nanolang-inspiration.md) · [MoonBit Semantic Sampler](docs/design/moonbit-semantic-sampler.md) · [DSPy](docs/design/dspy-opportunities.md)
>
> **Archive (Milestones 1–6, 8):** [docs/history/phase5-milestones.md](docs/history/phase5-milestones.md)

### Completed Milestones

- [x] LLM Reference File (Nanolang-inspired)
- [x] Mandatory Function Return Types (MoonBit-inspired)
- [x] Inline Test Blocks (`test`) (Nanolang-inspired)
- [x] Structured Test Output (`--json`) (DSPy-inspired)
- [x] LSP and Incremental Type Checking (MoonBit-inspired)
- [x] Async/Await
- [x] [Windows Support](#milestone-7-windows-support)
- [x] [WebAssembly Target](#milestone-10-unambiguous-generic-syntax-t)
- [x] [Self-Hosting](#milestone-9-self-hosting) (paused at 9.8)
- [x] [Unambiguous Generic Syntax (`#[T]`)](#milestone-10-unambiguous-generic-syntax-t)
- [x] [Meta Layer](#milestone-m-meta-layer)

### Standard Library (Lodex Infrastructure) ✅

**Status:** Complete — Phases 0–6 done. See [PLAN.md](PLAN.md) for full detail.

Built general-purpose stdlib libraries needed by [Lodex](../Lodex/DESIGN.md):

- [x] Foundational builtins: env, fs_stat, process_run, timestamp
- [x] JSON parser/stringify (`stdlib/json.kl`)
- [x] SHA-256 hashing, FIPS 180-4 (`stdlib/sha256.kl`)
- [x] TOML parser/stringify (`stdlib/toml.kl`)
- [x] CLI argument parsing (`stdlib/cli.kl`)
- [x] Integration validation (`test/module/integration/`)
- [x] Async I/O and HTTP ([Phase 6](#phase-6-async-io-and-http-))

---

### Milestone 7: Windows Support ✅

**Objective:** First-class Windows developer and runtime support across build, test, and tooling workflows.

**Status:** Complete

- [x] **7.1–7.11** Compiler cross-platform changes
- [x] **7.12** Windows on-device testing (CI runs on `windows-latest` with MSVC + LLVM)
- [x] **7.13** CI: Windows matrix job in `.github/workflows/ci.yml`
- [x] **7.14** Documentation: Windows setup guide in `docs/getting-started/installation.md`

**Known Limitations:**
- Test scripts (`run-tests.sh`, etc.) require WSL or Git Bash on Windows
- `klar run` on Windows shows temp binary path as `args[0]` instead of source path
- Cross-compilation of filesystem operations to Windows from non-Windows is not supported

**Success Criteria:**
- [x] Repository builds on macOS with zero regressions
- [x] Repository builds and full test suite pass on supported Windows environments
- [x] CLI and LSP workflows behave consistently with macOS/Linux
- [x] Windows-specific path/stdio regressions are covered by tests
- [x] Contributor docs include complete Windows development setup

---

### Milestone M: Meta Layer ✅

**Objective:** Implement the `meta` keyword system for embedding intent, architecture, and design decisions in Klar source code — compiler-validated, CLI-queryable, zero runtime cost.

**Status:** Complete (M.1–M.8)

- [x] Token, AST, and declaration fields (M.1)
- [x] Simple annotation parsing: intent, decision, tag, hint, deprecated, pure (M.2)
- [x] Block and group parsing: module, guide, related, group def/join (M.3)
- [x] Backend passthrough and formatter (M.4)
- [x] Checker validation: related targets, group refs, deprecation warnings, scope rules (M.5)
- [x] Pure verification: side-effect analysis with transitive purity checking (M.6)
- [x] Custom annotations: `meta define`, string union constraints, scope restrictions, cross-module import (M.7)
- [x] `klar meta` CLI command: --tag, --module, --related, --deprecated, --hints, --json (M.8)

**Design Spec:** [docs/design/meta-layer.md](docs/design/meta-layer.md)

---

### Milestone 9: Self-Hosting ✅ (paused at 9.8)

**Objective:** Implement the Klar compiler front-end (lexer through type checker) in Klar itself, enabling the language to compile its own compiler.

**Status:** Paused — 9.1–9.8 complete. Lexer, AST, parser at full parity (259/259 files, 789/789 tests). Type system definitions done. Checker parity 284/284 (100%). E2E pipeline 258/258 (100%). Bootstrap validation passed (Stage 1 self-parsing). 9.9+ paused pending further work.

**Scope:** Frontend only (lexer through type checker). The LLVM codegen stays in Zig. The self-hosted frontend serializes AST/typed-AST for the Zig backend to consume.

**Estimated Total:** ~11,000–16,000 lines of Klar

---

### Milestone 10: Unambiguous Generic Syntax (`#[T]`) ✅

**Status:** Complete

Eliminated syntactic ambiguity between generics and array indexing. `[` is **always** arrays; `#[` is **always** generics. Deleted `isTypeArgsFollowedByCall()` lookahead heuristic entirely. Updated all 139+ `.kl` source files, selfhost parser, and documentation.

**Design Decisions:**
1. Two tokens (`hash` + `l_bracket`), not one compound token — lexer stays simple
2. `#[T]` everywhere — declarations, type applications, method calls, casts
3. `isTypeArgsFollowedByCall()` deleted — the entire lookahead heuristic became unnecessary

---

## Phase 6: Async I/O and HTTP ✅

**Status:** Complete (2026-03-05)

**Goal:** Build async I/O and HTTP capabilities for Lodex's evaluation engine and HTTP API.

### 6A: Async Subprocess
- [x] `process_spawn`, `process_poll`, `process_wait`, `process_read_stdout` builtins
- [x] Non-blocking subprocess execution via `posix_spawn` + pipe-based stdout capture

### 6B: TCP Sockets
- [x] `tcp_listen`, `tcp_accept`, `tcp_connect`, `tcp_read`, `tcp_write`, `tcp_close` builtins
- [x] `tcp_set_nonblocking`, `tcp_listener_close` builtins
- [x] Platform-specific constants (macOS vs Linux)

### 6C: HTTP Server
- [x] `stdlib/http_server.kl` — pure Klar HTTP server built on TCP builtins
- [x] Request routing, JSON request/response

### 6D: HTTP Client
- [x] `stdlib/http_client.kl` — pure Klar HTTP client built on TCP builtins
- [x] GET/POST requests, response parsing

## Phase 7: Standard Library for Self-Hosting (In Progress)

**Goal:** Build the stdlib modules required to complete the self-hosting bootstrap (Phase 8).

See [PLAN.md](PLAN.md) for full detail.

### 7.0: StringBuilder ✅
- [x] `stdlib/string_builder.kl` — efficient string building via `List#[string]` backing store
- [x] Module test (15 tests)

### 7.1: Path Manipulation ✅
- [x] `stdlib/path.kl` — path join, parent, file_name, extension, stem, is_absolute, normalize
- [x] Module test (48 tests)

### 7.2: Directory Walking ✅
- [x] `stdlib/dir.kl` — dir_list, dir_list_ext, dir_walk, dir_walk_ext
- [x] Module test (21 tests)

### 7.3: File Writing ✅
- [x] `stdlib/file.kl` — `file_write(path, content)`, `file_write_lines(path, lines)`, `file_append(path, content)`
- [x] Module test (`test/module/file/main.kl`) — write/read-back, append, overwrite, write_lines round-trip (follow pattern of 7.0–7.2 tests)

### 7.4: Integration & Selfhost Validation
- [ ] Integration test (`test/module/stdlib_integration/main.kl`) — use path.kl to construct paths, dir.kl to walk directories, string_builder.kl to build content, file.kl to write output, then read back and verify
- [x] Update `docs/README.md` table of contents to include file.kl, path.kl, dir.kl API references
- [ ] No regressions in full test suite (`./run-tests.sh` passes)

### Phase 7 Readiness Gate
Before Phase 8, these must be true:
- [x] All four stdlib modules (string_builder, path, dir, file) have passing module tests
- [ ] Integration test passes end-to-end
- [ ] `./run-tests.sh` passes with no regressions
- [ ] Selfhost compiler files can import and use these modules (smoke test)

---

## Phase 8: Self-Hosting Completion (Planned)

**Goal:** Complete the bootstrap loop — the selfhost compiler compiles itself.

Resumes from Milestone 9.8 (paused in Phase 5). The selfhost frontend (lexer, parser, checker) is at full parity (284/284 checker, 258/258 E2E).

### 8.1: Selfhost Frontend Output
- [ ] Define serialization format for typed AST (JSON schema or binary) consumable by Zig codegen backend
- [ ] Selfhost frontend (`selfhost/`) emits typed AST for all test/native/ files
- [ ] Zig backend (`src/codegen/`) accepts `--typed-ast-input` and produces identical binaries to the standard pipeline
- [ ] Validation: compile test/native/ files via both pipelines, compare exit codes and stdout

### 8.2: Bootstrap Stage 1
- [ ] Selfhost frontend compiles its own source files (lexer.kl, parser.kl, checker.kl, etc.) and emits typed AST
- [ ] Zig backend consumes that AST and produces a working selfhost binary (Stage 1 binary)
- [ ] Stage 1 binary passes the same E2E test suite as the Zig-compiled selfhost

### 8.3: Bootstrap Stage 2 (Reproducibility)
- [ ] Stage 1 binary compiles selfhost source → produces Stage 2 typed AST
- [ ] Stage 2 typed AST matches Stage 1 typed AST (bit-for-bit or normalized comparison)
- [ ] Document the bootstrap process in docs/

### Phase 8 Readiness Gate
Before Phase 9, these must be true:
- [ ] Selfhost frontend emits typed AST accepted by Zig backend
- [ ] Stage 1 binary passes full E2E suite
- [ ] Stage 2 AST matches Stage 1 AST (bootstrap is stable)

---

## Phase 9: Standard Library & Ecosystem (Planned)

**Goal:** Build a production-quality standard library and package ecosystem.

### 9.1: Collections
- [ ] BTreeMap with ordered iteration, insert, get, remove
- [ ] Deque (double-ended queue) with push_front, push_back, pop_front, pop_back
- [ ] PriorityQueue (min-heap) with push, pop, peek
- [ ] Module tests for each collection

### 9.2: Networking
- [x] UDP socket builtins: udp_bind, udp_send_to, udp_recv_from, udp_close + UdpSocket/UdpMessage structs
- [x] DNS resolution builtin: dns_lookup(hostname) -> Result#[string, IoError]
- [x] Module tests for UDP and DNS (test/module/net/main.kl, 8 tests)

### 9.3: Serialization
- [ ] YAML parser/stringify (`stdlib/yaml.kl`)
- [ ] Module tests for YAML

### 9.4: Concurrency
- [ ] Channel-based communication: channel#[T](), Sender, Receiver (as specified in DESIGN.md)
- [ ] Thread pool: spawn tasks across worker threads
- [ ] Module tests for channels and thread pool

### 9.5: Package Registry
- [ ] `klar add <package>` fetches from a registry URL
- [ ] `klar publish` uploads package to registry
- [ ] deps.lock file generation and resolution
- [ ] Registry server (minimal HTTP API for package upload/download)

### 9.6: Documentation Site
- [ ] `klar doc` generates HTML from doc comments (extend existing doc generator)
- [ ] Index page with module listing and search

### Stretch Goals
- [ ] Windows `process_spawn` via `CreateProcessW` (currently POSIX-only)

### Phase 9 Readiness Gate
Before Phase 10, these must be true:
- [ ] All new collections have module tests passing
- [ ] Networking and serialization modules have module tests passing
- [ ] `klar add` and `klar publish` work end-to-end against registry
- [ ] `./run-tests.sh` passes with no regressions

---

## Phase 10: Kira Interop — Consume Manifests (Planned)

**Goal:** Klar can read a Kira type manifest (JSON) and auto-generate the extern block, removing the manual copy-paste step.

Companion plan: `~/Fun/Kira/PLAN-interop.md` (Kira-side work).
Reference: [DESIGN.md](DESIGN.md) section "C Interoperability", [docs/advanced/ffi.md](docs/advanced/ffi.md).

### 10.1: Manifest Schema
- [ ] Define Kira type manifest JSON schema from Klar's perspective (function signatures, ADT definitions, type mappings)
- [ ] Coordinate with Kira's `PLAN-interop.md` Phase 4 on agreed format

### 10.2: Manifest Parser
- [ ] Implement manifest parser in `src/main.zig` or `src/interop/` module
- [ ] Map Kira types to Klar FFI types (i32→i32, f64→f64, string→CStr, bool→Bool, void→void, ADTs→extern struct)

### 10.3: CLI Command
- [ ] `klar import-kira <manifest.json> -o <output.kl>` — generate `.kl` file with extern block and extern struct/enum definitions

### 10.4: Module Resolution
- [ ] Generated `.kl` file integrates with module resolution (`import kira_mylib` finds the generated file)

---

## Phase 11: Kira Interop — ADT Consumption (Planned)

**Goal:** Klar can work with Kira algebraic data types through extern structs and pattern matching on tags.

### 11.1: Extern Enum Generation
- [ ] Generate `extern enum` for ADT tags (e.g., `extern enum ShapeTag: i32 { Circle = 0, Rectangle = 1 }`)

### 11.2: Extern Struct Generation
- [ ] Generate `extern struct` matching Kira's C struct layout (tag field + data union)

### 11.3: Safe Wrappers
- [ ] Generate wrapper functions for ADT access (e.g., `fn as_circle(shape: ref Shape) -> ?f64`)
- [ ] Generate match-friendly helpers returning tag enum values

### 11.4: Documentation
- [ ] Write `docs/advanced/kira-interop.md` — calling Kira functions, matching on ADTs

### Phase 11 Readiness Gate
Before Phase 12, these must be true:
- [ ] Klar can consume all Kira primitive types and ADTs
- [ ] Tag-based matching works for sum types
- [ ] Record types are accessible as extern structs
- [ ] Documentation covers the full workflow

---

## Phase 12: Kira Interop — String Interop (Planned)

**Goal:** Strings cross the Kira-Klar boundary safely with clear ownership.

### 12.1: String Wrappers
- [ ] Generate CStr wrappers for Kira string parameters (`.as_cstr()`)
- [ ] Generate string conversion for Kira string returns (`.to_string()`)
- [ ] Generate `kira_free` extern declaration for Kira-allocated memory

### 12.2: Ownership Documentation
- [ ] Document string ownership rules in `docs/advanced/kira-interop.md` (borrow for calls, own after conversion)

---

## Phase 13: Kira Interop — Build Integration (Planned)

**Goal:** Klar's build process can automatically build Kira dependencies and link them.

### 13.1: Dependency Configuration
- [ ] Define Kira dependency format in Klar's build configuration (`[[deps.kira]]` with `name` and `path`)

### 13.2: Build Orchestration
- [ ] During `klar build`, detect Kira dependencies, invoke `kira build --lib`, copy `.a` and `.kl` files, add `-l` flags
- [ ] Auto-import generated extern blocks (user writes `import kira_mylib` without managing files)
- [ ] `klar clean` removes interop artifacts (generated `.kl`, `.h`, `.a` files)

### 13.3: Documentation
- [ ] Document build integration in `docs/advanced/kira-interop.md` (project layout, config format, AI agent instructions)

### Phase 13 Readiness Gate
Before Phase 14, these must be true:
- [ ] A Klar project can declare a Kira dependency and build with zero manual steps
- [ ] Type mapping covers all primitive types, strings, and ADTs
- [ ] Memory safety is maintained across the boundary (no leaks, no use-after-free)
- [ ] An AI agent can follow the documentation to set up a cross-language project

---

## Phase 14: Kira Interop — Effect Awareness (Stretch)

**Goal:** Surface Kira's purity information so developers and AI agents know which imported functions are side-effect-free.

- [ ] Consume `effect` boolean per function from Kira manifest
- [ ] Emit `// pure` or `// effect` comments in generated extern blocks
- [ ] (Optional) Emit `@meta(pure)` annotations if Klar's meta system supports custom annotations

### Kira Interop Cross-Project Dependencies

```
Kira Phase 0 (fix types)     ──→  Klar Phase 10 (consume manifest)
Kira Phase 1 (--lib build)   ──→  Klar Phase 13 (build integration)
Kira Phase 2 (ADT layout)    ──→  Klar Phase 11 (ADT consumption)
Kira Phase 3 (string conv)   ──→  Klar Phase 12 (string interop)
Kira Phase 4 (manifest JSON) ──→  Klar Phase 10 (consume manifest)
```

Critical path: Kira Phase 0 → Kira Phase 4 → Klar Phase 10 → Klar Phases 11–13 (parallel with remaining Kira phases as they complete).

---

## Phase 15: Production Readiness (Planned)

**Goal:** Polish for real-world adoption.

### 15.1: Error Messages
- [ ] Source snippets in error output: show the offending line with underline caret pointing to error location
- [ ] "Did you mean X?" suggestions for undefined variables and typos (edit distance matching)
- [ ] Multi-line error context for type mismatch errors (show expected vs actual with source)

### 15.2: Debugging
- [ ] Verify `-g` flag produces correct DWARF debug info for lldb: stepping, breakpoints, variable inspection
- [ ] Fix any missing or incorrect source location mappings in LLVM codegen
- [ ] Document debugging workflow in docs/ (lldb commands for Klar binaries)

### 15.3: Compilation Performance
- [ ] Benchmark compilation speed on selfhost source files, establish baseline
- [ ] Profile and optimize slowest compilation phases (parser, checker, or codegen)

### 15.4: Runtime Performance
- [ ] Establish runtime benchmark suite (fibonacci, sorting, string processing)
- [ ] Add at least one new LLVM optimization pass or improve existing pass pipeline

### 15.5: Stability
- [ ] Fuzz the parser with AFL or libFuzzer — no crashes on arbitrary input
- [ ] Property-based tests for type checker (random valid programs type-check successfully)

### 15.6: Platform Support
- [ ] Linux ARM64: CI job, full test suite passing
- [ ] Windows ARM64: CI job, full test suite passing

### Phase 15 Readiness Gate
Before Phase 16, these must be true:
- [ ] Error messages include source snippets for all type errors
- [ ] `klar build -g` + lldb can step through a simple Klar program
- [ ] Parser fuzzer runs 1M+ inputs with zero crashes
- [ ] All CI platforms (x64 + ARM64) pass full test suite

## Phase 16: Advanced Language Features (Exploratory)

**Goal:** Evaluate and selectively adopt features that align with Klar's philosophy.

Each feature follows an evaluate-then-implement pattern. Evaluation may conclude with "not pursuing" — that is a valid outcome.

### 16.1: Effect System
- [ ] Research: write design doc evaluating algebraic effects for Klar (syntax, semantics, interaction with ownership)
- [ ] Decision: go/no-go based on complexity vs value for application-level programming
- [ ] If go: implement effect declarations, handler syntax, and checker support

### 16.2: Constrained Decoding
- [ ] Research: write design doc for grammar spec format (GBNF, JSON schema, or custom) enabling LLM-guided Klar code generation
- [ ] Decision: go/no-go based on LLM tooling ecosystem compatibility
- [ ] If go: implement `klar grammar` CLI command that outputs the Klar grammar spec

### 16.3: Formal Verification
- [ ] Research: write design doc for lightweight contracts (preconditions, postconditions, invariants) — runtime checks vs static analysis
- [ ] Decision: go/no-go based on alignment with "no ambiguity, no surprises" philosophy
- [ ] If go: implement contract syntax and checker/runtime support

### 16.4: Incremental Compilation
- [ ] Research: profile full rebuild to identify caching opportunities (AST, typed AST, object files)
- [ ] Decision: go/no-go based on measured rebuild times vs implementation effort
- [ ] If go: implement module-level caching with invalidation based on file content hash

---

### References

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

### Backlog: Selfhost Parser Known Limitations

Deferred tech debt from the Milestone 9.6 selfhost parser work.

- [ ] `parse_int_value`: hex/binary/octal literals use i64 computation (overflow possible for values > i64 max)
- [ ] `process_string_escapes`: `\u` and `\x` escape sequences not handled (falls through to unknown escape)
- [ ] `final_expr` detection in `parse_block` uses fragile JSON prefix string matching (acknowledged by COUPLING comment)
- [ ] `is_extern` exemption in mandatory return type check is moot: Zig parser rejects standalone `extern fn`, so the selfhost exemption has no test coverage
