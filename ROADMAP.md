# Klar — Project Roadmap

> **"No ambiguity. No surprises."** — An AI-native application programming language.

For the language specification and design philosophy, see [DESIGN.md](DESIGN.md).
For the active milestone plan, see [PLAN.md](PLAN.md).

---

Klar is a compiled language targeting application-level programming (like C#/Go) with ownership-based memory safety, explicit types, and AI-optimized syntax. The compiler is implemented in Zig with LLVM codegen, a bytecode VM, and a tree-walking interpreter.

Current status: **Phase 6 complete.** All milestones done (1–10, M, Phase 6). **2,052 tests passing.**

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

## Phase 7: Standard Library & Ecosystem (Planned)

**Goal:** Build a production-quality standard library and package ecosystem.

| Area | Description |
|------|-------------|
| Collections | HashMap improvements, BTreeMap, Deque, PriorityQueue |
| I/O | Path manipulation, directory walking |
| Networking | TCP/UDP sockets beyond HTTP |
| Serialization | YAML, binary formats |
| Concurrency | Channel-based communication, thread pool |
| Package registry | Central package repository, versioned dependencies |
| Documentation site | Auto-generated API docs from doc comments |

## Phase 8: Production Readiness (Planned)

**Goal:** Polish for real-world adoption.

| Area | Description |
|------|-------------|
| Error messages | Rich diagnostics with source snippets and suggestions |
| Debugging | DWARF debug info improvements, debugger integration |
| Performance | Compilation speed, runtime benchmarks, optimization passes |
| Stability | Fuzzing, property-based testing of compiler |
| Platform support | Linux ARM64, Windows ARM64 |

## Phase 9: Advanced Language Features (Exploratory)

**Goal:** Evaluate and selectively adopt features that align with Klar's philosophy.

| Feature | Notes |
|---------|-------|
| Effect system | Algebraic effects for controlled side effects |
| Constrained decoding | Grammar spec for LLM-guided code generation (MoonBit-inspired) |
| Formal verification | Lightweight contracts and invariants (Nanolang-inspired) |
| Incremental compilation | Module-level caching for faster rebuilds |

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
