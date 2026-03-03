# Klar — Project Roadmap

> **"No ambiguity. No surprises."** — An AI-native application programming language.

For the language specification and design philosophy, see [DESIGN.md](DESIGN.md).
For the active milestone plan, see [PLAN.md](PLAN.md).

---

## Overview

Klar is a compiled language targeting application-level programming (like C#/Go) with ownership-based memory safety, explicit types, and AI-optimized syntax. The compiler is implemented in Zig with LLVM codegen, a bytecode VM, and a tree-walking interpreter.

Current status: **Phase 5, Milestone 9 (Self-Hosting)** in progress.

---

## Phase 1–3: Compiler Foundation ✅

**Status:** Complete

Built the full compilation pipeline from scratch:
- Lexer → Parser → AST → Type Checker → three backends (interpreter, bytecode VM, LLVM native)
- Ownership-based memory management (Rc/Arc, automatic drop)
- Basic types, structs, enums, closures, optionals, results
- 252x speedup for native vs VM

### Implementation Language: Zig

**Reasons:** Simple, fast, explicit. Powerful comptime. No hidden allocations. Fast compilation.

---

## Phase 4: Language Completion ✅

**Status:** Complete (13 milestones)

> **Goal:** Complete the Klar language with generics, traits, modules, and standard library.
>
> **Archive:** [docs/history/phase4-language-completion.md](docs/history/phase4-language-completion.md)

| # | Milestone | Status |
|---|-----------|--------|
| 1 | Generic type checking | ✅ |
| 2 | Generic structs and enums | ✅ |
| 3 | Trait definitions and implementations | ✅ |
| 4 | Trait bounds and method dispatch | ✅ |
| 5 | Trait inheritance | ✅ |
| 6 | Builtin traits (Eq, Ordered, Clone, Drop) | ✅ |
| 7 | Associated types | ✅ |
| 8 | Module system (imports) | ✅ |
| 9 | Standard library (List, Map, Set, String) | ✅ |
| 10 | Iterators and error handling (`?` operator) | ✅ |
| 11 | REPL and comptime | ✅ |
| 12 | FFI (including function pointers) | ✅ |
| 13 | Package manager, formatter, doc generator | ✅ |

---

## Phase 5: AI-Native Language Improvements

> **Goal:** Strengthen Klar's AI-native story across documentation, language features, testing, and tooling.
>
> **Inspiration:** [Nanolang](docs/design/nanolang-inspiration.md) · [MoonBit Semantic Sampler](docs/design/moonbit-semantic-sampler.md) · [DSPy](docs/design/dspy-opportunities.md)
>
> **Archive (Milestones 1–6, 8):** [docs/history/phase5-milestones.md](docs/history/phase5-milestones.md)

### Completed Milestones

| # | Milestone | Source | Status |
|---|-----------|--------|--------|
| 1 | LLM Reference File (MEMORY.md) | Nanolang | ✅ |
| 2 | Mandatory Function Return Types | MoonBit | ✅ |
| 3 | Inline Test Blocks (`test`) | Nanolang | ✅ |
| 4 | Structured Test Output (`--json`) | DSPy | ✅ |
| 5 | LSP and Incremental Type Checking | MoonBit | ✅ |
| 6 | Async/Await | — | ✅ |
| 8 | WebAssembly Target | — | ✅ |
| 10 | Unambiguous Generic Syntax (`#[T]`) | — | ✅ |

### In Progress

| # | Milestone | Status |
|---|-----------|--------|
| 7 | [Windows Support](#milestone-7-windows-support) | ✅ Complete |
| 9 | [Self-Hosting](#milestone-9-self-hosting) | **Active** — see [PLAN.md](PLAN.md) |

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

### Milestone 9: Self-Hosting

**Objective:** Implement the Klar compiler front-end (lexer through type checker) in Klar itself, enabling the language to compile its own compiler.

**Status:** Active — Phases 9A–9E complete. Frontend self-hosting achieved with full parity (284/284 checker tests, 254/258 E2E). Bootstrap validation passed. Stretch goal (9.13 Tooling) remaining.

**Scope:** Frontend only (lexer through type checker). The 33K-line LLVM codegen stays in Zig. The self-hosted frontend serializes AST/typed-AST for the Zig backend to consume.

**Estimated Total:** ~11,000–16,000 lines of Klar

> **Full detail and active tasks:** [PLAN.md](PLAN.md)

---

### Milestone 10: Unambiguous Generic Syntax (`#[T]`) ✅

**Status:** Complete

Eliminated syntactic ambiguity between generics and array indexing. `[` is **always** arrays; `#[` is **always** generics. Deleted `isTypeArgsFollowedByCall()` lookahead heuristic entirely. Updated all 139+ `.kl` source files, selfhost parser, and documentation.

**Design Decisions:**
1. Two tokens (`hash` + `l_bracket`), not one compound token — lexer stays simple
2. `#[T]` everywhere — declarations, type applications, method calls, casts
3. `isTypeArgsFollowedByCall()` deleted — the entire lookahead heuristic became unnecessary

---

## Future Phases

### Phase 6: Standard Library & Ecosystem (Planned)

**Goal:** Build a production-quality standard library and package ecosystem.

| Area | Description |
|------|-------------|
| Collections | HashMap improvements, BTreeMap, Deque, PriorityQueue |
| I/O | Buffered readers/writers, path manipulation, directory walking |
| Networking | TCP/UDP sockets, HTTP client |
| Serialization | JSON (beyond TOML), YAML, binary formats |
| Concurrency | Channel-based communication, thread pool, async I/O integration |
| Package registry | Central package repository, versioned dependencies |
| Documentation site | Auto-generated API docs from doc comments |

### Phase 7: Production Readiness (Planned)

**Goal:** Polish for real-world adoption.

| Area | Description |
|------|-------------|
| Error messages | Rich diagnostics with source snippets and suggestions |
| Debugging | DWARF debug info improvements, debugger integration |
| Performance | Compilation speed, runtime benchmarks, optimization passes |
| Stability | Fuzzing, property-based testing of compiler |
| Platform support | Linux ARM64, Windows ARM64 |

### Phase 8: Advanced Language Features (Exploratory)

**Goal:** Evaluate and selectively adopt features that align with Klar's philosophy.

| Feature | Notes |
|---------|-------|
| Effect system | Algebraic effects for controlled side effects |
| Constrained decoding | Grammar spec for LLM-guided code generation (MoonBit-inspired) |
| Formal verification | Lightweight contracts and invariants (Nanolang-inspired) |
| Incremental compilation | Module-level caching for faster rebuilds |

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
