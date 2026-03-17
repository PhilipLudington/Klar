# Klar — Project Roadmap

> **"No ambiguity. No surprises."** — An AI-native application programming language.

For the language specification and design philosophy, see [DESIGN.md](DESIGN.md).
For the active milestone plan, see [PLAN.md](PLAN.md).

---

Klar is a compiled language targeting application-level programming (like C#/Go) with ownership-based memory safety, explicit types, and AI-optimized syntax. The compiler is implemented in Zig with LLVM codegen, a bytecode VM, and a tree-walking interpreter.

Current status: **All phases complete.** Phases 1–16 complete. Bootstrap achieved. Kira interop, production readiness, and advanced language features all done. Selfhost parser tech debt resolved. Windows process builtins implemented. Build caching added. 2137 tests pass.

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

## Phase 7: Standard Library for Self-Hosting ✅

**Status:** Complete (2026-03-06)

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
- [x] Module test (`test/module/file/main.kl`) — write/read-back, append, overwrite, write_lines round-trip

### 7.4: Integration & Selfhost Validation ✅
- [x] Integration test (`test/module/stdlib_integration/main.kl`) — path, dir, string_builder, file cross-module pipeline
- [x] Update `docs/README.md` table of contents to include file.kl, path.kl, dir.kl API references
- [x] No regressions in full test suite (`./run-tests.sh` passes)

### Phase 7 Readiness Gate
- [x] All four stdlib modules (string_builder, path, dir, file) have passing module tests
- [x] Integration test passes end-to-end
- [x] `./run-tests.sh` passes with no regressions
- [x] Selfhost compiler files can import and use these modules (smoke test)

---

## Phase 8: Self-Hosting Completion ✅

**Status:** Complete (2026-03-10)

**Goal:** Complete the bootstrap loop — the selfhost compiler compiles itself. Fixed-point bootstrap achieved: Stage 1, Stage 2, and Stage 3 produce byte-identical typed AST JSON.

See [PLAN.md](PLAN.md) for full detail.

### 8.1: Selfhost Frontend Output ✅
- [x] Typed AST JSON serialization format (`docs/design/typed-ast-format.md`)
- [x] Selfhost frontend emits typed AST for all test/native/ files (311/317 match, 98.1%)
- [x] `--typed-ast-input` flag in Zig backend produces identical binaries
- [x] Validation: pipeline comparison across test/native/ suite

### 8.2: Bootstrap Stage 1 ✅
- [x] Stage 1 binary (`build/selfhost_main`) built and verified
- [x] Stage 1 passes full E2E test suite (311/317 match, 2063/2063 tests)

### 8.3: Bootstrap Stage 2 (Multi-Module) ✅
- [x] Module discovery & topological sort (`selfhost/module_resolver.kl`)
- [x] Multi-module type checking with circular import support (two-pass)
- [x] Multi-module typed AST emission and loading
- [x] Stage 2 binary builds and runs via `scripts/run-bootstrap-stage2.sh`
- [x] Fixed-point achieved: Stage 1/2/3 produce identical output (SHA-256: `1593eec5...`)
- [x] Bootstrap documented in `docs/selfhost-bootstrap.md`

### Phase 8 Readiness Gate
- [x] Selfhost frontend emits typed AST accepted by Zig backend
- [x] Stage 1 binary passes full E2E suite
- [x] Stage 2 AST matches Stage 1 AST (bootstrap is stable)

---

## Phase 9: Standard Library & Ecosystem ✅

**Status:** Complete (2026-03-10)

**Goal:** Build a production-quality standard library and package ecosystem.

See [PLAN.md](PLAN.md) for full detail.

### 9.1: Collections ✅
- [x] BTreeMap (`stdlib/btree_map.kl`) — ordered key-value store, arena-based node storage
- [x] Deque (`stdlib/deque.kl`) — double-ended queue with ring buffer, 15 tests
- [x] PriorityQueue (`stdlib/priority_queue.kl`) — min-heap with array-backed binary heap, 16 tests

### 9.2: Networking ✅
- [x] UDP socket builtins: udp_bind, udp_send_to, udp_recv_from, udp_close + UdpSocket/UdpMessage structs
- [x] DNS resolution builtin: dns_lookup(hostname) -> Result#[string, IoError]
- [x] Module tests for UDP and DNS (test/module/net/main.kl, 8 tests)

### 9.3: Serialization ✅
- [x] YAML parser/stringify (`stdlib/yaml.kl`) — full parsing including block/flow collections, quoted strings, hex/octal/binary, special values
- [x] Module tests (test/module/yaml/main.kl, 39 tests)

### 9.4: Concurrency ✅
- [x] Channel builtins: channel_create#[T](capacity), Sender/Receiver, send/recv/close
- [x] Thread pool: ThreadPool.new(num_threads), pool.spawn(task_fn), pool.shutdown()
- [x] Module tests for channels (6 tests) and thread pool (5 tests)

### 9.5: Package Registry ✅
- [x] `klar add <package>[@version]` — fetches from registry, extracts to deps/, updates klar.json + klar.lock
- [x] `klar publish` — reads klar.json, uploads JSON archive to registry
- [x] Registry server (`tools/registry/main.kl`) — HTTP server with filesystem storage
- [x] 8 integration tests + 4 format tests

### 9.6: Documentation ✅
- [x] `klar doc` generates HTML from `///` doc comments — per-module pages with syntax-highlighted signatures
- [x] Index page with module listing and item counts
- [x] 17 module pages generated for stdlib

### Stretch Goals
- [x] Windows `process_spawn` via `CreateProcessA` + `CreatePipe` — all 4 builtins (spawn, poll, wait, read_stdout) implemented behind comptime Windows guard (Windows CI validation pending)

### Phase 9 Readiness Gate
- [x] All new collections have module tests passing (2089/2089 total tests)
- [x] Networking and serialization modules have module tests passing
- [x] `klar add` and `klar publish` work end-to-end against local registry
- [x] `klar doc` generates HTML for stdlib modules
- [x] `./run-tests.sh` passes with no regressions

---

## Phase 10: Kira Interop — Consume Manifests ✅
**Status:** Complete (2026-03-11)

**Goal:** Klar can read a Kira type manifest (JSON) and auto-generate the extern block, removing the manual copy-paste step.

Companion plan: `~/Fun/Kira/PLAN-interop.md` (Kira-side work).
Reference: [DESIGN.md](DESIGN.md) section "C Interoperability", [docs/advanced/ffi.md](docs/advanced/ffi.md).

### 10.1: Manifest Schema ✅
- [x] Define Kira type manifest JSON schema from Klar's perspective (function signatures, ADT definitions, type mappings)
- [x] Coordinate with Kira's `PLAN-interop.md` Phase 4 on agreed format

### 10.2: Manifest Parser ✅
- [x] Implement manifest parser in `src/interop/kira_manifest.zig` — 11 unit tests
- [x] Map Kira types to Klar FFI types (i32→i32, f64→f64, string→CStr, bool→Bool, void→void, ADTs→extern struct)

### 10.3: CLI Command ✅
- [x] `klar import-kira <manifest.json> -o <output.kl>` — generates `.kl` file with extern block and extern struct/enum definitions
- [x] Generated file passes `klar check` (valid Klar syntax)

### 10.4: Module Resolution ✅
- [x] Generated `.kl` file integrates with module resolution (`import kira_mylib` finds the generated file) — `import-kira` outputs to `deps/`, module resolver auto-discovers `deps/`, extern block functions exported from modules. 5 integration tests.

---

## Phase 11: Kira Interop — ADT Consumption ✅

**Status:** Complete (2026-03-12)

**Goal:** Klar can work with Kira algebraic data types through extern structs and pattern matching on tags.

### 11.1: Extern Enum Generation ✅
- [x] Generate `extern enum` for ADT tags (e.g., `extern enum ShapeTag: i32 { Circle = 0, Rectangle = 1 }`)

### 11.2: Extern Struct Generation ✅
- [x] Generate `extern struct` matching Kira's C struct layout (tag field + data union)
- [x] Unified struct uses largest variant as data field type; `estimateFieldSize`, `maxFieldAlignment`, `computeDataOffset`, `findLargestVariantIndex` compute correct C layout

### 11.3: Safe Wrappers ✅
- [x] Generate wrapper functions for ADT access (e.g., `fn as_circle(s: ref Shape) -> ?f64`)
- [x] Generate match-friendly helpers: `pub fn <type>_tag(s: ref <Type>) -> <Type>Tag`
- [x] Non-largest variants use `ptr_cast` + `offset` + `read` through unsafe block

### 11.4: Documentation ✅
- [x] `docs/advanced/kira-interop.md` — calling Kira functions, matching on ADTs, product/sum types, type mapping

### Phase 11 Readiness Gate
- [x] Klar can consume all Kira primitive types and ADTs
- [x] Tag-based matching works for sum types
- [x] Record types are accessible as extern structs
- [x] Documentation covers the full workflow

---

## Phase 12: Kira Interop — String Interop ✅

**Status:** Complete (2026-03-12)

**Goal:** Strings cross the Kira-Klar boundary safely with clear ownership.

### 12.1: String Wrappers ✅
- [x] Generate CStr wrappers for Kira string parameters (`.as_cstr()`) — `__raw_` prefix in extern block, wrapper accepts `string`
- [x] Generate string conversion for Kira string returns (`.to_string()` on `CStr` return)
- [x] `kira_free` extern declaration unconditionally generated since Phase 10

### 12.2: Ownership Documentation ✅
- [x] String ownership rules documented in `docs/advanced/kira-interop.md` — wrapper mechanics, raw access, ownership rules table

---

## Phase 13: Kira Interop — Build Integration ✅

**Status:** Complete (2026-03-12)

**Goal:** Klar's build process can automatically build Kira dependencies and link them.

### 13.1: Dependency Configuration ✅
- [x] `kira-dependencies` section in klar.json with `name` and `path` (or `git`/`ref` for remote)
- [x] `KiraDependency` struct in manifest.zig, `kira` source type in lockfile.zig

### 13.2: Build Orchestration ✅
- [x] During `klar build`, detect Kira dependencies, invoke `kira build --lib`, compile C to .o, run `klar import-kira`, link objects
- [x] Auto-import generated extern blocks (`deps/` auto-discovered by module resolver)
- [x] `klar clean` removes `build/` directory and `deps/kira_*.kl` interop files
- [x] Caching via mtime comparison skips rebuild when source unchanged

### 13.3: Documentation ✅
- [x] Build integration documented in `docs/advanced/kira-interop.md` — project layout, klar.json format, 6-step build pipeline, AI agent instructions

### Phase 13 Readiness Gate
- [x] A Klar project can declare a Kira dependency and build with zero manual steps
- [x] Type mapping covers all primitive types, strings, and ADTs
- [x] Memory safety is maintained across the boundary (string wrappers handle conversion safely)
- [x] An AI agent can follow the documentation to set up a cross-language project

---

## Phase 14: Kira Interop — Effect Awareness ✅

**Status:** Complete (2026-03-12)

**Goal:** Surface Kira's purity information so developers and AI agents know which imported functions are side-effect-free.

- [x] Consume `effect` boolean per function from Kira manifest (defaults to `false` for backward compat)
- [x] Emit `// pure` or `// effect` comments in generated extern blocks
- [x] Emit `meta pure` annotations on pure wrapper functions

### Kira Interop Cross-Project Dependencies

```
Kira Phase 0 (fix types)     ──→  Klar Phase 10 (consume manifest)
Kira Phase 1 (--lib build)   ──→  Klar Phase 13 (build integration)
Kira Phase 2 (ADT layout)    ──→  Klar Phase 11 (ADT consumption)
Kira Phase 3 (string conv)   ──→  Klar Phase 12 (string interop)
Kira Phase 4 (manifest JSON) ──→  Klar Phase 10 (consume manifest)
```

---

## Phase 15: Production Readiness ✅

**Status:** Complete (2026-03-14)

**Goal:** Polish for real-world adoption — better errors, debugging, performance, and stability.

### 15.1: Error Messages ✅
- [x] Source snippets in error output with caret (`^`) pointing to error column
- [x] "Did you mean?" suggestions for undefined variables and types (edit distance ≤ 2)
- [x] Type info in mismatch errors: "expected {type}, got {type}" via `types.typeToString`

### 15.2: Debugging ✅
- [x] DWARF debug info: `createAutoVariable`, `createParameterVariable`, `setDebugLoc` per statement
- [x] Source location mappings for all statement types (let, var, assignment, return, for, while, if, match)
- [x] Debugging guide in `docs/debugging.md` — lldb breakpoints, stepping, variable inspection

### 15.3: Compilation Performance ✅
- [x] Benchmark suite in `benchmarks/compile/` — 4 workloads, 3 runs each
- [x] Arena allocator for TypeChecker: check 10K lines 200ms→85ms (**57% faster**), 319 native tests 12.7s→4.0s (**69% faster**)

### 15.4: Runtime Performance ✅
- [x] Runtime benchmark suite: fibonacci, quicksort, string processing
- [x] LLVM `default<O2>` pass pipeline: fib40 645ms→347ms (**46% faster**), quicksort 62ms→39ms (**37% faster**)

### 15.5: Stability ✅
- [x] Fuzz the parser — seed corpus + 1M random inputs, found/fixed 1 lexer crash (unterminated escape OOB), 0 crashes remaining
- [x] Property-based tests for type checker — 10K random programs (valid + invalid), 0 crashes

### 15.6: Platform Support ✅
- [x] Linux ARM64: native CI job on `ubuntu-24.04-arm`, full test suite
- [x] Windows ARM64: cross-compilation CI job (`zig build -Dtarget=aarch64-windows`), build verification

### Phase 15 Readiness Gate
- [x] Error messages include source snippets for all type errors
- [x] `klar build -g` + lldb can step through a simple Klar program
- [x] Compilation benchmarks have baselines and ≥ 20% improvement (57% achieved)
- [x] Parser fuzzer runs 1M+ inputs with zero crashes
- [x] All CI platforms (x64 + ARM64) have CI jobs

## Phase 16: Advanced Language Features (Exploratory) ✅
**Status:** Complete (2026-03-15)

**Goal:** Evaluate and selectively adopt features that align with Klar's philosophy.

Each feature follows an evaluate-then-implement pattern. Evaluation may conclude with "not pursuing" — that is a valid outcome.

### 16.1: Effect System
- [x] Research: write design doc evaluating algebraic effects for Klar (syntax, semantics, interaction with ownership)
- [x] Decision: **No-go** — existing `meta pure` + `async fn`/`await` covers practical cases; effect systems add ~2000+ LOC checker complexity; annotation burden violates "explicitness earns its characters"
- [x] Instead: extended `meta pure` verification — builtin method purity checking, inout parameter rejection

### 16.2: Constrained Decoding
- [x] Research: write design doc for grammar spec format (GBNF, JSON schema, or custom) enabling LLM-guided Klar code generation
- [x] Decision: **No-go** — REPL verification loop + `klar check` (85ms) is strictly superior; cloud LLMs don't support custom grammars; Klar's context-sensitive struct literal parsing can't be expressed in CFG
- [x] Not implementing `klar grammar` — maintenance burden (~1000 LOC grammar spec) for marginal benefit

### 16.3: Formal Verification
- [x] Research: write design doc for lightweight contracts (preconditions, postconditions, invariants) — runtime checks vs static analysis
- [x] Decision: **Go** for `meta require` / `meta ensure` (runtime-checked contracts via meta system, ~400 LOC). **No-go** for struct invariants, static analysis, and new keywords.
- [x] Implement `meta require` / `meta ensure` in meta_validation.zig + codegen — postconditions auto-wrap all return paths

### 16.4: Incremental Compilation
- [x] Research: profile full rebuild to identify caching opportunities (AST, typed AST, object files)
- [x] Decision: **Conditional go** for object file caching; implementation deferred until build times exceed 10s. LLVM codegen is 92% of build time (3.4s of 3.7s). Caching .o files would reduce typical edits from 3.7s to ~200ms.
- [x] Whole-program .o caching with SHA-256 content-hash invalidation — `build/.cache/`, `--no-cache` flag, 47% faster -O2 rebuilds

### Phase 16 Readiness Gate
- [x] All four feature areas have documented go/no-go decisions with design docs
- [x] Approved features implemented: `meta pure` extended (16.1), `meta require`/`meta ensure` (16.3)
- [x] Design documents linked from docs/README.md
- [x] All 2135 tests pass with no regressions

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

### Backlog: Selfhost Parser Known Limitations ✅

**Status:** Complete (2026-03-15)

- [x] `parse_int_value`: overflow-safe string-based decimal arithmetic for hex/binary/octal
- [x] `process_string_escapes`: `\xNN` hex byte and `\u{NNNN}` Unicode escapes (both Zig and selfhost)
- [x] `final_expr` detection in `parse_block`: bool return from `parse_statement` replaces JSON prefix matching
- [x] `is_extern` exemption in mandatory return type check: dead code removed
