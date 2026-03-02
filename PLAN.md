# Klar Standard Library — Lodex Infrastructure Plan

## Overview
Build the Klar standard library capabilities that [Lodex](../Lodex/DESIGN.md) (AI-native source control) depends on. These features are general-purpose and benefit the entire Klar ecosystem, not just Lodex. See [Lodex PLAN.md Phase 0](../Lodex/PLAN.md) for the downstream requirements.
Current status: Phase 4 complete. Phase 5 (Integration Validation) is next.

## Parallel Workflow Strategy

This work runs in a dedicated worktree (`LodexNeeds`) alongside two active worktrees:
- **StartWorkOnMetaLayer** — modifies `parser.zig` (+580), `ast.zig` (+306), `checker.zig` (+209)
- **fizzy-nibbling-curry** — modifies `codegen/emit.zig` (+485)

To avoid merge conflicts:
- **Phase 0** (compiler builtins) adds name constants to `src/codegen/builtins.zig` and emission code to `emit.zig` (appended at end, minimal conflict surface)
- **Phases 1-4** are pure Klar libraries in `stdlib/` — zero compiler changes, zero conflict risk
- **Phase 5** integration test is a standalone `.kl` program — no compiler changes

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

**Status:** Complete — all tasks done, 422/422 Windows tests pass

**Effort:** Medium-High | **Impact:** High

### Rationale

Windows before WebAssembly: lower effort builds momentum, fixes platform assumptions (paths, line endings, stdio) that would otherwise complicate the WebAssembly target, and expands the contributor base before tackling the harder milestone.

### Completed Tasks

- [x] **7.1–7.11** Compiler cross-platform changes
- [x] **7.12** Windows on-device testing (Windows 11 Pro, Zig 0.15.2)
  - Build: `zig build` succeeds, produces 5.7MB `klar.exe` (VM-only mode, no LLVM)
  - Unit tests: 240/240 pass
  - Check tests: 61/61 pass
  - Formatter tests: 28/28 pass (after `.gitattributes` LF fix)
  - `klar run` (VM, interpreter): works
  - `klar check`, `klar test`, `klar fmt`: work
  - `klar lsp`: starts, responds to JSON-RPC
  - `klar build`: clear error message when LLVM unavailable
  - Native/App/Module tests: require LLVM (expected, not a blocker)
- [x] **7.14** Documentation: Windows setup/install guide (`docs/getting-started/installation.md`)

- [x] **7.13** CI: Add Windows matrix jobs
  - `.github/workflows/ci.yml`: macOS (full), Linux (full), Windows (full with vovkos LLVM)
  - `scripts/run-windows-tests.sh`: fallback runner for VM-only suites
  - LLVM-C linking, `@hasDecl` target init, `_stat64` offsets, `.Exited` PascalCase

### Fixes Applied During Windows Testing

- **Zig 0.15.2 `GetStdHandle` change**: Returns `?*anyopaque` (was `*anyopaque`); added `.?` unwrap at 18 call sites across 8 files
- **LLVM optional build**: Added `has_llvm` build option with `comptime` guards; compiler builds in VM-only mode when LLVM dev headers missing
- **CRLF line endings**: Created `.gitattributes` (`* text=auto eol=lf`) to prevent `core.autocrlf=true` from breaking formatter test comparisons
- **Windows errno**: Updated `emitGetErrno()` in `emit.zig` to use `_errno` on Windows (was `__errno_location`)
- **`statFile` on Windows**: `statFile` returns `error.IsDir` for directories on Windows (unlike macOS/Linux); fixed in `meta_query.zig`, `main.zig` (fmt and test commands) to handle this error
- **Formatter idempotence test**: `klar fmt /dev/stdin` hangs on Windows; replaced with temp file approach in `run-fmt-tests.sh`
- **LLVM-C linking on Windows**: vovkos packages provide `LLVM-C.lib` not `LLVM.lib`; `build.zig` now links `LLVM-C` on Windows
- **LLVM target availability**: `@hasDecl` guards on target init (vovkos amd64 only includes X86)
- **`_stat64` struct offsets**: Filled in Windows offsets: st_size=24, st_mtime=40 (verified via `offsetof`)
- **`process.Child.Term`**: Zig 0.15.2 uses PascalCase `.Exited`/`.Signal` not `.exited`/`.signal`

### Known Limitations

- Test scripts (`run-tests.sh`, etc.) require WSL or Git Bash on Windows
- `klar run` on Windows shows temp binary path as `args[0]` instead of source path
- Cross-compilation of filesystem operations to Windows from non-Windows is not supported (build on target platform)
- LLVM Windows binary (winget) lacks C API dev headers (`llvm-c/Core.h`); `klar build` requires building LLVM from source or using `LLVM_PREFIX`

### Success Criteria

- [x] Repository builds on macOS with zero regressions (667/667 tests pass)
- [x] Repository builds on Windows 11 (VM-only mode: 240 unit + 61 check + 59 fmt + 62 meta = 422 tests pass)
- [x] CLI and LSP workflows behave consistently with macOS/Linux
- [x] Windows-specific path/stdio regressions are covered by tests (`.gitattributes`, `GetStdHandle`, `statFile` fixes)
- [x] Contributor docs include complete Windows development setup
- [x] CI runs Windows matrix jobs (`.github/workflows/ci.yml`)

---

## Milestone 8: WebAssembly Target (Complete)

WebAssembly compilation target is fully working for wasm32 freestanding. `klar build --target wasm` emits valid `.wasm` modules; 6 smoke tests cover hello, arithmetic, structs, closures, generics, and control flow. Filesystem/readline builtins are gated with runtime traps. See [archive](docs/history/phase5-milestones.md#milestone-8-webassembly-target) for full detail.

**Remaining:** CI wasm-target regression coverage (deferred to Milestone 7.13 CI work).

---

## Milestone 9: Self-Hosting

**Objective:** Implement the Klar compiler front-end (lexer through type checker) in Klar itself, enabling the language to compile its own compiler.

**Status:** Paused — 9.1-9.8 complete (lexer, AST, parser at full parity: 259/259 files, 789/789 tests; type system definitions). 9.9+ paused pending Milestone M (Meta Layer).

**Effort:** Very High | **Impact:** Very High | **Dependencies:** Milestones 6, 7, 8

### Scope Boundary

Self-hosting means **frontend only** (lexer through type checker). The 33K-line LLVM codegen stays in Zig. The self-hosted frontend serializes AST/typed-AST for the Zig backend to consume. This is the standard bootstrap strategy (Go, Rust, etc.).

### Estimated Total: ~11,000–16,000 lines of Klar

### Dependency Chain

### Library Location: `stdlib/`

All pure Klar libraries live in a new top-level `stdlib/` directory:
```
stdlib/
├── json.kl          # JSON parse/stringify
├── sha256.kl        # SHA-256 hashing
├── toml.kl          # TOML parser
├── cli.kl           # CLI argument parsing
└── test/            # Tests for each module
    ├── test_json.kl
    ├── test_sha256.kl
    ├── test_toml.kl
    └── test_cli.kl
```

Imported as `import stdlib.json.*`, `import stdlib.toml.*`, etc. This is self-documenting, separate from compiler internals (`src/`), and establishes the pattern for Klar's standard library.

---

## Phase 0: Foundational Gaps ✅
**Status:** Complete (2026-02-27)

**Goal:** Fill small but critical missing primitives that later phases depend on.
**Estimated Effort:** 3-5 days

### Parallel Workflow
Touches compiler files: `src/codegen/builtins.zig`, `src/checker/checker.zig`, `src/vm_builtins.zig`, `src/interpreter.zig`, `src/codegen/emit.zig`. None of these are modified by other worktrees.

### Deliverables
- Environment variable access (`env_get`, `env_set`)
- Process spawning (`process_run` — run command, capture stdout/exit code via popen)
- Filesystem stat (`fs_stat` — file size, modification time, is_dir, is_file)
- Timestamp (`timestamp_now` — epoch-based, for checkpoint metadata)

### Tasks
- [x] Implement `env_get(name: string) -> ?string` builtin (wraps C `getenv`) (completed 2026-02-27)
- [x] Implement `env_set(name: string, value: string) -> Result#[void, IoError]` builtin (wraps C `setenv`) (completed 2026-02-27)
- [x] Implement `fs_stat(path: string) -> Result#[FileStat, IoError]` returning size, modified_time, is_dir, is_file (completed 2026-02-27)
- [x] Define `FileStat` struct: `size: i64`, `modified_epoch: i64`, `is_dir: bool`, `is_file: bool` (completed 2026-02-27)
- [x] Implement `process_run(cmd: string, args: List#[string]) -> Result#[ProcessOutput, IoError]` (completed 2026-02-27)
- [x] Define `ProcessOutput` struct: `stdout: string`, `stderr: string`, `exit_code: i32` (completed 2026-02-27)
- [x] Implement `timestamp_now() -> i64` (Unix epoch seconds) (completed 2026-02-27)
- [x] Add all of the above to checker, codegen/builtins, vm_builtins, interpreter (completed 2026-02-27)
- [x] Write native tests for each new builtin (completed 2026-02-27)

### Implementation Notes
- API names simplified from PLAN: `Process.run` → `process_run`, `Timestamp.now()` → `timestamp_now()` (static methods on structs not yet needed)
- `process_run` uses `popen`/`pclose` for subprocess execution. Stdout is captured; stderr returns empty string (sufficient for Lodex MVP). Exit code extracted via `WEXITSTATUS` macro equivalent.
- `FileStat` and `ProcessOutput` are registered as builtin struct types with field access through the standard struct infrastructure.
- VM and interpreter backends have stubs (return IOError) for complex builtins; `timestamp_now` works across all backends.

### Testing Strategy
Native tests exercising each builtin: env round-trip, stat on known files, process execution of simple commands, timestamp monotonicity.

### Phase 0 Readiness Gate
Before Phase 1, these must be true:
- [x] Can get/set environment variables
- [x] Can stat files for size and modification time
- [x] Can spawn a subprocess and capture its output
- [x] Can get the current Unix timestamp

---

## Milestone M: Meta Layer

**Objective:** Implement the `meta` keyword system for embedding intent, architecture, and design decisions in Klar source code — compiler-validated, CLI-queryable, zero runtime cost.

**Status:** Complete — M.1–M.8 all done (token, AST, parsing, backend passthrough, formatter, validation, pure verification, custom annotations, CLI query command)

**Effort:** High | **Impact:** Very High | **Design Spec:** [docs/design/meta-layer.md](docs/design/meta-layer.md)

### Rationale

AI agents waste tokens and make mistakes because context lives outside the code (CLAUDE.md, comments, external docs). The meta layer moves architectural intent *into* the language where the compiler validates it and tooling queries it. See the design spec for full motivation and philosophy alignment.

### Dependency Chain

```
M.1 → M.2 → M.3 → M.4
                 → M.5 → M.6
                      → M.7
                 → M.8
```

### Deferred (not in Milestone M)

- LSP integration (hover tooltips, completion) — post-M stretch goal
- Selfhost parser/AST updates — handled when 9.9+ resumes

---

### Phase MA: Foundation

#### M.1 — Token, AST, and Declaration Fields ✅

**Effort:** Low

Add the `meta` keyword token, AST node types for meta annotations, and meta fields on declaration structs.

- [x] **M.1.1** Add `meta` keyword token to `src/token.zig`
- [x] **M.1.2** Define `MetaAnnotation` union type in `src/ast.zig` (intent, decision, tag, hint, pure, deprecated, module, guide, related, group_def, group_join, define, custom)
- [x] **M.1.3** Add `meta: []const MetaAnnotation` field to function, struct, enum, trait, impl, and field declaration AST nodes
- [x] **M.1.4** Add `file_meta: []const MetaAnnotation` to the top-level program/module AST node
- [x] **M.1.5** Unit tests for MetaAnnotation construction and field access

**Success Criteria:**
- [x] `meta` is a recognized keyword token
- [x] MetaAnnotation can represent all 13 annotation kinds from the design spec
- [x] All declaration AST nodes carry meta annotation lists
- [x] Compiler builds with zero regressions

---

### Phase MB: Parsing

#### M.2 — Simple Annotation Parsing ✅

**Effort:** Medium

Parse the string-based annotations: `meta intent(...)`, `meta decision(...)`, `meta tag(...)`, `meta hint(...)`, `meta deprecated(...)`, `meta pure`.

- [x] **M.2.1** Add `parseMetaAnnotation()` to `src/parser.zig` — dispatches on the keyword after `meta`
- [x] **M.2.2** Parse `meta intent("...")` — string literal argument
- [x] **M.2.3** Parse `meta decision("...")` — string literal argument
- [x] **M.2.4** Parse `meta tag("...")` — string literal argument
- [x] **M.2.5** Parse `meta hint("...")` — string literal argument
- [x] **M.2.6** Parse `meta deprecated("...")` — string literal argument
- [x] **M.2.7** Parse `meta pure` — no arguments
- [x] **M.2.8** Parse stacked annotations (multiple `meta` lines before a declaration)
- [x] **M.2.9** Attach parsed meta annotations to the following declaration AST node
- [x] **M.2.10** Native test files for each simple annotation kind
- [x] **M.2.11** Verify `klar dump-ast` includes meta annotations in JSON output

**Success Criteria:**
- [x] All six simple annotation kinds parse correctly
- [x] Multiple annotations stack on a single declaration
- [x] `dump-ast` round-trips meta annotations
- [x] All existing tests pass (zero regressions)

#### M.3 — Block and Group Parsing ✅

**Effort:** Medium

Parse block-form annotations and group definitions/joins.

- [x] **M.3.1** Parse `meta module { key: value, ... }` — key-value block with string/list values
- [x] **M.3.2** Parse `meta guide { key: value, ... }` — same key-value block form
- [x] **M.3.3** Parse `meta related(path, path, "description")` — path list with optional trailing string
- [x] **M.3.4** Parse `meta group "name" { meta_annotation, ... }` — group definition with nested annotations
- [x] **M.3.5** Parse `meta in("name")` — group join
- [x] **M.3.6** Handle `in` keyword disambiguation (loop context vs meta context)
- [x] **M.3.7** Native test files for block annotations, related paths, and group def/join

**Success Criteria:**
- [x] Block annotations parse with arbitrary key-value pairs
- [x] `meta related(...)` accepts paths and validates trailing string position
- [x] Group definitions contain nested meta annotations
- [x] `meta in(...)` correctly joins groups
- [x] `in` keyword works correctly in both loop and meta contexts

---

### Phase MC: Backend Passthrough and Tooling

#### M.4 — Backend Passthrough and Formatter ✅

**Effort:** Low-Medium

Verify all three backends (interpreter, bytecode compiler, native codegen) skip meta annotations. Extend `dump-ast` and the formatter.

- [x] **M.4.1** Verify interpreter skips meta annotations (no codegen for meta nodes)
- [x] **M.4.2** Verify bytecode compiler skips meta annotations
- [x] **M.4.3** Verify native codegen (`src/codegen/emit.zig`) skips meta annotations — zero runtime cost
- [x] **M.4.4** Extend `dump-ast` JSON output to include all meta annotation types
- [x] **M.4.5** Extend formatter (`src/formatter.zig`) to preserve meta annotations with correct indentation
- [x] **M.4.6** Test: format → parse → format round-trip preserves meta annotations

**Success Criteria:**
- [x] Programs with meta annotations produce identical runtime output to programs without
- [x] `dump-ast` includes full meta annotation data in JSON
- [x] Formatter preserves meta annotations correctly
- [x] All existing tests pass (zero regressions)

---

### Phase MD: Checker Validation

#### M.5 — Basic Validation ✅

**Effort:** Medium-High

Validate meta annotations during type checking.

- [x] **M.5.1** Validate `meta related(...)` — all path targets resolve to existing declarations
- [x] **M.5.2** Validate `meta in("group")` — referenced group must be defined in scope
- [x] **M.5.3** Validate scope rules — file-level annotations (`meta module`, `meta guide`) only at top of file
- [x] **M.5.4** Emit deprecation warnings when calling functions marked `meta deprecated(...)`
- [x] **M.5.5** Validate `meta module` and `meta guide` field types (strings, string lists)
- [x] **M.5.6** Error messages with source spans for all meta validation failures
- [x] **M.5.7** Native test files: positive cases (valid annotations) and negative cases (`test/check/` error tests)

**Success Criteria:**
- [x] Invalid `meta related` targets produce clear error messages
- [x] Undefined group references are compile errors
- [x] Deprecation warnings fire with migration guidance text
- [x] All validation errors include file:line:column spans

#### M.6 — Pure Verification ✅

**Effort:** High

Verify that `meta pure` functions have no side effects. Pragmatic purity model: local `var` and assignment allowed (referential transparency, not strict immutability).

- [x] **M.6.1** Define "pure" criteria: no I/O, no impure function calls (local mutation allowed)
- [x] **M.6.2** Implement purity analysis in checker (`in_pure_function` flag, `pure_functions` registry)
- [x] **M.6.3** Error when `meta pure` function calls impure builtins (print, println, fs_*, etc.)
- [x] **M.6.4** Error when `meta pure` function calls non-pure user functions
- [x] **M.6.5** Allow `meta pure` functions to call other `meta pure` functions (transitive purity)
- [x] **M.6.6** Test files: `meta_pure_pass.kl` (positive), `meta_pure_calls_*.kl` (negative check tests)

**Success Criteria:**
- [x] Compiler rejects `meta pure` functions that perform side effects
- [x] Compiler accepts `meta pure` functions that are genuinely pure
- [x] Transitive purity checking works (pure calling pure is OK, pure calling impure is error)

#### M.7 — Custom Annotations ✅

**Effort:** Medium

Implement `meta define`, string union constraints, scope restrictions, and cross-module import.

- [x] **M.7.1** Parse `meta define name(params)` declarations
- [x] **M.7.2** Parse string union type constraints (`"high" | "medium" | "low"`)
- [x] **M.7.3** Parse scope restrictions (`for fn`, `for module`, etc.)
- [x] **M.7.4** Validate custom annotation usage matches its `meta define` shape
- [x] **M.7.5** Validate string union constraint violations (typo → compiler error)
- [x] **M.7.6** Validate scope restriction violations (wrong position → compiler error)
- [x] **M.7.7** Support importing `meta define` declarations from other modules
- [x] **M.7.8** Native test files for custom definitions, constraints, scope restrictions, and imports

**Success Criteria:**
- [x] `meta define` creates project-specific annotation vocabulary
- [x] String union constraints catch typos at compile time
- [x] Scope restrictions prevent annotations in wrong positions
- [x] Custom definitions can be imported and used across modules

---

### Phase ME: CLI

#### M.8 — `klar meta` Command ✅

**Effort:** Medium

Add the `klar meta` CLI command for querying meta annotations across a codebase.

- [x] **M.8.1** Add `meta` subcommand to `src/main.zig` command routing
- [x] **M.8.2** Implement `--tag "name"` — find all declarations with a specific tag
- [x] **M.8.3** Implement `--module` — list all module descriptions
- [x] **M.8.4** Implement `--related fn_name` — follow cross-references for a function
- [x] **M.8.5** Implement `--deprecated` — list all deprecated items with migration guidance
- [x] **M.8.6** Implement `--hints` — list all AI hints
- [x] **M.8.7** Implement `--json` output mode for all commands
- [x] **M.8.8** App-level test files for CLI output validation (62 tests in `scripts/run-meta-tests.sh`)

**Success Criteria:**
- [x] All six query modes produce correct, human-readable output
- [x] `--json` mode produces structured output matching the design spec format
- [x] CLI works across multi-file projects (processes all `.kl` files in directory)
- [x] Output is useful for both human developers and AI agents

---

## Phase 1: JSON Library ✅
**Status:** Complete (2026-02-28)

**Goal:** Implement JSON serialization and deserialization as a pure Klar library.
**Estimated Effort:** 5-7 days

### Parallel Workflow
Pure Klar code in `stdlib/json.kl`. Zero compiler changes — no conflict risk with any worktree.

### Deliverables
- `stdlib/json.kl` — JSON value type, parser, emitter, accessors (completed 2026-02-28)
- `test/module/json/main.kl` — comprehensive test suite (completed 2026-02-28)

### Tasks
- [x] Define `JsonValue` enum: `Null`, `Bool(bool)`, `Number(f64)`, `Str(string)`, `Array(List#[JsonValue])`, `Object(Map#[string, JsonValue])` (completed 2026-02-28)
- [x] Define `JsonError` struct: `message: string`, `pos: i32` (completed 2026-02-28)
- [x] Implement JSON parser (recursive descent: object, array, string, number, bool, null) (completed 2026-02-28)
- [x] Handle escape sequences in strings (`\"`, `\\`, `\/`, `\b`, `\f`, `\n`, `\r`, `\t`, `\uXXXX`) (completed 2026-02-28)
- [x] Implement `json_stringify(value: JsonValue) -> string` (compact output) (completed 2026-02-28)
- [x] Implement `json_stringify_pretty(value: JsonValue, indent: i32) -> string` (completed 2026-02-28)
- [x] Implement accessor helpers: `json_get`, `json_get_string`, `json_get_number`, `json_get_bool`, `json_get_array`, `json_get_object` (completed 2026-02-28)
- [x] Implement builder helpers: `json_null`, `json_bool`, `json_number`, `json_string`, `json_array`, `json_object` (completed 2026-02-28)
- [x] Write round-trip tests: parse → stringify → parse produces identical values (completed 2026-02-28)
- [x] Write edge-case tests: empty objects/arrays, nested structures, unicode, escape sequences, error handling (completed 2026-02-28)
- [x] Integrate into module test runner (`scripts/run-module-tests.sh`) (completed 2026-02-28)

### Testing Strategy
Round-trip tests for all JSON types. Edge cases: deeply nested objects, empty containers, escaped strings. 45+ test cases covering parse, stringify, round-trip, accessors, error handling, and pretty print.

### Implementation Notes
- Located in `stdlib/json.kl`
- Avoids `?` operator (codegen bug with non-primitive error types in Result)
- Stringify uses `String` builder via new `push_str(s: string)` method for O(n) serialization; parser uses string concatenation (input strings are typically short)
- `push_str` added to String type (checker, codegen, runtime) — appends a `string` (lowercase) to a `String` without creating a temporary `String.from()` wrapper
- Exponent parsing capped at magnitude 400 to prevent i32 overflow and DoS loops on inputs like `1e999999999`
- Recursive `JsonValue` enum works correctly for type sizing (List/Map are pointer-sized)

### Phase 1 Readiness Gate
Before Phase 2, these must be true:
- [x] Can parse any valid JSON string into a `JsonValue` (completed 2026-02-28)
- [x] Can stringify a `JsonValue` back to valid JSON (completed 2026-02-28)
- [x] Round-trip (parse → stringify → parse) produces identical values (completed 2026-02-28)
- [x] Handles all JSON escape sequences correctly (completed 2026-02-28)

---

## Phase 2: SHA-256 Hashing ✅
**Status:** Complete (2026-03-01)

**Goal:** Provide cryptographic hashing for content-addressed storage as a pure Klar library.
**Estimated Effort:** 2-3 days

### Parallel Workflow
Pure Klar code in `stdlib/sha256.kl`. No compiler changes needed.

### Deliverables
- `stdlib/sha256.kl` — SHA-256 hashing functions
- `test/module/sha256/main.kl` — test suite with NIST vectors

### Tasks
- [x] Evaluate approach: pure Klar vs FFI to OpenSSL/libcrypto (completed 2026-03-01)
- [x] Implement SHA-256 per FIPS 180-4 (message schedule, compression, padding) (completed 2026-03-01)
- [x] Implement `sha256(data: string) -> string` returning 64-char hex string (completed 2026-03-01)
- [x] Implement `sha256_bytes(data: List#[u8]) -> string` for binary data (completed 2026-03-01)
- [x] Write tests against known test vectors (empty string, "abc", multi-block, boundary cases) (completed 2026-03-01)

### Testing Strategy
Compare output against NIST test vectors and `sha256sum` command-line tool output. All 8 test vectors pass.

### Phase 2 Readiness Gate
Before Phase 3, these must be true:
- [x] `sha256("")` returns `e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855` (completed 2026-03-01)
- [x] `sha256("abc")` returns `ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad` (completed 2026-03-01)
- [x] Produces correct output for multi-block inputs (> 55 bytes) (completed 2026-03-01)

---

## Phase 3: TOML Parser ✅
**Status:** Complete (2026-03-01)

**Goal:** Parse TOML configuration files as a pure Klar library (needed for `lodex.toml`).
**Estimated Effort:** 4-5 days

### Parallel Workflow
Pure Klar code in `stdlib/toml.kl`. Zero compiler changes — no conflict risk.

### Deliverables
- `stdlib/toml.kl` — TOML value type, parser, accessors, stringify
- `test/module/toml/main.kl` — comprehensive test suite (35+ tests)

### Tasks
- [x] Define `TomlValue` enum: `Str(string)`, `Integer(i64)`, `Float(f64)`, `Bool(bool)`, `Array(List#[TomlValue])`, `Table(Map#[string, TomlValue])` (completed 2026-03-01)
- [x] Define `TomlError` struct: `message: string`, `line: i32` (completed 2026-03-01)
- [x] Implement TOML lexer (bare keys, quoted keys, `=`, `[section]`, `[section.sub]`) (completed 2026-03-01)
- [x] Implement TOML parser: key-value pairs, sections, nested tables, inline tables (completed 2026-03-01)
- [x] Handle TOML string types (basic, literal, multi-line basic, multi-line literal) (completed 2026-03-01)
- [x] Handle TOML arrays (including arrays of tables `[[section]]`) (completed 2026-03-01)
- [x] Implement accessor helpers: `toml_get(table, key)`, `toml_get_string(table, key)`, etc. (completed 2026-03-01)
- [x] Implement `toml_stringify` for serialization (completed 2026-03-01)
- [x] Write tests with `lodex.toml` format as primary test case (completed 2026-03-01)
- [x] Write edge-case tests: dotted keys, inline tables, mixed types (completed 2026-03-01)
- [x] Add TOML test to module test runner (completed 2026-03-01)

### Known Workarounds
- ~~**Map priming:** All `Map.new#[string, TomlValue]()` calls are followed by a sentinel insert (`""` → `Bool(false)`) to work around a codegen bug where unprimed maps don't allocate backing storage.~~ Fixed: Map.new() now eagerly allocates (Bug 1).
- ~~**List.push cross-function:** `List.push()` on lists extracted via `Map.get()` doesn't persist; the list must be re-inserted into the map after push.~~ Fixed: List heap-indirection (Bug 2).
- ~~**String buffer pass-by-value:** `String` passed to functions creates a copy; stringify uses string concatenation (`+`) instead of shared `String` buffer.~~ Fixed: String heap-indirection (Bug 3).
- ~~**Key corruption in recursive stringify:** Recursive functions that pass `List#[string]` (keys) corrupt key pointers; workaround uses per-iteration `Map.get()` in helper functions.~~ Fixed: List heap-indirection (Bug 4).
- **Note:** Map also now uses heap-indirection (Bug 8, fixed 2026-03-02), completing the shared-mutation pattern across all collection types (List, String, Map).

### Testing Strategy
Parse sample `lodex.toml` files and verify all sections/values extracted correctly. Test dotted keys, inline tables, and arrays of tables. 35+ tests covering: primitives, strings, sections, dotted keys, arrays, inline tables, array of tables, comments, number formats, accessors, error handling, stringify, and lodex-style config.

### Phase 3 Readiness Gate
Before Phase 4, these must be true:
- [x] Can parse `lodex.toml` evaluation config with nested sections
- [x] Handles dotted keys (`evaluation.suites.unit`)
- [x] Returns structured errors with line numbers for invalid TOML

---

## Phase 4: CLI Argument Parsing ✅
**Status:** Complete (2026-03-02)

**Goal:** Build a CLI argument parsing library in pure Klar.
**Estimated Effort:** 3-4 days

### Parallel Workflow
Pure Klar code in `stdlib/cli.kl`. Zero compiler changes — no conflict risk.

### Deliverables
- `stdlib/cli.kl` — Builder-pattern API with subcommands, flags, options, help generation (completed 2026-03-02)
- `test/module/cli/main.kl` — 30 tests modeling the Lodex CLI (completed 2026-03-02)

### Tasks
- [x] Design API: `cli_new`, `cli_def_flag`, `cli_def_option`, `cli_def_subcommand`, `cli_parse`, accessors (`cli_get_flag`, `cli_get_option`, `cli_has_subcommand`) (completed 2026-03-02)
- [x] Implement argument tokenization (handle `--flag`, `--key=value`, `--key value`, `-f`, positional args) (completed 2026-03-02)
- [x] Implement subcommand dispatch (first positional arg selects subcommand) (completed 2026-03-02)
- [x] Implement `cli_parse(def: CliDef, args: [String]) -> Result#[ParseResult, CliError]` (completed 2026-03-02)
- [x] Define `ParseResult` struct: `subcommand: ?string`, `flags: Map#[string, bool]`, `options: Map#[string, string]`, `positional: List#[string]` (completed 2026-03-02)
- [x] Implement help generation: `cli_help`, `cli_subcommand_help` (completed 2026-03-02)
- [x] Write tests modeling the Lodex CLI: `lodex init`, `lodex checkpoint --goal "..." --strategy "..."`, `lodex tree`, `lodex export --squash-strategy logical --json` (completed 2026-03-02)

### Testing Strategy
Test parsing of Lodex's full CLI surface. Verify help output. Test error cases: unknown flags, missing required args. 30 tests covering: basic flags, long options, short options, subcommands, positional args, help generation, error cases.

### Phase 4 Readiness Gate
Before Phase 5, these must be true:
- [x] Can parse `lodex checkpoint --goal "optimize auth" --strategy "cache" --json` (completed 2026-03-02)
- [x] `--help` prints formatted usage for each subcommand (completed 2026-03-02)
- [x] Unknown flags produce clear error messages (completed 2026-03-02)

---

## Phase 5: Integration Validation

**Goal:** Validate all libraries work together in a realistic Lodex-like program.
**Estimated Effort:** 2-3 days

### Parallel Workflow
Pure Klar code in `stdlib/test/`. Zero compiler changes — no conflict risk.

### Deliverables
- End-to-end integration test program in Klar
- Validation that all Phase 0-4 deliverables compose correctly

### Tasks
- [ ] Write a Klar program that: parses CLI args, reads a TOML config, reads/writes JSON files, computes SHA-256 hashes, stats files, spawns a subprocess
- [ ] Verify the program compiles and runs natively via `klar build`
- [ ] Profile for any performance issues in JSON parsing or SHA-256
- [ ] Document any bugs or missing features discovered during integration
- [ ] File issues upstream for any compiler bugs found

### Testing Strategy
The integration program itself is the test. It exercises all libraries in combination and must produce correct output.

---

## Phase 6: Async I/O and HTTP (Lodex Phase 3 Prerequisites)

**Goal:** Build async I/O and HTTP capabilities needed for Lodex's evaluation engine and HTTP API.
**Estimated Effort:** 3-4 weeks

### Parallel Workflow
This phase will require compiler changes (async execution model). By the time this phase starts, the other worktrees should be merged. If not, async runtime work lives in `src/runtime/` which is untouched by other branches. HTTP server/client can be pure Klar in `stdlib/http.kl` using FFI to libcurl/libmicrohttpd.

### Deliverables
- Improved async execution model (beyond current synchronous-completion Future#[T])
- `stdlib/http_server.kl` — HTTP server library (request routing, JSON request/response)
- `stdlib/http_client.kl` — HTTP client library (make requests, parse responses)

### Tasks
- [ ] Design async execution model: event loop, task scheduling, concurrent I/O
- [ ] Implement async subprocess execution (for evaluation engine: run tests without blocking)
- [ ] Build HTTP server: listen on port, route requests, serve JSON responses
- [ ] Build HTTP client: make GET/POST requests, parse response body
- [ ] Evaluate FFI approach: libuv for event loop, libmicrohttpd or libcurl for HTTP
- [ ] Write tests: concurrent async tasks, HTTP request/response round-trips

### Testing Strategy
Async tests with multiple concurrent tasks. HTTP server tests with curl/client. Load tests for checkpoint-rate scenarios.

### Phase 6 Readiness Gate
Before Lodex Phase 3, these must be true:
- [ ] Can serve HTTP endpoints from Klar
- [ ] Can make HTTP requests from Klar
- [ ] Can run subprocesses asynchronously without blocking the main thread

---

## Risk Register

| Risk | Impact | Likelihood | Mitigation |
|------|--------|------------|------------|
| JSON parser performance insufficient for rapid checkpoint serialization | Medium | Low | Profile early; can optimize hot paths or switch to FFI (jansson) |
| SHA-256 pure Klar too slow for content-addressed storage | Medium | Medium | Default to FFI to libcrypto; pure Klar as fallback |
| TOML spec is large; full compliance may not be needed | Low | Low | Implement subset needed by `lodex.toml`; extend later |
| Compiler bugs discovered when building complex pure-Klar libraries | High | Medium | File and fix upstream; use scratch/ for testing |
| Async model design is a significant language-level decision | High | High | Start with simple subprocess spawning; defer full async redesign |
| Process spawning requires platform-specific code (POSIX vs Windows) | Low | Low | Target macOS/Linux first; Klar already POSIX-focused |
| Merge conflicts with other worktrees during Phase 0 | Low | Low | Phase 0 codegen appended at end of `emit.zig`; minimal overlap with other branches |

## Timeline
Phase 0 → Phase 1 → Phase 2 → Phase 3 → Phase 4 → Phase 5 (sequential, each builds on prior).
Phase 6 is independent and can be developed later when Lodex approaches Phase 3.
Estimated total for Phases 0-5: 3-4 weeks.

Phases 1-4 are pure Klar libraries with zero compiler changes. They can begin as soon as Phase 0 builtins are available, and they can be developed in any order since they are independent of each other. The sequential ordering reflects priority (JSON is most critical for Lodex), not dependency.
