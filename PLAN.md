# Klar Standard Library — Lodex Infrastructure Plan

## Overview
Build the Klar standard library capabilities that [Lodex](../Lodex/DESIGN.md) (AI-native source control) depends on. These features are general-purpose and benefit the entire Klar ecosystem, not just Lodex. See [Lodex PLAN.md Phase 0](../Lodex/PLAN.md) for the downstream requirements.
Current status: Phase 0 not started.

## Parallel Workflow Strategy

This work runs in a dedicated worktree (`LodexNeeds`) alongside two active worktrees:
- **StartWorkOnMetaLayer** — modifies `parser.zig` (+580), `ast.zig` (+306), `checker.zig` (+209)
- **fizzy-nibbling-curry** — modifies `codegen/emit.zig` (+485)

To avoid merge conflicts:
- **Phase 0** (compiler builtins) keeps all new codegen in `src/codegen/builtins.zig`, not `emit.zig`
- **Phases 1-4** are pure Klar libraries in `stdlib/` — zero compiler changes, zero conflict risk
- **Phase 5** integration test is a standalone `.kl` program — no compiler changes

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

Imported as `import "stdlib/json"`, `import "stdlib/toml"`, etc. This is self-documenting, separate from compiler internals (`src/`), and establishes the pattern for Klar's standard library.

---

## Phase 0: Foundational Gaps

**Goal:** Fill small but critical missing primitives that later phases depend on.
**Estimated Effort:** 3-5 days

### Parallel Workflow
Touches compiler files: `src/codegen/builtins.zig`, `src/checker/builtins.zig`, `src/vm_builtins.zig`, `src/compiler.zig`, `src/interpreter.zig`. None of these are modified by other worktrees. Avoids `emit.zig` entirely — all native codegen for new builtins goes in `codegen/builtins.zig`.

### Deliverables
- Environment variable access (`env_get`, `env_set`)
- Process spawning (`Process.run` / `Process.output` — run command, capture stdout/stderr/exit code)
- Filesystem stat (`fs_stat` — file size, modification time, permissions)
- Timestamp type (epoch-based, for checkpoint metadata)

### Tasks
- [ ] Implement `env_get(name: string) -> ?string` builtin (wraps C `getenv`)
- [ ] Implement `env_set(name: string, value: string) -> Result#[void, IoError]` builtin (wraps C `setenv`)
- [ ] Implement `fs_stat(path: string) -> Result#[FileStat, IoError]` returning size, modified_time, is_dir, is_file
- [ ] Define `FileStat` struct: `size: i64`, `modified_epoch: i64`, `is_dir: bool`, `is_file: bool`
- [ ] Implement `Process.run(cmd: string, args: List#[string]) -> Result#[ProcessOutput, IoError]`
- [ ] Define `ProcessOutput` struct: `stdout: string`, `stderr: string`, `exit_code: i32`
- [ ] Implement `Timestamp.now() -> i64` (Unix epoch seconds)
- [ ] Add all of the above to checker/builtins, codegen/builtins, vm_builtins, compiler, interpreter
- [ ] Write native tests for each new builtin

### Testing Strategy
Native tests exercising each builtin: env round-trip, stat on known files, process execution of simple commands, timestamp monotonicity.

### Phase 0 Readiness Gate
Before Phase 1, these must be true:
- [ ] Can get/set environment variables
- [ ] Can stat files for size and modification time
- [ ] Can spawn a subprocess and capture its output
- [ ] Can get the current Unix timestamp

---

## Phase 1: JSON Library

**Goal:** Implement JSON serialization and deserialization as a pure Klar library.
**Estimated Effort:** 5-7 days

### Parallel Workflow
Pure Klar code in `stdlib/json.kl`. Zero compiler changes — no conflict risk with any worktree.

### Deliverables
- `stdlib/json.kl` — JSON value type, parser, emitter, accessors
- `stdlib/test/test_json.kl` — test suite

### Tasks
- [ ] Define `JsonValue` enum: `Null`, `Bool(bool)`, `Number(f64)`, `Str(string)`, `Array(List#[JsonValue])`, `Object(Map#[string, JsonValue])`
- [ ] Define `JsonError` struct: `message: string`, `line: i32`, `col: i32`
- [ ] Implement JSON lexer (tokenize string into JSON tokens)
- [ ] Implement JSON parser (recursive descent: object, array, string, number, bool, null)
- [ ] Handle escape sequences in strings (`\"`, `\\`, `\/`, `\b`, `\f`, `\n`, `\r`, `\t`, `\uXXXX`)
- [ ] Implement `json_stringify(value: JsonValue) -> string` (compact output)
- [ ] Implement `json_stringify_pretty(value: JsonValue, indent: i32) -> string`
- [ ] Implement accessor helpers: `json_get(obj, key)`, `json_get_string(obj, key)`, `json_get_i32(obj, key)`, etc.
- [ ] Implement builder helpers: `json_object()`, `json_array()`, `json_string(s)`, `json_number(n)`, etc.
- [ ] Write round-trip tests: parse → stringify → parse produces identical values
- [ ] Write edge-case tests: empty objects/arrays, nested structures, unicode, large numbers

### Testing Strategy
Round-trip tests for all JSON types. Edge cases: deeply nested objects, empty containers, special float values, escaped strings. Validate against known JSON test suites (RFC 8259 examples).

### Phase 1 Readiness Gate
Before Phase 2, these must be true:
- [ ] Can parse any valid JSON string into a `JsonValue`
- [ ] Can stringify a `JsonValue` back to valid JSON
- [ ] Round-trip (parse → stringify → parse) produces identical values
- [ ] Handles all JSON escape sequences correctly

---

## Phase 2: SHA-256 Hashing

**Goal:** Provide cryptographic hashing for content-addressed storage as a pure Klar library.
**Estimated Effort:** 2-3 days

### Parallel Workflow
Pure Klar code in `stdlib/sha256.kl`. If FFI approach is chosen, uses `extern` declarations within the `.kl` file — still no compiler changes. Zero conflict risk.

### Deliverables
- `stdlib/sha256.kl` — SHA-256 hashing functions
- `stdlib/test/test_sha256.kl` — test suite with NIST vectors

### Tasks
- [ ] Evaluate approach: pure Klar vs FFI to OpenSSL/libcrypto
- [ ] If pure Klar: implement SHA-256 per FIPS 180-4 (message schedule, compression, padding)
- [ ] If FFI: write `extern` declarations for `SHA256_Init`, `SHA256_Update`, `SHA256_Final` from libcrypto within `stdlib/sha256.kl`
- [ ] Implement `sha256(data: string) -> string` returning 64-char hex string
- [ ] Implement `sha256_bytes(data: List#[u8]) -> string` for binary data
- [ ] Write tests against known test vectors (empty string, "abc", etc.)

### Testing Strategy
Compare output against NIST test vectors and `sha256sum` command-line tool output.

### Phase 2 Readiness Gate
Before Phase 3, these must be true:
- [ ] `sha256("")` returns `e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`
- [ ] `sha256("abc")` returns `ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad`
- [ ] Produces correct output for multi-block inputs (> 55 bytes)

---

## Phase 3: TOML Parser

**Goal:** Parse TOML configuration files as a pure Klar library (needed for `lodex.toml`).
**Estimated Effort:** 4-5 days

### Parallel Workflow
Pure Klar code in `stdlib/toml.kl`. Zero compiler changes — no conflict risk.

### Deliverables
- `stdlib/toml.kl` — TOML value type, parser, accessors
- `stdlib/test/test_toml.kl` — test suite

### Tasks
- [ ] Define `TomlValue` enum: `Str(string)`, `Integer(i64)`, `Float(f64)`, `Bool(bool)`, `Array(List#[TomlValue])`, `Table(Map#[string, TomlValue])`
- [ ] Define `TomlError` struct: `message: string`, `line: i32`
- [ ] Implement TOML lexer (bare keys, quoted keys, `=`, `[section]`, `[section.sub]`)
- [ ] Implement TOML parser: key-value pairs, sections, nested tables, inline tables
- [ ] Handle TOML string types (basic, literal, multi-line basic, multi-line literal)
- [ ] Handle TOML arrays (including arrays of tables `[[section]]`)
- [ ] Implement accessor helpers: `toml_get(table, key)`, `toml_get_string(table, key)`, etc.
- [ ] Write tests with `lodex.toml` format as primary test case
- [ ] Write edge-case tests: dotted keys, inline tables, mixed types

### Testing Strategy
Parse sample `lodex.toml` files and verify all sections/values extracted correctly. Test dotted keys, inline tables, and arrays of tables.

### Phase 3 Readiness Gate
Before Phase 4, these must be true:
- [ ] Can parse `lodex.toml` evaluation config with nested sections
- [ ] Handles dotted keys (`evaluation.suites.unit`)
- [ ] Returns structured errors with line numbers for invalid TOML

---

## Phase 4: CLI Argument Parsing

**Goal:** Build a CLI argument parsing library in pure Klar.
**Estimated Effort:** 3-4 days

### Parallel Workflow
Pure Klar code in `stdlib/cli.kl`. Zero compiler changes — no conflict risk.

### Deliverables
- `stdlib/cli.kl` — argument parser with subcommands, flags, help generation
- `stdlib/test/test_cli.kl` — test suite

### Tasks
- [ ] Design API: `ArgParser.new(name, description)`, `.subcommand(name, description)`, `.flag(name, short, description)`, `.option(name, short, description, default)`
- [ ] Implement argument tokenization (handle `--flag`, `--key=value`, `--key value`, `-f`, positional args)
- [ ] Implement subcommand dispatch (first positional arg selects subcommand)
- [ ] Implement `parse(args: [String]) -> Result#[ParsedArgs, ArgError]`
- [ ] Define `ParsedArgs` struct: `subcommand: ?string`, `flags: Map#[string, bool]`, `options: Map#[string, string]`, `positional: List#[string]`
- [ ] Implement help generation: `--help` / `-h` prints usage, flags, subcommands
- [ ] Write tests modeling the Lodex CLI: `lodex init`, `lodex checkpoint --goal "..." --strategy "..."`, `lodex tree`, `lodex export --squash-strategy logical --json`

### Testing Strategy
Test parsing of Lodex's full CLI surface. Verify help output. Test error cases: unknown flags, missing required args.

### Phase 4 Readiness Gate
Before Phase 5, these must be true:
- [ ] Can parse `lodex checkpoint --goal "optimize auth" --strategy "cache" --json`
- [ ] `--help` prints formatted usage for each subcommand
- [ ] Unknown flags produce clear error messages

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
| Merge conflicts with other worktrees during Phase 0 | Low | Low | All Phase 0 codegen in `codegen/builtins.zig`; no overlap with other branches |

## Timeline
Phase 0 → Phase 1 → Phase 2 → Phase 3 → Phase 4 → Phase 5 (sequential, each builds on prior).
Phase 6 is independent and can be developed later when Lodex approaches Phase 3.
Estimated total for Phases 0-5: 3-4 weeks.

Phases 1-4 are pure Klar libraries with zero compiler changes. They can begin as soon as Phase 0 builtins are available, and they can be developed in any order since they are independent of each other. The sequential ordering reflects priority (JSON is most critical for Lodex), not dependency.
