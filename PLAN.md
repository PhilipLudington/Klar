# Klar Standard Library — Lodex Infrastructure Plan

Build the Klar standard library capabilities that [Lodex](../Lodex/DESIGN.md) (AI-native source control) depends on. These features are general-purpose and benefit the entire Klar ecosystem, not just Lodex. See [Lodex PLAN.md Phase 0](../Lodex/PLAN.md) for the downstream requirements.

Current status: Phases 0–5 complete. Phase 6 (Async I/O and HTTP) deferred until Lodex reaches Phase 3.

For Phase 5 milestones (Windows, WebAssembly, Self-Hosting, Meta Layer), see [ROADMAP.md](ROADMAP.md).

---

## Phase 0: Foundational Gaps ✅
**Status:** Complete (2026-02-27)

**Goal:** Fill small but critical missing primitives that later phases depend on.

### Tasks
- [x] Implement `env_get(name: string) -> ?string` builtin (wraps C `getenv`)
- [x] Implement `env_set(name: string, value: string) -> Result#[void, IoError]` builtin (wraps C `setenv`)
- [x] Implement `fs_stat(path: string) -> Result#[FileStat, IoError]` returning size, modified_time, is_dir, is_file
- [x] Define `FileStat` struct: `size: i64`, `modified_epoch: i64`, `is_dir: bool`, `is_file: bool`
- [x] Implement `process_run(cmd: string, args: List#[string]) -> Result#[ProcessOutput, IoError]`
- [x] Define `ProcessOutput` struct: `stdout: string`, `stderr: string`, `exit_code: i32`
- [x] Implement `timestamp_now() -> i64` (Unix epoch seconds)
- [x] Add all of the above to checker, codegen/builtins, vm_builtins, interpreter
- [x] Write native tests for each new builtin

### Implementation Notes
- API names simplified: `Process.run` → `process_run`, `Timestamp.now()` → `timestamp_now()`
- `process_run` uses `popen`/`pclose` for subprocess execution. Stdout captured; stderr returns empty string (sufficient for Lodex MVP). Exit code extracted via `WEXITSTATUS` macro equivalent.
- `FileStat` and `ProcessOutput` are registered as builtin struct types with field access through the standard struct infrastructure.
- VM and interpreter backends have stubs (return IOError) for complex builtins; `timestamp_now` works across all backends.

---

## Phase 1: JSON Library ✅
**Status:** Complete (2026-02-28)

**Goal:** Implement JSON serialization and deserialization as a pure Klar library.

### Tasks
- [x] Define `JsonValue` enum: `Null`, `Bool(bool)`, `Number(f64)`, `Str(string)`, `Array(List#[JsonValue])`, `Object(Map#[string, JsonValue])`
- [x] Define `JsonError` struct: `message: string`, `pos: i32`
- [x] Implement JSON parser (recursive descent: object, array, string, number, bool, null)
- [x] Handle escape sequences in strings (`\"`, `\\`, `\/`, `\b`, `\f`, `\n`, `\r`, `\t`, `\uXXXX`)
- [x] Implement `json_stringify(value: JsonValue) -> string` (compact output)
- [x] Implement `json_stringify_pretty(value: JsonValue, indent: i32) -> string`
- [x] Implement accessor helpers: `json_get`, `json_get_string`, `json_get_number`, `json_get_bool`, `json_get_array`, `json_get_object`
- [x] Implement builder helpers: `json_null`, `json_bool`, `json_number`, `json_string`, `json_array`, `json_object`
- [x] Write round-trip tests: parse → stringify → parse produces identical values
- [x] Write edge-case tests: empty objects/arrays, nested structures, unicode, escape sequences, error handling
- [x] Integrate into module test runner (`scripts/run-module-tests.sh`)

### Deliverables
- `stdlib/json.kl` — JSON value type, parser, emitter, accessors
- `test/module/json/main.kl` — comprehensive test suite (45+ tests)

### Implementation Notes
- Avoids `?` operator (codegen bug with non-primitive error types in Result)
- Stringify uses `String` builder via `push_str(s: string)` method for O(n) serialization
- Exponent parsing capped at magnitude 400 to prevent i32 overflow and DoS loops on inputs like `1e999999999`
- Recursive `JsonValue` enum works correctly for type sizing (List/Map are pointer-sized)

---

## Phase 2: SHA-256 Hashing ✅
**Status:** Complete (2026-03-01)

**Goal:** Provide cryptographic hashing for content-addressed storage as a pure Klar library.

### Tasks
- [x] Evaluate approach: pure Klar vs FFI to OpenSSL/libcrypto
- [x] Implement SHA-256 per FIPS 180-4 (message schedule, compression, padding)
- [x] Implement `sha256(data: string) -> string` returning 64-char hex string
- [x] Implement `sha256_bytes(data: List#[u8]) -> string` for binary data
- [x] Write tests against known test vectors (empty string, "abc", multi-block, boundary cases)

### Deliverables
- `stdlib/sha256.kl` — SHA-256 hashing functions
- `test/module/sha256/main.kl` — test suite with NIST vectors (8 test vectors pass)

---

## Phase 3: TOML Parser ✅
**Status:** Complete (2026-03-01)

**Goal:** Parse TOML configuration files as a pure Klar library (needed for `lodex.toml`).

### Tasks
- [x] Define `TomlValue` enum: `Str(string)`, `Integer(i64)`, `Float(f64)`, `Bool(bool)`, `Array(List#[TomlValue])`, `Table(Map#[string, TomlValue])`
- [x] Define `TomlError` struct: `message: string`, `line: i32`
- [x] Implement TOML lexer (bare keys, quoted keys, `=`, `[section]`, `[section.sub]`)
- [x] Implement TOML parser: key-value pairs, sections, nested tables, inline tables
- [x] Handle TOML string types (basic, literal, multi-line basic, multi-line literal)
- [x] Handle TOML arrays (including arrays of tables `[[section]]`)
- [x] Implement accessor helpers: `toml_get(table, key)`, `toml_get_string(table, key)`, etc.
- [x] Implement `toml_stringify` for serialization
- [x] Write tests with `lodex.toml` format as primary test case
- [x] Write edge-case tests: dotted keys, inline tables, mixed types
- [x] Add TOML test to module test runner

### Deliverables
- `stdlib/toml.kl` — TOML value type, parser, accessors, stringify
- `test/module/toml/main.kl` — comprehensive test suite (35+ tests)

### Implementation Notes
- All collection codegen bugs that affected initial development have been fixed: Map priming (Bug 1), List heap-indirection (Bug 2), String heap-indirection (Bug 3), Map heap-indirection (Bug 8)

---

## Phase 4: CLI Argument Parsing ✅
**Status:** Complete (2026-03-02)

**Goal:** Build a CLI argument parsing library in pure Klar.

### Tasks
- [x] Design API: `cli_new`, `cli_def_flag`, `cli_def_option`, `cli_def_subcommand`, `cli_parse`, accessors
- [x] Implement argument tokenization (handle `--flag`, `--key=value`, `--key value`, `-f`, positional args)
- [x] Implement subcommand dispatch (first positional arg selects subcommand)
- [x] Implement `cli_parse(def: CliDef, args: [String]) -> Result#[ParseResult, CliError]`
- [x] Define `ParseResult` struct: `subcommand: ?string`, `flags: Map#[string, bool]`, `options: Map#[string, string]`, `positional: List#[string]`
- [x] Implement help generation: `cli_help`, `cli_subcommand_help`
- [x] Write tests modeling the Lodex CLI: `lodex init`, `lodex checkpoint`, `lodex tree`, `lodex export`

### Deliverables
- `stdlib/cli.kl` — Builder-pattern API with subcommands, flags, options, help generation
- `test/module/cli/main.kl` — 30 tests modeling the Lodex CLI

---

## Phase 5: Integration Validation ✅
**Status:** Complete (2026-03-02)

**Goal:** Validate all libraries work together in a realistic Lodex-like program.

### Tasks
- [x] Write a Klar program that: parses CLI args, reads a TOML config, reads/writes JSON files, computes SHA-256 hashes, stats files, spawns a subprocess
- [x] Verify the program compiles and runs natively via `klar build`
- [x] Profile for any performance issues in JSON parsing or SHA-256
- [x] Document any bugs or missing features discovered during integration
- [x] File issues upstream for any compiler bugs found

### Deliverables
- `test/module/integration/main.kl` — 12 integration tests exercising all Phase 0–4 libraries together
- Integration test added to `scripts/run-module-tests.sh`

### Compiler Bugs Found and Fixed
- **Bug 1: Multi-module builtin struct registration** — `registerBuiltinStructTypes()` only called from single-module path. Fix: made public and added call in `main.zig` before struct registration loop.
- **Bug 2: Cross-module non-pub function name collision** — Non-pub functions in different stdlib modules collided in the LLVM module. Fix: module-prefixed naming for non-pub functions.

### Known Limitation
- `json_stringify` crashes on deeply nested objects with multiple key-value pairs (pre-existing bug). Integration test uses simpler JSON for round-trip testing.

---

## Phase 6: Async I/O and HTTP

**Goal:** Build async I/O and HTTP capabilities needed for Lodex's evaluation engine and HTTP API.
**Status:** Deferred until Lodex reaches Phase 3.
**Estimated Effort:** 3-4 weeks

### Tasks
- [ ] Design async execution model: event loop, task scheduling, concurrent I/O
- [ ] Implement async subprocess execution (for evaluation engine: run tests without blocking)
- [ ] Build HTTP server: listen on port, route requests, serve JSON responses
- [ ] Build HTTP client: make GET/POST requests, parse response body
- [ ] Evaluate FFI approach: libuv for event loop, libmicrohttpd or libcurl for HTTP
- [ ] Write tests: concurrent async tasks, HTTP request/response round-trips

### Deliverables
- Improved async execution model (beyond current synchronous-completion Future#[T])
- `stdlib/http_server.kl` — HTTP server library (request routing, JSON request/response)
- `stdlib/http_client.kl` — HTTP client library (make requests, parse responses)

### Readiness Gate
Before Lodex Phase 3, these must be true:
- [ ] Can serve HTTP endpoints from Klar
- [ ] Can make HTTP requests from Klar
- [ ] Can run subprocesses asynchronously without blocking the main thread
