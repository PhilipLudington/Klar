# Klar — Implementation Plan

## Overview

Klar is the AI-native application language: one checker, three backends (tree-walking
interpreter, bytecode VM, LLVM native), design in [DESIGN.md](DESIGN.md). The previous
plan — the Lodex stdlib infrastructure plan, Phases 0–6 — completed on 2026-03-05 and was
removed (`b854828`; it is in git history). This plan starts from the defect backlog that
the 2026-09-03/04 review and audit runs produced: `BUG.md` Bugs 5–13 from the `/qa-review`
fixture runs and Bugs 15–59 from the `/qa-audit --calibrate klar-e7b54e7` run
(report: `~/.claude/qa-reviews/Klar/audit/2026-09-04/qa-audit-report.md`).

Current status: Next Up queue populated (2026-09-04); Phase 0 not started.

## Next Up

Ordered queue of found issues. Each item is one branch off `main`, one PR, worked ahead of
phase tasks. Bug fixes follow `~/.claude/rules/bug-format.md` § Fixing a bug: the failing
test is committed first and seen failing, then the fix. Order: wrong results and crashes
in user programs first, then checker/contract correctness, then tooling, then debt.

**Crashes and wrong results in running programs**
- [ ] Bugs 15 + 16 + 17 — GC: unrooted half-built objects (`src/gc.zig:183`), unmarked async
      payloads (`gc.zig:374`), string methods popping the receiver before `createGC`
      (`src/vm.zig:1485`). One branch; add a stress-GC run to the VM tests. (qa-audit 2026-09-04)
- [ ] Bug 18 — VM carries no integer width: i128 arithmetic + no-op `.trunc#` (`src/vm.zig:1292`,
      `:1381`); carry the declared width in the value or opcode. (qa-audit 2026-09-04)
- [ ] Bugs 19 + 20 — interpreter `%` is Euclidean (`src/interpreter.zig:749,769`) and `len`
      is tagged `usize` (`:2818`, `:1177`). One branch. (qa-audit 2026-09-04)
- [ ] Bug 21 — interpreter `checkedAdd/Sub/Mul` overflow i128 before their own check
      (`src/interpreter.zig:786`); `@addWithOverflow`. (qa-audit 2026-09-04)
- [ ] Bugs 60 + 61 + 62 — VM pattern matching is unimplemented but runs: `op_match_variant`
      always pushes true (`src/vm.zig:966`), `op_is_type` emitted with no operand while the VM
      reads two bytes (`src/compiler.zig:966`, `src/vm.zig:882`), or-pattern success jump never
      patched (`src/compiler.zig:1503`). One branch: implement variant/type matching in the VM
      or refuse them at compile time; patch every `end` jump. (qa-audit 2026-09-05)
- [ ] Bug 63 — `let x = 1; x = 2` passes `klar check` and compiles natively (`src/checker/expressions.zig:392`
      never reads `sym.mutable`; the statement-level check at `statements.zig:78` is unreachable).
      Check mutability in the binary-assign path; failing test first. (qa-audit 2026-09-05)
- [ ] Bug 64 — HTTP stdlib uses codepoint `len()` where bytes are meant: `Content-Length`
      (`stdlib/http_client.kl:196`, `http_server.kl:281`), `slice(…, data.len())` body bounds, and
      `find_str_in`'s search limit. `byte_len()` at each site; a module test with a non-ASCII body
      first. (qa-review calibration 2026-09-06)
- [ ] Bug 34 — codegen `getTypeSize` sizes `char` as 1 byte and omits padding; enum payload
      stores overlap (`src/codegen/emit.zig:38333`, `:38342`). (qa-audit 2026-09-04)
- [ ] Bug 48 — REPL resets the AST arena every line while function bodies point into it
      (`src/repl.zig:228`); declarations get a non-reset arena. (qa-audit 2026-09-04)
- [ ] Bug 47 — `substring` returns `""` when `end` exceeds the count, both runtimes
      (`src/vm.zig:1601`, `src/interpreter.zig:1347`); clamp. (qa-audit 2026-09-04)
- [ ] Bug 5 — builtin emitters alloca at the call site; `fs_stat`/`process_run` in a loop
      grow the stack every iteration (`emit.zig`); hoist to the entry block. (qa-review 2026-09-03)
- [ ] Bug 6 — x86_64 macOS `stat`/`readdir` declared unversioned while offsets are the
      `$INODE64` layout. (qa-review 2026-09-03)
- [ ] Bug 13 — `fcntl` declared non-variadic; `FD_CLOEXEC`/`O_NONBLOCK` silently not set on
      arm64 macOS (`emit.zig:29236`); declare variadic + native test. (qa-review 2026-09-03)
- [ ] Bug 8 — `tcp_write` to a closed peer kills the process with SIGPIPE. (qa-review 2026-09-03)

**Checker correctness (programs the checker should refuse or type differently)**
- [ ] Bug 22 — `checkTypeCast` accepts string ↔ numeric and narrowing under `.as#`
      (`src/checker/expressions.zig:861`). (qa-audit 2026-09-04)
- [ ] Bug 23 — match arms bind into the enclosing scope (`src/checker/statements.zig:53`);
      push a scope per arm. (qa-audit 2026-09-04)
- [ ] Bug 24 — `impl` blocks checked in pass 3, methods invisible to bodies declared above
      them (`src/checker/checker.zig:5791`); register signatures in pass 2. (qa-audit 2026-09-04)
- [ ] Bug 25 — `appendTypeName` mangles most types as `"unknown"`, instantiations collide
      (`src/checker/checker.zig:2838`). (qa-audit 2026-09-04)
- [ ] Bugs 26 + 28 — compound assignment and bitwise ops never compare operand types
      (`src/checker/expressions.zig:406`, `:476`; `statements.zig:95`). One branch. (qa-audit 2026-09-04)
- [ ] Bugs 27 + 29 + 30 — trait machinery: `Self` checked only at index 0 (`methods.zig:198`),
      completeness against the union of impl blocks (`declarations.zig:887`), `.default()` on a
      variable (`method_calls.zig:69,157`). One branch. (qa-audit 2026-09-04)
- [ ] Bug 32 — literal patterns always `i32`/`f64` (`src/checker/patterns.zig:222`); pass the
      expected type. (qa-audit 2026-09-04)
- [ ] Bug 31 — `@sizeOf` no padding, `i128` = 8 (`src/checker/builtins_check.zig:675`). (qa-audit 2026-09-04)
- [ ] Bug 33 — no match-exhaustiveness pass; decide whether it is planned or a regression,
      then implement or record the decision on the bug. (qa-audit 2026-09-04)
- [ ] Bug 36 — `@fn_ptr(builtin)` yields a null pointer (`builtins_check.zig:523`,
      `emit.zig:14456`); reject in the checker. (qa-audit 2026-09-04)

**Three-backend contract**
- [ ] Bugs 35 + 14 — the checker registers fifteen builtins no runtime backend implements,
      and the builtin name list lives in four places with a dead fifth
      (`src/checker/checker.zig:933`, `src/codegen/builtins.zig`); one shared table that
      every backend reads, stub or reject per backend. (qa-audit 2026-09-04)
- [ ] Bugs 41 + 42 — `parse_int`/`parse_float` grammar and `debug` float form differ per
      backend (`emit.zig:17432`, `vm_builtins.zig:427`). One branch, one grammar. (qa-audit 2026-09-04)
- [ ] Bug 40 — native `debug` primitive: truncating 64-byte buffer, dangling alloca pointer,
      sign-extended unsigned (`src/codegen/emit.zig:15398`). (qa-audit 2026-09-04)
- [ ] Bug 43 — VM `readline`/`type_of`/`debug` strings allocated outside the GC
      (`src/vm_builtins.zig:163,342,398`); `createGC`. (qa-audit 2026-09-04)
- [ ] Bug 44 — `process_run` shell-joins its args through `popen` (`emit.zig:27365`); real
      argv via the Phase 6 spawn path. (qa-audit 2026-09-04)
- [ ] Bugs 7 + 9 + 10 + 11 + 12 — the Fixture 4 `Investigating` set: native runner never
      enforces two exit codes, `process_read_stdout` guard, `process_wait` double-close,
      `parse_int` stdlib vs checker declaration, `fs_read_to_string` unchecked `ftell`. Run each
      repro, close or fix. (qa-review 2026-09-03)
- [ ] Bug 45 — `env_get` `strdup` into a Copy `string` (`emit.zig:26961`); first check whether
      the HEAD return type is owned, then fix or close. (qa-audit 2026-09-04)
- [ ] Bugs 37 + 38 — wasm32: `usize` lowered as `i64` (`emit.zig:37806`), unsupported-builtin
      trap leaves the builder after a terminator (`:887`). One branch. (qa-audit 2026-09-04)
- [ ] Bug 39 — `fs_read_string` truncates the size to `i32` unchecked (`emit.zig:26301`). (qa-audit 2026-09-04)
- [ ] Bug 46 — `operandBytes(.op_closure)` ignores upvalue descriptors; `disasm` desyncs
      (`src/bytecode.zig:491`). (qa-audit 2026-09-04)

**CLI, driver and LSP**
- [ ] Bug 49 — `build`/`check` exit 0 on failure at `e7b54e7`; measure at HEAD first
      (`src/main.zig:3420`), then fix. (qa-audit 2026-09-04)
- [ ] Bug 50 — `run --interpret`/`--vm` discard `main`'s return (`src/main.zig:1087`, `:6149`). (qa-audit 2026-09-04)
- [ ] Bug 51 — imported-module parse failure `catch continue`, four pipelines
      (`src/main.zig:1198,3086,4460,6009`); fatal, as the test runner does. (qa-audit 2026-09-04)
- [ ] Bug 52 — ownership analysis only in `check`, entry module only (`src/main.zig:4645`);
      run it in every pipeline over every module. (qa-audit 2026-09-04)
- [ ] Bug 55 — LSP workspace containment is a bare `startsWith`, absent without `rootUri`
      (`src/lsp.zig:434`). (qa-audit 2026-09-04)
- [ ] Bug 56 — predictable `/tmp/klar-run-<seconds>` temp binary (`src/main.zig:3774`);
      unique 0700 directory. (qa-audit 2026-09-04)
- [ ] Bug 59 — `build`/`run` ignore unknown flags (`src/main.zig:648` region); same error
      branch as `check`/`test`. (qa-audit 2026-09-04)
- [ ] Bugs 54 + 57 — `klar update` deletes `klar.lock` first (`src/main.zig:6464`); git
      dependencies silently skipped (`:6375`). One branch. (qa-audit 2026-09-04)
- [ ] Bug 58 — `--emit-ir` lowers only the entry module (`src/main.zig:3221`). (qa-audit 2026-09-04)
- [ ] Bug 53 — `--include-source` never emits for `test_decl` (`src/main.zig:436`). (qa-audit 2026-09-04)

**Debt — File Growth (Limit band at `949fc4c`, measured by qa-audit 2026-09-04; seam TBD for all)**
- [ ] Extract `src/codegen/emit.zig` (27,265 code / 38,876 raw, 27× the Limit) — the builtin
      emitters (`emitFs*`, `emitProcess*`, `emitEnv*`, `emitTcp*`, `emitHttp*`), the
      `getOrDeclare*` libc bindings (88+), and the `debug`/`typeToLLVM` families are three
      natural files; start with the libc bindings. (ledger 2026-09-04)
- [ ] Extract `src/main.zig` (6,390 code) — one file per subcommand pipeline
      (`build`, `run`, `check`, `test`, `pkg`). (ledger 2026-09-04)
- [ ] Extract `src/checker/checker.zig` (4,548 code) — `initBuiltins` (1,300+ lines) to its own
      file, which is also where Bugs 35 + 14's shared table would live. (ledger 2026-09-04)
- [ ] Extract `src/parser.zig` (3,593 code) — declarations vs expressions vs types. (ledger 2026-09-04)
- [ ] Extract `src/interpreter.zig` (2,644 code) — builtins and `evalMethodCall` out. (ledger 2026-09-04)
- [ ] Extract `src/checker/method_calls.zig` (2,219 code) — `checkBuiltinMethod` (2,170 lines)
      by receiver family. (ledger 2026-09-04)
- [ ] Extract `src/formatter.zig` (1,798 code). (ledger 2026-09-04)
- [ ] Extract `src/vm.zig` (1,684 code) — `run` (749 lines) and the string/array method
      dispatch. (ledger 2026-09-04)
- [ ] Extract `stdlib/yaml.kl` (1,499 code) and `stdlib/toml.kl` (1,353 code). (ledger 2026-09-04)
- [ ] Extract `src/ast_from_json.zig` (1,330), `src/compiler.zig` (1,220),
      `src/interop/kira_manifest.zig` (1,212), `src/lsp.zig` (1,193), `src/types.zig` (1,131),
      `src/meta_query.zig` (1,084), `src/ast.zig` (1,018) — all just over the Limit; take each
      when a bug branch touches it. (ledger 2026-09-04)

## Phase 0: Regression floor for the backlog

**Goal:** Give the bug backlog the tests and tooling its two recurring classes need, so each
fix lands with a failing test and stays fixed.
**Estimated Effort:** 3–5 days

### Deliverables
- A three-backend parity test: one `.kl` corpus run under `--interpret`, the VM and native,
  outputs diffed; wired into `scripts/` and `run-tests.sh`.
- A stress-GC mode reachable from the VM test runner (collect on every allocation).
- The two `Investigating` audit bugs (45, 49) and the five `Investigating` review bugs
  (7, 9, 10, 11, 12) each measured and either closed or moved to `Open` with a repro.

### Tasks
- [ ] Add `scripts/run-parity-tests.sh`: run every file in `test/parity/` on all three backends
      and fail on any output difference; seed it with the Bug 18/19/20/41/42 triggers.
- [ ] Expose stress-GC (`stress_gc = true`) through a runner flag and run the VM suite under
      it once in `run-tests.sh`; seed `test/vm/` with the Bug 15/16/17 triggers.
- [ ] Measure Bug 49 at HEAD (`klar build broken.kl; echo $?`) and Bug 45's `env_get` return
      type; update both entries.
- [ ] Run the Bug 7/9/10/11/12 repros from their entries; close or open each.

### Testing Strategy
`./run-tests.sh` green with the two new suites included; each seeded trigger fails on
`main` before its fix and passes after.

---

## Risk Register

| Risk | Impact | Likelihood | Mitigation |
|------|--------|------------|------------|
| Tree targets Zig 0.15 and the installed toolchain is 0.16 | Cannot build or run tests until reconciled | High | First task of any branch: confirm `./run-build.sh` builds; if not, the toolchain pin is the real Phase 0 |
| `emit.zig` at 38k lines makes every codegen fix a merge hazard | Slow, conflict-prone PRs | High | Take the libc-binding extraction early; keep bug branches small |
| Bug 18 (VM width) is a representation change | Touches every VM arithmetic path | Medium | Parity suite first, then the change |

## Timeline

Next Up is worked top-down between PRs. Phase 0 runs alongside the first few bug branches
because its two suites are what those branches' failing tests plug into.
