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

**CI (blocks every PR: the workflow has been red since 2026-04-20)**
- [x] CI — baseline the stale workflow: change only Zig 0.15.2 → 0.16.0 (all four jobs) and
      add `workflow_dispatch`; leave runners, LLVM and action versions as they are. Record
      every job's result and the runner image each one reports. Each failure that is not the
      workflow itself (a real Linux, macOS or Windows test failure) becomes a BUG.md entry and
      is fixed before the upgrade line. Done when every job is green on the old
      infrastructure. Red since 2026-04-20; blocks PR 43. (Philip, 2026-09-26) (completed
      2026-09-27, PR 44: all five jobs green in run 36299203501 after Bugs 67, 72, 73)
- [x] Bug 66 — port `src/compat.zig` and `src/main.zig` stdio to Windows on Zig 0.16, on the
      baseline branch (`ci/baseline-zig-016`), so one PR takes CI from red to every job green.
      Only the Windows CI jobs can prove the port, and they only run with the baseline's Zig
      pin. No `continue-on-error`: the baseline PR waits for the port. Done when
      `zig build -Dtarget=x86_64-windows` and `-Dtarget=aarch64-windows` compile locally and
      both Windows jobs are green. (Philip, 2026-09-26: "get Windows working first")
      (completed 2026-09-27, PR 44)
- [x] PR 43 (Bugs 15–17): once the baseline + port PR merges, rebase `fix/gc-reachability`
      onto `main`, push `--force-with-lease`; it merges when its CI is green. (completed
      2026-09-29: merged as `4584f02` on local gates green at `225f232`, 2158 passed; PRs
      run no CI since 2026-09-28)
- [x] CI — upgrade runners and actions: `ubuntu-latest` → `ubuntu-26.04`, `ubuntu-24.04-arm` →
      `ubuntu-26.04-arm`, `macos-latest` → `macos-26` (the baseline run reported
      `macos-26-arm64`, 20260907.0351); `windows-latest` stays;
      `actions/checkout` v4 → v7, `actions/setup-python` v5 → v7, `actions/cache` v4 → v6; add
      `.github/dependabot.yml` (package-ecosystem `github-actions`, schedule monthly, one
      `groups:` entry matching `*` so every action bump arrives as a single PR,
      `open-pull-requests-limit: 1`) so action majors and the Node-20 stragglers
      (`mlugg/setup-zig`, `ilammy/msvc-dev-cmd`) arrive as PRs; `/today` lists open Dependabot
      PRs on its board (installed `~/.claude` e4b4d07). Done when every job is green.
      (Philip, 2026-09-26) (completed 2026-09-28, PR 45: all five jobs green in run
      36476998913 at `e8846bb`, after Bugs 76 and 78; the same PR moves CI to nightly on
      `main` and by hand, off PRs and merges, per the 2026-09-28 decision)
- [x] CI — one LLVM version on every build: 21. Today the builds use 17 on Linux and macOS CI
      (`apt llvm-17`, `brew llvm@17`), 18.1.8 on Windows CI (vovkos), and 21.1.8 locally
      (Homebrew `llvm`, which `build.zig` `detectLLVMPrefix` finds first). Move Linux to
      `llvm-21-dev` (packaged on Ubuntu 26.04), macOS to `brew install llvm@21`, and Windows
      to vovkos `llvm-21.1.1-windows-amd64-msvc17-msvcrt.7z`, changing its `actions/cache` key
      and the `LLVM_PREFIX`/PATH lines with it. Done when every job is green on 21 and
      `CLAUDE.md` names 21 as the supported LLVM. Why: a codegen difference that shows up on
      only one platform today could come from LLVM rather than Klar. (Philip, 2026-09-26)
      (completed 2026-09-29, PR 46: all five jobs green in dispatched run 36530730189 at
      `5c07a47`, each LLVM job logging `LLVM 21 at <prefix>`; merged as `c79cb8b`)
- [x] CI — protect `main` with no required status check: block deletion and force-push
      (non-fast-forward) only. A required `Linux x86_64 (gate)` check was rejected because
      no check runs on a PR since the 2026-09-28 nightly-only move, so it would block every
      PR. Done by enabling the repository ruleset 12199325 ("Ruleset Alpha", created
      2026-01-27 with exactly these two rules on `~DEFAULT_BRANCH` and left disabled) rather
      than adding classic branch protection beside it; no bypass actors. (Philip,
      2026-09-29) (completed 2026-09-29: `gh api repos/PhilipLudington/Klar/rules/branches/main`
      lists `deletion` and `non_fast_forward` and nothing else)

**Crashes and wrong results in running programs**
- [x] Native channels — Bugs 86 + 87: a `Sender` reached through a struct or tuple field
      (`w.tx.send(v)`, `src/codegen/emit.zig:31095`), or declared through a type alias
      (`getChannelTypeInfo`, `:7786`), is not recognised as an endpoint. The send is dropped
      and the receiver hangs. (qa-review 2026-09-29) (completed 2026-09-29: every channel
      test reads one path, `channelEndpointOf`, over the declared type of a local or a field
      path rooted at one; an aliased endpoint parameter lowers as the spelled-out one)
- [x] stdlib integration — Bug 79: the natively built `test/module/integration/main.kl`
      crashes in about 3% of runs (6 of 200: SIGABRT ×4, SIGSEGV ×2) and reddens
      `./run-tests.sh` on unrelated PRs. Reproduce under a sanitizer or with a heap-poisoning
      allocator to find the stdlib module or codegen path, then fix. (found: second sighting,
      2026-09-29, `plan/protect-main`) (completed 2026-09-29: `List.push` stored a u32
      literal above i32 max as 8 bytes, overflowing `sha256` `init_k`'s buffer; push, send
      and tuple elements now emit a literal at its hinted width)
- [x] Native channels — Bug 83: `send` on a `Sender#[T]` function parameter emits no code
      and the receiver blocks forever, `src/codegen/emit.zig:12370`, `:31070`. (qa-review
      2026-09-29) (completed 2026-09-29: parameters and `var`s record their channel fields
      as a `let` does)
- [x] contextual literal width — Bug 84: a bare `None` in a tuple element or `push` argument
      is emitted as `i32 0` (wrong tuple values, an uninitialized optional payload),
      `src/codegen/emit.zig:~4481`. (qa-review 2026-09-29) (completed 2026-09-29: a hinted
      bare `None` is the hinted optional's none value)
- [ ] Native codegen — Bugs 74 + 75: runtime checks that let undefined behavior through. A
      negative `i8`/`i16` index passes the bounds check (zext) and the GEP sign-extends it
      (`src/codegen/emit.zig:9898`, `:5160`, `:9957`); integer `/` and `%` have no zero or
      MIN/-1 check (`:4570`). One branch; both fail into `emitTrap`. (qa-review 2026-09-27)
- [ ] Platform layer — Bugs 68 + 69: `src/compat.zig` hard-codes Linux flag values that are
      wrong on macOS, so `createFile` ignores `truncate`/`exclusive` (`:525-528`; a shorter
      rewrite keeps the old tail) and `deleteTree` never removes directories (`:659`). Replace
      every literal with `std.c.O{…}` / `std.c.AT.REMOVEDIR`. (qa-review 2026-09-26)
- [x] Bugs 15 + 16 + 17 — GC: allocation no longer collects; the VM collects at the top of each
      instruction (`GC.collectIfRequested`), and `markValue` traces Future payloads. Stress-GC
      runs live in the unit tests (`src/vm_gc_test.zig`); the runner flag stays the Phase 0 task
      (departed: collect at safe points, not temp-root each caller). (qa-audit 2026-09-04)
      (completed 2026-09-26)
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
- [ ] Extend the stress-GC seeds past the stack roots the Bugs 15–17 tests cover. Add three
      programs: (1) a string in a global and a closure capturing a local string, first as
      an open upvalue and then closed, each allocating again before it is read back; (2)
      `op_array` → `op_array_push`, `op_tuple`, `op_struct`, `op_some` and `op_closure` with
      a captured upvalue, each result checked; (3) an async function that returns an array,
      a collection before `await`, then indexing the awaited array (Bug 16 end to end).
      (found: qa-review 2026-09-26, Bugs 15/16)
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
