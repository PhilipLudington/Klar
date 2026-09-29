# Project Debt

## [ ] Debt 1: Windows child pipes are read as synchronous handles

**Status:** Open
**Kind:** latent-defect
**Where:** `src/compat_windows.zig:288` (with `:38`, `:75`)
**Due when:** touching `src/compat_windows.zig` · a caller reads a child's piped stdout or stderr

**Description:** On Windows, `std.process.spawn` returns a child's piped stdout and stderr as
asynchronous handles (`nonblocking = true`). `childSpawn` keeps only the handle, and
`fileRead` rebuilds it through `ioFile()` as synchronous. A read before the child has written
anything then reaches `.PENDING => unreachable` in std's synchronous `NtReadFile` path, which
panics in safe builds and is undefined behaviour in ReleaseFast. Nothing reads a child pipe
today (the only `.Pipe` user, `linker.zig:382`, just waits), so this was not fixed with the port.

**Payment:** Keep the `nonblocking` flag with the handle (a field on `compat.File` on Windows),
or keep std's `Child` files and read through them. The repro still needed is a Windows test
that spawns with `.Pipe` and reads before the child writes.

**Found by:** /qa-review on ci/baseline-zig-016, 2026-09-26 — GenA — reviewer's reading, not re-verified.

---

## [ ] Debt 2: No local gate compiles the Windows or Linux targets

**Status:** Open
**Kind:** test-gap
**Where:** `run-tests.sh:50`, `.github/workflows/ci.yml:128`
**Due when:** touching `src/compat.zig` or `src/compat_windows.zig`

**Description:** Bugs 66 and 67 went unnoticed for five months because only CI compiles the
Windows and Linux targets, and only after the Linux gate passes.

**Payment:** `scripts/run-cross-check.sh`, called from `run-tests.sh`. It runs
`zig build -Dtarget=aarch64-windows --prefix <scratch>`, asserts that `klar.exe` exists, and
runs the Bug 67 type-check for `x86_64-linux-gnu`. It should be red at `fa3c287` and green after.

**Found by:** /qa-review on ci/baseline-zig-016, 2026-09-26 — TestCov — reviewer's reading, not re-verified.

---

## [ ] Debt 3: The compat shim's error mapping and stat paths have no unit tests

**Status:** Open
**Kind:** test-gap
**Where:** `src/compat_windows.zig:30` (`mapErr`), `:872` (`getEnvVarOwned`); `src/compat.zig:337`, `:370` (Linux `File.stat`, `statFile`)
**Due when:** touching `src/compat.zig` or `src/compat_windows.zig`

**Description:** Nothing checks that `mapErr` maps `PermissionDenied` to `AccessDenied` and
unknown errors to `Unexpected`, that `statFile` on a missing path returns `FileNotFound`, that
`File.stat()` through statx `AT_EMPTY_PATH` reports the right kind and size, or that a missing
environment variable gives `EnvironmentVariableNotFound`.

**Payment:** `test` blocks in `compat.zig` and `compat_windows.zig`: `mapErr` cases;
`cwd().statFile("does/not/exist")` returns `FileNotFound`; opening a known file and calling
`.stat()` gives `.kind == .file` and a size that matches `getEndPos()`;
`getEnvVarOwned(alloc, "KLAR_SURELY_UNSET_VAR")` returns `EnvironmentVariableNotFound`.
Also (qa-review 2026-09-27, TestCov2): a Windows test that runs a second allocating `std.Io`
path through `compat_windows.io()`, so a new call written against
`Io.Threaded.global_single_threaded` (Bug 72's cause) fails there.

**Found by:** /qa-review on ci/baseline-zig-016, 2026-09-26 — TestCov — reviewer's reading, not re-verified.

---

## [ ] Debt 4: The registry client's socket path is in no gate

**Status:** Open
**Kind:** test-gap
**Where:** `src/pkg/registry.zig` (`WindowsSocketStream`, `PosixSocketStream`), `scripts/run-registry-test.sh`
**Due when:** touching `src/pkg/registry.zig`

**Description:** `run-registry-test.sh` (8 add/publish cases) is called by neither
`run-tests.sh` nor CI, so connect, send, receive and close never run on any platform. The
Windows path also calls `vtable.netWrite` and `netRead` directly.

**Payment:** Call `run-registry-test.sh` from `run-tests.sh`, so every CI job runs it.

**Found by:** /qa-review on ci/baseline-zig-016, 2026-09-26 — TestCov — reviewer's reading, not re-verified.

---

## [ ] Debt 5: `klar run` exit codes above 255 and stdin reads are untested

**Status:** Open
**Kind:** test-gap
**Where:** `src/main.zig:3826`; `src/vm_builtins.zig:843`, `src/interpreter.zig:25`, `src/repl.zig:19`
**Due when:** touching `src/main.zig` `runNativeFileWithOptions` or any `getStdIn`

**Description:** No test covers a program whose `main` returns more than 255, and no test
reads stdin in the VM, the interpreter or the REPL. Only the LSP's stdin path is exercised.

**Payment:** Add two `run-args-tests.sh` cases:
- A `main` that returns 300 under `klar run` exits with 44.
- A `read_line` program fed one line through a pipe echoes the same output under `--vm` and under `--interpret`.

**Found by:** /qa-review on ci/baseline-zig-016, 2026-09-26 — TestCov — reviewer's reading, not re-verified.

---

## [ ] Debt 6: `runtime_trap_lowering` reaches 11 of 17 trap sites and passes on any count

**Status:** Open
**Kind:** test-gap
**Where:** `test/native/runtime_traps.kl`, `scripts/run-native-tests.sh` (`runtime_trap_lowering`)
**Due when:** touching `test/native/runtime_traps.kl` or a runtime-check block in `src/codegen/emit.zig`

**Description:** The fixture indexes only identifiers, so the list-field path
(`emit.zig:10104`), the non-identifier array, slice and List paths (`:10166`, `:10263`,
`:10316`) and `emitIndexAddressOf` (`:10384`, `:10439`) never reach the IR the check reads.
The check passes when `trap_total > 0`, so a lost failure block goes unnoticed too.

**Payment:** Extend the fixture with a struct holding a `List#[i32]` field indexed through the
struct, `make_arr()[i]`, `make_slice()[i]`, `make_list()[i]`, and `ref arr[i]` / `ref dv[i]`.
Assert the new block count (at least 19 today), or grep once per label kind (`overflow_trap`,
`bounds.fail`, `list.bounds.fail`, `set.fail`, `unwrap.fail`, `unwrap_err.fail`,
`match.failed`). Revert one missed site to bare `unreachable` and see it red.

**Found by:** /qa-review on ci/baseline-zig-016, 2026-09-27 — TestCov2, GenA2 — reviewer's reading, not re-verified.

---

## [ ] Debt 7: Native trap tests pass on any exit code, and the timeout branch is untested

**Status:** Open
**Kind:** test-gap
**Where:** `scripts/run-native-tests.sh:147` (`*) echo -1`), `:62`, `:129-135`; `test/native/overflow_add.kl`, `test/native/array_bounds.kl`
**Due when:** touching `scripts/run-native-tests.sh`

**Description:** `overflow_add.kl` says it must not return 0, but the runner accepts any exit
code, so a failed check that falls through and exits cleanly is green. Only a hang surfaced
Bug 73. The per-binary `timeout` has no `-k`, so a binary that ignores SIGTERM still hangs
the job, and nothing exercises the exit-124 branch.

**Payment:** Mark trap tests (e.g. an `expects_trap` list) and require a signal or non-zero
exit for them. Use `timeout -k 5`. Add a script-level check that runs a hanging binary under
`NATIVE_TEST_TIMEOUT=1` and expects one FAILED entry reading "timed out".

**Found by:** /qa-review on ci/baseline-zig-016, 2026-09-27 — GenA2, TestCov2 — reviewer's reading, not re-verified.

---

## [ ] Debt 8: A late nightly can skip a commit that no nightly has tested

**Kind:** latent-defect
**Where:** `.github/workflows/ci.yml` `changes` job (the `age -lt 90000` check)
**Due when:** touching `.github/workflows/ci.yml` · a nightly is found skipped while `main` has an untested commit

**Description:** The job's comment says a nightly skips "when main has not moved since the
last one", but the code skips whenever main's head commit is 25 h old or older. A commit
merged just after one nightly, followed by a scheduled run GitHub starts more than about an
hour late, is never tested. `nightly-ci.sh` passes over the skipped runs and keeps reporting
the older green sha.

**Payment:** Compare `HEAD` with the `head_sha` of the last scheduled run that ran the gate
(`gh api repos/{owner}/{repo}/actions/workflows/ci.yml/runs?event=schedule`), or widen the
window to about 48 h, since testing one commit twice costs nothing. Repro still needed: a
scheduled run that starts 25 h or more after a merge.

**Found by:** /qa-review on ci/upgrade-runners-actions, 2026-09-28 — GenA — reviewer's reading, not re-verified.

---

## [ ] Debt 9: `src/compat.zig` is over the 1000-line limit

**Kind:** extraction
**Where:** `src/compat.zig` (1004 code lines, 1238 raw)
**Due when:** touching `src/compat.zig`

**Description:** Six commits in three days took the POSIX compat shim past the limit, every
one of them an addition.

**Payment:** Move the process cluster (`posixFork`, `posixExecve`, `posixWaitpid`, `Child`,
about 145 lines, roughly lines 1044–1188) to `src/compat_process.zig`, following the
`compat_windows.zig` sibling pattern.

**Found by:** /qa-review on ci/upgrade-runners-actions, 2026-09-28 — Growth — reviewer's reading, not re-verified.
