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

## [x] Debt 6: `runtime_trap_lowering` reaches 11 of 17 trap sites and passes on any count

**Status:** Paid
**Paid:** fix/bug-74-75-runtime-checks, 2026-09-30: `runtime_traps.kl` also indexes a List
field through a struct (`h.items[i]`), `make_arr()[i]`, `make_slice()[i]`, `make_list()[i]`,
`ref fixed[i]` and `ref dv[i]`, and divides with `/` and `%`: 33 failure blocks. The check
now fails unless each of `bounds.fail`, `list.bounds.fail`, `set.fail`, `overflow_trap`,
`div.fail`, `unwrap.fail`, `unwrap_err.fail` and `match.failed` appears and the count is at
least 33 (`TRAP_BLOCK_FLOOR`). Seen red three ways: `emitCheckedIndex` failing into a bare
`unreachable` (13 of 33 bare), no division check (`div.fail` missing), and one index site
without its check (32 blocks).
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

## [x] Debt 7: Native trap tests pass on any exit code, and the timeout branch is untested

**Status:** Paid
**Paid:** fix/bug-74-75-runtime-checks, 2026-09-30: a test marked `// Expected: trap` passes
only on SIGILL or SIGTRAP (exit 132 or 133; any non-zero exit on Windows), and
`array_bounds.kl` and the three `overflow_*.kl` are marked. The run timeout is
`timeout -k 5`, and exit 137 counts as a timeout as well as 124. `native_timeout_branch`
runs `test/native/timeout_hang.kl`, which never returns, under a 1-second limit and fails
unless it is reported as timed out. Seen red with `overflow_add.kl` made non-trapping
(`+%`) and with the timeout test's 124 changed.
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

---

## [ ] Debt 10: The LLVM 21 check misses a non-numeric argument case and `build.zig`'s search order

**Kind:** test-gap
**Where:** `scripts/test-check-llvm-version.sh`, `scripts/check-llvm-version.sh:14`, `build.zig:177,200`
**Due when:** touching `scripts/check-llvm-version.sh` · touching `build.zig`

**Description:** Only the missing-argument usage error is tested; a non-numeric major
(`21.1`, `abc`) should also exit 2. Nothing pins that `build.zig`'s versioned Homebrew and
`/usr/lib/llvm-N` lists start at `"21"`, the major CI checks.

**Payment:** Add `check "non-numeric major is a usage error" 2 "$WORK/llvm21" 21.1`, and a
case like the ci.yml scan that asserts the first element of both `versions` arrays in
`build.zig` is `"21"`.

**Found by:** /qa-review on ci/llvm-21-everywhere, 2026-09-29 — TestCov — reviewer's reading, not re-verified.

---

## [ ] Debt 11: The contextual literal hint is untested for `None`, `Ok`/`Err`, tuples, grouping and the sender fallback

**Kind:** test-gap
**Where:** `src/codegen/emit.zig` `emitExprWithHint` (~4317-4319), `getSenderElementType` (~31242); `test/native/channel_send_literal_width.kl:8`
**Due when:** touching `emitExprWithHint` in `src/codegen/emit.zig` · touching `test/native/channel_send_literal_width.kl`

**Description:** Bug 79's tests pin a literal pushed or sent at the element width. They do not
pin: `None` pushed into `List#[?i64]`, `Ok(5)` into `List#[Result#[i64, string]]`, `(1, 2)`
into `List#[(i64, u8)]`, the `.grouped` recursion (`k.push((4000000000))` into `List#[u32]`),
or `getSenderElementType`'s checker fallback for a sender that is not a local
(`pair.0.send(7)`). The channel test's pre-fix red depends on non-zero stack garbage above a
4-byte temporary (it was red 3 of 3 on 2026-09-29).

**Payment:** One native test that pushes and reads back `None`, `Ok(5)` and a tuple under an
element hint, plus `pair.0.send(7)` on a `Sender#[i64]`. For grouping and the channel case,
an `--emit-llvm` check (`store i32` for the u32 push, `i64 7` for the send), or a helper
that dirties its stack frame with `-1` before the send.

**Found by:** /qa-review on fix/bug-79-integration-crash, 2026-09-29 — TestCov — reviewer's reading, not re-verified.

---

## [ ] Debt 12: `BUG.md` is over the 2000-line navigation ceiling

**Kind:** extraction
**Where:** `BUG.md` (2224 raw lines on 2026-09-29)
**Due when:** touching `BUG.md` and it is over 2500 raw lines · the next ledger-format change

**Description:** The ledger only grows. Closed entries make up most of it, and every review
of a branch that files or closes a bug measures it again as file growth.

**Payment:** Move closed (`## [x]`) entries into `docs/history/BUG-closed.md`, keeping their
numbers, and leave a one-line pointer at the top of `BUG.md`. Confirm first that AirTower's
bug badge reads only open entries.

**Found by:** /qa-review WATCH [file-growth], seen on ci-baseline-zig-016, ci-upgrade-runners-actions and fix/bug-79-integration-crash (2026-09-27 to 2026-09-29) — Growth — reviewer's reading, not re-verified.

---

## [x] Debt 13: A Receiver parameter's element width, a `var` Receiver and `send(None)` are untested

**Status:** Paid
**Paid:** fix/bug-86-channel-field-alias, 2026-09-29: `channel_param_endpoints.kl` reads
`4000000000` through `consume(rx)` (exit 3 when recv reads at i32), reads through a
`var vrx: Receiver#[i64]` (exit 6 when a `var` Receiver is not an endpoint), and sends
`None` then `Some(7)` on a `Sender#[?i64]`. The `None` half has no runtime symptom when
the hint is dropped (the 4-byte zero also zeroes the tag, as in Bug 84's push), so it
checks behavior but pins nothing.
**Kind:** test-gap
**Where:** `src/codegen/emit.zig` parameter registration (~2396-2401), `var` registration (~2888-2890), `emitIdentifier` `None` branch (~4501-4509); `test/native/channel_param_endpoints.kl`
**Due when:** touching `test/native/channel_param_endpoints.kl` · touching the channel-field registration in `src/codegen/emit.zig`

**Description:** Bug 83's test pins the Sender side of a parameter's element type, but
`consume(rx)` reads `5`, which fits in i32, so a Receiver parameter read at the wrong width
would pass. The `var` Receiver branch (`is_receiver = !ci.is_sender`) is not exercised, and
no test sends a bare `None` through a `Sender#[?i64]`, although the new comment in
`emitIdentifier` names `send` as a hint site.

**Payment:** In `channel_param_endpoints.kl`, send `4000000000` and read it through
`consume(rx)`; add `var vrx: Receiver#[i64] = rx` and `vrx.recv()`; and add `tx.send(None)`
then `tx.send(Some(7))` on a `Sender#[?i64]`, asserting both on the receiving side.

**Found by:** /qa-review on fix/bug-83-sender-param, 2026-09-29 — TestCov — reviewer's reading, not re-verified.

---

## [ ] Debt 14: `src/codegen/emit.zig` is far over the file-size limit; the channel cluster is one clean seam

**Kind:** extraction
**Where:** `src/codegen/emit.zig` (38947 raw lines on 2026-09-29), channel cluster `getChannelEndpointType` … `isReceiverExpr` (~30572-31126)
**Due when:** touching the channel cluster in `src/codegen/emit.zig` (Bugs 85-87 will)

**Description:** The file is about 19× the raw ceiling and grows on almost every codegen fix
(+881/−250 over its last 15 commits). The channel code sits in one contiguous block of about
555 lines, including the pthread mutex/cond declarers.

**Payment:** Move the channel cluster to `src/codegen/channels.zig`, a sibling of `list.zig`
and `map.zig`, before or as part of the Bug 85-87 fixes, so their new code lands there.

**Found by:** /qa-review WATCH [file-growth], seen on ci-baseline-zig-016, fix/bug-79-integration-crash and fix/bug-83-sender-param (2026-09-27 to 2026-09-29) — Growth — reviewer's reading, not re-verified.

## [ ] Debt 15: Channel endpoint paths through a Set loop, a generic `for` binding and a Receiver index are untested

**Kind:** test-gap
**Where:** `src/codegen/emit.zig` — `emitForLoopSet` binding `semantic_type`, `forBindingType`, `localPathType`'s `.index` arm
**Due when:** touching `localPathType`, `forBindingType` or the `emitForLoop*` emitters in `src/codegen/emit.zig`

**Description:** `test/native/channel_index_endpoints.kl` pins Sender sends through array and
List indexes and loops. A `for` over a `Set` of endpoints, a `for t: Sender#[T]` binding inside
a generic function, a Receiver reached by a List index, and `close()` through an index are
not covered, so a regression on those paths would pass the suite.

**Payment:** Add cases to `channel_index_endpoints.kl`: a Set loop (or record that a Sender
does not hash), a generic `fn f#[T]` with a `for` binding, `rxl[0].recv()` over
`List#[Receiver#[i64]]`, and `txs[0].close()` followed by a `recv` that returns `None`.

**Found by:** /qa-review WATCH [test-coverage] on `src/codegen/emit.zig`, seen on fix/bug-83-sender-param, fix/bug-86-channel-field-alias and fix/bug-90-channel-index-for (2026-09-29) — TestCov, GenA — reviewer's reading, not re-verified.

---

## [ ] Debt 16: `fn main(args: Args)` with `type Args = [String]` may not get the args wrapper

**Kind:** latent-defect
**Where:** `src/codegen/emit.zig` — `is_main_with_args` and the prototype's slice split (`:1551-1553`, `:1583`)
**Due when:** touching `is_main_with_args` or the `declareFunctionPrototype` parameter loop in `src/codegen/emit.zig`

**Description:** `is_main_with_args` and the prototype's slice split test
`func.params[0].type_` by its spelling, while `emitFunction` now tests the alias-resolved
`param_type`. They agree today because both check `is_main_with_args` first, but a `main`
whose parameter is an alias of `[String]` would not be recognised as taking args.

**Payment:** Build `type Args = [String]` / `fn main(args: Args) -> i32 { return args.len() }`
and run it with two arguments. If it does not return 3, resolve the alias in
`is_main_with_args` with `resolveAliasTypeExpr` and add the program as a native test.

**Found by:** /qa-review WATCH [correctness] on `src/codegen/emit.zig`, promoted at its fifth
sighting (ci-baseline-zig-016, fix/bug-83-sender-param, fix/bug-86-channel-field-alias,
fix/bug-90-channel-index-for, fix/bug-89-type-alias-lowering, 2026-09-29) — GenA — reviewer's
reading, not re-verified.

---

## [ ] Debt 17: Compound `/=`/`%=` by zero and the narrow-index extension are unpinned at most sites

**Status:** Open
**Kind:** test-gap
**Where:** `src/codegen/emit.zig` — compound division on an element, field and deref (~5370, ~5458, ~5500); `ref arr[k]` (~10470, ~10494) and computed array/slice index (~10330, ~10396); `test/native/runtime_traps.kl`
**Due when:** touching `emitCheckedIndex` or `emitCheckedDivRem` in `src/codegen/emit.zig` · touching `test/native/runtime_traps.kl`

**Description:** `runtime_traps.kl` covers binary `/` and `%` only, so reverting the element,
field or deref `/=`/`%=` site to a bare `sdiv` stays green, and `%=` by zero is tested
nowhere. `runtime_trap_lowering` counts failure blocks, so a narrow-index site reverted to
the old zero-extended compare still passes: it keeps its `bounds.fail` block. The exact
`TRAP_BLOCK_FLOOR` (33) must also be raised by hand when a site is added, or a lost check
hides behind the new one.

**Payment:** Add `arr[i] /= z`, `h.n /= z` and `%=` lines to `runtime_traps.kl` and raise
`TRAP_BLOCK_FLOOR`, or add `runtime_checks/mod_assign_by_zero.kl`. Add
`runtime_checks/index_neg_i8_ref.kl` (`ref arr[k]` with an i8 -1, trap) and a u8 200 read
through a computed array in `unsigned_index_and_division.kl`.

**Found by:** /qa-review on fix/bug-74-75-runtime-checks, 2026-09-30 — TestCov (GAPS, and a
WATCH on `runtime_traps.kl` seen on ci-baseline-zig-016 and fix/bug-83-sender-param) —
reviewer's reading, not re-verified.

---

## [ ] Debt 18: The native runner's trap and timeout branches have no self-test of their own

**Status:** Open
**Kind:** test-gap
**Where:** `scripts/run-native-tests.sh:201-203`, `:212-213`, `:256-267`, `:309`
**Due when:** touching `scripts/run-native-tests.sh`

**Description:** If `expects_trap` stops matching the `// Expected: trap` marker, every trap
test falls through to "any exit" and passes. `native_timeout_branch` exercises only exit 124,
with its own `-k 5 1` command rather than `RUN_TIMEOUT`, so dropping `-k` stays green. On
Windows any non-zero exit passes a trap test.

**Payment:** A self-check like `native_timeout_branch` that builds a skipped fixture carrying
the trap marker and returning 0, and asserts it is reported failed; derive the timeout
self-test's command from `RUN_TIMEOUT` with the limit substituted.

**Found by:** /qa-review on fix/bug-74-75-runtime-checks, 2026-09-30 — TestCov (GAPS, and a
WATCH on the runner seen on fix/bug-79-integration-crash and fix/bug-90-channel-index-for) —
reviewer's reading, not re-verified.

---

## [ ] Debt 19: `compat.zig`'s open modes, `truncate = false` and `seekTo` are unpinned

**Status:** Open
**Kind:** test-gap
**Where:** `src/compat.zig:509-515` (`openFile` modes), `:523-535` (`createFile`), `:377-382` (`seekTo`)
**Due when:** touching `src/compat.zig`

**Description:** Bugs 68/69's tests pin truncate, exclusive and deleteTree, but `createFile`
with `.read = true` (RDWR) or `truncate = false`, `openFile` `.write_only`/`.read_write`, and
`seekTo`'s `SEEK.SET` have no test. A break that always sets `.TRUNC` passes every test.

**Payment:** In `src/compat_test.zig`: `createFile(name, .{ .read = true, .truncate = false })`
on an existing file keeps the old bytes and reads them back through the same handle;
`openFile(.read_write)` writes then reads back; a `.read_only` handle refuses a write;
`seekTo(0)` after a write rereads from the start.

**Found by:** /qa-review on fix/bug-68-69-compat-flags, 2026-10-03 — TestCov (GAPS, and a
WATCH on `compat.zig` test coverage seen on ci-baseline-zig-016 and ci-upgrade-runners-actions)
— reviewer's reading, not re-verified.

---

## [ ] Debt 20: Windows `fileSeekTo` does not compile on Zig 0.16 (`SetFilePointerEx` is gone)

**Status:** Open
**Kind:** latent-defect
**Where:** `src/compat_windows.zig:116-121`
**Due when:** touching `src/compat_windows.zig` · when anything calls `compat.File.seekTo` on Windows

**Description:** `fileSeekTo` calls `std.os.windows.kernel32.SetFilePointerEx`, which Zig
0.16's kernel32 no longer declares. Zig compiles lazily, so the Windows build stays green only
because no Windows path reaches `seekTo` today. The first change that does will break the
nightly Windows job.

**Payment:** Re-implement it on `Io.File` (the pattern `fileStat` above it uses), or declare
`SetFilePointerEx` locally with `extern "kernel32"`. Confirm with a Windows type-check of
`seekTo` (`zig build-obj -target x86_64-windows` on a probe that calls it).

**Found by:** Builder's cross-target type-check on fix/bug-68-69-compat-flags, 2026-10-03 —
reported in its result, not routed by /qa-review; code re-read at `compat_windows.zig:116-121`,
not compiled since.
