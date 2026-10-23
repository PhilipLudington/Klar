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

---

## [ ] Debt 21: Array-to-slice coercion's heap backing and less common sources are unpinned

**Status:** Open
**Kind:** test-gap
**Where:** `src/codegen/emit.zig` `emitSliceValue` (~32681), struct-literal slice field (~9884), `emitAssignment` (~5144); `test/native/array_to_slice_coercion.kl`
**Due when:** touching `src/codegen/emit.zig`'s `emitSliceValue` · touching `test/native/array_to_slice_coercion.kl`

**Description:** The Bug 97 test pins `let`, `var` and plain `=` from an array variable or a
`@repeat`. Three things it does not pin: that the heap backing outlives the frame (case 7 would
still pass on a stack backing, since nothing overwrites the dead frame before the read); a
plain `=` from a large `@repeat`, or a non-variable array source (a call returning `[i32; 3]`,
a struct's array field) at a `[T]` declaration; and a slice-typed struct field initialized from
an array value, which the checker rejects today, so the emitter path is unreachable.

**Payment:** Add cases to `array_to_slice_coercion.kl`: return a slice built from a local
array, call a function that dirties a large local buffer, then read the slice; `f = @repeat(8,
5000)` into a `[i32]`; `let s: [i32] = make_arr()` and `let s: [i32] = h.arr`. Each must go red
when `emitSliceValue` stack-allocates or is bypassed.

**Found by:** /qa-review on fix/bug-97-array-slice-coercion, 2026-10-04 — QA Test Coverage
Review GAPS; reviewer's reading, not re-verified.

---

## [ ] Debt 22: Bug 98's alias test pins two readers, weakly, and fails on unfixed code only some runs

**Status:** Open
**Kind:** test-gap
**Where:** `test/module/type_alias_elements/`; `src/codegen/emit.zig` `resolveExpectedType` (~7952) and the readers routed through it
**Due when:** touching `test/module/type_alias_elements/` · touching `src/codegen/emit.zig`'s `resolveExpectedType` or `substituteAliases`

**Description:** The test pins a `var` `List#[Id]` local, a free-function `List#[Id]`
parameter and a `Map` value. Its `map_key` case returns its own local after
`contains_key(v)`, so a key truncated the same way on insert and lookup still passes. The
wrong-module pick depends on a pointer-keyed map walk, so unfixed code goes red only in some
runs (4 of 6 and 3 of 8 in the Builder's breaks). No alias case reaches a `let` local, an
impl-method parameter, the array or slice readers, `Set`, the three `with_capacity`
constructors, `channel_create`, a variant payload, the `resolveTypeExprDirect` fallbacks or a
nested element (`List#[?Id]`, `Map#[string, List#[Id]]`). That no other direct
`resolveTypeExpr` call is left in `emit.zig` is checked only by grep.

**Payment:** In `wide.kl`: `map_key` asserts `contains_key(705032707.as#[i64])` is false
after inserting 5000000003 (the low 32 bits); add a `let` local, an impl-method parameter, a
`[Id; 2]` local and `[Id]` parameter, `Set.with_capacity#[Id]`, `List.with_capacity#[Id]`,
`Map.with_capacity#[string, Id]`, `channel_create#[Id]` and `List#[?Id]`, each holding a value
above the `i32` range. Make a wrong pick near-certain: several sibling modules each declaring
a differently sized `Id`, or loop the test in `run-module-tests.sh`. Each case must go red
when its reader calls the checker's `resolveTypeExpr` directly.

**Found by:** /qa-review on fix/bug-98-alias-element-scope, 2026-10-04 — QA Test Coverage
Review GAPS and WATCH (`emit.zig` `[test-coverage]` seen in 4 prior branches); reviewer's
reading, not re-verified.

---

## [ ] Debt 23: `resolveTypeExprDirect`'s cross-module fallback reads an alias by bare name, from any module

**Status:** Open
**Kind:** latent-defect
**Where:** `src/codegen/emit.zig` `resolveTypeExprDirect` `.named` branch (~7629), `src/checker/checker.zig` `lookupSymbolAcrossModules` (~2679)
**Due when:** touching `src/codegen/emit.zig`'s `resolveTypeExprDirect` · touching `lookupSymbolAcrossModules`

**Description:** When the scoped resolve returns null, the `.named` branch falls back to
`lookupSymbolAcrossModules(n.name)`, which walks a pointer-keyed map of module scopes. A name
the current module cannot place therefore resolves to whichever module's symbol of that name
the walk reaches first, which is the mechanism behind Bug 98. Not shown reachable today: the
scoped resolve finds every alias the current module declares or imports.

**Payment:** Find a named type the scoped resolve misses while another module declares the
same name (an alias used only inside a generic body monomorphized from another module is the
first candidate), and show it reading the wrong width. If none exists, restrict the fallback
to `pub` symbols of imported modules, or remove it once the Zig multi-module selfhost build
no longer needs it (MEMORY: removing it broke that build once).

**Found by:** /qa-review on fix/bug-98-alias-element-scope, 2026-10-04 — GenA WATCH
(`emit.zig` `[correctness]` seen in 6 prior branches, promoted on recurrence); reviewer's
reading, not re-verified.

---

## [x] Debt 24: Deref reads and `op=` through a non-i32 reference are pinned only by the u8/i64 happy path

**Status:** Paid
**Kind:** test-gap
**Where:** `test/native/deref_compound_assign.kl`, `test/native/ref_inout.kl`, `src/codegen/emit.zig` `inferExprType` `.deref` arm (~9111)
**Due when:** touching `inferDerefType` or `emitDerefAssignment` in `src/codegen/emit.zig` · touching `test/native/deref_compound_assign.kl`

**Description:** Bug 99's test pins every `op=` at its width, but every u8 value is below
128, so signed and unsigned `/=`/`%=` give the same answer. No test reads a scalar `*x`
through `ref i64`/`ref u8`, and nothing observes `inferExprType` on `*p` for a reference
parameter that is not i32. All pass today (GenA probe: u8 200/3 = 66, 250 % 7 = 5).

**Payment:** In `deref_compound_assign.kl`, `div_u8` on 200 by 3 expects 66 and `mod_u8`
on 200 by 7 expects 4. In `ref_inout.kl`, `read_i64(x: ref i64) -> i64 { return *x }` on
a value above 2^32. One native test that interpolates `"{*p}"` for an `inout i64` above
2^32 and compares the string.

**Found by:** /qa-review on fix/bug-99-deref-compound-assign, 2026-10-17 — TestCov GAPS;
reviewer's reading, not re-verified.

**Paid:** 2026-10-19, fix/bug-111-compound-assign-overflow (due: the branch rewrote
`emitDerefAssignment`'s arms). `deref_compound_assign.kl` divides a u8 200 by 3 (66) and
takes 200 % 7 (4) through `inout u8`; `ref_inout.kl` reads an i64 above 2^32 through
`ref i64` and interpolates `"{(*p)}"` for an `inout i64` above 2^32. The interpolation is
`{(*p)}`, not the payment's `{*p}`: `*` does not start an interpolated expression, so
`"{*p}"` is the literal text.

---

## [ ] Debt 25: The native runner's build step has no timeout

**Status:** Open
**Kind:** test-gap
**Where:** `scripts/run-native-tests.sh` build step (~231); only the run (~244) is under `timeout`
**Due when:** touching `scripts/run-native-tests.sh`

**Description:** A compiler hang stalls the whole suite instead of failing one test. Bug
99's unfixed code hung `klar build` on the u8 case, so its regression test would stall
`./run-tests.sh` rather than turn red if that hang came back.

**Payment:** Wrap the build in `timeout -k 5 "$NATIVE_TEST_TIMEOUT"` like the run, report a
build timeout as a FAILED entry reading "build timed out", and add a self-test beside
`native_timeout_branch` (Debt 18) that proves it.

**Found by:** /qa-review on fix/bug-99-deref-compound-assign, 2026-10-17 — TestCov WATCH
(`run-native-tests.sh` `[test-coverage]` seen in 3 prior branches, promoted on
recurrence); reviewer's reading, not re-verified.

---

## [ ] Debt 26: Bug 106's Cell test pins only `let`/`var` bindings from `Cell.new`, and the Cell match lives in two places

**Status:** Open
**Kind:** test-gap
**Where:** `src/codegen/emit.zig` `isRcType` (~5927), `isCellExpr` (~5890), `tryGetRcInnerType` (~5818); `test/native/cell_shapes.kl`; `scripts/run-native-tests.sh` `cell_drop_lowering`
**Due when:** touching `isCellExpr`, `isRcType` or `tryGetRcInnerType` in `src/codegen/emit.zig` · touching `test/native/cell_shapes.kl`

**Description:** Bug 106's fix is pinned for `let`, `var`, alias and inner-block Cells
built by `Cell.new`. Three things are not pinned. (1) `isRcType`'s `!local.is_cell` term:
deleting it keeps every test green, because the checker rejects `clone()` on a Cell today
(GenA probe). (2) A Cell that arrives as a function parameter, a struct field or a call's
return value. None registers an Rc droppable today, because `tryGetRcInnerType` returns
null for them. (3) `cell_drop_lowering` counts `klar_rc_drop` call sites with `grep -c`,
so a lost Rc drop and a stray Cell drop would cancel out to 1. Separately, `isCellExpr`
repeats `tryGetRcInnerType`'s `Cell`/`new` match, so a second Cell constructor would have
to be added to both.

**Payment:** Add a checker test that pins the rejection of `clone()` on a `Cell#[i32]`. Add
`cell_shapes.kl` cases for a Cell parameter and a Cell returned from a function. Make
`cell_drop_lowering` check that the one call is inside `rc_still_dropped`'s body. When the
smart-pointer predicates move out of `emit.zig` (Debt 14's seam), make `isCellExpr` the
single Cell match that `tryGetRcInnerType` calls.

**Found by:** /qa-review on fix/bug-106-cell-get, 2026-10-17 — TestCov GAPS and WATCH,
GenA WATCH (`emit.zig` `[duplication]` seen in 3 prior branches), GenB WATCH
(`run-native-tests.sh` `[test-coverage]` seen in 4); reviewer's reading, not re-verified.

---

## [ ] Debt 27: Byval parameter binding is pinned only for generic functions, not for sret plain functions or methods

**Status:** Open
**Kind:** test-gap
**Where:** `src/codegen/emit.zig` `emitFunction` (~2385, `isByvalParam(byval_info, llvm_param_idx - 1 - param_offset)`), the method binders (~2052, ~38819); `test/native/generic_byval_sret.kl`
**Due when:** touching `bindParamAlloca` or `isByvalParam` in `src/codegen/emit.zig` · when method declarations start passing parameters byval

**Description:** Bug 107's fix routes both function binders through `bindParamAlloca`.
Its tests pin generic functions only. Two shapes are unpinned. (1) A plain (non-generic)
function that returns through sret and takes a struct over 8 bytes after a scalar. That
is the only case that exercises the `- param_offset` index in `emitFunction`. (2) A method,
plain or generic-impl, taking a struct over 8 bytes or a closure. Methods declare no byval
parameters today (claim C5, argued from code). A later change that made them byval would
bring Bug 107 back in methods with every test green.

**Payment:** Add `fn shift(n: i32, p: Point3) -> Point3 { return p }` to
`generic_byval_sret.kl` or a sibling, and assert all three fields. Add
`impl Box { fn take(self, p: Point3) -> f64 { return p.z } }` plus a generic-impl method
with `T = Point3`, each asserting a field.

**Found by:** /qa-review on fix/bug-107-generic-fn-param, 2026-10-17 — TestCov GAPS;
reviewer's reading, not re-verified.

---

## [ ] Debt 28: A String copy read after the original's drop(), and a String read after its List's drop(), are unpinned

**Status:** Open
**Kind:** test-gap
**Where:** `src/codegen/emit.zig` `emitStringDrop` (~24032), `emitListDrop` (~18583); `test/native/string_drop.kl`, `test/native/list_string_drop.kl`
**Due when:** touching `emitStringDrop` or `emitListDrop` in `src/codegen/emit.zig` · touching `test/native/string_drop.kl`

**Description:** Bug 108's fix keeps the String header on drop() and resets it to empty
in place, and its doc comment gives copies sharing the header as the reason. No test
takes a copy, drops the original and reads the copy. Separately, the fix's claim that
`List#[String].drop()` frees only strings nothing reads afterwards is unpinned: no test
copies an element out of the list, drops the list and reads the element.

**Payment:** In `string_drop.kl`: `var a: String = String.from("abc"); let b: String = a;
a.drop()`, then assert `b.len() == 0`, push to one and assert the other sees it. In
`list_string_drop.kl`: `let e: String = list[0]`, drop the list, then read `e`, and either
pin what it does or make reading it a compile error.

**Found by:** /qa-review on fix/bug-108-string-drop, 2026-10-18 — TestCov GAPS;
reviewer's reading, not re-verified.

---

## [ ] Debt 29: `emitStringDrop` frees the data buffer without knowing it was malloc'd

**Status:** Open
**Kind:** latent-defect
**Where:** `src/codegen/emit.zig` `emitStringDrop` (~24060, `free(data_ptr)`)
**Due when:** touching `emitStringDrop` in `src/codegen/emit.zig` · when a String can be built over a literal or borrowed buffer

**Description:** `drop()` calls `free` on whatever the header's data pointer holds. If any
path builds a String whose buffer is a string literal or borrowed memory, dropping it
frees memory malloc never returned. Nobody has checked every String constructor. This was
true before Bug 108's fix too. `emit.zig` `[correctness]` WATCH items have now been seen
in 10 earlier branches.

**Payment:** List every place that stores a data pointer into a String header
(`allocateStringHeader` users, `String.from`, method results) and confirm each one is a
malloc'd copy. If one is not, write the test that drops it (expect a crash or an ASan
report) and file it to BUG.md.

**Found by:** /qa-review on fix/bug-108-string-drop, 2026-10-18 — GenA WATCH (`emit.zig`
`[correctness]` seen in 10 prior branches); reviewer's reading, not re-verified.

---

## [ ] Debt 30: `xcrunSDKPath`'s fallback branches are unpinned

**Status:** Open
**Kind:** test-gap
**Where:** `src/codegen/linker.zig` `xcrunSDKPath` (~361-379)
**Due when:** touching `src/codegen/linker.zig` `findMacOSSDK` or `xcrunSDKPath`

**Description:** Bug 116's test pins only the case where xcrun answers. No test reaches the
branches that return null and fall back to the fixed SDK paths: a non-zero exit, a signal,
empty or whitespace-only output, output that fills the 4096-byte buffer, or a spawn failure.
The command is hard-coded and the result lands in a global buffer, so the branches cannot be
driven as the code stands.

**Payment:** Split the output and exit handling into a pure helper, e.g.
`sdkPathFromXcrun(stdout: []const u8, term: ?Term) ?[]const u8`, and assert null for each
case above, plus `"/path/MacOSX.sdk\n"` → `"/path/MacOSX.sdk"`.

**Found by:** /qa-review on fix/bug-116-macos-sdk, 2026-10-19 — test-checker GAPS;
reviewer's reading, not re-verified.

---

## [ ] Debt 31: `src/main.zig` is far over the file-size limit; the test runner is one clean seam

**Status:** Open
**Kind:** extraction
**Where:** `src/main.zig` (7433 raw lines on 2026-10-19), `runTestPath`/`runTestFile` cluster (~4881-5925)
**Due when:** touching `src/main.zig` `runTestPath` or `runTestFile`

**Description:** The CLI entry point is about 3.7× the 2000-line raw ceiling and keeps
growing (+378/−216 over its last 15 commits). File Growth has flagged it on three branches.

**Payment:** Move the test runner cluster (`runTestPath`, `runTestFile` and their helpers,
about 1000 lines) to `src/test_runner.zig`.

**Found by:** /qa-review on fix/bug-116-macos-sdk, 2026-10-19 — file-size-checker WATCH,
also seen on fix-bug-68-69-compat-flags and fix-gc-reachability; reviewer's reading, not
re-verified.

---

## [ ] Debt 32: Native compound assignment's zero-divisor, signed-deref and IR-trap paths are unpinned

**Status:** Open
**Kind:** test-gap
**Where:** `src/codegen/emit.zig` `emitCompoundValue`, `emitDerefAssignment`; `test/native/runtime_traps.kl`
**Due when:** touching `src/codegen/emit.zig` `emitCompoundValue` or `emitNumericOp`

**Description:** Bugs 111 and 117 pinned one operator per compound-assignment target. Still
unpinned: `%=` by zero (only a variable `/=` by zero is tested), a signed overflow through a
deref (the only deref overflow test is `inout u8`, so `isExprSigned` misreading the deref
operand as unsigned would pass), the element and field targets beyond `+=` and `*=`, and the
IR-level trap check in `runtime_traps.kl`, which has no `op=` site.

**Payment:** `runtime_checks/mod_assign_by_zero.kl` (`x %= args.len() - 1`, expected trap);
`runtime_checks/deref_add_assign_i32_overflow.kl` (`*p += 1` on an `inout i32` holding
2147483647); add `x += n` and `arr[0] *= n` to `runtime_traps.kl` and raise
`TRAP_BLOCK_FLOOR`.

**Found by:** /qa-review on fix/bug-111-compound-assign-overflow, 2026-10-19 — test-checker
GAPS and WATCH; reviewer's reading, not re-verified.

---

## [ ] Debt 33: Interpolating an `f32` prints garbage natively

**Status:** Open
**Kind:** latent-defect
**Where:** `src/codegen/emit.zig` string interpolation of float values
**Due when:** touching `src/codegen/emit.zig` string interpolation, or `src/codegen/strings_emit.zig`

**Description:** `"{f}"` with `f: f32` printed `5.28427e-315` and `nan` in a native probe,
with no compound assignment involved; the f32 is likely passed to a `%g` formatter without
promotion to double. Seen by the generalist while probing Bug 117; not reproduced since.

**Payment:** Reproduce with `let f: f32 = 1.5.as#[f32]` and `println("{f}")` under
`klar run`; if it misprints, file it as a bug with that test, then promote the f32 with
`fpext` before formatting.

**Found by:** /qa-review on fix/bug-111-compound-assign-overflow, 2026-10-19 — code-reviewer
WATCH (emit.zig `[correctness]` WATCH seen in 11 prior branches); reviewer's reading, not
re-verified.

---

## [ ] Debt 34: The native runner accepts any exit from an unregistered test, so 84 tests' failure returns are never checked

**Status:** Open
**Kind:** tooling
**Where:** `scripts/run-native-tests.sh` `get_expected` (`*) echo -1`, ~line 192)
**Due when:** touching `scripts/run-native-tests.sh` · before the next native test is added

**Description:** `get_expected` falls through to `-1` ("accept any result") for every test
it does not name. 319 of 418 `test/native/**/*.kl` files are unnamed and are neither trap
nor build-error tests; 84 of those have a `main` that ends in `return 0` with earlier
`return N` failure branches, so a regression that takes one of those branches still prints
`✓` and counts as passed. Found when PR 68's review showed two new tests (`ref_inout`,
`float_compound_assign`) could not fail; that PR registered only those two. Counted by
reading the files, not by running them: which of the 84 fail today is unknown.

**Payment:** Run each unregistered test once and record its exit. Then change the default to
`0`, register the tests whose success value is not 0 (e.g. `return 42`) by name, and
fix or file as bugs any whose exit today is a failure branch.

**Found by:** side-agent note on PR 68's /flow card, 2026-10-19; counted by the main session
(static read of `test/native`), not re-run.

---

## [ ] Debt 35: `tryGetClosureTypeInfo` drops a whole signature when one type fails to lower

**Status:** Open
**Kind:** latent-defect
**Where:** `src/codegen/emit.zig` `tryGetClosureTypeInfo` (~5832)
**Due when:** touching `src/codegen/emit.zig` `tryGetClosureTypeInfo`

**Description:** A parameter or return type that `typeExprToLLVM` rejects makes the
function `catch return null`, so the call silently falls back to `fn(..i32) -> i32`, the
failure mode of Bug 113. Since that fix, a rejected parameter also drops the return type
it used to keep. No input that triggers it is known.

**Payment:** Find a type `typeExprToLLVM` rejects in a function type; if a call through it
miscompiles, file a bug with that test. Then propagate `EmitError` instead of returning null.

**Found by:** /qa-review on fix/bug-113-fn-typed-params, 2026-10-20 — code-reviewer
(PRE-EXISTING) and test-checker (WATCH, emit.zig `[test-coverage]` seen in 6 prior
branches); reviewer's reading, not re-verified.

---

## [ ] Debt 36: Closure parameter metadata and `var`-bound closure calls are untested

**Status:** Open
**Kind:** test-gap
**Where:** `src/codegen/emit.zig` `emitClosure` → `paramLocalValue` (~13137), the `var` binder (~5828)
**Due when:** touching `src/codegen/emit.zig` `emitClosure` · touching `test/native/closure_struct_param.kl`

**Description:** Since Bug 114's fix a closure parameter gets reference, string, collection
and alias metadata as a function parameter does; only struct and function-typed closure
parameters are tested. No test calls a `var`-bound closure with a non-`i32` signature.

**Payment:** Extend `closure_struct_param.kl` with closures taking `ref Point`, a `String`,
a `List#[i64]` and an aliased struct type, and `fn_typed_param_calls.kl` with
`var g: fn(f64) -> f64 = half` called on `9.0`, each with its own failure return.

**Found by:** /qa-review on fix/bug-113-fn-typed-params, 2026-10-20 — test-checker.

---

## [ ] Debt 37: `closure_sig_arena` freeing has no leak-checked test

**Status:** Open
**Kind:** test-gap
**Where:** `src/codegen/emit.zig` `closure_sig_arena` (~184, deinit ~718)
**Due when:** touching `src/codegen/emit.zig` `closure_sig_arena`

**Description:** The signature slices moved into an arena freed with the emitter; nothing
emits a function-typed name under a leak-checking allocator, so a lost `deinit` would pass.

**Payment:** A Zig unit test in `emit.zig` that emits `fn applyf(f: fn(f64) -> f64, x: f64) -> f64`
under `std.testing.allocator`.

**Found by:** /qa-review on fix/bug-113-fn-typed-params, 2026-10-20 — test-checker.

---

## [ ] Debt 38: `linkWithClang`'s Linux branch omits `-lm` and the user's link flags

**Status:** Open
**Kind:** latent-defect
**Where:** `src/codegen/linker.zig` `linkWithClang` (~390-440, Linux branch ~414)
**Due when:** touching `src/codegen/linker.zig` `linkWithClang`

**Description:** `linkForPlatform` now ends every Linux link with `-lm`, so f64 `%` (LLVM
`frem` → libm `fmod`) links. `linkWithClang` is a second public Linux link path that adds
no `-lm` and ignores `link_libs`/`link_paths`, so any f64 `%` linked through it would fail
with `undefined reference to 'fmod'`. Nothing calls it today, so it cannot fail yet.

**Payment:** Delete `linkWithClang` if it stays unused; otherwise append `-lm` last on
Linux and pass the user's link options. Repro still needed: a caller on Linux linking an
object that contains `frem`.

**Found by:** /qa-review on fix/nightly-linux-libm, 2026-10-20 — code-reviewer; reviewer's
reading, not re-verified.

---

## [ ] Debt 39: The Linux link line's `-lm` placement is pinned only by a full CI link

**Status:** Open
**Kind:** test-gap
**Where:** `src/codegen/linker.zig` `linkForPlatform` (~291)
**Due when:** touching `src/codegen/linker.zig` `linkForPlatform`

**Description:** Only `test/native/float_compound_assign.kl` on the Linux CI job checks
the `-lm` fix, through the host-gcc path with no extra libraries. Nothing checks that
`-lm` comes after `link_libs` and `extra_objects` (gcc's `--as-needed` drops it otherwise),
that the cross-gcc path gets it, or that macOS and Windows do not.

**Payment:** Split argument building out of `linkForPlatform` into a pure function that
returns the args, and add a Zig unit test: a `.linux` platform with `link_libs = {"foo"}`
ends `"-lfoo", "-lm"`; `.macos` and `.windows` carry no `-lm`.

**Found by:** /qa-review on fix/nightly-linux-libm, 2026-10-20 — test-checker GAPS;
reviewer's reading, not re-verified.

---

## [ ] Debt 40: The VM's `bool.to_string()` bus-error fix (Bug 118) has no test

**Status:** Open
**Kind:** test-gap
**Where:** `src/vm.zig` `invokeMethod` `to_string` (~1422)
**Due when:** touching `test/args/string_to_string_backends.kl` · when Bug 122 is fixed

**Description:** Bug 118's fix stopped the VM freeing a bool's static `"true"`/`"false"`
text, which was a bus error. Nothing runs `bool.to_string()` under `--vm`, so a
regression of `needs_free` for bools would pass every gate.

**Payment:** Add a bool case to `test/args/string_to_string_backends.kl` that asserts
`true.to_string().len()` is 4 and `false`'s is 5 with `assert_eq`, so it fails on every
backend. Natively it stays red until Bug 122 is fixed, so land it with that fix.

**Found by:** /qa-review on fix/bug-118-string-to-string, 2026-10-20 — test-checker GAPS,
code-reviewer-b; reviewer's reading, not re-verified.

---

## [ ] Debt 41: `to_string()` on a `String` receiver is tested only natively

**Status:** Open
**Kind:** test-gap
**Where:** `test/args/string_to_string_backends.kl`, `src/vm.zig` `invokeMethod`, `src/interpreter.zig` `to_string`
**Due when:** when Bug 123 is fixed · touching `test/args/string_to_string_backends.kl`

**Description:** `test/native/string_data_to_string.kl` pins the `String`-receiver copy
natively. The VM and interpreter paths for a `String` receiver run in no test, because
`String.from` is undefined off-native (Bug 123).

**Payment:** Once Bug 123 is fixed, add `String.from("abc").to_string()` cases (a binding
and a temporary, growing the copy and checking the source) to
`test/args/string_to_string_backends.kl` with `assert_eq`.

**Found by:** /qa-review on fix/bug-118-string-to-string, 2026-10-20 — test-checker GAPS;
reviewer's reading, not re-verified.

---

## [ ] Debt 42: `emitStringFromPtr` measures a `string` with `strlen`, so an embedded NUL truncates it

**Status:** Open
**Kind:** latent-defect
**Where:** `src/codegen/emit.zig` `emitStringFromPtr` (~23359)
**Due when:** touching `src/codegen/emit.zig` `emitStringFromPtr`

**Description:** `String.from` and now `string.to_string()` both size the new String with
`strlen` on the source pointer. A `string` holding a `\0` byte would come out short. No
probe has produced such a `string` from Klar source yet; the WATCH note has recurred on
this file across 12 branches.

**Payment:** Build a `string` with an embedded NUL (an escape in a literal, or
`from_byte(0)` concatenation), call `.to_string()` natively, and compare `len()` with the
source's. If it is short, pass the source's known length into `emitStringFromPtr` instead
of calling `strlen`, and promote this entry to BUG.md.

**Found by:** /qa-review on fix/bug-118-string-to-string, 2026-10-20 — code-reviewer-a
WATCH, promoted on recurrence; reviewer's reading, not re-verified.

---

## [ ] Debt 43: Generic enum payload slots and struct-shaped variant stores are unpinned

**Status:** Open
**Kind:** test-gap
**Where:** `src/codegen/emit.zig` `registerMonomorphizedEnum` (~38050), the enum literal's
`.struct_` store loop (~10962-10976)
**Due when:** touching `src/codegen/emit.zig` `registerMonomorphizedEnum` · touching
`test/native/enum_payload_layout.kl`

**Description:** Bug 34's test pins the non-generic path and tuple payloads only. No
native test builds a generic enum with a `char` or padded-struct payload, and none
constructs a struct-shaped variant (`R { a: i8, b: i64 }`), so those two copies of the
sizing path could regress silently.

**Payment:** In `test/native/enum_payload_layout.kl`, add `enum Boxed#[T] { B(T), E }` as
`Boxed#[char]` (`'x'`) and `Boxed#[Mixed]`, each read back through a by-value argument
(exits 3, 4); add `enum Rec { R { a: i8, b: i64 } }` and extend the runner's
`enum_payload_offsets` check to require `i64 <v> 8` on an `%enum.payload.field` store.

**Found by:** /qa-review on fix/bug-34-enum-payload-layout, 2026-10-21 — test-checker
GAPS; reviewer's reading, not re-verified.

---

## [ ] Debt 44: The native runner's IR checks depend on exact value names and GEP spelling

**Status:** Open
**Kind:** test-gap
**Where:** `scripts/run-native-tests.sh` (the `enum_payload_offsets` block, ~152-158, and
the Bug 73 / Bug 106 IR checks beside it)
**Due when:** touching `scripts/run-native-tests.sh` · when the emitter renames
`%enum.payload.*` values

**Description:** The IR checks grep `%enum.payload.(elem|field)N = getelementptr …` and
split `store <ty> <val>,` by field position. A rename or a different GEP form turns them
red without a real regression. They fail loudly, so this is brittleness, not a hidden
pass; the WATCH note has recurred on this file across 6 branches.

**Payment:** Move the IR checks into `scripts/native-ir-checks.sh` (also the File Growth
seam for this file) with one helper that matches a store's type, value and offset by
pattern rather than field position, and a self-test on a fixed IR snippet.

**Found by:** /qa-review on fix/bug-34-enum-payload-layout, 2026-10-21 — code-reviewer and
test-checker WATCH, promoted on recurrence; reviewer's reading, not re-verified.

---

## [ ] Debt 45: `variantPayloadStruct`'s `void` element and the literal's equal-length loops are unchecked

**Status:** Open
**Kind:** latent-defect
**Where:** `src/codegen/emit.zig` `variantPayloadStruct` (~38206-38216), `emitEnumLiteral`
(~10942, ~10963)
**Due when:** touching `src/codegen/emit.zig` `variantPayloadStruct`

**Description:** A `void` payload element becomes an empty struct (size 0), but the
literal still emits and stores that element. The literal's `for (lit.payload,
tuple_types, 0..)` loops assume the checker already matched the arity. No constructible
input reaches either case yet; the `[correctness]` WATCH note has recurred on this file
across 13 branches.

**Payment:** Try a variant whose payload element is a `void`-returning call, and a
literal with the wrong payload count, through `klar build`. If either reaches codegen,
skip the store for a zero-size element and assert the arity, and promote this entry to
BUG.md.

**Found by:** /qa-review on fix/bug-34-enum-payload-layout, 2026-10-21 — code-reviewer and
test-checker WATCH, promoted on recurrence; reviewer's reading, not re-verified.

---

## [ ] Debt 46: Bug 5's entry-block hoist is IR-checked on one file, and nothing keeps new allocas going through it

**Status:** Open
**Kind:** test-gap
**Where:** `src/codegen/emit.zig` `buildEntryBlockAlloca` (~512-546) and its call sites;
`scripts/run-native-tests.sh` `alloca_entry_block` check
**Due when:** touching `src/codegen/emit.zig` `buildEntryBlockAlloca` · adding an alloca to
`src/codegen/emit.zig`

**Description:** The `alloca_entry_block` IR check reads only
`test/native/loop_stack_temporaries.kl` (14 allocas: `fs_stat`, `List.new`/`push`/`len` in
loops). The other converted sites — `process_run`, `env_get`/`env_set`, for-loops over
Map/Set/slice/array, struct, tuple and array literals, `Some`/`Ok`, sret and closure calls
— never have their IR checked. No check stops a new direct `self.builder.buildAlloca` or
`llvm.c.LLVMBuildAlloca` outside the helper. The helper-body path (allocas made while a
runtime helper is built) is pinned only by `string_primitives.kl` failing to build, and the
helper's two fallbacks (no insert block, no entry block) have no test.

**Payment:** A wider loop probe under the same IR check, with those constructs in loop
bodies; a runner step that allows `buildAlloca`/`LLVMBuildAlloca` only inside
`buildEntryBlockAlloca`; the IR check extended to the runtime-helper `define`s of
`string_primitives.kl`.

**Found by:** /qa-review on fix/bug-5-entry-block-allocas, 2026-10-22 — test-checker GAPS
and WATCH, code-reviewer-b WATCH (the `emit.zig` and `run-native-tests.sh`
`[test-coverage]` notes, seen in 9 and 7 prior branches); reviewer's reading, not
re-verified.

---

## [ ] Debt 47: The native runner's IR checks are inline in `run-native-tests.sh`, now in the Watch band

**Status:** Open
**Kind:** extraction
**Where:** `scripts/run-native-tests.sh` (561 raw, ~452 code lines)
**Due when:** touching `scripts/run-native-tests.sh` with another IR check

**Description:** Each new IR check (the latest, `alloca_entry_block`, +46 lines) is added
inline to the runner, which crossed into the Watch band on the Bug 5 branch.

**Payment:** Move the self-contained IR check blocks into their own script, called from the
runner, starting with `alloca_entry_block` (~46 lines).

**Found by:** /qa-review on fix/bug-5-entry-block-allocas, 2026-10-22 — file-size-checker
WATCH, seen also on fix/bug-34-enum-payload-layout and fix/bug-74-75-runtime-checks;
reviewer's reading, not re-verified.

---

## [ ] Debt 48: Bug 5's description still cites `emit.zig:491 at HEAD`

**Status:** Open
**Kind:** documentation
**Where:** `BUG.md` Bug 5 `**Description:**`
**Due when:** touching Bug 5's entry in `BUG.md`

**Description:** The fixed entry's description cites `emit.zig:491 at HEAD` and the
`e7b54e7` comparison; the line has moved since the fix.

**Payment:** Change the cite to the function name (`buildEntryBlockAlloca`) or mark it as
of the commit it was written at.

**Found by:** /qa-review on fix/bug-5-entry-block-allocas, 2026-10-22 — code-reviewer-b
WATCH (BUG.md `[documentation]` notes, seen in 5 prior branches); reviewer's reading, not
re-verified.

---

## [ ] Debt 49: The REPL's assignment check has no test

**Status:** Open
**Kind:** test-gap
**Where:** `src/checker/statements.zig` `checkAssignment`
**Due when:** touching `src/checker/statements.zig` · touching `src/repl.zig`

**Description:** The Bug 63 fix routed the REPL's `checkAssignment` (the statement node only
`parseReplInput` builds) through `checkAssignTarget`. That changed two REPL behaviours: `let`
reassignment is still refused with the new message, and an element write through a `let`
(`a[0] = 9`) is now accepted. No test reaches this path.

**Payment:** A Zig unit test in `src/repl.zig` beside the `parseCommand` tests: feed
`let x: i32 = 1` then `x = 2` and assert `mutability_error`; feed `let a: [i32; 2] = [1, 2]`
then `a[0] = 9` and assert no error.

**Found by:** /qa-review on fix/bug-63-immutable-assign, 2026-10-23 — test-checker GAPS;
reviewer's reading, not re-verified.

---

## [ ] Debt 50: Assignment-target refusals pinned for `=` and `+=` only

**Status:** Open
**Kind:** test-gap
**Where:** `src/checker/expressions.zig` `checkBinary`, `src/checker/type_utils.zig` `checkAssignTarget`
**Due when:** touching `src/checker/type_utils.zig`

**Description:** Of the compound operators on a `let`, only `+=` has a check test; `-=`,
`*=`, `/=`, `%=` share the switch arm but are unpinned. The `invalid_assignment` branch
(`1 = 2`, `f() = 3`) has no check test either.

**Payment:** One `test/check/` file per compound operator expecting `mutability_error` (the
runner reports one result per file), and `test/check/assign_non_place.kl` expecting
`invalid_assignment`.

**Found by:** /qa-review on fix/bug-63-immutable-assign, 2026-10-23 — test-checker GAPS;
reviewer's reading, not re-verified.

---

## [ ] Debt 51: Bug 63's `run` and `run --interpret` refusal is unpinned

**Status:** Open
**Kind:** test-gap
**Where:** `BUG.md` Bug 63 `**Fix:**`
**Due when:** touching `src/checker/type_utils.zig` · touching `scripts/run-app-tests.sh`

**Description:** Bug 63's Fix says the VM and interpreter now refuse a `let` reassignment
too, because the checker gates them. Only `check` and `build` are pinned.

**Payment:** An app or module test that runs `klar run` and `klar run --interpret` on the
`let x = 1; x = 2` program and asserts a non-zero exit with `mutability_error` in the output.

**Found by:** /qa-review on fix/bug-63-immutable-assign, 2026-10-23 — test-checker GAPS;
reviewer's reading, not re-verified.

---

## [ ] Debt 52: No suite checks `stdlib/` and `benchmarks/` on their own

**Status:** Open
**Kind:** test-gap
**Where:** `stdlib/*.kl`, `benchmarks/`, `scripts/run-check-tests.sh`
**Due when:** touching `src/checker/type_utils.zig` · when Bug 130 is taken

**Description:** Bug 63's claim that no tracked program is newly refused was a one-time
sweep. Bug 130 will tighten `checkAssignTarget` to element and field writes, which would
reject stdlib files, and nothing would notice.

**Payment:** A check-suite step that runs `klar check` over `stdlib/*.kl` (and the
benchmarks) and expects each to pass.

**Found by:** /qa-review on fix/bug-63-immutable-assign, 2026-10-23 — test-checker GAPS;
reviewer's reading, not re-verified.

---

## [ ] Debt 53: `src/checker/checker.zig` is 6094 raw lines, three times the ceiling

**Status:** Open
**Kind:** extraction
**Where:** `src/checker/checker.zig` `initBuiltins` (~793–2507), cursor/IDE queries (~3118–3735)
**Due when:** touching `src/checker/checker.zig` with more than a forwarder

**Description:** The file keeps growing with almost no deletions (+4, +8, +94, +60, +104,
+223 over the last 15 commits). It sits in the Limit band.

**Payment:** Move `initBuiltins` (~1715 lines, one function) to
`src/checker/builtins_init.zig`; then the `collect*AtCursor` / `inferExpectedTypeIn*` /
`extractScopeAtOffset*` group (~620 lines) to `src/checker/cursor_query.zig`.

**Found by:** /qa-review on fix/bug-63-immutable-assign, 2026-10-23 — file-size-checker
Growth Ledger (Limit band); reviewer's reading, not re-verified.

---

## [ ] Debt 54: Most of Bug 49's exit-status paths have no status test

**Status:** Open
**Kind:** test-gap
**Where:** `scripts/run-args-tests.sh` (Bug 49 section), `src/main.zig:866-934`, `src/main.zig:4538-4646`, `src/main.zig:748-780`
**Due when:** touching `scripts/run-args-tests.sh` · touching `src/main.zig` exit paths

**Description:** Bug 49's fix made many failure paths exit 1, but the args suite pins only
the six entry-point cases. Unpinned: `check --partial` (1 with diagnostics, 0 clean),
`--scope-at` / `--expected-type-at` exiting 0 on a program with type errors, `check`
argument errors (unknown option, a missing or invalid `--scope-at` value, both queries at
once, no input file), a query position out of range, a malformed `klar.json` under
`klar build`, and `build --target <bogus>`.

**Payment:** `expect_status` cases for each, e.g. `check exit_status_type_error.kl
--partial` → non-zero, `check no_args.kl --partial` → 0, `check exit_status_type_error.kl
--expected-type-at 3:18` → 0, `check no_args.kl --bogus` → non-zero, `check no_args.kl
--scope-at 999:1` → non-zero, `build no_args.kl --target not-a-triple` → non-zero.

**Found by:** /qa-review on fix/bug-49-exit-status, 2026-10-23 — test-checker GAPS;
reviewer's reading, not re-verified.

---

## [ ] Debt 55: The selfhost `OWNERSHIP_XFAIL` classifier's failure branches never run

**Status:** Open
**Kind:** test-gap
**Where:** `scripts/run-selfhost-tests.sh:286-327`
**Due when:** touching `scripts/run-selfhost-tests.sh` · when Bug 132 is taken

**Description:** Phase 1 marks four selfhost files as expected ownership failures. Its
"now passes, remove the mark" and "failed some other way" branches are never run, so a
wrong `grep` condition would leave a mark on after Bug 132 is fixed and nothing would say so.

**Payment:** Extract the Phase 1 classifier into a function and run it on a clean file and
a type-error file under an XFAIL name, asserting FAIL for each.

**Found by:** /qa-review on fix/bug-49-exit-status, 2026-10-23 — test-checker GAPS;
reviewer's reading, not re-verified.

---

## [ ] Debt 56: Nothing pins the selfhost files' use-after-move counts

**Status:** Open
**Kind:** test-gap
**Where:** `src/ownership/checker.zig` `moveInitializer`, `scripts/run-selfhost-tests.sh` Phase 1
**Due when:** touching `src/ownership/checker.zig` · when Bug 132 is taken

**Description:** Bug 115's fix claims the selfhost `klar check` results are unchanged: the
four `OWNERSHIP_XFAIL` files report 127, 12, 12 and 3 use-after-move errors. Phase 1 only
checks that those files fail. A new false move in one of them would raise its count and no
gate would notice.

**Payment:** In Phase 1, count `use of moved` lines per XFAIL file and compare them with a
recorded baseline (checker_decl 127, checker 12, module_resolver 12, parser 3). Fail when a
count goes up.

**Found by:** /qa-review on fix/bug-115-var-move, 2026-10-23 — test-checker GAPS;
reviewer's reading, not re-verified.
