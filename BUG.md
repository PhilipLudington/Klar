# Klar Compiler Bugs

## [x] Bug 1: `[String]` array indexing segfaults at runtime

**Status:** Fixed

**Root cause:** Two issues in native codegen:
1. LLVM's ARM64 backend mishandled `{ ptr, i64 }` aggregate parameters — the caller passed the slice in registers (x0/x1) but the callee read from the stack. Fixed by splitting `_klar_user_main`'s slice parameter into two scalar params (ptr, i64) and reconstructing the slice in the function body.
2. `isStringDataExpr` didn't recognize `.index` expressions on String arrays, causing `println(args[i])` to pass the String struct directly to puts instead of extracting the data pointer.

**Additional fix:** Added proper `target datalayout` strings for ARM64 and x86-64 platforms to the LLVM module (was missing for non-wasm targets).

---

## [x] Bug 2: Codegen crash with `Result` return types containing structs

**Status:** Fixed

**Root cause:** The sret (struct return) convention was applied inconsistently for large Result types — the function prototype would return void with an sret pointer parameter, but the `ret` instruction still tried to return the struct value directly. This was resolved by the byval/sret rework that unified large struct parameter and return handling in the LLVM codegen.

**Regression test:** `test/native/result_struct_return.kl` — covers Ok/Err paths, error propagation with `?`, and match on `Result#[Struct, Enum]`.

---

## [x] Bug 3: stdlib resolves from cwd instead of compiler binary location

**Status:** Fixed

**Root cause:** `findStdLibPath` only checked a few relative paths from the binary and missed the standard `/usr/local/lib/klar/stdlib` location. Additionally, `setStdLibPath` stored a raw pointer without duping, causing a use-after-free.

**Fix:**
1. Added `KLAR_HOME` environment variable support — if set, looks for `$KLAR_HOME/stdlib` first
2. Added `../lib/klar/stdlib` candidate path (resolves `/usr/local/bin/../lib/klar/stdlib`)
3. Fixed `setStdLibPath` to dupe the path into the resolver's arena
4. Updated `update-install.sh` to install stdlib to `/usr/local/lib/klar/stdlib/`

---

## [x] Bug 4: Tuple types containing `[char]` arrays crash codegen on import

**Status:** Fixed

**Root cause:** The LLVM codegen had incorrect type handling for tuple return types containing `[char]` slices when resolving imported module symbols. The crash manifested as exit code 133 or "UnsupportedFeature" errors, with LLVM verification failures like `Both operands to a binary operator are not of the same type!`. This was resolved as a side effect of the byval/sret rework for large struct parameter passing and the collection value semantics fixes (List/String heap indirection).

**Regression test:** `test/module/tuple_char_array/` — imports functions returning `(State, [char])` and `(char, State)` tuples across module boundaries, verifying tuple element access and chained calls.

---

## [ ] Bug 5: Builtin emitters allocate on the stack at the call site — `fs_stat` / `process_run` inside a loop grow the stack every iteration

**Status:** Open

**Description:** `buildEntryBlockAlloca` (`src/codegen/emit.zig:491` at HEAD) exists
and its comment states the reason: an `alloca` that is not in the function's entry
block allocates fresh stack space every time it executes, the space is not reclaimed
until the function returns, and LLVM's mem2reg only promotes entry-block allocas. The
Phase 0 builtin emitters ignore it and call `self.builder.buildAlloca` at the current
insertion point: at HEAD `emitFsStat` has 2 such allocas (including the 256-byte stat
buffer) and `emitProcessRun` 9 (buffer pointer, length, capacity, Result temporaries);
at the introducing commit `e7b54e7` the five emitters had 12 and no entry-block alloca.
When the Klar call sits inside a `while` / `for` body, those allocas are emitted into
the loop block and run once per iteration.

**Steps to reproduce:**
1. Native-build a program that calls `fs_stat("/tmp")` (or `process_run`) in a loop of
   ~50,000 iterations.
2. Run it.

**Expected:** Constant stack usage; the loop completes.

**Actual:** Stack grows by ~300+ bytes per iteration until the 8 MB main-thread stack
is exhausted and the process segfaults. (Established by reading the emitted-IR
construction, not reproduced at runtime.)

**Found by:** `/qa-review` calibration run over `e7b54e7` (Fixture 3), 2026-09-03 —
Fable 5.1 shard-A generalist, HIGH `[performance]`; verified by counting `buildAlloca`
vs `buildEntryBlockAlloca` in the two emitters at `e7b54e7` and at HEAD. Fix: use
`buildEntryBlockAlloca` for every alloca in `emitEnvGet`, `emitEnvSet`, `emitFsStat`,
`emitProcessRun` (and any later builtin that copied the pattern).

---

## [ ] Bug 6: x86_64 macOS — `stat` / `readdir` are declared with the unversioned symbols while the field offsets come from the `$INODE64` layout

**Status:** Open

**Description:** `getOrDeclareStat` (`src/codegen/emit.zig:31636` at HEAD) declares the
C symbol `stat` on every non-Windows target, and the `readdir` declaration likewise. The
struct offsets the emitter reads (`stat_mode_offset`, `stat_size_offset`,
`stat_mtime_offset`, `dirent_name_offset`) come from `@offsetOf(posix.struct_stat, …)`
via `@cImport`, and on macOS that is the 64-bit-inode layout. On x86_64 macOS the C
header maps `stat` to `stat$INODE64` with an asm label; a bare declaration named `stat`
in LLVM IR binds the **legacy** 32-bit-inode entry point, whose struct has `st_mode` at
offset 8 (not 4), `st_mtimespec` at 40 (not 48) and `st_size` at 72 (not 96). Zig's own
libc bindings select `stat$INODE64` / `readdir$INODE64` on x86_64 for this reason
(Zig 0.16 `std/c.zig:10585`, `:10500`). Apple Silicon has only the 64-bit-inode ABI, so
every macOS test so far has passed. Affects `fs_is_file`, `fs_is_dir`, `fs_stat`,
`fs_read_dir` on Intel Macs. Related: glibc before 2.33 exports no `stat` symbol at all
(only `__xstat`), so the same declaration fails to link on older Linux.

**Steps to reproduce:**
1. Build `klar` on an Intel Mac (or run the arm64 build under Rosetta with an x86_64
   target); native-compile a program calling `fs_is_dir("/tmp")`.
2. Run it.

**Expected:** `true`.

**Actual:** The mode test reads the low half of `st_ino` — wrong answer or garbage.
(Not reproducible on this Apple Silicon machine; established from the platform headers
and Zig's bindings.)

**Found by:** `/qa-review` calibration run over `e7b54e7` (Fixture 3), 2026-09-03 —
Fable 5.1 shard-A generalist (three independent arms), PRE-EXISTING; corroborated by
Zig 0.16 `std/c.zig`. Fix: when the target is x86_64-macos declare `stat$INODE64`,
`fstat$INODE64`, `readdir$INODE64` (as Zig does), or route these calls through a
Zig-compiled runtime shim so the C header does the mapping.

---

## [ ] Bug 7: Native test runner never enforces the exit code of `process_spawn` and `tcp_basic`

**Status:** Open

**Description:** `scripts/run-native-tests.sh` decides pass/fail with `get_expected`
(`:80-130` at HEAD `09b3b9b`), a hand-maintained `case` that falls through to
`*) echo -1` — "accept any result". The Phase 6 commit `20a5a3e` added
`test/native/process_spawn.kl` and `test/native/tcp_basic.kl` without adding their
cases; only `process_spawn_windows) echo 0` was ever added later (`:128`). Both tests
return non-zero on every failure path (`return 1` … `return 10`), and none of those
returns can fail the suite: the runner prints `✓ process_spawn (exit: 1)` and counts it
passed.

**Steps to reproduce:**
1. Edit `test/native/process_spawn.kl` so `main` returns 1 unconditionally.
2. `./scripts/run-native-tests.sh`.

**Expected:** `✗ process_spawn (expected: 0, got: 1)`, suite fails.

**Actual:** `✓ process_spawn (exit: 1)`, suite passes.

**Found by:** `/qa-review` calibration run over `20a5a3e` (Fixture 4), 2026-09-03 — Opus
shard-B generalist, CRITICAL `[test-coverage]`; verified in the fixture tree and at HEAD.
Fix: add `process_spawn) echo 0 ;;` and `tcp_basic) echo 0 ;;`, and consider making the
default expect 0 rather than accept anything (the same reviewer's PRE-EXISTING note), so
the next new test cannot silently opt out.

**Widened (2026-10-04):** the two tests above are two of many. Of the 338 tests in
`test/native/` that run normally (no `Expected: build-error`, `Expected: trap` or
`Skip: native-tests` header), **281 have no `get_expected()` entry**, so the `*) echo -1`
fallback passes them on any exit code. At `b9d81b2`, 194 of the 281 exit 0, 81 exit another
non-zero code (most look like deliberate returns such as 42, 50 or 15), and 7 exit ≥ 128.
Three of the 7 are expected aborts (`test_assert_fail`, `test_assert_eq_fail`, `test_panic`
→ 134). Three are real crashes reported as ✓: Bugs 106 (`cell_basic`), 107
(`meta_pure_generic`) and 108 (`string_drop`). The seventh, `hash_trait_string` → 133, is a
defect in the test: it subtracts two `i64` hashes (`h1 - h3`, -6615550055289275125 −
5717881983045765875), which overflows and traps as Klar defines it. It should compare the
hashes with `==`. (Count: PR 56 session, from the full run output; the four ≥ 128 cases
re-run individually 2026-10-04.)
New fix: the default expects 0. A test that returns a deliberate non-zero value gets an
explicit entry, each of the 81 is checked against what its source intends, and
`hash_trait_string` compares with `==`. This turns Bugs 106–108 red, so it lands after
they are fixed or on the same branch.

---

## [ ] Bug 8: `tcp_write` to a peer that has closed kills the process with SIGPIPE

**Status:** Open

**Description:** `emitTcpWrite` (`src/codegen/emit.zig:29719` at HEAD) emits
`send(fd, buf, len, 0)` (`:29767`) with flags 0, and nothing in the emitter sets
`SO_NOSIGPIPE` on the socket (macOS), passes `MSG_NOSIGNAL` (Linux), or installs a
SIGPIPE handler — `grep -n 'SIGPIPE\|NOSIGPIPE\|MSG_NOSIGNAL' src/codegen/emit.zig` is
empty. When the remote end has closed, the kernel raises SIGPIPE on the write and the
default disposition terminates the process before `send` can return `EPIPE`, so the
`Err(IoError)` path in `tcp_write` (and every stdlib caller's error handling) is
unreachable for the most ordinary network failure. `stdlib/http_server.kl`'s
`http_server_respond` is the usual victim: a client that aborts before the response
arrives takes the server down.

**Steps to reproduce:**
1. Native-build a program: `tcp_listen`, `tcp_accept`, then `tcp_write` a large response
   (> socket buffer) in a loop.
2. Connect with `curl` and kill it (Ctrl-C) mid-response, or connect with
   `nc 127.0.0.1 <port> </dev/null` which closes immediately, then have the server
   `tcp_write` twice.

**Expected:** `tcp_write` returns `Err(IoError)` (EPIPE / ECONNRESET); the program keeps
running.

**Actual:** The process exits with signal 13 (SIGPIPE); no Klar code runs after the
write. (Established by reading the emitted calls and the platform semantics of `send`
with flags 0; not reproduced at runtime.)

**Found by:** `/qa-review` calibration run over `20a5a3e` (Fixture 4), 2026-09-03 — Opus
shard-C generalist, HIGH `[error-handling]`, root cause in shard A's emitter; verified at
HEAD. Fix: on Linux pass `MSG_NOSIGNAL` (0x4000) to `send`; on macOS `setsockopt(fd,
SOL_SOCKET, SO_NOSIGPIPE, 1)` in `tcp_accept` / `tcp_connect` (SO_NOSIGPIPE = 0x1022);
UDP `sendto` in the Phase 9 builtins needs the same check.

---

## [ ] Bug 9: `process_read_stdout` lacks the `max_bytes > 0` guard that `tcp_read` has

**Status:** Investigating

**Description:** `emitTcpRead` rejects `max_bytes <= 0` with `Err` before allocating
(`src/codegen/emit.zig:29656-29668` at HEAD, the Phase 6 L7 hardening) and `udp_recv`
copies the guard (`:30290`). `emitProcessReadStdout` (`:29069`) does not: it sign-extends
`max_bytes` (`:29092`), computes `max_bytes + 1`, `malloc`s that (`:29095`) and calls
`read(fd, buf, max_bytes)`. For `max_bytes = 0` this is `malloc(1)` + `read(…, 0)` →
`Ok("")`, which callers can mistake for EOF; for `max_bytes = -1` it is `malloc(0)` +
`read(…, (size_t)-1)`. The reviewer's claim that the kernel then "writes far past the
allocation" is **overstated** — `read` with a count above `SSIZE_MAX` fails with `EINVAL`
on both Linux and macOS — but the inconsistency with `tcp_read` is real and the
`malloc(0)` result is then null-terminated through a pointer that may be NULL or a
zero-size block. Needs a runtime check of what `max_bytes = 0` and `= -1` actually do
before it is more than "inconsistent".

**Steps to reproduce:**
1. Native-build: `process_spawn("/bin/echo", ["x"])` then `process_read_stdout(h, 0)`
   and `process_read_stdout(h, -1)`.
2. Run it.

**Expected:** `Err(IoError)` for both, as `tcp_read` returns.

**Actual:** To be measured — `Ok("")` for 0 is what the code reads as; the −1 case
depends on the platform's `read` and the `malloc(0)` result.

**Found by:** `/qa-review` calibration run over `20a5a3e` (Fixture 4), 2026-09-03 — Opus
shard-A generalist, HIGH `[security]`; guard absence verified at HEAD, harm not verified.
Fix if confirmed: copy the `tcpr.max_ok` guard.

---

## [ ] Bug 10: `process_wait` closes the fds of a by-value `ProcessHandle` that nothing marks consumed — a second wait closes recycled descriptors

**Status:** Investigating

**Description:** `ProcessHandle { pid, stdout_fd, stderr_fd }` is a Copy struct
(`is_copy = true` in the checker). `emitProcessWait` closes `stdout_fd` and `stderr_fd`
(`src/codegen/emit.zig:28788-28789` and `:29000-29001` at HEAD, the two platform paths)
but the caller's copy of the handle still holds the same numbers, and there is no
"consumed" state (the `-1` selects at `:28828-28843` are the poll loop's own bookkeeping
inside the emitted function, not stored back into the caller's handle). A second
`process_wait` — or a `process_read_stdout` after `process_wait` — on the same handle
therefore `close()`s / `read()`s descriptor numbers that a later `tcp_accept`,
`File.open` or `pipe` may already have reused. The API has no documented single-use
rule, and the native tests never wait twice, so this has never been exercised.

**Steps to reproduce:**
1. Native-build: spawn a child, `process_wait(h)`, then open a file (or `tcp_listen`),
   then `process_wait(h)` again.
2. Run it and check whether the file/listener fd is still valid afterwards.

**Expected:** The second wait returns `Err(IoError)` (EBADF / ECHILD) and touches nothing
else.

**Actual:** To be measured — by reading, the second wait closes whatever now occupies the
old descriptor numbers.

**Found by:** `/qa-review` calibration run over `20a5a3e` (Fixture 4), 2026-09-03 — Opus
shard-A generalist, HIGH `[resources]`; not verified at runtime. Fix if confirmed: make
`ProcessHandle` non-Copy / consume it, or write `-1` into the caller's handle after
closing and guard every close/read on `fd >= 0`.

---

## [ ] Bug 11: `stdlib/string.kl` declares `parse_int` as `Result#[i64, ParseError]` while the checker's builtin is `?i64`

**Status:** Investigating

**Description:** `stdlib/string.kl:33` at HEAD declares
`pub fn parse_int(s: string) -> Result#[i64, ParseError]` with an empty "built-in
implementation" body, while `src/checker/checker.zig:1045` registers the builtin
`parse_int` as returning an optional (`?i64`), and both `stdlib/http_client.kl` and
`stdlib/http_server.kl` use the `?i64` form (`let port_opt: ?i64 = parse_int(port_str)`)
and compile. The question is what a module that does `import stdlib.string.*` sees: if
the stdlib declaration shadows the builtin, the same call type-checks differently
depending on an unrelated import; if the builtin always wins, the declaration is dead
and misleading documentation. Either way one of the two is wrong.

**Steps to reproduce:**
1. Write a module with `import stdlib.string.*` and `let v: ?i64 = parse_int("42")`.
2. `klar check` it; then repeat with `let v: Result#[i64, ParseError] = parse_int("42")`.

**Expected:** Exactly one of the two type-checks, and it is the same one as without the
import.

**Actual:** To be measured.

**Found by:** `/qa-review` calibration run over `20a5a3e` (Fixture 4), 2026-09-03 — Opus
shard-C generalist, PRE-EXISTING `[correctness]`; declarations verified at HEAD, behaviour
not. Fix if confirmed: make the stdlib declaration match the builtin (`?i64`) or remove it.

---

## [ ] Bug 12: `fs_read_to_string` uses `ftell`'s result unchecked — a directory path makes it `malloc(0)` and `fread` `(size_t)-1` bytes

**Status:** Investigating

**Description:** `emitFsReadToString` (`src/codegen/emit.zig:~24597-24606` at HEAD)
does `fseek(f, 0, SEEK_END)`, `ftell(f)`, `fseek(f, 0, SEEK_SET)`, then
`malloc(size + 1)` and `fread(buf, 1, size, f)` with no check on `ftell`'s return or on
the `malloc` result. `fopen` on a directory succeeds on Linux and macOS (reads then fail
with EISDIR), and `ftell` returns −1 there and on pipes / non-seekable files; `size + 1`
is then 0, `malloc(0)` returns NULL or a zero-size block, and `fread` is asked for
`(size_t)-1` bytes. Whether `fread` actually writes anything before failing is what
decides whether this is a crash or a wrong `Ok("")` — and the pre-Phase-6 emitters have
the same unchecked-allocation pattern the new ones copied (Fixture 4 P3/P4/P7/P10).

**Steps to reproduce:**
1. Native-build a program calling `fs_read_to_string("/tmp")` and one reading from a
   FIFO (`mkfifo`).
2. Run both.

**Expected:** `Err(IoError)` in both cases.

**Actual:** To be measured.

**Found by:** `/qa-review` calibration run over `20a5a3e` (Fixture 4), 2026-09-03 — Opus
shard-A generalist, PRE-EXISTING `[correctness]`; call shape verified at HEAD, behaviour
not. Fix if confirmed: branch to `Err` when `ftell < 0` and when `malloc` returns NULL.

---

## [ ] Bug 13: `fcntl` is declared non-variadic — on arm64 macOS the flags argument is read from the wrong place, so `FD_CLOEXEC` and `O_NONBLOCK` are silently not set

**Status:** Open

**Description:** `getOrDeclareFcntl` (`src/codegen/emit.zig:29236-29239` at HEAD;
`:27249-27252` in `20a5a3e`) declares `int fcntl(int, int, ...)` as a fixed three-argument
function — `LLVMFunctionType(i32, &param_types, 3, 0)`, the trailing `0` meaning
"not variadic" — while the file's own convention for variadic libc calls passes `1`
(`emit.zig:1142`, `13973`). On arm64 Apple platforms the anonymous arguments of a variadic
callee are passed on the stack, not in registers, so a call compiled through the
non-variadic prototype puts the third argument in `x2` and libSystem's `fcntl` wrapper
reads its `va_arg` from the stack: `F_SETFD, FD_CLOEXEC` (`emitProcessSpawn`, claim H3)
and `F_SETFL, flags | O_NONBLOCK` (`emitTcpSetNonblocking`) receive whatever happens to be
there. `F_GETFL` (no third argument) is unaffected. x86_64 and Linux arm64 pass the first
few variadic args in registers, so the mismatch is invisible there — which is why the
tests pass on CI and the symptom would show only on Apple Silicon, as a child that
inherits pipe fds it should not, or a socket that stays blocking.

**Steps to reproduce:**
1. On an Apple Silicon Mac, native-build a program that calls `tcp_listen`, then
   `tcp_set_nonblocking(stream, true)` on an accepted stream, then `tcp_read` with no
   data pending.
2. Run it.

**Expected:** `tcp_read` returns immediately with `Err(WouldBlock)` / an `EAGAIN`-shaped
error — the socket is non-blocking.

**Actual:** To be measured — expected to block, because the `F_SETFL` flags word is
garbage (or the call fails with `EINVAL`).

**Found by:** `/qa-review` calibration run over `20a5a3e` (Fixture 4, scripted run 2),
2026-09-03 — Opus shard-A generalist, HIGH `[correctness]`. The declaration and the
convention it breaks are verified by reading at both commits; the ABI consequence is
Apple's documented arm64 rule (variadic arguments always on the stack), not yet executed.
Fix: declare `fcntl` variadic (`…, 3, 1`) like the file's other variadic libc
declarations, and add a native test that sets `O_NONBLOCK` and reads an idle socket.

---

## [ ] Bug 14: `src/codegen/builtins.zig` is dead code — nothing imports it, and the builtin name list now exists in four places

**Status:** Open

**Description:** Every `@import("builtins.zig")` in the tree resolves to
`src/checker/builtins.zig` (`src/checker/checker.zig:22`, `src/checker/mod.zig:32`);
`src/codegen/builtins.zig`, with its `BuiltinName` constants and `isBuiltin`, is imported
by no file (`git grep '@import("[^"]*builtins\.zig")' HEAD -- src`). Native codegen
dispatches on string literals directly (`emit.zig`, the `std.mem.eql(u8, name, "…")`
chain), so a builtin has to be spelled in the checker's list, the interpreter's
registration, the VM's table, the emitter's dispatch chain — and, uselessly, in this file.
The `e7b54e7` (Fixture 3) and `20a5a3e` (Fixture 4) commits both extended it, and both
PLAN.md entries count that as a deliverable. A name that is added to three of the four
live places and not the fourth type-checks and then fails at runtime in one backend
(Bug 7's `parse_int` shape).

**Steps to reproduce:**
1. `git grep -n 'codegen/builtins' -- src build.zig` and
   `git grep -n '@import("builtins.zig")' -- src`.
2. Delete `src/codegen/builtins.zig` and run `./run-build.sh`.

**Expected:** Either the build breaks (the file is load-bearing) or the file is removed
and one list is the source for the others.

**Actual:** The build is unaffected; the file is maintained by hand for nothing.

**Found by:** `/qa-review` calibration run over `e7b54e7` (Fixture 3, scripted),
2026-09-03 — Opus shard-B generalist, HIGH `[duplication]`; verified with `git grep` at
`e7b54e7` and at HEAD. Fix: delete the file, or make `emit.zig`'s dispatch and the
checker's table read one shared list.

---

## [x] Bug 15: GC — `allocObject` returns an unrooted object, so the caller's next `allocBytes` can collect and free it half-built

**Status:** Fixed

**System:** VM GC reachability — when `src/gc.zig` collects (`allocObject`/`allocBytes`) and what it marks (`markRoots`/`markValue`), against every `createGC` caller in `src/vm.zig`, `src/vm_value.zig`, `src/vm_builtins.zig`

**Description:** `GC.allocObject` (`src/gc.zig:183`) links the new object into the sweep
list and returns it; nothing roots it. Every `createGC` then calls `allocBytes` for the
payload (`vm_value.zig` `ObjArray.createGC`, `ObjString.createGC`, `gc.zig:250`), and
`allocBytes` (`gc.zig:215`) may run `collectGarbage` first. The new object is not on the VM
stack yet, so it is white, gets swept, and `freeObject` frees `items`/`chars` while they
are still `undefined`. `next_gc = bytes_allocated * 2` after a collection can be small, so
the window is real; under `stress_gc` it is certain.

**Steps to reproduce:**
1. Run any program that builds an array literal with `--debug`/stress GC enabled (or a
   heap near `next_gc`).
2. Observe the crash in `freeObject` on the half-initialised object.

**Expected:** A freshly allocated object survives the allocation of its own payload.

**Actual:** It can be swept between `allocObject` and `allocBytes`.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — backends territory,
CRITICAL `[resources]`; verified by reading at `e7b54e7` and at HEAD `949fc4c`
(`src/gc.zig:183-215`). Fix: pin the new object as a temporary root until the caller has
initialised it (or allocate the payload first).

**Fix:** Allocation no longer collects. `allocObject` and `allocBytes` (`src/gc.zig`) only set
`collection_requested` (on every allocation under `stress_gc`, else when the heap crosses
`next_gc`), and `GC.collectIfRequested` — called once per instruction at the top of the VM's
`run` loop, where every live value is on a root — is the only place a collection starts. One
path covers this bug, Bug 17 and every other caller that holds a half-built object or a popped
operand in a Zig local across an allocation.

**Test:** `src/vm_gc_test.zig` — "an object allocated under stress survives the allocation of
its own payload (Bug 15)", "a finished but unrooted object survives the next object allocation
under stress", and the stress-mode and threshold program runs.

---

## [x] Bug 16: GC — `markValue` treats `.future` as a primitive, so an async return payload's objects are collected while `await` still points at them

**Status:** Fixed

**System:** VM GC reachability — when `src/gc.zig` collects (`allocObject`/`allocBytes`) and what it marks (`markRoots`/`markValue`), against every `createGC` caller in `src/vm.zig`, `src/vm_value.zig`, `src/vm_builtins.zig`

**Description:** `op_return` from an async function stores the result in a heap `*Value`
(`src/vm.zig:593` at HEAD, `:587` at `e7b54e7`) and wraps it in a Future. `GC.markValue`
(`src/gc.zig:374`) lists `.future` with `.int, .float, .bool_, .char_, .void_` and marks
nothing, so a payload holding an array/string/struct is unreachable to the collector.

**Steps to reproduce:**
1. `async fn f() -> [i32] { return [1, 2, 3] }`; hold the Future across further
   allocations that trigger a collection; then `await` it.

**Expected:** The array survives until the Future is consumed.

**Actual:** It is swept; `await` reads freed memory.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — backends territory,
CRITICAL `[correctness]`; verified by reading at `e7b54e7` and at HEAD `949fc4c`
(`src/gc.zig:374`, `src/vm.zig:593`). Fix: mark `future.value.*` in `markValue`.

**Fix:** `GC.markValue` (`src/gc.zig`) marks through a Future's payload box
(`.future => |f| if (f.value) |v| self.markValue(v.*)`).

**Test:** `src/vm_gc_test.zig` — "a completed Future's payload is marked, so its array survives
a collection (Bug 16)".

---

## [x] Bug 17: VM — `trim`/`slice`/`substring` pop the receiver, then allocate from a slice borrowed out of it

**Status:** Fixed

**System:** VM GC reachability — when `src/gc.zig` collects (`allocObject`/`allocBytes`) and what it marks (`markRoots`/`markValue`), against every `createGC` caller in `src/vm.zig`, `src/vm_value.zig`, `src/vm_builtins.zig`

**Description:** In `invokeStringMethod` (`src/vm.zig:1485` trim, `:1537` slice, `:1575`
substring at HEAD; `:1477/:1531/:1569` at `e7b54e7`) the receiver is popped, then a
sub-slice of `str.chars` is handed to `ObjString.createGC`, which can run a collection. A
temporary receiver (`("  " + name).trim()`) is unrooted by then, so its chars are freed and
then copied from.

**Steps to reproduce:**
1. Build a temporary string and call `.trim()` on it with the heap near `next_gc`.

**Expected:** The trimmed copy is taken before the receiver can be freed.

**Actual:** Use-after-free of the receiver's bytes.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — backends territory,
CRITICAL `[resources]`; verified by reading at both commits. Fix: create the new string
before popping the receiver, or pin it.

**Fix:** Bug 15's fix — allocation never collects, so the popped receiver's bytes stay valid
until the method's instruction ends; the collection its allocation requests runs at the next
instruction boundary, after the result is on the stack. `invokeStringMethod` is unchanged.

**Test:** `src/vm_gc_test.zig` — "a string method's popped receiver survives a collection
triggered by the method's own allocation (Bug 17)" (segfaulted in `internString` copying the
freed receiver on the unfixed tree).

---

## [ ] Bug 18: VM — integers carry no declared width, so narrow-type overflow is undetected and `.trunc#[T]` is a no-op

**Status:** Open

**Description:** VM integer arithmetic runs on untagged `i128` (`src/vm.zig:1292` at HEAD,
`:1284` at `e7b54e7`): `std.math.add(i128, …)` only traps at i128 bounds and `+%`/`-%`/`*%`/`+|`
wrap or saturate at i128 bounds. `castValue` (`:1381` at HEAD, `:1373` at `e7b54e7`)
discards both `truncating` and the target width with the comment "handled at native
level". The interpreter (`interpreter.zig:727`) and native codegen (`llvm.sadd.with.overflow`)
trap or wrap at the declared width.

**Steps to reproduce:**
1. `let a: u8 = 200; let b: u8 = 100; println(debug(a +% b))` on the default backend.
2. `let n: i32 = 300; println(debug(n.trunc#[u8]))`.

**Expected:** `44` and `44`, as the interpreter and native produce.

**Actual:** `300` and `300` on the VM.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — backends territory and
backend-parity contract (both), CRITICAL `[correctness]`; verified by reading at both
commits. Fix: carry the operand width in the value or the opcode and range-check per
declared type.

---

## [ ] Bug 19: Interpreter — `%` is Euclidean (`@mod`) while the VM (`@rem`) and native (`srem`) truncate

**Status:** Open

**Description:** `intArithmetic` uses `@mod` for `.mod` (`src/interpreter.zig:749` at HEAD,
`:674` at `e7b54e7`) and the float path the same (`:769` / `:694`); `vm.zig:1301` uses
`@rem` and `emit.zig` emits `srem`/`frem`.

**Steps to reproduce:**
1. `println(debug(-7 % 3))` under `--interpret`, then on the VM, then native.

**Expected:** The same answer on all three backends.

**Actual:** `2` under `--interpret`, `-1` on the other two.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — backend-parity contract,
CRITICAL `[correctness]`; verified by reading at both commits. Fix: `@rem` in the
interpreter for int and float.

---

## [ ] Bug 20: Interpreter — `len` and `.len()` return a `usize`-tagged value; the checker says `i32`

**Status:** Open

**Description:** `builtinLen` (`src/interpreter.zig:2818` at HEAD, `:2743` at `e7b54e7`) and
the `.len()` method arms (`:1177` at HEAD, `:1102-1117` at `e7b54e7`) build the result with
`.type_ = .usize_`. The checker declares `len -> i32` and `intArithmetic` ranges on the
left operand's tag, so subtraction below zero traps.

**Steps to reproduce:**
1. `let s: string = "ab"; println(debug(len(s) - 20))` under `--interpret`.

**Expected:** `-18`, as the VM and native produce.

**Actual:** `IntegerOverflow`.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — backend-parity contract
and backends territory, HIGH `[correctness]`; verified by reading at both commits. Fix: tag
the result `.i32_`.

---

## [ ] Bug 21: Interpreter — `checkedAdd/Sub/Mul` compute in raw `i128` before their own range check, so 128-bit arithmetic panics

**Status:** Open

**Description:** `checkedAdd` (`src/interpreter.zig:786` at HEAD, `:711` at `e7b54e7`),
`checkedSub` (`:800`) and `checkedMul` (`:816`) do `const result = a + b` on `i128`
operands first and compare against `minValue()/maxValue()` afterwards. For `i128`/`u128`
values the Zig addition itself overflows before the check runs, so `+%` and `+|` — which
are supposed to wrap and saturate — panic instead.

**Steps to reproduce:**
1. `let a: i128 = 170141183460469231731687303715884105727; let b: i128 = a +% 1` under
   `--interpret`.

**Expected:** Wraps to the minimum `i128`.

**Actual:** "integer overflow" panic.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — backends territory,
CRITICAL `[correctness]`; verified by reading at both commits. Fix: `@addWithOverflow` /
`@mulWithOverflow`, or compute in a wider type.

---

## [ ] Bug 22: Checker — `checkTypeCast` accepts any primitive-to-primitive `.as#[T]`, including `string` ↔ numeric and narrowing casts

**Status:** Open

**Description:** `checkTypeCast` (`src/checker/expressions.zig:861-880` at HEAD, `:815-835`
at `e7b54e7`) allows every numeric→numeric pair and then every primitive→primitive pair.
Two consequences: (a) `"hi".as#[i32]` type-checks; the interpreter's cast has no such case
and its fallback returns the value unchanged (`interpreter.zig:1810` at `e7b54e7`), so an
`i32` variable holds a string; (b) `.as#` is documented as the safe, always-succeeding
conversion, but `300.as#[i8]` compiles and the interpreter returns `InvalidCast` at
runtime (`:1855`).

**Steps to reproduce:**
1. `let s: string = "hi"; let n: i32 = s.as#[i32]; println(debug(n + 1))`.
2. `let big: i64 = 300; let small: i8 = big.as#[i8]`.

**Expected:** Both rejected at check time (or `.as` restricted to widening).

**Actual:** (1) `TypeError` at runtime under `--interpret`; (2) `InvalidCast` at runtime.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — checker territory,
CRITICAL + HIGH `[correctness]`; verified by reading at both commits. Fix: allow only the
pairs the backends implement, and route narrowing to `.to#`/`.trunc#`.

---

## [ ] Bug 23: Checker — match arms bind their patterns into the enclosing scope

**Status:** Open

**Description:** `checkMatchStmt` (`src/checker/statements.zig:53` at HEAD, `:53-62` at
`e7b54e7`) calls `checkPattern` per arm without pushing a scope; `checkFor` (`:244`) pushes
`.loop` and the interpreter pushes an env per arm (`interpreter.zig:1559`). A binding named
like an outer variable retypes it for the rest of the block, and a binding made in an arm
stays visible after the match, where the runtime never defined it.

**Steps to reproduce:**
1. `let msg: string = "x"` then `match r { Ok(msg) => {}, Err(e) => {} }` then use `msg`
   as a string.

**Expected:** `msg` is still the outer `string` after the match.

**Actual:** The checker now believes `msg` is the `Ok` payload type.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — checker territory,
CRITICAL `[correctness]`; verified by reading at both commits. Fix: push a scope per arm
around pattern + guard + body.

---

## [ ] Bug 24: Checker — `impl` blocks are registered in pass 3 beside function bodies, so a body declared above its `impl` cannot see the methods

**Status:** Open

**Description:** `checkModule` registers types in pass 1 and function signatures in pass 2,
then dispatches `.impl_decl => self.checkDecl(decl)` in pass 3 interleaved with function
bodies (`src/checker/checker.zig:5791` at HEAD, `:4993` at `e7b54e7`; the same in
`checkModuleDeclarations`, `:5521` / `:4773`). `struct_methods` is written only by
`checkImpl` (`declarations.zig:773`), so a method call in a body checked earlier in
declaration order fails with "method not found". Every `.kl` in `test/` and `examples/`
declares impls before use, which is why the suite does not see it.

**Steps to reproduce:**
1. Put `fn main` above `impl Point { fn show(self) -> void { … } }` and call `p.show()` in
   `main`.

**Expected:** Declaration order does not matter for methods, as it does not for functions.

**Actual:** "method 'show' not found".

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — checker territory,
CRITICAL `[architecture]`; verified by reading at both commits. Fix: register impl method
signatures in pass 2.

---

## [ ] Bug 25: Checker — `appendTypeName` mangles lists, maps, sets, results, functions and more as `"unknown"`, so distinct instantiations share one mangled name

**Status:** Open

**Description:** `appendTypeName` (`src/checker/checker.zig:2838` at HEAD, `:2281` at
`e7b54e7`) has cases for primitives, optional, array, slice, rc, tuple and
`else => "unknown"`. `recordMonomorphization` dedups by mangled name and returns the first
entry, so `tap#[T]` called with a `List#[i32]` and then with a `String` both become
`tap$unknown` and the second call site resolves to the first specialisation.

**Steps to reproduce:**
1. `fn tap#[T](x: T) -> T { return x }`; call it with a list, then with a string; use the
   second result.

**Expected:** Two specialisations.

**Actual:** One; the second call is emitted against the first's signature.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — checker territory,
CRITICAL `[correctness]`; verified by reading at both commits. Fix: mangle every `Type`
tag distinctly.

---

## [ ] Bug 26: Checker — compound assignment checks only "both numeric", never that the operand types match

**Status:** Open

**Description:** `.add_assign` and friends in `checkBinary` (`src/checker/expressions.zig:406`
at HEAD, `:355` at `e7b54e7`) and the statement form (`statements.zig:95` / `:88`) test
`isNumeric()` on each side and nothing else, while plain `.assign` two lines above calls
`checkAssignmentCompatible`. `var x: f64 = 0.0; x += 1` gets an `i32` literal with no
hint; the interpreter returns `TypeError`, native emits `add` on mismatched types.

**Steps to reproduce:**
1. `var x: f64 = 0.0; x += 1`.

**Expected:** Rejected at check time (no implicit conversions).

**Actual:** Accepted; fails at runtime or in LLVM.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — checker territory, HIGH
`[correctness]`; verified by reading at both commits. Fix: require `left.eql(right)` as
`.assign` does.

---

## [ ] Bug 27: Checker — `verifyMethodSignature` checks `Self`/`&Self` parameters only at index 0

**Status:** Open

**Description:** In `src/checker/methods.zig` (`:180` and `:198` at HEAD, `:198` at
`e7b54e7`) every Self and reference-to-Self check sits inside `if (idx == 0)`, followed by
`continue`, so a Self-typed parameter after the first is never compared. `Eq.eq` is
`(&Self, &Self) -> bool`.

**Steps to reproduce:**
1. `impl Point: Eq { fn eq(self, other: &string) -> bool { … } }` then `a == b` on two
   Points.

**Expected:** Signature mismatch error.

**Actual:** Verifies clean; `==` dispatches a Point where a string is expected.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — checker territory, HIGH
`[correctness]`; verified by reading at both commits. Fix: run the checks for every index.

---

## [ ] Bug 28: Checker — bitwise operators never compare their operand types

**Status:** Open

**Description:** The `.bit_and, .bit_or, .bit_xor, .shl, .shr` arm
(`src/checker/expressions.zig:476` at HEAD, `:430` at `e7b54e7`) requires each side to be
an integer and returns `left_type`; the arithmetic arm above enforces `eql` under "Types
must match exactly (no implicit conversions)".

**Steps to reproduce:**
1. `let x: i32 = 1; let y: i64 = 2; let z: i32 = x & y`.

**Expected:** Type mismatch.

**Actual:** Accepted as `i32`; LLVM receives an `and` of `i32` and `i64`.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — checker territory, HIGH
`[correctness]`; verified by reading at both commits. Fix: apply the same `eql` check
(shift amounts excepted, deliberately).

---

## [ ] Bug 29: Checker — trait completeness is checked against the union of every `impl` block for the type

**Status:** Open

**Description:** `checkImpl` reads `tc.struct_methods.get(struct_name)`
(`src/checker/declarations.zig:887` at HEAD, `:830` at `e7b54e7`) — the accumulated
methods of every impl block seen so far — not the methods in the block being verified. An
earlier inherent `eq` satisfies a later empty `impl Point: Eq {}`; swap the blocks and the
same program errors.

**Steps to reproduce:**
1. `impl Point { fn eq(self, o: Point) -> bool { return true } }` then `impl Point: Eq {}`.

**Expected:** "missing implementation for required trait method".

**Actual:** Accepted.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — checker territory, HIGH
`[correctness]`; verified by reading at both commits. Fix: verify against the methods
registered by this `impl_decl`.

---

## [ ] Bug 30: Checker — `.default()` is intercepted for any identifier receiver, and a *variable* of struct type passes as the type

**Status:** Open

**Description:** `checkStaticConstructor` intercepts every `.default()` call
(`src/checker/method_calls.zig:69` at HEAD, `:68` at `e7b54e7`) and `checkDefaultConstructor`
(`:157` / `:176`) accepts any symbol whose `type_` is a struct without testing
`sym.kind == .type_`, unlike `resolveTypeExpr`. A user method named `default` on a
variable is unreachable and the call is typed as the struct.

**Steps to reproduce:**
1. `impl Config { fn default(self) -> string { return self.name } }`; `let c: Config = …;
   let s: string = c.default()`.

**Expected:** `string`, via the user method.

**Actual:** Typed as `Config`; the user method is never called.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — checker territory, HIGH
`[correctness]`; verified by reading at both commits. Fix: require `sym.kind == .type_`.

---

## [ ] Bug 31: Checker — `@sizeOf` sums struct fields with no padding and reports 8 for `i128`; codegen emits the number verbatim

**Status:** Open

**Description:** `computeTypeSize` (`src/checker/builtins_check.zig:675` at HEAD, `:670-695`
at `e7b54e7`) adds raw field sizes, returns 8 for `i128`/`u128`/`isize`/`usize`
(`else => 8`) and 4 for any enum; `computeTypeAlignment` returns the max field alignment.
Codegen emits the checker's constant, and `i128` lowers to LLVM `i128`.

**Steps to reproduce:**
1. `struct S { a: i8, b: i64 }`; `malloc(@sizeOf(S))` via FFI; write a `S` into it.

**Expected:** 16.

**Actual:** 9 — the write overruns.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — checker territory, HIGH
`[correctness]`; verified by reading at both commits. Fix: align each field offset and
round the total to the alignment; size `i128` as 16.

---

## [ ] Bug 32: Checker — literal patterns are always typed `i32`/`f64`, ignoring the expected type

**Status:** Open

**Description:** `checkLiteralPattern` (`src/checker/patterns.zig:222` at HEAD, `:220` at
`e7b54e7`) returns `i32Type()` for every int literal and `f64Type()` for every float,
ignoring the `expected_type` its caller has, while `checkLiteralWithHint` adopts the hint
for expressions.

**Steps to reproduce:**
1. `fn f(x: u8) -> i32 { match x { 0 => { return 1 } _ => { return 2 } } }`.

**Expected:** Compiles.

**Actual:** "pattern type mismatch" on `0`.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — checker territory, HIGH
`[correctness]`; verified by reading at both commits. Fix: pass `expected_type` as the hint.

---

## [ ] Bug 33: Checker — no match-exhaustiveness check exists; the `exhaustiveness` error kind is declared and never raised

**Status:** Open

**Description:** `checker.zig:89` (HEAD; `:88` at `e7b54e7`) declares an `exhaustiveness`
error kind; no site raises it, and `checkMatchStmt` walks the arms without comparing them
to the enum's variants. A three-variant enum matched with two arms and no `_` compiles
and fails at runtime with `PatternMatchFailed`. If this is a planned feature rather than
a regression, say so here and keep the entry as the tracking item.

**Steps to reproduce:**
1. `enum C { R, G, B }`; `match c { R => {…} G => {…} }`; run with `c = B`.

**Expected:** Check-time "non-exhaustive match".

**Actual:** Runtime `PatternMatchFailed`.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — checker territory, HIGH
`[correctness]`; verified by reading at both commits. Fix: after the arm loop, diff the
bound variants against `enum_def.variants` unless a wildcard is present.

---

## [ ] Bug 34: Codegen — `getTypeSize` sizes `char` as 1 byte and omits alignment padding, so enum payload stores overlap

**Status:** Open

**Description:** `getTypeSize` (`src/codegen/emit.zig:38329` at HEAD, `:31565` at `e7b54e7`)
lists `.char_` with the 1-byte types (`:38333`) while `typeToLLVM` lowers `char` to `i32`
(`:37811`), and its struct/tuple branch sums raw sizes with no padding (`:38342`). The
enum-literal emitter uses the running sum as the store offset inside the payload byte
array (`:11025`, `:11045` at HEAD; `:9871`, `:9891` at `e7b54e7`) and the monomorphized-enum
registration uses it for the slot size. `layout.zig:93` and `types_emit.zig` both say
`char` is 4 bytes.

**Steps to reproduce:**
1. `enum T { C(char, i32) }`; `let t: T = T.C('x', 42)`; match and print both fields.
2. `enum E { V(i8, i64) }` — the `i64` lands at offset 1 in a 9-byte slot.

**Expected:** `'x'` and `42`.

**Actual:** The 4-byte `'x'` store is overwritten from offset 1 by the 4-byte `42` store;
both read back garbage.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — codegen territory,
CRITICAL + HIGH `[correctness]`; verified by reading at both commits. Fix: return 4 for
`char_`, align each offset and the total as `layout.zig:177` does.

---

## [ ] Bug 35: Checker registers fifteen builtins that neither the VM nor the interpreter implements — `fs_exists` type-checks and dies with `UndefinedVariable` on the default backend

**Status:** Open

**Description:** `initBuiltins` registers `dbg` (`src/checker/checker.zig:933` at HEAD),
`type_name` (`:954`), `stdout`/`stderr`/`stdin`, and all ten `fs_*` functions (`:1837`
onward); the VM native table (`vm_builtins.zig:40`) and the interpreter's `initBuiltins`
register none of them, and `compiler.zig` has no special case — a call compiles to
`op_get_global` and fails with `UndefinedVariable` (`vm.zig:533`). The five newest
builtins are at least stubbed with "use native build"; these fifteen are not. Conversely
both runtimes register `type_of`, which the checker rejects. The CLAUDE.md rule is "all
three backends".

**Steps to reproduce:**
1. `if fs_exists("/tmp") { println("yes") }`; `klar run file.kl` (VM) and `--interpret`.

**Expected:** Works, or a check-time "not available on this backend".

**Actual:** Type-checks, then `UndefinedVariable` at runtime.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — backend-parity contract
and backends territory, CRITICAL `[architecture]`; verified by reading at both commits.
Fix: implement, stub, or reject per backend from one shared table (see Bug 14).

---

## [ ] Bug 36: `@fn_ptr(builtin)` type-checks and yields a null function pointer

**Status:** Open

**Description:** `checkBuiltinFnPtr` accepts any identifier whose type is `.function`
(`src/checker/builtins_check.zig:523-528` at HEAD, `:517-524` at `e7b54e7`), which includes
every builtin the checker registers. `emitBuiltinFnPtr` looks the name up as an LLVM
function and, finding none, returns `LLVMConstNull` (`src/codegen/emit.zig:14456` at HEAD,
`:13134` at `e7b54e7`).

**Steps to reproduce:**
1. `let f: extern fn() -> i64 = @fn_ptr(timestamp_now)`; pass `f` to C and call it.

**Expected:** Rejected at check time.

**Actual:** Compiles; the C side calls NULL.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — checker-codegen contract,
HIGH `[error-handling]`; verified by reading at both commits. Fix: reject builtin names in
`checkBuiltinFnPtr`; make codegen error instead of returning null.

---

## [ ] Bug 37: Codegen — `typeToLLVM` lowers `usize`/`isize` and slice lengths to `i64` unconditionally while its siblings use pointer width (wasm32)

**Status:** Open

**Description:** `typeToLLVM` maps `.isize_, .usize_` to `int64` (`src/codegen/emit.zig:37806`
at HEAD, `:31032` at `e7b54e7`) and the slice struct to `{ptr, i64}`, while
`namedTypeToLLVM` and `inferExprType` use `usizeType()`/`isizeType()` (pointer width) and
one slice site uses `{ptr, usizeType()}`. On wasm32 a `[u8]` parameter is 12 bytes in one
place and 16 in another. `docs/guides/wasm.md` says usize width is "handled automatically".

**Steps to reproduce:**
1. `klar build --target wasm` a function taking `[u8]` and indexing it.

**Expected:** One slice layout.

**Actual:** The length is loaded as `i64` from a 4-byte field.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — checker-codegen contract,
HIGH `[correctness]`; verified by reading at both commits. Fix: route every site through
`usizeType()`.

---

## [ ] Bug 38: Codegen — `emitWasmUnsupportedTrap` emits `unreachable` and returns with the builder still in that block, so the caller appends after a terminator

**Status:** Open

**Description:** `emitWasmUnsupportedTrap` (`src/codegen/emit.zig:887-899` at HEAD,
`:698-711` at `e7b54e7`) calls `puts`, `buildUnreachable()`, and returns a dummy `i32 0`
without starting a new basic block. The caller keeps emitting into the terminated block
(e.g. a store of the dummy value), and module verification fails — the program does not
compile instead of trapping at runtime as `docs/guides/wasm.md` says.

**Steps to reproduce:**
1. `let t: i64 = timestamp_now()` built with `--target wasm`.

**Expected:** Compiles; traps at runtime.

**Actual:** LLVM verifier error at build time.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — checker-codegen contract,
HIGH `[correctness]`; verified by reading at both commits. Fix: append and position a fresh
block after the `unreachable`.

---

## [ ] Bug 39: Codegen — `fs_read_string` truncates the `ftell` size to `i32` for `String.len` with no range check

**Status:** Open

**Description:** `emitFsReadString` (`src/codegen/emit.zig:26301` at HEAD, `:24198` at
`e7b54e7`) stores `LLVMBuildTrunc(file_size, i32)` as the length and `+1` as the capacity.
A file over 2 GiB yields a negative `len()`, and every later bounds check on the String is
wrong. (Related but distinct from Bug 12, which is `stdlib` `fs_read_to_string`.)

**Steps to reproduce:**
1. `fs_read_string` on a file larger than `maxInt(i32)` bytes.

**Expected:** `Err(IoError)` above the representable size.

**Actual:** Negative length, `Ok`.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — checker-codegen contract,
HIGH `[correctness]`; verified by reading at both commits. Fix: range-check and fail.

---

## [ ] Bug 40: Native `debug` of primitives — 64-byte stack buffer truncates strings, the buffer pointer is returned dangling, and unsigned integers are sign-extended

**Status:** Open

**Description:** `emitDebugPrimitive` (`src/codegen/emit.zig:15398` at HEAD, `:13962` at
`e7b54e7`) formats into a 64-byte `alloca` (`:15402`): (a) a string over ~61 characters
comes back truncated and missing its closing quote; (b) every branch returns `buf_ptr`,
a pointer into the current frame — `fn label(n: i32) -> string { return debug(n) }`
returns a dangling pointer, while the sibling at `emitContextErrorDisplayChain` `strdup`s
its buffer before returning; (c) the integer branch `SExt`s every width and prints `%lld`
(`:15457-15464`), so `debug(3000000000u32)` prints `-1294967296`. The VM and interpreter
give the whole string, an owned copy, and the unsigned value.

**Steps to reproduce:**
1. `debug(s)` with `s` of 100 characters; 2. return `debug(n)` from a function and print it
   in the caller; 3. `let x: u32 = 3000000000; println(debug(x))` — all native.

**Expected:** Whole string; a valid owned string; `3000000000`.

**Actual:** Truncated; garbage/crash; `-1294967296`.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — backend-parity contract,
3 × HIGH `[correctness]`/`[resources]`; verified by reading at both commits. Fix: size the
buffer from the value, `strdup` the result, branch on unsigned prims with `%llu` + `ZExt`.

---

## [ ] Bug 41: `parse_int`/`parse_float` accept different strings on each backend — native `strtoll`/`strtod` skip leading whitespace, the VM and interpreter reject it

**Status:** Open

**Description:** `emitParseInt` uses `strtoll` (`src/codegen/emit.zig:17416` /
`getOrDeclareStrtol()` at `:17432` at HEAD; `:15835` at `e7b54e7`) and `emitParseFloat`
`strtod` (`:17515`); `vm_builtins.zig` `nativeParseInt` and `interpreter.zig`
`builtinParseInt` use `std.fmt.parseInt`, which rejects leading whitespace (and accepts
`_` separators the C functions do not).

**Steps to reproduce:**
1. `parse_int(" 12")` on each backend.

**Expected:** The same result.

**Actual:** `Some(12)` natively, `None` on the VM and interpreter.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — backend-parity contract,
HIGH `[correctness]`; verified by reading at both commits. Fix: one grammar, shared.

---

## [ ] Bug 42: VM `debug` appends `.0` to whole floats; the interpreter and native do not

**Status:** Open

**Description:** `debugValueToString` in the VM (`src/vm_builtins.zig:427` at HEAD, `:412`
at `e7b54e7`) formats floats with `{d}` and then appends `.0` when no decimal point is
present ("ensuring we show decimal point for whole numbers"); `values.zig` `formatValue`
(interpreter) prints `{d}` bare and native uses `%g`.

**Steps to reproduce:**
1. `println(debug(1.0))` on each backend.

**Expected:** The same text.

**Actual:** `1.0` on the VM, `1` on the other two.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — backend-parity contract,
HIGH `[correctness]`; verified by reading at both commits. Fix: pick one form for all three.

---

## [ ] Bug 43: VM — `readline`, `type_of` and `debug` build their result with the non-GC `ObjString.create`, so the strings leak

**Status:** Open

**Description:** `src/vm_builtins.zig:163` (`readline`), `:342` (`type_of`) and `:398`
(`debug`) at HEAD (`:149`, `:328`, `:384` at `e7b54e7`) call `ObjString.create(allocator, …)`
— the raw-allocator path — while `from_byte`/`parse_int`/`parse_float` use `createGC`. The
GC never sees them and `VM.deinit` frees only GC objects and globals.

**Steps to reproduce:**
1. A loop calling `readline()` many times under a leak-checking allocator.

**Expected:** Strings collected.

**Actual:** One leaked string per iteration.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — backend-parity contract,
HIGH `[resources]`; verified by reading at both commits. Fix: `createGC`.

---

## [ ] Bug 44: `process_run` space-joins `cmd` and `args` into one `popen` shell string — argument boundaries are lost and metacharacters execute

**Status:** Open

**Description:** `emitProcessRun` (`src/codegen/emit.zig:27215`, `popen` at `:27365` at HEAD;
`:24928`/`:25065` at `e7b54e7`) builds `"<cmd> <arg1> <arg2>…"` and hands it to `popen`,
i.e. `/bin/sh -c`. The checker declares `args: List#[string]` as separate arguments.
Raised by every `/qa-review` run over this commit since 2026-09-01; never filed.

**Steps to reproduce:**
1. `process_run("touch", ["a b.txt"])`; 2. `process_run("echo", ["; rm -rf x"])`.

**Expected:** One file `a b.txt`; the literal text echoed.

**Actual:** Two files; a second command runs.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — backend-parity contract,
HIGH `[security]`; verified by reading at both commits. Fix: `posix_spawn`/`execvp` with a
real argv (the Phase 6 `process_spawn` path may already be the model).

---

## [ ] Bug 45: `env_get` returns a `strdup`'d buffer typed as the Copy primitive `string`, so nothing frees it

**Status:** Investigating

**Description:** `emitEnvGet` (`src/codegen/emit.zig:26933`, `strdup` at `:26961` at HEAD;
`:24688` at `e7b54e7`) copies the environment value with `strdup` and returns it as
`?string`; `string` is a Copy primitive (`types.zig` `isCopyType`), so no drop is ever
emitted. Same shape as the `ProcessOutput.stdout` leak fixed in `144d17f`. **To settle:**
whether the checker's `env_get` return type at HEAD is still the primitive `string` or
became an owned `String` — if owned, close this.

**Steps to reproduce:**
1. `env_get("PATH")` in a loop under a leak checker, native build.

**Expected:** Freed by its owner.

**Actual:** Leaks one copy per call (if the return type is still `string`).

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — checker-codegen contract,
HIGH `[resources]`; the `strdup` verified by reading at both commits, the ownership half
only at `e7b54e7`.

---

## [ ] Bug 46: `operandBytes(.op_closure)` ignores the upvalue descriptors, so `klar disasm` desyncs after any capturing closure

**Status:** Open

**Description:** `OpCode.operandBytes` lists `.op_closure` among the 2-byte operands
(`src/bytecode.zig:491` at both commits) but the compiler emits `2 + 2 × upvalue_count`
bytes (`src/compiler.zig:348` "Emit upvalue descriptors"). The VM reads the descriptors
itself and is fine; `disasm.zig:150` walks by `offset + 1 + operandBytes()`, so every
instruction after a capturing closure is decoded from the wrong byte and an operand byte
above the opcode count makes `@enumFromInt` panic.

**Steps to reproduce:**
1. `klar disasm` on a file with a closure that captures a local.

**Expected:** Correct listing.

**Actual:** Misdecoded instructions or a panic.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — backends territory, HIGH
`[correctness]`; verified by reading at both commits. Fix: make `operandBytes` (or the
disassembler) account for the descriptors.

---

## [ ] Bug 47: `substring(start, end)` returns `""` whenever `end` exceeds the codepoint count — in both the VM and the interpreter

**Status:** Open

**Description:** The codepoint walk breaks when `cp_idx == char_end`; if `end` is past the
string, `byte_end` stays 0 and the guard `byte_start >= byte_end` returns the empty string
(`src/vm.zig:1601` at HEAD, `:1597` at `e7b54e7`; `src/interpreter.zig:1347` / `:1274`, the
same code). The comment says "Clamp" and the `slice` sibling clamps.

**Steps to reproduce:**
1. `"hello".substring(1, 6)`.

**Expected:** `"ello"` (clamped), as `slice` gives.

**Actual:** `""`.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — backends territory, HIGH
`[correctness]`; verified by reading at both commits. Fix: clamp `char_end` to the count.

---

## [ ] Bug 48: REPL — the AST arena is reset every line, but the interpreter keeps function bodies as pointers into it

**Status:** Open

**Description:** `Repl.parseInput` calls `self.arena.reset(.retain_capacity)` before
parsing each line (`src/repl.zig:228` at HEAD, `:224` at `e7b54e7`); `registerFunction`
stores `.body = body` (`src/interpreter.zig:2292` at HEAD, `:2259` at `e7b54e7`), a pointer
into that arena. The REPL's own `:help` example — define a function, call it on the next
line — reads a freed AST.

**Steps to reproduce:**
1. `klar repl`; `fn double(n: i32) -> i32 { return n * 2 }`; `double(4)`.

**Expected:** `8`.

**Actual:** Use-after-free of the function body (garbage or crash).

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — driver territory,
CRITICAL `[correctness]`; verified by reading at both commits. Fix: allocate declaration
ASTs from a non-reset arena, as source strings already are.

---

## [ ] Bug 49: `klar build` and `klar check` exit 0 when compilation fails

**Status:** Investigating

**Description:** At `e7b54e7`, `buildNative` and `checkFile` print type/codegen errors and
`return` normally (`src/main.zig:2620`), so the process exits 0, while `fmt` and `test`
call `process.exit(1)`; `scripts/run-module-tests.sh:42` gates on the build's exit code,
which can never fail. `docs/getting-started/cli-reference.md` promises non-zero. **At HEAD
the exit mechanism changed** (`main` is `pub fn main(minimal: …) !void` at `:476`, no
`process.exit` calls remain) — the error paths at `:3420` still `return`; whether `main`
now converts that to a non-zero status is the thing to check before fixing.

**Steps to reproduce:**
1. `klar build broken.kl; echo $?` and `klar check broken.kl; echo $?`.

**Expected:** Non-zero.

**Actual:** `0` at `e7b54e7`; to be measured at HEAD.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — driver territory, HIGH
`[error-handling]`; verified by reading at `e7b54e7`, unverified at HEAD.

---

## [ ] Bug 50: `klar run --interpret` and `--vm` discard `main`'s return value, so the exit code is always 0

**Status:** Open

**Description:** `_ = interp.callFunction(…)` (`src/main.zig:1087`, `:1099` at HEAD;
`:848`/`:860` at `e7b54e7`) and `_ = vm.callMain(…)` (`:6149` / `:5478`) throw the `i32`
away; the native path propagates the child's status. `cli-reference.md` says `klar run`
returns the program's exit code.

**Steps to reproduce:**
1. `fn main() -> i32 { return 1 }`; `klar run --interpret f.kl; echo $?`.

**Expected:** `1`.

**Actual:** `0`.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — driver territory, HIGH
`[correctness]`; verified by reading at both commits. Fix: use the returned value as the
process exit status on both paths.

---

## [ ] Bug 51: A parse error in an *imported* module is swallowed by `catch continue`, and the build reports success

**Status:** Open

**Description:** `parseModuleSource(…) catch continue` skips the module in the discovery
loop of `run --interpret`, `build`, `check` and `--vm` (`src/main.zig:1198`, `:3086`,
`:4460`, `:6009` at HEAD; `:959`, `:2554`, `:3774`, `:5331` at `e7b54e7`); `module_ast`
stays null, check and emit skip it, and if the entry uses none of its symbols the build
prints "Parse error in …" and then succeeds. `runTestFile` alone treats it as fatal.

**Steps to reproduce:**
1. Two files; the imported one has a syntax error; the entry imports it but uses nothing.

**Expected:** Build fails.

**Actual:** "Parse error in …" followed by a successful build.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — driver territory, HIGH
`[error-handling]`; verified by reading at both commits. Fix: treat it as fatal, as the test
runner does.

---

## [ ] Bug 52: Ownership analysis runs only inside `klar check`, and only on the entry module

**Status:** Open

**Description:** The only `OwnershipChecker` construction is in `checkFile`
(`src/main.zig:4645` at HEAD, `:3959` at `e7b54e7`) and it runs on the entry AST alone.
`build` and `run` never construct one, so the move rules gate nothing on the paths that
produce binaries, and a use-after-move in an imported module passes `check`.

**Steps to reproduce:**
1. A use-after-move in an imported module; `klar check entry.kl`; `klar build entry.kl`.

**Expected:** Both refuse.

**Actual:** "All checks passed"; the binary builds.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — driver territory, HIGH
`[architecture]`; verified by reading at both commits. Fix: run the ownership pass over
every module in every pipeline.

---

## [ ] Bug 53: `klar test --include-source` never emits the `source` field — `findFunctionSource` skips `test_decl`

**Status:** Open

**Description:** `findFunctionSource` (`src/main.zig:436` at HEAD, `:308` at `e7b54e7`)
iterates declarations and `continue`s on anything that is not `.function`; both callers
(`:5556`, `:5808` at HEAD) pass a `test_decl` name.

**Steps to reproduce:**
1. `klar test f.kl --json --include-source` on a file with `test "x" { … }`.

**Expected:** `source` populated.

**Actual:** Never emitted.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — driver territory, HIGH
`[correctness]`; verified by reading at both commits. Fix: match `test_decl` by name and
slice its span.

---

## [ ] Bug 54: `klar update` deletes `klar.lock` first and writes a new one only if something resolved

**Status:** Open

**Description:** `updateLockfile` deletes the lock file before resolving
(`src/main.zig:6464` at HEAD, `:5799` at `e7b54e7`) and saves only `if (updated_count > 0)`
(`:6521` / `:5855`). A temporarily missing dependency directory leaves the project with no
lock file and a warning.

**Steps to reproduce:**
1. Move a path dependency aside; `klar update`; move it back.

**Expected:** The old lock file survives a failed update.

**Actual:** It is gone.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — driver territory, HIGH
`[error-handling]`; verified by reading at both commits. Fix: build the new file first and
replace atomically.

---

## [ ] Bug 55: LSP — workspace containment is a bare `startsWith`, and absent when the client sent no `rootUri`

**Status:** Open

**Description:** `readSourceFile` (`src/lsp.zig:426`, check at `:434` at HEAD; `:415` at
`e7b54e7`) tests `std.mem.startsWith(real_path, root)` with no separator boundary, so
`/home/u/proj` admits `/home/u/proj-backup/secret.kl`; when `workspace_root` is null the
check is skipped entirely.

**Steps to reproduce:**
1. `initialize` with rootUri `file:///home/u/proj`; `textDocument/diagnostic` for
   `file:///home/u/proj-backup/secret.kl`.

**Expected:** `AccessDenied`.

**Actual:** Read and reported on.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — driver territory, HIGH
`[security]`; verified by reading at both commits. Fix: require `real_path.len == root.len
or real_path[root.len] == sep`; deny absolute reads with no root.

---

## [ ] Bug 56: `klar run` writes its temporary binary to the predictable `/tmp/klar-run-<unix-seconds>` without `O_EXCL`

**Status:** Open

**Description:** `runNativeFileWithOptions` formats `/tmp/klar-run-{timestamp}`
(`src/main.zig:3774` at HEAD, `:3121` at `e7b54e7`; the Windows arm the same) and the linker
writes it. On a shared host the path can be pre-created as a symlink; two runs in the same
second clobber and delete each other's binary.

**Steps to reproduce:**
1. Two `klar run` invocations in the same second.

**Expected:** Independent temp files.

**Actual:** Same path.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — driver territory, HIGH
`[security]`; verified by reading at both commits. Fix: a unique 0700 temp directory.

---

## [ ] Bug 57: `git` dependencies in `klar.json` are parsed and then silently ignored by every resolver

**Status:** Open

**Description:** `resolveDependencies`, `resolveDependenciesWithoutLock` and `updateLockfile`
only handle `if (dep.path) |rel_path|` (`src/main.zig:6375`, `:6435`, `:6481` at HEAD;
`:5710`, `:5770`, `:5816` at `e7b54e7`); `manifest.zig` parses `git`/`ref`/`version` and
exposes `isGit()`. A git dependency produces no message and the build later fails with
"module 'foo' not found".

**Steps to reproduce:**
1. Add a `git` dependency; `klar build`.

**Expected:** "git dependencies are not supported yet" (or support).

**Actual:** Silent skip, then "module not found".

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — driver territory, HIGH
`[error-handling]`; verified by reading at both commits. Fix: report unsupported kinds.

---

## [ ] Bug 58: `--emit-ir` lowers only the entry module

**Status:** Open

**Description:** The Klar-IR path lowers `modules_to_emit.items[0]` (`src/main.zig:3221` at
HEAD; `lowerModule(module)` at `:2670` at `e7b54e7`) while codegen loops over every module,
so the `.ir` file silently omits imported functions.

**Steps to reproduce:**
1. `klar build --emit-ir app.kl` on a program with imports.

**Expected:** The whole program's IR.

**Actual:** Entry module only.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — driver territory, HIGH
`[correctness]`; verified by reading at both commits. Fix: lower each module.

---

## [ ] Bug 59: `klar build` and `klar run` silently ignore unknown `-` flags; `check` and `test` reject them

**Status:** Open

**Description:** The `build` and `run` argument loops (`src/main.zig:648` region at HEAD,
`:486-530` at `e7b54e7`) have no unknown-option branch; `check` (`:913` at HEAD) and `test`
(`:1012`) print "unknown … option". `klar run --intepret app.kl` runs the native backend;
`klar build app.kl -o` with no value writes the default output.

**Steps to reproduce:**
1. `klar run --intepret app.kl`.

**Expected:** "unknown run option '--intepret'".

**Actual:** Runs natively.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-04 — driver territory, HIGH
`[error-handling]`; verified by reading at both commits. Fix: the same error branch in both
loops.

---

## [ ] Bug 60: VM `op_match_variant` always matches — the first enum arm wins

**Status:** Open

**Description:** `src/vm.zig:966` at HEAD (`:960` at `e7b54e7`) reads the variant-name
constant, discards it (`_ = variant_name; // TODO: Implement variant matching`) and pushes
`Value.true_val` unconditionally. Any `match` over an enum value under the bytecode VM takes
its first variant arm regardless of the value; the interpreter and native backends match
correctly.

**Steps to reproduce:**
1. `enum Color { Red, Green }` and `match c { Green => println("green"), Red => println("red") }` with `c = Color.Red`.
2. `klar run --vm prog.kl`.

**Expected:** `red`.

**Actual:** `green` (first arm).

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-05 — backends territory, CRITICAL
`[correctness]`; verified by reading in the export and at HEAD `97ba34f`. Fix: compare the
peeked value's variant tag/name with the constant and push the result; until then the VM
should refuse `match` on enums rather than return a wrong arm.

---

## [ ] Bug 61: `op_is_type` is emitted with no operand while the VM reads two bytes

**Status:** Open

**Description:** `src/compiler.zig:966` lowers the `is` operator to a bare `op_is_type`
(`// TODO: needs type operand`) with no operand bytes, but `src/bytecode.zig:497` declares the
opcode with 2 operand bytes and `src/vm.zig:882` at HEAD (`:876` at `e7b54e7`) does
`_ = self.readU16()`. The two bytes consumed as the "operand" are the next instruction's
opcode and first operand, so the bytecode stream desynchronises after every `is` expression.

**Steps to reproduce:**
1. Any program with `x is T` under the VM, followed by at least one more instruction.
2. `klar run --vm prog.kl`.

**Expected:** `is` evaluates to a boolean and execution continues.

**Actual:** The instruction after `is` is skipped or misdecoded; behaviour depends on the
following bytes (wrong result, `unreachable`, or a crash).

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-05 — backends territory, CRITICAL
`[correctness]`; verified by reading in the export and at HEAD. Fix: emit the type-constant
index as the u16 operand in the compiler (and implement the check in the VM), or declare the
opcode with 0 operand bytes and reject `is` under the VM until it is implemented.

---

## [ ] Bug 62: or-pattern success jump is never patched — a matching alternative jumps to the `0xffff` placeholder

**Status:** Open

**Description:** In `compilePatternTest` for `.or_` patterns (`src/compiler.zig:1501-1507` at
HEAD and at `e7b54e7`), each alternative emits `op_true` then `const end = try
self.emitJump(.op_jump, line)`, but only `next` is patched; `end` is discarded (`_ = end;`).
A successful alternative therefore executes `op_jump` with the unpatched `0xffff` placeholder
offset, jumping past the end of the chunk.

**Steps to reproduce:**
1. `match n { 1 | 2 => println("small"), _ => println("other") }` with `n = 1`.
2. `klar run --vm prog.kl`.

**Expected:** `small`.

**Actual:** The jump target is `0xffff`; the VM runs off the chunk (crash or `unreachable`).

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-05 — backends territory, CRITICAL
`[correctness]`; verified by reading in the export and at HEAD. Fix: collect the `end` jumps
per alternative and patch them all after the final `op_false`.

---

## [ ] Bug 63: Assignment to an immutable `let` binding passes `klar check` and compiles natively

**Status:** Open

**Description:** The checker's assignment path (`src/checker/expressions.zig:392` at HEAD, `:356`
at `e7b54e7`) runs only `isAssignable`, which is true for every identifier, index, field and
deref; it never consults the binding's mutability. The one site that does (`statements.zig:78`)
handles an `assign` statement node the parser never produces, because `=` is lowered as an infix
binary operator. Result: `let x = 1; x = 2` is accepted at check time, compiled by the native
backend, and executed; only the tree-walking interpreter refuses, and only at runtime.

**Steps to reproduce:**
1. `fn main() -> i32 { let x: i32 = 1  x = 2  return x }`
2. `klar check prog.kl` · `klar build prog.kl && ./build/prog; echo $?` · `klar run --vm prog.kl; echo $?` · `klar run --interpret prog.kl`

**Expected:** `klar check` reports "cannot assign to immutable binding `x`" and every backend refuses.

**Actual:** (Klar 0.5.0, 2026-09-05) `check`: "All checks passed". `build`: succeeds; the native binary exits **2** (the mutated value). `--vm`: exits 0. `--interpret`: "Runtime error in main: ImmutableAssignment" (exit 1) — the only backend that notices, and at runtime.

**Found by:** `/qa-audit --calibrate klar-e7b54e7`, 2026-09-05 — checker territory, CRITICAL
`[correctness]`, found independently by two runs; verified by reading at both commits and by
running the four commands above on the installed compiler. Fix: check `sym.mutable` in the
binary-assign path (the symbol carries it — `expressions.zig:315` reads it for references), or
route `=` to the statement node that already checks it.

---

## [ ] Bug 64: HTTP stdlib computes `Content-Length` and body bounds from codepoint `len()`, not bytes — any non-ASCII body is sent short and parsed truncated

**Status:** Open

**System:** HTTP stdlib message framing — `Content-Length` and body bounds in `stdlib/http_client.kl` (`http_request`, `parse_http_response`) and `stdlib/http_server.kl`

**Description:** On the native backend `string.len()` counts UTF-8 codepoints
(`klar_string_char_len`) and `byte_len()` counts bytes, while `slice()` is byte-indexed and
`tcp_write` sends `strlen` bytes. `stdlib/http_client.kl:196` and `stdlib/http_server.kl:281`
write `Content-Length: ` + `body.len()`, so every request or response with a multibyte
character declares fewer bytes than it sends; the peer reads a truncated body or treats the
tail as the start of the next message. On the receiving side both files extract the body with
`data.slice(body_start + 4, data.len())` — a byte start paired with a codepoint end — so a
non-ASCII body is cut mid-sequence, and `find_str_in`'s `0..s.len()` search window stops
short of `\r\n\r\n` when a header value is non-ASCII. The client's Content-Length early return
(`http_client.kl:245`) compares `response_data.len()` (codepoints) to a byte count and never
fires for such bodies.

**Steps to reproduce:**
1. `fn main() -> i32 { let s: string = "a€b"  println(s.len().to_string().as_str())  println(s.byte_len().to_string().as_str())  println("[" + s.slice(1, s.len()) + "]")  return 0 }` — `klar build` and run.
2. Serve `http_ok("héllo")` from `http_server.kl` and fetch it with `curl -v`.

**Expected:** `3`, `5`, `[€b]`; curl receives `Content-Length: 6` and the body `héllo`.

**Actual:** (Klar 0.5.0, 2026-09-06, step 1 run) `3`, `5`, and `[` + one broken byte + `]` —
`slice(1, s.len())` returns bytes 1..3 of a 5-byte string. Step 2 was not run: by the code at
`http_server.kl:281` the header is `Content-Length: 5` for the 6-byte body `héllo` (`len()` = 5
per step 1), and what the client does with a short declaration is the client's business — the
wire is wrong either way.

**Found by:** `/qa-review` fixture-4 calibration runs (`fable+low`, 2026-09-05 23:58 and
2026-09-06 00:06; also the `fable+sev` run of 2026-09-05 22:59), each filing it CRITICAL
independently; verified 2026-09-06 by running step 1 on the installed compiler and by reading
both stdlib files at HEAD. Fix: `byte_len()` for `Content-Length` and for every `slice` end bound
in both files (and `find_byte_in` / `find_str_in`'s search limit); a `len()`-as-byte-bound lint
would have caught all of them.

---

## [ ] Bug 65: `http_request` returns a truncated body as `Ok` when the read ends before `Content-Length`

**Status:** Open

**System:** HTTP stdlib message framing — `Content-Length` and body bounds in `stdlib/http_client.kl` (`http_request`, `parse_http_response`) and `stdlib/http_server.kl`

**Deferred:** rides the Bug 64 Next Up line, which `/continue-plan` rule 4 collects by System — alone it is none of the three Next Up kinds (no crash, no Phase 0 deliverable waits on it)

**Description:** `http_request` (`stdlib/http_client.kl`) returns
`parse_http_response(response_data)` on three paths without comparing the body it has to the
`Content-Length` header: a `tcp_read` error after some data has arrived (`:217`), a clean close
by the peer before every declared byte arrived (`:225`), and the 1000-read loop cap running out
(`:263`). `parse_http_response` never checks the body length against the header either, so the
caller gets the parsed status (200) and a cut-off body with nothing marking it incomplete.

**Steps to reproduce:**
1. Serve a response with `Content-Length: 100` and close the socket after 50 body bytes (or
   reset it mid-body).
2. Call `http_get` on it and print the status, `body.len()` and whether the result is `Ok`.

**Expected:** `Err` (or an explicit incomplete flag) when fewer than `Content-Length` body bytes
arrived.

**Actual:** (by reading HEAD `a8dfecd`, 2026-09-26; not run) `Ok` with status 200 and a 50-byte
body. Also reached by a download larger than 1000 reads' worth when `recv` returns one MSS at a
time (about 1.4 MB).

**Found by:** `~/.claude` QA calibration, Fixture 4 label pass
(`~/.claude/qa-reviews/Klar/fixture-4/20260922-202009/report.md` row 125), verified 2026-09-22 by
reading `20a5a3e:stdlib/http_client.kl` (`:213-217`, `:257-260`) and HEAD; filed through
`.claude/inbox/` and re-read at HEAD 2026-09-26. Present since Phase 6 (`20a5a3e`). Distinct
from Bug 64 (byte vs codepoint length) and from the server's single `tcp_read`.

---

## [x] Bug 66: Klar does not compile for Windows since the Zig 0.16 migration

**Status:** Fixed

**System:** platform layer — `src/compat.zig` (the Zig 0.16 file/dir/process shim),
`src/main.zig` `getStdOut`/`getStdErr`

**Description:** The Zig 0.15.2 → 0.16.0 migration (`2c69c7f`, 2026-04-20) rebuilt file,
directory and process I/O in `src/compat.zig` on libc and POSIX file descriptors, and says
so: `initArgs` "only targets the platforms currently exercised by the Klar build (macOS,
Linux)". There is no Windows path in the file. CI's Windows jobs, which would have caught
it, never ran after that date because the Linux gate they wait on was red on the stale Zig
pin. So both Windows targets CI builds (`x86_64-windows` full suite, `aarch64-windows`
cross-compile) fail to compile.

**Steps to reproduce:**
1. On macOS with Zig 0.16.0: `zig build -Dtarget=aarch64-windows --prefix <scratch dir>`
   (the CI `cross-compile-windows-arm64` job's command).
2. The same with `-Dtarget=x86_64-windows`.

**Expected:** `klar.exe` is produced for both targets, as it was on Zig 0.15.2.

**Actual:** (2026-09-26, `ci/baseline-zig-016` at `dd207a6`) both fail with the same
6 errors, and these are only Zig's first analysis wave, so more will follow once they are
fixed:
- `std/c.zig:10648`, `:10657`: "dependency on libc must be explicitly specified" (the
  aarch64 cross-compile does not link libc; `compat.zig` calls `std.c.*` unconditionally,
  65 call sites)
- `src/compat.zig:842`: `cwd()` builds a POSIX `AT_FDCWD` integer where Windows needs a
  `HANDLE`
- `src/compat.zig:885`: `initArgs` stores `minimal.args.vector`, which is `[]const u16` on
  Windows
- `src/main.zig:45`, `:55`: `std.os.windows.kernel32.GetStdHandle` no longer exists in 0.16

**Found by:** the CI baseline item (PLAN.md Next Up, 2026-09-26), running the Windows
ARM64 job's command locally before pushing.

**Fix:** Windows gets its own implementation of the compat surface, over Zig 0.16's
cross-platform `std.Io` (`std.Io.Dir`, `std.Io.File`, `std.process.spawn`,
`std.process.Args`/`Environ`, `std.Io.Clock`) on the process-wide single-threaded `Io`, in
the new `src/compat_windows.zig`. Every `compat` entry point in `src/compat.zig` returns into
it first when the target is Windows; the POSIX code after that branch is what macOS and
Linux still run. Linking libc on Windows was rejected: Windows libc has no `openat`,
`fstatat`, `readdir`, `fork` or `waitpid`, and the aarch64-windows build links no libc at
all. The 20 `kernel32.GetStdHandle` sites (`main.zig`, `interpreter.zig`, `vm.zig`,
`vm_builtins.zig`, `repl.zig`, `lsp.zig`, `meta_query.zig`, `formatter.zig`,
`interop/kira_manifest.zig`) read `std.Io.File.stdout/stderr/stdin().handle`. The registry
client (`src/pkg/registry.zig`) connects through `std.Io.net` on Windows. On Windows,
`klar run` exits with the child's exit code cut to 8 bits: std already reports a Windows exit
status as a `u8` (`std/Io/Threaded.zig:15196`), so the `@truncate` before `compat.exit`
changes nothing.

**Test:** none: the reproduction is the cross-compile itself.
`zig build -Dtarget=x86_64-windows` and `-Dtarget=aarch64-windows` into a scratch prefix
both fail at `fa3c287` and both produce `klar.exe` after the fix. With LLVM enabled (the CI
x86_64 job), `zig build-exe -fno-emit-bin -target x86_64-windows-gnu -lc` against the LLVM
headers and the same for `zig test` both type-check clean. That the Windows binary *runs*
correctly (the full suite on `windows-latest`) only the CI Windows job can show.

---

## [x] Bug 67: Klar does not compile for Linux since the Zig 0.16 migration

**Status:** Fixed

**System:** platform layer — `src/compat.zig` (the Zig 0.16 file/dir/process shim),
`src/main.zig` `getStdOut`/`getStdErr`

**Description:** Zig 0.16 defines `std.c.Stat` and `std.c.fstat` as `void` on Linux (std
steers callers to statx). `compat.File.stat` calls `std.c.fstat` and `compat.Dir.statFile`
and `statFromCStat` use `std.c.Stat`, so Klar has not compiled for Linux since the 0.16
migration (`2c69c7f`). The macOS gates stayed green because macOS still has both, and the
Linux CI gate never got as far as compiling Klar: it failed first in `build.zig` on the stale
Zig 0.15.2 pin. The baseline CI run would have hit this at the gate job, and nothing
behind the gate would have run.

**Steps to reproduce:**
1. On macOS with Zig 0.16.0, at `fa3c287`: `zig build-exe -fno-emit-bin -target
   x86_64-linux-gnu -lc -I /opt/homebrew/opt/llvm/include --dep build_options
   -Mroot=src/main.zig -Mbuild_options=<file with pub const has_llvm: bool = true;>`
2. The same with `-target aarch64-linux-gnu`.

**Expected:** Both type-check clean, as the macOS target does.

**Actual:** `src/compat.zig:325:25: error: type 'void' not a function` (`std.c.fstat`), and
once that is bypassed, `type 'void' does not support field access` on `st.mode` in
`statFromCStat`.

**Found by:** the Bug 66 port (2026-09-26), type-checking the Linux targets to confirm the
port left them alone.

**Fix:** `compat.File.stat` and `compat.Dir.statFile` stat through the `statx` syscall on
Linux (`linuxStat` in `src/compat.zig`, `AT_EMPTY_PATH` for the open-file case), returning
the same `compat.Stat` shape. macOS keeps `std.c.fstat`/`fstatat`, untouched.

**Test:** none: the reproduction is the type-check in Steps to reproduce, red at
`fa3c287`, clean for both Linux targets after the fix, for `zig build-exe` and `zig test`
alike. Whether the Linux binary links against CI's LLVM 17 and passes the suite only the
CI Linux jobs can show.

---

## [x] Bug 68: On macOS, `compat.Dir.createFile` ignores `truncate` and `exclusive` — a shorter rewrite leaves the old file's tail behind

**Status:** Fixed

**System:** platform layer — `src/compat.zig` (the Zig 0.16 file/dir/process shim)

**Description:** `createFile` (`src/compat.zig:525-528`) builds its `openat` flags from Linux
octal literals: `0o100` for `O_CREAT`, `0o1000` for `O_TRUNC`, `0o200` for `O_EXCL`. macOS
numbers them differently (`O_CREAT` 0x200, `O_TRUNC` 0x400, `O_EXCL` 0x800). So on macOS
`0o1000` sets `O_CREAT`, `0o100` sets `O_ASYNC` and `0o200` sets `O_FSYNC`. `truncate` and
`exclusive` are never honoured. Any rewrite shorter than the file it replaces (`klar fmt`,
`klar.lock`, `klar.json`) keeps the old file's trailing bytes. Present since the 0.16
migration (`2c69c7f`).

**Steps to reproduce:**
1. Through `compat`, write "LONG CONTENT HERE 1234567890" to a file.
2. `writeFile` the same path with "short".
3. Read it back. Also `createFile(.{ .exclusive = true })` on that existing file.

**Expected:** "short" (5 bytes); the exclusive create fails with `PathAlreadyExists`.

**Actual:** `'shortCONTENT HERE 1234567890'` (28 bytes); the exclusive create returns a handle.

**Found by:** /qa-review on ci/baseline-zig-016, 2026-09-26 — GenA; probe
`scratch/qa-shardA/probe/probe.zig` — reviewer's evidence, not re-read.

**Fix:** `createFile` and `openFile` build their `openat` flags as `std.c.O`, std's
per-target layout, so `CREAT`, `TRUNC`, `EXCL` and the access mode are the target's own
bits; `mapOpenErrno` maps `EEXIST` to `PathAlreadyExists`, which an exclusive create of an
existing file now returns (`src/compat.zig`, fix/bug-68-69-compat-flags).

**Test:** `src/compat_test.zig` — "Dir.createFile truncates…" and "Dir.createFile
exclusive refuses an existing file with PathAlreadyExists".

---

## [x] Bug 69: On macOS, `compat.Dir.deleteTree` never removes directories — `klar clean` leaves `build/` behind

**Status:** Fixed

**System:** platform layer — `src/compat.zig` (the Zig 0.16 file/dir/process shim)

**Description:** `deleteTree` (`src/compat.zig:659`) hard-codes `AT_REMOVEDIR = 0x200`,
which is the Linux value; the comment calls it common to both. On macOS it is 0x80, so the
final `unlinkat` of each directory is a plain file unlink, which fails on a directory, and the
error is discarded. Files inside are deleted and every directory stays.

**Steps to reproduce:**
1. `makePath("…/tree/sub")`, then write a file in it.
2. `deleteTree("…/tree")`, then `access("…/tree")`.

**Expected:** `access` fails; the tree is gone.

**Actual:** `access` succeeds and `tree/sub/` is still on disk.

**Found by:** /qa-review on ci/baseline-zig-016, 2026-09-26 — GenA; probe
`scratch/qa-shardA/probe/probe2.zig` — reviewer's evidence, not re-read.

**Fix:** `deleteTree` passes `std.c.AT.REMOVEDIR` (0x80 on macOS, 0x200 on Linux) to its
final `unlinkat` (`src/compat.zig`, fix/bug-68-69-compat-flags).

**Test:** `src/compat_test.zig` — "Dir.deleteTree removes nested directories, not only
their files".

---

## [ ] Bug 70: POSIX `compat.Child.spawn` never reports a missing program — the linker fallback chain stops at the first missing linker

**Status:** Open

**System:** platform layer — `src/compat.zig` (the Zig 0.16 file/dir/process shim)

**Deferred:** after the current milestone. It only affects `linkBareMetalTarget`'s fallback,
and the first linker it tries is present on every supported host.

**Description:** `Child.spawn` (`src/compat.zig:1102-1118`) forks, then execs in the child.
When exec fails, the child exits 127, so the parent never sees `error.FileNotFound`.
`linker.zig:498-501` maps FileNotFound to `LinkerNotFound` so it can try the next linker, and
on macOS and Linux that branch never fires: the chain stops at the first missing name with
`LinkerFailed`. The Windows path (`std.process.spawn`) does return FileNotFound, so the two
platforms now disagree.

**Steps to reproduce:**
1. `compat.Child.init(&.{"surely-not-a-program"}, alloc)`, then `spawn()` and `wait()`.

**Expected:** `spawn` returns `error.FileNotFound`.

**Actual:** `spawn` succeeds and `wait` returns `.Exited = 127`.

**Found by:** /qa-review on ci/baseline-zig-016, 2026-09-26 — GenA; verified by reading
`src/compat.zig:1102-1118` and `src/codegen/linker.zig:498-501` — reviewer's evidence, not re-read.

---

## [ ] Bug 71: The linker call pipes stdout and stderr but never reads them — a linker that writes more than the pipe buffer hangs `klar build`

**Status:** Open

**System:** native codegen — `src/codegen/linker.zig`

**Deferred:** after the current milestone. Linker output is normally far under the 64 KB
pipe buffer; it takes a flood of warnings to hit.

**Description:** `linker.zig:382-392` spawns the linker with `.Pipe` for stdout and stderr,
never reads either, and calls `wait`. A linker that fills a pipe blocks on write while Klar
blocks in `wait`, so the build hangs. On POSIX `compat.Child.wait` also never closes the
parent's read ends.

**Steps to reproduce:**
1. Link through `linker.zig` with a linker (or a wrapper script in its place) that writes
   more than 64 KB to stderr before exiting.

**Expected:** The link finishes and reports the output or the exit code.

**Actual:** `klar build` hangs in `wait`.

**Found by:** /qa-review on ci/baseline-zig-016, 2026-09-26 — GenA; verified by reading
`src/codegen/linker.zig:382-392` — reviewer's evidence, not re-read.

---

## [x] Bug 72: On Windows every `compat.Child.spawn` fails before the child starts — every `klar build` link fails with no linker output

**Status:** Fixed

**System:** platform layer — `src/compat_windows.zig` `io()`, the one `std.Io` every Windows
compat call runs on

**Description:** `compat_windows.zig` runs every call on
`std.Io.Threaded.global_single_threaded`, whose allocator is `Allocator.failing`
(`std/Io/Threaded.zig:1677`). `std.process.spawn` on Windows builds the command line and
searches PATH in an arena over that allocator (`processSpawnWindows`, `:15578`), so every
spawn returns `error.OutOfMemory` before `CreateProcess` runs. `linker.zig` maps it to
`LinkerFailed`, and every native build on Windows prints "Linker error: Linker failed" with
nothing from `link.exe`, because `link.exe` never started.

**Steps to reproduce:**
1. CI run 36291851109, Windows (full suite), job 108543946245.
2. Same mechanism on macOS: `std.process.spawn(Io.Threaded.global_single_threaded.io(),
   .{ .argv = &.{"true"} })` returns `error.OutOfMemory`; the same call on a `Threaded`
   given a real allocator spawns and exits 0.

**Expected:** `klar build` links through `link.exe` and the native, selfhost, module, app and
args suites run on Windows.

**Actual:** 370 builds fail with "Linker error: Linker failed. Check that all required
libraries are available." (native 6/375 passed, selfhost 29/549, module 2/26, app 1/10,
args 55/61).

**Found by:** PR 44's first CI run (2026-09-27), reproduced from `scratch/win-job.log` and the
macOS spawn repro above.

**Fix:** `compat_windows.zig` runs on its own copy of std's `init_single_threaded` with
`std.heap.page_allocator` in place of `Allocator.failing`, so spawn (and every other Windows
compat call that allocates inside `std.Io`) gets memory. Everything else about the `Io` is
unchanged: the process environment block for PATH, no worker threads.

**Test:** `src/compat.zig` test "Child spawns a program found on PATH and reports its exit
code" (`cmd.exe /c exit 3` on Windows). Only the Windows CI job runs it on the Windows path.

---

## [x] Bug 73: Native runtime checks fail into a bare `unreachable` — on aarch64 Linux a failed bounds check falls through instead of trapping

**Status:** Fixed

**System:** native codegen — `src/codegen/emit.zig`, the failure block of every runtime check
(array, slice and List bounds, `List.set`, overflow, `!`, `unwrap`, `unwrap_err`, match
failure)

**Description:** Each runtime check branches to a failure block that holds only LLVM
`unreachable`. That is undefined behavior, not a trap: LLVM emits a trap for it only where
the target turns on TrapUnreachable (Darwin does; the x86 Linux gate's `array_bounds` also
stopped), and on aarch64 Linux it
emits no instruction at all, so a failed check runs off the end of the function into
whatever code follows. It also licenses the optimizer to delete the check. Klar promises no
undefined behavior and bounds-checked indexing.

**Steps to reproduce:**
1. `klar build test/native/array_bounds.kl -c --target aarch64-linux --emit-asm`.
2. Read `main`: the `bounds.fail` block (`.LBB0_2`) is empty and is the last label before
   `.Lfunc_end0`. The macOS build of the same file has `brk #0x1` there.

**Expected:** Every failure block traps on every target (`llvm.trap`), so `array_bounds`
aborts.

**Actual:** On the Linux ARM64 runner `klar_test_array_bounds` never exited; the job hung
98 minutes until the run was cancelled (CI run 36291851109, job 108543946262).

**Found by:** PR 44's first CI run (2026-09-27), reproduced locally by cross-compiling to
aarch64 Linux assembly.

**Fix:** New `Emitter.emitTrap` (`llvm.trap`, then `unreachable`). All 17 failure blocks
call it: the 16 runtime-check sites and `match.failed`. The aarch64 Linux `bounds.fail` block
is now `brk #0x1`. Left as bare `unreachable`: blocks after a call that does not return
(`abort`, `exit`), merge blocks no branch reaches, the two `?` fallbacks the checker rules
out, and the wasm unsupported-feature trap (wasm's `unreachable` always traps).

**Test:** `test/native/runtime_traps.kl`, checked by `runtime_trap_lowering` in
`scripts/run-native-tests.sh`.

---

## [x] Bug 74: A negative narrow signed index passes the native bounds check — `arr[k]` with an `i8` of -1 reads before the array

**Status:** Fixed

**System:** native codegen — `src/codegen/emit.zig`, the failure block of every runtime check

**Description:** The bounds check zero-extends a narrow index before its unsigned compare,
but the GEP takes the raw index, which LLVM sign-extends (`src/codegen/emit.zig:9898-9930`;
the same pattern at `:5160-5185` and `:9957-9990`). An `i8` of -1 becomes 255 for the check,
passes against any length above 255, and then addresses element -1. The checker accepts any
integer index type (`src/checker/expressions.zig:1149`). Reads and writes both go out of
bounds, with no trap: the undefined behavior Bug 73's work set out to remove.

**Steps to reproduce:**
1. `var arr: [i32; 300] = @repeat(0, 300)`, `let one: i8 = 1`, `let zero: i8 = 0`,
   `let k: i8 = zero - one`, `let v: i32 = arr[k]`, then `println`.
2. `klar build` it and run the binary.

**Expected:** The bounds check traps.

**Actual:** The program prints and exits normally (probe `scratch/probe/neg_index.kl`, macOS
arm64, 2026-09-27: "read arr[-1] without trapping", exit 7).

**Found by:** /qa-review on ci/baseline-zig-016, 2026-09-27 — GenA2; verified by probe.

**Fix:** One bounds check, `Emitter.emitCheckedIndex`, for every trapping index: the eight
array and slice read, write and `ref arr[i]` sites, the three List index reads, and
`List.set` (which List index writes also use). It extends the index by its own signedness
(sign for a signed index, zero for an unsigned one) to at least 64 bits, compares it
unsigned against the zero-extended length, and returns that same i64 for the address. The
unsigned half of the bug went too: a `u8` of 200 was zero-extended for the check and
sign-extended to -56 by the GEP, so `arr[k] = 42` wrote outside `arr[200]`. An index read
through a field or element takes its signedness from `isExprSigned`, which calls it signed
(Bug 95): an unsigned one above its signed maximum now traps instead of misaddressing.

**Test:** `test/native/runtime_checks/index_neg_i8_read.kl`, `index_neg_i8_write.kl`,
`index_neg_i8_slice.kl` (each `// Expected: trap`), and
`test/native/runtime_checks/unsigned_index_and_division.kl`.

---

## [x] Bug 75: Native integer `/` and `%` have no zero or MIN/-1 check — `10 / 0` returns 0 on arm64

**Status:** Fixed

**System:** native codegen — `src/codegen/emit.zig`, the failure block of every runtime check

**Description:** Integer `/` and `%` lower to bare `sdiv`/`udiv`/`srem`/`urem`
(`src/codegen/emit.zig:4570-4581`), which is LLVM undefined behavior for a zero divisor and
for `MIN / -1`. x86 raises SIGFPE; aarch64 returns 0. The VM returns `DivisionByZero`
(`src/vm.zig:1294`), so the backends disagree on the same program.

**Steps to reproduce:**
1. `fn main(args: [String]) -> i32`, `let z: i32 = args.len() - 1`, `let r: i32 = 10 / z`,
   `println("10 / 0 returned {r}")`.
2. `klar build` it and run the binary with no arguments.

**Expected:** A runtime trap, as the VM reports division by zero.

**Actual:** Prints "10 / 0 returned 0" and exits 7 (probe `scratch/probe/div_zero.kl`, macOS
arm64, 2026-09-27).

**Found by:** /qa-review on ci/baseline-zig-016, 2026-09-27 — GenA2; verified by probe.

**Fix:** One lowering, `Emitter.emitCheckedDivRem`, for integer `/` and `%` and for `/=`
and `%=` on a local, an array element, a field and a dereference. Before dividing it
branches to a trapping `div.fail` block when the divisor is zero, or, for a signed
dividend, when it is MIN and the divisor -1. An unsigned dividend has no MIN / -1 case.
The four compound sites always emitted `sdiv`/`srem`; they now pass the target's
signedness, so a `u8` of 200 `/= 3` is 66, not 238. Left unchecked: three internal
divisions whose divisor is a nonzero constant or a channel capacity.

**Test:** `test/native/runtime_checks/div_by_zero.kl`, `mod_by_zero.kl`,
`div_assign_by_zero.kl`, `div_min_neg_one.kl`, `mod_min_neg_one.kl` (each
`// Expected: trap`), and `test/native/runtime_checks/unsigned_index_and_division.kl`.

---

## [x] Bug 76: `klar build -c -o <path>` fails when the path is on another filesystem — the freestanding tests are red on Ubuntu 26.04

**Status:** Fixed

**System:** file move — `compat.Dir.rename` (`src/compat.zig`), called by `buildNative`'s
`-c -o` path (`src/main.zig`)

**Description:** `-c` emits the object under `build/` and then renames it to the `-o` path.
`compat.Dir.rename` is a bare `renameat`, which fails with `EXDEV` when the two paths are on
different filesystems, and it maps `EXDEV` to `Unexpected`. Ubuntu 26.04 mounts `/tmp` as
tmpfs, so every `-c -o /tmp/…` build fails there.

**Steps to reproduce:**
1. Mount a RAM disk: `diskutil erasevolume HFS+ KlarXdev $(hdiutil attach -nomount ram://20480)`.
2. `klar build test/native/freestanding/bare_metal_target.kl --target aarch64-none-elf
   --freestanding -c -o /Volumes/KlarXdev/b.o`.

**Expected:** The object file lands at `/Volumes/KlarXdev/b.o`.

**Actual:** `Failed to rename object file: Unexpected`, and no file. In CI run 36301572227
(PR 45's first run on `ubuntu-26.04`) the gate's freestanding tests failed this way, 2 of 2.

**Found by:** PR 45's CI run 36301572227 (2026-09-27); reproduced locally on a RAM disk
2026-09-28.

**Fix:** `compat.Dir.rename` tries the rename first. When it fails with `CrossDevice`
(`EXDEV`, or `NOT_SAME_DEVICE` from Windows `dirRename`), it copies the file to the
destination and deletes the source (`copyThenDelete`), as `mv` does. Only a cross-filesystem
move stops being atomic. Every caller goes through this one path: `buildNative`'s `-c -o`,
and the same-directory temp-file rename in `src/main.zig`, which never crosses filesystems.

**Test:** `src/compat.zig`, test "Dir.rename moves a file onto another filesystem" (runs the
cross-device case on Linux via `/dev/shm`, and on macOS when a RAM disk is mounted at
`/Volumes/KlarXdev`).

---

## [ ] Bug 77: A failed `klar build` prints its error and exits 0

**Status:** Open

**System:** CLI build errors — `buildNative` in `src/main.zig`, the `try stderr.writeAll(msg);
return;` failure paths

**Deferred:** after the current milestone. Every test script that builds also checks for the
output file, so a failure is still caught in CI; a user script that trusts the exit code is
not.

**Description:** `buildNative` reports a failure by writing to stderr and returning from a
`!void` function, so `main` sees success. `src/main.zig` has about 80 such paths. The `-c -o`
rename failure also leaves the object file in the current directory.

**Steps to reproduce:**
1. Mount a RAM disk at `/Volumes/KlarXdev` and check out a tree before Bug 76's fix.
2. `klar build test/native/freestanding/bare_metal_target.kl --target aarch64-none-elf
   --freestanding -c -o /Volumes/KlarXdev/b.o; echo $?`.

**Expected:** A nonzero exit status.

**Actual:** `Failed to rename object file: Unexpected`, exit status 0, and
`bare_metal_target.o` left in the current directory (seen 2026-09-28 while reproducing
Bug 76).

**Found by:** Builder on `ci/upgrade-runners-actions`, 2026-09-28, while reproducing Bug 76.

---

## [x] Bug 78: `run-unit-tests.sh` counts a skipped Zig test as a failure — the Windows job is red on a skip

**Status:** Fixed

**Description:** The wrapper reads `N/M tests passed` from `zig build test --summary all` and
sets `failed = M - N`. Zig counts a test that returns `error.SkipZigTest` in `M` but not in
`N`, so one skip made the wrapper report one failure and exit 1 while `zig build test`
itself succeeded. Bug 76's test skips on Windows, which exposed it.

**Steps to reproduce:**
1. Put a stub `zig` first on `PATH` that prints
   `Build Summary: 4/4 steps succeeded; 293/294 tests passed (1 skipped)` and exits 0.
2. `./scripts/run-unit-tests.sh`.

**Expected:** `All 293 tests passed (1 skipped)`, exit 0.

**Actual:** `1/294 tests failed`, exit 1. CI run 36473776418, Windows (full suite): "run test
293 pass, 1 skip", then "✗ Unit Tests failed".

**Found by:** CI run 36473776418 on `ci/upgrade-runners-actions`, 2026-09-28.

**Fix:** The wrapper reads the skip count from the parenthesis after `tests passed` only and
sets `failed = M - N - K` (`scripts/run-unit-tests.sh`). The summary line names the skips.
The first fix matched the first `(K skipped)` on the line, so a skipped build step or a
`(1 skipped, 1 failed)` test count still miscounted (qa-review 2026-09-28).

**Test:** `scripts/test-run-unit-tests.sh`, run by `run-tests.sh`: a stub `zig` prints real
Zig 0.16 summary lines (no skips, a skip, a skip beside a failure, a skipped step) and the
test checks the wrapper's exit code and `.test-results.json` counts.

---

## [x] Bug 79: Native `integration` module test crashes intermittently (SIGABRT or SIGSEGV, ~3% of runs)

**Status:** Fixed

**System:** contextual literal width — `emitExprWithHint` in `src/codegen/emit.zig`, the
codegen side of the checker's `checkExprWithHint`: `List.push`, `Sender.send`, tuple
elements

**Description:** The natively built integration binary sometimes dies before printing
anything, with SIGABRT (exit 134) or SIGSEGV (exit 11). The same binary passes on most runs,
so the crash depends on something that varies between runs (heap addresses, uninitialized
memory, timing), not on the input. The test writes and deletes the fixed paths
`/tmp/klar_integration_test.json` and `/tmp/klar_integration_manifest.json`, and the first
sighting guessed that another checkout raced on them. The second sighting rules that out as
the lead: it reproduced with no other Klar test run live.

**Steps to reproduce:**
1. `./zig-out/bin/klar build test/module/integration/main.kl -o <bin>`
2. Run `<bin>` 200 times and count nonzero exits.

**Expected:** `integration` exits 0 on every run.

**Actual:** First sighting, on `fix/gc-reachability` at `225f232`, 2026-09-29:
`✗ integration (expected: 0, got: 134)`, empty output. That rebuilt binary then exited 0 in
80 direct runs. Second sighting, on `plan/protect-main` at `1565e9b` plus a PLAN.md-only
edit, 2026-09-29: the same `./run-tests.sh` failure (2161/2162). The rebuilt binary then
crashed in **6 of 200** direct runs (exit 134 four times, exit 11 twice), with no other Klar
test run live. The next full `./run-tests.sh` passed 2162/2162.

**Found by:** Builder on `fix/gc-reachability`, 2026-09-29, re-gating PR 43.

**Cause:** heap corruption, not a race. Every crash is in test 11 (`test_hash_pipeline`), in
`stdlib/sha256.kl`. `init_k` pushes 64 constants into a `List#[u32]`; the 31 above i32 max
(`k.push(2870763221)`, …) were emitted as i64 and stored 8 bytes into a 4-byte slot. At a
buffer's last slot (index 7, 15, 31, 63) the extra 4 bytes land on the next heap block. When
that block is the header of `sha256`'s input list, the low half of its data pointer becomes
0, and `sha256_pad` faults at `0x100000000` (SIGSEGV) or a later `realloc` aborts on the bad
pointer (SIGABRT). The checker typed each literal as the element type through
`checkExprWithHint`; codegen emitted `push`'s argument with no hint at all. `Sender.send` had
the same gap. `MallocScribble=1` makes the crash likely: 22 of 40 runs failed.

**Fix:** `emitExprWithHint` (`src/codegen/emit.zig`) mirrors `checkExprWithHint`: the hint
reaches a literal, a bare `None`, an `Ok`/`Err`/`Some`/`None` call and a tuple (through
parentheses) and nothing nested inside them. `List.push` passes the list's element type,
`Sender.send` the channel's (`getSenderElementType`), and tuple elements their own element
type, which also stops a tuple element's hint leaking into an index literal or a user
call's arguments inside it. The integration binary then
passed 60 of 60 runs under `MallocScribble=1`.

**Test:** `test/native/list_push_literal_width.kl`, `test/native/channel_send_literal_width.kl`,
`test/native/hint_reach_literal_width.kl`

---

## [ ] Bug 80: `run-tests.sh` passes when a suite script exits nonzero without writing its results file

**Status:** Open

**System:** test wrapper — `run-tests.sh` aggregation

**Deferred:** after the current milestone. No suite has been seen to exit early on a green
run; every one writes its results file on the paths the gates take today.

**Description:** Each suite line ends `|| TOTAL_FAILED=$((TOTAL_FAILED + 1))`
(`run-tests.sh:55-64`), but `run-tests.sh:98` reassigns `TOTAL_FAILED` from the JSON
results files plus `WRAPPER_FAILED`, so those increments are dead stores. A suite that exits
before writing its results file (`scripts/run-native-tests.sh:23` `zig build || exit 1`,
`run-module-tests.sh:13`, `run-selfhost-tests.sh:20`, `run-check-tests.sh:16`, or a `set -e`
abort) counts as 0 failed when the file is missing, or as whatever a stale file from an
earlier run says. The gate that stands in for CI can then print green.

**Steps to reproduce:**
1. Make one suite script exit 1 before it writes results (e.g. add `exit 1` at the top of
   `scripts/run-check-tests.sh`), and delete `.check-test-results.json`.
2. `./run-tests.sh`.

**Expected:** `TOTAL:` shows at least 1 failed and the script exits 1.

**Actual:** The suite's failure is dropped at line 98; the total and exit status come only
from the results files.

**Fix direction:** a separate `SUITE_EXIT_FAILED` counter summed at line 98, as
`WRAPPER_FAILED` already is, with a stub-suite test that pins the exit code.

**Found by:** /qa-review on ci/llvm-21-everywhere, 2026-09-29 — GenA; verified by reading
`run-tests.sh:55-64` and `run-tests.sh:98`.

---

## [ ] Bug 81: `let x: i64 = xs.get(0)!` fails LLVM verification — a declaration's type reaches a nested index literal

**Status:** Open

**System:** contextual literal width — `emitExprWithHint` in `src/codegen/emit.zig`, the
codegen side of the checker's `checkExprWithHint`: `List.push`, `Sender.send`, tuple
elements

**Deferred:** after the current milestone. A valid program fails to build rather than
running wrong, and writing the index as a variable avoids it; no Phase 0 deliverable waits
on it.

**Description:** `let_decl`, `var_decl` and `return` (`src/codegen/emit.zig`, the
`self.expected_type = self.resolveExpectedType(decl.type_)` and
`self.expected_type = self.current_return_klar_type` sites) set `expected_type` for the
whole statement, so every integer literal inside the initializer takes the declared type.
The checker hints only the top expression (`checkExprWithHint`). An index literal inside
`xs.get(0)!` is emitted as i64 against the list's i32 length and the module fails
verification. Bug 79's fix added `emitExprWithHint`, which hints exactly what the checker
hints; these three sites still set the type by scope.

**Steps to reproduce:**
1. A file with `var xs: List#[i64] = List.new#[i64]()`, one push, then
   `let w0: i64 = xs.get(0)!`.
2. `klar build` it.

**Expected:** It builds; `w0` is the first element.

**Actual:** `LLVM Module verification failed: Both operands to ICmp instruction are not of
the same type! %get.idx_lt_len = icmp slt i64 0, i32 %get.current_len`, no binary, and
`klar build` exits 0 (Bug 49).

**Fix direction:** route the three statement sites' value through `emitExprWithHint`;
check what else reads `expected_type` inside a declaration's value (array and struct
literals, Ok/Err payloads) before narrowing it.

**Found by:** Builder on fix/bug-79-integration-crash, 2026-09-29, writing Bug 79's test.

---

## [ ] Bug 82: `List.set`, `Map.insert` and `Set.insert`/`contains` refuse an integer literal that `List.push` accepts

**Status:** Open

**System:** contextual literal width — `emitExprWithHint` in `src/codegen/emit.zig`, the
codegen side of the checker's `checkExprWithHint`: `List.push`, `Sender.send`, tuple
elements

**Deferred:** after the current milestone. The checker refuses the program with a clear
message and `.as#[T]` on the literal works around it; nothing runs wrong.

**Description:** `List.push` and `Sender.send` check their argument with
`checkExprWithHint(arg, element_type)`, so `bytes.push(255)` into a `List#[u8]` types 255
as u8. `List.set`'s value, `Map.insert`'s key and value, and `Set.insert`/`contains`/`remove`
use plain `checkExpr` (`src/checker/method_calls.zig`, e.g. the `set()` branch at ~1353), so
the same literal is an i32 and the call is refused. Giving them the hint also needs their
codegen argument to go through `emitExprWithHint`, or the literal is emitted at i32 into a
wider slot (Bug 79's cause).

**Steps to reproduce:**
1. `var c: List#[u8] = List.new#[u8]()`, `c.push(1)`, `c.set(0, 255)`; and
   `var s: Set#[i64] = Set.new#[i64]()`, `s.insert(3)`.
2. `klar build` it.

**Expected:** It builds, as `c.push(255)` does.

**Actual:** `set() value type mismatch`, `insert() value type mismatch` (Map),
`insert() element type mismatch` and `contains() element type mismatch` (Set).

**Found by:** Builder on fix/bug-79-integration-crash, 2026-09-29, writing Bug 79's test.

---

## [x] Bug 83: `send` on a `Sender#[T]` function parameter emits no code, so the receiver blocks forever

**Status:** Fixed

**System:** Native channels — `isSenderExpr` / `getSenderElementType` in `src/codegen/emit.zig`

**Description:** The send dispatch (`src/codegen/emit.zig:12370-12388`) recognises a sender
through `isSenderExpr` (`:31070`), which looks for a local marked `is_sender` and then asks
the checker. A `Sender#[T]` that arrives as a function parameter matches neither, so the
`send` call falls through and nothing is emitted: the function body loads `%tx` and returns.
The receiver never gets a value and `recv()` blocks forever. `getSenderElementType` uses
the same two-step lookup and would fail the same way once the dispatch is fixed.

**Steps to reproduce:**
1. `fn produce(tx: Sender#[i64]) -> void { tx.send(7) }`, and in `main` create a channel,
   call `produce(tx)`, then `rx.recv()`.
2. `klar build` it with `--emit-llvm` and run it.

**Expected:** `@produce` calls the channel send, and `recv()` returns 7.

**Actual:** `define void @produce(ptr %0)` contains only `load ptr %tx` and `ret void`; the
binary hangs until it is killed.

**Found by:** /qa-review on fix/bug-79-integration-crash, 2026-09-29 — GenA — reviewer's evidence (probe `scratch/qa79a/send_param.kl`, IR and hang), not re-read.

**Fix:** Only a `let` recorded the channel fields (`is_sender`, `is_receiver`,
`channel_element_type`) on its local; a function parameter and a `var` of the same type did
not, and the checker fallback in `isSenderExpr` cannot see a parameter once its function
has been checked. Both now take them from `getChannelTypeInfo(type_)`, as the `let` does
(`src/codegen/emit.zig`). A `var Sender` hung the same way and is fixed with it. Methods,
generic functions and closures taking a `Sender` fail earlier, at LLVM verification: see
Bug 85.

**Test:** `test/native/channel_param_endpoints.kl`

---

## [x] Bug 84: A bare `None` in a tuple element or a `push` argument is emitted as `i32 0`

**Status:** Fixed

**System:** contextual literal width — `emitExprWithHint` in `src/codegen/emit.zig`, the
codegen side of the checker's `checkExprWithHint`: `List.push`, `Sender.send`, tuple
elements

**Description:** `emitExprWithHint` sets `expected_type` for a bare `None` identifier, but
`emitIdentifier` (`src/codegen/emit.zig:~4481`) never reads it, so every bare `None`
outside `return` becomes the `i32 0` placeholder. A tuple is then built from `{i32, i32}` and
read as `{?i64, i32}`. A `push(None)` stores 4 bytes into a 16-byte optional slot, which
leaves the payload uninitialized; the tag reads 0 only by luck.

**Steps to reproduce:**
1. `let t: (?i64, i32) = (None, 3)` then `return t.1`; separately
   `var os: List#[?i64] = List.new#[?i64]()`, `os.push(None)`, with `--emit-llvm`.
2. `klar build` and run.

**Expected:** Exit 3; the push stores a full `?i64` none value.

**Actual:** Exit 192; the IR has `store i32 0, ptr %push.elem_ptr`. It is the same before
and after Bug 79's fix.

**Found by:** /qa-review fix-check on fix/bug-79-integration-crash, 2026-09-29 — GenA — reviewer's evidence (probes `scratch/qa79a/fc_none.kl`, `fc_pushnone.ll`), not re-read.

**Fix:** `emitIdentifier` (`src/codegen/emit.zig`) emits a bare `None` that is not a local
as `emitNone` of the hinted optional when `expected_type` is an optional, as `None()` does.
The tuple is now built as `{ {i1, i64}, i32 }` and `push(None)` stores the whole `{i1, i64}`.
The push half has no runtime symptom (the 4-byte zero also zeroed the tag); it is checked
in the IR only.

**Test:** `test/native/none_hint_width.kl`

---

## [ ] Bug 85: A method, generic function or closure taking a `Sender#[T]` fails LLVM verification

**Status:** Open

**System:** Native channels — the lowering of a `Sender#[T]`/`Receiver#[T]` parameter type
outside a plain function's prototype, `src/codegen/emit.zig`

**Deferred:** after the current milestone. A valid program fails to build rather than
running wrong, and a plain function parameter (fixed in Bug 83) works; no Phase 0
deliverable waits on it.

**Description:** A plain function lowers a `Sender#[i64]` parameter to `ptr`. A method
prototype lowers it to `i32`, a monomorphized generic function to `{ ptr }` while its call
site passes `ptr`, and a closure's call site types the argument `i32`. Each call then fails
"Call parameter type does not match function signature".

**Steps to reproduce:**
1. `impl Pump { fn feed(self: Self, tx: Sender#[i64]) -> void { tx.send(1) } }`, or
   `fn g#[T](tx: Sender#[T], v: T) -> void { tx.send(v) }` called as `g#[i64](tx, x)`, or
   `let f: fn(Sender#[i64]) -> void = |s: Sender#[i64]| -> void { s.send(9) }` then `f(tx)`.
2. `klar build` it.

**Expected:** It builds, and the value arrives on the receiver.

**Actual:** `LLVM Module verification failed: Call parameter type does not match function
signature!` with `call void @Pump_feed({ i64 } %p, ptr %tx)` (declared `i32`),
`call void @"produce_generic$i64"(ptr %tx, …)` (declared `{ ptr }`), and
`call i32 %fn.ptr(ptr %env.ptr, ptr %tx)`.

**Found by:** /continue-plan on fix/bug-83-sender-param, 2026-09-29 — Builder — reproduced
(a first cut of `test/native/channel_param_endpoints.kl` with all three cases).

---

## [x] Bug 86: `send` on a struct-field or tuple-field `Sender` emits no code, so the receiver blocks

**Status:** Fixed

**System:** Native channels — `isSenderExpr` / `getSenderElementType` in `src/codegen/emit.zig`

**Description:** `isSenderExpr` (`src/codegen/emit.zig:31095`) and `isReceiverExpr` (`:31110`)
recognise a local marked `is_sender`/`is_receiver`, then ask the checker. A target that is not
a bare identifier, such as a struct field `w.tx.send(v)` or a tuple field `pair.0.send(v)`, is
neither, so the send falls through and nothing is emitted. This happens in `main` as well as
in a function, and it is Bug 83's mechanism on a different target.

**Steps to reproduce:**
1. `struct W { tx: Sender#[i64] }`, build `w` from `channel#[i64]()`'s sender, then
   `w.tx.send(7)` followed by `rx.recv()`.
2. `klar build` and run it.

**Expected:** `recv` returns 7.

**Actual:** The send emits only the field GEP and load. With a later `send(1)`, `recv` reads
1 (probes exit 1). Without one, `recv` blocks forever.

**Found by:** /qa-review on fix/bug-83-sender-param, 2026-09-29 — GenA — reviewer's evidence
(probes `scratch/qa83/struct_field_main.kl`, `param_recv_via_field.kl`, `struct_field_sender.kl`), not re-read.

**Fix:** Every channel test (`isSenderExpr`, `isReceiverExpr`, `getSenderElementType`,
`getReceiverElementType`, and `recv`'s element width) now goes through
`channelEndpointOf` (`src/codegen/emit.zig`), which reads the declared type of a local or
of a struct or tuple field path rooted at one (`localPathType`), at any depth, before
falling back to the checker. The per-local `is_sender`/`is_receiver`/`channel_element_type`
flags are gone: the local's recorded `semantic_type` carries the same fact. Impl methods
(with `Self` read as the impl's struct) and monomorphized functions and methods record it
for their parameters too, and a field path through a generic struct (`Holder#[i32]`)
reads the monomorphized struct's fields. Closure parameters do not, since a native closure
cannot yet take a struct parameter at all (the call fails LLVM verification).

**Test:** `test/native/channel_field_endpoints.kl`

---

## [x] Bug 87: An aliased channel endpoint type is not a channel endpoint — `send` hangs or fails verification

**Status:** Fixed

**System:** Native channels — `isSenderExpr` / `getSenderElementType` in `src/codegen/emit.zig`

**Description:** `getChannelTypeInfo` (`src/codegen/emit.zig:7786-7803`) counts only a literal
`generic_apply` of `Sender`/`Receiver` as a channel type. An alias (`type Tx = Sender#[i64]`)
is not resolved, so a `let` of that type gets no channel fields and a parameter of that type
is lowered as `i32`. Since Bug 83's fix, parameters and `var`s go through the same helper
and share the gap.

**Steps to reproduce:**
1. `type Tx = Sender#[i64]`, then `let tx: Tx = pair.0`, `tx.send(4000000000)`, `rx.recv()`.
2. Separately, `fn produce(tx: Tx) -> void { tx.send(1) }` called with the sender.
3. `klar build` and run each one.

**Expected:** The value arrives, as it does with `Sender#[i64]` written out.

**Actual:** The `let` case hangs (`timeout 5` exit 124). The parameter case fails LLVM
verification with `call void @produce(ptr %tx3)`, where the function is declared `i32`.

**Found by:** /qa-review on fix/bug-83-sender-param, 2026-09-29 — GenA — reviewer's evidence
(probes `scratch/qa83/alias_let.kl`, `alias_param.kl`), not re-read.

**Fix:** A local's `semantic_type` is its declared type resolved through the checker, so
an alias of `Sender#[T]`/`Receiver#[T]` resolves to the endpoint, and `channelEndpointOf`
(Bug 86's one path) reads it for a `let`, a `var` or a parameter. `namedTypeToLLVM` lowers
a named alias of an endpoint as `typeExprToLLVM` lowers a spelled-out `Sender#[T]` (the
pointer), so a parameter of the alias matches its call site. How that pointer relates to
the `{ ptr }` endpoint layout elsewhere is Bug 85's question, unchanged here.

**Test:** `test/native/channel_alias_endpoints.kl`

---

## [ ] Bug 88: A bare `None` passed as a user-function argument is emitted as `i32 0`

**Status:** Open

**System:** contextual literal width — `emitExprWithHint` in `src/codegen/emit.zig`, the
codegen side of the checker's `checkExprWithHint`: `List.push`, `Sender.send`, tuple
elements

**Deferred:** after the current milestone. The program fails to build rather than running
wrong, and `Some`/typed locals work around it.

**Description:** Call arguments are not hinted with their parameter types, so a bare `None`
argument reaches `emitIdentifier` (`src/codegen/emit.zig:4511-4517`) with no optional hint
and is emitted as the `i32 0` placeholder. Bug 84's fix covers only hinted sites.
`let n: ?i64 = f(None)` works only because the statement's hint happens to match the
parameter's type.

**Steps to reproduce:**
1. `fn pick(a: ?i32) -> i64 { ... }`, then `let x: i64 = pick(None)`.
2. `klar build` it.

**Expected:** It builds, and `pick` receives none.

**Actual:** LLVM verification fails with `call i64 @pick(i32 0)`, where `pick` is declared to
take `{ i1, i32 }`.

**Found by:** /qa-review on fix/bug-83-sender-param, 2026-09-29 — GenA — reviewer's evidence
(probe `scratch/qa83/pre_nest_call.kl`), not re-read.

---

## [x] Bug 89: A non-channel type alias lowers to `i32` — wrong-width parameters and a compiler segfault

**Status:** Fixed

**System:** native codegen — `namedTypeToLLVM` in `src/codegen/emit.zig`

**Description:** `namedTypeToLLVM` (`src/codegen/emit.zig:7418-7433`) resolves an alias only
for extern types and, since Bug 87, for channel endpoints. Every other alias falls through
to the `i32` default, so a parameter or local declared through `type Id = i64` or
`type P = (i64, i64)` gets the wrong LLVM type.

**Steps to reproduce:**
1. `type Id = i64` with `fn f(x: Id) -> i64 { return x }`, called from `main`; or
   `type P = (i64, i64)` with `let pair: P = (1, 2)` then `pair.0`.
2. `klar build` it.

**Expected:** It builds; the alias lowers as the type it names.

**Actual:** The first fails LLVM verification (`ret i32 %x1` in a function returning `i64`).
The second segfaults the compiler in `LLVMStructGetTypeAtIndex` (`emitFieldAccess`, `:9864`).

**Found by:** /qa-review on fix/bug-86-channel-field-alias, 2026-09-29 — GenA — reviewer's
evidence (probes in the review scratchpad), not re-read.

**Fix:** The emitter records every non-generic `type Name = T` under its module
(`registerTypeAlias`, from `registerAllStructDecls`), and reads aliases under the module
whose code it is emitting, a monomorphized generic body included (`decl_alias_scopes`).
A `pub` alias is also kept by bare name, which is how an importer finds it; two modules
exporting a `pub` alias of the same name still collide there, the last registered winning.
`resolveAliasTypeExpr` follows a named type through its aliases, and `namedTypeToLLVM`
lowers an alias as the type expression it names. These sites resolve the declared type:
the `let` and `var` arms of `emitStmt`, the parameter loops of `emitFunction` and
`emitImplMethods` (a `ref` parameter's inner type included), the literal hint a `let`,
`var` or return type asks the checker for (`substituteAliases`, at every depth of a tuple,
optional, array, result or generic argument), a struct field's recorded type name, and a cast's target in `emitTypeCast` and
`isExprSigned`. The
struct name, signedness, string, array and collection readers below them see the named
type. Reading the code turned up the same cause past the LLVM type: a struct alias lost its
field names (`UnsupportedFeature`), an unsigned alias compared and divided as signed, and a
string alias local read `len()` as 0. The duplicate signedness reader `isTypeSigned` is
gone, its callers on `isTypeExprSigned`. A generic alias is still unrecorded.

**Test:** `test/native/type_alias_lowering.kl`, `test/native/type_alias_readers.kl`,
`test/native/type_alias_declarations.kl`, `test/module/type_alias_scope/`

---

## [x] Bug 90: A channel endpoint reached by an index or a `for` binding is not an endpoint

**Status:** Fixed

**System:** Native channels — `isSenderExpr` / `getSenderElementType` in `src/codegen/emit.zig`

**Description:** `channelEndpointOf` (`src/codegen/emit.zig:7780-7823`) finds an endpoint
through a local or a field path rooted at one (`localPathType`). An index expression has no
case there, and a `for` loop binding is registered without `semantic_type`, so both fall to
the checker fallback, which cannot see them. The send is silently dropped, as in Bug 86.

**Steps to reproduce:**
1. `let txs: [Sender#[i64]; 1] = [tx]` then `txs[0].send(4000000001)` and `rx.recv()`; or
   `for t: Sender#[i64] in txs { t.send(4000000001) }` followed by a sentinel send.
2. `klar build` it and run.

**Expected:** The value arrives on the receiver.

**Actual:** The indexed send emits nothing and the `recv` hangs (exit 124); the loop
binding's send is dropped and `recv` reads the sentinel (exit 1).

**Found by:** /qa-review on fix/bug-86-channel-field-alias, 2026-09-29 — GenA — reviewer's
evidence (probes `arr.kl`, `forl.kl`), not re-read.

**Fix:** `localPathType` (`src/codegen/emit.zig`), the declared-type reader behind Bug 86's
one path `channelEndpointOf`, now reads an index of an array, a slice or a List as the
element type, so `txs[0]`, `s[0]` on a slice parameter, `fleet.txs[0]` and `ws[0].tx` resolve
like any field path. A `for` binding over an array, slice, List or Set records its annotated
type (which the parser requires) as its `semantic_type`, as a `let` does, and a `for (k, v)`
over a Map records the Map's key and value types.

**Test:** `test/native/channel_index_endpoints.kl`

---

## [ ] Bug 91: Channels do not exist in the tree-walking interpreter

**Status:** Open

**System:** interpreter builtins — `src/interpreter.zig`

**Deferred:** after the current milestone. The bytecode VM (the default `klar run`) and
native both run channel programs; only `--interpret` fails, and no Phase 0 deliverable
depends on interpreter channels.

**Description:** `channel_create` and the `Sender`/`Receiver` methods are implemented in the
VM and native codegen but not in `src/interpreter.zig`, against the rule that a language
feature works in all three backends.

**Steps to reproduce:**
1. `klar run test/native/channel_param_endpoints.kl --interpret` (or either new channel test).

**Expected:** Exit 0, as with the VM and a native build.

**Actual:** `Undefined variable: 'channel_create'`, exit 1.

**Found by:** /qa-review on fix/bug-86-channel-field-alias, 2026-09-29 — GenB — reviewer's
evidence (`--interpret` runs of the three channel tests), not re-read.

---

## [ ] Bug 92: Checking `channel_create` leaks the tuple's element slice

**Status:** Open

**System:** type checker — `checkCallImpl` in `src/checker/checker.zig`

**Deferred:** after the current milestone. A debug-allocator leak report at compile time;
the compiled program is correct and no Phase 0 deliverable depends on it.

**Description:** The `channel_create` case dupes `tuple_elems` with `self.allocator`
(`src/checker/checker.zig:3880`) and passes it to `type_builder.tupleType`, which copies the
elements into its own arena (`src/types.zig:809`). The first copy is never freed.

**Steps to reproduce:**
1. `klar build test/native/channel_index_endpoints.kl` (any program that calls `channel_create`).

**Expected:** No leak report.

**Actual:** `error(DebugAllocator): memory address … leaked`, from `checkCallImpl`.

**Found by:** /qa-review on fix/bug-90-channel-index-for, 2026-09-29 — GenA; verified by
reading `src/checker/checker.zig:3880` and `src/types.zig:807-811`.

---

## [ ] Bug 93: A `ref` array parameter cannot be indexed

**Status:** Open

**System:** type checker — index and for-iterable checks in `src/checker/`

**Deferred:** after the current milestone. Passing the array by value, or iterating a local,
works; no Phase 0 deliverable indexes through a `ref` parameter.

**Description:** Indexing a parameter of type `ref [T; N]` types as `?unknown`, so the
function is rejected. Indexing or iterating a `ref List#[T]` parameter is rejected the same
way (`cannot index this type`, `cannot iterate over this type`). Codegen already unwraps a
`.reference` in `elementTypeOf` for channel endpoints, but that path is unreachable until the
checker accepts the form.

**Steps to reproduce:**
1. `fn first(xs: ref [i32; 2]) -> i32 { return xs[0] }`, called as `first(ref a)`.
2. `klar check` the file.

**Expected:** It checks, and `first` returns `a[0]`.

**Actual:** `return type mismatch: expected i32, got ?unknown` at `xs[0]`.

**Found by:** /qa-review on fix/bug-90-channel-index-for, 2026-09-29 — GenA (probe
`scratch/b90/refparam.kl`); verified with `scratch/rv/refidx.kl`.

---

## [ ] Bug 94: `http_client` lets `"https://"`, `"http://"` and an uppercase `HTTPS://` through, and hides the https message

**Status:** Open

**System:** HTTP stdlib URL parsing — `parse_url` and `http_request` in `stdlib/http_client.kl`

**Deferred:** after the current milestone — a malformed URL or an uppercase scheme; no crash,
and no Phase 0 deliverable fetches over HTTP.

**Description:** `parse_url` (`stdlib/http_client.kl:46-56`) rejects `https://` only when
`url.len() > 8`, so the exact string `"https://"` falls through to host parsing and comes
back `Ok` with host `https` and path `//`. The `http://` strip uses the same `>` where `>=` is
meant, so a bare `"http://"` keeps its scheme as the host. The prefix compare is
case-sensitive, so `HTTPS://host/` parses to host `HTTPS` and reaches `tcp_connect`. And
`http_request` (`:163-165`) replaces every `parse_url` error with `"invalid URL: " + url`, so
the "https is not supported, use http://" message never reaches a caller. No test calls
`http_get` with an `https://` URL.

**Steps to reproduce:**
1. A copy of `parse_url` under `klar run`: call it with `"https://"`, `"https://x"` and
   `"HTTPS://x/"`.
2. `http_get("https://example.com/")` and print the error.

**Expected:** Each of the three is `Err("https is not supported, use http://")`, and
`http_get` returns that message.

**Actual:** (probe 2026-09-29) `parse_url("https://")` → `Ok(host=https, path=//)`;
`parse_url("https://x")` → `Err("https is not supported, use http://")`. By reading HEAD
`2ab1b84`: `HTTPS://x/` → `Ok(host=HTTPS)`, and `http_get` returns `"invalid URL: …"`.

**Found by:** `~/.claude` qa-calibrate trial c3gen, Fixture 4 export of `20a5a3e` (runs
0929-173800, 0929-174150, 0929-173426); verified at `20a5a3e` and `adde2f0`, filed through
`.claude/inbox/`, re-read at `2ab1b84` 2026-09-29.

---

## [ ] Bug 95: An unsigned value read through a field, element, `for` binding or call result compares signed natively

**Status:** Open

**System:** native codegen — `isExprSigned` in `src/codegen/emit.zig`

**Deferred:** after the current milestone. Wrong results only when an unsigned value is above
its signed maximum; locals and parameters compare correctly, and no Phase 0 deliverable
depends on it.

**Description:** `isExprSigned` (`src/codegen/emit.zig:4272-4273`) knows the signedness of a
local or parameter, but not of a `u8`/`u32` value reached through a struct field, an array or
tuple element, a `for` binding or a function's return value. Those compare and divide as
signed in native code, while the interpreter treats them as unsigned. `u32.to_string()` and
`"{a}"` print the signed reading too. No type alias is involved.

**Steps to reproduce:**
1. `struct H { w: u32 }`, `let h: H = H { w: 3000000000 }`, `if h.w < 5.as#[u32] { return 2 }`.
2. The same with `let arr: [u32; 2] = [3000000000, 1]` and `arr[0] < arr[1]`, a tuple
   `t.0 < 5.as#[u32]`, `for k: u32 in arr { if k < 1 … }`, and `fn get() -> u8` returning 200
   compared with `get() < 100.as#[u8]`.
3. `klar build` and run each; then `klar run --interpret`.

**Expected:** Each comparison is false (exit 0), and `3000000000.as#[u32].to_string()` is
`"3000000000"`.

**Actual:** Native exits 2 (field), 8 (array), 9 (tuple), 11 (`for`) and 2 (call result);
the interpreter exits 0. `to_string()` prints `-1294967296`.

**Found by:** /qa-review on fix/bug-89-type-alias-lowering, 2026-09-29 — GenA, GenB (probes
`scratch/gb89/b_holder_w.kl`, `b_arr.kl`, `b_tup.kl`, `b_forb.kl`, `tostr.kl`,
`scratch/qa89a/ret_ctl.kl`) — reviewer's evidence, not re-read.

---

## [ ] Bug 96: A field read on a `List` index (`ps[0].y`) fails native build with `UnsupportedFeature`

**Status:** Open

**System:** native codegen — field access in `src/codegen/emit.zig`

**Deferred:** after the current milestone. The build refuses rather than miscompiling, and
copying the element to a local first works.

**Description:** Reading a field straight off a `List#[Point]` index fails the native build,
because the chained field reader cannot find the element's struct type. The interpreter runs
the same program. An alias makes no difference.

**Steps to reproduce:**
1. `struct Point { x: i64, y: i64 }`, `var ps: List#[Point] = List.new#[Point]()`,
   `ps.push(Point { x: 1, y: 2 })`, `if ps[0].y != 2 { return 1 }`.
2. `klar build` the file.

**Expected:** It builds and exits 0.

**Actual:** `Codegen error: UnsupportedFeature`.

**Found by:** /qa-review on fix/bug-89-type-alias-lowering, 2026-09-29 — GenB (probes
`scratch/gb89/b_listq.kl`, `p_listq.kl`) — reviewer's evidence, not re-read.

---

## [x] Bug 97: An array variable or `@repeat` assigned to a slice is not converted natively — `let s: [i32] = arr` crashes

**Status:** Fixed

**System:** array-to-slice coercion — the `let` and `var` declaration paths in
`src/codegen/emit.zig` (`is_slice_decl and is_array_literal_value`, ~2636 and ~2807) and
the slice-typed struct field path (~9901), which call `convertArrayToSlice` only for an
array literal

**Description:** A slice-typed declaration converts its value to `{ ptr, len }` only when
the value is an array literal. Any other array value, such as an array variable or an
`@repeat(...)`, takes the normal path and stores the array's bytes straight into the slice
slot, so the slice's pointer and length are the array's first elements. The interpreter runs
both programs correctly.

**Steps to reproduce:**
1. `let arr: [i32; 3] = [1, 2, 3]`, `let s: [i32] = arr`, `if s.len() != 3 { return 1 }`,
   `if s[2] != 3 { return 2 }`, `return 0`.
2. The same with `let s: [i32] = @repeat(7, 3)` and `if s.len() != 3 { return 1 }`.
3. `klar build` each and run; then `klar run --interpret`.

**Expected:** Both exit 0, as they do under `--interpret`.

**Actual:** The array variable exits 138 (SIGBUS); `@repeat` exits 1 (the length is wrong).
With `var arr: [i32; 300]` the same line exits 139 or 133 depending on size (probes
`scratch/probe74/coerce.kl`, `rep.kl`, `sl*.kl`, macOS arm64, 2026-09-30).

**Found by:** Builder on fix/bug-74-75-runtime-checks, 2026-09-30, writing Bug 74's slice
test; verified by probe.

**Fix:** Every stored array-to-slice coercion goes through one path, `emitSliceValue`
(`src/codegen/emit.zig`, beside `convertArrayToSlice`), which decides on the emitted value's
type rather than the AST's shape: any array value is copied to a heap backing and becomes
`{ ptr, len }`, a large `@repeat` is written straight into that backing, and a slice passes
through unchanged. The `let` and `var` declarations of a `[T]` take it ahead of the
large-`@repeat` path, a plain `=` to a slice variable takes it, and so does a slice-typed
struct field initializer. Function arguments keep `convertArgIfNeeded`'s stack copy, which
lives as long as the call. (fix/bug-97-array-slice-coercion, 2026-10-04)

**Test:** `test/native/array_to_slice_coercion.kl`

---

## [ ] Bug 98: `List#[Id]` of a module-private alias crashes the compiler when another module declares its own `Id`

**Status:** Open

**System:** type alias scope — the element-type readers in `src/codegen/emit.zig`
(`getListTypeInfo` `:7795`, array/slice readers `:7769`, `:7777`, `List.new` `:18353`,
`Map.new` `:20233-20234`, `:21946`)

**Escaped from:** fix/bug-89-type-alias-lowering (PR 53) — scoped aliases for declared,
parameter and return types, but not for the element-type readers, and its Fix text says
every `let` and `var` reads its own module's alias

**Description:** Bug 89's fix substitutes a module's own aliases before the codegen asks the
checker for a declared type. The readers that get a collection's element type still ask the
checker directly, and the checker does not keep aliases per module, so when two modules each
declare `type Id` the element type of `List#[Id]` can be the other module's `Id`.

**Steps to reproduce:**
1. Module `a`: `type Id = i64`, `pub fn f() -> Id { var xs: List#[Id] = List.new#[Id]();
   xs.push(5000000000); let k: i32 = 0; return xs[k] }`.
2. Module `b`: `type Id = i32`, `pub fn b_val() -> Id { return 7 }`.
3. `main.kl` imports both and returns 42 when `f()` is 5000000000 and `b_val()` is 7.
4. `klar build main.kl`.

**Expected:** It builds and exits 42.

**Actual:** The compiler crashes with "integer does not fit in destination type" at
`src/codegen/emit.zig:4472`. It builds and exits 42 without module `b`, with `List#[i64]`,
or when `b`'s `Id` is also `i64` (probe `scratch/gb89/mod/q/r3_l3/`).

**Found by:** /qa-review fix-check round 3 on fix/bug-89-type-alias-lowering, 2026-09-30 —
GenB, reply received after PR 53 merged; filed by /qa-review on fix/bug-74-75-runtime-checks
— reviewer's evidence (probe with three controls), not re-read.

---

## [ ] Bug 99: `*p += …` (any compound assignment) through an `inout` parameter hangs the compiler or fails LLVM verification

**Status:** Open

**System:** deref compound assignment — the `.unary` deref arm of compound assignment in
`src/codegen/emit.zig` (`ptr_elem_type = LLVMGetElementType(typeOf(ptr))`, `:5518`)

**Description:** The compound-assignment path through a dereference takes the load type
from `LLVMGetElementType` of the pointer's LLVM type. Pointers are opaque, so that call
returns no real element type, and every `op=` arm loads with a garbage type. Plain
`*p = *p + 1` works (`test/native/ref_inout.kl`).

**Steps to reproduce:**
1. `fn bump(p: inout i32) -> void { *p += 1 }`, called with `ref d` from `main`.
2. The same with `p: inout u8` and `*p += 1.as#[u8]`, or `*p /= 3.as#[u8]`.
3. `klar build` each.

**Expected:** Each builds; `d` is updated through the reference.

**Actual:** The `i32` form fails LLVM verification (`%addtmp = add half %loadtmp, i32 1`);
the `u8` forms never finish building (killed after 20 s) (probes `scratch/qr7475/h5.kl`,
`h7.kl`, `h2.kl`, macOS arm64, 2026-09-30).

**Found by:** /qa-fix on fix/bug-74-75-runtime-checks, 2026-09-30, writing the deref case
of finding #1's test; verified by probe (the `+=` arm is untouched by that branch).

---

## [ ] Bug 100: `tcp_read(s, max_bytes <= 0)` returns an IoError built from a stale errno

**Status:** Open

**System:** native socket argument guards — the `max_bytes <= 0` blocks of `emitTcpRead`
(`tcpr.max_bad`) and `udp_read` (`udpr.max_bad`) in `src/codegen/emit.zig`

**Deferred:** after the current milestone. The read is still refused with `Err`; only the
error kind is wrong, and only for a caller passing a non-positive size.

**Description:** The L7 guard rejects `max_bytes <= 0` before any syscall runs, then calls
`emitErrnoToIoError`, so the error kind is whatever errno an earlier call left behind.
`udp_read` has the same block.

**Steps to reproduce:**
1. Make any failing libc call (open a missing file), then `tcp_read(stream, 0)`.

**Expected:** A fixed kind such as `InvalidInput`, the same every time.

**Actual:** The earlier failure's kind (e.g. `NotFound`), or `Other` when errno is 0.

**Found by:** `~/.claude` qa-calibrate trial c4ens (Fixture 4 ensemble), 3 of 6 runs
(2026-09-30 171611, 172024, 172442); filed through `.claude/inbox/`, code re-read at
`3ab4ec6` 2026-10-03 — not run.

---

## [ ] Bug 101: The HTTP server matches routes against the raw request target, query string included

**Status:** Open

**System:** HTTP server routing — `parse_request_line_path` and `match_route` in
`stdlib/http_server.kl`

**Deferred:** after the current milestone. A stdlib routing defect with a caller-side
workaround (strip the query before matching); no compiler path depends on it.

**Description:** The request path is the whole request target, and nothing strips `?…`, so
a query string becomes part of the path a route is matched against.

**Steps to reproduce:**
1. Register the exact route `/health`, then `curl 'http://127.0.0.1:<port>/health?x=1'`.

**Expected:** `/health` matches, and the query is available separately.

**Actual:** No route matches; the param route `/api/users/{id}` given `/api/users/42?v=1`
binds `id = "42?v=1"`.

**Found by:** `~/.claude` qa-calibrate trial c4ens (Fixture 4 ensemble), 2 of 6 runs
(2026-09-30 171611, 172605); filed through `.claude/inbox/`, code re-read at `3ab4ec6`
2026-10-03 — not run.

---

## [ ] Bug 102: `http_request` sends caller headers unvalidated and duplicates `Content-Length`

**Status:** Open

**System:** HTTP stdlib message framing — `Content-Length` and body bounds in
`stdlib/http_client.kl` (`http_request`, `parse_http_response`) and `stdlib/http_server.kl`

**Deferred:** after the current milestone. Reached only by a caller that passes its own
`Content-Length` or a header containing CR/LF; Bug 64 already holds this System's next fix.

**Description:** `http_request` concatenates the caller's header names and values, and the
path, into the request with no CR/LF check, so a value containing `\r\n` injects headers.
A caller that passes its own `Content-Length` gets a second one appended whenever the body
is non-empty.

**Steps to reproduce:**
1. `http_request("POST", url, headers {"Content-Length": "3"}, "abc")`.

**Expected:** One `Content-Length` header; a header containing CR or LF rejected with `Err`.

**Actual:** Two `Content-Length` lines (a request-smuggling shape); CR/LF passed through.

**Found by:** `~/.claude` qa-calibrate trial c4ens (Fixture 4 ensemble), 1 of 6 runs
(2026-09-30 172132); filed through `.claude/inbox/`, code re-read at `3ab4ec6` 2026-10-03
— not run.

---

## [ ] Bug 103: TCP sockets are created without close-on-exec, so a spawned child inherits them

**Status:** Open

**System:** native fd inheritance — the `socket` and `accept` calls in `src/codegen/emit.zig`
(`emitTcp*`); only the `process_spawn` pipes get `FD_CLOEXEC`

**Deferred:** after the current milestone. Only a program that holds a socket while calling
`process_spawn` is affected, and Bug 13 (non-variadic `fcntl`) must be settled first for an
`fcntl`-based fix to work on arm64 macOS.

**Description:** A server that calls `process_spawn` while holding a listener or a
connection leaks those fds into the child, which keeps the port bound and the peer's
connection open after the parent closes it. Related to Bug 13: `SOCK_CLOEXEC` / `accept4`
on Linux avoids `fcntl` altogether.

**Steps to reproduce:**
1. `tcp_listen`, `process_spawn("sleep", ["30"])`, `tcp_listener_close`, exit.
2. `lsof -i :<port>` while the child sleeps.

**Expected:** The port is free once the parent exits.

**Actual:** The child holds the listening socket.

**Found by:** `~/.claude` qa-calibrate trial c4ens (Fixture 4 ensemble), 1 of 6 runs
(2026-09-30 172605); filed through `.claude/inbox/`, code re-read at `3ab4ec6` 2026-10-03
— not run.

---

## [ ] Bug 104: `process_wait` treats a read error while draining stdout/stderr as EOF

**Status:** Open

**System:** native process wait — the poll drain loop of `emitProcessWait` in
`src/codegen/emit.zig` (`wait.oeof` / `wait.eeof`, `nr <= 0`)

**Deferred:** after the current milestone. Needs a signal handler installed without
`SA_RESTART`, or an EIO, to reach; the common path reads to a real EOF.

**Description:** A `read` returning -1 (EINTR from a signal, or EIO) ends that stream as if
at EOF, so the output comes back silently truncated with `Ok`.

**Steps to reproduce:**
1. Install a signal handler without `SA_RESTART`, spawn a child producing large output, and
   deliver the signal during `process_wait`.

**Expected:** The read is retried on EINTR, and any other error returns `Err`.

**Actual:** `Ok(ProcessOutput)` with stdout cut where the signal landed.

**Found by:** `~/.claude` qa-calibrate trial c4ens (Fixture 4 ensemble), 1 of 6 runs
(2026-09-30 172024, against `20a5a3e`); the `<= 0` test survives at `3ab4ec6`, re-read
2026-10-03 — not run.

---

## [ ] Bug 105: `compat.Dir.deleteTree` reports success when a delete fails, so `klar clean` never warns

**Status:** Open

**System:** platform layer — `src/compat.zig` (the Zig 0.16 file/dir/process shim)

**Deferred:** after the current milestone — `klar clean` still removes `build/` in the
ordinary case since Bug 69; this only hides a failure (a permission error, a file in use).

**Description:** `deleteTree` (`src/compat.zig:696-707`) discards each file's `deleteFile`
error (`catch {}`) and the final `unlinkat` result (`_ =`), so it returns success with the
tree still on disk. `cleanCommand` (`src/main.zig:6860`) has a "Warning: could not remove
build/" branch that cannot fire, and counts `build/` as removed. Bug 69 hid behind the
same silence: the wrong flag failed with ENOTDIR and nothing said so.

**Steps to reproduce:**
1. `mkdir -p build/sub && touch build/sub/f && chmod 555 build/sub` (so `f` cannot be unlinked).
2. `klar clean`.

**Expected:** `Warning: could not remove build/`, and `build/` not counted as removed.

**Actual:** No warning; `build/` is reported removed and is still there.

**Found by:** /qa-review on fix/bug-68-69-compat-flags, 2026-10-03 — GenA; verified by
reading `src/compat.zig:700-706` and `src/main.zig:6860-6863`.

---

## [ ] Bug 106: A second `let x: T = cell.get()` after `cell.set(v)` segfaults natively

**Status:** Open

**System:** Cell — the native `Cell.new` / `.get()` / `.set()` lowering in
`src/codegen/emit.zig`

**Description:** `test/native/cell_basic.kl` crashes with SIGSEGV (exit 139) and the native
runner reports it as passing, because the test has no `get_expected()` entry (Bug 7). The
crash needs a `let` bound from `get()` after a `set()`: returning `counter.get()` directly
works.

**Steps to reproduce:**
1. `let counter: Cell#[i32] = Cell.new(10)`, `let initial: i32 = counter.get()`,
   `counter.set(42)`, `let updated: i32 = counter.get()`, `return updated`.
2. `klar build` it and run the binary.

**Expected:** Exit 42.

**Actual:** Exit 139 (SIGSEGV). The same program with `return counter.get()` in place of the
second `let` exits 42 (probes `scratch/c/c2.kl`, `scratch/c/c3.kl`, macOS arm64).

**Found by:** PR 56 session's exit-code count from the `./run-tests.sh` output at `b9d81b2`,
2026-10-04; re-run individually and narrowed by the Bug 98 pick-up, 2026-10-04.

---

## [ ] Bug 107: A generic function with a `fn(T) -> T` parameter segfaults natively when it calls it

**Status:** Open

**System:** generic function-typed parameters — monomorphization of a generic function whose
parameter is a function type, in `src/codegen/emit.zig` (`declareMonomorphizedFunction` and
the indirect call through the parameter)

**Description:** `test/native/meta_pure_generic.kl` crashes with SIGSEGV (exit 139) and the
native runner reports it as passing (Bug 7). `meta pure` is not involved: the crash is
`apply#[T](f: fn(T) -> T, x: T)` calling `f(x)`. The test's other two generic functions,
`identity#[T]` and `add_generic#[T: Ordered]`, work.

**Steps to reproduce:**
1. `fn apply#[T](f: fn(T) -> T, x: T) -> T { return f(x) }`.
2. In `main`: `let inc: fn(i32) -> i32 = |x: i32| -> i32 { return x + 1 }`,
   `let e: i32 = apply#[i32](inc, 5)`, print `e`, return 0.
3. `klar build` it and run the binary.

**Expected:** Prints `6`, exits 0.

**Actual:** Exit 139 (SIGSEGV), nothing printed (probe `scratch/c/m2.kl`, macOS arm64).

**Found by:** PR 56 session's exit-code count from the `./run-tests.sh` output at `b9d81b2`,
2026-10-04; re-run individually and narrowed by the Bug 98 pick-up, 2026-10-04.

---

## [ ] Bug 108: Using a `String` after `.drop()` segfaults natively

**Status:** Open

**System:** String drop — the native `String.drop()` lowering in `src/codegen/emit.zig`
(the heap-header `String` representation)

**Description:** `test/native/string_drop.kl` crashes with SIGSEGV (exit 139) and the native
runner reports it as passing (Bug 7). The test, and the method's comment, say a dropped
`String` is reset to empty and can be pushed to again. `drop()` alone is safe; the next use
of the variable crashes, which is consistent with `drop()` freeing the heap header and
leaving the variable pointing at it.

**Steps to reproduce:**
1. `var s1: String = String.from("Drop me")`, `s1.drop()`, `println(s1.len().to_string())`,
   `return 0`.
2. `klar build` it and run the binary.

**Expected:** Prints `0`, exits 0.

**Actual:** Exit 139 (SIGSEGV). Dropping and returning without touching `s1` again exits 0
(probes `scratch/c/s.kl`, `scratch/c/s2.kl`, macOS arm64).

**Found by:** PR 56 session's exit-code count from the `./run-tests.sh` output at `b9d81b2`,
2026-10-04; re-run individually and narrowed by the Bug 98 pick-up, 2026-10-04.
