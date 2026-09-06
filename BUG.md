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

## [ ] Bug 15: GC — `allocObject` returns an unrooted object, so the caller's next `allocBytes` can collect and free it half-built

**Status:** Open

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

---

## [ ] Bug 16: GC — `markValue` treats `.future` as a primitive, so an async return payload's objects are collected while `await` still points at them

**Status:** Open

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

---

## [ ] Bug 17: VM — `trim`/`slice`/`substring` pop the receiver, then allocate from a slice borrowed out of it

**Status:** Open

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
