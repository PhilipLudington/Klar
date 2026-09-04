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
