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
