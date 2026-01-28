# Klar Compiler Bugs

Bugs discovered while implementing the JSON parser for Klar.

---

## [x] Bug 1: Inline method call in struct return causes LLVM type mismatch

**Status:** Fixed

**Description:** When a function returns a struct type and the return statement calls a method inline, LLVM codegen produces mismatched types causing verification failure.

**Steps to reproduce:**
1. Create a struct `ParseError` with fields
2. Create a struct `State` with an `error()` method that returns `ParseError`
3. Create a function that returns `ParseError` and calls `state.error()` inline

```klar
struct ParseError { message: string, line: i32, column: i32 }
struct State { line: i32, column: i32 }

impl State {
    fn error(self: State, msg: string) -> ParseError {
        return ParseError { message: msg, line: self.line, column: self.column }
    }
}

fn parser_error(state: State, message: string) -> ParseError {
    return state.error(message)  // BUG: causes LLVM verification failure
}
```

**Expected:** Function compiles and returns the ParseError correctly.

**Actual:** LLVM Module verification failed:
```
Function return type does not match operand type of return inst!
  ret i32 0
 { ptr, i32, i32 }
```

**Workaround:** Use a local variable:
```klar
fn parser_error(state: State, message: string) -> ParseError {
    let err: ParseError = state.error(message)
    return err
}
```

---

## [x] Bug 2: Inline method call in Err() wrapper causes LLVM type mismatch

**Status:** Fixed (by Bug 1 fix)

**Description:** When returning `Err(method())` in a function that returns `Result[T, E]`, the LLVM codegen produces incorrect types.

**Steps to reproduce:**
1. Create a function returning `Result[JsonValue, ParseError]`
2. In a match arm, return `Err(state.method("message"))`

```klar
fn parse(source: string) -> Result[JsonValue, ParseError] {
    // ... setup ...
    match ps.current {
        Token.Null => { return Ok(JsonValue::Null) }
        _ => { return Err(ps.lexer_state.error("Unexpected")) }  // BUG
    }
}
```

**Expected:** Function compiles and returns the error correctly.

**Actual:** LLVM Module verification failed:
```
Function return type does not match operand type of return inst!
  ret { i1, { i8, [20 x i8] }, i32 } %err.result
 { i1, { i8, [20 x i8] }, { ptr, i32, i32 } }
```

**Workaround:** Use a local variable:
```klar
_ => {
    let err: ParseError = ps.lexer_state.error("Unexpected")
    return Err(err)
}
```

---

## [x] Bug 3: Method call on enum in if condition causes LLVM branch type error

**Status:** Fixed (by Bug 1 fix)

**Description:** Calling a method on an enum value inside an `if` condition causes LLVM to generate an `i32` instead of `i1` for the branch condition.

**Steps to reproduce:**
1. Define an enum with an `impl` block containing a method returning `bool`
2. Use that method in an `if` condition

```klar
enum JsonValue {
    Null,
    Bool(bool),
    // ...
}

impl JsonValue {
    fn is_null(self: JsonValue) -> bool {
        match self {
            JsonValue.Null => { return true }
            _ => { return false }
        }
    }
}

fn test(v: JsonValue) {
    if v.is_null() {  // BUG: branch condition is i32, not i1
        println("null")
    }
}
```

**Expected:** Code compiles and the if condition works correctly.

**Actual:** LLVM Module verification failed:
```
Branch condition is not 'i1' type!
  br i32 0, label %then, label %else
```

**Workaround:** Use match instead of method call:
```klar
fn test(v: JsonValue) {
    match v {
        JsonValue.Null => { println("null") }
        _ => { }
    }
}
```

---

## [x] Bug 4: Runtime segfault when extracting string from enum variant

**Status:** Fixed (likely by Bug 1 fix or prior changes)

**Description:** Extracting a string value from an enum variant (e.g., `Token.Str(s)`) and using it causes a segmentation fault at runtime. The code type-checks correctly but crashes when executed.

**Steps to reproduce:**
1. Define an enum with a string variant: `Token { Str(string), ... }`
2. Match on the enum and extract the string
3. Use the extracted string in any way

```klar
enum Token {
    Str(string),
    // ...
}

fn test_string() {
    let tok: Token = Token::Str("hello")
    match tok {
        Token.Str(s) => {
            if s == "hello" {  // SEGFAULT here
                println("matched")
            }
        }
        _ => { }
    }
}
```

**Expected:** String comparison works and prints "matched".

**Actual:** Process terminated by signal: 11 (SIGSEGV)

**Workaround:** None known. Avoid string extraction from enum variants.

---

## [x] Bug 5: Runtime segfault with List/Map containing recursive enum types

**Status:** Fixed (likely by Bug 1-4 fixes)

**Description:** Creating and using `List[JsonValue]` or `Map[string, JsonValue]` where `JsonValue` is a recursive enum causes segmentation faults at runtime.

**Steps to reproduce:**
1. Define a recursive enum: `JsonValue { Array(List[JsonValue]), ... }`
2. Parse JSON that creates a `List[JsonValue]`
3. Access the list

```klar
enum JsonValue {
    Null,
    Array(List[JsonValue]),
    Object(Map[string, JsonValue]),
}

fn test_array() {
    let result: Result[JsonValue, ParseError] = parse("[]")
    match result {
        Ok(v) => {
            match v {
                JsonValue.Array(arr) => {
                    if arr.len() == 0 {  // SEGFAULT
                        println("empty")
                    }
                }
                _ => { }
            }
        }
        Err(_) => { }
    }
}
```

**Expected:** Empty array is parsed and length check succeeds.

**Actual:** Process terminated by signal: 11 (SIGSEGV)

**Workaround:** None known. Primitive types (null, bool, number) work; complex types (string, array, object) cause crashes.

---

## [x] Bug 6: Cross-file imports not working

**Status:** Fixed

**Description:** Import statements between files in a project don't resolve correctly. The module system documented in Klar doesn't function as expected.

**Steps to reproduce:**
1. Create `lib/json/lexer.kl` with `pub` exports
2. Create `lib/json/parser.kl` with `import lexer.{ Token, ... }`
3. Run `klar check parser.kl`

```klar
// parser.kl
import lexer.{ Token, LexerState, lexer_new }  // BUG: module not found
```

**Expected:** Imports resolve and types are available.

**Actual:** `[undefined_module]: module 'lexer' not found`

**Fix:** Added module resolution support to `checkFile()`, `runVmFile()`, and `runInterpreterFile()` in `main.zig`. These functions now use the same multi-file compilation pipeline as `buildNative()` when the entry module has imports.

---

## Summary

| Bug | Type | Severity | Workaround |
|-----|------|----------|------------|
| 1 | Codegen | High | **FIXED** |
| 2 | Codegen | High | **FIXED** (by Bug 1 fix) |
| 3 | Codegen | High | **FIXED** (by Bug 1 fix) |
| 4 | Runtime | Critical | **FIXED** |
| 5 | Runtime | Critical | **FIXED** |
| 6 | Module System | High | **FIXED** |

All bugs resolved. Multi-file JSON parser development is now unblocked.

---

# PR #3 Review: Bare-Metal/Freestanding Compilation Support

Issues and suggestions from code review of the BareMetal branch.

---

## [x] Bug 1: Code Duplication in linker.zig

**File:** `src/codegen/linker.zig`
**Severity:** Medium (code quality)

`linkBareMetalTarget` and `linkBareMetalWithGenericLd` were nearly identical (~150 lines duplicated).

**Fix:** Refactored into `linkBareMetalTarget` (tries multiple linker names) and `tryLinkBareMetal` (single attempt). Removed ~70 lines of duplication.

---

## [x] Bug 2: Unused Process Pipes in linker.zig

**File:** `src/codegen/linker.zig`
**Severity:** Medium (potential deadlock)

Pipes were set but never read, risking deadlock if linker produced output.

**Fix:** Changed to `.Inherit` so linker errors are visible to user.

---

## [x] Bug 3: Duplicate Entry Point Logic in emit.zig

**File:** `src/codegen/emit.zig:929-943, 1413-1424`
**Severity:** Low (code quality)

The `func_name` block logic was duplicated between `declareFunction` and `emitFunctionBody`.

**Fix:** Extracted to `getFunctionName` helper method at line 866. Removed ~20 lines of duplication.

---

## [x] Bug 4: Missing --entry Flag Validation

**File:** `src/main.zig`
**Severity:** Low (UX improvement)

`--entry` is only meaningful with `--freestanding`, but no warning was issued if used alone.

**Fix:** Added validation after flag parsing to warn the user.

---

## [x] Bug 5: Freestanding Tests Not Integrated

**File:** `test/native/freestanding/`
**Severity:** Low (test coverage)

The tests in `test/native/freestanding/` were manual (comment-based instructions) and not integrated into the test runner.

**Fix:** Created `scripts/run-freestanding-tests.sh` that:
- Runs compilation-only tests (work without cross-linker)
- Conditionally runs linking tests if cross-linker is available
- Integrated into `run-tests.sh` with JSON results output

---

## [x] Suggestion 1: Linker Script Validation

**File:** `src/codegen/linker.zig`

Added `LinkerScriptNotFound` error and validation in `tryLinkBareMetal` to check if the linker script file exists before invoking the linker. Now provides clear error message:
```
error: linker script not found: <path>
```

---

## [x] Suggestion 2: Better -c Output Default

**Status:** Already working as expected.

When using `-c` without `-o`, the object file is output as `<source>.o` in the current working directory. This matches standard compiler behavior (gcc/clang):
```
klar build path/to/foo.kl -c  ->  foo.o (in current directory)
```

---

## [x] Suggestion 3: Document Platform Method Relationship

**File:** `src/codegen/target.zig`

Added documentation to all three `isFreestanding()` methods explaining:
- `Os.isFreestanding()` - Canonical check for parsed target triples (checks `.none`)
- `TargetInfo.isFreestanding()` - Delegates to `Os.isFreestanding()`
- `Platform.isFreestanding()` - Uses `std.Target.Os.Tag.freestanding` for LLVM/linker ops

The docs clarify that `TargetInfo` uses custom enums for parsing user input, while `Platform` uses `std.Target` types for LLVM integration.

---

## [x] Bug 6: Wrong Cross-Linker Name

**File:** `src/codegen/linker.zig`
**Severity:** High (linker not found)

The code used `aarch64-none-elf-ld` but Homebrew installs `aarch64-elf-ld`.

**Fix:** Now tries multiple names in order: `aarch64-elf-ld`, `aarch64-none-elf-ld`, `ld`.

---

## [x] Bug 7: Output Directory Not Created for Explicit -o Path

**File:** `src/main.zig`
**Severity:** Medium (linker fails with "No such file or directory")

When using `-o build/foo`, the `build/` directory was only created for default output paths.

**Fix:** Added parent directory creation for explicit `-o` paths before linking.

---

## PR #3 Test Status

| Test | Status |
|------|--------|
| All 436 tests | Pass |
| Freestanding: bare_metal_target.kl | Pass (automated) |
| Freestanding: custom_entry.kl | Pass (automated) |
| Freestanding: linker_script.kl | Pass (automated, skipped if no cross-linker) |

---

# PR #2 Review: Add Path type and filesystem operations

Issues identified during code review of PR #2.

---

## [x] Bug 1: Platform-specific hardcoded struct offsets are fragile

**File:** `src/codegen/emit.zig:1241`, `1299`, `1981`

**Description:** The `st_mode` offset in the stat struct and `d_name` offset in dirent are hardcoded based on OS:

```zig
const mode_offset: u32 = if (builtin.os.tag == .macos) 4 else 24;
const d_name_offset: u32 = if (builtin.os.tag == .macos) 21 else 19;
```

This is fragile and may break on:
- Different Linux architectures (32-bit vs 64-bit)
- musl libc vs glibc
- Other Unix variants (FreeBSD, OpenBSD)

**Suggested Fix:** Use Zig's `@cImport` to get proper struct definitions from system headers, or define platform-specific constants more carefully with architecture checks.

**Severity:** Medium

---

## [x] Bug 2: Path.join() doesn't handle trailing slashes

**File:** `src/codegen/emit.zig:21109-21169`

**Description:** The `Path.join()` implementation always adds a `/` separator between the base path and the component being joined. This can lead to double slashes:

```klar
let p: Path = Path.new("/home/user/")
let p2: Path = p.join("documents")
// Result: "/home/user//documents" (double slash)
```

**Suggested Fix:** Check if the base path already ends with `/` before adding the separator, or normalize paths after joining.

**Resolution:** Updated `emitPathJoin` to check if the path already ends with `/` before adding a separator. The fix:
1. Loads the last character of the path
2. Compares it to '/'
3. Uses LLVM select to conditionally determine if separator is needed
4. Adjusts buffer allocation and copy positions accordingly

Added test cases in `test/native/fs/path_basic.kl` for:
- Path with trailing slash joined with component
- Root path "/" joined with component

**Severity:** Low

---

## [x] Bug 3: Memory ownership unclear for fs_read_dir results

**File:** `src/codegen/emit.zig:1893-2089`

**Description:** The `fs_read_dir` function returns a `Result[List[String], IoError]` where each String in the list is heap-allocated. The ownership semantics are not documented:
- Who is responsible for freeing each String?
- Who is responsible for freeing the List's backing array?
- Does Klar's ownership system handle this automatically?

**Suggested Fix:** Document ownership clearly in PLAN.md or add automatic cleanup via Drop trait implementation for List[String].

**Resolution:** Documented ownership semantics in:
- `docs/types/io-types.md` - Added Filesystem Functions section with clear ownership note for `fs_read_dir`
- `docs/types/collections.md` - Clarified that `drop()` only frees backing array, not nested heap allocations

**Severity:** Medium

---

## [x] Bug 4: Missing test coverage for fs_create_dir_all with deep nesting

**File:** `test/native/fs/`

**Description:** The `fs_create_dir_all` function has complex logic for creating nested directories, but there's no test case for deeply nested paths like `/tmp/a/b/c/d/e`.

**Suggested Fix:** Add a test case:
```klar
let deep_path: string = "/tmp/klar_test/a/b/c/d/e"
let result: Result[void, IoError] = fs_create_dir_all(deep_path)
// Verify all intermediate directories were created
```

**Resolution:** Added `test/native/fs/fs_create_dir_all.kl` with comprehensive tests for:
- Creating a deeply nested path `/tmp/klar_test_deep/a/b/c/d/e`
- Verifying all intermediate directories (`a`, `a/b`, `a/b/c`, `a/b/c/d`) are created
- Creating an already-existing directory (should succeed - idempotent)
- Creating a path with trailing slash `/tmp/klar_test_deep/x/y/z/`
- Proper cleanup of all created directories

Also fixed a bug in `emitFsCreateDirAll` in `src/codegen/emit.zig` where basic blocks were created inline and then incorrectly looked up via `LLVMGetPreviousBasicBlock`. The fix creates all basic blocks upfront and references them directly.

**Severity:** Low

---

## [x] Bug 5: Magic numbers for file mode constants

**File:** `src/codegen/emit.zig:1257-1258`, `1313-1315`

**Description:** File type constants are used as magic numbers:
```zig
// S_IFMT = 0xF000, S_IFREG = 0x8000
const masked = llvm.c.LLVMBuildAnd(self.builder.ref, mode_i32, llvm.Const.int32(self.ctx, 0xF000), "mode.masked");
const is_file = self.builder.buildICmp(llvm.c.LLVMIntEQ, masked, llvm.Const.int32(self.ctx, 0x8000), "isfile.check");
```

**Suggested Fix:** Define named constants at the top of the file:
```zig
const S_IFMT: u32 = 0xF000;
const S_IFREG: u32 = 0x8000;
const S_IFDIR: u32 = 0x4000;
```

**Resolution:** Named constants are already defined at the top of `src/codegen/emit.zig` (lines 41-44):
```zig
const S_IFMT: u32 = 0o170000; // File type mask
const S_IFREG: u32 = 0o100000; // Regular file
const S_IFDIR: u32 = 0o040000; // Directory
```
These constants are used in `emitStatIsFile` and `emitStatIsDir`. The octal notation (`0o170000`) is equivalent to hex (`0xF000`) but is more consistent with POSIX documentation.

**Severity:** Low (code quality)

---

## [x] Bug 6: Code duplication in emitStatIsFile and emitStatIsDir

**File:** `src/codegen/emit.zig:1213-1330`

**Description:** `emitStatIsFile` and `emitStatIsDir` share ~90% identical code. Only the final comparison value differs (0x8000 vs 0x4000).

**Suggested Fix:** Extract a common helper function:
```zig
fn emitStatModeCheck(self: *Emitter, path_val: llvm.ValueRef, mode_mask: u32) EmitError!llvm.ValueRef
```

**Resolution:** Extracted common helper `emitStatModeCheck` that takes comptime parameters for the file type mask and label prefix:
```zig
fn emitStatModeCheck(self: *Emitter, path_val: llvm.ValueRef, comptime file_type_mask: u32, comptime label_prefix: []const u8) EmitError!llvm.ValueRef
```
The original functions are now one-liners that delegate to this helper:
- `emitStatIsFile` calls `emitStatModeCheck(path_val, S_IFREG, "isfile")`
- `emitStatIsDir` calls `emitStatModeCheck(path_val, S_IFDIR, "isdir")`

This reduces ~120 lines of duplicated code to ~60 lines of shared code plus 2 one-liner wrappers.

**Severity:** Low (code quality)

---

## [x] Bug 7: No error case tests for filesystem functions

**File:** `test/native/fs/`

**Description:** All test cases focus on the happy path. There are no tests for error conditions:
- `fs_remove_dir` on a non-empty directory
- `fs_remove_file` on a non-existent file
- `fs_create_dir` on an existing directory
- `fs_read_string` on a file without read permissions
- `fs_write_string` to a read-only directory

**Suggested Fix:** Add error case tests that verify the correct `IoError` variant is returned.

**Resolution:** Added `test/native/fs/fs_errors.kl` with tests for:
- `fs_remove_file` on non-existent file (returns error)
- `fs_remove_dir` on non-existent directory (returns error)
- `fs_create_dir` on existing directory (returns error)
- `fs_remove_dir` on non-empty directory (returns error)
- `fs_read_string` on non-existent file (returns error)
- `fs_write_string` to non-existent parent directory (returns error)
- `fs_read_dir` on non-existent directory (returns error)
- `fs_read_dir` on a file (not directory) (returns error)

Also updated `scripts/run-native-tests.sh` to include tests from subdirectories.

**Severity:** Medium

---

## [x] Bug 8: Windows support not documented as unsupported

**File:** `PLAN.md`, `docs/`

**Description:** The filesystem implementation uses POSIX syscalls exclusively (stat, mkdir, unlink, rmdir, opendir, readdir, closedir). Windows is not supported, but this limitation is not documented.

**Suggested Fix:** Add a note to PLAN.md Milestone 5 section:
> Note: Filesystem operations currently support macOS and Linux only. Windows support is not implemented.

**Resolution:** Platform support is now documented in two places:
- `PLAN.md` Milestone 5 section: Added "Platform Support" subsection with note about POSIX-only implementation
- `docs/types/io-types.md` line 375: Already had a note stating "Filesystem operations currently support macOS and Linux only. Windows is not implemented."

**Severity:** Low (documentation)

---

## [x] Bug 9: errno values are Linux-specific in emitErrnoToIoError

**File:** `src/codegen/emit.zig:1361-1436`

**Description:** The errno-to-IoError mapping uses hardcoded errno values:
```zig
// ENOENT=2, EACCES=13, EEXIST=17, EINVAL=22
```

While these values are consistent across most Unix systems, they should ideally come from system headers to ensure portability.

**Suggested Fix:** Consider using `@cImport` to get errno constants, or document that these are POSIX-standard values.

**Resolution:** Used `@cImport` to get errno values from system headers:
1. Added `@cInclude("errno.h")` to the existing posix `@cImport` block
2. Defined named constants at module level:
   ```zig
   const ENOENT: i32 = posix.ENOENT; // No such file or directory
   const EACCES: i32 = posix.EACCES; // Permission denied
   const EEXIST: i32 = posix.EEXIST; // File exists
   const EINVAL: i32 = posix.EINVAL; // Invalid argument
   ```
3. Updated `emitErrnoToIoError` and `emitFsCreateDirAll` to use these named constants instead of magic numbers

This ensures correct errno values on all POSIX platforms (macOS, Linux, *BSD).

**Severity:** Low

---

## [x] Bug 10: Path.parent() returns None for paths without separator

**File:** `src/codegen/emit.zig:802-942`

**Description:** For a path like `"filename.txt"` (no directory component), `parent()` returns `None`. This might be unexpected - some users might expect `"."` (current directory) to be returned.

**Suggested Fix:** Document the behavior clearly, or consider returning `Some(Path.new("."))` for relative paths without a parent.

**Resolution:** Documented the behavior clearly in two places:

1. **PLAN.md** - Added detailed note with examples showing `.parent()` behavior for different path types
2. **docs/types/io-types.md** - Added comprehensive "Path Type" section with:
   - Method reference table
   - "Edge Cases for `.parent()`" table showing all scenarios
   - Explicit note explaining that Klar returns `None` (not `"."`) to make it explicit there's no parent

The current behavior (returning `None`) is intentional as it makes the absence of a parent explicit, requiring pattern matching to handle the case.

**Severity:** Low (design decision)

---

## PR #2 Summary

| Severity | Count | Fixed |
|----------|-------|-------|
| Medium   | 3     | 3     |
| Low      | 7     | 7     |

**Total Issues:** 10 (10 fixed, 0 remaining)

**All issues from PR #2 code review have been addressed.**
