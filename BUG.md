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

## [ ] Bug 3: Duplicate Entry Point Logic in emit.zig

**File:** `src/codegen/emit.zig:929-943, 1413-1424`
**Severity:** Low (code quality)

The `func_name` block logic is duplicated between `declareFunction` and `emitFunctionBody`.

**Suggested fix:** Extract to a helper method:

```zig
fn getFunctionName(self: *Emitter, func: *const ast.Function, is_main_with_args: bool) []const u8 {
    if (self.freestanding) {
        if (std.mem.eql(u8, func.name, "main")) {
            return self.entry_point orelse "main";
        }
        return func.name;
    }
    return if (is_main_with_args) "_klar_user_main" else func.name;
}
```

---

## [ ] Bug 4: Missing --entry Flag Validation

**File:** `src/main.zig`
**Severity:** Low (UX improvement)

`--entry` is only meaningful with `--freestanding`, but no warning is issued if used alone.

**Suggested fix:** Add validation after flag parsing:

```zig
if (entry_point != null and !freestanding) {
    try stderr.writeAll("Warning: --entry has no effect without --freestanding\n");
}
```

---

## [ ] Bug 5: Freestanding Tests Not Integrated

**File:** `test/native/freestanding/`
**Severity:** Low (test coverage)

The tests in `test/native/freestanding/` are manual (comment-based instructions) and not integrated into the test runner.

**Suggested fix:** Either:
- Add to test runner with conditional skip if cross-toolchain unavailable
- Document explicitly why they are manual-only

---

# Suggestions (Non-blocking)

## [ ] Suggestion 1: Linker Script Validation

Check if the linker script file exists before invoking the linker to provide a better error message.

---

## [ ] Suggestion 2: Better -c Output Default

When using `-c` without `-o`, the object file stays in the temp directory. Consider defaulting to `<source>.o` in the current directory for better discoverability.

---

## [ ] Suggestion 3: Document Platform Method Relationship

`isFreestanding()` exists on both `TargetInfo` and `Platform`. The delegation relationship could be documented for clarity.

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

# Test Status

| Test | Status |
|------|--------|
| Existing 433 tests | Pass |
| `--target aarch64-none-elf --freestanding -c` | Pass |
| `--entry kernel_main` | Pass |
| Full linking with `aarch64-elf-ld` | Pass |
| Full linking with linker script | Pass |
