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
| All 436 tests | Pass |
| Freestanding: bare_metal_target.kl | Pass (automated) |
| Freestanding: custom_entry.kl | Pass (automated) |
| Freestanding: linker_script.kl | Pass (automated, skipped if no cross-linker) |
