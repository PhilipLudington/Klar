# CarbideZig Validation Check

Run validation tooling to check code quality and correctness.

## Arguments

- `$ARGUMENTS` - Optional: specific check to run (build, test, fmt, all)

## Instructions

Execute the following validation steps:

### 1. Format Check
```bash
zig fmt --check src/
```

If formatting issues found:
- List files that need formatting
- Suggest running `zig fmt src/` to fix

### 2. Build Check
```bash
zig build
```

If build fails:
- Show compiler errors
- Provide guidance on fixing common issues
- Check for unused variables, unreachable code warnings

### 3. Test Suite
```bash
zig build test
```

If tests fail:
- Show failing test names and error messages
- Provide debugging suggestions
- Check for memory leaks (std.testing.allocator reports these)

### 4. Additional Checks

If the project has a `check` step defined:
```bash
zig build check
```

## Output Format

```
## CarbideZig Validation Results

### Format Check
✓ All files properly formatted
  OR
✗ N files need formatting:
  - src/file.zig

### Build Check
✓ Build successful
  OR
✗ Build failed:
  [error details]

### Test Suite
✓ All N tests passed
  OR
✗ M/N tests failed:
  - test name: error message

### Summary
[PASS] All checks passed
  OR
[FAIL] X check(s) failed - see details above
```

## Quick Commands

- `zig build` - Build project
- `zig build test` - Run tests
- `zig fmt src/` - Format code
- `zig fmt --check src/` - Check formatting
