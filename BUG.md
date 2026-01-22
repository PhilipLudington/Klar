# Bug Report: Klar VM Struct Field Corruption

## Summary

The Klar VM corrupts struct fields after approximately 11-12 loop iterations. Fields that should contain `f64` values return `<object>` instead, causing a TypeError and crashing the program. This bug persists across multiple compiler versions and attempted fixes.

## Environment

- **Klar Version**: 0.3.1-dev (also tested with 0.3.0-dev)
- **Platform**: macOS (Darwin 24.6.0)
- **Project**: TheNarrowWindow

## Status

**FIXED** - Root cause found and fixed on 2026-01-22. See "Second Root Cause" below.

---

## Bug: Struct Field Corruption Across Loop Iterations

### Severity: Critical (Blocks all development)

### Description

Struct fields become corrupted after approximately 11-12 iterations of a game loop. Fields declared as `f64` return `<object>` instead of numeric values, causing type errors at runtime.

The program compiles and type-checks successfully. The corruption only manifests at runtime in the VM.

### Minimal Reproduction

```klar
struct Region {
    emissions: f64
    // ... other fields
}

var region: Region = create_region(...)

var turn: i32 = 0
loop {
    turn = turn + 1

    // Works for turns 1-11
    // Fails on turn 12 with: emissions = <object>
    let e: f64 = region.emissions

    if turn >= 200 {
        break
    }
}
```

### Actual Behavior

```
DEBUG: game.turn = 11
DEBUG: Emissions calculated: 31.50336790716223  // Valid f64

DEBUG: game.turn = 12
DEBUG: Emissions calculated: <object>           // CORRUPTED
Runtime error in main: TypeError
```

### Expected Behavior

The `emissions` field should return a valid `f64` value on all iterations.

### Test Results

| Klar Version | Turns Completed | Result |
|--------------|-----------------|--------|
| 0.3.0-dev | 11 | TypeError on turn 12 |
| 0.3.1-dev | 11 | TypeError on turn 12 |
| 0.3.1-dev (post-fix attempt) | 11 | TypeError on turn 12 |

### Observations

1. Corruption occurs consistently at turn 12 (after 11 successful iterations)
2. The corrupted value is `<object>` rather than garbage - suggests a type tag issue
3. Multiple struct instances are in use (8 regions, 7 sectors)
4. Native codegen is unavailable (UnsupportedFeature), so `--vm` flag is required
5. The issue may be related to:
   - VM garbage collection
   - Struct field offset calculation
   - Reference counting / ownership tracking
   - Stack frame management across loop iterations

### Workarounds Attempted

1. **Extract fields immediately** after function returns - no effect
2. **Copy structs** before accessing fields - no effect
3. **Reduce loop iterations** - works but defeats purpose of simulation

---

## Other Known Issues

### Bug 2: Optional Type Methods (High)

Calling `.is_some()` on `?T` types causes TypeError at runtime.

**Workaround**: Use integer return codes (0/1) instead of optional types.

### Bug 3: Native Codegen (Medium)

Native compilation fails with "UnsupportedFeature" for fixed-size arrays.

**Workaround**: Use `--vm` flag.

---

## Impact

Phase 3 of TheNarrowWindow is completely blocked. The game requires 200 turns minimum; currently only 11 complete successfully.

---

**Reported**: 2026-01-22
**Last Tested**: 2026-01-22
**Klar Version**: 0.3.1-dev
**Status**: **FIXED** (2026-01-22)

---

## Investigation Notes (2026-01-22)

### Tests Performed

Created multiple test cases in `scratch/` directory attempting to reproduce the bug:
- `gc_bug_test.kl` - Basic struct field access in loop (PASSES)
- `gc_bug_stress.kl` - Multiple structs, function returns, field updates (PASSES)
- `gc_bug_strings.kl` - Heavy string allocation to trigger GC faster (PASSES)
- `gc_bug_nested.kl` - Nested structs with object fields (PASSES)

All tests pass even with `stress_gc = true` (GC on every allocation).

### Findings

1. **VM struct method calls not supported** - Calling methods on structs (e.g., `point.sum()`) returns `TypeError` because `invokeMethod()` at `vm.zig:1319` falls through to the `else` branch for struct receivers.

2. **Optional.clone() not implemented** - `invokeOptionalMethod()` doesn't handle `clone()`, returning `UndefinedField`.

3. **GC marking appears correct** - Struct field values are properly marked during GC blackening (`gc.zig:424-430`).

4. **HashMap operations are safe** - `setField()` doesn't hold entry pointers across potential reallocations.

### Hypotheses

The bug may require specific conditions not reproduced in tests:
1. **Struct method calls** - If TheNarrowWindow uses `region.calculate()` type patterns, it would hit the unsupported struct method path
2. **Complex object graph** - The actual game may have circular references or deep nesting
3. **Specific allocation pattern** - The 11-12 iteration threshold suggests memory size triggers, but tests didn't reproduce

## ROOT CAUSE FOUND

### The Bug

**`compileLoopStmt` in `compiler.zig` was missing `beginScope()`/`endScope()` calls.**

Both `compileLoopStmt` (for `loop { }`) and `compileWhileLoop` (for `while condition { }`) were missing `beginScope()`/`endScope()` calls. Only `compileForLoop` properly managed scope. This meant:

1. Variables declared inside loop bodies never went out of scope
2. Each iteration accumulated more local variables
3. `local_count` grew unbounded across iterations
4. After ~11-12 iterations with 8+ locals per iteration, slot indices became corrupted

### The Fix

Added scope management to both `compileLoopStmt` and `compileWhileLoop`:

```zig
// compileLoopStmt fix:
fn compileLoopStmt(self: *Compiler, l: *ast.LoopStmt) Error!void {
    self.beginScope();  // Added
    // ... body compilation ...
    self.endScope();    // Added
    try self.emitLoop(loop_start, line);
}

// compileWhileLoop fix:
fn compileWhileLoop(self: *Compiler, w: *ast.WhileLoop) Error!void {
    // ... condition evaluation ...
    self.beginScope();  // Added
    try self.compileBlockStatements(w.body);
    self.endScope();    // Added
    try self.emitLoop(loop_start, line);
}
```

### Why The Symptoms Were Confusing

- Field access for string interpolation worked because it read the correct value
- But arithmetic operations failed because they read from WRONG stack slots
- The "corruption" was actually the VM reading from misaligned slot indices
- Values at wrong slots contained structs (hence `<object>`) or booleans (hence `true`)

### Other Issues Found (Unrelated)

1. **VM struct method calls not supported** - `invokeMethod()` doesn't handle `.struct_` receivers
2. **Optional.clone() not implemented** - causes `UndefinedField` error

## SECOND ROOT CAUSE FOUND (2026-01-22)

### The Bug

**`compileIfStmt` was not popping the value left on the stack by `compileBlock`.**

The `compileBlock` function always leaves a value on the stack:
- If the block has a `final_expr` (last expression without trailing newline/semicolon), that expression's value is left on the stack
- Otherwise, `op_void` is emitted, leaving `void` on the stack

The parser treats the last expression in a block as `final_expr` if it's immediately followed by `}` without a newline. For example:

```klar
if game.month > 12 {
    game.month = 1
    game.year = game.year + 1  // No newline before }
}
```

Here, `game.year = game.year + 1` is parsed as `final_expr`, not a statement. This means:
1. The assignment compiles via `compileExpr`, not `compileExprStmt`
2. No `op_pop` is emitted after the assignment
3. The result (the `game` object) is left on the stack

### Why This Caused Corruption

Every time the `if` branch executed, an extra value was left on the stack. This shifted all subsequent local variable slot indices:
- Variables at slot N were now at stack position N+1
- The compiler's bytecode used slot indices calculated at compile time
- At runtime, the misaligned slots caused reads from wrong positions
- Struct fields (like `r0.emissions`) were read as objects or other types

### The Fix

Added `op_pop` after `compileBlock` in both the then-branch and else-branch of `compileIfStmt`:

```zig
// Compile then branch
self.beginScope();
try self.compileBlock(if_stmt.then_branch);
self.endScope();
try self.emitOp(.op_pop, line);  // Added: pop the block's result value
```

### Why The Previous Fix Didn't Work

The previous fix (adding `beginScope()`/`endScope()` to `compileLoopStmt` and `compileWhileLoop`) was correct but incomplete. It fixed one stack leak but not the one from `compileIfStmt`.

The bug was only triggered when:
1. An `if` statement inside a loop had a final expression (no newline before `}`)
2. The condition was initially false for several iterations
3. When the condition first became true, the leaked value caused slot misalignment
4. On subsequent iterations, the misaligned slots caused the corruption
