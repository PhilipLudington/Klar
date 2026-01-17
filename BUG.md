# Known Bugs

## BUG-001: Ownership Checker Segfault on Generic Functions

**Status:** Open
**Severity:** High
**Discovered:** 2026-01-16
**Component:** Ownership Checker (`src/ownership/`)

### Description

The ownership checker crashes with a segmentation fault when analyzing certain generic functions. The crash occurs in `invalidateBorrowsAtDepth` when accessing `self.variables.capacity()` on an uninitialized or corrupted hash map.

### Reproduction

```klar
trait Displayable {
    fn display(self: Self) -> string
}

struct Point {
    x: i32,
    y: i32,
}

fn show[T: Displayable](item: T) -> i32 {
    42
}

fn main() -> i32 {
    let p = Point { x: 1, y: 2 }
    show(p)
}
```

Run with:
```bash
klar check /path/to/file.kl
```

### Stack Trace

```
Segmentation fault at address 0x10113fff8
/opt/homebrew/Cellar/zig/0.15.2/lib/zig/std/hash_map.zig:798:33: in capacity
            return self.header().capacity;
                                ^
/Users/mrphil/Fun/Klar/src/ownership/state.zig:318:36: in invalidateBorrowsAtDepth
        if (self.variables.capacity() == 0) return;
                                   ^
/Users/mrphil/Fun/Klar/src/ownership/checker.zig:501:39: in popScope
            s.invalidateBorrowsAtDepth(depth);
                                      ^
/Users/mrphil/Fun/Klar/src/ownership/checker.zig:253:28: in analyzeBlock
        defer self.popScope();
                           ^
/Users/mrphil/Fun/Klar/src/ownership/checker.zig:122:34: in analyzeFunction
            try self.analyzeBlock(body);
                                 ^
/Users/mrphil/Fun/Klar/src/ownership/checker.zig:103:54: in analyzeDecl
            .function => |f| try self.analyzeFunction(f),
                                                     ^
/Users/mrphil/Fun/Klar/src/ownership/checker.zig:86:33: in analyze
```

### Analysis

The crash appears to be caused by:
1. The ownership checker's `OwnershipState` struct has a `variables` hash map
2. When `popScope` is called, it calls `invalidateBorrowsAtDepth`
3. The check `self.variables.capacity() == 0` crashes because the hash map header is invalid

Likely root causes:
- The `OwnershipState` may not be properly initialized for generic functions
- The state may be corrupted during analysis of generic function bodies
- There may be a use-after-free or double-free of the state

### Workaround

Use `klar build` instead of `klar check` - the native compilation path may not trigger the same code path. Alternatively, avoid running the type checker on files with generic functions that have trait bounds until this is fixed.

### Related Files

- `src/ownership/state.zig:318` - Crash location
- `src/ownership/checker.zig:501` - `popScope` caller
- `src/ownership/checker.zig:122` - `analyzeFunction`

### Notes

- This bug is **not related** to the trait system implementation
- The bug exists in the ownership checker infrastructure
- All unit tests (195) and native tests (60) pass, suggesting the bug is triggered by specific code patterns
- The ownership checker may need review for proper initialization of generic function analysis

---

## BUG-002: Memory Leaks in Error Paths

**Status:** Open
**Severity:** Low
**Discovered:** 2026-01-16
**Component:** Type Checker (`src/checker.zig`)

### Description

When type checking fails with errors, allocated error messages and some intermediate allocations are leaked. This is visible when running with the GPA (General Purpose Allocator) in debug mode.

### Example Output

```
Type check failed with 1 error(s):
  14:1 [trait_not_implemented]: missing implementation for required trait method 'to_string'
error(gpa): memory address 0x105080140 leaked:
/Users/mrphil/Fun/Klar/src/checker.zig:619:43: in addError__anon_38384
        const message = std.fmt.allocPrint(self.allocator, fmt, args) catch "error formatting message";
```

### Analysis

The `addError` function allocates error message strings but these are stored in an `ArrayList` and freed in `deinit`. However, if the checker errors out early or certain code paths are taken, some allocations may be missed.

Additionally, `pushTypeParams` allocates type variable slices that may not be freed if an error occurs before `popTypeParams` is called.

### Workaround

None needed for correctness - this only affects memory usage in error cases. The program will still produce correct error messages.

### Priority

Low - only affects error cases and development/debugging. Production use with successful compilation is unaffected.
