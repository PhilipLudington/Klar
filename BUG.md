# Klar Bug Reports

Bugs and limitations discovered while developing The Narrow Window.

---

## BUG-001: Array Element Mutation Causes Runtime TypeError

**Severity:** High
**Status:** Fixed
**Discovered:** Session 3
**Fixed:** Session 4

### Description

Assigning to an array element causes a runtime TypeError. The code compiles successfully but fails at runtime when executing the assignment.

### Reproduction

```klar
fn main() {
    var regions = [1, 2, 3]
    regions[0] = 10  // TypeError at runtime
}
```

### Expected Behavior

Array element assignment should update the value at the specified index.

### Actual Behavior (Before Fix)

```
Runtime error in main: TypeError
```

### Root Cause

Stack operand order mismatch between the compiler and VM for `op_set_index`. The compiler produced stack order `[value, index, container]` (bottom to top), but the VM was popping in the wrong order, causing it to treat the integer value as the container.

### Fix

Changed the pop order in `op_set_index` in `src/vm.zig` to match the stack order produced by the compiler:
- Pop container first (top of stack)
- Pop index second
- Pop value third (bottom)

---

## BUG-002: Let Declarations Inside While Loops Cause Parse Error

**Severity:** Medium
**Status:** Cannot Reproduce (Closed)
**Discovered:** Session 3
**Reviewed:** Session 4

### Description

Using `let` to declare a variable inside a `while` loop body was reported to cause a parse error "expected identifier".

### Reported Reproduction

```klar
fn main() {
    var arr = [1, 2, 3]
    var i = 0
    while i < 3 {
        let x = arr[i]  // Parse error: expected identifier
        println("{x}")
        i = i + 1
    }
}
```

### Current Status

This code now runs correctly and produces the expected output:
```
1
2
3
```

All tested variations of `let` declarations inside `while` loops work correctly:
- Basic `let` with literals
- `let` with array indexing
- `let` with complex expressions
- `let` with function calls
- Multiple `let` declarations in sequence
- Nested while loops with `let`

The bug may have been a transient issue or was fixed as a side effect of other parser changes.

---

## BUG-003: Comparing Loop Counter with .len() Type Mismatch

**Severity:** Low
**Status:** Fixed
**Discovered:** Session 3
**Fixed:** Session 4

### Description

Comparing an `i32` loop counter with the result of `.len()` causes a type error. The `.len()` method returns `usize` while integer literals default to `i32`.

### Reproduction

```klar
fn main() {
    let arr = [1, 2, 3]
    var i = 0
    while i < arr.len() {  // Type error: comparison operands must have same type
        i = i + 1
    }
}
```

### Expected Behavior

Comparison operators should allow comparing integers of different sizes.

### Actual Behavior (Before Fix)

```
Type error(s):
  X:Y: comparison operands must have same type
```

### Root Cause

The `.len()` method returned `usize` while integer literals default to `i32`. Since the type checker requires exact type equality for comparisons, `i < arr.len()` failed.

### Fix

Changed `.len()` to return `i32` instead of `usize` (`src/checker.zig` lines 2281, 2284). This is appropriate for Klar because:
- It's a VM-based language, not a systems language
- Arrays with 2+ billion elements are impractical in this context
- Ergonomic use with loop counters is more valuable than theoretical capacity
- Maintains consistency with requiring explicit type casts elsewhere

---

## Notes

### VM Memory Leak Warnings

The Klar VM produces memory leak warnings after program execution. These appear to be cosmetic and do not affect program correctness. Example:

```
error(gpa): memory address 0x... leaked:
???:?:?: 0x... in _vm_value.ObjOptional.createNone (???)
```

This is a known issue documented in PLAN.md and does not require a workaround.

### Fixed-Size Array Syntax Not Supported

The syntax `[T; N]` for fixed-size arrays is not yet implemented. Use dynamic array syntax `[1, 2, 3]` instead. This is a known limitation, not a bug.
