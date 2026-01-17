# Klar Bug Reports

Bugs and limitations discovered while developing The Narrow Window.

---

## BUG-001: Array Element Mutation Causes Runtime TypeError

**Severity:** High
**Status:** Open
**Discovered:** Session 3

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

### Actual Behavior

```
Runtime error in main: TypeError
```

### Workaround

Use individual variables instead of an array when mutation is needed:

```klar
var r0 = create_region(0)
var r1 = create_region(1)
// ... update individually
r0 = update_region(r0)
r1 = update_region(r1)
```

---

## BUG-002: Let Declarations Inside While Loops Cause Parse Error

**Severity:** Medium
**Status:** Open
**Discovered:** Session 3

### Description

Using `let` to declare a variable inside a `while` loop body causes a parse error "expected identifier".

### Reproduction

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

### Expected Behavior

`let` declarations should work inside loop bodies, creating block-scoped variables.

### Actual Behavior

```
Parse error: error.ExpectedIdentifier
  X:Y: expected identifier
```

### Workaround

Avoid `let` inside loops. Either:
1. Unroll the loop with explicit indexing
2. Declare `var` outside the loop and reassign

```klar
// Option 1: Unroll
println("{arr[0]}")
println("{arr[1]}")
println("{arr[2]}")

// Option 2: var outside loop
var x = 0
var i = 0
while i < 3 {
    x = arr[i]
    println("{x}")
    i = i + 1
}
```

---

## BUG-003: Comparing Loop Counter with .len() Type Mismatch

**Severity:** Low
**Status:** Open
**Discovered:** Session 3

### Description

Comparing an `i32` loop counter with the result of `.len()` causes a type error. The `.len()` method appears to return a different integer type.

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

`.len()` should return a type compatible with `i32` for comparison, or there should be implicit conversion.

### Actual Behavior

```
Type error(s):
  X:Y: comparison operands must have same type
```

### Workaround

Use a constant for the known array length instead of calling `.len()`:

```klar
const NUM_ITEMS = 3

fn main() {
    let arr = [1, 2, 3]
    var i = 0
    while i < NUM_ITEMS {
        i = i + 1
    }
}
```

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
