# Klar Compiler Bug Reports

## Environment

- **Klar Version**: 0.3.1-dev
- **Platform**: macOS (Darwin 24.6.0)
- **Date**: 2026-01-23
- **Status**: All known bugs fixed (2026-01-23)

## Fixed Issues (compiler-side)

- Arrays of structs - FIXED
- Enum variant construction (`Status::Active`) - FIXED
- Array element boolean field access - FIXED (2026-01-23)
- f64 constants passed as wrong type - FIXED (2026-01-23)
- Policy function codegen issues - FIXED (2026-01-23)
- String equality with readline() - FIXED (2026-01-23)

## Active Workarounds

None - all known bugs have been fixed.

The Narrow Window game now compiles and runs successfully with 131 declarations.

---

## Bug 1: Array Element Boolean Field Access - FIXED

### Summary

Accessing a boolean field of a struct via array indexing in a conditional expression passes type checking but fails at code generation with "UnsupportedFeature" error.

### Minimal Example

```klar
struct Item { flag: bool }

fn test(items: [Item; 3]) -> bool {
    if items[0].flag == false { return true }
    return false
}

fn main() {
    let items: [Item; 3] = [
        Item { flag: false },
        Item { flag: true },
        Item { flag: false }
    ]
    if test(items) {
        println("Works")
    } else {
        println("Broken")
    }
}
```

### Commands

```bash
$ klar check test.kl
All checks passed for 'test.kl'
  3 declaration(s)

$ klar run test.kl
Codegen error: UnsupportedFeature
```

### Variants That Fail

```klar
// Equality comparison
if items[0].flag == false { ... }

// Direct boolean use
if items[0].flag { ... }

// Not operator
if not items[0].flag { ... }
```

### Working Cases

```klar
// Extract to local variable first
fn test(items: [Item; 3]) -> bool {
    let first: Item = items[0]
    if first.flag == false { return true }
    return false
}

// Direct struct (not array element)
fn test(item: Item) -> bool {
    if item.flag == false { return true }
    return false
}

// Passing array element to function works
fn check(item: Item) -> bool { return item.flag }
fn test(items: [Item; 3]) -> bool { return check(items[0]) }
```

### Workaround

Extract the array element to a local variable before accessing boolean fields:

```klar
// Instead of:
if items[0].flag == false { ... }

// Use:
let item: Item = items[0]
if item.flag == false { ... }

// Or use a helper function:
fn is_flag_false(item: Item) -> bool {
    return item.flag == false
}
if is_flag_false(items[0]) { ... }
```

### Suggested Fix

The codegen phase should support boolean field access on struct values obtained via array indexing.

---

## Bug 2: Constants Passed to Functions as Wrong Type - FIXED

### Summary

Constants declared with f64 values (e.g., `const X: f64 = 0.70`) are passed to functions as `i32 0` instead of the correct floating-point value. This causes LLVM verification to fail with type mismatch errors.

### Minimal Example

```klar
const VULN: f64 = 0.70

fn process(value: f64) -> f64 {
    return value * 2.0
}

fn main() {
    let result: f64 = process(VULN)
    println("Result: {result}")
}
```

### Error Output

```
LLVM Module verification failed: Call parameter type does not match function signature!
i32 0
 double  %calltmp = call double @process(i32 0)
```

### Variants Tested

All fail:

```klar
// Direct constant use
const VALUE: f64 = 0.70
fn use_it(x: f64) { ... }
use_it(VALUE)  // FAILS - passes i32 0

// Explicit type annotation doesn't help
const VALUE: f64 = 0.70
let local: f64 = VALUE
use_it(local)  // FAILS - local is also i32

// Multiple constants
const A: f64 = 0.10
const B: f64 = 0.80
fn calc(a: f64, b: f64) { ... }
calc(A, B)  // FAILS
```

### Working Cases

```klar
// Inline literal values work
fn use_it(x: f64) { ... }
use_it(0.70)  // Works

// Local variables with literals work
let value: f64 = 0.70
use_it(value)  // Works
```

### Workaround

Replace constant references with inline literal values:

```klar
// Instead of:
const VULN_MANUFACTURING: f64 = 0.70
update_sector(data, VULN_MANUFACTURING)

// Use inline literals:
update_sector(data, 0.70)

// Or document the values in comments:
// Manufacturing: 0.70 | Transport: 0.80 | Retail: 0.60
update_sector(data, 0.70)  // Manufacturing vulnerability
```

### Suggested Fix

The codegen phase should correctly emit f64 constant values when used as function arguments, rather than emitting `i32 0`.

---

## Bug 3: Unknown Codegen Issue - FIXED

### Summary

After applying workarounds for Bugs 1 and 2, there is still at least one more codegen issue causing "UnsupportedFeature" errors.

### Status

- Narrowed down to lines 1695-1798 in the project's main.kl
- Functions in this range:
  - `display_single_policy`
  - `display_policies_for_region`
  - `check_policy_for_region`
  - `count_active_policies`
  - `get_policy_name_internal`
  - `get_policy_name`

### To Reproduce

See RESUME.md for binary search methodology.

---

## Bug 4: String Equality Fails with readline() Input - FIXED

### Summary

Strings returned from `readline()` do not compare equal to string literals using `==`, even when the content and length are identical. This appears to be an issue with the internal string representation from readline().

### Minimal Example

```klar
fn main() {
    println("Enter something:")
    let input: string = readline().trim()
    println("You entered: [{input}] len={input.len()}")

    if input == "1" {
        println("It's 1!")
    } else {
        println("Not 1")
    }
}
```

### Commands and Output

```bash
$ echo "1" | klar run test.kl
Enter something:
You entered: [1] len=1
Not 1
```

The input displays as "1" with length 1, but `input == "1"` evaluates to false.

### Variants Tested

All fail with readline() input:

```klar
// Direct comparison
if input == "1" { ... }  // FAILS

// With trim
let trimmed: string = input.trim()
if trimmed == "1" { ... }  // FAILS

// Assigned to variable
let expected: string = "1"
if input == expected { ... }  // FAILS
```

### Working Cases

```klar
// String literals compare correctly
let a: string = "hello"
let b: string = "hello"
if a == b { ... }  // WORKS

// String literal to literal
if "1" == "1" { ... }  // WORKS

// contains() works with readline input
if input.contains("1") { ... }  // WORKS

// len() works correctly
if input.len() == 1 { ... }  // WORKS
```

### Workaround

Use `contains()` combined with length check instead of equality:

```klar
// Instead of:
if input == "1" { ... }

// Use:
if input.len() == 1 and input.contains("1") { ... }

// For single-character menu input, just contains() is sufficient:
var choice: i32 = -1
if input.len() == 1 {
    if input.contains("1") { choice = 1 }
    if input.contains("2") { choice = 2 }
    if input.contains("3") { choice = 3 }
    // etc.
}
```

### Additional Notes

- The `starts_with()` method has a separate codegen bug (returns i32 instead of bool) so it cannot be used as a workaround
- String indexing (`input[0]`) is not supported in Klar
- The bug only affects strings from `readline()`, not string literals or other string operations

### Suggested Fix

Investigate why strings returned from `readline()` have a different internal representation that breaks equality comparison. Possible causes:
- Different string encoding or null termination
- Reference vs value comparison issue
- Memory allocation difference between runtime-created and compile-time strings

---

## Impact on Project

The Narrow Window game now compiles and runs successfully with full interactive gameplay. All known bugs have been fixed.

- **Bugs 1-3**: Fixed in Klar compiler update (2026-01-23)
- **Bug 4**: Fixed (2026-01-23) - string binary `==` now uses strcmp instead of pointer comparison

## Historical Notes

Bugs 1-3 were fixed in a Klar compiler update. The issues were:
- All bugs passed type checking (`klar check`) but failed at code generation (`klar run`)
- Error messages were unhelpful - just "UnsupportedFeature" with no indication of which feature
- Binary search on file contents was the only practical way to isolate issues

Bug 4 was caused by the `emitBinary()` function in codegen doing pointer comparison for strings instead of content comparison using strcmp. The fix adds detection for pointer types (strings) and calls strcmp for equality/inequality operators.
