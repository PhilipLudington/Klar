# Klar Compiler Bug Reports

## Environment

- **Klar Version**: 0.3.1-dev
- **Platform**: macOS (Darwin 24.6.0)
- **Date**: 2026-01-23

## Previously Fixed Issues

- Arrays of structs - FIXED
- Enum variant construction (`Status::Active`) - FIXED
- Constants passed to functions as wrong type (f64 constants emitted as `i32 0`) - FIXED

---

## Bug 1: Array Element Boolean Field Access

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

## Bug 2: Unknown Codegen Issue (Under Investigation)

### Summary

There is at least one more codegen issue causing "UnsupportedFeature" errors.

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

## Impact on Project

The Narrow Window game passes type checking with 132 declarations but cannot run due to these codegen bugs. Bug 1 still needs a workaround, Bug 2 needs investigation.

## General Notes

- All bugs pass type checking (`klar check`) but fail at code generation (`klar run`)
- Error messages are not helpful - just "UnsupportedFeature" with no indication of which feature
- Binary search on file contents is the only practical way to isolate issues
