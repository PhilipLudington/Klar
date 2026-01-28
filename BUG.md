# Klar Compiler Bugs

Active bugs discovered during development.

---

## [x] Bug 1: `ref arr[idx]` returns pointer to wrong memory

**Status:** Fixed
**Type:** Codegen / FFI
**Severity:** Medium

### Description

Taking a reference to an array element via `ref arr[idx]` and converting it to a C pointer with `ref_to_ptr` results in a pointer to the wrong memory location (reads as 0, not the actual element value).

### Steps to Reproduce

```klar
fn main() -> i32 {
    var arr: [i32; 5] = [5, 2, 8, 1, 9]

    // This works - single variable
    var x: i32 = 42
    let x_ptr: CPtr[i32] = unsafe { ref_to_ptr(ref x) }
    let x_val: i32 = unsafe { read(x_ptr) }
    // x_val == 42 ✓

    // This FAILS - array element
    let ptr0: CPtr[i32] = unsafe { ref_to_ptr(ref arr[0]) }
    let val0: i32 = unsafe { read(ptr0) }
    // val0 == 0 ✗ (expected 5)

    return 0
}
```

### Expected Behavior

`ref arr[0]` should return a reference to the first element of the array, and `ref_to_ptr` should convert that to a valid `CPtr[i32]` pointing to the element's memory location.

### Actual Behavior

The pointer returned by `ref_to_ptr(ref arr[0])` does not point to the array element. Reading through it returns 0 (or garbage) instead of the actual element value.

### Workaround

Get a pointer to the whole array, then cast to element type:

```klar
var arr: [i32; 5] = [5, 2, 8, 1, 9]

// Instead of: ref_to_ptr(ref arr[0])  -- BROKEN
// Use:
let arr_ptr: CPtr[[i32; 5]] = unsafe { ref_to_ptr(ref arr) }
let elem_ptr: CPtr[i32] = unsafe { ptr_cast[i32](arr_ptr) }
let val0: i32 = unsafe { read(elem_ptr) }
// val0 == 5 ✓

// For arr[n], use offset:
let elem_n: CPtr[i32] = unsafe { offset(elem_ptr, n.as[isize]) }
```

### Impact

Affects FFI code that needs pointers to array elements. The workaround is verbose but functional.

### Investigation Notes

The issue is likely in how `ref` handles indexed expressions in codegen. The reference to `arr[0]` may be:
1. Creating a temporary copy and returning a reference to that
2. Incorrectly computing the GEP (GetElementPtr) offset
3. Not properly handling the array indexing in the reference context

### Discovered

While implementing the `extern_fn_qsort.kl` test for FFI function pointers.

### Fix

Added `emitIndexAddressOf()` function in `src/codegen/emit.zig` that computes the element pointer via GEP without loading the value. Modified `.ref` and `.ref_mut` cases in `emitUnary()` to call this function when the operand is an `.index` expression.

The root cause was that `emitIndexAccess()` always loads the element value after computing its address. For `ref arr[idx]`, we need the address, not the value. The fix adds a separate code path that returns the GEP result directly.

---
