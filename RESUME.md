# Resume Point: List[T] Iteration Complete

## Context

Working on **Phase 4: Language Completion** - Milestone 4 (Stdlib Core). `List[T]` is now a fully functional builtin growable collection type with for-loop iteration support.

## Progress Summary

### Completed

- **Milestone 6: Iterator Protocol** ✅
- **List[T] Full Implementation** ✅
- **For-loop iteration over List[T]** ✅ ← LATEST

---

## Current State: List[T] Type

`List[T]` is a dynamic/growable list type with the following layout:
```
{ ptr: *T, len: i32, capacity: i32 }
```

### Methods Status

| Method | Status | Implementation |
|--------|--------|----------------|
| `List.new[T]()` | ✅ Working | Inline - returns `{ null, 0, 0 }` |
| `list.len()` | ✅ Working | Inline - reads struct field |
| `list.is_empty()` | ✅ Working | Inline - compares len to 0 |
| `list.capacity()` | ✅ Working | Inline - reads struct field |
| `list.push(value)` | ✅ Working | Inline - capacity check, realloc, store |
| `list.pop()` | ✅ Working | Inline - returns ?T, decrements len |
| `list.get(index)` | ✅ Working | Inline - bounds check, returns ?T |
| `list.set(index, value)` | ✅ Working | Inline - bounds check, stores value |
| `list.first()` | ✅ Working | Inline - returns ?T (first element) |
| `list.last()` | ✅ Working | Inline - returns ?T (last element) |
| `list.clear()` | ✅ Working | Inline - sets len to 0 |
| **For-loop iteration** | ✅ Working | `for x in list { ... }` |
| `with_capacity(n)` | ❌ Not started | Pre-allocate capacity |
| Drop/cleanup | ❌ Not started | Free memory on scope exit |

### Test Files

**`test/native/list_basic.kl`** - Tests core operations:
```klar
fn main() -> i32 {
    var list: List[i32] = List.new[i32]()
    if list.len() != 0 { return 1 }
    if not list.is_empty() { return 2 }

    list.push(10)
    list.push(20)
    list.push(30)
    if list.len() != 3 { return 4 }

    return 42
}
```

**`test/native/for_list.kl`** - Tests for-loop iteration:
```klar
fn main() -> i32 {
    var nums: List[i32] = List.new[i32]()
    nums.push(10)
    nums.push(20)
    nums.push(30)

    var sum: i32 = 0
    for n: i32 in nums {
        sum = sum + n
    }
    // sum = 60

    // Also tests break, continue, empty list iteration
    return 0
}
```

### Verification

All 371 tests pass.

---

## Files Modified

| File | Changes |
|------|---------|
| `src/types.zig` | `ListType` struct and `.list` variant |
| `src/checker.zig` | Type checking for `List[T]`, all methods, for-loop iteration |
| `src/parser.zig` | Method calls with type args: `foo.method[T]()` |
| `src/codegen/emit.zig` | LLVM codegen for all methods and for-loop iteration |
| `test/native/list_basic.kl` | Test file for List operations |
| `test/native/for_list.kl` | Test file for for-loop iteration |

### Key Functions in emit.zig

**List type helpers:**
- `isListExpr(expr)` - checks if expression is List type
- `isListType(ty)` - checks LLVM type structure
- `getListElementType(expr)` - gets element type
- `getListStructType()` - returns `{ ptr, i32, i32 }`
- `getListTypeInfo(type_expr)` - extracts element type from `List[T]`
- `getListInfo(expr)` - returns ListInfo{alloca, element_type} for iteration

**List method emitters:**
- `emitListNew()` - creates empty list `{ null, 0, 0 }`
- `emitListLen()` - reads len field
- `emitListIsEmpty()` - compares len to 0
- `emitListCapacity()` - reads capacity field
- `emitListPush()` - capacity check, realloc if needed, store value
- `emitListPop()` - decrement len, return Optional[T]
- `emitListGet()` - bounds check, return Optional[T]
- `emitListSet()` - bounds check, store value
- `emitListFirst()` - return first element as Optional[T]
- `emitListLast()` - return last element as Optional[T]
- `emitListClear()` - set len to 0

**For-loop iteration:**
- `emitForLoopList()` - generates index-based iteration over List[T]

### LocalValue Changes

Added `list_element_type: ?types.Type` field to track element type for List variables.

---

## Architecture Notes

The Klar compiler generates standalone native executables without linking a separate runtime library. All List methods are implemented inline in LLVM IR within `emit.zig`.

For-loop iteration over List[T]:
1. Allocates index counter (starts at 0)
2. Loop condition: `idx < list.len`
3. In body: load `list.ptr[idx]`, bind to loop variable
4. Increment: `idx += 1`
5. Supports `break` and `continue`

---

## What's Next

To complete List[T]:

1. **`with_capacity(n)`** - Pre-allocate capacity for performance
2. **Drop trait** - Free the ptr on scope exit (memory safety)
3. **Clone trait** - Clone list with cloneable elements

Other Milestone 4 work:
- **Map[K, V]** - Hash-based key-value store
- **Set[T]** - Hash-based unique collection
- **String type** - Heap-allocated growable strings

---

## Commands to Resume

```bash
# Rebuild and run tests
./build.sh && ./run-tests.sh

# Test list specifically
./zig-out/bin/klar run test/native/list_basic.kl
./zig-out/bin/klar run test/native/for_list.kl

# Check generated IR
./zig-out/bin/klar build test/native/for_list.kl -o /tmp/test --emit-llvm
cat for_list.ll
```
