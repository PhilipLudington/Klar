# Resume Point: List[T] Implementation In Progress

## Context

Working on **Phase 4: Language Completion** - Milestone 4 (Stdlib Core). Implementing `List[T]` as a builtin growable collection type.

## Progress Summary

### Completed

- **Milestone 6: Iterator Protocol** ✅
- **List[T] Basic Infrastructure** ✅
- **List[T] push() method** ✅ ← LATEST

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
| `list.pop()` | ⚠️ Declared | Needs inline impl |
| `list.get(index)` | ⚠️ Declared | Needs inline impl |
| `list.set(index, value)` | ⚠️ Declared | Needs inline impl |
| `list.first()` | ⚠️ Declared | Needs inline impl |
| `list.last()` | ⚠️ Declared | Needs inline impl |
| `list.clear()` | ⚠️ Declared | Needs inline impl |
| Drop/cleanup | ❌ Not started | Needs inline impl |

### Test File

`test/native/list_basic.kl`:
```klar
fn main() -> i32 {
    var list: List[i32] = List.new[i32]()

    // Test empty list
    if list.len() != 0 { return 1 }
    if not list.is_empty() { return 2 }

    // Test push and growth
    list.push(10)
    if list.len() != 1 { return 3 }

    list.push(20)
    list.push(30)
    if list.len() != 3 { return 4 }
    if list.capacity() < 8 { return 5 }
    if list.is_empty() { return 6 }

    return 42
}
```

### Verification

All 370 tests pass.

---

## Files Modified

| File | Changes |
|------|---------|
| `src/types.zig` | Added `ListType` struct and `.list` variant |
| `src/checker.zig` | Type checking for `List[T]`, `List.new[T]()`, all methods |
| `src/parser.zig` | Method calls with type args: `foo.method[T]()` |
| `src/codegen/emit.zig` | LLVM codegen, inline emit methods, element type tracking |
| `test/native/list_basic.kl` | Test file for List operations |

### Key Functions in emit.zig

- `isListExpr(expr)` - checks if expression is List type
- `isListType(ty)` - checks LLVM type structure
- `getListElementType(expr)` - gets element type (checks LocalValue first)
- `getListStructType()` - returns `{ ptr, i32, i32 }`
- `getListTypeInfo(type_expr)` - extracts element type from `List[T]` annotation
- `getOrDeclareRealloc()` - declares libc realloc function
- `emitListNew()` - creates empty list `{ null, 0, 0 }`
- `emitListLen()` - reads len field
- `emitListIsEmpty()` - compares len to 0
- `emitListCapacity()` - reads capacity field
- `emitListPush()` - capacity check, realloc if needed, store value, increment len

### LocalValue Changes

Added `list_element_type: ?types.Type` field to track element type for List variables, similar to how `array_element_type` tracks array element types.

---

## Architecture Notes

The Klar compiler generates standalone native executables without linking a separate runtime library. All List methods are implemented inline in LLVM IR within `emit.zig`.

The `push` implementation:
1. Loads current len and capacity
2. If len >= capacity: calculates new_cap = max(8, cap*2), calls realloc
3. Stores value at ptr[len * element_size]
4. Increments len

---

## What's Next

To complete List[T]:

1. **Implement `get` inline** - Bounds check, return Optional[T]
2. **Implement `pop` inline** - Decrement len, return Optional[T]
3. **Implement other methods** - `set`, `first`, `last`, `clear`
4. **Implement Drop** - Free the ptr on scope exit

---

## Commands to Resume

```bash
# Rebuild and run tests
./build.sh && ./run-tests.sh

# Test list specifically
./zig-out/bin/klar run test/native/list_basic.kl

# Check generated IR
./zig-out/bin/klar build test/native/list_basic.kl -o /tmp/test --emit-llvm
cat list_basic.ll
```
