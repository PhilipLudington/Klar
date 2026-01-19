# Resume Point: List[T] Feature Complete

## Context

Working on **Phase 4: Language Completion** - Milestone 4 (Stdlib Core). `List[T]` is now a **fully complete** builtin growable collection type with all planned methods implemented.

## Progress Summary

### Completed

- **Milestone 6: Iterator Protocol** ✅
- **List[T] Full Implementation** ✅
- **For-loop iteration over List[T]** ✅
- **List.with_capacity[T](n)** ✅ ← NEW
- **list.clone()** ✅ ← NEW
- **list.drop()** ✅ ← NEW

---

## Current State: List[T] Type

`List[T]` is a dynamic/growable list type with the following layout:
```
{ ptr: *T, len: i32, capacity: i32 }
```

### Methods Status - ALL COMPLETE

| Method | Status | Implementation |
|--------|--------|----------------|
| `List.new[T]()` | ✅ Working | Inline - returns `{ null, 0, 0 }` |
| `List.with_capacity[T](n)` | ✅ Working | Inline - malloc, returns `{ ptr, 0, n }` |
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
| `list.clone()` | ✅ Working | Inline - malloc + memcpy, deep copy |
| `list.drop()` | ✅ Working | Inline - free + reset to empty |
| **For-loop iteration** | ✅ Working | `for x in list { ... }` |

### Test Files

- `test/native/list_basic.kl` - Core operations (push, pop, get, len, etc.)
- `test/native/for_list.kl` - For-loop iteration with break/continue
- `test/native/list_with_capacity.kl` - Pre-allocated capacity
- `test/native/list_clone.kl` - Deep copy verification
- `test/native/list_drop.kl` - Memory cleanup and reuse

### Verification

All 374 tests pass (211 unit + 147 native + 10 app + 6 module).

---

## Files Modified (This Session)

| File | Changes |
|------|---------|
| `src/checker.zig` | Type checking for `with_capacity`, `clone`, `drop` |
| `src/codegen/emit.zig` | `emitListWithCapacity`, `emitListClone`, `emitListDrop`, fixed method dispatch |
| `src/runtime/list.zig` | Runtime functions (documented, not linked) |
| `src/runtime/mod.zig` | Runtime exports |
| `test/native/list_with_capacity.kl` | Test for with_capacity |
| `test/native/list_clone.kl` | Test for clone |
| `test/native/list_drop.kl` | Test for drop |
| `PLAN.md` | Updated to mark List[T] features complete |

### Key Functions Added in emit.zig

**New List method emitters:**
- `emitListWithCapacity()` - malloc capacity*size, returns `{ ptr, 0, capacity }`
- `emitListClone()` - malloc + memcpy, returns deep copy
- `emitListDrop()` - free ptr, reset to `{ null, 0, 0 }`

**Bug Fix:**
- Fixed method dispatch ordering - List.get/set/clone/drop now checked before Cell methods

---

## Architecture Notes

All List methods are implemented as **inline LLVM IR** in `emit.zig`. No separate runtime library is linked. This approach:
- Eliminates linker dependencies
- Enables optimization across method boundaries
- Keeps executables self-contained

Method dispatch in `emitMethodCall()`:
1. Array methods (for `[T; N]`)
2. **List methods** (for `List[T]`) ← Fixed ordering
3. User-defined struct methods
4. Cell methods
5. Rc/Arc methods
6. Builtin trait methods

---

## What's Next

List[T] is **feature complete**. Remaining Milestone 4 work:

1. **Map[K, V]** - Hash-based key-value store
   - `new()`, `insert()`, `get()`, `remove()`
   - `contains_key()`, `keys()`, `values()`
   - Requires K: Hash + Eq

2. **Set[T]** - Hash-based unique collection
   - `new()`, `insert()`, `remove()`, `contains()`
   - Requires T: Hash + Eq

3. **String type** - Heap-allocated growable strings
   - Methods: `len()`, `push()`, `concat()`, `slice()`
   - UTF-8 support

---

## Commands to Resume

```bash
# Rebuild and run tests
./build.sh && ./run-tests.sh

# Test list specifically
./zig-out/bin/klar run test/native/list_basic.kl
./zig-out/bin/klar run test/native/list_with_capacity.kl
./zig-out/bin/klar run test/native/list_clone.kl
./zig-out/bin/klar run test/native/list_drop.kl

# Check generated IR
./zig-out/bin/klar build test/native/list_clone.kl -o /tmp/test --emit-llvm
cat list_clone.ll
```
