# Resume Point: List[T] Implementation In Progress

## Context

Working on **Phase 4: Language Completion** - Milestone 4 (Stdlib Core). Implementing `List[T]` as a builtin growable collection type.

## Progress Summary

### Completed

- **Milestone 6: Iterator Protocol** ✅
- **List[T] Basic Infrastructure** ✅ ← NEW

---

## What Was Just Implemented: List[T] Type

`List[T]` is a dynamic/growable list type with the following layout:
```
{ ptr: *T, len: i32, capacity: i32 }
```

### Files Modified

| File | Changes |
|------|---------|
| `src/types.zig` | Added `ListType` struct and `.list` variant |
| `src/checker.zig` | Type checking for `List[T]`, `List.new[T]()`, all methods |
| `src/parser.zig` | Method calls with type args: `foo.method[T]()` |
| `src/codegen/emit.zig` | LLVM type, type inference, `isListExpr`, `isListType`, inline emit methods |
| `src/runtime/list.zig` | Runtime functions (not yet linked) |
| `src/runtime/mod.zig` | Re-exports for list functions |
| `test/native/list_basic.kl` | Basic test file |

### Methods Status

| Method | Status | Implementation |
|--------|--------|----------------|
| `List.new[T]()` | ✅ Working | Inline - returns `{ null, 0, 0 }` |
| `list.len()` | ✅ Working | Inline - reads struct field |
| `list.is_empty()` | ✅ Working | Inline - compares len to 0 |
| `list.capacity()` | ✅ Working | Inline - reads struct field |
| `list.push(value)` | ⚠️ Declared | Needs inline impl (malloc/realloc) |
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
    let list: List[i32] = List.new[i32]()

    let len: i32 = list.len()
    if len != 0 { return 1 }

    let empty: bool = list.is_empty()
    if not empty { return 2 }

    return 42
}
```

### Verification

All 370 tests pass.

---

## Architecture Notes

### Why Inline Implementation?

The Klar compiler generates standalone native executables without linking a separate runtime library. Runtime functions (like `klar_rc_alloc`) are implemented inline in LLVM IR within the codegen phase. The Zig runtime files (`src/runtime/*.zig`) exist but aren't linked - they serve as reference implementations.

For List to work fully, methods like `push` need to be implemented inline in `emit.zig`, generating LLVM IR that:
1. Calls malloc/realloc for growth
2. Copies elements
3. Updates len/capacity fields

### Key Functions Added to emit.zig

- `isListExpr(expr)` - checks if expression is List type
- `isListType(ty)` - checks LLVM type structure
- `getListElementType(expr)` - gets element type for generics
- `getListStructType()` - returns `{ ptr, i32, i32 }`
- `emitListNew()` - creates empty list inline
- `emitListLen()` - reads len field
- `emitListIsEmpty()` - compares len to 0
- `emitListCapacity()` - reads capacity field

---

## What's Next

To complete List[T]:

1. **Implement `push` inline** - Most important, requires:
   - Check if len >= capacity
   - If so, realloc to 2x capacity (or initial 8)
   - Copy value to ptr[len]
   - Increment len

2. **Implement other mutation methods** - Similar pattern

3. **Implement Drop** - Free the ptr on scope exit

Alternative: Could simplify by using a fixed-capacity array internally for MVP.

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
