# Resume Point: String Type Complete

## Context

Working on **Phase 4: Language Completion** - Milestone 4 (Stdlib Core). `String` is now a **fully implemented** builtin heap-allocated string type.

## Progress Summary

### Completed

- **Milestone 6: Iterator Protocol** ✅
- **List[T] Full Implementation** ✅
- **String Type Full Implementation** ✅
  - Static constructors: `new()`, `from()`, `with_capacity()`
  - Accessors: `len()`, `is_empty()`, `capacity()`
  - Mutation: `push(char)`, `append(other)`, `clear()`
  - Concatenation: `concat(other)` (supports chained calls)
  - Conversion: `as_str()`
  - Memory: `clone()`, `drop()`
  - Comparison: `eq(other)`, `hash()`

---

## Current State: String Type

`String` is a heap-allocated, growable UTF-8 string type with the following layout:
```
{ ptr: *u8, len: i32, capacity: i32 }
```

### All Methods Implemented

| Method | Implementation |
|--------|----------------|
| `String.new()` | Inline - returns `{ null, 0, 0 }` |
| `String.from(s)` | Inline - strlen + malloc + memcpy |
| `String.with_capacity(n)` | Inline - malloc, returns `{ ptr, 0, n }` |
| `s.len()` | Inline - reads struct field |
| `s.is_empty()` | Inline - compares len to 0 |
| `s.capacity()` | Inline - reads struct field |
| `s.push(char)` | Inline - capacity check, realloc, store |
| `s.concat(other)` | Inline - malloc, 2x memcpy, returns new String |
| `s.append(other)` | Inline - grow if needed, memcpy in place |
| `s.as_str()` | Inline - returns ptr field |
| `s.clear()` | Inline - sets len=0, null-terminates |
| `s.clone()` | Inline - malloc + memcpy, returns new String |
| `s.drop()` | Inline - free(ptr), zeros struct |
| `s.eq(other)` | Inline - length check + memcmp |
| `s.hash()` | Inline - FNV-1a loop |

### Test Files

- `test/native/string_basic.kl` - Core operations
- `test/native/string_push.kl` - Push character
- `test/native/string_concat.kl` - Concatenation with chaining
- `test/native/string_append.kl` - Append mutation
- `test/native/string_clone.kl` - Deep copy
- `test/native/string_clear.kl` - Clear contents
- `test/native/string_drop.kl` - Memory free
- `test/native/string_eq.kl` - Equality
- `test/native/string_hash.kl` - Hash code
- `test/native/string_as_str.kl` - Primitive conversion

### Verification

All 394 tests pass (220 unit + 158 native + 10 app + 6 module).

---

## Key Bug Fix

**String/List type collision**: Both String and List have identical LLVM struct layout `{ ptr, i32, i32 }`. Fixed by adding `is_string_data` check in `isListExpr()` to exclude String types before checking LLVM structure. This prevented String methods from being incorrectly dispatched to List handlers.

---

## What's Next

Continue with **Milestone 4: Standard Library - Core**:

1. **Map[K, V]** - Hash-based key-value store
   - Requires K: Hash + Eq bound
   - `new()`, `insert()`, `get()`, `remove()`
   - `contains_key()`, `keys()`, `values()`

2. **Set[T]** - Hash-based unique collection
   - Requires T: Hash + Eq bound
   - `new()`, `insert()`, `contains()`, `remove()`
   - `union()`, `intersection()`, `difference()`

3. **Prelude** - Auto-imported types
   - Include Option, Result, String, List
   - Include core traits (Eq, Clone, etc.)

---

## Commands to Resume

```bash
# Rebuild and run tests
./build.sh && ./run-tests.sh

# Test String specifically
./zig-out/bin/klar run test/native/string_concat.kl
./zig-out/bin/klar run test/native/string_clone.kl
./zig-out/bin/klar run test/native/string_eq.kl

# Check generated IR
./zig-out/bin/klar build test/native/string_concat.kl -o /tmp/test --emit-llvm
```
