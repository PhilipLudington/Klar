# Resume Point: Map[K,V] Type Complete

## Context

Working on **Phase 4: Language Completion** - Milestone 4 (Stdlib Core). `Map[K,V]` is now a **fully implemented** builtin hash map type.

## Progress Summary

### Completed

- **Milestone 6: Iterator Protocol** ✅
- **List[T] Full Implementation** ✅
- **String Type Full Implementation** ✅
- **Map[K,V] Full Implementation** ✅
  - Static constructors: `Map.new[K,V]()`, `Map.with_capacity[K,V](n)`
  - Accessors: `len()`, `is_empty()`, `capacity()`
  - Lookup: `get(key) -> ?V`, `contains_key(key) -> bool`
  - Mutation: `insert(key, value)`, `remove(key) -> ?V`, `clear()`
  - Collection: `keys() -> List[K]`, `values() -> List[V]`
  - Memory: `clone()`, `drop()`
  - Automatic resize/rehash at 75% load factor

---

## Current State: Map[K,V] Type

`Map[K,V]` is a hash map using open addressing with linear probing and tombstone deletion:

```
Map layout:   { entries: *Entry, len: i32, capacity: i32, tombstone_count: i32 }
Entry layout: { state: i8, cached_hash: i32, key: K, value: V }

state: EMPTY=0, OCCUPIED=1, TOMBSTONE=2
Initial capacity: 8
Load factor threshold: 75%
Growth factor: 2x
```

### All Methods Implemented

| Method | Signature | Notes |
|--------|-----------|-------|
| `Map.new[K,V]()` | `() -> Map[K,V]` | Returns zeroed struct |
| `Map.with_capacity[K,V](n)` | `(i32) -> Map[K,V]` | Pre-allocates entries |
| `m.insert(key, value)` | `(K, V) -> void` | Updates if key exists, triggers resize at 75% load |
| `m.get(key)` | `(K) -> ?V` | Returns None if not found |
| `m.remove(key)` | `(K) -> ?V` | Uses tombstone deletion |
| `m.contains_key(key)` | `(K) -> bool` | Probe loop check |
| `m.len()` | `() -> i32` | Excludes tombstones |
| `m.is_empty()` | `() -> bool` | len == 0 |
| `m.capacity()` | `() -> i32` | Current allocated capacity |
| `m.keys()` | `() -> List[K]` | Collects all keys |
| `m.values()` | `() -> List[V]` | Collects all values |
| `m.clear()` | `() -> void` | Resets to empty |
| `m.clone()` | `() -> Map[K,V]` | Deep copy with memcpy |
| `m.drop()` | `() -> void` | Frees entries |

### Test Files

- `test/native/map_basic.kl` - Core operations: new, insert, get, contains_key, update, remove
- `test/native/map_resize.kl` - Resize/rehash with 20+ inserts, value verification after resize

### Verification

All 402 tests pass (220 unit + 166 native + 10 app + 6 module).

---

## Key Bug Fixes

1. **Cell methods guard**: Cell's `get()` method was incorrectly matching Map's `get()` call because there was no type check. Fixed by adding guard in both `emitMethodCall` and `inferExprType` to only dispatch to Cell methods when the object is a pointer and not Map/List/Array.

2. **Map emit functions**: Four functions (`emitMapRemove`, `emitMapContainsKey`, `emitMapValues`, `emitMapClone`) were still using `checkExpr` which failed during codegen. Fixed to use `getMapKeyType`/`getMapValueType` helper functions that read from stored LocalValue info.

3. **Resize implementation**: Initial implementation only handled capacity=0 case. Added full resize logic with rehashing when `(len + tombstone_count + 1) * 4 > capacity * 3`.

---

## What's Next

Continue with **Milestone 4: Standard Library - Core**:

1. **Set[T]** - Hash-based unique collection
   - Requires T: Hash + Eq bound
   - `new()`, `insert()`, `contains()`, `remove()`
   - `union()`, `intersection()`, `difference()`

2. **Prelude** - Auto-imported types
   - Include Option, Result, String, List, Map
   - Include core traits (Eq, Clone, Hash, etc.)

---

## Commands to Resume

```bash
# Rebuild and run tests
./build.sh && ./run-tests.sh

# Test Map specifically
./zig-out/bin/klar run test/native/map_basic.kl
./zig-out/bin/klar run test/native/map_resize.kl

# Check generated IR
./zig-out/bin/klar build test/native/map_basic.kl -o /tmp/test --emit-llvm
```
