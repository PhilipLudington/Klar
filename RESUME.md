# Resume Point: Map[K,V] Iteration Complete

## Context

Working on **Phase 4: Language Completion** - Milestone 6 (Iterator Protocol). For-loop iteration over `Map[K,V]` is now implemented.

## Progress Summary

### Just Completed

- **For-loop over Map[K,V]** ✅
  - `for (k, v) in map { ... }` syntax with tuple pattern
  - Iterates over all occupied entries in the hash map
  - Skips empty and tombstone entries
  - Supports `break` and `continue`
  - Test: `test/native/map_for.kl`

### Previously Completed

- **Milestone 6: Iterator Protocol** ✅ (Core complete)
  - For-loops over Range literals and variables
  - For-loops over arrays
  - For-loops over List[T]
  - For-loops over Set[T]
  - For-loops over Map[K,V] (NEW)
- **List[T] Full Implementation** ✅
- **String Type Full Implementation** ✅
- **Map[K,V] Full Implementation** ✅
- **Set[T] Full Implementation** ✅

---

## Implementation Details: Map Iteration

### Type Checker (checker.zig)

Added `.map` case to `checkFor()` that creates a tuple type `(K, V)`:

```zig
const elem_type: Type = switch (iter_type) {
    .array => |a| a.element,
    .list => |l| l.element,
    .set => |s| s.element,
    .map => |m| blk: {
        // Map iteration yields (key, value) tuples
        const tuple_elems = [_]Type{ m.key, m.value };
        break :blk self.type_builder.tupleType(&tuple_elems) catch self.type_builder.unknownType();
    },
    // ...
};
```

The existing `bindPattern()` function already handles tuple patterns correctly.

### Code Generation (emit.zig)

Added tuple pattern detection in `emitForLoop()`:
```zig
if (loop.pattern == .tuple_pattern) {
    if (self.isMapExpr(loop.iterable)) {
        try self.emitForLoopMap(func, loop.pattern.tuple_pattern, loop.iterable, loop.body);
        return;
    }
}
```

Implemented `emitForLoopMap()` that:
1. Extracts key/value binding names from tuple pattern
2. Iterates index from 0 to capacity
3. For each index, checks if entry state == OCCUPIED (1)
4. If occupied, loads key (field 2) and value (field 3) from entry
5. Stores in key_alloca and value_alloca
6. Executes loop body
7. Skips EMPTY (0) and TOMBSTONE (2) entries

```
Loop structure:
  cond:  idx < capacity? -> check or end
  check: state == OCCUPIED? -> body or incr
  body:  load key/value, store in bindings, execute body -> incr
  incr:  idx++, -> cond
  end:   continue after loop
```

### Entry Layout Reminder
- Map: `{ entries: *Entry, len: i32, capacity: i32, tombstone_count: i32 }`
- Entry: `{ state: i8, cached_hash: i32, key: K, value: V }`
- State: 0=EMPTY, 1=OCCUPIED, 2=TOMBSTONE

### Test Coverage

`test/native/map_for.kl`:
- Sum all values (verifies value iteration)
- Sum all keys (verifies key iteration)
- Count elements (verifies correct count)
- Empty map iteration (verifies zero iterations)
- Break statement (verifies early exit)
- Continue statement (verifies skip behavior)

---

## Current Test Status

All 401 tests pass:
- Unit Tests: 220 passed
- Native Tests: 165 passed (includes new map_for.kl)
- App Tests: 10 passed
- Module Tests: 6 passed

---

## What's Next

Continue with **Milestone 6: Iterator Protocol** or other tasks:

1. **Iterator adapters** (Milestone 6):
   - `map()`, `filter()`, `take()`, `skip()`, `enumerate()`, `zip()`

2. **`?` operator for early return** (Milestone 7):
   - For Result: return early on Err
   - For Option: return early on None

3. **Standard Library I/O** (Milestone 5):
   - File, Read/Write traits, stdin/stdout

---

## Commands to Resume

```bash
# Rebuild and run tests
./build.sh && ./run-tests.sh

# Test iteration specifically
./zig-out/bin/klar run test/native/map_for.kl
./zig-out/bin/klar run test/native/set_for.kl
./zig-out/bin/klar run test/native/list_for.kl
./zig-out/bin/klar run test/native/for_range.kl
./zig-out/bin/klar run test/native/for_array.kl

# Check generated IR for map iteration
./zig-out/bin/klar build test/native/map_for.kl -o /tmp/test --emit-llvm
```
