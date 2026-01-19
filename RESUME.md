# Resume Point: String Type Implementation

## Context

Working on **Phase 4: Language Completion** - Milestone 4 (Stdlib Core). `String` is now a **partially implemented** builtin heap-allocated string type with core methods working.

## Progress Summary

### Completed

- **Milestone 6: Iterator Protocol** ✅
- **List[T] Full Implementation** ✅
- **String Type Basic Implementation** ✅
  - Static constructors: `new()`, `from()`, `with_capacity()`
  - Accessors: `len()`, `is_empty()`, `capacity()`
  - Mutation: `push(char)` with automatic growth
  - **Concatenation: `concat(other)`** ✅ ← NEW (supports chained calls)

---

## Current State: String Type

`String` is a heap-allocated, growable UTF-8 string type with the following layout:
```
{ ptr: *u8, len: i32, capacity: i32 }
```

### Methods Status

| Method | Status | Implementation |
|--------|--------|----------------|
| `String.new()` | ✅ Working | Inline - returns `{ null, 0, 0 }` |
| `String.from(s)` | ✅ Working | Inline - strlen + malloc + memcpy |
| `String.with_capacity(n)` | ✅ Working | Inline - malloc, returns `{ ptr, 0, n }` |
| `s.len()` | ✅ Working | Inline - reads struct field |
| `s.is_empty()` | ✅ Working | Inline - compares len to 0 |
| `s.capacity()` | ✅ Working | Inline - reads struct field |
| `s.push(char)` | ✅ Working | Inline - capacity check, realloc, store |
| `s.concat(other)` | ✅ Working | Inline - malloc, 2x memcpy, returns new String |
| `s.append(other)` | ❌ Not yet | Append another string |
| `s.as_str()` | ❌ Not yet | Get as primitive string |
| `s.clear()` | ❌ Not yet | Clear contents |
| `s.clone()` | ❌ Not yet | Deep copy |
| `s.drop()` | ❌ Not yet | Free memory |
| `s.eq(other)` | ❌ Not yet | Equality comparison |
| `s.hash()` | ❌ Not yet | Hash code |

### Test Files

- `test/native/string_basic.kl` - Core operations (new, from, with_capacity, len, is_empty, capacity)
- `test/native/string_push.kl` - Push character with length verification
- `test/native/string_simple.kl` - Minimal push test
- `test/native/string_concat.kl` - Concatenation including chained calls

### Verification

All 387 tests pass (220 unit + 151 native + 10 app + 6 module).

---

## Files Modified (This Session)

| File | Changes |
|------|---------|
| `src/types.zig` | Added `StringDataType` definition |
| `src/checker.zig` | Type checking for String methods, added `string_data` to len/is_empty checks |
| `src/codegen/emit.zig` | String emitters, `is_string_data` flag, `isTypeStringData()` |
| `src/runtime/string_heap.zig` | Runtime reference (inline codegen used) |
| `src/runtime/mod.zig` | Runtime exports |
| `test/native/string_*.kl` | Test files |
| `PLAN.md` | Updated String Type status |

### Key Functions Added in emit.zig

**String method emitters:**
- `emitStringNew()` - returns `{ null, 0, 0 }`
- `emitStringFrom()` - strlen + malloc + memcpy
- `emitStringWithCapacity()` - malloc, returns `{ ptr, 0, n }`
- `emitStringDataLen()` - reads len field
- `emitStringDataIsEmpty()` - compares len to 0
- `emitStringCapacity()` - reads capacity field
- `emitStringPush()` - capacity check, realloc if needed, store char
- `emitStringConcat()` - malloc new buffer, 2x memcpy, returns new String struct

**Bug Fixes:**
- Added `is_string_data` flag to `LocalValue` struct
- Fixed scope state issue where type checker lookups failed during codegen
- `isStringDataExpr()` enhanced to detect String-returning method calls (for chained concat support)
- String method dispatch now handles non-identifier objects by storing to temp alloca

---

## Architecture Notes

All String methods are implemented as **inline LLVM IR** in `emit.zig`, same as List[T]. This approach:
- Eliminates linker dependencies
- Enables optimization across method boundaries
- Keeps executables self-contained

Method dispatch in `emitMethodCall()`:
1. Array methods (for `[T; N]`)
2. List methods (for `List[T]`)
3. **String methods** (for `String`) ← NEW
4. User-defined struct methods
5. Cell methods
6. Rc/Arc methods
7. Builtin trait methods

---

## What's Next

String type needs remaining methods:

1. **String mutation methods:**
   - `append(other)` - Append in place (mutates self)
   - `clear()` - Reset to empty

2. **String conversion:**
   - `as_str()` - Get primitive string reference

3. **Memory management:**
   - `clone()` - Deep copy
   - `drop()` - Free memory

4. **Comparison/hashing:**
   - `eq(other)` - Equality check
   - `hash()` - Hash code for use in Map/Set

Then continue with:
- **Map[K, V]** - Hash-based key-value store
- **Set[T]** - Hash-based unique collection

---

## Commands to Resume

```bash
# Rebuild and run tests
./build.sh && ./run-tests.sh

# Test String specifically
./zig-out/bin/klar run test/native/string_basic.kl
./zig-out/bin/klar run test/native/string_concat.kl

# Check generated IR
./zig-out/bin/klar build test/native/string_concat.kl -o /tmp/test --emit-llvm
cat string_concat.ll
```
