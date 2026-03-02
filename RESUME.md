# RESUME — Codegen Bugs to Fix

These bugs were discovered during the TOML parser implementation (Phase 3) and worked around in `stdlib/toml.kl`. They should be fixed in the compiler to prevent future stdlib/user code from hitting the same issues.

## [x] Bug 1: Map.new() doesn't allocate backing storage (fixed 2026-03-01)

**Symptom:** A freshly created `Map.new#[K,V]()` passed to a function or stored in an enum before any `.insert()` call silently loses all subsequent inserts made by the callee.

**Root cause:** `Map.new()` returned a map with null/zero backing storage. The first `.insert()` allocated the hash table. If the map was passed by value to a function before any insert, the callee's insert allocated new storage on the callee's copy, which the caller never saw.

**Fix:** `emitMapNew()` in `src/codegen/emit.zig` now eagerly allocates a backing table with `map_constants.initial_capacity` (8) entries via malloc+memset, matching what `Map.with_capacity()` already did. Extracted shared helpers `emitMapAlloc` and `resolveMapEntrySize` to deduplicate with `emitMapWithCapacity`.

**Test:** `test/native/map_eager_alloc.kl`

---

## [x] Bug 2: List.push() on lists extracted from Map.get() doesn't persist (fixed 2026-03-01)

**Symptom:** Extracting a `List` from a `Map` via `.get()` + match, then calling `.push()` on it, modifies a copy. The original list in the map is unchanged.

**Root cause:** `Map.get()` returns a copy of the value. For `Map` values, the copy shares the underlying hash table (pointer semantics), so `.insert()` works. For `List` values, the copy created an independent list header (pointer + length + capacity), so `.push()` updated the copy's length but not the original's.

**Fix:** Changed List from a flat `{ ptr, len, capacity }` struct (16 bytes) to a heap-indirected `{ header_ptr }` struct (8 bytes) where the header `{ ptr, len, capacity }` lives on the heap. When a List is copied (passed to functions, extracted from Map.get(), etc.), both copies share the same heap header, so mutations like `.push()` are visible through both.

Key changes:
- `src/codegen/list.zig`: New `ListHeaderField`, `createListHeaderType()`, updated `ListField` and `createListStructType()` for single-pointer wrapper
- `src/codegen/emit.zig`: New helpers `getListHeaderType()`, `derefListHeader()`, `allocateListHeader()`, `buildListFromHeader()`. Updated all ~30 call sites that access list internals (emitListNew, emitListPush, emitListGet, emitListLen, emitListDrop, emitMapKeys, emitMapValues, emitSetMap, emitSetEnumerate, emitSetZip, emitForLoopList, emitListPushInline, etc.)

**Tests:** `test/native/list_shared_push.kl`

---

## [ ] Bug 3: String (capital-S) passed to functions creates independent copy

**Symptom:** A `String` buffer passed to a function and mutated via `.push_str()` inside the function does not affect the caller's `String`. The caller's buffer remains empty/unchanged.

**Root cause:** Same value-copy semantics as Bug 2. `String` is likely backed by a `List#[u8]` or similar, and passing it copies the header. The callee's `.push_str()` may reallocate or update length on the copy.

**Workaround:** Don't pass `String` buffers to helper functions. Instead, have each function return a `string` result and concatenate at the call site:
```klar
// Instead of: fill_buffer(buf)
// Do: let result: string = build_string()
fn build_string() -> string {
    var buf: String = String.new()
    buf.push_str("hello")
    return buf.as_str()
}
```

**Fix approach:** Same as Bug 2 — make `String` use pointer-to-heap-header semantics so copies share state.

**Repro:** `scratch/string_pass.kl`, `scratch/string_pass2.kl`

---

## [x] Bug 4: Key string corruption in recursive functions passing List#[string] (fixed 2026-03-01)

**Symptom:** When a `List#[string]` (e.g., map keys) is passed through recursive function calls, key string pointers become corrupted after ~2 iterations. Accessing `keys.get(i)!` returns a pointer to invalid memory, causing SIGSEGV in `strcmp`.

**Root cause:** The old flat `List` struct `{ ptr, i32, i32 }` was passed by value. When a function pushed to the list (causing realloc), the caller's copy still had the old data pointer. In recursive scenarios, the stack copies diverged further with each call.

**Fix:** Fixed by the same List heap-indirection change as Bug 2. The list is now `{ header_ptr }` — a single pointer to a shared heap header. Recursive function calls copy only the 8-byte pointer, not the data/len/capacity. All copies point to the same header, so reallocs update the shared data pointer.

**Test:** `test/native/list_recursive_pass.kl`

---

## [ ] Bug 5: Cross-module string length metadata corruption

**Symptom:** Strings extracted from enum payloads across module boundaries have corrupted `.len()` metadata — `.len()` returns 0 or wrong values, and `.index_of()` fails. However, `==` comparison works correctly.

**Root cause:** Previously documented (also affects JSON module). The string's length field is not correctly propagated when extracting from an enum variant across module boundaries.

**Workaround:** Use `==` comparison instead of `.len()` or `.index_of()` for strings obtained from cross-module enum extraction:
```klar
// Instead of: if val.index_of("\n") ...
// Do: let expected: string = "hello" + from_byte(10.as#[u8]) + "world"
//     if val == expected { ... }
```

**Fix approach:** Check how enum payload extraction generates LLVM IR for string types across module boundaries. The string metadata (length) may not be correctly loaded from the enum's memory layout.

**Repro:** Already documented in JSON test comments. Also visible in TOML escape sequence tests.

---

## [ ] Bug 6: Returning strings through Result tuples from helper functions causes SIGSEGV

**Symptom:** A function that returns `Result#[(string, i32), TomlError]` and is called from within another function (e.g., `parse_basic_string`) causes a SIGSEGV at runtime. The calling function itself returns the same type and works fine. Simple string returns work; the crash only occurs when a *called* helper returns a string inside a Result tuple.

**Root cause:** Unclear. Likely related to how string memory is managed when returned through nested `Ok((string_val, i32))` tuples from a callee back to the caller. The string pointer may become invalid after the callee's stack frame is cleaned up.

**Workaround:** Keep string-producing escape logic inline rather than extracting to a shared helper function. This means escape-sequence handling is duplicated between `parse_basic_string` and `parse_ml_basic_string_val` in `stdlib/toml.kl`.

**Fix approach:** Investigate LLVM IR for functions returning `Result#[(string, i32), E]` when called from other functions. Check if the string pointer component of the tuple is correctly copied/moved when unwinding through the Result match in the caller.

**Repro:** Extract escape handling from `parse_basic_string` into a `parse_escape_sequence(chars, pos, len) -> Result#[(string, i32), TomlError]` helper and call it. The compiled binary will SIGSEGV on any input containing escape sequences (e.g., `"msg = \"hello\\nworld\""`).

---

## [ ] Bug 7: f64 infinity and NaN operations crash native codegen

**Symptom:** Any program that produces or operates on `f64` infinity or NaN values crashes with SIGSEGV. This includes: `1.0e308 * 10.0` (overflow to inf), `0.0 / 0.0` (NaN), `inf.to_string()`, and arithmetic on inf/nan.

**Root cause:** Unclear. Possibly `f64.to_string()` doesn't handle special float values (inf, NaN, -inf) and dereferences an invalid pointer or buffer.

**Workaround:** Avoid producing or operating on inf/NaN values in native-compiled code. In `stdlib/toml.kl`, the `try_parse_special_float` function produces inf via `1.0e308 * 10.0` and NaN via `0.0 / 0.0`, but these values can only be safely stored — any operation on them (comparison, stringify, print) may crash.

**Fix approach:** Check the `f64.to_string()` implementation in the runtime. Likely needs explicit handling for IEEE 754 special values. Also check if LLVM constant folding produces unexpected IR for `1.0e308 * 10.0`.

**Repro:**
```klar
fn main() -> i32 {
    let big: f64 = 1.0e308
    let inf: f64 = big * 10.0
    println(inf.to_string().as_str())  // SIGSEGV
    return 0
}
```
