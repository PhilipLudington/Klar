# RESUME

## Recently Completed

### Map Heap-Indirection + QA Fixes (committed 2026-03-02)

- **Map heap-indirection:** Changed Map from flat `{ entries, len, cap, tombstones }` (20 bytes) to `{ header_ptr }` (8 bytes) with shared heap header. Same pattern as List (Bug 2) and String (Bug 3). Enables shared mutation — passing a Map by value to a function that inserts is now visible to the caller.
- **QA fixes from review:**
  - `requiresSretForTypeExpr`: Map no longer hardcoded to sret (8 bytes fits in register); uses dynamic size check like List
  - `getSizeOfTypeExpr`: Map returns 8 (was 20); Set correctly stays at 20
  - `buildMapFromHeader`: Now accepts `name` parameter for LLVM IR readability (was discarded)
  - Stale comments updated to reflect heap-indirected layout
- **`src/codegen/map.zig`** — New `MapField`, `MapHeaderField`, `createMapHeaderType()`, size constants
- **`src/codegen/emit.zig`** — New helpers `getMapHeaderType()`, `derefMapHeader()`, `allocateMapHeader()`, `buildMapFromHeader()`. Updated all ~20 Map emitter call sites.
- **`test/native/map_shared_insert.kl`** — Shared mutation test (analogous to `list_shared_push.kl`)
- All 357 native tests pass.

### CLI Argument Parsing Library (committed 2026-03-02)

- **`stdlib/cli.kl`** — Builder-pattern API: `cli_def_flag`, `cli_def_option`, `cli_def_subcommand`, `cli_parse`, accessors (`cli_get_flag`, `cli_get_option`, `cli_has_subcommand`), help generation (`cli_help`, `cli_subcommand_help`)
- **`test/module/cli/main.kl`** — 30 tests modeling the Lodex CLI (init, checkpoint, tree, export)
- **`scripts/run-module-tests.sh`** — Updated with CLI as test #12
- All 30 tests pass. Module test suite: 12/13 pass (TOML has pre-existing intermittent crash).

---

## Codegen Bugs to Fix

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

## [x] Bug 3: String (capital-S) passed to functions creates independent copy (fixed 2026-03-02)

**Symptom:** A `String` buffer passed to a function and mutated via `.push_str()` inside the function does not affect the caller's `String`. The caller's buffer remains empty/unchanged.

**Root cause:** Same value-copy semantics as Bug 2. `String` was `{ ptr, len, capacity }` (16 bytes) — passing it copied the flat struct. The callee's `.push_str()` could reallocate or update length on the copy.

**Fix:** Changed String from flat `{ ptr, len, capacity }` (16 bytes) to heap-indirected `{ header_ptr }` (8 bytes), identical to the List fix in Bug 2. The header `{ ptr, len, capacity }` lives on the heap. When a String is copied (passed to functions, extracted from Map.get(), etc.), both copies share the same heap header, so mutations like `.push_str()` are visible through both.

Key changes:
- `src/codegen/strings_emit.zig`: New `StringHeaderField`, `createStringHeaderType()`, updated `StringField` and `createStringStructType()` for single-pointer wrapper, updated size constants (8 bytes outer, 16 bytes header)
- `src/codegen/emit.zig`: New helpers `getStringHeaderType()`, `derefStringHeader()`, `allocateStringHeader()`, `buildStringFromHeader()`. Updated all ~30 call sites that access String internals (emitStringNew, emitStringFrom, emitStringFromPtr, emitStringWithCapacity, emitStringPush, emitStringPushStr, emitStringConcat, emitStringLen, emitStringCapacity, emitStringAsStr, emitStringClear, emitStringClone, emitStringDrop, emitStringEq, emitStringHash, emitStringAsCstr, emitCstrToString, emitStringToCstrOwned, typeToLLVM, getTypeSize, main argv setup/cleanup, List#[String] drop, readdir, read_line, etc.)

**Test:** `test/native/string_shared_push.kl`

---

## [x] Bug 4: Key string corruption in recursive functions passing List#[string] (fixed 2026-03-01)

**Symptom:** When a `List#[string]` (e.g., map keys) is passed through recursive function calls, key string pointers become corrupted after ~2 iterations. Accessing `keys.get(i)!` returns a pointer to invalid memory, causing SIGSEGV in `strcmp`.

**Root cause:** The old flat `List` struct `{ ptr, i32, i32 }` was passed by value. When a function pushed to the list (causing realloc), the caller's copy still had the old data pointer. In recursive scenarios, the stack copies diverged further with each call.

**Fix:** Fixed by the same List heap-indirection change as Bug 2. The list is now `{ header_ptr }` — a single pointer to a shared heap header. Recursive function calls copy only the 8-byte pointer, not the data/len/capacity. All copies point to the same header, so reallocs update the shared data pointer.

**Test:** `test/native/list_recursive_pass.kl`

---

## [x] Bug 8: Map passed to functions creates independent copy (fixed 2026-03-02)

**Symptom:** A `Map#[K,V]` passed to a function and mutated via `.insert()` inside the function does not affect the caller's map. The caller's map remains unchanged.

**Root cause:** Same value-copy semantics as Bugs 2–4. Map was `{ entries_ptr, len, capacity, tombstones }` (20 bytes) — passing it copied the flat struct. The callee's `.insert()` (and potential rehash) updated the callee's copy only.

**Fix:** Changed Map from flat `{ entries_ptr, len, capacity, tombstones }` (20 bytes) to heap-indirected `{ header_ptr }` (8 bytes), identical to the List (Bug 2) and String (Bug 3) fixes. The header `{ entries_ptr, len, capacity, tombstones }` lives on the heap. When a Map is copied, both copies share the same heap header, so mutations like `.insert()` are visible through both.

Key changes:
- `src/codegen/map.zig`: New `MapField`, `MapHeaderField`, `createMapHeaderType()`, `createMapStructType()` for single-pointer wrapper, size constants (`map_struct_size=8`, `map_header_size=20`)
- `src/codegen/emit.zig`: New helpers `getMapHeaderType()`, `derefMapHeader()`, `allocateMapHeader()`, `buildMapFromHeader()`. Updated all ~20 call sites that access Map internals (emitMapNew, emitMapWithCapacity, emitMapInsert, emitMapGet, emitMapRemove, emitMapLen, emitMapContains, emitMapKeys, emitMapValues, emitMapClone, emitMapDrop, emitMapTake, emitMapSkip, emitMapFilter, emitForLoopMap, emitSetMap, etc.)
- QA fixes: `requiresSretForTypeExpr` (Map now uses dynamic size check), `getSizeOfTypeExpr` (returns 8 for Map), stale comments updated, `buildMapFromHeader` name passthrough

**Test:** `test/native/map_shared_insert.kl`

---

## [x] Bug 5: Cross-module string length metadata corruption (fixed 2026-03-02)

**Symptom:** Strings extracted from enum payloads across module boundaries have corrupted `.len()` metadata — `.len()` returns 0 or wrong values, and `.index_of()` fails. However, `==` comparison works correctly.

**Root cause:** The `is_string` flag on pattern-bound variables depends on `resolveTypeExprDirect` resolving the enum's payload type. For cross-module named types (e.g., `Token` imported from `lib.kl`), `tc.resolveTypeExpr()` could fail silently when the type checker's current scope didn't contain the imported type during codegen.

**Fix:** Two changes:
1. **`resolveTypeExprDirect` cross-module fallback** (`emit.zig`): After `tc.resolveTypeExpr()` fails for named types, falls back to `tc.lookupSymbolAcrossModules(name)` to search all module scopes. This ensures cross-module enum types like `Token` (imported from `lib.kl`) are resolved even when the type checker's current scope doesn't contain them.
2. **`layout.zig` string size** (`layout.zig`): Fixed `string_` from `pointer_size * 2` (16 bytes) to `pointer_size` (8 bytes) — string in LLVM codegen is a single pointer (null-terminated C string), not ptr+len. Latent ABI bug from pre-heap-indirection era.

**Tests:** `test/module/string_enum/main.kl`, `test/module/string_enum_complex/main.kl` (new: index_of, starts_with, contains across module boundaries)

---

## [x] Bug 6: Returning strings through Result tuples from helper functions causes SIGSEGV (fixed 2026-03-02)

**Symptom:** A function that returns `Result#[(string, i32), TomlError]` and is called from within another function (e.g., `parse_basic_string`) causes a SIGSEGV at runtime. The calling function itself returns the same type and works fine. Simple string returns work; the crash only occurs when a *called* helper returns a string inside a Result tuple.

**Root cause:** The old flat string representation `{ptr, len, cap}` (16 bytes) — when a string was returned inside a Result tuple via sret, the flat struct copy could hold a stale pointer after the callee's stack frame was cleaned up.

**Fix:** Already resolved by the Bug 3 fix (String heap indirection). With the new `{header_ptr}` (8 bytes) representation, strings are shared via a single heap pointer. Copies through Result tuples always point to valid heap data.

**Workaround (can now be removed):** The TOML parser's duplicated escape handling between `parse_basic_string` and `parse_ml_basic_string_val` in `stdlib/toml.kl` can now be deduplicated by extracting a shared helper. This is a separate cleanup task.

**Test:** `test/native/result_tuple_string_helper.kl` — tests helper returning `Result#[(string, i32), ParseError]` with struct error type, string building via `from_byte()`, error propagation, and double-nested call patterns.

---

## [x] Bug 7: f64 infinity and NaN operations crash native codegen (fixed 2026-03-02)

**Symptom:** Any program that produces or operates on `f64` infinity or NaN values crashes with SIGSEGV. This includes: `1.0e308 * 10.0` (overflow to inf), `0.0 / 0.0` (NaN), `inf.to_string()`, and arithmetic on inf/nan.

**Root cause:** `f64.to_string()` was not implemented in `emitMethodCall()`. The method call fell through to the fallback `return llvm.Const.int32(self.ctx, 0)` — a null pointer. When `.as_str()` or `println()` tried to dereference this, it caused SIGSEGV.

**Fix:** Three changes in `src/codegen/emit.zig`:
1. **`isFloatExpr()`** — New helper (like `isIntegerExpr`, `isCharExpr`) that detects float expressions via literal kind, semantic type, LLVM type, or type checker fallback.
2. **Float dispatch in `emitMethodCall()`** — After integer methods check, added `isFloatExpr` → `emitF64ToString` dispatch for `to_string()`.
3. **`emitF64ToString()`** — Full implementation with special value handling:
   - NaN: detected via `fcmp uno value, value`, returns `"nan"`
   - Infinity: detected via `fcmp oeq value, ±inf`, returns `"inf"` or `"-inf"`
   - Normal: uses `snprintf(buf, 64, "%g", value)`, copies to heap, returns heap-indirected String
4. **`emitStringFromLiteral()`** — Helper to create a heap-indirected String from a compile-time string constant.

**Test:** `test/native/f64_to_string.kl` — normal values (pi, negative, zero, small), infinity, negative infinity, NaN, String operations on result.
