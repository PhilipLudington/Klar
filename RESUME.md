# Resume Point: emit.zig Modularization

**Date:** 2026-02-03
**Branch:** QAReview

## Context

QA review of the QAReview branch is complete. All bug fixes (7-14) verified, missing tests added, silent `catch {}` fixed. Full test suite passes (507 tests). Ready to modularize emit.zig.

## Completed This Session

- Reviewed PR (3 parallel review agents: checker refactoring, codegen, bug fixes)
- Added 8 new tests for bugs 9-14:
  - `test/native/self_type_impl.kl` — Bug 9: Self type in ref/inout impl methods
  - `test/native/enum_struct_payload.kl` — Bug 12: Enum struct payload construction
  - `test/native/enum_struct_payload_mismatch.kl` — Bug 12: Type mismatch rejected
  - `test/check/ownership_string_move.kl` — Bug 13: String is non-Copy
  - `test/check/ownership_var_string_move.kl` — Bug 14: Second move caught
  - `test/check/ownership_i32_copy.kl` — Bug 13: i32 is Copy
  - `test/check/ownership_f64_copy.kl` — Bug 13: f64 is Copy
  - `test/check/ownership_bool_copy.kl` — Bug 13: bool is Copy
- Created `scripts/run-check-tests.sh` — new test suite for `klar check` tests
- Wired check tests into `run-tests.sh`
- Fixed silent `catch {}` in `src/checker/patterns.zig` (3 instances → report via `tc.addError`)

## Untestable Bugs (documented limitations)

- **Bugs 10-11** (struct patterns): Parser doesn't support struct pattern syntax (`Point { x, y }` in match). Checker fixes are forward-looking.
- **Bug 9** (Self as return type / non-self param): Codegen doesn't handle `Self` in these positions. Works for `ref Self` and `inout Self` only.

## Next: Modularize emit.zig

**Goal:** Reduce emit.zig from ~29,600 → ~9,000-12,000 lines (60-70% reduction).

### Phase 1: C Library Declarations → `clib_emit.zig` (~8,500 lines)

Extract 83 `getOrDeclare*` functions (malloc, strlen, strcmp, fopen, stat, Rc/Arc runtime, etc.). Pure LLVM function declarations — lowest risk, highest line reduction.

### Phase 2: Scope/Drop → `scope_emit.zig` (~600 lines)

Extract: `pushScope`, `popScope`, `registerDroppable`, `emitDropsForVars`, `emitDropForVar`, `emitDropsForReturn`, `emitDropsForLoopExit`.

### Phase 3: Debug Info → `debug_emit.zig` (~600 lines)

Extract: `initDebugInfo`, `finalizeDebugInfo`, all `emitDebug*` variants.

### Phase 4: Collections → `collections_emit.zig` (~6,000 lines)

Extract List (21 fn), Map (20 fn), Set (20 fn) operations plus type info helpers.

### Phase 5: Pattern Matching → `pattern_emit.zig` (~800 lines)

Extract: `emitPatternMatch`, `bindPatternVariables`, `lookupVariantIndex`, `extractEnumPayload`, etc.

### Phase 6: Closures → `closure_emit.zig` (~900 lines)

Extract: `emitClosure`, `createClosureEnvironment`, `emitClosureCallTyped`, `emitClosureCallWithValue`, `getClosureStructType`.

### Implementation Pattern

Follow checker.zig approach — use `anytype` duck typing:

```zig
// clib_emit.zig
pub fn getOrDeclareMalloc(emitter: anytype) llvm.ValueRef {
    if (emitter.module.getNamedFunction("malloc")) |f| return f;
    // ...
}

// emit.zig
const clib = @import("clib_emit.zig");
// Replace self.getOrDeclareMalloc() → clib.getOrDeclareMalloc(self)
```

### What Stays in emit.zig

- Emitter struct + init/deinit
- Core dispatchers: `emitModule`, `emitStmt`, `emitExpr`, `emitBlock`
- Expression/statement emission (high fan-out, deeply recursive)
- Type management: `typeToLLVM`, `typeExprToLLVM`, `inferExprType`
- Function/method declaration + monomorphization
- String, Optional/Result, Rc/Arc operations (moderate coupling)
- ABI/struct layout

### Verification

After each phase: `./build.sh && ./run-tests.sh` — all 507 tests must pass.
