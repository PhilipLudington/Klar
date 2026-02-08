# Project Bugs

## [x] Bug 1: Compiler memory leak during native codegen of json_parser.kl

**Status:** Fixed

**Description:** The Klar compiler leaks memory during native compilation (`klar build`) of `examples/apps/json_parser.kl`. The leak originates in `resolveTypeExprDirect` within the LLVM codegen emitter. The compiled binary itself runs correctly — this is a compiler-internal leak, not a runtime issue.

**Steps to reproduce:**
1. Run `klar build examples/apps/json_parser.kl -o build/json_parser`
2. Observe the `error(gpa): memory address ... leaked` message in stderr

**Expected:** Clean build with no memory leak diagnostics.

**Actual:** Build succeeds but reports a leaked allocation:
```
error(gpa): memory address 0x104dc7f00 leaked:
???:?:?: ... in _codegen.emit.Emitter.resolveTypeExprDirect (???)
???:?:?: ... in _codegen.emit.Emitter.emitStmt (???)
???:?:?: ... in _codegen.emit.Emitter.emitBlock (???)
???:?:?: ... in _codegen.emit.Emitter.emitFunction (???)
???:?:?: ... in _codegen.emit.Emitter.emitModule (???)
???:?:?: ... in _main.buildNative (???)
```

**Notes:** Likely related to codegen handling of recursive enums with generics (`JsonValue` containing `List[JsonValue]` and `Map[string, JsonValue]`). The leak is in `resolveTypeExprDirect`, suggesting a type resolution result is allocated but not freed during statement emission.

**Fix:** Added `resolved_type_allocs` tracking list to `Emitter` to track heap allocations made by `resolveTypeExprDirect` (for `*Type`, `*ResultType`, `*CptrType`, `*CoptPtrType`). All tracked allocations are freed in `deinit()`.
