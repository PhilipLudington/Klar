# Resume: Fix Two Compiler Bugs (Sign-Extension + Literal Typing)

## Status: Research Complete, Implementation Not Started

All relevant code has been read and analyzed. No edits have been made yet.

---

## Bug 1: u8 → u32 Sign-Extension

### Problem
`.as#[u32]` on u8 values ≥ 128 sign-extends instead of zero-extending.
`128.as#[u8].as#[u32]` → `0xFFFFFF80` instead of `0x00000080`.

### Root Cause
`src/codegen/emit.zig:4848` — `emitTypeCast` calls `self.isSignedIntType(cast.expr)` (actually `isExprSigned` at line 3670). That function only handles `.identifier`, `.type_cast`, `.grouped`, `.unary`, `.binary` — everything else defaults to `true` (signed). So expressions like `list.get(i)!` fall through → sext.

### Fix (3 files)

**1. `src/ast.zig:330-335` — Add field to TypeCast**
```zig
pub const TypeCast = struct {
    expr: Expr,
    target_type: TypeExpr,
    truncating: bool,
    source_is_signed: bool = true,  // NEW: default signed for safety
    span: Span,
};
```

**2. `src/checker/expressions.zig:815-834` — Annotate in checkTypeCast**
After line 816 (`const expr_type = checkExpr(tc, cast.expr);`), add:
```zig
cast.source_is_signed = expr_type.isSigned();
```

**3. `src/codegen/emit.zig:4848` — Use annotation**
Change:
```zig
const is_signed = self.isSignedIntType(cast.expr);  // OLD (wrong name in plan; actual: isExprSigned)
```
To:
```zig
const is_signed = cast.source_is_signed;
```

### Key Finding: method_calls.zig does NOT need changes
`.as#[T]` in `method_calls.zig:505-523` only returns the target type — the parser creates `.type_cast` AST nodes for `.as#[T]` syntax. The TypeCast annotation flows through the normal `checkTypeCast` path.

### Supporting Code Locations
- `Type.isSigned()` — `src/types.zig:191` (works on Type union)
- `Primitive.isSigned()` — `src/types.zig:325` (u8_, u16_, u32_, u64_ → false)
- `isExprSigned()` — `src/codegen/emit.zig:3670` (the buggy fallback function)
- `emitTypeCast` integer widening — `src/codegen/emit.zig:4842-4858`

---

## Bug 2: Integer Literal Type Inference in List.push()

### Problem
`List#[u32].push(42)` fails because `42` defaults to i32. Requires `.as#[u32]` on every constant.

### Root Cause
`src/checker/method_calls.zig:1291` — `tc.checkExpr(method.args[0])` has no type hint. `checkExprWithHint` exists and handles integer literal typing.

### Fix (1 file)

**`src/checker/method_calls.zig:1291`**
Change:
```zig
const arg_type = tc.checkExpr(method.args[0]);
```
To:
```zig
const arg_type = expressions.checkExprWithHint(tc, method.args[0], element_type);
```

Note: Need to check how `expressions` is imported in method_calls.zig — it likely uses `@import("expressions.zig")` or calls through `tc`.

### Supporting Code Locations
- `checkExprWithHint` — `src/checker/expressions.zig:224` (pub fn, takes optional Type hint)
- `checkExpr` — `src/checker/expressions.zig:218` (just calls `checkExprWithHint(tc, expr, null)`)
- List push check — `src/checker/method_calls.zig:1286-1298`
- `element_type` available at line 1283 (`list_type.element`)

---

## Post-Fix: Clean Up sha256.kl Workarounds

After both bugs are fixed, remove workarounds in `stdlib/sha256.kl`:
- Remove `& byte_mask` / `& 255` masks after `.as#[u32]` conversions
- Remove `.as#[u32]` on integer constant literals passed to `List#[u32].push()`

---

## Verification Steps

1. `./run-build.sh` — compiler builds
2. Write `scratch/u8_cast_test.kl`: `let x: u8 = 200; let y: u32 = x.as#[u32]; assert y == 200`
3. Write `scratch/push_literal_test.kl`: `var k: List#[u32] = List.new#[u32](); k.push(42); k.push(3000000000)`
4. Clean up `stdlib/sha256.kl` and re-run: `/klar-run scratch/sha256_verify.kl`
5. `./run-tests.sh` — all tests pass (zero regressions)
