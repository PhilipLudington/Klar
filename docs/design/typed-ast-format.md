# Typed AST JSON Serialization Format

> Specification for the post-type-checking AST serialization used by the selfhost frontend to communicate with the Zig codegen backend.

**Status:** Design
**Relates to:** ROADMAP.md Phase 8.1, DESIGN.md "Generics and Traits"

---

## Overview

The typed AST format extends the existing `dump-ast` JSON format with type information produced by the checker. It serves as the interface between the selfhost frontend (lexer, parser, checker in Klar) and the Zig codegen backend.

**Key additions over `dump-ast`:**

1. Every expression carries a `resolved_type` field
2. Generic calls include the monomorphized function name
3. Method calls include trait dispatch resolution
4. Comptime expressions carry their evaluated values
5. A top-level `monomorphizations` section lists all instantiated generics

The format is consumed via `klar build --typed-ast-input <file>` (extension of existing `--ast-input`).

---

## Design Principles

1. **Superset of `dump-ast`** --- All existing `dump-ast` fields are preserved. The typed AST adds fields; it never removes or renames existing ones. A consumer that ignores the new fields gets the same untyped AST.

2. **Resolved types use a structured representation** --- Types are JSON objects with a `kind` discriminator, not opaque strings. This makes the format parseable without a type-name parser.

3. **No type variables remain** --- After type checking, all `TypeVar` references are resolved to concrete types. The `type_var` kind only appears in the `monomorphizations` section to describe the original generic signature.

4. **Monomorphization is explicit** --- Every generic instantiation is listed with its mangled name, original name, and concrete type arguments. Call sites reference the mangled name.

5. **Deterministic output** --- For a given input, the output is identical across runs. Maps are serialized in sorted key order.

---

## Top-Level Structure

```json
{
  "format": "typed-ast",
  "version": 1,
  "module_decl": <ModuleDecl> | null,
  "imports": [<ImportDecl>, ...],
  "declarations": [<TypedDecl>, ...],
  "monomorphizations": {
    "functions": [<MonomorphizedFunction>, ...],
    "structs": [<MonomorphizedStruct>, ...],
    "enums": [<MonomorphizedEnum>, ...],
    "methods": [<MonomorphizedMethod>, ...]
  },
  "trait_impls": [<TraitImpl>, ...],
  "comptime_values": {<node_id>: <ComptimeValue>, ...}
}
```

New fields vs `dump-ast`:
- `format` and `version` --- identify this as a typed AST (not plain `dump-ast`)
- `monomorphizations` --- all generic instantiations
- `trait_impls` --- all trait implementation registrations
- `comptime_values` --- evaluated comptime results keyed by source location

---

## Resolved Type Representation

Every resolved type is a JSON object with a `"kind"` discriminator. This mirrors `types.zig`'s `Type` union.

### Primitive Types

```json
{ "kind": "primitive", "name": "i32" }
{ "kind": "primitive", "name": "f64" }
{ "kind": "primitive", "name": "bool" }
{ "kind": "primitive", "name": "char" }
{ "kind": "primitive", "name": "string" }
```

Valid primitive names: `i8`, `i16`, `i32`, `i64`, `i128`, `isize`, `u8`, `u16`, `u32`, `u64`, `u128`, `usize`, `f32`, `f64`, `bool`, `char`, `string`.

### Composite Types

```json
// Array: [i32; 5]
{ "kind": "array", "element": <ResolvedType>, "size": 5 }

// Slice: [i32]
{ "kind": "slice", "element": <ResolvedType> }

// Tuple: (i32, string)
{ "kind": "tuple", "elements": [<ResolvedType>, ...] }

// Optional: ?i32
{ "kind": "optional", "inner": <ResolvedType> }

// Result: Result#[i32, Error]
{ "kind": "result", "ok_type": <ResolvedType>, "err_type": <ResolvedType> }

// Function (closure): fn(i32) -> i32
{
  "kind": "function",
  "params": [<ResolvedType>, ...],
  "return_type": <ResolvedType>,
  "is_async": false,
  "is_unsafe": false
}

// Extern function pointer: extern fn(i32) -> void
{
  "kind": "extern_fn",
  "params": [<ResolvedType>, ...],
  "return_type": <ResolvedType>
}

// Reference: ref T / inout T
{ "kind": "reference", "inner": <ResolvedType>, "mutable": false }
```

### User-Defined Types

```json
// Struct
{ "kind": "struct", "name": "Point" }

// Enum
{ "kind": "enum", "name": "Direction" }

// Trait (for dyn trait objects)
{ "kind": "trait", "name": "Drawable" }
```

For monomorphized generic types, `name` is the mangled name (e.g., `"Pair$i32$string"`).

### Collection Types

```json
{ "kind": "list", "element": <ResolvedType> }
{ "kind": "map", "key": <ResolvedType>, "value": <ResolvedType> }
{ "kind": "set", "element": <ResolvedType> }
{ "kind": "string_data" }
```

### Smart Pointer Types

```json
{ "kind": "rc", "inner": <ResolvedType> }
{ "kind": "weak_rc", "inner": <ResolvedType> }
{ "kind": "arc", "inner": <ResolvedType> }
{ "kind": "weak_arc", "inner": <ResolvedType> }
{ "kind": "cell", "inner": <ResolvedType> }
```

### Range Type

```json
{ "kind": "range", "element": <ResolvedType> }
```

### I/O Types

```json
{ "kind": "file" }
{ "kind": "io_error" }
{ "kind": "stdout_handle" }
{ "kind": "stderr_handle" }
{ "kind": "stdin_handle" }
{ "kind": "path" }
{ "kind": "buf_reader", "inner": <ResolvedType> }
{ "kind": "buf_writer", "inner": <ResolvedType> }
```

### FFI Types

```json
// Extern opaque/sized type
{ "kind": "extern_type", "name": "SDL_Window", "size": null }
{ "kind": "extern_type", "name": "SizedType", "size": 16 }

// Raw pointers
{ "kind": "cptr", "inner": <ResolvedType> }
{ "kind": "copt_ptr", "inner": <ResolvedType> }
{ "kind": "cstr" }
{ "kind": "cstr_owned" }
```

### Special Types

```json
{ "kind": "void" }
{ "kind": "never" }
{ "kind": "unknown" }
{ "kind": "error" }
```

### Context Error Type

```json
{ "kind": "context_error", "inner": <ResolvedType> }
```

### Associated Type Reference

Represents an unresolved associated type (e.g., `T.Item` where `T` is a type variable). This only appears in `monomorphizations` entries and generic function signatures --- in concrete code, it is resolved to the actual type.

```json
{
  "kind": "associated_type_ref",
  "type_var": { "kind": "type_var", "id": 0, "name": "T", "bounds": ["Iterator"] },
  "assoc_name": "Item",
  "trait_name": "Iterator"
}
```

### Applied Generic Type

In the monomorphizations section only (for describing original signatures):

```json
{ "kind": "applied", "base": <ResolvedType>, "args": [<ResolvedType>, ...] }
```

### Type Variable

Only appears in `monomorphizations` to describe original generic signatures:

```json
{
  "kind": "type_var",
  "id": 0,
  "name": "T",
  "bounds": ["Ordered", "Printable"]
}
```

---

## Typed Expressions

Every expression from `dump-ast` is extended with a `"resolved_type"` field. The field is always present on expression nodes.

### Literal

```json
{
  "kind": "literal",
  "type": "int",
  "value": "42",
  "resolved_type": { "kind": "primitive", "name": "i32" }
}
```

### Identifier

```json
{
  "kind": "identifier",
  "name": "x",
  "resolved_type": { "kind": "primitive", "name": "i32" }
}
```

### Binary

```json
{
  "kind": "binary",
  "op": "add",
  "left": <TypedExpr>,
  "right": <TypedExpr>,
  "resolved_type": { "kind": "primitive", "name": "i32" }
}
```

### Unary

```json
{
  "kind": "unary",
  "op": "negate",
  "operand": <TypedExpr>,
  "resolved_type": { "kind": "primitive", "name": "i32" }
}
```

### Postfix

```json
{
  "kind": "postfix",
  "op": "force_unwrap",
  "operand": <TypedExpr>,
  "resolved_type": { "kind": "primitive", "name": "i32" }
}
```

### Call (Non-Generic)

```json
{
  "kind": "call",
  "callee": <TypedExpr>,
  "args": [<TypedExpr>, ...],
  "type_args": null,
  "resolved_type": { "kind": "primitive", "name": "i32" },
  "resolved_callee": null
}
```

### Call (Generic --- Monomorphized)

```json
{
  "kind": "call",
  "callee": { "kind": "identifier", "name": "max", "resolved_type": ... },
  "args": [<TypedExpr>, ...],
  "type_args": [{ "kind": "named", "name": "i32" }],
  "resolved_type": { "kind": "primitive", "name": "i32" },
  "resolved_callee": "max$i32"
}
```

The `resolved_callee` is the mangled name from the `monomorphizations.functions` table. This is `null` for non-generic calls.

### Index

```json
{
  "kind": "index",
  "object": <TypedExpr>,
  "index": <TypedExpr>,
  "resolved_type": { "kind": "primitive", "name": "i32" }
}
```

### Field Access

```json
{
  "kind": "field",
  "object": <TypedExpr>,
  "field_name": "x",
  "resolved_type": { "kind": "primitive", "name": "f64" }
}
```

### Method Call (Direct)

```json
{
  "kind": "method_call",
  "object": <TypedExpr>,
  "method_name": "len",
  "type_args": null,
  "args": [],
  "resolved_type": { "kind": "primitive", "name": "i32" },
  "resolved_dispatch": null
}
```

### Method Call (Trait Dispatch)

When a method is called through a trait bound on a generic type parameter, `resolved_dispatch` provides the concrete resolution:

```json
{
  "kind": "method_call",
  "object": <TypedExpr>,
  "method_name": "compare",
  "type_args": null,
  "args": [<TypedExpr>],
  "resolved_type": { "kind": "enum", "name": "Ordering" },
  "resolved_dispatch": {
    "trait_name": "Ordered",
    "impl_type": "i32",
    "method_name": "compare"
  }
}
```

`resolved_dispatch` is `null` for direct method calls (non-trait). For generic methods on generic structs, a `resolved_method` field provides the mangled method name:

```json
{
  "kind": "method_call",
  "object": <TypedExpr>,
  "method_name": "get_first",
  "type_args": null,
  "args": [],
  "resolved_type": { "kind": "primitive", "name": "i32" },
  "resolved_dispatch": null,
  "resolved_method": "Pair$i32$string_get_first"
}
```

### Closure

```json
{
  "kind": "closure",
  "params": [{ "name": "x", "type": <TypeExpr> }],
  "return_type": <TypeExpr>,
  "body": <TypedExpr>,
  "resolved_type": {
    "kind": "function",
    "params": [{ "kind": "primitive", "name": "i32" }],
    "return_type": { "kind": "primitive", "name": "i32" },
    "is_async": false,
    "is_unsafe": false
  },
  "captures": [
    { "name": "factor", "is_mutable": false, "type": { "kind": "primitive", "name": "i32" } }
  ]
}
```

The `captures` field (populated by the checker) lists variables captured from enclosing scopes. `null` if no captures.

### Range

```json
{
  "kind": "range",
  "inclusive": false,
  "start": <TypedExpr> | null,
  "end": <TypedExpr> | null,
  "resolved_type": { "kind": "range", "element": { "kind": "primitive", "name": "i32" } }
}
```

### Struct Literal

```json
{
  "kind": "struct_literal",
  "type_name": <TypeExpr> | null,
  "fields": [{ "name": "x", "value": <TypedExpr> }, ...],
  "spread": <TypedExpr> | null,
  "resolved_type": { "kind": "struct", "name": "Point" }
}
```

### Array Literal

```json
{
  "kind": "array_literal",
  "elements": [<TypedExpr>, ...],
  "resolved_type": { "kind": "array", "element": { "kind": "primitive", "name": "i32" }, "size": 3 }
}
```

### Tuple Literal

```json
{
  "kind": "tuple_literal",
  "elements": [<TypedExpr>, ...],
  "resolved_type": {
    "kind": "tuple",
    "elements": [
      { "kind": "primitive", "name": "i32" },
      { "kind": "primitive", "name": "string" }
    ]
  }
}
```

### Type Cast

```json
{
  "kind": "type_cast",
  "truncating": false,
  "target_type": <TypeExpr>,
  "expr": <TypedExpr>,
  "resolved_type": { "kind": "primitive", "name": "i64" },
  "source_is_signed": true
}
```

### Grouped

```json
{
  "kind": "grouped",
  "expr": <TypedExpr>,
  "resolved_type": <ResolvedType>
}
```

### Interpolated String

```json
{
  "kind": "interpolated_string",
  "parts": [
    { "kind": "string", "value": "Hello, " },
    { "kind": "expr", "value": <TypedExpr> }
  ],
  "resolved_type": { "kind": "primitive", "name": "string" }
}
```

### Enum Literal

```json
{
  "kind": "enum_literal",
  "enum_type": <TypeExpr>,
  "variant_name": "Some",
  "payload": [<TypedExpr>],
  "resolved_type": { "kind": "optional", "inner": { "kind": "primitive", "name": "i32" } }
}
```

### Comptime Block

```json
{
  "kind": "comptime_block",
  "body": <Block>,
  "resolved_type": <ResolvedType>,
  "comptime_value": <ComptimeValue> | null
}
```

### Builtin Call

```json
{
  "kind": "builtin_call",
  "name": "typeName",
  "args": [{ "kind": "type", "value": <TypeExpr> }],
  "resolved_type": { "kind": "primitive", "name": "string" },
  "comptime_value": { "kind": "string", "value": "i32" }
}
```

The `comptime_value` field holds the result for builtins evaluated at compile time:

```json
// String result
{ "kind": "string", "value": "i32" }

// Bool result
{ "kind": "bool", "value": true }

// Int result
{ "kind": "int", "value": 4 }

// Repeat info (@repeat)
{ "kind": "repeat", "value_expr": <TypedExpr>, "count": 256 }
```

### Unsafe Block

```json
{
  "kind": "unsafe_block",
  "body": <Block>,
  "resolved_type": <ResolvedType>
}
```

### Out Arg

```json
{
  "kind": "out_arg",
  "name": "result",
  "resolved_type": { "kind": "reference", "inner": <ResolvedType>, "mutable": true }
}
```

### Block Expression

```json
{
  "kind": "block",
  "body": {
    "statements": [<TypedStmt>, ...],
    "final_expr": <TypedExpr> | null
  },
  "resolved_type": <ResolvedType>
}
```

---

## Typed Statements

Statements do not have a `resolved_type` (they are not expressions). They are identical to the `dump-ast` format, except their child expressions carry `resolved_type`.

### Let / Var Declaration

```json
{
  "kind": "let_decl",
  "name": "x",
  "is_shadow": false,
  "type": <TypeExpr>,
  "value": <TypedExpr>,
  "resolved_var_type": <ResolvedType>
}
```

The `resolved_var_type` is the checker's resolved type for the variable. This may differ from the syntactic `type` field when implicit conversions apply (e.g., `T` to `?T`).

### Assignment

```json
{
  "kind": "assignment",
  "op": "assign",
  "target": <TypedExpr>,
  "value": <TypedExpr>
}
```

### Expression Statement

```json
{
  "kind": "expr_stmt",
  "expr": <TypedExpr>
}
```

### Return

```json
{
  "kind": "return_stmt",
  "value": <TypedExpr> | null
}
```

### Break / Continue

```json
{ "kind": "break_stmt", "value": <TypedExpr> | null }
{ "kind": "continue_stmt" }
```

### For Loop

```json
{
  "kind": "for_loop",
  "pattern": <Pattern>,
  "iterable": <TypedExpr>,
  "body": <Block>,
  "resolved_element_type": <ResolvedType>
}
```

The `resolved_element_type` is the type of the loop variable (element type of the iterable).

### While Loop

```json
{
  "kind": "while_loop",
  "condition": <TypedExpr>,
  "body": <Block>
}
```

### Loop

```json
{
  "kind": "loop_stmt",
  "body": <Block>
}
```

### If Statement

```json
{
  "kind": "if_stmt",
  "condition": <TypedExpr>,
  "then_branch": <Block>,
  "else_branch": <ElseBranch> | null
}
```

### Match Statement

```json
{
  "kind": "match_stmt",
  "subject": <TypedExpr>,
  "arms": [
    {
      "pattern": <Pattern>,
      "guard": <TypedExpr> | null,
      "body": <Block>,
      "resolved_bindings": [
        { "name": "x", "type": <ResolvedType> }
      ]
    }
  ]
}
```

The `resolved_bindings` field lists the types of all pattern-bound variables in each arm.

---

## Typed Declarations

Declarations extend the `dump-ast` format with resolved type information.

### Function Declaration

```json
{
  "kind": "function",
  "name": "add",
  "is_pub": false,
  "is_async": false,
  "is_comptime": false,
  "is_unsafe": false,
  "is_extern": false,
  "is_variadic": false,
  "type_params": [],
  "params": [
    {
      "name": "a",
      "type": { "kind": "named", "name": "i32" },
      "is_comptime": false,
      "is_out": false,
      "default_value": null
    }
  ],
  "return_type": { "kind": "named", "name": "i32" },
  "where_clause": null,
  "body": <Block>,
  "resolved_signature": {
    "params": [{ "kind": "primitive", "name": "i32" }],
    "return_type": { "kind": "primitive", "name": "i32" }
  }
}
```

For generic functions, the `resolved_signature` contains the uninstantiated signature with type variables. The concrete instantiations are in `monomorphizations.functions`.

### Struct Declaration

```json
{
  "kind": "struct_decl",
  "name": "Point",
  "is_pub": false,
  "is_extern": false,
  "is_packed": false,
  "type_params": [],
  "fields": [
    { "name": "x", "type": <TypeExpr>, "is_pub": false }
  ],
  "traits": [],
  "resolved_fields": [
    { "name": "x", "type": { "kind": "primitive", "name": "f64" }, "is_pub": false }
  ],
  "is_copy": false
}
```

### Enum Declaration

```json
{
  "kind": "enum_decl",
  "name": "Direction",
  "is_pub": false,
  "is_extern": false,
  "type_params": [],
  "repr_type": null,
  "variants": [
    {
      "name": "North",
      "value": null,
      "payload": null
    }
  ],
  "resolved_variants": [
    {
      "name": "North",
      "value": null,
      "payload": null
    },
    {
      "name": "Some",
      "value": null,
      "payload": { "kind": "tuple", "types": [{ "kind": "primitive", "name": "i32" }] }
    }
  ]
}
```

### Trait Declaration

```json
{
  "kind": "trait_decl",
  "name": "Ordered",
  "is_pub": false,
  "is_unsafe": false,
  "type_params": [],
  "super_traits": [],
  "associated_types": [
    {
      "name": "Item",
      "bounds": [],
      "default": null
    }
  ],
  "methods": [<FunctionDecl>, ...],
  "resolved_methods": [
    {
      "name": "compare",
      "signature": {
        "params": [<ResolvedType>, <ResolvedType>],
        "return_type": <ResolvedType>
      },
      "has_default": false
    }
  ]
}
```

### Impl Declaration

```json
{
  "kind": "impl_decl",
  "is_unsafe": false,
  "type_params": [],
  "target_type": <TypeExpr>,
  "trait_type": <TypeExpr> | null,
  "associated_types": [
    { "name": "Item", "value": <TypeExpr> }
  ],
  "where_clause": null,
  "methods": [<FunctionDecl>, ...],
  "resolved_target": <ResolvedType>,
  "resolved_trait": <ResolvedType> | null,
  "resolved_associated_types": [
    { "name": "Item", "type": <ResolvedType> }
  ]
}
```

### Type Alias

```json
{
  "kind": "type_alias",
  "name": "UserId",
  "is_pub": false,
  "type_params": [],
  "target": <TypeExpr>,
  "resolved_target": <ResolvedType>
}
```

### Const Declaration

```json
{
  "kind": "const_decl",
  "name": "MAX_SIZE",
  "is_pub": false,
  "type": <TypeExpr> | null,
  "value": <TypedExpr>,
  "resolved_type": <ResolvedType>
}
```

### Test Declaration

```json
{
  "kind": "test_decl",
  "name": "test_addition",
  "body": <Block>
}
```

### Import / Module / Extern Declarations

These are unchanged from `dump-ast` --- they carry no type information:

```json
// Import
{ "kind": "import_decl", "data": { "path": [...], "items": ..., "alias": ... } }

// Module
{ "kind": "module_decl", "data": { "path": [...] } }

// Extern type
{ "kind": "extern_type_decl", "name": "SDL_Window", "is_pub": false, "size": null }

// Extern block
{ "kind": "extern_block", "functions": [<FunctionDecl>, ...] }
```

---

## Patterns

Patterns are unchanged from `dump-ast`. The resolved types of pattern-bound variables appear in the `resolved_bindings` field of match arms and `resolved_var_type` / `resolved_element_type` on declarations/loops.

### Pattern Kinds

| Kind | Fields |
|------|--------|
| `wildcard` | (none) |
| `literal` | `type`, `value` |
| `binding` | `name`, `mutable`, `type_annotation` |
| `variant` | `type_expr`, `variant_name`, `payload` |
| `struct_pattern` | `type_name`, `fields` |
| `tuple_pattern` | `elements` |
| `or_pattern` | `alternatives` |
| `guarded` | `pattern`, `guard` |

---

## Type Expressions (Syntactic)

Type expressions (`TypeExpr`) are the syntactic type annotations as written in source code. They are **unchanged** from `dump-ast`. The resolved types appear separately in `resolved_type` / `resolved_*` fields.

### TypeExpr Kinds

| Kind | Fields |
|------|--------|
| `named` | `name` |
| `array` | `element`, `size` |
| `slice` | `element` |
| `tuple` | `elements` |
| `optional` | `inner` |
| `result` | `ok_type`, `err_type` |
| `function` | `params`, `return_type` |
| `extern_function` | `params`, `return_type` |
| `reference` | `inner`, `mutable` |
| `generic_apply` | `base`, `args` |
| `qualified` | `base`, `member` |

---

## Monomorphizations Section

Lists all generic instantiations discovered during type checking.

### Monomorphized Function

```json
{
  "original_name": "max",
  "mangled_name": "max$i32",
  "type_args": [{ "kind": "primitive", "name": "i32" }],
  "concrete_signature": {
    "params": [{ "kind": "primitive", "name": "i32" }, { "kind": "primitive", "name": "i32" }],
    "return_type": { "kind": "primitive", "name": "i32" }
  }
}
```

### Monomorphized Struct

```json
{
  "original_name": "Pair",
  "mangled_name": "Pair$i32$string",
  "type_args": [
    { "kind": "primitive", "name": "i32" },
    { "kind": "primitive", "name": "string" }
  ],
  "concrete_fields": [
    { "name": "first", "type": { "kind": "primitive", "name": "i32" }, "is_pub": false },
    { "name": "second", "type": { "kind": "primitive", "name": "string" }, "is_pub": false }
  ]
}
```

### Monomorphized Enum

```json
{
  "original_name": "Option",
  "mangled_name": "Option$i32",
  "type_args": [{ "kind": "primitive", "name": "i32" }],
  "concrete_variants": [
    { "name": "Some", "payload": { "kind": "tuple", "types": [{ "kind": "primitive", "name": "i32" }] } },
    { "name": "None", "payload": null }
  ]
}
```

### Monomorphized Method

```json
{
  "struct_name": "Pair",
  "method_name": "get_first",
  "mangled_name": "Pair$i32$string_get_first",
  "type_args": [
    { "kind": "primitive", "name": "i32" },
    { "kind": "primitive", "name": "string" }
  ],
  "concrete_signature": {
    "params": [{ "kind": "struct", "name": "Pair$i32$string" }],
    "return_type": { "kind": "primitive", "name": "i32" }
  }
}
```

---

## Trait Implementations Section

Lists all trait implementations registered during type checking.

```json
{
  "impl_type": "i32",
  "trait_name": "Ordered",
  "associated_types": [],
  "methods": [
    {
      "name": "compare",
      "signature": {
        "params": [
          { "kind": "primitive", "name": "i32" },
          { "kind": "primitive", "name": "i32" }
        ],
        "return_type": { "kind": "enum", "name": "Ordering" }
      }
    }
  ]
}
```

---

## Meta Annotations

Meta annotations are serialized identically to `dump-ast`. They carry no type information and pass through unchanged. See the existing `dump-ast` meta format for details.

---

## Complete Example

Given this Klar source:

```klar
struct Point { x: f64, y: f64 }

fn max#[T: Ordered](a: T, b: T) -> T {
    if a > b {
        return a
    }
    return b
}

fn main() -> i32 {
    let p: Point = Point { x: 1.0, y: 2.0 }
    let m: i32 = max#[i32](3, 7)
    return m
}
```

The typed AST (abbreviated) would be:

```json
{
  "format": "typed-ast",
  "version": 1,
  "module_decl": null,
  "imports": [],
  "declarations": [
    {
      "kind": "struct_decl",
      "name": "Point",
      "is_pub": false,
      "is_extern": false,
      "is_packed": false,
      "type_params": [],
      "fields": [
        { "name": "x", "type": { "kind": "named", "name": "f64" }, "is_pub": false },
        { "name": "y", "type": { "kind": "named", "name": "f64" }, "is_pub": false }
      ],
      "traits": [],
      "resolved_fields": [
        { "name": "x", "type": { "kind": "primitive", "name": "f64" }, "is_pub": false },
        { "name": "y", "type": { "kind": "primitive", "name": "f64" }, "is_pub": false }
      ],
      "is_copy": false
    },
    {
      "kind": "function",
      "name": "max",
      "is_pub": false,
      "is_async": false,
      "is_comptime": false,
      "is_unsafe": false,
      "is_extern": false,
      "is_variadic": false,
      "type_params": [{ "name": "T", "bounds": [{ "kind": "named", "name": "Ordered" }] }],
      "params": [
        { "name": "a", "type": { "kind": "named", "name": "T" }, "is_comptime": false, "is_out": false, "default_value": null },
        { "name": "b", "type": { "kind": "named", "name": "T" }, "is_comptime": false, "is_out": false, "default_value": null }
      ],
      "return_type": { "kind": "named", "name": "T" },
      "where_clause": null,
      "body": { "statements": ["..."], "final_expr": null },
      "resolved_signature": {
        "params": [
          { "kind": "type_var", "id": 0, "name": "T", "bounds": ["Ordered"] },
          { "kind": "type_var", "id": 0, "name": "T", "bounds": ["Ordered"] }
        ],
        "return_type": { "kind": "type_var", "id": 0, "name": "T", "bounds": ["Ordered"] }
      }
    },
    {
      "kind": "function",
      "name": "main",
      "is_pub": false,
      "is_async": false,
      "is_comptime": false,
      "is_unsafe": false,
      "is_extern": false,
      "is_variadic": false,
      "type_params": [],
      "params": [],
      "return_type": { "kind": "named", "name": "i32" },
      "where_clause": null,
      "body": {
        "statements": [
          {
            "kind": "let_decl",
            "name": "p",
            "is_shadow": false,
            "type": { "kind": "named", "name": "Point" },
            "value": {
              "kind": "struct_literal",
              "type_name": { "kind": "named", "name": "Point" },
              "fields": [
                { "name": "x", "value": { "kind": "literal", "type": "float", "value": 1.0, "resolved_type": { "kind": "primitive", "name": "f64" } } },
                { "name": "y", "value": { "kind": "literal", "type": "float", "value": 2.0, "resolved_type": { "kind": "primitive", "name": "f64" } } }
              ],
              "spread": null,
              "resolved_type": { "kind": "struct", "name": "Point" }
            },
            "resolved_var_type": { "kind": "struct", "name": "Point" }
          },
          {
            "kind": "let_decl",
            "name": "m",
            "is_shadow": false,
            "type": { "kind": "named", "name": "i32" },
            "value": {
              "kind": "call",
              "callee": { "kind": "identifier", "name": "max", "resolved_type": { "kind": "primitive", "name": "i32" } },
              "args": [
                { "kind": "literal", "type": "int", "value": "3", "resolved_type": { "kind": "primitive", "name": "i32" } },
                { "kind": "literal", "type": "int", "value": "7", "resolved_type": { "kind": "primitive", "name": "i32" } }
              ],
              "type_args": [{ "kind": "named", "name": "i32" }],
              "resolved_type": { "kind": "primitive", "name": "i32" },
              "resolved_callee": "max$i32"
            },
            "resolved_var_type": { "kind": "primitive", "name": "i32" }
          }
        ],
        "final_expr": null
      },
      "resolved_signature": {
        "params": [],
        "return_type": { "kind": "primitive", "name": "i32" }
      }
    }
  ],
  "monomorphizations": {
    "functions": [
      {
        "original_name": "max",
        "mangled_name": "max$i32",
        "type_args": [{ "kind": "primitive", "name": "i32" }],
        "concrete_signature": {
          "params": [{ "kind": "primitive", "name": "i32" }, { "kind": "primitive", "name": "i32" }],
          "return_type": { "kind": "primitive", "name": "i32" }
        }
      }
    ],
    "structs": [],
    "enums": [],
    "methods": []
  },
  "trait_impls": [],
  "comptime_values": {}
}
```

---

## Node Type Coverage

The following table lists every AST node type and its typed AST additions:

### Expressions (18 kinds)

| Expression Kind | `resolved_type` | Additional Typed Fields |
|----------------|-----------------|------------------------|
| `literal` | yes | --- |
| `identifier` | yes | --- |
| `binary` | yes | --- |
| `unary` | yes | --- |
| `postfix` | yes | --- |
| `call` | yes | `resolved_callee` (mangled name or null) |
| `index` | yes | --- |
| `field` | yes | --- |
| `method_call` | yes | `resolved_dispatch`, `resolved_method` |
| `block` | yes | --- |
| `closure` | yes | `captures` (from checker) |
| `range` | yes | --- |
| `struct_literal` | yes | --- |
| `array_literal` | yes | --- |
| `tuple_literal` | yes | --- |
| `type_cast` | yes | `source_is_signed` (from checker) |
| `grouped` | yes | --- |
| `interpolated_string` | yes | --- |
| `enum_literal` | yes | --- |
| `comptime_block` | yes | `comptime_value` |
| `builtin_call` | yes | `comptime_value` |
| `unsafe_block` | yes | --- |
| `out_arg` | yes | --- |

### Statements (12 kinds)

| Statement Kind | Additional Typed Fields |
|---------------|------------------------|
| `let_decl` | `resolved_var_type` |
| `var_decl` | `resolved_var_type` |
| `assignment` | --- |
| `expr_stmt` | --- |
| `return_stmt` | --- |
| `break_stmt` | --- |
| `continue_stmt` | --- |
| `for_loop` | `resolved_element_type` |
| `while_loop` | --- |
| `loop_stmt` | --- |
| `if_stmt` | --- |
| `match_stmt` | `resolved_bindings` per arm |

### Declarations (12 kinds)

| Declaration Kind | Additional Typed Fields |
|-----------------|------------------------|
| `function` | `resolved_signature` |
| `test_decl` | --- |
| `struct_decl` | `resolved_fields`, `is_copy` |
| `enum_decl` | `resolved_variants` |
| `trait_decl` | `resolved_methods` |
| `impl_decl` | `resolved_target`, `resolved_trait`, `resolved_associated_types` |
| `type_alias` | `resolved_target` |
| `const_decl` | `resolved_type` |
| `import_decl` | --- |
| `module_decl` | --- |
| `extern_type_decl` | --- |
| `extern_block` | --- |

### Patterns (8 kinds)

Patterns are unchanged from `dump-ast`. Resolved types flow through `resolved_bindings` on match arms and `resolved_var_type` / `resolved_element_type` on declarations.

### Type Expressions (11 kinds)

Type expressions are unchanged from `dump-ast`. They represent the syntactic annotations as written in source. Resolved types are separate fields.

---

## Validation Rules

A valid typed AST JSON file must satisfy:

1. **`format` is `"typed-ast"` and `version` is `1`.**

2. **Every expression node has a `resolved_type` field** that is a valid ResolvedType object (not null, not missing).

3. **No `type_var` kind appears in `resolved_type` fields** of expressions, declarations, or statement type annotations. Type variables are only valid in `monomorphizations` entries and `resolved_signature` of generic function declarations.

4. **Every `resolved_callee` value** in a call expression corresponds to a `mangled_name` in `monomorphizations.functions`.

5. **Every `resolved_method` value** in a method call expression corresponds to a `mangled_name` in `monomorphizations.methods`.

6. **Every `resolved_dispatch`** references a valid `trait_name` that appears in `trait_impls` and an `impl_type` that has an implementation for that trait.

7. **All `monomorphizations` entries** have non-empty `type_args` arrays and valid mangled names.

8. **ResolvedType `kind` values** are from the closed set defined in this spec.

9. **Source locations (line/column) are preserved** from the original AST for error reporting.

---

## Versioning

The `version` field allows future evolution:

- **Version 1** (this document): Initial format.
- Future versions may add fields but will not remove or rename existing ones.
- Consumers should ignore unknown fields for forward compatibility.
- The version number increments only for breaking changes.
