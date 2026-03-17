# Typed AST JSON Serialization Format

## Overview

The typed AST format extends the existing `dump-ast` JSON format with type annotations produced by the type checker. This enables the selfhost frontend (lexer → parser → checker) to emit a fully-typed AST that the Zig backend can consume directly for LLVM codegen, **bypassing the Zig type checker entirely**.

### Pipeline

```
Current:   selfhost parser → JSON AST → Zig checker → Zig codegen → binary
Phase 8:   selfhost parser → selfhost checker → typed AST JSON → Zig codegen → binary
```

### Design Principles

1. **Superset of dump-ast** — A typed AST is valid dump-ast JSON with additional fields. Fields added by the checker are always optional; their absence means "not type-annotated."
2. **Node IDs for side tables** — Every expression and statement node gets a unique `"id"` integer. Side tables (call resolutions, comptime values) reference nodes by ID.
3. **Concrete types only** — After type checking and monomorphization, no type variables remain. All types are fully resolved.
4. **Mangled names are canonical** — Monomorphized functions/structs/enums use mangled names (e.g., `"identity$i32"`) as their unique identifiers.

---

## Type Representation

All type annotations use a JSON object with a `"kind"` discriminator:

```json
// Primitives
{"kind": "i32"}
{"kind": "i64"}
{"kind": "f64"}
{"kind": "bool"}
{"kind": "string"}
{"kind": "char"}
{"kind": "u8"}
{"kind": "void"}
{"kind": "never"}

// Composite types
{"kind": "array", "element": <type>, "size": <int>}
{"kind": "slice", "element": <type>}
{"kind": "tuple", "elements": [<type>, ...]}
{"kind": "optional", "inner": <type>}
{"kind": "result", "ok": <type>, "err": <type>}
{"kind": "function", "params": [<type>, ...], "return": <type>, "is_async": false}
{"kind": "reference", "inner": <type>, "mutable": false}

// User-defined types (after monomorphization, always concrete)
{"kind": "struct", "name": "Point"}
{"kind": "struct", "name": "Pair$i32$string"}
{"kind": "enum", "name": "Color"}
{"kind": "enum", "name": "Option$i32"}

// Collection types
{"kind": "list", "element": <type>}
{"kind": "map", "key": <type>, "value": <type>}
{"kind": "set", "element": <type>}
{"kind": "String"}

// Smart pointers
{"kind": "rc", "inner": <type>}
{"kind": "arc", "inner": <type>}
{"kind": "weak_rc", "inner": <type>}
{"kind": "weak_arc", "inner": <type>}
{"kind": "cell", "inner": <type>}

// Range type
{"kind": "range", "element": <type>}

// I/O types
{"kind": "file"}
{"kind": "io_error"}

// FFI types
{"kind": "cptr", "inner": <type>}
{"kind": "copt_ptr", "inner": <type>}
{"kind": "cstr"}
{"kind": "extern_fn", "params": [<type>, ...], "return": <type>}
{"kind": "extern_type", "name": "SomeCStruct"}
```

---

## Top-Level Document Structure

```json
{
  "format": "typed-ast",
  "version": 1,

  "module_decl": <module_decl | null>,
  "imports": [<import_decl>, ...],
  "declarations": [<declaration>, ...],

  "monomorphized_functions": [<mono_function>, ...],
  "monomorphized_structs": [<mono_struct>, ...],
  "monomorphized_enums": [<mono_enum>, ...],
  "monomorphized_methods": [<mono_method>, ...],

  "call_resolutions": { "<node_id>": "<mangled_name>", ... },
  "comptime_values": { "<node_id>": <comptime_value>, ... },
  "error_conversions": { "<node_id>": <error_conversion>, ... },
  "debug_call_types": { "<node_id>": <type>, ... }
}
```

The `"format"` and `"version"` fields distinguish this from plain dump-ast output and allow the Zig backend to select the correct parsing path.

**Note:** Only calls that resolve to monomorphized or method targets appear in `call_resolutions`; direct non-generic function calls are inferred from the callee name.

---

## Node IDs

Every expression and statement node MAY include an `"id"` field (integer). IDs are unique within a document and are used as keys in the side tables (`call_resolutions`, `comptime_values`, etc.).

```json
{
  "kind": "call",
  "id": 42,
  "callee": {"kind": "identifier", "name": "identity", "id": 41},
  "type_args": [{"kind": "named", "name": "i32"}],
  "args": [{"kind": "literal", "type": "int", "value": "5", "id": 43}]
}
```

Only nodes referenced by side tables strictly need IDs, but the emitter MAY assign IDs to all nodes for simplicity.

---

## Expression Nodes (Extended)

Expression nodes gain an optional `"resolved_type"` field containing the post-type-checking type:

```json
{
  "kind": "binary",
  "id": 10,
  "op": "add",
  "left": {"kind": "literal", "type": "int", "value": "10", "id": 11, "resolved_type": {"kind": "i32"}},
  "right": {"kind": "literal", "type": "int", "value": "20", "id": 12, "resolved_type": {"kind": "i32"}},
  "resolved_type": {"kind": "i32"}
}
```

### Call Expressions

Call nodes include `"resolved_type"` for the return type. The actual target function (after monomorphization) is found in the `call_resolutions` side table:

```json
{
  "kind": "call",
  "id": 42,
  "callee": {"kind": "identifier", "name": "identity"},
  "type_args": [{"kind": "named", "name": "i32"}],
  "args": [{"kind": "literal", "type": "int", "value": "5", "id": 43}],
  "resolved_type": {"kind": "i32"}
}
```

Side table entry: `"call_resolutions": {"42": "identity$i32"}`

### Method Call Expressions

Method calls include the resolved method's mangled name:

```json
{
  "kind": "method_call",
  "id": 50,
  "object": {"kind": "identifier", "name": "p", "resolved_type": {"kind": "struct", "name": "Point"}},
  "method_name": "sum",
  "args": [],
  "resolved_type": {"kind": "i32"}
}
```

Side table entry: `"call_resolutions": {"50": "Point_sum"}` (or the full mangled name for generic impls).

### Struct Literals

```json
{
  "kind": "struct_literal",
  "id": 60,
  "name": "Point",
  "fields": [
    {"name": "x", "value": {"kind": "literal", "type": "int", "value": "1", "resolved_type": {"kind": "i32"}}},
    {"name": "y", "value": {"kind": "literal", "type": "int", "value": "2", "resolved_type": {"kind": "i32"}}}
  ],
  "resolved_type": {"kind": "struct", "name": "Point"}
}
```

For generic struct literals, the `name` is the mangled name: `"Pair$i32$string"`.

### Enum Literals

```json
{
  "kind": "enum_literal",
  "id": 70,
  "enum_type": "Color",
  "variant": "Red",
  "payload": null,
  "resolved_type": {"kind": "enum", "name": "Color"}
}
```

For `Some(42)`:
```json
{
  "kind": "enum_literal",
  "id": 71,
  "enum_type": "Option",
  "variant": "Some",
  "payload": {"kind": "literal", "type": "int", "value": "42", "resolved_type": {"kind": "i32"}},
  "resolved_type": {"kind": "enum", "name": "Option$i32"}
}
```

---

## Declaration Nodes

Declarations retain the same structure as dump-ast but include resolved types where applicable.

### Function Declarations

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
    {"name": "a", "type": {"kind": "named", "name": "i32"}, "is_comptime": false, "is_out": false, "default_value": null},
    {"name": "b", "type": {"kind": "named", "name": "i32"}, "is_comptime": false, "is_out": false, "default_value": null}
  ],
  "return_type": {"kind": "named", "name": "i32"},
  "where_clause": null,
  "body": { ... }
}
```

Generic function declarations (templates) are kept as-is. Their concrete instantiations appear in `monomorphized_functions`.

### Variable Declarations (let/var)

```json
{
  "kind": "let_decl",
  "name": "x",
  "is_shadow": false,
  "type": {"kind": "named", "name": "i32"},
  "value": {"kind": "literal", "type": "int", "value": "42", "resolved_type": {"kind": "i32"}},
  "resolved_type": {"kind": "i32"}
}
```

The `"resolved_type"` on let/var declarations is the checker-resolved type (which may differ from the syntactic type annotation in cases like type inference from `Ok(value)` where the full `Result#[T, E]` type is inferred).

---

## Monomorphized Definitions

### Monomorphized Functions

Each concrete instantiation of a generic function:

```json
{
  "original_name": "identity",
  "mangled_name": "identity$i32",
  "type_args": [{"kind": "i32"}],
  "params": [
    {"name": "x", "type": {"kind": "i32"}}
  ],
  "return_type": {"kind": "i32"},
  "body": {
    "statements": [
      {"kind": "return_stmt", "value": {"kind": "identifier", "name": "x", "resolved_type": {"kind": "i32"}}}
    ],
    "final_expr": null
  }
}
```

The body is a copy of the original generic function's body with all type references substituted to concrete types.

### Monomorphized Structs

```json
{
  "original_name": "Pair",
  "mangled_name": "Pair$i32$string",
  "type_args": [{"kind": "i32"}, {"kind": "string"}],
  "fields": [
    {"name": "first", "type": {"kind": "i32"}, "is_pub": false},
    {"name": "second", "type": {"kind": "string"}, "is_pub": false}
  ]
}
```

### Monomorphized Enums

```json
{
  "original_name": "Option",
  "mangled_name": "Option$i32",
  "type_args": [{"kind": "i32"}],
  "variants": [
    {"name": "Some", "payload": {"kind": "i32"}},
    {"name": "None", "payload": null}
  ]
}
```

### Monomorphized Methods

```json
{
  "struct_name": "Pair$i32$string",
  "method_name": "get_first",
  "mangled_name": "Pair$i32$string_get_first",
  "type_args": [{"kind": "i32"}, {"kind": "string"}],
  "params": [
    {"name": "self", "type": {"kind": "struct", "name": "Pair$i32$string"}}
  ],
  "return_type": {"kind": "i32"},
  "body": { ... }
}
```

---

## Side Tables

### call_resolutions

Maps node IDs (as string keys) to the mangled name of the resolved function:

```json
{
  "42": "identity$i32",
  "50": "Point_sum",
  "67": "Pair$i32$string_get_first"
}
```

This is the most critical side table — it tells codegen which concrete function to call for every generic function call and method call.

### comptime_values

Maps node IDs to values computed at compile time. Each entry includes a `"node_kind"` discriminator so the Zig backend can route to the correct internal table (the Zig checker uses separate maps keyed by `*ast.BuiltinCall`, `*ast.ComptimeBlock`, and `*ast.Call`):

```json
{
  "100": {"node_kind": "builtin_call", "kind": "string", "value": "i32"},
  "101": {"node_kind": "builtin_call", "kind": "int", "value": 4},
  "102": {"node_kind": "builtin_call", "kind": "bool", "value": true},
  "103": {"node_kind": "builtin_call", "kind": "repeat", "count": 10, "element_type": {"kind": "i32"}, "value": {"kind": "literal", "type": "int", "value": "0"}},
  "104": {"node_kind": "comptime_block", "kind": "int", "value": 42},
  "105": {"node_kind": "comptime_call", "kind": "string", "value": "result"}
}
```

Node kind values:
- `"builtin_call"` — `@sizeOf`, `@alignOf`, `@typeName`, `@repeat` (maps to Zig's `comptime_strings`, `comptime_bools`, `comptime_ints`, `comptime_repeats`)
- `"comptime_block"` — `comptime { ... }` block expressions (maps to Zig's `comptime_values`)
- `"comptime_call"` — compile-time evaluated function calls (maps to Zig's `comptime_call_values`)

**Implementation note:** The selfhost `TypeChecker` struct does not currently track comptime values. Phase 8 Task 2 will need to add fields for these tables before the selfhost checker can emit them.

### error_conversions

Maps `?`-operator expression IDs to error conversion info. The `conversion_method` is the fully mangled method name (matching the Zig checker's `ErrorConversionInfo.from_method_name`):

```json
{
  "80": {
    "source_type": {"kind": "io_error"},
    "target_type": {"kind": "enum", "name": "AppError"},
    "conversion_method": "AppError_from_IoError"
  }
}
```

### debug_call_types

Maps `debug()` call expression IDs to the type of the argument (for type-specific formatting):

```json
{
  "90": {"kind": "i32"},
  "91": {"kind": "struct", "name": "Point"}
}
```

---

## Complete Example

Source:
```klar
fn identity#[T](x: T) -> T {
    return x
}

fn main() -> i32 {
    let a: i32 = identity#[i32](42)
    return a
}
```

Typed AST JSON:
```json
{
  "format": "typed-ast",
  "version": 1,
  "module_decl": null,
  "imports": [],
  "declarations": [
    {
      "kind": "function",
      "name": "identity",
      "is_pub": false,
      "is_async": false,
      "is_comptime": false,
      "is_unsafe": false,
      "is_extern": false,
      "is_variadic": false,
      "type_params": [{"name": "T", "bounds": []}],
      "params": [{"name": "x", "type": {"kind": "named", "name": "T"}, "is_comptime": false, "is_out": false, "default_value": null}],
      "return_type": {"kind": "named", "name": "T"},
      "where_clause": null,
      "body": {
        "statements": [
          {"kind": "return_stmt", "value": {"kind": "identifier", "name": "x"}}
        ],
        "final_expr": null
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
      "return_type": {"kind": "named", "name": "i32"},
      "where_clause": null,
      "body": {
        "statements": [
          {
            "kind": "let_decl",
            "name": "a",
            "is_shadow": false,
            "type": {"kind": "named", "name": "i32"},
            "value": {
              "kind": "call",
              "id": 1,
              "callee": {"kind": "identifier", "name": "identity"},
              "type_args": [{"kind": "named", "name": "i32"}],
              "args": [{"kind": "literal", "type": "int", "value": "42", "resolved_type": {"kind": "i32"}}],
              "resolved_type": {"kind": "i32"}
            },
            "resolved_type": {"kind": "i32"}
          },
          {
            "kind": "return_stmt",
            "value": {"kind": "identifier", "name": "a", "resolved_type": {"kind": "i32"}}
          }
        ],
        "final_expr": null
      }
    }
  ],
  "monomorphized_functions": [
    {
      "original_name": "identity",
      "mangled_name": "identity$i32",
      "type_args": [{"kind": "i32"}],
      "params": [{"name": "x", "type": {"kind": "i32"}}],
      "return_type": {"kind": "i32"},
      "body": {
        "statements": [
          {"kind": "return_stmt", "value": {"kind": "identifier", "name": "x", "resolved_type": {"kind": "i32"}}}
        ],
        "final_expr": null
      }
    }
  ],
  "monomorphized_structs": [],
  "monomorphized_enums": [],
  "monomorphized_methods": [],
  "call_resolutions": {
    "1": "identity$i32"
  },
  "comptime_values": {},
  "error_conversions": {},
  "debug_call_types": {}
}
```

---

## AST Node Coverage

All AST node kinds from the parser are preserved. The typed AST adds:

### Expression Nodes
| Node Kind | Added Fields |
|-----------|-------------|
| All expressions | `id` (optional int), `resolved_type` (optional type) |
| `call` | (uses `call_resolutions` side table) |
| `method_call` | (uses `call_resolutions` side table) |
| `builtin_call` | (uses `comptime_values` side table) |
| `postfix` (with `?` op) | (uses `error_conversions` side table) |

### Statement Nodes
| Node Kind | Added Fields |
|-----------|-------------|
| `let_decl` / `var_decl` | `resolved_type` (optional type) |
| `for_stmt` | `iter_type` (optional type — type of the iterator element) |

### Declaration Nodes
| Node Kind | Added Fields |
|-----------|-------------|
| All declarations | Unchanged — type info lives in TypeExprs and resolved_type on child expressions |

### Top-Level Sections
| Section | Purpose |
|---------|---------|
| `monomorphized_functions` | Concrete generic function instantiations |
| `monomorphized_structs` | Concrete generic struct definitions |
| `monomorphized_enums` | Concrete generic enum definitions |
| `monomorphized_methods` | Concrete generic method instantiations |
| `call_resolutions` | Node ID → mangled function name |
| `comptime_values` | Node ID → compile-time value |
| `error_conversions` | Node ID → error conversion info |
| `debug_call_types` | Node ID → debug argument type |

---

## Zig Backend Consumption

The Zig backend detects the typed AST format via the `"format": "typed-ast"` field:

1. If present, use `typedAstFromJson()` — parse the full typed AST, skip the Zig checker, go directly to codegen.
2. If absent, use `moduleFromJson()` — parse as untyped AST, run Zig checker as before.

The codegen reads:
- `monomorphized_*` tables to emit concrete generic definitions
- `call_resolutions` to emit correct function calls
- `comptime_values` to inline compile-time constants
- `resolved_type` on expressions for type-dependent codegen (e.g., string vs integer comparison)
- `error_conversions` for `?` operator error type adaptation

---

## Multi-Module Format

For multi-module programs (programs with `import` declarations), a separate envelope format wraps per-module typed ASTs:

```json
{
  "format": "typed-ast-multi",
  "version": 1,

  "modules": [
    {
      "name": "utils",
      "is_entry": false,
      "module_decl": {"path": ["utils"]},
      "imports": [],
      "declarations": [<declaration>, ...],
      "file_meta": [...]
    },
    {
      "name": "",
      "is_entry": true,
      "module_decl": null,
      "imports": [<import_decl>, ...],
      "declarations": [<declaration>, ...],
      "file_meta": [...]
    }
  ],

  "monomorphized_functions": [<mono_function>, ...],
  "monomorphized_structs": [<mono_struct>, ...],
  "monomorphized_enums": [<mono_enum>, ...],
  "monomorphized_methods": [<mono_method>, ...]
}
```

### Key differences from single-module format

| Aspect | Single-module (`typed-ast`) | Multi-module (`typed-ast-multi`) |
|--------|----------------------------|----------------------------------|
| Format field | `"typed-ast"` | `"typed-ast-multi"` |
| Declarations | Top-level `"declarations"` | Per-module in `"modules"` array |
| Imports | Top-level `"imports"` | Per-module in `"modules"` array |
| Mono tables | Top-level | Top-level (shared across modules) |
| Module ordering | N/A | Topological (dependencies first) |

### Module entry fields

- **`name`** — Module name (e.g., `"utils"`, `"checker_builtins"`). Empty string for the entry module.
- **`is_entry`** — `true` for the main entry file, `false` for imported modules.
- **`module_decl`** — Module declaration with `"path"` array (e.g., `{"path": ["utils"]}`), or `null` for the entry module.
- **`imports`**, **`declarations`**, **`file_meta`** — Same structure as the single-module format.

### Module prefixes in codegen

Non-entry modules get a prefix derived from `"name"` for symbol resolution. For example, a function `greet` in module `"utils"` is emitted as `utils.greet` in LLVM IR. The entry module has no prefix.

### Monomorphized tables

Monomorphization is global — a generic function defined in module A may be instantiated by a call in module B. All monomorphized definitions are collected at the top level of the envelope, not per-module.

### Zig backend consumption

The `--typed-ast-input` flag auto-detects the format:

1. If `"typed-ast-multi"` is found in the first 100 bytes, use `loadMultiTypedAst()` — loads all modules, registers declarations, populates monomorphized tables.
2. Otherwise, use `loadTypedAst()` — single-module path.

For multi-module, the backend:
1. Builds each module's AST via `buildModule()` (reusing the dump-ast infrastructure)
2. Registers declarations in the checker (accumulating across modules)
3. Loads monomorphized tables from the top-level envelope
4. Checks bodies for each module to populate side tables
5. Emits LLVM IR with module prefixes (3-phase: register structs → declare functions → emit bodies)

---

## Versioning

The `"version"` field allows forward-compatible evolution:
- **v1** — Initial format (Phase 8)
- Future versions may add fields but must remain backward-compatible within the same major version
