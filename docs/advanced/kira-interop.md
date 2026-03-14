# Kira Interop

Klar can consume Kira libraries through automatically generated extern declarations. The `klar import-kira` command reads a Kira type manifest (JSON) and produces a `.kl` file with extern structs, enums, functions, and safe wrapper functions.

## Table of Contents

- [Quick Start](#quick-start)
- [Importing a Kira Manifest](#importing-a-kira-manifest)
- [Generated Declarations](#generated-declarations)
- [Working with Product Types](#working-with-product-types)
- [Working with Sum Types (ADTs)](#working-with-sum-types-adts)
- [Pattern Matching on ADTs](#pattern-matching-on-adts)
- [Type Mapping](#type-mapping)
- [String Interop](#string-interop)
- [Effect Awareness](#effect-awareness)
- [Build Integration](#build-integration)
- [Memory Management](#memory-management)

## Quick Start

```bash
# Generate Klar declarations from a Kira manifest
klar import-kira manifest.json

# This creates deps/kira_<module>.kl
# Now import it in your Klar code:
```

```klar
import kira_mathlib.*

fn main() -> i32 {
    let result: i32 = unsafe { add(2, 3) }
    println(result.to_string())
    return 0
}
```

## Importing a Kira Manifest

```bash
# Default: generates deps/kira_<module>.kl
klar import-kira manifest.json

# Custom output path
klar import-kira manifest.json -o my_bindings.kl
```

The generated file is placed in `deps/` by default. The Klar module resolver automatically adds `deps/` to the search path, so `import kira_<module>` works without configuration.

**Do not edit the generated file** — regenerate it with `klar import-kira` when the Kira library changes.

## Generated Declarations

For a Kira manifest with functions and types, `import-kira` generates:

1. **Extern enum** for each sum type's tag
2. **Extern struct** for each variant's payload data
3. **Unified extern struct** matching the C tagged union layout
4. **Tag getter function** for match-based dispatch
5. **Safe accessor functions** for extracting variant payloads
6. **Extern function block** with all exported functions (string functions prefixed with `__raw_`), each with a `// pure` or `// effect` comment
7. **String wrapper functions** that accept/return `string` instead of `CStr`, with `meta pure` annotation for pure functions
8. **`kira_free`** declaration for memory management

## Working with Product Types

A Kira product type (record) maps directly to a Klar extern struct:

```klar
// Generated from: type Point = { x: f64, y: f64 }
pub extern struct Point {
    x: f64,
    y: f64,
}
```

Access fields normally:

```klar
fn use_point(p: ref Point) -> void {
    println(p.x.to_string())
    println(p.y.to_string())
}
```

## Working with Sum Types (ADTs)

A Kira sum type like `type Shape = Circle(f64) | Rectangle(f64, f64) | Point` generates several declarations:

### Tag Enum

```klar
pub extern enum ShapeTag: i32 {
    Circle = 0,
    Rectangle = 1,
    Point = 2,
}
```

### Variant Data Structs

One struct per variant that has payload fields:

```klar
pub extern struct ShapeCircleData {
    radius: f64,
}

pub extern struct ShapeRectangleData {
    w: f64,
    h: f64,
}
```

Unit variants (like `Point` with no fields) do not generate a data struct.

### Unified Struct

A single struct matching the C tagged union memory layout:

```klar
pub extern struct Shape {
    tag: ShapeTag,
    data: ShapeRectangleData,  // sized to largest variant
}
```

The `data` field uses the largest variant's struct type. Smaller variants' data occupies the same memory region and is accessed through safe wrapper functions.

### Tag Getter

```klar
pub fn shape_tag(s: ref Shape) -> ShapeTag {
    return s.tag
}
```

### Safe Accessor Functions

For each variant with payload fields:

```klar
// Single-field variant: returns ?FieldType
pub fn as_circle(s: ref Shape) -> ?f64 {
    if s.tag == ShapeTag.Circle {
        // ... extracts radius via ptr_cast
    }
    return None
}

// Multi-field variant: returns ?VariantData
pub fn as_rectangle(s: ref Shape) -> ?ShapeRectangleData {
    if s.tag == ShapeTag.Rectangle {
        return Some(s.data)
    }
    return None
}
```

Unit variants have no accessor — use the tag getter with `match` instead.

## Pattern Matching on ADTs

Use the tag getter function with `match` to dispatch on variants:

```klar
import kira_shapes.*

fn describe(s: ref Shape) -> string {
    match shape_tag(s) {
        ShapeTag.Circle => {
            let r: ?f64 = as_circle(s)
            match r {
                Some(radius) => {
                    return "Circle with radius " + radius.to_string()
                }
                None => { return "error" }
            }
        }
        ShapeTag.Rectangle => {
            let rect: ?ShapeRectangleData = as_rectangle(s)
            match rect {
                Some(data) => {
                    return "Rectangle " + data.w.to_string() + "x" + data.h.to_string()
                }
                None => { return "error" }
            }
        }
        ShapeTag.Point => {
            return "Point"
        }
    }
}
```

## Type Mapping

| Kira type | Klar FFI type |
|-----------|---------------|
| `i8` | `i8` |
| `i16` | `i16` |
| `i32`, `int` | `i32` |
| `i64` | `i64` |
| `u8` | `u8` |
| `u16` | `u16` |
| `u32` | `u32` |
| `u64` | `u64` |
| `f32` | `f32` |
| `f64`, `float` | `f64` |
| `bool` | `Bool` |
| `char` | `Char` |
| `string` | `CStr` (raw) / `string` (wrapper) |
| `void` | `void` |
| User-defined types | Pass through unchanged |

## String Interop

Functions that take or return `string` in Kira get automatic wrapper functions. You call the wrapper with Klar `string` values — no `unsafe` or manual `CStr` conversion needed.

### How It Works

For a Kira function `greet(name: string) -> string`, the generator produces:

```klar
// Raw extern (CStr types, prefixed with __raw_)
extern {
    fn __raw_greet(name: CStr) -> CStr
}

// Safe wrapper (string types, original name)
pub fn greet(name: string) -> string {
    unsafe {
        return __raw_greet(name.as_cstr()).to_string()
    }
}
```

### Usage

```klar
import kira_hello.*

fn main() -> i32 {
    // Just use string — wrapper handles CStr conversion
    let msg: string = greet("world")
    println(msg)
    return 0
}
```

### Wrapper Rules

| Kira signature | Wrapper behavior |
|----------------|-----------------|
| `fn f(s: string) -> string` | Converts param via `.as_cstr()`, return via `.to_string()` |
| `fn f(s: string) -> i32` | Converts param only |
| `fn f() -> string` | Converts return only |
| `fn f(s: string) -> void` | Converts param, no return value |
| `fn f(a: i32) -> i32` | No wrapper — stays in extern block as-is |

Mixed parameters work correctly: only `string` params get `.as_cstr()`, non-string params pass through unchanged.

### Accessing the Raw Extern

If you need the raw `CStr` version (e.g., to avoid the copy from `.to_string()`), call the `__raw_` prefixed function directly inside an `unsafe` block:

```klar
unsafe {
    let raw: CStr = __raw_greet("world".as_cstr())
    // Use raw CStr without copying...
}
```

## Effect Awareness

Kira distinguishes between pure functions (`fn`) and effectful functions (`effect fn`). When the Kira manifest includes an `"effect"` field on function declarations, `import-kira` surfaces this information in the generated Klar code.

### Manifest Format

```json
{
  "module": "mylib",
  "functions": [
    {"name": "add", "params": [...], "return_type": "i32", "effect": false},
    {"name": "read_file", "params": [...], "return_type": "string", "effect": true}
  ],
  "types": []
}
```

The `"effect"` field is optional and defaults to `false` (pure) when absent, ensuring backward compatibility with older manifests.

### Generated Annotations

For each function in the extern block, a purity comment is emitted:

```klar
extern {
    // pure
    fn add(a: i32, b: i32) -> i32
    // effect
    fn read_file(path: CStr) -> CStr
}
```

For string wrapper functions, pure functions additionally receive a `meta pure` annotation:

```klar
meta pure
pub fn concat(a: string, b: string) -> string {
    unsafe {
        return __raw_concat(a.as_cstr(), b.as_cstr()).to_string()
    }
}
```

Effect functions get the `// effect` comment in the extern block but no `meta pure` on their wrapper.

### How to Use

The purity information is documentation-only — it does not change runtime behavior. It helps developers and AI agents understand which imported functions are side-effect-free:

- **Pure functions** are safe to call multiple times, reorder, or memoize
- **Effect functions** may perform I/O, mutate external state, or have other side effects

AI agents can use `meta pure` annotations to make optimization and refactoring decisions when working with Kira interop code.

## Build Integration

For projects with Kira dependencies, `klar build` automatically builds the Kira library, compiles its C output, generates the extern block, and links everything into the final binary.

### Project Layout

```
my-project/
├── klar.json          # Declares Kira dependencies
├── main.kl            # Uses: import kira_mathlib.*
├── build/             # Generated: executables and .o files
│   ├── myapp
│   └── kira_mathlib.o
└── deps/              # Generated: extern block .kl files
    └── kira_mathlib.kl

../kira-mathlib/       # External Kira library
└── mathlib.ki
```

### Configuration

Add a `kira-dependencies` section to `klar.json`:

```json
{
  "package": {
    "name": "myapp",
    "version": "0.1.0",
    "entry": "main.kl"
  },
  "dependencies": {},
  "kira-dependencies": {
    "mathlib": { "path": "../kira-mathlib" }
  }
}
```

Each dependency has a `name` (used for `import kira_<name>`) and a `path` to the Kira project directory. The path can be relative or absolute.

### Build Workflow

```bash
# Build everything (Kira deps + Klar source + link)
klar build

# What happens automatically:
# 1. Finds kira-dependencies in klar.json
# 2. Runs `kira build --lib` for each Kira dependency
# 3. Compiles generated C to .o files (via cc)
# 4. Generates deps/kira_<name>.kl (extern block)
# 5. Compiles Klar source with import resolution
# 6. Links Klar + Kira object files into final binary
```

Subsequent builds use mtime-based caching — if the Kira source hasn't changed, steps 2–4 are skipped.

### Importing

In your Klar source, import the generated module:

```klar
import kira_mathlib.*

fn main() -> i32 {
    let result: i32 = unsafe { add(2, 3) }
    println(result.to_string())
    return 0
}
```

The module name is always `kira_<dependency-name>` from your `klar.json`. The `deps/` directory is added to the module search path automatically — no manual file management needed.

### Cleaning

```bash
# Remove build artifacts and generated interop files
klar clean
```

This removes the `build/` directory (executables, object files) and all `deps/kira_*.kl` files. Non-Kira deps files in `deps/` are preserved. Run `klar build` again to regenerate everything.

### AI Agent Instructions

To set up a Klar project that uses a Kira library:

1. Create `klar.json` with a `kira-dependencies` entry pointing to the Kira project
2. Write Klar source with `import kira_<name>.*`
3. Call extern functions inside `unsafe { ... }` blocks (non-string functions) or directly (string-wrapped functions)
4. Run `klar build` — everything is automatic

Requires: `kira` command in PATH, C compiler (`cc` or `$CC`).

## Memory Management

Every generated file includes a `kira_free` declaration:

```klar
extern {
    fn kira_free(ptr: CPtr#[void]) -> void
}
```

### Ownership Rules

- **ADTs returned by value** are caller-owned (on stack). No deallocation needed.
- **Strings via wrappers** — the generated wrapper functions handle all conversion automatically. `string` params are borrowed as `CStr` for the call duration; `CStr` returns are copied to an owned Klar `string`. No manual memory management needed.
- **Strings via `__raw_` functions** — `CStr` returns are borrowed and valid only for the call duration. Copy to a Klar `string` via `.to_string()` if you need to keep them. `string` params passed via `.as_cstr()` are borrowed for the call.
- **Heap-allocated values** (pointer returns) must be freed via `kira_free`.

See also: [FFI documentation](ffi.md) for general foreign function interface details.
