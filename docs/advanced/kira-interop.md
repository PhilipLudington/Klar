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
6. **Extern function block** with all exported functions
7. **`kira_free`** declaration for memory management

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
| `string` | `CStr` |
| `void` | `void` |
| User-defined types | Pass through unchanged |

## Memory Management

Every generated file includes a `kira_free` declaration:

```klar
extern {
    fn kira_free(ptr: CPtr#[void]) -> void
}
```

### Ownership Rules

- **ADTs returned by value** are caller-owned (on stack). No deallocation needed.
- **Strings from Kira** (`CStr` returns) are borrowed — valid for the call duration. Copy to a Klar `string` if you need to keep them.
- **Strings to Kira** are borrowed for the call duration. Kira copies internally if needed.
- **Heap-allocated values** (pointer returns) must be freed via `kira_free`.

See also: [FFI documentation](ffi.md) for general foreign function interface details.
