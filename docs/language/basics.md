# Basics

This covers variables, types, expressions, and comments in Klar.

## Variables

### Immutable Variables (let)

Use `let` to declare immutable variables:

```klar
let x: i32 = 42
let name: string = "Alice"
let active: bool = true
```

Immutable variables cannot be reassigned:

```klar
let x: i32 = 10
x = 20  // Error: cannot assign to immutable variable
```

### Mutable Variables (var)

Use `var` to declare mutable variables:

```klar
var counter: i32 = 0
counter = counter + 1  // OK
counter = 10           // OK
```

### Type Annotations

Klar requires explicit type annotations on all variables:

```klar
let x: i32 = 42      // Required
let x = 42           // Error: missing type annotation
```

This is by design - explicit types make code self-documenting and unambiguous.

## Primitive Types

| Type | Description | Example |
|------|-------------|---------|
| `i8`, `i16`, `i32`, `i64` | Signed integers | `let x: i32 = -42` |
| `u8`, `u16`, `u32`, `u64` | Unsigned integers | `let x: u32 = 42` |
| `f32`, `f64` | Floating point | `let x: f64 = 3.14` |
| `bool` | Boolean | `let x: bool = true` |
| `char` | Character | `let x: char = 'a'` |
| `string` | String | `let x: string = "hello"` |
| `void` | No value | (used for functions) |

See [Primitives](../types/primitives.md) for details.

## Literals

### Integer Literals

```klar
let decimal: i32 = 42
let hex: i32 = 0xFF
let binary: i32 = 0b1010
let octal: i32 = 0o77
let with_underscore: i64 = 1_000_000
```

### Float Literals

```klar
let pi: f64 = 3.14159
let scientific: f64 = 1.5e10
let negative_exp: f64 = 2.5e-3
```

### Boolean Literals

```klar
let yes: bool = true
let no: bool = false
```

### Character Literals

```klar
let letter: char = 'a'
let newline: char = '\n'
let tab: char = '\t'
let quote: char = '\''
let backslash: char = '\\'
```

### String Literals

```klar
let greeting: string = "Hello, World!"
let with_escapes: string = "Line 1\nLine 2"
let with_quotes: string = "She said \"Hello\""
```

### String Interpolation

Use `{expression}` inside strings for interpolation:

```klar
let name: string = "Alice"
let age: i32 = 30
let message: string = "Name: {name}, Age: {age}"

// Field access works too
struct Point { x: i32, y: i32 }
let p: Point = Point { x: 10, y: 20 }
println("Point: ({p.x}, {p.y})")
```

## Expressions

### Arithmetic Operators

```klar
let a: i32 = 10
let b: i32 = 3

let sum: i32 = a + b       // 13
let diff: i32 = a - b      // 7
let prod: i32 = a * b      // 30
let quot: i32 = a / b      // 3 (integer division)
let rem: i32 = a % b       // 1 (remainder)
```

### Overflow Operators

Klar has explicit overflow handling:

```klar
let x: i32 = 2147483647    // Max i32

// These would panic at runtime (default):
// let overflow: i32 = x + 1

// Wrapping arithmetic (wraps around)
let wrapped: i32 = x +% 1  // -2147483648

// Saturating arithmetic (clamps at max/min)
let saturated: i32 = x +| 1  // 2147483647
```

### Comparison Operators

```klar
let a: i32 = 10
let b: i32 = 20

let eq: bool = a == b      // false
let ne: bool = a != b      // true
let lt: bool = a < b       // true
let le: bool = a <= b      // true
let gt: bool = a > b       // false
let ge: bool = a >= b      // false
```

### Logical Operators

Klar uses keywords instead of symbols:

```klar
let a: bool = true
let b: bool = false

let and_result: bool = a and b  // false
let or_result: bool = a or b    // true
let not_result: bool = not a    // false
```

### Bitwise Operators

```klar
let a: i32 = 0b1100
let b: i32 = 0b1010

let and_bits: i32 = a & b    // 0b1000
let or_bits: i32 = a | b     // 0b1110
let xor_bits: i32 = a ^ b    // 0b0110
let not_bits: i32 = ~a       // Bitwise NOT
let left: i32 = a << 2       // 0b110000
let right: i32 = a >> 2      // 0b0011
```

## Comments

### Single-Line Comments

```klar
// This is a single-line comment
let x: i32 = 42  // Inline comment
```

### Multi-Line Comments

```klar
/* This is a
   multi-line
   comment */

/* Comments can /* nest */ like this */
```

## Blocks

Blocks create a scope with `{` and `}`:

```klar
fn main() -> i32 {
    let x: i32 = 10

    {
        let y: i32 = 20  // y is only visible in this block
        println("{y}")
    }

    // y is not accessible here
    return x
}
```

## Next Steps

- [Functions](functions.md) - Function definitions
- [Control Flow](control-flow.md) - if, match, loops
- [Primitives](../types/primitives.md) - Detailed type information
