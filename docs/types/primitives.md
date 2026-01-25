# Primitive Types

Klar provides a set of built-in primitive types for numbers, text, and boolean values.

## Integer Types

### Signed Integers

| Type | Size | Range |
|------|------|-------|
| `i8` | 8 bits | -128 to 127 |
| `i16` | 16 bits | -32,768 to 32,767 |
| `i32` | 32 bits | -2,147,483,648 to 2,147,483,647 |
| `i64` | 64 bits | -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807 |

```klar
let byte: i8 = -100
let short: i16 = 30000
let int: i32 = 2000000000
let long: i64 = 9000000000000000000
```

### Unsigned Integers

| Type | Size | Range |
|------|------|-------|
| `u8` | 8 bits | 0 to 255 |
| `u16` | 16 bits | 0 to 65,535 |
| `u32` | 32 bits | 0 to 4,294,967,295 |
| `u64` | 64 bits | 0 to 18,446,744,073,709,551,615 |

```klar
let byte: u8 = 255
let short: u16 = 65000
let int: u32 = 4000000000
let long: u64 = 18000000000000000000
```

### Integer Literals

```klar
let decimal: i32 = 42
let hex: i32 = 0xFF           // 255
let binary: i32 = 0b1010      // 10
let octal: i32 = 0o77         // 63
let with_underscore: i64 = 1_000_000
```

### Integer Methods

```klar
let x: i32 = -42

// Conversion methods
let as_i64: i64 = x.as[i64]        // Safe widening conversion
let as_str: string = x.to_string() // "-42"

// Math operations
let abs_val: i32 = x.abs()         // 42
```

## Floating Point Types

| Type | Size | Precision |
|------|------|-----------|
| `f32` | 32 bits | ~7 decimal digits |
| `f64` | 64 bits | ~15 decimal digits |

```klar
let single: f32 = 3.14
let double: f64 = 3.141592653589793
```

### Float Literals

```klar
let pi: f64 = 3.14159
let scientific: f64 = 1.5e10      // 15,000,000,000
let negative_exp: f64 = 2.5e-3    // 0.0025
```

### Float Methods

```klar
let x: f64 = -3.7

let abs_val: f64 = x.abs()         // 3.7
let floor_val: f64 = x.floor()     // -4.0
let ceil_val: f64 = x.ceil()       // -3.0
let round_val: f64 = x.round()     // -4.0
let as_str: string = x.to_string() // "-3.7"
```

## Boolean Type

```klar
let yes: bool = true
let no: bool = false
```

### Boolean Operations

```klar
let a: bool = true
let b: bool = false

let and_result: bool = a and b  // false
let or_result: bool = a or b    // true
let not_result: bool = not a    // false
```

### Boolean Methods

```klar
let b: bool = true
let as_str: string = b.to_string()  // "true"
```

## Character Type

The `char` type represents a Unicode scalar value.

```klar
let letter: char = 'a'
let digit: char = '7'
let emoji: char = '🎉'
```

### Escape Sequences

| Escape | Character |
|--------|-----------|
| `\'` | Single quote |
| `\"` | Double quote |
| `\\` | Backslash |
| `\n` | Newline |
| `\r` | Carriage return |
| `\t` | Tab |
| `\0` | Null |

```klar
let newline: char = '\n'
let tab: char = '\t'
let quote: char = '\''
let backslash: char = '\\'
```

### Character Methods

```klar
let c: char = 'A'

let is_alpha: bool = c.is_alphabetic()
let is_digit: bool = c.is_numeric()
let lower: char = c.to_lowercase()   // 'a'
let upper: char = c.to_uppercase()   // 'A'
let as_str: string = c.to_string()   // "A"
```

## String Type

Strings are UTF-8 encoded text.

```klar
let greeting: string = "Hello, World!"
let empty: string = ""
```

### String Interpolation

```klar
let name: string = "Alice"
let age: i32 = 30
let message: string = "Name: {name}, Age: {age}"
```

### Escape Sequences in Strings

```klar
let multiline: string = "Line 1\nLine 2"
let with_tab: string = "Col1\tCol2"
let with_quote: string = "She said \"Hello\""
```

### String Methods

```klar
let s: string = "Hello, World!"

let len: i32 = s.len()              // 13
let upper: string = s.to_uppercase() // "HELLO, WORLD!"
let lower: string = s.to_lowercase() // "hello, world!"
let trimmed: string = s.trim()       // Removes whitespace
let contains: bool = s.contains("World")  // true
let starts: bool = s.starts_with("Hello") // true
let ends: bool = s.ends_with("!")         // true
```

### String Concatenation

```klar
let first: string = "Hello"
let second: string = "World"
let combined: string = first + ", " + second + "!"
```

## Void Type

The `void` type represents the absence of a value. It's used for functions that don't return anything:

```klar
fn print_greeting(name: string) {
    println("Hello, {name}!")
    // Implicitly returns void
}
```

## Type Conversions

### Safe Conversions (.as[T])

Use `.as[T]` for safe, lossless conversions:

```klar
let x: i32 = 42
let y: i64 = x.as[i64]  // i32 -> i64 is safe
```

### Fallible Conversions (.to[T])

Use `.to[T]` for conversions that might fail:

```klar
let s: string = "42"
let n: ?i32 = s.to[i32]  // Returns ?i32
```

### Truncating Conversions (.trunc[T])

Use `.trunc[T]` when you know truncation is acceptable:

```klar
let big: i64 = 300
let small: i8 = big.trunc[i8]  // Truncates to fit
```

## Default Values

Each primitive type has a default value:

| Type | Default |
|------|---------|
| Integer types | `0` |
| Float types | `0.0` |
| `bool` | `false` |
| `char` | `'\0'` |
| `string` | `""` |

Access defaults via the `Default` trait:

```klar
let zero: i32 = i32.default()
let empty: string = string.default()
```

## Next Steps

- [Arrays](arrays.md) - Fixed-size arrays
- [Tuples](tuples.md) - Tuple types
- [Collections](collections.md) - List, Map, Set
