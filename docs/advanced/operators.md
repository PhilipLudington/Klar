# Operators

Complete reference for all operators in Klar.

## Arithmetic Operators

### Standard Arithmetic

| Operator | Description | Example |
|----------|-------------|---------|
| `+` | Addition | `a + b` |
| `-` | Subtraction | `a - b` |
| `*` | Multiplication | `a * b` |
| `/` | Division | `a / b` |
| `%` | Remainder | `a % b` |
| `-` (unary) | Negation | `-x` |

```klar
let a: i32 = 10
let b: i32 = 3

let sum: i32 = a + b       // 13
let diff: i32 = a - b      // 7
let prod: i32 = a * b      // 30
let quot: i32 = a / b      // 3
let rem: i32 = a % b       // 1
let neg: i32 = -a          // -10
```

### Overflow Handling Operators

Klar provides explicit overflow handling:

| Operator | Description | On Overflow |
|----------|-------------|-------------|
| `+%` | Wrapping addition | Wraps around |
| `-%` | Wrapping subtraction | Wraps around |
| `*%` | Wrapping multiplication | Wraps around |
| `+\|` | Saturating addition | Clamps to max/min |
| `-\|` | Saturating subtraction | Clamps to max/min |
| `*\|` | Saturating multiplication | Clamps to max/min |

```klar
let max: i32 = 2147483647

// Wrapping (two's complement wrap)
let wrapped: i32 = max +% 1   // -2147483648

// Saturating (clamps at limits)
let saturated: i32 = max +| 1  // 2147483647 (stays at max)
```

## Comparison Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `==` | Equal | `a == b` |
| `!=` | Not equal | `a != b` |
| `<` | Less than | `a < b` |
| `<=` | Less than or equal | `a <= b` |
| `>` | Greater than | `a > b` |
| `>=` | Greater than or equal | `a >= b` |

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

## Logical Operators

Klar uses keywords for logical operations:

| Operator | Description | Example |
|----------|-------------|---------|
| `and` | Logical AND | `a and b` |
| `or` | Logical OR | `a or b` |
| `not` | Logical NOT | `not a` |

```klar
let a: bool = true
let b: bool = false

let and_result: bool = a and b  // false
let or_result: bool = a or b    // true
let not_result: bool = not a    // false
```

### Short-Circuit Evaluation

`and` and `or` use short-circuit evaluation:

```klar
// b() is not called if a is false
let result: bool = a() and b()

// b() is not called if a is true
let result: bool = a() or b()
```

## Bitwise Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `&` | Bitwise AND | `a & b` |
| `\|` | Bitwise OR | `a \| b` |
| `^` | Bitwise XOR | `a ^ b` |
| `~` | Bitwise NOT | `~a` |
| `<<` | Left shift | `a << n` |
| `>>` | Right shift | `a >> n` |

```klar
let a: i32 = 0b1100  // 12
let b: i32 = 0b1010  // 10

let and_bits: i32 = a & b    // 0b1000 = 8
let or_bits: i32 = a | b     // 0b1110 = 14
let xor_bits: i32 = a ^ b    // 0b0110 = 6
let not_bits: i32 = ~a       // Bitwise NOT
let left: i32 = a << 2       // 0b110000 = 48
let right: i32 = a >> 2      // 0b0011 = 3
```

## Assignment Operators

| Operator | Description | Equivalent |
|----------|-------------|------------|
| `=` | Assignment | - |
| `+=` | Add assign | `a = a + b` |
| `-=` | Subtract assign | `a = a - b` |
| `*=` | Multiply assign | `a = a * b` |
| `/=` | Divide assign | `a = a / b` |
| `%=` | Remainder assign | `a = a % b` |

```klar
var x: i32 = 10

x += 5   // x = 15
x -= 3   // x = 12
x *= 2   // x = 24
x /= 4   // x = 6
x %= 4   // x = 2
```

## Range Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `..` | Exclusive range | `0..5` (0,1,2,3,4) |
| `..=` | Inclusive range | `0..=5` (0,1,2,3,4,5) |

```klar
// Exclusive: end is not included
for i: i32 in 0..5 {
    println("{i}")  // 0, 1, 2, 3, 4
}

// Inclusive: end is included
for i: i32 in 0..=5 {
    println("{i}")  // 0, 1, 2, 3, 4, 5
}
```

## Optional/Error Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `?` | Error propagation | `value?` |
| `??` | Null coalescing | `opt ?? default` |
| `!` | Force unwrap | `opt!` |

```klar
// Error propagation
fn read() -> Result[Data, Error] {
    let content: string = read_file(path)?  // Returns Err if fails
    return Ok(parse(content))
}

// Null coalescing
let value: i32 = maybe_int ?? 0  // Use 0 if None

// Force unwrap (panics if None/Err)
let value: i32 = some_value!
```

## Member Access Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `.` | Field/method access | `obj.field` |
| `::` | Enum variant construction | `Enum::Variant` |
| `[]` | Index access | `arr[i]` |

```klar
// Field access
let x: i32 = point.x

// Method call
let len: i32 = string.len()

// Enum variant
let color: Color = Color::Red

// Index
let first: i32 = array[0]
```

## Type Conversion Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `.as[T]` | Safe conversion | `x.as[i64]` |
| `.to[T]` | Fallible conversion | `s.to[i32]` |
| `.trunc[T]` | Truncating conversion | `x.trunc[i8]` |

```klar
let x: i32 = 42

// Safe widening
let y: i64 = x.as[i64]

// Fallible conversion
let n: ?i32 = "42".to[i32]

// Truncating (may lose data)
let small: i8 = x.trunc[i8]
```

## Operator Precedence

From highest to lowest precedence:

1. Unary: `-`, `not`, `~`
2. Multiplication: `*`, `/`, `%`
3. Addition: `+`, `-`
4. Shifts: `<<`, `>>`
5. Bitwise AND: `&`
6. Bitwise XOR: `^`
7. Bitwise OR: `|`
8. Comparisons: `<`, `<=`, `>`, `>=`
9. Equality: `==`, `!=`
10. Logical AND: `and`
11. Logical OR: `or`
12. Range: `..`, `..=`
13. Assignment: `=`, `+=`, etc.

Use parentheses for clarity:

```klar
// Precedence might be unclear
let result: i32 = a + b * c

// Clearer with parentheses
let result: i32 = a + (b * c)
```

## Operator Traits

Operators are implemented through traits:

| Operator | Trait |
|----------|-------|
| `==`, `!=` | `Eq` |
| `<`, `<=`, `>`, `>=` | `Ordered` |

Implement these traits on your types to use the operators:

```klar
struct Point {
    x: i32,
    y: i32,
}

impl Point: Eq {
    fn eq(self: Point, other: Point) -> bool {
        return self.x == other.x and self.y == other.y
    }
}

// Now == works for Point
let p1: Point = Point { x: 1, y: 2 }
let p2: Point = Point { x: 1, y: 2 }
let equal: bool = p1 == p2  // true
```

## Next Steps

- [Primitives](../types/primitives.md) - Numeric types
- [Error Handling](../language/error-handling.md) - ? and ?? operators
- [Builtin Traits](builtin-traits.md) - Eq, Ordered traits
