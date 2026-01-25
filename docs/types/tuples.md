# Tuples

Tuples are fixed-size collections of values where each element can have a different type.

## Tuple Types

The type `(T1, T2, ...)` represents a tuple with elements of types `T1`, `T2`, etc.:

```klar
let pair: (i32, i32) = (10, 20)
let mixed: (string, i32, bool) = ("hello", 42, true)
```

## Creating Tuples

### Tuple Literals

```klar
let point: (i32, i32) = (10, 20)
let record: (string, i32) = ("Alice", 30)
let triple: (i32, i32, i32) = (1, 2, 3)
```

### From Functions

Tuples are commonly used to return multiple values:

```klar
fn divide_with_remainder(a: i32, b: i32) -> (i32, i32) {
    let quotient: i32 = a / b
    let remainder: i32 = a % b
    return (quotient, remainder)
}

fn main() -> i32 {
    let result: (i32, i32) = divide_with_remainder(17, 5)
    return result.0  // quotient = 3
}
```

## Accessing Elements

Use `.0`, `.1`, `.2`, etc. to access elements by position:

```klar
let tuple: (string, i32, bool) = ("hello", 42, true)

let first: string = tuple.0   // "hello"
let second: i32 = tuple.1     // 42
let third: bool = tuple.2     // true
```

### Mutable Tuples

For mutable tuples, you can modify individual elements:

```klar
var point: (i32, i32) = (10, 20)
point.0 = 30
point.1 = 40
// point is now (30, 40)
```

## Tuple Patterns

### Destructuring in for Loops

When iterating over Maps, tuples are destructured:

```klar
var map: Map[string, i32] = Map.new[string, i32]()
map.insert("a", 1)
map.insert("b", 2)

for (key, value) in map {
    println("{key}: {value}")
}
```

### Pattern Matching

Tuples can be matched in match expressions:

```klar
fn describe_point(p: (i32, i32)) -> string {
    var result: string
    match p {
        (0, 0) => { result = "origin" }
        (0, _) => { result = "on y-axis" }
        (_, 0) => { result = "on x-axis" }
        _ => { result = "somewhere else" }
    }
    return result
}
```

## Unit Type

The empty tuple `()` is called the unit type:

```klar
let unit: () = ()
```

This is equivalent to `void` in many contexts.

## Nested Tuples

Tuples can contain other tuples:

```klar
let nested: ((i32, i32), (i32, i32)) = ((1, 2), (3, 4))

let first_pair: (i32, i32) = nested.0   // (1, 2)
let element: i32 = nested.0.1           // 2
```

## Example: Coordinate Operations

```klar
fn add_points(a: (i32, i32), b: (i32, i32)) -> (i32, i32) {
    return (a.0 + b.0, a.1 + b.1)
}

fn scale_point(p: (i32, i32), factor: i32) -> (i32, i32) {
    return (p.0 * factor, p.1 * factor)
}

fn main() -> i32 {
    let p1: (i32, i32) = (10, 20)
    let p2: (i32, i32) = (5, 15)

    let sum: (i32, i32) = add_points(p1, p2)
    println("Sum: ({sum.0}, {sum.1})")  // (15, 35)

    let scaled: (i32, i32) = scale_point(p1, 3)
    println("Scaled: ({scaled.0}, {scaled.1})")  // (30, 60)

    return 0
}
```

## Example: Min and Max

```klar
fn min_max(arr: [i32]) -> (i32, i32) {
    var min_val: i32 = arr[0]
    var max_val: i32 = arr[0]

    for n: i32 in arr {
        if n < min_val {
            min_val = n
        }
        if n > max_val {
            max_val = n
        }
    }

    return (min_val, max_val)
}

fn main() -> i32 {
    let numbers: [i32; 5] = [3, 1, 4, 1, 5]
    let bounds: (i32, i32) = min_max(numbers)

    println("Min: {bounds.0}, Max: {bounds.1}")  // Min: 1, Max: 5
    return 0
}
```

## Tuples vs Structs

| Feature | Tuple | Struct |
|---------|-------|--------|
| Field names | No (positional) | Yes |
| Methods | No | Yes |
| Readability | Lower | Higher |
| Use case | Quick grouping | Domain modeling |

Use tuples for:
- Returning multiple values from functions
- Temporary groupings
- Simple pairs and triples

Use structs for:
- Named fields improve clarity
- Methods are needed
- The type will be used in multiple places

```klar
// Tuple - quick but unclear
fn get_user() -> (string, i32, bool) { ... }

// Struct - clearer
struct User { name: string, age: i32, active: bool }
fn get_user() -> User { ... }
```

## Next Steps

- [Arrays](arrays.md) - Fixed-size arrays
- [Structs](../language/structs.md) - Named fields
- [Collections](collections.md) - Dynamic collections
