# Arrays

Arrays are fixed-size, contiguous sequences of elements of the same type.

## Array Types

The type `[T; N]` represents an array of `N` elements of type `T`:

```klar
let numbers: [i32; 3] = [1, 2, 3]
let bools: [bool; 2] = [true, false]
let strings: [string; 4] = ["a", "b", "c", "d"]
```

### Type Inference

The array size can be inferred from the literal:

```klar
let arr: [i32] = [10, 20, 30]  // Size inferred as 3
```

## Creating Arrays

### Array Literals

```klar
let numbers: [i32; 5] = [1, 2, 3, 4, 5]
let empty: [i32; 0] = []
```

### Repeat Initialization

Use `@repeat` for arrays filled with the same value:

```klar
let zeros: [i32; 10] = @repeat(0, 10)
let ones: [bool; 5] = @repeat(true, 5)
```

## Accessing Elements

### Indexing

Use `[]` to access elements by index (0-based):

```klar
let arr: [i32; 3] = [10, 20, 30]

let first: i32 = arr[0]   // 10
let second: i32 = arr[1]  // 20
let third: i32 = arr[2]   // 30
```

### Bounds Checking

Array access is bounds-checked at runtime:

```klar
let arr: [i32; 3] = [1, 2, 3]
let bad: i32 = arr[10]  // Runtime panic: index out of bounds
```

### Mutable Arrays

For mutable arrays, use `var` and modify elements:

```klar
var arr: [i32; 3] = [1, 2, 3]
arr[0] = 10
arr[1] = 20
// arr is now [10, 20, 3]
```

## Array Methods

### len()

Returns the number of elements:

```klar
let arr: [i32; 5] = [1, 2, 3, 4, 5]
let size: i32 = arr.len()  // 5
```

### is_empty()

Checks if the array has no elements:

```klar
let arr: [i32; 0] = []
let empty: bool = arr.is_empty()  // true
```

## Iterating Over Arrays

### for Loop

```klar
let numbers: [i32; 3] = [10, 20, 30]

for n: i32 in numbers {
    println("{n}")
}
```

### With Index

Use a range to iterate with indices:

```klar
let arr: [string; 3] = ["a", "b", "c"]

for i: i32 in 0..arr.len() {
    println("arr[{i}] = {arr[i]}")
}
```

## Multi-dimensional Arrays

Arrays can be nested:

```klar
let matrix: [[i32; 3]; 2] = [
    [1, 2, 3],
    [4, 5, 6],
]

let element: i32 = matrix[1][2]  // 6
```

### Iterating 2D Arrays

```klar
let matrix: [[i32; 3]; 2] = [
    [1, 2, 3],
    [4, 5, 6],
]

for row: [i32; 3] in matrix {
    for cell: i32 in row {
        print("{cell} ")
    }
    println("")
}
```

## Array vs List

| Feature | Array `[T; N]` | List `List[T]` |
|---------|---------------|----------------|
| Size | Fixed at compile time | Dynamic |
| Memory | Stack or inline | Heap |
| Performance | Faster access | Slower (indirection) |
| Use case | Known size | Unknown/varying size |

Use arrays when:
- The size is known at compile time
- You need maximum performance
- Stack allocation is preferred

Use `List[T]` when:
- The size varies at runtime
- You need to add/remove elements
- The size might be large

## Example: Statistics

```klar
fn sum(arr: [i32]) -> i32 {
    var total: i32 = 0
    for n: i32 in arr {
        total = total + n
    }
    return total
}

fn average(arr: [i32]) -> f64 {
    let s: i32 = sum(arr)
    return s.as[f64] / arr.len().as[f64]
}

fn main() -> i32 {
    let data: [i32; 5] = [10, 20, 30, 40, 50]

    println("Sum: {sum(data)}")        // 150
    println("Average: {average(data)}") // 30.0

    return 0
}
```

## Example: Finding Elements

```klar
fn find_index(arr: [i32], target: i32) -> ?i32 {
    for i: i32 in 0..arr.len() {
        if arr[i] == target {
            return Some(i)
        }
    }
    return None
}

fn main() -> i32 {
    let numbers: [i32; 5] = [10, 20, 30, 40, 50]

    let idx: ?i32 = find_index(numbers, 30)
    match idx {
        Some(i) => { println("Found at index {i}") }
        None => { println("Not found") }
    }

    return 0
}
```

## Next Steps

- [Tuples](tuples.md) - Tuple types
- [Collections](collections.md) - Dynamic collections (List, Map, Set)
- [Control Flow](../language/control-flow.md) - Iterating with for loops
