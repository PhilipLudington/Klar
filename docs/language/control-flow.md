# Control Flow

Klar provides structured control flow with `if`, `match`, `for`, `while`, and `loop`.

## if Expressions

### Basic if

```klar
if condition {
    // code
}
```

### if-else

```klar
if condition {
    // when true
} else {
    // when false
}
```

### if-else if-else

```klar
if condition1 {
    // first case
} else if condition2 {
    // second case
} else {
    // default case
}
```

### Example

```klar
fn classify(n: i32) -> string {
    if n < 0 {
        return "negative"
    } else if n == 0 {
        return "zero"
    } else {
        return "positive"
    }
}
```

Note: In Klar, `if` is a statement, not an expression. Assign results to variables in each branch:

```klar
var result: string
if n > 0 {
    result = "positive"
} else {
    result = "not positive"
}
```

## match Expressions

Pattern matching with `match`:

### Basic match

```klar
match value {
    pattern1 => { /* code */ }
    pattern2 => { /* code */ }
    _ => { /* default */ }
}
```

### Matching Integers

```klar
fn describe(n: i32) -> string {
    var result: string
    match n {
        0 => { result = "zero" }
        1 => { result = "one" }
        2 => { result = "two" }
        _ => { result = "many" }
    }
    return result
}
```

### Matching Enums

```klar
enum Color { Red, Green, Blue }

fn to_hex(c: Color) -> string {
    var hex: string
    match c {
        Color.Red => { hex = "#FF0000" }
        Color.Green => { hex = "#00FF00" }
        Color.Blue => { hex = "#0000FF" }
    }
    return hex
}
```

### Matching with Payload Extraction

```klar
enum Option[T] { Some(T), None }

fn unwrap_or(opt: ?i32, default: i32) -> i32 {
    var result: i32
    match opt {
        Some(value) => { result = value }
        None => { result = default }
    }
    return result
}
```

### Wildcard Pattern

Use `_` to match anything:

```klar
match value {
    specific_case => { /* handle */ }
    _ => { /* everything else */ }
}
```

## for Loops

### Range Iteration

```klar
// Exclusive range: 0, 1, 2, 3, 4
for i: i32 in 0..5 {
    println("{i}")
}

// Inclusive range: 0, 1, 2, 3, 4, 5
for i: i32 in 0..=5 {
    println("{i}")
}
```

### Array Iteration

```klar
let numbers: [i32; 3] = [10, 20, 30]
for n: i32 in numbers {
    println("{n}")
}
```

### List Iteration

```klar
var list: List[i32] = List.new[i32]()
list.push(1)
list.push(2)
list.push(3)

for x: i32 in list {
    println("{x}")
}
```

### Map Iteration

Maps iterate as key-value tuples:

```klar
var map: Map[string, i32] = Map.new[string, i32]()
map.insert("a", 1)
map.insert("b", 2)

for (key, value) in map {
    println("{key}: {value}")
}
```

### Set Iteration

```klar
var set: Set[i32] = Set.new[i32]()
set.insert(10)
set.insert(20)

for x: i32 in set {
    println("{x}")
}
```

### break and continue

```klar
for i: i32 in 0..10 {
    if i == 3 {
        continue  // Skip 3
    }
    if i == 7 {
        break     // Stop at 7
    }
    println("{i}")
}
// Prints: 0, 1, 2, 4, 5, 6
```

## while Loops

```klar
var count: i32 = 0
while count < 5 {
    println("{count}")
    count = count + 1
}
```

### break and continue in while

```klar
var i: i32 = 0
while true {
    if i >= 10 {
        break
    }
    if i % 2 == 0 {
        i = i + 1
        continue
    }
    println("{i}")
    i = i + 1
}
```

## loop (Infinite Loop)

Use `loop` for infinite loops (clearer than `while true`):

```klar
var attempts: i32 = 0
loop {
    attempts = attempts + 1
    if try_operation() {
        break
    }
    if attempts >= 10 {
        break
    }
}
```

## Nested Loops

Loops can be nested:

```klar
for i: i32 in 0..3 {
    for j: i32 in 0..3 {
        println("({i}, {j})")
    }
}
```

### Breaking from Nested Loops

Currently, `break` only breaks from the innermost loop:

```klar
for i: i32 in 0..10 {
    for j: i32 in 0..10 {
        if some_condition(i, j) {
            break  // Breaks inner loop only
        }
    }
}
```

## Example: FizzBuzz

```klar
fn fizzbuzz(n: i32) {
    let div3: bool = n % 3 == 0
    let div5: bool = n % 5 == 0

    if div3 and div5 {
        println("FizzBuzz")
    } else if div3 {
        println("Fizz")
    } else if div5 {
        println("Buzz")
    } else {
        println("{n}")
    }
}

fn main() -> i32 {
    for i: i32 in 1..=20 {
        fizzbuzz(i)
    }
    return 0
}
```

## Next Steps

- [Structs](structs.md) - Struct definitions
- [Enums](enums.md) - Enum definitions and pattern matching
- [Error Handling](error-handling.md) - Optional and Result types
