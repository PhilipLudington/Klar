# References

References allow you to borrow values without taking ownership. Klar provides two types of references: `ref` for read-only access and `inout` for mutable access.

## Read-Only References (ref)

Use `ref` to borrow a value without being able to modify it:

```klar
fn print_length(ref s: string) {
    println("Length: {s.len()}")
    // Cannot modify s here
}

fn main() -> i32 {
    let greeting: string = "hello"
    print_length(ref greeting)  // Borrow greeting
    println(greeting)  // greeting is still valid
    return 0
}
```

### Passing ref Parameters

When calling a function with `ref` parameter, use `ref` at the call site:

```klar
fn analyze(ref data: [i32]) -> i32 {
    var sum: i32 = 0
    for x: i32 in data {
        sum = sum + x
    }
    return sum
}

fn main() -> i32 {
    let numbers: [i32; 5] = [1, 2, 3, 4, 5]
    let total: i32 = analyze(ref numbers)  // Explicit ref at call site
    return total
}
```

### Multiple ref Borrows

Multiple `ref` borrows are allowed simultaneously:

```klar
fn compare(ref a: string, ref b: string) -> bool {
    return a == b
}

fn main() -> i32 {
    let s: string = "hello"
    let same: bool = compare(ref s, ref s)  // OK - multiple ref borrows
    return 0
}
```

## Mutable References (inout)

Use `inout` to borrow a value with the ability to modify it:

```klar
fn double_in_place(inout n: i32) {
    n = n * 2
}

fn main() -> i32 {
    var x: i32 = 10
    double_in_place(inout x)  // x is now 20
    return x
}
```

### Passing inout Parameters

Use `inout` at both the call site and the function signature:

```klar
fn sort_in_place(inout arr: [i32]) {
    // Sort the array...
}

fn main() -> i32 {
    var numbers: [i32; 5] = [5, 3, 1, 4, 2]
    sort_in_place(inout numbers)  // Explicit inout at call site
    // numbers is now sorted
    return 0
}
```

### Variable Must Be Mutable

You can only take an `inout` reference to a `var`:

```klar
fn increment(inout n: i32) {
    n = n + 1
}

fn main() -> i32 {
    let x: i32 = 10
    // increment(inout x)  // Error: cannot take inout of immutable variable

    var y: i32 = 10
    increment(inout y)  // OK
    return y
}
```

### Exclusive Access

Only one `inout` reference is allowed at a time:

```klar
fn swap(inout a: i32, inout b: i32) {
    let temp: i32 = a
    a = b
    b = temp
}

fn main() -> i32 {
    var x: i32 = 10
    var y: i32 = 20
    swap(inout x, inout y)  // OK - different variables

    // swap(inout x, inout x)  // Error: cannot borrow x mutably twice
    return 0
}
```

## Methods with References

### ref self - Read-Only Methods

```klar
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn magnitude(ref self: Point) -> f64 {
        let x: f64 = self.x.as[f64]
        let y: f64 = self.y.as[f64]
        return sqrt(x * x + y * y)
    }

    fn to_string(ref self: Point) -> string {
        return "({self.x}, {self.y})"
    }
}
```

### inout self - Mutating Methods

```klar
impl Point {
    fn scale(inout self: Point, factor: i32) {
        self.x = self.x * factor
        self.y = self.y * factor
    }

    fn reset(inout self: Point) {
        self.x = 0
        self.y = 0
    }
}

fn main() -> i32 {
    var p: Point = Point { x: 10, y: 20 }
    p.scale(2)  // inout self is implicit for method calls
    return p.x  // 20
}
```

## Reference Rules

1. **ref borrows** don't require the source to be mutable
2. **inout borrows** require the source to be a `var`
3. Multiple **ref** borrows are allowed simultaneously
4. Only one **inout** borrow is allowed at a time
5. **ref** and **inout** cannot coexist for the same value

```klar
fn main() -> i32 {
    var x: i32 = 10

    // OK: multiple ref
    let r1: ref i32 = ref x
    let r2: ref i32 = ref x

    // OK: single inout
    let m1: inout i32 = inout x

    // Error: ref and inout together
    // let r3: ref i32 = ref x
    // let m2: inout i32 = inout x

    return 0
}
```

## When to Use Each

### Use ref When

- You only need to read the value
- Multiple parts of code need access simultaneously
- The function is a "query" that doesn't change state

```klar
fn contains(ref list: List[i32], target: i32) -> bool {
    for x: i32 in list {
        if x == target {
            return true
        }
    }
    return false
}
```

### Use inout When

- You need to modify the value
- The function updates state in place
- You want to avoid copying large values

```klar
fn append_all(inout list: List[i32], items: [i32]) {
    for x: i32 in items {
        list.push(x)
    }
}
```

### Use Ownership When

- You need full control of the value
- The function "consumes" the value
- You're building/transforming data

```klar
fn uppercase(s: string) -> string {
    return s.to_uppercase()  // Takes ownership, returns new string
}
```

## Example: In-Place Algorithms

```klar
fn reverse_in_place(inout arr: [i32]) {
    var left: i32 = 0
    var right: i32 = arr.len() - 1

    while left < right {
        let temp: i32 = arr[left]
        arr[left] = arr[right]
        arr[right] = temp
        left = left + 1
        right = right - 1
    }
}

fn main() -> i32 {
    var numbers: [i32; 5] = [1, 2, 3, 4, 5]
    reverse_in_place(inout numbers)
    // numbers is now [5, 4, 3, 2, 1]
    return numbers[0]  // 5
}
```

## Example: Accumulator Pattern

```klar
struct Stats {
    count: i32,
    sum: i32,
    min: ?i32,
    max: ?i32,
}

impl Stats {
    fn new() -> Stats {
        return Stats { count: 0, sum: 0, min: None, max: None }
    }

    fn add(inout self: Stats, value: i32) {
        self.count = self.count + 1
        self.sum = self.sum + value

        match self.min {
            Some(m) => {
                if value < m { self.min = Some(value) }
            }
            None => { self.min = Some(value) }
        }

        match self.max {
            Some(m) => {
                if value > m { self.max = Some(value) }
            }
            None => { self.max = Some(value) }
        }
    }

    fn average(ref self: Stats) -> f64 {
        if self.count == 0 {
            return 0.0
        }
        return self.sum.as[f64] / self.count.as[f64]
    }
}
```

## Next Steps

- [Ownership](ownership.md) - Ownership model basics
- [Reference Counting](reference-counting.md) - Shared ownership
- [Structs](../language/structs.md) - Methods with self parameters
