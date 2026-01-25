# Functions

Functions are first-class values in Klar with explicit type annotations and return types.

## Function Declarations

### Basic Syntax

```klar
fn function_name(param1: Type1, param2: Type2) -> ReturnType {
    // function body
    return value
}
```

### Simple Function

```klar
fn add(a: i32, b: i32) -> i32 {
    return a + b
}

fn main() -> i32 {
    let result: i32 = add(10, 20)
    return result  // 30
}
```

### Void Functions

Functions that don't return a value use implicit `void` return:

```klar
fn greet(name: string) {
    println("Hello, {name}!")
}

fn main() -> i32 {
    greet("Alice")
    return 0
}
```

### Explicit Return

All return values must be explicit. There is no implicit return of the last expression:

```klar
fn double(n: i32) -> i32 {
    return n * 2  // Explicit return required
}

// This would be an error:
// fn double(n: i32) -> i32 { n * 2 }  // Missing return
```

### Early Return

Use `return` for early exit:

```klar
fn absolute(n: i32) -> i32 {
    if n < 0 {
        return -n
    }
    return n
}
```

## Generic Functions

Functions can be generic over types using `[T]` syntax:

```klar
fn identity[T](x: T) -> T {
    return x
}

fn main() -> i32 {
    let a: i32 = identity(42)       // T = i32
    let b: string = identity("hi")  // T = string
    return a
}
```

### Multiple Type Parameters

```klar
fn swap[T, U](a: T, b: U) -> (U, T) {
    return (b, a)
}
```

### Trait Bounds

Constrain generic types with trait bounds:

```klar
fn max[T: Ordered](a: T, b: T) -> T {
    if a > b {
        return a
    }
    return b
}
```

See [Generics](generics.md) and [Traits](traits.md) for more details.

## Closures

Closures are anonymous functions that can capture variables from their environment.

### Basic Closure Syntax

```klar
let double: fn(i32) -> i32 = |x: i32| -> i32 { return x * 2 }

let result: i32 = double(21)  // 42
```

### Closure Type Annotation

Closure types are written as `fn(ParamTypes) -> ReturnType`:

```klar
let add: fn(i32, i32) -> i32 = |a: i32, b: i32| -> i32 { return a + b }
```

### Capturing Variables

Closures can capture variables from their enclosing scope:

```klar
fn main() -> i32 {
    let factor: i32 = 5

    let multiply: fn(i32) -> i32 = |x: i32| -> i32 { return x * factor }

    return multiply(10)  // 50
}
```

### Multiple Captures

```klar
fn main() -> i32 {
    let a: i32 = 10
    let b: i32 = 20

    let compute: fn(i32) -> i32 = |x: i32| -> i32 { return x + a + b }

    return compute(5)  // 35
}
```

## Higher-Order Functions

Functions can take other functions as parameters:

```klar
fn apply(f: fn(i32) -> i32, x: i32) -> i32 {
    return f(x)
}

fn main() -> i32 {
    let double: fn(i32) -> i32 = |x: i32| -> i32 { return x * 2 }
    return apply(double, 21)  // 42
}
```

### Returning Functions

Functions can return other functions:

```klar
fn make_adder(n: i32) -> fn(i32) -> i32 {
    return |x: i32| -> i32 { return x + n }
}

fn main() -> i32 {
    let add5: fn(i32) -> i32 = make_adder(5)
    return add5(10)  // 15
}
```

## Recursion

Functions can call themselves recursively:

```klar
fn factorial(n: i32) -> i32 {
    if n <= 1 {
        return 1
    }
    return n * factorial(n - 1)
}

fn fibonacci(n: i32) -> i32 {
    if n <= 1 {
        return n
    }
    return fibonacci(n - 1) + fibonacci(n - 2)
}
```

## The main Function

The `main` function is the entry point of a Klar program:

```klar
// Basic main - returns exit code
fn main() -> i32 {
    return 0  // 0 = success
}

// With command-line arguments
fn main(args: [String]) -> i32 {
    println("Arguments: {args.len()}")
    return 0
}

// Void main (implicitly returns 0)
fn main() {
    println("Hello!")
}
```

## Parameter Passing

### By Value (default)

Parameters are passed by value by default (copied):

```klar
fn increment(n: i32) -> i32 {
    return n + 1
}
```

### By Reference (ref)

Use `ref` for read-only references:

```klar
fn length(ref arr: [i32]) -> i32 {
    return arr.len()
}
```

### By Mutable Reference (inout)

Use `inout` for mutable references:

```klar
fn double_in_place(inout n: i32) {
    n = n * 2
}

fn main() -> i32 {
    var x: i32 = 10
    double_in_place(inout x)
    return x  // 20
}
```

See [References](../memory/references.md) for more details.

## Next Steps

- [Control Flow](control-flow.md) - if, match, loops
- [Generics](generics.md) - Generic programming
- [Traits](traits.md) - Trait bounds on functions
