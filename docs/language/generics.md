# Generics

Generics allow you to write code that works with multiple types while maintaining type safety.

## Generic Functions

### Basic Syntax

Use `[T]` to declare type parameters:

```klar
fn identity[T](x: T) -> T {
    return x
}
```

### Using Generic Functions

Type parameters are inferred from arguments:

```klar
let a: i32 = identity(42)       // T = i32
let b: string = identity("hi")  // T = string
let c: bool = identity(true)    // T = bool
```

### Multiple Type Parameters

```klar
fn pair[T, U](first: T, second: U) -> (T, U) {
    return (first, second)
}

let p: (i32, string) = pair(42, "hello")
```

## Generic Structs

### Definition

```klar
struct Box[T] {
    value: T,
}
```

### Creating Instances

Specify the type parameter:

```klar
let int_box: Box[i32] = Box { value: 42 }
let str_box: Box[string] = Box { value: "hello" }
```

### Methods on Generic Structs

```klar
impl Box[T] {
    fn get(self: Box[T]) -> T {
        return self.value
    }

    fn set(inout self: Box[T], new_value: T) {
        self.value = new_value
    }
}
```

### Multiple Type Parameters

```klar
struct Pair[T, U] {
    first: T,
    second: U,
}

impl Pair[T, U] {
    fn swap(self: Pair[T, U]) -> Pair[U, T] {
        return Pair { first: self.second, second: self.first }
    }
}
```

## Generic Enums

```klar
enum MyOption[T] {
    MySome(T),
    MyNone,
}
```

### Creating Generic Enum Values

```klar
let some_int: MyOption[i32] = MyOption[i32]::MySome(42)
let none_int: MyOption[i32] = MyOption[i32]::MyNone

let some_str: MyOption[string] = MyOption[string]::MySome("hello")
```

### Methods on Generic Enums

```klar
impl MyOption[T] {
    fn is_some(self: MyOption[T]) -> bool {
        var result: bool
        match self {
            MyOption.MySome(_) => { result = true }
            MyOption.MyNone => { result = false }
        }
        return result
    }

    fn unwrap_or(self: MyOption[T], default: T) -> T {
        var result: T
        match self {
            MyOption.MySome(value) => { result = value }
            MyOption.MyNone => { result = default }
        }
        return result
    }
}
```

## Trait Bounds

Constrain type parameters to types implementing specific traits:

```klar
fn max[T: Ordered](a: T, b: T) -> T {
    if a > b {
        return a
    }
    return b
}
```

### Multiple Bounds

Use `+` to require multiple traits:

```klar
fn print_and_compare[T: Eq + Clone](a: T, b: T) -> bool {
    let a_copy: T = a.clone()
    return a_copy == b
}
```

### Bounds on Struct Type Parameters

```klar
struct SortedList[T: Ordered] {
    items: List[T],
}

impl SortedList[T: Ordered] {
    fn insert(inout self: SortedList[T], value: T) {
        // Can use comparison operators because T: Ordered
        // ...
    }
}
```

## Monomorphization

Klar uses monomorphization to implement generics. Each unique type instantiation creates a specialized version of the code:

```klar
fn identity[T](x: T) -> T {
    return x
}

// These create two specialized functions:
identity(42)      // identity_i32
identity("hello") // identity_string
```

This means:

- No runtime overhead for generics
- Type errors are caught at compile time
- Binary size increases with more type instantiations

## Type Inference

Type parameters are usually inferred:

```klar
// Inferred from argument
let a: i32 = identity(42)

// Inferred from expected type
let b: string = identity("hello")

// Sometimes explicit type is needed
let list: List[i32] = List.new[i32]()
```

## Generic Constraints Example

```klar
trait Addable {
    fn add(self: Self, other: Self) -> Self
}

impl i32: Addable {
    fn add(self: i32, other: i32) -> i32 {
        return self + other
    }
}

fn sum_all[T: Addable](items: [T], zero: T) -> T {
    var total: T = zero
    for item: T in items {
        total = total.add(item)
    }
    return total
}
```

## Example: Generic Container

```klar
struct Stack[T] {
    items: List[T],
}

impl Stack[T] {
    fn new() -> Stack[T] {
        return Stack { items: List.new[T]() }
    }

    fn push(inout self: Stack[T], value: T) {
        self.items.push(value)
    }

    fn pop(inout self: Stack[T]) -> ?T {
        if self.items.len() == 0 {
            return None
        }
        return Some(self.items.pop())
    }

    fn is_empty(ref self: Stack[T]) -> bool {
        return self.items.len() == 0
    }
}

fn main() -> i32 {
    var stack: Stack[i32] = Stack.new[i32]()
    stack.push(1)
    stack.push(2)
    stack.push(3)

    while not stack.is_empty() {
        let value: ?i32 = stack.pop()
        println("{value!}")
    }

    return 0
}
```

## Example: Generic Result Handling

```klar
fn map_result[T, U, E](
    result: Result[T, E],
    f: fn(T) -> U
) -> Result[U, E] {
    match result {
        Ok(value) => { return Ok(f(value)) }
        Err(e) => { return Err(e) }
    }
}
```

## Next Steps

- [Traits](traits.md) - Trait definitions and bounds
- [Builtin Traits](../advanced/builtin-traits.md) - Standard traits
- [Error Handling](error-handling.md) - Generic Result and Option types
