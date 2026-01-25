# Traits

Traits define shared behavior that types can implement. They are similar to interfaces or type classes in other languages.

## Defining Traits

```klar
trait Describable {
    fn describe(self: Self) -> string
}
```

- `Self` refers to the implementing type
- Methods must be implemented by each type

## Implementing Traits

Use `impl Type: Trait` syntax:

```klar
struct Point {
    x: i32,
    y: i32,
}

impl Point: Describable {
    fn describe(self: Point) -> string {
        return "Point({self.x}, {self.y})"
    }
}
```

### Using Trait Methods

```klar
let p: Point = Point { x: 10, y: 20 }
let desc: string = p.describe()  // "Point(10, 20)"
```

## Multiple Traits

A type can implement multiple traits:

```klar
trait Printable {
    fn to_string(self: Self) -> string
}

trait Comparable {
    fn compare(self: Self, other: Self) -> i32
}

impl Point: Printable {
    fn to_string(self: Point) -> string {
        return "({self.x}, {self.y})"
    }
}

impl Point: Comparable {
    fn compare(self: Point, other: Point) -> i32 {
        let diff: i32 = (self.x - other.x) + (self.y - other.y)
        return diff
    }
}
```

## Default Method Implementations

Traits can provide default implementations:

```klar
trait Greetable {
    fn name(self: Self) -> string

    fn greet(self: Self) -> string {
        return "Hello, {self.name()}!"
    }
}

struct Person {
    full_name: string,
}

impl Person: Greetable {
    fn name(self: Person) -> string {
        return self.full_name
    }
    // greet() uses the default implementation
}
```

## Trait Bounds

Constrain generic types to types implementing specific traits:

```klar
fn print_description[T: Describable](item: T) {
    println(item.describe())
}
```

### Multiple Bounds

Use `+` to require multiple traits:

```klar
fn process[T: Printable + Comparable](a: T, b: T) {
    println(a.to_string())
    if a.compare(b) > 0 {
        println("a is greater")
    }
}
```

## Trait Inheritance

Traits can inherit from other traits:

```klar
trait Base {
    fn base_method(self: Self) -> i32
}

trait Derived: Base {
    fn derived_method(self: Self) -> i32
}
```

When implementing `Derived`, you must also implement `Base`:

```klar
struct MyType {
    value: i32,
}

impl MyType: Derived {
    fn base_method(self: MyType) -> i32 {
        return self.value
    }

    fn derived_method(self: MyType) -> i32 {
        return self.value * 2
    }
}
```

### Multiple Inheritance

```klar
trait A {
    fn method_a(self: Self) -> i32
}

trait B {
    fn method_b(self: Self) -> i32
}

trait C: A + B {
    fn method_c(self: Self) -> i32
}
```

## Associated Types

Traits can have associated types:

```klar
trait Container {
    type Item

    fn get(self: Self, index: i32) -> Self.Item
    fn len(self: Self) -> i32
}
```

### Implementing Associated Types

```klar
struct IntArray {
    data: [i32],
}

impl IntArray: Container {
    type Item = i32

    fn get(self: IntArray, index: i32) -> i32 {
        return self.data[index]
    }

    fn len(self: IntArray) -> i32 {
        return self.data.len()
    }
}
```

## Built-in Traits

Klar provides several built-in traits:

| Trait | Description | Methods |
|-------|-------------|---------|
| `Eq` | Equality comparison | `eq(self, other) -> bool` |
| `Ordered` | Ordering comparison | `cmp(self, other) -> i32` |
| `Clone` | Create a copy | `clone(self) -> Self` |
| `Drop` | Cleanup when destroyed | `drop(inout self)` |
| `Default` | Default value | `default() -> Self` |
| `Hash` | Hash value | `hash(self) -> u64` |
| `Iterator` | Iteration | `next(inout self) -> ?Self.Item` |

See [Builtin Traits](../advanced/builtin-traits.md) for details.

### Deriving Built-in Traits

Primitive types automatically implement appropriate traits:

```klar
// i32 implements Eq, Ordered, Clone, Default, Hash
let a: i32 = 10
let b: i32 = 20
let equal: bool = a == b  // Uses Eq
let less: bool = a < b    // Uses Ordered
```

## Example: Summable Trait

```klar
trait Summable {
    fn sum(self: Self) -> i32
}

struct Point {
    x: i32,
    y: i32,
}

impl Point: Summable {
    fn sum(self: Point) -> i32 {
        return self.x + self.y
    }
}

struct Rectangle {
    width: i32,
    height: i32,
}

impl Rectangle: Summable {
    fn sum(self: Rectangle) -> i32 {
        return self.width + self.height
    }
}

fn total[T: Summable](items: [T]) -> i32 {
    var total: i32 = 0
    for item: T in items {
        total = total + item.sum()
    }
    return total
}
```

## Example: Iterator Pattern

```klar
trait Iterator {
    type Item

    fn next(inout self: Self) -> ?Self.Item
}

struct RangeIter {
    current: i32,
    end: i32,
}

impl RangeIter: Iterator {
    type Item = i32

    fn next(inout self: RangeIter) -> ?i32 {
        if self.current >= self.end {
            return None
        }
        let value: i32 = self.current
        self.current = self.current + 1
        return Some(value)
    }
}
```

## Next Steps

- [Generics](generics.md) - Generic programming with trait bounds
- [Builtin Traits](../advanced/builtin-traits.md) - Standard trait implementations
- [Structs](structs.md) - Implementing traits on structs
