# Structs

Structs are custom data types that group related values together.

## Defining Structs

```klar
struct Point {
    x: i32,
    y: i32,
}
```

### Creating Struct Instances

Use struct literals with field names:

```klar
let origin: Point = Point { x: 0, y: 0 }
let point: Point = Point { x: 10, y: 20 }
```

### Field Access

Access fields with dot notation:

```klar
let p: Point = Point { x: 10, y: 20 }
let x_val: i32 = p.x  // 10
let y_val: i32 = p.y  // 20
```

### Field Assignment

For mutable struct variables:

```klar
var p: Point = Point { x: 10, y: 20 }
p.x = 30
p.y = 40
```

## Methods

Add methods to structs using `impl` blocks:

```klar
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn sum(self: Point) -> i32 {
        return self.x + self.y
    }

    fn distance_from_origin(self: Point) -> f64 {
        let x: f64 = self.x.as[f64]
        let y: f64 = self.y.as[f64]
        return sqrt(x * x + y * y)
    }
}

fn main() -> i32 {
    let p: Point = Point { x: 3, y: 4 }
    let s: i32 = p.sum()            // 7
    let d: f64 = p.distance_from_origin()  // 5.0
    return s
}
```

### Self Parameter

Methods take `self` as the first parameter:

| Syntax | Description |
|--------|-------------|
| `self: Type` | Takes ownership (consumes the value) |
| `ref self: Type` | Borrows immutably (read-only) |
| `inout self: Type` | Borrows mutably (can modify) |

### Immutable Borrow (ref self)

```klar
impl Point {
    fn magnitude(ref self: Point) -> f64 {
        // Can read fields but not modify
        return sqrt(self.x * self.x + self.y * self.y)
    }
}
```

### Mutable Borrow (inout self)

```klar
impl Point {
    fn scale(inout self: Point, factor: i32) {
        self.x = self.x * factor
        self.y = self.y * factor
    }
}

fn main() -> i32 {
    var p: Point = Point { x: 10, y: 20 }
    p.scale(2)
    return p.x  // 20
}
```

## Associated Functions

Functions in `impl` blocks that don't take `self` are associated functions (like static methods):

```klar
impl Point {
    fn origin() -> Point {
        return Point { x: 0, y: 0 }
    }

    fn new(x: i32, y: i32) -> Point {
        return Point { x: x, y: y }
    }
}

fn main() -> i32 {
    let p1: Point = Point.origin()
    let p2: Point = Point.new(10, 20)
    return p2.x
}
```

## Generic Structs

Structs can have type parameters:

```klar
struct Pair[T] {
    first: T,
    second: T,
}

impl Pair[T] {
    fn swap(self: Pair[T]) -> Pair[T] {
        return Pair { first: self.second, second: self.first }
    }
}

fn main() -> i32 {
    let p: Pair[i32] = Pair { first: 10, second: 20 }
    let swapped: Pair[i32] = p.swap()
    return swapped.first  // 20
}
```

### Multiple Type Parameters

```klar
struct KeyValue[K, V] {
    key: K,
    value: V,
}
```

## Implementing Traits

Structs can implement traits:

```klar
trait Printable {
    fn to_string(self: Self) -> string
}

impl Point: Printable {
    fn to_string(self: Point) -> string {
        return "({self.x}, {self.y})"
    }
}
```

See [Traits](traits.md) for more details.

## Nested Structs

Structs can contain other structs:

```klar
struct Rectangle {
    top_left: Point,
    bottom_right: Point,
}

fn main() -> i32 {
    let rect: Rectangle = Rectangle {
        top_left: Point { x: 0, y: 0 },
        bottom_right: Point { x: 100, y: 50 },
    }
    return rect.bottom_right.x  // 100
}
```

## Example: Game Entity

```klar
struct Position {
    x: f64,
    y: f64,
}

struct Velocity {
    dx: f64,
    dy: f64,
}

struct Entity {
    position: Position,
    velocity: Velocity,
    health: i32,
}

impl Entity {
    fn new(x: f64, y: f64) -> Entity {
        return Entity {
            position: Position { x: x, y: y },
            velocity: Velocity { dx: 0.0, dy: 0.0 },
            health: 100,
        }
    }

    fn update(inout self: Entity, dt: f64) {
        self.position.x = self.position.x + self.velocity.dx * dt
        self.position.y = self.position.y + self.velocity.dy * dt
    }

    fn is_alive(ref self: Entity) -> bool {
        return self.health > 0
    }
}
```

## Next Steps

- [Enums](enums.md) - Enum definitions
- [Traits](traits.md) - Implementing traits
- [Generics](generics.md) - Generic structs
