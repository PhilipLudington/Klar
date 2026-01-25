# Builtin Traits

Klar provides several built-in traits that enable core language functionality.

## Eq - Equality

The `Eq` trait enables equality comparison with `==` and `!=`.

```klar
trait Eq {
    fn eq(self: Self, other: Self) -> bool
}
```

### Implementing Eq

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

fn main() -> i32 {
    let p1: Point = Point { x: 1, y: 2 }
    let p2: Point = Point { x: 1, y: 2 }
    let p3: Point = Point { x: 3, y: 4 }

    println(p1 == p2)  // true
    println(p1 != p3)  // true
    return 0
}
```

### Types with Built-in Eq

- All integer types (`i32`, `u64`, etc.)
- All floating types (`f32`, `f64`)
- `bool`
- `char`
- `string`

## Ordered - Ordering

The `Ordered` trait enables comparison operators: `<`, `<=`, `>`, `>=`.

```klar
trait Ordered {
    fn cmp(self: Self, other: Self) -> i32
    // Returns: negative if self < other, 0 if equal, positive if self > other
}
```

### Implementing Ordered

```klar
struct Score {
    value: i32,
}

impl Score: Ordered {
    fn cmp(self: Score, other: Score) -> i32 {
        return self.value - other.value
    }
}

fn main() -> i32 {
    let s1: Score = Score { value: 100 }
    let s2: Score = Score { value: 85 }

    println(s1 > s2)   // true
    println(s1 < s2)   // false
    println(s1 >= s2)  // true
    return 0
}
```

### Using in Generic Functions

```klar
fn max[T: Ordered](a: T, b: T) -> T {
    if a > b {
        return a
    }
    return b
}

fn min[T: Ordered](a: T, b: T) -> T {
    if a < b {
        return a
    }
    return b
}
```

## Clone - Copying

The `Clone` trait enables explicit copying of values.

```klar
trait Clone {
    fn clone(self: Self) -> Self
}
```

### Implementing Clone

```klar
struct Data {
    values: List[i32],
}

impl Data: Clone {
    fn clone(self: Data) -> Data {
        var new_values: List[i32] = List.new[i32]()
        for v: i32 in self.values {
            new_values.push(v)
        }
        return Data { values: new_values }
    }
}

fn main() -> i32 {
    var original: Data = Data { values: List.new[i32]() }
    original.values.push(1)
    original.values.push(2)

    let copy: Data = original.clone()
    // copy has its own independent list

    return 0
}
```

### Types with Built-in Clone

- All primitive types (integers, floats, bool, char)
- `string`
- `Rc[T]` and `Arc[T]` (shallow clone - shares underlying data)

## Drop - Cleanup

The `Drop` trait allows custom cleanup when a value goes out of scope.

```klar
trait Drop {
    fn drop(inout self: Self)
}
```

### Implementing Drop

```klar
struct Connection {
    id: i32,
}

impl Connection: Drop {
    fn drop(inout self: Connection) {
        println("Closing connection {self.id}")
        // Cleanup resources
    }
}

fn main() -> i32 {
    let conn: Connection = Connection { id: 42 }
    // Use connection...
    return 0
}  // Prints: "Closing connection 42"
```

### Drop Order

Values are dropped in reverse declaration order:

```klar
fn example() {
    let a: Resource = acquire()  // Dropped third
    let b: Resource = acquire()  // Dropped second
    let c: Resource = acquire()  // Dropped first
}
```

## Default - Default Values

The `Default` trait provides a default value for a type.

```klar
trait Default {
    fn default() -> Self
}
```

### Implementing Default

```klar
struct Config {
    port: i32,
    timeout: i32,
}

impl Config: Default {
    fn default() -> Config {
        return Config {
            port: 8080,
            timeout: 30,
        }
    }
}

fn main() -> i32 {
    let config: Config = Config.default()
    println(config.port)  // 8080
    return 0
}
```

### Types with Built-in Default

| Type | Default Value |
|------|---------------|
| Integer types | `0` |
| Float types | `0.0` |
| `bool` | `false` |
| `char` | `'\0'` |
| `string` | `""` |

## Hash - Hashing

The `Hash` trait enables a type to be used as a key in hash-based collections.

```klar
trait Hash {
    fn hash(self: Self) -> u64
}
```

### Implementing Hash

```klar
struct UserId {
    id: i32,
}

impl UserId: Hash {
    fn hash(self: UserId) -> u64 {
        return self.id.as[u64]
    }
}

// Now UserId can be used as Map key
var users: Map[UserId, string] = Map.new[UserId, string]()
```

### Hash + Eq

Types used as map/set keys need both `Hash` and `Eq`:

```klar
impl UserId: Eq {
    fn eq(self: UserId, other: UserId) -> bool {
        return self.id == other.id
    }
}
```

## Iterator - Iteration

The `Iterator` trait enables `for-in` loops over a type.

```klar
trait Iterator {
    type Item

    fn next(inout self: Self) -> ?Self.Item
}
```

### Implementing Iterator

```klar
struct Counter {
    current: i32,
    max: i32,
}

impl Counter: Iterator {
    type Item = i32

    fn next(inout self: Counter) -> ?i32 {
        if self.current >= self.max {
            return None
        }
        let value: i32 = self.current
        self.current = self.current + 1
        return Some(value)
    }
}

fn main() -> i32 {
    var counter: Counter = Counter { current: 0, max: 5 }
    for i: i32 in counter {
        println("{i}")  // 0, 1, 2, 3, 4
    }
    return 0
}
```

### Types with Built-in Iterator

- `Range[T]`
- `List[T]`
- `Set[T]`
- `Map[K, V]` (iterates as `(K, V)` tuples)
- Arrays `[T; N]`

## Read - Reading

The `Read` trait enables reading bytes from a source.

```klar
trait Read {
    fn read(inout self: Self, buffer: inout [u8]) -> Result[i32, IoError]
}
```

## Write - Writing

The `Write` trait enables writing bytes to a destination.

```klar
trait Write {
    fn write(inout self: Self, data: [u8]) -> Result[i32, IoError]
    fn flush(inout self: Self) -> Result[(), IoError]
}
```

## From - Conversion From

The `From` trait enables conversion from one type to another.

```klar
trait From[T] {
    fn from(value: T) -> Self
}
```

### Implementing From

```klar
struct Celsius {
    value: f64,
}

struct Fahrenheit {
    value: f64,
}

impl Celsius: From[Fahrenheit] {
    fn from(f: Fahrenheit) -> Celsius {
        return Celsius { value: (f.value - 32.0) * 5.0 / 9.0 }
    }
}

fn main() -> i32 {
    let f: Fahrenheit = Fahrenheit { value: 98.6 }
    let c: Celsius = Celsius.from(f)
    println("Temperature: {c.value}C")
    return 0
}
```

## Into - Conversion Into

The `Into` trait is the reciprocal of `From`.

```klar
trait Into[T] {
    fn into(self: Self) -> T
}
```

If `A: From[B]` is implemented, `B: Into[A]` is automatically available.

## Trait Relationships

Some traits depend on others:

```
Ordered : Eq  (Ordered requires Eq)
```

When implementing a derived trait, you must also implement the base trait:

```klar
// Must implement both Eq and Ordered
impl MyType: Eq {
    fn eq(self: MyType, other: MyType) -> bool { ... }
}

impl MyType: Ordered {
    fn cmp(self: MyType, other: MyType) -> i32 { ... }
}
```

## Next Steps

- [Traits](../language/traits.md) - Custom trait definitions
- [Generics](../language/generics.md) - Using trait bounds
- [Collections](../types/collections.md) - Types that use these traits
