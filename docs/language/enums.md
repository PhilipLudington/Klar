# Enums

Enums define types with a fixed set of variants, optionally carrying data.

## Basic Enums

### Definition

```klar
enum Color {
    Red,
    Green,
    Blue,
}
```

### Creating Enum Values

Use `::` syntax to create enum variants:

```klar
let c: Color = Color::Red
let g: Color = Color::Green
```

### Pattern Matching

Use `match` to handle enum variants:

```klar
fn to_rgb(c: Color) -> (i32, i32, i32) {
    var r: i32 = 0
    var g: i32 = 0
    var b: i32 = 0

    match c {
        Color.Red => { r = 255 }
        Color.Green => { g = 255 }
        Color.Blue => { b = 255 }
    }

    return (r, g, b)
}
```

Note: Match patterns use `.` (not `::`) for variants.

## Enums with Payloads

Variants can carry data:

```klar
enum Message {
    Text(string),
    Number(i32),
    Position(i32, i32),
    Empty,
}
```

### Creating Payload Variants

```klar
let m1: Message = Message::Text("hello")
let m2: Message = Message::Number(42)
let m3: Message = Message::Position(10, 20)
let m4: Message = Message::Empty
```

### Extracting Payloads

Use pattern matching to extract data:

```klar
fn process(msg: Message) {
    match msg {
        Message.Text(s) => {
            println("Text: {s}")
        }
        Message.Number(n) => {
            println("Number: {n}")
        }
        Message.Position(x, y) => {
            println("Position: ({x}, {y})")
        }
        Message.Empty => {
            println("Empty message")
        }
    }
}
```

## Generic Enums

Enums can have type parameters:

```klar
enum MyOption[T] {
    MySome(T),
    MyNone,
}
```

### Creating Generic Enum Values

Specify the type parameter with the enum name:

```klar
let some_int: MyOption[i32] = MyOption[i32]::MySome(42)
let none_int: MyOption[i32] = MyOption[i32]::MyNone
```

### Pattern Matching Generic Enums

```klar
fn unwrap_or[T](opt: MyOption[T], default: T) -> T {
    var result: T
    match opt {
        MyOption.MySome(value) => { result = value }
        MyOption.MyNone => { result = default }
    }
    return result
}
```

## Built-in Enums

### Option[T] (written as ?T)

The optional type for values that may or may not exist:

```klar
let some_value: ?i32 = Some(42)
let no_value: ?i32 = None
```

See [Optional](../types/optional.md) for details.

### Result[T, E]

The result type for operations that may fail:

```klar
let success: Result[i32, string] = Ok(42)
let failure: Result[i32, string] = Err("something went wrong")
```

See [Result](../types/result.md) for details.

## Implementing Methods on Enums

Use `impl` blocks to add methods:

```klar
enum Direction {
    North,
    South,
    East,
    West,
}

impl Direction {
    fn opposite(self: Direction) -> Direction {
        var result: Direction
        match self {
            Direction.North => { result = Direction::South }
            Direction.South => { result = Direction::North }
            Direction.East => { result = Direction::West }
            Direction.West => { result = Direction::East }
        }
        return result
    }
}
```

## Implementing Traits on Enums

```klar
trait Describable {
    fn describe(self: Self) -> string
}

impl Color: Describable {
    fn describe(self: Color) -> string {
        var desc: string
        match self {
            Color.Red => { desc = "The color red" }
            Color.Green => { desc = "The color green" }
            Color.Blue => { desc = "The color blue" }
        }
        return desc
    }
}
```

## Example: Expression Tree

```klar
enum Expr {
    Number(i32),
    Add(Rc[Expr], Rc[Expr]),
    Multiply(Rc[Expr], Rc[Expr]),
}

fn eval(e: Expr) -> i32 {
    var result: i32
    match e {
        Expr.Number(n) => {
            result = n
        }
        Expr.Add(left, right) => {
            result = eval(left.get()) + eval(right.get())
        }
        Expr.Multiply(left, right) => {
            result = eval(left.get()) * eval(right.get())
        }
    }
    return result
}
```

## Example: State Machine

```klar
enum ConnectionState {
    Disconnected,
    Connecting(string),  // server address
    Connected(i32),      // connection id
    Error(string),       // error message
}

fn describe_state(state: ConnectionState) -> string {
    var desc: string
    match state {
        ConnectionState.Disconnected => {
            desc = "Not connected"
        }
        ConnectionState.Connecting(addr) => {
            desc = "Connecting to {addr}..."
        }
        ConnectionState.Connected(id) => {
            desc = "Connected (id: {id})"
        }
        ConnectionState.Error(msg) => {
            desc = "Error: {msg}"
        }
    }
    return desc
}
```

## Next Steps

- [Traits](traits.md) - Implementing traits on enums
- [Generics](generics.md) - Generic enums
- [Error Handling](error-handling.md) - Using Result and Option
