# Compile-Time Programming

Klar supports compile-time evaluation through special constructs prefixed with `@`.

## Comptime Blocks

Execute code at compile time using `@{ }`:

```klar
fn main() -> i32 {
    @{
        let x: i32 = 10
        let y: i32 = 20
        // This code runs at compile time
    }

    return 42
}
```

## Comptime Functions

Functions prefixed with `@` are evaluated at compile time:

```klar
fn @add(a: i32, b: i32) -> i32 {
    return a + b
}

fn @mul(a: i32, b: i32) -> i32 {
    return a * b
}

fn main() -> i32 {
    // These are computed at compile time
    let sum: i32 = @add(2, 3)       // 5 (computed at compile time)
    let product: i32 = @mul(4, 5)   // 20 (computed at compile time)

    // Nested comptime calls
    let nested: i32 = @add(@mul(2, 3), 4)  // 10

    return sum + product
}
```

### Comptime Function Rules

1. Can only call other comptime functions or perform basic operations
2. Results are embedded as constants in the final binary
3. Cannot have side effects (I/O, etc.)

```klar
fn @factorial(n: i32) -> i32 {
    if n <= 1 {
        return 1
    }
    return n * @factorial(n - 1)
}

fn main() -> i32 {
    let result: i32 = @factorial(5)  // 120, computed at compile time
    return result
}
```

## Comptime Parameters

Functions can have comptime parameters for generic computation:

```klar
fn make_array[comptime N: i32]() -> [i32; N] {
    // N is known at compile time
    return @repeat(0, N)
}

fn main() -> i32 {
    let arr: [i32; 5] = make_array[5]()
    return arr.len()
}
```

## Type Introspection

### @typeName

Get the name of a type as a string:

```klar
fn main() -> i32 {
    let name1: string = @typeName[i32]       // "i32"
    let name2: string = @typeName[string]    // "string"
    let name3: string = @typeName[Point]     // "Point"

    println(name1)
    return 0
}
```

### @typeInfo

Get information about a type:

```klar
struct Point {
    x: i32,
    y: i32,
}

fn main() -> i32 {
    // Check if type is a struct
    let info: TypeInfo = @typeInfo[Point]

    match info {
        TypeInfo.Struct(s) => {
            println("Struct with {s.fields.len()} fields")
        }
        _ => { }
    }

    return 0
}
```

### @hasField

Check if a struct has a specific field:

```klar
struct User {
    name: string,
    age: i32,
}

fn main() -> i32 {
    let has_name: bool = @hasField[User]("name")   // true
    let has_email: bool = @hasField[User]("email") // false

    return 0
}
```

### @fields

Get field information for a struct:

```klar
fn print_fields[T]() {
    let fields: [FieldInfo] = @fields[T]
    for field: FieldInfo in fields {
        println("Field: {field.name}, Type: {field.type_name}")
    }
}

fn main() -> i32 {
    print_fields[Point]()
    // Output:
    // Field: x, Type: i32
    // Field: y, Type: i32
    return 0
}
```

## Comptime Assertions

### @assert

Assert conditions at compile time:

```klar
fn @check_positive(n: i32) {
    @assert(n > 0, "value must be positive")
}

fn main() -> i32 {
    @check_positive(10)  // OK
    // @check_positive(-1)  // Compile error: value must be positive
    return 0
}
```

### @compileError

Emit a custom compile-time error:

```klar
fn @validate_size[comptime N: i32]() {
    if N <= 0 {
        @compileError("size must be positive")
    }
    if N > 1000 {
        @compileError("size too large")
    }
}
```

## Array Repeat

Create arrays with repeated values:

```klar
let zeros: [i32; 10] = @repeat(0, 10)
let ones: [bool; 5] = @repeat(true, 5)
```

## Conditional Compilation

Use comptime conditions for conditional code:

```klar
fn @is_debug_build() -> bool {
    // Check build configuration
    return DEBUG_MODE
}

fn main() -> i32 {
    @{
        if @is_debug_build() {
            // Include debug code
        }
    }
    return 0
}
```

## Example: Compile-Time Validation

```klar
struct Config {
    port: i32,
    max_connections: i32,
}

fn @validate_config(config: Config) {
    @assert(config.port > 0 and config.port < 65536, "invalid port")
    @assert(config.max_connections > 0, "max_connections must be positive")
}

// Configuration is validated at compile time
const APP_CONFIG: Config = @{
    let config: Config = Config {
        port: 8080,
        max_connections: 100,
    }
    @validate_config(config)
    config
}
```

## Example: Compile-Time Lookup Tables

```klar
fn @generate_squares(comptime n: i32) -> [i32; n] {
    var result: [i32; n] = @repeat(0, n)
    for i: i32 in 0..n {
        result[i] = i * i
    }
    return result
}

// Table generated at compile time
const SQUARES: [i32; 10] = @generate_squares(10)

fn main() -> i32 {
    println("5 squared = {SQUARES[5]}")  // 25
    return 0
}
```

## Example: Generic Serialization

```klar
fn serialize_struct[T](value: T) -> string {
    var result: string = "{"
    let fields: [FieldInfo] = @fields[T]

    for (i, field) in fields.enumerate() {
        if i > 0 {
            result = result + ", "
        }
        let field_value = @field(value, field.name)
        result = result + "\"{field.name}\": {field_value}"
    }

    return result + "}"
}
```

## Limitations

Comptime code has restrictions:

1. **No I/O**: Can't read files, print, etc.
2. **No allocation**: Can't use dynamic collections
3. **Pure computation**: Must be deterministic
4. **Limited recursion**: Depth limits apply

```klar
// Invalid - I/O in comptime
fn @bad() {
    println("Hello")  // Error: I/O not allowed at comptime
}

// Invalid - dynamic allocation
fn @also_bad() {
    var list: List[i32] = List.new[i32]()  // Error: allocation not allowed
}
```

## Best Practices

### Use Comptime for Constants

```klar
const PI: f64 = @{ 3.14159265358979 }
const MAX_SIZE: i32 = @mul(1024, 1024)
```

### Use Comptime for Validation

```klar
fn @valid_port(port: i32) -> i32 {
    @assert(port > 0 and port < 65536, "invalid port")
    return port
}

const PORT: i32 = @valid_port(8080)
```

### Use Comptime for Code Generation

```klar
fn @generate_handler[T]() {
    // Generate specialized code based on type
}
```

## Next Steps

- [Generics](../language/generics.md) - Generic programming
- [Builtin Traits](builtin-traits.md) - Standard traits
- [Operators](operators.md) - All operators
