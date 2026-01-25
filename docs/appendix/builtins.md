# Built-in Functions

Klar provides built-in functions that are available without imports.

## Output Functions

### print

Print a string without a trailing newline.

```klar
print("Hello ")
print("World!")
// Output: Hello World!
```

**Signature:** `fn print(s: string)`

### println

Print a string with a trailing newline.

```klar
println("Hello, World!")
// Output: Hello, World!\n
```

**Signature:** `fn println(s: string)`

### String Interpolation

Both `print` and `println` support string interpolation:

```klar
let name: string = "Alice"
let age: i32 = 30
println("Name: {name}, Age: {age}")
// Output: Name: Alice, Age: 30
```

## Input Functions

### readline

Read a line from standard input.

```klar
print("Enter your name: ")
let name: string = readline()
println("Hello, {name}!")
```

**Signature:** `fn readline() -> string`

## Assertion Functions

### assert

Assert that a condition is true. Panics if false.

```klar
let x: i32 = 10
assert(x > 0)           // OK
assert(x > 100)         // Panic: assertion failed
```

**Signature:** `fn assert(condition: bool)`

### assert_eq

Assert that two values are equal. Panics if not equal.

```klar
let a: i32 = 42
let b: i32 = 42
assert_eq(a, b)         // OK
assert_eq(a, 100)       // Panic: assertion failed: 42 != 100
```

**Signature:** `fn assert_eq[T: Eq](left: T, right: T)`

## Debugging Functions

### dbg

Print a value with its expression for debugging. Returns the value.

```klar
let x: i32 = 10
let y: i32 = dbg(x + 5)  // Prints: [dbg] x + 5 = 15
// y is 15
```

**Signature:** `fn dbg[T](value: T) -> T`

### panic

Terminate the program with an error message.

```klar
fn divide(a: i32, b: i32) -> i32 {
    if b == 0 {
        panic("division by zero")
    }
    return a / b
}
```

**Signature:** `fn panic(message: string) -> !`

## Type Information

### len

Get the length of a collection or string.

```klar
let s: string = "hello"
let n: i32 = len(s)  // 5

let arr: [i32; 3] = [1, 2, 3]
let m: i32 = len(arr)  // 3
```

**Signature:** `fn len[T](collection: T) -> i32`

Most collections also have a `.len()` method:

```klar
let length: i32 = "hello".len()  // 5
```

### type_name

Get the name of a type as a string (runtime version).

```klar
let x: i32 = 42
println(type_name(x))  // "i32"

struct Point { x: i32, y: i32 }
let p: Point = Point { x: 1, y: 2 }
println(type_name(p))  // "Point"
```

**Signature:** `fn type_name[T](value: T) -> string`

## Mathematical Functions

### sqrt

Square root (for floating point types).

```klar
let x: f64 = 16.0
let root: f64 = sqrt(x)  // 4.0
```

**Signature:** `fn sqrt(x: f64) -> f64`

### abs

Absolute value.

```klar
let x: i32 = -42
let a: i32 = abs(x)  // 42

let y: f64 = -3.14
let b: f64 = abs(y)  // 3.14
```

**Signatures:**
- `fn abs(x: i32) -> i32`
- `fn abs(x: f64) -> f64`

### min / max

Minimum and maximum of two values.

```klar
let a: i32 = 10
let b: i32 = 20

let smaller: i32 = min(a, b)  // 10
let larger: i32 = max(a, b)   // 20
```

**Signatures:**
- `fn min[T: Ordered](a: T, b: T) -> T`
- `fn max[T: Ordered](a: T, b: T) -> T`

## Memory Functions

### drop

Explicitly drop a value (call its Drop implementation).

```klar
var list: List[i32] = List.new[i32]()
list.push(1)
list.push(2)
drop(list)  // Explicitly free memory
```

**Signature:** `fn drop[T](value: T)`

Note: Values are automatically dropped when they go out of scope. Use `drop` only when you need to free resources early.

## Collection Constructors

### Some / None

Create optional values.

```klar
let present: ?i32 = Some(42)
let absent: ?i32 = None
```

### Ok / Err

Create result values.

```klar
let success: Result[i32, string] = Ok(42)
let failure: Result[i32, string] = Err("error message")
```

## Comptime Builtins

These are available only in comptime contexts:

| Function | Description |
|----------|-------------|
| `@typeName[T]` | Get type name at compile time |
| `@typeInfo[T]` | Get type metadata |
| `@hasField[T](name)` | Check if struct has field |
| `@fields[T]` | Get struct field information |
| `@assert(cond, msg)` | Compile-time assertion |
| `@compileError(msg)` | Emit compile error |
| `@repeat(value, n)` | Create array with repeated value |

See [Comptime](../advanced/comptime.md) for details.

## Summary Table

| Function | Description | Return Type |
|----------|-------------|-------------|
| `print(s)` | Print without newline | `void` |
| `println(s)` | Print with newline | `void` |
| `readline()` | Read line from stdin | `string` |
| `assert(cond)` | Assert condition | `void` |
| `assert_eq(a, b)` | Assert equality | `void` |
| `dbg(value)` | Debug print | `T` |
| `panic(msg)` | Terminate program | `!` |
| `len(coll)` | Get length | `i32` |
| `type_name(val)` | Get type name | `string` |
| `sqrt(x)` | Square root | `f64` |
| `abs(x)` | Absolute value | `T` |
| `min(a, b)` | Minimum | `T` |
| `max(a, b)` | Maximum | `T` |
| `drop(val)` | Explicit drop | `void` |
| `Some(val)` | Create Some | `?T` |
| `None` | None value | `?T` |
| `Ok(val)` | Create Ok | `Result[T, E]` |
| `Err(val)` | Create Err | `Result[T, E]` |

## Next Steps

- [Keywords](keywords.md) - Reserved keywords
- [Primitives](../types/primitives.md) - Built-in types
- [Operators](../advanced/operators.md) - All operators
