# Keywords

Reserved keywords in Klar that cannot be used as identifiers.

## Declaration Keywords

| Keyword | Description | Example |
|---------|-------------|---------|
| `let` | Immutable variable | `let x: i32 = 42` |
| `var` | Mutable variable | `var count: i32 = 0` |
| `fn` | Function definition | `fn add(a: i32, b: i32) -> i32` |
| `struct` | Struct definition | `struct Point { x: i32, y: i32 }` |
| `enum` | Enum definition | `enum Color { Red, Green, Blue }` |
| `trait` | Trait definition | `trait Printable { fn print(self: Self) }` |
| `impl` | Implementation block | `impl Point: Printable { ... }` |
| `type` | Type alias | `type Item = i32` |
| `const` | Constant | `const MAX: i32 = 100` |

## Control Flow Keywords

| Keyword | Description | Example |
|---------|-------------|---------|
| `if` | Conditional | `if x > 0 { ... }` |
| `else` | Alternative branch | `if x > 0 { ... } else { ... }` |
| `match` | Pattern matching | `match value { ... }` |
| `for` | For loop | `for i: i32 in 0..10 { ... }` |
| `while` | While loop | `while x > 0 { ... }` |
| `loop` | Infinite loop | `loop { ... break }` |
| `break` | Exit loop | `break` |
| `continue` | Skip iteration | `continue` |
| `return` | Return from function | `return value` |
| `in` | Iterator binding | `for x: i32 in list` |

## Type Keywords

| Keyword | Description | Example |
|---------|-------------|---------|
| `i8`, `i16`, `i32`, `i64` | Signed integers | `let x: i32 = 42` |
| `u8`, `u16`, `u32`, `u64` | Unsigned integers | `let x: u32 = 42` |
| `f32`, `f64` | Floating point | `let x: f64 = 3.14` |
| `bool` | Boolean | `let x: bool = true` |
| `char` | Character | `let x: char = 'a'` |
| `string` | String | `let x: string = "hello"` |
| `void` | No value | `fn print(s: string)` |
| `Self` | Current type in trait | `fn clone(self: Self) -> Self` |

## Literal Keywords

| Keyword | Description | Example |
|---------|-------------|---------|
| `true` | Boolean true | `let x: bool = true` |
| `false` | Boolean false | `let x: bool = false` |
| `None` | Optional none | `let x: ?i32 = None` |
| `Some` | Optional some | `let x: ?i32 = Some(42)` |
| `Ok` | Result success | `let x: Result[i32, E] = Ok(42)` |
| `Err` | Result error | `let x: Result[T, string] = Err("fail")` |

## Logical Operators

| Keyword | Description | Example |
|---------|-------------|---------|
| `and` | Logical AND | `a and b` |
| `or` | Logical OR | `a or b` |
| `not` | Logical NOT | `not a` |

## Reference Keywords

| Keyword | Description | Example |
|---------|-------------|---------|
| `ref` | Read-only reference | `fn f(ref x: Data)` |
| `inout` | Mutable reference | `fn f(inout x: Data)` |
| `self` | Instance in method | `fn f(self: Self)` |

## Module Keywords

| Keyword | Description | Example |
|---------|-------------|---------|
| `import` | Import module | `import utils.{ add }` |
| `pub` | Public visibility | `pub fn api() { }` |
| `as` | Alias | `import math.{ add as sum }` |

## FFI Keywords

| Keyword | Description | Example |
|---------|-------------|---------|
| `extern` | External C declaration | `extern { fn puts(s: CStr) -> i32 }` |
| `unsafe` | Unsafe code block | `unsafe { c_function() }` |
| `out` | Out parameter modifier | `fn get_result(out ptr: CPtr[i32])` |

## Comptime Keywords

| Keyword | Description | Example |
|---------|-------------|---------|
| `comptime` | Compile-time parameter | `fn f[comptime N: i32]()` |

## Smart Pointer Keywords

| Keyword | Description | Example |
|---------|-------------|---------|
| `Rc` | Reference counted | `Rc.new(value)` |
| `Arc` | Atomic ref counted | `Arc.new(value)` |
| `Cell` | Interior mutability | `Cell.new(value)` |

## FFI Types

| Keyword | Description | Example |
|---------|-------------|---------|
| `CPtr` | Non-null C pointer | `CPtr[i32]` |
| `COptPtr` | Nullable C pointer | `COptPtr[i32]` |
| `CStr` | Borrowed C string | `msg.as_cstr()` |
| `CStrOwned` | Owned C string | `msg.to_cstr()` |

## Collection Types

| Keyword | Description | Example |
|---------|-------------|---------|
| `List` | Dynamic array | `List.new[i32]()` |
| `Map` | Hash map | `Map.new[K, V]()` |
| `Set` | Hash set | `Set.new[T]()` |
| `Range` | Range type | `0..10` |

## Result/Option Types

| Keyword | Description | Example |
|---------|-------------|---------|
| `Result` | Result type | `Result[T, E]` |
| `?` | Optional type | `?T` (same as `Option[T]`) |

## Reserved for Future Use

The following keywords are reserved and may be used in future versions:

| Keyword | Potential Use |
|---------|---------------|
| `async` | Asynchronous functions |
| `await` | Await asynchronous result |
| `yield` | Generator functions |
| `macro` | Macro definitions |
| `where` | Additional trait bounds |
| `dyn` | Dynamic dispatch |
| `move` | Move semantics |
| `mut` | Mutability (alternative to `var`) |
| `static` | Static variables |
| `union` | Union types |

## Contextual Keywords

Some identifiers have special meaning in specific contexts but can be used as regular identifiers elsewhere:

| Keyword | Context | Description |
|---------|---------|-------------|
| `_` | Pattern matching | Wildcard pattern |
| `self` | Methods | Instance reference |
| `Self` | Traits/impl | Current type |

## Identifier Rules

Valid identifiers in Klar:

- Must start with a letter or underscore
- Can contain letters, digits, and underscores
- Cannot be a keyword
- Case-sensitive

```klar
// Valid identifiers
let myVariable: i32 = 1
let _private: i32 = 2
let camelCase: i32 = 3
let snake_case: i32 = 4
let PascalCase: i32 = 5
let x1: i32 = 6

// Invalid identifiers
// let 1x: i32 = 1       // Cannot start with digit
// let let: i32 = 1      // Cannot use keyword
// let my-var: i32 = 1   // Cannot use hyphen
```

## Next Steps

- [Builtins](builtins.md) - Built-in functions
- [Basics](../language/basics.md) - Language basics
- [Operators](../advanced/operators.md) - All operators
