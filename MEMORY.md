# Klar Language Reference

> **For LLM consumption.** This file contains everything needed to generate correct Klar code.
>
> **Philosophy:** "No ambiguity. No surprises."
> - Explicit types everywhere — no inference, no implicit conversions
> - Keywords over symbols — `and`, `or`, `not` instead of `&&`, `||`, `!`
> - Statement-based — no implicit returns, no expression-bodied functions
> - Ownership without complexity — no lifetime annotations

## Language Version & Status

**Phase 4 complete.** All core features implemented:

| Feature | Status |
|---------|--------|
| Generics & monomorphization | Done |
| Traits (bounds, inheritance, associated types) | Done |
| Modules & imports | Done |
| Standard library (List, Map, Set, String, Option, Result) | Done |
| Iterators | Done |
| Error handling (`?` operator) | Done |
| REPL | Done |
| Comptime | Done |
| FFI (including function pointers) | Done |
| Package manager | Done |
| Formatter | Done |
| Doc generator | Done |
| Ownership & borrow checking | Done |

**Three backends:** Native (LLVM, default), Bytecode VM, Tree-walking interpreter.

---

## Quick Reference: Types

### Primitive Types

| Type | Size | Default | Example |
|------|------|---------|---------|
| `i8` | 8-bit signed | `0` | `let x: i8 = -100` |
| `i16` | 16-bit signed | `0` | `let x: i16 = 30000` |
| `i32` | 32-bit signed | `0` | `let x: i32 = 42` |
| `i64` | 64-bit signed | `0` | `let x: i64 = 9000000000` |
| `u8` | 8-bit unsigned | `0` | `let x: u8 = 255` |
| `u16` | 16-bit unsigned | `0` | `let x: u16 = 65000` |
| `u32` | 32-bit unsigned | `0` | `let x: u32 = 4000000000` |
| `u64` | 64-bit unsigned | `0` | `let x: u64 = 18000000000` |
| `f32` | 32-bit float | `0.0` | `let x: f32 = 3.14` |
| `f64` | 64-bit float | `0.0` | `let x: f64 = 3.14159` |
| `bool` | boolean | `false` | `let x: bool = true` |
| `char` | Unicode scalar | `'\0'` | `let x: char = 'a'` |
| `string` | UTF-8 text | `""` | `let x: string = "hello"` |
| `void` | no value | — | `fn f() -> void { }` |

### Composite Types

| Type | Syntax | Example |
|------|--------|---------|
| Array (fixed) | `[T; N]` | `let a: [i32; 3] = [1, 2, 3]` |
| Tuple | `(T1, T2)` | `let t: (i32, string) = (1, "hi")` |
| Optional | `?T` | `let o: ?i32 = Some(42)` |
| Result | `Result[T, E]` | `let r: Result[i32, string] = Ok(42)` |
| Function | `fn(T) -> U` | `let f: fn(i32) -> i32 = \|x: i32\| -> i32 { return x }` |

### Collection Types

| Type | Constructor | Example |
|------|-------------|---------|
| `List[T]` | `List.new[T]()` | `var l: List[i32] = List.new[i32]()` |
| `Map[K, V]` | `Map.new[K, V]()` | `var m: Map[string, i32] = Map.new[string, i32]()` |
| `Set[T]` | `Set.new[T]()` | `var s: Set[i32] = Set.new[i32]()` |
| `Range[T]` | `start..end` | `let r: Range[i32] = 0..10` |

### Smart Pointer Types

| Type | Purpose | Constructor |
|------|---------|-------------|
| `Rc[T]` | Single-threaded shared ownership | `Rc.new(value)` |
| `Arc[T]` | Thread-safe shared ownership | `Arc.new(value)` |
| `Cell[T]` | Interior mutability | `Cell.new(value)` |

### I/O Types

| Type | Description |
|------|-------------|
| `File` | Open file handle |
| `Stdout` | Standard output handle |
| `Stderr` | Standard error handle |
| `Stdin` | Standard input handle |
| `IoError` | I/O error type |

### FFI Pointer Types

| Type | Description |
|------|-------------|
| `CPtr[T]` | Non-null C pointer |
| `COptPtr[T]` | Nullable C pointer |
| `CStr` | Borrowed C string (null-terminated) |
| `CStrOwned` | Owned C string (auto-freed on drop) |

---

## Quick Reference: Operators

### Arithmetic

| Operator | Description | Example |
|----------|-------------|---------|
| `+` | Addition | `a + b` |
| `-` | Subtraction | `a - b` |
| `*` | Multiplication | `a * b` |
| `/` | Division | `a / b` |
| `%` | Remainder | `a % b` |
| `-` (unary) | Negation | `-x` |

### Overflow-Safe Arithmetic

| Operator | Behavior | Example |
|----------|----------|---------|
| `+%` | Wrapping add | `max +% 1` wraps to min |
| `-%` | Wrapping sub | `0 -% 1` wraps to max |
| `*%` | Wrapping mul | wraps on overflow |
| `+\|` | Saturating add | `max +\| 1` stays at max |
| `-\|` | Saturating sub | `0 -\| 1` stays at 0 |
| `*\|` | Saturating mul | clamps at max |

### Comparison

| Operator | Description |
|----------|-------------|
| `==` | Equal |
| `!=` | Not equal |
| `<` | Less than |
| `<=` | Less than or equal |
| `>` | Greater than |
| `>=` | Greater than or equal |

### Logical (keywords, not symbols)

| Operator | Description | Short-circuits |
|----------|-------------|----------------|
| `and` | Logical AND | Yes |
| `or` | Logical OR | Yes |
| `not` | Logical NOT | — |

### Bitwise

| Operator | Description |
|----------|-------------|
| `&` | AND |
| `\|` | OR |
| `^` | XOR |
| `~` | NOT |
| `<<` | Left shift |
| `>>` | Right shift |

### Assignment

| Operator | Description |
|----------|-------------|
| `=` | Assign |
| `+=` | Add-assign |
| `-=` | Sub-assign |
| `*=` | Mul-assign |
| `/=` | Div-assign |
| `%=` | Rem-assign |

### Range

| Operator | Description | Example |
|----------|-------------|---------|
| `..` | Exclusive range | `0..5` → 0,1,2,3,4 |
| `..=` | Inclusive range | `0..=5` → 0,1,2,3,4,5 |

### Optional / Error

| Operator | Description | Example |
|----------|-------------|---------|
| `?` | Propagate error/None | `value?` |
| `??` | Null coalesce | `opt ?? 0` |
| `!` | Force unwrap (panics if None/Err) | `opt!` |

### Member Access

| Operator | Description | Example |
|----------|-------------|---------|
| `.` | Field/method | `point.x` |
| `::` | Enum variant | `Color::Red` |
| `[]` | Index | `arr[0]` |

### Type Conversion

| Operator | Description | Returns | Example |
|----------|-------------|---------|---------|
| `.as[T]` | Safe widening | `T` | `x.as[i64]` |
| `.to[T]` | Fallible conversion | `?T` | `"42".to[i32]` |
| `.trunc[T]` | Truncating | `T` | `big.trunc[i8]` |

### Operator Precedence (highest to lowest)

1. Unary: `-`, `not`, `~`
2. Multiplicative: `*`, `/`, `%`
3. Additive: `+`, `-`
4. Shifts: `<<`, `>>`
5. Bitwise AND: `&`
6. Bitwise XOR: `^`
7. Bitwise OR: `|`
8. Comparison: `<`, `<=`, `>`, `>=`
9. Equality: `==`, `!=`
10. Logical AND: `and`
11. Logical OR: `or`
12. Range: `..`, `..=`
13. Assignment: `=`, `+=`, etc.

---

## Quick Reference: Keywords

### Declarations

`let`, `var`, `fn`, `struct`, `enum`, `trait`, `impl`, `type`, `const`

### Control Flow

`if`, `else`, `match`, `for`, `while`, `loop`, `break`, `continue`, `return`, `in`

### Type Names

`i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f32`, `f64`, `bool`, `char`, `string`, `void`, `Self`

### Literals

`true`, `false`, `None`, `Some`, `Ok`, `Err`

### Logical

`and`, `or`, `not`

### References

`ref`, `inout`, `self`

### Modules

`import`, `pub`, `as`

### FFI

`extern`, `unsafe`, `out`

### Other

`comptime`, `async`, `await`, `Future`

- `async fn` declares an async function; must return `Future[T]`
- `await expr` suspends until `Future[T]` resolves, yields `T`; only valid inside `async fn`
- `Future[T]` is the return type wrapper for async functions
- Async trait/impl methods are not yet supported

Async runtime internals (state tags/layout conventions): `docs/design/async-runtime-internal.md`

### Reserved (future)

`yield`, `macro`, `where`, `dyn`, `move`, `mut`, `static`, `union`

---

## Canonical Patterns

One correct way to write each construct. Follow these exactly.

### Variables

```klar
// Immutable (preferred)
let x: i32 = 42

// Mutable
var counter: i32 = 0

// Compile-time constant
const MAX: i32 = 100

// Type alias
type Score = i32
```

### Functions

```klar
// Basic function
fn add(a: i32, b: i32) -> i32 {
    return a + b
}

// Void function (return type required — use -> void)
fn greet(name: string) -> void {
    println("Hello, {name}!")
}

// Generic function with trait bound
fn max[T: Ordered](a: T, b: T) -> T {
    if a > b {
        return a
    }
    return b
}

// Multiple trait bounds
fn process[T: Eq + Clone](item: T) -> T {
    return item.clone()
}

// Comptime parameter
fn make_array[comptime N: i32]() -> [i32; N] {
    return @repeat(0, N)
}

// Async function (must return Future[T], body returns T)
async fn fetch(url: string) -> Future[i32] {
    return 200
}

// Awaiting a future (only inside async fn, yields T from Future[T])
async fn process() -> Future[i32] {
    let status: i32 = await fetch("https://example.com")
    return status
}
```

### Closures

```klar
// Closure with explicit types (always required)
let double: fn(i32) -> i32 = |x: i32| -> i32 { return x * 2 }

// As function parameter
fn apply(f: fn(i32) -> i32, x: i32) -> i32 {
    return f(x)
}

// Capturing from enclosing scope
let factor: i32 = 5
let multiply: fn(i32) -> i32 = |x: i32| -> i32 { return x * factor }
```

### Entry Point

```klar
// Basic main
fn main() -> i32 {
    return 0
}

// With command-line arguments
fn main(args: [String]) -> i32 {
    println("Args: {args.len()}")
    return 0
}

// Void main (implicitly returns 0)
fn main() -> void {
    println("Hello!")
}
```

### Structs

```klar
// Definition
struct Point {
    x: i32,
    y: i32,
}

// Construction
let p: Point = Point { x: 10, y: 20 }

// Access
let x: i32 = p.x

// Generic struct
struct Box[T] {
    value: T,
}
```

### Methods

```klar
impl Point {
    // Takes ownership
    fn distance(self: Point) -> f64 {
        return sqrt(self.x.as[f64] * self.x.as[f64] + self.y.as[f64] * self.y.as[f64])
    }

    // Read-only borrow
    fn display(ref self: Point) -> void {
        println("({self.x}, {self.y})")
    }

    // Mutable borrow
    fn translate(inout self: Point, dx: i32, dy: i32) -> void {
        self.x += dx
        self.y += dy
    }

    // Associated function (no self)
    fn origin() -> Point {
        return Point { x: 0, y: 0 }
    }
}

// Calling methods
let p: Point = Point.origin()
p.display()                    // ref self: auto-borrow
var q: Point = Point { x: 1, y: 2 }
q.translate(inout q, 5, 5)    // inout requires explicit keyword at call site
```

### Enums

```klar
// Simple enum
enum Color { Red, Green, Blue }

// Enum with data
enum Message {
    Text(string),
    Number(i32),
    Empty,
}

// Generic enum (Option and Result are built-in)
enum Option[T] { Some(T), None }
enum Result[T, E] { Ok(T), Err(E) }

// Creation
let c: Color = Color::Red
let m: Message = Message::Text("hello")
let o: ?i32 = Some(42)
let r: Result[i32, string] = Ok(42)
```

### Pattern Matching

```klar
// Match on enum
match color {
    Color::Red => { println("red") }
    Color::Green => { println("green") }
    Color::Blue => { println("blue") }
}

// Match with extraction
match message {
    Message::Text(s) => { println(s) }
    Message::Number(n) => { println("{n}") }
    Message::Empty => { println("empty") }
}

// Match on optional
match opt {
    Some(value) => { println("{value}") }
    None => { println("nothing") }
}

// Match on result
match result {
    Ok(value) => { println("success: {value}") }
    Err(e) => { println("error: {e}") }
}

// Wildcard
match x {
    0 => { println("zero") }
    _ => { println("other") }
}
```

### Traits

```klar
// Definition
trait Drawable {
    fn draw(self: Self) -> void
}

// Implementation
impl Circle: Drawable {
    fn draw(self: Circle) -> void {
        println("Drawing circle")
    }
}

// Trait with default implementation
trait Printable {
    fn print(self: Self) -> void {
        println("default")
    }
}

// Trait inheritance
trait Shaped: Drawable {
    fn area(self: Self) -> f64
}

// Associated types
trait Container {
    type Item
    fn get(self: Self, index: i32) -> ?Self.Item
}
```

### Builtin Trait Implementations

```klar
// Eq - enables == and !=
impl Point: Eq {
    fn eq(self: Point, other: Point) -> bool {
        return self.x == other.x and self.y == other.y
    }
}

// Ordered - enables <, <=, >, >=
impl Score: Ordered {
    fn cmp(self: Score, other: Score) -> i32 {
        if self.value < other.value { return -1 }
        if self.value > other.value { return 1 }
        return 0
    }
}

// Clone
impl Point: Clone {
    fn clone(self: Point) -> Point {
        return Point { x: self.x, y: self.y }
    }
}

// Drop - cleanup on destruction
impl Connection: Drop {
    fn drop(inout self: Connection) -> void {
        // cleanup resources
    }
}

// Default
impl Point: Default {
    fn default() -> Point {
        return Point { x: 0, y: 0 }
    }
}
```

### Control Flow

```klar
// if/else (statement, not expression)
if x > 0 {
    println("positive")
} else if x == 0 {
    println("zero")
} else {
    println("negative")
}

// for loop with range
for i: i32 in 0..10 {
    println("{i}")
}

// for loop with inclusive range
for i: i32 in 0..=10 {
    println("{i}")
}

// for loop over collection
for item: i32 in list {
    println("{item}")
}

// for loop with tuple destructuring (maps)
for (key, value) in map {
    println("{key}: {value}")
}

// while loop
while count > 0 {
    count -= 1
}

// infinite loop
loop {
    if done {
        break
    }
}

// break and continue
for i: i32 in 0..100 {
    if i % 2 == 0 {
        continue
    }
    if i > 50 {
        break
    }
    println("{i}")
}
```

### Error Handling

```klar
// Optional: value that might not exist
let opt: ?i32 = Some(42)
let empty: ?i32 = None

// Force unwrap (panics if None)
let val: i32 = opt!

// Null coalesce (default if None)
let val: i32 = opt ?? 0

// Propagate None in ?T-returning function
fn find_email(id: i32) -> ?string {
    let user: User = find_user(id)?  // returns None if not found
    return user.email
}

// Result: operation that might fail
let ok: Result[i32, string] = Ok(42)
let err: Result[i32, string] = Err("failed")

// Propagate error in Result-returning function
fn process() -> Result[i32, string] {
    let data: string = read_file("input.txt")?
    let num: i32 = parse_int(data)?
    return Ok(num * 2)
}

// Check status
if result.is_ok() { println("success") }
if result.is_err() { println("failed") }
if opt.is_some() { println("has value") }
if opt.is_none() { println("empty") }
```

### Modules

```klar
// Selective import
import utils.{ greet, add }

// Glob import
import utils.*

// Aliased import
import math.{ add as sum }

// Nested module import
import lib/math.{ sqrt }

// Public declarations
pub fn api_function() -> i32 { return 42 }
pub struct Point { pub x: i32, pub y: i32 }
pub trait Drawable { fn draw(self: Self) }
```

### Collections

```klar
// List
var list: List[i32] = List.new[i32]()
list.push(1)
list.push(2)
let first: i32 = list[0]
let popped: i32 = list.pop()
let length: i32 = list.len()
list.drop()  // free memory

// Map
var map: Map[string, i32] = Map.new[string, i32]()
map.insert("key", 42)
let val: i32 = map.get("key")
let has: bool = map.contains_key("key")
map.remove("key")
map.drop()

// Set
var set: Set[i32] = Set.new[i32]()
set.insert(10)
let has: bool = set.contains(10)
set.remove(10)
set.drop()

// Iteration
for n: i32 in list { println("{n}") }
for (k, v) in map { println("{k}: {v}") }
for n: i32 in set { println("{n}") }
```

### Smart Pointers

```klar
// Rc - shared ownership (single-threaded)
let shared: Rc[i32] = Rc.new(42)
let copy: Rc[i32] = shared.clone()
let val: i32 = shared.get()
let refs: i32 = shared.ref_count()

// Arc - shared ownership (thread-safe)
let shared: Arc[i32] = Arc.new(42)
let copy: Arc[i32] = shared.clone()

// Cell - interior mutability
let cell: Cell[i32] = Cell.new(0)
cell.set(42)
let val: i32 = cell.get()
let old: i32 = cell.swap(100)

// Combined: shared mutable state
let state: Rc[Cell[i32]] = Rc.new(Cell.new(0))
let alias: Rc[Cell[i32]] = state.clone()
alias.get().set(42)
```

### References

```klar
// Read-only reference parameter
fn display(ref data: Data) -> void {
    println("{data.name}")
}

// Mutable reference parameter
fn increment(inout counter: i32) -> void {
    counter += 1
}

// Call site must be explicit
display(ref my_data)
increment(inout my_counter)

// Method self variants
impl MyType {
    fn consume(self: MyType) -> void { }       // takes ownership
    fn read(ref self: MyType) -> void { }      // borrows read-only
    fn mutate(inout self: MyType) -> void { }  // borrows mutably
}
```

### String Operations

```klar
// String interpolation
let msg: string = "Name: {name}, Age: {age}"

// Concatenation
let full: string = first + " " + last

// Methods
let len: i32 = s.len()
let upper: string = s.to_uppercase()
let lower: string = s.to_lowercase()
let trimmed: string = s.trim()
let has: bool = s.contains("sub")
let starts: bool = s.starts_with("pre")
let ends: bool = s.ends_with("suf")

// Escape sequences
"\n"  // newline
"\t"  // tab
"\\"  // backslash
"\""  // double quote
"\'"  // single quote
"\r"  // carriage return
"\0"  // null
```

### Integer Literals

```klar
let decimal: i32 = 42
let hex: i32 = 0xFF
let binary: i32 = 0b1010
let octal: i32 = 0o77
let underscore: i64 = 1_000_000
```

### Float Literals

```klar
let normal: f64 = 3.14
let scientific: f64 = 1.5e10
let neg_exp: f64 = 2.5e-3
```

### Comptime

```klar
// Comptime block
@{ let x: i32 = 10 }

// Comptime function
fn @factorial(n: i32) -> i32 {
    if n <= 1 { return 1 }
    return n * @factorial(n - 1)
}

// Comptime constant
const RESULT: i32 = @factorial(5)  // 120

// Comptime introspection
let name: string = @typeName[i32]           // "i32"
let has: bool = @hasField[Point]("x")       // true
let fields: [FieldInfo] = @fields[Point]    // field metadata
@assert(N > 0, "must be positive")          // compile-time assertion
@compileError("not supported")              // compile-time error

// Array repeat
let zeros: [i32; 10] = @repeat(0, 10)
```

### FFI

```klar
// External function declaration
extern {
    fn puts(s: CStr) -> i32
    fn malloc(size: u64) -> COptPtr[u8]
    fn free(ptr: CPtr[void]) -> void
}

// External type
extern type FILE
extern struct CPoint { x: i32, y: i32 }
extern enum CStatus: i32 { Ok = 0, Error = 1 }

// Calling C functions (requires unsafe)
unsafe {
    let result: i32 = puts("hello".as_cstr())
}

// String conversion for FFI
let cstr: CStr = my_string.as_cstr()          // borrow (no allocation)
let owned: CStrOwned = my_string.to_cstr()     // copy (allocates, auto-frees)
let back: string = cstr.to_string()            // copy to Klar string

// Pointer operations (all unsafe)
unsafe {
    let val: i32 = read(ptr)                   // dereference
    write(ptr, 42)                             // write through pointer
    let next: CPtr[i32] = offset(ptr, 1)      // pointer arithmetic
    let raw: CPtr[i32] = ref_to_ptr(ref x)    // reference to pointer
    let cast: CPtr[u8] = ptr_cast[u8](ptr)    // type cast
}

// Null check (safe, no unsafe needed)
if is_null(opt_ptr) { println("null") }

// Function pointers
let fp: extern fn(i32) -> i32 = @fn_ptr(my_function)

// Linking
// klar build file.kl -lm -lcurl -L/usr/local/lib
```

---

## Standard Library Quick Reference

### Built-in Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `print(s)` | `fn print(s: string)` | Print without newline |
| `println(s)` | `fn println(s: string)` | Print with newline |
| `readline()` | `fn readline() -> string` | Read line from stdin |
| `assert(c)` | `fn assert(c: bool)` | Panic if false |
| `assert_eq(a, b)` | `fn assert_eq[T: Eq](a: T, b: T)` | Panic if not equal |
| `dbg(v)` | `fn dbg[T](v: T) -> T` | Debug print, returns value |
| `panic(msg)` | `fn panic(msg: string) -> !` | Terminate with message |
| `len(c)` | `fn len[T](c: T) -> i32` | Collection/string length |
| `type_name(v)` | `fn type_name[T](v: T) -> string` | Type name as string |
| `sqrt(x)` | `fn sqrt(x: f64) -> f64` | Square root |
| `abs(x)` | `fn abs(x: T) -> T` | Absolute value |
| `min(a, b)` | `fn min[T: Ordered](a: T, b: T) -> T` | Minimum |
| `max(a, b)` | `fn max[T: Ordered](a: T, b: T) -> T` | Maximum |
| `drop(v)` | `fn drop[T](v: T)` | Explicitly free value |
| `Some(v)` | `Some(v: T) -> ?T` | Wrap in optional |
| `None` | `None -> ?T` | Empty optional |
| `Ok(v)` | `Ok(v: T) -> Result[T, E]` | Success result |
| `Err(e)` | `Err(e: E) -> Result[T, E]` | Error result |

### I/O Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `stdout()` | `fn stdout() -> Stdout` | Get stdout handle |
| `stderr()` | `fn stderr() -> Stderr` | Get stderr handle |
| `stdin()` | `fn stdin() -> Stdin` | Get stdin handle |

### Filesystem Functions

| Function | Returns | Description |
|----------|---------|-------------|
| `fs_exists(path)` | `bool` | Check if path exists |
| `fs_is_file(path)` | `bool` | Check if path is file |
| `fs_is_dir(path)` | `bool` | Check if path is directory |
| `fs_create_dir(path)` | `Result[void, IoError]` | Create directory |
| `fs_create_dir_all(path)` | `Result[void, IoError]` | Create directory tree |
| `fs_remove_file(path)` | `Result[void, IoError]` | Remove file |
| `fs_remove_dir(path)` | `Result[void, IoError]` | Remove directory |
| `fs_read_string(path)` | `Result[string, IoError]` | Read file contents |
| `fs_write_string(path, content)` | `Result[void, IoError]` | Write file contents |
| `fs_read_dir(path)` | `Result[List[string], IoError]` | List directory |

### File Type

| Method | Returns | Description |
|--------|---------|-------------|
| `File.open(path)` | `Result[File, IoError]` | Open for reading |
| `File.create(path)` | `Result[File, IoError]` | Create/truncate for writing |
| `File.append(path)` | `Result[File, IoError]` | Open for appending |
| `.read_to_string()` | `Result[string, IoError]` | Read entire file |
| `.read_bytes()` | `Result[bytes, IoError]` | Read as bytes |
| `.write(s)` | `Result[void, IoError]` | Write string |
| `.write_bytes(b)` | `Result[void, IoError]` | Write bytes |
| `.close()` | `void` | Close handle |

### FFI Functions (require `unsafe` except `is_null`)

| Function | Signature | Description |
|----------|-----------|-------------|
| `is_null(p)` | `fn is_null[T](p: COptPtr[T]) -> bool` | Check null (safe) |
| `unwrap_ptr(p)` | `fn unwrap_ptr[T](p: COptPtr[T]) -> CPtr[T]` | Nullable to non-null |
| `read(p)` | `fn read[T](p: CPtr[T]) -> T` | Read at pointer |
| `write(p, v)` | `fn write[T](p: CPtr[T], v: T)` | Write at pointer |
| `offset(p, n)` | `fn offset[T](p: CPtr[T], n: isize) -> CPtr[T]` | Pointer arithmetic |
| `ref_to_ptr(r)` | `fn ref_to_ptr[T](r: ref T) -> CPtr[T]` | Reference to pointer |
| `ptr_cast[U](p)` | `fn ptr_cast[U, T](p: CPtr[T]) -> CPtr[U]` | Cast pointer type |

### Comptime Builtins

| Function | Description |
|----------|-------------|
| `@typeName[T]` | Type name as string |
| `@typeInfo[T]` | Type metadata |
| `@hasField[T](name)` | Check struct has field |
| `@fields[T]` | Get struct field info |
| `@assert(cond, msg)` | Compile-time assertion |
| `@compileError(msg)` | Emit compile error |
| `@repeat(value, n)` | Array with repeated value |

### Integer Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.as[T]` | `T` | Safe widening |
| `.to_string()` | `string` | Convert to string |
| `.abs()` | same type | Absolute value |

### Float Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.abs()` | same type | Absolute value |
| `.floor()` | same type | Round down |
| `.ceil()` | same type | Round up |
| `.round()` | same type | Round nearest |
| `.to_string()` | `string` | Convert to string |

### Char Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.is_alphabetic()` | `bool` | Is letter |
| `.is_numeric()` | `bool` | Is digit |
| `.to_lowercase()` | `char` | Lowercase |
| `.to_uppercase()` | `char` | Uppercase |
| `.to_string()` | `string` | Convert to string |

### String Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.len()` | `i32` | Length |
| `.to_uppercase()` | `string` | Uppercase copy |
| `.to_lowercase()` | `string` | Lowercase copy |
| `.trim()` | `string` | Remove whitespace |
| `.contains(sub)` | `bool` | Substring check |
| `.starts_with(pre)` | `bool` | Prefix check |
| `.ends_with(suf)` | `bool` | Suffix check |
| `.as_cstr()` | `CStr` | Borrow as C string |
| `.to_cstr()` | `CStrOwned` | Copy as C string |

### Bool Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.to_string()` | `string` | `"true"` or `"false"` |

### Optional (?T) Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.is_some()` | `bool` | Has value |
| `.is_none()` | `bool` | No value |
| `.map(f)` | `?U` | Transform if Some |
| `.and_then(f)` | `?U` | Chain optionals |
| `.or(other)` | `?T` | Alternative |
| `.unwrap_or(default)` | `T` | Value or default |
| `.unwrap_or_else(f)` | `T` | Value or compute |

### Result[T, E] Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.is_ok()` | `bool` | Is success |
| `.is_err()` | `bool` | Is error |
| `.map(f)` | `Result[U, E]` | Transform Ok value |
| `.map_err(f)` | `Result[T, F]` | Transform Err value |
| `.and_then(f)` | `Result[U, E]` | Chain results |
| `.or_else(f)` | `Result[T, F]` | Alternative on error |
| `.unwrap_or(default)` | `T` | Value or default |
| `.unwrap_or_else(f)` | `T` | Value or compute |

### List[T] Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.push(value)` | `void` | Add to end |
| `.pop()` | `T` | Remove and return last |
| `.len()` | `i32` | Element count |
| `.is_empty()` | `bool` | Check if empty |
| `.get(index)` | `?T` | Safe access by index |
| `.clear()` | `void` | Remove all |
| `.drop()` | `void` | Free memory |

### Map[K, V] Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.insert(k, v)` | `void` | Add or update |
| `.get(k)` | `V` | Get value for key |
| `.contains_key(k)` | `bool` | Key exists check |
| `.remove(k)` | `void` | Remove key |
| `.len()` | `i32` | Entry count |
| `.is_empty()` | `bool` | Check if empty |
| `.clear()` | `void` | Remove all |
| `.keys()` | `List[K]` | All keys |
| `.values()` | `List[V]` | All values |
| `.drop()` | `void` | Free memory |

### Set[T] Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.insert(v)` | `void` | Add element |
| `.contains(v)` | `bool` | Element exists |
| `.remove(v)` | `void` | Remove element |
| `.len()` | `i32` | Element count |
| `.is_empty()` | `bool` | Check if empty |
| `.clear()` | `void` | Remove all |
| `.drop()` | `void` | Free memory |

### Rc[T] Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.get()` | `T` | Get inner value |
| `.clone()` | `Rc[T]` | Increment ref count |
| `.ref_count()` | `i32` | Current ref count |

### Arc[T] Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.get()` | `T` | Get inner value |
| `.clone()` | `Arc[T]` | Increment ref count |
| `.ref_count()` | `i32` | Current ref count |

### Cell[T] Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `.get()` | `T` | Get value |
| `.set(v)` | `void` | Set value |
| `.swap(v)` | `T` | Swap, return old |

### Range[T] Properties

| Property | Type | Description |
|----------|------|-------------|
| `.start` | `T` | Start value |
| `.end` | `T` | End value |
| `.inclusive` | `bool` | Is inclusive range |

### Builtin Traits

| Trait | Method | Description |
|-------|--------|-------------|
| `Eq` | `fn eq(self: Self, other: Self) -> bool` | Enables `==`, `!=` |
| `Ordered` | `fn cmp(self: Self, other: Self) -> i32` | Enables `<`, `>`, `<=`, `>=` |
| `Clone` | `fn clone(self: Self) -> Self` | Deep copy |
| `Drop` | `fn drop(inout self: Self)` | Cleanup on destruction |
| `Default` | `fn default() -> Self` | Default value |
| `Hash` | `fn hash(self: Self) -> u64` | Hash value |
| `Iterator` | `type Item` + `fn next(inout self: Self) -> ?Self.Item` | Iteration |
| `From[T]` | `fn from(value: T) -> Self` | Conversion from T |
| `Into[T]` | `fn into(self: Self) -> T` | Conversion to T |

---

## Type Conversion Cheat Sheet

### Safe Widening (.as[T])

Always succeeds, no data loss:

```
i8  → i16, i32, i64, f32, f64
i16 → i32, i64, f32, f64
i32 → i64, f64
i64 → (no safe widening to float)
u8  → u16, u32, u64, i16, i32, i64, f32, f64
u16 → u32, u64, i32, i64, f32, f64
u32 → u64, i64, f64
u64 → (no safe widening)
f32 → f64
```

```klar
let x: i32 = 42
let y: i64 = x.as[i64]      // always safe
let z: f64 = x.as[f64]      // always safe
```

### Fallible Conversion (.to[T])

Returns `?T`, None on failure:

```klar
let s: string = "42"
let n: ?i32 = s.to[i32]     // Some(42)

let bad: string = "abc"
let m: ?i32 = bad.to[i32]   // None
```

### Truncating Conversion (.trunc[T])

May lose data, always succeeds:

```klar
let big: i64 = 300
let small: i8 = big.trunc[i8]   // truncates to fit
```

### To String

All types support `.to_string()`:

```klar
let s1: string = 42.to_string()          // "42"
let s2: string = 3.14.to_string()        // "3.14"
let s3: string = true.to_string()        // "true"
let s4: string = 'a'.to_string()         // "a"
```

### Default Values

```klar
let x: i32 = i32.default()       // 0
let s: string = string.default()  // ""
let b: bool = bool.default()      // false
```

---

## Common Errors and Fixes

| Error | Cause | Fix |
|-------|-------|-----|
| Missing type annotation | `let x = 42` | `let x: i32 = 42` |
| Missing return statement | `fn f() -> i32 { 42 }` | `fn f() -> i32 { return 42 }` |
| Using `&&` or `\|\|` | Wrong logical operators | Use `and`, `or`, `not` |
| Using `!x` for negation | Wrong NOT operator | Use `not x` |
| Using `null` | No null in Klar | Use `None` with `?T` |
| Implicit conversion | `let y: i64 = x` where x is i32 | `let y: i64 = x.as[i64]` |
| Missing `inout` at call site | `mutate(x)` | `mutate(inout x)` |
| Missing `ref` at call site | `read(x)` for ref param | `read(ref x)` |
| Wrong enum construction | `Color.Red` | `Color::Red` |
| Using `var` for immutable | Habit from other languages | Use `let` when not mutated |
| Missing `return` in all branches | Some branches don't return | Add `return` to every path |
| Using `try`/`catch` | Not a Klar construct | Use `?` operator or `match` |
| Using `throw` | Not a Klar construct | Use `return Err(...)` |
| Forgetting `.drop()` on collections | Memory leak | Call `.drop()` when done |
| Using `unsafe` without `extern` | Not valid outside FFI | `unsafe` only for FFI calls |
| Using `match` as expression | Match is a statement | Assign in each branch |
| Using ternary `? :` | Not available | Use `if`/`else` with assignment |
| String comparison with `is` | Not a Klar keyword | Use `==` |
| Omitting braces | All blocks need `{ }` | Always use braces |
| Semicolons | Not used in Klar | Remove semicolons |

---

## Anti-Patterns: What NOT to Generate

### DO NOT use implicit returns

```klar
// WRONG
fn add(a: i32, b: i32) -> i32 { a + b }

// CORRECT
fn add(a: i32, b: i32) -> i32 { return a + b }
```

### DO NOT use symbol-based logical operators

```klar
// WRONG
if x > 0 && y > 0 { }
if !valid { }

// CORRECT
if x > 0 and y > 0 { }
if not valid { }
```

### DO NOT omit type annotations

```klar
// WRONG
let x = 42
var name = "Alice"

// CORRECT
let x: i32 = 42
var name: string = "Alice"
```

### DO NOT use null

```klar
// WRONG
let x: i32 = null

// CORRECT
let x: ?i32 = None
```

### DO NOT use implicit type conversions

```klar
// WRONG
let x: i32 = 42
let y: i64 = x

// CORRECT
let x: i32 = 42
let y: i64 = x.as[i64]
```

### DO NOT use try/catch/throw

```klar
// WRONG
try {
    let data = read_file("data.txt")
} catch (e) {
    println(e)
}

// CORRECT
let result: Result[string, string] = read_file("data.txt")
match result {
    Ok(data) => { println(data) }
    Err(e) => { println(e) }
}
```

### DO NOT use match as an expression

```klar
// WRONG
let x: i32 = match value {
    1 => 10,
    _ => 0,
}

// CORRECT
var x: i32
match value {
    1 => { x = 10 }
    _ => { x = 0 }
}
```

### DO NOT forget braces on control flow

```klar
// WRONG
if x > 0 return x

// CORRECT
if x > 0 {
    return x
}
```

### DO NOT use semicolons

```klar
// WRONG
let x: i32 = 42;
return x;

// CORRECT
let x: i32 = 42
return x
```

### DO NOT use `self` without type in methods

```klar
// WRONG
impl Point {
    fn x(self) -> i32 { return self.x }
}

// CORRECT
impl Point {
    fn x(self: Point) -> i32 { return self.x }
}
```

### DO NOT use `new` keyword for construction

```klar
// WRONG
let p: Point = new Point(1, 2)

// CORRECT
let p: Point = Point { x: 1, y: 2 }
```

### DO NOT use dot notation for enum variants

```klar
// WRONG
let c: Color = Color.Red

// CORRECT
let c: Color = Color::Red
```

### DO NOT use `class` or `interface`

```klar
// WRONG
class Person { ... }
interface Drawable { ... }

// CORRECT
struct Person { ... }
trait Drawable { ... }
```

### DO NOT use generic syntax with angle brackets

```klar
// WRONG
fn identity<T>(x: T) -> T { return x }
let list: List<i32> = List.new<i32>()

// CORRECT
fn identity[T](x: T) -> T { return x }
let list: List[i32] = List.new[i32]()
```

### DO NOT use `this` keyword

```klar
// WRONG
impl Point {
    fn x(this) -> i32 { return this.x }
}

// CORRECT
impl Point {
    fn x(self: Point) -> i32 { return self.x }
}
```

### DO NOT mutate without `inout`

```klar
// WRONG
fn double(n: i32) -> void { n = n * 2 }

// CORRECT
fn double(inout n: i32) -> void { n = n * 2 }
```

### DO NOT omit `Future[T]` on async functions

```klar
// WRONG — async fn must return Future[T]
async fn fetch() -> i32 { return 1 }

// CORRECT
async fn fetch() -> Future[i32] { return 1 }
```

### DO NOT return `Future[T]` from non-async functions

```klar
// WRONG — only async fn may return Future[T]
fn fetch() -> Future[i32] { return 1 }

// CORRECT
async fn fetch() -> Future[i32] { return 1 }
```

### DO NOT use `await` outside async functions

```klar
// WRONG — await only valid inside async fn
fn main() -> i32 {
    let x: i32 = await fetch()
    return x
}

// CORRECT
async fn process() -> Future[i32] {
    return await fetch()
}
```

---

## CLI Reference

| Command | Description |
|---------|-------------|
| `klar run <file.kl>` | Run program (native by default) |
| `klar run <file.kl> --vm` | Run with bytecode VM |
| `klar run <file.kl> --interpret` | Run with interpreter |
| `klar build <file.kl>` | Compile to native binary |
| `klar build <file.kl> -o name` | Custom output name |
| `klar build <file.kl> -O2` | Optimized build |
| `klar build <file.kl> -g` | Debug build |
| `klar build <file.kl> -lm` | Link with library |
| `klar build <file.kl> --emit-llvm` | Output LLVM IR |
| `klar check <file.kl>` | Type-check only |
| `klar repl` | Interactive REPL |
| `klar init [name]` | Create new project |
| `klar update` | Regenerate lock file |

### REPL Commands

| Command | Description |
|---------|-------------|
| `:help` | Show commands |
| `:type <expr>` | Show expression type |
| `:list` | Show all bindings |
| `:load <file>` | Load from file |
| `:reset` | Clear all bindings |
| `:quit` / `:q` | Exit |

---

## Ownership Rules Summary

1. Each value has exactly one owner
2. When owner goes out of scope, value is dropped
3. Assignment transfers ownership (move) for non-copy types
4. Copy types (`i32`, `f64`, `bool`, `char`) are copied on assignment
5. Use `.clone()` for explicit deep copy of non-copy types
6. `ref` borrows read-only (multiple allowed)
7. `inout` borrows mutably (only one at a time)
8. `ref` and `inout` cannot coexist for the same value
9. Collections need explicit `.drop()` to free memory
10. Smart pointers (`Rc`, `Arc`) enable shared ownership

---

## Complete Example

```klar
import utils.{ format_name }

struct Person {
    name: string,
    age: i32,
}

impl Person: Eq {
    fn eq(self: Person, other: Person) -> bool {
        return self.name == other.name and self.age == other.age
    }
}

impl Person {
    fn new(name: string, age: i32) -> Person {
        return Person { name: name, age: age }
    }

    fn greet(ref self: Person) -> string {
        return "Hi, I'm {self.name}, age {self.age}"
    }

    fn birthday(inout self: Person) -> void {
        self.age += 1
    }
}

fn find_oldest(people: List[Person]) -> ?Person {
    if people.is_empty() {
        return None
    }
    var oldest: Person = people[0]
    for i: i32 in 1..people.len() {
        if people[i].age > oldest.age {
            oldest = people[i]
        }
    }
    return oldest
}

fn main() -> i32 {
    var people: List[Person] = List.new[Person]()
    people.push(Person.new("Alice", 30))
    people.push(Person.new("Bob", 25))
    people.push(Person.new("Charlie", 35))

    let oldest: ?Person = find_oldest(people)
    var name: string
    match oldest {
        Some(p) => { name = p.name }
        None => { name = "nobody" }
    }

    println("Oldest: {name}")

    people.drop()
    return 0
}
```
