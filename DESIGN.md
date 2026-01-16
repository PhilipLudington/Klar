# Klar Programming Language Specification

> **"No ambiguity. No surprises."**

Klar is a systems programming language designed for clarity, safety, and simplicity. It eliminates the pain points of C/C++ while remaining simpler than Rust.

---

## Table of Contents

1. [Design Philosophy](#design-philosophy)
2. [Lexical Elements](#lexical-elements)
3. [Syntax and Grammar](#syntax-and-grammar)
4. [Type System](#type-system)
5. [Memory and Ownership](#memory-and-ownership)
6. [Error Handling](#error-handling)
7. [Concurrency](#concurrency)
8. [Generics and Traits](#generics-and-traits)
9. [Standard Library](#standard-library)
10. [Module and Package System](#module-and-package-system)
11. [Implementation Strategy](#implementation-strategy)
12. [Zig Implementation Code](#zig-implementation-code)

---

## Design Philosophy

### Core Principles

1. **Unambiguous syntax** — no context needed to parse
2. **No undefined behavior** — every operation has defined semantics
3. **Memory safe by default** — with opt-out for performance
4. **Modules, not headers** — single source of truth
5. **Explicit over implicit** — no silent conversions
6. **One obvious way** — minimize redundant features
7. **Build-aware** — no separate build system needed

### Problems Solved (vs C/C++)

| C/C++ Problem | Klar Solution |
|---------------|---------------|
| Ambiguous syntax | Keyword-driven, no context sensitivity |
| Preprocessor | `comptime` blocks |
| Undefined behavior | Defined semantics + operators (`+%`, `+|`) |
| Null danger | `?T` option types |
| Memory unsafety | Ownership + borrows, no dangling refs |
| Header/source split | Modules |
| Implicit conversions | Explicit `.as[]`, `.to[]` |
| Overloading complexity | No overloading, use generics |
| Build system | In-language or convention-based |

---

## Lexical Elements

### File Extension

`.kl`

### Keywords

```
// Declarations
fn       let      var      struct   enum     trait
impl     type     const    static   module   import
pub      async    unsafe   comptime

// Control flow
if       else     match    for      while    loop
return   break    continue

// Expressions
true     false    self     Self

// Operators (word-form)
and      or       not      is       in       as

// Special
where    dyn      mut
```

### Reserved (Future)

```
await    yield    macro    move     box      do
try      catch    throw    virtual  override
abstract final    sealed   partial
```

### Operators

```
// Arithmetic
+   -   *   /   %
+%  -%  *%          // wrapping
+|  -|  *|          // saturating

// Comparison
==  !=  <   >   <=  >=

// Logical
and  or  not        // word-form, not && || !

// Bitwise
&   |   ^   ~   <<  >>

// Assignment
=   +=  -=  *=  /=  %=

// Reference
&       &mut

// Special
?       !       ??      ..      ..=
->      =>      :       ::
```

### Literals

```klar
// Integers
42                  // i32 default
42_000              // underscores allowed
0xff                // hex
0b1010              // binary
0o755               // octal
42i64               // explicit type suffix
42u8

// Floats
3.14                // f64 default
3.14f32             // explicit type
1e10                // scientific
1.5e-3

// Strings
"hello"             // regular string
"line 1\nline 2"    // escape sequences
r"C:\path\to\file"  // raw string
r#"contains "quotes""#

// Multi-line strings
"""
    This is a
    multi-line string
    """

// Characters
'a'
'\n'
'\u{1F600}'         // unicode

// Boolean
true
false
```

### Comments

```klar
// Line comment

/* Block comment */

/// Documentation comment (for next item)
/// Supports **markdown**

//! Module-level documentation
```

---

## Syntax and Grammar

### Basic Syntax Rules

- **No semicolons** — newline-terminated
- **4-space indentation** (convention)
- **Trailing commas** allowed in multi-line constructs
- **Line length** — soft limit 100, hard limit 120

### Functions

```klar
fn add(a: i32, b: i32) -> i32 {
    return a + b
}

// Implicit return (last expression)
fn multiply(a: i32, b: i32) -> i32 {
    a * b
}

// Single-expression shorthand
fn double(x: i32) -> i32 = x * 2

// Async function
async fn fetch(url: string) -> Result[Response, HttpError] {
    http.get(url).await
}

// Generic function
fn max[T: Ordered](a: T, b: T) -> T {
    if a > b { a } else { b }
}
```

### Variables

```klar
let x: i32 = 5          // immutable
var y: i32 = 10         // mutable

let inferred = 42       // type inferred as i32
var list = List.new()   // type inferred
```

### Structs

```klar
struct Point {
    x: f64
    y: f64
}

pub struct User {
    pub name: String
    pub email: String
    password_hash: String  // private field
}

// Struct with traits
struct Counter: Clone + Eq {
    value: i32
}

// Instantiation
let p = Point { x: 1.0, y: 2.0 }
let p2 = Point { x: 3.0, ..p }  // spread
```

### Enums

```klar
enum Direction {
    North
    South
    East
    West
}

// With data
enum Option[T] {
    Some(T)
    None
}

enum Result[T, E] {
    Ok(T)
    Err(E)
}

enum Message {
    Quit
    Move { x: i32, y: i32 }
    Write(String)
    Color(i32, i32, i32)
}
```

### Control Flow

```klar
// If expression
let max = if a > b { a } else { b }

// Match expression
let name = match direction {
    Direction.North => "north"
    Direction.South => "south"
    Direction.East => "east"
    Direction.West => "west"
}

// Match with guards
let description = match value {
    0 => "zero"
    n if n < 0 => "negative"
    n if n > 100 => "large"
    n => "normal: {n}"
}

// For loop
for item in list {
    process(item)
}

for i in 0..10 {
    print(i)
}

for (index, item) in list.enumerate() {
    print("{index}: {item}")
}

// While loop
while condition {
    work()
}

// Infinite loop
loop {
    if done { break }
}

// Loop with value
let result = loop {
    if found {
        break value
    }
}
```

### Pattern Matching

```klar
// Destructuring
let Point { x, y } = point
let (first, second) = tuple

// In match
match value {
    Option.Some(x) => use(x)
    Option.None => default()
}

// Or patterns
match value {
    1 | 2 | 3 => "small"
    _ => "other"
}

// Nested patterns
match value {
    Message.Move { x: 0, y } => "vertical"
    Message.Move { x, y: 0 } => "horizontal"
    Message.Move { x, y } => "diagonal"
}
```

### Closures

```klar
// Basic form (types inferred)
let double = |x| x * 2
let add = |a, b| a + b

// With parameter types
let double = |x: i32| x * 2
let add = |a: i32, b: i32| a + b

// With return type
let parse = |s: string| -> i32 { s.parse()? }

// Multi-line body
let process = |x: i32| -> i32 {
    let y = x * 2
    y + 1
}

// Capturing environment
let factor = 10
let scale = |x| x * factor
```

### Operator Precedence (Highest to Lowest)

| Level | Operators | Associativity |
|-------|-----------|---------------|
| 1 | `.` `()` `[]` | Left |
| 2 | `?` `!` (postfix) | Left |
| 3 | `-` `not` `&` `&mut` `*` (prefix) | Right |
| 4 | `*` `/` `%` `*%` `*|` | Left |
| 5 | `+` `-` `+%` `-%` `+|` `-|` | Left |
| 6 | `<<` `>>` | Left |
| 7 | `&` (bitwise) | Left |
| 8 | `^` | Left |
| 9 | `|` (bitwise) | Left |
| 10 | `..` `..=` | Neither |
| 11 | `==` `!=` `<` `>` `<=` `>=` `is` | Neither |
| 12 | `and` | Left |
| 13 | `or` | Left |
| 14 | `??` | Right |
| 15 | `=` `+=` `-=` etc. | Right |

---

## Type System

### Primitive Types

```klar
// Integers
i8, i16, i32, i64, i128    // signed
u8, u16, u32, u64, u128    // unsigned
isize, usize               // pointer-sized

// Floats
f32, f64

// Other
bool
char                        // Unicode scalar value
string                      // UTF-8 string
```

### Composite Types

```klar
// Arrays (fixed size)
let arr: [i32; 5] = [1, 2, 3, 4, 5]

// Slices
let slice: [i32] = arr[1..4]

// Tuples
let tuple: (i32, string, bool) = (42, "hello", true)

// Option (built-in)
let maybe: ?i32 = Some(42)
let nothing: ?i32 = None

// Function types
let f: fn(i32, i32) -> i32 = add
```

### Type Aliases

```klar
type UserId = i64
type Callback = fn(Event) -> Result[void, Error]
type StringMap[V] = Map[String, V]
```

### No Implicit Conversions

```klar
let x: i32 = 5
let y: i64 = x          // ERROR

let y: i64 = x.as[i64]  // explicit widening (safe)
let z: i16 = x.to[i16]  // explicit narrowing (may trap)
let w: i16 = x.trunc[i16]  // truncate (never traps)
```

### Defined Arithmetic Behavior

```klar
let a: i32 = 2_000_000_000
let b = a + a              // trap (default)
let c = a +% a             // wrap around
let d = a +| a             // saturate to max
```

---

## Memory and Ownership

### Core Concept

Every value has exactly one owner. When the owner goes out of scope, the value is destroyed.

### Move Semantics

```klar
let a = Buffer.new(1024)
let b = a                    // a is MOVED to b
// a is no longer valid

let c = b.clone()            // explicit copy
// both b and c are valid
```

### Copy Types

```klar
// Primitives are Copy
let x: i32 = 5
let y = x    // x is copied, still valid

// Opt-in for structs
struct Point: Copy {
    x: f64
    y: f64
}
```

### Borrowing

```klar
fn print_length(buf: &Buffer) {    // immutable borrow
    print(buf.len)
}

fn modify(buf: &mut Buffer) {      // mutable borrow
    buf.write(data)
}

let buffer = Buffer.new(1024)
print_length(&buffer)              // lend immutably
modify(&mut buffer)                // lend mutably
```

### Borrow Rules

1. **One mutable OR many immutable** — never both
2. **References can't outlive owner**

### Key Simplification: No Stored References

```klar
// NOT ALLOWED
struct Parser {
    source: &string     // ERROR: no refs in structs
}

// INSTEAD: own the data
struct Parser {
    source: string
}

// OR: use Rc for sharing
struct Parser {
    source: Rc[string]
}
```

This eliminates lifetime annotations entirely.

### Reference Counting

```klar
// Single-threaded
let data = Rc.new(Buffer.new(1024))
let alias = data.clone()   // cheap increment

// Thread-safe
let shared = Arc.new(Config.load())

// Weak references (break cycles)
let weak = strong.downgrade()
```

### Interior Mutability

```klar
// Cell for simple values
let counter = Rc.new(Cell.new(0))
counter.set(counter.get() + 1)

// RefCell for complex values (runtime checks)
let buffer = Rc.new(RefCell.new(Buffer.new(1024)))
buffer.borrow_mut().write(data)
```

### Unsafe Escape Hatch

```klar
unsafe {
    let ptr = raw_allocate(1024)
    ptr.write(0, 42)
    raw_free(ptr)
}
```

---

## Error Handling

### Two Categories

1. **Traps** — bugs, non-recoverable (programmer error)
2. **Errors** — expected failures, recoverable (use Result)

### Traps (Bugs)

```klar
let x = arr[100]         // trap if out of bounds
let y = maybe!           // trap if None
let z = 1 / 0            // trap
```

Traps halt execution immediately with a diagnostic.

### Result Type

```klar
enum Result[T, E] {
    Ok(T)
    Err(E)
}

fn divide(a: i32, b: i32) -> Result[i32, MathError] {
    if b == 0 {
        return Err(MathError.DivideByZero)
    }
    Ok(a / b)
}
```

### Handling Results

```klar
// Pattern matching
let value = match result {
    Ok(v) => v
    Err(e) => {
        log("Error: {e}")
        return
    }
}

// Default on error
let value = result ?? 0

// Trap on error (for "impossible" errors)
let value = result!

// Propagate with ?
fn process() -> Result[Data, Error] {
    let content = read_file(path)?   // returns early on Err
    let parsed = parse(content)?
    Ok(parsed)
}
```

### Defining Errors

```klar
enum IoError {
    NotFound { path: string }
    PermissionDenied { path: string }
    Timeout { after_ms: i32 }
}

enum ProcessError {
    Io(IoError)
    Parse(ParseError)
    Validation(ValidationError)
}

// Conversion for ? propagation
impl IoError: Into[ProcessError] {
    fn into(self) -> ProcessError {
        ProcessError.Io(self)
    }
}
```

### Try Blocks

```klar
let result = try {
    let a = step_one()?
    let b = step_two(a)?
    step_three(b)?
}
```

---

## Concurrency

### Async/Await (IO-bound)

```klar
async fn fetch_data(url: string) -> Result[Data, HttpError] {
    let response = http.get(url).await?
    let body = response.read_body().await?
    Ok(parse(body))
}

async fn main() {
    let data = fetch_data("https://api.example.com").await?
    process(data)
}
```

### Spawning Tasks

```klar
// Fire and forget
spawn handle_connection(conn)

// Await result
let task = spawn fetch_data(url)
let result = task.await

// Concurrent operations
let (a, b, c) = await_all(
    fetch("/api/a"),
    fetch("/api/b"),
    fetch("/api/c")
)

// First to complete
let first = await_first(
    fetch("/primary"),
    fetch("/backup")
)
```

### Structured Concurrency

```klar
async fn process_batch(items: List[Item]) -> List[Result] {
    scope {
        let tasks = items.map(|item| spawn process_item(item))
        await_all(tasks)
    }
    // All tasks guaranteed complete here
}
```

### Threads (CPU-bound)

```klar
thread.scope(fn(s) {
    s.spawn(fn() { work_a() })
    s.spawn(fn() { work_b() })
    // Both complete before scope exits
})
```

### Channels

```klar
let (tx, rx) = channel[Message]()

spawn async {
    tx.send(Message.Data(payload)).await
}

spawn async {
    loop {
        match rx.recv().await {
            Message.Data(payload) => process(payload)
            Message.Done => break
        }
    }
}

// Select
loop {
    select {
        msg from inbox => handle_message(msg)
        tick from timer => handle_tick()
        _ from shutdown => break
    }
}
```

### Synchronization

```klar
// Mutex
let data = Mutex.new(HashMap.new())
{
    let mut guard = data.lock()
    guard.insert("key", value)
}

// RwLock
let cache = RwLock.new(HashMap.new())
{
    let view = cache.read()      // many readers
}
{
    let mut edit = cache.write() // exclusive writer
}

// Atomics
let counter = Atomic[i64].new(0)
counter.fetch_add(1)
```

### Send and Sync

```klar
Rc[T]      // NOT Send, NOT Sync
Arc[T]     // Send + Sync
Mutex[T]   // Send + Sync
RefCell[T] // NOT Sync
```

---

## Generics and Traits

### Generic Functions

```klar
fn max[T: Ordered](a: T, b: T) -> T {
    if a.compare(b) is Ordering.Greater { a } else { b }
}

fn swap[T](a: &mut T, b: &mut T) {
    let temp = *a
    *a = *b
    *b = temp
}
```

### Generic Types

```klar
struct Pair[A, B] {
    first: A
    second: B
}

enum Option[T] {
    Some(T)
    None
}
```

### Trait Definition

```klar
trait Ordered {
    fn compare(self, other: Self) -> Ordering
}

trait Printable {
    fn to_string(self) -> String

    // Default implementation
    fn print(self) {
        io.write(self.to_string())
    }
}
```

### Trait Implementation

```klar
impl i32: Ordered {
    fn compare(self, other: i32) -> Ordering {
        if self < other { Ordering.Less }
        else if self > other { Ordering.Greater }
        else { Ordering.Equal }
    }
}
```

### Trait Bounds

```klar
fn process[T: Ordered + Printable](value: T) {
    print(value)
}

fn merge[K, V](a: Map[K, V], b: Map[K, V]) -> Map[K, V]
where
    K: Hashable + Eq
    V: Clone
{
    ...
}
```

### Associated Types

```klar
trait Iterator {
    type Item
    fn next(self: &mut Self) -> Option[Self.Item]
}

impl List[T]: Iterator {
    type Item = T
    fn next(self: &mut Self) -> Option[T] { ... }
}
```

### Deriving Traits

```klar
#[derive(Eq, Clone, Hash, Printable)]
struct User {
    id: i64
    name: String
}
```

### Operator Overloading

```klar
trait Add[Rhs] {
    type Output
    fn add(self, rhs: Rhs) -> Self.Output
}

impl Vec2: Add[Vec2] {
    type Output = Vec2
    fn add(self, rhs: Vec2) -> Vec2 {
        Vec2 { x: self.x + rhs.x, y: self.y + rhs.y }
    }
}

let c = a + b    // calls a.add(b)
```

### Trait Objects

```klar
// Dynamic dispatch
fn draw_all(shapes: List[dyn Drawable]) {
    for shape in shapes {
        shape.draw()
    }
}
```

---

## Standard Library

### Module Overview

```
std
├── core          # Primitives, Option, Result (auto-imported)
├── collections   # List, Map, Set, Queue
├── string        # String, StringBuilder
├── io            # Files, streams, console
├── net           # TCP, UDP, HTTP
├── sync          # Mutex, channels, atomics
├── async         # Async runtime
├── time          # Duration, DateTime
├── math          # Extended math
├── rand          # Random numbers
├── fs            # Filesystem
├── path          # Path manipulation
├── env           # Environment, args
├── process       # Spawning processes
├── json          # JSON serialization
├── testing       # Test framework
└── fmt           # Formatting
```

### std.core (Auto-Imported)

```klar
// Types
bool, i8, i16, i32, i64, i128
u8, u16, u32, u64, u128
f32, f64
char, string
Option[T], Result[T, E]

// Traits
Copy, Clone, Eq, Ordered, Hash, Printable, Default
Into, From, Iterator

// Functions
print(msg: string)
println(msg: string)
assert(condition: bool)
assert_eq[T: Eq](a: T, b: T)
panic(msg: string) -> !
```

### std.collections

```klar
struct List[T] {
    fn new() -> List[T]
    fn push(self: &mut Self, item: T)
    fn pop(self: &mut Self) -> Option[T]
    fn get(self: &Self, index: usize) -> Option[&T]
    fn len(self: &Self) -> usize
    fn iter(self: &Self) -> ListIter[T]
    // ...
}

struct Map[K: Hash + Eq, V] {
    fn new() -> Map[K, V]
    fn insert(self: &mut Self, key: K, value: V) -> Option[V]
    fn get(self: &Self, key: &K) -> Option[&V]
    fn contains_key(self: &Self, key: &K) -> bool
    // ...
}

struct Set[T: Hash + Eq] {
    fn new() -> Set[T]
    fn insert(self: &mut Self, item: T) -> bool
    fn contains(self: &Self, item: &T) -> bool
    fn union(self: &Self, other: &Set[T]) -> Set[T]
    // ...
}
```

### std.string

```klar
struct String {
    fn new() -> String
    fn len(self: &Self) -> usize
    fn chars(self: &Self) -> CharIter
    fn contains(self: &Self, pattern: &str) -> bool
    fn split(self: &Self, sep: &str) -> List[String]
    fn trim(self: &Self) -> &str
    fn to_uppercase(self: &Self) -> String
    fn replace(self: &Self, from: &str, to: &str) -> String
    // ...
}

struct StringBuilder {
    fn new() -> StringBuilder
    fn append(self: &mut Self, s: &str) -> &mut Self
    fn build(self) -> String
}
```

### std.io

```klar
trait Read {
    fn read(self: &mut Self, buf: &mut [u8]) -> Result[usize, IoError]
    fn read_all(self: &mut Self) -> Result[List[u8], IoError]
    fn read_string(self: &mut Self) -> Result[String, IoError]
}

trait Write {
    fn write(self: &mut Self, buf: &[u8]) -> Result[usize, IoError]
    fn flush(self: &mut Self) -> Result[void, IoError]
}

fn stdin() -> Stdin
fn stdout() -> Stdout
fn stderr() -> Stderr
```

### std.fs

```klar
fn read(path: &Path) -> Result[List[u8], IoError]
fn read_string(path: &Path) -> Result[String, IoError]
fn write(path: &Path, data: &[u8]) -> Result[void, IoError]
fn write_string(path: &Path, s: &str) -> Result[void, IoError]
fn exists(path: &Path) -> bool
fn create_dir(path: &Path) -> Result[void, IoError]
fn remove(path: &Path) -> Result[void, IoError]
fn read_dir(path: &Path) -> Result[DirIter, IoError]
```

### std.net.http

```klar
async fn get(url: &str) -> Result[Response, HttpError]
async fn post(url: &str, body: &[u8]) -> Result[Response, HttpError]

struct Response {
    status: u16
    headers: Map[String, String]
    async fn text(self) -> Result[String, HttpError]
    async fn json[T: Deserialize](self) -> Result[T, HttpError]
}
```

### std.time

```klar
struct Duration {
    fn from_secs(secs: u64) -> Duration
    fn from_millis(ms: u64) -> Duration
    fn as_secs(self: &Self) -> u64
}

struct Instant {
    fn now() -> Instant
    fn elapsed(self: &Self) -> Duration
}

struct DateTime {
    fn now() -> DateTime
    fn format(self: &Self, format: &str) -> String
}

async fn sleep(duration: Duration)
```

### std.json

```klar
enum Json {
    Null
    Bool(bool)
    Number(f64)
    String(String)
    Array(List[Json])
    Object(Map[String, Json])
}

impl Json {
    fn parse(s: &str) -> Result[Json, JsonError]
    fn to_string(self: &Self) -> String
}

trait Serialize {
    fn to_json(self: &Self) -> Json
}

trait Deserialize {
    fn from_json(json: &Json) -> Result[Self, JsonError]
}

#[derive(Serialize, Deserialize)]
struct User {
    name: String
    age: i32
}
```

### std.testing

```klar
#[test]
fn test_addition() {
    assert_eq(2 + 2, 4)
}

#[test]
#[should_panic]
fn test_panic() {
    panic("expected")
}
```

---

## Module and Package System

### Module Basics

One file = one module.

```klar
// src/math/vector.kl
module math.vector

pub struct Vec2 {
    pub x: f64
    pub y: f64
}

pub fn add(a: Vec2, b: Vec2) -> Vec2 {
    Vec2 { x: a.x + b.x, y: a.y + b.y }
}

fn helper() { ... }  // private
```

### Imports

```klar
import std.collections
import std.collections.{ List, Map }
import std.collections.*
import std.collections.HashMap as Map
import .sibling_module
import ..parent.other
```

### Visibility

```klar
pub fn public_function() { }
fn private_function() { }
pub(package) fn internal_api() { }

pub struct User {
    pub name: String      // public field
    password: String      // private field
}
```

### Package Structure

```
my_project/
├── package.kl           # manifest
├── src/
│   ├── lib.kl           # library root
│   ├── main.kl          # binary entry
│   └── utils.kl
├── tests/
│   └── utils_test.kl
└── deps.lock
```

### Package Manifest

```klar
// package.kl

package {
    name: "myproject"
    version: "1.0.0"
    authors: ["Author <author@example.com>"]
    license: "MIT"
}

targets {
    lib {
        path: "src/lib.kl"
    }
    bin "myapp" {
        path: "src/main.kl"
    }
}

dependencies {
    "json": "2.1.0"
    "http": ">=1.0.0, <2.0.0"
    "utils": { git: "https://github.com/example/utils", tag: "v1.2.3" }
}

dev_dependencies {
    "mocking": "1.0.0"
}
```

### CLI Commands

```bash
klar new myproject       # create package
klar build               # build
klar run                 # run default binary
klar test                # run tests
klar fmt                 # format
klar check               # type check only
klar add json            # add dependency
klar publish             # publish to registry
```

### Conditional Compilation

```klar
const DEBUG = env.var("DEBUG") ?? false

fn log(msg: string) {
    if comptime DEBUG {
        print(msg)
    }
}
```

---

## Implementation Strategy

### Phased Approach

```
Phase 1: Tree-walking interpreter (validate design)
    ↓
Phase 2: Bytecode VM (practical performance)
    ↓
Phase 3: Native compiler (Cranelift or LLVM)
```

### Implementation Language: Zig

**Reasons:**
- Simple, fast, explicit
- Powerful comptime
- Great for systems work
- No hidden allocations
- Fast compilation

### Project Structure

```
klar/
├── build.zig
├── build.zig.zon
├── src/
│   ├── main.zig
│   ├── lexer.zig
│   ├── token.zig
│   ├── ast.zig
│   ├── parser.zig
│   ├── types.zig
│   ├── checker.zig
│   ├── interpreter.zig
│   ├── values.zig
│   ├── builtins.zig
│   └── errors.zig
├── std/
│   └── *.kl
└── test/
    └── *.zig
```

### Phase 1 Milestones

| Week | Task |
|------|------|
| 1 | Lexer, basic CLI |
| 2 | AST, expression parser |
| 3 | Statement/declaration parser |
| 4 | Type checker (basic) |
| 5 | Tree-walking interpreter |
| 6 | Core builtins, first programs |

---

## Zig Implementation Code

### build.zig

```zig
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "klar",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the Klar compiler");
    run_step.dependOn(&run_cmd.step);

    const unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}
```

### src/token.zig

```zig
pub const Token = struct {
    kind: Kind,
    loc: Loc,

    pub const Loc = struct {
        start: usize,
        end: usize,
        line: u32,
        column: u32,
    };

    pub const Kind = enum {
        // Literals
        int_literal,
        float_literal,
        string_literal,
        char_literal,
        true_,
        false_,

        // Identifier
        identifier,

        // Keywords
        fn_,
        let,
        var_,
        struct_,
        enum_,
        trait,
        impl,
        if_,
        else_,
        match,
        for_,
        while_,
        loop,
        return_,
        break_,
        continue_,
        pub_,
        mut,
        async_,
        await_,
        unsafe_,
        import,
        module,
        as,
        in,
        is,
        and_,
        or_,
        not,
        comptime_,
        where,
        dyn,
        type_,
        const_,
        static,

        // Operators
        plus,
        minus,
        star,
        slash,
        percent,
        plus_wrap,
        minus_wrap,
        star_wrap,
        plus_sat,
        minus_sat,
        star_sat,
        eq,
        eq_eq,
        not_eq,
        lt,
        gt,
        lt_eq,
        gt_eq,
        amp,
        pipe,
        caret,
        tilde,
        lt_lt,
        gt_gt,
        question,
        bang,
        question_question,
        dot,
        dot_dot,
        dot_dot_eq,
        arrow,
        fat_arrow,
        colon,
        colon_colon,

        // Delimiters
        l_paren,
        r_paren,
        l_bracket,
        r_bracket,
        l_brace,
        r_brace,
        comma,
        newline,

        // Special
        eof,
        invalid,

        pub fn lexeme(self: Kind) ?[]const u8 {
            return switch (self) {
                .fn_ => "fn",
                .let => "let",
                .var_ => "var",
                .struct_ => "struct",
                .enum_ => "enum",
                .trait => "trait",
                .impl => "impl",
                .if_ => "if",
                .else_ => "else",
                .match => "match",
                .for_ => "for",
                .while_ => "while",
                .loop => "loop",
                .return_ => "return",
                .break_ => "break",
                .continue_ => "continue",
                .pub_ => "pub",
                .mut => "mut",
                .async_ => "async",
                .await_ => "await",
                .unsafe_ => "unsafe",
                .import => "import",
                .module => "module",
                .as => "as",
                .in => "in",
                .is => "is",
                .and_ => "and",
                .or_ => "or",
                .not => "not",
                .true_ => "true",
                .false_ => "false",
                .comptime_ => "comptime",
                .where => "where",
                .dyn => "dyn",
                .type_ => "type",
                .const_ => "const",
                .static => "static",
                else => null,
            };
        }
    };
};
```

### src/lexer.zig

```zig
const std = @import("std");
const Token = @import("token.zig").Token;

pub const Lexer = struct {
    source: []const u8,
    pos: usize,
    line: u32,
    column: u32,
    start_pos: usize,
    start_line: u32,
    start_column: u32,

    pub fn init(source: []const u8) Lexer {
        return .{
            .source = source,
            .pos = 0,
            .line = 1,
            .column = 1,
            .start_pos = 0,
            .start_line = 1,
            .start_column = 1,
        };
    }

    pub fn next(self: *Lexer) Token {
        self.skipWhitespace();
        self.start_pos = self.pos;
        self.start_line = self.line;
        self.start_column = self.column;

        if (self.isAtEnd()) {
            return self.makeToken(.eof);
        }

        const c = self.advance();

        if (isAlpha(c)) return self.identifier();
        if (isDigit(c)) return self.number();

        return switch (c) {
            '(' => self.makeToken(.l_paren),
            ')' => self.makeToken(.r_paren),
            '[' => self.makeToken(.l_bracket),
            ']' => self.makeToken(.r_bracket),
            '{' => self.makeToken(.l_brace),
            '}' => self.makeToken(.r_brace),
            ',' => self.makeToken(.comma),
            '+' => self.operatorWith('%', .plus_wrap, '|', .plus_sat, .plus),
            '-' => self.handleMinus(),
            '*' => self.operatorWith('%', .star_wrap, '|', .star_sat, .star),
            '/' => self.makeToken(.slash),
            '%' => self.makeToken(.percent),
            '=' => self.matchChar('=', .eq_eq, '>', .fat_arrow, .eq),
            '!' => self.matchSingle('=', .not_eq, .bang),
            '<' => self.handleLt(),
            '>' => self.handleGt(),
            '&' => self.makeToken(.amp),
            '|' => self.makeToken(.pipe),
            '^' => self.makeToken(.caret),
            '~' => self.makeToken(.tilde),
            '?' => self.matchSingle('?', .question_question, .question),
            '.' => self.handleDot(),
            ':' => self.matchSingle(':', .colon_colon, .colon),
            '"' => self.string(),
            '\'' => self.char(),
            '\n' => self.handleNewline(),
            else => self.makeToken(.invalid),
        };
    }

    fn skipWhitespace(self: *Lexer) void {
        while (!self.isAtEnd()) {
            switch (self.peek()) {
                ' ', '\t', '\r' => _ = self.advance(),
                '/' => {
                    if (self.peekNext() == '/') {
                        while (!self.isAtEnd() and self.peek() != '\n') {
                            _ = self.advance();
                        }
                    } else if (self.peekNext() == '*') {
                        _ = self.advance();
                        _ = self.advance();
                        while (!self.isAtEnd()) {
                            if (self.peek() == '*' and self.peekNext() == '/') {
                                _ = self.advance();
                                _ = self.advance();
                                break;
                            }
                            if (self.peek() == '\n') {
                                self.line += 1;
                                self.column = 0;
                            }
                            _ = self.advance();
                        }
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    fn identifier(self: *Lexer) Token {
        while (!self.isAtEnd() and (isAlpha(self.peek()) or isDigit(self.peek()))) {
            _ = self.advance();
        }
        const text = self.source[self.start_pos..self.pos];
        const kind = keywords.get(text) orelse .identifier;
        return self.makeToken(kind);
    }

    fn number(self: *Lexer) Token {
        // Handle hex, binary, octal
        if (self.source[self.start_pos] == '0' and !self.isAtEnd()) {
            switch (self.peek()) {
                'x', 'X' => {
                    _ = self.advance();
                    while (!self.isAtEnd() and (isHexDigit(self.peek()) or self.peek() == '_')) {
                        _ = self.advance();
                    }
                    return self.makeToken(.int_literal);
                },
                'b', 'B' => {
                    _ = self.advance();
                    while (!self.isAtEnd() and (self.peek() == '0' or self.peek() == '1' or self.peek() == '_')) {
                        _ = self.advance();
                    }
                    return self.makeToken(.int_literal);
                },
                'o', 'O' => {
                    _ = self.advance();
                    while (!self.isAtEnd() and (self.peek() >= '0' and self.peek() <= '7' or self.peek() == '_')) {
                        _ = self.advance();
                    }
                    return self.makeToken(.int_literal);
                },
                else => {},
            }
        }

        while (!self.isAtEnd() and (isDigit(self.peek()) or self.peek() == '_')) {
            _ = self.advance();
        }

        var is_float = false;
        if (!self.isAtEnd() and self.peek() == '.' and isDigit(self.peekNext())) {
            is_float = true;
            _ = self.advance();
            while (!self.isAtEnd() and (isDigit(self.peek()) or self.peek() == '_')) {
                _ = self.advance();
            }
        }

        // Scientific notation
        if (!self.isAtEnd() and (self.peek() == 'e' or self.peek() == 'E')) {
            is_float = true;
            _ = self.advance();
            if (!self.isAtEnd() and (self.peek() == '+' or self.peek() == '-')) {
                _ = self.advance();
            }
            while (!self.isAtEnd() and isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        // Type suffix
        if (!self.isAtEnd() and isAlpha(self.peek())) {
            while (!self.isAtEnd() and (isAlpha(self.peek()) or isDigit(self.peek()))) {
                _ = self.advance();
            }
        }

        return self.makeToken(if (is_float) .float_literal else .int_literal);
    }

    fn string(self: *Lexer) Token {
        while (!self.isAtEnd() and self.peek() != '"') {
            if (self.peek() == '\n') {
                self.line += 1;
                self.column = 0;
            }
            if (self.peek() == '\\' and !self.isAtEnd()) {
                _ = self.advance();
            }
            _ = self.advance();
        }
        if (self.isAtEnd()) return self.makeToken(.invalid);
        _ = self.advance(); // closing "
        return self.makeToken(.string_literal);
    }

    fn char(self: *Lexer) Token {
        if (!self.isAtEnd() and self.peek() == '\\') {
            _ = self.advance();
        }
        if (!self.isAtEnd()) _ = self.advance();
        if (!self.isAtEnd() and self.peek() == '\'') {
            _ = self.advance();
            return self.makeToken(.char_literal);
        }
        return self.makeToken(.invalid);
    }

    fn handleMinus(self: *Lexer) Token {
        if (!self.isAtEnd() and self.peek() == '>') {
            _ = self.advance();
            return self.makeToken(.arrow);
        }
        return self.operatorWith('%', .minus_wrap, '|', .minus_sat, .minus);
    }

    fn handleLt(self: *Lexer) Token {
        if (!self.isAtEnd()) {
            if (self.peek() == '=') {
                _ = self.advance();
                return self.makeToken(.lt_eq);
            }
            if (self.peek() == '<') {
                _ = self.advance();
                return self.makeToken(.lt_lt);
            }
        }
        return self.makeToken(.lt);
    }

    fn handleGt(self: *Lexer) Token {
        if (!self.isAtEnd()) {
            if (self.peek() == '=') {
                _ = self.advance();
                return self.makeToken(.gt_eq);
            }
            if (self.peek() == '>') {
                _ = self.advance();
                return self.makeToken(.gt_gt);
            }
        }
        return self.makeToken(.gt);
    }

    fn handleDot(self: *Lexer) Token {
        if (!self.isAtEnd() and self.peek() == '.') {
            _ = self.advance();
            if (!self.isAtEnd() and self.peek() == '=') {
                _ = self.advance();
                return self.makeToken(.dot_dot_eq);
            }
            return self.makeToken(.dot_dot);
        }
        return self.makeToken(.dot);
    }

    fn handleNewline(self: *Lexer) Token {
        const tok = self.makeToken(.newline);
        self.line += 1;
        self.column = 1;
        return tok;
    }

    fn advance(self: *Lexer) u8 {
        const c = self.source[self.pos];
        self.pos += 1;
        self.column += 1;
        return c;
    }

    fn peek(self: *Lexer) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.pos];
    }

    fn peekNext(self: *Lexer) u8 {
        if (self.pos + 1 >= self.source.len) return 0;
        return self.source[self.pos + 1];
    }

    fn isAtEnd(self: *Lexer) bool {
        return self.pos >= self.source.len;
    }

    fn matchSingle(self: *Lexer, expected: u8, if_match: Token.Kind, otherwise: Token.Kind) Token {
        if (!self.isAtEnd() and self.peek() == expected) {
            _ = self.advance();
            return self.makeToken(if_match);
        }
        return self.makeToken(otherwise);
    }

    fn matchChar(self: *Lexer, c1: u8, k1: Token.Kind, c2: u8, k2: Token.Kind, default: Token.Kind) Token {
        if (!self.isAtEnd()) {
            if (self.peek() == c1) {
                _ = self.advance();
                return self.makeToken(k1);
            }
            if (self.peek() == c2) {
                _ = self.advance();
                return self.makeToken(k2);
            }
        }
        return self.makeToken(default);
    }

    fn operatorWith(self: *Lexer, c1: u8, k1: Token.Kind, c2: u8, k2: Token.Kind, default: Token.Kind) Token {
        if (!self.isAtEnd()) {
            if (self.peek() == c1) {
                _ = self.advance();
                return self.makeToken(k1);
            }
            if (self.peek() == c2) {
                _ = self.advance();
                return self.makeToken(k2);
            }
        }
        return self.makeToken(default);
    }

    fn makeToken(self: *Lexer, kind: Token.Kind) Token {
        return .{
            .kind = kind,
            .loc = .{
                .start = self.start_pos,
                .end = self.pos,
                .line = self.start_line,
                .column = self.start_column,
            },
        };
    }

    fn isAlpha(c: u8) bool {
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn isHexDigit(c: u8) bool {
        return isDigit(c) or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F');
    }

    const keywords = std.StaticStringMap(Token.Kind).initComptime(.{
        .{ "fn", .fn_ },
        .{ "let", .let },
        .{ "var", .var_ },
        .{ "struct", .struct_ },
        .{ "enum", .enum_ },
        .{ "trait", .trait },
        .{ "impl", .impl },
        .{ "if", .if_ },
        .{ "else", .else_ },
        .{ "match", .match },
        .{ "for", .for_ },
        .{ "while", .while_ },
        .{ "loop", .loop },
        .{ "return", .return_ },
        .{ "break", .break_ },
        .{ "continue", .continue_ },
        .{ "pub", .pub_ },
        .{ "mut", .mut },
        .{ "async", .async_ },
        .{ "await", .await_ },
        .{ "unsafe", .unsafe_ },
        .{ "import", .import },
        .{ "module", .module },
        .{ "as", .as },
        .{ "in", .in },
        .{ "is", .is },
        .{ "and", .and_ },
        .{ "or", .or_ },
        .{ "not", .not },
        .{ "true", .true_ },
        .{ "false", .false_ },
        .{ "comptime", .comptime_ },
        .{ "where", .where },
        .{ "dyn", .dyn },
        .{ "type", .type_ },
        .{ "const", .const_ },
        .{ "static", .static },
    });
};
```

### src/main.zig

```zig
const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        try printUsage();
        return;
    }

    const command = args[1];

    if (std.mem.eql(u8, command, "run")) {
        if (args.len < 3) {
            std.debug.print("Error: no input file\n", .{});
            return;
        }
        try runFile(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "build")) {
        std.debug.print("Build not yet implemented\n", .{});
    } else if (std.mem.eql(u8, command, "check")) {
        std.debug.print("Check not yet implemented\n", .{});
    } else if (std.mem.eql(u8, command, "test")) {
        std.debug.print("Test not yet implemented\n", .{});
    } else if (std.mem.eql(u8, command, "fmt")) {
        std.debug.print("Format not yet implemented\n", .{});
    } else if (std.mem.eql(u8, command, "help") or std.mem.eql(u8, command, "--help") or std.mem.eql(u8, command, "-h")) {
        try printUsage();
    } else if (std.mem.eql(u8, command, "version") or std.mem.eql(u8, command, "--version") or std.mem.eql(u8, command, "-v")) {
        try printVersion();
    } else {
        std.debug.print("Unknown command: {s}\n\n", .{command});
        try printUsage();
    }
}

fn runFile(allocator: std.mem.Allocator, path: []const u8) !void {
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        std.debug.print("Error opening file '{s}': {}\n", .{ path, err });
        return;
    };
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 10 * 1024 * 1024);
    defer allocator.free(source);

    std.debug.print("=== Lexer Output ===\n", .{});
    var lexer = Lexer.init(source);

    while (true) {
        const token = lexer.next();
        const text = source[token.loc.start..token.loc.end];
        std.debug.print("{d}:{d} {s}: \"{s}\"\n", .{
            token.loc.line,
            token.loc.column,
            @tagName(token.kind),
            text,
        });
        if (token.kind == .eof) break;
    }

    std.debug.print("\n=== TODO: Parse and Execute ===\n", .{});
}

fn printUsage() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.writeAll(
        \\
        \\  Klar - No ambiguity. No surprises.
        \\
        \\Usage: klar <command> [options]
        \\
        \\Commands:
        \\  run <file>     Run a Klar program
        \\  build          Build a Klar project
        \\  check          Type check without building
        \\  test           Run tests
        \\  fmt            Format source files
        \\  help           Show this help
        \\  version        Show version
        \\
        \\Examples:
        \\  klar run hello.kl
        \\  klar build --release
        \\  klar test
        \\
    );
}

fn printVersion() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.writeAll("Klar 0.1.0-dev\n");
}

test "lexer basics" {
    const source = "fn main() { let x = 42 }";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Kind.fn_, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.identifier, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.l_paren, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.r_paren, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.l_brace, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.let, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.identifier, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.eq, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.int_literal, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.r_brace, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.eof, lexer.next().kind);
}

test "lexer operators" {
    const source = "+% -| * / == != <= >= -> => ?? ..";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Kind.plus_wrap, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.minus_sat, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.star, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.slash, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.eq_eq, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.not_eq, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.lt_eq, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.gt_eq, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.arrow, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.fat_arrow, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.question_question, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.dot_dot, lexer.next().kind);
}

test "lexer strings" {
    const source =
        \\"hello world"
    ;
    var lexer = Lexer.init(source);

    const tok = lexer.next();
    try std.testing.expectEqual(Token.Kind.string_literal, tok.kind);
}
```

---

## Example Programs

### Hello World

```klar
// hello.kl
fn main() {
    println("Hello, Klar!")
}
```

### FizzBuzz

```klar
fn main() {
    for i in 1..=100 {
        let msg = match (i % 3, i % 5) {
            (0, 0) => "FizzBuzz"
            (0, _) => "Fizz"
            (_, 0) => "Buzz"
            _ => i.to_string()
        }
        println(msg)
    }
}
```

### Fibonacci

```klar
fn fib(n: i32) -> i32 {
    if n <= 1 {
        return n
    }
    fib(n - 1) + fib(n - 2)
}

fn main() {
    for i in 0..20 {
        println("fib({i}) = {fib(i)}")
    }
}
```

### HTTP Server

```klar
module example.server

import std.net.http.{ Server, Request, Response, Status }
import std.io

async fn handle(req: Request) -> Response {
    match req.path {
        "/" => Response.text("Hello, World!")
        "/health" => Response.json(Health { ok: true })
        _ => Response.status(Status.NotFound)
    }
}

pub async fn main() -> Result[void, Error] {
    let server = Server.bind("127.0.0.1:8080").await?
    println("Listening on http://127.0.0.1:8080")

    loop {
        let conn = server.accept().await?
        spawn handle(conn)
    }
}
```

---

## Implementation Status

### Completed Phases

**Phase 1: Tree-Walking Interpreter** ✅
- Lexer, parser, type checker
- Tree-walking interpreter
- Core builtins (print, assert)

**Phase 2: Bytecode VM** ✅
- Bytecode compiler
- Stack-based virtual machine
- Garbage collection

**Phase 3: Native Compiler** ✅
- LLVM-based code generation
- Ownership-based memory management (Rc/Arc)
- 252x speedup over VM for compute-bound code
- Cross-compilation support

---

## Phase 4: Language Completion Plan

> **Goal:** Complete the Klar language with generics, traits, modules, and standard library.

### Executive Summary

Phase 4 transforms Klar from a working compiler into a complete, usable programming language. This phase implements the remaining language features specified above: **generics**, **traits**, **module system**, and **standard library**. The focus is on making Klar practical for real-world use.

**Current State (Post Phase 3):**
- ✅ Full compilation pipeline (lexer → parser → checker → LLVM → native)
- ✅ Ownership-based memory management (Rc/Arc, automatic drop)
- ✅ Basic types, structs, enums, closures, optionals, results
- ⚠️ Parser supports generics/traits/modules but checker doesn't fully implement them
- ⚠️ No standard library beyond builtins (print, panic, assert)
- ⚠️ Single-file compilation only

**Phase 4 Adds:**
- Generic functions and types with monomorphization
- Trait definitions, implementations, and bounds
- Multi-file compilation with module system
- Standard library (collections, I/O, strings)
- Package manager and tooling

**Design Philosophy Alignment:**
- `comptime` blocks (replacing C preprocessor) - stretch goal for Phase 4
- No macros - design uses `comptime` for metaprogramming instead
- Word operators (`and`, `or`, `not`) - already implemented ✅
- Explicit casts (`.as[T]`, `.to[T]`) - already implemented ✅

---

### Phase 4 Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         KLAR PHASE 4 ADDITIONS                          │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│   Source Files (.kl)                                                    │
│        │                                                                │
│        ▼                                                                │
│   ┌─────────────┐                                                       │
│   │   Module    │  ← NEW: Resolve imports, build dependency graph       │
│   │  Resolver   │                                                       │
│   └──────┬──────┘                                                       │
│          │ module tree                                                  │
│          ▼                                                              │
│   ┌─────────────┐                                                       │
│   │   Parser    │  ← existing (already parses generics/traits)          │
│   └──────┬──────┘                                                       │
│          │ AST with generics                                            │
│          ▼                                                              │
│   ┌─────────────┐                                                       │
│   │   Checker   │  ← ENHANCED: Generic instantiation, trait resolution  │
│   └──────┬──────┘                                                       │
│          │ monomorphized AST                                            │
│          ▼                                                              │
│   ┌─────────────┐                                                       │
│   │   Codegen   │  ← existing (works on monomorphized code)             │
│   └──────┬──────┘                                                       │
│          │                                                              │
│          ▼                                                              │
│   Native Binary + std library                                           │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

---

### Milestone 1: Generic Type Checking

**Objective:** Implement full generic type checking with monomorphization.

**Current State:** Parser accepts `fn foo[T](x: T)` syntax, but checker treats type parameters as unknown types.

**Deliverables:**
- [ ] Track type parameters in checker scope
- [ ] Implement type parameter substitution
- [ ] Implement monomorphization (generate concrete types at call sites)
- [ ] Support generic structs: `struct Pair[A, B] { first: A, second: B }`
- [ ] Support generic enums: `enum Option[T] { Some(T), None }`
- [ ] Support generic functions: `fn swap[T](a: &mut T, b: &mut T)`
- [ ] Implement type inference at call sites
- [ ] Cache monomorphized instances to avoid duplication

**Type Inference Example:**
```klar
fn identity[T](x: T) -> T { x }

let a = identity(42)        // T inferred as i32
let b = identity("hello")   // T inferred as string
let c: f64 = identity(3.14) // T inferred as f64 from context
```

**Monomorphization Strategy:**
```
Source:
  fn swap[T](a: &mut T, b: &mut T) { ... }
  swap(&mut x, &mut y)  // x, y are i32
  swap(&mut p, &mut q)  // p, q are f64

Generated:
  fn swap_i32(a: &mut i32, b: &mut i32) { ... }
  fn swap_f64(a: &mut f64, b: &mut f64) { ... }
```

**Files to Modify:**
```
src/checker.zig          # Add generic type checking, monomorphization
src/types.zig            # Enhance TypeVar, AppliedType handling
src/codegen/emit.zig     # Emit monomorphized functions
```

**Success Criteria:**
- Generic identity function works with multiple types
- Generic Pair struct can hold different types
- No code bloat from unused instantiations

---

### Milestone 2: Trait System

**Objective:** Implement trait definitions, implementations, and bounds.

**Deliverables:**
- [ ] Trait definition parsing (already done) and checking
- [ ] Trait implementation (`impl Type: Trait { ... }`)
- [ ] Trait bounds on generics (`fn sort[T: Ordered](list: List[T])`)
- [ ] Multiple trait bounds (`T: Ordered + Clone`)
- [ ] Default method implementations
- [ ] Associated types (`type Item` in traits)
- [ ] `Self` type in trait methods
- [ ] Derive macro basics (`#[derive(Eq, Clone)]`)

**Core Traits to Implement:**
```klar
trait Eq {
    fn eq(self, other: Self) -> bool
}

trait Ordered: Eq {
    fn compare(self, other: Self) -> Ordering
}

trait Clone {
    fn clone(self) -> Self
}

trait Drop {
    fn drop(self: &mut Self)
}

trait Default {
    fn default() -> Self
}

trait Hash {
    fn hash(self) -> u64
}
```

**Trait Resolution:**
```klar
trait Printable {
    fn to_string(self) -> String
}

impl i32: Printable {
    fn to_string(self) -> String {
        // integer to string conversion
    }
}

fn print_value[T: Printable](value: T) {
    println(value.to_string())
}

print_value(42)  // Resolves to i32's Printable impl
```

**Files to Create/Modify:**
```
src/traits.zig           # NEW: Trait registry, resolution
src/checker.zig          # Add trait checking, impl validation
src/types.zig            # Add trait bounds to type system
```

**Success Criteria:**
- Can define and implement traits
- Trait bounds restrict generic parameters
- Method calls resolve to correct implementation

---

### Milestone 3: Module System

**Objective:** Implement multi-file compilation with imports and exports.

**Deliverables:**
- [ ] Module declaration (`module math.vector`)
- [ ] Import resolution (`import std.collections.List`)
- [ ] Selective imports (`import std.io.{ read, write }`)
- [ ] Glob imports (`import std.collections.*`)
- [ ] Relative imports (`import .sibling`, `import ..parent.child`)
- [ ] Visibility modifiers (`pub fn`, `pub struct`)
- [ ] Module dependency graph construction
- [ ] Cycle detection in imports
- [ ] Compile multiple files into single binary

**Module Resolution Rules:**
```
import std.collections.List
  → Look in: std/collections.kl or std/collections/mod.kl
  → Find: pub struct List[T] { ... }

import .utils
  → Look in: ./utils.kl (relative to current file)

import ..parent.helper
  → Look in: ../parent/helper.kl
```

**Project Structure Example:**
```
myproject/
├── klar.toml           # Package manifest
├── src/
│   ├── main.kl         # module main
│   ├── utils.kl        # module utils
│   └── models/
│       ├── mod.kl      # module models
│       └── user.kl     # module models.user
└── std/                # Standard library (symlinked or copied)
```

**Files to Create:**
```
src/module_resolver.zig  # NEW: Module discovery, import resolution
src/project.zig          # NEW: Project/package handling
```

**Success Criteria:**
- Can compile multi-file projects
- Imports resolve correctly
- Circular imports detected and reported

---

### Milestone 4: Standard Library - Core

**Objective:** Implement core standard library types.

**Deliverables:**
- [ ] `Option[T]` - replaces built-in `?T` with stdlib type
- [ ] `Result[T, E]` - replaces built-in with richer API
- [ ] `String` - owned, growable string type
- [ ] `List[T]` - dynamic array (like Vec in Rust)
- [ ] `Map[K, V]` - hash map
- [ ] `Set[T]` - hash set
- [ ] `Range` - iteration support

**String Type:**
```klar
// std/string.kl
pub struct String {
    data: Rc[List[u8]]
    len: usize
}

impl String {
    pub fn new() -> String
    pub fn from(s: &str) -> String
    pub fn len(self) -> usize
    pub fn push(self: &mut Self, c: char)
    pub fn concat(self, other: &String) -> String
    pub fn slice(self, start: usize, end: usize) -> &str
    pub fn chars(self) -> Iterator[char]
}

impl String: Eq + Clone + Hash + Printable
```

**List Type:**
```klar
// std/collections/list.kl
pub struct List[T] {
    data: *mut T
    len: usize
    capacity: usize
}

impl List[T] {
    pub fn new() -> List[T]
    pub fn with_capacity(cap: usize) -> List[T]
    pub fn push(self: &mut Self, value: T)
    pub fn pop(self: &mut Self) -> Option[T]
    pub fn get(self, index: usize) -> Option[&T]
    pub fn len(self) -> usize
    pub fn iter(self) -> Iterator[&T]
}

impl List[T]: Clone where T: Clone
impl List[T]: Drop  // Drops all elements
```

**Files to Create:**
```
std/
├── core/
│   ├── mod.kl          # Re-exports
│   ├── option.kl       # Option[T]
│   ├── result.kl       # Result[T, E]
│   └── ordering.kl     # Ordering enum
├── string.kl           # String type
├── collections/
│   ├── mod.kl
│   ├── list.kl         # List[T]
│   ├── map.kl          # Map[K, V]
│   └── set.kl          # Set[T]
└── prelude.kl          # Auto-imported types
```

**Success Criteria:**
- Can create and manipulate strings
- Can use List for dynamic collections
- Map and Set work with hashable keys

---

### Milestone 5: Standard Library - I/O

**Objective:** Implement file and console I/O.

**Deliverables:**
- [ ] `File` type with read/write
- [ ] `stdin`, `stdout`, `stderr` handles
- [ ] `Read` and `Write` traits
- [ ] `BufReader` and `BufWriter`
- [ ] Path manipulation
- [ ] Directory operations

**I/O Traits:**
```klar
// std/io/traits.kl
pub trait Read {
    fn read(self: &mut Self, buf: &mut [u8]) -> Result[usize, IoError]
}

pub trait Write {
    fn write(self: &mut Self, buf: &[u8]) -> Result[usize, IoError]
    fn flush(self: &mut Self) -> Result[void, IoError]
}
```

**File API:**
```klar
// std/fs.kl
pub struct File { ... }

impl File {
    pub fn open(path: &Path) -> Result[File, IoError]
    pub fn create(path: &Path) -> Result[File, IoError]
}

impl File: Read + Write

// Usage
let file = File.open("data.txt")?
let contents = file.read_to_string()?
```

**Files to Create:**
```
std/
├── io/
│   ├── mod.kl
│   ├── traits.kl       # Read, Write
│   ├── stdio.kl        # stdin, stdout, stderr
│   └── buffer.kl       # BufReader, BufWriter
├── fs.kl               # File, directory operations
└── path.kl             # Path type
```

**Success Criteria:**
- Can read and write files
- Buffered I/O works correctly
- Proper error handling with Result

---

### Milestone 6: Iterator Protocol

**Objective:** Implement iterators and for-loop integration.

**Deliverables:**
- [ ] `Iterator` trait with `next()` method
- [ ] `IntoIterator` trait for for-loop support
- [ ] Iterator adapters: `map`, `filter`, `take`, `skip`
- [ ] `collect()` to gather into collections
- [ ] Range iterators (`0..10`, `0..=10`)

**Iterator Trait:**
```klar
pub trait Iterator {
    type Item
    fn next(self: &mut Self) -> Option[Self.Item]

    // Default implementations
    fn map[B](self, f: fn(Self.Item) -> B) -> Map[Self, B]
    fn filter(self, pred: fn(&Self.Item) -> bool) -> Filter[Self]
    fn collect[C: FromIterator[Self.Item]](self) -> C
}

pub trait IntoIterator {
    type Item
    type IntoIter: Iterator[Item = Self.Item]
    fn into_iter(self) -> Self.IntoIter
}
```

**For Loop Desugaring:**
```klar
// Source
for x in collection {
    process(x)
}

// Desugared to
let mut iter = collection.into_iter()
while let Some(x) = iter.next() {
    process(x)
}
```

**Success Criteria:**
- For loops work with any IntoIterator
- Iterator chains are lazy
- Can collect into List, Set, etc.

---

### Milestone 7: Error Handling Improvements

**Objective:** Complete the `?` operator and improve error handling.

**Deliverables:**
- [ ] Full `?` operator implementation (early return on Err/None)
- [ ] `try` blocks for localized error handling
- [ ] Error conversion with `From` trait
- [ ] `anyhow`-style error boxing
- [ ] Stack traces in debug mode

**Error Propagation:**
```klar
fn read_config() -> Result[Config, Error] {
    let file = File.open("config.toml")?  // Returns Err if fails
    let contents = file.read_to_string()?
    let config = parse_toml(contents)?
    Ok(config)
}
```

**Error Conversion:**
```klar
trait From[T] {
    fn from(value: T) -> Self
}

// Automatic conversion via ?
impl Error: From[IoError] { ... }
impl Error: From[ParseError] { ... }

fn example() -> Result[void, Error] {
    let file = File.open("x")?  // IoError → Error via From
    Ok(())
}
```

**Success Criteria:**
- `?` operator properly propagates errors
- Error types can be converted automatically
- Clear error messages with context

---

### Milestone 8: Package Manager

**Objective:** Implement basic package management.

**Deliverables:**
- [ ] `klar.toml` manifest format
- [ ] `klar init` - create new project
- [ ] `klar build` - build project (already exists, enhance)
- [ ] `klar run` - build and run
- [ ] `klar test` - run tests
- [ ] `klar add <package>` - add dependency
- [ ] Dependency resolution
- [ ] Package registry integration (optional)

**Manifest Format:**
```toml
# klar.toml
[package]
name = "myapp"
version = "0.1.0"
authors = ["Alice <alice@example.com>"]

[dependencies]
json = "1.0"
http = { git = "https://github.com/klar/http" }

[dev-dependencies]
test-utils = "0.1"
```

**CLI Commands:**
```bash
klar init myproject          # Create new project
klar build                   # Build current project
klar build --release         # Build with optimizations
klar run                     # Build and run
klar test                    # Run tests
klar add json                # Add dependency
klar update                  # Update dependencies
```

**Files to Create:**
```
src/package/
├── mod.zig                  # Package management entry
├── manifest.zig             # klar.toml parsing
├── resolver.zig             # Dependency resolution
└── registry.zig             # Package registry client
```

**Success Criteria:**
- Can create and build projects
- Dependencies resolved correctly
- Reproducible builds with lockfile

---

### Milestone 9: Tooling

**Objective:** Developer tooling for productive Klar development.

**Deliverables:**
- [ ] `klar fmt` - code formatter
- [ ] `klar check` - type check without compiling (already exists, enhance)
- [ ] `klar doc` - documentation generator
- [ ] Language Server Protocol (LSP) implementation
- [ ] Syntax highlighting definitions (VS Code, etc.)

**Language Server Features:**
- Go to definition
- Find references
- Hover for type info
- Autocomplete
- Diagnostics (errors/warnings)
- Code actions (quick fixes)

**Files to Create:**
```
src/lsp/
├── mod.zig                  # LSP entry point
├── server.zig               # JSON-RPC server
├── handlers.zig             # Request handlers
└── protocol.zig             # LSP protocol types

tools/
├── klar-fmt/                # Formatter
└── vscode-klar/             # VS Code extension
```

**Success Criteria:**
- Formatter produces consistent output
- LSP provides go-to-definition and autocomplete
- VS Code extension available

---

### Phase 4 Dependency Graph

```
Milestone 1: Generics
    │
    ├──► Milestone 2: Traits (needs generics for bounds)
    │         │
    │         ├──► Milestone 6: Iterators (needs Iterator trait)
    │         │
    │         └──► Milestone 7: Error Handling (needs From trait)
    │
    ├──► Milestone 3: Modules (parallel, foundation)
    │         │
    │         └──► Milestone 4: Stdlib Core (needs modules)
    │                   │
    │                   └──► Milestone 5: Stdlib I/O (needs core)
    │
    └──► Milestone 8: Package Manager (needs modules)
              │
              └──► Milestone 9: Tooling (needs stable language)
```

**Recommended Order:**
1. Generics (Milestone 1)
2. Modules (Milestone 3) - can start in parallel
3. Traits (Milestone 2)
4. Stdlib Core (Milestone 4)
5. Iterators (Milestone 6)
6. Error Handling (Milestone 7)
7. Stdlib I/O (Milestone 5)
8. Package Manager (Milestone 8)
9. Tooling (Milestone 9)

---

### Phase 4 Timeline Estimate

| Milestone | Effort | Dependencies |
|-----------|--------|--------------|
| 1. Generics | Large | None |
| 2. Traits | Large | Generics |
| 3. Modules | Medium | None |
| 4. Stdlib Core | Medium | Generics, Traits, Modules |
| 5. Stdlib I/O | Medium | Stdlib Core |
| 6. Iterators | Medium | Traits |
| 7. Error Handling | Small | Traits |
| 8. Package Manager | Medium | Modules |
| 9. Tooling | Large | All above |

---

### Phase 4 Success Metrics

**Phase 4 is complete when:**

1. **Language Completeness**
   - [ ] Generic functions and types work
   - [ ] Traits can be defined and implemented
   - [ ] Multi-file projects compile
   - [ ] Standard library provides core functionality

2. **Usability**
   - [ ] Can write non-trivial programs (HTTP server, CLI tools)
   - [ ] Error messages are helpful
   - [ ] Documentation exists

3. **Tooling**
   - [ ] Package manager works
   - [ ] IDE support via LSP
   - [ ] Code formatter available

4. **Example Programs**
   - [ ] JSON parser using generics
   - [ ] HTTP client using async (stretch)
   - [ ] File processing utility

---

### Stretch Goals

These are valuable but not required for Phase 4 completion:

1. **Async/Await** - Designed above but complex to implement
2. **Comptime** - Compile-time evaluation (replaces C preprocessor per design philosophy)
3. **Self-Hosting** - Compiler written in Klar
4. **REPL** - Interactive Klar shell
5. **WebAssembly Target** - Compile to WASM

> **Note:** The design philosophy specifically calls for `comptime` blocks instead of macros.
> `macro` is a reserved keyword for potential future use, but `comptime` is the intended
> mechanism for metaprogramming and compile-time code generation.

---

*Document version: 2.0*
*Last updated: January 2026*
