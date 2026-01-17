# Klar Programming Language Specification

> **"No ambiguity. No surprises."**

Klar is an application language designed for clarity, safety, and simplicity. Its primary focus is game development and other performance-sensitive applications. It eliminates the pain points of C/C++ while remaining simpler than Rust.

Klar is not a systems language (like C, Rust, or Zig). It targets the same space as C# or Go — compiled, performant, but higher-level than bare metal. This influences design decisions like `.len()` returning `i32` rather than `usize` for ergonomic use with loop counters.

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

Functions always use explicit `return` statements. This follows the "explicit over implicit" principle—there is one obvious way to return a value.

```klar
fn add(a: i32, b: i32) -> i32 {
    return a + b
}

fn greet(name: string) {
    println("Hello, {name}!")
}

// Generic function
fn max[T: Ordered](a: T, b: T) -> T {
    if a > b {
        return a
    }
    return b
}

// Async function
async fn fetch(url: string) -> Result[Response, HttpError] {
    let response = http.get(url).await?
    return Ok(response)
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
    return Ok(a / b)
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
    return Ok(parsed)
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
        return ProcessError.Io(self)
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
    return Ok(parse(body))
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
    let results = scope {
        let tasks = items.map(|item| spawn process_item(item))
        await_all(tasks)
    }
    // All tasks guaranteed complete here
    return results
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
    if a.compare(b) is Ordering.Greater {
        return a
    }
    return b
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
        if self < other {
            return Ordering.Less
        } else if self > other {
            return Ordering.Greater
        }
        return Ordering.Equal
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
        return Vec2 { x: self.x + rhs.x, y: self.y + rhs.y }
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
    return Vec2 { x: a.x + b.x, y: a.y + b.y }
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

*See [ROADMAP.md](ROADMAP.md) for implementation phases and project roadmap.*
