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
13. [Phase 4: Language Completion Plan](#phase-4-language-completion-plan)
14. [Phase 5: C Interoperability](#phase-5-c-interoperability)
15. [Phase 6: Bootstrap and Self-Hosting](#phase-6-bootstrap-and-self-hosting)

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

## Phase 5: C Interoperability

> **Goal:** Enable Klar programs to call C libraries like SDL3, OpenGL, SQLite, and system APIs.

### Executive Summary

Phase 5 adds Foreign Function Interface (FFI) capabilities to Klar, allowing seamless interoperability with C libraries. This unlocks access to the vast ecosystem of existing C code including graphics (SDL3, OpenGL, Vulkan), databases (SQLite, PostgreSQL), compression (zlib), cryptography (OpenSSL), and operating system APIs.

**Why C Interop?**
- Graphics/multimedia libraries are written in C (SDL3, OpenGL, Vulkan)
- System APIs are C-based (POSIX, Win32)
- Performance-critical libraries exist in C
- Gradual migration path from C codebases

**Design Philosophy:**
- Explicit is better than implicit - C calls are clearly marked
- Safety at the boundary - validate C data entering Klar
- Zero-overhead when possible - direct calls, no wrappers for simple cases
- Ergonomic bindings - high-level Klar APIs wrap low-level C

---

### Phase 5 Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                      KLAR PHASE 5: C INTEROP                            │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│   Klar Source (.kl)              C Headers (.h)                         │
│        │                              │                                 │
│        ▼                              ▼                                 │
│   ┌─────────────┐              ┌─────────────┐                          │
│   │   Parser    │              │  Bindgen    │  ← NEW: Generate Klar    │
│   │             │              │   Tool      │    bindings from headers │
│   └──────┬──────┘              └──────┬──────┘                          │
│          │                            │                                 │
│          │ AST with extern fns        │ Generated .kl bindings          │
│          │                            │                                 │
│          └──────────┬─────────────────┘                                 │
│                     ▼                                                   │
│              ┌─────────────┐                                            │
│              │   Checker   │  ← ENHANCED: Validate C type mappings      │
│              └──────┬──────┘                                            │
│                     │                                                   │
│                     ▼                                                   │
│              ┌─────────────┐                                            │
│              │   Codegen   │  ← ENHANCED: C calling conventions,        │
│              └──────┬──────┘    symbol linkage, ABI compatibility       │
│                     │                                                   │
│                     ▼                                                   │
│              ┌─────────────┐                                            │
│              │   Linker    │  ← ENHANCED: Link C libraries              │
│              └──────┬──────┘    (.a, .so, .dylib, .dll)                 │
│                     │                                                   │
│                     ▼                                                   │
│              Native Binary + C libraries                                │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

---

### Milestone 1: Extern Functions

**Objective:** Allow Klar to declare and call C functions.

**Deliverables:**
- [ ] `extern fn` declarations for C functions
- [ ] C calling convention support
- [ ] Symbol name specification with `@link_name`
- [ ] Variadic function support (`...`)
- [ ] Proper ABI handling (System V AMD64, Win64, ARM64)

**Extern Function Syntax:**
```klar
// Declare C functions
extern fn printf(format: *const u8, ...) -> i32
extern fn malloc(size: usize) -> *mut void
extern fn free(ptr: *mut void)

// With explicit link name (for name mangling)
@link_name("SDL_Init")
extern fn sdl_init(flags: u32) -> i32

// In a block for organization
extern "C" {
    fn puts(s: *const u8) -> i32
    fn getenv(name: *const u8) -> *const u8
    fn exit(status: i32) -> !
}
```

**Usage:**
```klar
import std.ffi.{ CString }

fn main() {
    let msg = CString.from("Hello from C!\n")
    unsafe {
        printf(msg.as_ptr())
    }
}
```

**Calling Convention:**
```klar
// Default is C calling convention
extern fn foo() -> i32

// Explicit conventions for platform APIs
@calling_convention("stdcall")
extern fn MessageBoxA(hwnd: *void, text: *const u8, caption: *const u8, type_: u32) -> i32
```

**Files to Modify:**
```
src/ast.zig              # Add ExternFn node
src/parser.zig           # Parse extern declarations
src/checker.zig          # Validate extern signatures
src/codegen/emit.zig     # Emit with C ABI
src/token.zig            # Add 'extern' keyword
```

**Success Criteria:**
- Can call `printf` from Klar
- Can call SDL_Init and other SDL functions
- Proper handling of different calling conventions

---

### Milestone 2: C-Compatible Types

**Objective:** Define Klar types that map directly to C types.

**Deliverables:**
- [ ] Raw pointer types: `*T`, `*mut T`, `*const T`
- [ ] Void pointer: `*void`, `*mut void`
- [ ] C integer types: `c_int`, `c_long`, `c_size_t`, etc.
- [ ] Null pointer constant: `null`
- [ ] Pointer arithmetic in unsafe blocks
- [ ] Array-to-pointer decay for C interop

**Pointer Types:**
```klar
// Immutable pointer (like const T* in C)
let ptr: *const i32 = &value

// Mutable pointer (like T* in C)
let ptr: *mut i32 = &mut value

// Void pointer (like void* in C)
let ptr: *void = raw_ptr
let ptr: *mut void = raw_mut_ptr

// Nullable pointer (optional pointer)
let maybe_ptr: ?*i32 = get_optional_ptr()

// Pointer to many (like T* array in C)
let arr_ptr: [*]i32 = get_array_ptr()
let arr_ptr: [*:0]u8 = get_null_terminated_string()  // Sentinel-terminated
```

**C Type Aliases:**
```klar
// std/ffi/ctypes.kl - Platform-specific C types
pub type c_char = i8       // or u8 on some platforms
pub type c_short = i16
pub type c_int = i32
pub type c_long = i64      // platform-dependent
pub type c_longlong = i64
pub type c_uchar = u8
pub type c_ushort = u16
pub type c_uint = u32
pub type c_ulong = u64
pub type c_ulonglong = u64
pub type c_size_t = usize
pub type c_ssize_t = isize
pub type c_float = f32
pub type c_double = f64
```

**Pointer Operations (unsafe):**
```klar
unsafe {
    // Dereference
    let value = *ptr
    *mut_ptr = 42

    // Pointer arithmetic
    let next = ptr.offset(1)
    let prev = ptr.offset(-1)

    // Cast between pointer types
    let void_ptr: *void = ptr.cast()
    let int_ptr: *i32 = void_ptr.cast()

    // Pointer to integer and back
    let addr: usize = ptr.to_int()
    let ptr2: *i32 = Ptr.from_int(addr)

    // Null check
    if ptr != null {
        use(*ptr)
    }
}
```

**Files to Create/Modify:**
```
src/types.zig            # Add raw pointer types
std/ffi/
├── mod.kl               # FFI module root
├── ctypes.kl            # C type aliases
├── ptr.kl               # Pointer utilities
└── cstring.kl           # C string handling
```

**Success Criteria:**
- Can represent any C pointer type
- Pointer arithmetic works correctly
- Platform-specific C types have correct sizes

---

### Milestone 3: C Structs and Unions

**Objective:** Define structs and unions with C-compatible memory layout.

**Deliverables:**
- [ ] `@repr(C)` attribute for C-compatible layout
- [ ] `@packed` attribute for packed structs
- [ ] Union types
- [ ] Opaque types for incomplete C types
- [ ] Bitfields (basic support)

**C-Compatible Structs:**
```klar
// C-compatible layout (fields in declaration order, C alignment)
@repr(C)
struct SDL_Rect {
    x: c_int
    y: c_int
    w: c_int
    h: c_int
}

// Packed struct (no padding)
@repr(C)
@packed
struct PackedData {
    flag: u8
    value: u32  // No padding before this
}

// Matches C struct exactly
@repr(C)
struct SDL_Event {
    type_: u32
    padding: [52]u8  // Union simulation for now
}
```

**Unions:**
```klar
@repr(C)
union Value {
    i: i64
    f: f64
    p: *void
}

// Access requires unsafe
let v = Value { i: 42 }
unsafe {
    let as_float = v.f  // Reinterpret bits
}
```

**Opaque Types:**
```klar
// For C types we don't need to know the layout of
@opaque
struct SDL_Window

@opaque
struct SDL_Renderer

// Used only through pointers
extern fn SDL_CreateWindow(
    title: *const u8,
    x: c_int, y: c_int,
    w: c_int, h: c_int,
    flags: u32
) -> ?*SDL_Window

extern fn SDL_DestroyWindow(window: *SDL_Window)
```

**Alignment Control:**
```klar
@repr(C)
@align(16)
struct AlignedData {
    data: [64]u8
}
```

**Files to Modify:**
```
src/ast.zig              # Add repr, packed, opaque attributes
src/parser.zig           # Parse attributes
src/checker.zig          # Compute C-compatible layouts
src/codegen/emit.zig     # Emit with correct alignment/padding
```

**Success Criteria:**
- SDL_Rect has same memory layout as C version
- Can pass structs to C functions
- Opaque types prevent invalid access

---

### Milestone 4: Library Linking

**Objective:** Link Klar programs with C libraries.

**Deliverables:**
- [ ] `@link` attribute to specify libraries
- [ ] Static library linking (`.a`, `.lib`)
- [ ] Dynamic library linking (`.so`, `.dylib`, `.dll`)
- [ ] System library paths
- [ ] pkg-config integration
- [ ] Build system support in `klar.toml`

**Link Attribute:**
```klar
// Link with SDL3
@link("SDL3")
extern fn SDL_Init(flags: u32) -> c_int

@link("SDL3")
extern fn SDL_Quit()

// Multiple libraries
@link("ssl")
@link("crypto")
extern fn SSL_library_init() -> c_int

// Framework on macOS
@link(framework: "Cocoa")
extern fn NSApplicationMain(argc: c_int, argv: *const *const u8) -> c_int
```

**Build Configuration:**
```toml
# klar.toml
[package]
name = "my-game"
version = "0.1.0"

[dependencies]
# Klar packages...

[native-dependencies]
# C library dependencies
SDL3 = { pkg-config = "sdl3" }
opengl = { system = true }

[native-dependencies.custom-lib]
include = ["vendor/include"]
lib-path = ["vendor/lib"]
lib = ["mylib"]

# Platform-specific
[native-dependencies.platform.linux]
lib = ["GL", "X11"]

[native-dependencies.platform.macos]
frameworks = ["OpenGL", "Cocoa"]

[native-dependencies.platform.windows]
lib = ["opengl32", "user32", "gdi32"]
```

**CLI Support:**
```bash
# Link with library
klar build --link SDL3

# Add library path
klar build --lib-path /usr/local/lib --link mylib

# Use pkg-config
klar build --pkg-config sdl3
```

**Files to Create/Modify:**
```
src/linker.zig           # NEW: Library linking logic
src/package/manifest.zig # Parse native-dependencies
src/build.zig            # Pass link flags to LLVM/system linker
```

**Success Criteria:**
- Can link with SDL3 and create a window
- pkg-config integration works
- Cross-platform library resolution

---

### Milestone 5: Binding Generator

**Objective:** Automatically generate Klar bindings from C headers.

**Deliverables:**
- [ ] `klar bindgen` command
- [ ] Parse C headers using libclang
- [ ] Generate `extern fn` declarations
- [ ] Generate `@repr(C)` struct definitions
- [ ] Generate type aliases
- [ ] Handle preprocessor macros (constants)
- [ ] Configurable naming conventions

**Bindgen Usage:**
```bash
# Generate bindings for SDL3
klar bindgen SDL3/SDL.h -o src/sdl3.kl

# With configuration
klar bindgen SDL3/SDL.h \
    --config bindgen.toml \
    --include /usr/include/SDL3 \
    -o src/sdl3.kl
```

**Bindgen Configuration:**
```toml
# bindgen.toml
[options]
# Rename SDL_ prefix
remove-prefix = "SDL_"

# Only include functions matching pattern
allowlist-function = "SDL_.*"
allowlist-type = "SDL_.*"

# Block certain items
blocklist-function = "SDL_main"

# Type mappings
[type-map]
"Uint32" = "u32"
"Sint32" = "i32"
```

**Generated Output Example:**
```klar
// Auto-generated by klar bindgen
// Source: SDL3/SDL.h

@link("SDL3")
module sdl3

// Constants
pub const INIT_VIDEO: u32 = 0x00000020
pub const INIT_AUDIO: u32 = 0x00000010

// Opaque types
@opaque
pub struct Window

@opaque
pub struct Renderer

// Structs
@repr(C)
pub struct Rect {
    pub x: c_int
    pub y: c_int
    pub w: c_int
    pub h: c_int
}

// Functions
pub extern fn Init(flags: u32) -> c_int
pub extern fn Quit()
pub extern fn CreateWindow(
    title: *const u8,
    x: c_int, y: c_int,
    w: c_int, h: c_int,
    flags: u32
) -> ?*Window
pub extern fn DestroyWindow(window: *Window)
```

**Files to Create:**
```
tools/bindgen/
├── main.zig             # CLI entry point
├── clang.zig            # libclang bindings
├── parser.zig           # Parse C declarations
├── generator.zig        # Generate Klar code
└── config.zig           # Configuration handling
```

**Success Criteria:**
- Can generate SDL3 bindings automatically
- Generated code compiles and works
- Handles complex headers (nested structs, function pointers)

---

### Milestone 6: Safe Wrappers

**Objective:** Provide idiomatic Klar wrappers around C APIs.

**Deliverables:**
- [ ] RAII wrappers for C resources
- [ ] Error handling integration
- [ ] Slice-to-pointer conversions
- [ ] String conversions (Klar string ↔ C string)
- [ ] Callback wrappers
- [ ] Standard wrapper patterns documentation

**Resource Management:**
```klar
// Safe wrapper around SDL_Window
pub struct Window {
    ptr: *sdl.Window
}

impl Window {
    pub fn create(title: &str, width: i32, height: i32) -> Result[Window, SdlError] {
        let c_title = CString.from(title)
        let ptr = unsafe {
            sdl.CreateWindow(
                c_title.as_ptr(),
                sdl.WINDOWPOS_CENTERED,
                sdl.WINDOWPOS_CENTERED,
                width, height,
                sdl.WINDOW_SHOWN
            )
        }
        match ptr {
            Some(p) => Ok(Window { ptr: p })
            None => Err(SdlError.from_last())
        }
    }
}

impl Window: Drop {
    fn drop(self: &mut Self) {
        unsafe { sdl.DestroyWindow(self.ptr) }
    }
}

// Usage - automatically cleaned up
fn main() -> Result[void, Error] {
    let window = Window.create("My Game", 800, 600)?
    // window.drop() called automatically at end of scope
}
```

**String Conversions:**
```klar
// std/ffi/cstring.kl
pub struct CString {
    data: List[u8]
}

impl CString {
    // Create null-terminated C string from Klar string
    pub fn from(s: &str) -> CString {
        let mut data = List.with_capacity(s.len() + 1)
        data.extend(s.bytes())
        data.push(0)  // Null terminator
        CString { data }
    }

    pub fn as_ptr(self) -> *const u8 {
        self.data.as_ptr()
    }
}

// Convert C string to Klar string (copies data)
pub fn from_c_str(ptr: *const u8) -> String {
    unsafe {
        let len = c_strlen(ptr)
        let slice = Slice.from_raw_parts(ptr, len)
        String.from_utf8(slice)
    }
}
```

**Callback Wrappers:**
```klar
// Wrap Klar closure as C function pointer
extern fn qsort(
    base: *mut void,
    num: usize,
    size: usize,
    compare: extern fn(*const void, *const void) -> c_int
)

fn sort_integers(arr: &mut [i32]) {
    // Create C-compatible comparison function
    extern fn compare(a: *const void, b: *const void) -> c_int {
        unsafe {
            let a = *(a as *const i32)
            let b = *(b as *const i32)
            if a < b { -1 } else if a > b { 1 } else { 0 }
        }
    }

    unsafe {
        qsort(
            arr.as_mut_ptr() as *mut void,
            arr.len(),
            @sizeOf(i32),
            compare
        )
    }
}
```

**Slice/Array Conversions:**
```klar
impl [T] {
    // Get raw pointer to slice data
    pub fn as_ptr(self) -> *const T {
        // Internal implementation
    }

    pub fn as_mut_ptr(self: &mut Self) -> *mut T {
        // Internal implementation
    }
}

// Create slice from C pointer and length
pub unsafe fn slice_from_raw[T](ptr: *const T, len: usize) -> &[T]
pub unsafe fn slice_from_raw_mut[T](ptr: *mut T, len: usize) -> &mut [T]
```

**Files to Create:**
```
std/ffi/
├── cstring.kl           # C string utilities
├── slice.kl             # Slice/pointer conversions
├── callback.kl          # Callback helpers
└── resource.kl          # RAII wrapper utilities

packages/sdl3/           # Example: SDL3 safe wrapper package
├── klar.toml
├── src/
│   ├── lib.kl
│   ├── window.kl
│   ├── renderer.kl
│   ├── event.kl
│   └── raw.kl           # Raw bindings (generated)
└── examples/
    └── hello_sdl.kl
```

**Success Criteria:**
- SDL3 wrapper provides safe, idiomatic API
- Resources are automatically cleaned up
- No memory leaks or dangling pointers in safe code

---

### SDL3 Example Program

**Goal:** Demonstrate complete SDL3 integration with Phase 5 features.

```klar
// hello_sdl.kl
module hello_sdl

import sdl3.{ Sdl, Window, Renderer, Event, Color }
import std.time.Duration

fn main() -> Result[void, Error] {
    // Initialize SDL (RAII - automatically quits on drop)
    let sdl = Sdl.init(.{ video: true })?

    // Create window (RAII - automatically destroyed on drop)
    let window = Window.create(
        "Hello SDL3 from Klar!",
        .{ width: 800, height: 600 }
    )?

    // Create renderer (RAII)
    let mut renderer = Renderer.create(&window)?

    // Main loop
    var running = true
    while running {
        // Process events
        for event in sdl.poll_events() {
            match event {
                Event.Quit => running = false
                Event.KeyDown { key: .Escape } => running = false
                _ => {}
            }
        }

        // Clear screen
        renderer.set_draw_color(Color.rgb(64, 64, 64))
        renderer.clear()

        // Draw a rectangle
        renderer.set_draw_color(Color.rgb(255, 0, 0))
        renderer.fill_rect(.{ x: 100, y: 100, w: 200, h: 150 })

        // Present
        renderer.present()

        // Cap frame rate
        std.time.sleep(Duration.from_millis(16))
    }

    Ok(())
    // sdl, window, renderer automatically cleaned up here
}
```

**Build Configuration:**
```toml
# klar.toml
[package]
name = "hello-sdl"
version = "0.1.0"

[dependencies]
sdl3 = "0.1"  # Klar SDL3 wrapper package

[native-dependencies]
SDL3 = { pkg-config = "sdl3" }
```

**Build and Run:**
```bash
klar build
klar run
```

---

### Phase 5 Dependency Graph

```
Milestone 1: Extern Functions
    │
    ├──► Milestone 2: C-Compatible Types
    │         │
    │         └──► Milestone 3: C Structs/Unions
    │                   │
    │                   └──► Milestone 6: Safe Wrappers
    │
    ├──► Milestone 4: Library Linking (parallel)
    │
    └──► Milestone 5: Binding Generator (needs 1-3)
```

**Recommended Order:**
1. Extern Functions (Milestone 1)
2. C-Compatible Types (Milestone 2)
3. Library Linking (Milestone 4) - can start in parallel
4. C Structs/Unions (Milestone 3)
5. Binding Generator (Milestone 5)
6. Safe Wrappers (Milestone 6)

---

### Phase 5 Timeline Estimate

| Milestone | Effort | Dependencies |
|-----------|--------|--------------|
| 1. Extern Functions | Medium | Phase 4 complete |
| 2. C-Compatible Types | Medium | Milestone 1 |
| 3. C Structs/Unions | Medium | Milestone 2 |
| 4. Library Linking | Medium | Milestone 1 |
| 5. Binding Generator | Large | Milestones 1-3 |
| 6. Safe Wrappers | Medium | Milestones 1-4 |

---

### Phase 5 Success Metrics

**Phase 5 is complete when:**

1. **C Interop Works**
   - [ ] Can call C functions with correct ABI
   - [ ] Pointers work correctly across boundary
   - [ ] Structs have C-compatible layout
   - [ ] Libraries link correctly on all platforms

2. **SDL3 Integration**
   - [ ] Can create SDL3 window from Klar
   - [ ] Can handle SDL3 events
   - [ ] Can render graphics with SDL3
   - [ ] Safe wrappers prevent common errors

3. **Developer Experience**
   - [ ] Bindgen generates usable bindings
   - [ ] Error messages help debug FFI issues
   - [ ] Documentation covers common patterns

4. **Example Programs**
   - [ ] SDL3 hello world (window + events)
   - [ ] SDL3 game loop with rendering
   - [ ] SQLite database access
   - [ ] System API usage (file dialogs, etc.)

---

### Phase 5 Stretch Goals

1. **C++ Interop** - Basic C++ class binding (vtables)
2. **Inline C** - Embed C code in Klar source
3. **Dynamic Loading** - `dlopen`/`LoadLibrary` support
4. **CFFI** - Call Klar from C (export functions)
5. **WebAssembly FFI** - JavaScript interop for WASM target

---

## Phase 6: Bootstrap and Self-Hosting

> **Goal:** Rewrite the Klar compiler in Klar itself, achieving self-hosting.

### Executive Summary

Phase 6 marks the maturation of Klar from a language implemented in Zig to a fully self-hosting language. The Klar compiler will be rewritten in Klar, proving the language is capable of implementing complex systems software. This is both a practical milestone (the compiler becomes a first-class Klar project) and a symbolic one (the language can sustain itself).

**Why Self-Host?**
- Proves the language is production-ready for systems programming
- Dogfooding surfaces language design issues
- Compiler becomes the largest Klar codebase, driving improvements
- Reduces dependency on Zig toolchain for development
- Attracts contributors who prefer writing Klar over Zig

**Prerequisites:**
- Phase 4 complete (generics, traits, modules, std library)
- Phase 5 complete (C interop for LLVM/Cranelift, system APIs)

**Design Philosophy:**
- Incremental migration - component by component, not big-bang rewrite
- Zig version remains authoritative until Klar version passes all tests
- Maintain feature parity - no regressions during transition
- Use bootstrap as stress test for language ergonomics

---

### Phase 6 Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    KLAR PHASE 6: BOOTSTRAP STRATEGY                     │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│   Stage 0: Zig Compiler (current)                                       │
│   ┌─────────────────────────────────────────────────────────────────┐   │
│   │  Lexer → Parser → Checker → VM/Codegen  (all Zig)               │   │
│   └─────────────────────────────────────────────────────────────────┘   │
│                              │                                          │
│                              ▼                                          │
│   Stage 1: Partial Bootstrap                                            │
│   ┌─────────────────────────────────────────────────────────────────┐   │
│   │  Lexer.kl → Parser.kl → Checker.kl → Codegen (Zig+LLVM)         │   │
│   │       ▲                                    │                    │   │
│   │       │         Compiled by Stage 0        │                    │   │
│   │       └────────────────────────────────────┘                    │   │
│   └─────────────────────────────────────────────────────────────────┘   │
│                              │                                          │
│                              ▼                                          │
│   Stage 2: Full Bootstrap                                               │
│   ┌─────────────────────────────────────────────────────────────────┐   │
│   │  Lexer.kl → Parser.kl → Checker.kl → Codegen.kl (LLVM via FFI)  │   │
│   │       ▲                                    │                    │   │
│   │       │         Compiled by Stage 1        │                    │   │
│   │       └────────────────────────────────────┘                    │   │
│   └─────────────────────────────────────────────────────────────────┘   │
│                              │                                          │
│                              ▼                                          │
│   Stage 3: Self-Sustaining                                              │
│   ┌─────────────────────────────────────────────────────────────────┐   │
│   │  klarc (Klar) compiles itself                                   │   │
│   │  Zig compiler archived as historical reference                  │   │
│   └─────────────────────────────────────────────────────────────────┘   │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

---

### Milestone 1: Compiler Infrastructure in Klar

**Objective:** Port foundational data structures and utilities to Klar.

**Deliverables:**
- [ ] String interning / symbol table
- [ ] Source location tracking
- [ ] Diagnostic/error reporting system
- [ ] Arena allocator for AST nodes
- [ ] File I/O for source reading
- [ ] Command-line argument parsing

**Key Files:**
```
src/klar/
├── intern.kl          # String interning
├── source.kl          # Source locations, spans
├── diagnostics.kl     # Error/warning reporting
├── arena.kl           # Arena allocator
└── cli.kl             # Argument parsing
```

**Example - String Interning:**
```klar
pub struct Interner {
    strings: Map[string, u32]
    by_id: List[string]
}

impl Interner {
    pub fn new() -> Interner {
        Interner {
            strings: Map.new(),
            by_id: List.new(),
        }
    }

    pub fn intern(self: &mut Self, s: string) -> Symbol {
        match self.strings.get(&s) {
            Some(id) => Symbol(id)
            None => {
                let id = self.by_id.len().as[u32]
                self.by_id.push(s.clone())
                self.strings.insert(s, id)
                Symbol(id)
            }
        }
    }

    pub fn resolve(self: &Self, sym: Symbol) -> &string {
        &self.by_id[sym.0.as[usize]]
    }
}

pub struct Symbol(u32)
```

**Success Criteria:**
- Infrastructure compiles with Stage 0 (Zig) compiler
- All unit tests pass
- Performance comparable to Zig implementation

---

### Milestone 2: Lexer in Klar

**Objective:** Rewrite the lexer in Klar.

**Deliverables:**
- [ ] Token definitions
- [ ] Lexer state machine
- [ ] String/number literal parsing
- [ ] Comment handling
- [ ] Error recovery and diagnostics

**Key Files:**
```
src/klar/
├── token.kl           # Token enum and spans
└── lexer.kl           # Lexer implementation
```

**Example - Token Definition:**
```klar
pub enum TokenKind {
    // Literals
    Integer(i64)
    Float(f64)
    String(string)
    Char(char)

    // Identifiers and keywords
    Identifier(Symbol)
    Keyword(Keyword)

    // Operators
    Plus
    Minus
    Star
    Slash
    // ... etc

    // Delimiters
    LParen
    RParen
    LBrace
    RBrace
    // ... etc

    // Special
    Newline
    Eof
    Error(string)
}

pub struct Token {
    pub kind: TokenKind
    pub span: Span
}

pub struct Lexer {
    source: string
    pos: usize
    line: u32
    col: u32
    interner: &mut Interner
}

impl Lexer {
    pub fn new(source: string, interner: &mut Interner) -> Lexer {
        Lexer {
            source: source,
            pos: 0,
            line: 1,
            col: 1,
            interner: interner,
        }
    }

    pub fn next_token(self: &mut Self) -> Token {
        self.skip_whitespace()
        // ... lexing logic
    }
}
```

**Validation:**
- Lex entire Klar standard library
- Compare token output with Zig lexer (should be identical)
- Benchmark: within 2x of Zig lexer performance

---

### Milestone 3: AST and Parser in Klar

**Objective:** Rewrite the parser and AST definitions in Klar.

**Deliverables:**
- [ ] Complete AST node definitions
- [ ] Recursive descent parser
- [ ] Operator precedence parsing (Pratt parser)
- [ ] Error recovery with synchronization
- [ ] Source location preservation

**Key Files:**
```
src/klar/
├── ast.kl             # AST node definitions
├── parser.kl          # Parser implementation
└── precedence.kl      # Operator precedence table
```

**Example - AST Nodes:**
```klar
pub enum Expr {
    Literal(LiteralExpr)
    Identifier(Symbol, Span)
    Binary(Rc[BinaryExpr])
    Unary(Rc[UnaryExpr])
    Call(Rc[CallExpr])
    If(Rc[IfExpr])
    Match(Rc[MatchExpr])
    Block(Rc[BlockExpr])
    // ... etc
}

pub struct BinaryExpr {
    pub left: Expr
    pub op: BinaryOp
    pub right: Expr
    pub span: Span
}

pub enum Stmt {
    Let(LetStmt)
    Var(VarStmt)
    Expr(Expr)
    Return(ReturnStmt)
    // ... etc
}

pub enum Decl {
    Function(FnDecl)
    Struct(StructDecl)
    Enum(EnumDecl)
    Impl(ImplDecl)
    // ... etc
}
```

**Validation:**
- Parse entire Klar standard library
- Round-trip test: parse → pretty-print → parse → compare AST
- Error messages match Zig parser quality

---

### Milestone 4: Type Checker in Klar

**Objective:** Rewrite the type checker and semantic analysis in Klar.

**Deliverables:**
- [ ] Type representation and equality
- [ ] Type inference engine
- [ ] Generic instantiation
- [ ] Trait resolution
- [ ] Borrow checking (if applicable)
- [ ] Semantic error reporting

**Key Files:**
```
src/klar/
├── types.kl           # Type representation
├── checker.kl         # Type checking logic
├── infer.kl           # Type inference
├── traits.kl          # Trait resolution
└── scope.kl           # Scope/symbol management
```

**Example - Type Representation:**
```klar
pub enum Type {
    // Primitives
    I8, I16, I32, I64, I128
    U8, U16, U32, U64, U128
    F32, F64
    Bool
    Char
    String
    Void
    Never

    // Compound
    Array(Rc[Type], usize)
    Slice(Rc[Type])
    Option(Rc[Type])
    Result(Rc[Type], Rc[Type])
    Tuple(List[Type])
    Function(Rc[FnType])
    Struct(StructId)
    Enum(EnumId)

    // References
    Ref(Rc[Type])
    RefMut(Rc[Type])

    // Generics
    TypeVar(TypeVarId)
    Generic(GenericId, List[Type])

    // Special
    Infer(InferId)
    Error
}
```

**Validation:**
- Type-check entire Klar standard library
- All existing test cases pass
- Error messages are clear and actionable

---

### Milestone 5: Code Generator in Klar

**Objective:** Rewrite native code generation in Klar using LLVM via FFI.

**Deliverables:**
- [ ] LLVM C API bindings (via Phase 5 FFI)
- [ ] IR generation from typed AST
- [ ] Function codegen with proper ABI
- [ ] Struct layout and access
- [ ] Control flow (if, match, loops)
- [ ] Optimization pipeline integration

**Key Files:**
```
src/klar/
├── codegen/
│   ├── llvm.kl        # LLVM FFI bindings
│   ├── emit.kl        # IR emission
│   ├── abi.kl         # Calling convention handling
│   └── layout.kl      # Type layout computation
```

**Example - LLVM Bindings:**
```klar
// Bindings generated via Phase 5 bindgen
extern "C" {
    fn LLVMContextCreate() -> *mut LLVMContext
    fn LLVMModuleCreateWithNameInContext(name: *const u8, ctx: *mut LLVMContext) -> *mut LLVMModule
    fn LLVMCreateBuilderInContext(ctx: *mut LLVMContext) -> *mut LLVMBuilder
    fn LLVMBuildAdd(builder: *mut LLVMBuilder, lhs: *mut LLVMValue, rhs: *mut LLVMValue, name: *const u8) -> *mut LLVMValue
    // ... etc
}

pub struct CodeGen {
    ctx: *mut LLVMContext
    module: *mut LLVMModule
    builder: *mut LLVMBuilder
    // ...
}

impl CodeGen {
    pub fn new(module_name: string) -> CodeGen {
        unsafe {
            let ctx = LLVMContextCreate()
            let name = CString.from(module_name)
            let module = LLVMModuleCreateWithNameInContext(name.as_ptr(), ctx)
            let builder = LLVMCreateBuilderInContext(ctx)
            CodeGen { ctx, module, builder }
        }
    }

    pub fn emit_expr(self: &mut Self, expr: &TypedExpr) -> *mut LLVMValue {
        match expr {
            TypedExpr.Binary(bin) => self.emit_binary(bin)
            TypedExpr.Call(call) => self.emit_call(call)
            // ...
        }
    }
}
```

**Validation:**
- Compile and run all native test programs
- Generated code matches Zig compiler output quality
- Benchmark within 10% of Zig compiler performance

---

### Milestone 6: Bootstrap Validation

**Objective:** Prove the Klar compiler can compile itself.

**Deliverables:**
- [ ] Stage 1 compiler (Klar compiler compiled by Zig compiler)
- [ ] Stage 2 compiler (Klar compiler compiled by Stage 1)
- [ ] Stage 3 compiler (Klar compiler compiled by Stage 2)
- [ ] Binary comparison: Stage 2 == Stage 3 (fixed point)
- [ ] Full test suite passes on all stages

**Bootstrap Process:**
```
# Build Stage 1: Zig compiles Klar compiler
zig build -Doptimize=ReleaseFast
./zig-out/bin/klarc src/klar/main.kl -o stage1

# Build Stage 2: Stage 1 compiles Klar compiler
./stage1 src/klar/main.kl -o stage2

# Build Stage 3: Stage 2 compiles Klar compiler
./stage2 src/klar/main.kl -o stage3

# Verify fixed point (Stage 2 and Stage 3 should be identical)
diff stage2 stage3 && echo "Bootstrap successful!"

# Run full test suite with Stage 2
./stage2 test
```

**Validation Criteria:**
- All three stages produce working compilers
- Stage 2 and Stage 3 binaries are byte-identical
- Full test suite passes on Stage 2 compiler
- Compilation time reasonable (< 2x Zig compiler)

---

### Milestone 7: Transition to Self-Hosting

**Objective:** Make the Klar compiler the primary/official implementation.

**Deliverables:**
- [ ] CI/CD builds using Klar compiler
- [ ] Distribution packages built with Klar compiler
- [ ] Documentation updated for self-hosted workflow
- [ ] Zig implementation archived
- [ ] Contributing guide for Klar-based development

**Repository Structure (Post-Bootstrap):**
```
klar/
├── src/                    # Klar compiler source (in Klar)
│   ├── main.kl
│   ├── lexer.kl
│   ├── parser.kl
│   ├── checker.kl
│   └── codegen/
├── std/                    # Standard library (in Klar)
├── bootstrap/              # Prebuilt binaries for bootstrapping
│   ├── klarc-linux-x64
│   ├── klarc-macos-x64
│   ├── klarc-macos-arm64
│   └── klarc-windows-x64.exe
├── archive/                # Historical Zig implementation
│   └── zig-compiler/
└── test/
```

**Bootstrap from Source:**
```bash
# For users without a Klar compiler
./bootstrap/klarc-$(uname -s | tr A-Z a-z)-$(uname -m) \
    src/main.kl -o klarc

# Verify
./klarc --version
```

---

### Phase 6 Dependency Graph

```
Milestone 1: Compiler Infrastructure
    │
    ├──► Milestone 2: Lexer (needs infrastructure)
    │        │
    │        ▼
    │    Milestone 3: Parser (needs lexer)
    │        │
    │        ▼
    │    Milestone 4: Type Checker (needs parser)
    │        │
    │        ▼
    │    Milestone 5: Code Generator (needs checker + Phase 5 FFI)
    │        │
    │        ▼
    └──► Milestone 6: Bootstrap Validation (needs all above)
             │
             ▼
         Milestone 7: Transition (needs validated bootstrap)
```

**Critical Path:**
1. Infrastructure (Milestone 1)
2. Lexer (Milestone 2)
3. Parser (Milestone 3)
4. Type Checker (Milestone 4)
5. Code Generator (Milestone 5)
6. Bootstrap Validation (Milestone 6)
7. Transition (Milestone 7)

---

### Phase 6 Timeline Estimate

| Milestone | Effort | Dependencies |
|-----------|--------|--------------|
| 1. Infrastructure | Medium | Phase 5 complete |
| 2. Lexer | Small | Milestone 1 |
| 3. Parser | Large | Milestone 2 |
| 4. Type Checker | Large | Milestone 3 |
| 5. Code Generator | Large | Milestone 4, Phase 5 |
| 6. Bootstrap Validation | Medium | Milestone 5 |
| 7. Transition | Small | Milestone 6 |

---

### Phase 6 Success Metrics

**Phase 6 is complete when:**

1. **Bootstrap Achieved**
   - [ ] Klar compiler compiles itself
   - [ ] Stage 2 == Stage 3 (fixed point reached)
   - [ ] All tests pass on self-compiled compiler

2. **Production Ready**
   - [ ] CI/CD uses self-hosted compiler
   - [ ] Release binaries built with Klar compiler
   - [ ] No regressions from Zig implementation

3. **Performance Acceptable**
   - [ ] Compilation speed within 2x of Zig implementation
   - [ ] Memory usage reasonable for large projects
   - [ ] Generated code quality unchanged

4. **Developer Experience**
   - [ ] Clear bootstrap instructions in README
   - [ ] Prebuilt binaries for major platforms
   - [ ] Contributing guide updated for Klar development

---

### Phase 6 Stretch Goals

1. **Incremental Compilation** - Only recompile changed modules
2. **Parallel Compilation** - Multi-threaded frontend and codegen
3. **Self-Hosted Standard Library** - Rewrite std in pure Klar where possible
4. **Debug Info** - DWARF/CodeView debug information generation
5. **Cross-Compilation** - Compile for other targets from Klar compiler
6. **Language Server** - LSP implementation in Klar

---

### Bootstrapping FAQ

**Q: Why not bootstrap earlier (before Phase 5)?**

A: The compiler needs C interop to use LLVM for native code generation. Without Phase 5, we'd either need to write a native backend from scratch or keep parts in Zig permanently.

**Q: What if we find language design issues during bootstrap?**

A: This is expected and valuable! Issues found will be fixed in both compilers (Zig and Klar) until the Klar version is authoritative.

**Q: How do new users build without a Klar compiler?**

A: Prebuilt bootstrap binaries are provided for major platforms. These are updated with each release.

**Q: Can we still use the Zig compiler after bootstrap?**

A: Yes, it's archived and can be used for debugging or if issues arise. However, the Klar compiler becomes the official implementation.

**Q: What about compile times?**

A: Initial Klar compiler may be slower than Zig version. Optimization is a stretch goal. The priority is correctness first.

---

*Document version: 2.2*
*Last updated: January 2026*
