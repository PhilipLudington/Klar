# AI Agent's Guide to Klar

> A practical guide for AI agents generating, analyzing, and working with Klar code.

Klar is designed specifically for AI code generation. This guide explains the design decisions that make Klar AI-friendly and provides patterns for effective code generation.

## Why Klar for AI?

Traditional languages present challenges for AI code generation:

| Language | Challenge |
|----------|-----------|
| C/C++ | Ambiguous syntax, undefined behavior, preprocessor complexity |
| Python | Dynamic typing makes errors hard to catch statically |
| Rust | Complex lifetimes, borrow checker friction |
| JavaScript | Type coercion, `this` binding confusion |

Klar eliminates these problems through deliberate design.

## Core Principles

### 1. Everything is Explicit

**No type inference** — Every variable must have a type annotation:

```klar
// ✓ Correct - explicit types
let x: i32 = 42
let name: string = "Alice"
var count: i32 = 0

// ✗ Won't compile - no type inference
let x = 42  // ERROR
```

**Why this helps AI:** You never have to infer types from context. The type is right there.

### 2. One Way to Do Things

Klar avoids redundant syntax forms:

```klar
// Arithmetic
let sum: i32 = a + b           // Standard (traps on overflow)
let sum: i32 = a +% b          // Wrapping
let sum: i32 = a +| b          // Saturating

// Logic
if condition and other_condition { }    // Always 'and', never '&&'
if not done { }                          // Always 'not', never '!'
```

**Why this helps AI:** Fewer choices means more consistent code generation.

### 3. Explicit Returns

Functions always use `return` statements:

```klar
fn add(a: i32, b: i32) -> i32 {
    return a + b    // Explicit return
}

// ✗ No implicit returns
fn add(a: i32, b: i32) -> i32 {
    a + b  // ERROR - not valid
}
```

**Why this helps AI:** Return points are always visible and unambiguous.

### 4. Explicit Conversions

Type conversions are always marked:

```klar
let x: i32 = 42
let y: i64 = x.as[i64]     // Safe widening
let z: i16 = x.to[i16]     // Checked narrowing (may trap)
let w: i8 = x.trunc[i8]    // Truncating (never traps)

// ✗ No implicit conversions
let y: i64 = x  // ERROR
```

**Why this helps AI:** Every conversion is intentional and visible.

## Code Generation Patterns

### Pattern: Error Handling with Result

```klar
fn read_config(path: string) -> Result[Config, IoError] {
    let content: string = fs_read_string(path)?  // Propagates error
    let config: Config = parse_config(content)?
    return Ok(config)
}

fn main() -> i32 {
    let config_result: Result[Config, IoError] = read_config("config.json")
    match config_result {
        Ok(config) => {
            use_config(config)
        }
        Err(e) => {
            println("Failed to load config: {e}")
            return 1
        }
    }
    return 0
}
```

### Pattern: Optional Handling

```klar
fn find_user(id: i32) -> ?User {
    if id == 0 {
        return None
    }
    return Some(User { id: id, name: "User" })
}

fn main() -> i32 {
    let user: ?User = find_user(42)
    
    // Pattern 1: Default value
    let name: string = user?.name ?? "Unknown"
    
    // Pattern 2: Match for complex logic
    match user {
        Some(u) => { println("Found: {u.name}") }
        None => { println("Not found") }
    }
    
    // Pattern 3: Force unwrap (traps if None)
    let forced: User = user!  // Use only when None is a bug
    
    return 0
}
```

### Pattern: Iteration

```klar
// Fixed array iteration
let arr: [i32; 5] = [1, 2, 3, 4, 5]
for item: i32 in arr {
    println("{item}")
}

// Range iteration
for i: i32 in 0..10 {
    println("{i}")
}

// List iteration
var list: List[string] = List.new[string]()
list.push("one")
list.push("two")
for item: string in list {
    println(item)
}

// Map iteration
var map: Map[string, i32] = Map.new[string, i32]()
map.insert("a", 1)
map.insert("b", 2)
for entry: (string, i32) in map {
    println("{entry.0}: {entry.1}")
}
```

### Pattern: Structs with Methods

```klar
struct Point {
    x: f64,
    y: f64,
}

impl Point {
    fn new(x: f64, y: f64) -> Point {
        return Point { x: x, y: y }
    }
    
    fn distance(self: Point) -> f64 {
        return sqrt(self.x * self.x + self.y * self.y)
    }
    
    fn translate(self: inout Point, dx: f64, dy: f64) {
        self.x = self.x + dx
        self.y = self.y + dy
    }
}

fn main() -> i32 {
    var p: Point = Point::new(3.0, 4.0)
    println("Distance: {p.distance()}")  // 5.0
    p.translate(1.0, 1.0)
    return 0
}
```

### Pattern: Generic Functions

```klar
fn swap[T](a: inout T, b: inout T) {
    let temp: T = *a
    *a = *b
    *b = temp
}

fn max[T: Ordered](a: T, b: T) -> T {
    if a > b {
        return a
    }
    return b
}

fn main() -> i32 {
    var x: i32 = 1
    var y: i32 = 2
    swap(ref x, ref y)
    
    let m: i32 = max(x, y)
    println("Max: {m}")
    
    return 0
}
```

## Common Mistakes to Avoid

### Mistake 1: Missing Type Annotations

```klar
// ✗ Wrong
let x = 42
var items = List.new()

// ✓ Correct
let x: i32 = 42
var items: List[i32] = List.new[i32]()
```

### Mistake 2: Using Symbolic Operators

```klar
// ✗ Wrong
if (a && b) || !c { }

// ✓ Correct
if (a and b) or not c { }
```

### Mistake 3: Implicit Returns

```klar
// ✗ Wrong
fn add(a: i32, b: i32) -> i32 {
    a + b
}

// ✓ Correct
fn add(a: i32, b: i32) -> i32 {
    return a + b
}
```

### Mistake 4: Implicit Type Conversions

```klar
// ✗ Wrong
let x: i32 = 42
let y: i64 = x

// ✓ Correct
let x: i32 = 42
let y: i64 = x.as[i64]
```

### Mistake 5: Missing Struct Field Types

```klar
// ✗ Wrong (if such syntax existed)
struct Point { x, y }

// ✓ Correct
struct Point {
    x: f64,
    y: f64,
}
```

## REPL for Verification

Klar provides a REPL for interactive code verification. AI agents can use this to test code snippets before presenting them to users:

```bash
$ klar repl
klar> let x: i32 = 42
klar> x + 10
52
klar> :type x + 10
i32
klar> fn double(n: i32) -> i32 { return n * 2 }
klar> double(x)
84
```

### REPL Commands

| Command | Purpose |
|---------|---------|
| `:help` | Show available commands |
| `:type <expr>` | Show expression type without evaluating |
| `:list` | Show all current bindings |
| `:load <file>` | Load definitions from a file |
| `:reset` | Clear all bindings |
| `:quit` | Exit the REPL |

## File I/O Patterns

```klar
fn main() -> i32 {
    // Reading a file
    let content_result: Result[String, IoError] = fs_read_string("input.txt")
    match content_result {
        Ok(content) => {
            println("File contents:")
            println(content)
        }
        Err(e) => {
            println("Error reading file: {e}")
            return 1
        }
    }
    
    // Writing a file
    let write_result: Result[void, IoError] = fs_write_string("output.txt", "Hello!")
    match write_result {
        Ok(_) => { println("File written successfully") }
        Err(e) => { println("Error writing file: {e}") }
    }
    
    // Checking existence
    if fs_exists("config.json") {
        println("Config exists")
    }
    
    return 0
}
```

## FFI Patterns (Calling C)

```klar
extern {
    fn strlen(s: CStr) -> usize
    fn puts(s: CStr) -> i32
}

fn main() -> i32 {
    let msg: string = "Hello from Klar!"
    
    unsafe {
        let len: usize = strlen(msg.as_cstr())
        println("Length: {len}")
        puts(msg.as_cstr())
    }
    
    return 0
}
```

## Best Practices Summary

1. **Always provide explicit types** — No inference
2. **Use `and`/`or`/`not`** — Not `&&`/`||`/`!`
3. **Use explicit `return`** — Every return is visible
4. **Use explicit conversions** — `.as[]`, `.to[]`, `.trunc[]`
5. **Handle errors with `Result`** — Use `?` for propagation
6. **Handle nullability with `?T`** — No null pointers
7. **Test in REPL when unsure** — Verify before presenting

## Quick Reference

### Type Conversions

| Conversion | Method | Behavior |
|------------|--------|----------|
| Safe widening | `.as[T]` | Always succeeds |
| Checked narrowing | `.to[T]` | Traps on overflow |
| Truncating | `.trunc[T]` | Discards high bits |

### Arithmetic Operators

| Operation | Standard | Wrapping | Saturating |
|-----------|----------|----------|------------|
| Add | `+` | `+%` | `+|` |
| Subtract | `-` | `-%` | `-|` |
| Multiply | `*` | `*%` | `*|` |

### Control Flow

```klar
// If (statement, not expression)
if condition { } else { }

// Match (statement, not expression)
match value {
    Pattern => { }
    _ => { }
}

// For loop
for item: T in collection { }
for i: i32 in 0..n { }

// While loop
while condition { }

// Loop (infinite)
loop {
    if done { break }
}
```

---

*Klar: designed by Claude Code, for Claude Code.*
