# Idiom Translation Guide

How to translate common patterns from C, Rust, and Python to idiomatic Zig.

## Memory Management

### C malloc/free → Zig Allocators

```c
// C
char* buffer = malloc(1024);
if (buffer == NULL) return -1;
// use buffer
free(buffer);
```

```zig
// Zig
const buffer = try allocator.alloc(u8, 1024);
defer allocator.free(buffer);
// use buffer
```

### C realloc → Zig realloc

```c
// C
buffer = realloc(buffer, new_size);
if (buffer == NULL) { /* handle error */ }
```

```zig
// Zig
buffer = try allocator.realloc(buffer, new_size);
```

### Rust Box → Zig allocator.create

```rust
// Rust
let node = Box::new(Node { value: 42, next: None });
```

```zig
// Zig
const node = try allocator.create(Node);
node.* = .{ .value = 42, .next = null };
defer allocator.destroy(node);
```

### Rust Vec → Zig ArrayList

```rust
// Rust
let mut items: Vec<i32> = Vec::new();
items.push(1);
items.push(2);
```

```zig
// Zig
var items = std.ArrayListUnmanaged(i32){};
defer items.deinit(allocator);
try items.append(allocator, 1);
try items.append(allocator, 2);
```

### Python with (context manager) → Zig defer

```python
# Python
with open("file.txt") as f:
    data = f.read()
# file automatically closed
```

```zig
// Zig
const file = try std.fs.cwd().openFile("file.txt", .{});
defer file.close();
const data = try file.readToEndAlloc(allocator, max_size);
defer allocator.free(data);
```

## Error Handling

### C Error Codes → Zig Error Unions

```c
// C
int result = do_something();
if (result < 0) {
    return result;  // Error code
}
```

```zig
// Zig
const result = try doSomething();  // Propagates error automatically
```

### C errno → Zig Error Sets

```c
// C
FILE* f = fopen(path, "r");
if (f == NULL) {
    switch (errno) {
        case ENOENT: /* not found */
        case EACCES: /* permission denied */
    }
}
```

```zig
// Zig
const file = std.fs.cwd().openFile(path, .{}) catch |err| switch (err) {
    error.FileNotFound => { /* not found */ },
    error.AccessDenied => { /* permission denied */ },
    else => return err,
};
```

### Rust Result → Zig Error Unions

```rust
// Rust
fn divide(a: i32, b: i32) -> Result<i32, DivError> {
    if b == 0 {
        return Err(DivError::DivideByZero);
    }
    Ok(a / b)
}

// Usage
match divide(10, 2) {
    Ok(result) => println!("{}", result),
    Err(e) => eprintln!("Error: {:?}", e),
}
```

```zig
// Zig
const DivError = error{DivideByZero};

fn divide(a: i32, b: i32) DivError!i32 {
    if (b == 0) return error.DivideByZero;
    return @divTrunc(a, b);
}

// Usage
const result = divide(10, 2) catch |err| {
    std.debug.print("Error: {}\n", .{err});
    return;
};
std.debug.print("{}\n", .{result});
```

### Rust ? operator → Zig try

```rust
// Rust
fn process() -> Result<Data, Error> {
    let file = File::open("data.txt")?;
    let content = read_to_string(file)?;
    Ok(parse(content)?)
}
```

```zig
// Zig
fn process() !Data {
    const file = try std.fs.cwd().openFile("data.txt", .{});
    defer file.close();
    const content = try file.readToEndAlloc(allocator, max_size);
    defer allocator.free(content);
    return try parse(content);
}
```

### Python try/except → Zig catch

```python
# Python
try:
    result = risky_operation()
except ValueError as e:
    result = default_value
```

```zig
// Zig
const result = riskyOperation() catch |err| switch (err) {
    error.InvalidValue => default_value,
    else => return err,
};
```

## Nullability

### C NULL Checks → Zig Optionals

```c
// C
Node* find(List* list, int value) {
    for (Node* n = list->head; n != NULL; n = n->next) {
        if (n->value == value) return n;
    }
    return NULL;
}

// Usage
Node* node = find(list, 42);
if (node != NULL) {
    // use node
}
```

```zig
// Zig
fn find(list: *List, value: i32) ?*Node {
    var n = list.head;
    while (n) |node| : (n = node.next) {
        if (node.value == value) return node;
    }
    return null;
}

// Usage
if (find(list, 42)) |node| {
    // use node
}
```

### Rust Option → Zig Optional

```rust
// Rust
fn find(items: &[i32], target: i32) -> Option<usize> {
    items.iter().position(|&x| x == target)
}

// Usage
match find(&items, 42) {
    Some(index) => println!("Found at {}", index),
    None => println!("Not found"),
}
```

```zig
// Zig
fn find(items: []const i32, target: i32) ?usize {
    for (items, 0..) |item, index| {
        if (item == target) return index;
    }
    return null;
}

// Usage
if (find(items, 42)) |index| {
    std.debug.print("Found at {}\n", .{index});
} else {
    std.debug.print("Not found\n", .{});
}
```

### Python None → Zig null

```python
# Python
def get_user(id: int) -> Optional[User]:
    if id in users:
        return users[id]
    return None
```

```zig
// Zig
fn getUser(id: u64) ?*User {
    return users.get(id);
}
```

## Data Structures

### C Arrays → Zig Slices

```c
// C
void process(int* arr, size_t len) {
    for (size_t i = 0; i < len; i++) {
        arr[i] *= 2;
    }
}
```

```zig
// Zig
fn process(arr: []i32) void {
    for (arr) |*item| {
        item.* *= 2;
    }
}
```

### C Strings → Zig Slices

```c
// C
size_t len = strlen(str);
char* copy = malloc(len + 1);
strcpy(copy, str);
```

```zig
// Zig
const copy = try allocator.dupe(u8, str);
defer allocator.free(copy);
```

### Rust HashMap → Zig HashMap

```rust
// Rust
let mut map: HashMap<String, i32> = HashMap::new();
map.insert("key".to_string(), 42);
if let Some(value) = map.get("key") {
    println!("{}", value);
}
```

```zig
// Zig
var map = std.StringHashMap(i32).init(allocator);
defer map.deinit();
try map.put("key", 42);
if (map.get("key")) |value| {
    std.debug.print("{}\n", .{value});
}
```

### Python dict → Zig HashMap

```python
# Python
data = {"name": "Alice", "age": 30}
name = data.get("name", "Unknown")
```

```zig
// Zig (typed values)
var data = std.StringHashMap(i32).init(allocator);
defer data.deinit();
try data.put("age", 30);
const age = data.get("age") orelse 0;

// For mixed types, use tagged unions or std.json.Value
```

## Iteration

### C for loop → Zig for

```c
// C
for (int i = 0; i < 10; i++) {
    printf("%d\n", i);
}
```

```zig
// Zig
for (0..10) |i| {
    std.debug.print("{}\n", .{i});
}
```

### C while → Zig while

```c
// C
while (condition) {
    // body
}
```

```zig
// Zig
while (condition) {
    // body
}
```

### Rust Iterator → Zig for with slices

```rust
// Rust
for (i, item) in items.iter().enumerate() {
    println!("{}: {}", i, item);
}
```

```zig
// Zig
for (items, 0..) |item, i| {
    std.debug.print("{}: {}\n", .{i, item});
}
```

### Python enumerate → Zig indexed for

```python
# Python
for i, item in enumerate(items):
    print(f"{i}: {item}")
```

```zig
// Zig
for (items, 0..) |item, i| {
    std.debug.print("{}: {}\n", .{i, item});
}
```

### Python zip → Zig multi-object for

```python
# Python
for a, b in zip(list_a, list_b):
    print(a, b)
```

```zig
// Zig (requires same length)
for (list_a, list_b) |a, b| {
    std.debug.print("{} {}\n", .{a, b});
}
```

## Generics

### C void* → Zig comptime T

```c
// C
void swap(void* a, void* b, size_t size) {
    char temp[size];
    memcpy(temp, a, size);
    memcpy(a, b, size);
    memcpy(b, temp, size);
}
```

```zig
// Zig
fn swap(comptime T: type, a: *T, b: *T) void {
    const temp = a.*;
    a.* = b.*;
    b.* = temp;
}
```

### Rust Generics → Zig Comptime

```rust
// Rust
struct Container<T> {
    items: Vec<T>,
}

impl<T> Container<T> {
    fn new() -> Self {
        Container { items: Vec::new() }
    }
}
```

```zig
// Zig
fn Container(comptime T: type) type {
    return struct {
        const Self = @This();
        items: std.ArrayListUnmanaged(T) = .{},

        pub fn init() Self {
            return .{};
        }
    };
}
```

### Rust Traits → Zig Interfaces (duck typing)

```rust
// Rust
trait Writer {
    fn write(&mut self, data: &[u8]) -> Result<usize, Error>;
}

fn write_message<W: Writer>(writer: &mut W) -> Result<(), Error> {
    writer.write(b"Hello")?;
    Ok(())
}
```

```zig
// Zig (duck typing with anytype or concrete types)
fn writeMessage(writer: anytype) !void {
    try writer.writeAll("Hello");
}

// Or with Zig 0.15+ concrete Writer type
fn writeMessage(writer: std.Io.Writer) !void {
    try writer.writeAll("Hello");
}
```

## Closures/Lambdas

### Python Lambda → Zig Anonymous Struct

```python
# Python
items.sort(key=lambda x: x.name)
```

```zig
// Zig
std.sort.sort(Item, items, {}, struct {
    fn lessThan(_: void, a: Item, b: Item) bool {
        return std.mem.lessThan(u8, a.name, b.name);
    }
}.lessThan);
```

### Rust Closure → Zig Function + Context

```rust
// Rust
let multiplier = 3;
let multiply = |x| x * multiplier;
```

```zig
// Zig (explicit context)
const Context = struct {
    multiplier: i32,

    fn multiply(self: Context, x: i32) i32 {
        return x * self.multiplier;
    }
};
const ctx = Context{ .multiplier = 3 };
const result = ctx.multiply(5);
```

## OOP Patterns

### C++ Class → Zig Struct

```cpp
// C++
class Counter {
private:
    int count;
public:
    Counter() : count(0) {}
    void increment() { count++; }
    int get() const { return count; }
};
```

```zig
// Zig
const Counter = struct {
    count: i32 = 0,

    pub fn increment(self: *Counter) void {
        self.count += 1;
    }

    pub fn get(self: Counter) i32 {
        return self.count;
    }
};
```

### Inheritance → Composition

```cpp
// C++ (inheritance)
class Animal {
    virtual void speak() = 0;
};
class Dog : public Animal {
    void speak() override { cout << "Woof"; }
};
```

```zig
// Zig (composition + interfaces)
const Animal = struct {
    speakFn: *const fn(*Animal) void,

    pub fn speak(self: *Animal) void {
        self.speakFn(self);
    }
};

const Dog = struct {
    animal: Animal,

    pub fn init() Dog {
        return .{
            .animal = .{ .speakFn = speakImpl },
        };
    }

    fn speakImpl(_: *Animal) void {
        std.debug.print("Woof\n", .{});
    }
};
```

## Concurrency

### C pthread → Zig std.Thread

```c
// C
pthread_t thread;
pthread_create(&thread, NULL, thread_func, arg);
pthread_join(thread, NULL);
```

```zig
// Zig
const thread = try std.Thread.spawn(.{}, threadFunc, .{arg});
thread.join();
```

### Rust Mutex → Zig std.Thread.Mutex

```rust
// Rust
let counter = Arc::new(Mutex::new(0));
let mut num = counter.lock().unwrap();
*num += 1;
```

```zig
// Zig
var mutex = std.Thread.Mutex{};
var counter: i32 = 0;

mutex.lock();
defer mutex.unlock();
counter += 1;
```

## Quick Reference Table

| Concept | C | Rust | Python | Zig |
|---------|---|------|--------|-----|
| Allocate | `malloc` | `Box::new` | automatic | `allocator.alloc` |
| Free | `free` | automatic | automatic | `allocator.free` |
| Error | return code | `Result<T,E>` | exception | error union `!T` |
| Null | `NULL` | `Option<T>` | `None` | `?T` (optional) |
| Propagate error | manual | `?` | automatic | `try` |
| Array | `T*` + len | `Vec<T>` | `list` | `[]T` (slice) |
| String | `char*` | `String` | `str` | `[]const u8` |
| Generic | `void*` | `<T>` | duck typing | `comptime T: type` |
| Class | struct + functions | `impl` | `class` | struct + methods |
| Interface | function pointers | trait | protocol | anytype or vtable |
