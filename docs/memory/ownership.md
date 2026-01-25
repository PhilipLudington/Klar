# Ownership

Klar uses an ownership model for memory safety without garbage collection. Each value has a single owner, and the value is dropped when the owner goes out of scope.

## Ownership Rules

1. Each value has exactly one owner at a time
2. When the owner goes out of scope, the value is dropped
3. Values can be moved to a new owner
4. Values can be borrowed without transferring ownership

## Single Owner

Every value has one owner:

```klar
fn main() -> i32 {
    let s: string = "hello"  // s owns the string
    // ...
}  // s goes out of scope, string is dropped
```

## Move Semantics

Assignment moves ownership by default:

```klar
fn main() -> i32 {
    let a: string = "hello"
    let b: string = a  // Ownership moved from a to b

    // a is no longer valid here
    // println(a)  // Error: use of moved value

    println(b)  // OK
    return 0
}
```

### Move in Function Calls

Passing a value to a function transfers ownership:

```klar
fn consume(s: string) {
    println(s)
}  // s is dropped here

fn main() -> i32 {
    let greeting: string = "hello"
    consume(greeting)  // Ownership moved to consume

    // greeting is no longer valid
    // println(greeting)  // Error: use of moved value
    return 0
}
```

### Returning Ownership

Functions can transfer ownership back to the caller:

```klar
fn create_greeting() -> string {
    let s: string = "hello"
    return s  // Ownership transferred to caller
}

fn main() -> i32 {
    let greeting: string = create_greeting()  // greeting owns the string
    println(greeting)
    return 0
}
```

## Copy Types

Some types are copied instead of moved:

- Integers (`i32`, `u64`, etc.)
- Floating point (`f32`, `f64`)
- Booleans (`bool`)
- Characters (`char`)

```klar
fn main() -> i32 {
    let a: i32 = 42
    let b: i32 = a  // Copy, not move

    println("{a}")  // OK, a is still valid
    println("{b}")  // OK
    return 0
}
```

## Clone

For types that aren't copied, use `.clone()` to create an explicit copy:

```klar
fn main() -> i32 {
    let a: string = "hello"
    let b: string = a.clone()  // Explicit copy

    println("{a}")  // OK, a is still valid
    println("{b}")  // OK, b is a separate copy
    return 0
}
```

## Scope and Dropping

Values are dropped when they go out of scope:

```klar
fn main() -> i32 {
    {
        let s: string = "hello"
        // s is valid here
    }  // s goes out of scope and is dropped

    // s is not valid here
    return 0
}
```

### Drop Order

Values are dropped in reverse declaration order:

```klar
fn main() -> i32 {
    let a: Resource = acquire_a()
    let b: Resource = acquire_b()
    let c: Resource = acquire_c()
    // ...
}  // Dropped: c, then b, then a
```

## The Drop Trait

Types can implement `Drop` to run cleanup code:

```klar
struct Connection {
    id: i32,
}

impl Connection: Drop {
    fn drop(inout self: Connection) {
        println("Closing connection {self.id}")
    }
}

fn main() -> i32 {
    let conn: Connection = Connection { id: 42 }
    // use conn...
    return 0
}  // Prints: "Closing connection 42"
```

## Ownership and Structs

Structs own their fields:

```klar
struct Person {
    name: string,
    age: i32,
}

fn main() -> i32 {
    let person: Person = Person { name: "Alice", age: 30 }
    // person owns both name and age
}  // person dropped, name dropped
```

### Moving Out of Structs

Moving a field out of a struct partially moves the struct:

```klar
fn main() -> i32 {
    let person: Person = Person { name: "Alice", age: 30 }
    let name: string = person.name  // name moved out

    // person.name is no longer valid
    // let n: string = person.name  // Error
    let a: i32 = person.age  // OK (copy type)
    return 0
}
```

## Ownership in Collections

Collections own their elements:

```klar
fn main() -> i32 {
    var list: List[string] = List.new[string]()
    let s: string = "hello"
    list.push(s)  // s moved into list

    // s is no longer valid
    // println(s)  // Error
    return 0
}
```

## Shared Ownership

When multiple owners are needed, use reference-counted pointers:

```klar
let rc1: Rc[Data] = Rc.new(data)
let rc2: Rc[Data] = rc1.clone()  // Both own the data
// Data is dropped when both rc1 and rc2 are dropped
```

See [Reference Counting](reference-counting.md) for details.

## No Lifetimes

Unlike Rust, Klar doesn't require explicit lifetime annotations. The compiler infers lifetimes automatically, making the common cases simple while still ensuring memory safety.

## Example: Resource Management

```klar
struct FileHandle {
    path: string,
    handle: i32,
}

impl FileHandle: Drop {
    fn drop(inout self: FileHandle) {
        println("Closing file: {self.path}")
        // Close the underlying handle
    }
}

fn open_file(path: string) -> FileHandle {
    return FileHandle { path: path.clone(), handle: 42 }
}

fn process_files() {
    let file1: FileHandle = open_file("data.txt")
    let file2: FileHandle = open_file("config.txt")

    // Use files...

}  // Both files automatically closed
```

## Example: Builder Pattern

```klar
struct QueryBuilder {
    table: string,
    conditions: List[string],
}

impl QueryBuilder {
    fn new(table: string) -> QueryBuilder {
        return QueryBuilder {
            table: table,
            conditions: List.new[string](),
        }
    }

    fn where_clause(self: QueryBuilder, condition: string) -> QueryBuilder {
        var builder: QueryBuilder = self  // Takes ownership
        builder.conditions.push(condition)
        return builder  // Returns ownership
    }

    fn build(self: QueryBuilder) -> string {
        // Build query string...
        return query
    }
}

fn main() -> i32 {
    let query: string = QueryBuilder.new("users")
        .where_clause("age > 18")
        .where_clause("active = true")
        .build()

    println(query)
    return 0
}
```

## Next Steps

- [References](references.md) - Borrowing without ownership transfer
- [Reference Counting](reference-counting.md) - Shared ownership with Rc/Arc
- [Smart Pointers](../types/smart-pointers.md) - Rc, Arc, Cell
