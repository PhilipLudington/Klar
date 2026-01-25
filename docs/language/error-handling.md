# Error Handling

Klar uses two main types for error handling: `?T` (Optional) for values that may be absent, and `Result[T, E]` for operations that may fail with an error.

## Optional Type (?T)

The optional type `?T` represents a value that may or may not exist.

### Creating Optional Values

```klar
let some_value: ?i32 = Some(42)
let no_value: ?i32 = None
```

### Returning Optional from Functions

When a function returns `?T`, returning a value wraps it in `Some`:

```klar
fn find_value(id: i32) -> ?i32 {
    if id == 1 {
        return 100  // Automatically wrapped as Some(100)
    }
    // Implicit None return at end of function
}
```

### Unwrapping Optionals

#### Force Unwrap (!)

Use `!` to unwrap when you're certain the value exists:

```klar
let opt: ?i32 = Some(42)
let value: i32 = opt!  // 42

// Panics if None!
let empty: ?i32 = None
let bad: i32 = empty!  // Runtime panic
```

#### Null Coalescing (??)

Use `??` to provide a default value:

```klar
let opt: ?i32 = None
let value: i32 = opt ?? 0  // 0 (default used)

let opt2: ?i32 = Some(42)
let value2: i32 = opt2 ?? 0  // 42 (value used)
```

### Pattern Matching Optionals

```klar
fn process(opt: ?i32) -> string {
    var result: string
    match opt {
        Some(value) => { result = "Got: {value}" }
        None => { result = "Nothing" }
    }
    return result
}
```

## Result Type

`Result[T, E]` represents an operation that either succeeds with a value of type `T` or fails with an error of type `E`.

### Creating Results

```klar
let success: Result[i32, string] = Ok(42)
let failure: Result[i32, string] = Err("something went wrong")
```

### Returning Results from Functions

```klar
fn divide(a: i32, b: i32) -> Result[i32, string] {
    if b == 0 {
        return Err("division by zero")
    }
    return Ok(a / b)
}
```

### Unwrapping Results

#### Force Unwrap (!)

```klar
let result: Result[i32, string] = Ok(42)
let value: i32 = result!  // 42

// Panics if Err!
let err: Result[i32, string] = Err("oops")
let bad: i32 = err!  // Runtime panic
```

### Pattern Matching Results

```klar
fn handle_result(r: Result[i32, string]) {
    match r {
        Ok(value) => {
            println("Success: {value}")
        }
        Err(message) => {
            println("Error: {message}")
        }
    }
}
```

### Checking Result Status

```klar
let result: Result[i32, string] = Ok(42)

if result.is_ok() {
    println("Success!")
}

if result.is_err() {
    println("Failed!")
}
```

## Error Propagation (?)

The `?` operator propagates errors up the call stack.

### With Result

```klar
fn read_config() -> Result[Config, string] {
    let content: string = read_file("config.txt")?  // Propagates if Err
    let config: Config = parse_config(content)?     // Propagates if Err
    return Ok(config)
}
```

If `read_file` returns `Err`, the function immediately returns that error. If it returns `Ok`, the value is unwrapped and execution continues.

### With Optional

The `?` operator can also propagate `None`:

```klar
fn get_user_email(id: i32) -> ?string {
    let user: User = find_user(id)?      // Returns None if not found
    let email: string = user.email?       // Returns None if email is None
    return Some(email)
}
```

## Combining Approaches

### Converting Between Types

```klar
// Optional to Result
fn opt_to_result(opt: ?i32) -> Result[i32, string] {
    match opt {
        Some(value) => { return Ok(value) }
        None => { return Err("value not found") }
    }
}

// Result to Optional (discards error)
fn result_to_opt(r: Result[i32, string]) -> ?i32 {
    match r {
        Ok(value) => { return Some(value) }
        Err(_) => { return None }
    }
}
```

## Example: File Processing

```klar
fn process_file(path: string) -> Result[i32, string] {
    // Read file, propagate error if fails
    let content: string = read_file(path)?

    // Parse number, propagate if fails
    let number: i32 = parse_int(content)?

    // Process the number
    return Ok(number * 2)
}

fn main() -> i32 {
    let result: Result[i32, string] = process_file("data.txt")

    match result {
        Ok(value) => {
            println("Result: {value}")
            return 0
        }
        Err(message) => {
            println("Error: {message}")
            return 1
        }
    }
}
```

## Example: Chained Optionals

```klar
struct User {
    name: string,
    address: ?Address,
}

struct Address {
    city: ?string,
}

fn get_user_city(user: ?User) -> ?string {
    // Each ? returns None if the value is None
    let u: User = user?
    let addr: Address = u.address?
    let city: string = addr.city?
    return Some(city)
}
```

## Best Practices

### Use Optional When

- A value might legitimately be absent
- The absence is not an error condition
- You need a simple "present or not" distinction

```klar
fn find_by_id(id: i32) -> ?User {
    // User might not exist - that's normal
}
```

### Use Result When

- An operation can fail
- You need to communicate why it failed
- The caller needs to handle the error

```klar
fn connect(host: string) -> Result[Connection, NetworkError] {
    // Connection might fail for various reasons
}
```

### Prefer ? Over Match for Propagation

```klar
// Good - concise
fn process() -> Result[i32, string] {
    let a: i32 = step1()?
    let b: i32 = step2(a)?
    return Ok(b)
}

// Verbose - only use when you need custom handling
fn process_verbose() -> Result[i32, string] {
    var a: i32
    match step1() {
        Ok(value) => { a = value }
        Err(e) => { return Err(e) }
    }
    // ...
}
```

## Next Steps

- [Optional](../types/optional.md) - Full optional type reference
- [Result](../types/result.md) - Full result type reference
- [Control Flow](control-flow.md) - Pattern matching details
