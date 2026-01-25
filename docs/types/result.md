# Result Type

The `Result[T, E]` type represents an operation that either succeeds with a value of type `T` or fails with an error of type `E`.

## Creating Results

### Ok - Success

```klar
let success: Result[i32, string] = Ok(42)
let file: Result[File, IoError] = Ok(opened_file)
```

### Err - Failure

```klar
let failure: Result[i32, string] = Err("something went wrong")
let io_err: Result[File, IoError] = Err(IoError::NotFound)
```

## Checking Results

### is_ok() and is_err()

```klar
let result: Result[i32, string] = Ok(42)

if result.is_ok() {
    println("Success!")
}

if result.is_err() {
    println("Failed!")
}
```

## Unwrapping Results

### Force Unwrap (!)

Use `!` when you're certain the result is `Ok`:

```klar
let result: Result[i32, string] = Ok(42)
let value: i32 = result!  // 42
```

**Warning**: Panics if the result is `Err`:

```klar
let err: Result[i32, string] = Err("oops")
let bad: i32 = err!  // Runtime panic!
```

### Pattern Matching

The safest way to handle both cases:

```klar
fn handle(result: Result[i32, string]) {
    match result {
        Ok(value) => {
            println("Success: {value}")
        }
        Err(message) => {
            println("Error: {message}")
        }
    }
}
```

## Error Propagation (?)

The `?` operator propagates errors up the call stack:

```klar
fn read_number() -> Result[i32, string] {
    let content: string = read_file("number.txt")?  // Propagates Err
    let number: i32 = parse_int(content)?           // Propagates Err
    return Ok(number)
}
```

This is equivalent to:

```klar
fn read_number() -> Result[i32, string] {
    var content: string
    match read_file("number.txt") {
        Ok(c) => { content = c }
        Err(e) => { return Err(e) }
    }

    var number: i32
    match parse_int(content) {
        Ok(n) => { number = n }
        Err(e) => { return Err(e) }
    }

    return Ok(number)
}
```

## Result Methods

### map()

Transform the success value:

```klar
let result: Result[i32, string] = Ok(5)
let doubled: Result[i32, string] = result.map(|x: i32| -> i32 { return x * 2 })
// doubled is Ok(10)

let err: Result[i32, string] = Err("fail")
let still_err: Result[i32, string] = err.map(|x: i32| -> i32 { return x * 2 })
// still_err is Err("fail")
```

### map_err()

Transform the error value:

```klar
let err: Result[i32, string] = Err("fail")
let new_err: Result[i32, i32] = err.map_err(|s: string| -> i32 { return s.len() })
// new_err is Err(4)
```

### and_then()

Chain operations that return Results:

```klar
fn parse_and_validate(s: string) -> Result[i32, string] {
    return parse_int(s).and_then(|n: i32| -> Result[i32, string] {
        if n > 0 {
            return Ok(n)
        }
        return Err("must be positive")
    })
}
```

### or_else()

Provide an alternative on error:

```klar
fn try_primary() -> Result[i32, string] {
    return primary_source().or_else(|_: string| -> Result[i32, string] {
        return backup_source()
    })
}
```

### unwrap_or()

Default value on error:

```klar
let result: Result[i32, string] = Err("fail")
let value: i32 = result.unwrap_or(0)  // 0
```

### unwrap_or_else()

Compute default lazily:

```klar
let result: Result[i32, string] = Err("fail")
let value: i32 = result.unwrap_or_else(|e: string| -> i32 {
    println("Error: {e}")
    return 0
})
```

## Common Error Types

### String Errors

Simple error messages:

```klar
fn divide(a: i32, b: i32) -> Result[i32, string] {
    if b == 0 {
        return Err("division by zero")
    }
    return Ok(a / b)
}
```

### Enum Errors

Structured error types:

```klar
enum ParseError {
    InvalidFormat,
    Overflow,
    Empty,
}

fn parse_number(s: string) -> Result[i32, ParseError] {
    if s.len() == 0 {
        return Err(ParseError::Empty)
    }
    // ... parsing logic
}
```

### Error Context

Add context to errors:

```klar
fn load_config(path: string) -> Result[Config, string] {
    let content: string = read_file(path).map_err(|e: string| -> string {
        return "failed to read {path}: {e}"
    })?

    let config: Config = parse_config(content).map_err(|e: string| -> string {
        return "failed to parse config: {e}"
    })?

    return Ok(config)
}
```

## Chaining Multiple Operations

```klar
fn process_data(input: string) -> Result[Output, Error] {
    let step1: Intermediate = parse(input)?
    let step2: Processed = transform(step1)?
    let result: Output = finalize(step2)?
    return Ok(result)
}
```

## Example: File Processing

```klar
fn read_numbers(path: string) -> Result[List[i32], string] {
    let content: string = read_file(path)?
    let lines: List[string] = content.split("\n")

    var numbers: List[i32] = List.new[i32]()

    for line: string in lines {
        let trimmed: string = line.trim()
        if trimmed.len() > 0 {
            let n: i32 = trimmed.to[i32].ok_or("invalid number: {trimmed}")?
            numbers.push(n)
        }
    }

    return Ok(numbers)
}

fn main() -> i32 {
    let result: Result[List[i32], string] = read_numbers("data.txt")

    match result {
        Ok(numbers) => {
            println("Read {numbers.len()} numbers")
            return 0
        }
        Err(message) => {
            println("Error: {message}")
            return 1
        }
    }
}
```

## Example: Validation Pipeline

```klar
struct User {
    name: string,
    age: i32,
    email: string,
}

fn validate_name(name: string) -> Result[string, string] {
    if name.len() < 2 {
        return Err("name too short")
    }
    return Ok(name)
}

fn validate_age(age: i32) -> Result[i32, string] {
    if age < 0 or age > 150 {
        return Err("invalid age")
    }
    return Ok(age)
}

fn validate_email(email: string) -> Result[string, string] {
    if not email.contains("@") {
        return Err("invalid email")
    }
    return Ok(email)
}

fn create_user(name: string, age: i32, email: string) -> Result[User, string] {
    let valid_name: string = validate_name(name)?
    let valid_age: i32 = validate_age(age)?
    let valid_email: string = validate_email(email)?

    return Ok(User {
        name: valid_name,
        age: valid_age,
        email: valid_email,
    })
}
```

## Best Practices

### Use Descriptive Error Types

```klar
// Good - specific error
enum ConfigError {
    FileNotFound(string),
    ParseError(string),
    InvalidValue(string),
}

// Okay for simple cases
fn simple() -> Result[i32, string] { ... }
```

### Prefer ? for Clean Code

```klar
// Good - clean chain
fn process() -> Result[Output, Error] {
    let a = step1()?
    let b = step2(a)?
    return Ok(step3(b)?)
}
```

### Add Context to Errors

```klar
// Good - adds context
let data = read_file(path).map_err(|e| "reading {path}: {e}")?

// Loses context
let data = read_file(path)?
```

## Next Steps

- [Optional](optional.md) - For values that may be absent
- [Error Handling](../language/error-handling.md) - Complete guide
- [Enums](../language/enums.md) - Custom error types
