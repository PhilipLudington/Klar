# Optional Type

The optional type `?T` represents a value that may or may not exist. It's Klar's way of handling nullable values safely.

## Creating Optionals

### Some - A Value Exists

```klar
let some_number: ?i32 = Some(42)
let some_string: ?string = Some("hello")
```

### None - No Value

```klar
let no_number: ?i32 = None
let no_string: ?string = None
```

### Implicit Some

When a function returns `?T`, returning a plain value wraps it in `Some`:

```klar
fn find_positive(n: i32) -> ?i32 {
    if n > 0 {
        return n  // Automatically wrapped as Some(n)
    }
    // Implicit None at end
}
```

## Checking Optionals

### is_some() and is_none()

```klar
let opt: ?i32 = Some(42)

if opt.is_some() {
    println("Has a value")
}

if opt.is_none() {
    println("No value")
}
```

## Unwrapping Optionals

### Force Unwrap (!)

Use `!` when you're certain the value exists:

```klar
let opt: ?i32 = Some(42)
let value: i32 = opt!  // 42
```

**Warning**: Panics if the optional is `None`:

```klar
let empty: ?i32 = None
let bad: i32 = empty!  // Runtime panic!
```

### Null Coalescing (??)

Provide a default value for `None`:

```klar
let opt: ?i32 = None
let value: i32 = opt ?? 0  // 0 (default used)

let opt2: ?i32 = Some(42)
let value2: i32 = opt2 ?? 0  // 42 (value used)
```

### Pattern Matching

The safest way to handle both cases:

```klar
fn handle(opt: ?i32) -> string {
    var result: string
    match opt {
        Some(value) => { result = "Got: {value}" }
        None => { result = "Nothing" }
    }
    return result
}
```

## Error Propagation (?)

The `?` operator propagates `None` up the call stack:

```klar
fn get_first_char(s: ?string) -> ?char {
    let str: string = s?  // Returns None if s is None
    if str.len() == 0 {
        return None
    }
    return Some(str[0])
}
```

This is equivalent to:

```klar
fn get_first_char(s: ?string) -> ?char {
    var str: string
    match s {
        Some(v) => { str = v }
        None => { return None }
    }
    // ... continue with str
}
```

## Optional Methods

### map()

Transform the contained value if present:

```klar
let opt: ?i32 = Some(5)
let doubled: ?i32 = opt.map(|x: i32| -> i32 { return x * 2 })
// doubled is Some(10)

let empty: ?i32 = None
let still_empty: ?i32 = empty.map(|x: i32| -> i32 { return x * 2 })
// still_empty is None
```

### and_then()

Chain operations that return optionals:

```klar
fn parse_and_double(s: ?string) -> ?i32 {
    return s.and_then(|str: string| -> ?i32 {
        let n: ?i32 = str.to[i32]
        return n.map(|x: i32| -> i32 { return x * 2 })
    })
}
```

### or()

Provide an alternative optional:

```klar
let first: ?i32 = None
let second: ?i32 = Some(42)
let result: ?i32 = first.or(second)  // Some(42)
```

### unwrap_or()

Like `??` but as a method:

```klar
let opt: ?i32 = None
let value: i32 = opt.unwrap_or(0)  // 0
```

### unwrap_or_else()

Compute default lazily:

```klar
let opt: ?i32 = None
let value: i32 = opt.unwrap_or_else(|| -> i32 { return compute_default() })
```

## Chaining Optionals

Optional field access can be chained:

```klar
struct User {
    name: string,
    email: ?string,
}

fn get_email_length(user: ?User) -> ?i32 {
    let u: User = user?           // Propagate if None
    let email: string = u.email?  // Propagate if None
    return Some(email.len())
}
```

## Example: Safe Dictionary Lookup

```klar
fn get_value(map: Map[string, i32], key: string) -> ?i32 {
    if map.contains_key(key) {
        return Some(map.get(key))
    }
    return None
}

fn main() -> i32 {
    var scores: Map[string, i32] = Map.new[string, i32]()
    scores.insert("alice", 100)
    scores.insert("bob", 85)

    let alice_score: i32 = get_value(scores, "alice") ?? 0  // 100
    let unknown_score: i32 = get_value(scores, "charlie") ?? 0  // 0

    return alice_score
}
```

## Example: Find First Match

```klar
fn find_even(numbers: [i32]) -> ?i32 {
    for n: i32 in numbers {
        if n % 2 == 0 {
            return Some(n)
        }
    }
    return None
}

fn main() -> i32 {
    let nums: [i32; 5] = [1, 3, 4, 7, 9]

    let even: ?i32 = find_even(nums)
    match even {
        Some(n) => { println("First even: {n}") }
        None => { println("No even numbers") }
    }

    return 0
}
```

## Best Practices

### Prefer ?? for Simple Defaults

```klar
// Good - concise
let value: i32 = opt ?? 0

// Verbose for simple case
var value: i32
match opt {
    Some(v) => { value = v }
    None => { value = 0 }
}
```

### Prefer ? for Propagation

```klar
// Good - propagate None
fn process(opt: ?i32) -> ?i32 {
    let v: i32 = opt?
    return Some(v * 2)
}

// Verbose
fn process_verbose(opt: ?i32) -> ?i32 {
    match opt {
        Some(v) => { return Some(v * 2) }
        None => { return None }
    }
}
```

### Avoid Force Unwrap When Possible

```klar
// Risky - can panic
let value: i32 = opt!

// Safer - handle both cases
let value: i32 = opt ?? default
// or
match opt {
    Some(v) => { /* use v */ }
    None => { /* handle missing */ }
}
```

## Next Steps

- [Result](result.md) - For operations that can fail with errors
- [Error Handling](../language/error-handling.md) - Complete guide
- [Control Flow](../language/control-flow.md) - Pattern matching
