# Hello World

Let's write your first Klar program.

## Your First Program

Create a file named `hello.kl`:

```klar
fn main() -> i32 {
    println("Hello, Klar!")
    return 0
}
```

Run it:

```bash
klar run hello.kl
```

Output:

```
Hello, Klar!
```

## Understanding the Code

### The main Function

Every Klar program needs a `main` function as its entry point:

```klar
fn main() -> i32 {
    // Your code here
    return 0
}
```

- `fn` - Keyword to declare a function
- `main` - The function name (entry point)
- `-> i32` - Return type (32-bit integer)
- `return 0` - Exit code (0 = success)

### Explicit Types

Klar requires explicit type annotations. This is by design - it makes code self-documenting and unambiguous:

```klar
let x: i32 = 42         // Immutable integer
var count: i32 = 0      // Mutable integer
let name: string = "Klar"  // String
```

### Printing Output

Use `print` and `println` for output:

```klar
fn main() -> i32 {
    print("Hello ")      // No newline
    println("World!")    // With newline
    return 0
}
```

## A More Complete Example

Here's a program that demonstrates basic features:

```klar
fn add(a: i32, b: i32) -> i32 {
    return a + b
}

fn main() -> i32 {
    let x: i32 = 10
    let y: i32 = 20
    let sum: i32 = add(x, y)

    println("The sum is:")
    println("{sum}")    // String interpolation

    return 0
}
```

### String Interpolation

Klar supports string interpolation with `{expression}`:

```klar
let name: string = "Alice"
let age: i32 = 30
println("Name: {name}, Age: {age}")
```

## Command-Line Arguments

Your program can accept command-line arguments:

```klar
fn main(args: [String]) -> i32 {
    println("Number of arguments: {args.len()}")

    for arg: String in args {
        println("  {arg}")
    }

    return 0
}
```

Run with arguments:

```bash
klar run program.kl arg1 arg2
# Or use -- to separate klar flags from program args:
klar run program.kl -- --flag value
```

Note: `args[0]` is the program path.

## Compiling to an Executable

Instead of running directly, you can compile to a native executable:

```bash
klar build hello.kl           # Creates build/hello
klar build hello.kl -o myapp  # Creates myapp
./build/hello                 # Run the executable
```

The compiled executable runs without needing the Klar compiler.

## Next Steps

- [CLI Reference](cli-reference.md) - All command-line options
- [Basics](../language/basics.md) - Variables, types, and expressions
- [Functions](../language/functions.md) - Function definitions
