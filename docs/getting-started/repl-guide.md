# REPL Guide

The Klar REPL (Read-Eval-Print Loop) enables interactive exploration and experimentation with the language.

## Starting the REPL

```bash
klar repl
```

You'll see a prompt:

```
Klar REPL v0.3.1-dev
Type :help for available commands

klar>
```

## Basic Usage

Enter expressions and statements to evaluate them immediately:

```
klar> let x: i32 = 42
klar> x + 10
52
klar> let y: i32 = x * 2
klar> y
84
```

### Defining Functions

```
klar> fn double(n: i32) -> i32 {
...       return n * 2
...   }
klar> double(21)
42
```

### Defining Structs

```
klar> struct Point { x: i32, y: i32 }
klar> let p: Point = Point { x: 10, y: 20 }
klar> p.x + p.y
30
```

## REPL Commands

Special commands start with `:`:

| Command | Description |
|---------|-------------|
| `:help` | Show available commands |
| `:type <expr>` | Show the type of an expression |
| `:list` | Show all current bindings |
| `:load <file>` | Load definitions from a file |
| `:reset` | Clear all bindings |
| `:quit` or `:q` | Exit the REPL |

### :type

Inspect the type of an expression without evaluating it:

```
klar> :type 42
i32

klar> :type "hello"
string

klar> fn add(a: i32, b: i32) -> i32 { return a + b }
klar> :type add
fn(i32, i32) -> i32

klar> :type add(1, 2)
i32
```

### :list

Show all defined variables, functions, and types:

```
klar> let x: i32 = 10
klar> fn square(n: i32) -> i32 { return n * n }
klar> :list
Variables:
  x: i32
Functions:
  square: fn(i32) -> i32
```

### :load

Load definitions from a Klar source file:

```
klar> :load utils.kl
Loaded: utils.kl

klar> :list
Functions:
  helper: fn(i32) -> i32
  process: fn(string) -> string
```

### :reset

Clear all bindings and start fresh:

```
klar> let x: i32 = 42
klar> :reset
Environment cleared.

klar> x
Error: undefined variable 'x'
```

## State Persistence

Bindings persist across inputs within a session:

```
klar> let factor: i32 = 10
klar> fn scale(n: i32) -> i32 { return n * factor }
klar> scale(5)
50
```

## Error Handling

Errors don't crash the session - you can continue after errors:

```
klar> let x: i32 = "not a number"
Error: type mismatch: expected i32, got string

klar> let x: i32 = 42
klar> x
42
```

## Limitations

The REPL uses the interpreter backend, which has some limitations compared to compiled code:

- Multi-file imports are not supported in REPL
- Performance is slower than compiled code
- Some advanced features may not be available

For multi-file projects, use `klar run` or `klar build` instead.

## Workflows

### Exploratory Programming

Use the REPL to quickly test ideas:

```
klar> fn fib(n: i32) -> i32 {
...       if n <= 1 { return n }
...       return fib(n - 1) + fib(n - 2)
...   }
klar> fib(10)
55
```

### Type Exploration

Use `:type` to understand how types work:

```
klar> :type Some(42)
?i32

klar> :type Ok[i32, string](42)
Result[i32, string]
```

### Testing Functions

Load a file and test its functions interactively:

```
klar> :load mylib.kl
klar> process_data(test_input)
[output]
klar> process_data(edge_case)
[output]
```

## Keyboard Shortcuts

| Shortcut | Action |
|----------|--------|
| `Ctrl+C` | Cancel current input |
| `Ctrl+D` | Exit REPL (same as `:quit`) |
| `Up/Down` | Navigate history |

## Next Steps

- [Basics](../language/basics.md) - Language fundamentals
- [Functions](../language/functions.md) - Function definitions
