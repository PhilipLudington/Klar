# Debugging Klar Programs

Klar generates DWARF debug information when compiled with the `-g` flag, enabling source-level debugging with `lldb`.

## Building with Debug Info

```bash
klar build program.kl -o program -g
```

On macOS, `dsymutil` runs automatically to create the `.dSYM` bundle.

## Using lldb

### Start a debug session

```bash
lldb ./program
```

### Set breakpoints

```lldb
# Break on a specific line
(lldb) b program.kl:10

# Break on function entry
(lldb) b main
(lldb) b my_function
```

### Run and control execution

```lldb
# Start the program
(lldb) r

# Step to next line (step over)
(lldb) n

# Step into function call
(lldb) s

# Continue to next breakpoint
(lldb) c

# Quit
(lldb) q
```

### Inspect state

```lldb
# Show local variables and their values
(lldb) frame variable

# Show a specific variable
(lldb) frame variable x

# Show the call stack
(lldb) bt

# Show source code around current location
(lldb) source list
```

## What Works

- Breakpoints on file:line (e.g., `b program.kl:7`)
- Source code display at breakpoints
- Step-by-step execution (`n` for next line, `s` for step into)
- Local variable inspection for primitive types (`i32`, `i64`, `f64`, `bool`)
- Function parameter inspection
- Call stack (`bt`)

## Known Limitations

- Complex types (structs, enums, collections) show as opaque values — only primitives have full type debug info
- Optimized builds (`-O2`) may inline functions and reorder instructions, making stepping less predictable
- Multi-module programs show debug info only for the entry file's source
- The `source list` command requires the source file to be accessible from the compilation directory

## Example Session

Given `hello.kl`:
```klar
fn add(a: i32, b: i32) -> i32 {
    let sum: i32 = a + b
    return sum
}

fn main() -> i32 {
    let x: i32 = 10
    let y: i32 = 20
    let result: i32 = add(x, y)
    return result
}
```

```
$ klar build hello.kl -o hello -g
$ lldb hello
(lldb) b hello.kl:7
Breakpoint 1: where = hello`main + 8 at hello.kl:7:5
(lldb) r
Process stopped at hello.kl:7:5
-> 7    let x: i32 = 10
(lldb) n
(lldb) n
(lldb) n
-> 10   return result
(lldb) frame variable
(int) x = 10
(int) y = 20
(int) result = 30
```
