# I/O Types

Klar provides types for file and stream I/O operations.

## File

The `File` type represents an open file handle.

### Opening Files

```klar
// Open for reading
let file: Result[File, IoError] = File.open("data.txt")

// Open for writing (creates or truncates)
let file: Result[File, IoError] = File.create("output.txt")

// Open for appending
let file: Result[File, IoError] = File.append("log.txt")
```

### Reading Files

```klar
fn read_file_contents(path: string) -> Result[string, IoError] {
    let file: File = File.open(path)?
    let contents: string = file.read_to_string()?
    file.close()
    return Ok(contents)
}
```

### Writing Files

```klar
fn write_to_file(path: string, content: string) -> Result[(), IoError] {
    let file: File = File.create(path)?
    file.write(content)?
    file.close()
    return Ok(())
}
```

### File Methods

| Method | Description |
|--------|-------------|
| `File.open(path)` | Open file for reading |
| `File.create(path)` | Create/truncate file for writing |
| `File.append(path)` | Open file for appending |
| `.read_to_string()` | Read entire file as string |
| `.read_bytes()` | Read entire file as bytes |
| `.write(string)` | Write string to file |
| `.write_bytes(bytes)` | Write bytes to file |
| `.close()` | Close the file handle |

### Example: Copy File

```klar
fn copy_file(src: string, dst: string) -> Result[(), IoError] {
    let content: string = File.open(src)?.read_to_string()?
    File.create(dst)?.write(content)?
    return Ok(())
}
```

## Standard Streams

### Stdin

Read from standard input:

```klar
fn read_line() -> Result[string, IoError] {
    return Stdin.read_line()
}

fn main() -> i32 {
    print("Enter your name: ")
    let name: string = Stdin.read_line() ?? ""
    println("Hello, {name}!")
    return 0
}
```

### Stdout

Write to standard output:

```klar
fn main() -> i32 {
    Stdout.write("Hello, ")
    Stdout.write("World!")
    Stdout.write("\n")
    return 0
}

// More commonly, use println:
fn main() -> i32 {
    println("Hello, World!")
    return 0
}
```

### Stderr

Write to standard error:

```klar
fn log_error(message: string) {
    Stderr.write("ERROR: ")
    Stderr.write(message)
    Stderr.write("\n")
}
```

## Buffered I/O

### BufReader

Buffered reading for efficient I/O:

```klar
let file: File = File.open("large_file.txt")?
let reader: BufReader = BufReader.new(file)

// Read line by line
while true {
    let line: ?string = reader.read_line()
    match line {
        Some(l) => { process_line(l) }
        None => { break }  // EOF
    }
}
```

### BufWriter

Buffered writing for efficient I/O:

```klar
let file: File = File.create("output.txt")?
let writer: BufWriter = BufWriter.new(file)

for i: i32 in 0..1000 {
    writer.write("Line {i}\n")
}

writer.flush()  // Ensure all data is written
```

### BufReader Methods

| Method | Description |
|--------|-------------|
| `BufReader.new(file)` | Create buffered reader |
| `.read_line()` | Read next line |
| `.read_bytes(n)` | Read n bytes |
| `.close()` | Close underlying file |

### BufWriter Methods

| Method | Description |
|--------|-------------|
| `BufWriter.new(file)` | Create buffered writer |
| `.write(string)` | Write string to buffer |
| `.write_bytes(bytes)` | Write bytes to buffer |
| `.flush()` | Flush buffer to file |
| `.close()` | Flush and close |

## IoError

I/O operations return `Result[T, IoError]`:

```klar
enum IoError {
    NotFound,
    PermissionDenied,
    AlreadyExists,
    InvalidData,
    UnexpectedEof,
    Other(string),
}
```

### Handling I/O Errors

```klar
fn read_config() -> Result[string, string] {
    let result: Result[File, IoError] = File.open("config.txt")

    match result {
        Ok(file) => {
            return file.read_to_string().map_err(|e: IoError| -> string {
                return "failed to read: {e}"
            })
        }
        Err(IoError.NotFound) => {
            return Err("config file not found")
        }
        Err(IoError.PermissionDenied) => {
            return Err("no permission to read config")
        }
        Err(e) => {
            return Err("unexpected error: {e}")
        }
    }
}
```

## Example: Line-by-Line Processing

```klar
fn process_log_file(path: string) -> Result[i32, IoError] {
    let file: File = File.open(path)?
    let reader: BufReader = BufReader.new(file)

    var error_count: i32 = 0

    loop {
        let line: ?string = reader.read_line()
        match line {
            Some(l) => {
                if l.contains("ERROR") {
                    error_count = error_count + 1
                    println("Found error: {l}")
                }
            }
            None => { break }
        }
    }

    reader.close()
    return Ok(error_count)
}
```

## Example: Writing Structured Data

```klar
fn write_csv(path: string, data: List[(string, i32)]) -> Result[(), IoError] {
    let file: File = File.create(path)?
    let writer: BufWriter = BufWriter.new(file)

    // Write header
    writer.write("name,value\n")

    // Write data rows
    for (name, value) in data {
        writer.write("{name},{value}\n")
    }

    writer.flush()
    writer.close()
    return Ok(())
}
```

## Example: Interactive Input

```klar
fn interactive_menu() -> i32 {
    loop {
        println("Menu:")
        println("1. Option A")
        println("2. Option B")
        println("3. Exit")
        print("Choice: ")

        let input: string = Stdin.read_line() ?? ""
        let choice: ?i32 = input.trim().to[i32]

        match choice {
            Some(1) => { println("Selected A") }
            Some(2) => { println("Selected B") }
            Some(3) => { return 0 }
            _ => { println("Invalid choice") }
        }
        println("")
    }
}
```

## Best Practices

### Always Handle Errors

```klar
// Good - handles potential failure
let file: File = File.open(path)?
// or
match File.open(path) {
    Ok(f) => { /* use f */ }
    Err(e) => { /* handle error */ }
}

// Bad - can panic
let file: File = File.open(path)!
```

### Close Files When Done

```klar
fn process_file(path: string) -> Result[(), IoError] {
    let file: File = File.open(path)?
    // ... do work ...
    file.close()  // Don't forget!
    return Ok(())
}
```

### Use Buffered I/O for Large Files

```klar
// Good for large files - efficient
let reader: BufReader = BufReader.new(File.open(path)?)
while let Some(line) = reader.read_line() {
    process(line)
}

// Less efficient for large files
let content: string = File.open(path)?.read_to_string()?
```

## Next Steps

- [Error Handling](../language/error-handling.md) - Working with Result
- [Collections](collections.md) - Storing file data
- [Primitives](primitives.md) - String operations
