# File Utilities (`stdlib/file.kl`)

> **Status:** Planned (Phase 7.3). See [ROADMAP.md](../../ROADMAP.md) for details.

Convenience functions for reading and writing files, built on compiler builtins.

```klar
import stdlib.file.*
```

## Planned Functions

### `file_write(path: string, content: string) -> Result#[void, string]`

Write string content to a file, creating it if it doesn't exist or overwriting if it does.

```klar
let result: Result#[void, string] = file_write("output.txt", "Hello, world!")
```

### `file_write_lines(path: string, lines: List#[string]) -> Result#[void, string]`

Write a list of strings to a file, each followed by a newline.

```klar
var lines: List#[string] = List.new#[string]()
lines.push("line 1")
lines.push("line 2")
let result: Result#[void, string] = file_write_lines("output.txt", lines)
```

### `file_append(path: string, content: string) -> Result#[void, string]`

Append string content to a file, creating it if it doesn't exist.

```klar
let result: Result#[void, string] = file_append("log.txt", "new entry\n")
```

## Function Reference

| Function | Description |
|----------|-------------|
| `file_write(path, content)` | Write content to file (create/overwrite) |
| `file_write_lines(path, lines)` | Write lines to file with newlines |
| `file_append(path, content)` | Append content to file (create if missing) |
