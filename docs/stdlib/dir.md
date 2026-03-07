# Directory Walking (`stdlib/dir.kl`)

Directory enumeration built on the `fs_read_dir` and `fs_is_dir` builtins. Uses `stdlib.path` for path joining.

```klar
import stdlib.dir.*
```

## Functions

### `dir_list(path: string) -> Result#[List#[string], string]`

List all entries in a directory, returning full paths.

```klar
let entries: Result#[List#[string], string] = dir_list("src")
match entries {
    Ok(paths) => {
        var i: i32 = 0
        while i < paths.len() {
            println(paths.get(i)!)  // "src/main.zig", "src/lexer.zig", ...
            i = i + 1
        }
    }
    Err(msg) => { println(msg) }
}
```

### `dir_list_ext(path: string, ext: string) -> Result#[List#[string], string]`

List directory entries filtered by file extension (without leading dot). Returns full paths.

```klar
let kl_files: Result#[List#[string], string] = dir_list_ext("src", "kl")
// Only files ending in ".kl"
```

### `dir_walk(path: string) -> Result#[List#[string], string]`

Recursively list all files under a directory. Returns full paths for files only (not directories).

```klar
let all_files: Result#[List#[string], string] = dir_walk("src")
// All files in src/ and its subdirectories
```

### `dir_walk_ext(path: string, ext: string) -> Result#[List#[string], string]`

Recursively list all files under a directory that have the given extension. Returns full paths.

```klar
let all_kl: Result#[List#[string], string] = dir_walk_ext("src", "kl")
// All .kl files in src/ and its subdirectories
```

## Function Reference

| Function | Description |
|----------|-------------|
| `dir_list(path)` | List directory entries as full paths |
| `dir_list_ext(path, ext)` | List entries filtered by extension |
| `dir_walk(path)` | Recursively list all files |
| `dir_walk_ext(path, ext)` | Recursively list files by extension |
