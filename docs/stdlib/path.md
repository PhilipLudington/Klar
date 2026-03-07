# Path Manipulation (`stdlib/path.kl`)

Pure Klar path parsing and manipulation for Unix-style paths. No compiler builtins needed — uses string primitives only.

```klar
import stdlib.path.*
```

## Functions

### `path_join(base: string, component: string) -> string`

Join two path segments, handling trailing/leading slashes.

```klar
path_join("a", "b")      // "a/b"
path_join("a/", "b")     // "a/b"
path_join("a", "/b")     // "/b"  (absolute component replaces base)
path_join("", "b")       // "b"
path_join("a", "")       // "a"
```

### `path_parent(path: string) -> ?string`

Return the directory portion of a path. `None` for bare filenames without slashes.

```klar
path_parent("/usr/local/bin")  // Some("/usr/local")
path_parent("/usr/local/")     // Some("/usr/local")
path_parent("foo/bar")         // Some("foo")
path_parent("foo")             // None
path_parent("/")               // None
path_parent("")                // None
```

### `path_file_name(path: string) -> ?string`

Return the filename component (after the last slash). `None` for empty or slash-only paths.

```klar
path_file_name("/usr/local/bin/klar")  // Some("klar")
path_file_name("foo/bar.kl")           // Some("bar.kl")
path_file_name("bar.kl")               // Some("bar.kl")
path_file_name("/")                    // None
path_file_name("")                     // None
```

### `path_extension(path: string) -> ?string`

Return the file extension without the leading dot. `None` if no extension.

```klar
path_extension("main.kl")         // Some("kl")
path_extension("archive.tar.gz")  // Some("gz")
path_extension(".hidden")         // None  (dot-files have no extension)
path_extension("Makefile")        // None
```

### `path_stem(path: string) -> ?string`

Return the filename without its extension. `None` if no filename.

```klar
path_stem("main.kl")         // Some("main")
path_stem("archive.tar.gz")  // Some("archive.tar")
path_stem(".hidden")          // Some(".hidden")
path_stem("Makefile")         // Some("Makefile")
path_stem("/usr/bin/klar")    // Some("klar")
```

### `path_is_absolute(path: string) -> bool`

Check if a path starts with `/`.

```klar
path_is_absolute("/usr/bin")  // true
path_is_absolute("src/main")  // false
path_is_absolute("")          // false
```

### `path_normalize(path: string) -> string`

Resolve `.` and `..` segments in a path.

```klar
path_normalize("a/./b/../c")    // "a/c"
path_normalize("/a/b/../c")     // "/a/c"
path_normalize("../a/b")        // "../a/b"
path_normalize("a/b/../../..")  // ".."
path_normalize("")              // "."
path_normalize("/")             // "/"
```

## Function Reference

| Function | Description |
|----------|-------------|
| `path_join(base, component)` | Join two path segments |
| `path_parent(path)` | Get parent directory as `?string` |
| `path_file_name(path)` | Get filename component as `?string` |
| `path_extension(path)` | Get file extension as `?string` |
| `path_stem(path)` | Get filename without extension as `?string` |
| `path_is_absolute(path)` | Check if path is absolute |
| `path_normalize(path)` | Resolve `.` and `..` segments |
