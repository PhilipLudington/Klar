---
globs: ["*.zig"]
---

# Naming Convention Rules

## N1: Case Conventions
| Element | Style | Example |
|---------|-------|---------|
| Types, structs, enums | PascalCase | `HttpClient` |
| Functions, methods | camelCase | `readFile` |
| Variables, fields | camelCase | `bufferSize` |
| Comptime constants | snake_case | `max_size` |
| Files | snake_case | `http_client.zig` |

## N2: Function Naming Patterns

```zig
// Lifecycle
init(), deinit()           // Stack-allocated
create(), destroy()        // Heap-allocated

// Accessors (no "get" prefix)
name(), isReady(), hasValue(), count()

// Mutators
setName(), enable(), reset()

// Conversions
toSlice(), fromBytes(), asConst()

// Actions
read(), write(), connect(), process()
```

## N3: Type Naming

```zig
// Structs - nouns
const HttpClient = struct {};
const BufferWriter = struct {};

// Enums - singular nouns
const Status = enum { pending, active };
const FileMode = enum { read, write };

// Error sets - suffix "Error"
const ParseError = error{ InvalidSyntax };
```

## N4: Avoid Ambiguous Names

```zig
// BAD
var data: []u8 = undefined;
var temp: i32 = 0;

// GOOD
var response_buffer: []u8 = undefined;
var retry_count: i32 = 0;
```

## N5: Boolean Naming
- Prefix with `is_`, `has_`, `can_`, `should_`
- Use positive form (avoid double negatives)

```zig
var is_connected: bool = false;
var has_error: bool = false;
```
