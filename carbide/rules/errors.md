---
globs: ["*.zig"]
---

# Error Handling Rules

## E1: Specific Error Sets
- Define domain-specific error sets with descriptive names
- Suffix error set types with `Error`

```zig
pub const ParseError = error{
    InvalidSyntax,
    UnexpectedToken,
    OutOfMemory,
};

pub fn parse(input: []const u8) ParseError!Ast
```

## E2: Error Propagation
- Use `try` for automatic propagation
- Use `catch` when you need to handle or transform errors

```zig
// Propagate
const value = try readFile(path);

// Handle
const value = readFile(path) catch |err| {
    log.err("Failed: {}", .{err});
    return err;
};

// Transform
const value = readFile(path) catch return error.ConfigLoadFailed;
```

## E3: Cleanup with errdefer
- Use `errdefer` for cleanup that should only run on error
- Order matters: later errdefer runs first

```zig
const a = try alloc();
errdefer free(a);

const b = try alloc();
errdefer free(b);
// If we fail here, b is freed, then a
```

## E4: Error Payloads
- Use optional out parameter for detailed error info
- Keep error sets small, details in payload

```zig
pub fn validate(input: []const u8, err_info: ?*ErrorInfo) !void
```

## E5: anyerror Usage
- Avoid `anyerror` except at API boundaries
- Use specific error sets for internal functions

## E6: Assertions
- Use `std.debug.assert` for invariants (disabled in release)
- Use `unreachable` for logically impossible states
