# Error Handling Patterns

This document describes error handling patterns for CarbideZig projects.

## Pattern 1: Domain-Specific Error Sets

Define focused error sets for each domain or operation.

### Implementation

```zig
pub const ParseError = error{
    InvalidSyntax,
    UnexpectedToken,
    UnterminatedString,
    NestingTooDeep,
};

pub const IoError = error{
    FileNotFound,
    PermissionDenied,
    DiskFull,
};

pub const ConfigError = ParseError || IoError || error{
    MissingRequiredField,
    InvalidValue,
};

pub fn loadConfig(path: []const u8) ConfigError!Config {
    const contents = readFile(path) catch |err| switch (err) {
        error.FileNotFound => return error.FileNotFound,
        error.PermissionDenied => return error.PermissionDenied,
        else => return error.FileNotFound,  // Fallback
    };
    return parseConfig(contents);
}
```

### Why?
- Clear documentation of possible failures
- Compile-time exhaustiveness checking
- Callers know exactly what can go wrong

---

## Pattern 2: Error Propagation with try

Use `try` for clean error propagation.

### Implementation

```zig
pub fn processFile(allocator: Allocator, path: []const u8) !Document {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const contents = try file.readToEndAlloc(allocator, max_size);
    defer allocator.free(contents);

    const parsed = try parse(contents);
    return try Document.fromParsed(allocator, parsed);
}
```

### When to Use
- When you want to propagate errors up the call stack
- When there's no recovery action at this level

---

## Pattern 3: Error Handling with catch

Handle errors locally when recovery is possible.

### Implementation

```zig
pub fn connectWithRetry(address: Address, max_retries: u32) !Connection {
    var attempt: u32 = 0;
    while (attempt < max_retries) : (attempt += 1) {
        const conn = Connection.init(address) catch |err| {
            switch (err) {
                error.ConnectionRefused, error.Timeout => {
                    std.time.sleep(std.time.ns_per_s);
                    continue;  // Retry
                },
                else => return err,  // Don't retry other errors
            }
        };
        return conn;
    }
    return error.MaxRetriesExceeded;
}
```

---

## Pattern 4: Error Transformation

Transform low-level errors into domain-specific ones.

### Implementation

```zig
pub const LoadError = error{
    ConfigNotFound,
    ConfigInvalid,
    ConfigPermissionDenied,
};

pub fn loadConfig(path: []const u8) LoadError!Config {
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        return switch (err) {
            error.FileNotFound => error.ConfigNotFound,
            error.AccessDenied => error.ConfigPermissionDenied,
            else => error.ConfigNotFound,
        };
    };
    defer file.close();

    const contents = file.readToEndAlloc(allocator, max_size) catch {
        return error.ConfigInvalid;
    };
    defer allocator.free(contents);

    return parseConfig(contents) catch error.ConfigInvalid;
}
```

---

## Pattern 5: Error Payload via Out Parameter

Provide detailed error information without bloating error unions.

### Implementation

```zig
pub const ValidationError = struct {
    line: usize,
    column: usize,
    message: []const u8,
    code: ErrorCode,

    pub const ErrorCode = enum {
        syntax_error,
        type_mismatch,
        undefined_reference,
    };
};

pub fn validate(
    source: []const u8,
    error_info: ?*ValidationError,
) error{ValidationFailed}!Ast {
    // During validation...
    if (found_error) {
        if (error_info) |info| {
            info.* = .{
                .line = current_line,
                .column = current_column,
                .message = "unexpected token",
                .code = .syntax_error,
            };
        }
        return error.ValidationFailed;
    }
    return ast;
}

// Usage
var err_info: ValidationError = undefined;
const ast = validate(source, &err_info) catch |err| {
    std.debug.print("Error at {d}:{d}: {s}\n", .{
        err_info.line, err_info.column, err_info.message,
    });
    return err;
};
```

---

## Pattern 6: Cleanup with errdefer Chain

Proper ordering of errdefer for complex initialization.

### Implementation

```zig
pub fn init(allocator: Allocator) !Self {
    // Resources are acquired in order
    const a = try allocateA(allocator);
    errdefer freeA(a);  // Cleaned up last (if error after this point)

    const b = try allocateB(allocator);
    errdefer freeB(b);  // Cleaned up second

    const c = try allocateC(allocator);
    errdefer freeC(c);  // Cleaned up first

    // On success, no errdefer runs
    return Self{ .a = a, .b = b, .c = c };
}

// On error, cleanup runs in reverse order: freeC, freeB, freeA
```

---

## Pattern 7: Fallible Initialization with Partial Cleanup

Handle cleanup of partially-initialized structs.

### Implementation

```zig
const Server = struct {
    socket: Socket,
    handlers: HandlerMap,
    pool: ThreadPool,

    pub fn init(allocator: Allocator, config: Config) !Server {
        var socket = try Socket.bind(config.address);
        errdefer socket.close();

        var handlers = HandlerMap.init(allocator);
        errdefer handlers.deinit();

        try handlers.put("/health", healthHandler);
        try handlers.put("/api", apiHandler);

        var pool = try ThreadPool.init(config.threads);
        // No errdefer needed - if we get here, we succeed

        return Server{
            .socket = socket,
            .handlers = handlers,
            .pool = pool,
        };
    }

    pub fn deinit(self: *Server) void {
        self.pool.deinit();
        self.handlers.deinit();
        self.socket.close();
    }
};
```

---

## Pattern 8: Optional Results vs Errors

Choose between `?T` and `!T` based on semantics.

### Implementation

```zig
// Use optional when absence is a normal case
pub fn find(haystack: []const u8, needle: u8) ?usize {
    return std.mem.indexOfScalar(u8, haystack, needle);
}

// Use error union when absence is exceptional
pub fn loadRequired(path: []const u8) !Config {
    return loadConfig(path) catch |err| {
        log.err("Required config missing: {s}", .{path});
        return err;
    };
}

// Combine when you need both
pub fn get(self: *Self, key: []const u8) !?Value {
    if (!self.connected) return error.NotConnected;
    return self.cache.get(key);  // null if not found
}
```

---

## Anti-Patterns

### DON'T: Silently Ignore Errors

```zig
// BAD
file.close();  // Ignores potential error
_ = doSomething();  // Discards result

// GOOD
file.close();  // void return is fine
doSomething() catch |err| {
    log.warn("Non-critical failure: {}", .{err});
};
```

### DON'T: Catch Everything with anyerror

```zig
// BAD - loses type information
pub fn process() anyerror!void {
    try step1();
    try step2();
    try step3();
}

// GOOD - specific error set
pub const ProcessError = Step1Error || Step2Error || Step3Error;
pub fn process() ProcessError!void { }
```

### DON'T: Panic on Recoverable Errors

```zig
// BAD
const file = std.fs.cwd().openFile(path, .{}) catch {
    @panic("Failed to open file");  // Unrecoverable crash
};

// GOOD
const file = std.fs.cwd().openFile(path, .{}) catch |err| {
    return err;  // Let caller decide
};
```
