---
globs: ["*.zig"]
---

# Logging Rules

## L1: Use std.log
- Use scoped logging for module identification
- Consistent API across the codebase

```zig
const std = @import("std");
const log = std.log.scoped(.my_module);

pub fn processRequest(req: Request) !Response {
    log.debug("Processing: {s}", .{req.path});
    // ...
}
```

## L2: Log Levels
- `err` - Error conditions requiring attention
- `warn` - Warning conditions, recoverable issues
- `info` - Informational, normal operation milestones
- `debug` - Debug-level, verbose details

```zig
log.err("Connection failed: {}", .{err});
log.warn("Retry attempt {d}/{d}", .{attempt, max_retries});
log.info("Server started on port {d}", .{port});
log.debug("Received packet: {d} bytes", .{len});
```

## L3: Structured Data
- Include context in log messages
- Use format placeholders, not string concatenation

```zig
// GOOD
log.info("Request completed: method={s} path={s} status={d}", .{
    method, path, status,
});

// BAD
log.info("Request completed", .{});
```

## L4: Security
- NEVER log sensitive data (passwords, tokens, PII)
- Sanitize user input before logging
- Be careful with error messages containing user data

```zig
// BAD
log.debug("Login attempt: user={s} password={s}", .{user, password});

// GOOD
log.debug("Login attempt: user={s}", .{user});
```

## L5: Custom Log Function
- Override `std_options.logFn` for custom formatting
- Add timestamps, structured output, etc.

```zig
pub const std_options = .{
    .log_level = .debug,
    .logFn = customLog,
};
```
