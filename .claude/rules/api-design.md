---
globs: ["*.zig"]
---

# API Design Rules

## A0: Common Import Patterns

Standard imports at top of every file:

```zig
const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const log = std.log.scoped(.my_module);

// For builtin checks
const builtin = @import("builtin");
const is_debug = builtin.mode == .Debug;
const is_test = builtin.is_test;
```

Import order convention:
1. `std` imports
2. Package dependencies
3. Local modules
4. Pub declarations

## A1: Accept Slices
- Accept `[]const u8` not fixed arrays or pointer+length
- More flexible for callers

```zig
// GOOD
pub fn process(data: []const u8) void

// BAD
pub fn process(data: [*]const u8, len: usize) void
```

## A2: Optional Types
- Use `?T` for nullable values
- Never use magic values like -1 or null pointers

```zig
// GOOD
pub fn find(haystack: []const u8, needle: u8) ?usize

// BAD
pub fn find(haystack: []const u8, needle: u8) isize  // -1 for not found
```

## A3: Configuration Structs
- Use struct with defaults for optional parameters
- Enables clean call sites with `.{}`

```zig
pub const Config = struct {
    port: u16 = 8080,
    timeout_ms: u32 = 30_000,
    tls_enabled: bool = false,
};

// Usage
const server = try Server.init(allocator, .{});
const server = try Server.init(allocator, .{ .port = 443 });
```

## A4: Return Structs
- Return structs for multiple values
- Avoid out parameters except for error info

```zig
pub const Result = struct { quotient: i32, remainder: i32 };
pub fn divide(a: i32, b: i32) Result
```

## A5: Const Correctness
- Use `const` slices when not modifying: `[]const u8`
- Use `*const Self` for read-only methods

## A6: Builder Pattern
- Use for complex object construction
- Return `*Self` from setters for chaining

## A7: File I/O Pattern (Zig 0.15+)
- File.Writer does NOT have `.print()` method
- Use `std.fmt.bufPrint()` + `file.writeAll()` for formatted output
- For simple writes, use `file.writeAll()` directly

```zig
// GOOD (Zig 0.15+): Format to buffer, then write
var buf: [256]u8 = undefined;
const formatted = std.fmt.bufPrint(&buf, "Value: {d}\n", .{value}) catch unreachable;
try file.writeAll(formatted);

// GOOD: Simple writes
try file.writeAll("Hello, world!\n");
try file.writeAll(data_slice);

// BAD: File.Writer has no .print() method
const writer = file.writer();
try writer.print("Hello {s}\n", .{name});  // Compile error!

// For stdout/stderr, same pattern applies
const stdout = std.io.getStdOut();
var buf: [256]u8 = undefined;
const msg = std.fmt.bufPrint(&buf, "Result: {d}\n", .{result}) catch unreachable;
try stdout.writeAll(msg);
```

## A8: Removed Features (Zig 0.15+)
- `usingnamespace` keyword removed - use explicit imports
- `async`/`await` keywords removed
- `BoundedArray` removed - accept slices or use dynamic allocation

## A9: Sentinel-Terminated Slice Patterns

Understanding when to use `[:0]`, `[*:0]`, and `[]` types:

```zig
// [:0]const u8 - Slice with known length AND null terminator
// Use when: Need both slice operations and C interop
fn processAndCallC(str: [:0]const u8) void {
    std.debug.print("Length: {d}\n", .{str.len});  // Can use .len
    c_api(str.ptr);  // Safe: guaranteed null-terminated
}

// [*:0]const u8 - Pointer to null-terminated (unknown length)
// Use when: Receiving from C, length not yet known
fn fromCString(c_str: [*:0]const u8) []const u8 {
    return std.mem.span(c_str);  // Convert to slice
}

// []const u8 - Regular slice (no sentinel)
// Use when: General string processing, no C interop needed
fn processString(str: []const u8) void {
    // Cannot pass to C APIs expecting null terminator!
}
```

### Converting Between Types

```zig
// [*:0]const u8 → [:0]const u8 (compute length)
const c_str: [*:0]const u8 = c.get_string();
const slice: [:0]const u8 = std.mem.sliceTo(c_str, 0);

// [*:0]const u8 → []const u8 (lose sentinel info)
const regular: []const u8 = std.mem.span(c_str);

// []const u8 → [:0]const u8 (allocate)
const z_str = try allocator.dupeZ(u8, regular);
defer allocator.free(z_str);

// [:0]const u8 → []const u8 (implicit, safe)
fn takesSlice(s: []const u8) void {}
takesSlice(sentinel_slice);  // OK, implicit conversion

// []const u8 → [:0]const u8 (NOT implicit, need to add sentinel)
// This is a compile error: cannot convert
```

### Sentinel Best Practices

```zig
// GOOD: Use sentinel types for C interop APIs
pub fn openFile(path: [:0]const u8) !File {
    return c.fopen(path.ptr, "r") orelse error.OpenFailed;
}

// GOOD: Accept regular slices for internal APIs
pub fn processData(data: []const u8) !void {
    // Internal processing
}

// BAD: Losing sentinel unnecessarily
fn callC(str: []const u8) void {  // Lost sentinel info!
    // c_api(str.ptr);  // DANGER: not null-terminated
}

// GOOD: Preserve sentinel in signature
fn callC(str: [:0]const u8) void {
    c_api(str.ptr);  // Safe
}
```

### String Literal Types

```zig
// String literals are already sentinel-terminated
const literal: [:0]const u8 = "hello";  // OK
const ptr: [*:0]const u8 = "hello";     // OK
const slice: []const u8 = "hello";      // OK (implicit conversion)

// But computed strings may not be
const computed = allocator.alloc(u8, 5);  // NOT sentinel-terminated
// Need dupeZ for sentinel:
const with_sentinel = try allocator.dupeZ(u8, computed);
```
