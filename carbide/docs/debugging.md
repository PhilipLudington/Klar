# Debugging Guide

Strategies and tools for debugging Zig programs.

## Print Debugging

### std.debug.print

Always available, writes to stderr:

```zig
std.debug.print("Value: {d}, String: {s}\n", .{value, string});

// Print any type with {any}
std.debug.print("Debug: {any}\n", .{complex_struct});

// Print pointer addresses
std.debug.print("Address: {*}\n", .{ptr});

// Print slices
std.debug.print("Data: {s}\n", .{byte_slice});  // As string
std.debug.print("Hex: {x}\n", .{std.fmt.fmtSliceHexLower(data)});
```

### Conditional Debug Output

```zig
const debug = @import("builtin").mode == .Debug;

fn debugLog(comptime fmt: []const u8, args: anytype) void {
    if (debug) {
        std.debug.print("[DEBUG] " ++ fmt ++ "\n", args);
    }
}

// Usage
debugLog("Processing item {d}", .{index});
```

### std.log for Structured Logging

```zig
const std = @import("std");
const log = std.log.scoped(.my_module);

pub fn process() void {
    log.debug("Starting process", .{});
    log.info("Processing {} items", .{count});
    log.warn("Item skipped: {s}", .{reason});
    log.err("Failed: {}", .{err});
}

// Configure log level at build time
pub const std_options = .{
    .log_level = .debug,  // Show all logs
};
```

## Memory Debugging

### Detecting Leaks with GPA

```zig
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .stack_trace_frames = 10,  // Capture allocation stack traces
    }){};
    defer {
        const status = gpa.deinit();
        if (status == .leak) {
            std.debug.print("LEAK DETECTED!\n", .{});
        }
    }

    try run(gpa.allocator());
}
```

### Finding Leak Sources

```zig
var gpa = std.heap.GeneralPurposeAllocator(.{
    .stack_trace_frames = 15,  // More frames = more context
    .retain_metadata = true,   // Keep info after free
    .verbose_log = true,       // Log all allocations
}){};
```

### Testing for Leaks

```zig
test "no memory leaks" {
    // std.testing.allocator reports leaks as test failure
    const data = try process(std.testing.allocator);
    defer std.testing.allocator.free(data);

    try std.testing.expectEqual(expected, data);
}  // Test fails if anything wasn't freed
```

### Use-After-Free Detection

Poison memory after free to catch use-after-free:

```zig
pub fn deinit(self: *Self) void {
    self.allocator.free(self.buffer);
    self.* = undefined;  // Fills with 0xAA in debug mode
}
```

## Runtime Checks

### Safety Checks in Debug Builds

Zig's debug builds include:

```zig
// Bounds checking (panic in debug, UB in release)
const value = array[index];  // Panics if out of bounds

// Integer overflow (panic in debug)
const result = a + b;  // Panics if overflows

// Null optional unwrap
const value = optional.?;  // Panics if null

// Unreachable (always panics)
unreachable;  // Signals "impossible" code path
```

### Explicit Assertions

```zig
// Only checked in debug builds
std.debug.assert(index < array.len);
std.debug.assert(ptr != null);

// Always checked (debug and release)
if (index >= array.len) {
    @panic("Index out of bounds");
}
```

## Debugger Integration

### @breakpoint()

Insert programmatic breakpoints:

```zig
fn process(data: []const u8) !void {
    if (data.len == 0) {
        @breakpoint();  // Break here when attached to debugger
        return error.EmptyInput;
    }
    // ...
}
```

### Building for Debugging

```bash
# Debug build (default)
zig build

# With debug info and no optimizations
zig build -Doptimize=Debug

# View disassembly
zig build-exe src/main.zig --verbose-llvm-ir
```

### LLDB/GDB Usage

```bash
# Start with debugger
lldb ./zig-out/bin/myapp

# In LLDB
(lldb) breakpoint set --name main
(lldb) run
(lldb) bt              # Backtrace
(lldb) frame variable  # Show local variables
(lldb) p variable_name # Print variable
```

### VS Code Integration

`.vscode/launch.json`:
```json
{
    "version": "0.2.0",
    "configurations": [
        {
            "type": "lldb",
            "request": "launch",
            "name": "Debug",
            "program": "${workspaceFolder}/zig-out/bin/myapp",
            "args": [],
            "cwd": "${workspaceFolder}",
            "preLaunchTask": "zig build"
        }
    ]
}
```

## Stack Traces

### Getting Stack Traces

```zig
// Print current stack trace
std.debug.dumpCurrentStackTrace(@returnAddress());

// Get stack trace on error
fn riskyOperation() !void {
    return error.SomethingWrong;
}

// When catching errors
riskyOperation() catch |err| {
    std.debug.print("Error: {}\n", .{err});
    if (@errorReturnTrace()) |trace| {
        std.debug.dumpStackTrace(trace.*);
    }
    return err;
};
```

### Error Return Traces

Enable with build option:
```zig
// In build.zig
exe.error_tracing = true;
```

Or compile with:
```bash
zig build-exe src/main.zig -freference-trace
```

## Compile-Time Debugging

### @compileLog

Print values at compile time:

```zig
fn GenericType(comptime T: type) type {
    @compileLog("Creating type for:", T);
    @compileLog("Size:", @sizeOf(T));
    // ...
}
```

### @compileError

Fail compilation with message:

```zig
fn process(comptime T: type) void {
    if (@sizeOf(T) > 1024) {
        @compileError("Type too large for this function");
    }
}
```

### Type Inspection

```zig
fn debugType(comptime T: type) void {
    const info = @typeInfo(T);
    @compileLog("Type name:", @typeName(T));
    @compileLog("Type info:", info);

    switch (info) {
        .Struct => |s| {
            @compileLog("Fields:", s.fields.len);
            inline for (s.fields) |field| {
                @compileLog("  Field:", field.name);
            }
        },
        else => {},
    }
}
```

## Common Issues and Solutions

### Issue: Segmentation Fault

**Possible causes:**
- Null pointer dereference
- Use after free
- Buffer overflow
- Stack overflow

**Debug steps:**
```zig
// 1. Add bounds checks
std.debug.assert(index < slice.len);

// 2. Check for null
if (ptr) |p| {
    // safe to use p
}

// 3. Verify allocator returned valid memory
const data = allocator.alloc(u8, size) catch |err| {
    std.debug.print("Allocation failed: {}\n", .{err});
    return err;
};
```

### Issue: Unexpected Behavior in Release

Debug works but release crashes:

```zig
// Integer overflow (undefined in ReleaseFast)
// Use explicit wrapping or saturating arithmetic
const result = @addWithOverflow(a, b);
if (result[1] != 0) {
    // Handle overflow
}

// Or use wrapping arithmetic explicitly
const result = a +% b;  // Wrapping add
```

### Issue: Test Passes Locally, Fails in CI

```zig
// Don't rely on undefined behavior
var x: u32 = undefined;
x = 42;  // Good

var x: u32 = undefined;
std.debug.print("{}\n", .{x});  // Bad - reading undefined
```

### Issue: Memory Corruption

```zig
// Use GPA to track allocations
var gpa = std.heap.GeneralPurposeAllocator(.{
    .safety = true,
    .never_unmap = true,  // Keep freed pages mapped to catch UAF
}){};

// Check for double-free
// GPA will panic on double-free in debug mode
```

## Build System Debugging

### Verbose Build Output

```bash
# Show all commands
zig build --verbose

# Show summary timing
zig build --summary all

# Watch mode (Zig 0.15+)
zig build --watch
```

### Dependency Issues

```bash
# List dependencies
zig fetch --help

# Clear cache
rm -rf ~/.cache/zig
rm -rf zig-cache
```

## Performance Debugging

### Built-in Timer

```zig
var timer = try std.time.Timer.start();

// ... code to measure ...

const elapsed_ns = timer.read();
std.debug.print("Elapsed: {d}ms\n", .{elapsed_ns / std.time.ns_per_ms});
```

### Profiling Build

```bash
# Build with profiling info
zig build -Doptimize=ReleaseSafe

# Use system profiler (Linux)
perf record ./zig-out/bin/myapp
perf report
```

## Quick Reference

| Task | Solution |
|------|----------|
| Print debug output | `std.debug.print("{any}\n", .{value})` |
| Detect memory leaks | Use `GeneralPurposeAllocator` |
| Set breakpoint | `@breakpoint()` |
| Get stack trace | `std.debug.dumpCurrentStackTrace(@returnAddress())` |
| Compile-time debug | `@compileLog(value)` |
| Assert in debug only | `std.debug.assert(condition)` |
| Always panic | `@panic("message")` |
| Check bounds | `if (i >= len) return error.OutOfBounds` |
