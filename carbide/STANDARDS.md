# CarbideZig Coding Standards

> **Hardened Zig Development Standards for AI-Assisted Programming**
>
> Version 2.0 | Zig 0.15+

## Philosophy

**Explicit over implicit. Simple over clever. Safe over fast.**

CarbideZig enables developers to write safe, maintainable, and trustworthy Zig code with AI assistance. These standards are designed to be unambiguous—both for humans and AI systems.

### Core Principles

1. **Leverage the Type System** — Let the compiler catch errors at compile time
2. **Explicit Resource Management** — Every allocation has an owner and a cleanup path
3. **Fail Loudly** — Errors should be visible and handled, never silently ignored
4. **Comptime Over Runtime** — Prefer compile-time computation when possible
5. **Minimal Dependencies** — Standard library first, external dependencies sparingly

---

## Table of Contents

1. [Naming Conventions](#1-naming-conventions)
2. [Memory Management](#2-memory-management)
3. [Error Handling](#3-error-handling)
4. [API Design](#4-api-design)
5. [Code Organization](#5-code-organization)
6. [Documentation](#6-documentation)
7. [Testing](#7-testing)
8. [Concurrency](#8-concurrency)
9. [Comptime Programming](#9-comptime-programming)
10. [Build System](#10-build-system)
11. [Security and Safety](#11-security-and-safety)
12. [Logging](#12-logging)
13. [Performance](#13-performance)
14. [Quick Reference](#14-quick-reference)
15. [Zig 0.15 Migration Guide](#15-zig-015-migration-guide)

---

## 1. Naming Conventions

### 1.1 General Rules

Follow the [Zig Style Guide](https://ziglang.org/documentation/master/#Naming-Conventions):

| Element | Style | Example |
|---------|-------|---------|
| Types, structs, enums, unions | PascalCase | `HttpClient`, `ParseError` |
| Functions, methods | camelCase | `readFile`, `parseHeader` |
| Variables, fields | camelCase | `bufferSize`, `isReady` |
| Constants (comptime) | snake_case | `max_buffer_size` |
| Compile-time types | PascalCase | `ComptimeStringMap` |

### 1.2 Function Naming Patterns

```zig
// Initialization/deinitialization
pub fn init(allocator: Allocator) !Self { }
pub fn deinit(self: *Self) void { }

// Getters - no "get" prefix for simple accessors
pub fn name(self: Self) []const u8 { }
pub fn isReady(self: Self) bool { }
pub fn hasValue(self: Self) bool { }

// Setters - use "set" prefix
pub fn setName(self: *Self, name: []const u8) void { }

// Actions - verb first
pub fn connect(self: *Self) !void { }
pub fn readBytes(self: *Self, buffer: []u8) !usize { }
pub fn writeAll(self: *Self, data: []const u8) !void { }

// Conversions
pub fn toSlice(self: Self) []const u8 { }
pub fn fromBytes(bytes: []const u8) !Self { }

// Creation with allocator
pub fn create(allocator: Allocator) !*Self { }
pub fn destroy(self: *Self) void { }
```

### 1.3 Type Naming

```zig
// Structs - noun, describes what it is
const HttpClient = struct { };
const ParseResult = struct { };
const BufferWriter = struct { };

// Enums - noun, often singular
const Status = enum { pending, active, complete };
const FileMode = enum { read, write, readWrite };

// Error sets - suffix with "Error"
const ParseError = error{ InvalidSyntax, UnexpectedToken };
const IoError = error{ FileNotFound, PermissionDenied };

// Interfaces (comptime) - adjective or capability
fn Comparable(comptime T: type) type { }
fn Hashable(comptime T: type) type { }
```

### 1.4 File Naming

```
src/
├── http_client.zig      # snake_case for file names
├── json_parser.zig
├── string_utils.zig
└── main.zig
```

### 1.5 Naming Clarity

```zig
// BAD: Ambiguous
var data: []u8 = undefined;
var temp: i32 = 0;
var flag: bool = false;

// GOOD: Descriptive
var response_buffer: []u8 = undefined;
var retry_count: i32 = 0;
var is_connected: bool = false;
```

---

## 2. Memory Management

### 2.1 Allocator Rules

**M1: Always accept an allocator parameter for functions that allocate.**

```zig
// BAD: Uses global/default allocator
pub fn loadConfig() !Config {
    const file = try std.fs.cwd().readFileAlloc(
        std.heap.page_allocator,  // Hidden dependency
        "config.json",
        1024 * 1024,
    );
    // ...
}

// GOOD: Explicit allocator
pub fn loadConfig(allocator: Allocator, path: []const u8) !Config {
    const file = try std.fs.cwd().readFileAlloc(allocator, path, 1024 * 1024);
    defer allocator.free(file);
    // ...
}
```

**M2: Document ownership in function signatures.**

```zig
/// Caller owns the returned slice and must free it.
pub fn duplicate(allocator: Allocator, source: []const u8) ![]u8 {
    return allocator.dupe(u8, source);
}

/// Returns a slice into the internal buffer. Valid until next mutation.
pub fn view(self: Self) []const u8 {
    return self.buffer[0..self.len];
}
```

**M3: Use defer for cleanup immediately after acquisition.**

```zig
pub fn processFile(allocator: Allocator, path: []const u8) !void {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();  // Immediately after open

    const contents = try file.readToEndAlloc(allocator, max_size);
    defer allocator.free(contents);  // Immediately after alloc

    try process(contents);
}
```

**M4: Use errdefer for cleanup on error paths.**

```zig
pub fn init(allocator: Allocator) !Self {
    const buffer = try allocator.alloc(u8, buffer_size);
    errdefer allocator.free(buffer);  // Only on error

    const handle = try openHandle();
    errdefer closeHandle(handle);  // Only on error

    return Self{
        .buffer = buffer,
        .handle = handle,
        .allocator = allocator,
    };
}
```

### 2.2 Allocator Patterns

**Pattern: Arena for batch allocations**

```zig
pub fn processMany(allocator: Allocator, items: []const Item) !Result {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();  // Frees everything at once

    const arena_alloc = arena.allocator();

    for (items) |item| {
        const processed = try processItem(arena_alloc, item);
        // No need to free individually
    }
    // ...
}
```

**Pattern: Testing allocator for leak detection**

```zig
test "no memory leaks" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);

    const allocator = gpa.allocator();
    var obj = try MyStruct.init(allocator);
    defer obj.deinit();

    // Test operations...
}
```

### 2.3 Struct Lifecycle

```zig
const Connection = struct {
    allocator: Allocator,
    buffer: []u8,
    socket: Socket,

    /// Initialize a new connection. Caller must call deinit().
    pub fn init(allocator: Allocator, address: Address) !Connection {
        const buffer = try allocator.alloc(u8, 4096);
        errdefer allocator.free(buffer);

        const socket = try Socket.connect(address);
        errdefer socket.close();

        return Connection{
            .allocator = allocator,
            .buffer = buffer,
            .socket = socket,
        };
    }

    /// Release all resources. Safe to call multiple times.
    pub fn deinit(self: *Connection) void {
        self.socket.close();
        self.allocator.free(self.buffer);
        self.* = undefined;  // Poison the struct
    }
};
```

---

## 3. Error Handling

### 3.1 Error Set Design

**E1: Define specific error sets for each domain.**

```zig
// BAD: Catch-all error
pub fn parse(input: []const u8) !Ast {
    return error.Failed;  // What failed? Why?
}

// GOOD: Specific errors
pub const ParseError = error{
    InvalidSyntax,
    UnexpectedToken,
    UnterminatedString,
    NestingTooDeep,
    OutOfMemory,
};

pub fn parse(allocator: Allocator, input: []const u8) ParseError!Ast {
    // ...
}
```

**E2: Use anyerror only at API boundaries.**

```zig
// Internal functions: specific errors
fn parseHeader(data: []const u8) ParseError!Header { }

// Callback/generic interfaces: anyerror acceptable
pub fn forEach(self: Self, callback: fn(Item) anyerror!void) anyerror!void { }
```

### 3.2 Error Handling Patterns

**Pattern: try for propagation**

```zig
pub fn loadAndParse(allocator: Allocator, path: []const u8) !Document {
    const contents = try readFile(allocator, path);
    defer allocator.free(contents);

    return try parse(allocator, contents);
}
```

**Pattern: catch for handling**

```zig
pub fn connectWithRetry(address: Address, max_retries: u32) !Connection {
    var attempts: u32 = 0;
    while (attempts < max_retries) : (attempts += 1) {
        return Connection.init(address) catch |err| {
            if (err == error.ConnectionRefused) {
                std.time.sleep(1 * std.time.ns_per_s);
                continue;
            }
            return err;
        };
    }
    return error.MaxRetriesExceeded;
}
```

**Pattern: Error payloads via optional out parameter**

```zig
pub const ValidationError = struct {
    line: usize,
    column: usize,
    message: []const u8,
};

pub fn validate(
    input: []const u8,
    error_info: ?*ValidationError,
) error{ValidationFailed}!void {
    // On error, provide details
    if (error_info) |info| {
        info.* = .{
            .line = current_line,
            .column = current_col,
            .message = "unexpected character",
        };
    }
    return error.ValidationFailed;
}
```

### 3.3 Cleanup with errdefer

```zig
pub fn createWidget(allocator: Allocator) !*Widget {
    const widget = try allocator.create(Widget);
    errdefer allocator.destroy(widget);

    widget.buffer = try allocator.alloc(u8, 1024);
    errdefer allocator.free(widget.buffer);

    widget.handle = try acquireHandle();
    // No errdefer needed - if we get here, we succeed

    return widget;
}
```

### 3.4 Unreachable and Assert

```zig
// unreachable: Logically impossible states
fn getDigit(c: u8) u8 {
    return switch (c) {
        '0'...'9' => c - '0',
        else => unreachable,  // Caller guarantees valid input
    };
}

// assert: Invariant checks (disabled in ReleaseFast)
fn processPositive(value: i32) void {
    std.debug.assert(value > 0);
    // ...
}
```

---

## 4. API Design

### 4.1 Function Signatures

**A1: Accept slices, not arrays or pointers-to-many.**

```zig
// BAD: Requires exact array size
pub fn hash(data: [32]u8) u64 { }
pub fn hash(data: [*]const u8, len: usize) u64 { }

// GOOD: Flexible slice
pub fn hash(data: []const u8) u64 { }
```

**A2: Use optional types for nullable values.**

```zig
// BAD: Magic values
pub fn find(haystack: []const u8, needle: u8) isize {
    // Returns -1 if not found
}

// GOOD: Optional type
pub fn find(haystack: []const u8, needle: u8) ?usize {
    return std.mem.indexOfScalar(u8, haystack, needle);
}
```

**A3: Return structs for multiple values.**

```zig
// BAD: Out parameters
pub fn divide(a: i32, b: i32, quotient: *i32, remainder: *i32) void { }

// GOOD: Return struct
pub const DivResult = struct { quotient: i32, remainder: i32 };
pub fn divide(a: i32, b: i32) DivResult {
    return .{ .quotient = @divTrunc(a, b), .remainder = @rem(a, b) };
}
```

### 4.2 Configuration Structs

**Pattern: Struct with defaults**

```zig
pub const Config = struct {
    port: u16 = 8080,
    host: []const u8 = "localhost",
    timeout_ms: u32 = 30_000,
    max_connections: u32 = 100,
    tls_enabled: bool = false,
};

// Usage
const server = try Server.init(allocator, .{});  // All defaults
const server = try Server.init(allocator, .{ .port = 443, .tls_enabled = true });
```

### 4.3 Generic/Comptime APIs

**Pattern: Comptime type parameters**

```zig
pub fn ArrayList(comptime T: type) type {
    return struct {
        const Self = @This();

        items: []T,
        capacity: usize,
        allocator: Allocator,

        pub fn init(allocator: Allocator) Self { }
        pub fn append(self: *Self, item: T) !void { }
        pub fn toOwnedSlice(self: *Self) ![]T { }
    };
}
```

**Pattern: Writer/Reader interfaces (Zig 0.15+)**

```zig
// Zig 0.15+: Use concrete std.Io.Writer instead of anytype
pub fn writeJson(value: anytype, writer: std.Io.Writer) !void {
    try writer.writeAll("{");
    // ...
    // Don't forget to flush when needed!
}

// Usage with explicit buffering
var buffer: [4096]u8 = undefined;
const file = try std.fs.cwd().createFile("output.json", .{});
defer file.close();
var writer = file.writer(&buffer);
try writeJson(data, writer);
try writer.flush();
```

### 4.4 Builder Pattern

```zig
pub const RequestBuilder = struct {
    method: Method = .get,
    uri: []const u8 = "/",
    headers: HeaderMap,
    body: ?[]const u8 = null,

    pub fn init(allocator: Allocator) RequestBuilder {
        return .{ .headers = HeaderMap.init(allocator) };
    }

    pub fn setMethod(self: *RequestBuilder, method: Method) *RequestBuilder {
        self.method = method;
        return self;
    }

    pub fn setUri(self: *RequestBuilder, uri: []const u8) *RequestBuilder {
        self.uri = uri;
        return self;
    }

    pub fn addHeader(self: *RequestBuilder, name: []const u8, value: []const u8) !*RequestBuilder {
        try self.headers.put(name, value);
        return self;
    }

    pub fn build(self: RequestBuilder) Request {
        return Request.fromBuilder(self);
    }
};
```

---

## 5. Code Organization

### 5.1 File Structure

```zig
//! Module-level documentation describing this file's purpose.
//!
//! Example usage:
//! ```zig
//! const parser = Parser.init(allocator);
//! defer parser.deinit();
//! ```

const std = @import("std");
const Allocator = std.mem.Allocator;

// Local imports
const utils = @import("utils.zig");
const Config = @import("config.zig").Config;

// Public declarations
pub const Parser = struct {
    // ...
};

pub const ParseError = error{
    // ...
};

// Private declarations
const internal_buffer_size = 4096;

fn internalHelper() void {
    // ...
}

// Tests at bottom
test "Parser basic usage" {
    // ...
}
```

### 5.2 Import Organization

```zig
// 1. Standard library
const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

// 2. External dependencies (blank line separator)
const zap = @import("zap");

// 3. Local modules (blank line separator)
const config = @import("config.zig");
const utils = @import("utils.zig");
```

### 5.3 Project Layout

```
project/
├── build.zig           # Build configuration
├── build.zig.zon       # Package manifest
├── src/
│   ├── main.zig        # Entry point (executable)
│   ├── root.zig        # Library root (re-exports public API)
│   ├── core/           # Core functionality
│   │   ├── parser.zig
│   │   └── lexer.zig
│   └── utils/          # Utilities
│       └── string.zig
└── tests/              # Integration tests
    └── integration.zig
```

---

## 6. Documentation

### 6.1 Doc Comments

```zig
/// Parses the input string into an AST.
///
/// The returned AST is owned by the caller and must be freed by calling
/// `deinit()` when no longer needed.
///
/// ## Arguments
/// - `allocator`: Used for AST node allocation
/// - `source`: The source code to parse
///
/// ## Returns
/// A parsed AST, or an error if parsing fails.
///
/// ## Example
/// ```zig
/// var ast = try parser.parse(allocator, source);
/// defer ast.deinit();
/// ```
///
/// ## Errors
/// - `InvalidSyntax`: Source contains invalid syntax
/// - `OutOfMemory`: Allocation failed
pub fn parse(allocator: Allocator, source: []const u8) ParseError!Ast {
    // ...
}
```

### 6.2 Module Documentation

```zig
//! # JSON Parser
//!
//! A streaming JSON parser with low memory overhead.
//!
//! ## Features
//! - Zero-copy string parsing
//! - Streaming API for large documents
//! - Comprehensive error messages
//!
//! ## Example
//! ```zig
//! const json = @import("json.zig");
//! var parser = json.Parser.init(allocator);
//! defer parser.deinit();
//!
//! const value = try parser.parse(input);
//! ```
```

### 6.3 When to Document

- **Always**: Public functions, types, and fields
- **Always**: Complex algorithms or non-obvious logic
- **Always**: Safety requirements and invariants
- **Skip**: Obvious getter/setter pairs
- **Skip**: Self-explanatory one-liners

---

## 7. Testing

### 7.1 Test Organization

```zig
// Unit tests in the same file
const MyStruct = struct {
    // implementation
};

test "MyStruct basic operations" {
    var s = MyStruct.init(std.testing.allocator);
    defer s.deinit();

    try s.add(42);
    try std.testing.expectEqual(@as(usize, 1), s.count());
}

test "MyStruct handles empty input" {
    // ...
}
```

### 7.2 Test Naming

```zig
// Pattern: "subject behavior [condition]"
test "Parser parses empty object" { }
test "Parser returns error on invalid input" { }
test "Connection reconnects after timeout" { }
test "Buffer grows when capacity exceeded" { }
```

### 7.3 Testing Allocator

```zig
test "no memory leaks" {
    // std.testing.allocator checks for leaks
    // Zig 0.15+: Prefer ArrayListUnmanaged
    var list = std.ArrayListUnmanaged(u8){};
    defer list.deinit(std.testing.allocator);

    try list.appendSlice(std.testing.allocator, "hello");
    try std.testing.expectEqualStrings("hello", list.items);
}
```

### 7.4 Failing Allocator

```zig
test "handles allocation failure" {
    var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{
        .fail_index = 3,  // Fail on 4th allocation
    });

    const result = MyStruct.init(failing.allocator());
    try std.testing.expectError(error.OutOfMemory, result);
}
```

### 7.5 Table-Driven Tests

```zig
test "parse integers" {
    const cases = [_]struct {
        input: []const u8,
        expected: ?i64,
    }{
        .{ .input = "0", .expected = 0 },
        .{ .input = "42", .expected = 42 },
        .{ .input = "-1", .expected = -1 },
        .{ .input = "abc", .expected = null },
    };

    for (cases) |case| {
        const result = parseInt(case.input);
        if (case.expected) |expected| {
            try std.testing.expectEqual(expected, result.?);
        } else {
            try std.testing.expect(result == null);
        }
    }
}
```

---

## 8. Concurrency

### 8.1 Thread Safety Documentation

```zig
/// Thread-safe counter using atomic operations.
///
/// Safe to read and increment from multiple threads simultaneously.
/// For bulk operations, use `addMany()` for better performance.
pub const AtomicCounter = struct {
    value: std.atomic.Value(u64),

    pub fn init() AtomicCounter {
        return .{ .value = std.atomic.Value(u64).init(0) };
    }

    /// Thread-safe increment. Returns previous value.
    pub fn increment(self: *AtomicCounter) u64 {
        return self.value.fetchAdd(1, .seq_cst);
    }

    /// Thread-safe read.
    pub fn get(self: *AtomicCounter) u64 {
        return self.value.load(.seq_cst);
    }
};
```

### 8.2 Mutex Patterns

```zig
const SharedState = struct {
    mutex: std.Thread.Mutex,
    data: ArrayList(u8),

    pub fn init(allocator: Allocator) SharedState {
        return .{
            .mutex = .{},
            .data = ArrayList(u8).init(allocator),
        };
    }

    pub fn append(self: *SharedState, value: u8) !void {
        self.mutex.lock();
        defer self.mutex.unlock();

        try self.data.append(value);
    }
};
```

### 8.3 Thread Pool

```zig
pub fn processInParallel(items: []const Item) !void {
    var pool: std.Thread.Pool = undefined;
    try pool.init(.{ .allocator = allocator });
    defer pool.deinit();

    for (items) |item| {
        pool.spawn(processItem, .{item}) catch |err| {
            // Handle spawn failure
        };
    }
}
```

---

## 9. Comptime Programming

### 9.1 Comptime Constants

```zig
// Compile-time configuration
pub const max_connections = 1000;
pub const buffer_size = 4096;
pub const version = std.SemanticVersion{ .major = 1, .minor = 0, .patch = 0 };
```

### 9.2 Comptime Functions

```zig
/// Generates a lookup table at compile time.
fn generateLookupTable() [256]bool {
    var table: [256]bool = undefined;
    for (0..256) |i| {
        table[i] = isValidChar(@intCast(i));
    }
    return table;
}

const lookup_table = generateLookupTable();

pub fn isValid(c: u8) bool {
    return lookup_table[c];
}
```

### 9.3 Type Reflection

```zig
pub fn serialize(value: anytype, writer: anytype) !void {
    const T = @TypeOf(value);
    const info = @typeInfo(T);

    switch (info) {
        .Struct => |s| {
            try writer.writeAll("{");
            inline for (s.fields, 0..) |field, i| {
                if (i > 0) try writer.writeAll(",");
                try writer.print("\"{s}\":", .{field.name});
                try serialize(@field(value, field.name), writer);
            }
            try writer.writeAll("}");
        },
        .Int => try writer.print("{d}", .{value}),
        .Pointer => |p| if (p.size == .Slice and p.child == u8) {
            try writer.print("\"{s}\"", .{value});
        },
        else => @compileError("Unsupported type: " ++ @typeName(T)),
    }
}
```

### 9.4 Comptime Validation

```zig
pub fn RingBuffer(comptime T: type, comptime capacity: usize) type {
    if (capacity == 0) {
        @compileError("RingBuffer capacity must be greater than 0");
    }
    if (!std.math.isPowerOfTwo(capacity)) {
        @compileError("RingBuffer capacity must be a power of 2");
    }

    return struct {
        buffer: [capacity]T = undefined,
        read_idx: usize = 0,
        write_idx: usize = 0,

        // ...
    };
}
```

---

## 10. Build System

### 10.1 Standard build.zig (Zig 0.15+)

```zig
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Zig 0.15+: Use modules with root_module pattern
    const lib_mod = b.addModule("mylib", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Executable using root_module
    const exe = b.addExecutable(.{
        .name = "myapp",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "mylib", .module = lib_mod },
            },
        }),
    });
    b.installArtifact(exe);

    // Run command
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    const run_step = b.step("run", "Run the application");
    run_step.dependOn(&run_cmd.step);

    // Tests using root_module
    const lib_tests = b.addTest(.{
        .root_module = lib_mod,
    });
    const run_lib_tests = b.addRunArtifact(lib_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_tests.step);
}
```

### 10.2 Package Manifest (build.zig.zon)

```zig
.{
    .name = "myproject",
    .version = "0.1.0",
    .dependencies = .{
        .zap = .{
            .url = "https://github.com/zigzap/zap/archive/v0.0.1.tar.gz",
            .hash = "...",
        },
    },
    .paths = .{
        "build.zig",
        "build.zig.zon",
        "src",
    },
}
```

### 10.3 Build Options

```zig
pub fn build(b: *std.Build) void {
    // Custom options
    const enable_logging = b.option(bool, "log", "Enable debug logging") orelse false;
    const max_threads = b.option(u32, "threads", "Maximum thread count") orelse 4;

    const options = b.addOptions();
    options.addOption(bool, "enable_logging", enable_logging);
    options.addOption(u32, "max_threads", max_threads);

    const lib = b.addStaticLibrary(.{ ... });
    lib.root_module.addOptions("config", options);
}
```

---

## 11. Security and Safety

### 11.1 Safety Modes

```zig
// ReleaseSafe (default release): safety checks enabled
// ReleaseFast: safety checks disabled for performance
// ReleaseSmall: optimized for size, safety disabled
// Debug: all checks, no optimization
```

**Recommendation:** Use `ReleaseSafe` for production unless performance-critical.

### 11.2 Input Validation

**S1: Validate all external input at boundaries.**

```zig
pub fn parsePort(input: []const u8) !u16 {
    const value = std.fmt.parseInt(u16, input, 10) catch {
        return error.InvalidPort;
    };

    if (value == 0) {
        return error.PortZeroNotAllowed;
    }

    return value;
}
```

**S2: Limit buffer sizes and counts.**

```zig
pub const max_header_size = 8 * 1024;  // 8KB
pub const max_header_count = 100;

pub fn parseHeaders(allocator: Allocator, input: []const u8) !Headers {
    if (input.len > max_header_size) {
        return error.HeaderTooLarge;
    }
    // ...
}
```

### 11.3 Avoiding Undefined Behavior

```zig
// BAD: Potential overflow
fn addUnsafe(a: u32, b: u32) u32 {
    return a + b;  // Can overflow in ReleaseFast
}

// GOOD: Explicit overflow handling
fn addSafe(a: u32, b: u32) ?u32 {
    return std.math.add(u32, a, b) catch null;
}

// Or use saturating arithmetic
fn addSaturating(a: u32, b: u32) u32 {
    return a +| b;  // Saturates at max value
}
```

### 11.4 Pointer Safety

```zig
// BAD: Dangling pointer risk
fn getReference(list: *ArrayList(u8)) *u8 {
    return &list.items[0];  // Invalid after list mutation
}

// GOOD: Return by value or document lifetime
fn getFirst(list: *ArrayList(u8)) ?u8 {
    return if (list.items.len > 0) list.items[0] else null;
}
```

### 11.5 Sentinel Values

```zig
// Use sentinel-terminated slices for C interop
const c_string: [:0]const u8 = "hello";

// Convert slice to sentinel-terminated
fn toCString(allocator: Allocator, slice: []const u8) ![:0]u8 {
    return allocator.dupeZ(u8, slice);
}
```

---

## 12. Logging

### 12.1 Using std.log

```zig
const std = @import("std");
const log = std.log.scoped(.my_module);

pub fn processRequest(req: Request) !Response {
    log.debug("Processing request: {s}", .{req.path});

    const result = doWork() catch |err| {
        log.err("Failed to process: {}", .{err});
        return err;
    };

    log.info("Request completed successfully", .{});
    return result;
}
```

### 12.2 Log Levels

```zig
log.err(...)    // Error conditions
log.warn(...)   // Warning conditions
log.info(...)   // Informational messages
log.debug(...)  // Debug-level messages
```

### 12.3 Custom Log Function

```zig
pub const std_options = .{
    .log_level = .debug,
    .logFn = customLog,
};

fn customLog(
    comptime level: std.log.Level,
    comptime scope: @TypeOf(.EnumLiteral),
    comptime format: []const u8,
    args: anytype,
) void {
    const timestamp = std.time.timestamp();
    const prefix = "[{d}] [{s}] [{s}] ";
    std.debug.print(prefix ++ format ++ "\n", .{
        timestamp,
        @tagName(level),
        @tagName(scope),
    } ++ args);
}
```

---

## 13. Performance

### 13.1 Benchmarking

```zig
test "benchmark parsing" {
    const input = @embedFile("test_data.json");

    var timer = std.time.Timer.start() catch unreachable;

    const iterations = 1000;
    for (0..iterations) |_| {
        _ = try parse(std.testing.allocator, input);
    }

    const elapsed = timer.read();
    std.debug.print("Average: {d}ns\n", .{elapsed / iterations});
}
```

### 13.2 Optimization Guidelines

1. **Profile first** — Don't optimize without measurement
2. **Algorithms first** — Better algorithms beat micro-optimizations
3. **Cache locality** — Keep related data together
4. **Avoid allocations** — In hot paths, pre-allocate or use stack
5. **Use comptime** — Move work to compile time when possible

### 13.3 Memory Layout

```zig
// Optimize struct layout for size
const Compact = packed struct {
    flags: u8,
    id: u16,
    value: u32,
};

// Optimize for cache alignment
const Aligned = struct {
    data: [64]u8 align(64),  // Cache line aligned
};
```

---

## 14. Quick Reference

### Naming At-a-Glance

| What | Style | Example |
|------|-------|---------|
| Type | PascalCase | `HttpClient` |
| Function | camelCase | `parseHeader` |
| Variable | camelCase | `bufferSize` |
| Constant | snake_case | `max_size` |
| File | snake_case | `http_client.zig` |

### Common Patterns

```zig
// Init/deinit pair
var obj = try Struct.init(allocator);
defer obj.deinit();

// Error handling
const value = try fallibleFunction();
const value = fallibleFunction() catch |err| handleError(err);
const value = fallibleFunction() catch return;

// Optional handling
const value = optional orelse default;
const value = optional.?;  // Assert non-null
if (optional) |value| { }

// Cleanup
defer cleanup();           // Always runs
errdefer cleanup();        // Only on error

// Slices
const slice = array[start..end];
const full = array[0..];
for (slice) |item| { }
for (slice, 0..) |item, index| { }
```

### Common Errors

```zig
error.OutOfMemory         // Allocation failed
error.InvalidArgument     // Bad input
error.NotFound           // Resource not found
error.Timeout            // Operation timed out
error.ConnectionRefused  // Network error
```

### Build Commands

```bash
zig build              # Build project
zig build run          # Build and run
zig build test         # Run tests
zig fmt src/           # Format code
zig fmt --check src/   # Check formatting
```

---

## 15. Zig 0.15 Migration Guide

### 15.1 Breaking Changes

**Reader/Writer Overhaul**
- Old `std.io` readers/writers are deprecated
- New `std.Io.Reader` and `std.Io.Writer` are concrete (non-generic)
- Must use explicit buffering and flush output

```zig
// Old (deprecated)
const writer = file.writer();
try writer.print("hello", .{});

// New (Zig 0.15+)
var buffer: [4096]u8 = undefined;
var writer = file.writer(&buffer);
try writer.print("hello", .{});
try writer.flush();
```

**ArrayList Changes**
- `std.ArrayList` now requires allocator per method call
- `std.ArrayListUnmanaged` is the simpler default pattern

```zig
// Prefer unmanaged in 0.15+
var list = std.ArrayListUnmanaged(u8){};
defer list.deinit(allocator);
try list.append(allocator, value);
```

**Build System**
- `root_source_file` deprecated; use `root_module` pattern
- Use `b.addModule()` and `b.createModule()` for modules

### 15.2 Removed Features

| Feature | Alternative |
|---------|-------------|
| `usingnamespace` | Explicit imports |
| `async`/`await` | Use threads or callbacks |
| `@frameSize` | Removed |
| `BoundedArray` | Accept slices or use dynamic allocation |
| `std.fifo.LinearFifo` | Use `std.RingBuffer` alternatives |
| `std.RingBuffer` | Removed from stdlib |

### 15.3 New Restrictions

- **Arithmetic on undefined**: Using `undefined` in arithmetic operations now causes compile errors
- **Lossy int-to-float**: Compile error when integer literals lose precision converting to floats
- **Packed unions**: Cannot specify align attribute on packed union fields

### 15.4 New Features

- Debug compilation 5x faster with x86 backend
- `zig init --minimal` for stub templates
- `zig test-obj` compiles tests to object files
- `--watch` file system watching (macOS)
- `--webui` build interface with timing reports

---

## Appendix: Tool Integration

### Editor Setup

1. **VS Code**: Install "Zig Language" extension
2. **Neovim**: Use `nvim-lspconfig` with `zls`
3. **Other**: Configure ZLS (Zig Language Server)

### CI/CD

```yaml
# GitHub Actions example
- uses: goto-bus-stop/setup-zig@v2
- run: zig build test
- run: zig fmt --check src/
```

---

*CarbideZig Standards v2.0 — Hardened Zig 0.15+ for AI-Assisted Development*
