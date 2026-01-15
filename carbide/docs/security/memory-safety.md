# Memory Safety in Zig

This document covers memory safety practices and patterns for CarbideZig projects.

## Overview

Zig provides memory safety through:

1. **Explicit allocators** - No hidden allocations
2. **Slices with length** - Bounds-checked access
3. **Optional types** - No null pointer exceptions
4. **defer/errdefer** - Deterministic cleanup
5. **Runtime checks** - Debug/ReleaseSafe mode protections

---

## Allocator Safety

### Always Use Allocator Parameters

```zig
// BAD: Hidden allocation with global allocator
pub fn createBuffer() ![]u8 {
    return std.heap.page_allocator.alloc(u8, 1024);
}

// GOOD: Explicit allocator
pub fn createBuffer(allocator: Allocator) ![]u8 {
    return allocator.alloc(u8, 1024);
}
```

### Leak Detection with GeneralPurposeAllocator

```zig
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .stack_trace_frames = 10,  // Capture allocation stack traces
    }){};
    defer {
        const status = gpa.deinit();
        if (status == .leak) {
            @panic("Memory leak detected");
        }
    }

    const allocator = gpa.allocator();
    try runApp(allocator);
}
```

### Testing Allocator for Tests

```zig
test "no memory leaks" {
    // Automatically fails test if allocations aren't freed
    var list = std.ArrayList(u8).init(std.testing.allocator);
    defer list.deinit();

    try list.append(42);
}
```

---

## Buffer Safety

### Slice-Based APIs

```zig
// BAD: Separate pointer and length
fn process(data: [*]u8, len: usize) void {
    // Easy to pass wrong length
}

// GOOD: Slice carries length
fn process(data: []u8) void {
    for (data) |*byte| {  // Bounds-checked
        byte.* = 0;
    }
}
```

### Bounds-Checked Access

```zig
var array = [_]u8{ 1, 2, 3, 4, 5 };
const idx = getUserIndex();

// Runtime bounds check (panics if out of bounds in safe modes)
const value = array[idx];

// Explicit check for graceful handling
if (idx < array.len) {
    const value = array[idx];
    process(value);
} else {
    return error.IndexOutOfBounds;
}
```

### Safe String Operations

```zig
// Use std.mem for string operations
const std = @import("std");

// Safe comparison
if (std.mem.eql(u8, str1, str2)) {
    // Equal
}

// Safe search
if (std.mem.indexOf(u8, haystack, needle)) |idx| {
    // Found at idx
}

// Safe copy
@memcpy(dest[0..src.len], src);

// Safe concatenation
var buffer: [256]u8 = undefined;
const result = try std.fmt.bufPrint(&buffer, "{s}{s}", .{a, b});
```

---

## Pointer Safety

### Optional Types for Nullable Pointers

```zig
// BAD: Nullable pointer without protection
var ptr: *Node = undefined;  // Dangerous!

// GOOD: Optional type
var ptr: ?*Node = null;

// Safe access
if (ptr) |node| {
    // node is guaranteed non-null
    process(node);
}

// Or with default
const node = ptr orelse return error.NullPointer;
```

### Pointer Lifetime Management

```zig
// BAD: Returning pointer to stack variable
fn bad() *u32 {
    var x: u32 = 42;
    return &x;  // Compile error! Zig catches this.
}

// GOOD: Allocate on heap if pointer must outlive function
fn good(allocator: Allocator) !*u32 {
    const ptr = try allocator.create(u32);
    ptr.* = 42;
    return ptr;  // Caller owns and must free
}
```

### Slice Lifetime Documentation

```zig
/// Returns a slice into the internal buffer.
/// **Valid until**: next call to append() or deinit().
pub fn items(self: Self) []const Item {
    return self.buffer[0..self.len];
}

/// Returns a newly allocated copy.
/// **Caller owns**: the returned slice and must free it.
pub fn toOwnedSlice(self: *Self) ![]Item {
    const slice = try self.allocator.dupe(Item, self.items());
    self.clearRetainingCapacity();
    return slice;
}
```

---

## Resource Cleanup

### defer for Unconditional Cleanup

```zig
pub fn processFile(path: []const u8) !void {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();  // Always runs

    const data = try file.readToEndAlloc(allocator, max_size);
    defer allocator.free(data);  // Always runs

    try process(data);
}
```

### errdefer for Error-Path Cleanup

```zig
pub fn init(allocator: Allocator) !Self {
    const a = try allocator.alloc(u8, 100);
    errdefer allocator.free(a);  // Only on error

    const b = try allocator.alloc(u8, 100);
    errdefer allocator.free(b);  // Only on error

    return Self{ .a = a, .b = b, .allocator = allocator };
}
```

### Poisoning After Free

```zig
pub fn deinit(self: *Self) void {
    self.allocator.free(self.buffer);
    self.* = undefined;  // Poison - catches use-after-free in debug
}
```

---

## Common Vulnerabilities and Prevention

### Use-After-Free

```zig
// BAD
var list = ArrayList(u8).init(allocator);
const ptr = &list.items[0];
try list.append(42);  // May reallocate!
_ = ptr.*;  // Dangling pointer!

// GOOD: Re-acquire pointer after mutation
var list = ArrayList(u8).init(allocator);
try list.append(42);
const ptr = &list.items[0];  // Valid until next mutation
```

### Double-Free

```zig
// BAD
allocator.free(buffer);
// ... later ...
allocator.free(buffer);  // Double free!

// GOOD: Set to undefined after free
allocator.free(buffer);
buffer = undefined;  // or use optional: buffer = null;
```

### Buffer Over-read

```zig
// BAD: Reading past valid data
fn process(data: []u8, valid_len: usize) void {
    for (data) |byte| {  // May read garbage after valid_len
        // ...
    }
}

// GOOD: Use correct slice
fn process(data: []u8, valid_len: usize) void {
    for (data[0..valid_len]) |byte| {
        // ...
    }
}
```

---

## Sensitive Data Handling

### Zero Before Free

```zig
const std = @import("std");

pub fn freeSecret(allocator: Allocator, secret: []u8) void {
    // Zero the memory before freeing
    std.crypto.utils.secureZero(u8, secret);
    allocator.free(secret);
}
```

### Avoid Sensitive Data in Logs

```zig
const log = std.log.scoped(.auth);

pub fn authenticate(username: []const u8, password: []const u8) !void {
    log.debug("Auth attempt: user={s}", .{username});
    // NEVER log password!

    // ...
}
```

---

## Testing for Memory Safety

### Failing Allocator

```zig
test "handles allocation failure" {
    var failing = std.testing.FailingAllocator.init(
        std.testing.allocator,
        .{ .fail_index = 2 },  // Fail on 3rd allocation
    );

    const result = MyStruct.init(failing.allocator());
    try std.testing.expectError(error.OutOfMemory, result);
}
```

### Fuzz Testing

```zig
test "fuzz parser" {
    // std.testing provides fuzzing infrastructure
    const input = std.testing.random_bytes(1000);
    _ = Parser.parse(input) catch {};
    // Should not crash regardless of input
}
```

---

## Checklist

- [ ] All allocating functions accept `Allocator` parameter
- [ ] All allocations have corresponding `defer`/`errdefer` cleanup
- [ ] All public APIs use slices instead of pointer+length
- [ ] All nullable values use optional types
- [ ] Sensitive data is zeroed before freeing
- [ ] Tests use `std.testing.allocator` for leak detection
- [ ] Pointer lifetimes are documented
- [ ] No use of `undefined` without immediate initialization
