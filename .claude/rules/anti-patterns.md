---
globs: ["*.zig"]
---

# Anti-Patterns to Avoid

Patterns that lead to bugs, security issues, or maintenance problems.

## AP1: Unsafe Pointer Casts

**DON'T**: Use `@ptrCast` or `@intToPtr` without validation.

```zig
// BAD - No alignment check
const ptr: *u32 = @ptrCast(byte_ptr);

// BAD - Arbitrary integer to pointer
const ptr: *Node = @ptrFromInt(address);

// GOOD - Validate alignment
const ptr: *u32 = @ptrCast(@alignCast(byte_ptr));

// GOOD - Validate address came from valid pointer
if (known_addresses.contains(address)) {
    const ptr: *Node = @ptrFromInt(address);
}
```

## AP2: Ignoring Errors Carelessly

**DON'T**: Use `catch unreachable` without certainty.

```zig
// BAD - Will panic on any error
const file = std.fs.cwd().openFile(path, .{}) catch unreachable;

// BAD - Silent failure with no indication
mayFail() catch {};

// GOOD - Handle the error
const file = std.fs.cwd().openFile(path, .{}) catch |err| {
    log.err("Failed to open {s}: {}", .{path, err});
    return err;
};

// GOOD - Explicit about acceptable failure
socket.write(data) catch {};  // Best-effort notification, failure OK
```

## AP3: Using std.mem.zeroes for Pointer Types

**DON'T**: Zero-initialize structs containing pointers.

```zig
// BAD - Creates null/dangling pointers
const state = std.mem.zeroes(State);  // State has pointer fields!

// GOOD - Explicit initialization
const state = State{
    .allocator = allocator,
    .buffer = &.{},
    .callback = null,  // Explicit null is fine for optionals
};
```

## AP4: Storing Pointers to Stack Variables

**DON'T**: Return or store pointers to local variables.

```zig
// BAD - Returns dangling pointer
fn getBuffer() *[256]u8 {
    var buffer: [256]u8 = undefined;
    return &buffer;  // Stack frame gone after return!
}

// GOOD - Use heap allocation
fn getBuffer(allocator: Allocator) ![]u8 {
    return allocator.alloc(u8, 256);
}

// GOOD - Pass buffer in
fn fillBuffer(buffer: *[256]u8) void {
    // Fill caller-provided buffer
}
```

## AP5: Global/Static Allocators

**DON'T**: Use global allocators hidden in functions.

```zig
// BAD - Hidden allocation strategy
pub fn load() !Config {
    const data = try std.fs.cwd().readFileAlloc(
        std.heap.page_allocator,  // Who frees this? Which allocator?
        "config.json",
        max_size,
    );
    // ...
}

// GOOD - Explicit allocator injection
pub fn load(allocator: Allocator) !Config {
    const data = try std.fs.cwd().readFileAlloc(
        allocator,
        "config.json",
        max_size,
    );
    defer allocator.free(data);
    // ...
}
```

## AP6: Forgetting errdefer in Multi-Allocation

**DON'T**: Allocate multiple resources without errdefer.

```zig
// BAD - Leaks 'a' if 'b' allocation fails
pub fn init(allocator: Allocator) !Self {
    const a = try allocator.alloc(u8, 100);
    const b = try allocator.alloc(u8, 100);  // If fails, 'a' leaks!
    return Self{ .a = a, .b = b };
}

// GOOD - Each allocation has cleanup
pub fn init(allocator: Allocator) !Self {
    const a = try allocator.alloc(u8, 100);
    errdefer allocator.free(a);

    const b = try allocator.alloc(u8, 100);
    errdefer allocator.free(b);

    const c = try allocator.alloc(u8, 100);
    // No errdefer needed for last allocation

    return Self{ .a = a, .b = b, .c = c };
}
```

## AP7: Integer Overflow in Size Calculations

**DON'T**: Multiply sizes without overflow checks.

```zig
// BAD - Can overflow
const size = count * element_size;
const buffer = try allocator.alloc(u8, size);

// GOOD - Checked arithmetic
const size = std.math.mul(usize, count, element_size) catch return error.Overflow;
const buffer = try allocator.alloc(u8, size);
```

## AP8: Unchecked Slice Indexing

**DON'T**: Index slices without bounds validation.

```zig
// BAD - Can panic on out-of-bounds
fn getItem(items: []const Item, index: usize) Item {
    return items[index];
}

// GOOD - Return optional or error
fn getItem(items: []const Item, index: usize) ?Item {
    if (index >= items.len) return null;
    return items[index];
}

// GOOD - Let caller handle bounds
fn getItem(items: []const Item, index: usize) error{IndexOutOfBounds}!Item {
    if (index >= items.len) return error.IndexOutOfBounds;
    return items[index];
}
```

## AP9: Magic Numbers

**DON'T**: Use unexplained numeric literals.

```zig
// BAD - What do these numbers mean?
if (response[0] == 0x1F and response[1] == 0x8B) {
    try decompress(response[10..]);
}

// GOOD - Named constants with context
const gzip_magic = [_]u8{ 0x1F, 0x8B };
const gzip_header_size = 10;

if (std.mem.startsWith(u8, response, &gzip_magic)) {
    try decompress(response[gzip_header_size..]);
}
```

## AP10: Mixing Allocation Strategies

**DON'T**: Free with different allocator than allocated.

```zig
// BAD - Allocated with arena, freed with GPA
const data = try arena.allocator().alloc(u8, 100);
// ... later ...
gpa.allocator().free(data);  // Wrong allocator!

// GOOD - Store allocator with data
const Self = struct {
    allocator: Allocator,
    data: []u8,

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.data);
    }
};
```

## AP11: Sentinel Confusion

**DON'T**: Mix sentinel-terminated and regular slices carelessly.

```zig
// BAD - Loses null terminator
fn process(str: []const u8) void {
    // str is NOT null-terminated here
}
const c_str: [*:0]const u8 = "hello";
process(std.mem.span(c_str));  // Passed to C API expecting null terminator - UB!

// GOOD - Preserve sentinel when needed
fn processForC(str: [:0]const u8) void {
    c_api(str.ptr);  // Sentinel preserved
}
```

## AP12: Undefined for Arithmetic (Zig 0.15+)

**DON'T**: Use undefined values in arithmetic operations.

```zig
// BAD (Zig 0.15+ compile error)
var buffer: [100]u8 = undefined;
buffer[0] += 1;  // Error: arithmetic on undefined

// GOOD - Initialize first
var buffer: [100]u8 = std.mem.zeroes([100]u8);
buffer[0] += 1;

// GOOD - Or use for read-then-write
var buffer: [100]u8 = undefined;
buffer[0] = 42;  // Write first, no arithmetic on undefined
```

## AP13: Capturing Loop Variables

**DON'T**: Capture loop variable addresses in closures.

```zig
// BAD - All callbacks point to same memory
var callbacks: [10]*const fn() void = undefined;
for (items, 0..) |*item, i| {
    callbacks[i] = &struct {
        fn call() void {
            process(item);  // 'item' address is reused!
        }
    }.call;
}

// GOOD - Copy value or use different approach
// Consider restructuring to avoid this pattern
```

## AP14: Assuming Packed Struct Layout

**DON'T**: Assume struct field ordering without `packed`.

```zig
// BAD - Field order not guaranteed
const Header = struct {
    version: u8,
    flags: u8,
    length: u16,
};
const bytes = @as(*const [4]u8, @ptrCast(&header));  // May not match!

// GOOD - Use packed for binary layouts
const Header = packed struct {
    version: u8,
    flags: u8,
    length: u16,
};
```

## AP15: Test-Only Code in Production

**DON'T**: Leave test helpers accessible in release builds.

```zig
// BAD - Test helper in public API
pub fn _testReset(self: *Self) void {
    self.* = undefined;
}

// GOOD - Conditional compilation
pub usingnamespace if (@import("builtin").is_test) struct {
    pub fn _testReset(self: *Self) void {
        self.* = undefined;
    }
} else struct {};

// Note: usingnamespace removed in 0.15+, use this instead:
pub const testing = if (@import("builtin").is_test) struct {
    pub fn reset(self: *Self) void {
        self.* = undefined;
    }
} else struct {};
```

## Quick Reference: Anti-Pattern Detection

| Code Pattern | Risk | Fix |
|--------------|------|-----|
| `@ptrCast(x)` without `@alignCast` | Misalignment | Add `@alignCast` |
| `catch unreachable` | Runtime panic | Proper error handling |
| `std.mem.zeroes(T)` with pointers | Null deref | Explicit init |
| `return &local_var` | Dangling pointer | Heap allocate |
| Hidden `page_allocator` | Memory leaks | Inject allocator |
| Missing `errdefer` | Resource leaks | Add errdefer chain |
| `count * size` | Integer overflow | Use `std.math.mul` |
| `array[i]` without check | Panic | Bounds check first |
| Magic numbers | Unmaintainable | Named constants |
| Sentinel loss | C interop bugs | Use `[:0]const u8` |
