# Memory Management Patterns

This document describes memory management patterns for CarbideZig projects.

## Pattern 1: Allocator Injection

Every function that allocates memory should accept an `Allocator` parameter.

### Implementation

```zig
const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Buffer = struct {
    allocator: Allocator,
    data: []u8,

    pub fn init(allocator: Allocator, size: usize) !Buffer {
        const data = try allocator.alloc(u8, size);
        return Buffer{
            .allocator = allocator,
            .data = data,
        };
    }

    pub fn deinit(self: *Buffer) void {
        self.allocator.free(self.data);
        self.* = undefined;
    }
};
```

### Why?
- Enables testing with leak-detecting allocators
- Allows callers to control memory strategy
- Makes memory ownership explicit

---

## Pattern 2: Arena Allocator for Batch Operations

Use `ArenaAllocator` when many allocations share a lifetime.

### Implementation

```zig
pub fn processDocument(
    allocator: Allocator,
    source: []const u8,
) !Document {
    // Arena for temporary allocations during processing
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();  // Frees ALL arena allocations at once

    const temp_allocator = arena.allocator();

    // All temporary data uses the arena
    const tokens = try tokenize(temp_allocator, source);
    const ast = try parse(temp_allocator, tokens);
    const analyzed = try analyze(temp_allocator, ast);

    // Final result uses the main allocator (survives arena cleanup)
    return try Document.fromAnalyzed(allocator, analyzed);
}
```

### When to Use
- Parsing/processing with many intermediate allocations
- Request handling in servers
- Any scope where allocations have shared lifetime

---

## Pattern 3: GeneralPurposeAllocator for Debugging

Use GPA in main() for development to detect leaks.

### Implementation

```zig
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const status = gpa.deinit();
        if (status == .leak) {
            std.debug.print("Memory leak detected!\n", .{});
        }
    }

    const allocator = gpa.allocator();
    try run(allocator);
}
```

### Configuration Options

```zig
var gpa = std.heap.GeneralPurposeAllocator(.{
    .stack_trace_frames = 10,  // Capture stack traces
    .retain_metadata = true,   // Keep info for debugging
    .enable_memory_limit = true,
}){};

gpa.setRequestedMemoryLimit(1024 * 1024 * 100);  // 100MB limit
```

---

## Pattern 4: Fixed Buffer Allocator

For stack-based allocation with known maximum size.

### Implementation

```zig
pub fn formatMessage(name: []const u8) ![]const u8 {
    var buffer: [256]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator = fba.allocator();

    // Allocations come from the stack buffer
    return try std.fmt.allocPrint(allocator, "Hello, {s}!", .{name});
}
```

### When to Use
- Known maximum size
- Performance-critical paths
- Avoiding heap allocation

---

## Pattern 5: Struct Lifecycle (init/deinit)

Standard pattern for stack-allocated structs.

### Implementation

```zig
pub const Connection = struct {
    allocator: Allocator,
    socket: Socket,
    buffer: []u8,

    /// Creates a new connection. Caller must call deinit().
    pub fn init(allocator: Allocator, address: Address) !Connection {
        const buffer = try allocator.alloc(u8, 4096);
        errdefer allocator.free(buffer);

        const socket = try Socket.connect(address);
        errdefer socket.close();

        return Connection{
            .allocator = allocator,
            .socket = socket,
            .buffer = buffer,
        };
    }

    /// Releases all resources.
    pub fn deinit(self: *Connection) void {
        self.socket.close();
        self.allocator.free(self.buffer);
        self.* = undefined;  // Poison to catch use-after-deinit
    }
};

// Usage
var conn = try Connection.init(allocator, address);
defer conn.deinit();
```

---

## Pattern 6: Heap-Allocated Struct (create/destroy)

For structs that need to be heap-allocated.

### Implementation

```zig
pub const Node = struct {
    allocator: Allocator,
    value: i32,
    next: ?*Node,

    /// Creates a heap-allocated node. Caller must call destroy().
    pub fn create(allocator: Allocator, value: i32) !*Node {
        const node = try allocator.create(Node);
        node.* = .{
            .allocator = allocator,
            .value = value,
            .next = null,
        };
        return node;
    }

    /// Frees this node (not the chain).
    pub fn destroy(self: *Node) void {
        const allocator = self.allocator;
        self.* = undefined;
        allocator.destroy(self);
    }
};
```

---

## Pattern 7: Ownership Documentation

Always document who owns returned memory.

### Implementation

```zig
/// Duplicates the input string.
/// **Caller owns** the returned slice and must free it.
pub fn duplicate(allocator: Allocator, source: []const u8) ![]u8 {
    return allocator.dupe(u8, source);
}

/// Returns a view into the internal buffer.
/// **Valid until** next modification or deinit.
pub fn view(self: Self) []const u8 {
    return self.buffer[0..self.len];
}

/// Takes ownership of the input slice.
/// **Callee owns** the provided memory after this call.
pub fn takeOwnership(self: *Self, data: []u8) void {
    if (self.data) |old| {
        self.allocator.free(old);
    }
    self.data = data;
}
```

---

## Anti-Patterns

### DON'T: Hidden Allocations

```zig
// BAD: Where does the memory come from?
pub fn load() !Config {
    const data = try std.fs.cwd().readFileAlloc(
        std.heap.page_allocator,  // Hidden!
        "config.json",
        max_size,
    );
    // ...
}
```

### DON'T: Forget errdefer

```zig
// BAD: Leaks `a` if second allocation fails
pub fn init(allocator: Allocator) !Self {
    const a = try allocator.alloc(u8, 100);
    const b = try allocator.alloc(u8, 100);  // If this fails, `a` leaks!
    return Self{ .a = a, .b = b };
}

// GOOD
pub fn init(allocator: Allocator) !Self {
    const a = try allocator.alloc(u8, 100);
    errdefer allocator.free(a);

    const b = try allocator.alloc(u8, 100);
    return Self{ .a = a, .b = b };
}
```

---

## Allocator Selection Guide

Use this decision tree to choose the right allocator for your use case.

### Decision Tree

```
Is this for testing?
├── Yes → std.testing.allocator (leak detection)
└── No
    ├── Is this a one-time allocation at startup?
    │   └── Yes → std.heap.page_allocator (direct OS)
    └── No
        ├── Do all allocations share a lifetime?
        │   └── Yes → ArenaAllocator (batch free)
        └── No
            ├── Is max size known at compile time?
            │   └── Yes → FixedBufferAllocator (stack/no heap)
            └── No
                ├── Is this development/debugging?
                │   └── Yes → GeneralPurposeAllocator (leak reports)
                └── No (production)
                    └── Consider c_allocator or custom
```

### Allocator Comparison

| Allocator | Use Case | Pros | Cons |
|-----------|----------|------|------|
| `GeneralPurposeAllocator` | Development, debugging | Leak detection, safety checks | Overhead |
| `ArenaAllocator` | Request handling, batch ops | Fast alloc, single free | Can't free individual items |
| `FixedBufferAllocator` | Known max size, embedded | No heap, predictable | Fixed capacity |
| `page_allocator` | Large allocations, one-time | Direct OS, simple | Page-granular |
| `c_allocator` | C interop, production | Fast, standard | No safety checks |
| `std.testing.allocator` | Unit tests | Leak detection | Test only |

### Pattern: Layered Allocators

Combine allocators for different lifetime scopes:

```zig
pub fn main() !void {
    // Outer: GPA for leak detection
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    try runServer(allocator);
}

fn runServer(allocator: Allocator) !void {
    while (true) {
        // Per-request: Arena for fast cleanup
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();

        try handleRequest(arena.allocator());
    }
}

fn handleRequest(allocator: Allocator) !void {
    // All allocations freed when arena.deinit() called
    const headers = try parseHeaders(allocator);
    const body = try parseBody(allocator);
    try processRequest(allocator, headers, body);
}
```

### Pattern: Testing Allocator for Failure Paths

```zig
test "handles allocation failure" {
    // Configure to fail on 3rd allocation
    var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{
        .fail_index = 2,
    });

    const result = MyStruct.init(failing.allocator());
    try std.testing.expectError(error.OutOfMemory, result);
}
```

### Pattern: Memory-Limited Allocator

```zig
var gpa = std.heap.GeneralPurposeAllocator(.{
    .enable_memory_limit = true,
}){};
gpa.setRequestedMemoryLimit(50 * 1024 * 1024);  // 50MB limit

const allocator = gpa.allocator();
// Allocations beyond limit return error.OutOfMemory
```

### Pattern: Scratch Buffer for Temporary Work

```zig
fn processData(allocator: Allocator, input: []const u8) ![]u8 {
    // Stack buffer for small temporary allocations
    var scratch: [4096]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&scratch);

    // Use scratch for intermediate work
    const temp = try parse(fba.allocator(), input);

    // Final result uses passed-in allocator (survives function)
    return try transform(allocator, temp);
}
```
