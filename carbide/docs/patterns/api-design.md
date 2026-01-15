# API Design Patterns

This document describes API design patterns for CarbideZig projects.

## Pattern 1: Configuration Structs with Defaults

Use structs with default values for optional parameters.

### Implementation

```zig
pub const ServerConfig = struct {
    port: u16 = 8080,
    host: []const u8 = "0.0.0.0",
    max_connections: u32 = 1000,
    read_timeout_ms: u32 = 30_000,
    write_timeout_ms: u32 = 30_000,
    tls: ?TlsConfig = null,

    pub const TlsConfig = struct {
        cert_path: []const u8,
        key_path: []const u8,
        ca_path: ?[]const u8 = null,
    };
};

pub const Server = struct {
    pub fn init(allocator: Allocator, config: ServerConfig) !Server {
        // ...
    }
};

// Usage - all defaults
const server1 = try Server.init(allocator, .{});

// Usage - override some
const server2 = try Server.init(allocator, .{
    .port = 443,
    .tls = .{
        .cert_path = "/etc/ssl/cert.pem",
        .key_path = "/etc/ssl/key.pem",
    },
});
```

### Why?
- Clean call sites with only relevant options
- New options can be added without breaking existing code
- Self-documenting defaults

---

## Pattern 2: Writer/Reader Interfaces

Accept `anytype` for I/O flexibility.

### Implementation

```zig
pub fn serialize(value: anytype, writer: anytype) !void {
    const T = @TypeOf(value);

    switch (@typeInfo(T)) {
        .Int => try writer.print("{d}", .{value}),
        .Float => try writer.print("{d:.6}", .{value}),
        .Bool => try writer.writeAll(if (value) "true" else "false"),
        .Pointer => |ptr| {
            if (ptr.size == .Slice and ptr.child == u8) {
                try writer.print("\"{s}\"", .{value});
            }
        },
        .Struct => {
            try writer.writeAll("{");
            // ... serialize fields
            try writer.writeAll("}");
        },
        else => @compileError("Unsupported type"),
    }
}

// Usage with different writers
var list = std.ArrayList(u8).init(allocator);
try serialize(data, list.writer());

const stdout = std.io.getStdOut().writer();
try serialize(data, stdout);
```

---

## Pattern 3: Builder Pattern

For complex object construction with validation.

### Implementation

```zig
pub const HttpRequest = struct {
    method: Method,
    uri: []const u8,
    headers: std.StringHashMap([]const u8),
    body: ?[]const u8,

    pub const Builder = struct {
        allocator: Allocator,
        method: Method = .GET,
        uri: []const u8 = "/",
        headers: std.StringHashMap([]const u8),
        body: ?[]const u8 = null,

        pub fn init(allocator: Allocator) Builder {
            return .{
                .allocator = allocator,
                .headers = std.StringHashMap([]const u8).init(allocator),
            };
        }

        pub fn setMethod(self: *Builder, method: Method) *Builder {
            self.method = method;
            return self;
        }

        pub fn setUri(self: *Builder, uri: []const u8) *Builder {
            self.uri = uri;
            return self;
        }

        pub fn addHeader(self: *Builder, name: []const u8, value: []const u8) !*Builder {
            try self.headers.put(name, value);
            return self;
        }

        pub fn setBody(self: *Builder, body: []const u8) *Builder {
            self.body = body;
            return self;
        }

        pub fn build(self: Builder) HttpRequest {
            return .{
                .method = self.method,
                .uri = self.uri,
                .headers = self.headers,
                .body = self.body,
            };
        }
    };
};

// Usage
var builder = HttpRequest.Builder.init(allocator);
const request = (try builder
    .setMethod(.POST)
    .setUri("/api/users")
    .addHeader("Content-Type", "application/json"))
    .setBody("{\"name\": \"Alice\"}")
    .build();
```

---

## Pattern 4: Generic Containers

Use comptime type parameters for reusable containers.

### Implementation

```zig
pub fn RingBuffer(comptime T: type, comptime capacity: usize) type {
    if (capacity == 0) {
        @compileError("RingBuffer capacity must be > 0");
    }

    return struct {
        const Self = @This();

        buffer: [capacity]T = undefined,
        read_pos: usize = 0,
        write_pos: usize = 0,
        count: usize = 0,

        pub fn push(self: *Self, item: T) !void {
            if (self.count >= capacity) {
                return error.BufferFull;
            }
            self.buffer[self.write_pos] = item;
            self.write_pos = (self.write_pos + 1) % capacity;
            self.count += 1;
        }

        pub fn pop(self: *Self) ?T {
            if (self.count == 0) return null;
            const item = self.buffer[self.read_pos];
            self.read_pos = (self.read_pos + 1) % capacity;
            self.count -= 1;
            return item;
        }

        pub fn len(self: Self) usize {
            return self.count;
        }
    };
}

// Usage
var ring = RingBuffer(u32, 16){};
try ring.push(42);
const value = ring.pop();
```

---

## Pattern 5: Optional Return vs Error

Choose the right return type based on semantics.

### Implementation

```zig
// Optional: Absence is normal, not exceptional
pub fn find(self: *Self, key: []const u8) ?*Value {
    return self.map.get(key);
}

// Error union: Absence is exceptional
pub fn mustFind(self: *Self, key: []const u8) !*Value {
    return self.map.get(key) orelse error.KeyNotFound;
}

// Combined: Operation can fail, result may be absent
pub fn fetchFromCache(self: *Self, key: []const u8) !?Value {
    if (!self.connected) return error.NotConnected;
    return self.cache.get(key);  // null if not in cache
}
```

---

## Pattern 6: Slice-Based APIs

Accept slices for maximum flexibility.

### Implementation

```zig
// GOOD: Accepts any contiguous sequence
pub fn hash(data: []const u8) u64 {
    var h: u64 = 0;
    for (data) |byte| {
        h = h *% 31 +% byte;
    }
    return h;
}

// Works with:
const arr: [4]u8 = .{ 1, 2, 3, 4 };
_ = hash(&arr);           // Array pointer
_ = hash(arr[0..]);       // Slice of array
_ = hash("hello");        // String literal
_ = hash(dynamic_slice);  // Dynamic slice
```

---

## Pattern 7: Method Receivers

Choose receiver type based on mutation needs.

### Implementation

```zig
pub const Counter = struct {
    value: u64,

    // Read-only: use `Self` (value receiver)
    pub fn get(self: Self) u64 {
        return self.value;
    }

    // Mutating: use `*Self` (pointer receiver)
    pub fn increment(self: *Self) void {
        self.value += 1;
    }

    // Const pointer for large structs that aren't mutated
    pub fn isZero(self: *const Self) bool {
        return self.value == 0;
    }
};
```

---

## Pattern 8: Callback Interfaces

Use function pointers or `anytype` for callbacks.

### Implementation

```zig
// Function pointer approach
pub const Comparator = *const fn (a: *const Item, b: *const Item) std.math.Order;

pub fn sort(items: []Item, compare: Comparator) void {
    std.mem.sort(Item, items, {}, struct {
        fn lessThan(_: void, a: Item, b: Item) bool {
            return compare(&a, &b) == .lt;
        }
    }.lessThan);
}

// anytype approach (more flexible, but less type-safe)
pub fn forEach(self: *Self, callback: anytype) !void {
    for (self.items) |item| {
        try callback(item);
    }
}
```

---

## Anti-Patterns

### DON'T: Pointer + Length Pairs

```zig
// BAD
pub fn process(data: [*]const u8, len: usize) void { }

// GOOD
pub fn process(data: []const u8) void { }
```

### DON'T: Magic Return Values

```zig
// BAD
pub fn find(key: []const u8) isize {
    // Returns -1 if not found
}

// GOOD
pub fn find(key: []const u8) ?usize {
    // Returns null if not found
}
```

### DON'T: Out Parameters for Results

```zig
// BAD
pub fn divide(a: i32, b: i32, result: *i32, remainder: *i32) bool { }

// GOOD
pub const DivResult = struct { quotient: i32, remainder: i32 };
pub fn divide(a: i32, b: i32) ?DivResult { }
```
