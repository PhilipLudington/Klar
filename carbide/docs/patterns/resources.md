# Resource Lifecycle Patterns

This document describes resource lifecycle patterns using defer and errdefer.

## Pattern 1: Basic defer for Cleanup

Place defer immediately after resource acquisition.

### Implementation

```zig
pub fn processFile(allocator: Allocator, path: []const u8) !void {
    // Acquire resource
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();  // Guaranteed cleanup

    // Acquire another resource
    const contents = try file.readToEndAlloc(allocator, max_size);
    defer allocator.free(contents);  // Guaranteed cleanup

    // Use resources - cleanup happens automatically on return
    try process(contents);
}
```

### Why Immediately After?
- Ensures cleanup is always paired with acquisition
- Prevents forgetting cleanup as code evolves
- Makes resource lifetime visually clear

---

## Pattern 2: errdefer for Error Paths

Use errdefer when cleanup should only happen on error.

### Implementation

```zig
pub fn createConnection(allocator: Allocator) !*Connection {
    // Allocate the connection struct
    const conn = try allocator.create(Connection);
    errdefer allocator.destroy(conn);  // Only if we fail below

    // Initialize socket
    conn.socket = try Socket.connect(address);
    errdefer conn.socket.close();  // Only if we fail below

    // Allocate buffer
    conn.buffer = try allocator.alloc(u8, 4096);
    // No errdefer needed - if we get here, we succeed

    conn.allocator = allocator;
    return conn;
}
```

### Key Insight
- `errdefer` runs only when function returns an error
- On success path, ownership transfers to caller
- Caller is responsible for eventual cleanup

---

## Pattern 3: errdefer Chain Ordering

errdefer blocks run in reverse order (LIFO).

### Implementation

```zig
pub fn init(allocator: Allocator) !Resources {
    const a = try allocateA();
    errdefer cleanupA(a);  // Runs 3rd on error

    const b = try allocateB();
    errdefer cleanupB(b);  // Runs 2nd on error

    const c = try allocateC();
    errdefer cleanupC(c);  // Runs 1st on error

    return Resources{ .a = a, .b = b, .c = c };
}

// If allocateC fails:
// 1. cleanupB(b) runs
// 2. cleanupA(a) runs
// (cleanupC never registered)
```

---

## Pattern 4: Scoped Resources

Use blocks to limit resource lifetime.

### Implementation

```zig
pub fn processItems(allocator: Allocator, items: []const Item) !void {
    for (items) |item| {
        // Each iteration has its own scope
        const temp = try allocator.alloc(u8, item.size);
        defer allocator.free(temp);

        try processItem(item, temp);
        // temp is freed here, before next iteration
    }
}

// Explicit block for limiting scope
pub fn example() !void {
    const result = blk: {
        var temp = try expensiveComputation();
        defer temp.deinit();

        break :blk temp.extractResult();
    };
    // temp is cleaned up, but result survives

    useResult(result);
}
```

---

## Pattern 5: Struct Lifecycle (init/deinit)

Standard pattern for struct resource management.

### Implementation

```zig
pub const Database = struct {
    allocator: Allocator,
    connection: Connection,
    cache: Cache,
    pool: ConnectionPool,

    /// Initialize database. Caller must call deinit().
    pub fn init(allocator: Allocator, config: Config) !Database {
        var connection = try Connection.open(config.url);
        errdefer connection.close();

        var cache = Cache.init(allocator);
        errdefer cache.deinit();

        var pool = try ConnectionPool.init(allocator, config.pool_size);
        // No errdefer - success path

        return Database{
            .allocator = allocator,
            .connection = connection,
            .cache = cache,
            .pool = pool,
        };
    }

    /// Release all resources. Safe to call multiple times.
    pub fn deinit(self: *Database) void {
        self.pool.deinit();
        self.cache.deinit();
        self.connection.close();
        self.* = undefined;  // Poison the struct
    }
};

// Usage
var db = try Database.init(allocator, config);
defer db.deinit();
```

---

## Pattern 6: Arena for Transaction-like Scopes

Use arena allocator for atomic cleanup of related allocations.

### Implementation

```zig
pub fn processTransaction(allocator: Allocator, tx: Transaction) !Result {
    // All transaction-related allocations use the arena
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();  // Frees everything at once

    const arena_alloc = arena.allocator();

    // Multiple allocations - no individual cleanup needed
    const parsed = try parse(arena_alloc, tx.data);
    const validated = try validate(arena_alloc, parsed);
    const transformed = try transform(arena_alloc, validated);

    // Only the final result uses the main allocator
    return try Result.fromTransformed(allocator, transformed);
}
```

---

## Pattern 7: Conditional Cleanup

Handle cleanup that depends on initialization state.

### Implementation

```zig
pub const OptionalResource = struct {
    required: Resource,
    optional: ?OptResource = null,

    pub fn init(allocator: Allocator, config: Config) !OptionalResource {
        var required = try Resource.init(allocator);
        errdefer required.deinit();

        var optional: ?OptResource = null;
        if (config.enable_optional) {
            optional = try OptResource.init(allocator);
            // errdefer for optional handled in deinit
        }

        return OptionalResource{
            .required = required,
            .optional = optional,
        };
    }

    pub fn deinit(self: *OptionalResource) void {
        if (self.optional) |*opt| {
            opt.deinit();
        }
        self.required.deinit();
        self.* = undefined;
    }
};
```

---

## Pattern 8: Resource Transfer

Explicit ownership transfer between scopes.

### Implementation

```zig
pub const Buffer = struct {
    allocator: Allocator,
    data: []u8,

    pub fn init(allocator: Allocator, size: usize) !Buffer {
        return Buffer{
            .allocator = allocator,
            .data = try allocator.alloc(u8, size),
        };
    }

    /// Transfer ownership of data to caller.
    /// Buffer becomes invalid after this call.
    pub fn toOwnedSlice(self: *Buffer) []u8 {
        const data = self.data;
        self.data = &.{};
        return data;
    }

    pub fn deinit(self: *Buffer) void {
        if (self.data.len > 0) {
            self.allocator.free(self.data);
        }
        self.* = undefined;
    }
};

// Usage
var buffer = try Buffer.init(allocator, 1024);
// ... fill buffer ...
const owned = buffer.toOwnedSlice();  // Caller now owns
defer allocator.free(owned);

buffer.deinit();  // Safe - no double free
```

---

## Anti-Patterns

### DON'T: Forget errdefer on Multi-step Init

```zig
// BAD: `a` leaks if second allocation fails
pub fn init(alloc: Allocator) !Self {
    const a = try alloc.alloc(u8, 100);
    const b = try alloc.alloc(u8, 100);  // If fails, `a` leaks
    return Self{ .a = a, .b = b };
}
```

### DON'T: defer in Loop without Scope

```zig
// BAD: All defers run at function end, not per iteration
pub fn process(items: []Item) void {
    for (items) |item| {
        const resource = acquire(item);
        defer release(resource);  // Won't run until function ends!
    }
}

// GOOD: Use block scope
pub fn process(items: []Item) void {
    for (items) |item| {
        {
            const resource = acquire(item);
            defer release(resource);  // Runs at block end
            use(resource);
        }
    }
}
```

### DON'T: Clean Up Transferred Resources

```zig
// BAD: Caller owns data, but we try to clean it
pub fn create(alloc: Allocator) ![]u8 {
    const data = try alloc.alloc(u8, 100);
    defer alloc.free(data);  // WRONG! Caller should own this
    return data;
}
```
