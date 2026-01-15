---
globs: ["*.zig"]
---

# Concurrency Rules

## C1: Document Thread Safety
- Explicitly state thread safety in doc comments
- Default assumption: NOT thread-safe

```zig
/// Thread-safe counter using atomic operations.
/// Safe to read and increment from multiple threads.
pub const AtomicCounter = struct {
    // ...
};

/// NOT thread-safe. Each thread should have its own instance.
pub const Parser = struct {
    // ...
};
```

## C2: Mutex Usage
- Lock scope should be as small as possible
- ALWAYS use `defer` for unlock

```zig
pub fn append(self: *Self, value: u8) !void {
    self.mutex.lock();
    defer self.mutex.unlock();

    try self.data.append(value);
}
```

## C3: Atomic Operations
- Use `std.atomic.Value` for lock-free primitives
- Choose appropriate memory ordering

```zig
const Counter = struct {
    value: std.atomic.Value(u64),

    pub fn increment(self: *Counter) u64 {
        return self.value.fetchAdd(1, .seq_cst);
    }
};
```

## C4: Avoid Data Races
- Never share mutable state without synchronization
- Prefer message passing over shared memory
- Use `std.Thread.Pool` for work distribution

## C5: Thread-Local Storage
- Use `threadlocal` for per-thread state
- Useful for error contexts, caches

```zig
threadlocal var thread_id: ?u64 = null;
```

## C6: Async/Await (if using async)
- Prefer `nosuspend` when suspension is impossible
- Use `@Frame` for manual frame management
- Be aware of stack usage with deep async chains
