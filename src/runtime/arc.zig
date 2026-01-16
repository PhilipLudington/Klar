// ============================================================================
// Klar Atomic Reference Counting Runtime
// ============================================================================
//
// Implements Arc[T] (atomic reference-counted) and WeakArc[T] (weak reference) types.
// Thread-safe version of Rc using atomic operations for reference count updates.
//
// Memory layout (same as Rc):
// ┌─────────────────────────────────┐
// │ strong_count: usize (atomic)    │  (number of Arc handles)
// │ weak_count: usize (atomic)      │  (number of WeakArc handles + 1 if strong > 0)
// │ value: [value_size]u8           │  (the owned value, inline)
// └─────────────────────────────────┘
//
// The Arc handle is just a pointer to this structure.
// Strong count of 0 means the value is deallocated (but header may remain for weak refs).
// Weak count of 0 (after strong becomes 0) means the entire allocation is freed.
//
// Memory Ordering:
// - Clone (increment): .monotonic is sufficient (relaxed ordering)
// - Drop (decrement): .release for the decrement, .acquire fence before destruction
// - Upgrade (weak to strong): .acquire/.release for compare-and-swap
// This follows the standard ARC implementation pattern (similar to Rust's Arc).

const std = @import("std");
const alloc = @import("alloc.zig");

/// Destructor function type - called when the value is dropped.
/// Takes a pointer to the value (after the header).
pub const DestructorFn = *const fn (*anyopaque) callconv(.C) void;

/// Header for atomic reference-counted allocations.
/// This is placed before the actual value in memory.
/// Uses atomic integers for thread-safe reference counting.
pub const ArcHeader = extern struct {
    strong_count: std.atomic.Value(usize),
    weak_count: std.atomic.Value(usize),

    /// Get pointer to the value (immediately after header)
    pub fn getValue(self: *ArcHeader) *anyopaque {
        const header_ptr: [*]u8 = @ptrCast(self);
        return @ptrCast(header_ptr + @sizeOf(ArcHeader));
    }

    /// Get header from value pointer
    pub fn fromValue(value: *anyopaque) *ArcHeader {
        const value_ptr: [*]u8 = @ptrCast(value);
        return @ptrCast(@alignCast(value_ptr - @sizeOf(ArcHeader)));
    }
};

/// Allocate a new Arc with the given value size.
/// Returns a pointer to the value (NOT the header).
/// The value is uninitialized - caller must initialize it.
/// Initial strong_count = 1, weak_count = 1 (implicit weak for strong refs).
pub export fn klar_arc_alloc(value_size: usize, value_align: usize) ?*anyopaque {
    const header_size = @sizeOf(ArcHeader);
    const total_size = header_size + value_size;

    // Use max of header alignment and value alignment
    const align_log2: u8 = @intCast(std.math.log2_int(usize, @max(@alignOf(ArcHeader), value_align)));
    const ptr = alloc.klar_alloc(total_size, align_log2);

    if (ptr) |p| {
        const header: *ArcHeader = @ptrCast(@alignCast(p));
        header.strong_count = std.atomic.Value(usize).init(1);
        header.weak_count = std.atomic.Value(usize).init(1); // Implicit weak reference for all strong references
        return header.getValue();
    }
    return null;
}

/// Clone an Arc (atomically increment strong count).
/// Returns the same pointer (for convenience in generated code).
/// Thread-safe: uses atomic fetch_add.
pub export fn klar_arc_clone(value_ptr: *anyopaque) *anyopaque {
    const header = ArcHeader.fromValue(value_ptr);

    // Increment strong count atomically.
    // Using .monotonic because:
    // - We don't need to synchronize with any other memory operations
    // - The ordering is provided by the fact that we already have a valid reference
    // This is safe because if we have a reference, strong_count >= 1.
    _ = header.strong_count.fetchAdd(1, .monotonic);

    return value_ptr;
}

/// Drop an Arc (atomically decrement strong count).
/// If strong count reaches 0, calls the destructor and decrements weak count.
/// If weak count also reaches 0, frees the allocation.
/// Thread-safe: uses atomic operations with proper memory ordering.
pub export fn klar_arc_drop(value_ptr: *anyopaque, value_size: usize, value_align: usize, destructor: ?DestructorFn) void {
    const header = ArcHeader.fromValue(value_ptr);

    // Decrement strong count atomically.
    // Using .release to ensure all writes to the data are visible to the thread
    // that will destroy the value.
    const prev_strong = header.strong_count.fetchSub(1, .release);

    if (prev_strong == 1) {
        // We were the last strong reference.
        // Acquire fence to synchronize with all release operations that
        // decremented the strong count before us.
        std.atomic.fence(.acquire);

        // Value is now dead - call destructor
        if (destructor) |dtor| {
            dtor(value_ptr);
        }

        // Decrement the implicit weak reference held by strong references.
        // Using .release to ensure destructor writes are visible.
        const prev_weak = header.weak_count.fetchSub(1, .release);

        if (prev_weak == 1) {
            // We were the last weak reference too, free the entire allocation.
            // Acquire fence to synchronize with all weak reference drops.
            std.atomic.fence(.acquire);

            const header_size = @sizeOf(ArcHeader);
            const total_size = header_size + value_size;
            const align_log2: u8 = @intCast(std.math.log2_int(usize, @max(@alignOf(ArcHeader), value_align)));
            alloc.klar_free(@ptrCast(header), total_size, align_log2);
        }
    }
}

/// Clone a WeakArc reference (atomically increment weak count).
/// Returns the same pointer.
/// Thread-safe: uses atomic fetch_add.
pub export fn klar_weak_arc_clone(value_ptr: *anyopaque) *anyopaque {
    const header = ArcHeader.fromValue(value_ptr);
    // Monotonic is sufficient for increment (similar to strong clone)
    _ = header.weak_count.fetchAdd(1, .monotonic);
    return value_ptr;
}

/// Drop a WeakArc reference (atomically decrement weak count).
/// If weak count reaches 0 and strong count is also 0, frees the allocation.
/// Thread-safe: uses atomic operations.
pub export fn klar_weak_arc_drop(value_ptr: *anyopaque, value_size: usize, value_align: usize) void {
    const header = ArcHeader.fromValue(value_ptr);

    // Release to ensure any prior reads/writes are visible
    const prev_weak = header.weak_count.fetchSub(1, .release);

    if (prev_weak == 1) {
        // Acquire fence before potentially freeing
        std.atomic.fence(.acquire);

        // Check strong count - if 0, we can free
        // Note: strong_count being 0 is stable once it reaches 0 (can't increase)
        if (header.strong_count.load(.acquire) == 0) {
            const header_size = @sizeOf(ArcHeader);
            const total_size = header_size + value_size;
            const align_log2: u8 = @intCast(std.math.log2_int(usize, @max(@alignOf(ArcHeader), value_align)));
            alloc.klar_free(@ptrCast(header), total_size, align_log2);
        }
    }
}

/// Attempt to upgrade a WeakArc reference to an Arc.
/// Returns the value pointer if successful (strong_count > 0), null otherwise.
/// If successful, increments strong_count atomically.
/// Thread-safe: uses compare-and-swap loop.
pub export fn klar_weak_arc_upgrade(value_ptr: *anyopaque) ?*anyopaque {
    const header = ArcHeader.fromValue(value_ptr);

    // Use a compare-and-swap loop to safely increment strong_count only if > 0.
    // We can't just check and then increment because another thread might drop
    // the last strong reference between our check and increment.
    var current = header.strong_count.load(.acquire);

    while (current > 0) {
        // Try to increment the count. If another thread modified it, retry.
        if (header.strong_count.cmpxchgWeak(
            current,
            current + 1,
            .acquire, // Success: acquire to synchronize with release from drops
            .monotonic, // Failure: just reload and try again
        )) |new_current| {
            // CAS failed, update current and retry
            current = new_current;
        } else {
            // CAS succeeded, we now have a strong reference
            return value_ptr;
        }
    }

    // strong_count is 0, can't upgrade
    return null;
}

/// Create a WeakArc reference from an Arc.
/// Increments weak_count and returns the same pointer.
/// Thread-safe: uses atomic fetch_add.
pub export fn klar_arc_downgrade(value_ptr: *anyopaque) *anyopaque {
    const header = ArcHeader.fromValue(value_ptr);
    // Monotonic is sufficient for increment
    _ = header.weak_count.fetchAdd(1, .monotonic);
    return value_ptr;
}

/// Get the current strong count (for debugging/testing).
/// Note: The returned value may be stale by the time you use it.
pub export fn klar_arc_strong_count(value_ptr: *anyopaque) usize {
    const header = ArcHeader.fromValue(value_ptr);
    return header.strong_count.load(.acquire);
}

/// Get the current weak count (for debugging/testing).
/// Note: The returned value may be stale by the time you use it.
pub export fn klar_arc_weak_count(value_ptr: *anyopaque) usize {
    const header = ArcHeader.fromValue(value_ptr);
    return header.weak_count.load(.acquire);
}

// ============================================================================
// Tests
// ============================================================================

test "Arc basic allocation and deallocation" {
    const testing = std.testing;

    // Allocate an Arc for a u64 value
    const ptr = klar_arc_alloc(@sizeOf(u64), @alignOf(u64));
    try testing.expect(ptr != null);

    const value: *u64 = @ptrCast(@alignCast(ptr.?));
    value.* = 42;

    // Check initial counts
    try testing.expectEqual(@as(usize, 1), klar_arc_strong_count(ptr.?));
    try testing.expectEqual(@as(usize, 1), klar_arc_weak_count(ptr.?));

    // Drop the Arc
    klar_arc_drop(ptr.?, @sizeOf(u64), @alignOf(u64), null);
    // Memory should be freed (can't verify directly, but no crash is good)
}

test "Arc cloning" {
    const testing = std.testing;

    const ptr = klar_arc_alloc(@sizeOf(u64), @alignOf(u64));
    try testing.expect(ptr != null);

    // Clone the Arc
    const clone1 = klar_arc_clone(ptr.?);
    try testing.expectEqual(ptr.?, clone1);
    try testing.expectEqual(@as(usize, 2), klar_arc_strong_count(ptr.?));

    const clone2 = klar_arc_clone(ptr.?);
    try testing.expectEqual(@as(usize, 3), klar_arc_strong_count(ptr.?));

    // Drop one clone
    klar_arc_drop(clone2, @sizeOf(u64), @alignOf(u64), null);
    try testing.expectEqual(@as(usize, 2), klar_arc_strong_count(ptr.?));

    // Drop another clone
    klar_arc_drop(clone1, @sizeOf(u64), @alignOf(u64), null);
    try testing.expectEqual(@as(usize, 1), klar_arc_strong_count(ptr.?));

    // Drop the original
    klar_arc_drop(ptr.?, @sizeOf(u64), @alignOf(u64), null);
}

test "WeakArc references" {
    const testing = std.testing;

    const ptr = klar_arc_alloc(@sizeOf(u64), @alignOf(u64));
    try testing.expect(ptr != null);

    // Downgrade to WeakArc
    const weak = klar_arc_downgrade(ptr.?);
    try testing.expectEqual(@as(usize, 1), klar_arc_strong_count(ptr.?));
    try testing.expectEqual(@as(usize, 2), klar_arc_weak_count(ptr.?)); // 1 implicit + 1 explicit

    // Upgrade should succeed while Arc exists
    const upgraded = klar_weak_arc_upgrade(weak);
    try testing.expect(upgraded != null);
    try testing.expectEqual(@as(usize, 2), klar_arc_strong_count(ptr.?));

    // Drop the upgraded Arc
    klar_arc_drop(upgraded.?, @sizeOf(u64), @alignOf(u64), null);
    try testing.expectEqual(@as(usize, 1), klar_arc_strong_count(ptr.?));

    // Drop the original Arc
    klar_arc_drop(ptr.?, @sizeOf(u64), @alignOf(u64), null);

    // Weak reference still valid but upgrade should fail
    // Note: After strong count drops to 0, the value is destroyed but header remains
    // We can still access the header through the weak pointer
    const upgrade_after_drop = klar_weak_arc_upgrade(weak);
    try testing.expect(upgrade_after_drop == null);

    // Drop the weak reference (this should free the allocation)
    klar_weak_arc_drop(weak, @sizeOf(u64), @alignOf(u64));
}

var destructor_call_count: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);

fn test_destructor(ptr: *anyopaque) callconv(.C) void {
    _ = ptr;
    _ = destructor_call_count.fetchAdd(1, .seq_cst);
}

test "Arc with destructor" {
    const testing = std.testing;

    destructor_call_count.store(0, .seq_cst);

    const ptr = klar_arc_alloc(@sizeOf(u64), @alignOf(u64));
    try testing.expect(ptr != null);

    // Clone twice
    _ = klar_arc_clone(ptr.?);
    _ = klar_arc_clone(ptr.?);

    // Drop all three - destructor should only be called once
    klar_arc_drop(ptr.?, @sizeOf(u64), @alignOf(u64), &test_destructor);
    try testing.expectEqual(@as(u32, 0), destructor_call_count.load(.seq_cst)); // Still 2 refs

    klar_arc_drop(ptr.?, @sizeOf(u64), @alignOf(u64), &test_destructor);
    try testing.expectEqual(@as(u32, 0), destructor_call_count.load(.seq_cst)); // Still 1 ref

    klar_arc_drop(ptr.?, @sizeOf(u64), @alignOf(u64), &test_destructor);
    try testing.expectEqual(@as(u32, 1), destructor_call_count.load(.seq_cst)); // Now 0 refs, destructor called
}

test "Arc header size matches Rc header size" {
    const testing = std.testing;
    const rc = @import("rc.zig");

    // Arc and Rc should have the same header layout for compatibility
    try testing.expectEqual(@sizeOf(rc.RcHeader), @sizeOf(ArcHeader));
    try testing.expectEqual(@alignOf(rc.RcHeader), @alignOf(ArcHeader));
}
