// ============================================================================
// Klar Reference Counting Runtime
// ============================================================================
//
// Implements Rc[T] (reference-counted) and Weak[T] (weak reference) types.
//
// Memory layout:
// ┌─────────────────────────────────┐
// │ strong_count: usize             │  (number of Rc handles)
// │ weak_count: usize               │  (number of Weak handles + 1 if strong > 0)
// │ value: [value_size]u8           │  (the owned value, inline)
// └─────────────────────────────────┘
//
// The Rc handle is just a pointer to this structure.
// Strong count of 0 means the value is deallocated (but header may remain for weak refs).
// Weak count of 0 (after strong becomes 0) means the entire allocation is freed.

const std = @import("std");
const alloc = @import("alloc.zig");

/// Destructor function type - called when the value is dropped.
/// Takes a pointer to the value (after the header).
pub const DestructorFn = *const fn (*anyopaque) callconv(.C) void;

/// Header for reference-counted allocations.
/// This is placed before the actual value in memory.
pub const RcHeader = extern struct {
    strong_count: usize,
    weak_count: usize,

    /// Get pointer to the value (immediately after header)
    pub fn getValue(self: *RcHeader) *anyopaque {
        const header_ptr: [*]u8 = @ptrCast(self);
        return @ptrCast(header_ptr + @sizeOf(RcHeader));
    }

    /// Get header from value pointer
    pub fn fromValue(value: *anyopaque) *RcHeader {
        const value_ptr: [*]u8 = @ptrCast(value);
        return @ptrCast(@alignCast(value_ptr - @sizeOf(RcHeader)));
    }
};

/// Allocate a new Rc with the given value size.
/// Returns a pointer to the value (NOT the header).
/// The value is uninitialized - caller must initialize it.
/// Initial strong_count = 1, weak_count = 1 (implicit weak for strong refs).
pub export fn klar_rc_alloc(value_size: usize, value_align: usize) ?*anyopaque {
    const header_size = @sizeOf(RcHeader);
    const total_size = header_size + value_size;

    // Use max of header alignment and value alignment
    const align_log2: u8 = @intCast(std.math.log2_int(usize, @max(@alignOf(RcHeader), value_align)));
    const ptr = alloc.klar_alloc(total_size, align_log2);

    if (ptr) |p| {
        const header: *RcHeader = @ptrCast(@alignCast(p));
        header.strong_count = 1;
        header.weak_count = 1; // Implicit weak reference for all strong references
        return header.getValue();
    }
    return null;
}

/// Clone an Rc (increment strong count).
/// Returns the same pointer (for convenience in generated code).
pub export fn klar_rc_clone(value_ptr: *anyopaque) *anyopaque {
    const header = RcHeader.fromValue(value_ptr);

    // Increment strong count
    // Note: This is NOT thread-safe. Use Arc for thread-safe reference counting.
    header.strong_count += 1;

    return value_ptr;
}

/// Drop an Rc (decrement strong count).
/// If strong count reaches 0, calls the destructor and decrements weak count.
/// If weak count also reaches 0, frees the allocation.
pub export fn klar_rc_drop(value_ptr: *anyopaque, value_size: usize, value_align: usize, destructor: ?DestructorFn) void {
    const header = RcHeader.fromValue(value_ptr);

    // Decrement strong count
    header.strong_count -= 1;

    if (header.strong_count == 0) {
        // Value is now dead - call destructor
        if (destructor) |dtor| {
            dtor(value_ptr);
        }

        // Decrement the implicit weak reference held by strong references
        header.weak_count -= 1;

        if (header.weak_count == 0) {
            // No more weak references, free the entire allocation
            const header_size = @sizeOf(RcHeader);
            const total_size = header_size + value_size;
            const align_log2: u8 = @intCast(std.math.log2_int(usize, @max(@alignOf(RcHeader), value_align)));
            alloc.klar_free(@ptrCast(header), total_size, align_log2);
        }
    }
}

/// Clone a Weak reference (increment weak count).
/// Returns the same pointer.
pub export fn klar_weak_clone(value_ptr: *anyopaque) *anyopaque {
    const header = RcHeader.fromValue(value_ptr);
    header.weak_count += 1;
    return value_ptr;
}

/// Drop a Weak reference (decrement weak count).
/// If weak count reaches 0 and strong count is also 0, frees the allocation.
pub export fn klar_weak_drop(value_ptr: *anyopaque, value_size: usize, value_align: usize) void {
    const header = RcHeader.fromValue(value_ptr);

    header.weak_count -= 1;

    if (header.weak_count == 0 and header.strong_count == 0) {
        // No more references at all, free the allocation
        const header_size = @sizeOf(RcHeader);
        const total_size = header_size + value_size;
        const align_log2: u8 = @intCast(std.math.log2_int(usize, @max(@alignOf(RcHeader), value_align)));
        alloc.klar_free(@ptrCast(header), total_size, align_log2);
    }
}

/// Attempt to upgrade a Weak reference to an Rc.
/// Returns the value pointer if successful (strong_count > 0), null otherwise.
/// If successful, increments strong_count.
pub export fn klar_weak_upgrade(value_ptr: *anyopaque) ?*anyopaque {
    const header = RcHeader.fromValue(value_ptr);

    if (header.strong_count > 0) {
        header.strong_count += 1;
        return value_ptr;
    }
    return null;
}

/// Create a Weak reference from an Rc.
/// Increments weak_count and returns the same pointer.
pub export fn klar_rc_downgrade(value_ptr: *anyopaque) *anyopaque {
    const header = RcHeader.fromValue(value_ptr);
    header.weak_count += 1;
    return value_ptr;
}

/// Get the current strong count (for debugging/testing).
pub export fn klar_rc_strong_count(value_ptr: *anyopaque) usize {
    const header = RcHeader.fromValue(value_ptr);
    return header.strong_count;
}

/// Get the current weak count (for debugging/testing).
pub export fn klar_rc_weak_count(value_ptr: *anyopaque) usize {
    const header = RcHeader.fromValue(value_ptr);
    return header.weak_count;
}

// ============================================================================
// Tests
// ============================================================================

test "Rc basic allocation and deallocation" {
    const testing = std.testing;

    // Allocate an Rc for a u64 value
    const ptr = klar_rc_alloc(@sizeOf(u64), @alignOf(u64));
    try testing.expect(ptr != null);

    const value: *u64 = @ptrCast(@alignCast(ptr.?));
    value.* = 42;

    // Check initial counts
    try testing.expectEqual(@as(usize, 1), klar_rc_strong_count(ptr.?));
    try testing.expectEqual(@as(usize, 1), klar_rc_weak_count(ptr.?));

    // Drop the Rc
    klar_rc_drop(ptr.?, @sizeOf(u64), @alignOf(u64), null);
    // Memory should be freed (can't verify directly, but no crash is good)
}

test "Rc cloning" {
    const testing = std.testing;

    const ptr = klar_rc_alloc(@sizeOf(u64), @alignOf(u64));
    try testing.expect(ptr != null);

    // Clone the Rc
    const clone1 = klar_rc_clone(ptr.?);
    try testing.expectEqual(ptr.?, clone1);
    try testing.expectEqual(@as(usize, 2), klar_rc_strong_count(ptr.?));

    const clone2 = klar_rc_clone(ptr.?);
    try testing.expectEqual(@as(usize, 3), klar_rc_strong_count(ptr.?));

    // Drop one clone
    klar_rc_drop(clone2, @sizeOf(u64), @alignOf(u64), null);
    try testing.expectEqual(@as(usize, 2), klar_rc_strong_count(ptr.?));

    // Drop another clone
    klar_rc_drop(clone1, @sizeOf(u64), @alignOf(u64), null);
    try testing.expectEqual(@as(usize, 1), klar_rc_strong_count(ptr.?));

    // Drop the original
    klar_rc_drop(ptr.?, @sizeOf(u64), @alignOf(u64), null);
}

test "Weak references" {
    const testing = std.testing;

    const ptr = klar_rc_alloc(@sizeOf(u64), @alignOf(u64));
    try testing.expect(ptr != null);

    // Downgrade to Weak
    const weak = klar_rc_downgrade(ptr.?);
    try testing.expectEqual(@as(usize, 1), klar_rc_strong_count(ptr.?));
    try testing.expectEqual(@as(usize, 2), klar_rc_weak_count(ptr.?)); // 1 implicit + 1 explicit

    // Upgrade should succeed while Rc exists
    const upgraded = klar_weak_upgrade(weak);
    try testing.expect(upgraded != null);
    try testing.expectEqual(@as(usize, 2), klar_rc_strong_count(ptr.?));

    // Drop the upgraded Rc
    klar_rc_drop(upgraded.?, @sizeOf(u64), @alignOf(u64), null);
    try testing.expectEqual(@as(usize, 1), klar_rc_strong_count(ptr.?));

    // Drop the original Rc
    klar_rc_drop(ptr.?, @sizeOf(u64), @alignOf(u64), null);

    // Weak reference still valid but upgrade should fail
    // Note: After strong count drops to 0, the value is destroyed but header remains
    // We can still access the header through the weak pointer
    const upgrade_after_drop = klar_weak_upgrade(weak);
    try testing.expect(upgrade_after_drop == null);

    // Drop the weak reference (this should free the allocation)
    klar_weak_drop(weak, @sizeOf(u64), @alignOf(u64));
}

var destructor_call_count: u32 = 0;

fn test_destructor(ptr: *anyopaque) callconv(.C) void {
    _ = ptr;
    destructor_call_count += 1;
}

test "Rc with destructor" {
    const testing = std.testing;

    destructor_call_count = 0;

    const ptr = klar_rc_alloc(@sizeOf(u64), @alignOf(u64));
    try testing.expect(ptr != null);

    // Clone twice
    _ = klar_rc_clone(ptr.?);
    _ = klar_rc_clone(ptr.?);

    // Drop all three - destructor should only be called once
    klar_rc_drop(ptr.?, @sizeOf(u64), @alignOf(u64), &test_destructor);
    try testing.expectEqual(@as(u32, 0), destructor_call_count); // Still 2 refs

    klar_rc_drop(ptr.?, @sizeOf(u64), @alignOf(u64), &test_destructor);
    try testing.expectEqual(@as(u32, 0), destructor_call_count); // Still 1 ref

    klar_rc_drop(ptr.?, @sizeOf(u64), @alignOf(u64), &test_destructor);
    try testing.expectEqual(@as(u32, 1), destructor_call_count); // Now 0 refs, destructor called
}
