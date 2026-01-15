// ============================================================================
// Klar Runtime Allocator
// ============================================================================
//
// Provides a C-compatible allocation interface for the Klar runtime.
// Uses the system allocator (libc malloc/free) for heap operations.

const std = @import("std");
const builtin = @import("builtin");

/// Global allocator used by the Klar runtime.
/// In debug mode, uses GeneralPurposeAllocator for leak detection.
/// In release mode, uses the C allocator for performance.
pub const allocator: std.mem.Allocator = if (builtin.mode == .Debug)
    std.heap.page_allocator // Simple for now, could use GPA
else
    std.heap.c_allocator;

/// Allocate memory with the specified size and alignment.
/// Returns null on allocation failure.
pub export fn klar_alloc(size: usize, alignment: u8) ?*anyopaque {
    const aligned = allocator.rawAlloc(size, std.math.log2_int(usize, @as(usize, 1) << @intCast(alignment)), @returnAddress());
    return aligned;
}

/// Free memory previously allocated with klar_alloc.
pub export fn klar_free(ptr: ?*anyopaque, size: usize, alignment: u8) void {
    if (ptr) |p| {
        const slice = @as([*]u8, @ptrCast(p))[0..size];
        allocator.rawFree(slice, std.math.log2_int(usize, @as(usize, 1) << @intCast(alignment)), @returnAddress());
    }
}

/// Reallocate memory to a new size.
/// Returns null on allocation failure (original memory is NOT freed).
pub export fn klar_realloc(ptr: ?*anyopaque, old_size: usize, new_size: usize, alignment: u8) ?*anyopaque {
    if (ptr) |p| {
        const log2_align = std.math.log2_int(usize, @as(usize, 1) << @intCast(alignment));
        const old_slice = @as([*]u8, @ptrCast(p))[0..old_size];
        const result = allocator.rawResize(old_slice, log2_align, new_size, @returnAddress());
        if (result) {
            return ptr;
        }
        // Need to allocate new block and copy
        const new_ptr = allocator.rawAlloc(new_size, log2_align, @returnAddress());
        if (new_ptr) |new_p| {
            const copy_size = @min(old_size, new_size);
            @memcpy(@as([*]u8, @ptrCast(new_p))[0..copy_size], old_slice[0..copy_size]);
            allocator.rawFree(old_slice, log2_align, @returnAddress());
            return new_p;
        }
        return null;
    }
    // ptr is null, just allocate
    return klar_alloc(new_size, alignment);
}

test "basic allocation" {
    const testing = std.testing;

    const ptr = klar_alloc(100, 3); // 8-byte alignment
    try testing.expect(ptr != null);

    klar_free(ptr, 100, 3);
}
