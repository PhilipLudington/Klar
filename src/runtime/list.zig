const std = @import("std");
const alloc = @import("alloc.zig");

/// Runtime representation of a List.
/// Layout: { ptr: *anyopaque, len: i32, capacity: i32 }
pub const ListHeader = extern struct {
    ptr: ?*anyopaque,
    len: i32,
    capacity: i32,
};

const INITIAL_CAPACITY: i32 = 8;

/// Create a new empty list.
/// Returns a ListHeader struct with ptr=null, len=0, capacity=0.
export fn klar_list_new() ListHeader {
    return ListHeader{
        .ptr = null,
        .len = 0,
        .capacity = 0,
    };
}

/// Push an element onto the list, growing if necessary.
/// element_size: size of each element in bytes
/// element_align_log2: alignment of elements as log2 (e.g., 2 for 4-byte alignment)
/// value_ptr: pointer to the value to copy
export fn klar_list_push(list: *ListHeader, element_size: usize, element_align_log2: u8, value_ptr: *const anyopaque) void {
    // Check if we need to grow
    if (list.len >= list.capacity) {
        const new_capacity: i32 = if (list.capacity == 0) INITIAL_CAPACITY else list.capacity * 2;
        const new_size = @as(usize, @intCast(new_capacity)) * element_size;

        if (list.ptr) |old_ptr| {
            const old_size = @as(usize, @intCast(list.capacity)) * element_size;
            list.ptr = alloc.klar_realloc(old_ptr, old_size, new_size, element_align_log2);
        } else {
            list.ptr = alloc.klar_alloc(new_size, element_align_log2);
        }
        list.capacity = new_capacity;
    }

    // Copy the value to the list
    if (list.ptr) |ptr| {
        const dest: [*]u8 = @ptrCast(ptr);
        const offset = @as(usize, @intCast(list.len)) * element_size;
        const src: [*]const u8 = @ptrCast(value_ptr);
        @memcpy(dest[offset..][0..element_size], src[0..element_size]);
        list.len += 1;
    }
}

/// Pop an element from the list.
/// Returns true if an element was popped, false if list was empty.
/// If true, the value is copied to value_out_ptr.
export fn klar_list_pop(list: *ListHeader, element_size: usize, value_out_ptr: *anyopaque) bool {
    if (list.len == 0) {
        return false;
    }

    list.len -= 1;

    if (list.ptr) |ptr| {
        const src: [*]const u8 = @ptrCast(ptr);
        const offset = @as(usize, @intCast(list.len)) * element_size;
        const dest: [*]u8 = @ptrCast(value_out_ptr);
        @memcpy(dest[0..element_size], src[offset..][0..element_size]);
    }

    return true;
}

/// Get an element from the list at the given index.
/// Returns true if index is valid, false otherwise.
/// If true, the value is copied to value_out_ptr.
export fn klar_list_get(list: *const ListHeader, element_size: usize, index: i32, value_out_ptr: *anyopaque) bool {
    if (index < 0 or index >= list.len) {
        return false;
    }

    if (list.ptr) |ptr| {
        const src: [*]const u8 = @ptrCast(ptr);
        const offset = @as(usize, @intCast(index)) * element_size;
        const dest: [*]u8 = @ptrCast(value_out_ptr);
        @memcpy(dest[0..element_size], src[offset..][0..element_size]);
        return true;
    }

    return false;
}

/// Set an element in the list at the given index.
/// Panics if index is out of bounds.
export fn klar_list_set(list: *ListHeader, element_size: usize, index: i32, value_ptr: *const anyopaque) void {
    if (index < 0 or index >= list.len) {
        @panic("List index out of bounds");
    }

    if (list.ptr) |ptr| {
        const dest: [*]u8 = @ptrCast(ptr);
        const offset = @as(usize, @intCast(index)) * element_size;
        const src: [*]const u8 = @ptrCast(value_ptr);
        @memcpy(dest[offset..][0..element_size], src[0..element_size]);
    }
}

/// Get the length of the list.
export fn klar_list_len(list: *const ListHeader) i32 {
    return list.len;
}

/// Check if the list is empty.
export fn klar_list_is_empty(list: *const ListHeader) bool {
    return list.len == 0;
}

/// Get the capacity of the list.
export fn klar_list_capacity(list: *const ListHeader) i32 {
    return list.capacity;
}

/// Clear the list (set len to 0 but keep capacity).
export fn klar_list_clear(list: *ListHeader) void {
    list.len = 0;
}

/// Get the first element if the list is non-empty.
/// Returns true if there is a first element, false otherwise.
export fn klar_list_first(list: *const ListHeader, element_size: usize, value_out_ptr: *anyopaque) bool {
    return klar_list_get(list, element_size, 0, value_out_ptr);
}

/// Get the last element if the list is non-empty.
/// Returns true if there is a last element, false otherwise.
export fn klar_list_last(list: *const ListHeader, element_size: usize, value_out_ptr: *anyopaque) bool {
    if (list.len == 0) {
        return false;
    }
    return klar_list_get(list, element_size, list.len - 1, value_out_ptr);
}

/// Free the list's memory.
export fn klar_list_drop(list: *ListHeader, element_size: usize, element_align_log2: u8) void {
    if (list.ptr) |ptr| {
        const size = @as(usize, @intCast(list.capacity)) * element_size;
        alloc.klar_free(ptr, size, element_align_log2);
        list.ptr = null;
        list.len = 0;
        list.capacity = 0;
    }
}
