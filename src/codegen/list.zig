//! List emission utilities for codegen.
//!
//! This module documents the List[T] type implementation and provides
//! utilities for working with lists during code generation.
//!
//! ## List Struct Layout
//!
//! Klar lists are generic dynamic arrays with the following layout:
//!
//! ```
//! struct List[T] {
//!     data: *T,      // Pointer to element array
//!     len: i32,      // Current number of elements
//!     capacity: i32, // Allocated capacity (number of T)
//! }
//! ```
//!
//! Total size: 16 bytes (8 + 4 + 4)
//!
//! ## Element Storage
//!
//! Elements are stored contiguously in memory. The `data` pointer
//! points to an array of T values. Growth is typically 2x when
//! capacity is exceeded.
//!
//! ## List Operations
//!
//! Key functions in emit.zig:
//!
//! - `getListStructType`: Returns the LLVM struct type for List
//! - `emitListNew`: Create a new empty list
//! - `emitListWithCapacity`: Create list with initial capacity
//! - `emitListPush`: Append element to list
//! - `emitListPop`: Remove and return last element
//! - `emitListGet`: Get element at index (returns Optional)
//! - `emitListSet`: Set element at index
//! - `emitListLen`: Get current length
//! - `emitListCapacity`: Get current capacity
//! - `emitListIsEmpty`: Check if list is empty
//! - `emitListFirst/Last`: Get first/last element (Optional)
//! - `emitListClear`: Remove all elements
//! - `emitListClone`: Create a copy of the list
//! - `emitListDrop`: Free list memory
//!
//! ## Runtime Functions
//!
//! These are generated inline in the emitter:
//! - `klar_list_new`: Allocate empty list
//! - `klar_list_push`: Push with capacity check
//! - `klar_list_get`: Bounds-checked access

const std = @import("std");
const llvm = @import("llvm.zig");

/// List struct field indices.
pub const ListField = struct {
    pub const data = 0;
    pub const len = 1;
    pub const capacity = 2;
};

/// Size of the List struct in bytes (excluding element storage).
pub const list_struct_size = 16;

/// Initial capacity for new lists.
pub const initial_capacity = 4;

/// Growth factor when resizing.
pub const growth_factor = 2;

/// Create the LLVM type for Klar's List struct.
pub fn createListStructType(ctx: llvm.Context) llvm.TypeRef {
    var fields = [_]llvm.TypeRef{
        llvm.Types.pointer(ctx), // data pointer
        llvm.Types.int32(ctx), // len
        llvm.Types.int32(ctx), // capacity
    };
    return llvm.Types.struct_(ctx, &fields, false);
}

/// Calculate new capacity after growth.
pub fn growCapacity(current: u32) u32 {
    if (current == 0) return initial_capacity;
    return current * growth_factor;
}

test "growCapacity" {
    try std.testing.expectEqual(@as(u32, 4), growCapacity(0));
    try std.testing.expectEqual(@as(u32, 8), growCapacity(4));
    try std.testing.expectEqual(@as(u32, 16), growCapacity(8));
}
