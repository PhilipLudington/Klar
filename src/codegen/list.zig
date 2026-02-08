//! List[T] helper utilities for codegen.
//!
//! Provides constants, type constructors, and growth helpers for List[T]
//! code generation. The emission implementation (emitListNew, emitListPush,
//! etc.) remains in emit.zig.
//!
//! ## Provided by this module
//!
//! - `ListField`: Struct field index constants
//! - `list_struct_size`: Size constant (16 bytes)
//! - `initial_capacity` / `growth_factor`: Growth policy constants
//! - `createListStructType`: Build the LLVM struct type for List
//! - `growCapacity`: Calculate new capacity after growth
//!
//! ## List Struct Layout
//!
//! ```
//! struct List[T] {
//!     data: *T,      // Pointer to element array
//!     len: i32,      // Current number of elements
//!     capacity: i32, // Allocated capacity (number of T)
//! }
//! ```
//!
//! Total size: 16 bytes (8 + 4 + 4). Growth is 2x when capacity is exceeded.

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
