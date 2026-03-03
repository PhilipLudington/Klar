//! List#[T] helper utilities for codegen.
//!
//! Provides constants, type constructors, and growth helpers for List#[T]
//! code generation. The emission implementation (emitListNew, emitListPush,
//! etc.) remains in emit.zig.
//!
//! ## Provided by this module
//!
//! - `ListField`: Outer list struct field index (header_ptr = 0)
//! - `ListHeaderField`: Header struct field indices (data, len, capacity)
//! - `list_struct_size`: Size constant (8 bytes — single pointer)
//! - `list_header_size`: Size constant (16 bytes — heap-allocated header)
//! - `initial_capacity` / `growth_factor`: Growth policy constants
//! - `createListStructType`: Build the LLVM type for List (outer wrapper)
//! - `createListHeaderType`: Build the LLVM type for ListHeader (heap data)
//! - `growCapacity`: Calculate new capacity after growth
//!
//! ## List Struct Layout (Heap-Indirected)
//!
//! ```
//! struct List#[T] {
//!     header: *ListHeader,  // Pointer to shared heap header
//! }
//!
//! struct ListHeader {
//!     data: *T,      // Pointer to element array
//!     len: i32,      // Current number of elements
//!     capacity: i32, // Allocated capacity (number of T)
//! }
//! ```
//!
//! Outer struct: 8 bytes (single pointer). When copied, both copies share
//! the same heap header, so mutations through either are visible to both.
//! Header: 16 bytes (8 + 4 + 4), heap-allocated. Growth is 2x when
//! capacity is exceeded.

const std = @import("std");
const llvm = @import("llvm.zig");

/// Outer list struct field index (just the header pointer).
pub const ListField = struct {
    pub const header_ptr = 0;
};

/// Header struct field indices (heap-allocated).
pub const ListHeaderField = struct {
    pub const data = 0;
    pub const len = 1;
    pub const capacity = 2;
};

/// Size of the outer List struct in bytes (single pointer).
pub const list_struct_size = 8;

/// Size of the heap-allocated ListHeader in bytes.
pub const list_header_size = 16;

/// Initial capacity for new lists.
pub const initial_capacity = 4;

/// Growth factor when resizing.
pub const growth_factor = 2;

/// Create the LLVM type for Klar's outer List struct: { ptr }
pub fn createListStructType(ctx: llvm.Context) llvm.TypeRef {
    var fields = [_]llvm.TypeRef{
        llvm.Types.pointer(ctx), // header pointer
    };
    return llvm.Types.struct_(ctx, &fields, false);
}

/// Create the LLVM type for the heap-allocated ListHeader: { ptr, i32, i32 }
pub fn createListHeaderType(ctx: llvm.Context) llvm.TypeRef {
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
