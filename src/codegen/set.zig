//! Set emission utilities for codegen.
//!
//! This module documents the Set[T] type implementation and provides
//! utilities for working with hash sets during code generation.
//!
//! ## Set Struct Layout
//!
//! Klar sets use the same open addressing approach as Map:
//!
//! ```
//! struct Set[T] {
//!     entries: *Entry,   // Pointer to entry array
//!     len: i32,          // Current number of elements
//!     capacity: i32,     // Total slots in entry array
//!     tombstones: i32,   // Count of deleted entries
//! }
//!
//! struct Entry {
//!     value: T,
//!     state: u8,  // 0=empty, 1=occupied, 2=tombstone
//! }
//! ```
//!
//! Total Set struct size: 20 bytes (8 + 4 + 4 + 4)
//!
//! ## Set Operations
//!
//! Key functions in emit.zig:
//!
//! - `getSetStructType`: Returns the LLVM struct type for Set
//! - `emitSetNew`: Create a new empty set
//! - `emitSetWithCapacity`: Create set with initial capacity
//! - `emitSetInsert`: Add element to set
//! - `emitSetRemove`: Remove element from set
//! - `emitSetContains`: Check if element exists
//! - `emitSetLen`: Get current element count
//! - `emitSetIsEmpty`: Check if set is empty
//! - `emitSetClear`: Remove all elements
//! - `emitSetClone`: Create a copy of the set
//! - `emitSetDrop`: Free set memory
//!
//! ## Set Theory Operations (future)
//!
//! - `emitSetUnion`: Combine two sets
//! - `emitSetIntersection`: Elements in both sets
//! - `emitSetDifference`: Elements in first but not second
//! - `emitSetSymmetricDifference`: Elements in one but not both

const std = @import("std");
const llvm = @import("llvm.zig");

/// Set struct field indices.
pub const SetField = struct {
    pub const entries = 0;
    pub const len = 1;
    pub const capacity = 2;
    pub const tombstones = 3;
};

/// Entry state values (same as Map).
pub const EntryState = struct {
    pub const empty = 0;
    pub const occupied = 1;
    pub const tombstone = 2;
};

/// Size of the Set struct in bytes (excluding entry storage).
pub const set_struct_size = 20;

/// Initial capacity for new sets (must be power of 2).
pub const initial_capacity = 8;

/// Maximum load factor before resize (75%).
pub const max_load_factor = 0.75;

/// Growth factor when resizing.
pub const growth_factor = 2;

/// Create the LLVM type for Klar's Set struct.
pub fn createSetStructType(ctx: llvm.Context) llvm.TypeRef {
    var fields = [_]llvm.TypeRef{
        llvm.Types.pointer(ctx), // entries pointer
        llvm.Types.int32(ctx), // len
        llvm.Types.int32(ctx), // capacity
        llvm.Types.int32(ctx), // tombstones
    };
    return llvm.Types.struct_(ctx, &fields, false);
}

/// Check if resize is needed based on load factor.
pub fn needsResize(len: u32, tombstones: u32, capacity: u32) bool {
    const load = @as(f64, @floatFromInt(len + tombstones)) / @as(f64, @floatFromInt(capacity));
    return load > max_load_factor;
}

test "needsResize" {
    try std.testing.expect(!needsResize(5, 0, 8));
    try std.testing.expect(needsResize(6, 0, 8));
}
