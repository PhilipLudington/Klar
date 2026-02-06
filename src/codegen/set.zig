//! Set[T] helper utilities for codegen.
//!
//! Provides constants, type constructors, and hash table helpers for Set[T]
//! code generation. The emission implementation (emitSetNew, emitSetInsert,
//! etc.) remains in emit.zig.
//!
//! ## Provided by this module
//!
//! - `SetField` / `EntryState`: Struct field and entry state constants
//! - `set_struct_size`: Size constant (20 bytes)
//! - `initial_capacity` / `growth_factor` / `max_load_factor`: Policy constants
//! - `createSetStructType`: Build the LLVM struct type for Set
//! - `needsResize`: Check if load factor exceeds threshold
//!
//! ## Set Struct Layout
//!
//! Same open addressing approach as Map:
//!
//! ```
//! struct Set[T] {
//!     entries: *Entry,   // Pointer to entry array
//!     len: i32,          // Current number of elements
//!     capacity: i32,     // Total slots (power of 2)
//!     tombstones: i32,   // Count of deleted entries
//! }
//! ```

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
