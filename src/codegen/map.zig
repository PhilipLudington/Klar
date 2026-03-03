//! Map#[K, V] helper utilities for codegen.
//!
//! Provides constants, type constructors, and hash table helpers for Map#[K, V]
//! code generation. The emission implementation (emitMapNew, emitMapInsert,
//! etc.) remains in emit.zig.
//!
//! ## Provided by this module
//!
//! - `MapField` / `MapHeaderField` / `EntryState`: Struct field and entry state constants
//! - `map_struct_size`: Size constant (8 bytes — single pointer)
//! - `map_header_size`: Size constant (20 bytes — heap-allocated header)
//! - `initial_capacity` / `growth_factor` / `max_load_factor`: Policy constants
//! - `fnv_offset` / `fnv_prime`: FNV-1a hash constants
//! - `createMapStructType`: Build the LLVM type for Map (outer wrapper)
//! - `createMapHeaderType`: Build the LLVM type for MapHeader (heap data)
//! - `slotIndex`: Calculate slot from hash and capacity (power-of-2 mask)
//! - `needsResize`: Check if load factor exceeds threshold
//!
//! ## Map Struct Layout (Heap-Indirected)
//!
//! Open addressing with linear probing:
//!
//! ```
//! struct Map#[K, V] {
//!     header: *MapHeader,  // Pointer to shared heap header
//! }
//!
//! struct MapHeader {
//!     entries: *Entry,   // Pointer to entry array
//!     len: i32,          // Current number of elements
//!     capacity: i32,     // Total slots (power of 2)
//!     tombstones: i32,   // Count of deleted entries
//! }
//! ```
//!
//! Outer struct: 8 bytes (single pointer). When copied, both copies share
//! the same heap header, so mutations through either are visible to both.
//! Header: 20 bytes (8 + 4 + 4 + 4), heap-allocated.
//! Resizes when (len + tombstones) / capacity > 0.75.

const std = @import("std");
const llvm = @import("llvm.zig");

/// Outer map struct field index (just the header pointer).
pub const MapField = struct {
    pub const header_ptr = 0;
};

/// Header struct field indices (heap-allocated).
pub const MapHeaderField = struct {
    pub const entries = 0;
    pub const len = 1;
    pub const capacity = 2;
    pub const tombstones = 3;
};

/// Entry state values.
pub const EntryState = struct {
    pub const empty = 0;
    pub const occupied = 1;
    pub const tombstone = 2;
};

/// Size of the outer Map struct in bytes (single pointer).
pub const map_struct_size = 8;

/// Size of the heap-allocated MapHeader in bytes.
pub const map_header_size = 20;

/// Initial capacity for new maps (must be power of 2).
pub const initial_capacity = 8;

/// Maximum load factor before resize (75%).
pub const max_load_factor = 0.75;

/// Growth factor when resizing.
pub const growth_factor = 2;

/// Create the LLVM type for Klar's outer Map struct: { ptr }
pub fn createMapStructType(ctx: llvm.Context) llvm.TypeRef {
    var fields = [_]llvm.TypeRef{
        llvm.Types.pointer(ctx), // header pointer
    };
    return llvm.Types.struct_(ctx, &fields, false);
}

/// Create the LLVM type for the heap-allocated MapHeader: { ptr, i32, i32, i32 }
pub fn createMapHeaderType(ctx: llvm.Context) llvm.TypeRef {
    var fields = [_]llvm.TypeRef{
        llvm.Types.pointer(ctx), // entries pointer
        llvm.Types.int32(ctx), // len
        llvm.Types.int32(ctx), // capacity
        llvm.Types.int32(ctx), // tombstones
    };
    return llvm.Types.struct_(ctx, &fields, false);
}

/// FNV-1a hash constants.
pub const fnv_offset: u64 = 0xcbf29ce484222325;
pub const fnv_prime: u64 = 0x100000001b3;

/// Calculate slot index from hash and capacity.
/// Capacity must be a power of 2.
pub fn slotIndex(hash: u64, capacity: u32) u32 {
    return @truncate(hash & (@as(u64, capacity) - 1));
}

/// Check if resize is needed based on load factor.
pub fn needsResize(len: u32, tombstones: u32, capacity: u32) bool {
    const load = @as(f64, @floatFromInt(len + tombstones)) / @as(f64, @floatFromInt(capacity));
    return load > max_load_factor;
}

test "slotIndex" {
    try std.testing.expectEqual(@as(u32, 0), slotIndex(0, 8));
    try std.testing.expectEqual(@as(u32, 7), slotIndex(7, 8));
    try std.testing.expectEqual(@as(u32, 0), slotIndex(8, 8));
    try std.testing.expectEqual(@as(u32, 1), slotIndex(9, 8));
}

test "needsResize" {
    try std.testing.expect(!needsResize(5, 0, 8)); // 62.5% < 75%
    try std.testing.expect(needsResize(6, 0, 8)); // 75% >= 75%
    try std.testing.expect(needsResize(4, 2, 8)); // 75% (with tombstones)
}
