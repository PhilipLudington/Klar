//! Map emission utilities for codegen.
//!
//! This module documents the Map[K, V] type implementation and provides
//! utilities for working with hash maps during code generation.
//!
//! ## Map Struct Layout
//!
//! Klar maps use open addressing with linear probing:
//!
//! ```
//! struct Map[K, V] {
//!     entries: *Entry,   // Pointer to entry array
//!     len: i32,          // Current number of elements
//!     capacity: i32,     // Total slots in entry array
//!     tombstones: i32,   // Count of deleted entries
//! }
//!
//! struct Entry {
//!     key: K,
//!     value: V,
//!     state: u8,  // 0=empty, 1=occupied, 2=tombstone
//! }
//! ```
//!
//! Total Map struct size: 20 bytes (8 + 4 + 4 + 4)
//!
//! ## Hash Algorithm
//!
//! Uses FNV-1a hash for keys. Key types must implement the Hash trait.
//!
//! ## Load Factor
//!
//! Maps resize when (len + tombstones) / capacity > 0.75
//!
//! ## Map Operations
//!
//! Key functions in emit.zig:
//!
//! - `getMapStructType`: Returns the LLVM struct type for Map
//! - `emitMapNew`: Create a new empty map
//! - `emitMapWithCapacity`: Create map with initial capacity
//! - `emitMapInsert`: Insert or update key-value pair
//! - `emitMapGet`: Get value for key (returns Optional)
//! - `emitMapRemove`: Remove key-value pair
//! - `emitMapContains`: Check if key exists
//! - `emitMapLen`: Get current element count
//! - `emitMapIsEmpty`: Check if map is empty
//! - `emitMapClear`: Remove all entries
//! - `emitMapKeys`: Get iterator over keys
//! - `emitMapValues`: Get iterator over values
//! - `emitMapClone`: Create a copy of the map
//! - `emitMapDrop`: Free map memory
//!
//! ## Hashing Support
//!
//! - `emitHashValue`: Generate hash for a value (type-dependent)
//! - `emitEqComparison`: Generate equality check (type-dependent)

const std = @import("std");
const llvm = @import("llvm.zig");

/// Map struct field indices.
pub const MapField = struct {
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

/// Size of the Map struct in bytes (excluding entry storage).
pub const map_struct_size = 20;

/// Initial capacity for new maps (must be power of 2).
pub const initial_capacity = 8;

/// Maximum load factor before resize (75%).
pub const max_load_factor = 0.75;

/// Growth factor when resizing.
pub const growth_factor = 2;

/// Create the LLVM type for Klar's Map struct.
pub fn createMapStructType(ctx: llvm.Context) llvm.TypeRef {
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
