//! String helper utilities for codegen.
//!
//! Provides constants, type constructors, and a compile-time hash function
//! for String code generation. The emission implementation (emitStringConcat,
//! emitStringEq, etc.) remains in emit.zig.
//!
//! ## Provided by this module
//!
//! - `StringField`: Outer string struct field index (header_ptr = 0)
//! - `StringHeaderField`: Header struct field indices (data, len, capacity)
//! - `string_struct_size`: Size constant (8 bytes — single pointer)
//! - `string_header_size`: Size constant (16 bytes — heap-allocated header)
//! - `createStringStructType`: Build the LLVM type for String (outer wrapper)
//! - `createStringHeaderType`: Build the LLVM type for StringHeader (heap data)
//! - `hashString`: FNV-1a hash (matches runtime implementation)
//!
//! ## String Struct Layout (Heap-Indirected)
//!
//! ```
//! struct String {
//!     header: *StringHeader,  // Pointer to shared heap header
//! }
//!
//! struct StringHeader {
//!     data: *u8,     // Pointer to character data (null-terminated for C interop)
//!     len: i32,      // Current length in bytes (excludes null terminator)
//!     capacity: i32, // Allocated capacity (includes space for null terminator)
//! }
//! ```
//!
//! Outer struct: 8 bytes (single pointer). When copied, both copies share
//! the same heap header, so mutations through either are visible to both.
//! Header: 16 bytes (8 + 4 + 4), heap-allocated.

const std = @import("std");
const llvm = @import("llvm.zig");

/// Outer string struct field index (just the header pointer).
pub const StringField = struct {
    pub const header_ptr = 0;
};

/// Header struct field indices (heap-allocated).
pub const StringHeaderField = struct {
    pub const data = 0;
    pub const len = 1;
    pub const capacity = 2;
};

/// Size of the outer String struct in bytes (single pointer).
pub const string_struct_size = 8;

/// Size of the heap-allocated StringHeader in bytes.
pub const string_header_size = 16;

/// Create the LLVM type for Klar's outer String struct: { ptr }
pub fn createStringStructType(ctx: llvm.Context) llvm.TypeRef {
    var fields = [_]llvm.TypeRef{
        llvm.Types.pointer(ctx), // header pointer
    };
    return llvm.Types.struct_(ctx, &fields, false);
}

/// Create the LLVM type for the heap-allocated StringHeader: { ptr, i32, i32 }
pub fn createStringHeaderType(ctx: llvm.Context) llvm.TypeRef {
    var fields = [_]llvm.TypeRef{
        llvm.Types.pointer(ctx), // data pointer
        llvm.Types.int32(ctx), // len
        llvm.Types.int32(ctx), // capacity
    };
    return llvm.Types.struct_(ctx, &fields, false);
}

/// Hash function for strings (FNV-1a).
/// This matches the runtime implementation for consistency.
pub fn hashString(data: []const u8) u64 {
    const fnv_offset: u64 = 0xcbf29ce484222325;
    const fnv_prime: u64 = 0x100000001b3;

    var hash: u64 = fnv_offset;
    for (data) |byte| {
        hash ^= byte;
        hash *%= fnv_prime;
    }
    return hash;
}

test "hashString" {
    // Empty string
    const empty_hash = hashString("");
    try std.testing.expectEqual(@as(u64, 0xcbf29ce484222325), empty_hash);

    // Same string should produce same hash
    const hash1 = hashString("hello");
    const hash2 = hashString("hello");
    try std.testing.expectEqual(hash1, hash2);

    // Different strings should (usually) produce different hashes
    const hash3 = hashString("world");
    try std.testing.expect(hash1 != hash3);
}
