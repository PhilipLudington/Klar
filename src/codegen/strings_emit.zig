//! String emission utilities for codegen.
//!
//! This module documents the String type implementation and provides
//! utilities for working with strings during code generation.
//!
//! ## String Struct Layout
//!
//! Klar strings are heap-allocated with the following layout:
//!
//! ```
//! struct String {
//!     data: *u8,     // Pointer to character data (NOT null-terminated)
//!     len: i32,      // Current length in bytes
//!     capacity: i32, // Allocated capacity
//! }
//! ```
//!
//! Total size: 16 bytes (8 + 4 + 4)
//!
//! ## String Operations
//!
//! Key functions in emit.zig:
//!
//! - `getStringStructType`: Returns the LLVM struct type for String
//! - `emitStringConcat`: Emits code for string concatenation
//! - `emitStringEq`: Emits code for string equality comparison
//! - `emitStringHash`: Emits code for string hashing
//! - `emitStringChars`: Emits code to get character iterator
//! - `emitStringSlice`: Emits code for substring extraction
//! - `emitStringTrim`: Emits code for whitespace trimming
//! - `emitStringToUppercase/Lowercase`: Case conversion
//!
//! ## Runtime Functions
//!
//! String operations use these C library functions:
//! - `strlen`: Get string length (for C string input)
//! - `strcmp`: Compare strings
//! - `memcpy`: Copy string data
//! - `malloc`/`free`: Allocate/deallocate string data

const std = @import("std");
const llvm = @import("llvm.zig");

/// String struct field indices.
pub const StringField = struct {
    pub const data = 0;
    pub const len = 1;
    pub const capacity = 2;
};

/// Size of the String struct in bytes.
pub const string_struct_size = 16;

/// Create the LLVM type for Klar's String struct.
pub fn createStringStructType(ctx: llvm.Context) llvm.TypeRef {
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
