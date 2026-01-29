//! Type conversion utilities for codegen.
//!
//! This module documents the type system mapping between Klar types and LLVM IR.
//! The main implementation is in emit.zig, but this provides reference documentation
//! and utility functions for working with types during code generation.
//!
//! ## Klar to LLVM Type Mapping
//!
//! | Klar Type    | LLVM Type          | Notes                              |
//! |--------------|--------------------|------------------------------------|
//! | bool         | i1                 |                                    |
//! | i8/u8        | i8                 |                                    |
//! | i16/u16      | i16                |                                    |
//! | i32/u32      | i32                |                                    |
//! | i64/u64      | i64                |                                    |
//! | i128/u128    | i128               |                                    |
//! | isize/usize  | i64                | Platform-sized (64-bit)            |
//! | f32          | float              |                                    |
//! | f64          | double             |                                    |
//! | char         | i32                | Unicode codepoint                  |
//! | string       | {ptr, i32, i32}    | String struct                      |
//! | void         | void               |                                    |
//! | ?T           | {i1, T}            | Optional: has_value + payload      |
//! | Result[T,E]  | {i1, max(T,E)}     | Result: is_ok + payload            |
//! | [T; N]       | [N x T]            | Fixed array                        |
//! | [T]          | {ptr, i64}         | Slice                              |
//! | &T           | ptr                | Reference (opaque pointer)         |
//! | Rc[T]        | ptr                | Reference-counted pointer          |
//! | Arc[T]       | ptr                | Atomic reference-counted pointer   |
//! | struct {...} | {field1, field2..} | LLVM struct                        |
//! | enum {...}   | {i8, [N x i8]}     | Tagged union                       |
//! | fn(T) -> R   | {ptr, ptr}         | Closure: fn_ptr + env_ptr         |
//!
//! ## Collection Types
//!
//! | Klar Type    | LLVM Struct Layout                                    |
//! |--------------|-------------------------------------------------------|
//! | String       | {ptr data, i32 len, i32 capacity}                     |
//! | List[T]      | {ptr data, i32 len, i32 capacity}                     |
//! | Map[K,V]     | {ptr entries, i32 len, i32 cap, i32 tombstones}       |
//! | Set[T]       | {ptr entries, i32 len, i32 cap, i32 tombstones}       |
//!
//! ## Key Functions in emit.zig
//!
//! - `typeExprToLLVM`: Converts AST type expression to LLVM type
//! - `typeToLLVM`: Converts checker Type to LLVM type
//! - `namedTypeToLLVM`: Converts type name string to LLVM type
//! - `getOrCreateStructType`: Creates/retrieves LLVM struct type
//! - `isTypeSigned`: Checks if a type is signed integer
//! - `getLLVMTypeSize`: Gets size of LLVM type in bytes
//! - `getLLVMTypeAlignment`: Gets alignment of LLVM type
//! - `requiresSret`: Checks if type needs struct-return ABI

const std = @import("std");
const llvm = @import("llvm.zig");

/// Size in bytes for Klar primitive types.
pub const TypeSize = struct {
    pub const bool_ = 1;
    pub const i8_ = 1;
    pub const u8_ = 1;
    pub const i16_ = 2;
    pub const u16_ = 2;
    pub const i32_ = 4;
    pub const u32_ = 4;
    pub const i64_ = 8;
    pub const u64_ = 8;
    pub const i128_ = 16;
    pub const u128_ = 16;
    pub const isize_ = 8;
    pub const usize_ = 8;
    pub const f32_ = 4;
    pub const f64_ = 8;
    pub const char_ = 4;
    pub const ptr_ = 8;
    pub const string_ = 16; // ptr + len + cap
};

/// Check if a primitive type name represents a signed integer.
pub fn isSignedTypeName(name: []const u8) bool {
    // Unsigned types start with 'u'
    if (name.len > 0 and name[0] == 'u') {
        return false;
    }
    // i8, i16, i32, i64, i128, isize are signed
    if (name.len > 0 and name[0] == 'i') {
        return true;
    }
    // f32, f64 are "signed" for comparison purposes
    if (std.mem.eql(u8, name, "f32") or std.mem.eql(u8, name, "f64")) {
        return true;
    }
    return true; // Default to signed for safety
}

/// Get the size in bits for a primitive type name.
pub fn getPrimitiveBitSize(name: []const u8) ?u32 {
    const map = std.ComptimeStringMap(u32, .{
        .{ "bool", 1 },
        .{ "i8", 8 },
        .{ "u8", 8 },
        .{ "i16", 16 },
        .{ "u16", 16 },
        .{ "i32", 32 },
        .{ "u32", 32 },
        .{ "i64", 64 },
        .{ "u64", 64 },
        .{ "i128", 128 },
        .{ "u128", 128 },
        .{ "isize", 64 },
        .{ "usize", 64 },
        .{ "f32", 32 },
        .{ "f64", 64 },
        .{ "char", 32 },
    });
    return map.get(name);
}

/// Create LLVM type for a primitive type name.
pub fn primitiveToLLVM(ctx: llvm.Context, name: []const u8) ?llvm.TypeRef {
    if (std.mem.eql(u8, name, "bool")) return llvm.Types.int1(ctx);
    if (std.mem.eql(u8, name, "i8")) return llvm.Types.int8(ctx);
    if (std.mem.eql(u8, name, "u8")) return llvm.Types.int8(ctx);
    if (std.mem.eql(u8, name, "i16")) return llvm.Types.int16(ctx);
    if (std.mem.eql(u8, name, "u16")) return llvm.Types.int16(ctx);
    if (std.mem.eql(u8, name, "i32")) return llvm.Types.int32(ctx);
    if (std.mem.eql(u8, name, "u32")) return llvm.Types.int32(ctx);
    if (std.mem.eql(u8, name, "i64")) return llvm.Types.int64(ctx);
    if (std.mem.eql(u8, name, "u64")) return llvm.Types.int64(ctx);
    if (std.mem.eql(u8, name, "i128")) return llvm.c.LLVMInt128TypeInContext(ctx.ref);
    if (std.mem.eql(u8, name, "u128")) return llvm.c.LLVMInt128TypeInContext(ctx.ref);
    if (std.mem.eql(u8, name, "isize")) return llvm.Types.int64(ctx);
    if (std.mem.eql(u8, name, "usize")) return llvm.Types.int64(ctx);
    if (std.mem.eql(u8, name, "f32")) return llvm.Types.float_(ctx);
    if (std.mem.eql(u8, name, "f64")) return llvm.Types.double_(ctx);
    if (std.mem.eql(u8, name, "char")) return llvm.Types.int32(ctx);
    return null;
}

test "isSignedTypeName" {
    try std.testing.expect(isSignedTypeName("i32"));
    try std.testing.expect(isSignedTypeName("i64"));
    try std.testing.expect(!isSignedTypeName("u32"));
    try std.testing.expect(!isSignedTypeName("u64"));
    try std.testing.expect(!isSignedTypeName("usize"));
    try std.testing.expect(isSignedTypeName("isize"));
}

test "getPrimitiveBitSize" {
    try std.testing.expectEqual(@as(?u32, 1), getPrimitiveBitSize("bool"));
    try std.testing.expectEqual(@as(?u32, 8), getPrimitiveBitSize("i8"));
    try std.testing.expectEqual(@as(?u32, 32), getPrimitiveBitSize("i32"));
    try std.testing.expectEqual(@as(?u32, 64), getPrimitiveBitSize("i64"));
    try std.testing.expectEqual(@as(?u32, null), getPrimitiveBitSize("unknown"));
}
