//! Function helper utilities for codegen.
//!
//! Provides constants, predicates, and type constructors for function and
//! closure emission. The emission implementation (declareFunction, emitFunction,
//! emitClosure, etc.) remains in emit.zig.
//!
//! ## Provided by this module
//!
//! - `FnAttr`: LLVM function attribute name constants
//! - `getAttributeKind`: Look up LLVM attribute kind by name
//! - `sret_threshold_bytes` / `requiresSretForSize`: Struct-return ABI check
//! - `ClosureField`: Closure struct field index constants
//! - `createClosureType`: Build the LLVM type for a closure (fn_ptr + env_ptr)

const std = @import("std");
const llvm = @import("llvm.zig");

/// Function attribute types.
pub const FnAttr = struct {
    pub const noreturn_attr = "noreturn";
    pub const nounwind_attr = "nounwind";
    pub const readonly_attr = "readonly";
    pub const sret_attr = "sret";
    pub const noalias_attr = "noalias";
};

/// Get LLVM attribute kind for a named attribute.
pub fn getAttributeKind(name: []const u8) c_uint {
    return llvm.c.LLVMGetEnumAttributeKindForName(name.ptr, name.len);
}

/// Platform threshold for sret (struct return via pointer).
/// Structs larger than this are returned via hidden first parameter.
pub const sret_threshold_bytes: usize = 16;

/// Check if a struct size requires sret calling convention.
pub fn requiresSretForSize(size: usize) bool {
    return size > sret_threshold_bytes;
}

/// Closure struct field indices.
pub const ClosureField = struct {
    pub const fn_ptr = 0;
    pub const env_ptr = 1;
};

/// Create the LLVM type for a closure struct.
pub fn createClosureType(ctx: llvm.Context) llvm.TypeRef {
    var fields = [_]llvm.TypeRef{
        llvm.Types.pointer(ctx), // function pointer
        llvm.Types.pointer(ctx), // environment pointer
    };
    return llvm.Types.struct_(ctx, &fields, false);
}

test "requiresSretForSize" {
    try std.testing.expect(!requiresSretForSize(8));
    try std.testing.expect(!requiresSretForSize(16));
    try std.testing.expect(requiresSretForSize(17));
    try std.testing.expect(requiresSretForSize(32));
}
