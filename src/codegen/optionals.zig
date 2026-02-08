//! Optional and Result helper utilities for codegen.
//!
//! Provides constants, type constructors, and predicates for Optional[T]
//! and Result[T, E] code generation. The emission implementation
//! (emitSome, emitNone, emitOk, emitErr, etc.) remains in emit.zig.
//!
//! ## Provided by this module
//!
//! - `OptionalField` / `ResultField`: Struct field index constants
//! - `createOptionalType`: Build LLVM type for Optional[T]
//! - `createResultType`: Build LLVM type for Result[T, E]
//! - `createResultStringErrorType`: Build Result[T, String] type
//! - `isSomeValue` / `isOkValue`: Tag predicates
//!
//! ## Optional[T] Layout
//!
//! ```
//! struct Optional[T] {
//!     has_value: i1,  // Tag: 0=None, 1=Some
//!     value: T,       // Payload (undefined if None)
//! }
//! ```
//!
//! ## Result[T, E] Layout
//!
//! ```
//! struct Result[T, E] {
//!     is_ok: i1,           // Tag: 0=Err, 1=Ok
//!     payload: [max(sizeof(T), sizeof(E)) x i8],
//! }
//! ```

const std = @import("std");
const llvm = @import("llvm.zig");

/// Optional struct field indices.
pub const OptionalField = struct {
    pub const has_value = 0;
    pub const value = 1;
};

/// Result struct field indices.
pub const ResultField = struct {
    pub const is_ok = 0;
    pub const payload = 1;
};

/// Create the LLVM type for Optional[T].
pub fn createOptionalType(ctx: llvm.Context, inner_type: llvm.TypeRef) llvm.TypeRef {
    var fields = [_]llvm.TypeRef{
        llvm.Types.int1(ctx), // has_value flag
        inner_type, // payload
    };
    return llvm.Types.struct_(ctx, &fields, false);
}

/// Create the LLVM type for Result[T, E] with payload size for the larger of T or E.
/// Requires a module reference for target data layout (used to compute ABI type sizes).
pub fn createResultType(module: llvm.Module, ctx: llvm.Context, ok_type: llvm.TypeRef, err_type: llvm.TypeRef) llvm.TypeRef {
    const target_data = llvm.c.LLVMGetModuleDataLayout(module.ref);
    const ok_size = llvm.c.LLVMABISizeOfType(target_data, ok_type);
    const err_size = llvm.c.LLVMABISizeOfType(target_data, err_type);

    const payload_type = if (ok_size >= err_size) ok_type else err_type;

    var fields = [_]llvm.TypeRef{
        llvm.Types.int1(ctx), // is_ok flag
        payload_type, // payload (sized for larger of T, E)
    };
    return llvm.Types.struct_(ctx, &fields, false);
}

/// Standard Result type with String error.
pub fn createResultStringErrorType(module: llvm.Module, ctx: llvm.Context, ok_type: llvm.TypeRef) llvm.TypeRef {
    // String struct: { ptr, i32, i32 } = 16 bytes
    var string_fields = [_]llvm.TypeRef{
        llvm.Types.pointer(ctx),
        llvm.Types.int32(ctx),
        llvm.Types.int32(ctx),
    };
    const string_type = llvm.Types.struct_(ctx, &string_fields, false);

    return createResultType(module, ctx, ok_type, string_type);
}

/// Check if a value represents Some (has_value == 1).
pub fn isSomeValue(has_value: u64) bool {
    return has_value != 0;
}

/// Check if a value represents Ok (is_ok == 1).
pub fn isOkValue(is_ok: u64) bool {
    return is_ok != 0;
}

test "isSomeValue" {
    try std.testing.expect(!isSomeValue(0));
    try std.testing.expect(isSomeValue(1));
}

test "isOkValue" {
    try std.testing.expect(!isOkValue(0));
    try std.testing.expect(isOkValue(1));
}
