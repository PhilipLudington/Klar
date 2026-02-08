//! Type utility functions.
//!
//! This module contains helper functions for type checking operations:
//! type compatibility, FFI validation, assignability, and mutability checks.
//!
//! Functions receive a type checker instance via `anytype` to avoid
//! circular imports while maintaining type safety through duck typing.

const std = @import("std");
const ast = @import("../ast.zig");
const types = @import("../types.zig");
const Type = types.Type;

// ============================================================================
// Boolean Type Check
// ============================================================================

/// Check if a type is the boolean primitive type.
pub fn isBoolType(tc: anytype, t: Type) bool {
    _ = tc;
    return t == .primitive and t.primitive == .bool_;
}

// ============================================================================
// FFI Compatibility
// ============================================================================

/// Check if a type is FFI-compatible (can be used in extern structs and extern functions).
/// FFI-compatible types are:
/// - Primitive types (i8, i16, i32, i64, u8, u16, u32, u64, isize, usize, f32, f64, bool)
/// - CPtr[T] and COptPtr[T] (raw pointers)
/// - CStr (null-terminated C strings)
/// - Sized extern types (extern type(N) Name)
/// - Other extern structs
/// - Extern enums (C-compatible enum layout)
///
/// Note: Unsized/opaque extern types (extern type Name) are NOT FFI-compatible
/// for direct use - they must be used behind pointers (CPtr[T] or COptPtr[T]).
pub fn isFfiCompatibleType(tc: anytype, t: Type) bool {
    _ = tc;
    return switch (t) {
        .primitive => true,
        .cptr => true,
        .copt_ptr => true,
        .cstr => true,
        .cstr_owned => true, // Owned C strings are also FFI-compatible
        .extern_type => |et| et.size != null, // Only sized extern types can be used directly
        .struct_ => |s| s.is_extern,
        .enum_ => |e| e.is_extern,
        .extern_fn => true, // C function pointers are FFI-compatible
        else => false,
    };
}

/// Check if a type is an unsized extern type (opaque type that must be behind a pointer).
pub fn isUnsizedExternType(tc: anytype, t: Type) bool {
    _ = tc;
    return t == .extern_type and t.extern_type.size == null;
}

// ============================================================================
// Integer Type Utilities
// ============================================================================

/// Check if a primitive type is an integer type (suitable for extern enum repr).
pub fn isIntegerPrimitive(tc: anytype, prim: types.Primitive) bool {
    _ = tc;
    return switch (prim) {
        .i8_, .i16_, .i32_, .i64_,
        .u8_, .u16_, .u32_, .u64_,
        .isize_, .usize_ => true,
        else => false,
    };
}

/// Check if an i128 value fits within the range of a primitive integer type.
pub fn valueInRange(tc: anytype, value: i128, prim: types.Primitive) bool {
    _ = tc;
    return switch (prim) {
        .i8_ => value >= -128 and value <= 127,
        .i16_ => value >= -32768 and value <= 32767,
        .i32_ => value >= -2147483648 and value <= 2147483647,
        .i64_ => value >= -9223372036854775808 and value <= 9223372036854775807,
        .u8_ => value >= 0 and value <= 255,
        .u16_ => value >= 0 and value <= 65535,
        .u32_ => value >= 0 and value <= 4294967295,
        .u64_ => value >= 0 and value <= 18446744073709551615,
        // For isize/usize, assume 64-bit platform
        .isize_ => value >= -9223372036854775808 and value <= 9223372036854775807,
        .usize_ => value >= 0 and value <= 18446744073709551615,
        else => false,
    };
}

// ============================================================================
// Assignability and Mutability
// ============================================================================

/// Check if an expression is a valid assignment target (lvalue).
/// Valid targets: identifiers, index expressions, field access, dereferences.
pub fn isAssignable(tc: anytype, expr: ast.Expr) bool {
    _ = tc;
    return switch (expr) {
        .identifier => true,
        .index => true,
        .field => true,
        .unary => |u| u.op == .deref,
        else => false,
    };
}

/// Check if an expression refers to a mutable location.
/// Recursively checks through field access and indexing.
pub fn isMutable(tc: anytype, expr: ast.Expr) bool {
    switch (expr) {
        .identifier => |id| {
            if (tc.current_scope.lookup(id.name)) |sym| {
                return sym.mutable;
            }
            return false;
        },
        .field => |f| return isMutable(tc, f.object),
        .index => |i| return isMutable(tc, i.object),
        .unary => |u| {
            if (u.op == .deref) {
                const operand_type = tc.checkExpr(u.operand);
                if (operand_type == .reference) {
                    return operand_type.reference.mutable;
                }
            }
            return false;
        },
        else => return false,
    }
}

// ============================================================================
// Type Compatibility
// ============================================================================

/// Check if a value type can be assigned to a target type.
/// Delegates to isTypeCompatible.
pub fn checkAssignmentCompatible(tc: anytype, target: Type, value: Type) bool {
    return isTypeCompatible(tc, target, value);
}

/// Check if a value type is compatible with a target type.
/// This allows:
/// - Exact type equality
/// - Unknown types (for error recovery)
/// - Array to slice coercion (array[T, N] -> slice[T])
pub fn isTypeCompatible(tc: anytype, target: Type, value: Type) bool {
    _ = tc;

    // Exact match
    if (target.eql(value)) return true;

    // Unknown types (error recovery)
    if (target == .unknown or value == .unknown) return true;

    // Array to slice coercion: [T; N] can be assigned to [T]
    if (target == .slice and value == .array) {
        return target.slice.element.eql(value.array.element);
    }

    return false;
}
