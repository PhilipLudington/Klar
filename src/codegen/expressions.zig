//! Expression helper utilities for codegen.
//!
//! Provides enums and predicate conversion functions for expression emission.
//! The emission implementation (emitExpr, emitBinary, emitCall, etc.)
//! remains in emit.zig.
//!
//! ## Provided by this module
//!
//! - `ArithOp`: Arithmetic operation enum (add, sub, mul)
//! - `CompareOp`: Comparison operation enum (eq, ne, lt, le, gt, ge)
//! - `compareOpToSignedPredicate`: Convert to LLVM signed int predicate
//! - `compareOpToUnsignedPredicate`: Convert to LLVM unsigned int predicate
//! - `compareOpToFloatPredicate`: Convert to LLVM ordered float predicate
//!
//! ## Binary Operators (reference)
//!
//! | Operator | Operation       | Notes                          |
//! |----------|-----------------|--------------------------------|
//! | `+`      | Add             | Checked overflow (panics)      |
//! | `-`      | Subtract        | Checked overflow               |
//! | `*`      | Multiply        | Checked overflow               |
//! | `/`      | Divide          | Panic on division by zero      |
//! | `%`      | Modulo          | Panic on division by zero      |
//! | `+%`     | Wrapping add    | No overflow check              |
//! | `-%`     | Wrapping sub    | No overflow check              |
//! | `*%`     | Wrapping mul    | No overflow check              |
//! | `+\|`    | Saturating add  | Clamp at min/max               |
//! | `-\|`    | Saturating sub  | Clamp at min/max               |
//! | `*\|`    | Saturating mul  | Clamp at min/max               |
//! | `==`     | Equal           |                                |
//! | `!=`     | Not equal       |                                |
//! | `<`      | Less than       |                                |
//! | `<=`     | Less or equal   |                                |
//! | `>`      | Greater than    |                                |
//! | `>=`     | Greater or eq   |                                |
//! | `and`    | Logical and     | Short-circuit evaluation       |
//! | `or`     | Logical or      | Short-circuit evaluation       |

const std = @import("std");
const llvm = @import("llvm.zig");

/// Arithmetic operation type for overflow handling.
pub const ArithOp = enum {
    add,
    sub,
    mul,
};

/// Binary comparison operations.
pub const CompareOp = enum {
    eq,
    ne,
    lt,
    le,
    gt,
    ge,
};

/// Convert comparison op to LLVM signed integer predicate.
pub fn compareOpToSignedPredicate(op: CompareOp) c_uint {
    return switch (op) {
        .eq => llvm.c.LLVMIntEQ,
        .ne => llvm.c.LLVMIntNE,
        .lt => llvm.c.LLVMIntSLT,
        .le => llvm.c.LLVMIntSLE,
        .gt => llvm.c.LLVMIntSGT,
        .ge => llvm.c.LLVMIntSGE,
    };
}

/// Convert comparison op to LLVM unsigned integer predicate.
pub fn compareOpToUnsignedPredicate(op: CompareOp) c_uint {
    return switch (op) {
        .eq => llvm.c.LLVMIntEQ,
        .ne => llvm.c.LLVMIntNE,
        .lt => llvm.c.LLVMIntULT,
        .le => llvm.c.LLVMIntULE,
        .gt => llvm.c.LLVMIntUGT,
        .ge => llvm.c.LLVMIntUGE,
    };
}

/// Convert comparison op to LLVM float predicate (ordered).
pub fn compareOpToFloatPredicate(op: CompareOp) c_uint {
    return switch (op) {
        .eq => llvm.c.LLVMRealOEQ,
        .ne => llvm.c.LLVMRealONE,
        .lt => llvm.c.LLVMRealOLT,
        .le => llvm.c.LLVMRealOLE,
        .gt => llvm.c.LLVMRealOGT,
        .ge => llvm.c.LLVMRealOGE,
    };
}

test "compareOpToSignedPredicate" {
    try std.testing.expectEqual(llvm.c.LLVMIntSLT, compareOpToSignedPredicate(.lt));
    try std.testing.expectEqual(llvm.c.LLVMIntEQ, compareOpToSignedPredicate(.eq));
}
