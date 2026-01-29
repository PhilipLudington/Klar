//! Expression emission utilities for codegen.
//!
//! This module documents the expression emission logic in code generation.
//!
//! ## Expression Types
//!
//! Klar expressions that generate LLVM IR:
//!
//! | Expression      | Example              | Notes                    |
//! |-----------------|----------------------|--------------------------|
//! | Literal         | `42`, `"hello"`      | Constants                |
//! | Identifier      | `x`                  | Variable reference       |
//! | Binary          | `a + b`              | Binary operations        |
//! | Unary           | `-x`, `!b`           | Unary operations         |
//! | Call            | `foo(1, 2)`          | Function call            |
//! | MethodCall      | `x.foo()`            | Method invocation        |
//! | FieldAccess     | `point.x`            | Struct field access      |
//! | IndexAccess     | `arr[i]`             | Array/collection index   |
//! | TypeCast        | `x.as[i64]`          | Type conversion          |
//! | StructLiteral   | `Point { x: 1 }`     | Struct construction      |
//! | ArrayLiteral    | `[1, 2, 3]`          | Array construction       |
//! | EnumLiteral     | `Color::Red`         | Enum variant             |
//! | Range           | `0..10`              | Range literal            |
//! | Closure         | `|x| { x + 1 }`      | Lambda expression        |
//!
//! ## Binary Operators
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
//! | `=`      | Assignment      | Modifies lvalue                |
//!
//! ## Key Functions in emit.zig
//!
//! - `emitExpr`: Main dispatch for all expression types
//! - `emitLiteral`: Integer, float, bool, string literals
//! - `emitIdentifier`: Variable/parameter reference
//! - `emitBinary`: Binary operations with operator dispatch
//! - `emitUnary`: Unary operations (negation, not, deref)
//! - `emitCall`: Regular function calls
//! - `emitMethodCall`: Method calls with receiver
//! - `emitFieldAccess`: Struct field access
//! - `emitIndexAccess`: Array/slice/collection indexing
//! - `emitTypeCast`: Type conversions (.as, .to, .trunc)
//! - `emitStructLiteral`: Struct value construction
//! - `emitArrayLiteral`: Array value construction
//! - `emitEnumLiteral`: Enum variant construction
//! - `emitRangeLiteral`: Range value construction
//! - `emitClosure`: Closure/lambda construction
//!
//! ## Overflow Operations
//!
//! - `emitCheckedAdd/Sub/Mul`: Panic on overflow
//! - `emitSaturatingAdd/Sub/Mul`: Clamp to type limits

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
