//! Compile-time evaluation support for the type checker.
//!
//! This module provides types for representing compile-time evaluated values.
//! The main evaluation logic is in checker.zig.
//!
//! ## Key Concepts
//!
//! - **Comptime blocks**: `comptime { ... }` expressions evaluated at compile time
//! - **Comptime functions**: `comptime fn foo() { ... }` functions evaluated at compile time
//! - **Constant folding**: Evaluating constant expressions during type checking
//!
//! ## Supported Comptime Values
//!
//! | Type | Description |
//! |------|-------------|
//! | int | Integer with type info (i32 or i64) |
//! | float | Floating point (f64) |
//! | bool_ | Boolean |
//! | string | String literal |
//! | void_ | Void/unit value |
//! | struct_ | Struct value with fields |
//! | array | Array with elements |
//!
//! ## Comptime Function Evaluation Flow
//!
//! 1. Function marked as `comptime fn`
//! 2. Registered in `comptime_functions` map during signature checking
//! 3. When called:
//!    a. Verify all arguments are comptime-known
//!    b. Create/reuse shared interpreter
//!    c. Execute function body
//!    d. Convert result to ComptimeValue
//!    e. Store in `comptime_call_values` for codegen
//!
//! ## Builtin Comptime Functions
//!
//! | Builtin | Returns | Description |
//! |---------|---------|-------------|
//! | `@typeName(T)` | string | Type name as string |
//! | `@typeInfo(T)` | string | Type kind ("struct", "enum", etc.) |
//! | `@fields(T)` | string | Comma-separated field/variant names |
//! | `@compileError(msg)` | never | Emit compile error |
//! | `@assert(cond, msg?)` | void | Compile-time assertion |
//! | `@repeat(val, n)` | [T; n] | Create array of repeated value |
//! | `@sizeOf(T)` | i64 | Size of type in bytes |
//! | `@alignOf(T)` | i64 | Alignment of type |
//! | `@hasField(T, name)` | bool | Check if struct has field |
//! | `@fn_ptr(fn)` | extern fn | Convert to C function pointer |

const std = @import("std");
const types = @import("../types.zig");
const Type = types.Type;
const ast = @import("../ast.zig");

/// Represents a compile-time evaluated value.
pub const ComptimeValue = union(enum) {
    /// Integer with type info preserved
    int: struct {
        value: i64,
        is_i32: bool, // true for i32, false for i64
    },
    float: f64,
    bool_: bool,
    string: []const u8,
    void_,
    /// Struct value with type name and field values (preserves order)
    struct_: ComptimeStruct,
    /// Array value with element type and elements
    array: ComptimeArray,
};

/// A compile-time struct value.
pub const ComptimeStruct = struct {
    type_name: []const u8,
    /// Fields stored in declaration order using ArrayHashMap
    fields: std.StringArrayHashMapUnmanaged(ComptimeValue),
};

/// A compile-time array value.
pub const ComptimeArray = struct {
    /// Element type for codegen
    element_type: Type,
    /// Array elements in order
    elements: []const ComptimeValue,
};

/// Information for @repeat(value, count) builtin.
/// Stores the element type and count for codegen.
pub const RepeatInfo = struct {
    /// The type of the repeated element
    element_type: Type,
    /// The number of times to repeat (comptime-known)
    count: usize,
    /// The value expression to repeat
    value_expr: ast.Expr,
};

/// Maximum recursion depth for comptime functions.
pub const max_comptime_depth: u32 = 1000;
