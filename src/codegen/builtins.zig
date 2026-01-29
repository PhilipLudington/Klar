//! Built-in function emission utilities for codegen.
//!
//! This module documents the built-in functions available in Klar
//! and their code generation.
//!
//! ## Output Functions
//!
//! - `print(arg)`: Print any value to stdout (with newline)
//! - `println(arg)`: Alias for print
//! - `debug(arg)`: Print debug representation
//!
//! ## Assertions
//!
//! - `assert(condition)`: Panic if condition is false
//! - `assert_eq(a, b)`: Panic if a != b
//! - `assert_ne(a, b)`: Panic if a == b
//!
//! ## Panic
//!
//! - `panic(message)`: Abort with message
//!
//! ## Type Information
//!
//! - `type_name[T]()`: Get string name of type
//! - `len(collection)`: Get length (works on String, Array, List, etc.)
//!
//! ## Collection Utilities
//!
//! - `repeat(value, count)`: Create array of repeated value (comptime)
//!
//! ## Comptime Functions
//!
//! - `comptime_print(...)`: Print at compile time (debugging)
//!
//! ## Key Functions in emit.zig
//!
//! - `emitBuiltinCall`: Dispatch to specific builtin handler
//! - `emitBuiltinFnPtr`: Get function pointer for builtin
//! - `emitPrint`: Generate print code for any type
//! - `emitPanic`: Generate panic/abort code
//! - `emitAssert`: Generate assertion code
//! - `emitAssertEq`: Generate equality assertion
//! - `emitAssertNe`: Generate inequality assertion
//! - `emitTypeName`: Generate type name string
//! - `emitLen`: Generate length accessor
//! - `emitRepeat`: Generate array repetition (comptime)

const std = @import("std");
const llvm = @import("llvm.zig");

/// Built-in function names.
pub const BuiltinName = struct {
    pub const print = "print";
    pub const println = "println";
    pub const debug = "debug";
    pub const assert_ = "assert";
    pub const assert_eq = "assert_eq";
    pub const assert_ne = "assert_ne";
    pub const panic = "panic";
    pub const type_name = "type_name";
    pub const len = "len";
    pub const repeat = "repeat";
    pub const comptime_print = "comptime_print";
};

/// Check if a function name is a built-in.
pub fn isBuiltin(name: []const u8) bool {
    return std.mem.eql(u8, name, BuiltinName.print) or
        std.mem.eql(u8, name, BuiltinName.println) or
        std.mem.eql(u8, name, BuiltinName.debug) or
        std.mem.eql(u8, name, BuiltinName.assert_) or
        std.mem.eql(u8, name, BuiltinName.assert_eq) or
        std.mem.eql(u8, name, BuiltinName.assert_ne) or
        std.mem.eql(u8, name, BuiltinName.panic) or
        std.mem.eql(u8, name, BuiltinName.type_name) or
        std.mem.eql(u8, name, BuiltinName.len) or
        std.mem.eql(u8, name, BuiltinName.repeat) or
        std.mem.eql(u8, name, BuiltinName.comptime_print);
}

/// Check if a built-in is a print function.
pub fn isPrintBuiltin(name: []const u8) bool {
    return std.mem.eql(u8, name, BuiltinName.print) or
        std.mem.eql(u8, name, BuiltinName.println);
}

/// Check if a built-in is an assertion.
pub fn isAssertBuiltin(name: []const u8) bool {
    return std.mem.eql(u8, name, BuiltinName.assert_) or
        std.mem.eql(u8, name, BuiltinName.assert_eq) or
        std.mem.eql(u8, name, BuiltinName.assert_ne);
}

test "isBuiltin" {
    try std.testing.expect(isBuiltin("print"));
    try std.testing.expect(isBuiltin("assert"));
    try std.testing.expect(isBuiltin("panic"));
    try std.testing.expect(!isBuiltin("foo"));
}

test "isPrintBuiltin" {
    try std.testing.expect(isPrintBuiltin("print"));
    try std.testing.expect(isPrintBuiltin("println"));
    try std.testing.expect(!isPrintBuiltin("debug"));
}
