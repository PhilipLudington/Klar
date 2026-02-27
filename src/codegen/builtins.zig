//! Built-in function helper utilities for codegen.
//!
//! Provides name constants and predicates for Klar's built-in functions.
//! The emission implementation (emitPrint, emitPanic, emitAssert, etc.)
//! remains in emit.zig.
//!
//! ## Provided by this module
//!
//! - `BuiltinName`: String constants for all built-in function names
//! - `isBuiltin`: Check if a function name is a built-in
//! - `isPrintBuiltin`: Check if a built-in is a print function
//! - `isAssertBuiltin`: Check if a built-in is an assertion
//!
//! ## Built-in Functions (reference)
//!
//! Output: `print`, `println`, `debug`
//! Assertions: `assert`, `assert_eq`, `assert_ne`, `assert_ok`, `assert_err`, `assert_some`, `assert_none`
//! Control: `panic`
//! Introspection: `type_name`, `len`
//! String: `from_byte`, `parse_int`, `parse_float`
//! Comptime: `repeat`, `comptime_print`
//! Environment: `env_get`, `env_set`
//! Filesystem: `fs_stat`
//! Process: `process_run`, `timestamp_now`

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
    pub const assert_ok = "assert_ok";
    pub const assert_err = "assert_err";
    pub const assert_some = "assert_some";
    pub const assert_none = "assert_none";
    pub const panic = "panic";
    pub const type_name = "type_name";
    pub const len = "len";
    pub const repeat = "repeat";
    pub const comptime_print = "comptime_print";
    pub const from_byte = "from_byte";
    pub const parse_int = "parse_int";
    pub const parse_float = "parse_float";
    // Phase 0: environment, process, stat, timestamp
    pub const env_get = "env_get";
    pub const env_set = "env_set";
    pub const fs_stat = "fs_stat";
    pub const timestamp_now = "timestamp_now";
    pub const process_run = "process_run";
};

/// Check if a function name is a built-in.
pub fn isBuiltin(name: []const u8) bool {
    return std.mem.eql(u8, name, BuiltinName.print) or
        std.mem.eql(u8, name, BuiltinName.println) or
        std.mem.eql(u8, name, BuiltinName.debug) or
        std.mem.eql(u8, name, BuiltinName.assert_) or
        std.mem.eql(u8, name, BuiltinName.assert_eq) or
        std.mem.eql(u8, name, BuiltinName.assert_ne) or
        std.mem.eql(u8, name, BuiltinName.assert_ok) or
        std.mem.eql(u8, name, BuiltinName.assert_err) or
        std.mem.eql(u8, name, BuiltinName.assert_some) or
        std.mem.eql(u8, name, BuiltinName.assert_none) or
        std.mem.eql(u8, name, BuiltinName.panic) or
        std.mem.eql(u8, name, BuiltinName.type_name) or
        std.mem.eql(u8, name, BuiltinName.len) or
        std.mem.eql(u8, name, BuiltinName.repeat) or
        std.mem.eql(u8, name, BuiltinName.comptime_print) or
        std.mem.eql(u8, name, BuiltinName.from_byte) or
        std.mem.eql(u8, name, BuiltinName.parse_int) or
        std.mem.eql(u8, name, BuiltinName.parse_float) or
        std.mem.eql(u8, name, BuiltinName.env_get) or
        std.mem.eql(u8, name, BuiltinName.env_set) or
        std.mem.eql(u8, name, BuiltinName.fs_stat) or
        std.mem.eql(u8, name, BuiltinName.timestamp_now) or
        std.mem.eql(u8, name, BuiltinName.process_run);
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
        std.mem.eql(u8, name, BuiltinName.assert_ne) or
        std.mem.eql(u8, name, BuiltinName.assert_ok) or
        std.mem.eql(u8, name, BuiltinName.assert_err) or
        std.mem.eql(u8, name, BuiltinName.assert_some) or
        std.mem.eql(u8, name, BuiltinName.assert_none);
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
