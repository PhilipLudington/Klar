//! Builtin function support for the type checker.
//!
//! This module documents the builtin functions available in Klar.
//! The main checking logic is in checker.zig.
//!
//! ## Builtin Functions
//!
//! These are always available without import:
//!
//! | Function | Signature | Description |
//! |----------|-----------|-------------|
//! | `print` | `fn(string)` | Print without newline |
//! | `println` | `fn(string)` | Print with newline |
//! | `readline` | `fn() -> string` | Read line from stdin |
//! | `assert` | `fn(bool)` | Runtime assertion |
//! | `panic` | `fn(string) -> !` | Panic with message |
//! | `assert_eq` | `fn(T, T)` | Assert equality |
//! | `dbg` | `fn(T) -> T` | Debug print and return |
//! | `debug` | `fn(T) -> string` | Debug string representation |
//! | `type_name` | `fn(T) -> string` | Type name as string |
//! | `len` | `fn(T) -> i32` | Length of string/array |
//!
//! ## Result/Option Constructors
//!
//! | Function | Description |
//! |----------|-------------|
//! | `Ok(T)` | Create success Result |
//! | `Err(E)` | Create error Result |
//! | `Some(T)` | Create present Optional |
//! | `None` | Create absent Optional |
//!
//! ## I/O Functions
//!
//! | Function | Returns | Description |
//! |----------|---------|-------------|
//! | `stdout()` | `Stdout` | Get stdout handle |
//! | `stderr()` | `Stderr` | Get stderr handle |
//! | `stdin()` | `Stdin` | Get stdin handle |
//!
//! ## Filesystem Functions
//!
//! | Function | Signature | Description |
//! |----------|-----------|-------------|
//! | `fs_exists` | `fn(string) -> bool` | Check if path exists |
//! | `fs_is_file` | `fn(string) -> bool` | Check if path is file |
//! | `fs_is_dir` | `fn(string) -> bool` | Check if path is directory |
//! | `fs_create_dir` | `fn(string) -> Result[void, IoError]` | Create directory |
//! | `fs_create_dir_all` | `fn(string) -> Result[void, IoError]` | Create directory tree |
//! | `fs_remove_file` | `fn(string) -> Result[void, IoError]` | Remove file |
//! | `fs_remove_dir` | `fn(string) -> Result[void, IoError]` | Remove directory |
//! | `fs_read_string` | `fn(string) -> Result[String, IoError]` | Read file contents |
//! | `fs_write_string` | `fn(string, string) -> Result[void, IoError]` | Write file contents |
//! | `fs_read_dir` | `fn(string) -> Result[List[String], IoError]` | List directory |
//!
//! ## Comptime Builtins
//!
//! These are compile-time evaluated `@builtin(...)` calls:
//!
//! | Builtin | Returns | Description |
//! |---------|---------|-------------|
//! | `@typeName(T)` | string | Type name as string |
//! | `@typeInfo(T)` | string | Type kind |
//! | `@fields(T)` | string | Field/variant names |
//! | `@compileError(msg)` | never | Compile error |
//! | `@assert(cond)` | void | Compile-time assertion |
//! | `@repeat(val, n)` | [T; n] | Repeated array |
//! | `@sizeOf(T)` | i64 | Type size |
//! | `@alignOf(T)` | i64 | Type alignment |
//! | `@hasField(T, name)` | bool | Has field check |
//! | `@fn_ptr(fn)` | extern fn | Function pointer |

const std = @import("std");
const types = @import("../types.zig");
const Type = types.Type;

/// Builtin type information.
pub const BuiltinType = struct {
    name: []const u8,
    primitive: types.Primitive,
};

/// All primitive types registered as builtins.
pub const builtin_types: []const BuiltinType = &.{
    .{ .name = "i8", .primitive = .i8_ },
    .{ .name = "i16", .primitive = .i16_ },
    .{ .name = "i32", .primitive = .i32_ },
    .{ .name = "i64", .primitive = .i64_ },
    .{ .name = "i128", .primitive = .i128_ },
    .{ .name = "isize", .primitive = .isize_ },
    .{ .name = "u8", .primitive = .u8_ },
    .{ .name = "u16", .primitive = .u16_ },
    .{ .name = "u32", .primitive = .u32_ },
    .{ .name = "u64", .primitive = .u64_ },
    .{ .name = "u128", .primitive = .u128_ },
    .{ .name = "usize", .primitive = .usize_ },
    .{ .name = "f32", .primitive = .f32_ },
    .{ .name = "f64", .primitive = .f64_ },
    .{ .name = "bool", .primitive = .bool_ },
    .{ .name = "char", .primitive = .char_ },
    .{ .name = "string", .primitive = .string_ },
};
