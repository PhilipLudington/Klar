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
//!
//! ### Phase 0: Environment, Process, Stat, Timestamp
//!
//! | Function         | Signature                                        | Description                     |
//! |------------------|--------------------------------------------------|---------------------------------|
//! | `env_get`        | `fn(string) -> ?string`                          | Get environment variable        |
//! | `env_set`        | `fn(string, string) -> Result#[void, IoError]`   | Set environment variable        |
//! | `fs_stat`        | `fn(string) -> Result#[FileStat, IoError]`       | Stat a filesystem path          |
//! | `timestamp_now`  | `fn() -> i64`                                    | Current Unix epoch (seconds)    |
//! | `process_run`    | `fn(string, [string]) -> Result#[ProcessOutput, IoError]` | Run subprocess via shell (CAUTION: args are shell-interpreted, not isolated — see security note) |
//!
//! Struct types: `FileStat { size: i64, modified_epoch: i64, is_dir: bool, is_file: bool }`,
//! `ProcessOutput { stdout: string, stderr: string (always "" — not yet captured), exit_code: i32 }`
//!
//! **Security note:** `process_run` uses `popen()` internally. The command and arguments are
//! concatenated into a single string and passed to `/bin/sh`. Shell metacharacters in any
//! argument (`;`, `|`, `$()`, `` ` ``, `&&`, etc.) WILL be interpreted. Only pass trusted,
//! literal strings. A future `process_spawn` builtin will use `execvp()` for safe arg passing.

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
    // Phase 6: async subprocess and TCP socket builtins
    pub const process_spawn = "process_spawn";
    pub const process_poll = "process_poll";
    pub const process_wait = "process_wait";
    pub const process_read_stdout = "process_read_stdout";
    pub const tcp_listen = "tcp_listen";
    pub const tcp_accept = "tcp_accept";
    pub const tcp_connect = "tcp_connect";
    pub const tcp_read = "tcp_read";
    pub const tcp_write = "tcp_write";
    pub const tcp_close = "tcp_close";
    pub const tcp_set_nonblocking = "tcp_set_nonblocking";
    pub const tcp_listener_close = "tcp_listener_close";
    // Phase 9.2: UDP socket builtins
    pub const udp_bind = "udp_bind";
    pub const udp_send_to = "udp_send_to";
    pub const udp_recv_from = "udp_recv_from";
    pub const udp_close = "udp_close";
    pub const dns_lookup = "dns_lookup";
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
        std.mem.eql(u8, name, BuiltinName.process_run) or
        std.mem.eql(u8, name, BuiltinName.process_spawn) or
        std.mem.eql(u8, name, BuiltinName.process_poll) or
        std.mem.eql(u8, name, BuiltinName.process_wait) or
        std.mem.eql(u8, name, BuiltinName.process_read_stdout) or
        std.mem.eql(u8, name, BuiltinName.tcp_listen) or
        std.mem.eql(u8, name, BuiltinName.tcp_accept) or
        std.mem.eql(u8, name, BuiltinName.tcp_connect) or
        std.mem.eql(u8, name, BuiltinName.tcp_read) or
        std.mem.eql(u8, name, BuiltinName.tcp_write) or
        std.mem.eql(u8, name, BuiltinName.tcp_close) or
        std.mem.eql(u8, name, BuiltinName.tcp_set_nonblocking) or
        std.mem.eql(u8, name, BuiltinName.tcp_listener_close) or
        std.mem.eql(u8, name, BuiltinName.udp_bind) or
        std.mem.eql(u8, name, BuiltinName.udp_send_to) or
        std.mem.eql(u8, name, BuiltinName.udp_recv_from) or
        std.mem.eql(u8, name, BuiltinName.udp_close) or
        std.mem.eql(u8, name, BuiltinName.dns_lookup);
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
