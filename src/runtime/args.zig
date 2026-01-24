// ============================================================================
// Klar Command-Line Arguments Runtime Support
// ============================================================================
//
// This module provides runtime support for converting C-style argc/argv
// to Klar's [String] slice format.
//
// String struct: { ptr: *u8, len: i32, capacity: i32 }
// Slice struct: { ptr: *String, len: i64 }

const std = @import("std");
const c = @cImport({
    @cInclude("stdlib.h");
    @cInclude("string.h");
});

/// Klar String representation: { ptr, len, capacity }
const KlarString = extern struct {
    ptr: ?[*]u8,
    len: i32,
    capacity: i32,
};

/// Klar slice representation: { ptr, len }
const StringSlice = extern struct {
    ptr: ?[*]KlarString,
    len: i64,
};

/// Convert argc/argv to a Klar [String] slice.
/// Allocates memory that must be freed by _klar_args_free.
export fn _klar_args_from_argv(argc: i32, argv: [*][*:0]const u8) StringSlice {
    if (argc <= 0) {
        return .{ .ptr = null, .len = 0 };
    }

    const count: usize = @intCast(argc);

    // Allocate array of KlarString structs
    const strings_size = count * @sizeOf(KlarString);
    const strings_ptr = @as(?[*]KlarString, @ptrCast(@alignCast(c.malloc(strings_size))));
    if (strings_ptr == null) {
        return .{ .ptr = null, .len = 0 };
    }

    // Convert each argv string to KlarString
    for (0..count) |i| {
        const c_str = argv[i];
        const str_len = c.strlen(c_str);

        // Allocate buffer for string data (no null terminator needed for Klar strings)
        const buf = @as(?[*]u8, @ptrCast(c.malloc(str_len)));
        if (buf) |buffer| {
            // Copy string data
            _ = c.memcpy(buffer, c_str, str_len);
            strings_ptr.?[i] = .{
                .ptr = buffer,
                .len = @intCast(str_len),
                .capacity = @intCast(str_len),
            };
        } else {
            // Allocation failed - create empty string
            strings_ptr.?[i] = .{
                .ptr = null,
                .len = 0,
                .capacity = 0,
            };
        }
    }

    return .{
        .ptr = strings_ptr,
        .len = @intCast(count),
    };
}

/// Free a [String] slice created by _klar_args_from_argv.
export fn _klar_args_free(args: StringSlice) void {
    if (args.ptr == null or args.len <= 0) {
        return;
    }

    const count: usize = @intCast(args.len);
    const strings = args.ptr.?;

    // Free each string's data
    for (0..count) |i| {
        if (strings[i].ptr) |ptr| {
            c.free(ptr);
        }
    }

    // Free the array itself
    c.free(strings);
}
