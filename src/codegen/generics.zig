//! Generic/monomorphization helper utilities for codegen.
//!
//! Provides name-mangling helpers for monomorphized types and functions.
//! The monomorphization registration and emission logic remains in emit.zig.
//!
//! ## Provided by this module
//!
//! - `appendTypeNameForMangling`: Append type name with safe character substitution
//! - `buildMangledName`: Build a full mangled name (e.g., "Map$string_i32")

const std = @import("std");

/// Helper for building mangled names from type arguments.
/// Appends a type name suitable for use in mangled identifiers.
pub fn appendTypeNameForMangling(buffer: *std.ArrayList(u8), type_name: []const u8) !void {
    // Replace special characters with safe alternatives
    for (type_name) |c| {
        switch (c) {
            '[' => try buffer.append('$'),
            ']' => {}, // Skip closing brackets
            ',' => try buffer.append('_'),
            ' ' => {}, // Skip spaces
            else => try buffer.append(c),
        }
    }
}

/// Build a mangled name for a generic type instantiation.
/// Example: "List" + ["i32"] -> "List$i32"
/// Example: "Map" + ["string", "i32"] -> "Map$string_i32"
pub fn buildMangledName(allocator: std.mem.Allocator, base_name: []const u8, type_args: []const []const u8) ![]const u8 {
    var buffer = std.ArrayList(u8).init(allocator);
    errdefer buffer.deinit();

    try buffer.appendSlice(base_name);

    for (type_args) |arg| {
        try buffer.append('$');
        try appendTypeNameForMangling(&buffer, arg);
    }

    return buffer.toOwnedSlice();
}

test "buildMangledName simple" {
    const allocator = std.testing.allocator;
    const result = try buildMangledName(allocator, "List", &[_][]const u8{"i32"});
    defer allocator.free(result);
    try std.testing.expectEqualStrings("List$i32", result);
}

test "buildMangledName multiple args" {
    const allocator = std.testing.allocator;
    const result = try buildMangledName(allocator, "Map", &[_][]const u8{ "string", "i32" });
    defer allocator.free(result);
    try std.testing.expectEqualStrings("Map$string_i32", result);
}
