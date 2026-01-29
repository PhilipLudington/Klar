//! Generic/monomorphization support for codegen.
//!
//! This module provides utilities for handling generic types and functions
//! during code generation. The main implementation is in emit.zig, but this
//! module documents the interface for working with monomorphized entities.
//!
//! Key concepts:
//! - Monomorphization: The process of creating concrete type instantiations
//!   from generic templates (e.g., List[i32] from List[T])
//! - Mangled names: Unique names for each concrete instantiation
//!   (e.g., "List$i32" for List[i32])
//!
//! The TypeChecker tracks all monomorphized types and functions. The Emitter
//! uses this information to:
//! 1. Register concrete struct/enum types before emitting code
//! 2. Declare function signatures before emitting bodies
//! 3. Emit function bodies with concrete type parameters
//!
//! Usage flow:
//! 1. registerMonomorphizedStructs - Register all concrete struct types
//! 2. registerMonomorphizedEnums - Register all concrete enum types
//! 3. declareMonomorphizedFunctions - Declare all concrete function signatures
//! 4. declareMonomorphizedMethods - Declare all concrete method signatures
//! 5. emitModule - Emit main module code
//! 6. emitMonomorphizedFunctions - Emit all concrete function bodies
//! 7. emitMonomorphizedMethods - Emit all concrete method bodies

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
