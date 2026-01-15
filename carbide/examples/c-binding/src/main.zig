//! C Binding Example
//!
//! Demonstrates:
//! - Importing C headers with @cImport
//! - Type conversions between C and Zig
//! - Error handling for C functions
//! - Callbacks from C to Zig
//! - Memory management across the C/Zig boundary
//! - Struct interoperability

const std = @import("std");
const Allocator = std.mem.Allocator;

// Import the C library
const c = @cImport({
    @cInclude("mathlib.h");
});

// ============================================================================
// Error Mapping
// ============================================================================

/// Maps C error codes to Zig errors.
pub const MathError = error{
    NullPointer,
    OutOfRange,
    DivisionByZero,
    Unknown,
};

fn mapCError(code: c_int) MathError {
    return switch (code) {
        c.MATHLIB_ERR_NULL => error.NullPointer,
        c.MATHLIB_ERR_RANGE => error.OutOfRange,
        c.MATHLIB_ERR_ZERO => error.DivisionByZero,
        else => error.Unknown,
    };
}

// ============================================================================
// Safe Wrappers
// ============================================================================

/// Wrapper for simple math operations (no error handling needed).
pub const Math = struct {
    pub fn add(a: i32, b: i32) i32 {
        return c.mathlib_add(a, b);
    }

    pub fn multiply(a: i32, b: i32) i32 {
        return c.mathlib_multiply(a, b);
    }

    /// Divides a by b, returning error on division by zero.
    pub fn divide(a: i32, b: i32) MathError!i32 {
        var result: i32 = undefined;
        const status = c.mathlib_divide(a, b, &result);
        if (status != c.MATHLIB_OK) {
            return mapCError(status);
        }
        return result;
    }

    /// Computes integer square root.
    pub fn sqrt(n: i32) MathError!i32 {
        var result: i32 = undefined;
        const status = c.mathlib_sqrt(n, &result);
        if (status != c.MATHLIB_OK) {
            return mapCError(status);
        }
        return result;
    }
};

// ============================================================================
// Array Operations
// ============================================================================

pub const ArrayOps = struct {
    /// Sums all elements in a slice.
    pub fn sum(arr: []const i32) MathError!i64 {
        var result: i64 = undefined;
        const status = c.mathlib_sum_array(arr.ptr, arr.len, &result);
        if (status != c.MATHLIB_OK) {
            return mapCError(status);
        }
        return result;
    }

    /// Finds the maximum element in a slice.
    pub fn findMax(arr: []const i32) MathError!i32 {
        if (arr.len == 0) {
            return error.OutOfRange;
        }
        var result: i32 = undefined;
        const status = c.mathlib_find_max(arr.ptr, arr.len, &result);
        if (status != c.MATHLIB_OK) {
            return mapCError(status);
        }
        return result;
    }
};

// ============================================================================
// String Operations
// ============================================================================

pub const StringOps = struct {
    /// Gets length of a C string.
    pub fn strlen(str: [:0]const u8) usize {
        return c.mathlib_strlen(str.ptr);
    }

    /// Parses an integer from a string.
    pub fn parseInt(str: [:0]const u8) MathError!i32 {
        var result: i32 = undefined;
        const status = c.mathlib_parse_int(str.ptr, &result);
        if (status != c.MATHLIB_OK) {
            return mapCError(status);
        }
        return result;
    }
};

// ============================================================================
// Struct Interop
// ============================================================================

/// Zig-friendly Point type (mirrors C struct).
pub const Point = extern struct {
    x: i32,
    y: i32,

    /// Computes squared distance to another point.
    pub fn distanceSquared(self: Point, other: Point) i32 {
        return c.mathlib_point_distance_squared(&self, &other);
    }

    /// Formats point as string. Caller owns returned memory.
    pub fn format(self: Point, allocator: Allocator) ![]u8 {
        const c_str = c.mathlib_format_point(&self);
        if (c_str == null) {
            return error.OutOfMemory;
        }
        defer c.mathlib_free_string(c_str);

        // Copy to Zig-managed memory
        const len = std.mem.len(c_str);
        const result = try allocator.alloc(u8, len);
        @memcpy(result, c_str[0..len]);
        return result;
    }
};

/// Zig-friendly Rectangle type.
pub const Rectangle = extern struct {
    top_left: Point,
    bottom_right: Point,

    pub fn area(self: Rectangle) i32 {
        return c.mathlib_rectangle_area(&self);
    }
};

// ============================================================================
// Callback Example
// ============================================================================

/// Context for the callback.
const CallbackContext = struct {
    total: i64 = 0,
    count: usize = 0,
};

/// Zig function that will be called from C.
fn zigCallback(user_data: ?*anyopaque, value: i32) callconv(.C) void {
    const ctx: *CallbackContext = @ptrCast(@alignCast(user_data));
    ctx.total += value;
    ctx.count += 1;
}

/// Demonstrates using C callback with Zig function.
pub fn foreachWithCallback(arr: []const i32) struct { total: i64, count: usize } {
    var ctx = CallbackContext{};
    c.mathlib_foreach(arr.ptr, arr.len, zigCallback, &ctx);
    return .{ .total = ctx.total, .count = ctx.count };
}

// ============================================================================
// Main Demo
// ============================================================================

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    try stdout.writeAll("=== C Binding Demo ===\n\n");

    // Basic math
    try stdout.writeAll("Basic Math:\n");
    try stdout.print("  add(5, 3) = {d}\n", .{Math.add(5, 3)});
    try stdout.print("  multiply(4, 7) = {d}\n", .{Math.multiply(4, 7)});
    try stdout.print("  divide(20, 4) = {d}\n", .{try Math.divide(20, 4)});
    try stdout.print("  sqrt(16) = {d}\n", .{try Math.sqrt(16)});

    // Division by zero
    try stdout.writeAll("\nError Handling:\n");
    if (Math.divide(10, 0)) |_| {
        try stdout.writeAll("  Unexpected success\n");
    } else |err| {
        try stdout.print("  divide(10, 0) returned error: {}\n", .{err});
    }

    // Array operations
    try stdout.writeAll("\nArray Operations:\n");
    const arr = [_]i32{ 1, 5, 3, 9, 2 };
    try stdout.print("  sum([1,5,3,9,2]) = {d}\n", .{try ArrayOps.sum(&arr)});
    try stdout.print("  findMax([1,5,3,9,2]) = {d}\n", .{try ArrayOps.findMax(&arr)});

    // String operations
    try stdout.writeAll("\nString Operations:\n");
    try stdout.print("  strlen(\"hello\") = {d}\n", .{StringOps.strlen("hello")});
    try stdout.print("  parseInt(\"42\") = {d}\n", .{try StringOps.parseInt("42")});

    // Struct operations
    try stdout.writeAll("\nStruct Operations:\n");
    const p1 = Point{ .x = 0, .y = 0 };
    const p2 = Point{ .x = 3, .y = 4 };
    try stdout.print("  distanceSquared((0,0), (3,4)) = {d}\n", .{p1.distanceSquared(p2)});

    const rect = Rectangle{
        .top_left = .{ .x = 0, .y = 0 },
        .bottom_right = .{ .x = 10, .y = 5 },
    };
    try stdout.print("  rectangle area = {d}\n", .{rect.area()});

    // Callback
    try stdout.writeAll("\nCallback Demo:\n");
    const result = foreachWithCallback(&arr);
    try stdout.print("  foreach sum = {d}, count = {d}\n", .{ result.total, result.count });

    // Memory management
    try stdout.writeAll("\nMemory Management:\n");
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const formatted = try p2.format(gpa.allocator());
    defer gpa.allocator().free(formatted);
    try stdout.print("  Formatted: {s}\n", .{formatted});

    try stdout.writeAll("\nDemo complete!\n");
}

// ============================================================================
// Tests
// ============================================================================

test "Math.add" {
    try std.testing.expectEqual(@as(i32, 8), Math.add(5, 3));
    try std.testing.expectEqual(@as(i32, 0), Math.add(-5, 5));
}

test "Math.divide success" {
    try std.testing.expectEqual(@as(i32, 5), try Math.divide(20, 4));
}

test "Math.divide by zero" {
    try std.testing.expectError(error.DivisionByZero, Math.divide(10, 0));
}

test "Math.sqrt" {
    try std.testing.expectEqual(@as(i32, 4), try Math.sqrt(16));
    try std.testing.expectEqual(@as(i32, 3), try Math.sqrt(9));
    try std.testing.expectEqual(@as(i32, 0), try Math.sqrt(0));
}

test "Math.sqrt negative" {
    try std.testing.expectError(error.OutOfRange, Math.sqrt(-1));
}

test "ArrayOps.sum" {
    const arr = [_]i32{ 1, 2, 3, 4, 5 };
    try std.testing.expectEqual(@as(i64, 15), try ArrayOps.sum(&arr));
}

test "ArrayOps.findMax" {
    const arr = [_]i32{ 1, 5, 3, 9, 2 };
    try std.testing.expectEqual(@as(i32, 9), try ArrayOps.findMax(&arr));
}

test "ArrayOps.findMax empty" {
    const arr = [_]i32{};
    try std.testing.expectError(error.OutOfRange, ArrayOps.findMax(&arr));
}

test "StringOps.strlen" {
    try std.testing.expectEqual(@as(usize, 5), StringOps.strlen("hello"));
    try std.testing.expectEqual(@as(usize, 0), StringOps.strlen(""));
}

test "StringOps.parseInt" {
    try std.testing.expectEqual(@as(i32, 42), try StringOps.parseInt("42"));
    try std.testing.expectEqual(@as(i32, -123), try StringOps.parseInt("-123"));
}

test "StringOps.parseInt invalid" {
    try std.testing.expectError(error.OutOfRange, StringOps.parseInt("abc"));
}

test "Point.distanceSquared" {
    const p1 = Point{ .x = 0, .y = 0 };
    const p2 = Point{ .x = 3, .y = 4 };
    try std.testing.expectEqual(@as(i32, 25), p1.distanceSquared(p2));
}

test "Rectangle.area" {
    const rect = Rectangle{
        .top_left = .{ .x = 0, .y = 0 },
        .bottom_right = .{ .x = 10, .y = 5 },
    };
    try std.testing.expectEqual(@as(i32, 50), rect.area());
}

test "foreachWithCallback" {
    const arr = [_]i32{ 1, 2, 3 };
    const result = foreachWithCallback(&arr);
    try std.testing.expectEqual(@as(i64, 6), result.total);
    try std.testing.expectEqual(@as(usize, 3), result.count);
}

test "Point.format" {
    const p = Point{ .x = 10, .y = 20 };
    const formatted = try p.format(std.testing.allocator);
    defer std.testing.allocator.free(formatted);

    try std.testing.expectEqualStrings("Point(10, 20)", formatted);
}
