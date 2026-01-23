const std = @import("std");
const Allocator = std.mem.Allocator;
const vm_value = @import("vm_value.zig");
const Value = vm_value.Value;
const ObjString = vm_value.ObjString;
const ObjArray = vm_value.ObjArray;
const ObjOptional = vm_value.ObjOptional;
const ObjNative = vm_value.ObjNative;
const RuntimeError = vm_value.RuntimeError;

// ============================================================================
// Native Function Type
// ============================================================================

/// Native function type that takes allocator and args.
pub const NativeFn = *const fn (allocator: Allocator, args: []const Value) RuntimeError!Value;

/// Native function descriptor for registration.
pub const NativeDesc = struct {
    name: []const u8,
    arity: u8,
    function: NativeFn,
};

// ============================================================================
// Built-in Function Registry
// ============================================================================

/// All built-in functions to register with the VM.
pub const builtins = [_]NativeDesc{
    // Core output functions
    .{ .name = "print", .arity = 1, .function = nativePrint },
    .{ .name = "println", .arity = 1, .function = nativePrintln },

    // Core input functions
    .{ .name = "readline", .arity = 0, .function = nativeReadline },

    // Assertion functions
    .{ .name = "assert", .arity = 1, .function = nativeAssert },
    .{ .name = "assert_eq", .arity = 2, .function = nativeAssertEq },
    .{ .name = "panic", .arity = 1, .function = nativePanic },

    // Utility functions
    .{ .name = "len", .arity = 1, .function = nativeLen },
    .{ .name = "type_of", .arity = 1, .function = nativeTypeOf },

    // Integer functions
    .{ .name = "abs", .arity = 1, .function = nativeAbs },
    .{ .name = "min", .arity = 2, .function = nativeMin },
    .{ .name = "max", .arity = 2, .function = nativeMax },
};

// ============================================================================
// Core Output Functions
// ============================================================================

fn nativePrint(allocator: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.WrongArity;

    const output = try valueToString(allocator, args[0]);
    defer if (args[0] != .string) allocator.free(output);

    const stdout = getStdOut();
    stdout.writeAll(output) catch {};

    return .void_;
}

fn nativePrintln(allocator: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.WrongArity;

    const output = try valueToString(allocator, args[0]);
    defer if (args[0] != .string) allocator.free(output);

    const stdout = getStdOut();
    stdout.writeAll(output) catch {};
    stdout.writeAll("\n") catch {};

    return .void_;
}

// ============================================================================
// Core Input Functions
// ============================================================================

fn nativeReadline(allocator: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 0) return RuntimeError.WrongArity;

    const stdin = getStdIn();

    // Read line byte-by-byte using Zig 0.15 API
    var result = std.ArrayListUnmanaged(u8){};
    defer result.deinit(allocator);

    const max_line_size: usize = 4096;
    while (result.items.len < max_line_size) {
        var buf: [1]u8 = undefined;
        const bytes_read = stdin.read(&buf) catch {
            return RuntimeError.IOError;
        };

        if (bytes_read == 0) {
            // EOF
            break;
        }

        if (buf[0] == '\n') {
            // End of line (don't include newline)
            break;
        }

        result.append(allocator, buf[0]) catch return RuntimeError.OutOfMemory;
    }

    const str = ObjString.create(allocator, result.items) catch return RuntimeError.OutOfMemory;
    return .{ .string = str };
}

// ============================================================================
// Assertion Functions
// ============================================================================

fn nativeAssert(_: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.WrongArity;

    if (args[0].isFalsey()) {
        return RuntimeError.AssertionFailed;
    }

    return .void_;
}

fn nativeAssertEq(_: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 2) return RuntimeError.WrongArity;

    if (!args[0].eql(args[1])) {
        return RuntimeError.AssertionFailed;
    }

    return .void_;
}

fn nativePanic(_: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.WrongArity;

    const stderr = std.fs.File{ .handle = std.posix.STDERR_FILENO };
    stderr.writeAll("panic: ") catch {};

    switch (args[0]) {
        .string => |s| stderr.writeAll(s.chars) catch {},
        else => stderr.writeAll("(non-string value)") catch {},
    }
    stderr.writeAll("\n") catch {};

    return RuntimeError.Panic;
}

// ============================================================================
// Utility Functions
// ============================================================================

fn nativeLen(_: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.WrongArity;

    const len: i128 = switch (args[0]) {
        .array => |a| @intCast(a.items.len),
        .tuple => |t| @intCast(t.items.len),
        .string => |s| @intCast(s.chars.len),
        else => return RuntimeError.TypeError,
    };

    return Value.fromInt(len);
}

fn nativeTypeOf(allocator: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.WrongArity;

    const type_name: []const u8 = switch (args[0]) {
        .int => "int",
        .float => "float",
        .bool_ => "bool",
        .char_ => "char",
        .void_ => "void",
        .string => "string",
        .array => "array",
        .tuple => "tuple",
        .struct_ => "struct",
        .closure, .function, .native => "function",
        .optional => "optional",
        .range => "range",
        .upvalue => "upvalue",
    };

    // Create a string object with the type name
    const str = ObjString.create(allocator, type_name) catch return RuntimeError.OutOfMemory;
    return .{ .string = str };
}

// ============================================================================
// Integer Functions
// ============================================================================

fn nativeAbs(_: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.WrongArity;

    switch (args[0]) {
        .int => |i| {
            const result = if (i < 0) -i else i;
            return Value.fromInt(result);
        },
        .float => |f| {
            const result = @abs(f);
            return Value.fromFloat(result);
        },
        else => return RuntimeError.TypeError,
    }
}

fn nativeMin(_: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 2) return RuntimeError.WrongArity;

    if (args[0] == .int and args[1] == .int) {
        return Value.fromInt(@min(args[0].int, args[1].int));
    }

    const a = args[0].asFloat() orelse return RuntimeError.TypeError;
    const b = args[1].asFloat() orelse return RuntimeError.TypeError;
    return Value.fromFloat(@min(a, b));
}

fn nativeMax(_: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 2) return RuntimeError.WrongArity;

    if (args[0] == .int and args[1] == .int) {
        return Value.fromInt(@max(args[0].int, args[1].int));
    }

    const a = args[0].asFloat() orelse return RuntimeError.TypeError;
    const b = args[1].asFloat() orelse return RuntimeError.TypeError;
    return Value.fromFloat(@max(a, b));
}

// ============================================================================
// Value to String Conversion
// ============================================================================

fn valueToString(allocator: Allocator, value: Value) RuntimeError![]const u8 {
    return switch (value) {
        .int => |i| std.fmt.allocPrint(allocator, "{d}", .{i}) catch return RuntimeError.OutOfMemory,
        .float => |f| std.fmt.allocPrint(allocator, "{d}", .{f}) catch return RuntimeError.OutOfMemory,
        .bool_ => |b| if (b) "true" else "false",
        .char_ => |c| blk: {
            var buf = allocator.alloc(u8, 4) catch return RuntimeError.OutOfMemory;
            const len = std.unicode.utf8Encode(c, buf[0..4]) catch 0;
            break :blk buf[0..len];
        },
        .void_ => "void",
        .string => |s| s.chars,
        .array => "<array>",
        .tuple => "<tuple>",
        .struct_ => "<struct>",
        .optional => |opt| if (opt.value != null) "Some(...)" else "None",
        .closure, .function => "<function>",
        .native => |n| n.name,
        .range => "<range>",
        .upvalue => "<upvalue>",
    };
}

// ============================================================================
// Helper Functions
// ============================================================================

fn getStdOut() std.fs.File {
    return .{ .handle = std.posix.STDOUT_FILENO };
}

fn getStdIn() std.fs.File {
    return .{ .handle = std.posix.STDIN_FILENO };
}

// ============================================================================
// Tests
// ============================================================================

test "nativeLen returns correct length" {
    const testing = std.testing;

    // Test string length
    const str = try ObjString.create(testing.allocator, "hello");
    defer str.destroy(testing.allocator);

    const result = try nativeLen(testing.allocator, &.{.{ .string = str }});
    try testing.expect(result == .int);
    try testing.expectEqual(@as(i128, 5), result.int);
}

test "nativeAbs returns absolute value" {
    const testing = std.testing;

    // Positive int stays positive
    var result = try nativeAbs(testing.allocator, &.{Value.fromInt(42)});
    try testing.expect(result.eql(Value.fromInt(42)));

    // Negative int becomes positive
    result = try nativeAbs(testing.allocator, &.{Value.fromInt(-42)});
    try testing.expect(result.eql(Value.fromInt(42)));

    // Positive float stays positive
    result = try nativeAbs(testing.allocator, &.{Value.fromFloat(3.14)});
    try testing.expect(result.eql(Value.fromFloat(3.14)));

    // Negative float becomes positive
    result = try nativeAbs(testing.allocator, &.{Value.fromFloat(-3.14)});
    try testing.expect(result.eql(Value.fromFloat(3.14)));
}

test "nativeMin and nativeMax" {
    const testing = std.testing;

    // Min of integers
    var result = try nativeMin(testing.allocator, &.{ Value.fromInt(5), Value.fromInt(3) });
    try testing.expect(result.eql(Value.fromInt(3)));

    // Max of integers
    result = try nativeMax(testing.allocator, &.{ Value.fromInt(5), Value.fromInt(3) });
    try testing.expect(result.eql(Value.fromInt(5)));

    // Min of floats
    result = try nativeMin(testing.allocator, &.{ Value.fromFloat(5.5), Value.fromFloat(3.3) });
    try testing.expect(result.eql(Value.fromFloat(3.3)));

    // Max of floats
    result = try nativeMax(testing.allocator, &.{ Value.fromFloat(5.5), Value.fromFloat(3.3) });
    try testing.expect(result.eql(Value.fromFloat(5.5)));
}

test "nativeAssert passes on truthy values" {
    const testing = std.testing;

    // True should pass
    _ = try nativeAssert(testing.allocator, &.{Value.true_val});

    // Non-zero int should pass
    _ = try nativeAssert(testing.allocator, &.{Value.fromInt(1)});
}

test "nativeAssert fails on falsey values" {
    const testing = std.testing;

    // False should fail
    try testing.expectError(RuntimeError.AssertionFailed, nativeAssert(testing.allocator, &.{Value.false_val}));

    // Zero should fail
    try testing.expectError(RuntimeError.AssertionFailed, nativeAssert(testing.allocator, &.{Value.fromInt(0)}));

    // Void should fail
    try testing.expectError(RuntimeError.AssertionFailed, nativeAssert(testing.allocator, &.{.void_}));
}

test "nativeAssertEq compares values" {
    const testing = std.testing;

    // Equal values should pass
    _ = try nativeAssertEq(testing.allocator, &.{ Value.fromInt(42), Value.fromInt(42) });

    // Unequal values should fail
    try testing.expectError(RuntimeError.AssertionFailed, nativeAssertEq(testing.allocator, &.{ Value.fromInt(42), Value.fromInt(43) }));
}
