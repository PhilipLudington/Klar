const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const vm_value = @import("vm_value.zig");
const Value = vm_value.Value;
const ObjString = vm_value.ObjString;
const ObjArray = vm_value.ObjArray;
const ObjTuple = vm_value.ObjTuple;
const ObjStruct = vm_value.ObjStruct;
const ObjOptional = vm_value.ObjOptional;
const ObjNative = vm_value.ObjNative;
const RuntimeError = vm_value.RuntimeError;
const GC = vm_value.GC;

var active_gc: ?*GC = null;

pub fn setActiveGC(gc: ?*GC) void {
    active_gc = gc;
}

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
    .{ .name = "assert_ne", .arity = 2, .function = nativeAssertNe },
    .{ .name = "assert_ok", .arity = 1, .function = nativeAssertOk },
    .{ .name = "assert_err", .arity = 1, .function = nativeAssertErr },
    .{ .name = "assert_some", .arity = 1, .function = nativeAssertSome },
    .{ .name = "assert_none", .arity = 1, .function = nativeAssertNone },
    .{ .name = "Ok", .arity = 1, .function = nativeOk },
    .{ .name = "Err", .arity = 1, .function = nativeErr },
    .{ .name = "Some", .arity = 1, .function = nativeSome },
    .{ .name = "None", .arity = 0, .function = nativeNone },
    .{ .name = "panic", .arity = 1, .function = nativePanic },

    // Utility functions
    .{ .name = "len", .arity = 1, .function = nativeLen },
    .{ .name = "type_of", .arity = 1, .function = nativeTypeOf },

    // Integer functions
    .{ .name = "abs", .arity = 1, .function = nativeAbs },
    .{ .name = "min", .arity = 2, .function = nativeMin },
    .{ .name = "max", .arity = 2, .function = nativeMax },

    // Debug functions
    .{ .name = "debug", .arity = 1, .function = nativeDebug },

    // String primitive functions
    .{ .name = "from_byte", .arity = 1, .function = nativeFromByte },
    .{ .name = "parse_int", .arity = 1, .function = nativeParseInt },
    .{ .name = "parse_float", .arity = 1, .function = nativeParseFloat },

    // Environment, process, stat, timestamp
    .{ .name = "env_get", .arity = 1, .function = nativeEnvGet },
    .{ .name = "env_set", .arity = 2, .function = nativeEnvSet },
    .{ .name = "timestamp_now", .arity = 0, .function = nativeTimestampNow },
    .{ .name = "fs_stat", .arity = 1, .function = nativeFsStat },
    .{ .name = "process_run", .arity = 2, .function = nativeProcessRun },
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

fn nativeAssertNe(_: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 2) return RuntimeError.WrongArity;

    if (args[0].eql(args[1])) {
        return RuntimeError.AssertionFailed;
    }

    return .void_;
}

fn nativeAssertOk(_: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.WrongArity;
    if (args[0] != .struct_) return RuntimeError.TypeError;
    const result_struct = args[0].struct_;
    if (!std.mem.eql(u8, result_struct.type_name, "Result")) return RuntimeError.TypeError;
    const ok_field = result_struct.getField("is_ok") orelse return RuntimeError.TypeError;
    if (ok_field != .bool_) return RuntimeError.TypeError;
    if (!ok_field.bool_) return RuntimeError.AssertionFailed;
    return .void_;
}

fn nativeAssertErr(_: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.WrongArity;
    if (args[0] != .struct_) return RuntimeError.TypeError;
    const result_struct = args[0].struct_;
    if (!std.mem.eql(u8, result_struct.type_name, "Result")) return RuntimeError.TypeError;
    const ok_field = result_struct.getField("is_ok") orelse return RuntimeError.TypeError;
    if (ok_field != .bool_) return RuntimeError.TypeError;
    if (ok_field.bool_) return RuntimeError.AssertionFailed;
    return .void_;
}

fn nativeAssertSome(_: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.WrongArity;
    if (args[0] != .optional) return RuntimeError.TypeError;

    if (args[0].optional.value == null) {
        return RuntimeError.AssertionFailed;
    }

    return .void_;
}

fn nativeAssertNone(_: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.WrongArity;
    if (args[0] != .optional) return RuntimeError.TypeError;

    if (args[0].optional.value != null) {
        return RuntimeError.AssertionFailed;
    }

    return .void_;
}

fn nativeOk(allocator: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.WrongArity;
    const gc = active_gc orelse return RuntimeError.TypeError;
    const result_struct = ObjStruct.createGC(gc, "Result") catch return RuntimeError.OutOfMemory;
    result_struct.setField(allocator, "is_ok", .{ .bool_ = true }) catch return RuntimeError.OutOfMemory;
    result_struct.setField(allocator, "value", args[0]) catch return RuntimeError.OutOfMemory;
    return .{ .struct_ = result_struct };
}

fn nativeErr(allocator: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.WrongArity;
    const gc = active_gc orelse return RuntimeError.TypeError;
    const result_struct = ObjStruct.createGC(gc, "Result") catch return RuntimeError.OutOfMemory;
    result_struct.setField(allocator, "is_ok", .{ .bool_ = false }) catch return RuntimeError.OutOfMemory;
    result_struct.setField(allocator, "value", args[0]) catch return RuntimeError.OutOfMemory;
    return .{ .struct_ = result_struct };
}

fn nativeSome(allocator: Allocator, args: []const Value) RuntimeError!Value {
    _ = allocator;
    if (args.len != 1) return RuntimeError.WrongArity;
    const gc = active_gc orelse return RuntimeError.TypeError;
    const optional = ObjOptional.createSomeGC(gc, args[0]) catch return RuntimeError.OutOfMemory;
    return .{ .optional = optional };
}

fn nativeNone(allocator: Allocator, args: []const Value) RuntimeError!Value {
    _ = allocator;
    if (args.len != 0) return RuntimeError.WrongArity;
    const gc = active_gc orelse return RuntimeError.TypeError;
    const optional = ObjOptional.createNoneGC(gc) catch return RuntimeError.OutOfMemory;
    return .{ .optional = optional };
}

fn nativePanic(_: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.WrongArity;

    const stderr = getStdErr();
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
        .string => |s| blk: {
            // Count UTF-8 codepoints (not bytes)
            var cp_count: i128 = 0;
            var i: usize = 0;
            while (i < s.chars.len) {
                const cp_len = std.unicode.utf8ByteSequenceLength(s.chars[i]) catch 1;
                i += cp_len;
                cp_count += 1;
            }
            break :blk cp_count;
        },
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
        .future => "future",
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
// Debug Functions
// ============================================================================

fn nativeDebug(allocator: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.WrongArity;

    const output = try debugValueToString(allocator, args[0], 0);
    const str = ObjString.create(allocator, output) catch return RuntimeError.OutOfMemory;
    // Note: output is owned by the string now, but ObjString.create duplicates it
    // So we need to free output if it was allocated
    if (needsFreeDebug(args[0])) {
        allocator.free(output);
    }

    return .{ .string = str };
}

/// Returns true if debugValueToString allocates for this value type
fn needsFreeDebug(value: Value) bool {
    return switch (value) {
        .bool_, .void_ => false,
        else => true,
    };
}

/// Convert a value to its debug string representation.
/// Recursively formats nested structures with depth limit.
fn debugValueToString(allocator: Allocator, value: Value, depth: usize) RuntimeError![]const u8 {
    const max_depth = 10;
    if (depth > max_depth) {
        return allocator.dupe(u8, "...") catch return RuntimeError.OutOfMemory;
    }

    return switch (value) {
        .int => |i| std.fmt.allocPrint(allocator, "{d}", .{i}) catch return RuntimeError.OutOfMemory,
        .float => |f| blk: {
            // Format float, ensuring we show decimal point for whole numbers
            const formatted = std.fmt.allocPrint(allocator, "{d}", .{f}) catch return RuntimeError.OutOfMemory;
            // Check if it has a decimal point
            for (formatted) |c| {
                if (c == '.' or c == 'e' or c == 'E' or c == 'n' or c == 'i') {
                    // Already has decimal, exponent, nan, or inf
                    break :blk formatted;
                }
            }
            // Add .0 for whole number floats
            const with_decimal = std.fmt.allocPrint(allocator, "{s}.0", .{formatted}) catch {
                allocator.free(formatted);
                return RuntimeError.OutOfMemory;
            };
            allocator.free(formatted);
            break :blk with_decimal;
        },
        .bool_ => |b| if (b) "true" else "false",
        .char_ => |c| blk: {
            // Format as 'c' with proper escaping
            var buf = std.ArrayListUnmanaged(u8){};
            errdefer buf.deinit(allocator);
            buf.append(allocator, '\'') catch return RuntimeError.OutOfMemory;

            // Encode the character
            var char_buf: [4]u8 = undefined;
            const len = std.unicode.utf8Encode(c, &char_buf) catch 1;
            buf.appendSlice(allocator, char_buf[0..len]) catch return RuntimeError.OutOfMemory;

            buf.append(allocator, '\'') catch return RuntimeError.OutOfMemory;
            break :blk buf.toOwnedSlice(allocator) catch return RuntimeError.OutOfMemory;
        },
        .void_ => "void",
        .string => |s| blk: {
            // Format as "string" with quotes
            var buf = std.ArrayListUnmanaged(u8){};
            errdefer buf.deinit(allocator);
            buf.append(allocator, '"') catch return RuntimeError.OutOfMemory;

            // Escape special characters
            for (s.chars) |c| {
                switch (c) {
                    '\n' => buf.appendSlice(allocator, "\\n") catch return RuntimeError.OutOfMemory,
                    '\r' => buf.appendSlice(allocator, "\\r") catch return RuntimeError.OutOfMemory,
                    '\t' => buf.appendSlice(allocator, "\\t") catch return RuntimeError.OutOfMemory,
                    '\\' => buf.appendSlice(allocator, "\\\\") catch return RuntimeError.OutOfMemory,
                    '"' => buf.appendSlice(allocator, "\\\"") catch return RuntimeError.OutOfMemory,
                    else => buf.append(allocator, c) catch return RuntimeError.OutOfMemory,
                }
            }

            buf.append(allocator, '"') catch return RuntimeError.OutOfMemory;
            break :blk buf.toOwnedSlice(allocator) catch return RuntimeError.OutOfMemory;
        },
        .array => |a| blk: {
            var buf = std.ArrayListUnmanaged(u8){};
            errdefer buf.deinit(allocator);
            buf.append(allocator, '[') catch return RuntimeError.OutOfMemory;

            for (a.items, 0..) |item, i| {
                if (i > 0) {
                    buf.appendSlice(allocator, ", ") catch return RuntimeError.OutOfMemory;
                }
                const item_str = try debugValueToString(allocator, item, depth + 1);
                defer if (needsFreeDebug(item)) allocator.free(item_str);
                buf.appendSlice(allocator, item_str) catch return RuntimeError.OutOfMemory;
            }

            buf.append(allocator, ']') catch return RuntimeError.OutOfMemory;
            break :blk buf.toOwnedSlice(allocator) catch return RuntimeError.OutOfMemory;
        },
        .tuple => |t| blk: {
            var buf = std.ArrayListUnmanaged(u8){};
            errdefer buf.deinit(allocator);
            buf.append(allocator, '(') catch return RuntimeError.OutOfMemory;

            for (t.items, 0..) |item, i| {
                if (i > 0) {
                    buf.appendSlice(allocator, ", ") catch return RuntimeError.OutOfMemory;
                }
                const item_str = try debugValueToString(allocator, item, depth + 1);
                defer if (needsFreeDebug(item)) allocator.free(item_str);
                buf.appendSlice(allocator, item_str) catch return RuntimeError.OutOfMemory;
            }

            buf.append(allocator, ')') catch return RuntimeError.OutOfMemory;
            break :blk buf.toOwnedSlice(allocator) catch return RuntimeError.OutOfMemory;
        },
        .struct_ => |s| blk: {
            var buf = std.ArrayListUnmanaged(u8){};
            errdefer buf.deinit(allocator);

            // Start with type name
            buf.appendSlice(allocator, s.type_name) catch return RuntimeError.OutOfMemory;
            buf.appendSlice(allocator, " { ") catch return RuntimeError.OutOfMemory;

            // Iterate over fields
            var iter = s.fields.iterator();
            var first = true;
            while (iter.next()) |entry| {
                if (!first) {
                    buf.appendSlice(allocator, ", ") catch return RuntimeError.OutOfMemory;
                }
                first = false;

                // field_name: value
                buf.appendSlice(allocator, entry.key_ptr.*) catch return RuntimeError.OutOfMemory;
                buf.appendSlice(allocator, ": ") catch return RuntimeError.OutOfMemory;

                const field_str = try debugValueToString(allocator, entry.value_ptr.*, depth + 1);
                defer if (needsFreeDebug(entry.value_ptr.*)) allocator.free(field_str);
                buf.appendSlice(allocator, field_str) catch return RuntimeError.OutOfMemory;
            }

            buf.appendSlice(allocator, " }") catch return RuntimeError.OutOfMemory;
            break :blk buf.toOwnedSlice(allocator) catch return RuntimeError.OutOfMemory;
        },
        .optional => |opt| blk: {
            if (opt.value) |val| {
                const inner = try debugValueToString(allocator, val.*, depth + 1);
                defer if (needsFreeDebug(val.*)) allocator.free(inner);
                break :blk std.fmt.allocPrint(allocator, "Some({s})", .{inner}) catch return RuntimeError.OutOfMemory;
            } else {
                break :blk allocator.dupe(u8, "None") catch return RuntimeError.OutOfMemory;
            }
        },
        .closure, .function => blk: {
            break :blk allocator.dupe(u8, "<function>") catch return RuntimeError.OutOfMemory;
        },
        .native => |n| blk: {
            break :blk std.fmt.allocPrint(allocator, "<native {s}>", .{n.name}) catch return RuntimeError.OutOfMemory;
        },
        .range => |r| blk: {
            const op = if (r.inclusive) "..=" else "..";
            break :blk std.fmt.allocPrint(allocator, "{d}{s}{d}", .{ r.start, op, r.end }) catch return RuntimeError.OutOfMemory;
        },
        .upvalue => allocator.dupe(u8, "<upvalue>") catch return RuntimeError.OutOfMemory,
        .future => |future| blk: {
            break :blk std.fmt.allocPrint(allocator, "Future(task={d}, state={s})", .{
                future.task_id,
                switch (future.state) {
                    .pending => "pending",
                    .completed => "completed",
                    .failed => "failed",
                    .cancelled => "cancelled",
                },
            }) catch return RuntimeError.OutOfMemory;
        },
    };
}

// ============================================================================
// String Primitive Functions
// ============================================================================

fn nativeFromByte(_: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.WrongArity;
    const byte_val = args[0].asInt() orelse return RuntimeError.TypeError;
    if (byte_val < 0 or byte_val > 255) return RuntimeError.TypeError;
    const gc = active_gc orelse return RuntimeError.TypeError;
    var buf = [_]u8{@intCast(byte_val)};
    const str = ObjString.createGC(gc, &buf) catch return RuntimeError.OutOfMemory;
    return .{ .string = str };
}

fn nativeParseInt(_: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.WrongArity;
    if (args[0] != .string) return RuntimeError.TypeError;
    const str = args[0].string.chars;
    const gc = active_gc orelse return RuntimeError.TypeError;
    if (std.fmt.parseInt(i64, str, 10)) |val| {
        const some = ObjOptional.createSomeGC(gc, Value.fromInt(val)) catch return RuntimeError.OutOfMemory;
        return .{ .optional = some };
    } else |_| {
        const none = ObjOptional.createNoneGC(gc) catch return RuntimeError.OutOfMemory;
        return .{ .optional = none };
    }
}

fn nativeParseFloat(_: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.WrongArity;
    if (args[0] != .string) return RuntimeError.TypeError;
    const str = args[0].string.chars;
    const gc = active_gc orelse return RuntimeError.TypeError;
    if (std.fmt.parseFloat(f64, str)) |val| {
        const some = ObjOptional.createSomeGC(gc, Value.fromFloat(val)) catch return RuntimeError.OutOfMemory;
        return .{ .optional = some };
    } else |_| {
        const none = ObjOptional.createNoneGC(gc) catch return RuntimeError.OutOfMemory;
        return .{ .optional = none };
    }
}

// ============================================================================
// Environment, Process, Stat, Timestamp Functions
// ============================================================================

fn nativeEnvGet(_: Allocator, args: []const Value) RuntimeError!Value {
    // VM: env_get returns None (not supported in bytecode VM, use native build)
    _ = args;
    const gc = active_gc orelse return RuntimeError.TypeError;
    const none = ObjOptional.createNoneGC(gc) catch return RuntimeError.OutOfMemory;
    return .{ .optional = none };
}

fn nativeEnvSet(_: Allocator, args: []const Value) RuntimeError!Value {
    // VM lacks Result value representation; env_set requires native build.
    _ = args;
    std.debug.print("error: env_set is not supported in bytecode VM (use native build)\n", .{});
    return RuntimeError.IOError;
}

fn nativeTimestampNow(_: Allocator, _: []const Value) RuntimeError!Value {
    const now: i64 = std.time.timestamp();
    return Value.fromInt(now);
}

fn nativeFsStat(_: Allocator, args: []const Value) RuntimeError!Value {
    // VM lacks Result value representation; fs_stat requires native build.
    _ = args;
    std.debug.print("error: fs_stat is not supported in bytecode VM (use native build)\n", .{});
    return RuntimeError.IOError;
}

fn nativeProcessRun(_: Allocator, args: []const Value) RuntimeError!Value {
    // VM lacks Result value representation; process_run requires native build.
    _ = args;
    std.debug.print("error: process_run is not supported in bytecode VM (use native build)\n", .{});
    return RuntimeError.IOError;
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
        .future => "<future>",
    };
}

// ============================================================================
// Helper Functions
// ============================================================================

fn getStdOut() std.fs.File {
    if (comptime builtin.os.tag == .windows) {
        return .{ .handle = std.os.windows.kernel32.GetStdHandle(std.os.windows.STD_OUTPUT_HANDLE) };
    } else {
        return .{ .handle = std.posix.STDOUT_FILENO };
    }
}

fn getStdIn() std.fs.File {
    if (comptime builtin.os.tag == .windows) {
        return .{ .handle = std.os.windows.kernel32.GetStdHandle(std.os.windows.STD_INPUT_HANDLE) };
    } else {
        return .{ .handle = std.posix.STDIN_FILENO };
    }
}

fn getStdErr() std.fs.File {
    if (comptime builtin.os.tag == .windows) {
        return .{ .handle = std.os.windows.kernel32.GetStdHandle(std.os.windows.STD_ERROR_HANDLE) };
    } else {
        return .{ .handle = std.posix.STDERR_FILENO };
    }
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

test "nativeDebug formats primitives" {
    const testing = std.testing;

    // Integer
    var result = try nativeDebug(testing.allocator, &.{Value.fromInt(42)});
    try testing.expectEqualStrings("42", result.string.chars);
    result.string.destroy(testing.allocator);

    // Negative integer
    result = try nativeDebug(testing.allocator, &.{Value.fromInt(-123)});
    try testing.expectEqualStrings("-123", result.string.chars);
    result.string.destroy(testing.allocator);

    // Float with decimal
    result = try nativeDebug(testing.allocator, &.{Value.fromFloat(3.14)});
    try testing.expectEqualStrings("3.14", result.string.chars);
    result.string.destroy(testing.allocator);

    // Whole number float (should show .0)
    result = try nativeDebug(testing.allocator, &.{Value.fromFloat(42.0)});
    try testing.expectEqualStrings("42.0", result.string.chars);
    result.string.destroy(testing.allocator);

    // Boolean true
    result = try nativeDebug(testing.allocator, &.{Value.true_val});
    try testing.expectEqualStrings("true", result.string.chars);
    result.string.destroy(testing.allocator);

    // Boolean false
    result = try nativeDebug(testing.allocator, &.{Value.false_val});
    try testing.expectEqualStrings("false", result.string.chars);
    result.string.destroy(testing.allocator);

    // Void
    result = try nativeDebug(testing.allocator, &.{Value.void_val});
    try testing.expectEqualStrings("void", result.string.chars);
    result.string.destroy(testing.allocator);
}

test "nativeDebug formats strings with quotes" {
    const testing = std.testing;

    const str = try ObjString.create(testing.allocator, "hello");
    defer str.destroy(testing.allocator);

    const result = try nativeDebug(testing.allocator, &.{.{ .string = str }});
    defer result.string.destroy(testing.allocator);

    try testing.expectEqualStrings("\"hello\"", result.string.chars);
}

test "nativeDebug formats arrays" {
    const testing = std.testing;

    const items = [_]Value{
        Value.fromInt(1),
        Value.fromInt(2),
        Value.fromInt(3),
    };
    const arr = try ObjArray.create(testing.allocator, &items);
    defer arr.destroy(testing.allocator);

    const result = try nativeDebug(testing.allocator, &.{.{ .array = arr }});
    defer result.string.destroy(testing.allocator);

    try testing.expectEqualStrings("[1, 2, 3]", result.string.chars);
}

test "nativeDebug formats tuples" {
    const testing = std.testing;

    const items = [_]Value{
        Value.fromInt(1),
        Value.true_val,
    };
    const tuple = try ObjTuple.create(testing.allocator, &items);
    defer tuple.destroy(testing.allocator);

    const result = try nativeDebug(testing.allocator, &.{.{ .tuple = tuple }});
    defer result.string.destroy(testing.allocator);

    try testing.expectEqualStrings("(1, true)", result.string.chars);
}

test "nativeDebug formats optionals" {
    const testing = std.testing;

    // None
    const none = try ObjOptional.createNone(testing.allocator);
    defer none.destroy(testing.allocator);

    var result = try nativeDebug(testing.allocator, &.{.{ .optional = none }});
    try testing.expectEqualStrings("None", result.string.chars);
    result.string.destroy(testing.allocator);

    // Some
    const some = try ObjOptional.createSome(testing.allocator, Value.fromInt(42));
    defer some.destroy(testing.allocator);

    result = try nativeDebug(testing.allocator, &.{.{ .optional = some }});
    try testing.expectEqualStrings("Some(42)", result.string.chars);
    result.string.destroy(testing.allocator);
}
