const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("ast.zig");
const types = @import("types.zig");

// ============================================================================
// Runtime Values
// ============================================================================

/// Runtime value representation for the Klar interpreter.
/// All values during execution are represented as variants of this union.
pub const Value = union(enum) {
    // Primitive values
    int: Integer,
    float: Float,
    bool_: bool,
    char_: u21,
    string: []const u8,

    // Composite values
    array: *ArrayValue,
    tuple: *TupleValue,
    struct_: *StructValue,
    enum_: *EnumValue,

    // Optional/Result wrappers
    optional: *OptionalValue,
    result: *ResultValue,
    context_error: *ContextErrorValue,

    // References
    reference: *ReferenceValue,

    // Callable values
    function: *FunctionValue,
    closure: *ClosureValue,
    builtin: *BuiltinFunction,

    // Special values
    void_,
    never,

    pub fn isNone(self: Value) bool {
        return switch (self) {
            .optional => |opt| opt.value == null,
            else => false,
        };
    }

    pub fn isTruthy(self: Value) bool {
        return switch (self) {
            .bool_ => |b| b,
            .optional => |opt| opt.value != null,
            .int => |i| i.value != 0,
            else => true,
        };
    }

    pub fn eql(self: Value, other: Value) bool {
        if (@intFromEnum(self) != @intFromEnum(other)) return false;

        return switch (self) {
            .int => |i| i.value == other.int.value,
            .float => |f| f.value == other.float.value,
            .bool_ => |b| b == other.bool_,
            .char_ => |c| c == other.char_,
            .string => |s| std.mem.eql(u8, s, other.string),
            .void_, .never => true,
            .optional => |opt| blk: {
                const other_opt = other.optional;
                if (opt.value == null and other_opt.value == null) break :blk true;
                if (opt.value == null or other_opt.value == null) break :blk false;
                break :blk opt.value.?.eql(other_opt.value.?.*);
            },
            .tuple => |t| blk: {
                const other_t = other.tuple;
                if (t.elements.len != other_t.elements.len) break :blk false;
                for (t.elements, other_t.elements) |e1, e2| {
                    if (!e1.eql(e2)) break :blk false;
                }
                break :blk true;
            },
            .array => |a| blk: {
                const other_a = other.array;
                if (a.elements.len != other_a.elements.len) break :blk false;
                for (a.elements, other_a.elements) |e1, e2| {
                    if (!e1.eql(e2)) break :blk false;
                }
                break :blk true;
            },
            .struct_ => |s| blk: {
                const other_s = other.struct_;
                if (!std.mem.eql(u8, s.type_name, other_s.type_name)) break :blk false;
                if (s.fields.count() != other_s.fields.count()) break :blk false;
                var iter = s.fields.iterator();
                while (iter.next()) |entry| {
                    if (other_s.fields.get(entry.key_ptr.*)) |other_val| {
                        if (!entry.value_ptr.eql(other_val)) break :blk false;
                    } else {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .enum_ => |e| blk: {
                const other_e = other.enum_;
                if (!std.mem.eql(u8, e.type_name, other_e.type_name)) break :blk false;
                if (!std.mem.eql(u8, e.variant_name, other_e.variant_name)) break :blk false;
                if (e.payload == null and other_e.payload == null) break :blk true;
                if (e.payload == null or other_e.payload == null) break :blk false;
                break :blk e.payload.?.eql(other_e.payload.?.*);
            },
            else => false,
        };
    }

    pub fn clone(self: Value, allocator: Allocator) !Value {
        return switch (self) {
            .int, .float, .bool_, .char_, .void_, .never => self,
            .string => |s| .{ .string = try allocator.dupe(u8, s) },
            .array => |a| blk: {
                const new_arr = try allocator.create(ArrayValue);
                new_arr.* = .{
                    .elements = try allocator.alloc(Value, a.elements.len),
                };
                for (a.elements, 0..) |elem, i| {
                    new_arr.elements[i] = try elem.clone(allocator);
                }
                break :blk .{ .array = new_arr };
            },
            .tuple => |t| blk: {
                const new_tup = try allocator.create(TupleValue);
                new_tup.* = .{
                    .elements = try allocator.alloc(Value, t.elements.len),
                };
                for (t.elements, 0..) |elem, i| {
                    new_tup.elements[i] = try elem.clone(allocator);
                }
                break :blk .{ .tuple = new_tup };
            },
            .optional => |opt| blk: {
                const new_opt = try allocator.create(OptionalValue);
                if (opt.value) |val| {
                    const cloned = try allocator.create(Value);
                    cloned.* = try val.clone(allocator);
                    new_opt.* = .{ .value = cloned };
                } else {
                    new_opt.* = .{ .value = null };
                }
                break :blk .{ .optional = new_opt };
            },
            else => self, // References and functions are not deeply cloned
        };
    }
};

// ============================================================================
// Integer Values with Type Information
// ============================================================================

pub const Integer = struct {
    value: i128,
    type_: IntegerType,

    pub const IntegerType = enum {
        i8_,
        i16_,
        i32_,
        i64_,
        i128_,
        isize_,
        u8_,
        u16_,
        u32_,
        u64_,
        u128_,
        usize_,

        pub fn isSigned(self: IntegerType) bool {
            return switch (self) {
                .i8_, .i16_, .i32_, .i64_, .i128_, .isize_ => true,
                else => false,
            };
        }

        pub fn bitSize(self: IntegerType) u8 {
            return switch (self) {
                .i8_, .u8_ => 8,
                .i16_, .u16_ => 16,
                .i32_, .u32_ => 32,
                .i64_, .u64_ => 64,
                .i128_, .u128_ => 128,
                .isize_, .usize_ => @bitSizeOf(usize),
            };
        }

        pub fn minValue(self: IntegerType) i128 {
            return switch (self) {
                .i8_ => std.math.minInt(i8),
                .i16_ => std.math.minInt(i16),
                .i32_ => std.math.minInt(i32),
                .i64_ => std.math.minInt(i64),
                .i128_ => std.math.minInt(i128),
                .isize_ => std.math.minInt(isize),
                .u8_, .u16_, .u32_, .u64_, .u128_, .usize_ => 0,
            };
        }

        pub fn maxValue(self: IntegerType) i128 {
            return switch (self) {
                .i8_ => std.math.maxInt(i8),
                .i16_ => std.math.maxInt(i16),
                .i32_ => std.math.maxInt(i32),
                .i64_ => std.math.maxInt(i64),
                .i128_ => std.math.maxInt(i128),
                .isize_ => std.math.maxInt(isize),
                .u8_ => std.math.maxInt(u8),
                .u16_ => std.math.maxInt(u16),
                .u32_ => std.math.maxInt(u32),
                .u64_ => std.math.maxInt(u64),
                .u128_ => std.math.maxInt(i128), // Use i128 max since i128 can't represent u128 max
                .usize_ => std.math.maxInt(usize),
            };
        }

        pub fn fromPrimitive(prim: types.Primitive) ?IntegerType {
            return switch (prim) {
                .i8_ => .i8_,
                .i16_ => .i16_,
                .i32_ => .i32_,
                .i64_ => .i64_,
                .i128_ => .i128_,
                .isize_ => .isize_,
                .u8_ => .u8_,
                .u16_ => .u16_,
                .u32_ => .u32_,
                .u64_ => .u64_,
                .u128_ => .u128_,
                .usize_ => .usize_,
                else => null,
            };
        }
    };
};

pub const Float = struct {
    value: f64,
    type_: FloatType,

    pub const FloatType = enum {
        f32_,
        f64_,

        pub fn fromPrimitive(prim: types.Primitive) ?FloatType {
            return switch (prim) {
                .f32_ => .f32_,
                .f64_ => .f64_,
                else => null,
            };
        }
    };
};

// ============================================================================
// Composite Value Types
// ============================================================================

pub const ArrayValue = struct {
    elements: []Value,
};

pub const TupleValue = struct {
    elements: []Value,
};

pub const StructValue = struct {
    type_name: []const u8,
    fields: std.StringHashMapUnmanaged(Value),
};

pub const EnumValue = struct {
    type_name: []const u8,
    variant_name: []const u8,
    payload: ?*Value,
};

pub const OptionalValue = struct {
    value: ?*Value,
};

pub const ResultValue = struct {
    is_ok: bool,
    value: *Value,
};

pub const ContextErrorValue = struct {
    message: []const u8,
    cause: *Value,
};

pub const ReferenceValue = struct {
    target: *Value,
    mutable: bool,
};

// ============================================================================
// Callable Values
// ============================================================================

pub const FunctionValue = struct {
    name: []const u8,
    params: []const FunctionParam,
    body: *ast.Block,
    closure_env: ?*Environment,

    pub const FunctionParam = struct {
        name: []const u8,
    };
};

pub const ClosureValue = struct {
    params: []const ast.ClosureParam,
    body: ast.Expr,
    env: *Environment,
};

pub const BuiltinFunction = struct {
    name: []const u8,
    func: *const fn (allocator: Allocator, args: []const Value) RuntimeError!Value,
};

// ============================================================================
// Runtime Environment
// ============================================================================

pub const Environment = struct {
    allocator: Allocator,
    values: std.StringHashMapUnmanaged(ValueEntry),
    parent: ?*Environment,

    pub const ValueEntry = struct {
        value: Value,
        mutable: bool,
    };

    pub fn init(allocator: Allocator, parent: ?*Environment) Environment {
        return .{
            .allocator = allocator,
            .values = .{},
            .parent = parent,
        };
    }

    pub fn deinit(self: *Environment) void {
        self.values.deinit(self.allocator);
    }

    pub fn define(self: *Environment, name: []const u8, value: Value, mutable: bool) !void {
        try self.values.put(self.allocator, name, .{ .value = value, .mutable = mutable });
    }

    pub fn get(self: *const Environment, name: []const u8) ?Value {
        if (self.values.get(name)) |entry| {
            return entry.value;
        }
        if (self.parent) |p| {
            return p.get(name);
        }
        return null;
    }

    pub fn getEntry(self: *const Environment, name: []const u8) ?ValueEntry {
        if (self.values.get(name)) |entry| {
            return entry;
        }
        if (self.parent) |p| {
            return p.getEntry(name);
        }
        return null;
    }

    pub fn set(self: *Environment, name: []const u8, value: Value) !void {
        if (self.values.getPtr(name)) |entry| {
            if (!entry.mutable) {
                return RuntimeError.ImmutableAssignment;
            }
            entry.value = value;
            return;
        }
        if (self.parent) |p| {
            return p.set(name, value);
        }
        return RuntimeError.UndefinedVariable;
    }

    pub fn isMutable(self: *const Environment, name: []const u8) bool {
        if (self.values.get(name)) |entry| {
            return entry.mutable;
        }
        if (self.parent) |p| {
            return p.isMutable(name);
        }
        return false;
    }
};

// ============================================================================
// Runtime Errors
// ============================================================================

pub const RuntimeError = error{
    // Arithmetic errors
    IntegerOverflow,
    DivisionByZero,

    // Type errors
    TypeError,
    InvalidCast,

    // Variable errors
    UndefinedVariable,
    ImmutableAssignment,

    // Control flow
    ReturnValue,
    BreakValue,
    ContinueSignal,

    // Other errors
    IndexOutOfBounds,
    NullUnwrap,
    PatternMatchFailed,
    InvalidOperation,
    NotImplemented,
    AssertionFailed,
    Panic,
    IOError,

    // Comptime errors
    ComptimeError,

    // Allocation
    OutOfMemory,
};

// ============================================================================
// Value Builder - Helper for creating values
// ============================================================================

pub const ValueBuilder = struct {
    allocator: Allocator,

    pub fn init(allocator: Allocator) ValueBuilder {
        return .{ .allocator = allocator };
    }

    pub fn int(self: *ValueBuilder, value: i128, type_: Integer.IntegerType) Value {
        _ = self;
        return .{ .int = .{ .value = value, .type_ = type_ } };
    }

    pub fn i32Val(self: *ValueBuilder, value: i32) Value {
        _ = self;
        return .{ .int = .{ .value = value, .type_ = .i32_ } };
    }

    pub fn i64Val(self: *ValueBuilder, value: i64) Value {
        _ = self;
        return .{ .int = .{ .value = value, .type_ = .i64_ } };
    }

    pub fn float(self: *ValueBuilder, value: f64, type_: Float.FloatType) Value {
        _ = self;
        return .{ .float = .{ .value = value, .type_ = type_ } };
    }

    pub fn f64Val(self: *ValueBuilder, value: f64) Value {
        _ = self;
        return .{ .float = .{ .value = value, .type_ = .f64_ } };
    }

    pub fn boolean(self: *ValueBuilder, value: bool) Value {
        _ = self;
        return .{ .bool_ = value };
    }

    pub fn char(self: *ValueBuilder, value: u21) Value {
        _ = self;
        return .{ .char_ = value };
    }

    pub fn string(self: *ValueBuilder, value: []const u8) Value {
        _ = self;
        return .{ .string = value };
    }

    pub fn voidVal(self: *ValueBuilder) Value {
        _ = self;
        return .void_;
    }

    pub fn none(self: *ValueBuilder) !Value {
        const opt = try self.allocator.create(OptionalValue);
        opt.* = .{ .value = null };
        return .{ .optional = opt };
    }

    pub fn some(self: *ValueBuilder, value: Value) !Value {
        const opt = try self.allocator.create(OptionalValue);
        const val_ptr = try self.allocator.create(Value);
        val_ptr.* = value;
        opt.* = .{ .value = val_ptr };
        return .{ .optional = opt };
    }

    pub fn array(self: *ValueBuilder, elements: []const Value) !Value {
        const arr = try self.allocator.create(ArrayValue);
        arr.* = .{
            .elements = try self.allocator.dupe(Value, elements),
        };
        return .{ .array = arr };
    }

    pub fn tuple(self: *ValueBuilder, elements: []const Value) !Value {
        const tup = try self.allocator.create(TupleValue);
        tup.* = .{
            .elements = try self.allocator.dupe(Value, elements),
        };
        return .{ .tuple = tup };
    }

    pub fn structVal(self: *ValueBuilder, type_name: []const u8) !*StructValue {
        const s = try self.allocator.create(StructValue);
        s.* = .{
            .type_name = type_name,
            .fields = .{},
        };
        return s;
    }

    pub fn enumVal(self: *ValueBuilder, type_name: []const u8, variant_name: []const u8, payload: ?Value) !Value {
        const e = try self.allocator.create(EnumValue);
        var payload_ptr: ?*Value = null;
        if (payload) |p| {
            payload_ptr = try self.allocator.create(Value);
            payload_ptr.?.* = p;
        }
        e.* = .{
            .type_name = type_name,
            .variant_name = variant_name,
            .payload = payload_ptr,
        };
        return .{ .enum_ = e };
    }
};

// ============================================================================
// Value Formatting
// ============================================================================

pub fn formatValue(writer: anytype, value: Value) !void {
    switch (value) {
        .int => |i| try writer.print("{d}", .{i.value}),
        .float => |f| try writer.print("{d}", .{f.value}),
        .bool_ => |b| try writer.writeAll(if (b) "true" else "false"),
        .char_ => |c| {
            var buf: [4]u8 = undefined;
            const len = std.unicode.utf8Encode(c, &buf) catch 0;
            try writer.print("'{s}'", .{buf[0..len]});
        },
        .string => |s| try writer.print("\"{s}\"", .{s}),
        .void_ => try writer.writeAll("void"),
        .never => try writer.writeAll("!"),
        .array => |a| {
            try writer.writeAll("[");
            for (a.elements, 0..) |elem, i| {
                if (i > 0) try writer.writeAll(", ");
                try formatValue(writer, elem);
            }
            try writer.writeAll("]");
        },
        .tuple => |t| {
            try writer.writeAll("(");
            for (t.elements, 0..) |elem, i| {
                if (i > 0) try writer.writeAll(", ");
                try formatValue(writer, elem);
            }
            try writer.writeAll(")");
        },
        .struct_ => |s| {
            try writer.print("{s} {{ ", .{s.type_name});
            var iter = s.fields.iterator();
            var first = true;
            while (iter.next()) |entry| {
                if (!first) try writer.writeAll(", ");
                first = false;
                try writer.print("{s}: ", .{entry.key_ptr.*});
                try formatValue(writer, entry.value_ptr.*);
            }
            try writer.writeAll(" }");
        },
        .enum_ => |e| {
            try writer.print("{s}.{s}", .{ e.type_name, e.variant_name });
            if (e.payload) |p| {
                try writer.writeAll("(");
                try formatValue(writer, p.*);
                try writer.writeAll(")");
            }
        },
        .optional => |opt| {
            if (opt.value) |v| {
                try writer.writeAll("Some(");
                try formatValue(writer, v.*);
                try writer.writeAll(")");
            } else {
                try writer.writeAll("None");
            }
        },
        .result => |r| {
            if (r.is_ok) {
                try writer.writeAll("Ok(");
            } else {
                try writer.writeAll("Err(");
            }
            try formatValue(writer, r.value.*);
            try writer.writeAll(")");
        },
        .context_error => |ce| {
            try writer.writeAll("ContextError { message: \"");
            try writer.writeAll(ce.message);
            try writer.writeAll("\", cause: ");
            try formatValue(writer, ce.cause.*);
            try writer.writeAll(" }");
        },
        .reference => |r| {
            if (r.mutable) {
                try writer.writeAll("&mut ");
            } else {
                try writer.writeAll("&");
            }
            try formatValue(writer, r.target.*);
        },
        .function => |f| try writer.print("<fn {s}>", .{f.name}),
        .closure => try writer.writeAll("<closure>"),
        .builtin => |b| try writer.print("<builtin {s}>", .{b.name}),
    }
}

pub fn valueToString(allocator: Allocator, value: Value) ![]u8 {
    var list: std.ArrayListUnmanaged(u8) = .{};
    errdefer list.deinit(allocator);
    try formatValue(list.writer(allocator), value);
    return list.toOwnedSlice(allocator);
}

// ============================================================================
// Tests
// ============================================================================

test "Integer value creation" {
    const testing = std.testing;
    var builder = ValueBuilder.init(testing.allocator);

    const i32_val = builder.i32Val(42);
    try testing.expect(i32_val == .int);
    try testing.expectEqual(@as(i128, 42), i32_val.int.value);
    try testing.expectEqual(Integer.IntegerType.i32_, i32_val.int.type_);
}

test "Value equality" {
    const testing = std.testing;
    var builder = ValueBuilder.init(testing.allocator);

    const a = builder.i32Val(42);
    const b = builder.i32Val(42);
    const c = builder.i32Val(43);

    try testing.expect(a.eql(b));
    try testing.expect(!a.eql(c));

    const s1 = builder.string("hello");
    const s2 = builder.string("hello");
    const s3 = builder.string("world");

    try testing.expect(s1.eql(s2));
    try testing.expect(!s1.eql(s3));
}

test "Optional values" {
    const testing = std.testing;
    var builder = ValueBuilder.init(testing.allocator);

    const none_val = try builder.none();
    defer testing.allocator.destroy(none_val.optional);

    try testing.expect(none_val.isNone());
    try testing.expect(!none_val.isTruthy());

    const some_val = try builder.some(builder.i32Val(42));
    defer {
        testing.allocator.destroy(some_val.optional.value.?);
        testing.allocator.destroy(some_val.optional);
    }

    try testing.expect(!some_val.isNone());
    try testing.expect(some_val.isTruthy());
}

test "Array values" {
    const testing = std.testing;
    var builder = ValueBuilder.init(testing.allocator);

    const elements = [_]Value{
        builder.i32Val(1),
        builder.i32Val(2),
        builder.i32Val(3),
    };

    const arr = try builder.array(&elements);
    defer {
        testing.allocator.free(arr.array.elements);
        testing.allocator.destroy(arr.array);
    }

    try testing.expectEqual(@as(usize, 3), arr.array.elements.len);
    try testing.expect(arr.array.elements[0].eql(builder.i32Val(1)));
}

test "Environment basic operations" {
    const testing = std.testing;
    var builder = ValueBuilder.init(testing.allocator);

    var env = Environment.init(testing.allocator, null);
    defer env.deinit();

    try env.define("x", builder.i32Val(42), false);
    try env.define("y", builder.i32Val(10), true);

    const x = env.get("x");
    try testing.expect(x != null);
    try testing.expect(x.?.eql(builder.i32Val(42)));

    // Cannot assign to immutable
    try testing.expectError(RuntimeError.ImmutableAssignment, env.set("x", builder.i32Val(100)));

    // Can assign to mutable
    try env.set("y", builder.i32Val(20));
    const y = env.get("y");
    try testing.expect(y.?.eql(builder.i32Val(20)));
}

test "Environment scoping" {
    const testing = std.testing;
    var builder = ValueBuilder.init(testing.allocator);

    var parent = Environment.init(testing.allocator, null);
    defer parent.deinit();

    try parent.define("x", builder.i32Val(42), false);

    var child = Environment.init(testing.allocator, &parent);
    defer child.deinit();

    // Child can see parent's variables
    const x = child.get("x");
    try testing.expect(x != null);

    // Child can shadow parent's variables
    try child.define("x", builder.i32Val(100), false);
    const x_child = child.get("x");
    try testing.expect(x_child.?.eql(builder.i32Val(100)));

    // Parent still has original value
    const x_parent = parent.get("x");
    try testing.expect(x_parent.?.eql(builder.i32Val(42)));
}

test "Value formatting" {
    const testing = std.testing;
    var builder = ValueBuilder.init(testing.allocator);

    const int_val = builder.i32Val(42);
    const int_str = try valueToString(testing.allocator, int_val);
    defer testing.allocator.free(int_str);
    try testing.expectEqualStrings("42", int_str);

    const bool_val = builder.boolean(true);
    const bool_str = try valueToString(testing.allocator, bool_val);
    defer testing.allocator.free(bool_str);
    try testing.expectEqualStrings("true", bool_str);

    const str_val = builder.string("hello");
    const str_str = try valueToString(testing.allocator, str_val);
    defer testing.allocator.free(str_str);
    try testing.expectEqualStrings("\"hello\"", str_str);
}

test "Integer type properties" {
    const testing = std.testing;

    try testing.expect(Integer.IntegerType.i32_.isSigned());
    try testing.expect(!Integer.IntegerType.u32_.isSigned());
    try testing.expectEqual(@as(u8, 32), Integer.IntegerType.i32_.bitSize());
    try testing.expectEqual(@as(i128, std.math.minInt(i32)), Integer.IntegerType.i32_.minValue());
    try testing.expectEqual(@as(i128, std.math.maxInt(i32)), Integer.IntegerType.i32_.maxValue());
}
