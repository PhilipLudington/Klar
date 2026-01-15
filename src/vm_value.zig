const std = @import("std");
const Allocator = std.mem.Allocator;
const chunk_mod = @import("chunk.zig");
const Function = chunk_mod.Function;

// ============================================================================
// VM Value Representation
// ============================================================================

/// Runtime value representation for the Klar VM.
/// Uses a tagged union for type safety and easy debugging.
/// Future optimization: Consider NaN-boxing for improved performance.
pub const Value = union(enum) {
    // Primitive values
    int: i128,
    float: f64,
    bool_: bool,
    char_: u21,
    void_,

    // Heap-allocated objects (pointers)
    string: *ObjString,
    array: *ObjArray,
    tuple: *ObjTuple,
    struct_: *ObjStruct,
    closure: *ObjClosure,
    function: *ObjFunction,
    native: *ObjNative,
    upvalue: *ObjUpvalue,
    optional: *ObjOptional,
    range: *ObjRange,

    // -------------------------------------------------------------------------
    // Value predicates
    // -------------------------------------------------------------------------

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
            .int => |i| i != 0,
            .void_ => false,
            else => true,
        };
    }

    pub fn isFalsey(self: Value) bool {
        return !self.isTruthy();
    }

    pub fn isNumber(self: Value) bool {
        return self == .int or self == .float;
    }

    pub fn isString(self: Value) bool {
        return self == .string;
    }

    pub fn isCallable(self: Value) bool {
        return self == .closure or self == .function or self == .native;
    }

    // -------------------------------------------------------------------------
    // Value equality
    // -------------------------------------------------------------------------

    pub fn eql(self: Value, other: Value) bool {
        // Different types are not equal (except int/float comparisons)
        if (@intFromEnum(self) != @intFromEnum(other)) {
            // Handle int/float comparison
            if (self == .int and other == .float) {
                return @as(f64, @floatFromInt(self.int)) == other.float;
            }
            if (self == .float and other == .int) {
                return self.float == @as(f64, @floatFromInt(other.int));
            }
            return false;
        }

        return switch (self) {
            .int => |i| i == other.int,
            .float => |f| f == other.float,
            .bool_ => |b| b == other.bool_,
            .char_ => |c| c == other.char_,
            .void_ => true,
            .string => |s| std.mem.eql(u8, s.chars, other.string.chars),
            .array => |a| blk: {
                const b = other.array;
                if (a.items.len != b.items.len) break :blk false;
                for (a.items, b.items) |v1, v2| {
                    if (!v1.eql(v2)) break :blk false;
                }
                break :blk true;
            },
            .tuple => |t| blk: {
                const u = other.tuple;
                if (t.items.len != u.items.len) break :blk false;
                for (t.items, u.items) |v1, v2| {
                    if (!v1.eql(v2)) break :blk false;
                }
                break :blk true;
            },
            .optional => |o| blk: {
                const p = other.optional;
                if (o.value == null and p.value == null) break :blk true;
                if (o.value == null or p.value == null) break :blk false;
                break :blk o.value.?.eql(p.value.?.*);
            },
            // Reference equality for other heap objects
            .struct_ => |s| s == other.struct_,
            .closure => |c| c == other.closure,
            .function => |f| f == other.function,
            .native => |n| n == other.native,
            .upvalue => |u| u == other.upvalue,
            .range => |r| r.start == other.range.start and r.end == other.range.end and r.inclusive == other.range.inclusive,
        };
    }

    // -------------------------------------------------------------------------
    // Type ordering for comparison
    // -------------------------------------------------------------------------

    pub fn compare(self: Value, other: Value) ?std.math.Order {
        // Can only compare same types (and int/float)
        if (self == .int and other == .int) {
            return std.math.order(self.int, other.int);
        }
        if (self == .float and other == .float) {
            return std.math.order(self.float, other.float);
        }
        if (self == .int and other == .float) {
            return std.math.order(@as(f64, @floatFromInt(self.int)), other.float);
        }
        if (self == .float and other == .int) {
            return std.math.order(self.float, @as(f64, @floatFromInt(other.int)));
        }
        if (self == .char_ and other == .char_) {
            return std.math.order(self.char_, other.char_);
        }
        if (self == .string and other == .string) {
            return std.mem.order(u8, self.string.chars, other.string.chars);
        }
        return null;
    }

    // -------------------------------------------------------------------------
    // Conversion helpers
    // -------------------------------------------------------------------------

    pub fn asInt(self: Value) ?i128 {
        return switch (self) {
            .int => |i| i,
            .float => |f| blk: {
                if (f > @as(f64, @floatFromInt(std.math.maxInt(i128))) or
                    f < @as(f64, @floatFromInt(std.math.minInt(i128))))
                {
                    break :blk null;
                }
                break :blk @intFromFloat(f);
            },
            .char_ => |c| c,
            .bool_ => |b| if (b) 1 else 0,
            else => null,
        };
    }

    pub fn asFloat(self: Value) ?f64 {
        return switch (self) {
            .float => |f| f,
            .int => |i| @floatFromInt(i),
            else => null,
        };
    }

    // -------------------------------------------------------------------------
    // Value creation helpers
    // -------------------------------------------------------------------------

    pub fn fromInt(i: i128) Value {
        return .{ .int = i };
    }

    pub fn fromFloat(f: f64) Value {
        return .{ .float = f };
    }

    pub fn fromBool(b: bool) Value {
        return .{ .bool_ = b };
    }

    pub fn fromChar(c: u21) Value {
        return .{ .char_ = c };
    }

    pub const void_val: Value = .void_;
    pub const true_val: Value = .{ .bool_ = true };
    pub const false_val: Value = .{ .bool_ = false };
};

// ============================================================================
// Heap-Allocated Object Types
// ============================================================================

/// String object.
pub const ObjString = struct {
    /// The actual string data.
    chars: []const u8,

    /// Hash for interning (0 if not computed).
    hash: u32,

    /// Whether this string owns its data (should be freed).
    owns_data: bool,

    pub fn create(allocator: Allocator, chars: []const u8) !*ObjString {
        const obj = try allocator.create(ObjString);
        const owned = try allocator.dupe(u8, chars);
        obj.* = .{
            .chars = owned,
            .hash = hashString(owned),
            .owns_data = true,
        };
        return obj;
    }

    pub fn createBorrowed(allocator: Allocator, chars: []const u8) !*ObjString {
        const obj = try allocator.create(ObjString);
        obj.* = .{
            .chars = chars,
            .hash = hashString(chars),
            .owns_data = false,
        };
        return obj;
    }

    pub fn destroy(self: *ObjString, allocator: Allocator) void {
        if (self.owns_data) {
            allocator.free(@constCast(self.chars));
        }
        allocator.destroy(self);
    }
};

/// Array object.
pub const ObjArray = struct {
    items: []Value,

    pub fn create(allocator: Allocator, items: []const Value) !*ObjArray {
        const obj = try allocator.create(ObjArray);
        obj.* = .{
            .items = try allocator.dupe(Value, items),
        };
        return obj;
    }

    pub fn destroy(self: *ObjArray, allocator: Allocator) void {
        allocator.free(self.items);
        allocator.destroy(self);
    }
};

/// Tuple object.
pub const ObjTuple = struct {
    items: []Value,

    pub fn create(allocator: Allocator, items: []const Value) !*ObjTuple {
        const obj = try allocator.create(ObjTuple);
        obj.* = .{
            .items = try allocator.dupe(Value, items),
        };
        return obj;
    }

    pub fn destroy(self: *ObjTuple, allocator: Allocator) void {
        allocator.free(self.items);
        allocator.destroy(self);
    }
};

/// Struct object.
pub const ObjStruct = struct {
    type_name: []const u8,
    fields: std.StringHashMapUnmanaged(Value),

    pub fn create(allocator: Allocator, type_name: []const u8) !*ObjStruct {
        const obj = try allocator.create(ObjStruct);
        obj.* = .{
            .type_name = type_name,
            .fields = .{},
        };
        return obj;
    }

    pub fn destroy(self: *ObjStruct, allocator: Allocator) void {
        self.fields.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn setField(self: *ObjStruct, allocator: Allocator, name: []const u8, value: Value) !void {
        try self.fields.put(allocator, name, value);
    }

    pub fn getField(self: *ObjStruct, name: []const u8) ?Value {
        return self.fields.get(name);
    }
};

/// Closure object (function + captured upvalues).
pub const ObjClosure = struct {
    function: *Function,
    upvalues: []*ObjUpvalue,

    pub fn create(allocator: Allocator, function: *Function) !*ObjClosure {
        const upvalues = try allocator.alloc(*ObjUpvalue, function.upvalue_count);
        @memset(upvalues, undefined);

        const obj = try allocator.create(ObjClosure);
        obj.* = .{
            .function = function,
            .upvalues = upvalues,
        };
        return obj;
    }

    pub fn destroy(self: *ObjClosure, allocator: Allocator) void {
        allocator.free(self.upvalues);
        allocator.destroy(self);
    }
};

/// Upvalue object (captured variable from enclosing scope).
pub const ObjUpvalue = struct {
    /// Pointer to the captured value (on stack or closed).
    location: *Value,

    /// Storage for closed-over value (when stack frame is gone).
    closed: Value,

    /// Next upvalue in the linked list of open upvalues.
    next: ?*ObjUpvalue,

    pub fn create(allocator: Allocator, slot: *Value) !*ObjUpvalue {
        const obj = try allocator.create(ObjUpvalue);
        obj.* = .{
            .location = slot,
            .closed = .void_,
            .next = null,
        };
        return obj;
    }

    pub fn destroy(self: *ObjUpvalue, allocator: Allocator) void {
        allocator.destroy(self);
    }
};

/// Function object (for script-level function references).
pub const ObjFunction = struct {
    function: *Function,

    pub fn create(allocator: Allocator, function: *Function) !*ObjFunction {
        const obj = try allocator.create(ObjFunction);
        obj.* = .{
            .function = function,
        };
        return obj;
    }

    pub fn destroy(self: *ObjFunction, allocator: Allocator) void {
        allocator.destroy(self);
    }
};

/// Native function object.
pub const ObjNative = struct {
    name: []const u8,
    arity: u8,
    function: *const fn (args: []const Value) RuntimeError!Value,

    pub fn create(
        allocator: Allocator,
        name: []const u8,
        arity: u8,
        function: *const fn (args: []const Value) RuntimeError!Value,
    ) !*ObjNative {
        const obj = try allocator.create(ObjNative);
        obj.* = .{
            .name = name,
            .arity = arity,
            .function = function,
        };
        return obj;
    }

    pub fn destroy(self: *ObjNative, allocator: Allocator) void {
        allocator.destroy(self);
    }
};

/// Optional value object.
pub const ObjOptional = struct {
    value: ?*Value,

    pub fn createNone(allocator: Allocator) !*ObjOptional {
        const obj = try allocator.create(ObjOptional);
        obj.* = .{ .value = null };
        return obj;
    }

    pub fn createSome(allocator: Allocator, value: Value) !*ObjOptional {
        const obj = try allocator.create(ObjOptional);
        const val_ptr = try allocator.create(Value);
        val_ptr.* = value;
        obj.* = .{ .value = val_ptr };
        return obj;
    }

    pub fn destroy(self: *ObjOptional, allocator: Allocator) void {
        if (self.value) |v| {
            allocator.destroy(v);
        }
        allocator.destroy(self);
    }
};

/// Range object for iteration.
pub const ObjRange = struct {
    start: i128,
    end: i128,
    inclusive: bool,
    current: i128,

    pub fn create(allocator: Allocator, start: i128, end: i128, inclusive: bool) !*ObjRange {
        const obj = try allocator.create(ObjRange);
        obj.* = .{
            .start = start,
            .end = end,
            .inclusive = inclusive,
            .current = start,
        };
        return obj;
    }

    pub fn destroy(self: *ObjRange, allocator: Allocator) void {
        allocator.destroy(self);
    }

    pub fn next(self: *ObjRange) ?i128 {
        const limit = if (self.inclusive) self.end + 1 else self.end;
        if (self.current < limit) {
            const result = self.current;
            self.current += 1;
            return result;
        }
        return null;
    }

    pub fn reset(self: *ObjRange) void {
        self.current = self.start;
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
    UndefinedField,

    // Control flow
    StackOverflow,
    StackUnderflow,

    // Index errors
    IndexOutOfBounds,

    // Optional errors
    NullUnwrap,

    // Function errors
    NotCallable,
    WrongArity,

    // Match errors
    PatternMatchFailed,

    // Assertion
    AssertionFailed,
    Panic,

    // Allocation
    OutOfMemory,
};

// ============================================================================
// Hash function for string interning
// ============================================================================

fn hashString(chars: []const u8) u32 {
    var hash: u32 = 2166136261;
    for (chars) |c| {
        hash ^= c;
        hash *%= 16777619;
    }
    return hash;
}

// ============================================================================
// Tests
// ============================================================================

test "Value creation and comparison" {
    const testing = std.testing;

    const a = Value.fromInt(42);
    const b = Value.fromInt(42);
    const c = Value.fromInt(43);

    try testing.expect(a.eql(b));
    try testing.expect(!a.eql(c));

    const order = a.compare(c);
    try testing.expect(order != null);
    try testing.expectEqual(std.math.Order.lt, order.?);
}

test "Value truthiness" {
    const testing = std.testing;

    try testing.expect(Value.true_val.isTruthy());
    try testing.expect(!Value.false_val.isTruthy());
    try testing.expect(Value.fromInt(1).isTruthy());
    try testing.expect(!Value.fromInt(0).isTruthy());
    try testing.expect(!Value.void_val.isTruthy());
}

test "String object creation" {
    const testing = std.testing;

    const str = try ObjString.create(testing.allocator, "hello");
    defer str.destroy(testing.allocator);

    try testing.expectEqualStrings("hello", str.chars);
    try testing.expect(str.hash != 0);
    try testing.expect(str.owns_data);
}

test "Array object creation" {
    const testing = std.testing;

    const items = [_]Value{
        Value.fromInt(1),
        Value.fromInt(2),
        Value.fromInt(3),
    };
    const arr = try ObjArray.create(testing.allocator, &items);
    defer arr.destroy(testing.allocator);

    try testing.expectEqual(@as(usize, 3), arr.items.len);
    try testing.expect(arr.items[0].eql(Value.fromInt(1)));
}

test "Optional object" {
    const testing = std.testing;

    const none = try ObjOptional.createNone(testing.allocator);
    defer none.destroy(testing.allocator);

    try testing.expect(none.value == null);

    const some = try ObjOptional.createSome(testing.allocator, Value.fromInt(42));
    defer some.destroy(testing.allocator);

    try testing.expect(some.value != null);
    try testing.expect(some.value.?.eql(Value.fromInt(42)));
}

test "Range iteration" {
    const testing = std.testing;

    const range = try ObjRange.create(testing.allocator, 0, 3, false);
    defer range.destroy(testing.allocator);

    var count: usize = 0;
    while (range.next()) |_| {
        count += 1;
    }
    try testing.expectEqual(@as(usize, 3), count);

    // Reset and test inclusive
    range.reset();
    range.inclusive = true;
    count = 0;
    while (range.next()) |_| {
        count += 1;
    }
    try testing.expectEqual(@as(usize, 4), count);
}
