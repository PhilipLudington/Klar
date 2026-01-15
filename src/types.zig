const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("ast.zig");
const Span = ast.Span;

// ============================================================================
// Type Representation
// ============================================================================

/// Core type representation for the Klar type system.
/// All types are represented as variants of this union.
pub const Type = union(enum) {
    // Primitive types
    primitive: Primitive,

    // Composite types
    array: *ArrayType,
    slice: *SliceType,
    tuple: *TupleType,
    optional: *Type,
    result: *ResultType,
    function: *FunctionType,
    reference: *ReferenceType,

    // User-defined types
    struct_: *StructType,
    enum_: *EnumType,
    trait_: *TraitType,

    // Generic types
    type_var: TypeVar,
    applied: *AppliedType,

    // Special types
    void_,
    never,
    unknown,
    error_type,

    pub fn eql(self: Type, other: Type) bool {
        if (@intFromEnum(self) != @intFromEnum(other)) return false;

        return switch (self) {
            .primitive => |p| other.primitive == p,
            .array => |a| a.element.eql(other.array.element) and a.size == other.array.size,
            .slice => |s| s.element.eql(other.slice.element),
            .tuple => |t| blk: {
                const ot = other.tuple;
                if (t.elements.len != ot.elements.len) break :blk false;
                for (t.elements, ot.elements) |e1, e2| {
                    if (!e1.eql(e2)) break :blk false;
                }
                break :blk true;
            },
            .optional => |o| o.eql(other.optional.*),
            .result => |r| r.ok_type.eql(other.result.ok_type) and r.err_type.eql(other.result.err_type),
            .function => |f| blk: {
                const of = other.function;
                if (f.params.len != of.params.len) break :blk false;
                for (f.params, of.params) |p1, p2| {
                    if (!p1.eql(p2)) break :blk false;
                }
                break :blk f.return_type.eql(of.return_type);
            },
            .reference => |r| r.inner.eql(other.reference.inner) and r.mutable == other.reference.mutable,
            .struct_ => |s| std.mem.eql(u8, s.name, other.struct_.name),
            .enum_ => |e| std.mem.eql(u8, e.name, other.enum_.name),
            .trait_ => |t| std.mem.eql(u8, t.name, other.trait_.name),
            .type_var => |tv| tv.id == other.type_var.id,
            .applied => |a| blk: {
                const oa = other.applied;
                if (!a.base.eql(oa.base)) break :blk false;
                if (a.args.len != oa.args.len) break :blk false;
                for (a.args, oa.args) |arg1, arg2| {
                    if (!arg1.eql(arg2)) break :blk false;
                }
                break :blk true;
            },
            .void_, .never, .unknown, .error_type => true,
        };
    }

    pub fn isNumeric(self: Type) bool {
        return switch (self) {
            .primitive => |p| p.isNumeric(),
            else => false,
        };
    }

    pub fn isInteger(self: Type) bool {
        return switch (self) {
            .primitive => |p| p.isInteger(),
            else => false,
        };
    }

    pub fn isFloat(self: Type) bool {
        return switch (self) {
            .primitive => |p| p.isFloat(),
            else => false,
        };
    }

    pub fn isSigned(self: Type) bool {
        return switch (self) {
            .primitive => |p| p.isSigned(),
            else => false,
        };
    }
};

// ============================================================================
// Primitive Types
// ============================================================================

pub const Primitive = enum {
    // Signed integers
    i8_,
    i16_,
    i32_,
    i64_,
    i128_,
    isize_,

    // Unsigned integers
    u8_,
    u16_,
    u32_,
    u64_,
    u128_,
    usize_,

    // Floating point
    f32_,
    f64_,

    // Other primitives
    bool_,
    char_,
    string_,

    pub fn isNumeric(self: Primitive) bool {
        return self.isInteger() or self.isFloat();
    }

    pub fn isInteger(self: Primitive) bool {
        return switch (self) {
            .i8_, .i16_, .i32_, .i64_, .i128_, .isize_ => true,
            .u8_, .u16_, .u32_, .u64_, .u128_, .usize_ => true,
            else => false,
        };
    }

    pub fn isFloat(self: Primitive) bool {
        return switch (self) {
            .f32_, .f64_ => true,
            else => false,
        };
    }

    pub fn isSigned(self: Primitive) bool {
        return switch (self) {
            .i8_, .i16_, .i32_, .i64_, .i128_, .isize_ => true,
            .f32_, .f64_ => true,
            else => false,
        };
    }

    pub fn bitSize(self: Primitive) ?u16 {
        return switch (self) {
            .i8_, .u8_ => 8,
            .i16_, .u16_ => 16,
            .i32_, .u32_, .f32_ => 32,
            .i64_, .u64_, .f64_ => 64,
            .i128_, .u128_ => 128,
            .isize_, .usize_ => null, // platform dependent
            .bool_ => 1,
            .char_ => 21, // unicode scalar (up to 21 bits)
            .string_ => null,
        };
    }

    pub fn name(self: Primitive) []const u8 {
        return switch (self) {
            .i8_ => "i8",
            .i16_ => "i16",
            .i32_ => "i32",
            .i64_ => "i64",
            .i128_ => "i128",
            .isize_ => "isize",
            .u8_ => "u8",
            .u16_ => "u16",
            .u32_ => "u32",
            .u64_ => "u64",
            .u128_ => "u128",
            .usize_ => "usize",
            .f32_ => "f32",
            .f64_ => "f64",
            .bool_ => "bool",
            .char_ => "char",
            .string_ => "string",
        };
    }

    pub fn fromName(name_str: []const u8) ?Primitive {
        const primitives = std.StaticStringMap(Primitive).initComptime(.{
            .{ "i8", .i8_ },
            .{ "i16", .i16_ },
            .{ "i32", .i32_ },
            .{ "i64", .i64_ },
            .{ "i128", .i128_ },
            .{ "isize", .isize_ },
            .{ "u8", .u8_ },
            .{ "u16", .u16_ },
            .{ "u32", .u32_ },
            .{ "u64", .u64_ },
            .{ "u128", .u128_ },
            .{ "usize", .usize_ },
            .{ "f32", .f32_ },
            .{ "f64", .f64_ },
            .{ "bool", .bool_ },
            .{ "char", .char_ },
            .{ "string", .string_ },
        });
        return primitives.get(name_str);
    }

    /// Check if widening conversion from self to other is safe
    pub fn canWidenTo(self: Primitive, other: Primitive) bool {
        if (self == other) return true;

        // Get sizes
        const self_size = self.bitSize() orelse return false;
        const other_size = other.bitSize() orelse return false;

        // Float widening
        if (self.isFloat() and other.isFloat()) {
            return self_size <= other_size;
        }

        // Integer widening
        if (self.isInteger() and other.isInteger()) {
            // Signed can widen to larger signed
            if (self.isSigned() and other.isSigned()) {
                return self_size <= other_size;
            }
            // Unsigned can widen to larger unsigned
            if (!self.isSigned() and !other.isSigned()) {
                return self_size <= other_size;
            }
            // Unsigned can widen to larger signed (needs extra bit)
            if (!self.isSigned() and other.isSigned()) {
                return self_size < other_size;
            }
            // Signed cannot safely widen to unsigned
            return false;
        }

        return false;
    }
};

// ============================================================================
// Composite Types
// ============================================================================

pub const ArrayType = struct {
    element: Type,
    size: usize,
};

pub const SliceType = struct {
    element: Type,
};

pub const TupleType = struct {
    elements: []const Type,
};

pub const ResultType = struct {
    ok_type: Type,
    err_type: Type,
};

pub const FunctionType = struct {
    params: []const Type,
    return_type: Type,
    is_async: bool = false,
};

pub const ReferenceType = struct {
    inner: Type,
    mutable: bool,
};

// ============================================================================
// User-Defined Types
// ============================================================================

pub const StructType = struct {
    name: []const u8,
    type_params: []const TypeVar,
    fields: []const StructField,
    traits: []const *TraitType,
    is_copy: bool = false,
};

pub const StructField = struct {
    name: []const u8,
    type_: Type,
    is_pub: bool,
};

pub const EnumType = struct {
    name: []const u8,
    type_params: []const TypeVar,
    variants: []const EnumVariant,
};

pub const EnumVariant = struct {
    name: []const u8,
    payload: ?VariantPayload,
};

pub const VariantPayload = union(enum) {
    tuple: []const Type,
    struct_: []const StructField,
};

pub const TraitType = struct {
    name: []const u8,
    type_params: []const TypeVar,
    methods: []const TraitMethod,
    super_traits: []const *TraitType,
};

pub const TraitMethod = struct {
    name: []const u8,
    signature: FunctionType,
    has_default: bool,
};

// ============================================================================
// Generic Types
// ============================================================================

/// Type variable for generics (e.g., T in List[T])
pub const TypeVar = struct {
    id: u32,
    name: []const u8,
    bounds: []const *TraitType,
};

/// Applied generic type (e.g., List[i32])
pub const AppliedType = struct {
    base: Type,
    args: []const Type,
};

// ============================================================================
// Type Builder - Arena-based type allocation
// ============================================================================

pub const TypeBuilder = struct {
    arena: std.heap.ArenaAllocator,
    next_type_var_id: u32 = 0,

    pub fn init(alloc: Allocator) TypeBuilder {
        return .{
            .arena = std.heap.ArenaAllocator.init(alloc),
        };
    }

    pub fn deinit(self: *TypeBuilder) void {
        self.arena.deinit();
    }

    pub fn getAllocator(self: *TypeBuilder) Allocator {
        return self.arena.allocator();
    }

    // Primitive type constructors
    pub fn i32Type(self: *TypeBuilder) Type {
        _ = self;
        return .{ .primitive = .i32_ };
    }

    pub fn i64Type(self: *TypeBuilder) Type {
        _ = self;
        return .{ .primitive = .i64_ };
    }

    pub fn f64Type(self: *TypeBuilder) Type {
        _ = self;
        return .{ .primitive = .f64_ };
    }

    pub fn boolType(self: *TypeBuilder) Type {
        _ = self;
        return .{ .primitive = .bool_ };
    }

    pub fn charType(self: *TypeBuilder) Type {
        _ = self;
        return .{ .primitive = .char_ };
    }

    pub fn stringType(self: *TypeBuilder) Type {
        _ = self;
        return .{ .primitive = .string_ };
    }

    pub fn voidType(self: *TypeBuilder) Type {
        _ = self;
        return .void_;
    }

    pub fn neverType(self: *TypeBuilder) Type {
        _ = self;
        return .never;
    }

    pub fn unknownType(self: *TypeBuilder) Type {
        _ = self;
        return .unknown;
    }

    // Composite type constructors
    pub fn arrayType(self: *TypeBuilder, element: Type, size: usize) !Type {
        const arr = try self.arena.allocator().create(ArrayType);
        arr.* = .{ .element = element, .size = size };
        return .{ .array = arr };
    }

    pub fn sliceType(self: *TypeBuilder, element: Type) !Type {
        const slice = try self.arena.allocator().create(SliceType);
        slice.* = .{ .element = element };
        return .{ .slice = slice };
    }

    pub fn tupleType(self: *TypeBuilder, elements: []const Type) !Type {
        const tuple = try self.arena.allocator().create(TupleType);
        const elems = try self.arena.allocator().dupe(Type, elements);
        tuple.* = .{ .elements = elems };
        return .{ .tuple = tuple };
    }

    pub fn optionalType(self: *TypeBuilder, inner: Type) !Type {
        const opt = try self.arena.allocator().create(Type);
        opt.* = inner;
        return .{ .optional = opt };
    }

    pub fn resultType(self: *TypeBuilder, ok_type: Type, err_type: Type) !Type {
        const result = try self.arena.allocator().create(ResultType);
        result.* = .{ .ok_type = ok_type, .err_type = err_type };
        return .{ .result = result };
    }

    pub fn functionType(self: *TypeBuilder, params: []const Type, return_type: Type) !Type {
        const func = try self.arena.allocator().create(FunctionType);
        const params_copy = try self.arena.allocator().dupe(Type, params);
        func.* = .{ .params = params_copy, .return_type = return_type };
        return .{ .function = func };
    }

    pub fn referenceType(self: *TypeBuilder, inner: Type, mutable: bool) !Type {
        const ref = try self.arena.allocator().create(ReferenceType);
        ref.* = .{ .inner = inner, .mutable = mutable };
        return .{ .reference = ref };
    }

    // Type variable creation
    pub fn newTypeVar(self: *TypeBuilder, name_str: []const u8) !TypeVar {
        const id = self.next_type_var_id;
        self.next_type_var_id += 1;
        const name_copy = try self.arena.allocator().dupe(u8, name_str);
        return .{
            .id = id,
            .name = name_copy,
            .bounds = &.{},
        };
    }

    pub fn typeVarType(self: *TypeBuilder, name_str: []const u8) !Type {
        return .{ .type_var = try self.newTypeVar(name_str) };
    }

    // Applied type constructor
    pub fn appliedType(self: *TypeBuilder, base: Type, args: []const Type) !Type {
        const applied = try self.arena.allocator().create(AppliedType);
        const args_copy = try self.arena.allocator().dupe(Type, args);
        applied.* = .{ .base = base, .args = args_copy };
        return .{ .applied = applied };
    }
};

// ============================================================================
// Type Formatting
// ============================================================================

pub fn formatType(writer: anytype, t: Type) !void {
    switch (t) {
        .primitive => |p| try writer.writeAll(p.name()),
        .array => |a| {
            try writer.writeAll("[");
            try formatType(writer, a.element);
            try writer.print("; {d}]", .{a.size});
        },
        .slice => |s| {
            try writer.writeAll("[");
            try formatType(writer, s.element);
            try writer.writeAll("]");
        },
        .tuple => |tup| {
            try writer.writeAll("(");
            for (tup.elements, 0..) |elem, i| {
                if (i > 0) try writer.writeAll(", ");
                try formatType(writer, elem);
            }
            try writer.writeAll(")");
        },
        .optional => |o| {
            try writer.writeAll("?");
            try formatType(writer, o.*);
        },
        .result => |r| {
            try writer.writeAll("Result[");
            try formatType(writer, r.ok_type);
            try writer.writeAll(", ");
            try formatType(writer, r.err_type);
            try writer.writeAll("]");
        },
        .function => |f| {
            try writer.writeAll("fn(");
            for (f.params, 0..) |param, i| {
                if (i > 0) try writer.writeAll(", ");
                try formatType(writer, param);
            }
            try writer.writeAll(") -> ");
            try formatType(writer, f.return_type);
        },
        .reference => |r| {
            if (r.mutable) {
                try writer.writeAll("&mut ");
            } else {
                try writer.writeAll("&");
            }
            try formatType(writer, r.inner);
        },
        .struct_ => |s| try writer.writeAll(s.name),
        .enum_ => |e| try writer.writeAll(e.name),
        .trait_ => |tr| try writer.writeAll(tr.name),
        .type_var => |tv| try writer.writeAll(tv.name),
        .applied => |a| {
            try formatType(writer, a.base);
            try writer.writeAll("[");
            for (a.args, 0..) |arg, i| {
                if (i > 0) try writer.writeAll(", ");
                try formatType(writer, arg);
            }
            try writer.writeAll("]");
        },
        .void_ => try writer.writeAll("void"),
        .never => try writer.writeAll("!"),
        .unknown => try writer.writeAll("?unknown"),
        .error_type => try writer.writeAll("?error"),
    }
}

pub fn typeToString(allocator: Allocator, t: Type) ![]u8 {
    var list: std.ArrayListUnmanaged(u8) = .{};
    errdefer list.deinit(allocator);
    try formatType(list.writer(allocator), t);
    return list.toOwnedSlice(allocator);
}

// ============================================================================
// Tests
// ============================================================================

test "Primitive type properties" {
    const testing = std.testing;

    try testing.expect(Primitive.i32_.isInteger());
    try testing.expect(Primitive.i32_.isSigned());
    try testing.expect(!Primitive.u32_.isSigned());
    try testing.expect(Primitive.f64_.isFloat());
    try testing.expect(!Primitive.bool_.isNumeric());
    try testing.expectEqual(@as(u16, 32), Primitive.i32_.bitSize().?);
    try testing.expectEqualStrings("i32", Primitive.i32_.name());
}

test "Primitive widening" {
    const testing = std.testing;

    // Same type
    try testing.expect(Primitive.i32_.canWidenTo(.i32_));

    // Valid widening
    try testing.expect(Primitive.i32_.canWidenTo(.i64_));
    try testing.expect(Primitive.u8_.canWidenTo(.u16_));
    try testing.expect(Primitive.f32_.canWidenTo(.f64_));

    // Unsigned to larger signed
    try testing.expect(Primitive.u8_.canWidenTo(.i16_));
    try testing.expect(!Primitive.u8_.canWidenTo(.i8_)); // Not enough room

    // Invalid: signed to unsigned
    try testing.expect(!Primitive.i32_.canWidenTo(.u64_));

    // Invalid: narrowing
    try testing.expect(!Primitive.i64_.canWidenTo(.i32_));
}

test "Primitive fromName" {
    const testing = std.testing;

    try testing.expectEqual(Primitive.i32_, Primitive.fromName("i32").?);
    try testing.expectEqual(Primitive.string_, Primitive.fromName("string").?);
    try testing.expect(Primitive.fromName("invalid") == null);
}

test "TypeBuilder basic types" {
    const testing = std.testing;

    var builder = TypeBuilder.init(testing.allocator);
    defer builder.deinit();

    const i32_t = builder.i32Type();
    try testing.expect(i32_t.isInteger());
    try testing.expect(!i32_t.isFloat());

    const bool_t = builder.boolType();
    try testing.expect(!bool_t.isNumeric());
}

test "TypeBuilder composite types" {
    const testing = std.testing;

    var builder = TypeBuilder.init(testing.allocator);
    defer builder.deinit();

    // Array type
    const arr_t = try builder.arrayType(builder.i32Type(), 10);
    try testing.expect(arr_t == .array);
    try testing.expectEqual(@as(usize, 10), arr_t.array.size);

    // Optional type
    const opt_t = try builder.optionalType(builder.stringType());
    try testing.expect(opt_t == .optional);

    // Function type
    const func_t = try builder.functionType(&.{ builder.i32Type(), builder.i32Type() }, builder.i32Type());
    try testing.expect(func_t == .function);
    try testing.expectEqual(@as(usize, 2), func_t.function.params.len);
}

test "Type equality" {
    const testing = std.testing;

    var builder = TypeBuilder.init(testing.allocator);
    defer builder.deinit();

    const i32_1 = builder.i32Type();
    const i32_2 = builder.i32Type();
    const i64_t = builder.i64Type();

    try testing.expect(i32_1.eql(i32_2));
    try testing.expect(!i32_1.eql(i64_t));

    // Composite equality
    const arr1 = try builder.arrayType(builder.i32Type(), 10);
    const arr2 = try builder.arrayType(builder.i32Type(), 10);
    const arr3 = try builder.arrayType(builder.i32Type(), 20);

    try testing.expect(arr1.eql(arr2));
    try testing.expect(!arr1.eql(arr3));
}

test "Type formatting" {
    const testing = std.testing;

    var builder = TypeBuilder.init(testing.allocator);
    defer builder.deinit();

    // Primitive
    const i32_str = try typeToString(testing.allocator, builder.i32Type());
    defer testing.allocator.free(i32_str);
    try testing.expectEqualStrings("i32", i32_str);

    // Optional
    const opt_t = try builder.optionalType(builder.stringType());
    const opt_str = try typeToString(testing.allocator, opt_t);
    defer testing.allocator.free(opt_str);
    try testing.expectEqualStrings("?string", opt_str);

    // Function
    const func_t = try builder.functionType(&.{ builder.i32Type(), builder.i32Type() }, builder.boolType());
    const func_str = try typeToString(testing.allocator, func_t);
    defer testing.allocator.free(func_str);
    try testing.expectEqualStrings("fn(i32, i32) -> bool", func_str);
}
