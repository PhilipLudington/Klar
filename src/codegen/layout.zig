//! Memory layout calculation for composite types.
//!
//! Implements C ABI-compatible memory layout for structs, arrays, and tuples.
//! Handles alignment, padding, and size calculation.

const std = @import("std");
const Allocator = std.mem.Allocator;
const types = @import("../types.zig");
const llvm = @import("llvm.zig");
const target = @import("target.zig");

/// Layout information for a struct type.
pub const StructLayout = struct {
    /// Field offsets in bytes (same order as fields in struct).
    field_offsets: []const usize,
    /// Total size of the struct in bytes.
    size: usize,
    /// Alignment requirement in bytes.
    alignment: usize,
    /// Whether the struct is packed (no padding).
    is_packed: bool,

    /// Get the offset of a field by index.
    pub fn getFieldOffset(self: StructLayout, field_index: usize) ?usize {
        if (field_index >= self.field_offsets.len) return null;
        return self.field_offsets[field_index];
    }
};

/// Layout information for an array type.
pub const ArrayLayout = struct {
    /// Size of each element in bytes.
    element_size: usize,
    /// Alignment of each element.
    element_alignment: usize,
    /// Total size of the array in bytes.
    size: usize,
    /// Number of elements.
    count: usize,
};

/// Layout information for a tuple type.
pub const TupleLayout = struct {
    /// Element offsets in bytes.
    element_offsets: []const usize,
    /// Total size of the tuple in bytes.
    size: usize,
    /// Alignment requirement in bytes.
    alignment: usize,
};

/// Layout calculator for composite types.
pub const LayoutCalculator = struct {
    allocator: Allocator,
    platform: target.Platform,
    /// Cache of calculated layouts.
    struct_layouts: std.StringHashMap(StructLayout),

    pub fn init(allocator: Allocator, plat: target.Platform) LayoutCalculator {
        return .{
            .allocator = allocator,
            .platform = plat,
            .struct_layouts = std.StringHashMap(StructLayout).init(allocator),
        };
    }

    pub fn deinit(self: *LayoutCalculator) void {
        var it = self.struct_layouts.valueIterator();
        while (it.next()) |layout| {
            self.allocator.free(layout.field_offsets);
        }
        self.struct_layouts.deinit();
    }

    /// Get the size and alignment of a primitive type.
    pub fn getPrimitiveLayout(self: *LayoutCalculator, prim: types.Primitive) struct { size: usize, alignment: usize } {
        const pointer_size = self.platform.getPointerSize();

        return switch (prim) {
            .i8_, .u8_ => .{ .size = 1, .alignment = 1 },
            .i16_, .u16_ => .{ .size = 2, .alignment = 2 },
            .i32_, .u32_, .f32_ => .{ .size = 4, .alignment = 4 },
            .i64_, .u64_, .f64_ => .{ .size = 8, .alignment = 8 },
            .i128_, .u128_ => .{ .size = 16, .alignment = 16 },
            .isize_, .usize_ => .{ .size = pointer_size, .alignment = pointer_size },
            .bool_ => .{ .size = 1, .alignment = 1 },
            .char_ => .{ .size = 4, .alignment = 4 }, // Unicode scalar value
            .string_ => .{ .size = pointer_size * 2, .alignment = pointer_size }, // ptr + len
        };
    }

    /// Get the size and alignment of a type.
    pub fn getTypeLayout(self: *LayoutCalculator, t: types.Type) struct { size: usize, alignment: usize } {
        const pointer_size = self.platform.getPointerSize();

        return switch (t) {
            .primitive => |p| self.getPrimitiveLayout(p),
            .reference => .{ .size = pointer_size, .alignment = pointer_size },
            .optional => |inner| {
                // Optional adds a tag byte + alignment padding + inner type
                const inner_layout = self.getTypeLayout(inner.*);
                const tag_with_padding = alignUp(1, inner_layout.alignment);
                return .{
                    .size = tag_with_padding + inner_layout.size,
                    .alignment = inner_layout.alignment,
                };
            },
            .array => |arr| {
                const elem_layout = self.getTypeLayout(arr.element);
                return .{
                    .size = elem_layout.size * arr.size,
                    .alignment = elem_layout.alignment,
                };
            },
            .slice => {
                // Slice is ptr + len
                return .{ .size = pointer_size * 2, .alignment = pointer_size };
            },
            .tuple => |tup| {
                var max_align: usize = 1;
                var offset: usize = 0;
                for (tup.elements) |elem| {
                    const elem_layout = self.getTypeLayout(elem);
                    max_align = @max(max_align, elem_layout.alignment);
                    offset = alignUp(offset, elem_layout.alignment);
                    offset += elem_layout.size;
                }
                return .{
                    .size = alignUp(offset, max_align),
                    .alignment = max_align,
                };
            },
            .struct_ => |s| {
                // Calculate struct layout on demand
                if (self.struct_layouts.get(s.name)) |layout| {
                    return .{ .size = layout.size, .alignment = layout.alignment };
                }
                // Need to calculate - simplified version
                var max_align: usize = 1;
                var offset: usize = 0;
                for (s.fields) |field| {
                    const field_layout = self.getTypeLayout(field.type_);
                    max_align = @max(max_align, field_layout.alignment);
                    offset = alignUp(offset, field_layout.alignment);
                    offset += field_layout.size;
                }
                return .{
                    .size = alignUp(offset, max_align),
                    .alignment = max_align,
                };
            },
            .function => .{ .size = pointer_size * 2, .alignment = pointer_size }, // fn ptr + env ptr
            .void_, .never => .{ .size = 0, .alignment = 1 },
            else => .{ .size = pointer_size, .alignment = pointer_size }, // Default for unknown types
        };
    }

    /// Calculate the layout of a struct.
    pub fn calculateStructLayout(self: *LayoutCalculator, struct_type: *const types.StructType) !StructLayout {
        // Check cache
        if (self.struct_layouts.get(struct_type.name)) |cached| {
            return cached;
        }

        var field_offsets = try self.allocator.alloc(usize, struct_type.fields.len);
        errdefer self.allocator.free(field_offsets);

        var max_align: usize = 1;
        var offset: usize = 0;

        for (struct_type.fields, 0..) |field, i| {
            const field_layout = self.getTypeLayout(field.type_);
            max_align = @max(max_align, field_layout.alignment);

            // Align the offset for this field
            offset = alignUp(offset, field_layout.alignment);
            field_offsets[i] = offset;

            // Add field size to offset
            offset += field_layout.size;
        }

        // Round total size up to struct alignment
        const total_size = alignUp(offset, max_align);

        const layout = StructLayout{
            .field_offsets = field_offsets,
            .size = total_size,
            .alignment = max_align,
            .is_packed = false,
        };

        // Cache the result
        try self.struct_layouts.put(struct_type.name, layout);

        return layout;
    }

    /// Calculate the layout of a tuple.
    pub fn calculateTupleLayout(self: *LayoutCalculator, element_types: []const types.Type) !TupleLayout {
        var element_offsets = try self.allocator.alloc(usize, element_types.len);
        errdefer self.allocator.free(element_offsets);

        var max_align: usize = 1;
        var offset: usize = 0;

        for (element_types, 0..) |elem_type, i| {
            const elem_layout = self.getTypeLayout(elem_type);
            max_align = @max(max_align, elem_layout.alignment);

            // Align the offset for this element
            offset = alignUp(offset, elem_layout.alignment);
            element_offsets[i] = offset;

            // Add element size to offset
            offset += elem_layout.size;
        }

        // Round total size up to tuple alignment
        const total_size = alignUp(offset, max_align);

        return TupleLayout{
            .element_offsets = element_offsets,
            .size = total_size,
            .alignment = max_align,
        };
    }

    /// Calculate the layout of an array.
    pub fn calculateArrayLayout(self: *LayoutCalculator, element_type: types.Type, count: usize) ArrayLayout {
        const elem_layout = self.getTypeLayout(element_type);

        return ArrayLayout{
            .element_size = elem_layout.size,
            .element_alignment = elem_layout.alignment,
            .size = elem_layout.size * count,
            .count = count,
        };
    }
};

/// Align a value up to the given alignment.
pub fn alignUp(value: usize, alignment: usize) usize {
    if (alignment == 0) return value;
    return (value + alignment - 1) & ~(alignment - 1);
}

// ============================================================================
// Tests
// ============================================================================

test "alignUp" {
    const testing = std.testing;

    try testing.expectEqual(@as(usize, 0), alignUp(0, 4));
    try testing.expectEqual(@as(usize, 4), alignUp(1, 4));
    try testing.expectEqual(@as(usize, 4), alignUp(4, 4));
    try testing.expectEqual(@as(usize, 8), alignUp(5, 4));
    try testing.expectEqual(@as(usize, 8), alignUp(7, 8));
    try testing.expectEqual(@as(usize, 8), alignUp(8, 8));
    try testing.expectEqual(@as(usize, 16), alignUp(9, 8));
}

test "primitive layouts" {
    const testing = std.testing;

    var calc = LayoutCalculator.init(testing.allocator, .{ .arch = .x86_64, .os = .linux });
    defer calc.deinit();

    // i8 is 1 byte, 1 alignment
    const i8_layout = calc.getPrimitiveLayout(.i8_);
    try testing.expectEqual(@as(usize, 1), i8_layout.size);
    try testing.expectEqual(@as(usize, 1), i8_layout.alignment);

    // i32 is 4 bytes, 4 alignment
    const i32_layout = calc.getPrimitiveLayout(.i32_);
    try testing.expectEqual(@as(usize, 4), i32_layout.size);
    try testing.expectEqual(@as(usize, 4), i32_layout.alignment);

    // i64 is 8 bytes, 8 alignment
    const i64_layout = calc.getPrimitiveLayout(.i64_);
    try testing.expectEqual(@as(usize, 8), i64_layout.size);
    try testing.expectEqual(@as(usize, 8), i64_layout.alignment);

    // f64 is 8 bytes, 8 alignment
    const f64_layout = calc.getPrimitiveLayout(.f64_);
    try testing.expectEqual(@as(usize, 8), f64_layout.size);
    try testing.expectEqual(@as(usize, 8), f64_layout.alignment);

    // bool is 1 byte, 1 alignment
    const bool_layout = calc.getPrimitiveLayout(.bool_);
    try testing.expectEqual(@as(usize, 1), bool_layout.size);
    try testing.expectEqual(@as(usize, 1), bool_layout.alignment);
}

test "struct layout - simple Point" {
    const testing = std.testing;

    var calc = LayoutCalculator.init(testing.allocator, .{ .arch = .x86_64, .os = .linux });
    defer calc.deinit();

    // struct Point { x: f64, y: f64 }
    // Should be: size=16, align=8, offsets=[0, 8]
    const point_struct = types.StructType{
        .name = "Point",
        .type_params = &.{},
        .fields = &[_]types.StructField{
            .{ .name = "x", .type_ = .{ .primitive = .f64_ }, .is_pub = true },
            .{ .name = "y", .type_ = .{ .primitive = .f64_ }, .is_pub = true },
        },
        .traits = &.{},
        .is_copy = true,
    };

    const layout = try calc.calculateStructLayout(&point_struct);

    try testing.expectEqual(@as(usize, 16), layout.size);
    try testing.expectEqual(@as(usize, 8), layout.alignment);
    try testing.expectEqual(@as(usize, 2), layout.field_offsets.len);
    try testing.expectEqual(@as(usize, 0), layout.field_offsets[0]); // x at offset 0
    try testing.expectEqual(@as(usize, 8), layout.field_offsets[1]); // y at offset 8
}

test "struct layout - with padding" {
    const testing = std.testing;

    var calc = LayoutCalculator.init(testing.allocator, .{ .arch = .x86_64, .os = .linux });
    defer calc.deinit();

    // struct Padded { a: i8, b: i64, c: i8 }
    // Should have padding after 'a' to align 'b'
    // Layout: [a:1][pad:7][b:8][c:1][pad:7] = 24 bytes
    const padded_struct = types.StructType{
        .name = "Padded",
        .type_params = &.{},
        .fields = &[_]types.StructField{
            .{ .name = "a", .type_ = .{ .primitive = .i8_ }, .is_pub = true },
            .{ .name = "b", .type_ = .{ .primitive = .i64_ }, .is_pub = true },
            .{ .name = "c", .type_ = .{ .primitive = .i8_ }, .is_pub = true },
        },
        .traits = &.{},
        .is_copy = true,
    };

    const layout = try calc.calculateStructLayout(&padded_struct);

    try testing.expectEqual(@as(usize, 24), layout.size);
    try testing.expectEqual(@as(usize, 8), layout.alignment);
    try testing.expectEqual(@as(usize, 0), layout.field_offsets[0]); // a at offset 0
    try testing.expectEqual(@as(usize, 8), layout.field_offsets[1]); // b at offset 8 (after 7 bytes padding)
    try testing.expectEqual(@as(usize, 16), layout.field_offsets[2]); // c at offset 16
}

test "array layout" {
    const testing = std.testing;

    var calc = LayoutCalculator.init(testing.allocator, .{ .arch = .x86_64, .os = .linux });
    defer calc.deinit();

    // [i32; 10] should be 40 bytes (10 * 4)
    const layout = calc.calculateArrayLayout(.{ .primitive = .i32_ }, 10);

    try testing.expectEqual(@as(usize, 4), layout.element_size);
    try testing.expectEqual(@as(usize, 4), layout.element_alignment);
    try testing.expectEqual(@as(usize, 40), layout.size);
    try testing.expectEqual(@as(usize, 10), layout.count);
}
