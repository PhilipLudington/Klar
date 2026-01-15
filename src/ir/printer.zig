//! Klar IR text printer.
//!
//! Produces human-readable text representation of the IR for debugging.
//! Output format is similar to LLVM IR but with Klar-specific constructs.

const std = @import("std");
const Allocator = std.mem.Allocator;
const inst = @import("inst.zig");

const IrType = inst.IrType;
const Value = inst.Value;
const Constant = inst.Constant;
const Inst = inst.Inst;
const BasicBlock = inst.BasicBlock;
const BlockId = inst.BlockId;
const Function = inst.Function;
const Module = inst.Module;
const Param = inst.Param;

/// Print an IR module to the given writer.
pub fn printModule(writer: anytype, module: *const Module) !void {
    try writer.print("; Module: {s}\n", .{module.name});
    try writer.writeAll("\n");

    // Print struct type definitions
    var struct_iter = module.struct_types.iterator();
    while (struct_iter.next()) |entry| {
        try printStructType(writer, entry.key_ptr.*, entry.value_ptr.*);
        try writer.writeAll("\n");
    }

    // Print globals
    for (module.globals.items) |global| {
        try printGlobal(writer, global);
        try writer.writeAll("\n");
    }

    // Print functions
    for (module.functions.items) |func| {
        try printFunction(writer, &func);
        try writer.writeAll("\n");
    }
}

/// Print a struct type definition.
fn printStructType(writer: anytype, name: []const u8, info: IrType.StructTypeInfo) !void {
    try writer.print("; struct {s}", .{name});
    if (info.is_copy) {
        try writer.writeAll(" : Copy");
    }
    try writer.writeAll(" {\n");
    for (info.fields) |field| {
        try writer.print(";   {s}: ", .{field.name});
        try printType(writer, field.ty);
        try writer.print(" @ {d}\n", .{field.offset});
    }
    try writer.writeAll("; }\n");
}

/// Print a global variable.
fn printGlobal(writer: anytype, global: Module.Global) !void {
    try writer.print("@{s} = ", .{global.name});
    if (global.is_const) {
        try writer.writeAll("constant ");
    } else {
        try writer.writeAll("global ");
    }
    try printType(writer, global.ty);

    if (global.init) |init| {
        try writer.writeAll(" ");
        try printConstant(writer, init);
    }
    try writer.writeAll("\n");
}

/// Print a function.
fn printFunction(writer: anytype, func: *const Function) !void {
    if (func.is_extern) {
        try writer.writeAll("declare ");
    } else {
        try writer.writeAll("define ");
    }

    try printType(writer, func.return_ty);
    try writer.print(" @{s}(", .{func.name});

    for (func.params, 0..) |param, i| {
        if (i > 0) try writer.writeAll(", ");
        try printType(writer, param.ty);
        try writer.print(" %{d}", .{param.value.id});
        if (param.name.len > 0) {
            try writer.print(" ; {s}", .{param.name});
        }
    }
    try writer.writeAll(")");

    if (func.is_extern) {
        try writer.writeAll("\n");
        return;
    }

    try writer.writeAll(" {\n");

    // Print blocks
    for (func.blocks.items) |block| {
        try printBlock(writer, &block);
    }

    try writer.writeAll("}\n");
}

/// Print a basic block.
fn printBlock(writer: anytype, block: *const BasicBlock) !void {
    if (block.name) |name| {
        try writer.print("{s}:", .{name});
    } else {
        try writer.print("bb{d}:", .{block.id});
    }

    // Print predecessors as comment
    if (block.predecessors.items.len > 0) {
        try writer.writeAll(" ; preds: ");
        for (block.predecessors.items, 0..) |pred, i| {
            if (i > 0) try writer.writeAll(", ");
            try writer.print("bb{d}", .{pred});
        }
    }
    try writer.writeAll("\n");

    // Print instructions
    for (block.instructions.items) |instruction| {
        try writer.writeAll("  ");
        try printInst(writer, &instruction);
        try writer.writeAll("\n");
    }
}

/// Print an instruction.
fn printInst(writer: anytype, instruction: *const Inst) !void {
    // Print result if present
    if (instruction.result) |result| {
        try writer.print("%{d} = ", .{result.id});
    }

    // Print operation
    switch (instruction.op) {
        // Constants
        .constant => |c| {
            try writer.writeAll("const ");
            try printConstant(writer, c);
        },

        // Arithmetic (checked)
        .add => |op| try printBinaryOp(writer, "add", op),
        .sub => |op| try printBinaryOp(writer, "sub", op),
        .mul => |op| try printBinaryOp(writer, "mul", op),
        .sdiv => |op| try printBinaryOp(writer, "sdiv", op),
        .udiv => |op| try printBinaryOp(writer, "udiv", op),
        .srem => |op| try printBinaryOp(writer, "srem", op),
        .urem => |op| try printBinaryOp(writer, "urem", op),

        // Arithmetic (wrapping)
        .add_wrap => |op| try printBinaryOp(writer, "add.wrap", op),
        .sub_wrap => |op| try printBinaryOp(writer, "sub.wrap", op),
        .mul_wrap => |op| try printBinaryOp(writer, "mul.wrap", op),

        // Arithmetic (saturating)
        .add_sat => |op| try printBinaryOp(writer, "add.sat", op),
        .sub_sat => |op| try printBinaryOp(writer, "sub.sat", op),
        .mul_sat => |op| try printBinaryOp(writer, "mul.sat", op),

        // Float arithmetic
        .fadd => |op| try printBinaryOp(writer, "fadd", op),
        .fsub => |op| try printBinaryOp(writer, "fsub", op),
        .fmul => |op| try printBinaryOp(writer, "fmul", op),
        .fdiv => |op| try printBinaryOp(writer, "fdiv", op),
        .frem => |op| try printBinaryOp(writer, "frem", op),

        // Unary
        .neg => |op| try printUnaryOp(writer, "neg", op),
        .fneg => |op| try printUnaryOp(writer, "fneg", op),
        .not => |op| try printUnaryOp(writer, "not", op),

        // Comparison
        .icmp => |op| {
            try writer.print("icmp {s} %{d}, %{d}", .{
                @tagName(op.pred),
                op.lhs.id,
                op.rhs.id,
            });
        },
        .fcmp => |op| {
            try writer.print("fcmp {s} %{d}, %{d}", .{
                @tagName(op.pred),
                op.lhs.id,
                op.rhs.id,
            });
        },

        // Bitwise
        .bit_and => |op| try printBinaryOp(writer, "and", op),
        .bit_or => |op| try printBinaryOp(writer, "or", op),
        .bit_xor => |op| try printBinaryOp(writer, "xor", op),
        .shl => |op| try printBinaryOp(writer, "shl", op),
        .ashr => |op| try printBinaryOp(writer, "ashr", op),
        .lshr => |op| try printBinaryOp(writer, "lshr", op),

        // Type conversions
        .sext => |op| try printCastOp(writer, "sext", op),
        .zext => |op| try printCastOp(writer, "zext", op),
        .trunc => |op| try printCastOp(writer, "trunc", op),
        .fpext => |op| try printCastOp(writer, "fpext", op),
        .fptrunc => |op| try printCastOp(writer, "fptrunc", op),
        .sitofp => |op| try printCastOp(writer, "sitofp", op),
        .uitofp => |op| try printCastOp(writer, "uitofp", op),
        .fptosi => |op| try printCastOp(writer, "fptosi", op),
        .fptoui => |op| try printCastOp(writer, "fptoui", op),
        .ptrtoint => |op| try printCastOp(writer, "ptrtoint", op),
        .inttoptr => |op| try printCastOp(writer, "inttoptr", op),
        .bitcast => |op| try printCastOp(writer, "bitcast", op),

        // Memory
        .alloca => |op| {
            try writer.writeAll("alloca ");
            try printType(writer, op.ty);
            if (op.name) |name| {
                try writer.print(" ; {s}", .{name});
            }
        },
        .load => |op| {
            try writer.writeAll("load ");
            try printType(writer, op.ty);
            try writer.print(", ptr %{d}", .{op.ptr.id});
        },
        .store => |op| {
            try writer.print("store %{d}, ptr %{d}", .{
                op.value.id,
                op.ptr.id,
            });
        },
        .heap_alloc => |op| {
            try writer.print("heap.alloc %{d}, align {d}", .{
                op.size.id,
                op.align_,
            });
        },
        .heap_free => |op| {
            try writer.print("heap.free %{d}", .{op.operand.id});
        },

        // Aggregates
        .gep => |op| {
            try writer.print("gep %{d}", .{op.base.id});
            for (op.indices) |idx| {
                try writer.print(", %{d}", .{idx.id});
            }
        },
        .extract_value => |op| {
            try writer.print("extractvalue %{d}, {d}", .{
                op.aggregate.id,
                op.index,
            });
        },
        .insert_value => |op| {
            try writer.print("insertvalue %{d}, %{d}, {d}", .{
                op.aggregate.id,
                op.value.id,
                op.index,
            });
        },

        // Calls
        .call => |op| {
            try writer.print("call @{s}(", .{op.func_name});
            for (op.args, 0..) |arg, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.print("%{d}", .{arg.id});
            }
            try writer.writeAll(")");
        },
        .call_indirect => |op| {
            try writer.print("call.indirect %{d}(", .{op.callee.id});
            for (op.args, 0..) |arg, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.print("%{d}", .{arg.id});
            }
            try writer.writeAll(")");
        },

        // Ownership
        .move => |op| {
            try writer.print("move %{d}", .{op.src.id});
            if (op.dst_name) |name| {
                try writer.print(" ; -> {s}", .{name});
            }
        },
        .copy => |op| try printUnaryOp(writer, "copy", op),
        .drop => |op| {
            try writer.writeAll("drop ");
            try printType(writer, op.ty);
            try writer.print(" %{d}", .{op.value.id});
        },
        .borrow => |op| try printUnaryOp(writer, "borrow", op),
        .borrow_mut => |op| try printUnaryOp(writer, "borrow.mut", op),

        // Reference counting
        .rc_inc => |op| try printUnaryOp(writer, "rc.inc", op),
        .rc_dec => |op| try printUnaryOp(writer, "rc.dec", op),

        // Optionals
        .some => |op| try printUnaryOp(writer, "some", op),
        .none => |op| {
            try writer.writeAll("none ");
            try printType(writer, op.ty);
        },
        .unwrap => |op| try printUnaryOp(writer, "unwrap", op),
        .is_some => |op| try printUnaryOp(writer, "is_some", op),
        .is_none => |op| try printUnaryOp(writer, "is_none", op),

        // Control flow
        .br => |op| {
            try writer.print("br bb{d}", .{op.target});
        },
        .cond_br => |op| {
            try writer.print("br %{d}, bb{d}, bb{d}", .{
                op.cond.id,
                op.then_target,
                op.else_target,
            });
        },
        .ret => |op| {
            if (op.value) |val| {
                try writer.print("ret %{d}", .{val.id});
            } else {
                try writer.writeAll("ret void");
            }
        },
        .ret_void => {
            try writer.writeAll("ret void");
        },
        .unreachable_ => {
            try writer.writeAll("unreachable");
        },

        // Phi
        .phi => |op| {
            try writer.writeAll("phi ");
            for (op.incoming, 0..) |inc, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.print("[%{d}, bb{d}]", .{
                    inc.value.id,
                    inc.block,
                });
            }
        },
    }
}

/// Print a binary operation.
fn printBinaryOp(writer: anytype, name: []const u8, op: Inst.BinaryOp) !void {
    try writer.print("{s} %{d}, %{d}", .{ name, op.lhs.id, op.rhs.id });
}

/// Print a unary operation.
fn printUnaryOp(writer: anytype, name: []const u8, op: Inst.UnaryOp) !void {
    try writer.print("{s} %{d}", .{ name, op.operand.id });
}

/// Print a cast operation.
fn printCastOp(writer: anytype, name: []const u8, op: Inst.CastOp) !void {
    try writer.print("{s} %{d} to ", .{ name, op.operand.id });
    try printType(writer, op.target_ty);
}

/// Print a constant.
fn printConstant(writer: anytype, c: Constant) !void {
    switch (c) {
        .int => |i| {
            try printType(writer, i.ty);
            try writer.print(" {d}", .{i.value});
        },
        .float => |f| {
            try printType(writer, f.ty);
            try writer.print(" {any}", .{f.value});
        },
        .bool_ => |b| {
            try writer.print("bool {any}", .{b});
        },
        .void_ => {
            try writer.writeAll("void");
        },
        .null_ => {
            try writer.writeAll("null");
        },
        .undef => {
            try writer.writeAll("undef");
        },
        .string => |s| {
            try writer.print("string \"{s}\"", .{s});
        },
    }
}

/// Print an IR type.
pub fn printType(writer: anytype, ty: IrType) !void {
    switch (ty) {
        .void_ => try writer.writeAll("void"),
        .bool_ => try writer.writeAll("bool"),
        .i8_ => try writer.writeAll("i8"),
        .i16_ => try writer.writeAll("i16"),
        .i32_ => try writer.writeAll("i32"),
        .i64_ => try writer.writeAll("i64"),
        .i128_ => try writer.writeAll("i128"),
        .u8_ => try writer.writeAll("u8"),
        .u16_ => try writer.writeAll("u16"),
        .u32_ => try writer.writeAll("u32"),
        .u64_ => try writer.writeAll("u64"),
        .u128_ => try writer.writeAll("u128"),
        .isize_ => try writer.writeAll("isize"),
        .usize_ => try writer.writeAll("usize"),
        .f32_ => try writer.writeAll("f32"),
        .f64_ => try writer.writeAll("f64"),
        .char_ => try writer.writeAll("char"),
        .ptr => |inner| {
            try writer.writeAll("ptr<");
            try printType(writer, inner.*);
            try writer.writeAll(">");
        },
        .array => |arr| {
            try writer.writeAll("[");
            try printType(writer, arr.element.*);
            try writer.print("; {d}]", .{arr.size});
        },
        .slice => |elem| {
            try writer.writeAll("[");
            try printType(writer, elem.*);
            try writer.writeAll("]");
        },
        .tuple => |elems| {
            try writer.writeAll("(");
            for (elems, 0..) |elem, i| {
                if (i > 0) try writer.writeAll(", ");
                try printType(writer, elem);
            }
            try writer.writeAll(")");
        },
        .struct_ => |s| {
            try writer.print("%{s}", .{s.name});
        },
        .func => |f| {
            try writer.writeAll("fn(");
            for (f.params, 0..) |param, i| {
                if (i > 0) try writer.writeAll(", ");
                try printType(writer, param);
            }
            try writer.writeAll(") -> ");
            try printType(writer, f.return_type.*);
        },
        .optional => |inner| {
            try writer.writeAll("?");
            try printType(writer, inner.*);
        },
        .ref => |r| {
            try writer.writeAll("&");
            try printType(writer, r.inner.*);
        },
        .ref_mut => |r| {
            try writer.writeAll("&mut ");
            try printType(writer, r.inner.*);
        },
    }
}

/// Print a module to a string.
pub fn moduleToString(allocator: Allocator, module: *const Module) ![]u8 {
    var list: std.ArrayListUnmanaged(u8) = .{};
    errdefer list.deinit(allocator);

    try printModule(list.writer(allocator), module);
    return list.toOwnedSlice(allocator);
}

/// Print a function to a string.
pub fn functionToString(allocator: Allocator, func: *const Function) ![]u8 {
    var list: std.ArrayListUnmanaged(u8) = .{};
    errdefer list.deinit(allocator);

    try printFunction(list.writer(allocator), func);
    return list.toOwnedSlice(allocator);
}

// ============================================================================
// Tests
// ============================================================================

test "Print simple function" {
    const testing = std.testing;
    const builder_mod = @import("builder.zig");

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = inst.Module.init(allocator, "test");
    var builder = builder_mod.Builder.init(allocator, &module);

    _ = try builder.beginFunction("add", &.{ "a", "b" }, &.{ .i32_, .i32_ }, .i32_);

    const func = builder.getCurrentFunction().?;
    const a = func.params[0].value;
    const b = func.params[1].value;

    const sum = try builder.buildAdd(a, b);
    try builder.buildRet(sum);

    builder.endFunction();

    const output = try functionToString(allocator, func);

    // Check output contains expected strings
    try testing.expect(std.mem.indexOf(u8, output, "define i32 @add") != null);
    try testing.expect(std.mem.indexOf(u8, output, "add %0, %1") != null);
    try testing.expect(std.mem.indexOf(u8, output, "ret %2") != null);
}

test "Print control flow" {
    const testing = std.testing;
    const builder_mod = @import("builder.zig");

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = inst.Module.init(allocator, "test");
    var builder = builder_mod.Builder.init(allocator, &module);

    _ = try builder.beginFunction("test", &.{"x"}, &.{.i32_}, .i32_);

    const func = builder.getCurrentFunction().?;
    const x = func.params[0].value;

    // Create blocks
    const then_bb = try builder.createBlock("then");
    const merge_bb = try builder.createBlock("merge");

    // Entry: conditional branch
    const zero = try builder.constI32(0);
    const cond = try builder.buildICmp(.sgt, x, zero);
    try builder.buildCondBr(cond, then_bb, merge_bb);

    // Then block
    builder.positionAtEnd(then_bb);
    try builder.buildBr(merge_bb);

    // Merge block
    builder.positionAtEnd(merge_bb);
    try builder.buildRet(x);

    builder.endFunction();

    const output = try functionToString(allocator, func);

    // Check for block labels
    try testing.expect(std.mem.indexOf(u8, output, "entry:") != null);
    try testing.expect(std.mem.indexOf(u8, output, "then:") != null);
    try testing.expect(std.mem.indexOf(u8, output, "merge:") != null);
    // Check for branch
    try testing.expect(std.mem.indexOf(u8, output, "br %") != null);
}

test "Print type formatting" {
    const testing = std.testing;

    var buf: [256]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const writer = fbs.writer();

    try printType(writer, .i32_);
    try writer.writeAll(" ");
    try printType(writer, .f64_);
    try writer.writeAll(" ");
    try printType(writer, .bool_);

    const output = fbs.getWritten();
    try testing.expectEqualStrings("i32 f64 bool", output);
}
