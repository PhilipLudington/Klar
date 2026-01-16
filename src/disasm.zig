const std = @import("std");
const Allocator = std.mem.Allocator;
const bytecode = @import("bytecode.zig");
const OpCode = bytecode.OpCode;
const chunk_mod = @import("chunk.zig");
const Chunk = chunk_mod.Chunk;
const Constant = chunk_mod.Constant;
const Function = chunk_mod.Function;

// ============================================================================
// Disassembler
// ============================================================================

/// Disassembler for Klar bytecode chunks.
/// Provides pretty-printing of bytecode instructions, constant pools,
/// and debug information.
pub const Disassembler = struct {
    allocator: Allocator,
    output: std.ArrayListUnmanaged(u8),
    indent_level: u32,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .output = .{},
            .indent_level = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        self.output.deinit(self.allocator);
    }

    /// Get the disassembled output as a string slice.
    pub fn getOutput(self: *Self) []const u8 {
        return self.output.items;
    }

    /// Disassemble a complete function and all nested functions.
    pub fn disassembleFunction(self: *Self, function: *const Function) !void {
        try self.writeHeader(function.name, function.arity);
        try self.writeConstantPool(&function.chunk);
        try self.writeLine("");
        try self.writeInstructions(&function.chunk);

        // Recursively disassemble nested functions
        for (function.chunk.constants.items) |constant| {
            switch (constant) {
                .function => |nested_fn| {
                    try self.writeLine("");
                    try self.disassembleFunction(nested_fn);
                },
                else => {},
            }
        }
    }

    /// Disassemble just the bytecode instructions of a chunk.
    pub fn disassembleChunk(self: *Self, chunk_param: *const Chunk, name: []const u8) !void {
        try self.writeHeader(name, 0);
        try self.writeConstantPool(chunk_param);
        try self.writeLine("");
        try self.writeInstructions(chunk_param);
    }

    // ------------------------------------------------------------------------
    // Output helpers
    // ------------------------------------------------------------------------

    fn writeHeader(self: *Self, name: []const u8, arity: u8) !void {
        try self.writeLine("================================================================================");
        var buf: [128]u8 = undefined;
        const header = std.fmt.bufPrint(&buf, "== Function: {s} (arity: {d})", .{ name, arity }) catch "== Function: ?";
        try self.writeLine(header);
        try self.writeLine("================================================================================");
    }

    fn writeConstantPool(self: *Self, chunk_param: *const Chunk) !void {
        if (chunk_param.constants.items.len == 0) {
            try self.writeLine("Constants: (empty)");
            return;
        }

        var buf: [256]u8 = undefined;
        const header = std.fmt.bufPrint(&buf, "Constants: ({d} entries)", .{chunk_param.constants.items.len}) catch "Constants:";
        try self.writeLine(header);

        for (chunk_param.constants.items, 0..) |constant, i| {
            const desc = self.formatConstant(constant, &buf);
            var line_buf: [300]u8 = undefined;
            const line = std.fmt.bufPrint(&line_buf, "  [{d:>4}] {s}", .{ i, desc }) catch "  [?] ?";
            try self.writeLine(line);
        }
    }

    fn formatConstant(self: *Self, constant: Constant, buf: []u8) []const u8 {
        _ = self;
        return switch (constant) {
            .int => |v| std.fmt.bufPrint(buf, "int: {d}", .{v}) catch "int: ?",
            .float => |v| std.fmt.bufPrint(buf, "float: {d:.6}", .{v}) catch "float: ?",
            .string => |v| blk: {
                // Escape and truncate long strings
                const max_len = 50;
                const truncated = if (v.len > max_len) v[0..max_len] else v;
                const suffix: []const u8 = if (v.len > max_len) "..." else "";
                break :blk std.fmt.bufPrint(buf, "string: \"{s}{s}\"", .{ truncated, suffix }) catch "string: ?";
            },
            .char => |v| std.fmt.bufPrint(buf, "char: '{u}'", .{v}) catch "char: ?",
            .function => |f| std.fmt.bufPrint(buf, "function: <{s}/{d}>", .{ f.name, f.arity }) catch "function: ?",
            .struct_type => |s| std.fmt.bufPrint(buf, "struct: {s}", .{s.name}) catch "struct: ?",
            .enum_variant => |e| std.fmt.bufPrint(buf, "enum: {s}::{s}", .{ e.type_name, e.variant_name }) catch "enum: ?",
        };
    }

    fn writeInstructions(self: *Self, chunk_param: *const Chunk) !void {
        try self.writeLine("Instructions:");
        try self.writeLine("  Offset  Line  Opcode                     Operands");
        try self.writeLine("  ------  ----  -------------------------  --------------------");

        var offset: usize = 0;
        while (offset < chunk_param.code.items.len) {
            offset = try self.disassembleInstruction(chunk_param, offset);
        }
    }

    /// Disassemble a single instruction and return the next offset.
    pub fn disassembleInstruction(self: *Self, chunk_param: *const Chunk, offset: usize) !usize {
        const code = chunk_param.code.items;
        if (offset >= code.len) return offset;

        const instruction = code[offset];
        const op: OpCode = @enumFromInt(instruction);
        const line = chunk_param.getLine(offset);

        var buf: [128]u8 = undefined;
        var operand_buf: [64]u8 = undefined;

        const operand_str = try self.formatOperands(chunk_param, op, offset, &operand_buf);
        const line_str = std.fmt.bufPrint(&buf, "  {d:0>6}  {d:>4}  {s:<25}  {s}", .{
            offset,
            line,
            @tagName(op),
            operand_str,
        }) catch "  ? ? ?";

        try self.writeLine(line_str);

        return offset + 1 + op.operandBytes();
    }

    fn formatOperands(self: *Self, chunk_param: *const Chunk, op: OpCode, offset: usize, buf: []u8) ![]const u8 {
        _ = self;
        const code = chunk_param.code.items;
        const operand_bytes = op.operandBytes();

        if (operand_bytes == 0) {
            return "";
        }

        // Read operand value based on size
        const base = offset + 1;
        if (base + operand_bytes > code.len) {
            return "(truncated)";
        }

        return switch (operand_bytes) {
            1 => blk: {
                const operand = code[base];
                break :blk formatOperand1(op, operand, buf);
            },
            2 => blk: {
                const operand = (@as(u16, code[base]) << 8) | @as(u16, code[base + 1]);
                break :blk formatOperand2(op, operand, chunk_param, buf);
            },
            3 => blk: {
                const operand = (@as(u24, code[base]) << 16) |
                    (@as(u24, code[base + 1]) << 8) |
                    @as(u24, code[base + 2]);
                break :blk formatOperand3(op, operand, chunk_param, buf);
            },
            else => "(unknown operand size)",
        };
    }

    fn writeLine(self: *Self, line: []const u8) !void {
        try self.output.appendSlice(self.allocator, line);
        try self.output.append(self.allocator, '\n');
    }
};

// ============================================================================
// Operand formatters
// ============================================================================

fn formatOperand1(op: OpCode, operand: u8, buf: []u8) []const u8 {
    return switch (op) {
        .op_get_local, .op_set_local => std.fmt.bufPrint(buf, "slot {d}", .{operand}) catch "slot ?",
        .op_get_upvalue, .op_set_upvalue => std.fmt.bufPrint(buf, "upvalue {d}", .{operand}) catch "upvalue ?",
        .op_call, .op_tail_call => std.fmt.bufPrint(buf, "args {d}", .{operand}) catch "args ?",
        .op_tuple => std.fmt.bufPrint(buf, "elements {d}", .{operand}) catch "elements ?",
        .op_string_build => std.fmt.bufPrint(buf, "parts {d}", .{operand}) catch "parts ?",
        .op_cast, .op_cast_trunc => blk: {
            const type_tag: bytecode.TypeTag = @enumFromInt(operand);
            break :blk std.fmt.bufPrint(buf, "-> {s}", .{@tagName(type_tag)}) catch "-> ?";
        },
        else => std.fmt.bufPrint(buf, "{d}", .{operand}) catch "?",
    };
}

fn formatOperand2(op: OpCode, operand: u16, chunk_param: *const Chunk, buf: []u8) []const u8 {
    return switch (op) {
        .op_const => blk: {
            if (operand < chunk_param.constants.items.len) {
                const constant = chunk_param.constants.items[operand];
                break :blk formatConstantRef(operand, constant, buf);
            }
            break :blk std.fmt.bufPrint(buf, "const [{d}] (invalid)", .{operand}) catch "const ?";
        },
        .op_jump, .op_jump_if_false, .op_jump_if_true, .op_jump_if_false_no_pop => std.fmt.bufPrint(buf, "+{d}", .{operand}) catch "+?",
        .op_loop => std.fmt.bufPrint(buf, "-{d}", .{operand}) catch "-?",
        .op_and, .op_or => std.fmt.bufPrint(buf, "skip +{d}", .{operand}) catch "skip ?",
        .op_get_local_long, .op_set_local_long => std.fmt.bufPrint(buf, "slot {d}", .{operand}) catch "slot ?",
        .op_get_global, .op_set_global, .op_define_global => blk: {
            if (operand < chunk_param.constants.items.len) {
                const constant = chunk_param.constants.items[operand];
                if (constant == .string) {
                    break :blk std.fmt.bufPrint(buf, "'{s}'", .{constant.string}) catch "'?'";
                }
            }
            break :blk std.fmt.bufPrint(buf, "global [{d}]", .{operand}) catch "global ?";
        },
        .op_closure => blk: {
            if (operand < chunk_param.constants.items.len) {
                const constant = chunk_param.constants.items[operand];
                if (constant == .function) {
                    break :blk std.fmt.bufPrint(buf, "<{s}>", .{constant.function.name}) catch "<fn>";
                }
            }
            break :blk std.fmt.bufPrint(buf, "func [{d}]", .{operand}) catch "func ?";
        },
        .op_get_field, .op_set_field => blk: {
            if (operand < chunk_param.constants.items.len) {
                const constant = chunk_param.constants.items[operand];
                if (constant == .string) {
                    break :blk std.fmt.bufPrint(buf, ".{s}", .{constant.string}) catch ".?";
                }
            }
            break :blk std.fmt.bufPrint(buf, "field [{d}]", .{operand}) catch "field ?";
        },
        .op_array => std.fmt.bufPrint(buf, "elements {d}", .{operand}) catch "elements ?",
        .op_array_empty => std.fmt.bufPrint(buf, "capacity {d}", .{operand}) catch "capacity ?",
        .op_struct => std.fmt.bufPrint(buf, "type [{d}]", .{operand}) catch "type ?",
        .op_match_literal, .op_match_variant => blk: {
            if (operand < chunk_param.constants.items.len) {
                const constant = chunk_param.constants.items[operand];
                break :blk formatConstantRef(operand, constant, buf);
            }
            break :blk std.fmt.bufPrint(buf, "[{d}]", .{operand}) catch "?";
        },
        .op_is_type => std.fmt.bufPrint(buf, "type [{d}]", .{operand}) catch "type ?",
        else => std.fmt.bufPrint(buf, "{d}", .{operand}) catch "?",
    };
}

fn formatOperand3(op: OpCode, operand: u24, chunk_param: *const Chunk, buf: []u8) []const u8 {
    return switch (op) {
        .op_const_long => blk: {
            if (operand < chunk_param.constants.items.len) {
                const constant = chunk_param.constants.items[operand];
                break :blk formatConstantRef(operand, constant, buf);
            }
            break :blk std.fmt.bufPrint(buf, "const [{d}] (invalid)", .{operand}) catch "const ?";
        },
        .op_invoke, .op_invoke_super => blk: {
            // Operand is: u16 method name index, u8 arg count
            const method_idx = @as(u16, @truncate(operand >> 8));
            const arg_count = @as(u8, @truncate(operand));
            if (method_idx < chunk_param.constants.items.len) {
                const constant = chunk_param.constants.items[method_idx];
                if (constant == .string) {
                    break :blk std.fmt.bufPrint(buf, ".{s}({d} args)", .{ constant.string, arg_count }) catch ".?()";
                }
            }
            break :blk std.fmt.bufPrint(buf, "[{d}]({d} args)", .{ method_idx, arg_count }) catch "?";
        },
        else => std.fmt.bufPrint(buf, "{d}", .{operand}) catch "?",
    };
}

fn formatConstantRef(index: usize, constant: Constant, buf: []u8) []const u8 {
    return switch (constant) {
        .int => |v| std.fmt.bufPrint(buf, "[{d}] = {d}", .{ index, v }) catch "?",
        .float => |v| std.fmt.bufPrint(buf, "[{d}] = {d:.4}", .{ index, v }) catch "?",
        .string => |v| blk: {
            const max_len = 20;
            const truncated = if (v.len > max_len) v[0..max_len] else v;
            const suffix: []const u8 = if (v.len > max_len) "..." else "";
            break :blk std.fmt.bufPrint(buf, "[{d}] = \"{s}{s}\"", .{ index, truncated, suffix }) catch "?";
        },
        .char => |v| std.fmt.bufPrint(buf, "[{d}] = '{u}'", .{ index, v }) catch "?",
        .function => |f| std.fmt.bufPrint(buf, "[{d}] = <fn {s}>", .{ index, f.name }) catch "?",
        .struct_type => |s| std.fmt.bufPrint(buf, "[{d}] = struct {s}", .{ index, s.name }) catch "?",
        .enum_variant => |e| std.fmt.bufPrint(buf, "[{d}] = {s}::{s}", .{ index, e.type_name, e.variant_name }) catch "?",
    };
}

// ============================================================================
// Convenience functions
// ============================================================================

/// Disassemble a function to stdout.
pub fn disassembleToStdout(function: *const Function) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var disasm = Disassembler.init(allocator);
    defer disasm.deinit();

    try disasm.disassembleFunction(function);

    const stdout = std.io.getStdOut();
    try stdout.writeAll(disasm.getOutput());
}

/// Disassemble a function to a string (caller owns the returned memory).
pub fn disassembleToString(allocator: Allocator, function: *const Function) ![]u8 {
    var disasm = Disassembler.init(allocator);
    defer disasm.deinit();

    try disasm.disassembleFunction(function);

    return allocator.dupe(u8, disasm.getOutput());
}

// ============================================================================
// Tests
// ============================================================================

test "Disassembler basic output" {
    const testing = std.testing;

    var func = Function.init(testing.allocator, "test", 0);
    defer func.deinit();

    // Write some simple instructions
    try func.chunk.writeOp(.op_int_1, 1);
    try func.chunk.writeOp(.op_int_2, 1);
    try func.chunk.writeOp(.op_add, 1);
    try func.chunk.writeOp(.op_return, 2);

    var disasm = Disassembler.init(testing.allocator);
    defer disasm.deinit();

    try disasm.disassembleFunction(&func);

    const output = disasm.getOutput();

    // Verify key parts are present
    try testing.expect(std.mem.indexOf(u8, output, "Function: test") != null);
    try testing.expect(std.mem.indexOf(u8, output, "op_int_1") != null);
    try testing.expect(std.mem.indexOf(u8, output, "op_int_2") != null);
    try testing.expect(std.mem.indexOf(u8, output, "op_add") != null);
    try testing.expect(std.mem.indexOf(u8, output, "op_return") != null);
}

test "Disassembler constant pool" {
    const testing = std.testing;

    var func = Function.init(testing.allocator, "with_constants", 1);
    defer func.deinit();

    // Add constants
    _ = try func.chunk.addConstant(.{ .int = 42 });
    _ = try func.chunk.addConstant(.{ .string = "hello" });
    _ = try func.chunk.addConstant(.{ .float = 3.14159 });

    // Write instruction referencing constant
    try func.chunk.writeOp2(.op_const, 0, 1);
    try func.chunk.writeOp(.op_return, 1);

    var disasm = Disassembler.init(testing.allocator);
    defer disasm.deinit();

    try disasm.disassembleFunction(&func);

    const output = disasm.getOutput();

    // Verify constant pool is shown
    try testing.expect(std.mem.indexOf(u8, output, "int: 42") != null);
    try testing.expect(std.mem.indexOf(u8, output, "string: \"hello\"") != null);
    try testing.expect(std.mem.indexOf(u8, output, "float:") != null);
}

test "Disassembler jump instructions" {
    const testing = std.testing;

    var func = Function.init(testing.allocator, "with_jumps", 0);
    defer func.deinit();

    // Write a conditional jump
    _ = try func.chunk.writeJump(.op_jump_if_false, 1);
    try func.chunk.writeOp(.op_int_1, 1);
    try func.chunk.patchJump(1);
    try func.chunk.writeOp(.op_return, 2);

    var disasm = Disassembler.init(testing.allocator);
    defer disasm.deinit();

    try disasm.disassembleFunction(&func);

    const output = disasm.getOutput();

    // Verify jump instruction is shown with offset
    try testing.expect(std.mem.indexOf(u8, output, "op_jump_if_false") != null);
}

test "Disassembler local variable access" {
    const testing = std.testing;

    var func = Function.init(testing.allocator, "locals", 2);
    defer func.deinit();

    try func.chunk.writeOp1(.op_get_local, 0, 1);
    try func.chunk.writeOp1(.op_get_local, 1, 1);
    try func.chunk.writeOp(.op_add, 1);
    try func.chunk.writeOp1(.op_set_local, 2, 1);
    try func.chunk.writeOp(.op_return, 2);

    var disasm = Disassembler.init(testing.allocator);
    defer disasm.deinit();

    try disasm.disassembleFunction(&func);

    const output = disasm.getOutput();

    // Verify local slot references
    try testing.expect(std.mem.indexOf(u8, output, "slot 0") != null);
    try testing.expect(std.mem.indexOf(u8, output, "slot 1") != null);
    try testing.expect(std.mem.indexOf(u8, output, "slot 2") != null);
}
