const std = @import("std");
const Allocator = std.mem.Allocator;
const bytecode = @import("bytecode.zig");
const OpCode = bytecode.OpCode;

// ============================================================================
// Constant Pool Values
// ============================================================================

/// Values that can be stored in the constant pool.
/// These are compile-time constants referenced by bytecode instructions.
pub const Constant = union(enum) {
    /// Integer literal
    int: i128,

    /// Floating-point literal
    float: f64,

    /// String literal (interned)
    string: []const u8,

    /// Character literal
    char: u21,

    /// Function prototype
    function: *Function,

    /// Struct type descriptor
    struct_type: *StructDescriptor,

    /// Enum variant descriptor
    enum_variant: *EnumVariantDescriptor,
};

/// Function prototype stored in the constant pool.
pub const Function = struct {
    /// Function name (for debugging/stack traces)
    name: []const u8,

    /// Number of parameters
    arity: u8,

    /// Number of upvalues this function captures
    upvalue_count: u8,

    /// The function's bytecode chunk
    chunk: Chunk,

    pub fn init(allocator: Allocator, name: []const u8, arity: u8) Function {
        return .{
            .name = name,
            .arity = arity,
            .upvalue_count = 0,
            .chunk = Chunk.init(allocator),
        };
    }

    pub fn deinit(self: *Function) void {
        self.chunk.deinit();
    }
};

/// Descriptor for struct types used by OP_STRUCT
pub const StructDescriptor = struct {
    name: []const u8,
    field_names: []const []const u8,
};

/// Descriptor for enum variants
pub const EnumVariantDescriptor = struct {
    type_name: []const u8,
    variant_name: []const u8,
    has_payload: bool,
};

// ============================================================================
// Bytecode Chunk
// ============================================================================

/// A chunk of bytecode with its associated constant pool and debug info.
pub const Chunk = struct {
    allocator: Allocator,

    /// The raw bytecode instructions
    code: std.ArrayListUnmanaged(u8),

    /// Constant pool for literals and function prototypes
    constants: std.ArrayListUnmanaged(Constant),

    /// Line number for each bytecode instruction (run-length encoded)
    lines: std.ArrayListUnmanaged(LineInfo),

    pub fn init(allocator: Allocator) Chunk {
        return .{
            .allocator = allocator,
            .code = .{},
            .constants = .{},
            .lines = .{},
        };
    }

    pub fn deinit(self: *Chunk) void {
        // Free function chunks recursively
        for (self.constants.items) |constant| {
            switch (constant) {
                .function => |func| {
                    func.deinit();
                    self.allocator.destroy(func);
                },
                else => {},
            }
        }

        self.code.deinit(self.allocator);
        self.constants.deinit(self.allocator);
        self.lines.deinit(self.allocator);
    }

    // ------------------------------------------------------------------------
    // Writing bytecode
    // ------------------------------------------------------------------------

    /// Write a single byte to the chunk
    pub fn writeByte(self: *Chunk, byte: u8, line: u32) !void {
        try self.code.append(self.allocator, byte);
        try self.addLine(line);
    }

    /// Write an opcode to the chunk
    pub fn writeOp(self: *Chunk, op: OpCode, line: u32) !void {
        try self.writeByte(@intFromEnum(op), line);
    }

    /// Write an opcode with a single byte operand
    pub fn writeOp1(self: *Chunk, op: OpCode, operand: u8, line: u32) !void {
        try self.writeOp(op, line);
        try self.writeByte(operand, line);
    }

    /// Write an opcode with a 16-bit operand (big-endian)
    pub fn writeOp2(self: *Chunk, op: OpCode, operand: u16, line: u32) !void {
        try self.writeOp(op, line);
        try self.writeByte(@truncate(operand >> 8), line);
        try self.writeByte(@truncate(operand), line);
    }

    /// Write an opcode with a 24-bit operand (big-endian)
    pub fn writeOp3(self: *Chunk, op: OpCode, operand: u24, line: u32) !void {
        try self.writeOp(op, line);
        try self.writeByte(@truncate(operand >> 16), line);
        try self.writeByte(@truncate(operand >> 8), line);
        try self.writeByte(@truncate(operand), line);
    }

    /// Write a constant reference (automatically chooses short or long form)
    pub fn writeConstant(self: *Chunk, value: Constant, line: u32) !void {
        const index = try self.addConstant(value);
        if (index <= std.math.maxInt(u16)) {
            try self.writeOp2(.op_const, @intCast(index), line);
        } else {
            try self.writeOp3(.op_const_long, @intCast(index), line);
        }
    }

    /// Write a jump instruction and return the offset to patch later
    pub fn writeJump(self: *Chunk, op: OpCode, line: u32) !usize {
        try self.writeOp(op, line);
        // Write placeholder offset (will be patched)
        try self.writeByte(0xff, line);
        try self.writeByte(0xff, line);
        return self.code.items.len - 2;
    }

    /// Patch a jump instruction with the actual offset
    pub fn patchJump(self: *Chunk, offset: usize) !void {
        // Calculate jump distance (from after the jump instruction)
        const jump = self.code.items.len - offset - 2;

        if (jump > std.math.maxInt(u16)) {
            return error.JumpTooLarge;
        }

        self.code.items[offset] = @truncate(jump >> 8);
        self.code.items[offset + 1] = @truncate(jump);
    }

    /// Write a loop instruction (backward jump)
    pub fn writeLoop(self: *Chunk, loop_start: usize, line: u32) !void {
        try self.writeOp(.op_loop, line);

        // Calculate backward offset
        const offset = self.code.items.len - loop_start + 2;

        if (offset > std.math.maxInt(u16)) {
            return error.LoopTooLarge;
        }

        try self.writeByte(@truncate(offset >> 8), line);
        try self.writeByte(@truncate(offset), line);
    }

    /// Get current bytecode offset
    pub fn currentOffset(self: *Chunk) usize {
        return self.code.items.len;
    }

    // ------------------------------------------------------------------------
    // Constant pool management
    // ------------------------------------------------------------------------

    /// Add a constant to the pool and return its index
    pub fn addConstant(self: *Chunk, value: Constant) !usize {
        // Check for duplicate constants (for interning)
        if (self.findConstant(value)) |existing| {
            return existing;
        }

        const index = self.constants.items.len;
        try self.constants.append(self.allocator, value);
        return index;
    }

    /// Find an existing constant in the pool (for interning)
    fn findConstant(self: *Chunk, value: Constant) ?usize {
        for (self.constants.items, 0..) |existing, i| {
            if (constantsEqual(existing, value)) {
                return i;
            }
        }
        return null;
    }

    fn constantsEqual(a: Constant, b: Constant) bool {
        if (@intFromEnum(a) != @intFromEnum(b)) return false;

        return switch (a) {
            .int => |v| v == b.int,
            .float => |v| v == b.float,
            .string => |v| std.mem.eql(u8, v, b.string),
            .char => |v| v == b.char,
            // Functions and descriptors are compared by pointer
            .function => |v| v == b.function,
            .struct_type => |v| v == b.struct_type,
            .enum_variant => |v| v == b.enum_variant,
        };
    }

    /// Get constant at index
    pub fn getConstant(self: *Chunk, index: usize) Constant {
        return self.constants.items[index];
    }

    // ------------------------------------------------------------------------
    // Line number tracking (run-length encoded)
    // ------------------------------------------------------------------------

    /// Add line information for current instruction
    fn addLine(self: *Chunk, line: u32) !void {
        if (self.lines.items.len > 0) {
            const last = &self.lines.items[self.lines.items.len - 1];
            if (last.line == line) {
                last.count += 1;
                return;
            }
        }
        try self.lines.append(self.allocator, .{ .line = line, .count = 1 });
    }

    /// Get line number for instruction at offset
    pub fn getLine(self: *const Chunk, offset: usize) u32 {
        var accumulated: usize = 0;
        for (self.lines.items) |info| {
            accumulated += info.count;
            if (accumulated > offset) {
                return info.line;
            }
        }
        return 0;
    }
};

/// Run-length encoded line information
const LineInfo = struct {
    line: u32,
    count: usize,
};

// ============================================================================
// Chunk Errors
// ============================================================================

pub const ChunkError = error{
    JumpTooLarge,
    LoopTooLarge,
    OutOfMemory,
};

// ============================================================================
// Tests
// ============================================================================

test "Chunk write and read bytecode" {
    const testing = std.testing;

    var chunk = Chunk.init(testing.allocator);
    defer chunk.deinit();

    // Write some opcodes
    try chunk.writeOp(.op_int_1, 1);
    try chunk.writeOp(.op_int_2, 1);
    try chunk.writeOp(.op_add, 1);
    try chunk.writeOp(.op_return, 2);

    try testing.expectEqual(@as(usize, 4), chunk.code.items.len);
    try testing.expectEqual(@intFromEnum(OpCode.op_int_1), chunk.code.items[0]);
    try testing.expectEqual(@intFromEnum(OpCode.op_int_2), chunk.code.items[1]);
    try testing.expectEqual(@intFromEnum(OpCode.op_add), chunk.code.items[2]);
    try testing.expectEqual(@intFromEnum(OpCode.op_return), chunk.code.items[3]);
}

test "Chunk constant pool" {
    const testing = std.testing;

    var chunk = Chunk.init(testing.allocator);
    defer chunk.deinit();

    // Add constants
    const idx1 = try chunk.addConstant(.{ .int = 42 });
    const idx2 = try chunk.addConstant(.{ .string = "hello" });
    const idx3 = try chunk.addConstant(.{ .float = 3.14 });

    // Duplicate constant should return same index
    const idx1_dup = try chunk.addConstant(.{ .int = 42 });

    try testing.expectEqual(@as(usize, 0), idx1);
    try testing.expectEqual(@as(usize, 1), idx2);
    try testing.expectEqual(@as(usize, 2), idx3);
    try testing.expectEqual(idx1, idx1_dup);

    // Verify values
    try testing.expectEqual(@as(i128, 42), chunk.getConstant(idx1).int);
    try testing.expectEqualStrings("hello", chunk.getConstant(idx2).string);
}

test "Chunk write with operands" {
    const testing = std.testing;

    var chunk = Chunk.init(testing.allocator);
    defer chunk.deinit();

    // Write opcode with 1-byte operand
    try chunk.writeOp1(.op_get_local, 5, 1);

    // Write opcode with 2-byte operand
    try chunk.writeOp2(.op_const, 0x0102, 1);

    try testing.expectEqual(@as(usize, 5), chunk.code.items.len);

    // Verify 1-byte operand
    try testing.expectEqual(@as(u8, 5), chunk.code.items[1]);

    // Verify 2-byte operand (big-endian)
    try testing.expectEqual(@as(u8, 0x01), chunk.code.items[3]);
    try testing.expectEqual(@as(u8, 0x02), chunk.code.items[4]);
}

test "Chunk jump patching" {
    const testing = std.testing;

    var chunk = Chunk.init(testing.allocator);
    defer chunk.deinit();

    // Write jump with placeholder
    const jump_offset = try chunk.writeJump(.op_jump_if_false, 1);

    // Write some instructions
    try chunk.writeOp(.op_int_1, 1);
    try chunk.writeOp(.op_int_2, 1);
    try chunk.writeOp(.op_add, 1);

    // Patch the jump
    try chunk.patchJump(jump_offset);

    // Verify jump offset (3 instructions = 3 bytes after jump)
    const high: u16 = chunk.code.items[jump_offset];
    const low: u16 = chunk.code.items[jump_offset + 1];
    const offset = (high << 8) | low;

    try testing.expectEqual(@as(u16, 3), offset);
}

test "Chunk line numbers" {
    const testing = std.testing;

    var chunk = Chunk.init(testing.allocator);
    defer chunk.deinit();

    // Write instructions on different lines
    try chunk.writeOp(.op_int_1, 1);
    try chunk.writeOp(.op_int_2, 1);
    try chunk.writeOp(.op_add, 2);
    try chunk.writeOp(.op_return, 3);

    // Verify line lookups
    try testing.expectEqual(@as(u32, 1), chunk.getLine(0));
    try testing.expectEqual(@as(u32, 1), chunk.getLine(1));
    try testing.expectEqual(@as(u32, 2), chunk.getLine(2));
    try testing.expectEqual(@as(u32, 3), chunk.getLine(3));

    // Run-length encoding should compress same-line instructions
    try testing.expectEqual(@as(usize, 3), chunk.lines.items.len);
}

test "Chunk loop instruction" {
    const testing = std.testing;

    var chunk = Chunk.init(testing.allocator);
    defer chunk.deinit();

    // Mark loop start
    const loop_start = chunk.currentOffset();

    // Write loop body
    try chunk.writeOp(.op_int_1, 1);
    try chunk.writeOp(.op_pop, 1);

    // Write loop back to start
    try chunk.writeLoop(loop_start, 1);

    // Loop should jump back 5 bytes (2 instructions + loop + 2 operand bytes)
    const jump_offset = chunk.code.items.len - 2;
    const high: u16 = chunk.code.items[jump_offset];
    const low: u16 = chunk.code.items[jump_offset + 1];
    const offset = (high << 8) | low;

    try testing.expectEqual(@as(u16, 5), offset);
}

test "Function prototype" {
    const testing = std.testing;

    var func = Function.init(testing.allocator, "test_fn", 2);
    defer func.deinit();

    try testing.expectEqualStrings("test_fn", func.name);
    try testing.expectEqual(@as(u8, 2), func.arity);
    try testing.expectEqual(@as(u8, 0), func.upvalue_count);

    // Write to function's chunk
    // op_get_local (1) + operand (1) = 2 bytes
    try func.chunk.writeOp1(.op_get_local, 0, 1);
    // op_get_local (1) + operand (1) = 2 bytes
    try func.chunk.writeOp1(.op_get_local, 1, 1);
    // op_add (1) = 1 byte
    try func.chunk.writeOp(.op_add, 1);
    // op_return (1) = 1 byte
    try func.chunk.writeOp(.op_return, 1);

    // Total: 2 + 2 + 1 + 1 = 6 bytes
    try testing.expectEqual(@as(usize, 6), func.chunk.code.items.len);
}
