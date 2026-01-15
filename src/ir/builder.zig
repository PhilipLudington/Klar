//! Klar IR builder API.
//!
//! Provides a fluent interface for constructing IR instructions and
//! managing basic blocks within functions.

const std = @import("std");
const Allocator = std.mem.Allocator;
const inst = @import("inst.zig");
const ast = @import("../ast.zig");

const IrType = inst.IrType;
const Value = inst.Value;
const Constant = inst.Constant;
const Inst = inst.Inst;
const BasicBlock = inst.BasicBlock;
const BlockId = inst.BlockId;
const Function = inst.Function;
const Module = inst.Module;
const Param = inst.Param;

/// IR builder for constructing functions and instructions.
pub const Builder = struct {
    allocator: Allocator,
    module: *Module,
    current_func: ?*Function,
    current_block: ?BlockId,

    pub fn init(allocator: Allocator, module: *Module) Builder {
        return .{
            .allocator = allocator,
            .module = module,
            .current_func = null,
            .current_block = null,
        };
    }

    // ========================================================================
    // Function Management
    // ========================================================================

    /// Start building a new function.
    pub fn beginFunction(
        self: *Builder,
        name: []const u8,
        param_names: []const []const u8,
        param_types: []const IrType,
        return_ty: IrType,
    ) !*Function {
        std.debug.assert(param_names.len == param_types.len);

        // Create parameters with SSA values
        var params = std.ArrayListUnmanaged(Param){};
        errdefer params.deinit(self.allocator);

        for (param_names, param_types, 0..) |pname, pty, i| {
            try params.append(self.allocator, .{
                .name = pname,
                .ty = pty,
                .value = .{ .id = @intCast(i), .ty = pty },
            });
        }

        const func = try Function.init(
            self.allocator,
            name,
            try params.toOwnedSlice(self.allocator),
            return_ty,
        );

        try self.module.addFunction(func);

        // Get pointer to the function in the module
        self.current_func = self.module.getFunction(name);
        self.current_block = 0; // Entry block

        return self.current_func.?;
    }

    /// End the current function and return to module scope.
    pub fn endFunction(self: *Builder) void {
        self.current_func = null;
        self.current_block = null;
    }

    /// Get the current function being built.
    pub fn getCurrentFunction(self: *Builder) ?*Function {
        return self.current_func;
    }

    // ========================================================================
    // Block Management
    // ========================================================================

    /// Create a new basic block in the current function.
    pub fn createBlock(self: *Builder, name: ?[]const u8) !BlockId {
        const func = self.current_func orelse return error.NoCurrentFunction;
        return func.addBlock(self.allocator, name);
    }

    /// Position the builder at the end of a block.
    pub fn positionAtEnd(self: *Builder, block_id: BlockId) void {
        self.current_block = block_id;
    }

    /// Get the current block ID.
    pub fn getCurrentBlock(self: *Builder) ?BlockId {
        return self.current_block;
    }

    /// Get the current block.
    fn getCurrentBlockPtr(self: *Builder) ?*BasicBlock {
        const func = self.current_func orelse return null;
        const block_id = self.current_block orelse return null;
        return func.getBlock(block_id);
    }

    /// Check if current block is terminated.
    pub fn isTerminated(self: *Builder) bool {
        const block = self.getCurrentBlockPtr() orelse return true;
        return block.isTerminated();
    }

    // ========================================================================
    // Instruction Emission Helpers
    // ========================================================================

    /// Emit an instruction and return its result value.
    fn emit(self: *Builder, op: Inst.Op, result_ty: ?IrType, span: ?ast.Span) !?Value {
        const func = self.current_func orelse return error.NoCurrentFunction;
        const block = self.getCurrentBlockPtr() orelse return error.NoCurrentBlock;

        const result = if (result_ty) |ty| func.newValue(ty) else null;

        try block.instructions.append(self.allocator, .{
            .op = op,
            .result = result,
            .span = span,
        });

        return result;
    }

    /// Emit an instruction with a result.
    fn emitWithResult(self: *Builder, op: Inst.Op, result_ty: IrType, span: ?ast.Span) !Value {
        return (try self.emit(op, result_ty, span)).?;
    }

    /// Emit an instruction without a result.
    fn emitNoResult(self: *Builder, op: Inst.Op, span: ?ast.Span) !void {
        _ = try self.emit(op, null, span);
    }

    // ========================================================================
    // Constants
    // ========================================================================

    /// Create an integer constant.
    pub fn constInt(self: *Builder, ty: IrType, value: i128) !Value {
        return self.emitWithResult(.{
            .constant = .{ .int = .{ .value = value, .ty = ty } },
        }, ty, null);
    }

    /// Create an i32 constant.
    pub fn constI32(self: *Builder, value: i32) !Value {
        return self.constInt(.i32_, value);
    }

    /// Create an i64 constant.
    pub fn constI64(self: *Builder, value: i64) !Value {
        return self.constInt(.i64_, value);
    }

    /// Create a float constant.
    pub fn constFloat(self: *Builder, ty: IrType, value: f64) !Value {
        return self.emitWithResult(.{
            .constant = .{ .float = .{ .value = value, .ty = ty } },
        }, ty, null);
    }

    /// Create an f64 constant.
    pub fn constF64(self: *Builder, value: f64) !Value {
        return self.constFloat(.f64_, value);
    }

    /// Create a boolean constant.
    pub fn constBool(self: *Builder, value: bool) !Value {
        return self.emitWithResult(.{
            .constant = .{ .bool_ = value },
        }, .bool_, null);
    }

    /// Create a void constant.
    pub fn constVoid(self: *Builder) !Value {
        return self.emitWithResult(.{ .constant = .void_ }, .void_, null);
    }

    // ========================================================================
    // Arithmetic (Checked)
    // ========================================================================

    /// Integer addition (checked).
    pub fn buildAdd(self: *Builder, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .add = .{ .lhs = lhs, .rhs = rhs },
        }, lhs.ty, null);
    }

    /// Integer subtraction (checked).
    pub fn buildSub(self: *Builder, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .sub = .{ .lhs = lhs, .rhs = rhs },
        }, lhs.ty, null);
    }

    /// Integer multiplication (checked).
    pub fn buildMul(self: *Builder, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .mul = .{ .lhs = lhs, .rhs = rhs },
        }, lhs.ty, null);
    }

    /// Signed division.
    pub fn buildSDiv(self: *Builder, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .sdiv = .{ .lhs = lhs, .rhs = rhs },
        }, lhs.ty, null);
    }

    /// Unsigned division.
    pub fn buildUDiv(self: *Builder, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .udiv = .{ .lhs = lhs, .rhs = rhs },
        }, lhs.ty, null);
    }

    /// Signed remainder.
    pub fn buildSRem(self: *Builder, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .srem = .{ .lhs = lhs, .rhs = rhs },
        }, lhs.ty, null);
    }

    /// Unsigned remainder.
    pub fn buildURem(self: *Builder, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .urem = .{ .lhs = lhs, .rhs = rhs },
        }, lhs.ty, null);
    }

    // ========================================================================
    // Arithmetic (Wrapping)
    // ========================================================================

    /// Integer addition (wrapping).
    pub fn buildAddWrap(self: *Builder, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .add_wrap = .{ .lhs = lhs, .rhs = rhs },
        }, lhs.ty, null);
    }

    /// Integer subtraction (wrapping).
    pub fn buildSubWrap(self: *Builder, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .sub_wrap = .{ .lhs = lhs, .rhs = rhs },
        }, lhs.ty, null);
    }

    /// Integer multiplication (wrapping).
    pub fn buildMulWrap(self: *Builder, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .mul_wrap = .{ .lhs = lhs, .rhs = rhs },
        }, lhs.ty, null);
    }

    // ========================================================================
    // Arithmetic (Saturating)
    // ========================================================================

    /// Integer addition (saturating).
    pub fn buildAddSat(self: *Builder, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .add_sat = .{ .lhs = lhs, .rhs = rhs },
        }, lhs.ty, null);
    }

    /// Integer subtraction (saturating).
    pub fn buildSubSat(self: *Builder, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .sub_sat = .{ .lhs = lhs, .rhs = rhs },
        }, lhs.ty, null);
    }

    /// Integer multiplication (saturating).
    pub fn buildMulSat(self: *Builder, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .mul_sat = .{ .lhs = lhs, .rhs = rhs },
        }, lhs.ty, null);
    }

    // ========================================================================
    // Floating-Point Arithmetic
    // ========================================================================

    /// Float addition.
    pub fn buildFAdd(self: *Builder, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .fadd = .{ .lhs = lhs, .rhs = rhs },
        }, lhs.ty, null);
    }

    /// Float subtraction.
    pub fn buildFSub(self: *Builder, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .fsub = .{ .lhs = lhs, .rhs = rhs },
        }, lhs.ty, null);
    }

    /// Float multiplication.
    pub fn buildFMul(self: *Builder, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .fmul = .{ .lhs = lhs, .rhs = rhs },
        }, lhs.ty, null);
    }

    /// Float division.
    pub fn buildFDiv(self: *Builder, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .fdiv = .{ .lhs = lhs, .rhs = rhs },
        }, lhs.ty, null);
    }

    /// Float remainder.
    pub fn buildFRem(self: *Builder, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .frem = .{ .lhs = lhs, .rhs = rhs },
        }, lhs.ty, null);
    }

    // ========================================================================
    // Unary Operations
    // ========================================================================

    /// Integer negation.
    pub fn buildNeg(self: *Builder, operand: Value) !Value {
        return self.emitWithResult(.{
            .neg = .{ .operand = operand },
        }, operand.ty, null);
    }

    /// Float negation.
    pub fn buildFNeg(self: *Builder, operand: Value) !Value {
        return self.emitWithResult(.{
            .fneg = .{ .operand = operand },
        }, operand.ty, null);
    }

    /// Logical not.
    pub fn buildNot(self: *Builder, operand: Value) !Value {
        return self.emitWithResult(.{
            .not = .{ .operand = operand },
        }, .bool_, null);
    }

    // ========================================================================
    // Comparison
    // ========================================================================

    /// Integer comparison.
    pub fn buildICmp(self: *Builder, pred: Inst.CmpPred, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .icmp = .{ .pred = pred, .lhs = lhs, .rhs = rhs },
        }, .bool_, null);
    }

    /// Float comparison.
    pub fn buildFCmp(self: *Builder, pred: Inst.CmpPred, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .fcmp = .{ .pred = pred, .lhs = lhs, .rhs = rhs },
        }, .bool_, null);
    }

    // ========================================================================
    // Bitwise Operations
    // ========================================================================

    /// Bitwise AND.
    pub fn buildAnd(self: *Builder, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .bit_and = .{ .lhs = lhs, .rhs = rhs },
        }, lhs.ty, null);
    }

    /// Bitwise OR.
    pub fn buildOr(self: *Builder, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .bit_or = .{ .lhs = lhs, .rhs = rhs },
        }, lhs.ty, null);
    }

    /// Bitwise XOR.
    pub fn buildXor(self: *Builder, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .bit_xor = .{ .lhs = lhs, .rhs = rhs },
        }, lhs.ty, null);
    }

    /// Shift left.
    pub fn buildShl(self: *Builder, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .shl = .{ .lhs = lhs, .rhs = rhs },
        }, lhs.ty, null);
    }

    /// Arithmetic shift right.
    pub fn buildAShr(self: *Builder, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .ashr = .{ .lhs = lhs, .rhs = rhs },
        }, lhs.ty, null);
    }

    /// Logical shift right.
    pub fn buildLShr(self: *Builder, lhs: Value, rhs: Value) !Value {
        return self.emitWithResult(.{
            .lshr = .{ .lhs = lhs, .rhs = rhs },
        }, lhs.ty, null);
    }

    // ========================================================================
    // Type Conversions
    // ========================================================================

    /// Sign-extend integer.
    pub fn buildSExt(self: *Builder, operand: Value, target_ty: IrType) !Value {
        return self.emitWithResult(.{
            .sext = .{ .operand = operand, .target_ty = target_ty },
        }, target_ty, null);
    }

    /// Zero-extend integer.
    pub fn buildZExt(self: *Builder, operand: Value, target_ty: IrType) !Value {
        return self.emitWithResult(.{
            .zext = .{ .operand = operand, .target_ty = target_ty },
        }, target_ty, null);
    }

    /// Truncate integer.
    pub fn buildTrunc(self: *Builder, operand: Value, target_ty: IrType) !Value {
        return self.emitWithResult(.{
            .trunc = .{ .operand = operand, .target_ty = target_ty },
        }, target_ty, null);
    }

    /// Signed int to float.
    pub fn buildSIToFP(self: *Builder, operand: Value, target_ty: IrType) !Value {
        return self.emitWithResult(.{
            .sitofp = .{ .operand = operand, .target_ty = target_ty },
        }, target_ty, null);
    }

    /// Float to signed int.
    pub fn buildFPToSI(self: *Builder, operand: Value, target_ty: IrType) !Value {
        return self.emitWithResult(.{
            .fptosi = .{ .operand = operand, .target_ty = target_ty },
        }, target_ty, null);
    }

    // ========================================================================
    // Memory Operations
    // ========================================================================

    /// Stack allocation.
    pub fn buildAlloca(self: *Builder, ty: IrType, name: ?[]const u8) !Value {
        const ptr_ty = IrType{ .ptr = try self.allocType(ty) };
        return self.emitWithResult(.{
            .alloca = .{ .ty = ty, .count = null, .name = name },
        }, ptr_ty, null);
    }

    /// Load from memory.
    pub fn buildLoad(self: *Builder, ty: IrType, ptr: Value) !Value {
        return self.emitWithResult(.{
            .load = .{ .ptr = ptr, .ty = ty },
        }, ty, null);
    }

    /// Store to memory.
    pub fn buildStore(self: *Builder, value: Value, ptr: Value) !void {
        try self.emitNoResult(.{
            .store = .{ .value = value, .ptr = ptr },
        }, null);
    }

    // Helper to allocate a type for pointer types
    fn allocType(self: *Builder, ty: IrType) !*const IrType {
        const ptr = try self.allocator.create(IrType);
        ptr.* = ty;
        return ptr;
    }

    // ========================================================================
    // Function Calls
    // ========================================================================

    /// Direct function call.
    pub fn buildCall(self: *Builder, func_name: []const u8, args: []const Value, return_ty: IrType) !Value {
        const args_copy = try self.allocator.dupe(Value, args);
        return self.emitWithResult(.{
            .call = .{
                .func_name = func_name,
                .args = args_copy,
                .return_ty = return_ty,
            },
        }, return_ty, null);
    }

    /// Direct function call returning void.
    pub fn buildCallVoid(self: *Builder, func_name: []const u8, args: []const Value) !void {
        const args_copy = try self.allocator.dupe(Value, args);
        try self.emitNoResult(.{
            .call = .{
                .func_name = func_name,
                .args = args_copy,
                .return_ty = .void_,
            },
        }, null);
    }

    // ========================================================================
    // Ownership Operations
    // ========================================================================

    /// Move ownership.
    pub fn buildMove(self: *Builder, src: Value, dst_name: ?[]const u8) !Value {
        return self.emitWithResult(.{
            .move = .{ .src = src, .dst_name = dst_name },
        }, src.ty, null);
    }

    /// Copy value (for Copy types).
    pub fn buildCopy(self: *Builder, operand: Value) !Value {
        return self.emitWithResult(.{
            .copy = .{ .operand = operand },
        }, operand.ty, null);
    }

    /// Drop value (call destructor).
    pub fn buildDrop(self: *Builder, value: Value, ty: IrType) !void {
        try self.emitNoResult(.{
            .drop = .{ .value = value, .ty = ty },
        }, null);
    }

    /// Create immutable borrow.
    pub fn buildBorrow(self: *Builder, operand: Value) !Value {
        const ref_ty = IrType{
            .ref = .{ .inner = try self.allocType(operand.ty) },
        };
        return self.emitWithResult(.{
            .borrow = .{ .operand = operand },
        }, ref_ty, null);
    }

    /// Create mutable borrow.
    pub fn buildBorrowMut(self: *Builder, operand: Value) !Value {
        const ref_ty = IrType{
            .ref_mut = .{ .inner = try self.allocType(operand.ty) },
        };
        return self.emitWithResult(.{
            .borrow_mut = .{ .operand = operand },
        }, ref_ty, null);
    }

    // ========================================================================
    // Reference Counting
    // ========================================================================

    /// Increment reference count.
    pub fn buildRcInc(self: *Builder, operand: Value) !Value {
        return self.emitWithResult(.{
            .rc_inc = .{ .operand = operand },
        }, operand.ty, null);
    }

    /// Decrement reference count.
    pub fn buildRcDec(self: *Builder, operand: Value) !void {
        try self.emitNoResult(.{
            .rc_dec = .{ .operand = operand },
        }, null);
    }

    // ========================================================================
    // Optional Operations
    // ========================================================================

    /// Wrap value in Some.
    pub fn buildSome(self: *Builder, operand: Value) !Value {
        const opt_ty = IrType{ .optional = try self.allocType(operand.ty) };
        return self.emitWithResult(.{
            .some = .{ .operand = operand },
        }, opt_ty, null);
    }

    /// Create None value.
    pub fn buildNone(self: *Builder, inner_ty: IrType) !Value {
        const opt_ty = IrType{ .optional = try self.allocType(inner_ty) };
        return self.emitWithResult(.{
            .none = .{ .ty = opt_ty },
        }, opt_ty, null);
    }

    /// Unwrap optional (trap if None).
    pub fn buildUnwrap(self: *Builder, operand: Value) !Value {
        // Extract inner type from optional
        const inner_ty = switch (operand.ty) {
            .optional => |inner| inner.*,
            else => operand.ty, // Fallback
        };
        return self.emitWithResult(.{
            .unwrap = .{ .operand = operand },
        }, inner_ty, null);
    }

    /// Check if optional is Some.
    pub fn buildIsSome(self: *Builder, operand: Value) !Value {
        return self.emitWithResult(.{
            .is_some = .{ .operand = operand },
        }, .bool_, null);
    }

    /// Check if optional is None.
    pub fn buildIsNone(self: *Builder, operand: Value) !Value {
        return self.emitWithResult(.{
            .is_none = .{ .operand = operand },
        }, .bool_, null);
    }

    // ========================================================================
    // Control Flow (Terminators)
    // ========================================================================

    /// Unconditional branch.
    pub fn buildBr(self: *Builder, target: BlockId) !void {
        try self.emitNoResult(.{
            .br = .{ .target = target },
        }, null);

        // Update successor/predecessor info
        try self.addEdge(self.current_block.?, target);
    }

    /// Conditional branch.
    pub fn buildCondBr(self: *Builder, cond: Value, then_target: BlockId, else_target: BlockId) !void {
        try self.emitNoResult(.{
            .cond_br = .{
                .cond = cond,
                .then_target = then_target,
                .else_target = else_target,
            },
        }, null);

        // Update successor/predecessor info
        try self.addEdge(self.current_block.?, then_target);
        try self.addEdge(self.current_block.?, else_target);
    }

    /// Return from function.
    pub fn buildRet(self: *Builder, value: Value) !void {
        try self.emitNoResult(.{
            .ret = .{ .value = value },
        }, null);
    }

    /// Return void from function.
    pub fn buildRetVoid(self: *Builder) !void {
        try self.emitNoResult(.ret_void, null);
    }

    /// Unreachable (trap).
    pub fn buildUnreachable(self: *Builder) !void {
        try self.emitNoResult(.unreachable_, null);
    }

    // ========================================================================
    // Phi Nodes
    // ========================================================================

    /// Create a phi node.
    pub fn buildPhi(self: *Builder, ty: IrType, incoming: []const Inst.PhiIncoming) !Value {
        const incoming_copy = try self.allocator.dupe(Inst.PhiIncoming, incoming);
        return self.emitWithResult(.{
            .phi = .{ .incoming = incoming_copy },
        }, ty, null);
    }

    // ========================================================================
    // Helpers
    // ========================================================================

    /// Add an edge from src to dst block.
    fn addEdge(self: *Builder, src: BlockId, dst: BlockId) !void {
        const func = self.current_func orelse return;

        // Add successor to src
        if (func.getBlock(src)) |src_block| {
            try src_block.successors.append(self.allocator, dst);
        }

        // Add predecessor to dst
        if (func.getBlock(dst)) |dst_block| {
            try dst_block.predecessors.append(self.allocator, src);
        }
    }

    pub const Error = error{
        NoCurrentFunction,
        NoCurrentBlock,
        OutOfMemory,
    };
};

// ============================================================================
// Tests
// ============================================================================

test "Builder basic function" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = Module.init(allocator, "test");

    var builder = Builder.init(allocator, &module);

    _ = try builder.beginFunction("add", &.{ "a", "b" }, &.{ .i32_, .i32_ }, .i32_);

    const func = builder.getCurrentFunction().?;
    try testing.expectEqualStrings("add", func.name);
    try testing.expectEqual(@as(usize, 2), func.params.len);

    builder.endFunction();
    try testing.expect(builder.getCurrentFunction() == null);
}

test "Builder arithmetic instructions" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = Module.init(allocator, "test");

    var builder = Builder.init(allocator, &module);

    _ = try builder.beginFunction("calc", &.{"x"}, &.{.i32_}, .i32_);

    // Get parameter
    const func = builder.getCurrentFunction().?;
    const x = func.params[0].value;

    // Build: x + 1
    const one = try builder.constI32(1);
    const sum = try builder.buildAdd(x, one);

    try testing.expectEqual(IrType.i32_, sum.ty);

    // Return result
    try builder.buildRet(sum);

    builder.endFunction();
}

test "Builder control flow" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = Module.init(allocator, "test");

    var builder = Builder.init(allocator, &module);

    _ = try builder.beginFunction("abs", &.{"x"}, &.{.i32_}, .i32_);

    const func = builder.getCurrentFunction().?;
    const x = func.params[0].value;

    // Create blocks
    const then_bb = try builder.createBlock("then");
    const else_bb = try builder.createBlock("else");
    const merge_bb = try builder.createBlock("merge");

    // Entry: compare x < 0
    const zero = try builder.constI32(0);
    const is_neg = try builder.buildICmp(.slt, x, zero);
    try builder.buildCondBr(is_neg, then_bb, else_bb);

    // Then: negate x
    builder.positionAtEnd(then_bb);
    const neg_x = try builder.buildNeg(x);
    try builder.buildBr(merge_bb);

    // Else: use x as-is
    builder.positionAtEnd(else_bb);
    try builder.buildBr(merge_bb);

    // Merge: phi node
    builder.positionAtEnd(merge_bb);
    const result = try builder.buildPhi(.i32_, &.{
        .{ .value = neg_x, .block = then_bb },
        .{ .value = x, .block = else_bb },
    });
    try builder.buildRet(result);

    builder.endFunction();

    // Verify block structure
    try testing.expectEqual(@as(usize, 4), func.blocks.items.len);
}

test "Builder memory operations" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = Module.init(allocator, "test");

    var builder = Builder.init(allocator, &module);

    _ = try builder.beginFunction("stack_var", &.{}, &.{}, .i32_);

    // Allocate local
    const ptr = try builder.buildAlloca(.i32_, "local");

    // Store value
    const val = try builder.constI32(42);
    try builder.buildStore(val, ptr);

    // Load and return
    const loaded = try builder.buildLoad(.i32_, ptr);
    try builder.buildRet(loaded);

    builder.endFunction();
}
