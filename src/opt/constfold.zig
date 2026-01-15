//! Constant Folding Pass
//!
//! This pass evaluates constant expressions at compile time, replacing
//! operations on constants with their computed results.
//!
//! ## Optimizations Performed
//!
//! - Arithmetic on integer constants: `3 + 4` → `7`
//! - Arithmetic on float constants: `3.0 * 2.0` → `6.0`
//! - Comparison of constants: `5 > 3` → `true`
//! - Boolean operations: `true and false` → `false`
//! - Negation: `-(-5)` → `5`
//! - Bitwise operations on constants
//!
//! ## Example
//!
//! Before:
//! ```
//! %0 = const.i32 3
//! %1 = const.i32 4
//! %2 = add %0, %1
//! ret %2
//! ```
//!
//! After:
//! ```
//! %0 = const.i32 7
//! ret %0
//! ```

const std = @import("std");
const Allocator = std.mem.Allocator;
const ir = @import("../ir/mod.zig");
const PassStats = @import("mod.zig").PassStats;

/// Run constant folding on a module.
pub fn run(allocator: Allocator, module: *ir.Module) !PassStats {
    var stats = PassStats{};

    for (module.functions.items) |*func| {
        const func_stats = try runOnFunction(allocator, func);
        stats.merge(func_stats);
    }

    return stats;
}

/// Run constant folding on a single function.
fn runOnFunction(allocator: Allocator, func: *ir.Function) !PassStats {
    var stats = PassStats{};

    // Map from value ID to constant value (if known)
    var constants = std.AutoHashMap(u32, ir.Constant).init(allocator);
    defer constants.deinit();

    for (func.blocks.items) |*block| {
        var i: usize = 0;
        while (i < block.instructions.items.len) {
            const inst = &block.instructions.items[i];

            // First, record any constant definitions
            if (inst.result) |result| {
                if (inst.op == .constant) {
                    try constants.put(result.id, inst.op.constant);
                }
            }

            // Try to fold the instruction
            if (try tryFold(&constants, inst)) |new_const| {
                // Replace with constant
                inst.op = .{ .constant = new_const };
                stats.constants_folded += 1;

                // Update our constant map
                if (inst.result) |result| {
                    try constants.put(result.id, new_const);
                }
            }

            i += 1;
        }
    }

    return stats;
}

/// Try to fold an instruction to a constant.
fn tryFold(constants: *std.AutoHashMap(u32, ir.Constant), inst: *ir.Inst) !?ir.Constant {
    return switch (inst.op) {
        // Integer arithmetic
        .add => |op| tryFoldBinaryInt(constants, op, addInt),
        .sub => |op| tryFoldBinaryInt(constants, op, subInt),
        .mul => |op| tryFoldBinaryInt(constants, op, mulInt),
        .sdiv => |op| tryFoldBinaryInt(constants, op, sdivInt),
        .udiv => |op| tryFoldBinaryInt(constants, op, udivInt),
        .srem => |op| tryFoldBinaryInt(constants, op, sremInt),
        .urem => |op| tryFoldBinaryInt(constants, op, uremInt),

        // Wrapping arithmetic
        .add_wrap => |op| tryFoldBinaryInt(constants, op, addInt),
        .sub_wrap => |op| tryFoldBinaryInt(constants, op, subInt),
        .mul_wrap => |op| tryFoldBinaryInt(constants, op, mulInt),

        // Float arithmetic
        .fadd => |op| tryFoldBinaryFloat(constants, op, addFloat),
        .fsub => |op| tryFoldBinaryFloat(constants, op, subFloat),
        .fmul => |op| tryFoldBinaryFloat(constants, op, mulFloat),
        .fdiv => |op| tryFoldBinaryFloat(constants, op, divFloat),

        // Unary operations
        .neg => |op| tryFoldUnaryInt(constants, op, negInt),
        .fneg => |op| tryFoldUnaryFloat(constants, op, negFloat),
        .not => |op| tryFoldNot(constants, op),

        // Comparisons
        .icmp => |op| tryFoldICmp(constants, op),
        .fcmp => |op| tryFoldFCmp(constants, op),

        // Bitwise operations
        .bit_and => |op| tryFoldBinaryInt(constants, op, bitAndInt),
        .bit_or => |op| tryFoldBinaryInt(constants, op, bitOrInt),
        .bit_xor => |op| tryFoldBinaryInt(constants, op, bitXorInt),
        .shl => |op| tryFoldShift(constants, op, true),
        .lshr => |op| tryFoldShift(constants, op, false),

        else => null,
    };
}

// ============================================================================
// Integer Arithmetic Helpers
// ============================================================================

fn tryFoldBinaryInt(
    constants: *std.AutoHashMap(u32, ir.Constant),
    op: ir.Inst.BinaryOp,
    comptime fold_fn: fn (i128, i128) i128,
) ?ir.Constant {
    const lhs_const = constants.get(op.lhs.id) orelse return null;
    const rhs_const = constants.get(op.rhs.id) orelse return null;

    const lhs_int = switch (lhs_const) {
        .int => |i| i,
        else => return null,
    };
    const rhs_int = switch (rhs_const) {
        .int => |i| i,
        else => return null,
    };

    const result = fold_fn(lhs_int.value, rhs_int.value);
    return .{ .int = .{ .value = result, .ty = lhs_int.ty } };
}

fn addInt(a: i128, b: i128) i128 {
    return a +% b;
}

fn subInt(a: i128, b: i128) i128 {
    return a -% b;
}

fn mulInt(a: i128, b: i128) i128 {
    return a *% b;
}

fn sdivInt(a: i128, b: i128) i128 {
    if (b == 0) return 0; // Avoid division by zero
    return @divTrunc(a, b);
}

fn udivInt(a: i128, b: i128) i128 {
    if (b == 0) return 0;
    // Treat as unsigned
    const ua: u128 = @bitCast(a);
    const ub: u128 = @bitCast(b);
    return @bitCast(ua / ub);
}

fn sremInt(a: i128, b: i128) i128 {
    if (b == 0) return 0;
    return @rem(a, b);
}

fn uremInt(a: i128, b: i128) i128 {
    if (b == 0) return 0;
    const ua: u128 = @bitCast(a);
    const ub: u128 = @bitCast(b);
    return @bitCast(ua % ub);
}

fn bitAndInt(a: i128, b: i128) i128 {
    return a & b;
}

fn bitOrInt(a: i128, b: i128) i128 {
    return a | b;
}

fn bitXorInt(a: i128, b: i128) i128 {
    return a ^ b;
}

fn tryFoldUnaryInt(
    constants: *std.AutoHashMap(u32, ir.Constant),
    op: ir.Inst.UnaryOp,
    comptime fold_fn: fn (i128) i128,
) ?ir.Constant {
    const operand_const = constants.get(op.operand.id) orelse return null;

    const operand_int = switch (operand_const) {
        .int => |i| i,
        else => return null,
    };

    const result = fold_fn(operand_int.value);
    return .{ .int = .{ .value = result, .ty = operand_int.ty } };
}

fn negInt(a: i128) i128 {
    return -%a;
}

fn tryFoldShift(
    constants: *std.AutoHashMap(u32, ir.Constant),
    op: ir.Inst.BinaryOp,
    is_left: bool,
) ?ir.Constant {
    const lhs_const = constants.get(op.lhs.id) orelse return null;
    const rhs_const = constants.get(op.rhs.id) orelse return null;

    const lhs_int = switch (lhs_const) {
        .int => |i| i,
        else => return null,
    };
    const rhs_int = switch (rhs_const) {
        .int => |i| i,
        else => return null,
    };

    // Shift amount must be non-negative and less than bit width
    if (rhs_int.value < 0 or rhs_int.value >= 128) return null;

    const shift: u7 = @intCast(@as(u128, @bitCast(rhs_int.value)) & 0x7F);
    const result = if (is_left)
        lhs_int.value << shift
    else blk: {
        const u_val: u128 = @bitCast(lhs_int.value);
        break :blk @as(i128, @bitCast(u_val >> shift));
    };

    return .{ .int = .{ .value = result, .ty = lhs_int.ty } };
}

// ============================================================================
// Float Arithmetic Helpers
// ============================================================================

fn tryFoldBinaryFloat(
    constants: *std.AutoHashMap(u32, ir.Constant),
    op: ir.Inst.BinaryOp,
    comptime fold_fn: fn (f64, f64) f64,
) ?ir.Constant {
    const lhs_const = constants.get(op.lhs.id) orelse return null;
    const rhs_const = constants.get(op.rhs.id) orelse return null;

    const lhs_float = switch (lhs_const) {
        .float => |f| f,
        else => return null,
    };
    const rhs_float = switch (rhs_const) {
        .float => |f| f,
        else => return null,
    };

    const result = fold_fn(lhs_float.value, rhs_float.value);
    return .{ .float = .{ .value = result, .ty = lhs_float.ty } };
}

fn addFloat(a: f64, b: f64) f64 {
    return a + b;
}

fn subFloat(a: f64, b: f64) f64 {
    return a - b;
}

fn mulFloat(a: f64, b: f64) f64 {
    return a * b;
}

fn divFloat(a: f64, b: f64) f64 {
    return a / b;
}

fn tryFoldUnaryFloat(
    constants: *std.AutoHashMap(u32, ir.Constant),
    op: ir.Inst.UnaryOp,
    comptime fold_fn: fn (f64) f64,
) ?ir.Constant {
    const operand_const = constants.get(op.operand.id) orelse return null;

    const operand_float = switch (operand_const) {
        .float => |f| f,
        else => return null,
    };

    const result = fold_fn(operand_float.value);
    return .{ .float = .{ .value = result, .ty = operand_float.ty } };
}

fn negFloat(a: f64) f64 {
    return -a;
}

// ============================================================================
// Boolean and Comparison Helpers
// ============================================================================

fn tryFoldNot(
    constants: *std.AutoHashMap(u32, ir.Constant),
    op: ir.Inst.UnaryOp,
) ?ir.Constant {
    const operand_const = constants.get(op.operand.id) orelse return null;

    return switch (operand_const) {
        .bool_ => |b| .{ .bool_ = !b },
        else => null,
    };
}

fn tryFoldICmp(
    constants: *std.AutoHashMap(u32, ir.Constant),
    op: ir.Inst.CmpOp,
) ?ir.Constant {
    const lhs_const = constants.get(op.lhs.id) orelse return null;
    const rhs_const = constants.get(op.rhs.id) orelse return null;

    const lhs_int = switch (lhs_const) {
        .int => |i| i.value,
        else => return null,
    };
    const rhs_int = switch (rhs_const) {
        .int => |i| i.value,
        else => return null,
    };

    const result = switch (op.pred) {
        .eq => lhs_int == rhs_int,
        .ne => lhs_int != rhs_int,
        .slt => lhs_int < rhs_int,
        .sle => lhs_int <= rhs_int,
        .sgt => lhs_int > rhs_int,
        .sge => lhs_int >= rhs_int,
        .ult, .ule, .ugt, .uge => blk: {
            const lhs_u: u128 = @bitCast(lhs_int);
            const rhs_u: u128 = @bitCast(rhs_int);
            break :blk switch (op.pred) {
                .ult => lhs_u < rhs_u,
                .ule => lhs_u <= rhs_u,
                .ugt => lhs_u > rhs_u,
                .uge => lhs_u >= rhs_u,
                else => unreachable,
            };
        },
        else => return null, // Float predicates not applicable
    };

    return .{ .bool_ = result };
}

fn tryFoldFCmp(
    constants: *std.AutoHashMap(u32, ir.Constant),
    op: ir.Inst.CmpOp,
) ?ir.Constant {
    const lhs_const = constants.get(op.lhs.id) orelse return null;
    const rhs_const = constants.get(op.rhs.id) orelse return null;

    const lhs_float = switch (lhs_const) {
        .float => |f| f.value,
        else => return null,
    };
    const rhs_float = switch (rhs_const) {
        .float => |f| f.value,
        else => return null,
    };

    const result = switch (op.pred) {
        .oeq => lhs_float == rhs_float,
        .one => lhs_float != rhs_float,
        .olt => lhs_float < rhs_float,
        .ole => lhs_float <= rhs_float,
        .ogt => lhs_float > rhs_float,
        .oge => lhs_float >= rhs_float,
        .ord => !std.math.isNan(lhs_float) and !std.math.isNan(rhs_float),
        .uno => std.math.isNan(lhs_float) or std.math.isNan(rhs_float),
        else => return null, // Integer predicates not applicable
    };

    return .{ .bool_ = result };
}

// ============================================================================
// Tests
// ============================================================================

test "constant folding - integer addition" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = ir.Module.init(allocator, "test");

    var builder = ir.Builder.init(allocator, &module);

    _ = try builder.beginFunction("test", &.{}, &.{}, .i32_);

    // Build: const 3 + const 4
    const three = try builder.constI32(3);
    const four = try builder.constI32(4);
    _ = try builder.buildAdd(three, four);
    try builder.buildRet(three); // Just need a terminator

    builder.endFunction();

    // Run constant folding
    const stats = try run(allocator, &module);

    try testing.expectEqual(@as(usize, 1), stats.constants_folded);
}

test "constant folding - integer comparison" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = ir.Module.init(allocator, "test");

    var builder = ir.Builder.init(allocator, &module);

    _ = try builder.beginFunction("test", &.{}, &.{}, .bool_);

    // Build: 5 > 3 (should fold to true)
    const five = try builder.constI32(5);
    const three = try builder.constI32(3);
    const cmp = try builder.buildICmp(.sgt, five, three);
    try builder.buildRet(cmp);

    builder.endFunction();

    // Run constant folding
    const stats = try run(allocator, &module);

    try testing.expectEqual(@as(usize, 1), stats.constants_folded);
}

test "constant folding - float arithmetic" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = ir.Module.init(allocator, "test");

    var builder = ir.Builder.init(allocator, &module);

    _ = try builder.beginFunction("test", &.{}, &.{}, .f64_);

    // Build: 3.0 * 2.0 (should fold to 6.0)
    const three = try builder.constF64(3.0);
    const two = try builder.constF64(2.0);
    const result = try builder.buildFMul(three, two);
    try builder.buildRet(result);

    builder.endFunction();

    // Run constant folding
    const stats = try run(allocator, &module);

    try testing.expectEqual(@as(usize, 1), stats.constants_folded);
}

test "constant folding - boolean not" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = ir.Module.init(allocator, "test");

    var builder = ir.Builder.init(allocator, &module);

    _ = try builder.beginFunction("test", &.{}, &.{}, .bool_);

    // Build: not true (should fold to false)
    const t = try builder.constBool(true);
    const result = try builder.buildNot(t);
    try builder.buildRet(result);

    builder.endFunction();

    // Run constant folding
    const stats = try run(allocator, &module);

    try testing.expectEqual(@as(usize, 1), stats.constants_folded);
}

test "constant folding - no folding for non-constants" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = ir.Module.init(allocator, "test");

    var builder = ir.Builder.init(allocator, &module);

    _ = try builder.beginFunction("test", &.{"x"}, &.{.i32_}, .i32_);

    const func = builder.getCurrentFunction().?;
    const x = func.params[0].value;

    // Build: x + 1 (cannot fold because x is not constant)
    const one = try builder.constI32(1);
    const result = try builder.buildAdd(x, one);
    try builder.buildRet(result);

    builder.endFunction();

    // Run constant folding - should not fold anything
    const stats = try run(allocator, &module);

    try testing.expectEqual(@as(usize, 0), stats.constants_folded);
}
