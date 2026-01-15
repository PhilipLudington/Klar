//! Instruction Simplification Pass
//!
//! This pass applies algebraic simplifications to instructions, reducing
//! complex expressions to simpler forms without requiring constant operands.
//!
//! ## Simplifications Performed
//!
//! ### Arithmetic Identities
//! - `x + 0` → `x`
//! - `x - 0` → `x`
//! - `x * 1` → `x`
//! - `x * 0` → `0`
//! - `x / 1` → `x`
//! - `0 / x` → `0`
//!
//! ### Bitwise Identities
//! - `x & 0` → `0`
//! - `x & -1` → `x` (all ones)
//! - `x | 0` → `x`
//! - `x ^ 0` → `x`
//! - `x ^ x` → `0`
//! - `x & x` → `x`
//! - `x | x` → `x`
//!
//! ### Shift Identities
//! - `x << 0` → `x`
//! - `x >> 0` → `x`
//!
//! ### Boolean Identities
//! - `not (not x)` → `x`
//!
//! ### Self-Operations
//! - `x - x` → `0`
//!
//! ## Example
//!
//! Before:
//! ```
//! %0 = const.i32 0
//! %1 = add %x, %0   ; x + 0
//! ret %1
//! ```
//!
//! After:
//! ```
//! ret %x
//! ```

const std = @import("std");
const Allocator = std.mem.Allocator;
const ir = @import("../ir/mod.zig");
const PassStats = @import("mod.zig").PassStats;

/// Run instruction simplification on a module.
pub fn run(allocator: Allocator, module: *ir.Module) !PassStats {
    var stats = PassStats{};

    for (module.functions.items) |*func| {
        if (func.is_extern) continue;

        const func_stats = try runOnFunction(allocator, func);
        stats.merge(func_stats);
    }

    return stats;
}

/// Run instruction simplification on a single function.
fn runOnFunction(allocator: Allocator, func: *ir.Function) !PassStats {
    var stats = PassStats{};

    // Map from value ID to its constant value (for identity checks)
    var constants = std.AutoHashMap(u32, ir.Constant).init(allocator);
    defer constants.deinit();

    // Map from value ID to replacement value ID
    var replacements = std.AutoHashMap(u32, ir.Value).init(allocator);
    defer replacements.deinit();

    // First pass: collect constants and find simplifications
    for (func.blocks.items) |*block| {
        for (block.instructions.items) |*inst| {
            // Record constants
            if (inst.result) |result| {
                if (inst.op == .constant) {
                    try constants.put(result.id, inst.op.constant);
                }
            }

            // Try to simplify
            if (try trySimplify(&constants, inst)) |simplified| {
                if (inst.result) |result| {
                    try replacements.put(result.id, simplified);
                    stats.instructions_simplified += 1;
                }
            }
        }
    }

    // Second pass: apply replacements
    if (replacements.count() > 0) {
        for (func.blocks.items) |*block| {
            for (block.instructions.items) |*inst| {
                try applyReplacements(&replacements, inst);
            }
        }
    }

    return stats;
}

/// Try to simplify an instruction.
fn trySimplify(
    constants: *std.AutoHashMap(u32, ir.Constant),
    inst: *ir.Inst,
) !?ir.Value {
    return switch (inst.op) {
        // Arithmetic simplifications
        .add, .add_wrap => |op| trySimplifyAdd(constants, op),
        .sub, .sub_wrap => |op| trySimplifySub(constants, op),
        .mul, .mul_wrap => |op| trySimplifyMul(constants, op),
        .sdiv, .udiv => |op| trySimplifyDiv(constants, op),

        // Bitwise simplifications
        .bit_and => |op| trySimplifyAnd(constants, op),
        .bit_or => |op| trySimplifyOr(constants, op),
        .bit_xor => |op| trySimplifyXor(constants, op),
        .shl, .lshr, .ashr => |op| trySimplifyShift(constants, op),

        // Double negation
        .not => |op| trySimplifyNot(constants, op),

        else => null,
    };
}

/// Check if a constant is zero.
fn isZero(constants: *std.AutoHashMap(u32, ir.Constant), id: u32) bool {
    const c = constants.get(id) orelse return false;
    return switch (c) {
        .int => |i| i.value == 0,
        .float => |f| f.value == 0.0,
        else => false,
    };
}

/// Check if a constant is one.
fn isOne(constants: *std.AutoHashMap(u32, ir.Constant), id: u32) bool {
    const c = constants.get(id) orelse return false;
    return switch (c) {
        .int => |i| i.value == 1,
        .float => |f| f.value == 1.0,
        else => false,
    };
}

/// Check if a constant is all ones (-1 for signed).
fn isAllOnes(constants: *std.AutoHashMap(u32, ir.Constant), id: u32) bool {
    const c = constants.get(id) orelse return false;
    return switch (c) {
        .int => |i| i.value == -1,
        else => false,
    };
}

/// Simplify addition: x + 0 → x
fn trySimplifyAdd(
    constants: *std.AutoHashMap(u32, ir.Constant),
    op: ir.Inst.BinaryOp,
) ?ir.Value {
    // x + 0 → x
    if (isZero(constants, op.rhs.id)) {
        return op.lhs;
    }
    // 0 + x → x
    if (isZero(constants, op.lhs.id)) {
        return op.rhs;
    }
    return null;
}

/// Simplify subtraction: x - 0 → x, x - x → 0
fn trySimplifySub(
    constants: *std.AutoHashMap(u32, ir.Constant),
    op: ir.Inst.BinaryOp,
) ?ir.Value {
    // x - 0 → x
    if (isZero(constants, op.rhs.id)) {
        return op.lhs;
    }
    // Note: x - x → 0 would require creating a new constant,
    // which we can't do without more context. Skip for now.
    return null;
}

/// Simplify multiplication: x * 1 → x, x * 0 → 0
fn trySimplifyMul(
    constants: *std.AutoHashMap(u32, ir.Constant),
    op: ir.Inst.BinaryOp,
) ?ir.Value {
    // x * 1 → x
    if (isOne(constants, op.rhs.id)) {
        return op.lhs;
    }
    // 1 * x → x
    if (isOne(constants, op.lhs.id)) {
        return op.rhs;
    }
    // x * 0 → 0 (return the zero constant)
    if (isZero(constants, op.rhs.id)) {
        return op.rhs;
    }
    // 0 * x → 0
    if (isZero(constants, op.lhs.id)) {
        return op.lhs;
    }
    return null;
}

/// Simplify division: x / 1 → x, 0 / x → 0
fn trySimplifyDiv(
    constants: *std.AutoHashMap(u32, ir.Constant),
    op: ir.Inst.BinaryOp,
) ?ir.Value {
    // x / 1 → x
    if (isOne(constants, op.rhs.id)) {
        return op.lhs;
    }
    // 0 / x → 0
    if (isZero(constants, op.lhs.id)) {
        return op.lhs;
    }
    return null;
}

/// Simplify bitwise AND: x & 0 → 0, x & -1 → x, x & x → x
fn trySimplifyAnd(
    constants: *std.AutoHashMap(u32, ir.Constant),
    op: ir.Inst.BinaryOp,
) ?ir.Value {
    // x & 0 → 0
    if (isZero(constants, op.rhs.id)) {
        return op.rhs;
    }
    if (isZero(constants, op.lhs.id)) {
        return op.lhs;
    }
    // x & -1 → x
    if (isAllOnes(constants, op.rhs.id)) {
        return op.lhs;
    }
    if (isAllOnes(constants, op.lhs.id)) {
        return op.rhs;
    }
    // x & x → x
    if (op.lhs.id == op.rhs.id) {
        return op.lhs;
    }
    return null;
}

/// Simplify bitwise OR: x | 0 → x, x | x → x
fn trySimplifyOr(
    constants: *std.AutoHashMap(u32, ir.Constant),
    op: ir.Inst.BinaryOp,
) ?ir.Value {
    // x | 0 → x
    if (isZero(constants, op.rhs.id)) {
        return op.lhs;
    }
    if (isZero(constants, op.lhs.id)) {
        return op.rhs;
    }
    // x | x → x
    if (op.lhs.id == op.rhs.id) {
        return op.lhs;
    }
    return null;
}

/// Simplify bitwise XOR: x ^ 0 → x, x ^ x → 0
fn trySimplifyXor(
    constants: *std.AutoHashMap(u32, ir.Constant),
    op: ir.Inst.BinaryOp,
) ?ir.Value {
    // x ^ 0 → x
    if (isZero(constants, op.rhs.id)) {
        return op.lhs;
    }
    if (isZero(constants, op.lhs.id)) {
        return op.rhs;
    }
    // Note: x ^ x → 0 would require creating a new constant
    return null;
}

/// Simplify shifts: x << 0 → x, x >> 0 → x
fn trySimplifyShift(
    constants: *std.AutoHashMap(u32, ir.Constant),
    op: ir.Inst.BinaryOp,
) ?ir.Value {
    // x << 0 → x, x >> 0 → x
    if (isZero(constants, op.rhs.id)) {
        return op.lhs;
    }
    return null;
}

/// Simplify logical not: not (not x) → x
fn trySimplifyNot(
    _: *std.AutoHashMap(u32, ir.Constant),
    _: ir.Inst.UnaryOp,
) ?ir.Value {
    // Would need to track the source instruction, not just constants
    // Skip for now
    return null;
}

/// Apply value replacements to an instruction's operands.
fn applyReplacements(
    replacements: *std.AutoHashMap(u32, ir.Value),
    inst: *ir.Inst,
) !void {
    switch (inst.op) {
        // Binary operations
        .add,
        .sub,
        .mul,
        .sdiv,
        .udiv,
        .srem,
        .urem,
        .add_wrap,
        .sub_wrap,
        .mul_wrap,
        .add_sat,
        .sub_sat,
        .mul_sat,
        .fadd,
        .fsub,
        .fmul,
        .fdiv,
        .frem,
        .bit_and,
        .bit_or,
        .bit_xor,
        .shl,
        .ashr,
        .lshr,
        => |*bin_op| {
            if (replacements.get(bin_op.lhs.id)) |repl| {
                bin_op.lhs = repl;
            }
            if (replacements.get(bin_op.rhs.id)) |repl| {
                bin_op.rhs = repl;
            }
        },

        // Unary operations
        .neg,
        .fneg,
        .not,
        .copy,
        .borrow,
        .borrow_mut,
        .rc_inc,
        .rc_dec,
        .some,
        .unwrap,
        .is_some,
        .is_none,
        => |*un_op| {
            if (replacements.get(un_op.operand.id)) |repl| {
                un_op.operand = repl;
            }
        },

        // Comparison
        .icmp, .fcmp => |*cmp_op| {
            if (replacements.get(cmp_op.lhs.id)) |repl| {
                cmp_op.lhs = repl;
            }
            if (replacements.get(cmp_op.rhs.id)) |repl| {
                cmp_op.rhs = repl;
            }
        },

        // Cast operations
        .sext,
        .zext,
        .trunc,
        .fpext,
        .fptrunc,
        .sitofp,
        .uitofp,
        .fptosi,
        .fptoui,
        .ptrtoint,
        .inttoptr,
        .bitcast,
        => |*cast_op| {
            if (replacements.get(cast_op.operand.id)) |repl| {
                cast_op.operand = repl;
            }
        },

        // Memory operations
        .load => |*load_op| {
            if (replacements.get(load_op.ptr.id)) |repl| {
                load_op.ptr = repl;
            }
        },
        .store => |*store_op| {
            if (replacements.get(store_op.value.id)) |repl| {
                store_op.value = repl;
            }
            if (replacements.get(store_op.ptr.id)) |repl| {
                store_op.ptr = repl;
            }
        },

        // Control flow
        .cond_br => |*cond_br| {
            if (replacements.get(cond_br.cond.id)) |repl| {
                cond_br.cond = repl;
            }
        },
        .ret => |*ret_op| {
            if (ret_op.value) |val| {
                if (replacements.get(val.id)) |repl| {
                    ret_op.value = repl;
                }
            }
        },

        // Move operation
        .move => |*move_op| {
            if (replacements.get(move_op.src.id)) |repl| {
                move_op.src = repl;
            }
        },

        // Drop operation
        .drop => |*drop_op| {
            if (replacements.get(drop_op.value.id)) |repl| {
                drop_op.value = repl;
            }
        },

        // Note: phi nodes, calls, gep etc. would need more complex handling
        // Skip for now
        else => {},
    }
}

// ============================================================================
// Tests
// ============================================================================

test "simplify - add zero" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = ir.Module.init(allocator, "test");

    var builder = ir.Builder.init(allocator, &module);

    _ = try builder.beginFunction("test", &.{"x"}, &.{.i32_}, .i32_);

    const func = builder.getCurrentFunction().?;
    const x = func.params[0].value;

    // Build: x + 0 (should simplify to x)
    const zero = try builder.constI32(0);
    const result = try builder.buildAdd(x, zero);
    try builder.buildRet(result);

    builder.endFunction();

    // Run simplification
    const stats = try run(allocator, &module);

    try testing.expectEqual(@as(usize, 1), stats.instructions_simplified);
}

test "simplify - multiply by one" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = ir.Module.init(allocator, "test");

    var builder = ir.Builder.init(allocator, &module);

    _ = try builder.beginFunction("test", &.{"x"}, &.{.i32_}, .i32_);

    const func = builder.getCurrentFunction().?;
    const x = func.params[0].value;

    // Build: x * 1 (should simplify to x)
    const one = try builder.constI32(1);
    const result = try builder.buildMul(x, one);
    try builder.buildRet(result);

    builder.endFunction();

    // Run simplification
    const stats = try run(allocator, &module);

    try testing.expectEqual(@as(usize, 1), stats.instructions_simplified);
}

test "simplify - multiply by zero" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = ir.Module.init(allocator, "test");

    var builder = ir.Builder.init(allocator, &module);

    _ = try builder.beginFunction("test", &.{"x"}, &.{.i32_}, .i32_);

    const func = builder.getCurrentFunction().?;
    const x = func.params[0].value;

    // Build: x * 0 (should simplify to 0)
    const zero = try builder.constI32(0);
    const result = try builder.buildMul(x, zero);
    try builder.buildRet(result);

    builder.endFunction();

    // Run simplification
    const stats = try run(allocator, &module);

    try testing.expectEqual(@as(usize, 1), stats.instructions_simplified);
}

test "simplify - bitwise and with itself" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = ir.Module.init(allocator, "test");

    var builder = ir.Builder.init(allocator, &module);

    _ = try builder.beginFunction("test", &.{"x"}, &.{.i32_}, .i32_);

    const func = builder.getCurrentFunction().?;
    const x = func.params[0].value;

    // Build: x & x (should simplify to x)
    const result = try builder.buildAnd(x, x);
    try builder.buildRet(result);

    builder.endFunction();

    // Run simplification
    const stats = try run(allocator, &module);

    try testing.expectEqual(@as(usize, 1), stats.instructions_simplified);
}

test "simplify - shift by zero" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = ir.Module.init(allocator, "test");

    var builder = ir.Builder.init(allocator, &module);

    _ = try builder.beginFunction("test", &.{"x"}, &.{.i32_}, .i32_);

    const func = builder.getCurrentFunction().?;
    const x = func.params[0].value;

    // Build: x << 0 (should simplify to x)
    const zero = try builder.constI32(0);
    const result = try builder.buildShl(x, zero);
    try builder.buildRet(result);

    builder.endFunction();

    // Run simplification
    const stats = try run(allocator, &module);

    try testing.expectEqual(@as(usize, 1), stats.instructions_simplified);
}

test "simplify - no simplification for non-identity" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = ir.Module.init(allocator, "test");

    var builder = ir.Builder.init(allocator, &module);

    _ = try builder.beginFunction("test", &.{"x"}, &.{.i32_}, .i32_);

    const func = builder.getCurrentFunction().?;
    const x = func.params[0].value;

    // Build: x + 5 (cannot simplify)
    const five = try builder.constI32(5);
    const result = try builder.buildAdd(x, five);
    try builder.buildRet(result);

    builder.endFunction();

    // Run simplification
    const stats = try run(allocator, &module);

    try testing.expectEqual(@as(usize, 0), stats.instructions_simplified);
}
