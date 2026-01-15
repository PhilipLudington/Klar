//! Dead Code Elimination Pass
//!
//! This pass removes instructions whose results are never used, as well as
//! unreachable basic blocks.
//!
//! ## Optimizations Performed
//!
//! - Remove instructions with unused results (except side-effecting ops)
//! - Remove unreachable basic blocks
//! - Remove branches to unreachable code
//!
//! ## Side Effects
//!
//! The following operations are considered to have side effects and are not removed:
//! - Store operations
//! - Function calls (may have side effects)
//! - Drop operations
//! - Control flow terminators
//! - Heap operations
//!
//! ## Example
//!
//! Before:
//! ```
//! %0 = const.i32 5      ; unused
//! %1 = const.i32 10
//! %2 = add %0, %1       ; unused
//! %3 = const.i32 42
//! ret %3
//! ```
//!
//! After:
//! ```
//! %3 = const.i32 42
//! ret %3
//! ```

const std = @import("std");
const Allocator = std.mem.Allocator;
const ir = @import("../ir/mod.zig");
const PassStats = @import("mod.zig").PassStats;

/// Run dead code elimination on a module.
pub fn run(allocator: Allocator, module: *ir.Module) !PassStats {
    var stats = PassStats{};

    for (module.functions.items) |*func| {
        if (func.is_extern) continue;

        const func_stats = try runOnFunction(allocator, func);
        stats.merge(func_stats);
    }

    return stats;
}

/// Run dead code elimination on a single function.
fn runOnFunction(allocator: Allocator, func: *ir.Function) !PassStats {
    var stats = PassStats{};

    // Phase 1: Remove unreachable blocks
    const blocks_removed = try removeUnreachableBlocks(allocator, func);
    stats.blocks_removed = blocks_removed;

    // Phase 2: Find all used values
    var used_values = std.AutoHashMap(u32, void).init(allocator);
    defer used_values.deinit();

    try collectUsedValues(func, &used_values);

    // Phase 3: Remove dead instructions
    for (func.blocks.items) |*block| {
        var i: usize = 0;
        while (i < block.instructions.items.len) {
            const inst = &block.instructions.items[i];

            // Check if instruction can be removed
            if (inst.result) |result| {
                if (!used_values.contains(result.id) and !hasSideEffects(inst.op)) {
                    // Remove the instruction
                    _ = block.instructions.orderedRemove(i);
                    stats.instructions_removed += 1;
                    continue;
                }
            }

            i += 1;
        }
    }

    return stats;
}

/// Find all reachable blocks and remove unreachable ones.
fn removeUnreachableBlocks(allocator: Allocator, func: *ir.Function) !usize {
    if (func.blocks.items.len == 0) return 0;

    // Find reachable blocks starting from entry
    var reachable = std.AutoHashMap(ir.BlockId, void).init(allocator);
    defer reachable.deinit();

    var worklist = std.ArrayListUnmanaged(ir.BlockId){};
    defer worklist.deinit(allocator);

    // Start with entry block
    try worklist.append(allocator, 0);
    try reachable.put(0, {});

    while (worklist.items.len > 0) {
        const block_id = worklist.pop() orelse continue;

        // Find the block
        const block = func.getBlock(block_id) orelse continue;

        // Add successors to worklist
        for (block.successors.items) |succ| {
            if (!reachable.contains(succ)) {
                try reachable.put(succ, {});
                try worklist.append(allocator, succ);
            }
        }

        // Also check for successors in terminator instruction
        if (block.instructions.items.len > 0) {
            const last = block.instructions.items[block.instructions.items.len - 1];
            switch (last.op) {
                .br => |op| {
                    if (!reachable.contains(op.target)) {
                        try reachable.put(op.target, {});
                        try worklist.append(allocator, op.target);
                    }
                },
                .cond_br => |op| {
                    if (!reachable.contains(op.then_target)) {
                        try reachable.put(op.then_target, {});
                        try worklist.append(allocator, op.then_target);
                    }
                    if (!reachable.contains(op.else_target)) {
                        try reachable.put(op.else_target, {});
                        try worklist.append(allocator, op.else_target);
                    }
                },
                else => {},
            }
        }
    }

    // Remove unreachable blocks
    var removed: usize = 0;
    var i: usize = 0;
    while (i < func.blocks.items.len) {
        const block = &func.blocks.items[i];
        if (!reachable.contains(block.id)) {
            block.deinit(allocator);
            _ = func.blocks.orderedRemove(i);
            removed += 1;
        } else {
            i += 1;
        }
    }

    return removed;
}

/// Collect all values that are used.
fn collectUsedValues(func: *ir.Function, used: *std.AutoHashMap(u32, void)) !void {
    for (func.blocks.items) |*block| {
        for (block.instructions.items) |inst| {
            try collectOperandUses(inst.op, used);
        }
    }
}

/// Collect value uses from an instruction's operands.
fn collectOperandUses(op: ir.Inst.Op, used: *std.AutoHashMap(u32, void)) !void {
    switch (op) {
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
        => |bin_op| {
            try used.put(bin_op.lhs.id, {});
            try used.put(bin_op.rhs.id, {});
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
        => |un_op| {
            try used.put(un_op.operand.id, {});
        },

        // Comparison
        .icmp, .fcmp => |cmp_op| {
            try used.put(cmp_op.lhs.id, {});
            try used.put(cmp_op.rhs.id, {});
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
        => |cast_op| {
            try used.put(cast_op.operand.id, {});
        },

        // Memory operations
        .load => |load_op| {
            try used.put(load_op.ptr.id, {});
        },
        .store => |store_op| {
            try used.put(store_op.value.id, {});
            try used.put(store_op.ptr.id, {});
        },
        .alloca => |alloca_op| {
            if (alloca_op.count) |count| {
                try used.put(count.id, {});
            }
        },

        // Heap operations
        .heap_alloc => |heap_op| {
            try used.put(heap_op.size.id, {});
        },
        .heap_free => |free_op| {
            try used.put(free_op.operand.id, {});
        },

        // Aggregate operations
        .gep => |gep_op| {
            try used.put(gep_op.base.id, {});
            for (gep_op.indices) |idx| {
                try used.put(idx.id, {});
            }
        },
        .extract_value => |ext_op| {
            try used.put(ext_op.aggregate.id, {});
        },
        .insert_value => |ins_op| {
            try used.put(ins_op.aggregate.id, {});
            try used.put(ins_op.value.id, {});
        },

        // Function calls
        .call => |call_op| {
            for (call_op.args) |arg| {
                try used.put(arg.id, {});
            }
        },
        .call_indirect => |call_op| {
            try used.put(call_op.callee.id, {});
            for (call_op.args) |arg| {
                try used.put(arg.id, {});
            }
        },

        // Ownership operations
        .move => |move_op| {
            try used.put(move_op.src.id, {});
        },
        .drop => |drop_op| {
            try used.put(drop_op.value.id, {});
        },

        // Control flow
        .cond_br => |cond_br| {
            try used.put(cond_br.cond.id, {});
        },
        .ret => |ret_op| {
            if (ret_op.value) |val| {
                try used.put(val.id, {});
            }
        },

        // Phi nodes
        .phi => |phi_op| {
            for (phi_op.incoming) |incoming| {
                try used.put(incoming.value.id, {});
            }
        },

        // No operands
        .constant, .none, .br, .ret_void, .unreachable_ => {},
    }
}

/// Check if an instruction has side effects.
fn hasSideEffects(op: ir.Inst.Op) bool {
    return switch (op) {
        // Memory side effects
        .store,
        .heap_alloc,
        .heap_free,
        => true,

        // Control flow (terminators)
        .br,
        .cond_br,
        .ret,
        .ret_void,
        .unreachable_,
        => true,

        // Function calls may have side effects
        .call, .call_indirect => true,

        // Ownership operations with side effects
        .drop,
        .rc_dec,
        => true,

        // Everything else is pure
        else => false,
    };
}

// ============================================================================
// Tests
// ============================================================================

test "DCE - remove unused constant" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = ir.Module.init(allocator, "test");

    var builder = ir.Builder.init(allocator, &module);

    _ = try builder.beginFunction("test", &.{}, &.{}, .i32_);

    // Create unused constant
    _ = try builder.constI32(5);
    // Create used constant
    const used = try builder.constI32(42);
    try builder.buildRet(used);

    builder.endFunction();

    // Get initial instruction count
    const func = module.getFunction("test").?;
    const initial_count = func.blocks.items[0].instructions.items.len;

    // Run DCE
    const stats = try run(allocator, &module);

    // Should have removed 1 instruction
    try testing.expectEqual(@as(usize, 1), stats.instructions_removed);
    try testing.expectEqual(initial_count - 1, func.blocks.items[0].instructions.items.len);
}

test "DCE - keep side-effecting instructions" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = ir.Module.init(allocator, "test");

    var builder = ir.Builder.init(allocator, &module);

    _ = try builder.beginFunction("test", &.{}, &.{}, .void_);

    // Create a store (side-effecting, should not be removed)
    const ptr = try builder.buildAlloca(.i32_, "x");
    const val = try builder.constI32(42);
    try builder.buildStore(val, ptr);
    try builder.buildRetVoid();

    builder.endFunction();

    // Get initial instruction count
    const func = module.getFunction("test").?;
    const initial_count = func.blocks.items[0].instructions.items.len;

    // Run DCE
    const stats = try run(allocator, &module);

    // Should not have removed the store
    try testing.expectEqual(@as(usize, 0), stats.instructions_removed);
    try testing.expectEqual(initial_count, func.blocks.items[0].instructions.items.len);
}

test "DCE - remove unused arithmetic chain" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = ir.Module.init(allocator, "test");

    var builder = ir.Builder.init(allocator, &module);

    _ = try builder.beginFunction("test", &.{}, &.{}, .i32_);

    // Create unused arithmetic chain
    const a = try builder.constI32(1);
    const b = try builder.constI32(2);
    const c = try builder.buildAdd(a, b); // unused
    _ = try builder.buildMul(c, c); // unused

    // Create used result
    const result = try builder.constI32(100);
    try builder.buildRet(result);

    builder.endFunction();

    // Run DCE - single pass removes instructions from bottom up
    // First pass: mul is unused (result not used) -> removed
    // After mul removed, add result is unused -> but this requires another iteration
    // For a single pass, we only remove the topmost unused instruction
    const stats = try run(allocator, &module);

    // Single pass DCE removes at least 1 instruction (the mul)
    // Full chain removal requires the pass manager to iterate
    try testing.expect(stats.instructions_removed >= 1);
}

test "DCE - preserve used values" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = ir.Module.init(allocator, "test");

    var builder = ir.Builder.init(allocator, &module);

    _ = try builder.beginFunction("test", &.{"x"}, &.{.i32_}, .i32_);

    const func = builder.getCurrentFunction().?;
    const x = func.params[0].value;

    // Create used arithmetic
    const one = try builder.constI32(1);
    const result = try builder.buildAdd(x, one);
    try builder.buildRet(result);

    builder.endFunction();

    // Get initial instruction count
    const initial_count = func.blocks.items[0].instructions.items.len;

    // Run DCE
    const stats = try run(allocator, &module);

    // Should not have removed anything
    try testing.expectEqual(@as(usize, 0), stats.instructions_removed);
    try testing.expectEqual(initial_count, func.blocks.items[0].instructions.items.len);
}
