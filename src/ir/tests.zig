//! Unit tests for Klar IR generation.
//!
//! These tests verify that simple functions lower correctly to IR
//! and that the IR printer produces readable output.
//!
//! All tests use ArenaAllocator to avoid memory leak detection issues
//! since the IR builder allocates types that are intentionally long-lived.

const std = @import("std");
const testing = std.testing;
const inst = @import("inst.zig");
const builder_mod = @import("builder.zig");
const printer = @import("printer.zig");

const Module = inst.Module;
const Builder = builder_mod.Builder;
const IrType = inst.IrType;

// ============================================================================
// Simple Function Tests
// ============================================================================

test "IR: constant return function" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = Module.init(allocator, "test");
    var builder = Builder.init(allocator, &module);

    // fn main() -> i32 { 42 }
    _ = try builder.beginFunction("main", &.{}, &.{}, .i32_);

    const val = try builder.constI32(42);
    try builder.buildRet(val);

    builder.endFunction();

    // Verify output
    const output = try printer.moduleToString(allocator, &module);

    try testing.expect(std.mem.indexOf(u8, output, "define i32 @main") != null);
    try testing.expect(std.mem.indexOf(u8, output, "const i32 42") != null);
    try testing.expect(std.mem.indexOf(u8, output, "ret %") != null);
}

test "IR: add two parameters" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = Module.init(allocator, "test");
    var builder = Builder.init(allocator, &module);

    // fn add(a: i32, b: i32) -> i32 { a + b }
    _ = try builder.beginFunction("add", &.{ "a", "b" }, &.{ .i32_, .i32_ }, .i32_);

    const func = builder.getCurrentFunction().?;
    const a = func.params[0].value;
    const b = func.params[1].value;

    const sum = try builder.buildAdd(a, b);
    try builder.buildRet(sum);

    builder.endFunction();

    const output = try printer.moduleToString(allocator, &module);

    try testing.expect(std.mem.indexOf(u8, output, "define i32 @add") != null);
    try testing.expect(std.mem.indexOf(u8, output, "add %0, %1") != null);
}

test "IR: multiply with constant" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = Module.init(allocator, "test");
    var builder = Builder.init(allocator, &module);

    // fn double(x: i32) -> i32 { x * 2 }
    _ = try builder.beginFunction("double", &.{"x"}, &.{.i32_}, .i32_);

    const func = builder.getCurrentFunction().?;
    const x = func.params[0].value;

    const two = try builder.constI32(2);
    const result = try builder.buildMul(x, two);
    try builder.buildRet(result);

    builder.endFunction();

    const output = try printer.moduleToString(allocator, &module);

    try testing.expect(std.mem.indexOf(u8, output, "mul %0, %") != null);
}

// ============================================================================
// Control Flow Tests
// ============================================================================

test "IR: simple if-else" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = Module.init(allocator, "test");
    var builder = Builder.init(allocator, &module);

    _ = try builder.beginFunction("max", &.{ "a", "b" }, &.{ .i32_, .i32_ }, .i32_);

    const func = builder.getCurrentFunction().?;
    const a = func.params[0].value;
    const b = func.params[1].value;

    // Create blocks
    const then_bb = try builder.createBlock("then");
    const else_bb = try builder.createBlock("else");
    const merge_bb = try builder.createBlock("merge");

    // Compare
    const cond = try builder.buildICmp(.sgt, a, b);
    try builder.buildCondBr(cond, then_bb, else_bb);

    // Then: return a
    builder.positionAtEnd(then_bb);
    try builder.buildBr(merge_bb);

    // Else: return b
    builder.positionAtEnd(else_bb);
    try builder.buildBr(merge_bb);

    // Merge with phi
    builder.positionAtEnd(merge_bb);
    const result = try builder.buildPhi(.i32_, &.{
        .{ .value = a, .block = then_bb },
        .{ .value = b, .block = else_bb },
    });
    try builder.buildRet(result);

    builder.endFunction();

    const output = try printer.moduleToString(allocator, &module);

    try testing.expect(std.mem.indexOf(u8, output, "then:") != null);
    try testing.expect(std.mem.indexOf(u8, output, "else:") != null);
    try testing.expect(std.mem.indexOf(u8, output, "merge:") != null);
    try testing.expect(std.mem.indexOf(u8, output, "phi") != null);
}

test "IR: while loop" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = Module.init(allocator, "test");
    var builder = Builder.init(allocator, &module);

    _ = try builder.beginFunction("sum_to", &.{"n"}, &.{.i32_}, .i32_);

    const func = builder.getCurrentFunction().?;
    const n = func.params[0].value;

    // Allocate loop variables
    const sum_ptr = try builder.buildAlloca(.i32_, "sum");
    const i_ptr = try builder.buildAlloca(.i32_, "i");

    const zero = try builder.constI32(0);
    try builder.buildStore(zero, sum_ptr);
    try builder.buildStore(zero, i_ptr);

    // Create loop blocks
    const cond_bb = try builder.createBlock("while.cond");
    const body_bb = try builder.createBlock("while.body");
    const end_bb = try builder.createBlock("while.end");

    try builder.buildBr(cond_bb);

    // Condition: i < n
    builder.positionAtEnd(cond_bb);
    const i_val = try builder.buildLoad(.i32_, i_ptr);
    const cond = try builder.buildICmp(.slt, i_val, n);
    try builder.buildCondBr(cond, body_bb, end_bb);

    // Body: sum += i; i += 1
    builder.positionAtEnd(body_bb);
    const sum_val = try builder.buildLoad(.i32_, sum_ptr);
    const i_current = try builder.buildLoad(.i32_, i_ptr);
    const new_sum = try builder.buildAdd(sum_val, i_current);
    try builder.buildStore(new_sum, sum_ptr);

    const one = try builder.constI32(1);
    const i_next = try builder.buildAdd(i_current, one);
    try builder.buildStore(i_next, i_ptr);
    try builder.buildBr(cond_bb);

    // End: return sum
    builder.positionAtEnd(end_bb);
    const final_sum = try builder.buildLoad(.i32_, sum_ptr);
    try builder.buildRet(final_sum);

    builder.endFunction();

    const output = try printer.moduleToString(allocator, &module);

    try testing.expect(std.mem.indexOf(u8, output, "while.cond:") != null);
    try testing.expect(std.mem.indexOf(u8, output, "while.body:") != null);
    try testing.expect(std.mem.indexOf(u8, output, "while.end:") != null);
}

// ============================================================================
// Arithmetic Operation Tests
// ============================================================================

test "IR: all arithmetic operations" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = Module.init(allocator, "test");
    var builder = Builder.init(allocator, &module);

    _ = try builder.beginFunction("arith", &.{ "a", "b" }, &.{ .i32_, .i32_ }, .i32_);

    const func = builder.getCurrentFunction().?;
    const a = func.params[0].value;
    const b = func.params[1].value;

    // Test all arithmetic operations
    const sum = try builder.buildAdd(a, b);
    const diff = try builder.buildSub(sum, b);
    const prod = try builder.buildMul(diff, a);
    const quot = try builder.buildSDiv(prod, b);
    const rem = try builder.buildSRem(quot, a);

    try builder.buildRet(rem);
    builder.endFunction();

    const output = try printer.moduleToString(allocator, &module);

    try testing.expect(std.mem.indexOf(u8, output, "add") != null);
    try testing.expect(std.mem.indexOf(u8, output, "sub") != null);
    try testing.expect(std.mem.indexOf(u8, output, "mul") != null);
    try testing.expect(std.mem.indexOf(u8, output, "sdiv") != null);
    try testing.expect(std.mem.indexOf(u8, output, "srem") != null);
}

test "IR: wrapping and saturating arithmetic" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = Module.init(allocator, "test");
    var builder = Builder.init(allocator, &module);

    _ = try builder.beginFunction("safe_arith", &.{ "a", "b" }, &.{ .i32_, .i32_ }, .i32_);

    const func = builder.getCurrentFunction().?;
    const a = func.params[0].value;
    const b = func.params[1].value;

    // Wrapping
    const wrap_sum = try builder.buildAddWrap(a, b);
    const wrap_diff = try builder.buildSubWrap(wrap_sum, a);
    const wrap_prod = try builder.buildMulWrap(wrap_diff, b);

    // Saturating
    const sat_sum = try builder.buildAddSat(a, b);
    const sat_diff = try builder.buildSubSat(sat_sum, wrap_prod);
    _ = try builder.buildMulSat(sat_diff, a);

    try builder.buildRetVoid();
    builder.endFunction();

    const output = try printer.moduleToString(allocator, &module);

    try testing.expect(std.mem.indexOf(u8, output, "add.wrap") != null);
    try testing.expect(std.mem.indexOf(u8, output, "sub.wrap") != null);
    try testing.expect(std.mem.indexOf(u8, output, "mul.wrap") != null);
    try testing.expect(std.mem.indexOf(u8, output, "add.sat") != null);
    try testing.expect(std.mem.indexOf(u8, output, "sub.sat") != null);
    try testing.expect(std.mem.indexOf(u8, output, "mul.sat") != null);
}

test "IR: float arithmetic" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = Module.init(allocator, "test");
    var builder = Builder.init(allocator, &module);

    _ = try builder.beginFunction("float_arith", &.{ "a", "b" }, &.{ .f64_, .f64_ }, .f64_);

    const func = builder.getCurrentFunction().?;
    const a = func.params[0].value;
    const b = func.params[1].value;

    const sum = try builder.buildFAdd(a, b);
    const diff = try builder.buildFSub(sum, a);
    const prod = try builder.buildFMul(diff, b);
    const quot = try builder.buildFDiv(prod, a);
    const rem = try builder.buildFRem(quot, b);

    try builder.buildRet(rem);
    builder.endFunction();

    const output = try printer.moduleToString(allocator, &module);

    try testing.expect(std.mem.indexOf(u8, output, "define f64") != null);
    try testing.expect(std.mem.indexOf(u8, output, "fadd") != null);
    try testing.expect(std.mem.indexOf(u8, output, "fsub") != null);
    try testing.expect(std.mem.indexOf(u8, output, "fmul") != null);
    try testing.expect(std.mem.indexOf(u8, output, "fdiv") != null);
    try testing.expect(std.mem.indexOf(u8, output, "frem") != null);
}

// ============================================================================
// Comparison Tests
// ============================================================================

test "IR: integer comparisons" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = Module.init(allocator, "test");
    var builder = Builder.init(allocator, &module);

    _ = try builder.beginFunction("cmp", &.{ "a", "b" }, &.{ .i32_, .i32_ }, .bool_);

    const func = builder.getCurrentFunction().?;
    const a = func.params[0].value;
    const b = func.params[1].value;

    _ = try builder.buildICmp(.eq, a, b);
    _ = try builder.buildICmp(.ne, a, b);
    _ = try builder.buildICmp(.slt, a, b);
    _ = try builder.buildICmp(.sle, a, b);
    _ = try builder.buildICmp(.sgt, a, b);
    const result = try builder.buildICmp(.sge, a, b);

    try builder.buildRet(result);
    builder.endFunction();

    const output = try printer.moduleToString(allocator, &module);

    try testing.expect(std.mem.indexOf(u8, output, "icmp eq") != null);
    try testing.expect(std.mem.indexOf(u8, output, "icmp ne") != null);
    try testing.expect(std.mem.indexOf(u8, output, "icmp slt") != null);
    try testing.expect(std.mem.indexOf(u8, output, "icmp sle") != null);
    try testing.expect(std.mem.indexOf(u8, output, "icmp sgt") != null);
    try testing.expect(std.mem.indexOf(u8, output, "icmp sge") != null);
}

// ============================================================================
// Bitwise Operation Tests
// ============================================================================

test "IR: bitwise operations" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = Module.init(allocator, "test");
    var builder = Builder.init(allocator, &module);

    _ = try builder.beginFunction("bits", &.{ "a", "b" }, &.{ .i32_, .i32_ }, .i32_);

    const func = builder.getCurrentFunction().?;
    const a = func.params[0].value;
    const b = func.params[1].value;

    const and_val = try builder.buildAnd(a, b);
    const or_val = try builder.buildOr(and_val, a);
    const xor_val = try builder.buildXor(or_val, b);
    const shl_val = try builder.buildShl(xor_val, a);
    const ashr_val = try builder.buildAShr(shl_val, b);
    const lshr_val = try builder.buildLShr(ashr_val, a);

    try builder.buildRet(lshr_val);
    builder.endFunction();

    const output = try printer.moduleToString(allocator, &module);

    try testing.expect(std.mem.indexOf(u8, output, "and %") != null);
    try testing.expect(std.mem.indexOf(u8, output, "or %") != null);
    try testing.expect(std.mem.indexOf(u8, output, "xor %") != null);
    try testing.expect(std.mem.indexOf(u8, output, "shl %") != null);
    try testing.expect(std.mem.indexOf(u8, output, "ashr %") != null);
    try testing.expect(std.mem.indexOf(u8, output, "lshr %") != null);
}

// ============================================================================
// Memory Operation Tests
// ============================================================================

test "IR: stack allocation and load/store" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = Module.init(allocator, "test");
    var builder = Builder.init(allocator, &module);

    _ = try builder.beginFunction("stack_test", &.{}, &.{}, .i32_);

    // Allocate
    const ptr = try builder.buildAlloca(.i32_, "local");

    // Store
    const val = try builder.constI32(100);
    try builder.buildStore(val, ptr);

    // Load
    const loaded = try builder.buildLoad(.i32_, ptr);

    // Return
    try builder.buildRet(loaded);
    builder.endFunction();

    const output = try printer.moduleToString(allocator, &module);

    try testing.expect(std.mem.indexOf(u8, output, "alloca i32") != null);
    try testing.expect(std.mem.indexOf(u8, output, "store %") != null);
    try testing.expect(std.mem.indexOf(u8, output, "load i32") != null);
}

// ============================================================================
// Ownership Operation Tests
// ============================================================================

test "IR: ownership operations" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = Module.init(allocator, "test");
    var builder = Builder.init(allocator, &module);

    _ = try builder.beginFunction("ownership_test", &.{"x"}, &.{.i32_}, .i32_);

    const func = builder.getCurrentFunction().?;
    const x = func.params[0].value;

    // Copy (since i32 is Copy)
    const copied = try builder.buildCopy(x);

    // Move
    const moved = try builder.buildMove(copied, "moved_value");

    // Borrow
    const borrowed = try builder.buildBorrow(moved);
    _ = borrowed;

    // Drop
    try builder.buildDrop(moved, .i32_);

    try builder.buildRet(x);
    builder.endFunction();

    const output = try printer.moduleToString(allocator, &module);

    try testing.expect(std.mem.indexOf(u8, output, "copy %") != null);
    try testing.expect(std.mem.indexOf(u8, output, "move %") != null);
    try testing.expect(std.mem.indexOf(u8, output, "borrow %") != null);
    try testing.expect(std.mem.indexOf(u8, output, "drop i32") != null);
}

// ============================================================================
// Function Call Tests
// ============================================================================

test "IR: function call" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = Module.init(allocator, "test");
    var builder = Builder.init(allocator, &module);

    // Helper function
    _ = try builder.beginFunction("helper", &.{"x"}, &.{.i32_}, .i32_);
    const func1 = builder.getCurrentFunction().?;
    try builder.buildRet(func1.params[0].value);
    builder.endFunction();

    // Main function that calls helper
    _ = try builder.beginFunction("main", &.{}, &.{}, .i32_);
    const arg = try builder.constI32(42);
    const result = try builder.buildCall("helper", &.{arg}, .i32_);
    try builder.buildRet(result);
    builder.endFunction();

    const output = try printer.moduleToString(allocator, &module);

    try testing.expect(std.mem.indexOf(u8, output, "call @helper") != null);
}

// ============================================================================
// Optional Type Tests
// ============================================================================

test "IR: optional operations" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = Module.init(allocator, "test");
    var builder = Builder.init(allocator, &module);

    _ = try builder.beginFunction("optional_test", &.{"x"}, &.{.i32_}, .bool_);

    const func = builder.getCurrentFunction().?;
    const x = func.params[0].value;

    // Wrap in Some
    const some_x = try builder.buildSome(x);

    // Create None
    const none_val = try builder.buildNone(.i32_);
    _ = none_val;

    // Check is_some
    const is_some = try builder.buildIsSome(some_x);

    try builder.buildRet(is_some);
    builder.endFunction();

    const output = try printer.moduleToString(allocator, &module);

    try testing.expect(std.mem.indexOf(u8, output, "some %") != null);
    try testing.expect(std.mem.indexOf(u8, output, "none ?i32") != null);
    try testing.expect(std.mem.indexOf(u8, output, "is_some %") != null);
}

// ============================================================================
// Module Structure Tests
// ============================================================================

test "IR: multiple functions in module" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var module = Module.init(allocator, "multi_func");
    var builder = Builder.init(allocator, &module);

    // Function 1
    _ = try builder.beginFunction("func1", &.{}, &.{}, .i32_);
    const one = try builder.constI32(1);
    try builder.buildRet(one);
    builder.endFunction();

    // Function 2
    _ = try builder.beginFunction("func2", &.{}, &.{}, .i32_);
    const two = try builder.constI32(2);
    try builder.buildRet(two);
    builder.endFunction();

    // Function 3
    _ = try builder.beginFunction("func3", &.{}, &.{}, .i32_);
    const three = try builder.constI32(3);
    try builder.buildRet(three);
    builder.endFunction();

    try testing.expectEqual(@as(usize, 3), module.functions.items.len);

    const output = try printer.moduleToString(allocator, &module);

    try testing.expect(std.mem.indexOf(u8, output, "@func1") != null);
    try testing.expect(std.mem.indexOf(u8, output, "@func2") != null);
    try testing.expect(std.mem.indexOf(u8, output, "@func3") != null);
}
