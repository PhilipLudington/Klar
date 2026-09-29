//! GC reachability tests for the bytecode VM (Bugs 15, 16, 17).
//!
//! Each test builds its program as a hand-written chunk so the VM runs it
//! through `interpret`, the same loop `klar run` uses, with nothing from the
//! parser or checker in the way.

const std = @import("std");
const testing = std.testing;
const vm_mod = @import("vm.zig");
const VM = vm_mod.VM;
const chunk_mod = @import("chunk.zig");
const Function = chunk_mod.Function;
const OpCode = @import("bytecode.zig").OpCode;
const gc_mod = @import("gc.zig");
const ObjHeader = gc_mod.ObjHeader;
const vm_value = @import("vm_value.zig");
const Value = vm_value.Value;
const ObjString = vm_value.ObjString;
const ObjArray = vm_value.ObjArray;

/// True when `header` is still on the GC's object list (i.e. not swept).
/// Compares pointers only, so it is safe to call after a sweep.
fn gcHolds(vm: *VM, header: *ObjHeader) bool {
    var obj = vm.gc.objects;
    while (obj) |o| : (obj = o.next) {
        if (o == header) return true;
    }
    return false;
}

/// True when some live GC string has exactly these bytes.
fn gcHoldsString(vm: *VM, chars: []const u8) bool {
    var obj = vm.gc.objects;
    while (obj) |o| : (obj = o.next) {
        if (o.obj_type != .string) continue;
        const s: *ObjString = @alignCast(@fieldParentPtr("header", o));
        if (std.mem.eql(u8, s.chars, chars)) return true;
    }
    return false;
}

fn emitInvoke(func: *Function, name: []const u8, arg_count: u8) !void {
    const idx = try func.chunk.addConstant(.{ .string = name });
    try func.chunk.writeOp(.op_invoke, 1);
    try func.chunk.writeByte(@truncate(idx >> 8), 1);
    try func.chunk.writeByte(@truncate(idx), 1);
    try func.chunk.writeByte(arg_count, 1);
}

/// `("  x" + "yz").trim()` — the receiver is a temporary string that exists
/// only on the VM stack, and `trim` pops it before allocating its result.
fn buildTrimProgram(func: *Function) !void {
    try func.chunk.writeConstant(.{ .string = "  x" }, 1);
    try func.chunk.writeConstant(.{ .string = "yz" }, 1);
    try func.chunk.writeOp(.op_concat, 1);
    try emitInvoke(func, "trim", 0);
    try func.chunk.writeOp(.op_return, 1);
}

/// `"g" + "h"` computed and discarded, then one more allocation (the first
/// push of the constant "k"), then `return 1`. "gh" is garbage from the moment
/// `op_pop` runs, and the "k" allocation is the one whose collection must find
/// it dead.
fn buildGarbageProgram(func: *Function) !void {
    try func.chunk.writeConstant(.{ .string = "g" }, 1);
    try func.chunk.writeConstant(.{ .string = "h" }, 1);
    try func.chunk.writeOp(.op_concat, 1);
    try func.chunk.writeOp(.op_pop, 1);
    try func.chunk.writeConstant(.{ .string = "k" }, 1);
    try func.chunk.writeOp(.op_pop, 1);
    try func.chunk.writeConstant(.{ .int = 1 }, 1);
    try func.chunk.writeOp(.op_return, 1);
}

test "GC: an object allocated under stress survives the allocation of its own payload (Bug 15)" {
    var vm = try VM.init(testing.allocator);
    defer vm.deinit();
    try vm.setup();
    vm.gc.stress_gc = true;

    // ObjString.createGC is allocObject followed by allocBytes for the chars;
    // nothing roots the object in between.
    const s = try ObjString.createGC(&vm.gc, "half-built payload");

    try testing.expect(gcHolds(&vm, &s.header));
    try testing.expectEqualStrings("half-built payload", s.chars);
}

test "GC: a finished but unrooted object survives the next object allocation under stress" {
    var vm = try VM.init(testing.allocator);
    defer vm.deinit();
    try vm.setup();
    vm.gc.stress_gc = true;

    // Two allocations inside one VM operation (a closure then its upvalue,
    // an array then an Optional wrapping it): the first object is complete
    // but held only in a Zig local when the second allocObject runs.
    const first = try ObjString.createGC(&vm.gc, "held in a local");
    const second = try ObjString.createGC(&vm.gc, "allocated next");

    try testing.expect(gcHolds(&vm, &first.header));
    try testing.expect(gcHolds(&vm, &second.header));
    try testing.expectEqualStrings("held in a local", first.chars);
}

test "GC: a completed Future's payload is marked, so its array survives a collection (Bug 16)" {
    var vm = try VM.init(testing.allocator);
    defer vm.deinit();
    try vm.setup();

    const arr = try ObjArray.createGC(&vm.gc, &.{Value.fromInt(7)});
    const payload = try testing.allocator.create(Value);
    defer testing.allocator.destroy(payload);
    payload.* = .{ .array = arr };

    // The Future is the only root that reaches the array.
    vm.stack[0] = .{ .future = .{ .task_id = 1, .state = .completed, .value = payload } };
    vm.stack_top = 1;

    vm.gc.collectGarbage();

    try testing.expect(gcHolds(&vm, &arr.header));
    try testing.expectEqual(@as(usize, 1), arr.items.len);
}

test "GC: a string method's popped receiver survives a collection triggered by the method's own allocation (Bug 17)" {
    var func = Function.init(testing.allocator, "<script>", 0);
    defer func.deinit();
    try buildTrimProgram(&func);

    // Dry run: measure the heap with no collection, so the real run can set
    // the threshold to trip on trim's allocation and nowhere earlier.
    var dry = try VM.init(testing.allocator);
    const dry_bytes = blk: {
        defer dry.deinit();
        try dry.setup();
        const r = try dry.interpret(&func);
        try testing.expectEqualStrings("xyz", r.string.chars);
        break :blk dry.gc.bytes_allocated;
    };
    // Heap just before trim allocates its result ("xyz": object + 3 bytes).
    const before_trim = dry_bytes - @sizeOf(ObjString) - 3;

    var vm = try VM.init(testing.allocator);
    defer vm.deinit();
    try vm.setup();
    vm.gc.next_gc = before_trim - 1;

    const result = try vm.interpret(&func);
    try testing.expect(result == .string);
    try testing.expectEqualStrings("xyz", result.string.chars);
}

test "GC: stress mode runs a program that allocates in every instruction and gets the right answer" {
    var func = Function.init(testing.allocator, "<script>", 0);
    defer func.deinit();
    try buildTrimProgram(&func);

    var vm = try VM.init(testing.allocator);
    defer vm.deinit();
    try vm.setup();
    vm.gc.stress_gc = true;

    const result = try vm.interpret(&func);
    try testing.expect(result == .string);
    try testing.expectEqualStrings("xyz", result.string.chars);
}

test "GC: stress mode collects garbage at the instruction boundary after the allocation" {
    var func = Function.init(testing.allocator, "<script>", 0);
    defer func.deinit();
    try buildGarbageProgram(&func);

    var vm = try VM.init(testing.allocator);
    defer vm.deinit();
    try vm.setup();
    vm.gc.stress_gc = true;

    const result = try vm.interpret(&func);
    try testing.expect(result.eql(Value.fromInt(1)));
    try testing.expect(!gcHoldsString(&vm, "gh"));
}

test "GC: crossing the heap threshold without stress still collects at the next instruction boundary" {
    var func = Function.init(testing.allocator, "<script>", 0);
    defer func.deinit();
    try buildGarbageProgram(&func);

    // Dry run: measure the heap with no collection. A collection resets the
    // threshold to twice the live heap, so the threshold must be crossed by
    // the allocation after "gh" dies (the constant "k": object + 1 byte), not
    // by an earlier one.
    var dry = try VM.init(testing.allocator);
    const dry_bytes = blk: {
        defer dry.deinit();
        try dry.setup();
        _ = try dry.interpret(&func);
        try testing.expect(gcHoldsString(&dry, "gh"));
        break :blk dry.gc.bytes_allocated;
    };
    const before_k = dry_bytes - @sizeOf(ObjString) - 1;

    var vm = try VM.init(testing.allocator);
    defer vm.deinit();
    try vm.setup();
    vm.gc.next_gc = before_k;

    const result = try vm.interpret(&func);
    try testing.expect(result.eql(Value.fromInt(1)));
    try testing.expect(!gcHoldsString(&vm, "gh"));
}
