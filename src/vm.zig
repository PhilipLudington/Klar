const std = @import("std");
const Allocator = std.mem.Allocator;
const bytecode = @import("bytecode.zig");
const OpCode = bytecode.OpCode;
const TypeTag = bytecode.TypeTag;
const chunk_mod = @import("chunk.zig");
const Chunk = chunk_mod.Chunk;
const Constant = chunk_mod.Constant;
const Function = chunk_mod.Function;
const vm_value = @import("vm_value.zig");
const Value = vm_value.Value;
const ObjString = vm_value.ObjString;
const ObjArray = vm_value.ObjArray;
const ObjTuple = vm_value.ObjTuple;
const ObjStruct = vm_value.ObjStruct;
const ObjClosure = vm_value.ObjClosure;
const ObjUpvalue = vm_value.ObjUpvalue;
const ObjOptional = vm_value.ObjOptional;
const ObjRange = vm_value.ObjRange;
const ObjNative = vm_value.ObjNative;
const RuntimeError = vm_value.RuntimeError;
const SourceLocation = vm_value.SourceLocation;
const StackFrame = vm_value.StackFrame;
const ErrorContext = vm_value.ErrorContext;
const vm_builtins = @import("vm_builtins.zig");
const gc_mod = @import("gc.zig");
const GC = gc_mod.GC;

// Zig 0.15 IO helpers
fn getStdOut() std.fs.File {
    return .{ .handle = std.posix.STDOUT_FILENO };
}

// ============================================================================
// VM Configuration
// ============================================================================

/// Maximum stack size (number of Value slots).
const stack_max: usize = 256 * 64;

/// Maximum call stack depth.
const frames_max: usize = 64;

// ============================================================================
// Call Frame
// ============================================================================

/// Represents an active function invocation.
const CallFrame = struct {
    /// The closure being executed.
    closure: *ObjClosure,

    /// Instruction pointer into the function's bytecode.
    ip: usize,

    /// Base pointer into the value stack for this frame's slots.
    slots_base: usize,
};

// ============================================================================
// Virtual Machine
// ============================================================================

/// Stack-based bytecode virtual machine for Klar.
pub const VM = struct {
    allocator: Allocator,

    /// Garbage collector for heap-allocated objects.
    gc: GC,

    /// Value stack.
    stack: [stack_max]Value,

    /// Stack top pointer (points to next free slot).
    stack_top: usize,

    /// Call stack.
    frames: [frames_max]CallFrame,

    /// Current frame count.
    frame_count: usize,

    /// Global variables.
    globals: std.StringHashMapUnmanaged(Value),

    /// Linked list of open upvalues (for closure implementation).
    open_upvalues: ?*ObjUpvalue,

    /// Output file for print statements.
    stdout: std.fs.File,

    /// Debug mode flag.
    debug_trace: bool,

    /// Enable GC debug logging.
    debug_gc: bool,

    /// Enable GC stress testing (collect on every allocation).
    stress_gc: bool,

    /// Last error context (for detailed error reporting).
    last_error: ?ErrorContext,

    // -------------------------------------------------------------------------
    // Initialization
    // -------------------------------------------------------------------------

    pub fn init(allocator: Allocator) !VM {
        var vm = VM{
            .allocator = allocator,
            .gc = GC.init(allocator),
            .stack = undefined,
            .stack_top = 0,
            .frames = undefined,
            .frame_count = 0,
            .globals = .{},
            .open_upvalues = null,
            .stdout = getStdOut(),
            .debug_trace = false,
            .debug_gc = false,
            .stress_gc = false,
            .last_error = null,
        };

        // Initialize stack with void values
        for (&vm.stack) |*slot| {
            slot.* = .void_;
        }

        return vm;
    }

    /// Set up GC roots and register built-in functions.
    /// Must be called after VM is in its final memory location.
    pub fn setup(self: *VM) !void {
        // Set up GC roots
        // Note: We cast the frames pointer because vm.CallFrame and gc.GC.CallFrame
        // have identical layouts but are distinct types in Zig.
        self.gc.setRoots(
            &self.stack,
            &self.stack_top,
            &self.globals,
            &self.open_upvalues,
            @ptrCast(&self.frames),
            &self.frame_count,
        );

        // Register built-in functions using GC allocation
        try self.registerBuiltins();
    }

    /// Register all built-in native functions.
    fn registerBuiltins(self: *VM) !void {
        for (vm_builtins.builtins) |builtin| {
            const native = try ObjNative.createGC(
                &self.gc,
                builtin.name,
                builtin.arity,
                builtin.function,
            );
            try self.globals.put(self.allocator, builtin.name, .{ .native = native });
        }
    }

    pub fn deinit(self: *VM) void {
        self.gc.deinit();
        self.globals.deinit(self.allocator);
    }

    // -------------------------------------------------------------------------
    // Error Reporting
    // -------------------------------------------------------------------------

    /// Get the current source location based on the active call frame.
    pub fn getCurrentLocation(self: *VM) SourceLocation {
        if (self.frame_count == 0) {
            return .{
                .function_name = "<unknown>",
                .line = 0,
                .offset = 0,
            };
        }

        const frame = &self.frames[self.frame_count - 1];
        const func = frame.closure.function;
        // Subtract 1 from ip because it's already advanced past the current instruction
        const offset = if (frame.ip > 0) frame.ip - 1 else 0;
        const line = func.chunk.getLine(offset);

        return .{
            .function_name = func.name,
            .line = line,
            .offset = offset,
        };
    }

    /// Build a full error context with stack trace.
    pub fn buildErrorContext(self: *VM, err: RuntimeError) ErrorContext {
        var ctx = ErrorContext.init(err, self.getCurrentLocation());

        // Add stack trace (walk up call stack)
        var i = self.frame_count;
        while (i > 0) {
            i -= 1;
            const frame = &self.frames[i];
            const func = frame.closure.function;
            const offset = if (frame.ip > 0) frame.ip - 1 else 0;
            const line = func.chunk.getLine(offset);

            ctx.addFrame(.{
                .location = .{
                    .function_name = func.name,
                    .line = line,
                    .offset = offset,
                },
            });
        }

        return ctx;
    }

    /// Record an error with full context.
    pub fn recordError(self: *VM, err: RuntimeError) RuntimeError {
        self.last_error = self.buildErrorContext(err);
        return err;
    }

    /// Record an error with a custom message.
    pub fn recordErrorWithMessage(self: *VM, err: RuntimeError, message: []const u8) RuntimeError {
        var ctx = self.buildErrorContext(err);
        ctx.message = message;
        self.last_error = ctx;
        return err;
    }

    /// Get the last error context (if any).
    pub fn getLastError(self: *VM) ?*const ErrorContext {
        if (self.last_error) |*ctx| {
            return ctx;
        }
        return null;
    }

    /// Print the last error to stderr.
    pub fn printLastError(self: *VM) void {
        if (self.last_error) |ctx| {
            var buf: [2048]u8 = undefined;
            const msg = ctx.format(&buf);
            const stderr = std.io.getStdErr();
            stderr.writeAll(msg) catch {};
        }
    }

    // -------------------------------------------------------------------------
    // Execution
    // -------------------------------------------------------------------------

    /// Interpret compiled bytecode.
    pub fn interpret(self: *VM, function: *Function) !Value {
        // Create a closure for the script function (via GC).
        const closure = try ObjClosure.createGC(&self.gc, function);

        // Push the closure onto the stack.
        try self.push(.{ .closure = closure });

        // Call the script.
        try self.callValue(.{ .closure = closure }, 0);

        // Run the bytecode.
        return self.run();
    }

    /// Main execution loop.
    fn run(self: *VM) !Value {
        while (true) {
            if (self.debug_trace) {
                self.printStack();
                _ = self.disassembleInstruction();
            }

            const instruction = self.readByte();
            const op: OpCode = @enumFromInt(instruction);

            switch (op) {
                // -------------------------------------------------------------
                // Constants and immediates
                // -------------------------------------------------------------
                .op_const => {
                    const index = self.readU16();
                    const constant = self.currentChunk().getConstant(index);
                    try self.pushConstant(constant);
                },
                .op_const_long => {
                    const index = self.readU24();
                    const constant = self.currentChunk().getConstant(index);
                    try self.pushConstant(constant);
                },
                .op_true => try self.push(Value.true_val),
                .op_false => try self.push(Value.false_val),
                .op_void => try self.push(Value.void_val),
                .op_int_neg1 => try self.push(Value.fromInt(-1)),
                .op_int_0 => try self.push(Value.fromInt(0)),
                .op_int_1 => try self.push(Value.fromInt(1)),
                .op_int_2 => try self.push(Value.fromInt(2)),
                .op_int_3 => try self.push(Value.fromInt(3)),
                .op_int_4 => try self.push(Value.fromInt(4)),
                .op_int_5 => try self.push(Value.fromInt(5)),
                .op_none => {
                    const opt = try ObjOptional.createNone(self.allocator);
                    try self.push(.{ .optional = opt });
                },

                // -------------------------------------------------------------
                // Stack manipulation
                // -------------------------------------------------------------
                .op_pop => _ = try self.pop(),
                .op_dup => {
                    const value = try self.peek(0);
                    try self.push(value);
                },
                .op_swap => {
                    const a = try self.pop();
                    const b = try self.pop();
                    try self.push(a);
                    try self.push(b);
                },

                // -------------------------------------------------------------
                // Arithmetic
                // -------------------------------------------------------------
                .op_add => try self.binaryOp(.add),
                .op_sub => try self.binaryOp(.sub),
                .op_mul => try self.binaryOp(.mul),
                .op_div => try self.binaryOp(.div),
                .op_mod => try self.binaryOp(.mod),
                .op_add_wrap => try self.binaryOp(.add_wrap),
                .op_sub_wrap => try self.binaryOp(.sub_wrap),
                .op_mul_wrap => try self.binaryOp(.mul_wrap),
                .op_add_sat => try self.binaryOp(.add_sat),
                .op_sub_sat => try self.binaryOp(.sub_sat),
                .op_mul_sat => try self.binaryOp(.mul_sat),
                .op_neg => {
                    const value = try self.pop();
                    switch (value) {
                        .int => |i| try self.push(Value.fromInt(-i)),
                        .float => |f| try self.push(Value.fromFloat(-f)),
                        else => return RuntimeError.TypeError,
                    }
                },

                // -------------------------------------------------------------
                // Comparison
                // -------------------------------------------------------------
                .op_eq => {
                    const b = try self.pop();
                    const a = try self.pop();
                    try self.push(Value.fromBool(a.eql(b)));
                },
                .op_ne => {
                    const b = try self.pop();
                    const a = try self.pop();
                    try self.push(Value.fromBool(!a.eql(b)));
                },
                .op_lt => try self.comparisonOp(.lt),
                .op_gt => try self.comparisonOp(.gt),
                .op_le => try self.comparisonOp(.le),
                .op_ge => try self.comparisonOp(.ge),

                // -------------------------------------------------------------
                // Logical
                // -------------------------------------------------------------
                .op_not => {
                    const value = try self.pop();
                    try self.push(Value.fromBool(value.isFalsey()));
                },
                .op_and => {
                    const offset = self.readU16();
                    const value = try self.peek(0);
                    if (value.isFalsey()) {
                        self.currentFrame().ip += offset;
                    } else {
                        _ = try self.pop();
                    }
                },
                .op_or => {
                    const offset = self.readU16();
                    const value = try self.peek(0);
                    if (value.isTruthy()) {
                        self.currentFrame().ip += offset;
                    } else {
                        _ = try self.pop();
                    }
                },

                // -------------------------------------------------------------
                // Bitwise
                // -------------------------------------------------------------
                .op_bit_and => try self.bitwiseOp(.bit_and),
                .op_bit_or => try self.bitwiseOp(.bit_or),
                .op_bit_xor => try self.bitwiseOp(.bit_xor),
                .op_bit_not => {
                    const value = try self.pop();
                    if (value != .int) return RuntimeError.TypeError;
                    try self.push(Value.fromInt(~value.int));
                },
                .op_shl => try self.bitwiseOp(.shl),
                .op_shr => try self.bitwiseOp(.shr),

                // -------------------------------------------------------------
                // Control flow
                // -------------------------------------------------------------
                .op_jump => {
                    const offset = self.readU16();
                    self.currentFrame().ip += offset;
                },
                .op_loop => {
                    const offset = self.readU16();
                    self.currentFrame().ip -= offset;
                },
                .op_jump_if_false => {
                    const offset = self.readU16();
                    const value = try self.pop();
                    if (value.isFalsey()) {
                        self.currentFrame().ip += offset;
                    }
                },
                .op_jump_if_false_no_pop => {
                    const offset = self.readU16();
                    const value = try self.peek(0);
                    if (value.isFalsey()) {
                        self.currentFrame().ip += offset;
                    }
                },
                .op_jump_if_true => {
                    const offset = self.readU16();
                    const value = try self.pop();
                    if (value.isTruthy()) {
                        self.currentFrame().ip += offset;
                    }
                },

                // -------------------------------------------------------------
                // Variable access
                // -------------------------------------------------------------
                .op_get_local => {
                    const slot = self.readByte();
                    const base = self.currentFrame().slots_base;
                    try self.push(self.stack[base + slot]);
                },
                .op_set_local => {
                    const slot = self.readByte();
                    const base = self.currentFrame().slots_base;
                    self.stack[base + slot] = try self.peek(0);
                },
                .op_get_local_long => {
                    const slot = self.readU16();
                    const base = self.currentFrame().slots_base;
                    try self.push(self.stack[base + slot]);
                },
                .op_set_local_long => {
                    const slot = self.readU16();
                    const base = self.currentFrame().slots_base;
                    self.stack[base + slot] = try self.peek(0);
                },
                .op_get_global => {
                    const name_idx = self.readU16();
                    const name = self.currentChunk().getConstant(name_idx).string;
                    if (self.globals.get(name)) |value| {
                        try self.push(value);
                    } else {
                        return RuntimeError.UndefinedVariable;
                    }
                },
                .op_set_global => {
                    const name_idx = self.readU16();
                    const name = self.currentChunk().getConstant(name_idx).string;
                    if (!self.globals.contains(name)) {
                        return RuntimeError.UndefinedVariable;
                    }
                    self.globals.put(self.allocator, name, try self.peek(0)) catch {
                        return RuntimeError.OutOfMemory;
                    };
                },
                .op_define_global => {
                    const name_idx = self.readU16();
                    const name = self.currentChunk().getConstant(name_idx).string;
                    const value = try self.pop();
                    self.globals.put(self.allocator, name, value) catch {
                        return RuntimeError.OutOfMemory;
                    };
                },
                .op_get_upvalue => {
                    const slot = self.readByte();
                    const upvalue = self.currentFrame().closure.upvalues[slot];
                    try self.push(upvalue.location.*);
                },
                .op_set_upvalue => {
                    const slot = self.readByte();
                    const upvalue = self.currentFrame().closure.upvalues[slot];
                    upvalue.location.* = try self.peek(0);
                },
                .op_close_upvalue => {
                    try self.closeUpvalues(self.stack_top - 1);
                    _ = try self.pop();
                },

                // -------------------------------------------------------------
                // Function calls
                // -------------------------------------------------------------
                .op_call => {
                    const arg_count = self.readByte();
                    const callee = try self.peek(arg_count);
                    try self.callValue(callee, arg_count);
                },
                .op_tail_call => {
                    // TODO: Implement tail call optimization
                    const arg_count = self.readByte();
                    const callee = try self.peek(arg_count);
                    try self.callValue(callee, arg_count);
                },
                .op_return => {
                    const result = try self.pop();

                    // Close any open upvalues in the returning function's scope.
                    try self.closeUpvalues(self.currentFrame().slots_base);

                    self.frame_count -= 1;
                    if (self.frame_count == 0) {
                        _ = try self.pop(); // Pop the script closure
                        return result;
                    }

                    self.stack_top = self.frames[self.frame_count].slots_base;
                    try self.push(result);
                },
                .op_return_void => {
                    // Close any open upvalues.
                    try self.closeUpvalues(self.currentFrame().slots_base);

                    self.frame_count -= 1;
                    if (self.frame_count == 0) {
                        _ = try self.pop();
                        return .void_;
                    }

                    self.stack_top = self.frames[self.frame_count].slots_base;
                    try self.push(.void_);
                },
                .op_closure => {
                    const func_idx = self.readU16();
                    const func = self.currentChunk().getConstant(func_idx).function;
                    const closure = try ObjClosure.createGC(&self.gc, func);

                    // Read upvalue descriptors.
                    for (0..func.upvalue_count) |i| {
                        const is_local = self.readByte() == 1;
                        const index = self.readByte();
                        if (is_local) {
                            closure.upvalues[i] = try self.captureUpvalue(
                                &self.stack[self.currentFrame().slots_base + index],
                            );
                        } else {
                            closure.upvalues[i] = self.currentFrame().closure.upvalues[index];
                        }
                    }

                    try self.push(.{ .closure = closure });
                },

                // -------------------------------------------------------------
                // Composite types
                // -------------------------------------------------------------
                .op_array => {
                    const count = self.readU16();
                    var items = std.ArrayListUnmanaged(Value){};
                    defer items.deinit(self.allocator);

                    // Pop items in reverse order.
                    var i: usize = 0;
                    while (i < count) : (i += 1) {
                        try items.insert(self.allocator, 0, try self.pop());
                    }

                    const arr = try ObjArray.createGC(&self.gc, items.items);
                    try self.push(.{ .array = arr });
                },
                .op_tuple => {
                    const count = self.readByte();
                    var items = std.ArrayListUnmanaged(Value){};
                    defer items.deinit(self.allocator);

                    var i: u8 = 0;
                    while (i < count) : (i += 1) {
                        try items.insert(self.allocator, 0, try self.pop());
                    }

                    const tuple = try ObjTuple.createGC(&self.gc, items.items);
                    try self.push(.{ .tuple = tuple });
                },
                .op_struct => {
                    const desc_idx = self.readU16();
                    const descriptor = self.currentChunk().getConstant(desc_idx).struct_type;
                    const struc = try ObjStruct.createGC(&self.gc, descriptor.name);

                    // Pop values in reverse order (last field first on stack).
                    const field_count = descriptor.field_names.len;
                    var i: usize = 0;
                    while (i < field_count) : (i += 1) {
                        const value = try self.pop();
                        // Use actual field name from descriptor (fields are compiled in order,
                        // so we pop in reverse: last field first).
                        const field_name = descriptor.field_names[field_count - 1 - i];
                        try struc.setField(self.allocator, field_name, value);
                    }

                    try self.push(.{ .struct_ = struc });
                },
                .op_get_index => {
                    const index_val = try self.pop();
                    const container = try self.pop();

                    switch (container) {
                        .array => |arr| {
                            const idx = index_val.asInt() orelse return RuntimeError.TypeError;
                            if (idx < 0 or idx >= arr.items.len) {
                                return RuntimeError.IndexOutOfBounds;
                            }
                            try self.push(arr.items[@intCast(idx)]);
                        },
                        .tuple => |tup| {
                            const idx = index_val.asInt() orelse return RuntimeError.TypeError;
                            if (idx < 0 or idx >= tup.items.len) {
                                return RuntimeError.IndexOutOfBounds;
                            }
                            try self.push(tup.items[@intCast(idx)]);
                        },
                        .string => |str| {
                            const idx = index_val.asInt() orelse return RuntimeError.TypeError;
                            if (idx < 0 or idx >= str.chars.len) {
                                return RuntimeError.IndexOutOfBounds;
                            }
                            // Return character as integer
                            try self.push(Value.fromInt(str.chars[@intCast(idx)]));
                        },
                        else => return RuntimeError.TypeError,
                    }
                },
                .op_set_index => {
                    // Stack order from compiler: [value, index, container] (bottom to top)
                    // Pop order: container (top), index, value (bottom)
                    const container = try self.pop();
                    const index_val = try self.pop();
                    const value = try self.pop();

                    switch (container) {
                        .array => |arr| {
                            const idx = index_val.asInt() orelse return RuntimeError.TypeError;
                            if (idx < 0 or idx >= arr.items.len) {
                                return RuntimeError.IndexOutOfBounds;
                            }
                            arr.items[@intCast(idx)] = value;
                            try self.push(value);
                        },
                        else => return RuntimeError.TypeError,
                    }
                },
                .op_get_field => {
                    const name_idx = self.readU16();
                    const name = self.currentChunk().getConstant(name_idx).string;
                    const obj = try self.pop();

                    switch (obj) {
                        .struct_ => |struc| {
                            if (struc.getField(name)) |value| {
                                try self.push(value);
                            } else {
                                return RuntimeError.UndefinedField;
                            }
                        },
                        else => return RuntimeError.TypeError,
                    }
                },
                .op_set_field => {
                    const name_idx = self.readU16();
                    const name = self.currentChunk().getConstant(name_idx).string;
                    // Stack after op_swap: [object, value] with value at TOS
                    const value = try self.pop(); // Pop value from TOS
                    const obj = try self.peek(0); // Peek at object

                    switch (obj) {
                        .struct_ => |struc| {
                            try struc.setField(self.allocator, name, value);
                        },
                        else => return RuntimeError.TypeError,
                    }
                },
                .op_array_empty => {
                    const capacity = self.readU16();
                    _ = capacity; // TODO: Use capacity hint
                    const arr = try ObjArray.createGC(&self.gc, &.{});
                    try self.push(.{ .array = arr });
                },
                .op_array_push => {
                    const value = try self.pop();
                    const arr_val = try self.pop();
                    if (arr_val != .array) return RuntimeError.TypeError;

                    // Create a new array with the value appended
                    var new_items = std.ArrayListUnmanaged(Value){};
                    defer new_items.deinit(self.allocator);
                    try new_items.appendSlice(self.allocator, arr_val.array.items);
                    try new_items.append(self.allocator, value);

                    const new_arr = try ObjArray.createGC(&self.gc, new_items.items);
                    try self.push(.{ .array = new_arr });
                },

                // -------------------------------------------------------------
                // Optional operations
                // -------------------------------------------------------------
                .op_some => {
                    const value = try self.pop();
                    const opt = try ObjOptional.createSome(self.allocator, value);
                    try self.push(.{ .optional = opt });
                },
                .op_unwrap => {
                    const opt_val = try self.pop();
                    if (opt_val != .optional) return RuntimeError.TypeError;
                    if (opt_val.optional.value) |v| {
                        try self.push(v.*);
                    } else {
                        return RuntimeError.NullUnwrap;
                    }
                },
                .op_unwrap_or => {
                    const default = try self.pop();
                    const opt_val = try self.pop();
                    if (opt_val != .optional) {
                        try self.push(opt_val);
                    } else if (opt_val.optional.value) |v| {
                        try self.push(v.*);
                    } else {
                        try self.push(default);
                    }
                },
                .op_is_none => {
                    const value = try self.pop();
                    try self.push(Value.fromBool(value.isNone()));
                },
                .op_is_some => {
                    const value = try self.pop();
                    try self.push(Value.fromBool(!value.isNone()));
                },
                .op_null_coalesce => {
                    const rhs = try self.pop();
                    const lhs = try self.pop();
                    if (lhs.isNone()) {
                        try self.push(rhs);
                    } else {
                        try self.push(lhs);
                    }
                },

                // -------------------------------------------------------------
                // Type operations
                // -------------------------------------------------------------
                .op_cast => {
                    const target_tag: TypeTag = @enumFromInt(self.readByte());
                    const value = try self.pop();
                    const result = try self.castValue(value, target_tag, false);
                    try self.push(result);
                },
                .op_cast_trunc => {
                    const target_tag: TypeTag = @enumFromInt(self.readByte());
                    const value = try self.pop();
                    const result = try self.castValue(value, target_tag, true);
                    try self.push(result);
                },
                .op_type_of => {
                    const value = try self.pop();
                    const type_name = self.getTypeName(value);
                    const str = try ObjString.createGC(&self.gc, type_name);
                    try self.push(.{ .string = str });
                },
                .op_is_type => {
                    // TODO: Implement type checking
                    _ = self.readU16();
                    try self.push(Value.true_val);
                },

                // -------------------------------------------------------------
                // String operations
                // -------------------------------------------------------------
                .op_concat => {
                    const b = try self.pop();
                    const a = try self.pop();

                    if (a == .string and b == .string) {
                        const new_str = try std.mem.concat(self.allocator, u8, &.{
                            a.string.chars,
                            b.string.chars,
                        });
                        const str_obj = try ObjString.createGC(&self.gc, new_str);
                        self.allocator.free(new_str);
                        try self.push(.{ .string = str_obj });
                    } else {
                        return RuntimeError.TypeError;
                    }
                },
                .op_string_build => {
                    const count = self.readByte();
                    var parts = std.ArrayListUnmanaged([]const u8){};
                    defer parts.deinit(self.allocator);

                    // Track which strings need to be freed (allocated by valueToString)
                    var to_free = std.ArrayListUnmanaged([]const u8){};
                    defer {
                        for (to_free.items) |s| {
                            self.allocator.free(s);
                        }
                        to_free.deinit(self.allocator);
                    }

                    // Pop parts in reverse order
                    var i: u8 = 0;
                    while (i < count) : (i += 1) {
                        const part = try self.pop();
                        const str_info = try self.valueToStringAlloc(part);
                        try parts.insert(self.allocator, 0, str_info.str);
                        if (str_info.needs_free) {
                            try to_free.append(self.allocator, str_info.str);
                        }
                    }

                    const result = try std.mem.concat(self.allocator, u8, parts.items);
                    const str_obj = try ObjString.createGC(&self.gc, result);
                    self.allocator.free(result);
                    try self.push(.{ .string = str_obj });
                },

                // -------------------------------------------------------------
                // Method calls
                // -------------------------------------------------------------
                .op_invoke => {
                    const name_high = self.readByte();
                    const name_low = self.readByte();
                    const name_idx = (@as(u16, name_high) << 8) | name_low;
                    const arg_count = self.readByte();
                    const method_name = self.currentChunk().getConstant(name_idx).string;
                    try self.invokeMethod(method_name, arg_count);
                },
                .op_invoke_super => {
                    // TODO: Implement super method invocation
                    _ = self.readByte();
                    _ = self.readByte();
                    _ = self.readByte();
                },

                // -------------------------------------------------------------
                // Pattern matching
                // -------------------------------------------------------------
                .op_match_literal => {
                    const const_idx = self.readU16();
                    const expected = self.currentChunk().getConstant(const_idx);
                    const value = try self.peek(0);
                    const expected_val = try self.constantToValue(expected);
                    try self.push(Value.fromBool(value.eql(expected_val)));
                },
                .op_match_variant => {
                    const name_idx = self.readU16();
                    const variant_name = self.currentChunk().getConstant(name_idx).string;
                    _ = variant_name;
                    // TODO: Implement variant matching
                    try self.push(Value.true_val);
                },
                .op_match_bind => {
                    // Duplicate TOS for binding
                    const value = try self.peek(0);
                    try self.push(value);
                },

                // -------------------------------------------------------------
                // Range operations
                // -------------------------------------------------------------
                .op_range_inclusive => {
                    const end_val = try self.pop();
                    const start_val = try self.pop();
                    const start = start_val.asInt() orelse return RuntimeError.TypeError;
                    const end = end_val.asInt() orelse return RuntimeError.TypeError;
                    const range = try ObjRange.createGC(&self.gc, start, end, true);
                    try self.push(.{ .range = range });
                },
                .op_range_exclusive => {
                    const end_val = try self.pop();
                    const start_val = try self.pop();
                    const start = start_val.asInt() orelse return RuntimeError.TypeError;
                    const end = end_val.asInt() orelse return RuntimeError.TypeError;
                    const range = try ObjRange.createGC(&self.gc, start, end, false);
                    try self.push(.{ .range = range });
                },
                .op_in_range => {
                    const range_val = try self.pop();
                    const value_val = try self.pop();
                    if (range_val != .range) return RuntimeError.TypeError;
                    const value = value_val.asInt() orelse return RuntimeError.TypeError;
                    const range = range_val.range;
                    const in = if (range.inclusive)
                        value >= range.start and value <= range.end
                    else
                        value >= range.start and value < range.end;
                    try self.push(Value.fromBool(in));
                },

                // -------------------------------------------------------------
                // Iteration
                // -------------------------------------------------------------
                .op_get_iter => {
                    const iterable = try self.pop();
                    switch (iterable) {
                        .range => try self.push(iterable), // Range is its own iterator
                        .array => {
                            // Create a range iterator over indices
                            const range = try ObjRange.create(
                                self.allocator,
                                0,
                                @intCast(iterable.array.items.len),
                                false,
                            );
                            try self.push(.{ .range = range });
                            try self.push(iterable); // Push array for indexing
                        },
                        else => return RuntimeError.TypeError,
                    }
                },
                .op_iter_next => {
                    const iter = try self.peek(0);
                    switch (iter) {
                        .range => |range| {
                            if (range.next()) |val| {
                                try self.push(Value.fromInt(val));
                                try self.push(Value.true_val);
                            } else {
                                try self.push(Value.false_val);
                            }
                        },
                        else => return RuntimeError.TypeError,
                    }
                },

                // -------------------------------------------------------------
                // Debug/introspection
                // -------------------------------------------------------------
                .op_print => {
                    const value = try self.pop();
                    try self.printValue(value);
                },
                .op_println => {
                    const value = try self.pop();
                    try self.printValue(value);
                    self.write("\n");
                },
                .op_assert => {
                    const value = try self.pop();
                    if (value.isFalsey()) {
                        return RuntimeError.AssertionFailed;
                    }
                },
                .op_panic => {
                    const msg = try self.pop();
                    _ = msg;
                    return RuntimeError.Panic;
                },
                .op_nop => {},
            }
        }
    }

    // -------------------------------------------------------------------------
    // Stack operations
    // -------------------------------------------------------------------------

    fn push(self: *VM, value: Value) !void {
        if (self.stack_top >= stack_max) {
            return RuntimeError.StackOverflow;
        }
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    fn pop(self: *VM) !Value {
        if (self.stack_top == 0) {
            return RuntimeError.StackUnderflow;
        }
        self.stack_top -= 1;
        return self.stack[self.stack_top];
    }

    fn peek(self: *VM, distance: usize) !Value {
        if (self.stack_top <= distance) {
            return RuntimeError.StackUnderflow;
        }
        return self.stack[self.stack_top - 1 - distance];
    }

    // -------------------------------------------------------------------------
    // Bytecode reading
    // -------------------------------------------------------------------------

    fn currentFrame(self: *VM) *CallFrame {
        return &self.frames[self.frame_count - 1];
    }

    fn currentChunk(self: *VM) *Chunk {
        return &self.currentFrame().closure.function.chunk;
    }

    fn readByte(self: *VM) u8 {
        const frame = self.currentFrame();
        const byte = frame.closure.function.chunk.code.items[frame.ip];
        frame.ip += 1;
        return byte;
    }

    fn readU16(self: *VM) u16 {
        const high = self.readByte();
        const low = self.readByte();
        return (@as(u16, high) << 8) | low;
    }

    fn readU24(self: *VM) u24 {
        const b1 = self.readByte();
        const b2 = self.readByte();
        const b3 = self.readByte();
        return (@as(u24, b1) << 16) | (@as(u24, b2) << 8) | b3;
    }

    // -------------------------------------------------------------------------
    // Function calls
    // -------------------------------------------------------------------------

    fn callValue(self: *VM, callee: Value, arg_count: u8) !void {
        switch (callee) {
            .closure => |closure| try self.call(closure, arg_count),
            .native => |native| {
                if (arg_count != native.arity) {
                    return RuntimeError.WrongArity;
                }
                const args = self.stack[self.stack_top - arg_count .. self.stack_top];
                const result = try native.function(self.allocator, args);
                self.stack_top -= arg_count + 1; // Pop args and callee
                try self.push(result);
            },
            else => return RuntimeError.NotCallable,
        }
    }

    fn call(self: *VM, closure: *ObjClosure, arg_count: u8) !void {
        if (arg_count != closure.function.arity) {
            return RuntimeError.WrongArity;
        }
        if (self.frame_count >= frames_max) {
            return RuntimeError.StackOverflow;
        }

        self.frames[self.frame_count] = .{
            .closure = closure,
            .ip = 0,
            .slots_base = self.stack_top - arg_count - 1,
        };
        self.frame_count += 1;
    }

    /// Call the main() function after module execution.
    /// This is called externally after interpret() completes.
    pub fn callMain(self: *VM, closure: *ObjClosure) !Value {
        // Push the closure as the callee
        try self.push(.{ .closure = closure });

        // Call it with 0 arguments
        try self.callValue(.{ .closure = closure }, 0);

        // Run until completion
        return self.run();
    }

    // -------------------------------------------------------------------------
    // Upvalue handling
    // -------------------------------------------------------------------------

    fn captureUpvalue(self: *VM, local: *Value) !*ObjUpvalue {
        // Check if we already have an upvalue for this slot.
        var prev_upvalue: ?*ObjUpvalue = null;
        var upvalue = self.open_upvalues;

        while (upvalue) |uv| {
            if (@intFromPtr(uv.location) <= @intFromPtr(local)) {
                break;
            }
            prev_upvalue = uv;
            upvalue = uv.next;
        }

        if (upvalue) |uv| {
            if (uv.location == local) {
                return uv;
            }
        }

        // Create a new upvalue.
        const created = try ObjUpvalue.createGC(&self.gc, local);
        created.next = upvalue;

        if (prev_upvalue) |prev| {
            prev.next = created;
        } else {
            self.open_upvalues = created;
        }

        return created;
    }

    fn closeUpvalues(self: *VM, last_slot: usize) !void {
        while (self.open_upvalues) |upvalue| {
            if (@intFromPtr(upvalue.location) < @intFromPtr(&self.stack[last_slot])) {
                break;
            }
            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed;
            self.open_upvalues = upvalue.next;
        }
    }

    // -------------------------------------------------------------------------
    // Arithmetic helpers
    // -------------------------------------------------------------------------

    const BinaryOp = enum {
        add,
        sub,
        mul,
        div,
        mod,
        add_wrap,
        sub_wrap,
        mul_wrap,
        add_sat,
        sub_sat,
        mul_sat,
    };

    fn binaryOp(self: *VM, op: BinaryOp) !void {
        const b = try self.pop();
        const a = try self.pop();

        // Handle string concatenation for add.
        if (op == .add and a == .string and b == .string) {
            const new_str = try std.mem.concat(self.allocator, u8, &.{
                a.string.chars,
                b.string.chars,
            });
            const str_obj = try ObjString.createGC(&self.gc, new_str);
            self.allocator.free(new_str);
            try self.push(.{ .string = str_obj });
            return;
        }

        // Numeric operations.
        if (a == .int and b == .int) {
            const result = switch (op) {
                .add => std.math.add(i128, a.int, b.int) catch return RuntimeError.IntegerOverflow,
                .sub => std.math.sub(i128, a.int, b.int) catch return RuntimeError.IntegerOverflow,
                .mul => std.math.mul(i128, a.int, b.int) catch return RuntimeError.IntegerOverflow,
                .div => blk: {
                    if (b.int == 0) return RuntimeError.DivisionByZero;
                    break :blk @divTrunc(a.int, b.int);
                },
                .mod => blk: {
                    if (b.int == 0) return RuntimeError.DivisionByZero;
                    break :blk @rem(a.int, b.int);
                },
                .add_wrap => a.int +% b.int,
                .sub_wrap => a.int -% b.int,
                .mul_wrap => a.int *% b.int,
                .add_sat => a.int +| b.int,
                .sub_sat => a.int -| b.int,
                .mul_sat => a.int *| b.int,
            };
            try self.push(Value.fromInt(result));
            return;
        }

        // Float operations.
        const fa = a.asFloat() orelse return RuntimeError.TypeError;
        const fb = b.asFloat() orelse return RuntimeError.TypeError;

        const result: f64 = switch (op) {
            .add, .add_wrap, .add_sat => fa + fb,
            .sub, .sub_wrap, .sub_sat => fa - fb,
            .mul, .mul_wrap, .mul_sat => fa * fb,
            .div => blk: {
                if (fb == 0) return RuntimeError.DivisionByZero;
                break :blk fa / fb;
            },
            .mod => blk: {
                if (fb == 0) return RuntimeError.DivisionByZero;
                break :blk @rem(fa, fb);
            },
        };
        try self.push(Value.fromFloat(result));
    }

    const ComparisonOp = enum { lt, gt, le, ge };

    fn comparisonOp(self: *VM, op: ComparisonOp) !void {
        const b = try self.pop();
        const a = try self.pop();

        const order = a.compare(b) orelse return RuntimeError.TypeError;

        const result = switch (op) {
            .lt => order == .lt,
            .gt => order == .gt,
            .le => order != .gt,
            .ge => order != .lt,
        };
        try self.push(Value.fromBool(result));
    }

    const BitwiseOp = enum { bit_and, bit_or, bit_xor, shl, shr };

    fn bitwiseOp(self: *VM, op: BitwiseOp) !void {
        const b = try self.pop();
        const a = try self.pop();

        if (a != .int or b != .int) return RuntimeError.TypeError;

        const result: i128 = switch (op) {
            .bit_and => a.int & b.int,
            .bit_or => a.int | b.int,
            .bit_xor => a.int ^ b.int,
            .shl => blk: {
                const shift: u7 = @intCast(@max(0, @min(127, b.int)));
                break :blk a.int << shift;
            },
            .shr => blk: {
                const shift: u7 = @intCast(@max(0, @min(127, b.int)));
                break :blk a.int >> shift;
            },
        };
        try self.push(Value.fromInt(result));
    }

    // -------------------------------------------------------------------------
    // Type casting
    // -------------------------------------------------------------------------

    fn castValue(self: *VM, value: Value, target: TypeTag, truncating: bool) !Value {
        _ = self;
        _ = truncating; // VM uses i128 for all ints, so truncation semantics are handled at native level

        switch (target) {
            .i8_, .i16_, .i32_, .i64_, .i128_, .isize_, .u8_, .u16_, .u32_, .u64_, .u128_, .usize_ => {
                const int = value.asInt() orelse return RuntimeError.InvalidCast;
                return Value.fromInt(int);
            },
            .f32_, .f64_ => {
                const float = value.asFloat() orelse return RuntimeError.InvalidCast;
                return Value.fromFloat(float);
            },
            .bool_ => {
                return Value.fromBool(value.isTruthy());
            },
            .char_ => {
                if (value == .char_) return value;
                if (value == .int) {
                    const code: u21 = @intCast(@max(0, @min(0x10FFFF, value.int)));
                    return Value.fromChar(code);
                }
                return RuntimeError.InvalidCast;
            },
            .string_ => {
                return RuntimeError.InvalidCast; // Handled separately
            },
        }
    }

    // -------------------------------------------------------------------------
    // Method invocation
    // -------------------------------------------------------------------------

    fn invokeMethod(self: *VM, method_name: []const u8, arg_count: u8) !void {
        const receiver = try self.peek(arg_count);

        // Handle universal methods first.
        if (std.mem.eql(u8, method_name, "to_string")) {
            if (arg_count != 0) return RuntimeError.WrongArity;
            _ = try self.pop(); // Pop receiver
            const str_data = try self.valueToString(receiver);
            defer self.allocator.free(str_data);
            const str = try ObjString.createGC(&self.gc, str_data);
            try self.push(.{ .string = str });
            return;
        }

        // Built-in methods based on receiver type.
        switch (receiver) {
            .string => |str| {
                try self.invokeStringMethod(str, method_name, arg_count);
            },
            .array => |arr| {
                try self.invokeArrayMethod(arr, method_name, arg_count);
            },
            .optional => |opt| {
                try self.invokeOptionalMethod(opt, method_name, arg_count);
            },
            .int => |i| {
                try self.invokeIntegerMethod(i, method_name, arg_count);
            },
            else => return RuntimeError.TypeError,
        }
    }

    fn invokeStringMethod(self: *VM, str: *ObjString, method: []const u8, arg_count: u8) !void {
        if (std.mem.eql(u8, method, "len")) {
            if (arg_count != 0) return RuntimeError.WrongArity;
            _ = try self.pop(); // Pop receiver
            try self.push(Value.fromInt(@intCast(str.chars.len)));
        } else if (std.mem.eql(u8, method, "is_empty")) {
            if (arg_count != 0) return RuntimeError.WrongArity;
            _ = try self.pop();
            try self.push(Value.fromBool(str.chars.len == 0));
        } else if (std.mem.eql(u8, method, "contains")) {
            if (arg_count != 1) return RuntimeError.WrongArity;
            const needle = try self.pop();
            _ = try self.pop(); // Pop receiver
            if (needle != .string) return RuntimeError.TypeError;
            const found = std.mem.indexOf(u8, str.chars, needle.string.chars) != null;
            try self.push(Value.fromBool(found));
        } else if (std.mem.eql(u8, method, "starts_with")) {
            if (arg_count != 1) return RuntimeError.WrongArity;
            const prefix = try self.pop();
            _ = try self.pop();
            if (prefix != .string) return RuntimeError.TypeError;
            const result = std.mem.startsWith(u8, str.chars, prefix.string.chars);
            try self.push(Value.fromBool(result));
        } else if (std.mem.eql(u8, method, "ends_with")) {
            if (arg_count != 1) return RuntimeError.WrongArity;
            const suffix = try self.pop();
            _ = try self.pop();
            if (suffix != .string) return RuntimeError.TypeError;
            const result = std.mem.endsWith(u8, str.chars, suffix.string.chars);
            try self.push(Value.fromBool(result));
        } else if (std.mem.eql(u8, method, "trim")) {
            if (arg_count != 0) return RuntimeError.WrongArity;
            _ = try self.pop();
            const trimmed = std.mem.trim(u8, str.chars, " \t\n\r");
            const new_str = try ObjString.createGC(&self.gc, trimmed);
            try self.push(.{ .string = new_str });
        } else if (std.mem.eql(u8, method, "to_uppercase")) {
            if (arg_count != 0) return RuntimeError.WrongArity;
            _ = try self.pop();
            const upper = self.allocator.alloc(u8, str.chars.len) catch return RuntimeError.OutOfMemory;
            for (str.chars, 0..) |c, i| {
                upper[i] = std.ascii.toUpper(c);
            }
            const new_str = try ObjString.createGC(&self.gc, upper);
            self.allocator.free(upper);
            try self.push(.{ .string = new_str });
        } else if (std.mem.eql(u8, method, "to_lowercase")) {
            if (arg_count != 0) return RuntimeError.WrongArity;
            _ = try self.pop();
            const lower = self.allocator.alloc(u8, str.chars.len) catch return RuntimeError.OutOfMemory;
            for (str.chars, 0..) |c, i| {
                lower[i] = std.ascii.toLower(c);
            }
            const new_str = try ObjString.createGC(&self.gc, lower);
            self.allocator.free(lower);
            try self.push(.{ .string = new_str });
        } else if (std.mem.eql(u8, method, "chars")) {
            // Return array of characters
            if (arg_count != 0) return RuntimeError.WrongArity;
            _ = try self.pop();
            var chars = std.ArrayListUnmanaged(Value){};
            defer chars.deinit(self.allocator);
            var i: usize = 0;
            while (i < str.chars.len) {
                const len = std.unicode.utf8ByteSequenceLength(str.chars[i]) catch 1;
                if (i + len <= str.chars.len) {
                    const codepoint = std.unicode.utf8Decode(str.chars[i..][0..len]) catch str.chars[i];
                    try chars.append(self.allocator, Value.fromChar(codepoint));
                }
                i += len;
            }
            const arr = try ObjArray.createGC(&self.gc, chars.items);
            try self.push(.{ .array = arr });
        } else if (std.mem.eql(u8, method, "bytes")) {
            // Return array of bytes
            if (arg_count != 0) return RuntimeError.WrongArity;
            _ = try self.pop();
            var bytes = self.allocator.alloc(Value, str.chars.len) catch return RuntimeError.OutOfMemory;
            defer self.allocator.free(bytes);
            for (str.chars, 0..) |b, i| {
                bytes[i] = Value.fromInt(b);
            }
            const arr = try ObjArray.createGC(&self.gc, bytes);
            try self.push(.{ .array = arr });
        } else {
            return RuntimeError.UndefinedField;
        }
    }

    fn invokeArrayMethod(self: *VM, arr: *ObjArray, method: []const u8, arg_count: u8) !void {
        if (std.mem.eql(u8, method, "len")) {
            if (arg_count != 0) return RuntimeError.WrongArity;
            _ = try self.pop();
            try self.push(Value.fromInt(@intCast(arr.items.len)));
        } else if (std.mem.eql(u8, method, "is_empty")) {
            if (arg_count != 0) return RuntimeError.WrongArity;
            _ = try self.pop();
            try self.push(Value.fromBool(arr.items.len == 0));
        } else if (std.mem.eql(u8, method, "first")) {
            if (arg_count != 0) return RuntimeError.WrongArity;
            _ = try self.pop();
            if (arr.items.len > 0) {
                const opt = try ObjOptional.createSome(self.allocator, arr.items[0]);
                try self.push(.{ .optional = opt });
            } else {
                const opt = try ObjOptional.createNone(self.allocator);
                try self.push(.{ .optional = opt });
            }
        } else if (std.mem.eql(u8, method, "last")) {
            if (arg_count != 0) return RuntimeError.WrongArity;
            _ = try self.pop();
            if (arr.items.len > 0) {
                const opt = try ObjOptional.createSome(self.allocator, arr.items[arr.items.len - 1]);
                try self.push(.{ .optional = opt });
            } else {
                const opt = try ObjOptional.createNone(self.allocator);
                try self.push(.{ .optional = opt });
            }
        } else if (std.mem.eql(u8, method, "get")) {
            if (arg_count != 1) return RuntimeError.WrongArity;
            const idx_val = try self.pop();
            _ = try self.pop();
            const idx = idx_val.asInt() orelse return RuntimeError.TypeError;
            if (idx < 0 or idx >= arr.items.len) {
                const opt = try ObjOptional.createNone(self.allocator);
                try self.push(.{ .optional = opt });
            } else {
                const opt = try ObjOptional.createSome(self.allocator, arr.items[@intCast(idx)]);
                try self.push(.{ .optional = opt });
            }
        } else if (std.mem.eql(u8, method, "contains")) {
            if (arg_count != 1) return RuntimeError.WrongArity;
            const needle = try self.pop();
            _ = try self.pop();
            var found = false;
            for (arr.items) |item| {
                if (item.eql(needle)) {
                    found = true;
                    break;
                }
            }
            try self.push(Value.fromBool(found));
        } else {
            return RuntimeError.UndefinedField;
        }
    }

    fn invokeOptionalMethod(self: *VM, opt: *ObjOptional, method: []const u8, arg_count: u8) !void {
        if (std.mem.eql(u8, method, "is_some")) {
            if (arg_count != 0) return RuntimeError.WrongArity;
            _ = try self.pop();
            try self.push(Value.fromBool(opt.value != null));
        } else if (std.mem.eql(u8, method, "is_none")) {
            if (arg_count != 0) return RuntimeError.WrongArity;
            _ = try self.pop();
            try self.push(Value.fromBool(opt.value == null));
        } else if (std.mem.eql(u8, method, "unwrap")) {
            if (arg_count != 0) return RuntimeError.WrongArity;
            _ = try self.pop();
            if (opt.value) |v| {
                try self.push(v.*);
            } else {
                return RuntimeError.NullUnwrap;
            }
        } else if (std.mem.eql(u8, method, "unwrap_or")) {
            if (arg_count != 1) return RuntimeError.WrongArity;
            const default = try self.pop();
            _ = try self.pop();
            if (opt.value) |v| {
                try self.push(v.*);
            } else {
                try self.push(default);
            }
        } else if (std.mem.eql(u8, method, "expect")) {
            if (arg_count != 1) return RuntimeError.WrongArity;
            const msg = try self.pop();
            _ = try self.pop();
            if (opt.value) |v| {
                try self.push(v.*);
            } else {
                // Print error message and panic
                const stderr = std.fs.File{ .handle = std.posix.STDERR_FILENO };
                stderr.writeAll("expect failed: ") catch {};
                if (msg == .string) {
                    stderr.writeAll(msg.string.chars) catch {};
                }
                stderr.writeAll("\n") catch {};
                return RuntimeError.Panic;
            }
        } else {
            return RuntimeError.UndefinedField;
        }
    }

    fn invokeIntegerMethod(self: *VM, value: i128, method: []const u8, arg_count: u8) !void {
        if (std.mem.eql(u8, method, "abs")) {
            if (arg_count != 0) return RuntimeError.WrongArity;
            _ = try self.pop();
            try self.push(Value.fromInt(if (value < 0) -value else value));
        } else {
            return RuntimeError.UndefinedField;
        }
    }

    // -------------------------------------------------------------------------
    // Value conversion helpers
    // -------------------------------------------------------------------------

    fn pushConstant(self: *VM, constant: Constant) !void {
        const value = try self.constantToValue(constant);
        try self.push(value);
    }

    fn constantToValue(self: *VM, constant: Constant) !Value {
        return switch (constant) {
            .int => |i| Value.fromInt(i),
            .float => |f| Value.fromFloat(f),
            .string => |s| blk: {
                const str = try ObjString.createGC(&self.gc, s);
                break :blk .{ .string = str };
            },
            .char => |c| Value.fromChar(c),
            .function => |f| blk: {
                const closure = try ObjClosure.createGC(&self.gc, f);
                break :blk .{ .closure = closure };
            },
            .struct_type, .enum_variant => .void_,
        };
    }

    fn valueToString(self: *VM, value: Value) ![]const u8 {
        const info = try self.valueToStringAlloc(value);
        return info.str;
    }

    const StringAllocInfo = struct {
        str: []const u8,
        needs_free: bool,
    };

    fn valueToStringAlloc(self: *VM, value: Value) !StringAllocInfo {
        return switch (value) {
            .int => |i| blk: {
                const str = try std.fmt.allocPrint(self.allocator, "{d}", .{i});
                break :blk .{ .str = str, .needs_free = true };
            },
            .float => |f| blk: {
                const str = try std.fmt.allocPrint(self.allocator, "{d}", .{f});
                break :blk .{ .str = str, .needs_free = true };
            },
            .bool_ => |b| .{ .str = if (b) "true" else "false", .needs_free = false },
            .char_ => |c| blk: {
                var buf = try self.allocator.alloc(u8, 4);
                const len = std.unicode.utf8Encode(c, buf[0..4]) catch 0;
                break :blk .{ .str = buf[0..len], .needs_free = true };
            },
            .void_ => .{ .str = "void", .needs_free = false },
            .string => |s| .{ .str = s.chars, .needs_free = false },
            else => .{ .str = "<object>", .needs_free = false },
        };
    }

    fn getTypeName(_: *VM, value: Value) []const u8 {
        return switch (value) {
            .int => "int",
            .float => "float",
            .bool_ => "bool",
            .char_ => "char",
            .void_ => "void",
            .string => "string",
            .array => "array",
            .tuple => "tuple",
            .struct_ => "struct",
            .closure, .function, .native => "function",
            .optional => "optional",
            .range => "range",
            .upvalue => "upvalue",
        };
    }

    // -------------------------------------------------------------------------
    // Debug/output
    // -------------------------------------------------------------------------

    fn write(self: *VM, data: []const u8) void {
        self.stdout.writeAll(data) catch {};
    }

    fn printValue(self: *VM, value: Value) !void {
        switch (value) {
            .int => |i| {
                var buf: [64]u8 = undefined;
                const formatted = std.fmt.bufPrint(&buf, "{d}", .{i}) catch "?";
                self.write(formatted);
            },
            .float => |f| {
                var buf: [64]u8 = undefined;
                const formatted = std.fmt.bufPrint(&buf, "{d}", .{f}) catch "?";
                self.write(formatted);
            },
            .bool_ => |b| self.write(if (b) "true" else "false"),
            .char_ => |c| {
                var buf: [4]u8 = undefined;
                const len = std.unicode.utf8Encode(c, &buf) catch 0;
                self.write(buf[0..len]);
            },
            .void_ => self.write("void"),
            .string => |s| self.write(s.chars),
            .array => |arr| {
                self.write("[");
                for (arr.items, 0..) |item, i| {
                    if (i > 0) self.write(", ");
                    try self.printValue(item);
                }
                self.write("]");
            },
            .tuple => |tup| {
                self.write("(");
                for (tup.items, 0..) |item, i| {
                    if (i > 0) self.write(", ");
                    try self.printValue(item);
                }
                self.write(")");
            },
            .optional => |opt| {
                if (opt.value) |v| {
                    self.write("Some(");
                    try self.printValue(v.*);
                    self.write(")");
                } else {
                    self.write("None");
                }
            },
            .closure => self.write("<closure>"),
            .function => self.write("<function>"),
            .native => |n| {
                var buf: [64]u8 = undefined;
                const formatted = std.fmt.bufPrint(&buf, "<native {s}>", .{n.name}) catch "<native>";
                self.write(formatted);
            },
            .struct_ => |s| {
                self.write(s.type_name);
                self.write(" { ... }");
            },
            .range => |r| {
                var buf: [128]u8 = undefined;
                const sep = if (r.inclusive) "..=" else "..";
                const formatted = std.fmt.bufPrint(&buf, "{d}{s}{d}", .{ r.start, sep, r.end }) catch "?..?";
                self.write(formatted);
            },
            .upvalue => self.write("<upvalue>"),
        }
    }

    fn printStack(self: *VM) void {
        std.debug.print("          ", .{});
        for (self.stack[0..self.stack_top]) |slot| {
            std.debug.print("[ ", .{});
            self.debugPrintValue(slot);
            std.debug.print(" ]", .{});
        }
        std.debug.print("\n", .{});
    }

    fn debugPrintValue(_: *VM, value: Value) void {
        switch (value) {
            .int => |i| std.debug.print("{d}", .{i}),
            .float => |f| std.debug.print("{any}", .{f}),
            .bool_ => |b| std.debug.print("{any}", .{b}),
            .void_ => std.debug.print("void", .{}),
            .string => |s| std.debug.print("\"{s}\"", .{s.chars}),
            .closure => std.debug.print("<closure>", .{}),
            else => std.debug.print("<obj>", .{}),
        }
    }

    fn disassembleInstruction(self: *VM) usize {
        const frame = self.currentFrame();
        const chunk = &frame.closure.function.chunk;
        const offset = frame.ip;

        if (offset >= chunk.code.items.len) return offset;

        const instruction = chunk.code.items[offset];
        const op: OpCode = @enumFromInt(instruction);
        std.debug.print("{d:0>4} {s}\n", .{ offset, @tagName(op) });

        return offset + 1 + op.operandBytes();
    }
};

// ============================================================================
// Tests
// ============================================================================

test "VM init/deinit" {
    const testing = std.testing;

    var vm = try VM.init(testing.allocator);
    defer vm.deinit();

    // Set up GC roots and register builtins
    try vm.setup();

    try testing.expectEqual(@as(usize, 0), vm.stack_top);
    try testing.expectEqual(@as(usize, 0), vm.frame_count);

    // Check that built-ins are registered
    try testing.expect(vm.globals.contains("print"));
    try testing.expect(vm.globals.contains("println"));
    try testing.expect(vm.globals.contains("assert"));
    try testing.expect(vm.globals.contains("len"));
}

test "VM stack operations" {
    const testing = std.testing;

    var vm = try VM.init(testing.allocator);
    defer vm.deinit();

    // Push values.
    try vm.push(Value.fromInt(42));
    try vm.push(Value.fromInt(10));
    try testing.expectEqual(@as(usize, 2), vm.stack_top);

    // Peek.
    const top = try vm.peek(0);
    try testing.expect(top.eql(Value.fromInt(10)));

    // Pop.
    const popped = try vm.pop();
    try testing.expect(popped.eql(Value.fromInt(10)));
    try testing.expectEqual(@as(usize, 1), vm.stack_top);
}

test "VM arithmetic" {
    const testing = std.testing;

    var vm = try VM.init(testing.allocator);
    defer vm.deinit();

    // Test addition.
    try vm.push(Value.fromInt(5));
    try vm.push(Value.fromInt(3));
    try vm.binaryOp(.add);
    const result = try vm.pop();
    try testing.expect(result.eql(Value.fromInt(8)));

    // Test division by zero.
    try vm.push(Value.fromInt(10));
    try vm.push(Value.fromInt(0));
    try testing.expectError(RuntimeError.DivisionByZero, vm.binaryOp(.div));
}

test "VM comparison" {
    const testing = std.testing;

    var vm = try VM.init(testing.allocator);
    defer vm.deinit();

    // Test less than.
    try vm.push(Value.fromInt(3));
    try vm.push(Value.fromInt(5));
    try vm.comparisonOp(.lt);
    const result = try vm.pop();
    try testing.expect(result.eql(Value.true_val));

    // Test greater than.
    try vm.push(Value.fromInt(5));
    try vm.push(Value.fromInt(3));
    try vm.comparisonOp(.gt);
    const gt_result = try vm.pop();
    try testing.expect(gt_result.eql(Value.true_val));
}
