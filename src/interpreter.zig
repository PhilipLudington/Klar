const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("ast.zig");
const types = @import("types.zig");
const values = @import("values.zig");
const Value = values.Value;
const Integer = values.Integer;
const Float = values.Float;
const Environment = values.Environment;
const RuntimeError = values.RuntimeError;
const ValueBuilder = values.ValueBuilder;

// Zig 0.15 IO helper
fn getStdOut() std.fs.File {
    return .{ .handle = std.posix.STDOUT_FILENO };
}

// ============================================================================
// Interpreter
// ============================================================================

pub const Interpreter = struct {
    allocator: Allocator,
    global_env: *Environment,
    current_env: *Environment,
    builder: ValueBuilder,

    // Arena for runtime string allocations (freed all at once on deinit)
    string_arena: std.heap.ArenaAllocator,

    // Control flow state
    return_value: ?Value,
    break_value: ?Value,
    is_breaking: bool,
    is_continuing: bool,

    // Output capture for testing
    output: std.ArrayListUnmanaged(u8),

    // Track allocated functions for cleanup
    allocated_functions: std.ArrayListUnmanaged(*values.FunctionValue),

    pub fn init(allocator: Allocator) !Interpreter {
        const global_env = try allocator.create(Environment);
        global_env.* = Environment.init(allocator, null);

        var interp = Interpreter{
            .allocator = allocator,
            .global_env = global_env,
            .current_env = global_env,
            .builder = ValueBuilder.init(allocator),
            .string_arena = std.heap.ArenaAllocator.init(allocator),
            .return_value = null,
            .break_value = null,
            .is_breaking = false,
            .is_continuing = false,
            .output = .{},
            .allocated_functions = .{},
        };

        try interp.initBuiltins();
        return interp;
    }

    pub fn deinit(self: *Interpreter) void {
        // Free the string arena (frees all runtime-allocated strings at once)
        self.string_arena.deinit();

        self.output.deinit(self.allocator);

        // Free user-defined functions
        for (self.allocated_functions.items) |func| {
            self.allocator.free(func.params);
            self.allocator.destroy(func);
        }
        self.allocated_functions.deinit(self.allocator);

        // Free builtin functions
        if (self.global_env.get("print")) |v| {
            if (v == .builtin) self.allocator.destroy(v.builtin);
        }
        if (self.global_env.get("println")) |v| {
            if (v == .builtin) self.allocator.destroy(v.builtin);
        }
        if (self.global_env.get("assert")) |v| {
            if (v == .builtin) self.allocator.destroy(v.builtin);
        }
        if (self.global_env.get("assert_eq")) |v| {
            if (v == .builtin) self.allocator.destroy(v.builtin);
        }
        if (self.global_env.get("panic")) |v| {
            if (v == .builtin) self.allocator.destroy(v.builtin);
        }
        if (self.global_env.get("len")) |v| {
            if (v == .builtin) self.allocator.destroy(v.builtin);
        }
        if (self.global_env.get("type_of")) |v| {
            if (v == .builtin) self.allocator.destroy(v.builtin);
        }
        self.global_env.deinit();
        self.allocator.destroy(self.global_env);
    }

    /// Get the allocator for runtime string allocations (uses arena)
    fn stringAllocator(self: *Interpreter) Allocator {
        return self.string_arena.allocator();
    }

    fn initBuiltins(self: *Interpreter) !void {
        // Register built-in functions
        const print_fn = try self.allocator.create(values.BuiltinFunction);
        print_fn.* = .{ .name = "print", .func = &builtinPrint };
        try self.global_env.define("print", .{ .builtin = print_fn }, false);

        const println_fn = try self.allocator.create(values.BuiltinFunction);
        println_fn.* = .{ .name = "println", .func = &builtinPrintln };
        try self.global_env.define("println", .{ .builtin = println_fn }, false);

        const assert_fn = try self.allocator.create(values.BuiltinFunction);
        assert_fn.* = .{ .name = "assert", .func = &builtinAssert };
        try self.global_env.define("assert", .{ .builtin = assert_fn }, false);

        const assert_eq_fn = try self.allocator.create(values.BuiltinFunction);
        assert_eq_fn.* = .{ .name = "assert_eq", .func = &builtinAssertEq };
        try self.global_env.define("assert_eq", .{ .builtin = assert_eq_fn }, false);

        const panic_fn = try self.allocator.create(values.BuiltinFunction);
        panic_fn.* = .{ .name = "panic", .func = &builtinPanic };
        try self.global_env.define("panic", .{ .builtin = panic_fn }, false);

        const len_fn = try self.allocator.create(values.BuiltinFunction);
        len_fn.* = .{ .name = "len", .func = &builtinLen };
        try self.global_env.define("len", .{ .builtin = len_fn }, false);

        const type_of_fn = try self.allocator.create(values.BuiltinFunction);
        type_of_fn.* = .{ .name = "type_of", .func = &builtinTypeOf };
        try self.global_env.define("type_of", .{ .builtin = type_of_fn }, false);
    }

    // ========================================================================
    // Environment Management
    // ========================================================================

    fn pushEnv(self: *Interpreter) !*Environment {
        const env = try self.allocator.create(Environment);
        env.* = Environment.init(self.allocator, self.current_env);
        self.current_env = env;
        return env;
    }

    fn popEnv(self: *Interpreter) void {
        if (self.current_env.parent) |parent| {
            const old = self.current_env;
            self.current_env = parent;
            old.deinit();
            self.allocator.destroy(old);
        }
    }

    // ========================================================================
    // Expression Evaluation
    // ========================================================================

    pub fn evaluate(self: *Interpreter, expr: ast.Expr) RuntimeError!Value {
        return switch (expr) {
            .literal => |l| self.evalLiteral(l),
            .identifier => |i| self.evalIdentifier(i),
            .binary => |b| self.evalBinary(b),
            .unary => |u| self.evalUnary(u),
            .postfix => |p| self.evalPostfix(p),
            .call => |c| self.evalCall(c),
            .index => |i| self.evalIndex(i),
            .field => |f| self.evalField(f),
            .method_call => |m| self.evalMethodCall(m),
            .block => |b| self.evalBlock(b),
            .closure => |c| self.evalClosure(c),
            .range => |r| self.evalRange(r),
            .struct_literal => |s| self.evalStructLiteral(s),
            .array_literal => |a| self.evalArrayLiteral(a),
            .tuple_literal => |t| self.evalTupleLiteral(t),
            .type_cast => |tc| self.evalTypeCast(tc),
            .grouped => |g| self.evaluate(g.expr),
            .interpolated_string => |is| self.evalInterpolatedString(is),
            .enum_literal => {
                // TODO: Enum literal evaluation for interpreter
                // For now, enum literals are only supported in native compilation
                return error.NotImplemented;
            },
            .comptime_block => |cb| self.evalComptimeBlock(cb),
            .builtin_call => |bc| self.evalBuiltinCall(bc),
        };
    }

    fn evalLiteral(self: *Interpreter, lit: ast.Literal) RuntimeError!Value {
        return switch (lit.kind) {
            .int => |i| self.builder.int(i, .i32_),
            .float => |f| self.builder.f64Val(f),
            .string => |s| self.builder.string(s),
            .char => |c| self.builder.char(c),
            .bool_ => |b| self.builder.boolean(b),
        };
    }

    fn evalInterpolatedString(self: *Interpreter, interp: *ast.InterpolatedString) RuntimeError!Value {
        // Build the result string by concatenating parts
        var result = std.ArrayListUnmanaged(u8){};
        const arena_alloc = self.stringAllocator();

        for (interp.parts) |part| {
            switch (part) {
                .string => |s| {
                    result.appendSlice(arena_alloc, s) catch return RuntimeError.OutOfMemory;
                },
                .expr => |e| {
                    const value = try self.evaluate(e);
                    // For strings, use the raw value without quotes
                    if (value == .string) {
                        result.appendSlice(arena_alloc, value.string) catch return RuntimeError.OutOfMemory;
                    } else {
                        const str = values.valueToString(arena_alloc, value) catch return RuntimeError.OutOfMemory;
                        result.appendSlice(arena_alloc, str) catch return RuntimeError.OutOfMemory;
                    }
                },
            }
        }

        return self.builder.string(result.items);
    }

    fn evalIdentifier(self: *Interpreter, id: ast.Identifier) RuntimeError!Value {
        if (self.current_env.get(id.name)) |value| {
            return value;
        }
        return RuntimeError.UndefinedVariable;
    }

    fn evalBinary(self: *Interpreter, bin: *ast.Binary) RuntimeError!Value {
        // Handle assignment operations first
        switch (bin.op) {
            .assign => {
                const value = try self.evaluate(bin.right);
                try self.assignToExpr(bin.left, value);
                return value;
            },
            .add_assign, .sub_assign, .mul_assign, .div_assign, .mod_assign => {
                const current = try self.evaluate(bin.left);
                const rhs = try self.evaluate(bin.right);
                const new_value = switch (bin.op) {
                    .add_assign => try self.intArithmetic(current, rhs, .add, .trap),
                    .sub_assign => try self.intArithmetic(current, rhs, .sub, .trap),
                    .mul_assign => try self.intArithmetic(current, rhs, .mul, .trap),
                    .div_assign => try self.intArithmetic(current, rhs, .div, .trap),
                    .mod_assign => try self.intArithmetic(current, rhs, .mod, .trap),
                    else => unreachable,
                };
                try self.assignToExpr(bin.left, new_value);
                return new_value;
            },
            else => {},
        }

        // Short-circuit evaluation for logical operators
        switch (bin.op) {
            .and_ => {
                const left = try self.evaluate(bin.left);
                if (!left.isTruthy()) return self.builder.boolean(false);
                const right = try self.evaluate(bin.right);
                return self.builder.boolean(right.isTruthy());
            },
            .or_ => {
                const left = try self.evaluate(bin.left);
                if (left.isTruthy()) return self.builder.boolean(true);
                const right = try self.evaluate(bin.right);
                return self.builder.boolean(right.isTruthy());
            },
            .null_coalesce => {
                const left = try self.evaluate(bin.left);
                if (left == .optional and left.optional.value != null) {
                    return left.optional.value.?.*;
                }
                if (left == .optional and left.optional.value == null) {
                    return self.evaluate(bin.right);
                }
                return left;
            },
            else => {},
        }

        const left = try self.evaluate(bin.left);
        const right = try self.evaluate(bin.right);

        // Arithmetic operations
        switch (bin.op) {
            .add => return self.intArithmetic(left, right, .add, .trap),
            .sub => return self.intArithmetic(left, right, .sub, .trap),
            .mul => return self.intArithmetic(left, right, .mul, .trap),
            .div => return self.intArithmetic(left, right, .div, .trap),
            .mod => return self.intArithmetic(left, right, .mod, .trap),
            .add_wrap => return self.intArithmetic(left, right, .add, .wrap),
            .sub_wrap => return self.intArithmetic(left, right, .sub, .wrap),
            .mul_wrap => return self.intArithmetic(left, right, .mul, .wrap),
            .add_sat => return self.intArithmetic(left, right, .add, .saturate),
            .sub_sat => return self.intArithmetic(left, right, .sub, .saturate),
            .mul_sat => return self.intArithmetic(left, right, .mul, .saturate),

            // Comparison operations
            .eq => return self.builder.boolean(left.eql(right)),
            .not_eq => return self.builder.boolean(!left.eql(right)),
            .lt => return self.compareValues(left, right, .lt),
            .gt => return self.compareValues(left, right, .gt),
            .lt_eq => return self.compareValues(left, right, .lt_eq),
            .gt_eq => return self.compareValues(left, right, .gt_eq),

            // Bitwise operations
            .bit_and => return self.bitwiseOp(left, right, .and_),
            .bit_or => return self.bitwiseOp(left, right, .or_),
            .bit_xor => return self.bitwiseOp(left, right, .xor),
            .shl => return self.bitwiseOp(left, right, .shl),
            .shr => return self.bitwiseOp(left, right, .shr),

            else => return RuntimeError.InvalidOperation,
        }
    }

    fn assignToExpr(self: *Interpreter, expr: ast.Expr, value: Value) RuntimeError!void {
        switch (expr) {
            .identifier => |id| {
                try self.current_env.set(id.name, value);
            },
            .index => |idx| {
                // TODO: implement array index assignment
                _ = idx;
                return RuntimeError.InvalidOperation;
            },
            .field => |fld| {
                // TODO: implement field assignment
                _ = fld;
                return RuntimeError.InvalidOperation;
            },
            else => return RuntimeError.InvalidOperation,
        }
    }

    const ArithOp = enum { add, sub, mul, div, mod };
    const OverflowBehavior = enum { trap, wrap, saturate };

    fn intArithmetic(self: *Interpreter, left: Value, right: Value, op: ArithOp, behavior: OverflowBehavior) RuntimeError!Value {
        // Handle integer arithmetic
        if (left == .int and right == .int) {
            const l = left.int;
            const r = right.int;

            // Division by zero check
            if ((op == .div or op == .mod) and r.value == 0) {
                return RuntimeError.DivisionByZero;
            }

            const result = switch (op) {
                .add => self.checkedAdd(l.value, r.value, l.type_, behavior),
                .sub => self.checkedSub(l.value, r.value, l.type_, behavior),
                .mul => self.checkedMul(l.value, r.value, l.type_, behavior),
                .div => @divTrunc(l.value, r.value),
                .mod => @mod(l.value, r.value),
            };

            return self.builder.int(result orelse return RuntimeError.IntegerOverflow, l.type_);
        }

        // Handle float arithmetic
        if (left == .float and right == .float) {
            const l = left.float.value;
            const r = right.float.value;

            if ((op == .div or op == .mod) and r == 0.0) {
                return RuntimeError.DivisionByZero;
            }

            const result = switch (op) {
                .add => l + r,
                .sub => l - r,
                .mul => l * r,
                .div => l / r,
                .mod => @mod(l, r),
            };

            return self.builder.float(result, left.float.type_);
        }

        // String concatenation
        if (left == .string and right == .string and op == .add) {
            const combined = try std.mem.concat(self.stringAllocator(), u8, &.{ left.string, right.string });
            return self.builder.string(combined);
        }

        return RuntimeError.TypeError;
    }

    fn checkedAdd(self: *Interpreter, a: i128, b: i128, type_: Integer.IntegerType, behavior: OverflowBehavior) ?i128 {
        _ = self;
        const result = a + b;
        const min = type_.minValue();
        const max = type_.maxValue();

        if (result < min or result > max) {
            return switch (behavior) {
                .trap => null,
                .wrap => @mod(result - min, max - min + 1) + min,
                .saturate => if (result < min) min else max,
            };
        }
        return result;
    }

    fn checkedSub(self: *Interpreter, a: i128, b: i128, type_: Integer.IntegerType, behavior: OverflowBehavior) ?i128 {
        _ = self;
        const result = a - b;
        const min = type_.minValue();
        const max = type_.maxValue();

        if (result < min or result > max) {
            return switch (behavior) {
                .trap => null,
                .wrap => @mod(result - min, max - min + 1) + min,
                .saturate => if (result < min) min else max,
            };
        }
        return result;
    }

    fn checkedMul(self: *Interpreter, a: i128, b: i128, type_: Integer.IntegerType, behavior: OverflowBehavior) ?i128 {
        _ = self;
        const result = a * b;
        const min = type_.minValue();
        const max = type_.maxValue();

        if (result < min or result > max) {
            return switch (behavior) {
                .trap => null,
                .wrap => @mod(result - min, max - min + 1) + min,
                .saturate => if (result < min) min else max,
            };
        }
        return result;
    }

    const CompareOp = enum { lt, gt, lt_eq, gt_eq };

    fn compareValues(self: *Interpreter, left: Value, right: Value, op: CompareOp) RuntimeError!Value {
        if (left == .int and right == .int) {
            const result = switch (op) {
                .lt => left.int.value < right.int.value,
                .gt => left.int.value > right.int.value,
                .lt_eq => left.int.value <= right.int.value,
                .gt_eq => left.int.value >= right.int.value,
            };
            return self.builder.boolean(result);
        }

        if (left == .float and right == .float) {
            const result = switch (op) {
                .lt => left.float.value < right.float.value,
                .gt => left.float.value > right.float.value,
                .lt_eq => left.float.value <= right.float.value,
                .gt_eq => left.float.value >= right.float.value,
            };
            return self.builder.boolean(result);
        }

        if (left == .string and right == .string) {
            const cmp = std.mem.order(u8, left.string, right.string);
            const result = switch (op) {
                .lt => cmp == .lt,
                .gt => cmp == .gt,
                .lt_eq => cmp != .gt,
                .gt_eq => cmp != .lt,
            };
            return self.builder.boolean(result);
        }

        return RuntimeError.TypeError;
    }

    const BitwiseOp = enum { and_, or_, xor, shl, shr };

    fn bitwiseOp(self: *Interpreter, left: Value, right: Value, op: BitwiseOp) RuntimeError!Value {
        if (left != .int or right != .int) {
            return RuntimeError.TypeError;
        }

        const l = left.int.value;
        const r = right.int.value;

        const result: i128 = switch (op) {
            .and_ => l & r,
            .or_ => l | r,
            .xor => l ^ r,
            .shl => blk: {
                if (r < 0 or r >= 128) return RuntimeError.InvalidOperation;
                const shift: u7 = @intCast(@as(u128, @intCast(r)));
                break :blk l << shift;
            },
            .shr => blk: {
                if (r < 0 or r >= 128) return RuntimeError.InvalidOperation;
                const shift: u7 = @intCast(@as(u128, @intCast(r)));
                break :blk l >> shift;
            },
        };

        return self.builder.int(result, left.int.type_);
    }

    fn evalUnary(self: *Interpreter, un: *ast.Unary) RuntimeError!Value {
        const operand = try self.evaluate(un.operand);

        switch (un.op) {
            .negate => {
                if (operand == .int) {
                    const result = self.checkedSub(0, operand.int.value, operand.int.type_, .trap);
                    return self.builder.int(result orelse return RuntimeError.IntegerOverflow, operand.int.type_);
                }
                if (operand == .float) {
                    return self.builder.float(-operand.float.value, operand.float.type_);
                }
                return RuntimeError.TypeError;
            },
            .not => {
                if (operand == .bool_) {
                    return self.builder.boolean(!operand.bool_);
                }
                return RuntimeError.TypeError;
            },
            .ref => {
                const ref = try self.allocator.create(values.ReferenceValue);
                const target = try self.allocator.create(Value);
                target.* = operand;
                ref.* = .{ .target = target, .mutable = false };
                return .{ .reference = ref };
            },
            .ref_mut => {
                const ref = try self.allocator.create(values.ReferenceValue);
                const target = try self.allocator.create(Value);
                target.* = operand;
                ref.* = .{ .target = target, .mutable = true };
                return .{ .reference = ref };
            },
            .deref => {
                if (operand == .reference) {
                    return operand.reference.target.*;
                }
                return RuntimeError.TypeError;
            },
        }
    }

    fn evalPostfix(self: *Interpreter, post: *ast.Postfix) RuntimeError!Value {
        const operand = try self.evaluate(post.operand);

        switch (post.op) {
            .unwrap => {
                // ? operator - propagate None/Err
                if (operand == .optional) {
                    if (operand.optional.value) |v| {
                        return v.*;
                    }
                    return RuntimeError.NullUnwrap;
                }
                if (operand == .result) {
                    if (operand.result.is_ok) {
                        return operand.result.value.*;
                    }
                    return RuntimeError.NullUnwrap;
                }
                return RuntimeError.TypeError;
            },
            .force_unwrap => {
                // ! operator - trap on None/Err
                if (operand == .optional) {
                    if (operand.optional.value) |v| {
                        return v.*;
                    }
                    return RuntimeError.NullUnwrap;
                }
                if (operand == .result) {
                    if (operand.result.is_ok) {
                        return operand.result.value.*;
                    }
                    return RuntimeError.NullUnwrap;
                }
                return RuntimeError.TypeError;
            },
        }
    }

    fn evalCall(self: *Interpreter, call: *ast.Call) RuntimeError!Value {
        const callee = try self.evaluate(call.callee);

        // Evaluate arguments
        var args = std.ArrayListUnmanaged(Value){};
        defer args.deinit(self.allocator);

        for (call.args) |arg| {
            const value = try self.evaluate(arg);
            args.append(self.allocator, value) catch return RuntimeError.OutOfMemory;
        }

        // Call builtin function
        if (callee == .builtin) {
            return callee.builtin.func(self.stringAllocator(), args.items);
        }

        // Call user function
        if (callee == .function) {
            return self.callFunction(callee.function, args.items);
        }

        // Call closure
        if (callee == .closure) {
            return self.callClosure(callee.closure, args.items);
        }

        return RuntimeError.TypeError;
    }

    pub fn callFunction(self: *Interpreter, func: *values.FunctionValue, args: []const Value) RuntimeError!Value {
        // Create new environment for function scope
        const old_env = self.current_env;
        const func_env = try self.allocator.create(Environment);

        // Use closure environment if available, otherwise global
        const parent_env = func.closure_env orelse self.global_env;
        func_env.* = Environment.init(self.allocator, parent_env);
        self.current_env = func_env;

        defer {
            self.current_env = old_env;
            func_env.deinit();
            self.allocator.destroy(func_env);
        }

        // Bind parameters
        for (func.params, 0..) |param, i| {
            const arg = if (i < args.len) args[i] else return RuntimeError.InvalidOperation;
            try self.current_env.define(param.name, arg, false);
        }

        // Execute body
        self.return_value = null;
        const block_result = try self.evalBlock(func.body);

        // If there was an explicit return, use that
        if (self.return_value) |ret| {
            const result = ret;
            self.return_value = null;
            return result;
        }

        // Otherwise use the block's final expression value
        return block_result;
    }

    fn callClosure(self: *Interpreter, closure: *values.ClosureValue, args: []const Value) RuntimeError!Value {
        // Create new environment extending closure's captured environment
        const old_env = self.current_env;
        const closure_env = try self.allocator.create(Environment);
        closure_env.* = Environment.init(self.allocator, closure.env);
        self.current_env = closure_env;

        defer {
            self.current_env = old_env;
            closure_env.deinit();
            self.allocator.destroy(closure_env);
        }

        // Bind parameters
        for (closure.params, 0..) |param, i| {
            const arg = if (i < args.len) args[i] else return RuntimeError.InvalidOperation;
            try self.current_env.define(param.name, arg, false);
        }

        // Evaluate body
        return self.evaluate(closure.body);
    }

    fn evalIndex(self: *Interpreter, idx: *ast.Index) RuntimeError!Value {
        const object = try self.evaluate(idx.object);
        const index = try self.evaluate(idx.index);

        if (index != .int) {
            return RuntimeError.TypeError;
        }

        const i = index.int.value;
        if (i < 0) {
            return RuntimeError.IndexOutOfBounds;
        }

        const index_usize: usize = @intCast(i);

        if (object == .array) {
            if (index_usize >= object.array.elements.len) {
                return RuntimeError.IndexOutOfBounds;
            }
            return object.array.elements[index_usize];
        }

        if (object == .tuple) {
            if (index_usize >= object.tuple.elements.len) {
                return RuntimeError.IndexOutOfBounds;
            }
            return object.tuple.elements[index_usize];
        }

        if (object == .string) {
            if (index_usize >= object.string.len) {
                return RuntimeError.IndexOutOfBounds;
            }
            return self.builder.char(object.string[index_usize]);
        }

        return RuntimeError.TypeError;
    }

    fn evalField(self: *Interpreter, fld: *ast.Field) RuntimeError!Value {
        const object = try self.evaluate(fld.object);

        if (object == .struct_) {
            if (object.struct_.fields.get(fld.field_name)) |value| {
                return value;
            }
            return RuntimeError.UndefinedVariable;
        }

        if (object == .tuple) {
            const idx = std.fmt.parseInt(usize, fld.field_name, 10) catch {
                return RuntimeError.InvalidOperation;
            };
            if (idx >= object.tuple.elements.len) {
                return RuntimeError.IndexOutOfBounds;
            }
            return object.tuple.elements[idx];
        }

        return RuntimeError.TypeError;
    }

    fn evalMethodCall(self: *Interpreter, method: *ast.MethodCall) RuntimeError!Value {
        const object = try self.evaluate(method.object);

        // Evaluate arguments
        var args = std.ArrayListUnmanaged(Value){};
        defer args.deinit(self.allocator);
        for (method.args) |arg| {
            const value = try self.evaluate(arg);
            args.append(self.allocator, value) catch return RuntimeError.OutOfMemory;
        }

        // Built-in methods on all types
        if (std.mem.eql(u8, method.method_name, "to_string")) {
            const str = values.valueToString(self.stringAllocator(), object) catch return RuntimeError.OutOfMemory;
            return self.builder.string(str);
        }

        // Array/Tuple/String len method
        if (std.mem.eql(u8, method.method_name, "len")) {
            if (object == .array) {
                return self.builder.int(@intCast(object.array.elements.len), .usize_);
            }
            if (object == .tuple) {
                return self.builder.int(@intCast(object.tuple.elements.len), .usize_);
            }
            if (object == .string) {
                return self.builder.int(@intCast(object.string.len), .usize_);
            }
            return RuntimeError.InvalidOperation;
        }

        // String methods
        if (object == .string) {
            const str = object.string;

            if (std.mem.eql(u8, method.method_name, "is_empty")) {
                return self.builder.boolean(str.len == 0);
            }

            if (std.mem.eql(u8, method.method_name, "contains")) {
                if (args.items.len != 1 or args.items[0] != .string) {
                    return RuntimeError.InvalidOperation;
                }
                const needle = args.items[0].string;
                const found = std.mem.indexOf(u8, str, needle) != null;
                return self.builder.boolean(found);
            }

            if (std.mem.eql(u8, method.method_name, "starts_with")) {
                if (args.items.len != 1 or args.items[0] != .string) {
                    return RuntimeError.InvalidOperation;
                }
                const prefix = args.items[0].string;
                const starts = std.mem.startsWith(u8, str, prefix);
                return self.builder.boolean(starts);
            }

            if (std.mem.eql(u8, method.method_name, "ends_with")) {
                if (args.items.len != 1 or args.items[0] != .string) {
                    return RuntimeError.InvalidOperation;
                }
                const suffix = args.items[0].string;
                const ends = std.mem.endsWith(u8, str, suffix);
                return self.builder.boolean(ends);
            }

            if (std.mem.eql(u8, method.method_name, "trim")) {
                const trimmed = std.mem.trim(u8, str, " \t\n\r");
                return self.builder.string(trimmed);
            }

            if (std.mem.eql(u8, method.method_name, "to_uppercase")) {
                var upper = self.stringAllocator().alloc(u8, str.len) catch return RuntimeError.OutOfMemory;
                for (str, 0..) |c, i| {
                    upper[i] = std.ascii.toUpper(c);
                }
                return self.builder.string(upper);
            }

            if (std.mem.eql(u8, method.method_name, "to_lowercase")) {
                var lower = self.stringAllocator().alloc(u8, str.len) catch return RuntimeError.OutOfMemory;
                for (str, 0..) |c, i| {
                    lower[i] = std.ascii.toLower(c);
                }
                return self.builder.string(lower);
            }

            if (std.mem.eql(u8, method.method_name, "chars")) {
                // Return array of characters
                var chars = std.ArrayListUnmanaged(Value){};
                var i: usize = 0;
                while (i < str.len) {
                    const cp_len = std.unicode.utf8ByteSequenceLength(str[i]) catch 1;
                    const end = @min(i + cp_len, str.len);
                    if (std.unicode.utf8Decode(str[i..end])) |cp| {
                        chars.append(self.allocator, self.builder.char(cp)) catch return RuntimeError.OutOfMemory;
                    } else |_| {
                        // Invalid UTF-8, add replacement char
                        chars.append(self.allocator, self.builder.char(0xFFFD)) catch return RuntimeError.OutOfMemory;
                    }
                    i = end;
                }
                return self.builder.array(chars.items);
            }

            if (std.mem.eql(u8, method.method_name, "bytes")) {
                // Return array of bytes
                var bytes = std.ArrayListUnmanaged(Value){};
                bytes.ensureTotalCapacity(self.allocator, str.len) catch return RuntimeError.OutOfMemory;
                for (str) |b| {
                    bytes.appendAssumeCapacity(self.builder.int(@intCast(b), .u8_));
                }
                return self.builder.array(bytes.items);
            }
        }

        // Array methods
        if (object == .array) {
            const arr = object.array;

            if (std.mem.eql(u8, method.method_name, "is_empty")) {
                return self.builder.boolean(arr.elements.len == 0);
            }

            if (std.mem.eql(u8, method.method_name, "first")) {
                if (arr.elements.len == 0) {
                    return self.builder.none();
                }
                return self.builder.some(arr.elements[0]);
            }

            if (std.mem.eql(u8, method.method_name, "last")) {
                if (arr.elements.len == 0) {
                    return self.builder.none();
                }
                return self.builder.some(arr.elements[arr.elements.len - 1]);
            }

            if (std.mem.eql(u8, method.method_name, "get")) {
                if (args.items.len != 1 or args.items[0] != .int) {
                    return RuntimeError.InvalidOperation;
                }
                const idx = args.items[0].int.value;
                if (idx < 0 or idx >= arr.elements.len) {
                    return self.builder.none();
                }
                return self.builder.some(arr.elements[@intCast(idx)]);
            }

            if (std.mem.eql(u8, method.method_name, "contains")) {
                if (args.items.len != 1) return RuntimeError.InvalidOperation;
                for (arr.elements) |elem| {
                    if (elem.eql(args.items[0])) return self.builder.boolean(true);
                }
                return self.builder.boolean(false);
            }
        }

        // Integer methods
        if (object == .int) {
            const int = object.int;

            if (std.mem.eql(u8, method.method_name, "abs")) {
                const abs_val = if (int.value < 0) -int.value else int.value;
                return self.builder.int(abs_val, int.type_);
            }

            if (std.mem.eql(u8, method.method_name, "min")) {
                if (args.items.len != 1 or args.items[0] != .int) {
                    return RuntimeError.InvalidOperation;
                }
                const other = args.items[0].int.value;
                return self.builder.int(@min(int.value, other), int.type_);
            }

            if (std.mem.eql(u8, method.method_name, "max")) {
                if (args.items.len != 1 or args.items[0] != .int) {
                    return RuntimeError.InvalidOperation;
                }
                const other = args.items[0].int.value;
                return self.builder.int(@max(int.value, other), int.type_);
            }
        }

        // Optional methods
        if (object == .optional) {
            const opt = object.optional;

            if (std.mem.eql(u8, method.method_name, "is_some")) {
                return self.builder.boolean(opt.value != null);
            }

            if (std.mem.eql(u8, method.method_name, "is_none")) {
                return self.builder.boolean(opt.value == null);
            }

            if (std.mem.eql(u8, method.method_name, "unwrap")) {
                if (opt.value) |v| {
                    return v.*;
                }
                return RuntimeError.NullUnwrap;
            }

            if (std.mem.eql(u8, method.method_name, "unwrap_or")) {
                if (args.items.len != 1) return RuntimeError.InvalidOperation;
                if (opt.value) |v| {
                    return v.*;
                }
                return args.items[0];
            }

            if (std.mem.eql(u8, method.method_name, "expect")) {
                if (opt.value) |v| {
                    return v.*;
                }
                // Print error message and panic
                if (args.items.len == 1 and args.items[0] == .string) {
                    const stderr = std.fs.File{ .handle = std.posix.STDERR_FILENO };
                    stderr.writeAll("panic: ") catch {};
                    stderr.writeAll(args.items[0].string) catch {};
                    stderr.writeAll("\n") catch {};
                }
                return RuntimeError.Panic;
            }
        }

        // Type conversion methods handled in type checker, just return the value
        // since types are checked at compile time
        if (std.mem.eql(u8, method.method_name, "as") or
            std.mem.eql(u8, method.method_name, "to") or
            std.mem.eql(u8, method.method_name, "trunc"))
        {
            return object; // Type checking done by checker
        }

        return RuntimeError.InvalidOperation;
    }

    fn execIfStmt(self: *Interpreter, if_stmt: *ast.IfStmt) RuntimeError!void {
        const condition = try self.evaluate(if_stmt.condition);

        if (condition.isTruthy()) {
            _ = try self.evalBlock(if_stmt.then_branch);
            return;
        }

        if (if_stmt.else_branch) |else_branch| {
            switch (else_branch.*) {
                .block => |block| _ = try self.evalBlock(block),
                .if_stmt => |nested_if| try self.execIfStmt(nested_if),
            }
        }
    }

    fn execMatchStmt(self: *Interpreter, match_stmt: *ast.MatchStmt) RuntimeError!void {
        const subject = try self.evaluate(match_stmt.subject);

        for (match_stmt.arms) |arm| {
            // Try to match pattern
            _ = self.pushEnv() catch return RuntimeError.OutOfMemory;
            defer self.popEnv();

            if (self.matchPattern(arm.pattern, subject)) {
                // Check guard if present
                if (arm.guard) |guard| {
                    const guard_val = try self.evaluate(guard);
                    if (!guard_val.isTruthy()) {
                        continue;
                    }
                }
                // Execute the arm body (a block)
                _ = try self.evalBlock(arm.body);
                return;
            }
        }

        return RuntimeError.PatternMatchFailed;
    }

    pub fn evalBlock(self: *Interpreter, block: *ast.Block) RuntimeError!Value {
        _ = self.pushEnv() catch return RuntimeError.OutOfMemory;
        defer self.popEnv();

        for (block.statements) |stmt| {
            try self.execute(stmt);

            // Check for control flow
            if (self.return_value != null or self.is_breaking or self.is_continuing) {
                break;
            }
        }

        if (self.return_value != null or self.is_breaking or self.is_continuing) {
            return self.builder.voidVal();
        }

        if (block.final_expr) |final| {
            return self.evaluate(final);
        }

        return self.builder.voidVal();
    }

    fn evalClosure(self: *Interpreter, closure: *ast.Closure) RuntimeError!Value {
        const clos = try self.allocator.create(values.ClosureValue);
        clos.* = .{
            .params = closure.params,
            .body = closure.body,
            .env = self.current_env,
        };
        return .{ .closure = clos };
    }

    fn evalRange(self: *Interpreter, range: *ast.Range) RuntimeError!Value {
        // For now, create an array containing the range values
        // This is a simplified implementation
        const start_val = if (range.start) |s| try self.evaluate(s) else self.builder.i32Val(0);
        const end_val = if (range.end) |e| try self.evaluate(e) else return RuntimeError.InvalidOperation;

        if (start_val != .int or end_val != .int) {
            return RuntimeError.TypeError;
        }

        const start: i64 = @intCast(start_val.int.value);
        const end: i64 = @intCast(end_val.int.value);
        const size: usize = @intCast(@max(0, end - start + @as(i64, if (range.inclusive) 1 else 0)));

        var elements = std.ArrayListUnmanaged(Value){};
        elements.ensureTotalCapacity(self.allocator, size) catch return RuntimeError.OutOfMemory;

        var i: i64 = start;
        const limit: i64 = end + @as(i64, if (range.inclusive) 1 else 0);
        while (i < limit) : (i += 1) {
            elements.appendAssumeCapacity(self.builder.int(i, .i32_));
        }

        return self.builder.array(elements.items);
    }

    fn evalStructLiteral(self: *Interpreter, lit: *ast.StructLiteral) RuntimeError!Value {
        const s = try self.allocator.create(values.StructValue);
        s.* = .{
            .type_name = if (lit.type_name) |tn| blk: {
                switch (tn) {
                    .named => |n| break :blk n.name,
                    else => break :blk "anonymous",
                }
            } else "anonymous",
            .fields = .{},
        };

        for (lit.fields) |field| {
            const value = try self.evaluate(field.value);
            s.fields.put(self.allocator, field.name, value) catch return RuntimeError.OutOfMemory;
        }

        return .{ .struct_ = s };
    }

    fn evalArrayLiteral(self: *Interpreter, arr: *ast.ArrayLiteral) RuntimeError!Value {
        var elements = std.ArrayListUnmanaged(Value){};
        elements.ensureTotalCapacity(self.allocator, arr.elements.len) catch return RuntimeError.OutOfMemory;

        for (arr.elements) |elem| {
            const value = try self.evaluate(elem);
            elements.appendAssumeCapacity(value);
        }

        return self.builder.array(elements.items);
    }

    fn evalTupleLiteral(self: *Interpreter, tup: *ast.TupleLiteral) RuntimeError!Value {
        var elements = std.ArrayListUnmanaged(Value){};
        elements.ensureTotalCapacity(self.allocator, tup.elements.len) catch return RuntimeError.OutOfMemory;

        for (tup.elements) |elem| {
            const value = try self.evaluate(elem);
            elements.appendAssumeCapacity(value);
        }

        return self.builder.tuple(elements.items);
    }

    fn evalTypeCast(self: *Interpreter, cast: *ast.TypeCast) RuntimeError!Value {
        const value = try self.evaluate(cast.expr);
        const truncating = cast.truncating;

        // Get target type from type expression
        const target_type_name: []const u8 = switch (cast.target_type) {
            .named => |n| n.name,
            else => return value, // Non-named types just pass through for now
        };

        // Map type name to integer type
        const target_int_type = getIntegerType(target_type_name);
        const target_float_type = getFloatType(target_type_name);

        // Handle integer conversions
        if (value == .int) {
            if (target_int_type) |int_type| {
                return self.convertInteger(value.int, int_type, truncating);
            }
            if (target_float_type) |float_type| {
                // Integer to float conversion
                const float_val: f64 = @floatFromInt(value.int.value);
                return self.builder.float(float_val, float_type);
            }
            if (std.mem.eql(u8, target_type_name, "string")) {
                // Integer to string
                const str = values.valueToString(self.stringAllocator(), value) catch return RuntimeError.OutOfMemory;
                return self.builder.string(str);
            }
        }

        // Handle float conversions
        if (value == .float) {
            if (target_float_type) |float_type| {
                return self.builder.float(value.float.value, float_type);
            }
            if (target_int_type) |int_type| {
                // Float to integer conversion
                return self.convertFloatToInt(value.float.value, int_type, truncating);
            }
            if (std.mem.eql(u8, target_type_name, "string")) {
                const str = values.valueToString(self.stringAllocator(), value) catch return RuntimeError.OutOfMemory;
                return self.builder.string(str);
            }
        }

        // Handle bool to string
        if (value == .bool_ and std.mem.eql(u8, target_type_name, "string")) {
            return self.builder.string(if (value.bool_) "true" else "false");
        }

        // Handle char to string
        if (value == .char_ and std.mem.eql(u8, target_type_name, "string")) {
            var buf: [4]u8 = undefined;
            const len = std.unicode.utf8Encode(value.char_, &buf) catch return RuntimeError.InvalidCast;
            const str = self.stringAllocator().dupe(u8, buf[0..len]) catch return RuntimeError.OutOfMemory;
            return self.builder.string(str);
        }

        // Handle char to integer
        if (value == .char_ and target_int_type != null) {
            return self.builder.int(@intCast(value.char_), target_int_type.?);
        }

        // Handle integer to char
        if (value == .int and std.mem.eql(u8, target_type_name, "char")) {
            if (value.int.value < 0 or value.int.value > 0x10FFFF) {
                if (truncating) {
                    return self.builder.char(@truncate(@as(u32, @intCast(@as(u128, @bitCast(value.int.value)) & 0x1FFFFF))));
                }
                return RuntimeError.InvalidCast;
            }
            return self.builder.char(@intCast(value.int.value));
        }

        // Fallback: return value unchanged
        return value;
    }

    fn getIntegerType(name: []const u8) ?Integer.IntegerType {
        const map = std.StaticStringMap(Integer.IntegerType).initComptime(.{
            .{ "i8", .i8_ },
            .{ "i16", .i16_ },
            .{ "i32", .i32_ },
            .{ "i64", .i64_ },
            .{ "i128", .i128_ },
            .{ "isize", .isize_ },
            .{ "u8", .u8_ },
            .{ "u16", .u16_ },
            .{ "u32", .u32_ },
            .{ "u64", .u64_ },
            .{ "u128", .u128_ },
            .{ "usize", .usize_ },
        });
        return map.get(name);
    }

    fn getFloatType(name: []const u8) ?Float.FloatType {
        if (std.mem.eql(u8, name, "f32")) return .f32_;
        if (std.mem.eql(u8, name, "f64")) return .f64_;
        return null;
    }

    fn convertInteger(self: *Interpreter, int_val: Integer, target_type: Integer.IntegerType, truncating: bool) RuntimeError!Value {
        const val = int_val.value;
        const min = target_type.minValue();
        const max = target_type.maxValue();

        // Check if value fits
        if (val >= min and val <= max) {
            return self.builder.int(val, target_type);
        }

        // Value doesn't fit - truncate or error
        if (truncating) {
            const range = max - min + 1;
            const wrapped = @mod(val - min, range) + min;
            return self.builder.int(wrapped, target_type);
        }
        return RuntimeError.InvalidCast;
    }

    fn convertFloatToInt(self: *Interpreter, float_val: f64, target_type: Integer.IntegerType, truncating: bool) RuntimeError!Value {
        const min = target_type.minValue();
        const max = target_type.maxValue();

        // Check for NaN or infinity
        if (std.math.isNan(float_val) or std.math.isInf(float_val)) {
            return RuntimeError.InvalidCast;
        }

        const int_val: i128 = @intFromFloat(@trunc(float_val));

        // Check if value fits
        if (int_val >= min and int_val <= max) {
            return self.builder.int(int_val, target_type);
        }

        // Value doesn't fit - truncate or error
        if (truncating) {
            const range = max - min + 1;
            const wrapped = @mod(int_val - min, range) + min;
            return self.builder.int(wrapped, target_type);
        }
        return RuntimeError.InvalidCast;
    }

    // ========================================================================
    // Pattern Matching
    // ========================================================================

    fn matchPattern(self: *Interpreter, pattern: ast.Pattern, value: Value) bool {
        return switch (pattern) {
            .wildcard => true,
            .literal => |lit| self.matchLiteralPattern(lit, value),
            .binding => |bind| {
                self.current_env.define(bind.name, value, bind.mutable) catch return false;
                return true;
            },
            .variant => |v| self.matchVariantPattern(v, value),
            .struct_pattern => |s| self.matchStructPattern(s, value),
            .tuple_pattern => |t| self.matchTuplePattern(t, value),
            .or_pattern => |o| {
                for (o.alternatives) |alt| {
                    if (self.matchPattern(alt, value)) return true;
                }
                return false;
            },
            .guarded => |g| self.matchPattern(g.pattern, value),
        };
    }

    fn matchLiteralPattern(self: *Interpreter, lit: ast.PatternLiteral, value: Value) bool {
        switch (lit.kind) {
            .int => |i| {
                if (value != .int) return false;
                return value.int.value == i;
            },
            .float => |f| {
                if (value != .float) return false;
                return value.float.value == f;
            },
            .bool_ => |b| {
                if (value != .bool_) return false;
                return value.bool_ == b;
            },
            .char => |c| {
                if (value != .char_) return false;
                return value.char_ == c;
            },
            .string => |s| {
                if (value != .string) return false;
                return std.mem.eql(u8, value.string, s);
            },
        }
        _ = self;
    }

    fn matchVariantPattern(self: *Interpreter, pat: *ast.VariantPattern, value: Value) bool {
        if (value != .enum_) return false;

        const enum_val = value.enum_;
        if (!std.mem.eql(u8, enum_val.variant_name, pat.variant_name)) return false;

        if (pat.payload) |payload_pattern| {
            if (enum_val.payload) |payload_value| {
                return self.matchPattern(payload_pattern, payload_value.*);
            }
            return false;
        }

        return enum_val.payload == null;
    }

    fn matchStructPattern(self: *Interpreter, pat: *ast.StructPattern, value: Value) bool {
        if (value != .struct_) return false;

        const struct_val = value.struct_;
        for (pat.fields) |field| {
            if (struct_val.fields.get(field.name)) |field_val| {
                if (field.pattern) |field_pattern| {
                    if (!self.matchPattern(field_pattern, field_val)) return false;
                } else {
                    // Shorthand: field name is also the binding name
                    self.current_env.define(field.name, field_val, false) catch return false;
                }
            } else {
                return false;
            }
        }

        return true;
    }

    fn matchTuplePattern(self: *Interpreter, pat: *ast.TuplePattern, value: Value) bool {
        if (value != .tuple) return false;

        const tuple_val = value.tuple;
        if (pat.elements.len != tuple_val.elements.len) return false;

        for (pat.elements, tuple_val.elements) |elem_pattern, elem_val| {
            if (!self.matchPattern(elem_pattern, elem_val)) return false;
        }

        return true;
    }

    // ========================================================================
    // Statement Execution
    // ========================================================================

    pub fn execute(self: *Interpreter, stmt: ast.Stmt) RuntimeError!void {
        switch (stmt) {
            .let_decl => |l| try self.execLetDecl(l),
            .var_decl => |v| try self.execVarDecl(v),
            .assignment => |a| try self.execAssignment(a),
            .expr_stmt => |e| _ = try self.evaluate(e.expr),
            .return_stmt => |r| try self.execReturn(r),
            .break_stmt => |b| try self.execBreak(b),
            .continue_stmt => self.execContinue(),
            .for_loop => |f| try self.execFor(f),
            .while_loop => |w| try self.execWhile(w),
            .loop_stmt => |l| try self.execLoop(l),
            .if_stmt => |i| try self.execIfStmt(i),
            .match_stmt => |m| try self.execMatchStmt(m),
        }
    }

    fn execLetDecl(self: *Interpreter, decl: *ast.LetDecl) RuntimeError!void {
        const value = try self.evaluate(decl.value);
        try self.current_env.define(decl.name, value, false);
    }

    fn execVarDecl(self: *Interpreter, decl: *ast.VarDecl) RuntimeError!void {
        const value = try self.evaluate(decl.value);
        try self.current_env.define(decl.name, value, true);
    }

    fn execAssignment(self: *Interpreter, assign: *ast.Assignment) RuntimeError!void {
        const value = try self.evaluate(assign.value);

        // Get the target identifier name
        const name = switch (assign.target) {
            .identifier => |id| id.name,
            else => return RuntimeError.InvalidOperation,
        };

        // Handle compound assignment
        if (assign.op != .assign) {
            const current = self.current_env.get(name) orelse return RuntimeError.UndefinedVariable;
            const new_value = switch (assign.op) {
                .add_assign => try self.intArithmetic(current, value, .add, .trap),
                .sub_assign => try self.intArithmetic(current, value, .sub, .trap),
                .mul_assign => try self.intArithmetic(current, value, .mul, .trap),
                .div_assign => try self.intArithmetic(current, value, .div, .trap),
                .mod_assign => try self.intArithmetic(current, value, .mod, .trap),
                else => return RuntimeError.InvalidOperation,
            };
            try self.current_env.set(name, new_value);
            return;
        }

        try self.current_env.set(name, value);
    }

    fn execReturn(self: *Interpreter, ret: *ast.ReturnStmt) RuntimeError!void {
        if (ret.value) |value_expr| {
            self.return_value = try self.evaluate(value_expr);
        } else {
            self.return_value = self.builder.voidVal();
        }
    }

    fn execBreak(self: *Interpreter, brk: *ast.BreakStmt) RuntimeError!void {
        if (brk.value) |value_expr| {
            self.break_value = try self.evaluate(value_expr);
        }
        self.is_breaking = true;
    }

    fn execContinue(self: *Interpreter) void {
        self.is_continuing = true;
    }

    fn execFor(self: *Interpreter, for_loop: *ast.ForLoop) RuntimeError!void {
        const iterable = try self.evaluate(for_loop.iterable);

        const elements: []Value = switch (iterable) {
            .array => |a| a.elements,
            .tuple => |t| t.elements,
            else => return RuntimeError.TypeError,
        };

        for (elements) |elem| {
            _ = self.pushEnv() catch return RuntimeError.OutOfMemory;

            // Bind pattern
            if (!self.matchPattern(for_loop.pattern, elem)) {
                self.popEnv();
                continue;
            }

            // Execute body
            _ = self.evalBlock(for_loop.body) catch |err| {
                self.popEnv();
                return err;
            };

            self.popEnv();

            // Handle control flow
            if (self.is_breaking) {
                self.is_breaking = false;
                break;
            }
            if (self.is_continuing) {
                self.is_continuing = false;
                continue;
            }
            if (self.return_value != null) {
                break;
            }
        }
    }

    fn execWhile(self: *Interpreter, while_loop: *ast.WhileLoop) RuntimeError!void {
        while (true) {
            const condition = try self.evaluate(while_loop.condition);
            if (!condition.isTruthy()) break;

            _ = try self.evalBlock(while_loop.body);

            // Handle control flow
            if (self.is_breaking) {
                self.is_breaking = false;
                break;
            }
            if (self.is_continuing) {
                self.is_continuing = false;
                continue;
            }
            if (self.return_value != null) {
                break;
            }
        }
    }

    fn execLoop(self: *Interpreter, loop: *ast.LoopStmt) RuntimeError!void {
        while (true) {
            _ = try self.evalBlock(loop.body);

            // Handle control flow
            if (self.is_breaking) {
                self.is_breaking = false;
                break;
            }
            if (self.is_continuing) {
                self.is_continuing = false;
                continue;
            }
            if (self.return_value != null) {
                break;
            }
        }
    }

    // ========================================================================
    // Declaration Execution
    // ========================================================================

    pub fn executeDecl(self: *Interpreter, decl: ast.Decl) RuntimeError!void {
        switch (decl) {
            .function => |f| try self.registerFunction(f),
            .struct_decl => {}, // Structs are types, handled by type checker
            .enum_decl => {}, // Enums are types, handled by type checker
            .const_decl => |c| try self.execConst(c),
            else => {},
        }
    }

    fn registerFunction(self: *Interpreter, func: *ast.FunctionDecl) RuntimeError!void {
        if (func.body) |body| {
            const func_val = try self.allocator.create(values.FunctionValue);
            errdefer self.allocator.destroy(func_val);

            // Convert params
            var params = std.ArrayListUnmanaged(values.FunctionValue.FunctionParam){};
            for (func.params) |param| {
                params.append(self.allocator, .{ .name = param.name }) catch return RuntimeError.OutOfMemory;
            }

            const params_slice = params.toOwnedSlice(self.allocator) catch return RuntimeError.OutOfMemory;
            errdefer self.allocator.free(params_slice);

            func_val.* = .{
                .name = func.name,
                .params = params_slice,
                .body = body,
                .closure_env = self.current_env,
            };

            // Track for cleanup
            self.allocated_functions.append(self.allocator, func_val) catch return RuntimeError.OutOfMemory;

            try self.current_env.define(func.name, .{ .function = func_val }, false);
        }
    }

    fn execConst(self: *Interpreter, const_decl: *ast.ConstDecl) RuntimeError!void {
        const value = try self.evaluate(const_decl.value);
        try self.current_env.define(const_decl.name, value, false);
    }

    // ========================================================================
    // Module Execution
    // ========================================================================

    pub fn executeModule(self: *Interpreter, module: ast.Module) RuntimeError!void {
        // First pass: register all functions and constants
        for (module.declarations) |decl| {
            try self.executeDecl(decl);
        }
    }

    // ========================================================================
    // Output Helpers (for testing)
    // ========================================================================

    pub fn captureOutput(self: *Interpreter, comptime fmt: []const u8, args: anytype) void {
        var buf: [4096]u8 = undefined;
        const formatted = std.fmt.bufPrint(&buf, fmt, args) catch return;
        self.output.appendSlice(self.allocator, formatted) catch {};
    }

    pub fn getOutput(self: *Interpreter) []const u8 {
        return self.output.items;
    }

    // ========================================================================
    // Comptime Evaluation
    // ========================================================================

    /// Evaluate a comptime block - executes the block body and returns its value
    fn evalComptimeBlock(self: *Interpreter, block: *ast.ComptimeBlock) RuntimeError!Value {
        // Comptime blocks are just regular blocks evaluated at compile time
        // The interpreter evaluates them normally
        return self.evalBlock(block.body);
    }

    /// Evaluate a builtin call (@typeName, @typeInfo, etc.)
    fn evalBuiltinCall(self: *Interpreter, builtin: *ast.BuiltinCall) RuntimeError!Value {
        if (std.mem.eql(u8, builtin.name, "typeName")) {
            return self.evalBuiltinTypeName(builtin);
        } else if (std.mem.eql(u8, builtin.name, "typeInfo")) {
            return self.evalBuiltinTypeInfo(builtin);
        } else if (std.mem.eql(u8, builtin.name, "fields")) {
            return self.evalBuiltinFields(builtin);
        } else if (std.mem.eql(u8, builtin.name, "compileError")) {
            return self.evalBuiltinCompileError(builtin);
        } else if (std.mem.eql(u8, builtin.name, "hasField")) {
            return self.evalBuiltinHasField(builtin);
        } else {
            return RuntimeError.InvalidOperation;
        }
    }

    /// @typeName(T) -> string
    fn evalBuiltinTypeName(self: *Interpreter, builtin: *ast.BuiltinCall) RuntimeError!Value {
        _ = self;
        if (builtin.args.len != 1) return RuntimeError.InvalidOperation;

        // The type argument should have been resolved by the type checker
        // For now, just return a placeholder string
        switch (builtin.args[0]) {
            .type_arg => |type_expr| {
                // Convert type expression to string
                const name = switch (type_expr) {
                    .named => |n| n.name,
                    else => "<type>",
                };
                return .{ .string = name };
            },
            .expr_arg => {
                return RuntimeError.TypeError;
            },
        }
    }

    /// @typeInfo(T) -> string describing the type kind
    fn evalBuiltinTypeInfo(self: *Interpreter, builtin: *ast.BuiltinCall) RuntimeError!Value {
        _ = self;
        if (builtin.args.len != 1) return RuntimeError.InvalidOperation;

        // The type argument determines what kind of type it is
        switch (builtin.args[0]) {
            .type_arg => |type_expr| {
                const kind: []const u8 = switch (type_expr) {
                    .named => |n| blk: {
                        // Check if it's a primitive type name
                        const primitives = [_][]const u8{ "i8", "i16", "i32", "i64", "i128", "isize", "u8", "u16", "u32", "u64", "u128", "usize", "f32", "f64", "bool", "char", "string" };
                        for (primitives) |p| {
                            if (std.mem.eql(u8, n.name, p)) {
                                break :blk "primitive";
                            }
                        }
                        // Could be a struct, enum, or trait - assume struct for now
                        break :blk "struct";
                    },
                    .array => "array",
                    .slice => "slice",
                    .tuple => "tuple",
                    .optional => "optional",
                    .result => "result",
                    .function => "function",
                    .reference => "reference",
                    else => "unknown",
                };
                return .{ .string = kind };
            },
            .expr_arg => {
                return RuntimeError.TypeError;
            },
        }
    }

    /// @fields(T) -> string with comma-separated field names
    fn evalBuiltinFields(self: *Interpreter, builtin: *ast.BuiltinCall) RuntimeError!Value {
        _ = self;
        if (builtin.args.len != 1) return RuntimeError.InvalidOperation;

        // In the interpreter, we don't have full type info, so we return empty string
        // The real work is done by the type checker which has access to struct definitions
        return .{ .string = "" };
    }

    /// @compileError("message") -> never returns
    fn evalBuiltinCompileError(self: *Interpreter, builtin: *ast.BuiltinCall) RuntimeError!Value {
        if (builtin.args.len != 1) return RuntimeError.InvalidOperation;

        switch (builtin.args[0]) {
            .expr_arg => |expr| {
                const msg_value = try self.evaluate(expr);
                if (msg_value != .string) return RuntimeError.TypeError;

                // In the interpreter, we just report the error
                // The actual compile error happens during type checking
                const stdout = getStdOut();
                stdout.writeAll("compile error: ") catch {};
                stdout.writeAll(msg_value.string) catch {};
                stdout.writeAll("\n") catch {};

                return RuntimeError.ComptimeError;
            },
            .type_arg => {
                return RuntimeError.TypeError;
            },
        }
    }

    /// @hasField(T, "field_name") -> bool
    fn evalBuiltinHasField(self: *Interpreter, builtin: *ast.BuiltinCall) RuntimeError!Value {
        _ = self;
        if (builtin.args.len != 2) return RuntimeError.InvalidOperation;

        // TODO: Implement actual field checking
        // For now, return false as placeholder
        return .{ .bool_ = false };
    }
};

// ============================================================================
// Built-in Functions
// ============================================================================

fn builtinPrint(allocator: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.InvalidOperation;

    const output = switch (args[0]) {
        .string => |s| s,
        else => blk: {
            const str = values.valueToString(allocator, args[0]) catch return RuntimeError.OutOfMemory;
            break :blk str;
        },
    };

    const stdout = getStdOut();
    stdout.writeAll(output) catch {};

    return .void_;
}

fn builtinPrintln(allocator: Allocator, args: []const Value) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.InvalidOperation;

    const output = switch (args[0]) {
        .string => |s| s,
        else => blk: {
            const str = values.valueToString(allocator, args[0]) catch return RuntimeError.OutOfMemory;
            break :blk str;
        },
    };

    const stdout = getStdOut();
    stdout.writeAll(output) catch {};
    stdout.writeAll("\n") catch {};

    return .void_;
}

fn builtinAssert(allocator: Allocator, args: []const Value) RuntimeError!Value {
    _ = allocator;
    if (args.len != 1) return RuntimeError.InvalidOperation;

    if (args[0] != .bool_) return RuntimeError.TypeError;

    if (!args[0].bool_) {
        return RuntimeError.AssertionFailed;
    }

    return .void_;
}

fn builtinAssertEq(allocator: Allocator, args: []const Value) RuntimeError!Value {
    _ = allocator;
    if (args.len != 2) return RuntimeError.InvalidOperation;

    if (!args[0].eql(args[1])) {
        return RuntimeError.AssertionFailed;
    }

    return .void_;
}

fn builtinPanic(allocator: Allocator, args: []const Value) RuntimeError!Value {
    _ = allocator;
    if (args.len != 1) return RuntimeError.InvalidOperation;

    // Print panic message to stderr
    const stderr = std.fs.File{ .handle = std.posix.STDERR_FILENO };
    stderr.writeAll("panic: ") catch {};

    switch (args[0]) {
        .string => |s| stderr.writeAll(s) catch {},
        else => stderr.writeAll("(non-string value)") catch {},
    }
    stderr.writeAll("\n") catch {};

    return RuntimeError.Panic;
}

fn builtinLen(allocator: Allocator, args: []const Value) RuntimeError!Value {
    _ = allocator;
    if (args.len != 1) return RuntimeError.InvalidOperation;

    const len: i128 = switch (args[0]) {
        .array => |a| @intCast(a.elements.len),
        .tuple => |t| @intCast(t.elements.len),
        .string => |s| @intCast(s.len),
        else => return RuntimeError.TypeError,
    };

    return .{ .int = .{ .value = len, .type_ = .usize_ } };
}

fn builtinTypeOf(allocator: Allocator, args: []const Value) RuntimeError!Value {
    _ = allocator;
    if (args.len != 1) return RuntimeError.InvalidOperation;

    const type_name: []const u8 = switch (args[0]) {
        .int => |i| switch (i.type_) {
            .i8_ => "i8",
            .i16_ => "i16",
            .i32_ => "i32",
            .i64_ => "i64",
            .i128_ => "i128",
            .isize_ => "isize",
            .u8_ => "u8",
            .u16_ => "u16",
            .u32_ => "u32",
            .u64_ => "u64",
            .u128_ => "u128",
            .usize_ => "usize",
        },
        .float => |f| switch (f.type_) {
            .f32_ => "f32",
            .f64_ => "f64",
        },
        .bool_ => "bool",
        .char_ => "char",
        .string => "string",
        .array => "[T]",
        .tuple => "(T...)",
        .struct_ => |s| s.type_name,
        .enum_ => |e| e.type_name,
        .optional => "?T",
        .result => "Result[T, E]",
        .reference => "&T",
        .function => "fn",
        .closure => "closure",
        .builtin => "builtin",
        .void_ => "void",
        .never => "!",
    };

    return .{ .string = type_name };
}

// ============================================================================
// Tests
// ============================================================================

test "Interpreter initialization" {
    const testing = std.testing;
    var interp = try Interpreter.init(testing.allocator);
    defer interp.deinit();

    // Check builtins are registered
    try testing.expect(interp.global_env.get("print") != null);
    try testing.expect(interp.global_env.get("println") != null);
    try testing.expect(interp.global_env.get("assert") != null);
}

test "Evaluate literal expressions" {
    const testing = std.testing;
    var interp = try Interpreter.init(testing.allocator);
    defer interp.deinit();

    // Integer literal
    const int_lit = ast.Literal{
        .kind = .{ .int = 42 },
        .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
    };
    const int_val = try interp.evaluate(.{ .literal = int_lit });
    try testing.expect(int_val == .int);
    try testing.expectEqual(@as(i128, 42), int_val.int.value);

    // Boolean literal
    const bool_lit = ast.Literal{
        .kind = .{ .bool_ = true },
        .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
    };
    const bool_val = try interp.evaluate(.{ .literal = bool_lit });
    try testing.expect(bool_val == .bool_);
    try testing.expect(bool_val.bool_);

    // String literal
    const str_lit = ast.Literal{
        .kind = .{ .string = "hello" },
        .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
    };
    const str_val = try interp.evaluate(.{ .literal = str_lit });
    try testing.expect(str_val == .string);
    try testing.expectEqualStrings("hello", str_val.string);
}

test "Evaluate identifier" {
    const testing = std.testing;
    var interp = try Interpreter.init(testing.allocator);
    defer interp.deinit();

    try interp.current_env.define("x", interp.builder.i32Val(42), false);

    const id = ast.Identifier{
        .name = "x",
        .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
    };
    const val = try interp.evaluate(.{ .identifier = id });
    try testing.expect(val.eql(interp.builder.i32Val(42)));
}

test "Arithmetic operations" {
    const testing = std.testing;
    var interp = try Interpreter.init(testing.allocator);
    defer interp.deinit();

    // Test addition
    const result = try interp.intArithmetic(
        interp.builder.i32Val(10),
        interp.builder.i32Val(5),
        .add,
        .trap,
    );
    try testing.expect(result.eql(interp.builder.i32Val(15)));

    // Test subtraction
    const sub_result = try interp.intArithmetic(
        interp.builder.i32Val(10),
        interp.builder.i32Val(5),
        .sub,
        .trap,
    );
    try testing.expect(sub_result.eql(interp.builder.i32Val(5)));

    // Test multiplication
    const mul_result = try interp.intArithmetic(
        interp.builder.i32Val(10),
        interp.builder.i32Val(5),
        .mul,
        .trap,
    );
    try testing.expect(mul_result.eql(interp.builder.i32Val(50)));

    // Test division
    const div_result = try interp.intArithmetic(
        interp.builder.i32Val(10),
        interp.builder.i32Val(5),
        .div,
        .trap,
    );
    try testing.expect(div_result.eql(interp.builder.i32Val(2)));
}

test "Division by zero" {
    const testing = std.testing;
    var interp = try Interpreter.init(testing.allocator);
    defer interp.deinit();

    const result = interp.intArithmetic(
        interp.builder.i32Val(10),
        interp.builder.i32Val(0),
        .div,
        .trap,
    );
    try testing.expectError(RuntimeError.DivisionByZero, result);
}

test "Comparison operations" {
    const testing = std.testing;
    var interp = try Interpreter.init(testing.allocator);
    defer interp.deinit();

    // Less than
    const lt = try interp.compareValues(
        interp.builder.i32Val(5),
        interp.builder.i32Val(10),
        .lt,
    );
    try testing.expect(lt.bool_);

    // Greater than
    const gt = try interp.compareValues(
        interp.builder.i32Val(10),
        interp.builder.i32Val(5),
        .gt,
    );
    try testing.expect(gt.bool_);
}

test "Wrapping arithmetic" {
    const testing = std.testing;
    var interp = try Interpreter.init(testing.allocator);
    defer interp.deinit();

    // Test wrapping on overflow
    const max_i32 = interp.builder.int(std.math.maxInt(i32), .i32_);
    const result = try interp.intArithmetic(max_i32, interp.builder.i32Val(1), .add, .wrap);
    try testing.expectEqual(@as(i128, std.math.minInt(i32)), result.int.value);
}

test "Saturating arithmetic" {
    const testing = std.testing;
    var interp = try Interpreter.init(testing.allocator);
    defer interp.deinit();

    // Test saturation on overflow
    const max_i32 = interp.builder.int(std.math.maxInt(i32), .i32_);
    const result = try interp.intArithmetic(max_i32, interp.builder.i32Val(1), .add, .saturate);
    try testing.expectEqual(@as(i128, std.math.maxInt(i32)), result.int.value);
}
