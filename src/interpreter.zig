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
        self.global_env.deinit();
        self.allocator.destroy(self.global_env);
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
            .if_expr => |i| self.evalIfExpr(i),
            .match_expr => |m| self.evalMatchExpr(m),
            .block => |b| self.evalBlock(b),
            .closure => |c| self.evalClosure(c),
            .range => |r| self.evalRange(r),
            .struct_literal => |s| self.evalStructLiteral(s),
            .array_literal => |a| self.evalArrayLiteral(a),
            .tuple_literal => |t| self.evalTupleLiteral(t),
            .type_cast => |tc| self.evalTypeCast(tc),
            .grouped => |g| self.evaluate(g.expr),
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

    fn evalIdentifier(self: *Interpreter, id: ast.Identifier) RuntimeError!Value {
        if (self.current_env.get(id.name)) |value| {
            return value;
        }
        return RuntimeError.UndefinedVariable;
    }

    fn evalBinary(self: *Interpreter, bin: *ast.Binary) RuntimeError!Value {
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
            const combined = try std.mem.concat(self.allocator, u8, &.{ left.string, right.string });
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
            return callee.builtin.func(self.allocator, args.items);
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

        // Built-in methods
        if (std.mem.eql(u8, method.method_name, "len")) {
            if (object == .array) {
                return self.builder.int(@intCast(object.array.elements.len), .usize_);
            }
            if (object == .string) {
                return self.builder.int(@intCast(object.string.len), .usize_);
            }
            return RuntimeError.InvalidOperation;
        }

        if (std.mem.eql(u8, method.method_name, "to_string")) {
            const str = values.valueToString(self.allocator, object) catch return RuntimeError.OutOfMemory;
            return self.builder.string(str);
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

    fn evalIfExpr(self: *Interpreter, if_expr: *ast.IfExpr) RuntimeError!Value {
        const condition = try self.evaluate(if_expr.condition);

        if (condition.isTruthy()) {
            return self.evaluate(if_expr.then_branch);
        }

        if (if_expr.else_branch) |else_branch| {
            return self.evaluate(else_branch);
        }

        return self.builder.voidVal();
    }

    fn evalMatchExpr(self: *Interpreter, match: *ast.MatchExpr) RuntimeError!Value {
        const subject = try self.evaluate(match.subject);

        for (match.arms) |arm| {
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
                return self.evaluate(arm.body);
            }
        }

        return RuntimeError.PatternMatchFailed;
    }

    fn evalBlock(self: *Interpreter, block: *ast.Block) RuntimeError!Value {
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

        // For now, just return the value - real type conversion would happen here
        // The type checker validates conversions at compile time
        _ = cast.cast_kind;
        return value;
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
