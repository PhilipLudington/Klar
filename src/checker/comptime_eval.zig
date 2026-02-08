//! Compile-time evaluation.
//!
//! This module contains functions for evaluating expressions at compile time.
//! Used for const expressions, array sizes, and comptime function calls.

const std = @import("std");
const ast = @import("../ast.zig");
const types = @import("../types.zig");
const Type = types.Type;
const interpreter = @import("../interpreter.zig");
const Interpreter = interpreter.Interpreter;
const values = @import("../values.zig");
const Value = values.Value;
const comptime_ = @import("comptime.zig");
const ComptimeValue = comptime_.ComptimeValue;
const max_comptime_depth = comptime_.max_comptime_depth;

// ============================================================================
// Compile-Time Evaluation
// ============================================================================

/// Populate interpreter environment with constants from the current scope.
/// Copies compile-time evaluated constants to the interpreter's global environment.
pub fn populateInterpreterEnv(tc: anytype, interp: *Interpreter) void {
    // Copy all evaluated constant values to the interpreter
    var iter = tc.constant_values.iterator();
    while (iter.next()) |entry| {
        const name = entry.key_ptr.*;
        const cv = entry.value_ptr.*;
        const value = comptimeValueToInterpreterValue(tc, cv);
        interp.global_env.define(name, value, false) catch {};
    }
}

/// Evaluate a call to a comptime function at compile time.
/// Returns the computed ComptimeValue, or null if evaluation fails.
/// Supports recursive comptime function calls by sharing the interpreter.
pub fn evaluateComptimeCall(tc: anytype, call: *ast.Call, func_decl: *ast.FunctionDecl) ?ComptimeValue {
    // Check recursion depth limit
    if (tc.comptime_depth >= max_comptime_depth) {
        tc.addError(.comptime_error, call.span, "comptime recursion depth limit exceeded (max {d})", .{max_comptime_depth});
        return null;
    }

    // First, check that all arguments are compile-time known
    var arg_values = std.ArrayListUnmanaged(Value){};
    defer arg_values.deinit(tc.allocator);

    for (call.args) |arg| {
        // Try to evaluate the argument at compile time
        const arg_value = evaluateComptimeExpr(tc, arg) orelse {
            tc.addError(.comptime_error, arg.span(), "argument to comptime function must be comptime-known", .{});
            return null;
        };
        arg_values.append(tc.allocator, arg_value) catch {
            return null;
        };
    }

    // Check if we need to create a new interpreter (outermost call)
    const is_outermost = tc.comptime_interpreter == null;
    if (is_outermost) {
        // Create the shared interpreter
        const interp = tc.allocator.create(Interpreter) catch {
            tc.addError(.comptime_error, call.span, "failed to allocate interpreter for comptime function", .{});
            return null;
        };
        interp.* = Interpreter.init(tc.allocator) catch {
            tc.allocator.destroy(interp);
            tc.addError(.comptime_error, call.span, "failed to create interpreter for comptime function", .{});
            return null;
        };
        tc.comptime_interpreter = interp;

        // Populate with constants from outer scope
        populateInterpreterEnv(tc, interp);

        // Register all comptime functions in the interpreter
        registerComptimeFunctionsInInterpreter(tc, interp);
    }

    const interp = tc.comptime_interpreter.?;

    // Track recursion depth
    tc.comptime_depth += 1;
    defer tc.comptime_depth -= 1;

    // Cleanup interpreter when outermost call completes
    defer if (is_outermost) {
        interp.deinit();
        tc.allocator.destroy(interp);
        tc.comptime_interpreter = null;
    };

    // Get the function body
    const body = func_decl.body orelse {
        tc.addError(.comptime_error, call.span, "comptime function must have a body", .{});
        return null;
    };

    // Create a new environment for this function call (for proper parameter scoping)
    const old_env = interp.current_env;
    const func_env = tc.allocator.create(values.Environment) catch {
        tc.addError(.comptime_error, call.span, "failed to create function environment", .{});
        return null;
    };
    func_env.* = values.Environment.init(tc.allocator, interp.global_env);
    interp.current_env = func_env;

    defer {
        interp.current_env = old_env;
        func_env.deinit();
        tc.allocator.destroy(func_env);
    }

    // Bind parameters to argument values in the function environment
    for (func_decl.params, arg_values.items) |param, arg_val| {
        interp.current_env.define(param.name, arg_val, false) catch {
            tc.addError(.comptime_error, call.span, "failed to bind comptime function parameter", .{});
            return null;
        };
    }

    // Clear return value from any previous call
    interp.return_value = null;

    // Evaluate function body
    const block_result = interp.evalBlock(body) catch |err| {
        // Convert interpreter errors to compile errors
        if (err == error.RuntimeError) {
            tc.addError(.comptime_error, call.span, "comptime function evaluation failed", .{});
        } else if (err == error.ComptimeError) {
            // @compileError was called
            // Error message already written to stdout by interpreter
        }
        return null;
    };

    // Check for return value (set when return statement was executed)
    // If there's a return_value, use that; otherwise use the block result
    const result = if (interp.return_value) |rv| rv else block_result;

    // Clear the return value so it doesn't affect other calls
    interp.return_value = null;

    // Convert interpreter Value to ComptimeValue
    return tc.valueToComptimeValue(result, call.span);
}

/// Register all comptime functions in the interpreter's environment.
/// This enables recursive calls by making functions available for lookup.
pub fn registerComptimeFunctionsInInterpreter(tc: anytype, interp: *Interpreter) void {
    var iter = tc.comptime_functions.iterator();
    while (iter.next()) |entry| {
        const func_name = entry.key_ptr.*;
        const func_decl = entry.value_ptr.*;

        if (func_decl.body) |body| {
            // Create a FunctionValue for this comptime function
            const func_val = tc.allocator.create(values.FunctionValue) catch continue;

            // Convert params
            var params = std.ArrayListUnmanaged(values.FunctionValue.FunctionParam){};
            for (func_decl.params) |param| {
                params.append(tc.allocator, .{ .name = param.name }) catch continue;
            }

            const params_slice = params.toOwnedSlice(tc.allocator) catch continue;

            func_val.* = .{
                .name = func_name,
                .params = params_slice,
                .body = body,
                .closure_env = interp.global_env,
            };

            // Track for cleanup
            interp.allocated_functions.append(tc.allocator, func_val) catch continue;

            // Register in global environment
            interp.global_env.define(func_name, .{ .function = func_val }, false) catch continue;
        }
    }
}

/// Evaluate a comptime function call via @name(...) builtin syntax.
/// Similar to evaluateComptimeCall but handles BuiltinCall arguments.
pub fn evaluateComptimeCallBuiltin(tc: anytype, bc: *ast.BuiltinCall, func_decl: *ast.FunctionDecl) ?ComptimeValue {
    // Check recursion depth limit
    if (tc.comptime_depth >= max_comptime_depth) {
        tc.addError(.comptime_error, bc.span, "comptime recursion depth limit exceeded (max {d})", .{max_comptime_depth});
        return null;
    }

    // First, check that all arguments are compile-time known
    var arg_values = std.ArrayListUnmanaged(Value){};
    defer arg_values.deinit(tc.allocator);

    for (bc.args) |arg| {
        const arg_expr = switch (arg) {
            .expr_arg => |e| e,
            .type_arg => {
                tc.addError(.comptime_error, bc.span, "type arguments not supported in comptime function calls", .{});
                return null;
            },
        };
        // Try to evaluate the argument at compile time
        const arg_value = evaluateComptimeExpr(tc, arg_expr) orelse {
            tc.addError(.comptime_error, arg_expr.span(), "argument to comptime function must be comptime-known", .{});
            return null;
        };
        arg_values.append(tc.allocator, arg_value) catch {
            return null;
        };
    }

    // Check if we need to create a new interpreter (outermost call)
    const is_outermost = tc.comptime_interpreter == null;
    if (is_outermost) {
        // Create the shared interpreter
        const interp = tc.allocator.create(Interpreter) catch {
            tc.addError(.comptime_error, bc.span, "failed to allocate interpreter for comptime function", .{});
            return null;
        };
        interp.* = Interpreter.init(tc.allocator) catch {
            tc.allocator.destroy(interp);
            tc.addError(.comptime_error, bc.span, "failed to create interpreter for comptime function", .{});
            return null;
        };
        tc.comptime_interpreter = interp;

        // Populate with constants from outer scope
        populateInterpreterEnv(tc, interp);

        // Register all comptime functions in the interpreter
        registerComptimeFunctionsInInterpreter(tc, interp);
    }

    const interp = tc.comptime_interpreter.?;

    // Track recursion depth
    tc.comptime_depth += 1;
    defer tc.comptime_depth -= 1;

    // Cleanup interpreter when outermost call completes
    defer if (is_outermost) {
        interp.deinit();
        tc.allocator.destroy(interp);
        tc.comptime_interpreter = null;
    };

    // Get the function body
    const body = func_decl.body orelse {
        tc.addError(.comptime_error, bc.span, "comptime function must have a body", .{});
        return null;
    };

    // Create a new environment for this function call (for proper parameter scoping)
    const old_env = interp.current_env;
    const func_env = tc.allocator.create(values.Environment) catch {
        tc.addError(.comptime_error, bc.span, "failed to create function environment", .{});
        return null;
    };
    func_env.* = values.Environment.init(tc.allocator, interp.global_env);
    interp.current_env = func_env;

    defer {
        interp.current_env = old_env;
        func_env.deinit();
        tc.allocator.destroy(func_env);
    }

    // Bind parameters to argument values in the function environment
    for (func_decl.params, arg_values.items) |param, arg_val| {
        interp.current_env.define(param.name, arg_val, false) catch {
            tc.addError(.comptime_error, bc.span, "failed to bind comptime function parameter", .{});
            return null;
        };
    }

    // Clear return value from any previous call
    interp.return_value = null;

    // Evaluate function body
    const block_result = interp.evalBlock(body) catch |err| {
        // Convert interpreter errors to compile errors
        if (err == error.RuntimeError) {
            tc.addError(.comptime_error, bc.span, "comptime function evaluation failed", .{});
        } else if (err == error.ComptimeError) {
            // @compileError was called
            // Error message already written to stdout by interpreter
        }
        return null;
    };

    // Check for return value (set when return statement was executed)
    // If there's a return_value, use that; otherwise use the block result
    const result = if (interp.return_value) |rv| rv else block_result;

    // Clear the return value so it doesn't affect other calls
    interp.return_value = null;

    // Convert interpreter Value to ComptimeValue
    return tc.valueToComptimeValue(result, bc.span);
}

/// Evaluate an expression at compile time to get an interpreter Value.
/// Returns null if the expression is not comptime-known.
pub fn evaluateComptimeExpr(tc: anytype, expr: ast.Expr) ?Value {
    return switch (expr) {
        .literal => |lit| switch (lit.kind) {
            .int => |v| .{ .int = values.Integer{ .value = v, .type_ = .i64_ } },
            .float => |v| .{ .float = values.Float{ .value = v, .type_ = .f64_ } },
            .bool_ => |v| .{ .bool_ = v },
            .string => |v| .{ .string = v },
            .char => |v| .{ .char_ = v },
        },
        .unary => |u| {
            const operand = evaluateComptimeExpr(tc, u.operand) orelse return null;
            switch (u.op) {
                .negate => {
                    if (operand == .int) return .{ .int = values.Integer{ .value = -operand.int.value, .type_ = operand.int.type_ } };
                    if (operand == .float) return .{ .float = values.Float{ .value = -operand.float.value, .type_ = operand.float.type_ } };
                    return null;
                },
                .not => {
                    if (operand == .bool_) return .{ .bool_ = !operand.bool_ };
                    return null;
                },
                else => return null,
            }
        },
        .binary => |b| {
            const left = evaluateComptimeExpr(tc, b.left) orelse return null;
            const right = evaluateComptimeExpr(tc, b.right) orelse return null;
            return evaluateComptimeBinaryOp(b.op, left, right);
        },
        .identifier => |ident| {
            // Check if this identifier refers to a const with a known comptime value
            if (tc.constant_values.get(ident.name)) |cv| {
                return comptimeValueToInterpreterValue(tc, cv);
            }
            return null;
        },
        .call => |c| {
            // Check if calling another comptime function
            if (c.callee == .identifier) {
                const func_name = c.callee.identifier.name;
                if (tc.comptime_functions.get(func_name)) |comptime_func| {
                    const result = evaluateComptimeCall(tc, c, comptime_func);
                    if (result) |cv| {
                        return comptimeValueToInterpreterValue(tc, cv);
                    }
                }
            }
            return null;
        },
        .builtin_call => |bc| {
            // Check if this is a comptime function call via @name(...) syntax
            if (tc.comptime_functions.get(bc.name)) |comptime_func| {
                // Delegate to evaluateComptimeCallBuiltin which uses the shared interpreter
                const result = evaluateComptimeCallBuiltin(tc, bc, comptime_func);
                if (result) |cv| {
                    return comptimeValueToInterpreterValue(tc, cv);
                }
            }
            return null;
        },
        .array_literal => |arr| {
            // Evaluate each element at compile time
            var elements = std.ArrayListUnmanaged(Value){};
            for (arr.elements) |elem| {
                const val = evaluateComptimeExpr(tc, elem) orelse return null;
                elements.append(tc.allocator, val) catch return null;
            }
            // Create an interpreter array value (heap-allocated)
            const array_val = tc.allocator.create(values.ArrayValue) catch return null;
            array_val.* = .{ .elements = elements.items };
            return .{ .array = array_val };
        },
        else => null,
    };
}

/// Evaluate a binary operation at compile time.
fn evaluateComptimeBinaryOp(op: ast.BinaryOp, left: Value, right: Value) ?Value {
    // Integer operations
    if (left == .int and right == .int) {
        const l = left.int.value;
        const r = right.int.value;
        const t = left.int.type_;
        return switch (op) {
            .add => .{ .int = values.Integer{ .value = l + r, .type_ = t } },
            .sub => .{ .int = values.Integer{ .value = l - r, .type_ = t } },
            .mul => .{ .int = values.Integer{ .value = l * r, .type_ = t } },
            .div => if (r != 0) .{ .int = values.Integer{ .value = @divTrunc(l, r), .type_ = t } } else null,
            .mod => if (r != 0) .{ .int = values.Integer{ .value = @mod(l, r), .type_ = t } } else null,
            .eq => .{ .bool_ = l == r },
            .not_eq => .{ .bool_ = l != r },
            .lt => .{ .bool_ = l < r },
            .lt_eq => .{ .bool_ = l <= r },
            .gt => .{ .bool_ = l > r },
            .gt_eq => .{ .bool_ = l >= r },
            else => null,
        };
    }
    // Float operations
    if ((left == .float or left == .int) and (right == .float or right == .int)) {
        const l: f64 = if (left == .float) left.float.value else @floatFromInt(left.int.value);
        const r: f64 = if (right == .float) right.float.value else @floatFromInt(right.int.value);
        return switch (op) {
            .add => .{ .float = values.Float{ .value = l + r, .type_ = .f64_ } },
            .sub => .{ .float = values.Float{ .value = l - r, .type_ = .f64_ } },
            .mul => .{ .float = values.Float{ .value = l * r, .type_ = .f64_ } },
            .div => if (r != 0) .{ .float = values.Float{ .value = l / r, .type_ = .f64_ } } else null,
            .eq => .{ .bool_ = l == r },
            .not_eq => .{ .bool_ = l != r },
            .lt => .{ .bool_ = l < r },
            .lt_eq => .{ .bool_ = l <= r },
            .gt => .{ .bool_ = l > r },
            .gt_eq => .{ .bool_ = l >= r },
            else => null,
        };
    }
    // Boolean operations
    if (left == .bool_ and right == .bool_) {
        const l = left.bool_;
        const r = right.bool_;
        return switch (op) {
            .and_ => .{ .bool_ = l and r },
            .or_ => .{ .bool_ = l or r },
            .eq => .{ .bool_ = l == r },
            .not_eq => .{ .bool_ = l != r },
            else => null,
        };
    }
    // String operations
    if (left == .string and right == .string) {
        return switch (op) {
            .eq => .{ .bool_ = std.mem.eql(u8, left.string, right.string) },
            .not_eq => .{ .bool_ = !std.mem.eql(u8, left.string, right.string) },
            else => null,
        };
    }
    return null;
}

/// Convert an interpreter Value to a ComptimeValue.
pub fn valueToComptimeValue(tc: anytype, value: Value, span: ast.Span) ?ComptimeValue {
    return switch (value) {
        .int => |v| blk: {
            const int_val = v.value;
            // Clamp to i64 range for ComptimeValue
            const clamped: i64 = if (int_val > std.math.maxInt(i64))
                std.math.maxInt(i64)
            else if (int_val < std.math.minInt(i64))
                std.math.minInt(i64)
            else
                @intCast(int_val);
            break :blk .{ .int = .{ .value = clamped, .is_i32 = clamped >= std.math.minInt(i32) and clamped <= std.math.maxInt(i32) } };
        },
        .float => |v| .{ .float = v.value },
        .bool_ => |v| .{ .bool_ = v },
        .string => |v| blk: {
            // Need to duplicate the string as interpreter may free it
            const duped = tc.allocator.dupe(u8, v) catch {
                tc.addError(.comptime_error, span, "failed to allocate comptime string", .{});
                break :blk null;
            };
            break :blk .{ .string = duped };
        },
        .void_ => .{ .void_ = {} },
        .struct_ => |sv| blk: {
            // Convert struct value to comptime struct
            var comptime_fields = std.StringArrayHashMapUnmanaged(ComptimeValue){};
            var iter = sv.fields.iterator();
            while (iter.next()) |entry| {
                // Recursively convert field value
                const field_comptime_value = valueToComptimeValue(tc, entry.value_ptr.*, span) orelse {
                    // Clean up on failure
                    comptime_fields.deinit(tc.allocator);
                    break :blk null;
                };
                // Duplicate field name to ensure ownership
                const field_name = tc.allocator.dupe(u8, entry.key_ptr.*) catch {
                    comptime_fields.deinit(tc.allocator);
                    break :blk null;
                };
                comptime_fields.put(tc.allocator, field_name, field_comptime_value) catch {
                    tc.allocator.free(field_name);
                    comptime_fields.deinit(tc.allocator);
                    break :blk null;
                };
            }
            // Duplicate type name
            const type_name = tc.allocator.dupe(u8, sv.type_name) catch {
                comptime_fields.deinit(tc.allocator);
                break :blk null;
            };
            break :blk .{ .struct_ = .{ .type_name = type_name, .fields = comptime_fields } };
        },
        .array => |arr| blk: {
            // Convert array value to comptime array
            if (arr.elements.len == 0) {
                tc.addError(.comptime_error, span, "empty arrays not supported in comptime blocks", .{});
                break :blk null;
            }

            // Recursively convert all elements
            var comptime_elements = std.ArrayListUnmanaged(ComptimeValue){};
            for (arr.elements) |elem| {
                const elem_comptime = valueToComptimeValue(tc, elem, span) orelse {
                    comptime_elements.deinit(tc.allocator);
                    break :blk null;
                };
                comptime_elements.append(tc.allocator, elem_comptime) catch {
                    comptime_elements.deinit(tc.allocator);
                    break :blk null;
                };
            }

            // Infer element type from first element
            const element_type = inferTypeFromComptimeValue(tc, comptime_elements.items[0]);

            break :blk .{ .array = .{
                .element_type = element_type,
                .elements = comptime_elements.toOwnedSlice(tc.allocator) catch {
                    comptime_elements.deinit(tc.allocator);
                    break :blk null;
                },
            } };
        },
        else => {
            tc.addError(.comptime_error, span, "comptime block produced a non-primitive value", .{});
            return null;
        },
    };
}

/// Infer the Klar Type from a ComptimeValue.
pub fn inferTypeFromComptimeValue(tc: anytype, cv: ComptimeValue) Type {
    return switch (cv) {
        .int => |i| if (i.is_i32) tc.type_builder.i32Type() else tc.type_builder.i64Type(),
        .float => tc.type_builder.f64Type(),
        .bool_ => tc.type_builder.boolType(),
        .string => tc.type_builder.stringType(),
        .void_ => tc.type_builder.voidType(),
        .struct_ => |cs| blk: {
            if (tc.lookupType(cs.type_name)) |t| {
                break :blk t;
            }
            break :blk tc.type_builder.unknownType();
        },
        .array => |arr| blk: {
            // Recursively infer element type
            const elem_type = inferTypeFromComptimeValue(tc, arr.elements[0]);
            break :blk tc.type_builder.arrayType(elem_type, arr.elements.len) catch tc.type_builder.unknownType();
        },
    };
}

/// Convert a ComptimeValue back to an interpreter Value.
pub fn comptimeValueToInterpreterValue(tc: anytype, cv: ComptimeValue) Value {
    return switch (cv) {
        .int => |i| .{ .int = values.Integer{ .value = i.value, .type_ = if (i.is_i32) .i32_ else .i64_ } },
        .float => |f| .{ .float = values.Float{ .value = f, .type_ = .f64_ } },
        .bool_ => |b| .{ .bool_ = b },
        .string => |s| .{ .string = s },
        .void_ => .{ .void_ = {} },
        .struct_ => |cs| blk: {
            // Convert comptime struct back to interpreter struct value
            const sv = tc.allocator.create(values.StructValue) catch {
                break :blk .{ .void_ = {} }; // Fallback on allocation failure
            };
            sv.* = .{
                .type_name = cs.type_name,
                .fields = .{},
            };
            // Convert each field
            var iter = cs.fields.iterator();
            while (iter.next()) |entry| {
                const field_value = comptimeValueToInterpreterValue(tc, entry.value_ptr.*);
                sv.fields.put(tc.allocator, entry.key_ptr.*, field_value) catch {};
            }
            break :blk .{ .struct_ = sv };
        },
        .array => |arr| blk: {
            // Convert comptime array back to interpreter array value (heap-allocated)
            var elements = std.ArrayListUnmanaged(Value){};
            for (arr.elements) |elem| {
                const elem_value = comptimeValueToInterpreterValue(tc, elem);
                elements.append(tc.allocator, elem_value) catch {
                    break :blk .{ .void_ = {} };
                };
            }
            const array_val = tc.allocator.create(values.ArrayValue) catch {
                break :blk .{ .void_ = {} };
            };
            array_val.* = .{ .elements = elements.items };
            break :blk .{ .array = array_val };
        },
    };
}
