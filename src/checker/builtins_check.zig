//! Builtin function type checking.
//!
//! This module contains functions for type-checking Klar builtin functions
//! like @typeName, @typeInfo, @fields, @assert, @repeat, @fn_ptr, etc.

const std = @import("std");
const ast = @import("../ast.zig");
const types = @import("../types.zig");
const Type = types.Type;
const values = @import("../values.zig");
const Value = values.Value;

// ============================================================================
// Builtin Call Checking
// ============================================================================

pub fn checkBuiltinCall(tc: anytype, builtin: *ast.BuiltinCall) Type {
    // First, check if this is a user-defined comptime function call
    if (tc.comptime_functions.get(builtin.name)) |comptime_func| {
        return checkComptimeFunctionCallFromBuiltin(tc, builtin, comptime_func);
    }

    if (std.mem.eql(u8, builtin.name, "typeName")) {
        return checkBuiltinTypeName(tc, builtin);
    } else if (std.mem.eql(u8, builtin.name, "typeInfo")) {
        return checkBuiltinTypeInfo(tc, builtin);
    } else if (std.mem.eql(u8, builtin.name, "fields")) {
        return checkBuiltinFields(tc, builtin);
    } else if (std.mem.eql(u8, builtin.name, "compileError")) {
        return checkBuiltinCompileError(tc, builtin);
    } else if (std.mem.eql(u8, builtin.name, "hasField")) {
        return checkBuiltinHasField(tc, builtin);
    } else if (std.mem.eql(u8, builtin.name, "sizeOf")) {
        return checkBuiltinSizeOf(tc, builtin);
    } else if (std.mem.eql(u8, builtin.name, "alignOf")) {
        return checkBuiltinAlignOf(tc, builtin);
    } else if (std.mem.eql(u8, builtin.name, "assert")) {
        return checkBuiltinAssert(tc, builtin);
    } else if (std.mem.eql(u8, builtin.name, "repeat")) {
        return checkBuiltinRepeat(tc, builtin);
    } else if (std.mem.eql(u8, builtin.name, "fn_ptr")) {
        return checkBuiltinFnPtr(tc, builtin);
    } else {
        tc.addError(.undefined_function, builtin.span, "unknown builtin '@{s}'", .{builtin.name});
        return tc.type_builder.unknownType();
    }
}

/// Handle a user-defined comptime function call using @name(...) syntax
fn checkComptimeFunctionCallFromBuiltin(tc: anytype, builtin: *ast.BuiltinCall, func_decl: *ast.FunctionDecl) Type {
    // Check argument count
    if (builtin.args.len != func_decl.params.len) {
        tc.addError(.wrong_number_of_args, builtin.span, "@{s} expects {d} argument(s), got {d}", .{ builtin.name, func_decl.params.len, builtin.args.len });
        return tc.type_builder.unknownType();
    }

    // If we're inside a comptime function body, just type-check arguments
    // without trying to evaluate them. The interpreter will handle evaluation.
    if (tc.checking_comptime_function_body) {
        // Type-check arguments against parameter types
        for (builtin.args, 0..) |arg, i| {
            const expr = switch (arg) {
                .expr_arg => |e| e,
                .type_arg => {
                    tc.addError(.type_mismatch, builtin.span, "comptime function expects expression arguments, not types", .{});
                    return tc.type_builder.unknownType();
                },
            };

            const arg_type = tc.checkExpr(expr);
            const param_type = tc.resolveTypeExpr(func_decl.params[i].type_) catch tc.type_builder.unknownType();
            if (!tc.isTypeCompatible(arg_type, param_type)) {
                tc.addError(.type_mismatch, expr.span(), "argument type mismatch", .{});
            }
        }
        // Return the function's return type without evaluating
        return checkFunctionDeclReturnType(tc, func_decl);
    }

    // Normal path: evaluate at compile time
    var arg_values = std.ArrayListUnmanaged(Value){};
    defer arg_values.deinit(tc.allocator);

    for (builtin.args) |arg| {
        const expr = switch (arg) {
            .expr_arg => |e| e,
            .type_arg => {
                tc.addError(.type_mismatch, builtin.span, "comptime function expects expression arguments, not types", .{});
                return tc.type_builder.unknownType();
            },
        };

        const arg_value = tc.evaluateComptimeExpr(expr) orelse {
            tc.addError(.comptime_error, expr.span(), "argument to comptime function must be comptime-known", .{});
            return tc.type_builder.unknownType();
        };
        arg_values.append(tc.allocator, arg_value) catch {
            return tc.type_builder.unknownType();
        };
    }

    // Use the shared interpreter approach from evaluateComptimeCallBuiltin
    const comptime_value = tc.evaluateComptimeCallBuiltin(builtin, func_decl) orelse {
        return tc.type_builder.unknownType();
    };

    // Store the result for codegen
    tc.comptime_builtin_values.put(tc.allocator, builtin, comptime_value) catch {};

    // Return the function's return type
    return checkFunctionDeclReturnType(tc, func_decl);
}

/// Get the return type of a function declaration
pub fn checkFunctionDeclReturnType(tc: anytype, func_decl: *ast.FunctionDecl) Type {
    if (func_decl.return_type) |ret_type| {
        return tc.resolveTypeExpr(ret_type) catch tc.type_builder.unknownType();
    }
    return tc.type_builder.voidType();
}

/// @typeName(T) -> string
fn checkBuiltinTypeName(tc: anytype, builtin: *ast.BuiltinCall) Type {
    if (builtin.args.len != 1) {
        tc.addError(.wrong_number_of_args, builtin.span, "@typeName expects 1 argument, got {d}", .{builtin.args.len});
        return tc.type_builder.unknownType();
    }

    // The argument must be a type
    const resolved_type: Type = switch (builtin.args[0]) {
        .type_arg => |type_expr| blk: {
            // Resolve the type expression to a concrete type
            const t = tc.resolveTypeExpr(type_expr) catch {
                return tc.type_builder.unknownType();
            };
            break :blk t;
        },
        .expr_arg => {
            tc.addError(.type_mismatch, builtin.span, "@typeName expects a type argument, not an expression", .{});
            return tc.type_builder.unknownType();
        },
    };

    // Format the type to a string and store it for codegen
    var buf = std.ArrayListUnmanaged(u8){};
    types.formatType(buf.writer(tc.allocator), resolved_type) catch {
        buf.deinit(tc.allocator);
        return tc.type_builder.stringType();
    };

    // Store the computed string value for codegen
    const type_name = buf.toOwnedSlice(tc.allocator) catch {
        buf.deinit(tc.allocator);
        return tc.type_builder.stringType();
    };
    tc.comptime_strings.put(tc.allocator, builtin, type_name) catch {
        tc.allocator.free(type_name);
    };

    return tc.type_builder.stringType();
}

/// @typeInfo(T) -> string describing the type kind
fn checkBuiltinTypeInfo(tc: anytype, builtin: *ast.BuiltinCall) Type {
    if (builtin.args.len != 1) {
        tc.addError(.wrong_number_of_args, builtin.span, "@typeInfo expects 1 argument, got {d}", .{builtin.args.len});
        return tc.type_builder.unknownType();
    }

    const type_expr = switch (builtin.args[0]) {
        .type_arg => |te| te,
        .expr_arg => {
            tc.addError(.type_mismatch, builtin.span, "@typeInfo expects a type argument, not an expression", .{});
            return tc.type_builder.unknownType();
        },
    };

    // Resolve the type expression
    const resolved_type = tc.resolveTypeExpr(type_expr) catch {
        return tc.type_builder.unknownType();
    };

    // Determine the type kind string
    const type_kind_literal: []const u8 = switch (resolved_type) {
        .primitive => "primitive",
        .struct_ => "struct",
        .enum_ => "enum",
        .trait_ => "trait",
        .array => "array",
        .slice => "slice",
        .tuple => "tuple",
        .optional => "optional",
        .result => "result",
        .function => "function",
        .reference => "reference",
        .type_var => "type_var",
        .applied => "applied",
        .associated_type_ref => "associated_type",
        .rc => "rc",
        .weak_rc => "weak_rc",
        .arc => "arc",
        .weak_arc => "weak_arc",
        .cell => "cell",
        .context_error => "context_error",
        .range => "range",
        .list => "list",
        .map => "map",
        .set => "set",
        .string_data => "string",
        .file => "file",
        .io_error => "io_error",
        .stdout_handle => "stdout",
        .stderr_handle => "stderr",
        .stdin_handle => "stdin",
        .path => "path",
        .buf_reader => "buf_reader",
        .buf_writer => "buf_writer",
        .extern_type => "extern",
        .cptr => "cptr",
        .copt_ptr => "copt_ptr",
        .cstr => "cstr",
        .cstr_owned => "cstr_owned",
        .extern_fn => "extern_fn",
        .void_ => "void",
        .never => "never",
        .unknown => "unknown",
        .error_type => "error",
    };

    // Duplicate the string literal so it can be freed during deinit
    const type_kind = tc.allocator.dupe(u8, type_kind_literal) catch {
        return tc.type_builder.stringType();
    };

    // Store the type kind string for codegen
    tc.comptime_strings.put(tc.allocator, builtin, type_kind) catch {
        tc.allocator.free(type_kind);
    };

    return tc.type_builder.stringType();
}

/// @fields(T) -> string with comma-separated field names
fn checkBuiltinFields(tc: anytype, builtin: *ast.BuiltinCall) Type {
    if (builtin.args.len != 1) {
        tc.addError(.wrong_number_of_args, builtin.span, "@fields expects 1 argument, got {d}", .{builtin.args.len});
        return tc.type_builder.unknownType();
    }

    const type_expr = switch (builtin.args[0]) {
        .type_arg => |te| te,
        .expr_arg => {
            tc.addError(.type_mismatch, builtin.span, "@fields expects a type argument, not an expression", .{});
            return tc.type_builder.unknownType();
        },
    };

    // Resolve the type expression
    const resolved_type = tc.resolveTypeExpr(type_expr) catch {
        return tc.type_builder.unknownType();
    };

    // Get fields for struct types - always allocate so deinit can free
    const fields_str: []u8 = switch (resolved_type) {
        .struct_ => |s| blk: {
            if (s.fields.len == 0) {
                break :blk tc.allocator.alloc(u8, 0) catch {
                    return tc.type_builder.stringType();
                };
            }
            // Build comma-separated field names
            var total_len: usize = 0;
            for (s.fields) |field| {
                total_len += field.name.len + 1; // +1 for comma
            }
            total_len -= 1; // No trailing comma

            const result = tc.allocator.alloc(u8, total_len) catch {
                return tc.type_builder.stringType();
            };
            var pos: usize = 0;
            for (s.fields, 0..) |field, i| {
                @memcpy(result[pos..][0..field.name.len], field.name);
                pos += field.name.len;
                if (i < s.fields.len - 1) {
                    result[pos] = ',';
                    pos += 1;
                }
            }
            break :blk result;
        },
        .enum_ => |e| blk: {
            // For enums, return variant names
            if (e.variants.len == 0) {
                break :blk tc.allocator.alloc(u8, 0) catch {
                    return tc.type_builder.stringType();
                };
            }
            var total_len: usize = 0;
            for (e.variants) |variant| {
                total_len += variant.name.len + 1;
            }
            total_len -= 1;

            const result = tc.allocator.alloc(u8, total_len) catch {
                return tc.type_builder.stringType();
            };
            var pos: usize = 0;
            for (e.variants, 0..) |variant, i| {
                @memcpy(result[pos..][0..variant.name.len], variant.name);
                pos += variant.name.len;
                if (i < e.variants.len - 1) {
                    result[pos] = ',';
                    pos += 1;
                }
            }
            break :blk result;
        },
        else => tc.allocator.alloc(u8, 0) catch {
            return tc.type_builder.stringType();
        },
    };

    // Store the fields string for codegen
    tc.comptime_strings.put(tc.allocator, builtin, fields_str) catch {
        tc.allocator.free(fields_str);
    };

    return tc.type_builder.stringType();
}

/// @compileError("message") -> never returns (compile error)
fn checkBuiltinCompileError(tc: anytype, builtin: *ast.BuiltinCall) Type {
    if (builtin.args.len != 1) {
        tc.addError(.wrong_number_of_args, builtin.span, "@compileError expects 1 argument, got {d}", .{builtin.args.len});
        return tc.type_builder.unknownType();
    }

    switch (builtin.args[0]) {
        .expr_arg => |expr| {
            // Check if the expression is a string literal
            switch (expr) {
                .literal => |lit| {
                    switch (lit.kind) {
                        .string => |msg| {
                            // Emit the compile error with the user's message
                            tc.addError(.comptime_error, builtin.span, "{s}", .{msg});
                        },
                        else => {
                            tc.addError(.type_mismatch, builtin.span, "@compileError expects a string literal", .{});
                        },
                    }
                },
                else => {
                    tc.addError(.type_mismatch, builtin.span, "@compileError expects a string literal", .{});
                },
            }
        },
        .type_arg => {
            tc.addError(.type_mismatch, builtin.span, "@compileError expects a string, not a type", .{});
        },
    }

    return tc.type_builder.neverType();
}

/// @assert(condition) or @assert(condition, "message")
fn checkBuiltinAssert(tc: anytype, builtin: *ast.BuiltinCall) Type {
    if (builtin.args.len == 0 or builtin.args.len > 2) {
        tc.addError(.wrong_number_of_args, builtin.span, "@assert expects 1 or 2 arguments, got {d}", .{builtin.args.len});
        return tc.type_builder.unknownType();
    }

    // First arg must be a comptime-known boolean expression
    const condition_expr = switch (builtin.args[0]) {
        .expr_arg => |expr| expr,
        .type_arg => {
            tc.addError(.type_mismatch, builtin.span, "@assert first argument must be a boolean expression, not a type", .{});
            return tc.type_builder.voidType();
        },
    };

    // Try to evaluate the condition at compile time
    const condition_value = tc.evaluateComptimeExpr(condition_expr);
    if (condition_value == null) {
        tc.addError(.comptime_error, builtin.span, "@assert condition must be comptime-known", .{});
        return tc.type_builder.voidType();
    }

    // Check that it's a boolean
    if (condition_value.? != .bool_) {
        tc.addError(.type_mismatch, builtin.span, "@assert condition must be a boolean, not {s}", .{@tagName(condition_value.?)});
        return tc.type_builder.voidType();
    }

    // If condition is false, emit compile error
    if (!condition_value.?.bool_) {
        // Check for optional message argument
        if (builtin.args.len == 2) {
            switch (builtin.args[1]) {
                .expr_arg => |expr| {
                    switch (expr) {
                        .literal => |lit| {
                            switch (lit.kind) {
                                .string => |msg| {
                                    tc.addError(.comptime_error, builtin.span, "assertion failed: {s}", .{msg});
                                    return tc.type_builder.voidType();
                                },
                                else => {},
                            }
                        },
                        else => {},
                    }
                },
                .type_arg => {},
            }
            // If we get here, message wasn't a string literal
            tc.addError(.type_mismatch, builtin.span, "@assert message must be a string literal", .{});
        } else {
            tc.addError(.comptime_error, builtin.span, "assertion failed", .{});
        }
    }

    return tc.type_builder.voidType();
}

/// @repeat(value, count) -> [typeof(value); count]
fn checkBuiltinRepeat(tc: anytype, builtin: *ast.BuiltinCall) Type {
    if (builtin.args.len != 2) {
        tc.addError(.wrong_number_of_args, builtin.span, "@repeat expects 2 arguments (value, count), got {d}", .{builtin.args.len});
        return tc.type_builder.unknownType();
    }

    // First arg is the value to repeat - must be an expression
    const value_expr = switch (builtin.args[0]) {
        .expr_arg => |expr| expr,
        .type_arg => {
            tc.addError(.type_mismatch, builtin.span, "@repeat first argument must be a value, not a type", .{});
            return tc.type_builder.unknownType();
        },
    };

    // Type-check the value expression
    const element_type = tc.checkExpr(value_expr);
    if (element_type == .unknown or element_type == .error_type) {
        return tc.type_builder.unknownType();
    }

    // Second arg is the count - must be a comptime-known integer
    const count_expr = switch (builtin.args[1]) {
        .expr_arg => |expr| expr,
        .type_arg => {
            tc.addError(.type_mismatch, builtin.span, "@repeat second argument must be an integer, not a type", .{});
            return tc.type_builder.unknownType();
        },
    };

    // Evaluate count at compile time
    const count_value = tc.evaluateComptimeExpr(count_expr);
    if (count_value == null) {
        tc.addError(.comptime_error, builtin.span, "@repeat count must be comptime-known", .{});
        return tc.type_builder.unknownType();
    }

    // Check that it's an integer
    const count: usize = switch (count_value.?) {
        .int => |i| blk: {
            if (i.value < 0) {
                tc.addError(.type_mismatch, builtin.span, "@repeat count must be non-negative, got {d}", .{i.value});
                return tc.type_builder.unknownType();
            }
            break :blk @intCast(i.value);
        },
        else => {
            tc.addError(.type_mismatch, builtin.span, "@repeat count must be an integer", .{});
            return tc.type_builder.unknownType();
        },
    };

    // Store repeat info for codegen
    tc.comptime_repeats.put(tc.allocator, builtin, .{
        .element_type = element_type,
        .count = count,
        .value_expr = value_expr,
    }) catch {};

    // Return the array type
    return tc.type_builder.arrayType(element_type, count) catch tc.type_builder.unknownType();
}

/// @fn_ptr(func) -> extern fn(Params) -> Return
fn checkBuiltinFnPtr(tc: anytype, builtin: *ast.BuiltinCall) Type {
    if (builtin.args.len != 1) {
        tc.addError(.wrong_number_of_args, builtin.span, "@fn_ptr expects 1 argument, got {d}", .{builtin.args.len});
        return tc.type_builder.unknownType();
    }

    // Argument must be an expression (not a type)
    const arg_expr = switch (builtin.args[0]) {
        .expr_arg => |expr| expr,
        .type_arg => {
            tc.addError(.type_mismatch, builtin.span, "@fn_ptr expects a function or closure, not a type", .{});
            return tc.type_builder.unknownType();
        },
    };

    // Type-check the expression
    const arg_type = tc.checkExpr(arg_expr);

    // Check if it's a closure and validate it has no captures
    if (arg_expr == .closure) {
        const closure = arg_expr.closure;
        if (closure.captures != null and closure.captures.?.len > 0) {
            tc.addError(.type_mismatch, builtin.span, "cannot create C function pointer from closure with captures", .{});
            return tc.type_builder.unknownType();
        }
        // Stateless closure - allowed
    } else if (arg_expr == .identifier) {
        // Named function - check that it resolves to a function
        if (arg_type != .function) {
            tc.addError(.type_mismatch, builtin.span, "@fn_ptr expects a function, got {s}", .{@tagName(arg_type)});
            return tc.type_builder.unknownType();
        }
        // Named function - allowed
    } else {
        // Not a closure or identifier - check if it's a function type
        if (arg_type != .function) {
            tc.addError(.type_mismatch, builtin.span, "@fn_ptr expects a function or stateless closure, got {s}", .{@tagName(arg_type)});
            return tc.type_builder.unknownType();
        }
    }

    // At this point, arg_type should be a function type
    if (arg_type != .function) {
        tc.addError(.type_mismatch, builtin.span, "@fn_ptr expects a function type", .{});
        return tc.type_builder.unknownType();
    }

    const func = arg_type.function;

    // Validate all parameter types and return type are FFI-compatible
    for (func.params, 0..) |param_type, i| {
        if (!tc.isFfiCompatibleType(param_type)) {
            const type_str = types.typeToString(tc.allocator, param_type) catch "unknown";
            tc.addError(.type_mismatch, builtin.span, "@fn_ptr: parameter {d} has non-FFI-compatible type '{s}'", .{ i + 1, type_str });
            return tc.type_builder.unknownType();
        }
    }

    // Check return type (void is always compatible)
    if (func.return_type != .void_ and !tc.isFfiCompatibleType(func.return_type)) {
        const type_str = types.typeToString(tc.allocator, func.return_type) catch "unknown";
        tc.addError(.type_mismatch, builtin.span, "@fn_ptr: return type '{s}' is not FFI-compatible", .{type_str});
        return tc.type_builder.unknownType();
    }

    // Build the extern fn type from the function signature
    return tc.type_builder.externFnType(func.params, func.return_type) catch tc.type_builder.unknownType();
}

/// @hasField(T, "field_name") -> bool
fn checkBuiltinHasField(tc: anytype, builtin: *ast.BuiltinCall) Type {
    if (builtin.args.len != 2) {
        tc.addError(.wrong_number_of_args, builtin.span, "@hasField expects 2 arguments, got {d}", .{builtin.args.len});
        return tc.type_builder.unknownType();
    }

    // First arg must be a type
    const type_arg = switch (builtin.args[0]) {
        .type_arg => |te| te,
        .expr_arg => {
            tc.addError(.type_mismatch, builtin.span, "@hasField first argument must be a type", .{});
            return tc.type_builder.unknownType();
        },
    };

    // Second arg must be a string literal
    const field_name: ?[]const u8 = switch (builtin.args[1]) {
        .expr_arg => |expr| blk: {
            switch (expr) {
                .literal => |lit| {
                    switch (lit.kind) {
                        .string => |s| break :blk s,
                        else => {},
                    }
                },
                else => {},
            }
            tc.addError(.type_mismatch, builtin.span, "@hasField second argument must be a string literal", .{});
            break :blk null;
        },
        .type_arg => blk: {
            tc.addError(.type_mismatch, builtin.span, "@hasField second argument must be a string, not a type", .{});
            break :blk null;
        },
    };

    // Evaluate at compile time
    if (field_name) |name| {
        const resolved_type = tc.resolveTypeExpr(type_arg) catch {
            return tc.type_builder.boolType();
        };
        const has_field = switch (resolved_type) {
            .struct_ => |s| blk: {
                for (s.fields) |field| {
                    if (std.mem.eql(u8, field.name, name)) {
                        break :blk true;
                    }
                }
                break :blk false;
            },
            else => false,
        };
        tc.comptime_bools.put(tc.allocator, builtin, has_field) catch {};
    }

    return tc.type_builder.boolType();
}

/// @sizeOf(T) -> i32 (size of type in bytes)
fn checkBuiltinSizeOf(tc: anytype, builtin: *ast.BuiltinCall) Type {
    if (builtin.args.len != 1) {
        tc.addError(.wrong_number_of_args, builtin.span, "@sizeOf expects 1 argument, got {d}", .{builtin.args.len});
        return tc.type_builder.unknownType();
    }

    const type_arg = switch (builtin.args[0]) {
        .type_arg => |ta| ta,
        .expr_arg => {
            tc.addError(.type_mismatch, builtin.span, "@sizeOf expects a type argument, not an expression", .{});
            return tc.type_builder.unknownType();
        },
    };

    const resolved_type = tc.resolveTypeExpr(type_arg) catch {
        return tc.type_builder.i32Type();
    };

    const size: i64 = computeTypeSize(tc, resolved_type);
    tc.comptime_ints.put(tc.allocator, builtin, size) catch {};

    return tc.type_builder.i32Type();
}

/// @alignOf(T) -> i32 (alignment of type in bytes)
fn checkBuiltinAlignOf(tc: anytype, builtin: *ast.BuiltinCall) Type {
    if (builtin.args.len != 1) {
        tc.addError(.wrong_number_of_args, builtin.span, "@alignOf expects 1 argument, got {d}", .{builtin.args.len});
        return tc.type_builder.unknownType();
    }

    const type_arg = switch (builtin.args[0]) {
        .type_arg => |ta| ta,
        .expr_arg => {
            tc.addError(.type_mismatch, builtin.span, "@alignOf expects a type argument, not an expression", .{});
            return tc.type_builder.unknownType();
        },
    };

    const resolved_type = tc.resolveTypeExpr(type_arg) catch {
        return tc.type_builder.i32Type();
    };

    const alignment: i64 = computeTypeAlignment(tc, resolved_type);
    tc.comptime_ints.put(tc.allocator, builtin, alignment) catch {};

    return tc.type_builder.i32Type();
}

/// Compute the size of a type in bytes
pub fn computeTypeSize(tc: anytype, typ: Type) i64 {
    return switch (typ) {
        .primitive => |p| switch (p) {
            .i8_, .u8_ => 1,
            .i16_, .u16_ => 2,
            .i32_, .u32_, .f32_ => 4,
            .i64_, .u64_, .f64_ => 8,
            .bool_ => 1,
            else => 8, // default for other primitives (i128, isize, etc.)
        },
        .void_ => 0,
        .reference, .function => 8, // 64-bit pointers
        .struct_ => |s| blk: {
            var total: i64 = 0;
            for (s.fields) |field| {
                total += computeTypeSize(tc, field.type_);
            }
            break :blk if (total == 0) 1 else total; // empty struct has size 1
        },
        .enum_ => 4, // enum tag
        .array => |a| computeTypeSize(tc, a.element) * @as(i64, @intCast(a.size)),
        .slice => 16, // pointer + length
        .optional => |o| computeTypeSize(tc, o.*) + 1, // child + tag byte
        else => 8, // default for unknown types
    };
}

/// Compute the alignment of a type in bytes
pub fn computeTypeAlignment(tc: anytype, typ: Type) i64 {
    return switch (typ) {
        .primitive => |p| switch (p) {
            .i8_, .u8_, .bool_ => 1,
            .i16_, .u16_ => 2,
            .i32_, .u32_, .f32_ => 4,
            .i64_, .u64_, .f64_ => 8,
            else => 8, // default for other primitives (i128, isize, etc.)
        },
        .void_ => 1,
        .reference, .function => 8, // 64-bit pointers
        .struct_ => |s| blk: {
            var max_align: i64 = 1;
            for (s.fields) |field| {
                const field_align = computeTypeAlignment(tc, field.type_);
                if (field_align > max_align) {
                    max_align = field_align;
                }
            }
            break :blk max_align;
        },
        .enum_ => 4,
        .array => |a| computeTypeAlignment(tc, a.element),
        .slice => 8, // pointer alignment
        .optional => |o| computeTypeAlignment(tc, o.*),
        else => 8,
    };
}
