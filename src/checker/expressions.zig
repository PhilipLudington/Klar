//! Expression type checking.
//!
//! This module contains functions for type-checking Klar expressions.
//! Functions receive a type checker instance via `anytype` to avoid
//! circular imports while maintaining type safety through duck typing.

const std = @import("std");
const ast = @import("../ast.zig");
const types = @import("../types.zig");
const Type = types.Type;
const interpreter = @import("../interpreter.zig");
const Interpreter = interpreter.Interpreter;
const values = @import("../values.zig");
const Value = values.Value;

// Import from checker for error types and submodule access
const checker_mod = @import("checker.zig");
const CheckError = checker_mod.CheckError;
const comptime_eval = @import("comptime_eval.zig");
const builtins_check = @import("builtins_check.zig");
const statements = @import("statements.zig");

// ============================================================================
// Expression Checking - Main Entry Points
// ============================================================================

pub fn checkExpr(tc: anytype, expr: ast.Expr) Type {
    return checkExprWithHint(tc, expr, null);
}

/// Check an expression with an optional type hint for contextual typing.
/// The hint is used for numeric literals to adopt the expected type.
pub fn checkExprWithHint(tc: anytype, expr: ast.Expr, hint: ?Type) Type {
    return switch (expr) {
        .literal => |l| checkLiteralWithHint(tc, l, hint),
        .identifier => |i| checkIdentifier(tc, i),
        .binary => |b| checkBinary(tc, b),
        .unary => |u| checkUnary(tc, u),
        .postfix => |p| checkPostfix(tc, p),
        .call => |c| blk: {
            // Set expected_type for Ok/Err type inference
            const prev_expected = tc.expected_type;
            tc.expected_type = hint;
            defer tc.expected_type = prev_expected;
            break :blk checkCall(tc, c);
        },
        .index => |i| checkIndex(tc, i),
        .field => |f| checkField(tc, f),
        .method_call => |m| checkMethodCall(tc, m),
        .block => |b| checkBlock(tc, b),
        .closure => |c| checkClosure(tc, c),
        .range => |r| checkRange(tc, r),
        .struct_literal => |s| checkStructLiteral(tc, s),
        .array_literal => |a| checkArrayLiteral(tc, a),
        .tuple_literal => |t| checkTupleLiteralWithHint(tc, t, hint),
        .type_cast => |tc_node| checkTypeCast(tc, tc_node),
        .grouped => |g| checkExprWithHint(tc, g.expr, hint),
        .interpolated_string => |is| checkInterpolatedString(tc, is),
        .enum_literal => |e| checkEnumLiteral(tc, e),
        .comptime_block => |cb| checkComptimeBlock(tc, cb),
        .builtin_call => |bc| builtins_check.checkBuiltinCall(tc, bc),
        .unsafe_block => |ub| checkUnsafeBlock(tc, ub),
        .out_arg => |oa| checkOutArg(tc, oa),
    };
}

// ============================================================================
// Literal Checking
// ============================================================================

fn checkLiteralWithHint(tc: anytype, lit: ast.Literal, hint: ?Type) Type {
    return switch (lit.kind) {
        .int => {
            // Use type hint if it's a compatible integer type
            if (hint) |h| {
                if (h == .primitive and h.primitive.isInteger()) {
                    return h;
                }
            }
            return tc.type_builder.i32Type(); // Default int type
        },
        .float => {
            // Use type hint if it's a compatible float type
            if (hint) |h| {
                if (h == .primitive and h.primitive.isFloat()) {
                    return h;
                }
            }
            return tc.type_builder.f64Type(); // Default float type
        },
        .string => tc.type_builder.stringType(),
        .char => tc.type_builder.charType(),
        .bool_ => tc.type_builder.boolType(),
    };
}

fn checkInterpolatedString(tc: anytype, interp: *ast.InterpolatedString) Type {
    // Type-check all embedded expressions (any type is allowed, will be converted to string)
    for (interp.parts) |part| {
        switch (part) {
            .string => {},
            .expr => |e| _ = checkExpr(tc, e),
        }
    }
    // Interpolated strings always produce a string
    return tc.type_builder.stringType();
}

// ============================================================================
// Identifier Checking
// ============================================================================

fn checkIdentifier(tc: anytype, id: ast.Identifier) Type {
    if (tc.current_scope.lookup(id.name)) |sym| {
        // Track captures if we're inside a closure
        if (tc.closure_scope) |closure_scope| {
            // Check if this variable is from outside the closure's scope
            // It's a capture if not found in closure scope's local symbols
            // and not found in any child scope of the closure
            if (!isInClosureScope(tc, closure_scope, id.name)) {
                // Skip functions and types - they don't need to be captured
                if (sym.kind == .variable or sym.kind == .parameter or sym.kind == .constant) {
                    tc.closure_captures.put(tc.allocator, id.name, .{
                        .is_mutable = sym.mutable,
                    }) catch {};
                }
            }
        }
        return sym.type_;
    }
    tc.addError(.undefined_variable, id.span, "undefined variable '{s}'", .{id.name});
    return tc.type_builder.unknownType();
}

/// Check if a variable is defined within the closure scope (not a capture).
fn isInClosureScope(tc: anytype, closure_scope: anytype, name: []const u8) bool {
    // Check the closure scope and all its child scopes (which are nested inside it)
    // Starting from current_scope, walk up to closure_scope checking each scope's locals
    var check_scope: ?@TypeOf(tc.current_scope) = tc.current_scope;
    while (check_scope) |s| {
        if (s.lookupLocal(name) != null) {
            return true;
        }
        // Stop when we've checked up to and including the closure scope
        if (s == closure_scope) {
            break;
        }
        check_scope = s.parent;
    }
    return false;
}

// ============================================================================
// Binary Expression Checking
// ============================================================================

fn checkBinary(tc: anytype, bin: *ast.Binary) Type {
    const left_type = checkExpr(tc, bin.left);
    const right_type = checkExpr(tc, bin.right);

    // Handle assignment operators
    switch (bin.op) {
        .assign, .add_assign, .sub_assign, .mul_assign, .div_assign, .mod_assign => {
            // Check that left side is assignable and mutable
            if (!tc.isAssignable(bin.left)) {
                tc.addError(.invalid_assignment, bin.span, "cannot assign to this expression", .{});
            }
            if (bin.op == .assign) {
                if (!tc.checkAssignmentCompatible(left_type, right_type)) {
                    tc.addError(.type_mismatch, bin.span, "cannot assign different types", .{});
                }
                return tc.type_builder.voidType();
            } else {
                // Compound assignment: check arithmetic compatibility
                if (!left_type.isNumeric() or !right_type.isNumeric()) {
                    tc.addError(.invalid_operation, bin.span, "compound assignment requires numeric types", .{});
                }
                return tc.type_builder.voidType();
            }
        },
        else => {},
    }

    // String concatenation with +
    if (bin.op == .add) {
        const left_is_string = left_type == .primitive and left_type.primitive == .string_;
        const right_is_string = right_type == .primitive and right_type.primitive == .string_;
        if (left_is_string and right_is_string) {
            return tc.type_builder.stringType();
        }
        if (left_is_string or right_is_string) {
            tc.addError(.type_mismatch, bin.span, "cannot concatenate string with non-string", .{});
            return tc.type_builder.unknownType();
        }
    }

    // Arithmetic operators
    switch (bin.op) {
        .add, .sub, .mul, .div, .mod, .add_wrap, .sub_wrap, .mul_wrap, .add_sat, .sub_sat, .mul_sat => {
            if (!left_type.isNumeric()) {
                tc.addError(.invalid_operation, bin.span, "left operand must be numeric", .{});
                return tc.type_builder.unknownType();
            }
            if (!right_type.isNumeric()) {
                tc.addError(.invalid_operation, bin.span, "right operand must be numeric", .{});
                return tc.type_builder.unknownType();
            }
            // Types must match exactly (no implicit conversions)
            if (!left_type.eql(right_type)) {
                tc.addError(.type_mismatch, bin.span, "operand types must match", .{});
            }
            return left_type;
        },
        // Comparison operators
        .eq, .not_eq, .lt, .gt, .lt_eq, .gt_eq => {
            if (!left_type.eql(right_type)) {
                tc.addError(.type_mismatch, bin.span, "comparison operands must have same type", .{});
            }
            return tc.type_builder.boolType();
        },
        // Logical operators
        .and_, .or_ => {
            if (!tc.isBoolType(left_type)) {
                tc.addError(.type_mismatch, bin.span, "'and'/'or' requires bool operands", .{});
            }
            if (!tc.isBoolType(right_type)) {
                tc.addError(.type_mismatch, bin.span, "'and'/'or' requires bool operands", .{});
            }
            return tc.type_builder.boolType();
        },
        // Bitwise operators
        .bit_and, .bit_or, .bit_xor, .shl, .shr => {
            if (!left_type.isInteger()) {
                tc.addError(.invalid_operation, bin.span, "bitwise operations require integers", .{});
                return tc.type_builder.unknownType();
            }
            if (!right_type.isInteger()) {
                tc.addError(.invalid_operation, bin.span, "bitwise operations require integers", .{});
                return tc.type_builder.unknownType();
            }
            return left_type;
        },
        // Null coalescing
        .null_coalesce => {
            // Left must be optional
            if (left_type != .optional) {
                tc.addError(.invalid_operation, bin.span, "'??' requires optional on left", .{});
                return right_type;
            }
            // Right must match inner type of optional
            const inner = left_type.optional.*;
            if (!inner.eql(right_type)) {
                tc.addError(.type_mismatch, bin.span, "'??' types must match", .{});
            }
            return right_type;
        },
        // 'is' operator for type checking
        .is => {
            return tc.type_builder.boolType();
        },
        else => return tc.type_builder.unknownType(),
    }
}

// ============================================================================
// Unary Expression Checking
// ============================================================================

fn checkUnary(tc: anytype, un: *ast.Unary) Type {
    const operand_type = checkExpr(tc, un.operand);

    switch (un.op) {
        .negate => {
            if (!operand_type.isNumeric()) {
                tc.addError(.invalid_operation, un.span, "negation requires numeric type", .{});
                return tc.type_builder.unknownType();
            }
            if (!operand_type.isSigned()) {
                tc.addError(.invalid_operation, un.span, "negation requires signed type", .{});
            }
            return operand_type;
        },
        .not => {
            if (!tc.isBoolType(operand_type)) {
                tc.addError(.invalid_operation, un.span, "'not' requires bool type", .{});
                return tc.type_builder.unknownType();
            }
            return tc.type_builder.boolType();
        },
        .ref => {
            // With new syntax, mutability is determined by whether the operand is mutable
            // `ref x` where x is `var` -> inout T (mutable reference)
            // `ref x` where x is `let` -> ref T (immutable reference)
            const is_mutable = tc.isMutable(un.operand);
            return tc.type_builder.referenceType(operand_type, is_mutable) catch tc.type_builder.unknownType();
        },
        .ref_mut => {
            // Legacy syntax support (for backward compatibility during transition)
            if (!tc.isMutable(un.operand)) {
                tc.addError(.mutability_error, un.span, "cannot take mutable reference to immutable value", .{});
            }
            return tc.type_builder.referenceType(operand_type, true) catch tc.type_builder.unknownType();
        },
        .deref => {
            // Only allow dereferencing reference types (&T, &mut T)
            // Rc[T] should use .get() or Cell/RefCell patterns instead
            if (operand_type == .reference) {
                return operand_type.reference.inner;
            } else {
                tc.addError(.invalid_operation, un.span, "cannot dereference non-reference type", .{});
                return tc.type_builder.unknownType();
            }
        },
    }
}

// ============================================================================
// Postfix Expression Checking
// ============================================================================

fn checkPostfix(tc: anytype, post: *ast.Postfix) Type {
    const operand_type = checkExpr(tc, post.operand);

    switch (post.op) {
        .unwrap => {
            // ? operator: propagate error/none via early return
            // Accepts both Optional and Result types

            if (operand_type != .optional and operand_type != .result) {
                tc.addError(.invalid_operation, post.span, "'?' requires optional or result type", .{});
                return tc.type_builder.unknownType();
            }

            // Validate function return type is compatible
            const return_type = tc.current_return_type orelse {
                tc.addError(.invalid_operation, post.span, "'?' cannot be used outside a function", .{});
                return tc.type_builder.unknownType();
            };

            if (operand_type == .optional) {
                // Optional? requires function to return Optional
                if (return_type != .optional) {
                    tc.addError(.type_mismatch, post.span, "'?' on optional requires function to return optional type", .{});
                }
                return operand_type.optional.*;
            } else {
                // Result? requires function to return Result with same or convertible error type
                if (return_type != .result) {
                    tc.addError(.type_mismatch, post.span, "'?' on result requires function to return result type", .{});
                } else if (!operand_type.result.err_type.eql(return_type.result.err_type)) {
                    // Error types differ - check if From trait conversion exists
                    const source_err = operand_type.result.err_type;
                    const target_err = return_type.result.err_type;

                    if (tc.hasFromImpl(target_err, source_err)) {
                        // From impl exists - record the conversion for codegen
                        // Method name follows struct method naming: {TypeName}_{method_name}
                        const target_name = tc.getTypeName(target_err) orelse "unknown";
                        const from_method_name = std.fmt.allocPrint(
                            tc.allocator,
                            "{s}_from",
                            .{target_name},
                        ) catch "";

                        tc.error_conversions.put(tc.allocator, post, .{
                            .source_type = source_err,
                            .target_type = target_err,
                            .from_method_name = from_method_name,
                        }) catch {};
                    } else {
                        tc.addError(.type_mismatch, post.span, "error type mismatch: cannot convert from '{s}' to '{s}'; consider implementing From[{s}] for {s}", .{
                            tc.getTypeName(source_err) orelse "unknown",
                            tc.getTypeName(target_err) orelse "unknown",
                            tc.getTypeName(source_err) orelse "unknown",
                            tc.getTypeName(target_err) orelse "unknown",
                        });
                    }
                }
                return operand_type.result.ok_type;
            }
        },
        .force_unwrap => {
            // ! operator: optional -> inner type (traps on None)
            if (operand_type != .optional and operand_type != .result) {
                tc.addError(.invalid_operation, post.span, "'!' requires optional or result type", .{});
                return tc.type_builder.unknownType();
            }
            if (operand_type == .optional) {
                return operand_type.optional.*;
            }
            return operand_type.result.ok_type;
        },
    }
}

// ============================================================================
// Block Checking
// ============================================================================

pub fn checkBlock(tc: anytype, block: *ast.Block) Type {
    _ = tc.pushScope(.block) catch return tc.type_builder.unknownType();
    defer tc.popScope();

    for (block.statements) |stmt| {
        tc.checkStmt(stmt);
    }

    if (block.final_expr) |final| {
        return checkExpr(tc, final);
    }

    return tc.type_builder.voidType();
}

// ============================================================================
// Closure Checking
// ============================================================================

fn checkClosure(tc: anytype, closure: *ast.Closure) Type {
    const new_scope = tc.pushScope(.function) catch return tc.type_builder.unknownType();
    defer tc.popScope();

    // Set up capture tracking for this closure
    const old_closure_scope = tc.closure_scope;
    tc.closure_scope = new_scope;
    tc.closure_captures.clearRetainingCapacity();
    defer {
        tc.closure_scope = old_closure_scope;
        // Note: captures are extracted before this defer runs
    }

    var param_types: std.ArrayListUnmanaged(Type) = .{};
    defer param_types.deinit(tc.allocator);

    for (closure.params) |param| {
        const param_type = tc.resolveTypeExpr(param.type_) catch tc.type_builder.unknownType();

        param_types.append(tc.allocator, param_type) catch {};

        tc.current_scope.define(.{
            .name = param.name,
            .type_ = param_type,
            .kind = .parameter,
            .mutable = false,
            .span = param.span,
        }) catch {};
    }

    const return_type = tc.resolveTypeExpr(closure.return_type) catch tc.type_builder.unknownType();

    const old_return_type = tc.current_return_type;
    tc.current_return_type = return_type;
    defer tc.current_return_type = old_return_type;

    _ = checkExpr(tc, closure.body);

    // Store captured variables in the AST node
    // Note: This allocation is intentionally not freed here as it becomes
    // part of the AST and is used during code generation. The memory will
    // be freed when the allocator is cleaned up (arena-style).
    if (tc.closure_captures.count() > 0) {
        var captures = std.ArrayListUnmanaged(ast.CapturedVar){};
        var iter = tc.closure_captures.iterator();
        while (iter.next()) |entry| {
            captures.append(tc.allocator, .{
                .name = entry.key_ptr.*,
                .is_mutable = entry.value_ptr.is_mutable,
            }) catch {};
        }
        closure.captures = captures.toOwnedSlice(tc.allocator) catch null;
    }

    return tc.type_builder.functionType(param_types.items, return_type) catch tc.type_builder.unknownType();
}

// ============================================================================
// Range Checking
// ============================================================================

fn checkRange(tc: anytype, range: *ast.Range) Type {
    var elem_type: Type = tc.type_builder.i32Type();

    if (range.start) |start| {
        const start_type = checkExpr(tc, start);
        if (!start_type.isInteger()) {
            tc.addError(.type_mismatch, range.span, "range bounds must be integer", .{});
        }
        elem_type = start_type;
    }

    if (range.end) |end| {
        const end_type = checkExpr(tc, end);
        if (!end_type.isInteger()) {
            tc.addError(.type_mismatch, range.span, "range bounds must be integer", .{});
        }
        if (range.start != null and !elem_type.eql(end_type)) {
            tc.addError(.type_mismatch, range.span, "range bounds must have same type", .{});
        }
        // If no start was specified, infer element type from end
        if (range.start == null) {
            elem_type = end_type;
        }
    }

    // Range is now a proper Range[T] type
    return tc.type_builder.rangeType(elem_type, range.inclusive) catch tc.type_builder.unknownType();
}

// ============================================================================
// Struct Literal Checking
// ============================================================================

fn checkStructLiteral(tc: anytype, lit: *ast.StructLiteral) Type {
    if (lit.type_name) |type_name| {
        const resolved = tc.resolveTypeExpr(type_name) catch return tc.type_builder.unknownType();
        if (resolved != .struct_) {
            tc.addError(.type_mismatch, lit.span, "expected struct type", .{});
            return tc.type_builder.unknownType();
        }

        // Check field types
        const struct_type = resolved.struct_;
        for (lit.fields) |field_init| {
            const field_type = checkExpr(tc, field_init.value);
            var found = false;
            for (struct_type.fields) |struct_field| {
                if (std.mem.eql(u8, struct_field.name, field_init.name)) {
                    found = true;
                    if (!field_type.eql(struct_field.type_)) {
                        tc.addError(.type_mismatch, field_init.span, "field type mismatch", .{});
                    }
                    break;
                }
            }
            if (!found) {
                tc.addError(.undefined_field, field_init.span, "unknown field '{s}'", .{field_init.name});
            }
        }

        return resolved;
    }

    // Anonymous struct literal
    tc.addError(.type_mismatch, lit.span, "struct literal requires type name", .{});
    return tc.type_builder.unknownType();
}

// ============================================================================
// Array Literal Checking
// ============================================================================

fn checkArrayLiteral(tc: anytype, arr: *ast.ArrayLiteral) Type {
    if (arr.elements.len == 0) {
        // Empty array: type unknown without context
        return tc.type_builder.unknownType();
    }

    const first_type = checkExpr(tc, arr.elements[0]);
    for (arr.elements[1..]) |elem| {
        const elem_type = checkExpr(tc, elem);
        if (!elem_type.eql(first_type)) {
            tc.addError(.type_mismatch, elem.span(), "array elements must have same type", .{});
        }
    }

    return tc.type_builder.arrayType(first_type, arr.elements.len) catch tc.type_builder.unknownType();
}

// ============================================================================
// Tuple Literal Checking
// ============================================================================

fn checkTupleLiteral(tc: anytype, tup: *ast.TupleLiteral) Type {
    return checkTupleLiteralWithHint(tc, tup, null);
}

fn checkTupleLiteralWithHint(tc: anytype, tup: *ast.TupleLiteral, hint: ?Type) Type {
    var elem_types: std.ArrayListUnmanaged(Type) = .{};
    defer elem_types.deinit(tc.allocator);

    // Extract expected element types from hint if it's a tuple with matching arity
    const hint_elem_types: ?[]const Type = if (hint) |h| blk: {
        if (h == .tuple and h.tuple.elements.len == tup.elements.len) {
            break :blk h.tuple.elements;
        }
        break :blk null;
    } else null;

    for (tup.elements, 0..) |elem, i| {
        const elem_hint: ?Type = if (hint_elem_types) |het| het[i] else null;
        elem_types.append(tc.allocator, checkExprWithHint(tc, elem, elem_hint)) catch {};
    }

    return tc.type_builder.tupleType(elem_types.items) catch tc.type_builder.unknownType();
}

// ============================================================================
// Type Cast Checking
// ============================================================================

fn checkTypeCast(tc: anytype, cast: *ast.TypeCast) Type {
    const expr_type = checkExpr(tc, cast.expr);
    const target_type = tc.resolveTypeExpr(cast.target_type) catch return tc.type_builder.unknownType();

    // Validate the cast is between compatible types (numeric to numeric, etc.)
    const expr_numeric = expr_type.isNumeric();
    const target_numeric = target_type.isNumeric();

    if (expr_numeric and target_numeric) {
        // Numeric conversions are always allowed with .as[]
    } else if (expr_type == .primitive and target_type == .primitive) {
        // Other primitive-to-primitive conversions
    } else if (expr_type == .unknown or target_type == .unknown) {
        // Allow casts involving unknown types (error already reported)
    } else {
        tc.addError(.invalid_conversion, cast.span, "invalid type cast", .{});
    }

    return target_type;
}

// ============================================================================
// Unsafe Block Checking
// ============================================================================

fn checkUnsafeBlock(tc: anytype, block: *ast.UnsafeBlock) Type {
    // Save the previous unsafe context state
    const was_unsafe = tc.in_unsafe_context;

    // Enter unsafe context
    tc.in_unsafe_context = true;

    // Type-check the inner block
    const block_type = checkBlock(tc, block.body);

    // Restore the previous unsafe context state
    tc.in_unsafe_context = was_unsafe;

    return block_type;
}

// ============================================================================
// Out Argument Checking
// ============================================================================

fn checkOutArg(tc: anytype, out_arg: *ast.OutArg) Type {
    // Look up the variable to get its type
    if (tc.lookupSymbol(out_arg.name)) |sym| {
        if (sym.kind == .variable or sym.kind == .parameter) {
            return sym.type_;
        }
    }
    tc.addError(.undefined_variable, out_arg.span, "undefined variable '{s}'", .{out_arg.name});
    return tc.type_builder.unknownType();
}

// ============================================================================
// Comptime Block Checking
// ============================================================================

fn checkComptimeBlock(tc: anytype, block: *ast.ComptimeBlock) Type {
    // First, type-check the block to ensure it's valid
    const block_type = checkBlock(tc, block.body);

    // Create an interpreter instance for comptime evaluation
    var interp = Interpreter.init(tc.allocator) catch {
        tc.addError(.comptime_error, block.span, "failed to initialize comptime interpreter", .{});
        return tc.type_builder.unknownType();
    };
    defer interp.deinit();

    // Populate the interpreter with constants from the current scope
    comptime_eval.populateInterpreterEnv(tc, &interp);

    // Evaluate the comptime block
    const result = interp.evalBlock(block.body) catch |err| {
        // Convert runtime error to type-checking error
        const msg = switch (err) {
            values.RuntimeError.UndefinedVariable => "undefined variable in comptime block",
            values.RuntimeError.TypeError => "type error in comptime block",
            values.RuntimeError.DivisionByZero => "division by zero in comptime block",
            values.RuntimeError.IndexOutOfBounds => "index out of bounds in comptime block",
            values.RuntimeError.InvalidOperation => "invalid operation in comptime block",
            values.RuntimeError.ComptimeError => "compile error triggered in comptime block",
            values.RuntimeError.AssertionFailed => "assertion failed in comptime block",
            else => "runtime error in comptime block",
        };
        tc.addError(.comptime_error, block.span, "{s}", .{msg});
        return tc.type_builder.unknownType();
    };

    // Convert the Value to ComptimeValue using the shared helper
    const comptime_value = comptime_eval.valueToComptimeValue(tc, result, block.span) orelse {
        return tc.type_builder.unknownType();
    };

    // Store the evaluated value for codegen
    tc.comptime_values.put(tc.allocator, block, comptime_value) catch {
        tc.addError(.comptime_error, block.span, "failed to store comptime value", .{});
        return tc.type_builder.unknownType();
    };

    // Return the type based on the evaluated value
    return switch (comptime_value) {
        .int => |i| if (i.is_i32) tc.type_builder.i32Type() else tc.type_builder.i64Type(),
        .float => tc.type_builder.f64Type(),
        .bool_ => tc.type_builder.boolType(),
        .string => tc.type_builder.stringType(),
        .void_ => block_type, // Return the block's inferred type
        .struct_ => |cs| blk: {
            // Look up the struct type by name
            if (tc.lookupType(cs.type_name)) |t| {
                if (t == .struct_) {
                    break :blk t;
                }
            }
            // If struct not found, return unknown
            tc.addError(.comptime_error, block.span, "unknown struct type '{s}' in comptime block", .{cs.type_name});
            break :blk tc.type_builder.unknownType();
        },
        .array => |arr| blk: {
            // Build array type from element type and length
            break :blk tc.type_builder.arrayType(arr.element_type, arr.elements.len) catch tc.type_builder.unknownType();
        },
    };
}

// ============================================================================
// Enum Literal Checking (includes struct static method calls)
// ============================================================================

pub fn checkEnumLiteral(tc: anytype, lit: *ast.EnumLiteral) Type {
    // Resolve the enum type
    const resolved_type = tc.resolveTypeExpr(lit.enum_type) catch return tc.type_builder.unknownType();

    // Handle struct static method calls: StructType::method(args)
    if (resolved_type == .struct_) {
        const struct_type = resolved_type.struct_;
        if (tc.lookupStructMethod(struct_type.name, lit.variant_name)) |struct_method| {
            // Verify this is a static method (no self parameter)
            if (struct_method.has_self) {
                tc.addError(.invalid_call, lit.span, "cannot call instance method '{s}' without an object", .{lit.variant_name});
                return tc.type_builder.unknownType();
            }

            // Check argument count
            const expected_args = struct_method.func_type.function.params.len;
            if (lit.payload.len != expected_args) {
                tc.addError(.invalid_call, lit.span, "method '{s}' expects {d} argument(s), got {d}", .{ lit.variant_name, expected_args, lit.payload.len });
            }

            // Type check arguments
            for (lit.payload, 0..) |arg, i| {
                const arg_type = checkExpr(tc, arg);
                if (i < struct_method.func_type.function.params.len) {
                    const expected_type = struct_method.func_type.function.params[i];
                    // Skip type var parameters for now (generic methods)
                    if (expected_type != .type_var and !arg_type.eql(expected_type)) {
                        tc.addError(.type_mismatch, lit.span, "argument type mismatch", .{});
                    }
                }
            }

            // Handle monomorphized generic structs
            if (std.mem.indexOf(u8, struct_type.name, "$")) |_| {
                tc.recordMethodMonomorphization(struct_type, struct_method) catch {};

                var mono_iter2 = tc.monomorphized_structs.valueIterator();
                while (mono_iter2.next()) |mono| {
                    if (std.mem.eql(u8, mono.mangled_name, struct_type.name)) {
                        var substitutions = std.AutoHashMapUnmanaged(u32, Type){};
                        defer substitutions.deinit(tc.allocator);

                        for (struct_method.impl_type_params, 0..) |type_param, i| {
                            if (i < mono.type_args.len) {
                                substitutions.put(tc.allocator, type_param.id, mono.type_args[i]) catch {};
                            }
                        }

                        const return_type = struct_method.func_type.function.return_type;
                        const concrete_return = tc.substituteTypeParams(return_type, substitutions) catch return_type;
                        return concrete_return;
                    }
                }
            }

            return struct_method.func_type.function.return_type;
        } else {
            tc.addError(.undefined_method, lit.span, "struct '{s}' has no method '{s}'", .{ struct_type.name, lit.variant_name });
            return tc.type_builder.unknownType();
        }
    }

    if (resolved_type != .enum_) {
        tc.addError(.type_mismatch, lit.span, "expected enum type", .{});
        return tc.type_builder.unknownType();
    }

    const enum_def = resolved_type.enum_;

    // Find the variant
    var found_variant: ?types.EnumVariant = null;
    for (enum_def.variants) |variant| {
        if (std.mem.eql(u8, variant.name, lit.variant_name)) {
            found_variant = variant;
            break;
        }
    }

    if (found_variant == null) {
        tc.addError(.undefined_variant, lit.span, "unknown variant '{s}'", .{lit.variant_name});
        return tc.type_builder.unknownType();
    }

    const variant = found_variant.?;

    // Check payload matches variant definition
    if (variant.payload) |payload| {
        switch (payload) {
            .tuple => |tuple_types| {
                if (lit.payload.len != tuple_types.len) {
                    tc.addError(.type_mismatch, lit.span, "expected {d} payload values, got {d}", .{ tuple_types.len, lit.payload.len });
                } else {
                    for (lit.payload, tuple_types) |payload_expr, expected_type| {
                        const actual_type = checkExpr(tc, payload_expr);
                        if (!actual_type.eql(expected_type)) {
                            tc.addError(.type_mismatch, payload_expr.span(), "payload type mismatch", .{});
                        }
                    }
                }
            },
            .struct_ => |fields| {
                // For struct payloads, check each field
                if (lit.payload.len != fields.len) {
                    tc.addError(.type_mismatch, lit.span, "expected {d} struct fields, got {d}", .{ fields.len, lit.payload.len });
                }
                // TODO: Proper struct field matching by name
            },
        }
    } else {
        // Unit variant - no payload expected
        if (lit.payload.len > 0) {
            tc.addError(.type_mismatch, lit.span, "variant '{s}' takes no payload", .{lit.variant_name});
        }
    }

    // If this is a generic enum, record the monomorphization
    if (enum_def.type_params.len > 0) {
        // Extract type arguments from the resolved enum type
        if (lit.enum_type == .generic_apply) {
            const generic = lit.enum_type.generic_apply;
            var type_args = std.ArrayListUnmanaged(Type){};
            defer type_args.deinit(tc.allocator);

            for (generic.args) |arg| {
                const resolved_arg = tc.resolveTypeExpr(arg) catch continue;
                type_args.append(tc.allocator, resolved_arg) catch {};
            }

            // Record enum monomorphization - pass original enum def
            _ = tc.recordEnumMonomorphization(enum_def.name, enum_def, type_args.items) catch {};
        }
    }

    return resolved_type;
}

// ============================================================================
// Index/Field Checking
// ============================================================================

pub fn checkIndex(tc: anytype, idx: *ast.Index) Type {
    const object_type = checkExpr(tc, idx.object);
    const index_type = checkExpr(tc, idx.index);

    // Index must be integer
    if (!index_type.isInteger()) {
        tc.addError(.invalid_index, idx.span, "index must be integer type", .{});
    }

    // Check what we're indexing
    switch (object_type) {
        .array => |a| return a.element,
        .slice => |s| return s.element,
        .tuple => |t| {
            // TODO: check if index is comptime known and in bounds
            if (t.elements.len > 0) {
                return t.elements[0];
            }
            return tc.type_builder.unknownType();
        },
        else => {
            tc.addError(.invalid_index, idx.span, "cannot index this type", .{});
            return tc.type_builder.unknownType();
        },
    }
}

pub fn checkField(tc: anytype, fld: *ast.Field) Type {
    var object_type = checkExpr(tc, fld.object);

    // Auto-dereference references for field access
    while (object_type == .reference) {
        object_type = object_type.reference.inner;
    }

    switch (object_type) {
        .struct_ => |s| {
            for (s.fields) |field| {
                if (std.mem.eql(u8, field.name, fld.field_name)) {
                    return field.type_;
                }
            }
            tc.addError(.undefined_field, fld.span, "no field '{s}' on struct", .{fld.field_name});
            return tc.type_builder.unknownType();
        },
        .applied => |a| {
            // For applied types like Pair[T], access fields from the base struct
            if (a.base == .struct_) {
                const s = a.base.struct_;
                for (s.fields) |field| {
                    if (std.mem.eql(u8, field.name, fld.field_name)) {
                        // Build substitution map from struct's type params to applied args
                        var substitutions = std.AutoHashMapUnmanaged(u32, Type){};
                        defer substitutions.deinit(tc.allocator);

                        // Map each struct type param to corresponding applied arg
                        const min_len = @min(s.type_params.len, a.args.len);
                        for (s.type_params[0..min_len], a.args[0..min_len]) |type_param, arg| {
                            substitutions.put(tc.allocator, type_param.id, arg) catch {};
                        }

                        // Substitute type vars in field type
                        return tc.substituteTypeParams(field.type_, substitutions) catch field.type_;
                    }
                }
                tc.addError(.undefined_field, fld.span, "no field '{s}' on struct", .{fld.field_name});
                return tc.type_builder.unknownType();
            }
            tc.addError(.undefined_field, fld.span, "cannot access field on this type", .{});
            return tc.type_builder.unknownType();
        },
        .tuple => |t| {
            // Tuple field access by index (e.g., tuple.0, tuple.1)
            const idx = std.fmt.parseInt(usize, fld.field_name, 10) catch {
                tc.addError(.undefined_field, fld.span, "invalid tuple field", .{});
                return tc.type_builder.unknownType();
            };
            if (idx >= t.elements.len) {
                tc.addError(.undefined_field, fld.span, "tuple index out of bounds", .{});
                return tc.type_builder.unknownType();
            }
            return t.elements[idx];
        },
        else => {
            tc.addError(.undefined_field, fld.span, "cannot access field on this type", .{});
            return tc.type_builder.unknownType();
        },
    }
}

// ============================================================================
// Call Checking - Delegated to TypeChecker
// ============================================================================

// The checkCall and checkMethodCall functions are very large (~2000+ lines combined)
// due to extensive special-case handling for builtins, FFI, traits, and generics.
// They remain in checker.zig and are called via tc.checkCallImpl/tc.checkMethodCallImpl.

pub fn checkCall(tc: anytype, call: *ast.Call) Type {
    return tc.checkCallImpl(call);
}

pub fn checkMethodCall(tc: anytype, method: *ast.MethodCall) Type {
    return tc.checkMethodCallImpl(method);
}
