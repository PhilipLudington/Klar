//! Statement type checking.
//!
//! This module contains functions for type-checking Klar statements.
//! Functions receive a type checker instance via `anytype` to avoid
//! circular imports while maintaining type safety through duck typing.

const std = @import("std");
const ast = @import("../ast.zig");
const types = @import("../types.zig");
const Type = types.Type;
const CheckError = @import("checker.zig").CheckError;

// ============================================================================
// Statement Checking
// ============================================================================

pub fn checkStmt(tc: anytype, stmt: ast.Stmt) void {
    switch (stmt) {
        .let_decl => |l| checkLetDecl(tc, l),
        .var_decl => |v| checkVarDecl(tc, v),
        .assignment => |a| checkAssignment(tc, a),
        .expr_stmt => |e| _ = tc.checkExpr(e.expr),
        .return_stmt => |r| checkReturn(tc, r),
        .break_stmt => |b| checkBreak(tc, b),
        .continue_stmt => |c| checkContinue(tc, c),
        .for_loop => |f| checkFor(tc, f),
        .while_loop => |w| checkWhile(tc, w),
        .loop_stmt => |l| checkLoop(tc, l),
        .if_stmt => |i| checkIfStmt(tc, i),
        .match_stmt => |m| checkMatchStmt(tc, m),
    }
}

pub fn checkIfStmt(tc: anytype, if_stmt: *ast.IfStmt) void {
    const cond_type = tc.checkExpr(if_stmt.condition);
    if (!tc.isBoolType(cond_type)) {
        tc.addError(.type_mismatch, if_stmt.condition.span(), "if condition must be bool", .{});
    }

    // Check then branch as a block (doesn't produce a value)
    _ = tc.checkBlock(if_stmt.then_branch);

    // Check else branch if present
    if (if_stmt.else_branch) |else_branch| {
        switch (else_branch.*) {
            .block => |block| _ = tc.checkBlock(block),
            .if_stmt => |nested_if| checkIfStmt(tc, nested_if),
        }
    }
}

pub fn checkMatchStmt(tc: anytype, match_stmt: *ast.MatchStmt) void {
    const subject_type = tc.checkExpr(match_stmt.subject);

    for (match_stmt.arms) |arm| {
        // Check pattern against subject type
        tc.checkPattern(arm.pattern, subject_type);

        // Check guard if present
        if (arm.guard) |guard| {
            const guard_type = tc.checkExpr(guard);
            if (!tc.isBoolType(guard_type)) {
                tc.addError(.type_mismatch, guard.span(), "match guard must be bool", .{});
            }
        }

        // Check arm body as a block (doesn't produce a value)
        _ = tc.checkBlock(arm.body);
    }
}

pub fn checkAssignment(tc: anytype, assign: *ast.Assignment) void {
    const target_type = tc.checkExpr(assign.target);
    const value_type = tc.checkExpr(assign.value);

    if (!tc.isAssignable(assign.target)) {
        tc.addError(.invalid_assignment, assign.span, "cannot assign to this expression", .{});
    }
    if (!tc.isMutable(assign.target)) {
        tc.addError(.mutability_error, assign.span, "cannot assign to immutable variable", .{});
    }

    if (assign.op == .assign) {
        if (!tc.checkAssignmentCompatible(target_type, value_type)) {
            tc.addError(.type_mismatch, assign.span, "cannot assign different types", .{});
        }
    } else {
        // Compound assignment
        if (!target_type.isNumeric() or !value_type.isNumeric()) {
            tc.addError(.invalid_operation, assign.span, "compound assignment requires numeric types", .{});
        }
    }
}

pub fn checkLetDecl(tc: anytype, decl: *ast.LetDecl) void {
    // Type is always provided - resolve it first for use as hint
    const declared_type = tc.resolveTypeExpr(decl.type_) catch tc.type_builder.unknownType();

    // Pass declared type as hint for contextual typing of literals
    const value_type = tc.checkExprWithHint(decl.value, declared_type);

    if (!tc.isTypeCompatible(declared_type, value_type)) {
        tc.addError(.type_mismatch, decl.span, "initializer type doesn't match declared type", .{});
    }

    // Check for duplicate in current scope
    if (tc.current_scope.lookupLocal(decl.name) != null) {
        tc.addError(.duplicate_definition, decl.span, "'{s}' already defined in this scope", .{decl.name});
        return;
    }

    tc.current_scope.define(.{
        .name = decl.name,
        .type_ = declared_type,
        .kind = .variable,
        .mutable = false,
        .span = decl.span,
    }) catch {};
}

pub fn checkVarDecl(tc: anytype, decl: *ast.VarDecl) void {
    // Type is always provided - resolve it first for use as hint
    const declared_type = tc.resolveTypeExpr(decl.type_) catch tc.type_builder.unknownType();

    // Pass declared type as hint for contextual typing of literals
    const value_type = tc.checkExprWithHint(decl.value, declared_type);

    if (!tc.isTypeCompatible(declared_type, value_type)) {
        tc.addError(.type_mismatch, decl.span, "initializer type doesn't match declared type", .{});
    }

    if (tc.current_scope.lookupLocal(decl.name) != null) {
        tc.addError(.duplicate_definition, decl.span, "'{s}' already defined in this scope", .{decl.name});
        return;
    }

    tc.current_scope.define(.{
        .name = decl.name,
        .type_ = declared_type,
        .kind = .variable,
        .mutable = true,
        .span = decl.span,
    }) catch {};
}

pub fn checkReturn(tc: anytype, ret: *ast.ReturnStmt) void {
    const func_scope = tc.current_scope.getFunctionScope();
    if (func_scope == null) {
        tc.addError(.invalid_operation, ret.span, "return outside function", .{});
        return;
    }

    if (ret.value) |value| {
        // Pass expected return type as hint for contextual typing of literals
        const value_type = tc.checkExprWithHint(value, tc.current_return_type);
        if (tc.current_return_type) |expected| {
            if (!value_type.eql(expected)) {
                // Allow returning T when ?T is expected (implicit Some wrapping)
                if (expected == .optional) {
                    if (value_type.eql(expected.optional.*)) {
                        return; // OK: T can be returned as ?T (becomes Some(T))
                    }
                }
                tc.addError(.return_type_mismatch, ret.span, "return type mismatch", .{});
            }
        }
    } else {
        if (tc.current_return_type) |expected| {
            // Allow returning nothing when ?T is expected (becomes None)
            if (expected == .optional) {
                return; // OK: implicit None return
            }
            if (expected != .void_) {
                tc.addError(.return_type_mismatch, ret.span, "missing return value", .{});
            }
        }
    }
}

pub fn checkBreak(tc: anytype, brk: *ast.BreakStmt) void {
    if (!tc.current_scope.isInLoop()) {
        tc.addError(.break_outside_loop, brk.span, "break outside loop", .{});
    }
    if (brk.value) |value| {
        _ = tc.checkExpr(value);
    }
}

pub fn checkContinue(tc: anytype, cont: *ast.ContinueStmt) void {
    if (!tc.current_scope.isInLoop()) {
        tc.addError(.continue_outside_loop, cont.span, "continue outside loop", .{});
    }
}

pub fn checkFor(tc: anytype, for_loop: *ast.ForLoop) void {
    const iter_type = tc.checkExpr(for_loop.iterable);

    // Get element type from iterable
    const elem_type: Type = switch (iter_type) {
        .array => |a| a.element,
        .slice => |s| s.element,
        .range => |r| r.element_type,
        .list => |l| l.element,
        .set => |s| s.element,
        .map => |m| blk: {
            // Map iteration yields (key, value) tuples
            const tuple_elems = [_]Type{ m.key, m.value };
            break :blk tc.type_builder.tupleType(&tuple_elems) catch tc.type_builder.unknownType();
        },
        else => blk: {
            tc.addError(.not_iterable, for_loop.span, "cannot iterate over this type", .{});
            break :blk tc.type_builder.unknownType();
        },
    };

    // Create loop scope and bind pattern
    _ = tc.pushScope(.loop) catch return;
    defer tc.popScope();

    tc.bindPattern(for_loop.pattern, elem_type);

    _ = tc.checkBlock(for_loop.body);
}

pub fn checkWhile(tc: anytype, while_loop: *ast.WhileLoop) void {
    const cond_type = tc.checkExpr(while_loop.condition);
    if (!tc.isBoolType(cond_type)) {
        tc.addError(.type_mismatch, while_loop.condition.span(), "while condition must be bool", .{});
    }

    _ = tc.pushScope(.loop) catch return;
    defer tc.popScope();

    _ = tc.checkBlock(while_loop.body);
}

pub fn checkLoop(tc: anytype, loop: *ast.LoopStmt) void {
    _ = tc.pushScope(.loop) catch return;
    defer tc.popScope();

    _ = tc.checkBlock(loop.body);
}
