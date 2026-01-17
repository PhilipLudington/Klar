const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("../ast.zig");
const Span = ast.Span;
const types = @import("../types.zig");
const Type = types.Type;
const state = @import("state.zig");
const OwnershipState = state.OwnershipState;
const BorrowKind = state.BorrowKind;
const BorrowInfo = state.BorrowInfo;
const VariableState = state.VariableState;
const OwnershipScope = state.OwnershipScope;

// ============================================================================
// Ownership Errors
// ============================================================================

pub const OwnershipError = struct {
    kind: Kind,
    span: Span,
    message: []const u8,

    pub const Kind = enum {
        /// Using a variable after its value was moved.
        use_after_move,
        /// Creating a mutable borrow while immutable borrows exist.
        conflicting_borrow,
        /// Creating a mutable borrow while another mutable borrow exists.
        double_mutable_borrow,
        /// Trying to borrow a moved value.
        borrow_of_moved,
        /// Trying to move a borrowed value.
        move_of_borrowed,
        /// Assigning to a borrowed variable.
        assign_to_borrowed,
        /// Moving out of a reference.
        move_out_of_reference,
    };

    pub fn format(self: OwnershipError, allocator: Allocator) ![]u8 {
        return std.fmt.allocPrint(allocator, "ownership error at {d}:{d}: {s}", .{
            self.span.line,
            self.span.column,
            self.message,
        });
    }
};

// ============================================================================
// Ownership Checker
// ============================================================================

/// Performs ownership analysis on a typed AST.
/// Detects use-after-move, borrow conflicts, and other ownership violations.
pub const OwnershipChecker = struct {
    allocator: Allocator,
    errors: std.ArrayListUnmanaged(OwnershipError),
    current_scope: *OwnershipScope,
    global_scope: OwnershipScope,
    scope_arena: std.heap.ArenaAllocator,
    message_arena: std.heap.ArenaAllocator,

    pub fn init(allocator: Allocator) OwnershipChecker {
        var self = OwnershipChecker{
            .allocator = allocator,
            .errors = .{},
            .current_scope = undefined,
            .global_scope = OwnershipScope.init(allocator, null, .global),
            .scope_arena = std.heap.ArenaAllocator.init(allocator),
            .message_arena = std.heap.ArenaAllocator.init(allocator),
        };
        self.current_scope = &self.global_scope;
        return self;
    }

    pub fn deinit(self: *OwnershipChecker) void {
        self.errors.deinit(self.allocator);
        self.global_scope.deinit();
        self.scope_arena.deinit();
        self.message_arena.deinit();
    }

    /// Analyze a module for ownership violations.
    pub fn analyze(self: *OwnershipChecker, module: ast.Module) !void {
        for (module.declarations) |decl| {
            try self.analyzeDecl(decl);
        }
    }

    /// Check if a type is Copy (values are copied rather than moved).
    /// Delegates to Type.isCopyType() for the actual check.
    pub fn isCopyType(self: *OwnershipChecker, ty: Type) bool {
        _ = self;
        return ty.isCopyType();
    }

    // ========================================================================
    // Declaration Analysis
    // ========================================================================

    fn analyzeDecl(self: *OwnershipChecker, decl: ast.Decl) !void {
        switch (decl) {
            .function => |f| try self.analyzeFunction(f),
            .impl_decl => |impl| try self.analyzeImpl(impl),
            .const_decl => |c| try self.analyzeConst(c),
            .struct_decl, .enum_decl, .trait_decl, .import_decl, .type_alias, .module_decl => {},
        }
    }

    fn analyzeFunction(self: *OwnershipChecker, func: *ast.FunctionDecl) !void {
        // Push function scope
        try self.pushScope(.function);
        defer self.popScope();

        // Define parameters as owned variables
        for (func.params) |param| {
            try self.defineVariable(param.name, .unknown, param.span, true);
        }

        // Analyze function body
        if (func.body) |body| {
            try self.analyzeBlock(body);
        }
    }

    fn analyzeImpl(self: *OwnershipChecker, impl: *ast.ImplDecl) !void {
        for (impl.methods) |method| {
            try self.analyzeFunctionValue(method);
        }
    }

    fn analyzeFunctionValue(self: *OwnershipChecker, func: ast.FunctionDecl) !void {
        try self.pushScope(.function);
        defer self.popScope();

        for (func.params) |param| {
            try self.defineVariable(param.name, .unknown, param.span, true);
        }

        if (func.body) |body| {
            try self.analyzeBlock(body);
        }
    }

    fn analyzeConst(self: *OwnershipChecker, const_decl: *ast.ConstDecl) !void {
        // Analyze the value expression
        _ = try self.analyzeExpr(const_decl.value);
    }

    // ========================================================================
    // Statement Analysis
    // ========================================================================

    fn analyzeStmt(self: *OwnershipChecker, stmt: ast.Stmt) !void {
        switch (stmt) {
            .expr_stmt => |e| _ = try self.analyzeExpr(e.expr),
            .let_decl => |l| try self.analyzeLet(l),
            .var_decl => |v| try self.analyzeVar(v),
            .assignment => |a| try self.analyzeAssignment(a),
            .while_loop => |w| try self.analyzeWhile(w),
            .for_loop => |f| try self.analyzeFor(f),
            .loop_stmt => |l| try self.analyzeLoop(l),
            .return_stmt => |r| try self.analyzeReturn(r),
            .break_stmt => {},
            .continue_stmt => {},
        }
    }

    fn analyzeLet(self: *OwnershipChecker, let_decl: *ast.LetDecl) !void {
        // Analyze the value expression
        const value_state = try self.analyzeExpr(let_decl.value);

        // Get the type (may need to infer from value)
        const is_copy = true; // TODO: Determine from actual type

        // Define the new variable
        try self.defineVariable(let_decl.name, .unknown, let_decl.span, is_copy);

        // If RHS was an identifier that's not Copy, mark it as moved
        if (!is_copy) {
            if (value_state) |vs| {
                if (!vs.is_copy and vs.state == .owned) {
                    vs.markMoved(let_decl.span);
                }
            }
        }
    }

    fn analyzeVar(self: *OwnershipChecker, var_decl: *ast.VarDecl) !void {
        _ = try self.analyzeExpr(var_decl.value);
        try self.defineVariable(var_decl.name, .unknown, var_decl.span, true);
    }

    fn analyzeAssignment(self: *OwnershipChecker, assign: *ast.Assignment) !void {
        // Check the target
        const target_state = try self.analyzeExpr(assign.target);

        // Check if target is borrowed
        if (target_state) |ts| {
            if (ts.state == .borrowed or ts.state == .borrowed_mut) {
                self.addError(.assign_to_borrowed, assign.span, "cannot assign to borrowed variable '{s}'", .{ts.name});
            }
        }

        // Analyze the value being assigned
        _ = try self.analyzeExpr(assign.value);
    }

    fn analyzeWhile(self: *OwnershipChecker, while_stmt: *ast.WhileLoop) !void {
        _ = try self.analyzeExpr(while_stmt.condition);

        try self.pushScope(.loop);
        try self.analyzeBlock(while_stmt.body);
        self.popScope();
    }

    fn analyzeFor(self: *OwnershipChecker, for_stmt: *ast.ForLoop) !void {
        _ = try self.analyzeExpr(for_stmt.iterable);

        try self.pushScope(.loop);

        // Define loop variable based on pattern
        switch (for_stmt.pattern) {
            .binding => |b| try self.defineVariable(b.name, .unknown, b.span, true),
            else => {},
        }

        try self.analyzeBlock(for_stmt.body);
        self.popScope();
    }

    fn analyzeLoop(self: *OwnershipChecker, loop_stmt: *ast.LoopStmt) !void {
        try self.pushScope(.loop);
        try self.analyzeBlock(loop_stmt.body);
        self.popScope();
    }

    fn analyzeReturn(self: *OwnershipChecker, return_stmt: *ast.ReturnStmt) !void {
        if (return_stmt.value) |value| {
            const value_state = try self.analyzeExpr(value);

            // If returning a variable, it's being moved out
            if (value_state) |vs| {
                if (!vs.is_copy and vs.state == .owned) {
                    vs.markMoved(return_stmt.span);
                }
            }
        }
    }

    fn analyzeBlock(self: *OwnershipChecker, block: *ast.Block) error{OutOfMemory}!void {
        try self.pushScope(.block);
        defer self.popScope();

        for (block.statements) |stmt| {
            try self.analyzeStmt(stmt);
        }

        // Analyze trailing expression if present
        if (block.final_expr) |expr| {
            _ = try self.analyzeExpr(expr);
        }
    }

    // ========================================================================
    // Expression Analysis
    // ========================================================================

    /// Analyze an expression and return the variable state if it's an identifier.
    fn analyzeExpr(self: *OwnershipChecker, expr: ast.Expr) error{OutOfMemory}!?*VariableState {
        return switch (expr) {
            .identifier => |id| try self.analyzeIdentifier(id),
            .literal => null,
            .binary => |b| try self.analyzeBinary(b),
            .unary => |u| try self.analyzeUnary(u),
            .postfix => null,
            .call => |c| try self.analyzeCall(c),
            .field => |f| try self.analyzeField(f),
            .index => |i| try self.analyzeIndex(i),
            .method_call => |m| try self.analyzeMethodCall(m),
            .if_expr => |i| try self.analyzeIfExpr(i),
            .match_expr => |m| try self.analyzeMatchExpr(m),
            .block => |b| try self.analyzeBlockExpr(b),
            .closure => |c| try self.analyzeClosure(c),
            .range => |r| try self.analyzeRange(r),
            .struct_literal => |s| try self.analyzeStructLiteral(s),
            .array_literal => |a| try self.analyzeArrayLiteral(a),
            .tuple_literal => |t| try self.analyzeTupleLiteral(t),
            .type_cast => |tc| try self.analyzeTypeCast(tc),
            .grouped => |g| try self.analyzeExpr(g.expr),
            .interpolated_string => |i| try self.analyzeInterpolatedString(i),
            .enum_literal => |e| try self.analyzeEnumLiteral(e),
        };
    }

    fn analyzeEnumLiteral(self: *OwnershipChecker, lit: *ast.EnumLiteral) !?*VariableState {
        // Analyze each payload expression
        for (lit.payload) |payload_expr| {
            _ = try self.analyzeExpr(payload_expr);
        }
        return null;
    }

    fn analyzeIdentifier(self: *OwnershipChecker, id: ast.Identifier) !?*VariableState {
        if (self.current_scope.lookup(id.name)) |var_state| {
            // Check if variable has been moved
            if (var_state.state == .moved) {
                self.addError(.use_after_move, id.span, "use of moved variable '{s}'", .{id.name});
            }
            return var_state;
        }
        return null;
    }

    fn analyzeBinary(self: *OwnershipChecker, binary: *ast.Binary) !?*VariableState {
        _ = try self.analyzeExpr(binary.left);
        _ = try self.analyzeExpr(binary.right);
        return null;
    }

    fn analyzeUnary(self: *OwnershipChecker, unary: *ast.Unary) !?*VariableState {
        const operand_state = try self.analyzeExpr(unary.operand);

        switch (unary.op) {
            .ref => {
                // Creating immutable borrow
                if (operand_state) |vs| {
                    if (vs.state == .moved) {
                        self.addError(.borrow_of_moved, unary.span, "cannot borrow moved variable '{s}'", .{vs.name});
                    } else if (!vs.canBorrowImmutable()) {
                        if (vs.getConflictingBorrow(.immutable)) |conflict| {
                            self.addError(.conflicting_borrow, unary.span, "cannot borrow '{s}' as immutable because it is already borrowed as mutable at {d}:{d}", .{ vs.name, conflict.span.line, conflict.span.column });
                        }
                    } else {
                        try vs.addBorrow(self.allocator, .{
                            .kind = .immutable,
                            .span = unary.span,
                            .scope_depth = self.current_scope.depth,
                            .borrow_expr = null,
                        });
                    }
                }
            },
            .ref_mut => {
                // Creating mutable borrow
                if (operand_state) |vs| {
                    if (vs.state == .moved) {
                        self.addError(.borrow_of_moved, unary.span, "cannot borrow moved variable '{s}'", .{vs.name});
                    } else if (!vs.canBorrowMutable()) {
                        if (vs.getConflictingBorrow(.mutable)) |conflict| {
                            const kind_str = if (conflict.kind == .mutable) "mutable" else "immutable";
                            self.addError(.double_mutable_borrow, unary.span, "cannot borrow '{s}' as mutable because it is already borrowed as {s} at {d}:{d}", .{ vs.name, kind_str, conflict.span.line, conflict.span.column });
                        }
                    } else {
                        try vs.addBorrow(self.allocator, .{
                            .kind = .mutable,
                            .span = unary.span,
                            .scope_depth = self.current_scope.depth,
                            .borrow_expr = null,
                        });
                    }
                }
            },
            else => {},
        }

        return null;
    }

    fn analyzeCall(self: *OwnershipChecker, call: *ast.Call) !?*VariableState {
        _ = try self.analyzeExpr(call.callee);
        for (call.args) |arg| {
            _ = try self.analyzeExpr(arg);
        }
        return null;
    }

    fn analyzeMethodCall(self: *OwnershipChecker, method_call: *ast.MethodCall) !?*VariableState {
        _ = try self.analyzeExpr(method_call.object);
        for (method_call.args) |arg| {
            _ = try self.analyzeExpr(arg);
        }
        return null;
    }

    fn analyzeField(self: *OwnershipChecker, field: *ast.Field) !?*VariableState {
        _ = try self.analyzeExpr(field.object);
        return null;
    }

    fn analyzeIndex(self: *OwnershipChecker, index: *ast.Index) !?*VariableState {
        _ = try self.analyzeExpr(index.object);
        _ = try self.analyzeExpr(index.index);
        return null;
    }

    fn analyzeIfExpr(self: *OwnershipChecker, if_expr: *ast.IfExpr) !?*VariableState {
        _ = try self.analyzeExpr(if_expr.condition);

        try self.pushScope(.conditional);
        _ = try self.analyzeExpr(if_expr.then_branch);
        self.popScope();

        if (if_expr.else_branch) |else_branch| {
            try self.pushScope(.conditional);
            _ = try self.analyzeExpr(else_branch);
            self.popScope();
        }

        return null;
    }

    fn analyzeMatchExpr(self: *OwnershipChecker, match_expr: *ast.MatchExpr) !?*VariableState {
        _ = try self.analyzeExpr(match_expr.subject);

        for (match_expr.arms) |arm| {
            try self.pushScope(.conditional);
            _ = try self.analyzeExpr(arm.body);
            self.popScope();
        }

        return null;
    }

    fn analyzeBlockExpr(self: *OwnershipChecker, block: *ast.Block) !?*VariableState {
        try self.analyzeBlock(block);
        return null;
    }

    fn analyzeClosure(self: *OwnershipChecker, closure: *ast.Closure) !?*VariableState {
        try self.pushScope(.function);
        for (closure.params) |param| {
            try self.defineVariable(param.name, .unknown, param.span, true);
        }
        _ = try self.analyzeExpr(closure.body);
        self.popScope();
        return null;
    }

    fn analyzeRange(self: *OwnershipChecker, range: *ast.Range) !?*VariableState {
        if (range.start) |start| {
            _ = try self.analyzeExpr(start);
        }
        if (range.end) |end| {
            _ = try self.analyzeExpr(end);
        }
        return null;
    }

    fn analyzeStructLiteral(self: *OwnershipChecker, struct_lit: *ast.StructLiteral) !?*VariableState {
        for (struct_lit.fields) |field| {
            _ = try self.analyzeExpr(field.value);
        }
        return null;
    }

    fn analyzeArrayLiteral(self: *OwnershipChecker, array_lit: *ast.ArrayLiteral) !?*VariableState {
        for (array_lit.elements) |elem| {
            _ = try self.analyzeExpr(elem);
        }
        return null;
    }

    fn analyzeTupleLiteral(self: *OwnershipChecker, tuple_lit: *ast.TupleLiteral) !?*VariableState {
        for (tuple_lit.elements) |elem| {
            _ = try self.analyzeExpr(elem);
        }
        return null;
    }

    fn analyzeTypeCast(self: *OwnershipChecker, type_cast: *ast.TypeCast) !?*VariableState {
        _ = try self.analyzeExpr(type_cast.expr);
        return null;
    }

    fn analyzeInterpolatedString(self: *OwnershipChecker, interp_str: *ast.InterpolatedString) !?*VariableState {
        for (interp_str.parts) |part| {
            switch (part) {
                .string => {},
                .expr => |e| _ = try self.analyzeExpr(e),
            }
        }
        return null;
    }

    // ========================================================================
    // Scope Management
    // ========================================================================

    fn pushScope(self: *OwnershipChecker, kind: OwnershipScope.Kind) !void {
        const new_scope = try self.scope_arena.allocator().create(OwnershipScope);
        new_scope.* = OwnershipScope.init(self.allocator, self.current_scope, kind);
        self.current_scope = new_scope;
    }

    fn popScope(self: *OwnershipChecker) void {
        const depth = self.current_scope.depth;
        var scope: ?*OwnershipScope = self.current_scope.parent;
        while (scope) |s| {
            // Call the safe iteration helper on the scope
            s.invalidateBorrowsAtDepth(depth);
            scope = s.parent;
        }

        self.current_scope.deinit();

        if (self.current_scope.parent) |parent| {
            self.current_scope = parent;
        }
    }

    fn defineVariable(self: *OwnershipChecker, name: []const u8, ty: Type, span: Span, is_copy: bool) !void {
        const var_state = VariableState.init(name, ty, span, is_copy);
        try self.current_scope.define(name, var_state);
    }

    // ========================================================================
    // Error Handling
    // ========================================================================

    fn addError(self: *OwnershipChecker, kind: OwnershipError.Kind, span: Span, comptime fmt: []const u8, args: anytype) void {
        const message = std.fmt.allocPrint(self.message_arena.allocator(), fmt, args) catch "error message allocation failed";
        self.errors.append(self.allocator, .{
            .kind = kind,
            .span = span,
            .message = message,
        }) catch {};
    }

    // ========================================================================
    // Debug Output
    // ========================================================================

    /// Dump ownership state for debugging (--dump-ownership flag).
    pub fn dumpState(self: *OwnershipChecker, writer: anytype) !void {
        try writer.writeAll("=== Ownership State ===\n");
        try self.dumpScopeInternal(writer, &self.global_scope, 0);
    }

    fn dumpScopeInternal(_: *OwnershipChecker, writer: anytype, scope: *OwnershipScope, indent: usize) !void {
        const indent_str = "  " ** 10;
        const prefix = indent_str[0..@min(indent * 2, 20)];

        try writer.print("{s}Scope (depth={d}, kind={s}):\n", .{
            prefix,
            scope.depth,
            @tagName(scope.kind),
        });

        var it = scope.variables.iterator();
        while (it.next()) |entry| {
            try writer.print("{s}  {}\n", .{ prefix, entry.value_ptr });
        }
    }

    /// Check if there are any ownership errors.
    pub fn hasErrors(self: *const OwnershipChecker) bool {
        return self.errors.items.len > 0;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "OwnershipChecker initialization" {
    const allocator = std.testing.allocator;

    var checker = OwnershipChecker.init(allocator);
    defer checker.deinit();

    try std.testing.expect(!checker.hasErrors());
}

test "isCopyType primitives" {
    const allocator = std.testing.allocator;

    var checker = OwnershipChecker.init(allocator);
    defer checker.deinit();

    // All primitives are Copy
    try std.testing.expect(checker.isCopyType(.{ .primitive = .i32_ }));
    try std.testing.expect(checker.isCopyType(.{ .primitive = .bool_ }));
    try std.testing.expect(checker.isCopyType(.{ .primitive = .f64_ }));
    try std.testing.expect(checker.isCopyType(.{ .primitive = .string_ }));

    // Void and never are Copy
    try std.testing.expect(checker.isCopyType(.void_));
    try std.testing.expect(checker.isCopyType(.never));

    // Unknown is not Copy
    try std.testing.expect(!checker.isCopyType(.unknown));
}
