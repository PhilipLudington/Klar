const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("../ast.zig");
const Span = ast.Span;
const types = @import("../types.zig");
const Type = types.Type;
const state = @import("state.zig");
const OwnershipState = state.OwnershipState;
const VariableState = state.VariableState;
const OwnershipScope = state.OwnershipScope;
const checker_mod = @import("checker.zig");
const OwnershipChecker = checker_mod.OwnershipChecker;

// ============================================================================
// Drop Point Types
// ============================================================================

/// Represents a location where a variable needs to be dropped.
pub const DropPoint = struct {
    /// Name of the variable to drop.
    variable: []const u8,
    /// Type of the variable (needed for calling correct destructor).
    ty: Type,
    /// Location in the source where the drop should occur.
    location: Location,
    /// Source span for error messages.
    span: Span,

    pub const Location = enum {
        /// Drop at the end of a scope (normal case).
        scope_exit,
        /// Drop before a return statement.
        early_return,
        /// Drop before a break statement.
        break_,
        /// Drop before a continue statement.
        continue_,
    };

    pub fn format(
        self: DropPoint,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("drop {s} ({s}) at {d}:{d}", .{
            self.variable,
            @tagName(self.location),
            self.span.line,
            self.span.column,
        });
    }
};

/// Information about drops needed at a specific AST node.
pub const DropInfo = struct {
    /// Drops that must occur at this location.
    drops: std.ArrayListUnmanaged(DropPoint),
    /// The AST node ID this drop info is attached to.
    node_id: usize,

    pub fn init() DropInfo {
        return .{
            .drops = .{},
            .node_id = 0,
        };
    }

    pub fn deinit(self: *DropInfo, allocator: Allocator) void {
        self.drops.deinit(allocator);
    }

    pub fn addDrop(self: *DropInfo, allocator: Allocator, drop: DropPoint) !void {
        try self.drops.append(allocator, drop);
    }
};

// ============================================================================
// Drop Inserter
// ============================================================================

/// Analyzes ownership flow and determines where drop calls need to be inserted.
/// Works in conjunction with OwnershipChecker to produce drop information.
pub const DropInserter = struct {
    allocator: Allocator,
    /// Maps AST node locations to drop information.
    /// Key: span offset, Value: drops needed at that location.
    drop_map: std.AutoHashMapUnmanaged(usize, DropInfo),
    /// Stack of scopes for tracking live variables.
    scope_stack: std.ArrayListUnmanaged(ScopeInfo),
    /// Arena for allocating drop info.
    arena: std.heap.ArenaAllocator,
    /// Reference to ownership checker for Copy type checking.
    ownership_checker: *OwnershipChecker,

    const ScopeInfo = struct {
        /// Variables declared in this scope that may need dropping.
        variables: std.ArrayListUnmanaged(VarInfo),
        /// Kind of scope (affects drop behavior).
        kind: OwnershipScope.Kind,
        /// Depth of this scope.
        depth: u32,

        const VarInfo = struct {
            name: []const u8,
            ty: Type,
            is_copy: bool,
            is_moved: bool,
            decl_span: Span,
        };

        fn init(_: Allocator, kind: OwnershipScope.Kind, depth: u32) ScopeInfo {
            return .{
                .variables = std.ArrayListUnmanaged(VarInfo){},
                .kind = kind,
                .depth = depth,
            };
        }

        fn deinit(self: *ScopeInfo, allocator: Allocator) void {
            self.variables.deinit(allocator);
        }

        /// Get variables that need to be dropped (owned, not Copy, not moved).
        fn getDroppableVars(self: *ScopeInfo) []const VarInfo {
            // Return variables that need dropping
            // This creates a view of variables that are:
            // - Not Copy types
            // - Not moved
            var count: usize = 0;
            for (self.variables.items) |v| {
                if (!v.is_copy and !v.is_moved) {
                    count += 1;
                }
            }
            // For simplicity, return all and let caller filter
            return self.variables.items;
        }
    };

    pub fn init(allocator: Allocator, ownership_checker: *OwnershipChecker) DropInserter {
        return .{
            .allocator = allocator,
            .drop_map = .{},
            .scope_stack = .{},
            .arena = std.heap.ArenaAllocator.init(allocator),
            .ownership_checker = ownership_checker,
        };
    }

    pub fn deinit(self: *DropInserter) void {
        var it = self.drop_map.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
        }
        self.drop_map.deinit(self.allocator);

        for (self.scope_stack.items) |*scope| {
            scope.deinit(self.allocator);
        }
        self.scope_stack.deinit(self.allocator);

        self.arena.deinit();
    }

    /// Analyze a module and compute drop points.
    pub fn analyze(self: *DropInserter, module: ast.Module) !void {
        // Push global scope
        try self.pushScope(.global);

        for (module.declarations) |decl| {
            try self.analyzeDecl(decl);
        }

        // Pop global scope
        try self.popScope(null);
    }

    /// Get drop information for a specific AST node location.
    pub fn getDropsAt(self: *DropInserter, span_offset: usize) ?*const DropInfo {
        return self.drop_map.getPtr(span_offset);
    }

    /// Get all drop points (for debugging).
    pub fn getAllDrops(self: *DropInserter, allocator: Allocator) ![]DropPoint {
        var result = std.ArrayListUnmanaged(DropPoint){};
        var it = self.drop_map.iterator();
        while (it.next()) |entry| {
            for (entry.value_ptr.drops.items) |drop| {
                try result.append(allocator, drop);
            }
        }
        return result.toOwnedSlice(allocator);
    }

    // ========================================================================
    // Declaration Analysis
    // ========================================================================

    fn analyzeDecl(self: *DropInserter, decl: ast.Decl) !void {
        switch (decl) {
            .function => |f| try self.analyzeFunction(f),
            .impl_decl => |impl| try self.analyzeImpl(impl),
            .const_decl => |c| try self.analyzeConst(c),
            .struct_decl, .enum_decl, .trait_decl, .import_decl, .type_alias, .module_decl, .extern_type_decl => {},
        }
    }

    fn analyzeFunction(self: *DropInserter, func: *ast.FunctionDecl) !void {
        try self.pushScope(.function);

        // Track parameters as local variables
        for (func.params) |param| {
            const is_copy = self.ownership_checker.isCopyType(.unknown);
            try self.trackVariable(param.name, .unknown, is_copy, param.span);
        }

        // Analyze body
        if (func.body) |body| {
            try self.analyzeBlock(body);
        }

        // Generate drops for function exit
        try self.popScope(func.span.offset);
    }

    fn analyzeImpl(self: *DropInserter, impl: *ast.ImplDecl) !void {
        for (impl.methods) |method| {
            try self.analyzeFunction(method);
        }
    }

    fn analyzeConst(self: *DropInserter, const_decl: *ast.ConstDecl) !void {
        if (const_decl.value) |value| {
            try self.analyzeExpr(value);
        }
    }

    // ========================================================================
    // Statement Analysis
    // ========================================================================

    fn analyzeStmt(self: *DropInserter, stmt: ast.Stmt) !void {
        switch (stmt) {
            .expr_stmt => |e| try self.analyzeExpr(e.expr),
            .let_decl => |l| try self.analyzeLet(l),
            .var_decl => |v| try self.analyzeVar(v),
            .assignment => |a| try self.analyzeAssignment(a),
            .if_stmt => |i| try self.analyzeIf(i),
            .while_loop => |w| try self.analyzeWhile(w),
            .for_loop => |f| try self.analyzeFor(f),
            .return_stmt => |r| try self.analyzeReturn(r),
            .break_stmt => |b| try self.analyzeBreak(b),
            .continue_stmt => |c| try self.analyzeContinue(c),
            .loop_stmt => |l| try self.analyzeLoopStmt(l),
            .match_stmt => |m| try self.analyzeMatch(m),
        }
    }

    fn analyzeLet(self: *DropInserter, let_decl: *ast.LetDecl) !void {
        // Analyze value first
        try self.analyzeExpr(let_decl.value);

        // Determine Copy status from the declared type expression
        const is_copy = self.ownership_checker.isCopyTypeExpr(let_decl.type_);
        try self.trackVariable(let_decl.name, .unknown, is_copy, let_decl.span);
    }

    fn analyzeVar(self: *DropInserter, var_decl: *ast.VarDecl) !void {
        if (var_decl.value) |value| {
            try self.analyzeExpr(value);
        }

        const is_copy = self.ownership_checker.isCopyTypeExpr(var_decl.type_);
        try self.trackVariable(var_decl.name, .unknown, is_copy, var_decl.span);
    }

    fn analyzeAssignment(self: *DropInserter, assign: *ast.Assignment) !void {
        try self.analyzeExpr(assign.target);
        try self.analyzeExpr(assign.value);

        // If assigning from a variable, mark it as moved (if not Copy)
        if (assign.value == .identifier) {
            const id = assign.value.identifier;
            self.markMoved(id.name);
        }
    }

    fn analyzeIf(self: *DropInserter, if_stmt: *ast.IfStmt) !void {
        try self.analyzeExpr(if_stmt.condition);

        // Then branch
        try self.pushScope(.conditional);
        try self.analyzeBlock(if_stmt.then_branch);
        try self.popScope(if_stmt.then_branch.span.end); // After block

        // Else branch
        if (if_stmt.else_branch) |else_branch| {
            try self.pushScope(.conditional);
            switch (else_branch.*) {
                .block => |b| try self.analyzeBlock(b),
                .if_stmt => |nested| try self.analyzeIf(nested),
            }
            try self.popScope(null);
        }
    }

    fn analyzeWhile(self: *DropInserter, while_stmt: *ast.WhileLoop) !void {
        try self.analyzeExpr(while_stmt.condition);

        try self.pushScope(.loop);
        try self.analyzeBlock(while_stmt.body);
        try self.popScope(while_stmt.body.span.end);
    }

    fn analyzeFor(self: *DropInserter, for_stmt: *ast.ForLoop) !void {
        try self.analyzeExpr(for_stmt.iterable);

        try self.pushScope(.loop);

        // Track loop variable
        switch (for_stmt.pattern) {
            .binding => |b| try self.trackVariable(b.name, .unknown, true, b.span),
            else => {},
        }

        try self.analyzeBlock(for_stmt.body);
        try self.popScope(for_stmt.body.span.end);
    }

    fn analyzeLoopStmt(self: *DropInserter, loop_stmt: *ast.LoopStmt) !void {
        try self.pushScope(.loop);
        try self.analyzeBlock(loop_stmt.body);
        try self.popScope(loop_stmt.body.span.end);
    }

    fn analyzeReturn(self: *DropInserter, return_stmt: *ast.ReturnStmt) !void {
        // Before return, we need to drop all live variables in all scopes
        // (except the return value itself)

        if (return_stmt.value) |value| {
            try self.analyzeExpr(value);

            // If returning a variable, mark it as moved (not dropped)
            if (value == .identifier) {
                const id = value.identifier;
                self.markMoved(id.name);
            }
        }

        // Generate drops for all scopes
        try self.generateEarlyExitDrops(return_stmt.span, .early_return);
    }

    fn analyzeBreak(self: *DropInserter, break_stmt: *ast.BreakStmt) !void {
        // Drop all variables in scopes up to and including the loop
        try self.generateEarlyExitDrops(break_stmt.span, .break_);
    }

    fn analyzeContinue(self: *DropInserter, continue_stmt: *ast.ContinueStmt) !void {
        // Drop all variables in scopes up to (but not including) the loop
        try self.generateEarlyExitDrops(continue_stmt.span, .continue_);
    }

    fn analyzeBlock(self: *DropInserter, block: *ast.Block) !void {
        try self.pushScope(.block);

        for (block.statements) |stmt| {
            try self.analyzeStmt(stmt);
        }

        if (block.final_expr) |expr| {
            try self.analyzeExpr(expr);
        }

        try self.popScope(block.span.end);
    }

    fn analyzeMatch(self: *DropInserter, match_stmt: *ast.MatchStmt) !void {
        try self.analyzeExpr(match_stmt.subject);

        for (match_stmt.arms) |arm| {
            try self.pushScope(.conditional);
            try self.analyzeBlock(arm.body);
            try self.popScope(null);
        }
    }

    // ========================================================================
    // Expression Analysis
    // ========================================================================

    fn analyzeExpr(self: *DropInserter, expr: ast.Expr) !void {
        switch (expr) {
            .identifier => {},
            .literal => {},
            .binary => |b| {
                try self.analyzeExpr(b.left);
                try self.analyzeExpr(b.right);
            },
            .unary => |u| try self.analyzeExpr(u.operand),
            .call => |c| {
                try self.analyzeExpr(c.callee);
                for (c.args) |arg| {
                    try self.analyzeExpr(arg);
                    // Non-copy arguments are moved
                    if (arg == .identifier) {
                        self.markMoved(arg.identifier.name);
                    }
                }
            },
            .field => |f| try self.analyzeExpr(f.object),
            .index => |i| {
                try self.analyzeExpr(i.object);
                try self.analyzeExpr(i.index);
            },
            .array => |a| {
                for (a.elements) |elem| {
                    try self.analyzeExpr(elem);
                }
            },
            .tuple => |t| {
                for (t.elements) |elem| {
                    try self.analyzeExpr(elem);
                }
            },
            .struct_init => |s| {
                for (s.field_inits) |fi| {
                    try self.analyzeExpr(fi.value);
                }
            },
            .if_ => |i| {
                try self.analyzeExpr(i.condition);
                try self.analyzeExpr(i.then_branch);
                if (i.else_branch) |else_b| {
                    try self.analyzeExpr(else_b);
                }
            },
            .block => |b| try self.analyzeBlock(b),
            .lambda => |l| try self.analyzeExpr(l.body),
            .match => |m| {
                try self.analyzeExpr(m.expr);
                for (m.arms) |arm| {
                    if (arm.body) |body| {
                        switch (body.*) {
                            .block => |b| try self.analyzeBlock(b),
                            .expr => |e| try self.analyzeExpr(e),
                        }
                    }
                }
            },
            .range => |r| {
                if (r.start) |s| try self.analyzeExpr(s);
                if (r.end) |e| try self.analyzeExpr(e);
            },
            .type_expr, .error_expr => {},
        }
    }

    // ========================================================================
    // Scope Management
    // ========================================================================

    fn pushScope(self: *DropInserter, kind: OwnershipScope.Kind) !void {
        const depth: u32 = @intCast(self.scope_stack.items.len);
        try self.scope_stack.append(self.allocator, ScopeInfo.init(self.allocator, kind, depth));
    }

    fn popScope(self: *DropInserter, drop_location: ?usize) !void {
        if (self.scope_stack.items.len == 0) return;

        var scope = self.scope_stack.pop();
        defer scope.deinit(self.allocator);

        if (drop_location) |loc| {
            // Generate drops for all droppable variables in this scope
            for (scope.variables.items) |v| {
                if (!v.is_copy and !v.is_moved) {
                    try self.recordDrop(loc, .{
                        .variable = v.name,
                        .ty = v.ty,
                        .location = .scope_exit,
                        .span = v.decl_span,
                    });
                }
            }
        }
    }

    fn trackVariable(self: *DropInserter, name: []const u8, ty: Type, is_copy: bool, span: Span) !void {
        if (self.scope_stack.items.len == 0) return;

        const current = &self.scope_stack.items[self.scope_stack.items.len - 1];
        try current.variables.append(self.allocator, .{
            .name = name,
            .ty = ty,
            .is_copy = is_copy,
            .is_moved = false,
            .decl_span = span,
        });
    }

    fn markMoved(self: *DropInserter, name: []const u8) void {
        // Search through scopes to find and mark the variable
        var i = self.scope_stack.items.len;
        while (i > 0) {
            i -= 1;
            const scope = &self.scope_stack.items[i];
            for (scope.variables.items) |*v| {
                if (std.mem.eql(u8, v.name, name)) {
                    v.is_moved = true;
                    return;
                }
            }
        }
    }

    fn generateEarlyExitDrops(self: *DropInserter, span: Span, location: DropPoint.Location) !void {
        // For early exits (return, break, continue), we need to drop
        // all live variables in scopes being exited

        const stop_at_loop = location == .continue_;

        var i = self.scope_stack.items.len;
        while (i > 0) {
            i -= 1;
            const scope = &self.scope_stack.items[i];

            // For continue, stop at loop scope (don't drop loop variables)
            if (stop_at_loop and scope.kind == .loop) {
                break;
            }

            // Drop all non-moved, non-Copy variables in this scope
            for (scope.variables.items) |v| {
                if (!v.is_copy and !v.is_moved) {
                    try self.recordDrop(span.offset, .{
                        .variable = v.name,
                        .ty = v.ty,
                        .location = location,
                        .span = span,
                    });
                }
            }

            // For break, stop after exiting the loop
            if (location == .break_ and scope.kind == .loop) {
                break;
            }
        }
    }

    fn recordDrop(self: *DropInserter, location: usize, drop: DropPoint) !void {
        const result = try self.drop_map.getOrPut(self.allocator, location);
        if (!result.found_existing) {
            result.value_ptr.* = DropInfo.init();
        }
        try result.value_ptr.addDrop(self.allocator, drop);
    }

    // ========================================================================
    // Debug Output
    // ========================================================================

    /// Dump all computed drop points for debugging.
    pub fn dumpDrops(self: *DropInserter, writer: anytype) !void {
        try writer.writeAll("=== Drop Points ===\n");

        var it = self.drop_map.iterator();
        while (it.next()) |entry| {
            try writer.print("At offset {d}:\n", .{entry.key_ptr.*});
            for (entry.value_ptr.drops.items) |drop| {
                try writer.print("  {}\n", .{drop});
            }
        }
    }
};

// ============================================================================
// Tests
// ============================================================================

test "DropInserter initialization" {
    const allocator = std.testing.allocator;

    var ownership_checker = OwnershipChecker.init(allocator);
    defer ownership_checker.deinit();

    var inserter = DropInserter.init(allocator, &ownership_checker);
    defer inserter.deinit();

    const drops = try inserter.getAllDrops(allocator);
    defer allocator.free(drops);

    try std.testing.expectEqual(drops.len, 0);
}

test "DropPoint formatting" {
    const drop = DropPoint{
        .variable = "buffer",
        .ty = .unknown,
        .location = .scope_exit,
        .span = .{ .line = 10, .column = 5, .offset = 100 },
    };

    var buf: [100]u8 = undefined;
    const result = try std.fmt.bufPrint(&buf, "{}", .{drop});
    try std.testing.expect(std.mem.indexOf(u8, result, "buffer") != null);
    try std.testing.expect(std.mem.indexOf(u8, result, "scope_exit") != null);
}
