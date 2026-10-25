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

/// Resolves a named type to the type checker's view of it, so Copy is decided on the
/// resolved type rather than the spelling (Bug 137: `type Id = i32` is Copy).
pub const TypeLookup = struct {
    ctx: *anyopaque,
    lookupFn: *const fn (ctx: *anyopaque, name: []const u8) ?Type,

    pub fn lookup(self: TypeLookup, name: []const u8) ?Type {
        return self.lookupFn(self.ctx, name);
    }
};

/// One variable's move state, saved at a branch so each branch starts from it.
const SavedState = struct {
    vs: *VariableState,
    state: OwnershipState,
    move_span: ?Span,
};

/// Performs ownership analysis on a typed AST.
/// Detects use-after-move, borrow conflicts, and other ownership violations.
pub const OwnershipChecker = struct {
    allocator: Allocator,
    errors: std.ArrayListUnmanaged(OwnershipError),
    current_scope: *OwnershipScope,
    global_scope: OwnershipScope,
    scope_arena: std.heap.ArenaAllocator,
    message_arena: std.heap.ArenaAllocator,
    type_lookup: ?TypeLookup,
    /// Set by a `return` and cleared at each branch, so a branch that ends in `return`
    /// keeps its moves to itself (Bug 132).
    returned: bool,

    pub fn init(allocator: Allocator) OwnershipChecker {
        // NOTE: Do NOT set current_scope here! The struct is returned by value,
        // so any pointer to self.global_scope would become dangling after return.
        // current_scope must be set after the struct is in its final location.
        return OwnershipChecker{
            .allocator = allocator,
            .errors = .empty,
            .current_scope = undefined, // Set in analyze() after struct is placed
            .global_scope = OwnershipScope.init(allocator, null, .global),
            .scope_arena = std.heap.ArenaAllocator.init(allocator),
            .message_arena = std.heap.ArenaAllocator.init(allocator),
            .type_lookup = null,
            .returned = false,
        };
    }

    pub fn deinit(self: *OwnershipChecker) void {
        self.errors.deinit(self.allocator);
        self.global_scope.deinit();
        self.scope_arena.deinit();
        self.message_arena.deinit();
    }

    /// Analyze a module for ownership violations.
    pub fn analyze(self: *OwnershipChecker, module: ast.Module) !void {
        // Initialize current_scope now that the struct is in its final location.
        // This fixes the dangling pointer issue from init() returning by value.
        self.current_scope = &self.global_scope;

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

    /// Determine if a type expression represents a Copy type, by the design's § 2
    /// table (`docs/design/explicit-move-clone.md`): primitives (`string` among them)
    /// and references are Copy; `String`, collections, smart pointers, closures, user
    /// enums and user structs without `is_copy` are not. A name that is neither a
    /// primitive nor a known builtin is resolved through `type_lookup`, so an alias is
    /// as Copy as the type it names (Bug 137); with no lookup, or a name it cannot
    /// resolve (a type parameter), it is not Copy.
    pub fn isCopyTypeExpr(self: *OwnershipChecker, type_expr: ast.TypeExpr) bool {
        return isCopyFromTypeExpr(type_expr, self.type_lookup);
    }

    fn isCopyFromTypeExpr(type_expr: ast.TypeExpr, type_lookup: ?TypeLookup) bool {
        switch (type_expr) {
            .named => |n| {
                if (types.Primitive.fromName(n.name) != null) return true;
                if (std.mem.eql(u8, n.name, "void")) return true;
                // Non-Copy named types
                const non_copy_types = [_][]const u8{
                    "String", "Rc",        "Arc",       "List", "Map", "Set",
                    "File",   "Path",      "BufReader", "BufWriter",
                };
                for (non_copy_types) |nct| {
                    if (std.mem.eql(u8, n.name, nct)) return false;
                }
                if (type_lookup) |tl| {
                    if (tl.lookup(n.name)) |ty| return ty.isCopyType();
                }
                return false;
            },
            .reference => return true,
            .array => |a| return isCopyFromTypeExpr(a.element, type_lookup),
            .tuple => |t| {
                for (t.elements) |elem| {
                    if (!isCopyFromTypeExpr(elem, type_lookup)) return false;
                }
                return true;
            },
            .optional => |o| return isCopyFromTypeExpr(o.inner, type_lookup),
            .function => return false,
            .extern_function => return true,
            .generic_apply => return false, // Generic types (Rc[T], etc.) conservatively not Copy
            .slice => return false,
            .result => return false,
            .qualified => return false,
        }
    }

    // ========================================================================
    // Declaration Analysis
    // ========================================================================

    fn analyzeDecl(self: *OwnershipChecker, decl: ast.Decl) !void {
        switch (decl) {
            .function => |f| try self.analyzeFunction(f),
            .impl_decl => |impl| try self.analyzeImpl(impl),
            .const_decl => |c| try self.analyzeConst(c),
            .test_decl => {}, // Test declarations are excluded from normal compilation flows
            .struct_decl, .enum_decl, .trait_decl, .import_decl, .type_alias, .module_decl, .extern_type_decl, .extern_block => {},
        }
    }

    fn analyzeFunction(self: *OwnershipChecker, func: *ast.FunctionDecl) !void {
        // Push function scope
        try self.pushScope(.function);
        defer self.popScope();
        self.returned = false;

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
        self.returned = false;

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
            .if_stmt => |i| try self.analyzeIfStmt(i),
            .match_stmt => |m| try self.analyzeMatchStmt(m),
        }
    }

    fn analyzeLet(self: *OwnershipChecker, let_decl: *ast.LetDecl) !void {
        // Analyze the value expression
        const value_state = try self.analyzeExpr(let_decl.value);

        // Determine Copy status from the declared type expression
        const is_copy = self.isCopyTypeExpr(let_decl.type_);

        // Move before defining: a define can grow the scope's map, which would leave
        // `value_state` pointing at freed memory when both names share a scope.
        if (!is_copy) moveValue(value_state, let_decl.span);
        try self.defineVariable(let_decl.name, .unknown, let_decl.span, is_copy);
    }

    fn analyzeVar(self: *OwnershipChecker, var_decl: *ast.VarDecl) !void {
        const value_state = try self.analyzeExpr(var_decl.value);
        const is_copy = self.isCopyTypeExpr(var_decl.type_);
        if (!is_copy) moveValue(value_state, var_decl.span);
        try self.defineVariable(var_decl.name, .unknown, var_decl.span, is_copy);
    }

    /// Every consuming position the pass tracks — a `let`/`var` initializer (Bug 115),
    /// the right side of `=` (Bug 135), a `return` — moves the non-Copy owned variable
    /// it names through this one function.
    fn moveValue(value_state: ?*VariableState, span: Span) void {
        if (value_state) |vs| {
            if (!vs.is_copy and vs.state == .owned) {
                vs.markMoved(span);
            }
        }
    }

    fn analyzeAssignment(self: *OwnershipChecker, assign: *ast.Assignment) !void {
        try self.analyzeAssign(assign.target, assign.op, assign.value, assign.span);
    }

    /// `target = value` and the compound forms, whether the parser gave an assignment
    /// statement or a `binary` with an assignment op (Bug 135). Plain `=` moves a
    /// variable named on the right and makes a variable target owned again, so a moved
    /// name can be reassigned and then used (Bug 132's `p2 = advance(p2)`).
    fn analyzeAssign(self: *OwnershipChecker, target: ast.Expr, op: ast.BinaryOp, value: ast.Expr, span: Span) !void {
        const value_state = try self.analyzeExpr(value);

        // Writing a whole variable reads nothing from it, so a moved one may be assigned.
        const target_state = if (op == .assign and target == .identifier)
            self.current_scope.lookup(target.identifier.name)
        else
            try self.analyzeExpr(target);

        if (target_state) |ts| {
            if (ts.state == .borrowed or ts.state == .borrowed_mut) {
                self.addError(.assign_to_borrowed, span, "cannot assign to borrowed variable '{s}'", .{ts.name});
            }
        }

        if (op != .assign) return;
        if (value_state != target_state) moveValue(value_state, span);
        if (target == .identifier) {
            if (target_state) |ts| {
                if (ts.state == .moved) {
                    ts.state = .owned;
                    ts.move_span = null;
                }
            }
        }
    }

    fn analyzeWhile(self: *OwnershipChecker, while_stmt: *ast.WhileLoop) !void {
        _ = try self.analyzeExpr(while_stmt.condition);

        // The body may run zero times, so a `return` in it does not end the path.
        const outer_returned = self.returned;
        try self.pushScope(.loop);
        try self.analyzeBlock(while_stmt.body);
        self.popScope();
        self.returned = outer_returned;
    }

    fn analyzeFor(self: *OwnershipChecker, for_stmt: *ast.ForLoop) !void {
        _ = try self.analyzeExpr(for_stmt.iterable);

        const outer_returned = self.returned;
        try self.pushScope(.loop);

        // Define loop variable based on pattern
        switch (for_stmt.pattern) {
            .binding => |b| try self.defineVariable(b.name, .unknown, b.span, true),
            else => {},
        }

        try self.analyzeBlock(for_stmt.body);
        self.popScope();
        self.returned = outer_returned;
    }

    fn analyzeLoop(self: *OwnershipChecker, loop_stmt: *ast.LoopStmt) !void {
        // A `break` leaves a `loop`, so a `return` in its body does not end the path.
        const outer_returned = self.returned;
        try self.pushScope(.loop);
        try self.analyzeBlock(loop_stmt.body);
        self.popScope();
        self.returned = outer_returned;
    }

    fn analyzeReturn(self: *OwnershipChecker, return_stmt: *ast.ReturnStmt) !void {
        if (return_stmt.value) |value| {
            const value_state = try self.analyzeExpr(value);

            // If returning a variable, it's being moved out
            moveValue(value_state, return_stmt.span);
        }
        self.returned = true;
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
            .comptime_block => |cb| try self.analyzeComptimeBlock(cb),
            .builtin_call => |bc| try self.analyzeBuiltinCall(bc),
            .unsafe_block => |ub| try self.analyzeUnsafeBlock(ub),
            .out_arg => |oa| try self.analyzeOutArg(oa),
        };
    }

    fn analyzeEnumLiteral(self: *OwnershipChecker, lit: *ast.EnumLiteral) !?*VariableState {
        // Analyze each payload expression
        for (lit.payload) |payload_expr| {
            _ = try self.analyzeExpr(payload_expr);
        }
        return null;
    }

    fn analyzeComptimeBlock(self: *OwnershipChecker, block: *ast.ComptimeBlock) !?*VariableState {
        // Analyze the block body for ownership - comptime blocks still need ownership tracking
        _ = try self.analyzeBlockExpr(block.body);
        return null;
    }

    fn analyzeUnsafeBlock(self: *OwnershipChecker, block: *ast.UnsafeBlock) !?*VariableState {
        // Analyze the block body for ownership - unsafe blocks still need ownership tracking
        _ = try self.analyzeBlockExpr(block.body);
        return null;
    }

    fn analyzeOutArg(self: *OwnershipChecker, out_arg: *ast.OutArg) !?*VariableState {
        // Out arguments pass a pointer to a variable, which will be written to by the callee.
        // The variable's state after the call should be considered initialized.
        // For now, just look up the variable state.
        _ = self;
        _ = out_arg;
        return null;
    }

    fn analyzeBuiltinCall(self: *OwnershipChecker, builtin: *ast.BuiltinCall) !?*VariableState {
        // Analyze any expression arguments for ownership
        for (builtin.args) |arg| {
            switch (arg) {
                .expr_arg => |expr| _ = try self.analyzeExpr(expr),
                .type_arg => {}, // Type arguments don't affect ownership
            }
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
        switch (binary.op) {
            .assign, .add_assign, .sub_assign, .mul_assign, .div_assign, .mod_assign => {
                try self.analyzeAssign(binary.left, binary.op, binary.right, binary.span);
                return null;
            },
            else => {},
        }
        _ = try self.analyzeExpr(binary.left);
        _ = try self.analyzeExpr(binary.right);
        return null;
    }

    fn analyzeUnary(self: *OwnershipChecker, unary: *ast.Unary) !?*VariableState {
        const operand_state = try self.analyzeExpr(unary.operand);

        switch (unary.op) {
            .await_ => {
                // Await does not introduce ownership effects in current parser-only support.
            },
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

    fn analyzeIfStmt(self: *OwnershipChecker, if_stmt: *ast.IfStmt) !void {
        _ = try self.analyzeExpr(if_stmt.condition);

        var merge = try self.beginBranches();
        defer merge.deinit(self.allocator);

        try self.startBranch(&merge);
        try self.pushScope(.conditional);
        try self.analyzeBlock(if_stmt.then_branch);
        self.popScope();
        try self.endBranch(&merge);

        try self.startBranch(&merge);
        if (if_stmt.else_branch) |else_branch| {
            try self.pushScope(.conditional);
            switch (else_branch.*) {
                .block => |block| try self.analyzeBlock(block),
                .if_stmt => |nested_if| try self.analyzeIfStmt(nested_if),
            }
            self.popScope();
        }
        // With no `else`, this empty branch is the path that skips the `if`.
        try self.endBranch(&merge);

        self.finishBranches(&merge);
    }

    fn analyzeMatchStmt(self: *OwnershipChecker, match_stmt: *ast.MatchStmt) !void {
        _ = try self.analyzeExpr(match_stmt.subject);

        var merge = try self.beginBranches();
        defer merge.deinit(self.allocator);

        for (match_stmt.arms) |arm| {
            try self.startBranch(&merge);
            try self.pushScope(.conditional);
            try self.analyzeBlock(arm.body);
            self.popScope();
            try self.endBranch(&merge);
        }

        self.finishBranches(&merge);
    }

    // ========================================================================
    // Branch Merging (Bug 132)
    // ========================================================================

    /// The move states of every variable in scope when a set of branches begins, and
    /// the merged result over the branches that fall through to the code after them.
    const BranchMerge = struct {
        saved: std.ArrayListUnmanaged(SavedState),
        merged: std.ArrayListUnmanaged(SavedState),
        fallthrough: usize,
        outer_returned: bool,

        fn deinit(self: *BranchMerge, allocator: Allocator) void {
            self.saved.deinit(allocator);
            self.merged.deinit(allocator);
        }
    };

    fn beginBranches(self: *OwnershipChecker) !BranchMerge {
        var merge = BranchMerge{
            .saved = .empty,
            .merged = .empty,
            .fallthrough = 0,
            .outer_returned = self.returned,
        };
        var scope: ?*OwnershipScope = self.current_scope;
        while (scope) |s| : (scope = s.parent) {
            if (s.variables.capacity() == 0) continue;
            var it = s.variables.valueIterator();
            while (it.next()) |vs| {
                try merge.saved.append(self.allocator, .{ .vs = vs, .state = vs.state, .move_span = vs.move_span });
            }
        }
        try merge.merged.appendSlice(self.allocator, merge.saved.items);
        return merge;
    }

    /// Each branch starts from the states the branches began with.
    fn startBranch(self: *OwnershipChecker, merge: *BranchMerge) !void {
        for (merge.saved.items) |s| {
            s.vs.state = s.state;
            s.vs.move_span = s.move_span;
        }
        self.returned = false;
    }

    /// A branch that ended in `return` never reaches the code after the branches, so its
    /// moves are dropped. A variable moved at the end of any branch that falls through is
    /// moved afterwards; one every such branch left owned (reassigned) is owned.
    fn endBranch(self: *OwnershipChecker, merge: *BranchMerge) !void {
        if (self.returned) return;
        const first = merge.fallthrough == 0;
        merge.fallthrough += 1;
        for (merge.merged.items) |*m| {
            if (first or m.vs.state == .moved) {
                m.state = m.vs.state;
                m.move_span = m.vs.move_span;
            }
        }
    }

    /// When no branch falls through, the code after them is unreachable, and the states
    /// stay as they began.
    fn finishBranches(self: *OwnershipChecker, merge: *BranchMerge) void {
        const result = if (merge.fallthrough == 0) merge.saved.items else merge.merged.items;
        for (result) |s| {
            s.vs.state = s.state;
            s.vs.move_span = s.move_span;
        }
        self.returned = merge.outer_returned or merge.fallthrough == 0;
    }

    fn analyzeBlockExpr(self: *OwnershipChecker, block: *ast.Block) !?*VariableState {
        try self.analyzeBlock(block);
        return null;
    }

    fn analyzeClosure(self: *OwnershipChecker, closure: *ast.Closure) !?*VariableState {
        // A `return` in the closure ends the closure, not the enclosing path.
        const outer_returned = self.returned;
        try self.pushScope(.function);
        for (closure.params) |param| {
            try self.defineVariable(param.name, .unknown, param.span, true);
        }
        _ = try self.analyzeExpr(closure.body);
        self.popScope();
        self.returned = outer_returned;
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
