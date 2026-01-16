//! AST to Klar IR lowering pass.
//!
//! Translates typed AST nodes to Klar IR instructions. This is the bridge
//! between the frontend (parser, checker) and the backend (codegen).
//!
//! Key responsibilities:
//! - Convert AST expressions to SSA values
//! - Create proper control flow graphs
//! - Lower statements to IR instructions
//! - Preserve type information for codegen

const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("../ast.zig");
const klar_types = @import("../types.zig");
const inst = @import("inst.zig");
const builder_mod = @import("builder.zig");

const IrType = inst.IrType;
const Value = inst.Value;
const BlockId = inst.BlockId;
const Module = inst.Module;
const Builder = builder_mod.Builder;

/// Errors that can occur during IR lowering.
pub const LowerError = error{
    OutOfMemory,
    UnsupportedFeature,
    InvalidAST,
    NoCurrentFunction,
    NoCurrentBlock,
};

/// AST to IR lowering context.
pub const Lowerer = struct {
    allocator: Allocator,
    builder: Builder,

    /// Named values in current scope.
    named_values: std.StringHashMapUnmanaged(NamedValue),

    /// Scope stack for nested scopes.
    scope_stack: std.ArrayListUnmanaged(ScopeInfo),

    /// Loop context for break/continue.
    loop_stack: std.ArrayListUnmanaged(LoopContext),

    const NamedValue = struct {
        /// The SSA value (or pointer for mutable variables).
        value: Value,
        /// Whether this is a mutable variable (stored via alloca).
        is_mutable: bool,
        /// The underlying type (not the pointer type).
        ty: IrType,
    };

    const ScopeInfo = struct {
        /// Variables defined in this scope.
        variables: std.ArrayListUnmanaged([]const u8),
    };

    const LoopContext = struct {
        /// Block to jump to for 'continue'.
        continue_block: BlockId,
        /// Block to jump to for 'break'.
        break_block: BlockId,
    };

    pub fn init(allocator: Allocator, module: *Module) Lowerer {
        return .{
            .allocator = allocator,
            .builder = Builder.init(allocator, module),
            .named_values = .{},
            .scope_stack = .{},
            .loop_stack = .{},
        };
    }

    pub fn deinit(self: *Lowerer) void {
        self.named_values.deinit(self.allocator);
        for (self.scope_stack.items) |*scope| {
            scope.variables.deinit(self.allocator);
        }
        self.scope_stack.deinit(self.allocator);
        self.loop_stack.deinit(self.allocator);
    }

    // ========================================================================
    // Module Lowering
    // ========================================================================

    /// Lower an entire AST module to IR.
    pub fn lowerModule(self: *Lowerer, module: ast.Module) LowerError!void {
        // First pass: declare all functions (for forward references)
        for (module.declarations) |decl| {
            switch (decl) {
                .function => |f| try self.declareFunction(f),
                else => {},
            }
        }

        // Second pass: lower function bodies
        for (module.declarations) |decl| {
            switch (decl) {
                .function => |f| {
                    if (f.body != null) {
                        try self.lowerFunction(f);
                    }
                },
                else => {},
            }
        }
    }

    /// Declare a function (create signature without body).
    fn declareFunction(self: *Lowerer, func: *ast.FunctionDecl) LowerError!void {
        // Build parameter types
        var param_names = std.ArrayListUnmanaged([]const u8){};
        defer param_names.deinit(self.allocator);
        var param_types = std.ArrayListUnmanaged(IrType){};
        defer param_types.deinit(self.allocator);

        for (func.params) |param| {
            param_names.append(self.allocator, param.name) catch return LowerError.OutOfMemory;
            const ty = try self.typeExprToIrType(param.type_);
            param_types.append(self.allocator, ty) catch return LowerError.OutOfMemory;
        }

        // Get return type
        const return_ty = if (func.return_type) |rt|
            try self.typeExprToIrType(rt)
        else
            IrType.void_;

        // Create function (will be populated later if it has a body)
        _ = self.builder.beginFunction(
            func.name,
            param_names.items,
            param_types.items,
            return_ty,
        ) catch return LowerError.OutOfMemory;

        self.builder.endFunction();
    }

    // ========================================================================
    // Function Lowering
    // ========================================================================

    /// Lower a function with a body.
    fn lowerFunction(self: *Lowerer, func: *ast.FunctionDecl) LowerError!void {
        // Build parameter info
        var param_names = std.ArrayListUnmanaged([]const u8){};
        defer param_names.deinit(self.allocator);
        var param_types = std.ArrayListUnmanaged(IrType){};
        defer param_types.deinit(self.allocator);

        for (func.params) |param| {
            param_names.append(self.allocator, param.name) catch return LowerError.OutOfMemory;
            const ty = try self.typeExprToIrType(param.type_);
            param_types.append(self.allocator, ty) catch return LowerError.OutOfMemory;
        }

        const return_ty = if (func.return_type) |rt|
            try self.typeExprToIrType(rt)
        else
            IrType.void_;

        // Get or create the function
        const ir_func = self.builder.beginFunction(
            func.name,
            param_names.items,
            param_types.items,
            return_ty,
        ) catch return LowerError.OutOfMemory;

        // Clear named values for new function
        self.named_values.clearRetainingCapacity();

        // Add parameters to named values
        for (ir_func.params) |param| {
            // For parameters, we create stack slots for mutability
            const ptr = self.builder.buildAlloca(param.ty, param.name) catch
                return LowerError.OutOfMemory;
            _ = self.builder.buildStore(param.value, ptr) catch {};

            self.named_values.put(self.allocator, param.name, .{
                .value = ptr,
                .is_mutable = true, // Parameters can be reassigned in Klar
                .ty = param.ty,
            }) catch return LowerError.OutOfMemory;
        }

        // Lower function body
        if (func.body) |body| {
            try self.pushScope();
            const result = try self.lowerBlock(body);
            try self.popScope();

            // Add implicit return if needed
            if (!self.builder.isTerminated()) {
                if (result) |val| {
                    _ = self.builder.buildRet(val) catch {};
                } else if (return_ty == .void_) {
                    _ = self.builder.buildRetVoid() catch {};
                } else {
                    _ = self.builder.buildRetVoid() catch {};
                }
            }
        }

        self.builder.endFunction();
    }

    // ========================================================================
    // Statement Lowering
    // ========================================================================

    /// Lower a statement.
    fn lowerStmt(self: *Lowerer, stmt: ast.Stmt) LowerError!void {
        switch (stmt) {
            .let_decl => |decl| try self.lowerLetDecl(decl),
            .var_decl => |decl| try self.lowerVarDecl(decl),
            .assignment => |assign| try self.lowerAssignment(assign),
            .expr_stmt => |expr_stmt| {
                _ = try self.lowerExpr(expr_stmt.expr);
            },
            .return_stmt => |ret| try self.lowerReturn(ret),
            .while_loop => |loop| try self.lowerWhileLoop(loop),
            .for_loop => |loop| try self.lowerForLoop(loop),
            .break_stmt => |brk| try self.lowerBreak(brk),
            .continue_stmt => try self.lowerContinue(),
            .loop_stmt => |loop| try self.lowerLoop(loop),
        }
    }

    /// Lower a let declaration (immutable binding).
    fn lowerLetDecl(self: *Lowerer, decl: *ast.LetDecl) LowerError!void {
        const value = try self.lowerExpr(decl.value);

        // For let bindings, we can use the value directly or create a copy
        // depending on whether the type is Copy
        if (value.ty.isCopy()) {
            // Copy types: store directly
            self.named_values.put(self.allocator, decl.name, .{
                .value = value,
                .is_mutable = false,
                .ty = value.ty,
            }) catch return LowerError.OutOfMemory;
        } else {
            // Non-copy types: this is a move
            const moved = self.builder.buildMove(value, decl.name) catch
                return LowerError.OutOfMemory;
            self.named_values.put(self.allocator, decl.name, .{
                .value = moved,
                .is_mutable = false,
                .ty = value.ty,
            }) catch return LowerError.OutOfMemory;
        }

        // Track variable in current scope for cleanup
        try self.addToCurrentScope(decl.name);
    }

    /// Lower a var declaration (mutable binding).
    fn lowerVarDecl(self: *Lowerer, decl: *ast.VarDecl) LowerError!void {
        const value = try self.lowerExpr(decl.value);

        // Mutable variables need stack allocation
        const ptr = self.builder.buildAlloca(value.ty, decl.name) catch
            return LowerError.OutOfMemory;
        _ = self.builder.buildStore(value, ptr) catch {};

        self.named_values.put(self.allocator, decl.name, .{
            .value = ptr,
            .is_mutable = true,
            .ty = value.ty,
        }) catch return LowerError.OutOfMemory;

        try self.addToCurrentScope(decl.name);
    }

    /// Lower an assignment.
    fn lowerAssignment(self: *Lowerer, assign: *ast.Assignment) LowerError!void {
        const value = try self.lowerExpr(assign.value);

        switch (assign.target) {
            .identifier => |id| {
                if (self.named_values.get(id.name)) |named| {
                    if (named.is_mutable) {
                        _ = self.builder.buildStore(value, named.value) catch {};
                    }
                    // If not mutable, this should have been caught by the checker
                }
            },
            else => {
                // TODO: Handle field access, index, etc.
            },
        }
    }

    /// Lower a return statement.
    fn lowerReturn(self: *Lowerer, ret: *ast.ReturnStmt) LowerError!void {
        if (ret.value) |val| {
            const value = try self.lowerExpr(val);
            _ = self.builder.buildRet(value) catch {};
        } else {
            _ = self.builder.buildRetVoid() catch {};
        }
    }

    /// Lower a while loop.
    fn lowerWhileLoop(self: *Lowerer, loop: *ast.WhileLoop) LowerError!void {
        // Create blocks
        const cond_bb = self.builder.createBlock("while.cond") catch
            return LowerError.OutOfMemory;
        const body_bb = self.builder.createBlock("while.body") catch
            return LowerError.OutOfMemory;
        const end_bb = self.builder.createBlock("while.end") catch
            return LowerError.OutOfMemory;

        // Branch to condition
        _ = self.builder.buildBr(cond_bb) catch {};

        // Emit condition
        self.builder.positionAtEnd(cond_bb);
        const cond = try self.lowerExpr(loop.condition);
        _ = self.builder.buildCondBr(cond, body_bb, end_bb) catch {};

        // Push loop context for break/continue
        self.loop_stack.append(self.allocator, .{
            .continue_block = cond_bb,
            .break_block = end_bb,
        }) catch return LowerError.OutOfMemory;

        // Emit body
        self.builder.positionAtEnd(body_bb);
        try self.pushScope();
        _ = try self.lowerBlock(loop.body);
        try self.popScope();

        if (!self.builder.isTerminated()) {
            _ = self.builder.buildBr(cond_bb) catch {};
        }

        // Pop loop context
        _ = self.loop_stack.pop();

        // Continue after loop
        self.builder.positionAtEnd(end_bb);
    }

    /// Lower a for loop (range iteration).
    fn lowerForLoop(self: *Lowerer, loop: *ast.ForLoop) LowerError!void {
        // For now, only support simple range iteration
        switch (loop.iterable) {
            .range => |range| {
                try self.lowerRangeFor(loop.pattern, range, loop.body);
            },
            else => {
                // TODO: Support other iterables
                return LowerError.UnsupportedFeature;
            },
        }
    }

    /// Lower a range-based for loop.
    fn lowerRangeFor(
        self: *Lowerer,
        pattern: ast.Pattern,
        range: *ast.Range,
        body: *ast.Block,
    ) LowerError!void {
        // Get loop variable name from pattern
        const var_name = switch (pattern) {
            .binding => |b| b.name,
            else => return LowerError.UnsupportedFeature,
        };

        // Create blocks
        const init_bb = self.builder.getCurrentBlock() orelse return LowerError.NoCurrentBlock;
        _ = init_bb;
        const cond_bb = self.builder.createBlock("for.cond") catch
            return LowerError.OutOfMemory;
        const body_bb = self.builder.createBlock("for.body") catch
            return LowerError.OutOfMemory;
        const incr_bb = self.builder.createBlock("for.incr") catch
            return LowerError.OutOfMemory;
        const end_bb = self.builder.createBlock("for.end") catch
            return LowerError.OutOfMemory;

        // Initialize loop variable
        const start = if (range.start) |s|
            try self.lowerExpr(s)
        else
            self.builder.constI32(0) catch return LowerError.OutOfMemory;

        const counter_ptr = self.builder.buildAlloca(.i32_, var_name) catch
            return LowerError.OutOfMemory;
        _ = self.builder.buildStore(start, counter_ptr) catch {};

        // Branch to condition
        _ = self.builder.buildBr(cond_bb) catch {};

        // Emit condition
        self.builder.positionAtEnd(cond_bb);
        const counter = self.builder.buildLoad(.i32_, counter_ptr) catch
            return LowerError.OutOfMemory;
        const end_val = if (range.end) |e|
            try self.lowerExpr(e)
        else
            self.builder.constI32(0) catch return LowerError.OutOfMemory;

        const pred: inst.Inst.CmpPred = if (range.inclusive) .sle else .slt;
        const cond = self.builder.buildICmp(pred, counter, end_val) catch
            return LowerError.OutOfMemory;
        _ = self.builder.buildCondBr(cond, body_bb, end_bb) catch {};

        // Push loop context
        self.loop_stack.append(self.allocator, .{
            .continue_block = incr_bb,
            .break_block = end_bb,
        }) catch return LowerError.OutOfMemory;

        // Register loop variable
        try self.pushScope();
        self.named_values.put(self.allocator, var_name, .{
            .value = counter_ptr,
            .is_mutable = true,
            .ty = .i32_,
        }) catch return LowerError.OutOfMemory;
        try self.addToCurrentScope(var_name);

        // Emit body
        self.builder.positionAtEnd(body_bb);
        _ = try self.lowerBlock(body);

        if (!self.builder.isTerminated()) {
            _ = self.builder.buildBr(incr_bb) catch {};
        }

        // Emit increment
        self.builder.positionAtEnd(incr_bb);
        const current = self.builder.buildLoad(.i32_, counter_ptr) catch
            return LowerError.OutOfMemory;
        const one = self.builder.constI32(1) catch return LowerError.OutOfMemory;
        const next = self.builder.buildAdd(current, one) catch
            return LowerError.OutOfMemory;
        _ = self.builder.buildStore(next, counter_ptr) catch {};
        _ = self.builder.buildBr(cond_bb) catch {};

        try self.popScope();
        _ = self.loop_stack.pop();

        // Continue after loop
        self.builder.positionAtEnd(end_bb);
    }

    /// Lower an infinite loop.
    fn lowerLoop(self: *Lowerer, loop: *ast.LoopStmt) LowerError!void {
        const body_bb = self.builder.createBlock("loop.body") catch
            return LowerError.OutOfMemory;
        const end_bb = self.builder.createBlock("loop.end") catch
            return LowerError.OutOfMemory;

        _ = self.builder.buildBr(body_bb) catch {};

        self.loop_stack.append(self.allocator, .{
            .continue_block = body_bb,
            .break_block = end_bb,
        }) catch return LowerError.OutOfMemory;

        self.builder.positionAtEnd(body_bb);
        try self.pushScope();
        _ = try self.lowerBlock(loop.body);
        try self.popScope();

        if (!self.builder.isTerminated()) {
            _ = self.builder.buildBr(body_bb) catch {};
        }

        _ = self.loop_stack.pop();
        self.builder.positionAtEnd(end_bb);
    }

    /// Lower a break statement.
    fn lowerBreak(self: *Lowerer, brk: *ast.BreakStmt) LowerError!void {
        _ = brk;
        if (self.loop_stack.items.len > 0) {
            const ctx = self.loop_stack.items[self.loop_stack.items.len - 1];
            _ = self.builder.buildBr(ctx.break_block) catch {};
        }
    }

    /// Lower a continue statement.
    fn lowerContinue(self: *Lowerer) LowerError!void {
        if (self.loop_stack.items.len > 0) {
            const ctx = self.loop_stack.items[self.loop_stack.items.len - 1];
            _ = self.builder.buildBr(ctx.continue_block) catch {};
        }
    }

    // ========================================================================
    // Block Lowering
    // ========================================================================

    /// Lower a block and return its value (if any).
    fn lowerBlock(self: *Lowerer, block: *ast.Block) LowerError!?Value {
        for (block.statements) |stmt| {
            try self.lowerStmt(stmt);
            if (self.builder.isTerminated()) {
                return null;
            }
        }

        if (block.final_expr) |expr| {
            return try self.lowerExpr(expr);
        }

        return null;
    }

    // ========================================================================
    // Expression Lowering
    // ========================================================================

    /// Lower an expression and return its value.
    fn lowerExpr(self: *Lowerer, expr: ast.Expr) LowerError!Value {
        return switch (expr) {
            .literal => |lit| try self.lowerLiteral(lit),
            .identifier => |id| try self.lowerIdentifier(id),
            .binary => |bin| try self.lowerBinary(bin),
            .unary => |un| try self.lowerUnary(un),
            .call => |call| try self.lowerCall(call),
            .if_expr => |if_e| try self.lowerIf(if_e),
            .block => |blk| {
                try self.pushScope();
                const result = try self.lowerBlock(blk);
                try self.popScope();
                return result orelse self.builder.constVoid() catch
                    return LowerError.OutOfMemory;
            },
            .grouped => |g| try self.lowerExpr(g.expr),
            .range => |r| try self.lowerRange(r),
            else => {
                // Placeholder for unimplemented expressions
                return self.builder.constI32(0) catch
                    return LowerError.OutOfMemory;
            },
        };
    }

    /// Lower a literal.
    fn lowerLiteral(self: *Lowerer, lit: ast.Literal) LowerError!Value {
        return switch (lit.kind) {
            .int => |v| {
                // Determine type from value range
                if (v >= std.math.minInt(i32) and v <= std.math.maxInt(i32)) {
                    return self.builder.constI32(@intCast(v)) catch
                        return LowerError.OutOfMemory;
                } else {
                    return self.builder.constI64(@intCast(v)) catch
                        return LowerError.OutOfMemory;
                }
            },
            .float => |v| {
                return self.builder.constF64(v) catch
                    return LowerError.OutOfMemory;
            },
            .bool_ => |v| {
                return self.builder.constBool(v) catch
                    return LowerError.OutOfMemory;
            },
            .char => |_| {
                // TODO: Handle char properly
                return self.builder.constI32(0) catch
                    return LowerError.OutOfMemory;
            },
            .string => |_| {
                // TODO: Handle strings
                return self.builder.constI32(0) catch
                    return LowerError.OutOfMemory;
            },
        };
    }

    /// Lower an identifier.
    fn lowerIdentifier(self: *Lowerer, id: ast.Identifier) LowerError!Value {
        if (self.named_values.get(id.name)) |named| {
            if (named.is_mutable) {
                // Load from alloca
                return self.builder.buildLoad(named.ty, named.value) catch
                    return LowerError.OutOfMemory;
            }
            return named.value;
        }
        // Unknown variable - return placeholder
        return self.builder.constI32(0) catch return LowerError.OutOfMemory;
    }

    /// Lower a binary expression.
    fn lowerBinary(self: *Lowerer, bin: *ast.Binary) LowerError!Value {
        // Handle short-circuit operators specially
        if (bin.op == .and_ or bin.op == .or_) {
            return try self.lowerLogicalBinary(bin);
        }

        const lhs = try self.lowerExpr(bin.left);
        const rhs = try self.lowerExpr(bin.right);

        return switch (bin.op) {
            // Arithmetic
            .add => self.builder.buildAdd(lhs, rhs) catch return LowerError.OutOfMemory,
            .sub => self.builder.buildSub(lhs, rhs) catch return LowerError.OutOfMemory,
            .mul => self.builder.buildMul(lhs, rhs) catch return LowerError.OutOfMemory,
            .div => self.builder.buildSDiv(lhs, rhs) catch return LowerError.OutOfMemory,
            .mod => self.builder.buildSRem(lhs, rhs) catch return LowerError.OutOfMemory,

            // Wrapping arithmetic
            .add_wrap => self.builder.buildAddWrap(lhs, rhs) catch return LowerError.OutOfMemory,
            .sub_wrap => self.builder.buildSubWrap(lhs, rhs) catch return LowerError.OutOfMemory,
            .mul_wrap => self.builder.buildMulWrap(lhs, rhs) catch return LowerError.OutOfMemory,

            // Saturating arithmetic
            .add_sat => self.builder.buildAddSat(lhs, rhs) catch return LowerError.OutOfMemory,
            .sub_sat => self.builder.buildSubSat(lhs, rhs) catch return LowerError.OutOfMemory,
            .mul_sat => self.builder.buildMulSat(lhs, rhs) catch return LowerError.OutOfMemory,

            // Comparison
            .eq => self.builder.buildICmp(.eq, lhs, rhs) catch return LowerError.OutOfMemory,
            .not_eq => self.builder.buildICmp(.ne, lhs, rhs) catch return LowerError.OutOfMemory,
            .lt => self.builder.buildICmp(.slt, lhs, rhs) catch return LowerError.OutOfMemory,
            .gt => self.builder.buildICmp(.sgt, lhs, rhs) catch return LowerError.OutOfMemory,
            .lt_eq => self.builder.buildICmp(.sle, lhs, rhs) catch return LowerError.OutOfMemory,
            .gt_eq => self.builder.buildICmp(.sge, lhs, rhs) catch return LowerError.OutOfMemory,

            // Bitwise
            .bit_and => self.builder.buildAnd(lhs, rhs) catch return LowerError.OutOfMemory,
            .bit_or => self.builder.buildOr(lhs, rhs) catch return LowerError.OutOfMemory,
            .bit_xor => self.builder.buildXor(lhs, rhs) catch return LowerError.OutOfMemory,
            .shl => self.builder.buildShl(lhs, rhs) catch return LowerError.OutOfMemory,
            .shr => self.builder.buildAShr(lhs, rhs) catch return LowerError.OutOfMemory,

            else => self.builder.constI32(0) catch return LowerError.OutOfMemory,
        };
    }

    /// Lower short-circuit logical operators.
    fn lowerLogicalBinary(self: *Lowerer, bin: *ast.Binary) LowerError!Value {
        const lhs = try self.lowerExpr(bin.left);

        // Create blocks
        const rhs_bb = self.builder.createBlock(if (bin.op == .and_) "and.rhs" else "or.rhs") catch
            return LowerError.OutOfMemory;
        const merge_bb = self.builder.createBlock(if (bin.op == .and_) "and.merge" else "or.merge") catch
            return LowerError.OutOfMemory;

        const lhs_bb = self.builder.getCurrentBlock() orelse return LowerError.NoCurrentBlock;

        if (bin.op == .and_) {
            // AND: if LHS is false, skip RHS
            _ = self.builder.buildCondBr(lhs, rhs_bb, merge_bb) catch {};
        } else {
            // OR: if LHS is true, skip RHS
            _ = self.builder.buildCondBr(lhs, merge_bb, rhs_bb) catch {};
        }

        // Evaluate RHS
        self.builder.positionAtEnd(rhs_bb);
        const rhs = try self.lowerExpr(bin.right);
        const rhs_end_bb = self.builder.getCurrentBlock() orelse return LowerError.NoCurrentBlock;
        _ = self.builder.buildBr(merge_bb) catch {};

        // Merge
        self.builder.positionAtEnd(merge_bb);

        const short_circuit_val = if (bin.op == .and_)
            self.builder.constBool(false) catch return LowerError.OutOfMemory
        else
            self.builder.constBool(true) catch return LowerError.OutOfMemory;

        return self.builder.buildPhi(.bool_, &.{
            .{ .value = short_circuit_val, .block = lhs_bb },
            .{ .value = rhs, .block = rhs_end_bb },
        }) catch return LowerError.OutOfMemory;
    }

    /// Lower a unary expression.
    fn lowerUnary(self: *Lowerer, un: *ast.Unary) LowerError!Value {
        const operand = try self.lowerExpr(un.operand);

        return switch (un.op) {
            .negate => self.builder.buildNeg(operand) catch return LowerError.OutOfMemory,
            .not => self.builder.buildNot(operand) catch return LowerError.OutOfMemory,
            .ref => self.builder.buildBorrow(operand) catch return LowerError.OutOfMemory,
            .ref_mut => self.builder.buildBorrowMut(operand) catch return LowerError.OutOfMemory,
            .deref => {
                // TODO: proper dereference
                return operand;
            },
        };
    }

    /// Lower a function call.
    fn lowerCall(self: *Lowerer, call: *ast.Call) LowerError!Value {
        // Get function name
        const func_name = switch (call.callee) {
            .identifier => |id| id.name,
            else => return self.builder.constI32(0) catch return LowerError.OutOfMemory,
        };

        // Evaluate arguments
        var args = std.ArrayListUnmanaged(Value){};
        defer args.deinit(self.allocator);

        for (call.args) |arg| {
            const val = try self.lowerExpr(arg);
            args.append(self.allocator, val) catch return LowerError.OutOfMemory;
        }

        // TODO: Get actual return type from function signature
        return self.builder.buildCall(func_name, args.items, .i32_) catch
            return LowerError.OutOfMemory;
    }

    /// Lower an if expression.
    fn lowerIf(self: *Lowerer, if_expr: *ast.IfExpr) LowerError!Value {
        const cond = try self.lowerExpr(if_expr.condition);

        // Create blocks
        const then_bb = self.builder.createBlock("then") catch return LowerError.OutOfMemory;
        const else_bb = self.builder.createBlock("else") catch return LowerError.OutOfMemory;
        const merge_bb = self.builder.createBlock("ifcont") catch return LowerError.OutOfMemory;

        _ = self.builder.buildCondBr(cond, then_bb, else_bb) catch {};

        // Then block
        self.builder.positionAtEnd(then_bb);
        try self.pushScope();
        const then_val = try self.lowerExpr(if_expr.then_branch);
        try self.popScope();
        const then_terminated = self.builder.isTerminated();
        const then_end_bb = self.builder.getCurrentBlock() orelse return LowerError.NoCurrentBlock;
        if (!then_terminated) {
            _ = self.builder.buildBr(merge_bb) catch {};
        }

        // Else block
        self.builder.positionAtEnd(else_bb);
        try self.pushScope();
        const else_val = if (if_expr.else_branch) |else_b|
            try self.lowerExpr(else_b)
        else
            self.builder.constI32(0) catch return LowerError.OutOfMemory;
        try self.popScope();
        const else_terminated = self.builder.isTerminated();
        const else_end_bb = self.builder.getCurrentBlock() orelse return LowerError.NoCurrentBlock;
        if (!else_terminated) {
            _ = self.builder.buildBr(merge_bb) catch {};
        }

        // Merge block
        self.builder.positionAtEnd(merge_bb);

        if (then_terminated and else_terminated) {
            // Both branches terminate, merge is unreachable
            return self.builder.constI32(0) catch return LowerError.OutOfMemory;
        }

        return self.builder.buildPhi(then_val.ty, &.{
            .{ .value = then_val, .block = then_end_bb },
            .{ .value = else_val, .block = else_end_bb },
        }) catch return LowerError.OutOfMemory;
    }

    /// Lower a range expression.
    fn lowerRange(self: *Lowerer, range: *ast.Range) LowerError!Value {
        // Ranges as values are typically used in for loops
        // For now, return a placeholder
        _ = range;
        return self.builder.constI32(0) catch return LowerError.OutOfMemory;
    }

    // ========================================================================
    // Type Conversion
    // ========================================================================

    /// Convert AST type expression to IR type.
    fn typeExprToIrType(self: *Lowerer, type_expr: ast.TypeExpr) LowerError!IrType {
        _ = self;
        return switch (type_expr) {
            .named => |named| {
                if (std.mem.eql(u8, named.name, "i8")) return .i8_;
                if (std.mem.eql(u8, named.name, "i16")) return .i16_;
                if (std.mem.eql(u8, named.name, "i32")) return .i32_;
                if (std.mem.eql(u8, named.name, "i64")) return .i64_;
                if (std.mem.eql(u8, named.name, "i128")) return .i128_;
                if (std.mem.eql(u8, named.name, "u8")) return .u8_;
                if (std.mem.eql(u8, named.name, "u16")) return .u16_;
                if (std.mem.eql(u8, named.name, "u32")) return .u32_;
                if (std.mem.eql(u8, named.name, "u64")) return .u64_;
                if (std.mem.eql(u8, named.name, "u128")) return .u128_;
                if (std.mem.eql(u8, named.name, "f32")) return .f32_;
                if (std.mem.eql(u8, named.name, "f64")) return .f64_;
                if (std.mem.eql(u8, named.name, "bool")) return .bool_;
                if (std.mem.eql(u8, named.name, "char")) return .char_;
                if (std.mem.eql(u8, named.name, "isize")) return .isize_;
                if (std.mem.eql(u8, named.name, "usize")) return .usize_;
                // Default to i32 for unknown types
                return .i32_;
            },
            else => .i32_, // Default
        };
    }

    // ========================================================================
    // Scope Management
    // ========================================================================

    fn pushScope(self: *Lowerer) LowerError!void {
        self.scope_stack.append(self.allocator, .{
            .variables = .{},
        }) catch return LowerError.OutOfMemory;
    }

    fn popScope(self: *Lowerer) LowerError!void {
        if (self.scope_stack.items.len == 0) return;

        const scope = self.scope_stack.pop() orelse return;
        defer @constCast(&scope).variables.deinit(self.allocator);

        // Drop variables from this scope (ownership cleanup)
        for (scope.variables.items) |name| {
            if (self.named_values.get(name)) |named| {
                // Only drop non-Copy types
                if (!named.ty.isCopy()) {
                    _ = self.builder.buildDrop(named.value, named.ty) catch {};
                }
            }
            _ = self.named_values.remove(name);
        }
    }

    fn addToCurrentScope(self: *Lowerer, name: []const u8) LowerError!void {
        if (self.scope_stack.items.len == 0) return;
        const scope = &self.scope_stack.items[self.scope_stack.items.len - 1];
        scope.variables.append(self.allocator, name) catch return LowerError.OutOfMemory;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "Lower simple function" {
    const testing = std.testing;

    var module = inst.Module.init(testing.allocator, "test");
    defer module.deinit();

    var lowerer = Lowerer.init(testing.allocator, &module);
    defer lowerer.deinit();

    // Build a simple function manually (simulating parser output)
    // This would normally come from parsing "fn add(a: i32, b: i32) -> i32 { a + b }"

    // For now, verify the lowerer initializes correctly
    try testing.expect(lowerer.named_values.count() == 0);
}
