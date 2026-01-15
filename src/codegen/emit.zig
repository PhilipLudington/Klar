//! AST to LLVM IR emission.
//!
//! Translates typed AST to LLVM IR for native code generation.

const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("../ast.zig");
const types = @import("../types.zig");
const llvm = @import("llvm.zig");

/// Errors that can occur during IR emission.
pub const EmitError = error{
    OutOfMemory,
    UnsupportedFeature,
    InvalidAST,
    LLVMError,
};

/// LLVM IR emitter.
pub const Emitter = struct {
    allocator: Allocator,
    ctx: llvm.Context,
    module: llvm.Module,
    builder: llvm.Builder,
    current_function: ?llvm.ValueRef,

    /// Named values in current scope (locals, params).
    named_values: std.StringHashMap(LocalValue),

    /// Track whether we've seen a terminator in the current block.
    has_terminator: bool,

    const LocalValue = struct {
        value: llvm.ValueRef,
        is_alloca: bool,
        ty: llvm.TypeRef,
    };

    pub fn init(allocator: Allocator, module_name: [:0]const u8) Emitter {
        const ctx = llvm.Context.create();
        const module = llvm.Module.create(module_name, ctx);
        const builder = llvm.Builder.create(ctx);

        return .{
            .allocator = allocator,
            .ctx = ctx,
            .module = module,
            .builder = builder,
            .current_function = null,
            .named_values = std.StringHashMap(LocalValue).init(allocator),
            .has_terminator = false,
        };
    }

    pub fn deinit(self: *Emitter) void {
        self.named_values.deinit();
        self.builder.dispose();
        self.module.dispose();
        self.ctx.dispose();
    }

    /// Emit a complete module.
    pub fn emitModule(self: *Emitter, module: ast.Module) EmitError!void {
        // First pass: declare all functions
        for (module.declarations) |decl| {
            switch (decl) {
                .function => |f| try self.declareFunction(f),
                else => {},
            }
        }

        // Second pass: emit function bodies
        for (module.declarations) |decl| {
            switch (decl) {
                .function => |f| {
                    if (f.body != null) {
                        try self.emitFunction(f);
                    }
                },
                else => {},
            }
        }
    }

    fn declareFunction(self: *Emitter, func: *ast.FunctionDecl) EmitError!void {
        // Build parameter types
        var param_types = std.ArrayListUnmanaged(llvm.TypeRef){};
        defer param_types.deinit(self.allocator);

        for (func.params) |param| {
            const param_ty = try self.typeExprToLLVM(param.type_);
            param_types.append(self.allocator, param_ty) catch return EmitError.OutOfMemory;
        }

        // Get return type
        const return_type = if (func.return_type) |rt|
            try self.typeExprToLLVM(rt)
        else
            llvm.Types.void_(self.ctx);

        const fn_type = llvm.Types.function(return_type, param_types.items, false);
        const name = self.allocator.dupeZ(u8, func.name) catch return EmitError.OutOfMemory;
        defer self.allocator.free(name);
        _ = llvm.addFunction(self.module, name, fn_type);
    }

    fn emitFunction(self: *Emitter, func: *ast.FunctionDecl) EmitError!void {
        const name = self.allocator.dupeZ(u8, func.name) catch return EmitError.OutOfMemory;
        defer self.allocator.free(name);

        // Get or create function
        const function = self.module.getNamedFunction(name) orelse return EmitError.InvalidAST;
        self.current_function = function;

        // Create entry block
        const entry = llvm.appendBasicBlock(self.ctx, function, "entry");
        self.builder.positionAtEnd(entry);
        self.has_terminator = false;

        // Clear named values for new scope
        self.named_values.clearRetainingCapacity();

        // Add parameters to named values
        for (func.params, 0..) |param, i| {
            const param_value = llvm.getParam(function, @intCast(i));
            const param_ty = try self.typeExprToLLVM(param.type_);

            // Allocate stack space for parameter
            const param_name = self.allocator.dupeZ(u8, param.name) catch return EmitError.OutOfMemory;
            defer self.allocator.free(param_name);

            const alloca = self.builder.buildAlloca(param_ty, param_name);
            _ = self.builder.buildStore(param_value, alloca);

            self.named_values.put(param.name, .{
                .value = alloca,
                .is_alloca = true,
                .ty = param_ty,
            }) catch return EmitError.OutOfMemory;
        }

        // Emit function body
        if (func.body) |body| {
            const result = try self.emitBlock(body);

            // If block has a value and we haven't terminated, return it
            if (!self.has_terminator) {
                if (result) |val| {
                    _ = self.builder.buildRet(val);
                } else if (func.return_type == null) {
                    _ = self.builder.buildRetVoid();
                } else {
                    // Return type specified but no value - emit void return
                    _ = self.builder.buildRetVoid();
                }
            }
        }

        self.current_function = null;
    }

    fn emitBlock(self: *Emitter, block: *ast.Block) EmitError!?llvm.ValueRef {
        // Emit statements
        for (block.statements) |stmt| {
            try self.emitStmt(stmt);
            if (self.has_terminator) {
                // Don't emit unreachable code after terminators
                return null;
            }
        }

        // Return final expression value if present
        if (block.final_expr) |expr| {
            return try self.emitExpr(expr);
        }

        return null;
    }

    fn emitStmt(self: *Emitter, stmt: ast.Stmt) EmitError!void {
        switch (stmt) {
            .let_decl => |decl| {
                const value = try self.emitExpr(decl.value);
                // For let (immutable), we can store directly
                // But for consistency, use alloca
                const ty = try self.inferExprType(decl.value);
                const name = self.allocator.dupeZ(u8, decl.name) catch return EmitError.OutOfMemory;
                defer self.allocator.free(name);
                const alloca = self.builder.buildAlloca(ty, name);
                _ = self.builder.buildStore(value, alloca);
                self.named_values.put(decl.name, .{
                    .value = alloca,
                    .is_alloca = true,
                    .ty = ty,
                }) catch return EmitError.OutOfMemory;
            },
            .var_decl => |decl| {
                const value = try self.emitExpr(decl.value);
                const ty = try self.inferExprType(decl.value);
                const name = self.allocator.dupeZ(u8, decl.name) catch return EmitError.OutOfMemory;
                defer self.allocator.free(name);
                const alloca = self.builder.buildAlloca(ty, name);
                _ = self.builder.buildStore(value, alloca);
                self.named_values.put(decl.name, .{
                    .value = alloca,
                    .is_alloca = true,
                    .ty = ty,
                }) catch return EmitError.OutOfMemory;
            },
            .return_stmt => |ret| {
                if (ret.value) |val| {
                    const result = try self.emitExpr(val);
                    _ = self.builder.buildRet(result);
                } else {
                    _ = self.builder.buildRetVoid();
                }
                self.has_terminator = true;
            },
            .expr_stmt => |expr_stmt| {
                _ = try self.emitExpr(expr_stmt.expr);
            },
            .assignment => |assign| {
                const value = try self.emitExpr(assign.value);
                switch (assign.target) {
                    .identifier => |id| {
                        if (self.named_values.get(id.name)) |local| {
                            if (local.is_alloca) {
                                _ = self.builder.buildStore(value, local.value);
                            }
                        }
                    },
                    else => {},
                }
            },
            .while_loop => |loop| {
                try self.emitWhileLoop(loop);
            },
            .for_loop => |loop| {
                _ = loop;
                // TODO: implement for loops
            },
            .break_stmt, .continue_stmt, .loop_stmt => {
                // TODO: implement control flow
            },
        }
    }

    fn emitWhileLoop(self: *Emitter, loop: *ast.WhileLoop) EmitError!void {
        const func = self.current_function orelse return EmitError.InvalidAST;

        // Create blocks
        const cond_bb = llvm.appendBasicBlock(self.ctx, func, "while.cond");
        const body_bb = llvm.appendBasicBlock(self.ctx, func, "while.body");
        const end_bb = llvm.appendBasicBlock(self.ctx, func, "while.end");

        // Branch to condition
        _ = self.builder.buildBr(cond_bb);

        // Emit condition
        self.builder.positionAtEnd(cond_bb);
        const cond = try self.emitExpr(loop.condition);
        _ = self.builder.buildCondBr(cond, body_bb, end_bb);

        // Emit body
        self.builder.positionAtEnd(body_bb);
        self.has_terminator = false;
        _ = try self.emitBlock(loop.body);
        if (!self.has_terminator) {
            _ = self.builder.buildBr(cond_bb);
        }

        // Continue after loop
        self.builder.positionAtEnd(end_bb);
        self.has_terminator = false;
    }

    fn emitExpr(self: *Emitter, expr: ast.Expr) EmitError!llvm.ValueRef {
        return switch (expr) {
            .literal => |lit| self.emitLiteral(lit),
            .identifier => |id| self.emitIdentifier(id),
            .binary => |bin| try self.emitBinary(bin),
            .unary => |un| try self.emitUnary(un),
            .call => |call| try self.emitCall(call),
            .if_expr => |if_e| try self.emitIf(if_e),
            .block => |blk| blk: {
                const result = try self.emitBlock(blk);
                break :blk result orelse llvm.Const.int32(self.ctx, 0);
            },
            .grouped => |g| try self.emitExpr(g.expr),
            else => llvm.Const.int32(self.ctx, 0), // Placeholder for unimplemented
        };
    }

    fn emitLiteral(self: *Emitter, lit: ast.Literal) llvm.ValueRef {
        return switch (lit.kind) {
            .int => |v| blk: {
                // Determine appropriate type based on value
                if (v >= std.math.minInt(i32) and v <= std.math.maxInt(i32)) {
                    break :blk llvm.Const.int32(self.ctx, @intCast(v));
                } else {
                    break :blk llvm.Const.int64(self.ctx, @intCast(v));
                }
            },
            .float => |v| llvm.Const.float64(self.ctx, v),
            .bool_ => |v| llvm.Const.int1(self.ctx, v),
            .char => |v| llvm.Const.int(llvm.Types.int32(self.ctx), v, false),
            .string => llvm.Const.int32(self.ctx, 0), // TODO: string handling
        };
    }

    fn emitIdentifier(self: *Emitter, id: ast.Identifier) llvm.ValueRef {
        if (self.named_values.get(id.name)) |local| {
            if (local.is_alloca) {
                const name = self.allocator.dupeZ(u8, id.name) catch
                    return llvm.Const.int32(self.ctx, 0);
                defer self.allocator.free(name);
                return self.builder.buildLoad(local.ty, local.value, name);
            }
            return local.value;
        }
        // Variable not found - return placeholder
        return llvm.Const.int32(self.ctx, 0);
    }

    fn emitBinary(self: *Emitter, bin: *ast.Binary) EmitError!llvm.ValueRef {
        // Short-circuit evaluation for logical operators
        if (bin.op == .and_ or bin.op == .or_) {
            return self.emitLogicalBinary(bin);
        }

        const lhs = try self.emitExpr(bin.left);
        const rhs = try self.emitExpr(bin.right);

        return switch (bin.op) {
            // Arithmetic
            .add => self.builder.buildAdd(lhs, rhs, "addtmp"),
            .sub => self.builder.buildSub(lhs, rhs, "subtmp"),
            .mul => self.builder.buildMul(lhs, rhs, "multmp"),
            .div => self.builder.buildSDiv(lhs, rhs, "divtmp"),
            .mod => self.builder.buildSRem(lhs, rhs, "modtmp"),

            // Comparison
            .eq => self.builder.buildICmp(llvm.c.LLVMIntEQ, lhs, rhs, "eqtmp"),
            .not_eq => self.builder.buildICmp(llvm.c.LLVMIntNE, lhs, rhs, "netmp"),
            .lt => self.builder.buildICmp(llvm.c.LLVMIntSLT, lhs, rhs, "lttmp"),
            .gt => self.builder.buildICmp(llvm.c.LLVMIntSGT, lhs, rhs, "gttmp"),
            .lt_eq => self.builder.buildICmp(llvm.c.LLVMIntSLE, lhs, rhs, "letmp"),
            .gt_eq => self.builder.buildICmp(llvm.c.LLVMIntSGE, lhs, rhs, "getmp"),

            // Bitwise
            .bit_and => self.builder.buildAnd(lhs, rhs, "andtmp"),
            .bit_or => self.builder.buildOr(lhs, rhs, "ortmp"),
            .bit_xor => self.builder.buildXor(lhs, rhs, "xortmp"),
            .shl => self.builder.buildShl(lhs, rhs, "shltmp"),
            .shr => self.builder.buildAShr(lhs, rhs, "shrtmp"),

            else => llvm.Const.int32(self.ctx, 0), // Placeholder
        };
    }

    fn emitLogicalBinary(self: *Emitter, bin: *ast.Binary) EmitError!llvm.ValueRef {
        const func = self.current_function orelse return EmitError.InvalidAST;

        if (bin.op == .and_) {
            // Short-circuit AND: if LHS is false, skip RHS
            const rhs_bb = llvm.appendBasicBlock(self.ctx, func, "and.rhs");
            const merge_bb = llvm.appendBasicBlock(self.ctx, func, "and.merge");

            const lhs = try self.emitExpr(bin.left);
            const lhs_bb = llvm.c.LLVMGetInsertBlock(self.builder.ref);
            _ = self.builder.buildCondBr(lhs, rhs_bb, merge_bb);

            self.builder.positionAtEnd(rhs_bb);
            const rhs = try self.emitExpr(bin.right);
            const rhs_end_bb = llvm.c.LLVMGetInsertBlock(self.builder.ref);
            _ = self.builder.buildBr(merge_bb);

            self.builder.positionAtEnd(merge_bb);
            const phi = self.builder.buildPhi(llvm.Types.int1(self.ctx), "and.result");
            const false_val = llvm.Const.int1(self.ctx, false);
            var incoming_vals = [_]llvm.ValueRef{ false_val, rhs };
            var incoming_blocks = [_]llvm.BasicBlockRef{ lhs_bb, rhs_end_bb };
            llvm.addIncoming(phi, &incoming_vals, &incoming_blocks);
            return phi;
        } else {
            // Short-circuit OR: if LHS is true, skip RHS
            const rhs_bb = llvm.appendBasicBlock(self.ctx, func, "or.rhs");
            const merge_bb = llvm.appendBasicBlock(self.ctx, func, "or.merge");

            const lhs = try self.emitExpr(bin.left);
            const lhs_bb = llvm.c.LLVMGetInsertBlock(self.builder.ref);
            _ = self.builder.buildCondBr(lhs, merge_bb, rhs_bb);

            self.builder.positionAtEnd(rhs_bb);
            const rhs = try self.emitExpr(bin.right);
            const rhs_end_bb = llvm.c.LLVMGetInsertBlock(self.builder.ref);
            _ = self.builder.buildBr(merge_bb);

            self.builder.positionAtEnd(merge_bb);
            const phi = self.builder.buildPhi(llvm.Types.int1(self.ctx), "or.result");
            const true_val = llvm.Const.int1(self.ctx, true);
            var incoming_vals = [_]llvm.ValueRef{ true_val, rhs };
            var incoming_blocks = [_]llvm.BasicBlockRef{ lhs_bb, rhs_end_bb };
            llvm.addIncoming(phi, &incoming_vals, &incoming_blocks);
            return phi;
        }
    }

    fn emitUnary(self: *Emitter, un: *ast.Unary) EmitError!llvm.ValueRef {
        const operand = try self.emitExpr(un.operand);

        return switch (un.op) {
            .negate => self.builder.buildNeg(operand, "negtmp"),
            .not => self.builder.buildNot(operand, "nottmp"),
            else => operand, // Placeholder
        };
    }

    fn emitCall(self: *Emitter, call: *ast.Call) EmitError!llvm.ValueRef {
        // Get function name
        const func_name = switch (call.callee) {
            .identifier => |id| id.name,
            else => return llvm.Const.int32(self.ctx, 0),
        };

        const name = self.allocator.dupeZ(u8, func_name) catch return EmitError.OutOfMemory;
        defer self.allocator.free(name);

        // Get function from module
        const func = self.module.getNamedFunction(name) orelse
            return llvm.Const.int32(self.ctx, 0);

        // Emit arguments
        var args = std.ArrayListUnmanaged(llvm.ValueRef){};
        defer args.deinit(self.allocator);

        for (call.args) |arg| {
            args.append(self.allocator, try self.emitExpr(arg)) catch return EmitError.OutOfMemory;
        }

        const fn_type = llvm.getGlobalValueType(func);
        return self.builder.buildCall(fn_type, func, args.items, "calltmp");
    }

    fn emitIf(self: *Emitter, if_expr: *ast.IfExpr) EmitError!llvm.ValueRef {
        const func = self.current_function orelse return EmitError.InvalidAST;

        const cond = try self.emitExpr(if_expr.condition);

        // Create blocks
        const then_bb = llvm.appendBasicBlock(self.ctx, func, "then");
        const else_bb = llvm.appendBasicBlock(self.ctx, func, "else");
        const merge_bb = llvm.appendBasicBlock(self.ctx, func, "ifcont");

        _ = self.builder.buildCondBr(cond, then_bb, else_bb);

        // Emit then block
        self.builder.positionAtEnd(then_bb);
        self.has_terminator = false;
        const then_val = try self.emitExpr(if_expr.then_branch);
        const then_has_term = self.has_terminator;
        const then_end_bb = llvm.c.LLVMGetInsertBlock(self.builder.ref);
        if (!then_has_term) {
            _ = self.builder.buildBr(merge_bb);
        }

        // Emit else block
        self.builder.positionAtEnd(else_bb);
        self.has_terminator = false;
        const else_val = if (if_expr.else_branch) |else_b|
            try self.emitExpr(else_b)
        else
            llvm.Const.int32(self.ctx, 0);
        const else_has_term = self.has_terminator;
        const else_end_bb = llvm.c.LLVMGetInsertBlock(self.builder.ref);
        if (!else_has_term) {
            _ = self.builder.buildBr(merge_bb);
        }

        // Emit merge block with PHI
        self.builder.positionAtEnd(merge_bb);
        self.has_terminator = false;

        // Only create PHI if both branches reach merge
        if (then_has_term and else_has_term) {
            // Both branches terminate, merge is unreachable
            return llvm.Const.int32(self.ctx, 0);
        }

        const phi = self.builder.buildPhi(llvm.Types.int32(self.ctx), "iftmp");
        if (!then_has_term and !else_has_term) {
            var incoming_vals = [_]llvm.ValueRef{ then_val, else_val };
            var incoming_blocks = [_]llvm.BasicBlockRef{ then_end_bb, else_end_bb };
            llvm.addIncoming(phi, &incoming_vals, &incoming_blocks);
        } else if (!then_has_term) {
            var incoming_vals = [_]llvm.ValueRef{then_val};
            var incoming_blocks = [_]llvm.BasicBlockRef{then_end_bb};
            llvm.addIncoming(phi, &incoming_vals, &incoming_blocks);
        } else {
            var incoming_vals = [_]llvm.ValueRef{else_val};
            var incoming_blocks = [_]llvm.BasicBlockRef{else_end_bb};
            llvm.addIncoming(phi, &incoming_vals, &incoming_blocks);
        }

        return phi;
    }

    /// Convert a type expression to LLVM type.
    fn typeExprToLLVM(self: *Emitter, type_expr: ast.TypeExpr) EmitError!llvm.TypeRef {
        return switch (type_expr) {
            .named => |named| self.namedTypeToLLVM(named.name),
            else => llvm.Types.int32(self.ctx), // Default to i32 for now
        };
    }

    fn namedTypeToLLVM(self: *Emitter, name: []const u8) llvm.TypeRef {
        // Map Klar type names to LLVM types
        if (std.mem.eql(u8, name, "i8")) return llvm.Types.int8(self.ctx);
        if (std.mem.eql(u8, name, "i16")) return llvm.Types.int16(self.ctx);
        if (std.mem.eql(u8, name, "i32")) return llvm.Types.int32(self.ctx);
        if (std.mem.eql(u8, name, "i64")) return llvm.Types.int64(self.ctx);
        if (std.mem.eql(u8, name, "i128")) return llvm.Types.int128(self.ctx);
        if (std.mem.eql(u8, name, "u8")) return llvm.Types.int8(self.ctx);
        if (std.mem.eql(u8, name, "u16")) return llvm.Types.int16(self.ctx);
        if (std.mem.eql(u8, name, "u32")) return llvm.Types.int32(self.ctx);
        if (std.mem.eql(u8, name, "u64")) return llvm.Types.int64(self.ctx);
        if (std.mem.eql(u8, name, "u128")) return llvm.Types.int128(self.ctx);
        if (std.mem.eql(u8, name, "f32")) return llvm.Types.float32(self.ctx);
        if (std.mem.eql(u8, name, "f64")) return llvm.Types.float64(self.ctx);
        if (std.mem.eql(u8, name, "bool")) return llvm.Types.int1(self.ctx);

        // Default to i32
        return llvm.Types.int32(self.ctx);
    }

    /// Infer LLVM type from expression (simplified).
    fn inferExprType(self: *Emitter, expr: ast.Expr) EmitError!llvm.TypeRef {
        return switch (expr) {
            .literal => |lit| switch (lit.kind) {
                .int => llvm.Types.int32(self.ctx),
                .float => llvm.Types.float64(self.ctx),
                .bool_ => llvm.Types.int1(self.ctx),
                .char => llvm.Types.int32(self.ctx),
                .string => llvm.Types.pointer(self.ctx),
            },
            .identifier => |id| {
                if (self.named_values.get(id.name)) |local| {
                    return local.ty;
                }
                return llvm.Types.int32(self.ctx);
            },
            .binary => |bin| {
                // Comparison operators return bool
                switch (bin.op) {
                    .eq, .not_eq, .lt, .gt, .lt_eq, .gt_eq, .and_, .or_ => {
                        return llvm.Types.int1(self.ctx);
                    },
                    else => {
                        return try self.inferExprType(bin.left);
                    },
                }
            },
            else => llvm.Types.int32(self.ctx),
        };
    }

    /// Get the LLVM module reference for further operations.
    pub fn getModule(self: *Emitter) llvm.Module {
        return self.module;
    }

    /// Verify the module.
    pub fn verify(self: *Emitter) !void {
        try self.module.verify();
    }
};
