//! AST to LLVM IR emission.
//!
//! Translates typed AST to LLVM IR for native code generation.

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const ast = @import("../ast.zig");
const types = @import("../types.zig");
const llvm = @import("llvm.zig");
const target = @import("target.zig");
const layout = @import("layout.zig");

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

    /// Target platform information.
    platform: target.Platform,
    /// ABI information for struct passing/returning.
    abi: target.ABI,
    /// Calling convention for the target.
    calling_convention: target.CallingConvention,

    /// Named values in current scope (locals, params).
    named_values: std.StringHashMap(LocalValue),

    /// Track whether we've seen a terminator in the current block.
    has_terminator: bool,

    /// Stack of loop contexts for break/continue.
    loop_stack: std.ArrayListUnmanaged(LoopContext),

    /// Stack of scopes for automatic drop insertion.
    scope_stack: std.ArrayListUnmanaged(ScopeInfo),

    /// Current function's return type info (for optional wrapping).
    current_return_type: ?ReturnTypeInfo,

    /// Cached overflow intrinsic IDs.
    sadd_overflow_id: ?c_uint,
    ssub_overflow_id: ?c_uint,
    smul_overflow_id: ?c_uint,
    uadd_overflow_id: ?c_uint,
    usub_overflow_id: ?c_uint,
    umul_overflow_id: ?c_uint,

    /// Cache of LLVM struct types by name.
    struct_types: std.StringHashMap(StructTypeInfo),
    /// Layout calculator for composite types.
    layout_calc: layout.LayoutCalculator,
    /// Counter for generating unique closure names.
    closure_counter: u32 = 0,
    /// Cache of generated closure types.
    closure_types: std.StringHashMap(ClosureTypeInfo),

    // --- Debug info fields ---
    /// Debug info builder (null if debug info is disabled).
    di_builder: ?llvm.DIBuilder,
    /// Compile unit metadata (root of debug info).
    di_compile_unit: ?llvm.MetadataRef,
    /// Current file metadata.
    di_file: ?llvm.MetadataRef,
    /// Current scope for debug locations.
    di_scope: ?llvm.MetadataRef,

    // --- Type declarations (must come after all fields in Zig 0.15+) ---

    const ReturnTypeInfo = struct {
        llvm_type: llvm.TypeRef,
        is_optional: bool,
        inner_type: ?llvm.TypeRef, // Non-null if is_optional
    };

    const StructTypeInfo = struct {
        llvm_type: llvm.TypeRef,
        field_indices: []const u32,
        field_names: []const []const u8,
    };

    const LocalValue = struct {
        value: llvm.ValueRef,
        is_alloca: bool,
        ty: llvm.TypeRef,
        is_signed: bool,
        /// For struct variables, the name of the struct type for field resolution.
        struct_type_name: ?[]const u8 = null,
        /// For Rc[T] or reference types, the inner type that can be dereferenced.
        inner_type: ?llvm.TypeRef = null,
    };

    const LoopContext = struct {
        continue_block: llvm.BasicBlockRef,
        break_block: llvm.BasicBlockRef,
    };

    /// Info about a variable that needs to be dropped at scope exit.
    const DroppableVar = struct {
        name: []const u8,
        alloca: llvm.ValueRef,
        inner_type: llvm.TypeRef,
        /// True if this is an Rc type (needs klar_rc_drop).
        is_rc: bool,
    };

    /// Info about a scope for drop tracking.
    const ScopeInfo = struct {
        /// Variables declared in this scope that need dropping.
        droppables: std.ArrayListUnmanaged(DroppableVar),
        /// True if this scope is a loop body (break/continue need drops).
        is_loop: bool,
    };

    /// Info about a generated closure type.
    const ClosureTypeInfo = struct {
        struct_type: llvm.TypeRef,
        fn_ptr_type: llvm.TypeRef,
        lifted_fn: llvm.ValueRef,
        capture_names: []const []const u8,
    };

    pub fn init(allocator: Allocator, module_name: [:0]const u8) Emitter {
        const ctx = llvm.Context.create();
        const module = llvm.Module.create(module_name, ctx);
        const builder = llvm.Builder.create(ctx);

        // Initialize platform and ABI info
        const platform = target.Platform.current();
        const abi = target.ABI.init(platform);
        const calling_convention = target.CallingConvention.forPlatform(platform);

        // Set target triple on the module for proper code generation
        const triple = target.getDefaultTriple();
        module.setTarget(triple);

        return .{
            .allocator = allocator,
            .ctx = ctx,
            .module = module,
            .builder = builder,
            .current_function = null,
            .platform = platform,
            .abi = abi,
            .calling_convention = calling_convention,
            .named_values = std.StringHashMap(LocalValue).init(allocator),
            .has_terminator = false,
            .loop_stack = .{},
            .scope_stack = .{},
            .current_return_type = null,
            .sadd_overflow_id = null,
            .ssub_overflow_id = null,
            .smul_overflow_id = null,
            .uadd_overflow_id = null,
            .usub_overflow_id = null,
            .umul_overflow_id = null,
            .struct_types = std.StringHashMap(StructTypeInfo).init(allocator),
            .layout_calc = layout.LayoutCalculator.init(allocator, platform),
            .closure_counter = 0,
            .closure_types = std.StringHashMap(ClosureTypeInfo).init(allocator),
            .di_builder = null,
            .di_compile_unit = null,
            .di_file = null,
            .di_scope = null,
        };
    }

    /// Initialize debug info for the module.
    pub fn initDebugInfo(self: *Emitter, filename: []const u8, directory: []const u8) void {
        const di_builder = llvm.DIBuilder.create(self.module);

        // Create the file
        const di_file = di_builder.createFile(filename, directory);

        // Create the compile unit
        // Use C99 as the language since DWARF doesn't have a Klar entry
        const di_compile_unit = di_builder.createCompileUnit(
            llvm.c.LLVMDWARFSourceLanguageC99,
            di_file,
            "Klar Compiler 0.3.0",
            false, // not optimized
            "", // flags
            0, // runtime version
            "", // split name
            llvm.c.LLVMDWARFEmissionFull,
            0, // DWO id
            false, // split debug inlining
            false, // debug info for profiling
            "", // sysroot
            "", // SDK
        );

        self.di_builder = di_builder;
        self.di_compile_unit = di_compile_unit;
        self.di_file = di_file;
        self.di_scope = di_compile_unit;
    }

    /// Finalize debug info. Must be called before module verification.
    pub fn finalizeDebugInfo(self: *Emitter) void {
        if (self.di_builder) |di_builder| {
            di_builder.finalize();
        }
    }

    pub fn deinit(self: *Emitter) void {
        // Dispose debug info builder first
        if (self.di_builder) |di_builder| {
            di_builder.dispose();
        }
        self.loop_stack.deinit(self.allocator);
        // Clean up any remaining scopes (shouldn't normally have any)
        for (self.scope_stack.items) |*scope| {
            scope.droppables.deinit(self.allocator);
        }
        self.scope_stack.deinit(self.allocator);
        self.named_values.deinit();
        // Free struct type info allocations
        var it = self.struct_types.valueIterator();
        while (it.next()) |info| {
            self.allocator.free(info.field_indices);
            self.allocator.free(info.field_names);
        }
        self.struct_types.deinit();
        // Free closure type info allocations
        var cit = self.closure_types.valueIterator();
        while (cit.next()) |info| {
            self.allocator.free(info.capture_names);
        }
        self.closure_types.deinit();
        self.layout_calc.deinit();
        self.builder.dispose();
        self.module.dispose();
        self.ctx.dispose();
    }

    /// Emit a complete module.
    pub fn emitModule(self: *Emitter, module: ast.Module) EmitError!void {
        // First pass: collect struct declarations for field name resolution
        for (module.declarations) |decl| {
            switch (decl) {
                .struct_decl => |s| try self.registerStructDecl(s),
                else => {},
            }
        }

        // Second pass: declare all functions
        for (module.declarations) |decl| {
            switch (decl) {
                .function => |f| try self.declareFunction(f),
                else => {},
            }
        }

        // Third pass: emit function bodies
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

    /// Register a struct declaration for later field name resolution.
    fn registerStructDecl(self: *Emitter, struct_decl: *ast.StructDecl) EmitError!void {
        // Skip if already registered
        if (self.struct_types.contains(struct_decl.name)) {
            return;
        }

        // Use getOrCreateStructType to register the struct with field names
        _ = try self.getOrCreateStructType(struct_decl.name, struct_decl.fields);
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
        const llvm_func = llvm.addFunction(self.module, name, fn_type);

        // Set the calling convention for proper ABI compliance
        llvm.setFunctionCallConv(llvm_func, self.calling_convention.toLLVM());
    }

    fn emitFunction(self: *Emitter, func: *ast.FunctionDecl) EmitError!void {
        const name = self.allocator.dupeZ(u8, func.name) catch return EmitError.OutOfMemory;
        defer self.allocator.free(name);

        // Get or create function
        const function = self.module.getNamedFunction(name) orelse return EmitError.InvalidAST;
        self.current_function = function;

        // Set up return type info
        if (func.return_type) |rt| {
            const llvm_rt = try self.typeExprToLLVM(rt);
            const is_opt = rt == .optional;
            self.current_return_type = .{
                .llvm_type = llvm_rt,
                .is_optional = is_opt,
                .inner_type = if (is_opt) try self.typeExprToLLVM(rt.optional.inner) else null,
            };
        } else {
            self.current_return_type = null;
        }
        defer self.current_return_type = null;

        // Create debug info for function if enabled
        if (self.di_builder) |di_builder| {
            if (self.di_file) |di_file| {
                // Create subroutine type (empty for now - just void return for simplicity)
                const subroutine_type = di_builder.createSubroutineType(
                    di_file,
                    &.{}, // empty param types
                    llvm.c.LLVMDIFlagZero,
                );

                // Get line number from function span
                const line_no: c_uint = @intCast(func.span.line);

                // Create function debug info
                const subprogram = di_builder.createFunction(
                    self.di_compile_unit.?, // scope (compile unit)
                    func.name, // name
                    func.name, // linkage name
                    di_file, // file
                    line_no, // line number
                    subroutine_type, // type
                    false, // is local
                    true, // is definition
                    line_no, // scope line
                    llvm.c.LLVMDIFlagZero, // flags
                    false, // is optimized
                );

                // Attach subprogram to function
                llvm.setSubprogram(function, subprogram);

                // Set as current scope for debug locations
                self.di_scope = subprogram;
            }
        }

        // Create entry block
        const entry = llvm.appendBasicBlock(self.ctx, function, "entry");
        self.builder.positionAtEnd(entry);
        self.has_terminator = false;

        // Set debug location for entry block if debug info enabled
        if (self.di_builder != null) {
            if (self.di_scope) |scope| {
                const loc = llvm.createDebugLocation(
                    self.ctx,
                    @intCast(func.span.line),
                    @intCast(func.span.column),
                    scope,
                    null,
                );
                llvm.setCurrentDebugLocation(self.builder, loc);
            }
        }

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

            const is_signed = self.isTypeSigned(param.type_);
            self.named_values.put(param.name, .{
                .value = alloca,
                .is_alloca = true,
                .ty = param_ty,
                .is_signed = is_signed,
            }) catch return EmitError.OutOfMemory;
        }

        // Emit function body
        if (func.body) |body| {
            // Push function scope for drop tracking
            try self.pushScope(false);

            const result = try self.emitBlock(body);

            // Check if the final_expr is an if-without-else (produces dummy value)
            // In that case, for optional returns, we should return None instead
            const has_if_without_else = if (body.final_expr) |expr|
                expr == .if_expr and expr.if_expr.else_branch == null
            else
                false;

            // If block has a value and we haven't terminated, return it
            if (!self.has_terminator) {
                // For optional return types with if-without-else as final expr,
                // treat it as if there's no value (return None)
                if (has_if_without_else) {
                    if (self.current_return_type) |rt_info| {
                        if (rt_info.is_optional) {
                            self.emitDropsForReturn();
                            const none_val = self.emitNone(rt_info.llvm_type);
                            _ = self.builder.buildRet(none_val);
                            self.popScope();
                            self.current_function = null;
                            return;
                        }
                    }
                }

                if (result) |val| {
                    // If return type is optional and value isn't already optional, wrap in Some
                    if (self.current_return_type) |rt_info| {
                        if (rt_info.is_optional) {
                            const val_type = llvm.typeOf(val);
                            const val_kind = llvm.getTypeKind(val_type);
                            // Check if value is already an optional (struct with our layout)
                            if (val_kind != llvm.c.LLVMStructTypeKind or
                                llvm.c.LLVMCountStructElementTypes(val_type) != 2)
                            {
                                // Not an optional, wrap in Some
                                self.emitDropsForReturn();
                                const some_val = self.emitSome(val, rt_info.inner_type.?);
                                _ = self.builder.buildRet(some_val);
                                self.popScope();
                                self.current_function = null;
                                return;
                            }
                        }
                    }
                    self.emitDropsForReturn();
                    _ = self.builder.buildRet(val);
                } else if (func.return_type == null) {
                    self.emitDropsForReturn();
                    _ = self.builder.buildRetVoid();
                } else if (self.current_return_type) |rt_info| {
                    if (rt_info.is_optional) {
                        // Return None for optional type with no value
                        self.emitDropsForReturn();
                        const none_val = self.emitNone(rt_info.llvm_type);
                        _ = self.builder.buildRet(none_val);
                    } else {
                        self.emitDropsForReturn();
                        _ = self.builder.buildRetVoid();
                    }
                } else {
                    // Return type specified but no value - emit void return
                    self.emitDropsForReturn();
                    _ = self.builder.buildRetVoid();
                }
            }

            // Pop the function scope - just cleanup, drops already emitted
            // Note: when we reach here from the non-terminator path, drops were emitted by emitDropsForReturn
            // When we reach here from the terminator path (has_terminator), drops were emitted by the statement
            if (self.scope_stack.pop()) |scope| {
                var s = scope;
                s.droppables.deinit(self.allocator);
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
                const is_signed = if (decl.type_) |t| self.isTypeSigned(t) else true;
                // Extract struct type name if this is a struct literal
                const struct_type_name = self.getStructTypeName(decl.value);
                // For Rc types, track the inner type for dereferencing
                const inner_type = self.tryGetRcInnerType(decl.value);
                self.named_values.put(decl.name, .{
                    .value = alloca,
                    .is_alloca = true,
                    .ty = ty,
                    .is_signed = is_signed,
                    .struct_type_name = struct_type_name,
                    .inner_type = inner_type,
                }) catch return EmitError.OutOfMemory;

                // Register Rc variables for automatic dropping
                if (inner_type) |it| {
                    try self.registerDroppable(decl.name, alloca, it, true);
                }
            },
            .var_decl => |decl| {
                const value = try self.emitExpr(decl.value);
                const ty = try self.inferExprType(decl.value);
                const name = self.allocator.dupeZ(u8, decl.name) catch return EmitError.OutOfMemory;
                defer self.allocator.free(name);
                const alloca = self.builder.buildAlloca(ty, name);
                _ = self.builder.buildStore(value, alloca);
                const is_signed = if (decl.type_) |t| self.isTypeSigned(t) else true;
                // Extract struct type name if this is a struct literal
                const struct_type_name = self.getStructTypeName(decl.value);
                // For Rc types, track the inner type for dereferencing
                const inner_type = self.tryGetRcInnerType(decl.value);
                self.named_values.put(decl.name, .{
                    .value = alloca,
                    .is_alloca = true,
                    .ty = ty,
                    .is_signed = is_signed,
                    .struct_type_name = struct_type_name,
                    .inner_type = inner_type,
                }) catch return EmitError.OutOfMemory;

                // Register Rc variables for automatic dropping
                if (inner_type) |it| {
                    try self.registerDroppable(decl.name, alloca, it, true);
                }
            },
            .return_stmt => |ret| {
                // Emit drops for all variables before returning
                self.emitDropsForReturn();

                if (ret.value) |val| {
                    var result = try self.emitExpr(val);
                    // If return type is optional, wrap value in Some if needed
                    if (self.current_return_type) |rt_info| {
                        if (rt_info.is_optional) {
                            const val_type = llvm.typeOf(result);
                            const val_kind = llvm.getTypeKind(val_type);
                            // Check if value is already an optional (struct with our layout)
                            if (val_kind != llvm.c.LLVMStructTypeKind or
                                llvm.c.LLVMCountStructElementTypes(val_type) != 2)
                            {
                                // Not an optional, wrap in Some
                                result = self.emitSome(result, rt_info.inner_type.?);
                            }
                        }
                    }
                    _ = self.builder.buildRet(result);
                } else {
                    // Return with no value
                    if (self.current_return_type) |rt_info| {
                        if (rt_info.is_optional) {
                            // Return None for optional type
                            const none_val = self.emitNone(rt_info.llvm_type);
                            _ = self.builder.buildRet(none_val);
                        } else {
                            _ = self.builder.buildRetVoid();
                        }
                    } else {
                        _ = self.builder.buildRetVoid();
                    }
                }
                self.has_terminator = true;
            },
            .expr_stmt => |expr_stmt| {
                _ = try self.emitExpr(expr_stmt.expr);
            },
            .assignment => |assign| {
                // Handle assignment statements (legacy path - parser now uses binary with .assign op)
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
                try self.emitForLoop(loop);
            },
            .loop_stmt => |loop| {
                try self.emitInfiniteLoop(loop);
            },
            .break_stmt => {
                try self.emitBreak();
            },
            .continue_stmt => {
                try self.emitContinue();
            },
        }
    }

    fn emitWhileLoop(self: *Emitter, loop: *ast.WhileLoop) EmitError!void {
        const func = self.current_function orelse return EmitError.InvalidAST;

        // Create blocks
        const cond_bb = llvm.appendBasicBlock(self.ctx, func, "while.cond");
        const body_bb = llvm.appendBasicBlock(self.ctx, func, "while.body");
        const end_bb = llvm.appendBasicBlock(self.ctx, func, "while.end");

        // Push loop context for break/continue
        self.loop_stack.append(self.allocator, .{
            .continue_block = cond_bb,
            .break_block = end_bb,
        }) catch return EmitError.OutOfMemory;
        defer _ = self.loop_stack.pop();

        // Branch to condition
        _ = self.builder.buildBr(cond_bb);

        // Emit condition
        self.builder.positionAtEnd(cond_bb);
        const cond = try self.emitExpr(loop.condition);
        _ = self.builder.buildCondBr(cond, body_bb, end_bb);

        // Emit body with loop scope for drop tracking
        self.builder.positionAtEnd(body_bb);
        self.has_terminator = false;
        try self.pushScope(true); // is_loop = true
        _ = try self.emitBlock(loop.body);
        if (!self.has_terminator) {
            // Emit drops for variables declared in this iteration before looping back
            self.popScope();
            _ = self.builder.buildBr(cond_bb);
        } else {
            // Terminator already emitted (break/continue/return)
            // Drops were already emitted by the terminator, just clean up scope without emitting
            if (self.scope_stack.pop()) |scope| {
                var s = scope;
                s.droppables.deinit(self.allocator);
            }
        }

        // Continue after loop
        self.builder.positionAtEnd(end_bb);
        self.has_terminator = false;
    }

    fn emitForLoop(self: *Emitter, loop: *ast.ForLoop) EmitError!void {
        const func = self.current_function orelse return EmitError.InvalidAST;

        // Get the binding name from the pattern
        const binding_name = switch (loop.pattern) {
            .binding => |b| b.name,
            else => return EmitError.UnsupportedFeature, // Only simple bindings for now
        };

        // For now, handle range-based for loops: for i in start..end { body }
        // We emit this as:
        //   %i = alloca i32
        //   store start, %i
        //   br cond
        // cond:
        //   %cur = load %i
        //   %cmp = icmp slt %cur, end
        //   br %cmp, body, end
        // body:
        //   ... loop body ...
        //   %next = add %cur, 1
        //   store %next, %i
        //   br cond
        // end:

        // Get range bounds
        const range = switch (loop.iterable) {
            .range => |r| r,
            else => return EmitError.UnsupportedFeature, // Only range iteration for now
        };

        // Range can have optional start/end; default start to 0
        const start_val = if (range.start) |s|
            try self.emitExpr(s)
        else
            llvm.Const.int32(self.ctx, 0);
        const end_val = if (range.end) |e|
            try self.emitExpr(e)
        else
            return EmitError.UnsupportedFeature; // End is required for for loops
        const iter_ty = llvm.Types.int32(self.ctx);

        // Allocate loop variable
        const var_name = self.allocator.dupeZ(u8, binding_name) catch return EmitError.OutOfMemory;
        defer self.allocator.free(var_name);
        const iter_alloca = self.builder.buildAlloca(iter_ty, var_name);
        _ = self.builder.buildStore(start_val, iter_alloca);

        // Add loop variable to scope
        self.named_values.put(binding_name, .{
            .value = iter_alloca,
            .is_alloca = true,
            .ty = iter_ty,
            .is_signed = true,
        }) catch return EmitError.OutOfMemory;

        // Create blocks
        const cond_bb = llvm.appendBasicBlock(self.ctx, func, "for.cond");
        const body_bb = llvm.appendBasicBlock(self.ctx, func, "for.body");
        const incr_bb = llvm.appendBasicBlock(self.ctx, func, "for.incr");
        const end_bb = llvm.appendBasicBlock(self.ctx, func, "for.end");

        // Push loop context (continue goes to increment, break goes to end)
        self.loop_stack.append(self.allocator, .{
            .continue_block = incr_bb,
            .break_block = end_bb,
        }) catch return EmitError.OutOfMemory;
        defer _ = self.loop_stack.pop();

        // Branch to condition
        _ = self.builder.buildBr(cond_bb);

        // Emit condition: i < end (or i <= end for inclusive)
        self.builder.positionAtEnd(cond_bb);
        const cur_val = self.builder.buildLoad(iter_ty, iter_alloca, "for.cur");
        const cmp = if (range.inclusive)
            self.builder.buildICmp(llvm.c.LLVMIntSLE, cur_val, end_val, "for.cmp")
        else
            self.builder.buildICmp(llvm.c.LLVMIntSLT, cur_val, end_val, "for.cmp");
        _ = self.builder.buildCondBr(cmp, body_bb, end_bb);

        // Emit body with loop scope for drop tracking
        self.builder.positionAtEnd(body_bb);
        self.has_terminator = false;
        try self.pushScope(true); // is_loop = true
        _ = try self.emitBlock(loop.body);
        if (!self.has_terminator) {
            // Emit drops for variables declared in this iteration before continuing
            self.popScope();
            _ = self.builder.buildBr(incr_bb);
        } else {
            // Terminator already emitted (break/continue/return)
            // Drops were already emitted by the terminator, just clean up scope without emitting
            if (self.scope_stack.pop()) |scope| {
                var s = scope;
                s.droppables.deinit(self.allocator);
            }
        }

        // Emit increment
        self.builder.positionAtEnd(incr_bb);
        const cur_val2 = self.builder.buildLoad(iter_ty, iter_alloca, "for.cur2");
        const one = llvm.Const.int32(self.ctx, 1);
        const next_val = self.builder.buildAdd(cur_val2, one, "for.next");
        _ = self.builder.buildStore(next_val, iter_alloca);
        _ = self.builder.buildBr(cond_bb);

        // Continue after loop
        self.builder.positionAtEnd(end_bb);
        self.has_terminator = false;

        // Remove loop variable from scope
        _ = self.named_values.remove(binding_name);
    }

    fn emitInfiniteLoop(self: *Emitter, loop: *ast.LoopStmt) EmitError!void {
        const func = self.current_function orelse return EmitError.InvalidAST;

        // Create blocks
        const body_bb = llvm.appendBasicBlock(self.ctx, func, "loop.body");
        const end_bb = llvm.appendBasicBlock(self.ctx, func, "loop.end");

        // Push loop context
        self.loop_stack.append(self.allocator, .{
            .continue_block = body_bb,
            .break_block = end_bb,
        }) catch return EmitError.OutOfMemory;
        defer _ = self.loop_stack.pop();

        // Branch to body
        _ = self.builder.buildBr(body_bb);

        // Emit body with loop scope for drop tracking
        self.builder.positionAtEnd(body_bb);
        self.has_terminator = false;
        try self.pushScope(true); // is_loop = true
        _ = try self.emitBlock(loop.body);
        if (!self.has_terminator) {
            // Emit drops for variables declared in this iteration before looping
            self.popScope();
            _ = self.builder.buildBr(body_bb);
        } else {
            // Terminator already emitted (break/continue/return)
            // Drops were already emitted by the terminator, just clean up scope without emitting
            if (self.scope_stack.pop()) |scope| {
                var s = scope;
                s.droppables.deinit(self.allocator);
            }
        }

        // Continue after loop
        self.builder.positionAtEnd(end_bb);
        self.has_terminator = false;
    }

    fn emitBreak(self: *Emitter) EmitError!void {
        if (self.loop_stack.items.len == 0) {
            return EmitError.InvalidAST; // break outside of loop
        }
        // Emit drops for all scopes until we reach the loop scope
        self.emitDropsForLoopExit();
        const loop_ctx = self.loop_stack.items[self.loop_stack.items.len - 1];
        _ = self.builder.buildBr(loop_ctx.break_block);
        self.has_terminator = true;
    }

    fn emitContinue(self: *Emitter) EmitError!void {
        if (self.loop_stack.items.len == 0) {
            return EmitError.InvalidAST; // continue outside of loop
        }
        // Emit drops for all scopes until we reach the loop scope
        self.emitDropsForLoopExit();
        const loop_ctx = self.loop_stack.items[self.loop_stack.items.len - 1];
        _ = self.builder.buildBr(loop_ctx.continue_block);
        self.has_terminator = true;
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
            // Composite types
            .struct_literal => |s| try self.emitStructLiteral(s),
            .array_literal => |a| try self.emitArrayLiteral(a),
            .tuple_literal => |t| try self.emitTupleLiteral(t),
            .field => |f| try self.emitFieldAccess(f),
            .index => |i| try self.emitIndexAccess(i),
            .postfix => |p| try self.emitPostfix(p),
            .method_call => |m| try self.emitMethodCall(m),
            .closure => |c| try self.emitClosure(c),
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
            .string => |s| blk: {
                // Create a null-terminated string and get a pointer to it
                const str_z = self.allocator.dupeZ(u8, s) catch return llvm.Const.int32(self.ctx, 0);
                defer self.allocator.free(str_z);
                break :blk self.builder.buildGlobalStringPtr(str_z, "str");
            },
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

        // Handle assignment operators
        if (bin.op == .assign or bin.op == .add_assign or bin.op == .sub_assign or
            bin.op == .mul_assign or bin.op == .div_assign or bin.op == .mod_assign)
        {
            return self.emitAssignment(bin);
        }

        // Handle null coalescing (short-circuit evaluation)
        if (bin.op == .null_coalesce) {
            return self.emitNullCoalesce(bin);
        }

        const lhs = try self.emitExpr(bin.left);
        const rhs = try self.emitExpr(bin.right);

        // Check if operands are floating-point
        const lhs_ty = llvm.typeOf(lhs);
        const is_float = llvm.getTypeKind(lhs_ty) == llvm.c.LLVMFloatTypeKind or
            llvm.getTypeKind(lhs_ty) == llvm.c.LLVMDoubleTypeKind;

        return switch (bin.op) {
            // Standard arithmetic (with overflow checking for integers)
            .add => if (is_float)
                self.builder.buildFAdd(lhs, rhs, "faddtmp")
            else
                self.emitCheckedAdd(lhs, rhs, true),
            .sub => if (is_float)
                self.builder.buildFSub(lhs, rhs, "fsubtmp")
            else
                self.emitCheckedSub(lhs, rhs, true),
            .mul => if (is_float)
                self.builder.buildFMul(lhs, rhs, "fmultmp")
            else
                self.emitCheckedMul(lhs, rhs, true),
            .div => if (is_float)
                self.builder.buildFDiv(lhs, rhs, "fdivtmp")
            else
                self.builder.buildSDiv(lhs, rhs, "divtmp"),
            .mod => if (is_float)
                self.builder.buildFRem(lhs, rhs, "fremtmp")
            else
                self.builder.buildSRem(lhs, rhs, "modtmp"),

            // Wrapping arithmetic (no overflow check, wraps around)
            .add_wrap => self.builder.buildAdd(lhs, rhs, "addwrap"),
            .sub_wrap => self.builder.buildSub(lhs, rhs, "subwrap"),
            .mul_wrap => self.builder.buildMul(lhs, rhs, "mulwrap"),

            // Saturating arithmetic
            .add_sat => self.emitSaturatingAdd(lhs, rhs, true),
            .sub_sat => self.emitSaturatingSub(lhs, rhs, true),
            .mul_sat => self.emitSaturatingMul(lhs, rhs, true),

            // Comparison - use appropriate type
            .eq => if (is_float)
                self.builder.buildFCmp(llvm.c.LLVMRealOEQ, lhs, rhs, "feqtmp")
            else
                self.builder.buildICmp(llvm.c.LLVMIntEQ, lhs, rhs, "eqtmp"),
            .not_eq => if (is_float)
                self.builder.buildFCmp(llvm.c.LLVMRealONE, lhs, rhs, "fnetmp")
            else
                self.builder.buildICmp(llvm.c.LLVMIntNE, lhs, rhs, "netmp"),
            .lt => if (is_float)
                self.builder.buildFCmp(llvm.c.LLVMRealOLT, lhs, rhs, "flttmp")
            else
                self.builder.buildICmp(llvm.c.LLVMIntSLT, lhs, rhs, "lttmp"),
            .gt => if (is_float)
                self.builder.buildFCmp(llvm.c.LLVMRealOGT, lhs, rhs, "fgttmp")
            else
                self.builder.buildICmp(llvm.c.LLVMIntSGT, lhs, rhs, "gttmp"),
            .lt_eq => if (is_float)
                self.builder.buildFCmp(llvm.c.LLVMRealOLE, lhs, rhs, "fletmp")
            else
                self.builder.buildICmp(llvm.c.LLVMIntSLE, lhs, rhs, "letmp"),
            .gt_eq => if (is_float)
                self.builder.buildFCmp(llvm.c.LLVMRealOGE, lhs, rhs, "fgetmp")
            else
                self.builder.buildICmp(llvm.c.LLVMIntSGE, lhs, rhs, "getmp"),

            // Bitwise
            .bit_and => self.builder.buildAnd(lhs, rhs, "andtmp"),
            .bit_or => self.builder.buildOr(lhs, rhs, "ortmp"),
            .bit_xor => self.builder.buildXor(lhs, rhs, "xortmp"),
            .shl => self.builder.buildShl(lhs, rhs, "shltmp"),
            .shr => self.builder.buildAShr(lhs, rhs, "shrtmp"),

            else => llvm.Const.int32(self.ctx, 0), // Placeholder
        };
    }

    /// Emit checked addition that traps on overflow.
    fn emitCheckedAdd(self: *Emitter, lhs: llvm.ValueRef, rhs: llvm.ValueRef, is_signed: bool) llvm.ValueRef {
        // For now, use simple wrapping add.
        // In production, this would use LLVM overflow intrinsics and trap on overflow.
        // TODO: implement proper overflow checking with intrinsics
        if (is_signed) {
            return self.builder.buildNSWAdd(lhs, rhs, "addtmp");
        } else {
            return self.builder.buildAdd(lhs, rhs, "addtmp");
        }
    }

    /// Emit checked subtraction that traps on overflow.
    fn emitCheckedSub(self: *Emitter, lhs: llvm.ValueRef, rhs: llvm.ValueRef, is_signed: bool) llvm.ValueRef {
        if (is_signed) {
            return self.builder.buildNSWSub(lhs, rhs, "subtmp");
        } else {
            return self.builder.buildSub(lhs, rhs, "subtmp");
        }
    }

    /// Emit checked multiplication that traps on overflow.
    fn emitCheckedMul(self: *Emitter, lhs: llvm.ValueRef, rhs: llvm.ValueRef, is_signed: bool) llvm.ValueRef {
        if (is_signed) {
            return self.builder.buildNSWMul(lhs, rhs, "multmp");
        } else {
            return self.builder.buildMul(lhs, rhs, "multmp");
        }
    }

    /// Emit saturating addition.
    fn emitSaturatingAdd(self: *Emitter, lhs: llvm.ValueRef, rhs: llvm.ValueRef, is_signed: bool) llvm.ValueRef {
        // For saturating arithmetic, we use LLVM's select instruction to clamp
        // This is a simplified implementation - proper saturation would use intrinsics
        _ = is_signed;
        // For now, just do regular add (TODO: proper saturation)
        return self.builder.buildAdd(lhs, rhs, "addsattmp");
    }

    /// Emit saturating subtraction.
    fn emitSaturatingSub(self: *Emitter, lhs: llvm.ValueRef, rhs: llvm.ValueRef, is_signed: bool) llvm.ValueRef {
        _ = is_signed;
        return self.builder.buildSub(lhs, rhs, "subsattmp");
    }

    /// Emit saturating multiplication.
    fn emitSaturatingMul(self: *Emitter, lhs: llvm.ValueRef, rhs: llvm.ValueRef, is_signed: bool) llvm.ValueRef {
        _ = is_signed;
        return self.builder.buildMul(lhs, rhs, "mulsattmp");
    }

    /// Emit assignment (including compound assignment operators).
    fn emitAssignment(self: *Emitter, bin: *ast.Binary) EmitError!llvm.ValueRef {
        // Get the target variable
        const target_id = switch (bin.left) {
            .identifier => |id| id,
            else => return EmitError.UnsupportedFeature, // Only simple variable assignment for now
        };

        const local = self.named_values.get(target_id.name) orelse
            return EmitError.InvalidAST;

        if (!local.is_alloca) {
            return EmitError.InvalidAST; // Can only assign to allocas
        }

        // Evaluate the right-hand side
        const rhs = try self.emitExpr(bin.right);

        // For compound assignment, load current value and perform operation
        const value = switch (bin.op) {
            .assign => rhs,
            .add_assign => blk: {
                const lhs = self.builder.buildLoad(local.ty, local.value, "loadtmp");
                break :blk self.builder.buildAdd(lhs, rhs, "addtmp");
            },
            .sub_assign => blk: {
                const lhs = self.builder.buildLoad(local.ty, local.value, "loadtmp");
                break :blk self.builder.buildSub(lhs, rhs, "subtmp");
            },
            .mul_assign => blk: {
                const lhs = self.builder.buildLoad(local.ty, local.value, "loadtmp");
                break :blk self.builder.buildMul(lhs, rhs, "multmp");
            },
            .div_assign => blk: {
                const lhs = self.builder.buildLoad(local.ty, local.value, "loadtmp");
                break :blk self.builder.buildSDiv(lhs, rhs, "divtmp");
            },
            .mod_assign => blk: {
                const lhs = self.builder.buildLoad(local.ty, local.value, "loadtmp");
                break :blk self.builder.buildSRem(lhs, rhs, "modtmp");
            },
            else => return EmitError.InvalidAST,
        };

        // Store the result
        _ = self.builder.buildStore(value, local.value);

        // Return the stored value (assignments are expressions in Klar)
        return value;
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
        return switch (un.op) {
            .negate => {
                const operand = try self.emitExpr(un.operand);
                return self.builder.buildNeg(operand, "negtmp");
            },
            .not => {
                const operand = try self.emitExpr(un.operand);
                return self.builder.buildNot(operand, "nottmp");
            },
            .deref => {
                // Dereference a pointer: load the value from the pointer
                const operand = try self.emitExpr(un.operand);

                // Try to get the inner type from the operand
                // First, check if operand is an identifier with a known inner type
                const inner_type: llvm.TypeRef = if (un.operand == .identifier) blk: {
                    const id = un.operand.identifier;
                    if (self.named_values.get(id.name)) |local| {
                        if (local.inner_type) |inner| {
                            break :blk inner;
                        }
                    }
                    // Fallback: try to infer from expression
                    break :blk try self.inferDerefType(un.operand);
                } else try self.inferDerefType(un.operand);

                return self.builder.buildLoad(inner_type, operand, "deref");
            },
            .ref => {
                // Taking address of a value - for lvalues, return the alloca pointer
                // For now, handle identifiers which have an alloca
                if (un.operand == .identifier) {
                    const id = un.operand.identifier;
                    if (self.named_values.get(id.name)) |local| {
                        if (local.is_alloca) {
                            // Return the pointer to the alloca (the address)
                            return local.value;
                        }
                    }
                }
                // Fallback: emit the operand as-is (may need refinement)
                return try self.emitExpr(un.operand);
            },
            .ref_mut => {
                // Mutable reference - same as immutable ref for now
                if (un.operand == .identifier) {
                    const id = un.operand.identifier;
                    if (self.named_values.get(id.name)) |local| {
                        if (local.is_alloca) {
                            return local.value;
                        }
                    }
                }
                return try self.emitExpr(un.operand);
            },
        };
    }

    /// Infer the type that results from dereferencing an expression.
    /// Used when the inner type is not explicitly tracked.
    fn inferDerefType(self: *Emitter, expr: ast.Expr) EmitError!llvm.TypeRef {
        // For Rc[T] method calls, try to determine T
        if (expr == .method_call) {
            const method = expr.method_call;
            if (method.object == .identifier) {
                const obj_name = method.object.identifier.name;
                // Rc.new(value) - the inner type is the type of value
                if (std.mem.eql(u8, obj_name, "Rc") and std.mem.eql(u8, method.method_name, "new")) {
                    if (method.args.len > 0) {
                        return try self.inferExprType(method.args[0]);
                    }
                }
            }
            // clone() returns same Rc type, need to trace back
            if (std.mem.eql(u8, method.method_name, "clone")) {
                return try self.inferDerefType(method.object);
            }
        }

        // For identifiers, check the named_values for inner_type
        if (expr == .identifier) {
            const id = expr.identifier;
            if (self.named_values.get(id.name)) |local| {
                if (local.inner_type) |inner| {
                    return inner;
                }
            }
        }

        // Default to i32 if we can't infer
        return llvm.Types.int32(self.ctx);
    }

    /// Try to get the inner type of an Rc or Cell expression.
    /// Returns null if the expression is not an Rc or Cell type.
    fn tryGetRcInnerType(self: *Emitter, expr: ast.Expr) ?llvm.TypeRef {
        // Check for Rc.new(value) or Cell.new(value) - inner type is type of value
        if (expr == .method_call) {
            const method = expr.method_call;
            if (method.object == .identifier) {
                const obj_name = method.object.identifier.name;
                // Rc.new(value)
                if (std.mem.eql(u8, obj_name, "Rc") and std.mem.eql(u8, method.method_name, "new")) {
                    if (method.args.len > 0) {
                        return self.inferExprType(method.args[0]) catch null;
                    }
                }
                // Cell.new(value)
                if (std.mem.eql(u8, obj_name, "Cell") and std.mem.eql(u8, method.method_name, "new")) {
                    if (method.args.len > 0) {
                        return self.inferExprType(method.args[0]) catch null;
                    }
                }
            }
            // For clone(), the inner type is the same as the receiver's inner type
            if (std.mem.eql(u8, method.method_name, "clone")) {
                return self.tryGetRcInnerType(method.object);
            }
        }

        // Check for identifier that is already an Rc or Cell
        if (expr == .identifier) {
            const id = expr.identifier;
            if (self.named_values.get(id.name)) |local| {
                return local.inner_type;
            }
        }

        return null;
    }

    fn emitCall(self: *Emitter, call: *ast.Call) EmitError!llvm.ValueRef {
        // Check if callee is a direct function reference
        const func_name = switch (call.callee) {
            .identifier => |id| id.name,
            else => null,
        };

        // Check for builtin functions first
        if (func_name) |name| {
            if (std.mem.eql(u8, name, "print")) {
                return self.emitPrint(call.args, false);
            } else if (std.mem.eql(u8, name, "println")) {
                return self.emitPrint(call.args, true);
            } else if (std.mem.eql(u8, name, "panic")) {
                return self.emitPanic(call.args);
            } else if (std.mem.eql(u8, name, "assert")) {
                return self.emitAssert(call.args);
            }
        }

        // Try to find as a module-level function
        if (func_name) |name| {
            const name_z = self.allocator.dupeZ(u8, name) catch return EmitError.OutOfMemory;
            defer self.allocator.free(name_z);

            if (self.module.getNamedFunction(name_z)) |func| {
                // Direct function call
                var args = std.ArrayListUnmanaged(llvm.ValueRef){};
                defer args.deinit(self.allocator);

                for (call.args) |arg| {
                    args.append(self.allocator, try self.emitExpr(arg)) catch return EmitError.OutOfMemory;
                }

                const fn_type = llvm.getGlobalValueType(func);
                const return_type = llvm.getReturnType(fn_type);
                const call_name_str: [:0]const u8 = if (llvm.isVoidType(return_type)) "" else "calltmp";
                return self.builder.buildCall(fn_type, func, args.items, call_name_str);
            }

            // Check if it's a local variable (closure)
            if (self.named_values.get(name)) |local| {
                // Load the closure value from the local variable
                const closure_struct_type = self.getClosureStructType();
                const closure_value = if (local.is_alloca)
                    self.builder.buildLoad(closure_struct_type, local.value, "closure.load")
                else
                    local.value;
                return self.emitClosureCall(closure_value, call.args);
            }
        }

        // Generic callee - evaluate it and call as closure
        const callee_value = try self.emitExpr(call.callee);
        return self.emitClosureCall(callee_value, call.args);
    }

    /// Emit a call to a closure value.
    /// Closure struct is { fn_ptr: ptr, env_ptr: ptr }
    fn emitClosureCall(self: *Emitter, closure_value: llvm.ValueRef, args: []const ast.Expr) EmitError!llvm.ValueRef {
        // Closure struct type: { fn_ptr, env_ptr }
        const closure_struct_type = self.getClosureStructType();

        // Allocate space to store the closure value (since we have it by value)
        const closure_alloca = self.builder.buildAlloca(closure_struct_type, "closure.tmp");
        _ = self.builder.buildStore(closure_value, closure_alloca);

        // Load function pointer
        var fn_ptr_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const fn_ptr_gep = self.builder.buildGEP(closure_struct_type, closure_alloca, &fn_ptr_indices, "fn.ptr.ptr");
        const fn_ptr = self.builder.buildLoad(llvm.Types.pointer(self.ctx), fn_ptr_gep, "fn.ptr");

        // Load environment pointer
        var env_ptr_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 1),
        };
        const env_ptr_gep = self.builder.buildGEP(closure_struct_type, closure_alloca, &env_ptr_indices, "env.ptr.ptr");
        const env_ptr = self.builder.buildLoad(llvm.Types.pointer(self.ctx), env_ptr_gep, "env.ptr");

        // Build argument list: env_ptr + user args
        var call_args = std.ArrayListUnmanaged(llvm.ValueRef){};
        defer call_args.deinit(self.allocator);

        call_args.append(self.allocator, env_ptr) catch return EmitError.OutOfMemory;

        for (args) |arg| {
            call_args.append(self.allocator, try self.emitExpr(arg)) catch return EmitError.OutOfMemory;
        }

        // Build function type for the call
        // Return type: i32 (default), params: env_ptr + user params
        var param_types = std.ArrayListUnmanaged(llvm.TypeRef){};
        defer param_types.deinit(self.allocator);

        param_types.append(self.allocator, llvm.Types.pointer(self.ctx)) catch return EmitError.OutOfMemory;
        for (args) |_| {
            param_types.append(self.allocator, llvm.Types.int32(self.ctx)) catch return EmitError.OutOfMemory;
        }

        const fn_type = llvm.Types.function(llvm.Types.int32(self.ctx), param_types.items, false);

        // Call through function pointer
        return self.builder.buildCall(fn_type, fn_ptr, call_args.items, "closure.call");
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
            .array => |arr| {
                // Get element type
                const elem_type = try self.typeExprToLLVM(arr.element);
                // Get size from the size expression (must be compile-time constant)
                const size: u64 = switch (arr.size) {
                    .literal => |lit| switch (lit.kind) {
                        .int => |v| @intCast(v),
                        else => return EmitError.InvalidAST,
                    },
                    else => return EmitError.UnsupportedFeature, // Non-constant array size
                };
                return llvm.Types.array(elem_type, size);
            },
            .tuple => |tup| {
                // Create struct type for tuple
                var elem_types = std.ArrayListUnmanaged(llvm.TypeRef){};
                defer elem_types.deinit(self.allocator);

                for (tup.elements) |elem| {
                    const elem_llvm_type = try self.typeExprToLLVM(elem);
                    elem_types.append(self.allocator, elem_llvm_type) catch return EmitError.OutOfMemory;
                }

                return llvm.Types.struct_(self.ctx, elem_types.items, false);
            },
            .slice => |sl| {
                // Slice is a struct of {pointer, length}
                const elem_type = try self.typeExprToLLVM(sl.element);
                _ = elem_type; // Element type is for the pointed-to data
                var slice_fields = [_]llvm.TypeRef{
                    llvm.Types.pointer(self.ctx), // data pointer
                    llvm.Types.int64(self.ctx), // length (usize)
                };
                return llvm.Types.struct_(self.ctx, &slice_fields, false);
            },
            .reference => |ref| {
                // Reference is a pointer
                _ = ref;
                return llvm.Types.pointer(self.ctx);
            },
            .optional => |opt| {
                // Optional is a struct of {tag, value}
                const inner_type = try self.typeExprToLLVM(opt.inner);
                var opt_fields = [_]llvm.TypeRef{
                    llvm.Types.int1(self.ctx), // tag (0 = none, 1 = some)
                    inner_type, // value
                };
                return llvm.Types.struct_(self.ctx, &opt_fields, false);
            },
            .function => {
                // Function type is represented as closure struct: { fn_ptr: ptr, env_ptr: ptr }
                // This is the same layout used in emitClosure
                return self.getClosureStructType();
            },
            .result, .generic_apply => {
                // Complex types - return pointer as placeholder
                return llvm.Types.pointer(self.ctx);
            },
        };
    }

    /// Get the LLVM struct type for closures: { fn_ptr: ptr, env_ptr: ptr }
    /// This is a consistent layout used throughout closure handling.
    fn getClosureStructType(self: *Emitter) llvm.TypeRef {
        var types_arr = [_]llvm.TypeRef{
            llvm.Types.pointer(self.ctx), // fn_ptr
            llvm.Types.pointer(self.ctx), // env_ptr
        };
        return llvm.Types.struct_(self.ctx, &types_arr, false);
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

    /// Check if a type expression represents a signed type.
    fn isTypeSigned(self: *Emitter, type_expr: ast.TypeExpr) bool {
        _ = self;
        return switch (type_expr) {
            .named => |named| {
                // Unsigned types start with 'u'
                if (named.name.len > 0 and named.name[0] == 'u') {
                    // But could be "usize" which is architecture-dependent
                    // For now, treat it as unsigned
                    if (std.mem.eql(u8, named.name, "u8")) return false;
                    if (std.mem.eql(u8, named.name, "u16")) return false;
                    if (std.mem.eql(u8, named.name, "u32")) return false;
                    if (std.mem.eql(u8, named.name, "u64")) return false;
                    if (std.mem.eql(u8, named.name, "u128")) return false;
                    if (std.mem.eql(u8, named.name, "usize")) return false;
                }
                // Signed types: i8, i16, i32, i64, i128, isize, or floats
                return true;
            },
            else => true, // Default to signed
        };
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
            // Composite types - build the type from elements
            .tuple_literal => |tup| {
                if (tup.elements.len == 0) {
                    return llvm.Types.int32(self.ctx); // Empty tuple as unit
                }
                var elem_types = std.ArrayListUnmanaged(llvm.TypeRef){};
                defer elem_types.deinit(self.allocator);
                for (tup.elements) |elem| {
                    const elem_ty = try self.inferExprType(elem);
                    elem_types.append(self.allocator, elem_ty) catch return EmitError.OutOfMemory;
                }
                return llvm.Types.struct_(self.ctx, elem_types.items, false);
            },
            .array_literal => |arr| {
                if (arr.elements.len == 0) {
                    return llvm.Types.array(llvm.Types.int32(self.ctx), 0);
                }
                const elem_ty = try self.inferExprType(arr.elements[0]);
                return llvm.Types.array(elem_ty, @intCast(arr.elements.len));
            },
            .struct_literal => |s| {
                // Infer struct type from field values
                var field_types = std.ArrayListUnmanaged(llvm.TypeRef){};
                defer field_types.deinit(self.allocator);
                for (s.fields) |field_init| {
                    const field_ty = try self.inferExprType(field_init.value);
                    field_types.append(self.allocator, field_ty) catch return EmitError.OutOfMemory;
                }
                return llvm.Types.struct_(self.ctx, field_types.items, false);
            },
            .field => |f| {
                // Field access on a composite - get the base type and extract field type
                const base_ty = try self.inferExprType(f.object);
                const type_kind = llvm.getTypeKind(base_ty);
                if (type_kind == llvm.c.LLVMStructTypeKind) {
                    // Try to parse as numeric index for tuple
                    if (std.fmt.parseInt(u32, f.field_name, 10)) |idx| {
                        return llvm.c.LLVMStructGetTypeAtIndex(base_ty, idx);
                    } else |_| {
                        // Named field - for now return i32 as placeholder
                        return llvm.Types.int32(self.ctx);
                    }
                }
                return llvm.Types.int32(self.ctx);
            },
            .index => |i| {
                // Index access on array - return element type
                const base_ty = try self.inferExprType(i.object);
                const type_kind = llvm.getTypeKind(base_ty);
                if (type_kind == llvm.c.LLVMArrayTypeKind) {
                    return llvm.c.LLVMGetElementType(base_ty);
                }
                return llvm.Types.int32(self.ctx);
            },
            .grouped => |g| try self.inferExprType(g.expr),
            .call => |call| {
                // Get function return type
                const func_name = switch (call.callee) {
                    .identifier => |id| id.name,
                    else => return llvm.Types.int32(self.ctx),
                };
                const name = self.allocator.dupeZ(u8, func_name) catch return EmitError.OutOfMemory;
                defer self.allocator.free(name);

                if (self.module.getNamedFunction(name)) |func| {
                    const fn_type = llvm.getGlobalValueType(func);
                    return llvm.getReturnType(fn_type);
                }
                return llvm.Types.int32(self.ctx);
            },
            .postfix => |post| {
                // Postfix operators (unwrap) return the inner type
                const operand_type = try self.inferExprType(post.operand);
                const type_kind = llvm.getTypeKind(operand_type);
                if (type_kind == llvm.c.LLVMStructTypeKind) {
                    // For optional types (2-field struct), return the value field type
                    if (llvm.c.LLVMCountStructElementTypes(operand_type) == 2) {
                        return llvm.c.LLVMStructGetTypeAtIndex(operand_type, 1);
                    }
                }
                return operand_type;
            },
            .method_call => |m| {
                // Check for special Rc and Cell methods that return pointers
                if (m.object == .identifier) {
                    const obj_name = m.object.identifier.name;
                    if (std.mem.eql(u8, obj_name, "Rc") and std.mem.eql(u8, m.method_name, "new")) {
                        // Rc.new() returns a pointer (Rc[T])
                        return llvm.Types.pointer(self.ctx);
                    }
                    if (std.mem.eql(u8, obj_name, "Cell") and std.mem.eql(u8, m.method_name, "new")) {
                        // Cell.new() returns a pointer (Cell[T])
                        return llvm.Types.pointer(self.ctx);
                    }
                }
                // clone() and downgrade() return pointers
                if (std.mem.eql(u8, m.method_name, "clone") or
                    std.mem.eql(u8, m.method_name, "downgrade"))
                {
                    return llvm.Types.pointer(self.ctx);
                }
                // upgrade() returns optional pointer - struct {i1, ptr}
                if (std.mem.eql(u8, m.method_name, "upgrade")) {
                    const ptr_type = llvm.Types.pointer(self.ctx);
                    var opt_fields = [_]llvm.TypeRef{ llvm.Types.int1(self.ctx), ptr_type };
                    return llvm.Types.struct_(self.ctx, &opt_fields, false);
                }
                // get() returns the inner type
                if (std.mem.eql(u8, m.method_name, "get")) {
                    return self.inferCellInnerType(m.object);
                }
                // replace() returns the inner type (old value)
                if (std.mem.eql(u8, m.method_name, "replace")) {
                    return self.inferCellInnerType(m.object);
                }
                // set() returns void (we use i32 as placeholder)
                if (std.mem.eql(u8, m.method_name, "set")) {
                    return llvm.Types.int32(self.ctx);
                }
                return llvm.Types.int32(self.ctx);
            },
            .closure => {
                // Closure type is a struct { fn_ptr: ptr, env_ptr: ptr }
                var closure_fields = [_]llvm.TypeRef{
                    llvm.Types.pointer(self.ctx), // fn_ptr
                    llvm.Types.pointer(self.ctx), // env_ptr
                };
                return llvm.Types.struct_(self.ctx, &closure_fields, false);
            },
            .unary => |un| {
                return switch (un.op) {
                    .negate, .not => try self.inferExprType(un.operand),
                    .deref => {
                        // Dereference returns the inner type
                        return try self.inferDerefType(un.operand);
                    },
                    .ref, .ref_mut => {
                        // Reference returns a pointer type
                        return llvm.Types.pointer(self.ctx);
                    },
                };
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

    /// Extract struct type name from an expression if it's a struct literal.
    fn getStructTypeName(_: *Emitter, expr: ast.Expr) ?[]const u8 {
        return switch (expr) {
            .struct_literal => |lit| {
                if (lit.type_name) |type_name| {
                    return switch (type_name) {
                        .named => |n| n.name,
                        else => null,
                    };
                }
                return null;
            },
            else => null,
        };
    }

    /// Look up a field index by name for a struct type.
    fn lookupFieldIndex(self: *Emitter, struct_type_name: []const u8, field_name: []const u8) ?u32 {
        if (self.struct_types.get(struct_type_name)) |info| {
            for (info.field_names, 0..) |name, i| {
                if (std.mem.eql(u8, name, field_name)) {
                    return info.field_indices[i];
                }
            }
        }
        return null;
    }

    // =========================================================================
    // Composite Type Support
    // =========================================================================

    /// Get or create an LLVM struct type for a named struct.
    fn getOrCreateStructType(self: *Emitter, name: []const u8, fields: []const ast.StructField) EmitError!llvm.TypeRef {
        // Check cache first
        if (self.struct_types.get(name)) |info| {
            return info.llvm_type;
        }

        // Create array of field types
        var field_types = std.ArrayListUnmanaged(llvm.TypeRef){};
        defer field_types.deinit(self.allocator);

        var field_indices = try self.allocator.alloc(u32, fields.len);
        errdefer self.allocator.free(field_indices);

        var field_names = try self.allocator.alloc([]const u8, fields.len);
        errdefer self.allocator.free(field_names);

        for (fields, 0..) |field, i| {
            const field_llvm_type = try self.typeExprToLLVM(field.type_);
            field_types.append(self.allocator, field_llvm_type) catch return EmitError.OutOfMemory;
            field_indices[i] = @intCast(i);
            field_names[i] = field.name;
        }

        // Create LLVM struct type (not packed - follows C ABI alignment)
        const struct_type = llvm.Types.struct_(self.ctx, field_types.items, false);

        // Cache the struct type info
        self.struct_types.put(name, .{
            .llvm_type = struct_type,
            .field_indices = field_indices,
            .field_names = field_names,
        }) catch return EmitError.OutOfMemory;

        return struct_type;
    }

    /// Emit a struct literal expression.
    fn emitStructLiteral(self: *Emitter, lit: *ast.StructLiteral) EmitError!llvm.ValueRef {
        // Get the struct type name
        const type_name = switch (lit.type_name orelse return EmitError.InvalidAST) {
            .named => |n| n.name,
            else => return EmitError.UnsupportedFeature,
        };

        // Check if we have a registered struct type (from struct declaration)
        if (self.struct_types.get(type_name)) |struct_info| {
            // Use the registered struct type - store fields in correct order
            const struct_type = struct_info.llvm_type;

            // Allocate stack space for the struct
            const type_name_z = self.allocator.dupeZ(u8, type_name) catch return EmitError.OutOfMemory;
            defer self.allocator.free(type_name_z);
            const alloca = self.builder.buildAlloca(struct_type, type_name_z);

            // Store each field value by looking up field name -> index
            for (lit.fields) |field_init| {
                // Find the field index by name
                const field_idx = self.lookupFieldIndex(type_name, field_init.name) orelse
                    return EmitError.InvalidAST;

                const value = try self.emitExpr(field_init.value);
                var indices = [_]llvm.ValueRef{
                    llvm.Const.int32(self.ctx, 0),
                    llvm.Const.int32(self.ctx, @intCast(field_idx)),
                };
                const field_ptr = self.builder.buildGEP(struct_type, alloca, &indices, "field.ptr");
                _ = self.builder.buildStore(value, field_ptr);
            }

            // Load and return the complete struct value
            return self.builder.buildLoad(struct_type, alloca, "struct.val");
        }

        // Fallback: Create a simple struct type based on the field initializers (in order).
        // This is used when no struct declaration is available.
        var field_types = std.ArrayListUnmanaged(llvm.TypeRef){};
        defer field_types.deinit(self.allocator);

        var field_values = std.ArrayListUnmanaged(llvm.ValueRef){};
        defer field_values.deinit(self.allocator);

        var field_names = std.ArrayListUnmanaged([]const u8){};
        defer field_names.deinit(self.allocator);

        for (lit.fields) |field_init| {
            const value = try self.emitExpr(field_init.value);
            const value_type = llvm.typeOf(value);
            field_types.append(self.allocator, value_type) catch return EmitError.OutOfMemory;
            field_values.append(self.allocator, value) catch return EmitError.OutOfMemory;
            field_names.append(self.allocator, field_init.name) catch return EmitError.OutOfMemory;
        }

        // Create struct type and register it for field access
        const struct_type = llvm.Types.struct_(self.ctx, field_types.items, false);

        // Register the struct type info for later field access
        const field_indices = self.allocator.alloc(u32, lit.fields.len) catch return EmitError.OutOfMemory;
        errdefer self.allocator.free(field_indices);
        for (field_indices, 0..) |*idx, i| {
            idx.* = @intCast(i);
        }
        const field_names_owned = self.allocator.alloc([]const u8, lit.fields.len) catch return EmitError.OutOfMemory;
        errdefer self.allocator.free(field_names_owned);
        for (field_names_owned, field_names.items) |*dst, src| {
            dst.* = src;
        }

        self.struct_types.put(type_name, .{
            .llvm_type = struct_type,
            .field_indices = field_indices,
            .field_names = field_names_owned,
        }) catch return EmitError.OutOfMemory;

        // Allocate stack space for the struct
        const type_name_z = self.allocator.dupeZ(u8, type_name) catch return EmitError.OutOfMemory;
        defer self.allocator.free(type_name_z);
        const alloca = self.builder.buildAlloca(struct_type, type_name_z);

        // Store each field value using GEP
        for (field_values.items, 0..) |value, i| {
            var indices = [_]llvm.ValueRef{
                llvm.Const.int32(self.ctx, 0),
                llvm.Const.int32(self.ctx, @intCast(i)),
            };
            const field_ptr = self.builder.buildGEP(struct_type, alloca, &indices, "field.ptr");
            _ = self.builder.buildStore(value, field_ptr);
        }

        // Load and return the complete struct value
        return self.builder.buildLoad(struct_type, alloca, "struct.val");
    }

    /// Emit an array literal expression.
    fn emitArrayLiteral(self: *Emitter, arr: *ast.ArrayLiteral) EmitError!llvm.ValueRef {
        if (arr.elements.len == 0) {
            // Empty array - return undefined/zero value
            const arr_type = llvm.Types.array(llvm.Types.int32(self.ctx), 0);
            return llvm.Const.undef(arr_type);
        }

        // Emit all elements
        var element_values = std.ArrayListUnmanaged(llvm.ValueRef){};
        defer element_values.deinit(self.allocator);

        for (arr.elements) |elem| {
            const value = try self.emitExpr(elem);
            element_values.append(self.allocator, value) catch return EmitError.OutOfMemory;
        }

        // Get element type from first element
        const elem_type = llvm.typeOf(element_values.items[0]);
        const arr_type = llvm.Types.array(elem_type, @intCast(arr.elements.len));

        // Allocate stack space for the array
        const alloca = self.builder.buildAlloca(arr_type, "arr.tmp");

        // Store each element using GEP
        for (element_values.items, 0..) |value, i| {
            var indices = [_]llvm.ValueRef{
                llvm.Const.int32(self.ctx, 0),
                llvm.Const.int32(self.ctx, @intCast(i)),
            };
            const elem_ptr = self.builder.buildGEP(arr_type, alloca, &indices, "arr.elem.ptr");
            _ = self.builder.buildStore(value, elem_ptr);
        }

        // Load and return the complete array value
        return self.builder.buildLoad(arr_type, alloca, "arr.val");
    }

    /// Emit a tuple literal expression.
    fn emitTupleLiteral(self: *Emitter, tup: *ast.TupleLiteral) EmitError!llvm.ValueRef {
        if (tup.elements.len == 0) {
            // Empty tuple - treated as unit/void
            return llvm.Const.int32(self.ctx, 0);
        }

        // Emit all elements
        var element_values = std.ArrayListUnmanaged(llvm.ValueRef){};
        defer element_values.deinit(self.allocator);

        var element_types = std.ArrayListUnmanaged(llvm.TypeRef){};
        defer element_types.deinit(self.allocator);

        for (tup.elements) |elem| {
            const value = try self.emitExpr(elem);
            element_values.append(self.allocator, value) catch return EmitError.OutOfMemory;
            element_types.append(self.allocator, llvm.typeOf(value)) catch return EmitError.OutOfMemory;
        }

        // Create tuple type (anonymous struct in LLVM)
        const tuple_type = llvm.Types.struct_(self.ctx, element_types.items, false);

        // Allocate stack space for the tuple
        const alloca = self.builder.buildAlloca(tuple_type, "tup.tmp");

        // Store each element using GEP
        for (element_values.items, 0..) |value, i| {
            var indices = [_]llvm.ValueRef{
                llvm.Const.int32(self.ctx, 0),
                llvm.Const.int32(self.ctx, @intCast(i)),
            };
            const elem_ptr = self.builder.buildGEP(tuple_type, alloca, &indices, "tup.elem.ptr");
            _ = self.builder.buildStore(value, elem_ptr);
        }

        // Load and return the complete tuple value
        return self.builder.buildLoad(tuple_type, alloca, "tup.val");
    }

    /// Emit field access expression (e.g., point.x or tuple.0).
    fn emitFieldAccess(self: *Emitter, field: *ast.Field) EmitError!llvm.ValueRef {
        // Check if this is a tuple index (numeric field name like .0, .1)
        const field_index: ?u32 = std.fmt.parseInt(u32, field.field_name, 10) catch null;

        // Special handling for identifier access - we can GEP directly from the alloca
        if (field.object == .identifier) {
            const id = field.object.identifier;
            if (self.named_values.get(id.name)) |local| {
                if (local.is_alloca) {
                    // First try numeric index (for tuples)
                    if (field_index) |idx| {
                        var indices = [_]llvm.ValueRef{
                            llvm.Const.int32(self.ctx, 0),
                            llvm.Const.int32(self.ctx, @intCast(idx)),
                        };
                        const field_ptr = self.builder.buildGEP(local.ty, local.value, &indices, "field.ptr");
                        // Get the field type from struct type
                        const field_type = llvm.c.LLVMStructGetTypeAtIndex(local.ty, idx);
                        return self.builder.buildLoad(field_type, field_ptr, "field.val");
                    }

                    // Named field access - look up field index from struct type name
                    if (local.struct_type_name) |struct_name| {
                        if (self.lookupFieldIndex(struct_name, field.field_name)) |idx| {
                            var indices = [_]llvm.ValueRef{
                                llvm.Const.int32(self.ctx, 0),
                                llvm.Const.int32(self.ctx, @intCast(idx)),
                            };
                            const field_ptr = self.builder.buildGEP(local.ty, local.value, &indices, "field.ptr");
                            const field_type = llvm.c.LLVMStructGetTypeAtIndex(local.ty, idx);
                            return self.builder.buildLoad(field_type, field_ptr, "field.val");
                        }
                    }

                    return EmitError.UnsupportedFeature;
                }
            }
        }

        // For other cases, emit the object and handle it
        const obj = try self.emitExpr(field.object);
        const obj_type = llvm.typeOf(obj);
        const obj_type_kind = llvm.getTypeKind(obj_type);

        if (obj_type_kind == llvm.c.LLVMStructTypeKind) {
            // Object is a struct value - need to store to temp first for GEP
            const alloca = self.builder.buildAlloca(obj_type, "obj.tmp");
            _ = self.builder.buildStore(obj, alloca);

            if (field_index) |idx| {
                // Tuple access by index
                var indices = [_]llvm.ValueRef{
                    llvm.Const.int32(self.ctx, 0),
                    llvm.Const.int32(self.ctx, @intCast(idx)),
                };
                const field_ptr = self.builder.buildGEP(obj_type, alloca, &indices, "field.ptr");
                // Get the field type - need to extract from struct type
                const field_type = llvm.c.LLVMStructGetTypeAtIndex(obj_type, idx);
                return self.builder.buildLoad(field_type, field_ptr, "field.val");
            } else {
                // Named field access on non-identifier struct - need type context from expression
                // For now, search all registered struct types to find a match by field count
                return EmitError.UnsupportedFeature;
            }
        }

        return EmitError.UnsupportedFeature;
    }

    /// Emit array/slice index access expression (e.g., arr[i]).
    fn emitIndexAccess(self: *Emitter, idx: *ast.Index) EmitError!llvm.ValueRef {
        // Emit the array/slice object
        const obj = try self.emitExpr(idx.object);
        const obj_type = llvm.typeOf(obj);

        // Emit the index
        const index_val = try self.emitExpr(idx.index);

        const obj_type_kind = llvm.getTypeKind(obj_type);

        if (obj_type_kind == llvm.c.LLVMArrayTypeKind) {
            // Array access with bounds checking

            // Get array length
            const array_len = llvm.Types.getArrayLength(obj_type);
            const len_val = llvm.Const.int64(self.ctx, @intCast(array_len));

            // Zero-extend or sign-extend index to i64 for comparison
            const index_type = llvm.typeOf(index_val);
            const index_bits = llvm.c.LLVMGetIntTypeWidth(index_type);
            const i64_type = llvm.Types.int64(self.ctx);

            const index_i64 = if (index_bits < 64)
                self.builder.buildZExt(index_val, i64_type, "idx.ext")
            else if (index_bits > 64)
                self.builder.buildTrunc(index_val, i64_type, "idx.trunc")
            else
                index_val;

            // Bounds check: index < length (unsigned comparison)
            const in_bounds = self.builder.buildICmp(
                llvm.c.LLVMIntULT,
                index_i64,
                len_val,
                "bounds.check",
            );

            // Create blocks for bounds check
            const func = self.current_function orelse return EmitError.InvalidAST;
            const ok_block = llvm.appendBasicBlock(self.ctx, func, "bounds.ok");
            const fail_block = llvm.appendBasicBlock(self.ctx, func, "bounds.fail");

            // Branch based on bounds check
            _ = self.builder.buildCondBr(in_bounds, ok_block, fail_block);

            // Fail block: trap/unreachable
            self.builder.positionAtEnd(fail_block);
            _ = self.builder.buildUnreachable();

            // Continue in OK block
            self.builder.positionAtEnd(ok_block);

            // Array access - store to temp for GEP
            const alloca = self.builder.buildAlloca(obj_type, "arr.tmp");
            _ = self.builder.buildStore(obj, alloca);

            // GEP with the index
            var indices = [_]llvm.ValueRef{
                llvm.Const.int32(self.ctx, 0),
                index_val,
            };
            const elem_ptr = self.builder.buildGEP(obj_type, alloca, &indices, "elem.ptr");

            // Get element type and load
            const elem_type = llvm.c.LLVMGetElementType(obj_type);
            return self.builder.buildLoad(elem_type, elem_ptr, "elem.val");
        } else if (obj_type_kind == llvm.c.LLVMPointerTypeKind) {
            // Pointer/slice access
            // For slices, we'd need to bounds check and extract the data pointer
            // For now, treat as raw pointer indexing
            return EmitError.UnsupportedFeature;
        }

        return EmitError.UnsupportedFeature;
    }

    // =========================================================================
    // Optional Type Support
    // =========================================================================

    /// Emit a postfix operator expression (? for safe unwrap, ! for force unwrap).
    fn emitPostfix(self: *Emitter, post: *ast.Postfix) EmitError!llvm.ValueRef {
        // Emit the operand (should be an optional type)
        const operand = try self.emitExpr(post.operand);
        const operand_type = llvm.typeOf(operand);

        // Optional type layout is { i1, T } where i1 is the tag (0=None, 1=Some)
        // First, check if operand is a struct type (our optional representation)
        const type_kind = llvm.getTypeKind(operand_type);
        if (type_kind != llvm.c.LLVMStructTypeKind) {
            return EmitError.UnsupportedFeature;
        }

        // Store the optional to a temp for GEP access
        const opt_alloca = self.builder.buildAlloca(operand_type, "opt.tmp");
        _ = self.builder.buildStore(operand, opt_alloca);

        // Get the tag (index 0)
        var tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const tag_ptr = self.builder.buildGEP(operand_type, opt_alloca, &tag_indices, "opt.tag.ptr");
        const tag = self.builder.buildLoad(llvm.Types.int1(self.ctx), tag_ptr, "opt.tag");

        // Get the value (index 1)
        var val_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 1),
        };
        const val_ptr = self.builder.buildGEP(operand_type, opt_alloca, &val_indices, "opt.val.ptr");
        const val_type = llvm.c.LLVMStructGetTypeAtIndex(operand_type, 1);

        switch (post.op) {
            .force_unwrap => {
                // ! operator: trap if None, return value if Some
                const func = self.current_function orelse return EmitError.InvalidAST;

                const ok_block = llvm.appendBasicBlock(self.ctx, func, "unwrap.ok");
                const fail_block = llvm.appendBasicBlock(self.ctx, func, "unwrap.fail");

                // Branch based on tag
                _ = self.builder.buildCondBr(tag, ok_block, fail_block);

                // Fail block: unreachable/trap
                self.builder.positionAtEnd(fail_block);
                _ = self.builder.buildUnreachable();

                // OK block: load and return value
                self.builder.positionAtEnd(ok_block);
                return self.builder.buildLoad(val_type, val_ptr, "unwrap.val");
            },
            .unwrap => {
                // ? operator: safe unwrap - for now, same as force_unwrap
                // In full implementation, this would propagate None
                const func = self.current_function orelse return EmitError.InvalidAST;

                const ok_block = llvm.appendBasicBlock(self.ctx, func, "unwrap.ok");
                const fail_block = llvm.appendBasicBlock(self.ctx, func, "unwrap.fail");

                // Branch based on tag
                _ = self.builder.buildCondBr(tag, ok_block, fail_block);

                // Fail block: unreachable/trap for now
                self.builder.positionAtEnd(fail_block);
                _ = self.builder.buildUnreachable();

                // OK block: load and return value
                self.builder.positionAtEnd(ok_block);
                return self.builder.buildLoad(val_type, val_ptr, "unwrap.val");
            },
        }
    }

    /// Emit null coalescing operator (??) - returns left if Some, else right.
    fn emitNullCoalesce(self: *Emitter, bin: *ast.Binary) EmitError!llvm.ValueRef {
        const func = self.current_function orelse return EmitError.InvalidAST;

        // Emit the left side (optional value)
        const lhs = try self.emitExpr(bin.left);
        const lhs_type = llvm.typeOf(lhs);

        // Check type is struct (optional representation)
        const type_kind = llvm.getTypeKind(lhs_type);
        if (type_kind != llvm.c.LLVMStructTypeKind) {
            // Not an optional - just return right side as fallback
            return self.emitExpr(bin.right);
        }

        // Store optional to temp for GEP
        const opt_alloca = self.builder.buildAlloca(lhs_type, "coalesce.opt");
        _ = self.builder.buildStore(lhs, opt_alloca);

        // Extract tag
        var tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const tag_ptr = self.builder.buildGEP(lhs_type, opt_alloca, &tag_indices, "coalesce.tag.ptr");
        const tag = self.builder.buildLoad(llvm.Types.int1(self.ctx), tag_ptr, "coalesce.tag");

        // Create blocks for Some/None branches
        const lhs_bb = llvm.c.LLVMGetInsertBlock(self.builder.ref);
        const some_bb = llvm.appendBasicBlock(self.ctx, func, "coalesce.some");
        const none_bb = llvm.appendBasicBlock(self.ctx, func, "coalesce.none");
        const merge_bb = llvm.appendBasicBlock(self.ctx, func, "coalesce.merge");

        // Branch based on tag (1 = Some, 0 = None)
        _ = self.builder.buildCondBr(tag, some_bb, none_bb);

        // Some block: extract inner value
        self.builder.positionAtEnd(some_bb);
        var val_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 1),
        };
        const val_ptr = self.builder.buildGEP(lhs_type, opt_alloca, &val_indices, "coalesce.val.ptr");
        const val_type = llvm.c.LLVMStructGetTypeAtIndex(lhs_type, 1);
        const some_val = self.builder.buildLoad(val_type, val_ptr, "coalesce.some.val");
        const some_end_bb = llvm.c.LLVMGetInsertBlock(self.builder.ref);
        _ = self.builder.buildBr(merge_bb);

        // None block: evaluate right side
        self.builder.positionAtEnd(none_bb);
        const none_val = try self.emitExpr(bin.right);
        const none_end_bb = llvm.c.LLVMGetInsertBlock(self.builder.ref);
        _ = self.builder.buildBr(merge_bb);

        // Merge block: phi node to select result
        self.builder.positionAtEnd(merge_bb);
        const phi = self.builder.buildPhi(val_type, "coalesce.result");
        var incoming_vals = [_]llvm.ValueRef{ some_val, none_val };
        var incoming_blocks = [_]llvm.BasicBlockRef{ some_end_bb, none_end_bb };
        llvm.addIncoming(phi, &incoming_vals, &incoming_blocks);

        _ = lhs_bb; // Suppress unused variable warning

        return phi;
    }

    /// Create an optional Some value wrapping the given value.
    fn emitSome(self: *Emitter, value: llvm.ValueRef, inner_type: llvm.TypeRef) llvm.ValueRef {
        // Optional type is { i1, T }
        var opt_fields = [_]llvm.TypeRef{
            llvm.Types.int1(self.ctx), // tag
            inner_type, // value
        };
        const opt_type = llvm.Types.struct_(self.ctx, &opt_fields, false);

        // Allocate and populate
        const opt_alloca = self.builder.buildAlloca(opt_type, "some.tmp");

        // Set tag to 1 (Some)
        var tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const tag_ptr = self.builder.buildGEP(opt_type, opt_alloca, &tag_indices, "some.tag.ptr");
        _ = self.builder.buildStore(llvm.Const.int1(self.ctx, true), tag_ptr);

        // Set value
        var val_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 1),
        };
        const val_ptr = self.builder.buildGEP(opt_type, opt_alloca, &val_indices, "some.val.ptr");
        _ = self.builder.buildStore(value, val_ptr);

        // Load and return
        return self.builder.buildLoad(opt_type, opt_alloca, "some.val");
    }

    /// Create an optional None value of the given optional type.
    fn emitNone(self: *Emitter, opt_type: llvm.TypeRef) llvm.ValueRef {
        // Allocate and set tag to 0 (None)
        const opt_alloca = self.builder.buildAlloca(opt_type, "none.tmp");

        // Set tag to 0 (None)
        var tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const tag_ptr = self.builder.buildGEP(opt_type, opt_alloca, &tag_indices, "none.tag.ptr");
        _ = self.builder.buildStore(llvm.Const.int1(self.ctx, false), tag_ptr);

        // Load and return (value field is undefined/uninitialized)
        return self.builder.buildLoad(opt_type, opt_alloca, "none.val");
    }

    /// Emit a method call expression.
    /// Handles special methods like Rc.new(), Cell.new(), .clone(), .downgrade(), etc.
    fn emitMethodCall(self: *Emitter, method: *ast.MethodCall) EmitError!llvm.ValueRef {
        // Check for static constructors: Rc.new(value), Cell.new(value)
        if (method.object == .identifier) {
            const obj_name = method.object.identifier.name;

            if (std.mem.eql(u8, obj_name, "Rc") and std.mem.eql(u8, method.method_name, "new")) {
                return self.emitRcNew(method);
            }

            if (std.mem.eql(u8, obj_name, "Cell") and std.mem.eql(u8, method.method_name, "new")) {
                return self.emitCellNew(method);
            }
        }

        // Emit the object
        const object = try self.emitExpr(method.object);
        const object_type = llvm.typeOf(object);

        // Check for Cell methods: .get(), .set()
        if (std.mem.eql(u8, method.method_name, "get")) {
            // Cell.get() - load the value from the cell
            return self.emitCellGet(method, object, object_type);
        }

        if (std.mem.eql(u8, method.method_name, "set")) {
            // Cell.set(value) - store a new value in the cell
            return self.emitCellSet(method, object, object_type);
        }

        if (std.mem.eql(u8, method.method_name, "replace")) {
            // Cell.replace(value) - store new value, return old
            return self.emitCellReplace(method, object, object_type);
        }

        // Check for Rc methods
        if (std.mem.eql(u8, method.method_name, "clone")) {
            // For Rc, clone increments reference count and returns same pointer
            return self.emitRcClone(object, object_type);
        }

        if (std.mem.eql(u8, method.method_name, "downgrade")) {
            // Rc.downgrade() creates a Weak reference
            return self.emitRcDowngrade(object, object_type);
        }

        if (std.mem.eql(u8, method.method_name, "upgrade")) {
            // Weak.upgrade() attempts to get an Rc back
            return self.emitWeakUpgrade(object, object_type);
        }

        // For other methods, fall back to a placeholder for now
        // TODO: Implement general method lookup and dispatch
        return llvm.Const.int32(self.ctx, 0);
    }

    /// Emit Rc.new(value) - allocates an Rc and stores the value.
    fn emitRcNew(self: *Emitter, method: *ast.MethodCall) EmitError!llvm.ValueRef {
        if (method.args.len != 1) {
            return EmitError.InvalidAST;
        }

        // Emit the value to be wrapped
        const value = try self.emitExpr(method.args[0]);
        const value_type = llvm.typeOf(value);

        // Declare the runtime function if not already
        const rc_alloc_fn = self.getOrDeclareRcAlloc();

        // Call klar_rc_alloc(value_size, value_align)
        // For now, use sizeof based on type kind
        const value_size: u64 = switch (llvm.getTypeKind(value_type)) {
            llvm.c.LLVMIntegerTypeKind => @as(u64, llvm.c.LLVMGetIntTypeWidth(value_type)) / 8,
            llvm.c.LLVMFloatTypeKind => 4,
            llvm.c.LLVMDoubleTypeKind => 8,
            llvm.c.LLVMPointerTypeKind => 8, // Assume 64-bit pointers
            else => 8, // Default to 8 bytes
        };
        const value_align: u64 = value_size; // Assume natural alignment

        var args = [_]llvm.ValueRef{
            llvm.Const.int64(self.ctx, @intCast(value_size)),
            llvm.Const.int64(self.ctx, @intCast(value_align)),
        };
        const fn_type = llvm.c.LLVMGlobalGetValueType(rc_alloc_fn);
        const ptr = self.builder.buildCall(
            fn_type,
            rc_alloc_fn,
            &args,
            "rc.alloc",
        );

        // Store the value at the returned pointer (opaque pointer in LLVM 15+)
        _ = self.builder.buildStore(value, ptr);

        // Return the pointer (Rc[T] is represented as a pointer)
        return ptr;
    }

    /// Emit rc.clone() - increments reference count.
    fn emitRcClone(self: *Emitter, object: llvm.ValueRef, object_type: llvm.TypeRef) EmitError!llvm.ValueRef {
        _ = object_type;

        // Declare the runtime function
        const rc_clone_fn = self.getOrDeclareRcClone();

        // Call klar_rc_clone(ptr)
        var args = [_]llvm.ValueRef{object};
        return self.builder.buildCall(
            llvm.c.LLVMGlobalGetValueType(rc_clone_fn),
            rc_clone_fn,
            &args,
            "rc.clone",
        );
    }

    /// Emit rc.downgrade() - creates a Weak reference.
    fn emitRcDowngrade(self: *Emitter, object: llvm.ValueRef, object_type: llvm.TypeRef) EmitError!llvm.ValueRef {
        _ = object_type;

        // Declare the runtime function
        const rc_downgrade_fn = self.getOrDeclareRcDowngrade();

        // Call klar_rc_downgrade(ptr)
        var args = [_]llvm.ValueRef{object};
        return self.builder.buildCall(
            llvm.c.LLVMGlobalGetValueType(rc_downgrade_fn),
            rc_downgrade_fn,
            &args,
            "rc.downgrade",
        );
    }

    /// Emit weak.upgrade() - attempts to get Rc from Weak.
    fn emitWeakUpgrade(self: *Emitter, object: llvm.ValueRef, object_type: llvm.TypeRef) EmitError!llvm.ValueRef {
        _ = object_type;

        // Declare the runtime function
        const weak_upgrade_fn = self.getOrDeclareWeakUpgrade();

        // Call klar_weak_upgrade(ptr) - returns null if Rc is gone
        var args = [_]llvm.ValueRef{object};
        const result_ptr = self.builder.buildCall(
            llvm.c.LLVMGlobalGetValueType(weak_upgrade_fn),
            weak_upgrade_fn,
            &args,
            "weak.upgrade",
        );

        // Convert to optional: check if null
        const is_null = self.builder.buildICmp(llvm.c.LLVMIntEQ, result_ptr, llvm.c.LLVMConstNull(llvm.typeOf(result_ptr)), "is.null");

        // Create optional struct { i1, ptr }
        const ptr_type = llvm.Types.pointer(self.ctx);
        var struct_types = [_]llvm.TypeRef{ llvm.Types.int1(self.ctx), ptr_type };
        const opt_type = llvm.c.LLVMStructTypeInContext(self.ctx.ref, &struct_types, 2, 0);

        const opt_alloca = self.builder.buildAlloca(opt_type, "upgrade.opt");

        // Set tag: 1 if not null (Some), 0 if null (None)
        const tag = self.builder.buildNot(is_null, "tag");
        var tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const tag_ptr = self.builder.buildGEP(opt_type, opt_alloca, &tag_indices, "opt.tag.ptr");
        _ = self.builder.buildStore(tag, tag_ptr);

        // Set value (the pointer)
        var val_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 1),
        };
        const val_ptr = self.builder.buildGEP(opt_type, opt_alloca, &val_indices, "opt.val.ptr");
        _ = self.builder.buildStore(result_ptr, val_ptr);

        // Load and return the optional
        return self.builder.buildLoad(opt_type, opt_alloca, "upgrade.result");
    }

    /// Emit Cell.new(value) - creates a Cell containing the value.
    /// Cell is represented as a pointer to stack-allocated storage.
    fn emitCellNew(self: *Emitter, method: *ast.MethodCall) EmitError!llvm.ValueRef {
        if (method.args.len != 1) {
            return EmitError.InvalidAST;
        }

        // Emit the value to be stored in the cell
        const value = try self.emitExpr(method.args[0]);
        const value_type = llvm.typeOf(value);

        // Allocate stack space for the cell's value
        const cell_alloca = self.builder.buildAlloca(value_type, "cell.storage");

        // Store the initial value
        _ = self.builder.buildStore(value, cell_alloca);

        // Return the pointer to the cell (Cell[T] is represented as a pointer)
        return cell_alloca;
    }

    /// Emit cell.get() - returns a copy of the value in the cell.
    fn emitCellGet(self: *Emitter, method: *ast.MethodCall, object: llvm.ValueRef, object_type: llvm.TypeRef) EmitError!llvm.ValueRef {
        _ = object_type;

        // The object is a pointer to the cell's storage
        // We need to determine the type to load
        const inner_type = self.inferCellInnerType(method.object);
        return self.builder.buildLoad(inner_type, object, "cell.get");
    }

    /// Emit cell.set(value) - stores a new value in the cell.
    fn emitCellSet(self: *Emitter, method: *ast.MethodCall, object: llvm.ValueRef, object_type: llvm.TypeRef) EmitError!llvm.ValueRef {
        _ = object_type;

        if (method.args.len != 1) {
            return EmitError.InvalidAST;
        }

        // Emit the new value
        const new_value = try self.emitExpr(method.args[0]);

        // Store the new value in the cell
        _ = self.builder.buildStore(new_value, object);

        // set() returns void, but we need to return something
        return llvm.Const.int32(self.ctx, 0);
    }

    /// Emit cell.replace(value) - stores new value, returns old value.
    fn emitCellReplace(self: *Emitter, method: *ast.MethodCall, object: llvm.ValueRef, object_type: llvm.TypeRef) EmitError!llvm.ValueRef {
        _ = object_type;

        if (method.args.len != 1) {
            return EmitError.InvalidAST;
        }

        // Load the old value first
        const inner_type = self.inferCellInnerType(method.object);
        const old_value = self.builder.buildLoad(inner_type, object, "cell.old");

        // Emit and store the new value
        const new_value = try self.emitExpr(method.args[0]);
        _ = self.builder.buildStore(new_value, object);

        // Return the old value
        return old_value;
    }

    /// Infer the inner type of a Cell expression.
    fn inferCellInnerType(self: *Emitter, expr: ast.Expr) llvm.TypeRef {
        // Check if it's an identifier with tracked inner type
        if (expr == .identifier) {
            const id = expr.identifier;
            if (self.named_values.get(id.name)) |local| {
                if (local.inner_type) |inner| {
                    return inner;
                }
            }
        }

        // For Cell.new(value), the inner type is the type of value
        if (expr == .method_call) {
            const method = expr.method_call;
            if (method.object == .identifier) {
                const obj_name = method.object.identifier.name;
                if (std.mem.eql(u8, obj_name, "Cell") and std.mem.eql(u8, method.method_name, "new")) {
                    if (method.args.len > 0) {
                        return self.inferExprType(method.args[0]) catch llvm.Types.int32(self.ctx);
                    }
                }
            }
        }

        // Default to i32
        return llvm.Types.int32(self.ctx);
    }

    /// Emit a closure expression.
    /// Closures are represented as a struct { fn_ptr, env_ptr } where:
    /// - fn_ptr points to the lifted function
    /// - env_ptr points to captured variables (null if no captures)
    fn emitClosure(self: *Emitter, closure: *ast.Closure) EmitError!llvm.ValueRef {
        // Generate unique name for this closure's lifted function
        const closure_id = self.closure_counter;
        self.closure_counter += 1;

        // Build closure name
        var name_buf: [64]u8 = undefined;
        const name_slice = std.fmt.bufPrint(&name_buf, "__klar_closure_{d}", .{closure_id}) catch
            return EmitError.OutOfMemory;
        const fn_name = self.allocator.dupeZ(u8, name_slice) catch return EmitError.OutOfMemory;
        defer self.allocator.free(fn_name);

        // Determine return type
        const return_type = if (closure.return_type) |rt|
            try self.typeExprToLLVM(rt)
        else
            llvm.Types.int32(self.ctx); // Default to i32 if not specified

        // Build parameter types for the lifted function
        // First parameter is the environment pointer (for captured variables)
        var param_types = std.ArrayListUnmanaged(llvm.TypeRef){};
        defer param_types.deinit(self.allocator);

        // Add environment pointer as first parameter
        param_types.append(self.allocator, llvm.Types.pointer(self.ctx)) catch return EmitError.OutOfMemory;

        // Add closure's declared parameters
        for (closure.params) |param| {
            const param_ty = if (param.type_) |t|
                try self.typeExprToLLVM(t)
            else
                llvm.Types.int32(self.ctx);
            param_types.append(self.allocator, param_ty) catch return EmitError.OutOfMemory;
        }

        // Create function type
        const fn_type = llvm.Types.function(return_type, param_types.items, false);

        // Create the lifted function
        const lifted_fn = llvm.addFunction(self.module, fn_name, fn_type);
        llvm.setFunctionCallConv(lifted_fn, self.calling_convention.toLLVM());

        // Save current state before emitting the lifted function body
        const saved_bb = llvm.c.LLVMGetInsertBlock(self.builder.ref);
        const saved_func = self.current_function;
        const saved_named_values = self.named_values;
        const saved_return_type = self.current_return_type;
        const saved_terminator = self.has_terminator;

        // Set up new context for lifted function
        self.named_values = std.StringHashMap(LocalValue).init(self.allocator);
        self.current_function = lifted_fn;
        self.has_terminator = false;

        // Set up return type info
        self.current_return_type = .{
            .llvm_type = return_type,
            .is_optional = if (closure.return_type) |rt| rt == .optional else false,
            .inner_type = if (closure.return_type) |rt|
                if (rt == .optional) try self.typeExprToLLVM(rt.optional.inner) else null
            else
                null,
        };

        // Create entry block for lifted function
        const entry_bb = llvm.appendBasicBlock(self.ctx, lifted_fn, "entry");
        self.builder.positionAtEnd(entry_bb);

        // Get environment pointer (first parameter)
        const env_ptr = llvm.getParam(lifted_fn, 0);

        // Load captured variables from environment and add to named_values.
        // The environment is a heap-allocated struct with values stored directly (not pointers).
        if (closure.captures) |captures| {
            // Build the environment struct type (must match what createClosureEnvironment built).
            // For now, assume all captures are i32. TODO: pass type info through AST.
            var field_types: [32]llvm.TypeRef = undefined;
            for (0..captures.len) |i| {
                field_types[i] = llvm.Types.int32(self.ctx);
            }
            const env_struct_type = llvm.Types.struct_(self.ctx, field_types[0..captures.len], false);

            for (captures, 0..) |capture, i| {
                // GEP to get pointer to the value field in the env struct
                var indices = [_]llvm.ValueRef{
                    llvm.Const.int32(self.ctx, 0),
                    llvm.Const.int32(self.ctx, @intCast(i)),
                };
                const field_ptr = self.builder.buildGEP(
                    env_struct_type,
                    env_ptr,
                    &indices,
                    "cap.field.ptr",
                );

                // Load the value from the env struct into a local alloca
                const cap_ty = llvm.Types.int32(self.ctx);
                const local_alloca = self.builder.buildAlloca(cap_ty, "cap.local");
                const cap_value = self.builder.buildLoad(cap_ty, field_ptr, "cap.value");
                _ = self.builder.buildStore(cap_value, local_alloca);

                // Store the alloca as the named value (will be loaded when accessed)
                self.named_values.put(capture.name, .{
                    .value = local_alloca,
                    .is_alloca = true,
                    .ty = cap_ty,
                    .is_signed = true,
                }) catch return EmitError.OutOfMemory;
            }
        }

        // Add closure parameters to named_values
        for (closure.params, 0..) |param, i| {
            const param_value = llvm.getParam(lifted_fn, @intCast(i + 1)); // +1 for env ptr
            const param_ty = if (param.type_) |t|
                try self.typeExprToLLVM(t)
            else
                llvm.Types.int32(self.ctx);

            // Allocate stack space for parameter
            const param_name = self.allocator.dupeZ(u8, param.name) catch return EmitError.OutOfMemory;
            defer self.allocator.free(param_name);

            const alloca = self.builder.buildAlloca(param_ty, param_name);
            _ = self.builder.buildStore(param_value, alloca);

            self.named_values.put(param.name, .{
                .value = alloca,
                .is_alloca = true,
                .ty = param_ty,
                .is_signed = self.isTypeExprSigned(param.type_),
            }) catch return EmitError.OutOfMemory;
        }

        // Emit the closure body
        const body_value = try self.emitExpr(closure.body);

        // Add return if not terminated
        if (!self.has_terminator) {
            _ = self.builder.buildRet(body_value);
        }

        // Restore original context
        self.named_values.deinit();
        self.named_values = saved_named_values;
        self.current_function = saved_func;
        self.current_return_type = saved_return_type;
        self.has_terminator = saved_terminator;
        if (saved_bb) |bb| {
            self.builder.positionAtEnd(bb);
        }

        // Now create the closure value in the calling context
        // Closure struct: { fn_ptr: ptr, env_ptr: ptr }
        const closure_struct_type = self.getClosureStructType();

        // Allocate closure struct
        const closure_alloca = self.builder.buildAlloca(closure_struct_type, "closure");

        // Store function pointer
        var fn_ptr_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const fn_ptr_gep = self.builder.buildGEP(closure_struct_type, closure_alloca, &fn_ptr_indices, "closure.fn.ptr");
        _ = self.builder.buildStore(lifted_fn, fn_ptr_gep);

        // Create and store environment (captured variables)
        const env_value = try self.createClosureEnvironment(closure.captures);
        var env_ptr_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 1),
        };
        const env_ptr_gep = self.builder.buildGEP(closure_struct_type, closure_alloca, &env_ptr_indices, "closure.env.ptr");
        _ = self.builder.buildStore(env_value, env_ptr_gep);

        // Return the closure struct (loaded)
        return self.builder.buildLoad(closure_struct_type, closure_alloca, "closure.val");
    }

    /// Create the environment struct for captured variables.
    /// Allocates the environment on the heap so closures can be returned from functions.
    fn createClosureEnvironment(self: *Emitter, captures: ?[]const ast.CapturedVar) EmitError!llvm.ValueRef {
        if (captures == null or captures.?.len == 0) {
            // No captures - return null pointer
            return llvm.c.LLVMConstNull(llvm.Types.pointer(self.ctx));
        }

        const captures_list = captures.?;

        // Build a struct type containing all captured values (by value, not by pointer).
        // This allows closures to be returned from functions without dangling pointers.
        var field_types: [32]llvm.TypeRef = undefined;
        if (captures_list.len > 32) return EmitError.OutOfMemory;

        for (captures_list, 0..) |capture, i| {
            if (self.named_values.get(capture.name)) |local| {
                field_types[i] = local.ty;
            } else {
                // Fallback to i32 if not found
                field_types[i] = llvm.Types.int32(self.ctx);
            }
        }

        const env_struct_type = llvm.Types.struct_(self.ctx, field_types[0..captures_list.len], false);

        // Calculate size of the struct: each i32 is 4 bytes, no padding needed for homogeneous structs
        // TODO: Use proper target data layout when capturing non-i32 types
        const size_of_env: u64 = captures_list.len * 4;

        // Allocate on the heap using malloc
        const malloc_fn = self.getOrDeclareMalloc();
        var malloc_args = [_]llvm.ValueRef{llvm.Const.int64(self.ctx, @intCast(size_of_env))};
        const env_ptr = llvm.c.LLVMBuildCall2(
            self.builder.ref,
            llvm.c.LLVMGlobalGetValueType(malloc_fn),
            malloc_fn,
            &malloc_args,
            1,
            "env.heap",
        );

        // Copy each captured value into the heap-allocated struct
        for (captures_list, 0..) |capture, i| {
            if (self.named_values.get(capture.name)) |local| {
                // GEP to the i-th field in the environment struct
                var indices = [_]llvm.ValueRef{
                    llvm.Const.int32(self.ctx, 0),
                    llvm.Const.int32(self.ctx, @intCast(i)),
                };
                const field_ptr = self.builder.buildGEP(env_struct_type, env_ptr, &indices, "env.field");

                // Load the value from the local variable and store it in the env
                const value = self.builder.buildLoad(local.ty, local.value, "cap.load");
                _ = self.builder.buildStore(value, field_ptr);
            }
        }

        // Return pointer to heap-allocated environment
        return env_ptr;
    }

    /// Check if a type expression represents a signed type.
    fn isTypeExprSigned(self: *Emitter, type_expr: ?ast.TypeExpr) bool {
        _ = self;
        if (type_expr == null) return true; // Default to signed

        const te = type_expr.?;
        return switch (te) {
            .named => |n| {
                // Unsigned types
                if (std.mem.startsWith(u8, n.name, "u")) return false;
                return true;
            },
            else => true,
        };
    }

    // Runtime function declarations

    fn getOrDeclareRcAlloc(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_rc_alloc";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // Define: *anyopaque klar_rc_alloc(usize value_size, usize value_align)
        // Layout: [strong_count:i64][weak_count:i64][value:value_size]
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        var param_types = [_]llvm.TypeRef{ i64_type, i64_type };
        const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 2, 0);
        const func = llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);

        // Create function body
        const entry_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "entry");
        const saved_bb = llvm.c.LLVMGetInsertBlock(self.builder.ref);
        const saved_func = self.current_function;

        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, entry_bb);

        // Parameters
        const value_size = llvm.c.LLVMGetParam(func, 0);
        _ = llvm.c.LLVMGetParam(func, 1); // value_align (unused for now)

        // Calculate total size: 16 (header) + value_size
        const header_size = llvm.Const.int64(self.ctx, 16);
        const total_size = llvm.c.LLVMBuildAdd(self.builder.ref, header_size, value_size, "total_size");

        // Call malloc
        const malloc_fn = self.getOrDeclareMalloc();
        var malloc_args = [_]llvm.ValueRef{total_size};
        const raw_ptr = llvm.c.LLVMBuildCall2(self.builder.ref, llvm.c.LLVMGlobalGetValueType(malloc_fn), malloc_fn, &malloc_args, 1, "malloc_result");

        // Store strong_count = 1 at offset 0
        const strong_ptr = llvm.c.LLVMBuildBitCast(self.builder.ref, raw_ptr, llvm.c.LLVMPointerType(i64_type, 0), "strong_ptr");
        _ = llvm.c.LLVMBuildStore(self.builder.ref, llvm.Const.int64(self.ctx, 1), strong_ptr);

        // Store weak_count = 1 at offset 8
        const eight = llvm.Const.int64(self.ctx, 8);
        const weak_ptr_int = llvm.c.LLVMBuildAdd(self.builder.ref, llvm.c.LLVMBuildPtrToInt(self.builder.ref, raw_ptr, i64_type, "ptr_int"), eight, "weak_off");
        const weak_ptr = llvm.c.LLVMBuildIntToPtr(self.builder.ref, weak_ptr_int, llvm.c.LLVMPointerType(i64_type, 0), "weak_ptr");
        _ = llvm.c.LLVMBuildStore(self.builder.ref, llvm.Const.int64(self.ctx, 1), weak_ptr);

        // Return pointer to value (offset 16)
        const sixteen = llvm.Const.int64(self.ctx, 16);
        const val_ptr_int = llvm.c.LLVMBuildAdd(self.builder.ref, llvm.c.LLVMBuildPtrToInt(self.builder.ref, raw_ptr, i64_type, "ptr_int2"), sixteen, "val_off");
        const val_ptr = llvm.c.LLVMBuildIntToPtr(self.builder.ref, val_ptr_int, ptr_type, "val_ptr");

        _ = llvm.c.LLVMBuildRet(self.builder.ref, val_ptr);

        // Restore builder position
        if (saved_bb) |bb| {
            llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, bb);
        }
        self.current_function = saved_func;

        return func;
    }

    fn getOrDeclareMalloc(self: *Emitter) llvm.ValueRef {
        const fn_name = "malloc";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        const ptr_type = llvm.Types.pointer(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        var param_types = [_]llvm.TypeRef{i64_type};
        const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 1, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareRcClone(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_rc_clone";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // Define: *anyopaque klar_rc_clone(*anyopaque ptr)
        // Increments strong_count (at header offset -16 from value ptr) and returns ptr
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        var param_types = [_]llvm.TypeRef{ptr_type};
        const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, param_types.len, 0);
        const func = llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);

        // Create function body
        const entry_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "entry");
        const saved_bb = llvm.c.LLVMGetInsertBlock(self.builder.ref);
        const saved_func = self.current_function;

        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, entry_bb);

        // Get value pointer parameter
        const value_ptr = llvm.c.LLVMGetParam(func, 0);

        // Calculate header address: value_ptr - 16
        const ptr_int = llvm.c.LLVMBuildPtrToInt(self.builder.ref, value_ptr, i64_type, "ptr_int");
        const header_off = llvm.c.LLVMBuildSub(self.builder.ref, ptr_int, llvm.Const.int64(self.ctx, 16), "header_off");
        const strong_ptr = llvm.c.LLVMBuildIntToPtr(self.builder.ref, header_off, llvm.c.LLVMPointerType(i64_type, 0), "strong_ptr");

        // Load, increment, store strong_count
        const strong_count = llvm.c.LLVMBuildLoad2(self.builder.ref, i64_type, strong_ptr, "strong_count");
        const new_count = llvm.c.LLVMBuildAdd(self.builder.ref, strong_count, llvm.Const.int64(self.ctx, 1), "new_count");
        _ = llvm.c.LLVMBuildStore(self.builder.ref, new_count, strong_ptr);

        // Return the original pointer
        _ = llvm.c.LLVMBuildRet(self.builder.ref, value_ptr);

        // Restore builder position
        if (saved_bb) |bb| {
            llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, bb);
        }
        self.current_function = saved_func;

        return func;
    }

    fn getOrDeclareRcDowngrade(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_rc_downgrade";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // Define: *anyopaque klar_rc_downgrade(*anyopaque ptr)
        // Increments weak_count (at header offset -8 from value ptr) and returns ptr
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        var param_types = [_]llvm.TypeRef{ptr_type};
        const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, param_types.len, 0);
        const func = llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);

        // Create function body
        const entry_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "entry");
        const saved_bb = llvm.c.LLVMGetInsertBlock(self.builder.ref);
        const saved_func = self.current_function;

        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, entry_bb);

        // Get value pointer parameter
        const value_ptr = llvm.c.LLVMGetParam(func, 0);

        // Calculate weak_count address: value_ptr - 8 (header is at -16, weak_count is at +8 in header)
        const ptr_int = llvm.c.LLVMBuildPtrToInt(self.builder.ref, value_ptr, i64_type, "ptr_int");
        const weak_off = llvm.c.LLVMBuildSub(self.builder.ref, ptr_int, llvm.Const.int64(self.ctx, 8), "weak_off");
        const weak_ptr = llvm.c.LLVMBuildIntToPtr(self.builder.ref, weak_off, llvm.c.LLVMPointerType(i64_type, 0), "weak_ptr");

        // Load, increment, store weak_count
        const weak_count = llvm.c.LLVMBuildLoad2(self.builder.ref, i64_type, weak_ptr, "weak_count");
        const new_count = llvm.c.LLVMBuildAdd(self.builder.ref, weak_count, llvm.Const.int64(self.ctx, 1), "new_count");
        _ = llvm.c.LLVMBuildStore(self.builder.ref, new_count, weak_ptr);

        // Return the original pointer
        _ = llvm.c.LLVMBuildRet(self.builder.ref, value_ptr);

        // Restore builder position
        if (saved_bb) |bb| {
            llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, bb);
        }
        self.current_function = saved_func;

        return func;
    }

    fn getOrDeclareWeakUpgrade(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_weak_upgrade";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // Define: *anyopaque klar_weak_upgrade(*anyopaque ptr)
        // Returns ptr if strong_count > 0 (and increments it), else returns null
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        var param_types = [_]llvm.TypeRef{ptr_type};
        const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, param_types.len, 0);
        const func = llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);

        // Create function body
        const entry_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "entry");
        const success_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "success");
        const fail_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "fail");

        const saved_bb = llvm.c.LLVMGetInsertBlock(self.builder.ref);
        const saved_func = self.current_function;

        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, entry_bb);

        // Get value pointer parameter
        const value_ptr = llvm.c.LLVMGetParam(func, 0);

        // Calculate strong_count address: value_ptr - 16
        const ptr_int = llvm.c.LLVMBuildPtrToInt(self.builder.ref, value_ptr, i64_type, "ptr_int");
        const header_off = llvm.c.LLVMBuildSub(self.builder.ref, ptr_int, llvm.Const.int64(self.ctx, 16), "header_off");
        const strong_ptr = llvm.c.LLVMBuildIntToPtr(self.builder.ref, header_off, llvm.c.LLVMPointerType(i64_type, 0), "strong_ptr");

        // Load strong_count and check if > 0
        const strong_count = llvm.c.LLVMBuildLoad2(self.builder.ref, i64_type, strong_ptr, "strong_count");
        const is_alive = llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntUGT, strong_count, llvm.Const.int64(self.ctx, 0), "is_alive");
        _ = llvm.c.LLVMBuildCondBr(self.builder.ref, is_alive, success_bb, fail_bb);

        // Success: increment strong_count and return ptr
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, success_bb);
        const new_count = llvm.c.LLVMBuildAdd(self.builder.ref, strong_count, llvm.Const.int64(self.ctx, 1), "new_count");
        _ = llvm.c.LLVMBuildStore(self.builder.ref, new_count, strong_ptr);
        _ = llvm.c.LLVMBuildRet(self.builder.ref, value_ptr);

        // Fail: return null
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, fail_bb);
        _ = llvm.c.LLVMBuildRet(self.builder.ref, llvm.c.LLVMConstNull(ptr_type));

        // Restore builder position
        if (saved_bb) |bb| {
            llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, bb);
        }
        self.current_function = saved_func;

        return func;
    }

    fn getOrDeclareRcDrop(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_rc_drop";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // Define: void klar_rc_drop(*anyopaque ptr, usize size, usize align, *fn destructor)
        // Decrements strong_count. If 0, calls destructor (if not null), decrements weak_count.
        // If weak_count also 0, frees the allocation.
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        const void_type = llvm.Types.void_(self.ctx);
        var param_types = [_]llvm.TypeRef{ ptr_type, i64_type, i64_type, ptr_type };
        const fn_type = llvm.c.LLVMFunctionType(void_type, &param_types, param_types.len, 0);
        const func = llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);

        // Create basic blocks
        const entry_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "entry");
        const strong_zero_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "strong_zero");
        const call_dtor_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "call_dtor");
        const after_dtor_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "after_dtor");
        const free_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "free");
        const done_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "done");

        const saved_bb = llvm.c.LLVMGetInsertBlock(self.builder.ref);
        const saved_func = self.current_function;

        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, entry_bb);

        // Get parameters
        const value_ptr = llvm.c.LLVMGetParam(func, 0);
        _ = llvm.c.LLVMGetParam(func, 1); // value_size (for free calculation)
        _ = llvm.c.LLVMGetParam(func, 2); // value_align (unused)
        const destructor = llvm.c.LLVMGetParam(func, 3);

        // Calculate strong_count address: value_ptr - 16
        const ptr_int = llvm.c.LLVMBuildPtrToInt(self.builder.ref, value_ptr, i64_type, "ptr_int");
        const header_off = llvm.c.LLVMBuildSub(self.builder.ref, ptr_int, llvm.Const.int64(self.ctx, 16), "header_off");
        const strong_ptr = llvm.c.LLVMBuildIntToPtr(self.builder.ref, header_off, llvm.c.LLVMPointerType(i64_type, 0), "strong_ptr");

        // Load, decrement, store strong_count
        const strong_count = llvm.c.LLVMBuildLoad2(self.builder.ref, i64_type, strong_ptr, "strong_count");
        const new_strong = llvm.c.LLVMBuildSub(self.builder.ref, strong_count, llvm.Const.int64(self.ctx, 1), "new_strong");
        _ = llvm.c.LLVMBuildStore(self.builder.ref, new_strong, strong_ptr);

        // Check if strong_count == 0
        const is_zero = llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntEQ, new_strong, llvm.Const.int64(self.ctx, 0), "is_zero");
        _ = llvm.c.LLVMBuildCondBr(self.builder.ref, is_zero, strong_zero_bb, done_bb);

        // strong_zero: call destructor if not null, then decrement weak_count
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, strong_zero_bb);
        const dtor_null = llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntEQ, destructor, llvm.c.LLVMConstNull(ptr_type), "dtor_null");
        _ = llvm.c.LLVMBuildCondBr(self.builder.ref, dtor_null, after_dtor_bb, call_dtor_bb);

        // call_dtor: call destructor(value_ptr)
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, call_dtor_bb);
        var dtor_param_types = [_]llvm.TypeRef{ptr_type};
        const dtor_fn_type = llvm.c.LLVMFunctionType(void_type, &dtor_param_types, 1, 0);
        var dtor_args = [_]llvm.ValueRef{value_ptr};
        _ = llvm.c.LLVMBuildCall2(self.builder.ref, dtor_fn_type, destructor, &dtor_args, 1, "");
        _ = llvm.c.LLVMBuildBr(self.builder.ref, after_dtor_bb);

        // after_dtor: decrement weak_count
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, after_dtor_bb);
        const weak_off = llvm.c.LLVMBuildSub(self.builder.ref, ptr_int, llvm.Const.int64(self.ctx, 8), "weak_off");
        const weak_ptr = llvm.c.LLVMBuildIntToPtr(self.builder.ref, weak_off, llvm.c.LLVMPointerType(i64_type, 0), "weak_ptr");
        const weak_count = llvm.c.LLVMBuildLoad2(self.builder.ref, i64_type, weak_ptr, "weak_count");
        const new_weak = llvm.c.LLVMBuildSub(self.builder.ref, weak_count, llvm.Const.int64(self.ctx, 1), "new_weak");
        _ = llvm.c.LLVMBuildStore(self.builder.ref, new_weak, weak_ptr);

        // Check if weak_count == 0
        const weak_zero = llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntEQ, new_weak, llvm.Const.int64(self.ctx, 0), "weak_zero");
        _ = llvm.c.LLVMBuildCondBr(self.builder.ref, weak_zero, free_bb, done_bb);

        // free: call free on the header pointer
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, free_bb);
        const header_ptr = llvm.c.LLVMBuildIntToPtr(self.builder.ref, header_off, ptr_type, "header_ptr");
        const free_fn = self.getOrDeclareFree();
        var free_args = [_]llvm.ValueRef{header_ptr};
        _ = llvm.c.LLVMBuildCall2(self.builder.ref, llvm.c.LLVMGlobalGetValueType(free_fn), free_fn, &free_args, 1, "");
        _ = llvm.c.LLVMBuildBr(self.builder.ref, done_bb);

        // done: return
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, done_bb);
        _ = llvm.c.LLVMBuildRetVoid(self.builder.ref);

        // Restore builder position
        if (saved_bb) |bb| {
            llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, bb);
        }
        self.current_function = saved_func;

        return func;
    }

    fn getOrDeclareFree(self: *Emitter) llvm.ValueRef {
        const fn_name = "free";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        const ptr_type = llvm.Types.pointer(self.ctx);
        const void_type = llvm.Types.void_(self.ctx);
        var param_types = [_]llvm.TypeRef{ptr_type};
        const fn_type = llvm.c.LLVMFunctionType(void_type, &param_types, 1, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    /// Emit a print or println call using libc puts/printf
    fn emitPrint(self: *Emitter, args: []const ast.Expr, newline: bool) EmitError!llvm.ValueRef {
        if (args.len == 0) {
            // No arguments - just print newline if println
            if (newline) {
                const puts_fn = self.getOrDeclarePuts();
                const empty_str = self.builder.buildGlobalStringPtr("", "empty");
                var call_args = [_]llvm.ValueRef{empty_str};
                const fn_type = llvm.c.LLVMGlobalGetValueType(puts_fn);
                return self.builder.buildCall(fn_type, puts_fn, &call_args, "");
            }
            return llvm.Const.int32(self.ctx, 0);
        }

        // Emit the argument (should be a string)
        const arg_value = try self.emitExpr(args[0]);

        if (newline) {
            // Use puts for println (automatically adds newline)
            const puts_fn = self.getOrDeclarePuts();
            var call_args = [_]llvm.ValueRef{arg_value};
            const fn_type = llvm.c.LLVMGlobalGetValueType(puts_fn);
            return self.builder.buildCall(fn_type, puts_fn, &call_args, "");
        } else {
            // Use printf for print (no newline)
            const printf_fn = self.getOrDeclarePrintf();
            var call_args = [_]llvm.ValueRef{arg_value};
            const fn_type = llvm.c.LLVMGlobalGetValueType(printf_fn);
            return self.builder.buildCall(fn_type, printf_fn, &call_args, "");
        }
    }

    fn getOrDeclarePuts(self: *Emitter) llvm.ValueRef {
        const fn_name = "puts";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // int puts(const char *s)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);
        var param_types = [_]llvm.TypeRef{ptr_type};
        const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 1, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclarePrintf(self: *Emitter) llvm.ValueRef {
        const fn_name = "printf";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // int printf(const char *format, ...)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);
        var param_types = [_]llvm.TypeRef{ptr_type};
        // variadic = 1 (true)
        const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 1, 1);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    /// Emit a panic call that prints an error message and aborts
    fn emitPanic(self: *Emitter, args: []const ast.Expr) EmitError!llvm.ValueRef {
        // Print "panic: " prefix
        const fprintf_fn = self.getOrDeclareFprintf();
        const stderr_fn = self.getOrDeclareStderr();
        const stderr = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(stderr_fn), stderr_fn, &[_]llvm.ValueRef{}, "stderr");

        const panic_prefix = self.builder.buildGlobalStringPtr("panic: ", "panic_prefix");
        var prefix_args = [_]llvm.ValueRef{ stderr, panic_prefix };
        _ = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(fprintf_fn), fprintf_fn, &prefix_args, "");

        // Print the message if provided
        if (args.len > 0) {
            const msg = try self.emitExpr(args[0]);
            var msg_args = [_]llvm.ValueRef{ stderr, msg };
            _ = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(fprintf_fn), fprintf_fn, &msg_args, "");
        }

        // Print newline
        const newline_str = self.builder.buildGlobalStringPtr("\n", "newline");
        var nl_args = [_]llvm.ValueRef{ stderr, newline_str };
        _ = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(fprintf_fn), fprintf_fn, &nl_args, "");

        // Call abort
        const abort_fn = self.getOrDeclareAbort();
        _ = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(abort_fn), abort_fn, &[_]llvm.ValueRef{}, "");

        // Add unreachable since abort doesn't return
        _ = self.builder.buildUnreachable();

        // Mark that we have a terminator so no more code is emitted after this
        self.has_terminator = true;

        return llvm.Const.int32(self.ctx, 0);
    }

    /// Emit an assert that panics if condition is false
    fn emitAssert(self: *Emitter, args: []const ast.Expr) EmitError!llvm.ValueRef {
        if (args.len == 0) {
            return llvm.Const.int32(self.ctx, 0);
        }

        const func = self.current_function orelse return EmitError.InvalidAST;

        // Evaluate the condition
        const cond = try self.emitExpr(args[0]);

        // Create blocks
        const fail_bb = llvm.appendBasicBlock(self.ctx, func, "assert.fail");
        const continue_bb = llvm.appendBasicBlock(self.ctx, func, "assert.continue");

        // Branch based on condition
        _ = self.builder.buildCondBr(cond, continue_bb, fail_bb);

        // Emit failure block
        self.builder.positionAtEnd(fail_bb);

        // Print assertion failure message to stderr
        const fprintf_fn = self.getOrDeclareFprintf();
        const stderr_fn = self.getOrDeclareStderr();
        const stderr = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(stderr_fn), stderr_fn, &[_]llvm.ValueRef{}, "stderr");

        const fail_msg = self.builder.buildGlobalStringPtr("assertion failed\n", "assert_msg");
        var fail_args = [_]llvm.ValueRef{ stderr, fail_msg };
        _ = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(fprintf_fn), fprintf_fn, &fail_args, "");

        // Call abort
        const abort_fn = self.getOrDeclareAbort();
        _ = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(abort_fn), abort_fn, &[_]llvm.ValueRef{}, "");
        _ = self.builder.buildUnreachable();

        // Continue block - assertion passed
        self.builder.positionAtEnd(continue_bb);

        return llvm.Const.int32(self.ctx, 0);
    }

    fn getOrDeclareFprintf(self: *Emitter) llvm.ValueRef {
        const fn_name = "fprintf";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // int fprintf(FILE *stream, const char *format, ...)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);
        var param_types = [_]llvm.TypeRef{ ptr_type, ptr_type };
        // variadic = 1 (true)
        const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 2, 1);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareStderr(self: *Emitter) llvm.ValueRef {
        // On macOS/BSD, stderr is accessed via __stderrp
        // On Linux, it's accessed via stderr global
        const os = @import("builtin").os.tag;

        if (os == .macos) {
            // macOS: use __stderrp pointer
            const var_name = "__stderrp";
            if (llvm.c.LLVMGetNamedGlobal(self.module.ref, var_name)) |global| {
                // Declare a function that loads the global
                const fn_name = "klar_get_stderr";
                if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
                    return func;
                }

                const ptr_type = llvm.Types.pointer(self.ctx);
                const fn_type = llvm.c.LLVMFunctionType(ptr_type, null, 0, 0);
                const func = llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);

                // Create function body to load __stderrp
                const saved_bb = llvm.c.LLVMGetInsertBlock(self.builder.ref);
                const saved_func = self.current_function;

                const entry = llvm.appendBasicBlock(self.ctx, func, "entry");
                llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, entry);

                const stderr_val = self.builder.buildLoad(ptr_type, global, "stderr");
                _ = llvm.c.LLVMBuildRet(self.builder.ref, stderr_val);

                if (saved_bb) |bb| {
                    llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, bb);
                }
                self.current_function = saved_func;

                return func;
            }

            // Declare __stderrp external global
            const ptr_type = llvm.Types.pointer(self.ctx);
            const global = llvm.c.LLVMAddGlobal(self.module.ref, ptr_type, var_name);
            llvm.c.LLVMSetLinkage(global, llvm.c.LLVMExternalLinkage);

            // Now create getter function
            const fn_name = "klar_get_stderr";
            const fn_type = llvm.c.LLVMFunctionType(ptr_type, null, 0, 0);
            const func = llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);

            // Create function body to load __stderrp
            const saved_bb = llvm.c.LLVMGetInsertBlock(self.builder.ref);
            const saved_func = self.current_function;

            const entry = llvm.appendBasicBlock(self.ctx, func, "entry");
            llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, entry);

            const stderr_val = self.builder.buildLoad(ptr_type, global, "stderr");
            _ = llvm.c.LLVMBuildRet(self.builder.ref, stderr_val);

            if (saved_bb) |bb| {
                llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, bb);
            }
            self.current_function = saved_func;

            return func;
        } else {
            // Linux: use stderr directly (declared as extern)
            const fn_name = "klar_get_stderr";
            if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
                return func;
            }

            // Declare stderr external global
            const ptr_type = llvm.Types.pointer(self.ctx);
            const global = llvm.c.LLVMAddGlobal(self.module.ref, ptr_type, "stderr");
            llvm.c.LLVMSetLinkage(global, llvm.c.LLVMExternalLinkage);

            // Create getter function
            const fn_type = llvm.c.LLVMFunctionType(ptr_type, null, 0, 0);
            const func = llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);

            const saved_bb = llvm.c.LLVMGetInsertBlock(self.builder.ref);
            const saved_func = self.current_function;

            const entry = llvm.appendBasicBlock(self.ctx, func, "entry");
            llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, entry);

            const stderr_val = self.builder.buildLoad(ptr_type, global, "stderr");
            _ = llvm.c.LLVMBuildRet(self.builder.ref, stderr_val);

            if (saved_bb) |bb| {
                llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, bb);
            }
            self.current_function = saved_func;

            return func;
        }
    }

    fn getOrDeclareAbort(self: *Emitter) llvm.ValueRef {
        const fn_name = "abort";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // void abort(void) __attribute__((noreturn))
        const void_type = llvm.Types.void_(self.ctx);
        const fn_type = llvm.c.LLVMFunctionType(void_type, null, 0, 0);
        const func = llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
        // Note: noreturn attribute would be nice but not critical - abort() always exits
        return func;
    }

    // ==================== Scope Management for Automatic Drop ====================

    /// Push a new scope onto the scope stack.
    fn pushScope(self: *Emitter, is_loop: bool) EmitError!void {
        self.scope_stack.append(self.allocator, .{
            .droppables = .{},
            .is_loop = is_loop,
        }) catch return EmitError.OutOfMemory;
    }

    /// Pop a scope and emit drops for all droppable variables in it.
    fn popScope(self: *Emitter) void {
        if (self.scope_stack.pop()) |scope| {
            // Emit drops in reverse order (LIFO - last declared, first dropped)
            self.emitDropsForVars(scope.droppables.items);
            var s = scope;
            s.droppables.deinit(self.allocator);
        }
    }

    /// Emit drops for all variables in the given list (in reverse order).
    fn emitDropsForVars(self: *Emitter, droppables: []const DroppableVar) void {
        if (droppables.len == 0) return;

        // Drop in reverse declaration order
        var i: usize = droppables.len;
        while (i > 0) {
            i -= 1;
            const v = droppables[i];
            self.emitDropForVar(v);
        }
    }

    /// Emit a drop call for a single variable.
    fn emitDropForVar(self: *Emitter, v: DroppableVar) void {
        if (v.is_rc) {
            // Load the Rc pointer from the alloca
            const ptr_type = llvm.Types.pointer(self.ctx);
            const rc_ptr = self.builder.buildLoad(ptr_type, v.alloca, "rc_ptr_drop");

            // Get the size of the inner type
            const inner_size = llvm.c.LLVMSizeOf(v.inner_type);
            const i64_type = llvm.Types.int64(self.ctx);
            const size_val = llvm.c.LLVMBuildIntCast2(self.builder.ref, inner_size, i64_type, 0, "size");

            // Alignment (8 is a safe default for most types)
            const align_val = llvm.Const.int64(self.ctx, 8);

            // No destructor callback for primitive types (null pointer)
            const null_ptr = llvm.c.LLVMConstNull(ptr_type);

            // Call klar_rc_drop
            const drop_fn = self.getOrDeclareRcDrop();
            var args = [_]llvm.ValueRef{ rc_ptr, size_val, align_val, null_ptr };
            _ = llvm.c.LLVMBuildCall2(
                self.builder.ref,
                llvm.c.LLVMGlobalGetValueType(drop_fn),
                drop_fn,
                &args,
                4,
                "",
            );
        }
        // For non-Rc types that need dropping (future: closures, custom destructors),
        // we would add additional cases here.
    }

    /// Emit drops for all scopes up to (and including) the current function scope.
    /// Used for return statements.
    fn emitDropsForReturn(self: *Emitter) void {
        // Drop all variables in all scopes (innermost first)
        var i: usize = self.scope_stack.items.len;
        while (i > 0) {
            i -= 1;
            const scope = self.scope_stack.items[i];
            self.emitDropsForVars(scope.droppables.items);
        }
    }

    /// Emit drops for all scopes until we reach a loop scope.
    /// Used for break/continue statements.
    fn emitDropsForLoopExit(self: *Emitter) void {
        var i: usize = self.scope_stack.items.len;
        while (i > 0) {
            i -= 1;
            const scope = self.scope_stack.items[i];
            self.emitDropsForVars(scope.droppables.items);
            if (scope.is_loop) {
                // Stop at the loop scope itself (its variables will be dropped
                // when the loop ends normally or when outer scope exits)
                break;
            }
        }
    }

    /// Register a variable in the current scope as needing dropping.
    fn registerDroppable(self: *Emitter, name: []const u8, alloca: llvm.ValueRef, inner_type: llvm.TypeRef, is_rc: bool) EmitError!void {
        if (self.scope_stack.items.len == 0) return;

        const scope = &self.scope_stack.items[self.scope_stack.items.len - 1];
        scope.droppables.append(self.allocator, .{
            .name = name,
            .alloca = alloca,
            .inner_type = inner_type,
            .is_rc = is_rc,
        }) catch return EmitError.OutOfMemory;
    }
};
