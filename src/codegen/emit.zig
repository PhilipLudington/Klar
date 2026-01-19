//! AST to LLVM IR emission.
//!
//! Translates typed AST to LLVM IR for native code generation.

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const ast = @import("../ast.zig");
const types = @import("../types.zig");
const checker_mod = @import("../checker.zig");
const TypeChecker = checker_mod.TypeChecker;
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

    /// Reference to type checker for call resolution (generic functions).
    type_checker: ?*const TypeChecker,

    /// Cache of allocated mangled enum names (for cleanup).
    mangled_enum_names: std.ArrayListUnmanaged([]const u8),

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
        /// For Rc[T] or Arc[T], the inner type that can be dereferenced.
        inner_type: ?llvm.TypeRef = null,
        /// True if this is an Arc type (uses atomic operations).
        is_arc: bool = false,
        /// For closure variables, the return type of the closure function.
        /// This is needed because we can't determine the return type from the closure struct type alone.
        closure_return_type: ?llvm.TypeRef = null,
        /// For closure variables, the parameter types of the closure function.
        closure_param_types: ?[]const llvm.TypeRef = null,
        /// True if this is a primitive string type.
        is_string: bool = false,
        /// True if this is a heap-allocated String type.
        is_string_data: bool = false,
        /// True if this is an array or slice type.
        is_array: bool = false,
        /// For fixed-size arrays, the size. Null for slices.
        array_size: ?usize = null,
        /// For arrays/slices, the element type.
        array_element_type: ?types.Type = null,
        /// True if this is a reference parameter (&T or &mut T).
        /// The alloca contains a pointer to the struct, not the struct itself.
        is_reference: bool = false,
        /// For reference params, the LLVM type of the pointed-to struct.
        reference_inner_type: ?llvm.TypeRef = null,
        /// For List[T] types, the element type.
        list_element_type: ?types.Type = null,
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
        /// True if this is an Rc/Arc type (needs ref-counted drop).
        is_rc: bool,
        /// True if this is an Arc type (uses atomic operations).
        is_arc: bool = false,
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
            .type_checker = null,
            .mangled_enum_names = .{},
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

    /// Set the type checker reference for call resolution.
    /// Must be called before emitModule if you want generic function calls to be resolved.
    pub fn setTypeChecker(self: *Emitter, checker: *const TypeChecker) void {
        self.type_checker = checker;
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
        // Free allocated mangled enum names
        for (self.mangled_enum_names.items) |name| {
            self.allocator.free(name);
        }
        self.mangled_enum_names.deinit(self.allocator);
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

        // Second pass: declare all functions (including methods from impl blocks)
        for (module.declarations) |decl| {
            switch (decl) {
                .function => |f| try self.declareFunction(f),
                .impl_decl => |i| try self.declareImplMethods(i),
                else => {},
            }
        }

        // Third pass: emit function bodies (including methods from impl blocks)
        for (module.declarations) |decl| {
            switch (decl) {
                .function => |f| {
                    if (f.body != null) {
                        try self.emitFunction(f);
                    }
                },
                .impl_decl => |i| try self.emitImplMethods(i),
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

    /// Register all struct declarations from a module.
    /// This should be called early, before monomorphized function declarations,
    /// so that struct types are available when building function signatures.
    pub fn registerAllStructDecls(self: *Emitter, module: ast.Module) EmitError!void {
        for (module.declarations) |decl| {
            switch (decl) {
                .struct_decl => |s| try self.registerStructDecl(s),
                else => {},
            }
        }
    }

    fn declareFunction(self: *Emitter, func: *ast.FunctionDecl) EmitError!void {
        // Skip generic functions - they are handled via monomorphization
        if (func.type_params.len > 0) {
            return;
        }

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

    /// Declare methods from an impl block.
    /// Methods are named StructName_methodName for non-generic structs.
    fn declareImplMethods(self: *Emitter, impl_decl: *ast.ImplDecl) EmitError!void {
        // Skip generic impl blocks - they are handled via monomorphization
        if (impl_decl.type_params.len > 0) {
            return;
        }

        // Get the struct name from the target type
        const struct_name = switch (impl_decl.target_type) {
            .named => |n| n.name,
            .generic_apply => return, // Generic applications are handled via monomorphization
            else => return EmitError.InvalidAST,
        };

        for (impl_decl.methods) |method| {
            // Build the mangled method name: StructName_methodName
            var name_buf = std.ArrayListUnmanaged(u8){};
            defer name_buf.deinit(self.allocator);
            name_buf.appendSlice(self.allocator, struct_name) catch return EmitError.OutOfMemory;
            name_buf.append(self.allocator, '_') catch return EmitError.OutOfMemory;
            name_buf.appendSlice(self.allocator, method.name) catch return EmitError.OutOfMemory;

            const mangled_name = self.allocator.dupeZ(u8, name_buf.items) catch return EmitError.OutOfMemory;
            defer self.allocator.free(mangled_name);

            // Build parameter types
            var param_types = std.ArrayListUnmanaged(llvm.TypeRef){};
            defer param_types.deinit(self.allocator);

            for (method.params) |param| {
                const param_ty = try self.typeExprToLLVM(param.type_);
                param_types.append(self.allocator, param_ty) catch return EmitError.OutOfMemory;
            }

            // Get return type
            const return_type = if (method.return_type) |rt|
                try self.typeExprToLLVM(rt)
            else
                llvm.Types.void_(self.ctx);

            const fn_type = llvm.Types.function(return_type, param_types.items, false);
            const llvm_func = llvm.addFunction(self.module, mangled_name, fn_type);

            // Set the calling convention
            llvm.setFunctionCallConv(llvm_func, self.calling_convention.toLLVM());
        }
    }

    /// Emit method bodies from an impl block.
    fn emitImplMethods(self: *Emitter, impl_decl: *ast.ImplDecl) EmitError!void {
        // Skip generic impl blocks - they are handled via monomorphization
        if (impl_decl.type_params.len > 0) {
            return;
        }

        // Get the struct name from the target type
        const struct_name = switch (impl_decl.target_type) {
            .named => |n| n.name,
            .generic_apply => return,
            else => return EmitError.InvalidAST,
        };

        for (impl_decl.methods) |method| {
            if (method.body == null) continue;

            // Build the mangled method name
            var name_buf = std.ArrayListUnmanaged(u8){};
            defer name_buf.deinit(self.allocator);
            name_buf.appendSlice(self.allocator, struct_name) catch return EmitError.OutOfMemory;
            name_buf.append(self.allocator, '_') catch return EmitError.OutOfMemory;
            name_buf.appendSlice(self.allocator, method.name) catch return EmitError.OutOfMemory;

            const mangled_name = self.allocator.dupeZ(u8, name_buf.items) catch return EmitError.OutOfMemory;
            defer self.allocator.free(mangled_name);

            // Get the declared function
            const function = self.module.getNamedFunction(mangled_name) orelse return EmitError.InvalidAST;
            self.current_function = function;

            // Set up return type info
            if (method.return_type) |rt| {
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

            // Create entry block
            const entry = llvm.appendBasicBlock(self.ctx, function, "entry");
            self.builder.positionAtEnd(entry);
            self.has_terminator = false;

            // Clear named values for the new function
            self.named_values.clearRetainingCapacity();

            // Add parameters to named values
            for (method.params, 0..) |param, i| {
                const param_value = llvm.getParam(function, @intCast(i));
                const param_ty = try self.typeExprToLLVM(param.type_);

                const param_name = self.allocator.dupeZ(u8, param.name) catch return EmitError.OutOfMemory;
                defer self.allocator.free(param_name);

                const alloca = self.builder.buildAlloca(param_ty, param_name);
                _ = self.builder.buildStore(param_value, alloca);

                // Determine if parameter is signed
                const is_signed = self.isTypeExprSigned(param.type_);

                // For struct type parameters, record the struct type name for field resolution
                // For reference parameters (&Self or &mut Self), extract the inner struct name
                const param_struct_name: ?[]const u8 = switch (param.type_) {
                    .named => |n| n.name,
                    .reference => |ref| blk: {
                        // For self parameter, use the impl block's struct name
                        if (std.mem.eql(u8, param.name, "self")) {
                            break :blk struct_name;
                        }
                        // For other reference params, try to get inner type name
                        break :blk switch (ref.inner) {
                            .named => |n| n.name,
                            else => null,
                        };
                    },
                    else => null,
                };

                // Check if this is a reference parameter
                const is_ref = param.type_ == .reference;
                const ref_inner_type: ?llvm.TypeRef = if (is_ref) blk: {
                    break :blk try self.typeExprToLLVM(param.type_.reference.inner);
                } else null;

                self.named_values.put(param.name, .{
                    .value = alloca,
                    .is_alloca = true,
                    .ty = param_ty,
                    .is_signed = is_signed,
                    .struct_type_name = param_struct_name,
                    .is_reference = is_ref,
                    .reference_inner_type = ref_inner_type,
                }) catch return EmitError.OutOfMemory;
            }

            // Emit function body
            try self.pushScope(false);

            const result = try self.emitBlock(method.body.?);

            // Handle return
            if (!self.has_terminator) {
                if (method.return_type == null) {
                    self.emitDropsForReturn();
                    _ = self.builder.buildRetVoid();
                } else if (result) |val| {
                    if (self.current_return_type) |rt_info| {
                        if (rt_info.is_optional) {
                            self.emitDropsForReturn();
                            const wrapped = self.emitSome(val, rt_info.inner_type.?);
                            _ = self.builder.buildRet(wrapped);
                        } else {
                            self.emitDropsForReturn();
                            _ = self.builder.buildRet(val);
                        }
                    } else {
                        self.emitDropsForReturn();
                        _ = self.builder.buildRet(val);
                    }
                } else {
                    self.emitDropsForReturn();
                    _ = self.builder.buildRetVoid();
                }
            }

            self.popScope();
        }

        self.current_function = null;
    }

    fn emitFunction(self: *Emitter, func: *ast.FunctionDecl) EmitError!void {
        // Skip generic functions - they are handled via monomorphization
        if (func.type_params.len > 0) {
            return;
        }

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

            // For struct type parameters, record the struct type name for field resolution
            // For reference parameters, we need to track that the alloca contains a pointer
            const param_struct_name: ?[]const u8 = switch (param.type_) {
                .named => |n| n.name,
                .reference => |ref| switch (ref.inner) {
                    .named => |n| n.name,
                    else => null,
                },
                else => null,
            };

            // Check if this is a reference parameter
            const is_ref = param.type_ == .reference;
            const ref_inner_type: ?llvm.TypeRef = if (is_ref) blk: {
                // Get the LLVM type for the pointed-to struct
                break :blk try self.typeExprToLLVM(param.type_.reference.inner);
            } else null;

            self.named_values.put(param.name, .{
                .value = alloca,
                .is_alloca = true,
                .ty = param_ty,
                .is_signed = is_signed,
                .struct_type_name = param_struct_name,
                .is_reference = is_ref,
                .reference_inner_type = ref_inner_type,
            }) catch return EmitError.OutOfMemory;
        }

        // Emit function body
        if (func.body) |body| {
            // Push function scope for drop tracking
            try self.pushScope(false);

            const result = try self.emitBlock(body);

            // If block has a value and we haven't terminated, return it
            if (!self.has_terminator) {
                if (func.return_type == null) {
                    // Void function - always emit void return, ignore any expression value
                    // (e.g., if-without-else returns a placeholder that we discard)
                    self.emitDropsForReturn();
                    _ = self.builder.buildRetVoid();
                } else if (result) |val| {
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
                const is_signed = self.isTypeSigned(decl.type_);
                // Extract struct type name if this is a struct literal
                const struct_type_name = self.getStructTypeName(decl.value);
                // For Rc/Arc types, track the inner type for dereferencing
                const inner_type = self.tryGetRcInnerType(decl.value);
                const is_arc = self.isArcType(decl.value);
                // For closure types, extract return type and param types from annotation
                const closure_info = self.tryGetClosureTypeInfo(decl.type_);
                // Check if this is a string type
                const is_string = self.isTypeString(decl.type_);
                // Check if this is a heap-allocated String type
                const is_string_data = self.isTypeStringData(decl.type_);
                // Check if this is an array or slice type
                const is_array = self.isTypeArray(decl.type_);
                const array_info = self.getArrayTypeInfo(decl.type_);
                // Check if this is a List type
                const list_element_type = self.getListTypeInfo(decl.type_);
                self.named_values.put(decl.name, .{
                    .value = alloca,
                    .is_alloca = true,
                    .ty = ty,
                    .is_signed = is_signed,
                    .struct_type_name = struct_type_name,
                    .inner_type = inner_type,
                    .is_arc = is_arc,
                    .closure_return_type = if (closure_info) |ci| ci.return_type else null,
                    .closure_param_types = if (closure_info) |ci| ci.param_types else null,
                    .is_string = is_string,
                    .is_string_data = is_string_data,
                    .is_array = is_array,
                    .array_size = if (array_info) |ai| ai.size else null,
                    .array_element_type = if (array_info) |ai| ai.element_type else null,
                    .list_element_type = list_element_type,
                }) catch return EmitError.OutOfMemory;

                // Register Rc/Arc variables for automatic dropping
                if (inner_type) |it| {
                    try self.registerDroppable(decl.name, alloca, it, true, is_arc);
                }
            },
            .var_decl => |decl| {
                const value = try self.emitExpr(decl.value);
                const ty = try self.inferExprType(decl.value);
                const name = self.allocator.dupeZ(u8, decl.name) catch return EmitError.OutOfMemory;
                defer self.allocator.free(name);
                const alloca = self.builder.buildAlloca(ty, name);
                _ = self.builder.buildStore(value, alloca);
                const is_signed = self.isTypeSigned(decl.type_);
                // Extract struct type name if this is a struct literal
                const struct_type_name = self.getStructTypeName(decl.value);
                // For Rc/Arc types, track the inner type for dereferencing
                const inner_type = self.tryGetRcInnerType(decl.value);
                const is_arc = self.isArcType(decl.value);
                // For closure types, extract return type and param types from annotation
                const closure_info = self.tryGetClosureTypeInfo(decl.type_);
                // Check if this is a string type
                const is_string = self.isTypeString(decl.type_);
                // Check if this is a heap-allocated String type
                const is_string_data = self.isTypeStringData(decl.type_);
                // Check if this is an array or slice type
                const is_array = self.isTypeArray(decl.type_);
                const array_info = self.getArrayTypeInfo(decl.type_);
                // Check if this is a List type
                const list_element_type = self.getListTypeInfo(decl.type_);
                self.named_values.put(decl.name, .{
                    .value = alloca,
                    .is_alloca = true,
                    .ty = ty,
                    .is_signed = is_signed,
                    .struct_type_name = struct_type_name,
                    .inner_type = inner_type,
                    .is_arc = is_arc,
                    .closure_return_type = if (closure_info) |ci| ci.return_type else null,
                    .closure_param_types = if (closure_info) |ci| ci.param_types else null,
                    .is_string = is_string,
                    .is_string_data = is_string_data,
                    .is_array = is_array,
                    .array_size = if (array_info) |ai| ai.size else null,
                    .array_element_type = if (array_info) |ai| ai.element_type else null,
                    .list_element_type = list_element_type,
                }) catch return EmitError.OutOfMemory;

                // Register Rc/Arc variables for automatic dropping
                if (inner_type) |it| {
                    try self.registerDroppable(decl.name, alloca, it, true, is_arc);
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
            .if_stmt => |if_stmt| {
                try self.emitIfStmt(if_stmt);
            },
            .match_stmt => |match_stmt| {
                try self.emitMatchStmt(match_stmt);
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

        // Check if iterable is a range literal (fast path) or Range[T] type (iterator protocol)
        switch (loop.iterable) {
            .range => |range| {
                // Fast path for range literals: for i in start..end { body }
                try self.emitForLoopRangeLiteral(func, binding_name, range, loop.body);
            },
            else => {
                // Check if the iterable is a Range[T] type (e.g., a variable of type Range[i32])
                if (self.isRangeExpr(loop.iterable)) {
                    try self.emitForLoopRangeIterator(func, binding_name, loop.iterable, loop.body);
                } else if (self.isArrayExpr(loop.iterable)) {
                    // Array iteration: for x in arr { body }
                    try self.emitForLoopArray(func, binding_name, loop.iterable, loop.body);
                } else if (self.isListExpr(loop.iterable)) {
                    // List iteration: for x in list { body }
                    try self.emitForLoopList(func, binding_name, loop.iterable, loop.body);
                } else {
                    return EmitError.UnsupportedFeature;
                }
            },
        }
    }

    /// Emit for-loop for range literals using direct index iteration (fast path).
    fn emitForLoopRangeLiteral(
        self: *Emitter,
        func: llvm.ValueRef,
        binding_name: []const u8,
        range: *ast.Range,
        body: *ast.Block,
    ) EmitError!void {
        // For range literals: for i in start..end { body }
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
        _ = try self.emitBlock(body);
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

    /// Emit for-loop for Range[T] types using the iterator protocol.
    fn emitForLoopRangeIterator(
        self: *Emitter,
        func: llvm.ValueRef,
        binding_name: []const u8,
        iterable: ast.Expr,
        body: *ast.Block,
    ) EmitError!void {
        // For Range[T] iteration via iterator protocol:
        //   var iter = iterable  (or use iterable directly if already a mutable alloca)
        //   loop:
        //     let maybe = iter.next()
        //     if maybe.is_none() { break }
        //     let x = maybe!
        //     ... body ...
        //   end:

        const i32_type = llvm.Types.int32(self.ctx);
        const i1_type = llvm.Types.int1(self.ctx);

        // Build range struct type
        var range_fields = [_]llvm.TypeRef{
            i32_type, // start
            i32_type, // end
            i32_type, // current
            i1_type, // inclusive
        };
        const range_ty = llvm.Types.struct_(self.ctx, &range_fields, false);

        // Build Optional[i32] type: { i1 tag, i32 value }
        var opt_fields = [_]llvm.TypeRef{ i1_type, i32_type };
        const opt_ty = llvm.Types.struct_(self.ctx, &opt_fields, false);

        // Get pointer to the range (we need to mutate it via .next())
        const range_ptr = try self.getAddressOfRange(iterable, range_ty);

        // Allocate loop variable (element from iterator)
        const var_name = self.allocator.dupeZ(u8, binding_name) catch return EmitError.OutOfMemory;
        defer self.allocator.free(var_name);
        const elem_alloca = self.builder.buildAlloca(i32_type, var_name);

        // Add loop variable to scope
        self.named_values.put(binding_name, .{
            .value = elem_alloca,
            .is_alloca = true,
            .ty = i32_type,
            .is_signed = true,
        }) catch return EmitError.OutOfMemory;

        // Create blocks
        const loop_bb = llvm.appendBasicBlock(self.ctx, func, "foriter.loop");
        const body_bb = llvm.appendBasicBlock(self.ctx, func, "foriter.body");
        const end_bb = llvm.appendBasicBlock(self.ctx, func, "foriter.end");

        // Push loop context (continue goes to loop header, break goes to end)
        self.loop_stack.append(self.allocator, .{
            .continue_block = loop_bb,
            .break_block = end_bb,
        }) catch return EmitError.OutOfMemory;
        defer _ = self.loop_stack.pop();

        // Branch to loop
        _ = self.builder.buildBr(loop_bb);

        // Loop: call .next() and check for None
        self.builder.positionAtEnd(loop_bb);
        const maybe_val = try self.emitRangeNext(range_ptr);

        // Extract tag (is_some)
        const tag_alloca = self.builder.buildAlloca(opt_ty, "maybe.tmp");
        _ = self.builder.buildStore(maybe_val, tag_alloca);
        const tag_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_ty, tag_alloca, 0, "tag.ptr");
        const is_some = self.builder.buildLoad(i1_type, tag_ptr, "is_some");

        // Branch: if is_some goto body, else goto end
        _ = self.builder.buildCondBr(is_some, body_bb, end_bb);

        // Body: extract value and run body
        self.builder.positionAtEnd(body_bb);
        self.has_terminator = false;

        // Extract the value from Some
        const val_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_ty, tag_alloca, 1, "val.ptr");
        const elem_val = self.builder.buildLoad(i32_type, val_ptr, "elem");
        _ = self.builder.buildStore(elem_val, elem_alloca);

        // Emit body with loop scope for drop tracking
        try self.pushScope(true); // is_loop = true
        _ = try self.emitBlock(body);
        if (!self.has_terminator) {
            // Emit drops for variables declared in this iteration before continuing
            self.popScope();
            _ = self.builder.buildBr(loop_bb);
        } else {
            // Terminator already emitted (break/continue/return)
            if (self.scope_stack.pop()) |scope| {
                var s = scope;
                s.droppables.deinit(self.allocator);
            }
        }

        // Continue after loop
        self.builder.positionAtEnd(end_bb);
        self.has_terminator = false;

        // Remove loop variable from scope
        _ = self.named_values.remove(binding_name);
    }

    /// Get address of a Range expression. If it's an identifier, return its alloca.
    /// If it's a range literal, allocate it and return the alloca.
    fn getAddressOfRange(self: *Emitter, expr: ast.Expr, range_ty: llvm.TypeRef) EmitError!llvm.ValueRef {
        switch (expr) {
            .identifier => |id| {
                if (self.named_values.get(id.name)) |local| {
                    if (local.is_alloca) {
                        return local.value;
                    }
                }
                return EmitError.InvalidAST;
            },
            .range => {
                // Emit the range and store it in an alloca
                const range_val = try self.emitExpr(expr);
                const alloca = self.builder.buildAlloca(range_ty, "range.iter");
                _ = self.builder.buildStore(range_val, alloca);
                return alloca;
            },
            else => return EmitError.InvalidAST,
        }
    }

    /// Emit for-loop for arrays using direct index iteration.
    fn emitForLoopArray(
        self: *Emitter,
        func: llvm.ValueRef,
        binding_name: []const u8,
        iterable: ast.Expr,
        body: *ast.Block,
    ) EmitError!void {
        // For array iteration: for x in arr { body }
        // We emit this as:
        //   %idx = alloca i32
        //   store 0, %idx
        //   br cond
        // cond:
        //   %cur = load %idx
        //   %cmp = icmp slt %cur, len
        //   br %cmp, body, end
        // body:
        //   %elem_ptr = gep arr, 0, %cur
        //   %elem = load %elem_ptr
        //   ... loop body using %elem as binding ...
        //   %next = add %cur, 1
        //   store %next, %idx
        //   br cond
        // end:

        // Get the array pointer and its type
        const array_info = try self.getArrayInfo(iterable);
        const array_ptr = array_info.ptr;
        const array_type = array_info.ty;
        const array_len = array_info.len;
        const elem_type = llvm.c.LLVMGetElementType(array_type);

        const i32_type = llvm.Types.int32(self.ctx);

        // Allocate index counter
        const idx_alloca = self.builder.buildAlloca(i32_type, "for.idx");
        _ = self.builder.buildStore(llvm.Const.int32(self.ctx, 0), idx_alloca);

        // Allocate loop variable (element binding)
        const var_name = self.allocator.dupeZ(u8, binding_name) catch return EmitError.OutOfMemory;
        defer self.allocator.free(var_name);
        const elem_alloca = self.builder.buildAlloca(elem_type, var_name);

        // Determine if element type is signed (for integer types)
        const is_signed = self.isSignedType(elem_type);

        // Add loop variable to scope
        self.named_values.put(binding_name, .{
            .value = elem_alloca,
            .is_alloca = true,
            .ty = elem_type,
            .is_signed = is_signed,
        }) catch return EmitError.OutOfMemory;

        // Create blocks
        const cond_bb = llvm.appendBasicBlock(self.ctx, func, "forarr.cond");
        const body_bb = llvm.appendBasicBlock(self.ctx, func, "forarr.body");
        const incr_bb = llvm.appendBasicBlock(self.ctx, func, "forarr.incr");
        const end_bb = llvm.appendBasicBlock(self.ctx, func, "forarr.end");

        // Push loop context (continue goes to increment, break goes to end)
        self.loop_stack.append(self.allocator, .{
            .continue_block = incr_bb,
            .break_block = end_bb,
        }) catch return EmitError.OutOfMemory;
        defer _ = self.loop_stack.pop();

        // Branch to condition
        _ = self.builder.buildBr(cond_bb);

        // Emit condition: idx < len
        self.builder.positionAtEnd(cond_bb);
        const cur_idx = self.builder.buildLoad(i32_type, idx_alloca, "for.idx.cur");
        const len_val = llvm.Const.int32(self.ctx, @intCast(array_len));
        const cmp = self.builder.buildICmp(llvm.c.LLVMIntSLT, cur_idx, len_val, "for.cmp");
        _ = self.builder.buildCondBr(cmp, body_bb, end_bb);

        // Emit body
        self.builder.positionAtEnd(body_bb);
        self.has_terminator = false;

        // Load current element and store to binding
        var indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            cur_idx,
        };
        const elem_ptr = self.builder.buildGEP(array_type, array_ptr, &indices, "elem.ptr");
        const elem_val = self.builder.buildLoad(elem_type, elem_ptr, "elem.val");
        _ = self.builder.buildStore(elem_val, elem_alloca);

        // Emit body with loop scope for drop tracking
        try self.pushScope(true); // is_loop = true
        _ = try self.emitBlock(body);
        if (!self.has_terminator) {
            // Emit drops for variables declared in this iteration before continuing
            self.popScope();
            _ = self.builder.buildBr(incr_bb);
        } else {
            // Terminator already emitted (break/continue/return)
            if (self.scope_stack.pop()) |scope| {
                var s = scope;
                s.droppables.deinit(self.allocator);
            }
        }

        // Emit increment
        self.builder.positionAtEnd(incr_bb);
        const cur_idx2 = self.builder.buildLoad(i32_type, idx_alloca, "for.idx.cur2");
        const one = llvm.Const.int32(self.ctx, 1);
        const next_idx = self.builder.buildAdd(cur_idx2, one, "for.idx.next");
        _ = self.builder.buildStore(next_idx, idx_alloca);
        _ = self.builder.buildBr(cond_bb);

        // Continue after loop
        self.builder.positionAtEnd(end_bb);
        self.has_terminator = false;

        // Remove loop variable from scope
        _ = self.named_values.remove(binding_name);
    }

    /// Emit for-loop for List[T] types using index iteration.
    /// List layout: { ptr: *T, len: i32, capacity: i32 }
    fn emitForLoopList(
        self: *Emitter,
        func: llvm.ValueRef,
        binding_name: []const u8,
        iterable: ast.Expr,
        body: *ast.Block,
    ) EmitError!void {
        // Get the list alloca and element type
        const list_info = try self.getListInfo(iterable);
        const list_alloca = list_info.alloca;
        const element_type = list_info.element_type;
        const element_llvm_type = self.typeToLLVM(element_type);

        const i32_type = llvm.Types.int32(self.ctx);
        const list_type = self.getListStructType();

        // Allocate index counter
        const idx_alloca = self.builder.buildAlloca(i32_type, "forlist.idx");
        _ = self.builder.buildStore(llvm.Const.int32(self.ctx, 0), idx_alloca);

        // Allocate loop variable (element binding)
        const var_name = self.allocator.dupeZ(u8, binding_name) catch return EmitError.OutOfMemory;
        defer self.allocator.free(var_name);
        const elem_alloca = self.builder.buildAlloca(element_llvm_type, var_name);

        // Determine if element type is signed (for integer types)
        const is_signed = self.isSignedType(element_llvm_type);

        // Add loop variable to scope
        self.named_values.put(binding_name, .{
            .value = elem_alloca,
            .is_alloca = true,
            .ty = element_llvm_type,
            .is_signed = is_signed,
        }) catch return EmitError.OutOfMemory;

        // Create blocks
        const cond_bb = llvm.appendBasicBlock(self.ctx, func, "forlist.cond");
        const body_bb = llvm.appendBasicBlock(self.ctx, func, "forlist.body");
        const incr_bb = llvm.appendBasicBlock(self.ctx, func, "forlist.incr");
        const end_bb = llvm.appendBasicBlock(self.ctx, func, "forlist.end");

        // Push loop context (continue goes to increment, break goes to end)
        self.loop_stack.append(self.allocator, .{
            .continue_block = incr_bb,
            .break_block = end_bb,
        }) catch return EmitError.OutOfMemory;
        defer _ = self.loop_stack.pop();

        // Branch to condition
        _ = self.builder.buildBr(cond_bb);

        // Emit condition: idx < len
        self.builder.positionAtEnd(cond_bb);
        const cur_idx = self.builder.buildLoad(i32_type, idx_alloca, "forlist.idx.cur");
        // Load current length from list (it may change during iteration, but we snapshot it)
        const len_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, list_type, list_alloca, 1, "forlist.len_ptr");
        const list_len = self.builder.buildLoad(i32_type, len_ptr, "forlist.len");
        const cmp = self.builder.buildICmp(llvm.c.LLVMIntSLT, cur_idx, list_len, "forlist.cmp");
        _ = self.builder.buildCondBr(cmp, body_bb, end_bb);

        // Emit body
        self.builder.positionAtEnd(body_bb);
        self.has_terminator = false;

        // Load data pointer from list
        const ptr_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, list_type, list_alloca, 0, "forlist.ptr_ptr");
        const data_ptr = self.builder.buildLoad(llvm.Types.pointer(self.ctx), ptr_ptr, "forlist.data_ptr");

        // Load current element: data_ptr[idx]
        var gep_indices = [_]llvm.ValueRef{cur_idx};
        const elem_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, element_llvm_type, data_ptr, &gep_indices, 1, "forlist.elem.ptr");
        const elem_val = self.builder.buildLoad(element_llvm_type, elem_ptr, "forlist.elem.val");
        _ = self.builder.buildStore(elem_val, elem_alloca);

        // Emit body with loop scope for drop tracking
        try self.pushScope(true); // is_loop = true
        _ = try self.emitBlock(body);
        if (!self.has_terminator) {
            // Emit drops for variables declared in this iteration before continuing
            self.popScope();
            _ = self.builder.buildBr(incr_bb);
        } else {
            // Terminator already emitted (break/continue/return)
            if (self.scope_stack.pop()) |scope| {
                var s = scope;
                s.droppables.deinit(self.allocator);
            }
        }

        // Emit increment
        self.builder.positionAtEnd(incr_bb);
        const cur_idx2 = self.builder.buildLoad(i32_type, idx_alloca, "forlist.idx.cur2");
        const one = llvm.Const.int32(self.ctx, 1);
        const next_idx = self.builder.buildAdd(cur_idx2, one, "forlist.idx.next");
        _ = self.builder.buildStore(next_idx, idx_alloca);
        _ = self.builder.buildBr(cond_bb);

        // Continue after loop
        self.builder.positionAtEnd(end_bb);
        self.has_terminator = false;

        // Remove loop variable from scope
        _ = self.named_values.remove(binding_name);
    }

    /// Get information about a List expression: its alloca pointer and element type.
    const ListInfo = struct {
        alloca: llvm.ValueRef,
        element_type: types.Type,
    };

    fn getListInfo(self: *Emitter, expr: ast.Expr) EmitError!ListInfo {
        switch (expr) {
            .identifier => |id| {
                if (self.named_values.get(id.name)) |local| {
                    if (local.list_element_type) |elem_type| {
                        return ListInfo{
                            .alloca = local.value,
                            .element_type = elem_type,
                        };
                    }
                }
            },
            else => {},
        }
        // Fallback: use type checker
        if (self.type_checker) |tc| {
            const tc_mut = @constCast(tc);
            const expr_type = tc_mut.checkExpr(expr);
            if (expr_type == .list) {
                // Emit the expression and hope it's an alloca
                const list_val = try self.emitExpr(expr);
                return ListInfo{
                    .alloca = list_val,
                    .element_type = expr_type.list.element,
                };
            }
        }
        return EmitError.InvalidAST;
    }

    /// Get information about an array expression: its alloca pointer, LLVM type, and length.
    const ArrayInfo = struct {
        ptr: llvm.ValueRef,
        ty: llvm.TypeRef,
        len: u64,
    };

    fn getArrayInfo(self: *Emitter, expr: ast.Expr) EmitError!ArrayInfo {
        switch (expr) {
            .identifier => |id| {
                if (self.named_values.get(id.name)) |local| {
                    if (local.is_alloca and local.is_array) {
                        // For arrays, local.ty is the LLVM array type
                        const array_type = local.ty;
                        const array_len = if (local.array_size) |sz| sz else llvm.Types.getArrayLength(array_type);
                        return ArrayInfo{
                            .ptr = local.value,
                            .ty = array_type,
                            .len = array_len,
                        };
                    }
                }
                return EmitError.InvalidAST;
            },
            .array_literal => |arr| {
                // Emit the array literal and store it in an alloca
                const array_val = try self.emitExpr(expr);
                const array_type = llvm.typeOf(array_val);
                const array_len = arr.elements.len;
                const alloca = self.builder.buildAlloca(array_type, "arr.iter");
                _ = self.builder.buildStore(array_val, alloca);
                return ArrayInfo{
                    .ptr = alloca,
                    .ty = array_type,
                    .len = array_len,
                };
            },
            else => return EmitError.InvalidAST,
        }
    }

    /// Check if an LLVM type is a signed integer type.
    fn isSignedType(self: *Emitter, ty: llvm.TypeRef) bool {
        _ = self;
        // In LLVM, integer signedness is determined by the operations, not the type.
        // We default to signed for integer types used in loops.
        const kind = llvm.getTypeKind(ty);
        return kind == llvm.c.LLVMIntegerTypeKind;
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
            .type_cast => |tc| try self.emitTypeCast(tc),
            .enum_literal => |e| try self.emitEnumLiteral(e),
            .interpolated_string => |is| try self.emitInterpolatedString(is),
            // Comptime expressions
            .builtin_call => |bc| try self.emitBuiltinCall(bc),
            .comptime_block => |cb| try self.emitComptimeBlock(cb),
            // Range expression
            .range => |r| try self.emitRangeLiteral(r),
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

    /// Emit a range literal (e.g., 0..10 or 0..=10)
    /// Range layout: { start: T, end: T, current: T, inclusive: i1 }
    fn emitRangeLiteral(self: *Emitter, range: *ast.Range) EmitError!llvm.ValueRef {
        // Determine element type from start or end expression, default to i32
        const elem_ty = llvm.Types.int32(self.ctx);

        // Get start value (default to 0 if not specified)
        const start_val = if (range.start) |s|
            try self.emitExpr(s)
        else
            llvm.Const.int32(self.ctx, 0);

        // Get end value (required)
        const end_val = if (range.end) |e|
            try self.emitExpr(e)
        else
            return EmitError.UnsupportedFeature;

        // Create the Range struct type
        var fields = [_]llvm.TypeRef{
            elem_ty, // start
            elem_ty, // end
            elem_ty, // current
            llvm.Types.int1(self.ctx), // inclusive
        };
        const range_ty = llvm.Types.struct_(self.ctx, &fields, false);

        // Allocate space for the range struct
        const range_alloca = self.builder.buildAlloca(range_ty, "range.tmp");

        // Store start
        const start_gep = llvm.c.LLVMBuildStructGEP2(self.builder.ref, range_ty, range_alloca, 0, "range.start.ptr");
        _ = self.builder.buildStore(start_val, start_gep);

        // Store end
        const end_gep = llvm.c.LLVMBuildStructGEP2(self.builder.ref, range_ty, range_alloca, 1, "range.end.ptr");
        _ = self.builder.buildStore(end_val, end_gep);

        // Store current (initially same as start)
        const current_gep = llvm.c.LLVMBuildStructGEP2(self.builder.ref, range_ty, range_alloca, 2, "range.current.ptr");
        _ = self.builder.buildStore(start_val, current_gep);

        // Store inclusive flag
        const inclusive_gep = llvm.c.LLVMBuildStructGEP2(self.builder.ref, range_ty, range_alloca, 3, "range.inclusive.ptr");
        const inclusive_val = llvm.Const.int1(self.ctx, range.inclusive);
        _ = self.builder.buildStore(inclusive_val, inclusive_gep);

        // Load and return the range struct
        return self.builder.buildLoad(range_ty, range_alloca, "range");
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

        var lhs = try self.emitExpr(bin.left);
        var rhs = try self.emitExpr(bin.right);

        // Check if either operand is floating-point
        const lhs_ty = llvm.typeOf(lhs);
        const rhs_ty = llvm.typeOf(rhs);
        const lhs_is_float = llvm.getTypeKind(lhs_ty) == llvm.c.LLVMFloatTypeKind or
            llvm.getTypeKind(lhs_ty) == llvm.c.LLVMDoubleTypeKind;
        const rhs_is_float = llvm.getTypeKind(rhs_ty) == llvm.c.LLVMFloatTypeKind or
            llvm.getTypeKind(rhs_ty) == llvm.c.LLVMDoubleTypeKind;
        const is_float = lhs_is_float or rhs_is_float;

        // If mixed types, promote integer to float
        if (is_float and !lhs_is_float) {
            // lhs is int, rhs is float - convert lhs to float
            lhs = self.builder.buildSIToFP(lhs, rhs_ty, "promote_lhs");
        } else if (is_float and !rhs_is_float) {
            // lhs is float, rhs is int - convert rhs to float
            rhs = self.builder.buildSIToFP(rhs, lhs_ty, "promote_rhs");
        }

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
        // Handle array index assignment: arr[i] = value
        if (bin.left == .index) {
            return self.emitIndexAssignment(bin);
        }

        // Handle struct field assignment: s.field = value
        if (bin.left == .field) {
            return self.emitFieldAssignment(bin);
        }

        // Get the target variable for simple identifier assignment
        const target_id = switch (bin.left) {
            .identifier => |id| id,
            else => return EmitError.UnsupportedFeature,
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

    /// Emit array index assignment: arr[i] = value
    fn emitIndexAssignment(self: *Emitter, bin: *ast.Binary) EmitError!llvm.ValueRef {
        const idx = bin.left.index;

        // For simple assignment, we need the array variable's alloca
        // The array must be a variable (identifier) for assignment to work
        const arr_id = switch (idx.object) {
            .identifier => |id| id,
            else => return EmitError.UnsupportedFeature, // Only support arr[i] = v, not (expr)[i] = v
        };

        const local = self.named_values.get(arr_id.name) orelse
            return EmitError.InvalidAST;

        if (!local.is_alloca) {
            return EmitError.InvalidAST; // Can only assign to allocas
        }

        // Get the array type from the alloca
        const arr_type = local.ty;
        const arr_type_kind = llvm.getTypeKind(arr_type);

        if (arr_type_kind != llvm.c.LLVMArrayTypeKind) {
            return EmitError.UnsupportedFeature; // Only support fixed-size arrays for now
        }

        // Emit the index expression
        const index_val = try self.emitExpr(idx.index);

        // Get array length for bounds checking
        const array_len = llvm.Types.getArrayLength(arr_type);
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

        // Evaluate the right-hand side
        const rhs = try self.emitExpr(bin.right);

        // GEP to get pointer to array element
        var indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            index_val,
        };
        const elem_ptr = self.builder.buildGEP(arr_type, local.value, &indices, "elem.ptr");

        // For compound assignment, need to load current value and operate
        const value = switch (bin.op) {
            .assign => rhs,
            .add_assign => blk: {
                const elem_type = llvm.c.LLVMGetElementType(arr_type);
                const lhs = self.builder.buildLoad(elem_type, elem_ptr, "loadtmp");
                break :blk self.builder.buildAdd(lhs, rhs, "addtmp");
            },
            .sub_assign => blk: {
                const elem_type = llvm.c.LLVMGetElementType(arr_type);
                const lhs = self.builder.buildLoad(elem_type, elem_ptr, "loadtmp");
                break :blk self.builder.buildSub(lhs, rhs, "subtmp");
            },
            .mul_assign => blk: {
                const elem_type = llvm.c.LLVMGetElementType(arr_type);
                const lhs = self.builder.buildLoad(elem_type, elem_ptr, "loadtmp");
                break :blk self.builder.buildMul(lhs, rhs, "multmp");
            },
            .div_assign => blk: {
                const elem_type = llvm.c.LLVMGetElementType(arr_type);
                const lhs = self.builder.buildLoad(elem_type, elem_ptr, "loadtmp");
                break :blk self.builder.buildSDiv(lhs, rhs, "divtmp");
            },
            .mod_assign => blk: {
                const elem_type = llvm.c.LLVMGetElementType(arr_type);
                const lhs = self.builder.buildLoad(elem_type, elem_ptr, "loadtmp");
                break :blk self.builder.buildSRem(lhs, rhs, "modtmp");
            },
            else => return EmitError.InvalidAST,
        };

        // Store the value to the element
        _ = self.builder.buildStore(value, elem_ptr);

        // Return the stored value
        return value;
    }

    /// Emit struct field assignment: s.field = value
    fn emitFieldAssignment(self: *Emitter, bin: *ast.Binary) EmitError!llvm.ValueRef {
        const field = bin.left.field;

        // The struct must be a variable (identifier) for assignment to work
        const struct_id = switch (field.object) {
            .identifier => |id| id,
            else => return EmitError.UnsupportedFeature,
        };

        const local = self.named_values.get(struct_id.name) orelse
            return EmitError.InvalidAST;

        if (!local.is_alloca) {
            return EmitError.InvalidAST;
        }

        // Get struct type info
        const struct_name = local.struct_type_name orelse return EmitError.InvalidAST;
        const struct_info = self.struct_types.get(struct_name) orelse return EmitError.InvalidAST;

        // Find the field index
        var field_idx: ?u32 = null;
        for (struct_info.field_names, 0..) |name, i| {
            if (std.mem.eql(u8, name, field.field_name)) {
                field_idx = @intCast(i);
                break;
            }
        }
        const idx = field_idx orelse return EmitError.InvalidAST;

        // Evaluate the right-hand side
        const rhs = try self.emitExpr(bin.right);

        // For reference parameters, load the pointer first, then GEP into it
        // For regular struct parameters, GEP directly from the alloca
        const base_ptr = if (local.is_reference)
            self.builder.buildLoad(local.ty, local.value, "ref.load")
        else
            local.value;

        const gep_type = if (local.is_reference)
            local.reference_inner_type.?
        else
            local.ty;

        // GEP to get pointer to struct field
        var indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, @intCast(idx)),
        };
        const field_ptr = self.builder.buildGEP(gep_type, base_ptr, &indices, "field.ptr");

        // For compound assignment, need to load current value and operate
        const field_type = llvm.c.LLVMStructGetTypeAtIndex(gep_type, idx);
        const value = switch (bin.op) {
            .assign => rhs,
            .add_assign => blk: {
                const lhs = self.builder.buildLoad(field_type, field_ptr, "loadtmp");
                break :blk self.builder.buildAdd(lhs, rhs, "addtmp");
            },
            .sub_assign => blk: {
                const lhs = self.builder.buildLoad(field_type, field_ptr, "loadtmp");
                break :blk self.builder.buildSub(lhs, rhs, "subtmp");
            },
            .mul_assign => blk: {
                const lhs = self.builder.buildLoad(field_type, field_ptr, "loadtmp");
                break :blk self.builder.buildMul(lhs, rhs, "multmp");
            },
            .div_assign => blk: {
                const lhs = self.builder.buildLoad(field_type, field_ptr, "loadtmp");
                break :blk self.builder.buildSDiv(lhs, rhs, "divtmp");
            },
            .mod_assign => blk: {
                const lhs = self.builder.buildLoad(field_type, field_ptr, "loadtmp");
                break :blk self.builder.buildSRem(lhs, rhs, "modtmp");
            },
            else => return EmitError.InvalidAST,
        };

        // Store the value to the field
        _ = self.builder.buildStore(value, field_ptr);

        // Return the stored value
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
                const op_ty = llvm.typeOf(operand);
                const type_kind = llvm.getTypeKind(op_ty);
                if (type_kind == llvm.c.LLVMFloatTypeKind or type_kind == llvm.c.LLVMDoubleTypeKind) {
                    return self.builder.buildFNeg(operand, "fnegtmp");
                }
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

    fn emitTypeCast(self: *Emitter, cast: *ast.TypeCast) EmitError!llvm.ValueRef {
        const value = try self.emitExpr(cast.expr);
        const src_type = llvm.typeOf(value);
        const src_kind = llvm.getTypeKind(src_type);

        // Get target type name
        const target_type_name: []const u8 = switch (cast.target_type) {
            .named => |n| n.name,
            else => return value, // Non-named types pass through
        };

        // Get target LLVM type
        const dest_type = self.getTypeByName(target_type_name) orelse return value;
        const dest_kind = llvm.getTypeKind(dest_type);

        // Same type - no conversion needed
        if (src_type == dest_type) {
            return value;
        }

        // Integer to Float
        if ((src_kind == llvm.c.LLVMIntegerTypeKind) and
            (dest_kind == llvm.c.LLVMFloatTypeKind or dest_kind == llvm.c.LLVMDoubleTypeKind))
        {
            // Check if source is signed (i8, i16, i32, i64, isize) or unsigned (u8, u16, u32, u64, usize)
            const is_signed = self.isSignedIntType(cast.expr);
            if (is_signed) {
                return self.builder.buildSIToFP(value, dest_type, "sitofp");
            } else {
                return self.builder.buildUIToFP(value, dest_type, "uitofp");
            }
        }

        // Float to Integer
        if ((src_kind == llvm.c.LLVMFloatTypeKind or src_kind == llvm.c.LLVMDoubleTypeKind) and
            dest_kind == llvm.c.LLVMIntegerTypeKind)
        {
            const is_signed = self.isSignedTypeName(target_type_name);
            if (is_signed) {
                return self.builder.buildFPToSI(value, dest_type, "fptosi");
            } else {
                return self.builder.buildFPToUI(value, dest_type, "fptoui");
            }
        }

        // Float to Float (f32 <-> f64)
        if ((src_kind == llvm.c.LLVMFloatTypeKind or src_kind == llvm.c.LLVMDoubleTypeKind) and
            (dest_kind == llvm.c.LLVMFloatTypeKind or dest_kind == llvm.c.LLVMDoubleTypeKind))
        {
            const src_bits = if (src_kind == llvm.c.LLVMFloatTypeKind) @as(u32, 32) else @as(u32, 64);
            const dest_bits = if (dest_kind == llvm.c.LLVMFloatTypeKind) @as(u32, 32) else @as(u32, 64);

            if (dest_bits > src_bits) {
                return self.builder.buildFPExt(value, dest_type, "fpext");
            } else if (dest_bits < src_bits) {
                return self.builder.buildFPTrunc(value, dest_type, "fptrunc");
            }
            return value;
        }

        // Integer to Integer
        if (src_kind == llvm.c.LLVMIntegerTypeKind and dest_kind == llvm.c.LLVMIntegerTypeKind) {
            const src_bits = llvm.getIntTypeWidth(src_type);
            const dest_bits = llvm.getIntTypeWidth(dest_type);

            if (dest_bits > src_bits) {
                // Widening
                const is_signed = self.isSignedIntType(cast.expr);
                if (is_signed) {
                    return self.builder.buildSExt(value, dest_type, "sext");
                } else {
                    return self.builder.buildZExt(value, dest_type, "zext");
                }
            } else if (dest_bits < src_bits) {
                // Narrowing
                return self.builder.buildTrunc(value, dest_type, "trunc");
            }
            return value;
        }

        // Fallback: return value unchanged
        return value;
    }

    fn getTypeByName(self: *Emitter, name: []const u8) ?llvm.TypeRef {
        if (std.mem.eql(u8, name, "i8")) return llvm.Types.int8(self.ctx);
        if (std.mem.eql(u8, name, "i16")) return llvm.Types.int16(self.ctx);
        if (std.mem.eql(u8, name, "i32")) return llvm.Types.int32(self.ctx);
        if (std.mem.eql(u8, name, "i64")) return llvm.Types.int64(self.ctx);
        if (std.mem.eql(u8, name, "i128")) return llvm.Types.int128(self.ctx);
        if (std.mem.eql(u8, name, "isize")) return llvm.Types.int64(self.ctx); // Assume 64-bit
        if (std.mem.eql(u8, name, "u8")) return llvm.Types.int8(self.ctx);
        if (std.mem.eql(u8, name, "u16")) return llvm.Types.int16(self.ctx);
        if (std.mem.eql(u8, name, "u32")) return llvm.Types.int32(self.ctx);
        if (std.mem.eql(u8, name, "u64")) return llvm.Types.int64(self.ctx);
        if (std.mem.eql(u8, name, "u128")) return llvm.Types.int128(self.ctx);
        if (std.mem.eql(u8, name, "usize")) return llvm.Types.int64(self.ctx); // Assume 64-bit
        if (std.mem.eql(u8, name, "f32")) return llvm.Types.float32(self.ctx);
        if (std.mem.eql(u8, name, "f64")) return llvm.Types.float64(self.ctx);
        if (std.mem.eql(u8, name, "bool")) return llvm.Types.int1(self.ctx);
        return null;
    }

    fn isSignedTypeName(_: *Emitter, name: []const u8) bool {
        return std.mem.startsWith(u8, name, "i") and !std.mem.eql(u8, name, "isize") or
            std.mem.eql(u8, name, "isize");
    }

    fn isSignedIntType(self: *Emitter, expr: ast.Expr) bool {
        _ = self;
        // Try to determine signedness from expression
        // For literals, check if the value is negative
        if (expr == .literal) {
            const lit = expr.literal;
            if (lit.kind == .int) {
                // Negative values are definitely signed
                if (lit.kind.int < 0) return true;
                // Default: integers are signed (i32) in Klar
                return true;
            }
        }
        // For identifiers, we'd need type info from the checker
        // Default to signed for safety (i32 is the default int type)
        return true;
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

    /// Try to get the inner type of an Rc, Arc, or Cell expression.
    /// Returns null if the expression is not an Rc, Arc, or Cell type.
    fn tryGetRcInnerType(self: *Emitter, expr: ast.Expr) ?llvm.TypeRef {
        // Check for Rc.new(value), Arc.new(value), or Cell.new(value) - inner type is type of value
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
                // Arc.new(value)
                if (std.mem.eql(u8, obj_name, "Arc") and std.mem.eql(u8, method.method_name, "new")) {
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

        // Check for identifier that is already an Rc, Arc, or Cell
        if (expr == .identifier) {
            const id = expr.identifier;
            if (self.named_values.get(id.name)) |local| {
                return local.inner_type;
            }
        }

        return null;
    }

    /// Check if an expression represents an Arc type.
    fn isArcType(self: *Emitter, expr: ast.Expr) bool {
        if (expr == .method_call) {
            const method = expr.method_call;
            if (method.object == .identifier) {
                const obj_name = method.object.identifier.name;
                if (std.mem.eql(u8, obj_name, "Arc") and std.mem.eql(u8, method.method_name, "new")) {
                    return true;
                }
            }
            // For clone(), check the receiver
            if (std.mem.eql(u8, method.method_name, "clone")) {
                return self.isArcType(method.object);
            }
        }

        // Check for identifier that is already an Arc
        if (expr == .identifier) {
            const id = expr.identifier;
            if (self.named_values.get(id.name)) |local| {
                return local.is_arc;
            }
        }

        return false;
    }

    fn isRcType(self: *Emitter, expr: ast.Expr) bool {
        if (expr == .method_call) {
            const method = expr.method_call;
            if (method.object == .identifier) {
                const obj_name = method.object.identifier.name;
                if (std.mem.eql(u8, obj_name, "Rc") and std.mem.eql(u8, method.method_name, "new")) {
                    return true;
                }
            }
            // For clone(), check the receiver
            if (std.mem.eql(u8, method.method_name, "clone")) {
                return self.isRcType(method.object);
            }
        }

        // Check for identifier that is already an Rc (has inner_type but is not Arc)
        if (expr == .identifier) {
            const id = expr.identifier;
            if (self.named_values.get(id.name)) |local| {
                return local.inner_type != null and !local.is_arc;
            }
        }

        return false;
    }

    /// Information about a closure type extracted from its TypeExpr annotation.
    const ClosureTypeInfo2 = struct {
        return_type: llvm.TypeRef,
        param_types: ?[]const llvm.TypeRef,
    };

    /// Try to extract closure type information from a TypeExpr.
    /// Returns null if the type is not a function type.
    /// Note: The param_types slice is allocated from the emitter's allocator.
    /// The memory is intentionally not freed during the emitter's lifetime as a simplification.
    fn tryGetClosureTypeInfo(self: *Emitter, type_expr: ast.TypeExpr) ?ClosureTypeInfo2 {
        if (type_expr != .function) return null;

        const fn_type = type_expr.function;

        // Convert return type to LLVM
        const return_type = self.typeExprToLLVM(fn_type.return_type) catch return null;

        // For param types, we only need them if we're actually going to call the closure
        // with the typed call path. Since we primarily need the return type, and param
        // types can be inferred from the actual call arguments, we can skip allocating
        // param_types to avoid the memory management complexity.
        // The emitter will use null param_types and fall back to inferring from args.

        return ClosureTypeInfo2{
            .return_type = return_type,
            .param_types = null, // Simplified: param types inferred from call args
        };
    }

    fn emitCall(self: *Emitter, call: *ast.Call) EmitError!llvm.ValueRef {
        // Check if this is a comptime function call with a precomputed value
        if (self.type_checker) |tc| {
            if (tc.comptime_call_values.get(call)) |comptime_value| {
                // Emit the precomputed constant value
                return self.emitComptimeValue(comptime_value);
            }
        }

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
            } else if (std.mem.eql(u8, name, "assert_eq")) {
                return self.emitAssertEq(call.args);
            } else if (std.mem.eql(u8, name, "dbg")) {
                return self.emitDbg(call.args);
            } else if (std.mem.eql(u8, name, "type_name")) {
                return self.emitTypeName(call.args);
            } else if (std.mem.eql(u8, name, "len")) {
                return self.emitLen(call.args);
            } else if (std.mem.eql(u8, name, "Ok")) {
                // Result::Ok(value) constructor
                return self.emitOkCall(call.args);
            } else if (std.mem.eql(u8, name, "Err")) {
                // Result::Err(error) constructor
                return self.emitErrCall(call.args);
            }
        }

        // Check if this is a monomorphized generic function call
        if (self.type_checker) |checker| {
            if (checker.getCallResolution(call)) |mangled_name| {
                const mangled_z = self.allocator.dupeZ(u8, mangled_name) catch return EmitError.OutOfMemory;
                defer self.allocator.free(mangled_z);

                if (self.module.getNamedFunction(mangled_z)) |func| {
                    // Call the monomorphized function
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
            }
        }

        // Try to find as a module-level function
        if (func_name) |name| {
            // First, try to find the function directly by name
            var lookup_name = name;

            // Check if this is an aliased import - if so, use the original name
            if (self.type_checker) |tc| {
                if (tc.lookupSymbol(name)) |sym| {
                    if (sym.original_name) |orig| {
                        lookup_name = orig;
                    }
                }
            }

            const name_z = self.allocator.dupeZ(u8, lookup_name) catch return EmitError.OutOfMemory;
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
                return self.emitClosureCallTyped(closure_value, call.args, local.closure_return_type, local.closure_param_types);
            }
        }

        // Generic callee - evaluate it and call as closure
        const callee_value = try self.emitExpr(call.callee);
        return self.emitClosureCallTyped(callee_value, call.args, null, null);
    }

    /// Emit a call to a closure value with optional type information.
    /// Closure struct is { fn_ptr: ptr, env_ptr: ptr }
    /// If return_type and param_types are provided, they are used for the function signature.
    /// Otherwise, defaults to i32 return type and i32 parameters.
    fn emitClosureCallTyped(
        self: *Emitter,
        closure_value: llvm.ValueRef,
        args: []const ast.Expr,
        return_type: ?llvm.TypeRef,
        param_types_info: ?[]const llvm.TypeRef,
    ) EmitError!llvm.ValueRef {
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
        // Use provided types if available, otherwise default to i32
        var param_types = std.ArrayListUnmanaged(llvm.TypeRef){};
        defer param_types.deinit(self.allocator);

        // First param is always the environment pointer
        param_types.append(self.allocator, llvm.Types.pointer(self.ctx)) catch return EmitError.OutOfMemory;

        // Add user parameter types
        if (param_types_info) |pti| {
            // Use the provided parameter types
            for (pti) |pt| {
                param_types.append(self.allocator, pt) catch return EmitError.OutOfMemory;
            }
        } else {
            // Default to i32 for each argument
            for (args) |_| {
                param_types.append(self.allocator, llvm.Types.int32(self.ctx)) catch return EmitError.OutOfMemory;
            }
        }

        // Use provided return type or default to i32
        const actual_return_type = return_type orelse llvm.Types.int32(self.ctx);
        const fn_type = llvm.Types.function(actual_return_type, param_types.items, false);

        // Call through function pointer
        const call_name: [:0]const u8 = if (llvm.isVoidType(actual_return_type)) "" else "closure.call";
        return self.builder.buildCall(fn_type, fn_ptr, call_args.items, call_name);
    }

    /// Emit an if statement (no value produced, no PHI nodes).
    fn emitIfStmt(self: *Emitter, if_stmt: *ast.IfStmt) EmitError!void {
        const func = self.current_function orelse return EmitError.InvalidAST;

        const cond = try self.emitExpr(if_stmt.condition);

        // Create blocks
        const then_bb = llvm.appendBasicBlock(self.ctx, func, "then");
        const else_bb = if (if_stmt.else_branch != null)
            llvm.appendBasicBlock(self.ctx, func, "else")
        else
            null;
        const merge_bb = llvm.appendBasicBlock(self.ctx, func, "ifcont");

        // Branch to then or else/merge
        if (else_bb) |eb| {
            _ = self.builder.buildCondBr(cond, then_bb, eb);
        } else {
            _ = self.builder.buildCondBr(cond, then_bb, merge_bb);
        }

        // Emit then block
        self.builder.positionAtEnd(then_bb);
        self.has_terminator = false;
        _ = try self.emitBlock(if_stmt.then_branch);
        const then_has_term = self.has_terminator;
        if (!then_has_term) {
            _ = self.builder.buildBr(merge_bb);
        }

        // Emit else block if present
        var else_has_term = false;
        if (if_stmt.else_branch) |else_branch| {
            self.builder.positionAtEnd(else_bb.?);
            self.has_terminator = false;
            switch (else_branch.*) {
                .block => |block| {
                    _ = try self.emitBlock(block);
                },
                .if_stmt => |nested_if| {
                    try self.emitIfStmt(nested_if);
                },
            }
            else_has_term = self.has_terminator;
            if (!else_has_term) {
                _ = self.builder.buildBr(merge_bb);
            }
        }

        // Position at merge block
        self.builder.positionAtEnd(merge_bb);

        // If both branches terminate (e.g., both return), the merge block is unreachable
        if (then_has_term and (if_stmt.else_branch != null and else_has_term)) {
            _ = self.builder.buildUnreachable();
            self.has_terminator = true;
        } else {
            self.has_terminator = false;
        }
    }

    /// Emit a match statement (no value produced, no PHI nodes).
    /// Creates a chain of conditional branches that test each pattern in order.
    fn emitMatchStmt(self: *Emitter, match_stmt: *ast.MatchStmt) EmitError!void {
        const func = self.current_function orelse return EmitError.InvalidAST;

        // Emit the subject expression
        const subject_val = try self.emitExpr(match_stmt.subject);

        // Create the merge block for after all arms
        const merge_bb = llvm.appendBasicBlock(self.ctx, func, "match.merge");

        // Create a "match failed" block with unreachable (for non-exhaustive matches)
        const match_failed_bb = llvm.appendBasicBlock(self.ctx, func, "match.failed");

        // Process each arm
        const num_arms = match_stmt.arms.len;
        var all_arms_terminate = true;
        for (match_stmt.arms, 0..) |arm, i| {
            const is_last_arm = (i == num_arms - 1);

            // Create blocks for this arm
            const arm_body_bb = llvm.appendBasicBlock(self.ctx, func, "match.arm");
            // For last arm, failed pattern goes to match_failed, not merge
            const next_arm_bb = if (is_last_arm)
                match_failed_bb
            else
                llvm.appendBasicBlock(self.ctx, func, "match.next");

            // Emit pattern matching condition
            const pattern_matches = try self.emitPatternMatch(arm.pattern, subject_val);

            // Check guard if present
            const condition = if (arm.guard) |guard| blk: {
                // Create a block to evaluate the guard
                const guard_bb = llvm.appendBasicBlock(self.ctx, func, "match.guard");
                _ = self.builder.buildCondBr(pattern_matches, guard_bb, next_arm_bb);

                // Evaluate guard
                self.builder.positionAtEnd(guard_bb);
                const guard_val = try self.emitExpr(guard);
                // Combine pattern match + guard
                break :blk guard_val;
            } else pattern_matches;

            // Branch based on match result
            _ = self.builder.buildCondBr(condition, arm_body_bb, next_arm_bb);

            // Emit arm body
            self.builder.positionAtEnd(arm_body_bb);
            self.has_terminator = false;

            // Push a new scope for pattern bindings
            try self.pushScope(false);
            defer self.popScope();

            // Bind pattern variables
            try self.bindPatternVariables(arm.pattern, subject_val);

            // Emit the arm body (as a block)
            _ = try self.emitBlock(arm.body);
            const arm_has_term = self.has_terminator;

            if (!arm_has_term) {
                _ = self.builder.buildBr(merge_bb);
                all_arms_terminate = false;
            }

            // Move to next arm block
            if (!is_last_arm) {
                self.builder.positionAtEnd(next_arm_bb);
            }
        }

        // Emit match failed block - this should be unreachable for exhaustive matches
        self.builder.positionAtEnd(match_failed_bb);
        _ = self.builder.buildUnreachable();

        // Position at merge block
        self.builder.positionAtEnd(merge_bb);

        // If all arms terminate, merge block is unreachable
        if (all_arms_terminate) {
            _ = self.builder.buildUnreachable();
            self.has_terminator = true;
        } else {
            self.has_terminator = false;
        }
    }

    /// Emit code that evaluates to true (i1) if the pattern matches the subject.
    fn emitPatternMatch(self: *Emitter, pattern: ast.Pattern, subject: llvm.ValueRef) EmitError!llvm.ValueRef {
        return switch (pattern) {
            .wildcard => {
                // Wildcard always matches
                return llvm.Const.int1(self.ctx, true);
            },
            .literal => |lit| {
                // Compare subject to literal value
                return self.emitLiteralPatternMatch(lit, subject);
            },
            .binding => {
                // Binding always matches (variable capture is done in bindPatternVariables)
                return llvm.Const.int1(self.ctx, true);
            },
            .variant => |v| {
                // Check enum tag matches
                return self.emitVariantPatternMatch(v, subject);
            },
            .or_pattern => |o| {
                // Any alternative matches
                var result = llvm.Const.int1(self.ctx, false);
                for (o.alternatives) |alt| {
                    const alt_match = try self.emitPatternMatch(alt, subject);
                    result = self.builder.buildOr(result, alt_match, "or.pattern");
                }
                return result;
            },
            .guarded => |g| {
                // Just check the pattern; guard is handled separately
                return self.emitPatternMatch(g.pattern, subject);
            },
            .tuple_pattern => |t| {
                // Check each element matches
                var result = llvm.Const.int1(self.ctx, true);
                for (t.elements, 0..) |elem, i| {
                    // Extract tuple element
                    var indices = [_]llvm.ValueRef{
                        llvm.Const.int32(self.ctx, 0),
                        llvm.Const.int32(self.ctx, @intCast(i)),
                    };
                    const subject_type = llvm.typeOf(subject);
                    const elem_val = self.builder.buildGEP(subject_type, subject, &indices, "tuple.elem");
                    const elem_match = try self.emitPatternMatch(elem, elem_val);
                    result = self.builder.buildAnd(result, elem_match, "tuple.match");
                }
                return result;
            },
            .struct_pattern => {
                // For struct patterns, we need to match field by field
                // For now, return true (simplified)
                return llvm.Const.int1(self.ctx, true);
            },
        };
    }

    /// Emit comparison for a literal pattern.
    fn emitLiteralPatternMatch(self: *Emitter, lit: ast.PatternLiteral, subject: llvm.ValueRef) llvm.ValueRef {
        return switch (lit.kind) {
            .int => |v| {
                const lit_val = if (v >= std.math.minInt(i32) and v <= std.math.maxInt(i32))
                    llvm.Const.int32(self.ctx, @intCast(v))
                else
                    llvm.Const.int64(self.ctx, @intCast(v));
                return self.builder.buildICmp(llvm.c.LLVMIntEQ, subject, lit_val, "int.match");
            },
            .float => |v| {
                const lit_val = llvm.Const.float64(self.ctx, v);
                return self.builder.buildFCmp(llvm.c.LLVMRealOEQ, subject, lit_val, "float.match");
            },
            .bool_ => |v| {
                const lit_val = llvm.Const.int1(self.ctx, v);
                return self.builder.buildICmp(llvm.c.LLVMIntEQ, subject, lit_val, "bool.match");
            },
            .char => |v| {
                const lit_val = llvm.Const.int(llvm.Types.int32(self.ctx), v, false);
                return self.builder.buildICmp(llvm.c.LLVMIntEQ, subject, lit_val, "char.match");
            },
            .string => |s| {
                // String comparison - call strcmp or similar
                // For now, simplified: compare pointers (won't work for different string instances)
                const str_z = self.allocator.dupeZ(u8, s) catch return llvm.Const.int1(self.ctx, false);
                defer self.allocator.free(str_z);
                const lit_val = self.builder.buildGlobalStringPtr(str_z, "str.pat");
                return self.builder.buildICmp(llvm.c.LLVMIntEQ, subject, lit_val, "str.match");
            },
        };
    }

    /// Emit code to check if an enum variant matches.
    fn emitVariantPatternMatch(self: *Emitter, pat: *ast.VariantPattern, subject: llvm.ValueRef) EmitError!llvm.ValueRef {
        // Get the enum type to find the variant index
        const variant_index = try self.lookupVariantIndex(pat);

        // Extract the tag from subject (field 0)
        const subject_type = llvm.typeOf(subject);
        var tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };

        // For value types (not pointers), we need to allocate and then GEP
        // Check if subject is a struct type (value) or pointer
        const type_kind = llvm.getTypeKind(subject_type);
        if (type_kind == llvm.c.LLVMStructTypeKind) {
            // Subject is a value, extract tag directly using extractvalue
            const tag_val = self.builder.buildExtractValue(subject, 0, "enum.tag");
            const expected_tag = llvm.Const.int(llvm.Types.int8(self.ctx), variant_index, false);
            return self.builder.buildICmp(llvm.c.LLVMIntEQ, tag_val, expected_tag, "variant.match");
        } else {
            // Subject is a pointer, use GEP
            const tag_ptr = self.builder.buildGEP(subject_type, subject, &tag_indices, "enum.tag.ptr");
            const tag_val = self.builder.buildLoad(llvm.Types.int8(self.ctx), tag_ptr, "enum.tag");
            const expected_tag = llvm.Const.int(llvm.Types.int8(self.ctx), variant_index, false);
            return self.builder.buildICmp(llvm.c.LLVMIntEQ, tag_val, expected_tag, "variant.match");
        }
    }

    /// Look up the variant index for a pattern.
    fn lookupVariantIndex(self: *Emitter, pat: *ast.VariantPattern) EmitError!u32 {
        // Get the enum type name from the pattern
        const enum_name: []const u8 = if (pat.type_expr) |type_expr| blk: {
            break :blk switch (type_expr) {
                .named => |n| n.name,
                .generic_apply => |g| switch (g.base) {
                    .named => |n| n.name,
                    else => return EmitError.InvalidAST,
                },
                else => return EmitError.InvalidAST,
            };
        } else {
            // No explicit type - need to infer from context
            // For now, this is a limitation
            return EmitError.UnsupportedFeature;
        };

        // Look up in monomorphized enums first
        if (self.type_checker) |tc| {
            const monos = tc.getMonomorphizedEnums();
            for (monos) |mono| {
                // Check if this is the enum we're looking for
                if (std.mem.eql(u8, mono.original_name, enum_name)) {
                    for (mono.concrete_type.variants, 0..) |v, i| {
                        if (std.mem.eql(u8, v.name, pat.variant_name)) {
                            return @intCast(i);
                        }
                    }
                }
            }
        }

        // Check registered enum types (non-generic)
        // For non-generic enums, we'd need to look them up differently
        // For now, if we can't find it, return 0 as default
        return 0;
    }

    /// Bind pattern variables to extracted values.
    fn bindPatternVariables(self: *Emitter, pattern: ast.Pattern, subject: llvm.ValueRef) EmitError!void {
        switch (pattern) {
            .wildcard => {},
            .literal => {},
            .binding => |b| {
                // Bind the subject value to the variable name
                const name_z = self.allocator.dupeZ(u8, b.name) catch return EmitError.OutOfMemory;
                defer self.allocator.free(name_z);

                // Create alloca for the bound variable
                const var_type = llvm.typeOf(subject);
                const alloca = self.builder.buildAlloca(var_type, name_z);
                _ = self.builder.buildStore(subject, alloca);

                self.named_values.put(b.name, .{
                    .value = alloca,
                    .is_alloca = true,
                    .ty = var_type,
                    .is_signed = false,
                }) catch return EmitError.OutOfMemory;
            },
            .variant => |v| {
                // If variant has a payload pattern, extract and bind it
                if (v.payload) |payload_pattern| {
                    const payload_val = try self.extractEnumPayload(subject);
                    try self.bindPatternVariables(payload_pattern, payload_val);
                }
            },
            .tuple_pattern => |t| {
                for (t.elements, 0..) |elem, i| {
                    var indices = [_]llvm.ValueRef{
                        llvm.Const.int32(self.ctx, 0),
                        llvm.Const.int32(self.ctx, @intCast(i)),
                    };
                    const subject_type = llvm.typeOf(subject);
                    const elem_val = self.builder.buildGEP(subject_type, subject, &indices, "tuple.elem");
                    try self.bindPatternVariables(elem, elem_val);
                }
            },
            .or_pattern => |o| {
                // For or-patterns, bind from first alternative (they should have same bindings)
                if (o.alternatives.len > 0) {
                    try self.bindPatternVariables(o.alternatives[0], subject);
                }
            },
            .guarded => |g| {
                try self.bindPatternVariables(g.pattern, subject);
            },
            .struct_pattern => {},
        }
    }

    /// Extract the payload from an enum value (returns pointer to payload bytes).
    fn extractEnumPayload(self: *Emitter, enum_val: llvm.ValueRef) EmitError!llvm.ValueRef {
        const val_type = llvm.typeOf(enum_val);
        const type_kind = llvm.getTypeKind(val_type);

        if (type_kind == llvm.c.LLVMStructTypeKind) {
            // Value type: extract payload (field 1) - this gives us the byte array
            // For value types, we need to allocate first to get a pointer
            const alloca = self.builder.buildAlloca(val_type, "enum.tmp");
            _ = self.builder.buildStore(enum_val, alloca);

            // GEP to payload field (index 1, then index 0 for start of array)
            var payload_indices = [_]llvm.ValueRef{
                llvm.Const.int32(self.ctx, 0),
                llvm.Const.int32(self.ctx, 1),
                llvm.Const.int32(self.ctx, 0),
            };
            return self.builder.buildGEP(val_type, alloca, &payload_indices, "enum.payload");
        } else {
            // Pointer type: GEP directly
            var payload_indices = [_]llvm.ValueRef{
                llvm.Const.int32(self.ctx, 0),
                llvm.Const.int32(self.ctx, 1),
                llvm.Const.int32(self.ctx, 0),
            };
            return self.builder.buildGEP(val_type, enum_val, &payload_indices, "enum.payload");
        }
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
                    .identifier => |ident| blk: {
                        // Look up constant from type checker
                        if (self.type_checker) |tc| {
                            if (tc.getConstantValue(ident.name)) |cv| {
                                if (cv == .int) {
                                    break :blk @intCast(cv.int.value);
                                }
                            }
                        }
                        return EmitError.UnsupportedFeature;
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
            .result => |res| {
                // Result[T, E] is a struct of { tag: i1, ok_value: T, err_value: E }
                // tag: 1 = Ok, 0 = Err
                const ok_type = try self.typeExprToLLVM(res.ok_type);
                const err_type = try self.typeExprToLLVM(res.err_type);
                var result_fields = [_]llvm.TypeRef{
                    llvm.Types.int1(self.ctx), // tag (0 = err, 1 = ok)
                    ok_type, // ok_value
                    err_type, // err_value
                };
                return llvm.Types.struct_(self.ctx, &result_fields, false);
            },
            .generic_apply => |g| {
                // Handle builtin generic types like Result[T, E]
                if (g.base == .named) {
                    const base_name = g.base.named.name;
                    if (std.mem.eql(u8, base_name, "Result") and g.args.len == 2) {
                        // Result[T, E] is a struct of { tag: i1, ok_value: T, err_value: E }
                        const ok_type = try self.typeExprToLLVM(g.args[0]);
                        const err_type = try self.typeExprToLLVM(g.args[1]);
                        var result_fields = [_]llvm.TypeRef{
                            llvm.Types.int1(self.ctx), // tag (0 = err, 1 = ok)
                            ok_type, // ok_value
                            err_type, // err_value
                        };
                        return llvm.Types.struct_(self.ctx, &result_fields, false);
                    }
                }
                // Other complex types - return pointer as placeholder
                return llvm.Types.pointer(self.ctx);
            },
            .qualified => {
                // Qualified types like Self.Item are resolved by the type checker
                // At codegen time, they should have been substituted with concrete types
                // If we get here, it's an unresolved associated type - use pointer as placeholder
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
        if (std.mem.eql(u8, name, "string")) return llvm.Types.pointer(self.ctx);
        if (std.mem.eql(u8, name, "char")) return llvm.Types.int32(self.ctx); // Unicode codepoint
        if (std.mem.eql(u8, name, "isize")) return llvm.Types.int64(self.ctx);
        if (std.mem.eql(u8, name, "usize")) return llvm.Types.int64(self.ctx);

        // Check if it's a registered struct type
        if (self.struct_types.get(name)) |struct_info| {
            return struct_info.llvm_type;
        }

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

    /// Check if a type expression is a primitive string type.
    fn isTypeString(self: *Emitter, type_expr: ast.TypeExpr) bool {
        _ = self;
        return switch (type_expr) {
            .named => |named| std.mem.eql(u8, named.name, "string"),
            else => false,
        };
    }

    /// Check if a type expression is a heap-allocated String type.
    fn isTypeStringData(self: *Emitter, type_expr: ast.TypeExpr) bool {
        _ = self;
        return switch (type_expr) {
            .named => |named| std.mem.eql(u8, named.name, "String"),
            else => false,
        };
    }

    /// Check if a type expression is an array or slice type.
    fn isTypeArray(self: *Emitter, type_expr: ast.TypeExpr) bool {
        _ = self;
        return type_expr == .array or type_expr == .slice;
    }

    /// Get array info from a type expression.
    /// Returns (element_type, size) where size is null for slices.
    fn getArrayTypeInfo(self: *Emitter, type_expr: ast.TypeExpr) ?struct { element_type: ?types.Type, size: ?usize } {
        switch (type_expr) {
            .array => |arr| {
                // For fixed-size arrays, get the size from the expression
                const size: ?usize = if (arr.size == .literal) blk: {
                    if (arr.size.literal.kind == .int) {
                        // Get the integer value directly
                        const int_val = arr.size.literal.kind.int;
                        if (int_val >= 0 and int_val <= std.math.maxInt(usize)) {
                            break :blk @intCast(int_val);
                        }
                    }
                    break :blk null;
                } else null;
                // Convert the element type expression to a types.Type using type checker
                const element_type: ?types.Type = if (self.type_checker) |tc| blk: {
                    const tc_mut = @constCast(tc);
                    break :blk tc_mut.resolveTypeExpr(arr.element) catch null;
                } else null;
                return .{ .element_type = element_type, .size = size };
            },
            .slice => |slc| {
                // Slices don't have a size
                const element_type: ?types.Type = if (self.type_checker) |tc| blk: {
                    const tc_mut = @constCast(tc);
                    break :blk tc_mut.resolveTypeExpr(slc.element) catch null;
                } else null;
                return .{ .element_type = element_type, .size = null };
            },
            else => return null,
        }
    }

    /// Get List element type from type expression (e.g., List[i32] -> i32)
    fn getListTypeInfo(self: *Emitter, type_expr: ast.TypeExpr) ?types.Type {
        switch (type_expr) {
            .generic_apply => |g| {
                // Check if the base is "List"
                if (g.base == .named and std.mem.eql(u8, g.base.named.name, "List")) {
                    if (g.args.len == 1) {
                        // Resolve the element type using type checker
                        if (self.type_checker) |tc| {
                            const tc_mut = @constCast(tc);
                            return tc_mut.resolveTypeExpr(g.args[0]) catch null;
                        }
                    }
                }
                return null;
            },
            else => return null,
        }
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
                    .null_coalesce => {
                        // Null coalescing returns the inner type of the Optional
                        // which is the same as the right side type
                        return try self.inferExprType(bin.right);
                    },
                    else => {
                        // For arithmetic, check both sides - if either is float, result is float
                        const left_ty = try self.inferExprType(bin.left);
                        const right_ty = try self.inferExprType(bin.right);
                        const left_kind = llvm.getTypeKind(left_ty);
                        const right_kind = llvm.getTypeKind(right_ty);
                        const left_is_float = left_kind == llvm.c.LLVMFloatTypeKind or left_kind == llvm.c.LLVMDoubleTypeKind;
                        const right_is_float = right_kind == llvm.c.LLVMFloatTypeKind or right_kind == llvm.c.LLVMDoubleTypeKind;
                        if (left_is_float) return left_ty;
                        if (right_is_float) return right_ty;
                        return left_ty;
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

                // Handle builtin Result constructors: Ok(value) and Err(error)
                if (std.mem.eql(u8, func_name, "Ok")) {
                    // Ok(value) returns Result[T, i32] where T is inferred from argument
                    if (call.args.len == 1) {
                        const ok_type = try self.inferExprType(call.args[0]);
                        const err_type = llvm.Types.int32(self.ctx);
                        return self.getResultType(ok_type, err_type);
                    }
                    return llvm.Types.int32(self.ctx);
                }

                if (std.mem.eql(u8, func_name, "Err")) {
                    // Err(error) returns Result[i32, E] where E is inferred from argument
                    if (call.args.len == 1) {
                        const err_type = try self.inferExprType(call.args[0]);
                        const ok_type = llvm.Types.int32(self.ctx);
                        return self.getResultType(ok_type, err_type);
                    }
                    return llvm.Types.int32(self.ctx);
                }

                // Check if this is a monomorphized generic function call
                if (self.type_checker) |checker| {
                    if (checker.getCallResolution(call)) |mangled_name| {
                        const mangled_z = self.allocator.dupeZ(u8, mangled_name) catch return EmitError.OutOfMemory;
                        defer self.allocator.free(mangled_z);

                        if (self.module.getNamedFunction(mangled_z)) |func| {
                            const fn_type = llvm.getGlobalValueType(func);
                            return llvm.getReturnType(fn_type);
                        }
                    }
                }

                // Check if this is a local closure variable with known return type
                if (self.named_values.get(func_name)) |local| {
                    if (local.closure_return_type) |ret_type| {
                        return ret_type;
                    }
                }

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
                // Check for special Rc, Arc, Cell, and List methods that return pointers
                if (m.object == .identifier) {
                    const obj_name = m.object.identifier.name;
                    if (std.mem.eql(u8, obj_name, "Rc") and std.mem.eql(u8, m.method_name, "new")) {
                        // Rc.new() returns a pointer (Rc[T])
                        return llvm.Types.pointer(self.ctx);
                    }
                    if (std.mem.eql(u8, obj_name, "Arc") and std.mem.eql(u8, m.method_name, "new")) {
                        // Arc.new() returns a pointer (Arc[T])
                        return llvm.Types.pointer(self.ctx);
                    }
                    if (std.mem.eql(u8, obj_name, "Cell") and std.mem.eql(u8, m.method_name, "new")) {
                        // Cell.new() returns a pointer (Cell[T])
                        return llvm.Types.pointer(self.ctx);
                    }
                    if (std.mem.eql(u8, obj_name, "List") and std.mem.eql(u8, m.method_name, "new")) {
                        // List.new[T]() returns List struct { ptr, i32, i32 }
                        return self.getListStructType();
                    }
                    if (std.mem.eql(u8, obj_name, "List") and std.mem.eql(u8, m.method_name, "with_capacity")) {
                        // List.with_capacity[T](n) returns List struct { ptr, i32, i32 }
                        return self.getListStructType();
                    }
                    // String static constructors
                    if (std.mem.eql(u8, obj_name, "String") and std.mem.eql(u8, m.method_name, "new")) {
                        // String.new() returns String struct { ptr, i32, i32 }
                        return self.getStringStructType();
                    }
                    if (std.mem.eql(u8, obj_name, "String") and std.mem.eql(u8, m.method_name, "from")) {
                        // String.from(s) returns String struct { ptr, i32, i32 }
                        return self.getStringStructType();
                    }
                    if (std.mem.eql(u8, obj_name, "String") and std.mem.eql(u8, m.method_name, "with_capacity")) {
                        // String.with_capacity(n) returns String struct { ptr, i32, i32 }
                        return self.getStringStructType();
                    }
                }

                // Array/slice methods - check FIRST before Cell methods (which also have .get())
                if (self.isArrayExpr(m.object)) {
                    // len() returns i32
                    if (std.mem.eql(u8, m.method_name, "len")) {
                        return llvm.Types.int32(self.ctx);
                    }
                    // is_empty() and contains() return bool
                    if (std.mem.eql(u8, m.method_name, "is_empty") or
                        std.mem.eql(u8, m.method_name, "contains"))
                    {
                        return llvm.Types.int1(self.ctx);
                    }
                    // first(), last(), get() return Optional[T] - struct { i1 tag, T value }
                    if (std.mem.eql(u8, m.method_name, "first") or
                        std.mem.eql(u8, m.method_name, "last") or
                        std.mem.eql(u8, m.method_name, "get"))
                    {
                        const elem_llvm_type = if (self.getArrayElementType(m.object)) |element_type|
                            self.typeToLLVM(element_type)
                        else
                            llvm.Types.int32(self.ctx); // Fallback to i32 if element type unknown
                        var opt_fields = [_]llvm.TypeRef{
                            llvm.Types.int1(self.ctx), // tag (bool, matches built-in Optional)
                            elem_llvm_type, // value
                        };
                        return llvm.Types.struct_(self.ctx, &opt_fields, false);
                    }
                }

                // List methods - check BEFORE Cell methods (which also have .get())
                if (self.isListExpr(m.object)) {
                    // len() and capacity() return i32
                    if (std.mem.eql(u8, m.method_name, "len") or
                        std.mem.eql(u8, m.method_name, "capacity"))
                    {
                        return llvm.Types.int32(self.ctx);
                    }
                    // is_empty() returns bool
                    if (std.mem.eql(u8, m.method_name, "is_empty")) {
                        return llvm.Types.int1(self.ctx);
                    }
                    // push(), set(), clear() return void (we use i32 as placeholder)
                    if (std.mem.eql(u8, m.method_name, "push") or
                        std.mem.eql(u8, m.method_name, "set") or
                        std.mem.eql(u8, m.method_name, "clear"))
                    {
                        return llvm.Types.int32(self.ctx);
                    }
                    // pop(), first(), last(), get() return Optional[T]
                    if (std.mem.eql(u8, m.method_name, "pop") or
                        std.mem.eql(u8, m.method_name, "first") or
                        std.mem.eql(u8, m.method_name, "last") or
                        std.mem.eql(u8, m.method_name, "get"))
                    {
                        const elem_llvm_type = if (self.getListElementType(m.object)) |element_type|
                            self.typeToLLVM(element_type)
                        else
                            llvm.Types.int32(self.ctx); // Fallback to i32 if element type unknown
                        var opt_fields = [_]llvm.TypeRef{
                            llvm.Types.int1(self.ctx), // tag
                            elem_llvm_type, // value
                        };
                        return llvm.Types.struct_(self.ctx, &opt_fields, false);
                    }
                }

                // String (heap-allocated) methods
                if (self.isStringDataExpr(m.object)) {
                    // len() and capacity() return i32
                    if (std.mem.eql(u8, m.method_name, "len") or
                        std.mem.eql(u8, m.method_name, "capacity"))
                    {
                        return llvm.Types.int32(self.ctx);
                    }
                    // is_empty(), eq() return bool
                    if (std.mem.eql(u8, m.method_name, "is_empty") or
                        std.mem.eql(u8, m.method_name, "eq"))
                    {
                        return llvm.Types.int1(self.ctx);
                    }
                    // hash() returns i64
                    if (std.mem.eql(u8, m.method_name, "hash")) {
                        return llvm.Types.int64(self.ctx);
                    }
                    // push(), append(), clear(), drop() return void (we use i32 as placeholder)
                    if (std.mem.eql(u8, m.method_name, "push") or
                        std.mem.eql(u8, m.method_name, "append") or
                        std.mem.eql(u8, m.method_name, "clear") or
                        std.mem.eql(u8, m.method_name, "drop"))
                    {
                        return llvm.Types.int32(self.ctx);
                    }
                    // clone(), concat() return String
                    if (std.mem.eql(u8, m.method_name, "clone") or
                        std.mem.eql(u8, m.method_name, "concat"))
                    {
                        return self.getStringStructType();
                    }
                    // as_str() returns string pointer
                    if (std.mem.eql(u8, m.method_name, "as_str")) {
                        return llvm.Types.pointer(self.ctx);
                    }
                }

                // downgrade() always returns pointer (Weak type)
                if (std.mem.eql(u8, m.method_name, "downgrade")) {
                    return llvm.Types.pointer(self.ctx);
                }
                // clone() - for Rc/Arc returns pointer, for Clone trait returns same type as object
                if (std.mem.eql(u8, m.method_name, "clone")) {
                    if (self.isArcType(m.object) or self.isRcType(m.object)) {
                        return llvm.Types.pointer(self.ctx);
                    }
                    // For Clone trait, return the type of the object being cloned
                    return self.inferExprType(m.object);
                }
                // upgrade() returns optional pointer - struct {i1, ptr}
                if (std.mem.eql(u8, m.method_name, "upgrade")) {
                    const ptr_type = llvm.Types.pointer(self.ctx);
                    var opt_fields = [_]llvm.TypeRef{ llvm.Types.int1(self.ctx), ptr_type };
                    return llvm.Types.struct_(self.ctx, &opt_fields, false);
                }
                // Cell methods: get() returns the inner type
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
                // Eq trait: eq() returns bool (i1)
                if (std.mem.eql(u8, m.method_name, "eq")) {
                    return llvm.Types.int1(self.ctx);
                }
                // Ordered trait: lt(), le(), gt(), ge() return bool (i1)
                if (std.mem.eql(u8, m.method_name, "lt") or
                    std.mem.eql(u8, m.method_name, "le") or
                    std.mem.eql(u8, m.method_name, "gt") or
                    std.mem.eql(u8, m.method_name, "ge"))
                {
                    return llvm.Types.int1(self.ctx);
                }
                // is_ok, is_err, is_some, is_none all return bool
                if (std.mem.eql(u8, m.method_name, "is_ok") or
                    std.mem.eql(u8, m.method_name, "is_err") or
                    std.mem.eql(u8, m.method_name, "is_some") or
                    std.mem.eql(u8, m.method_name, "is_none"))
                {
                    return llvm.Types.int1(self.ctx);
                }
                // Result.ok() returns Optional[T] where Result is Result[T, E]
                if (std.mem.eql(u8, m.method_name, "ok")) {
                    // Get the Result type from the object
                    const result_type = try self.inferExprType(m.object);
                    // ok_type is at index 1 in the Result struct {tag, ok_value, err_value}
                    const ok_type = llvm.c.LLVMStructGetTypeAtIndex(result_type, 1);
                    // Build Optional type: {i1, T}
                    var opt_fields = [_]llvm.TypeRef{ llvm.Types.int1(self.ctx), ok_type };
                    return llvm.Types.struct_(self.ctx, &opt_fields, false);
                }
                // Result.err() returns Optional[E] where Result is Result[T, E]
                if (std.mem.eql(u8, m.method_name, "err")) {
                    // Get the Result type from the object
                    const result_type = try self.inferExprType(m.object);
                    // err_type is at index 2 in the Result struct {tag, ok_value, err_value}
                    const err_type = llvm.c.LLVMStructGetTypeAtIndex(result_type, 2);
                    // Build Optional type: {i1, E}
                    var opt_fields = [_]llvm.TypeRef{ llvm.Types.int1(self.ctx), err_type };
                    return llvm.Types.struct_(self.ctx, &opt_fields, false);
                }
                // map(f) returns Optional[U] or Result[U, E] where U is the function's return type
                if (std.mem.eql(u8, m.method_name, "map")) {
                    // The return type is determined by the function argument's return type
                    // wrapped in Optional or Result
                    const object_type = try self.inferExprType(m.object);
                    const num_fields = llvm.c.LLVMCountStructElementTypes(object_type);
                    const is_result = num_fields == 3;

                    // For now, use i32 as the mapped type (closure return type)
                    // The actual type will be determined at runtime from the closure call
                    if (is_result) {
                        const err_type = llvm.c.LLVMStructGetTypeAtIndex(object_type, 2);
                        var result_fields = [_]llvm.TypeRef{ llvm.Types.int1(self.ctx), llvm.Types.int32(self.ctx), err_type };
                        return llvm.Types.struct_(self.ctx, &result_fields, false);
                    } else {
                        var opt_fields = [_]llvm.TypeRef{ llvm.Types.int1(self.ctx), llvm.Types.int32(self.ctx) };
                        return llvm.Types.struct_(self.ctx, &opt_fields, false);
                    }
                }
                // and_then(f) returns the function's return type directly
                if (std.mem.eql(u8, m.method_name, "and_then")) {
                    // Same as map - the result type is determined by the closure
                    const object_type = try self.inferExprType(m.object);
                    const num_fields = llvm.c.LLVMCountStructElementTypes(object_type);
                    const is_result = num_fields == 3;

                    if (is_result) {
                        const err_type = llvm.c.LLVMStructGetTypeAtIndex(object_type, 2);
                        var result_fields = [_]llvm.TypeRef{ llvm.Types.int1(self.ctx), llvm.Types.int32(self.ctx), err_type };
                        return llvm.Types.struct_(self.ctx, &result_fields, false);
                    } else {
                        var opt_fields = [_]llvm.TypeRef{ llvm.Types.int1(self.ctx), llvm.Types.int32(self.ctx) };
                        return llvm.Types.struct_(self.ctx, &opt_fields, false);
                    }
                }
                // map_err(f) returns Result[T, F] where T is unchanged and F is the mapped error type
                if (std.mem.eql(u8, m.method_name, "map_err")) {
                    // Get the Result type from the object
                    const object_type = try self.inferExprType(m.object);
                    // ok_type is at index 1, unchanged
                    const ok_type = llvm.c.LLVMStructGetTypeAtIndex(object_type, 1);
                    // For now, use i32 as the mapped error type (closure return type)
                    var result_fields = [_]llvm.TypeRef{ llvm.Types.int1(self.ctx), ok_type, llvm.Types.int32(self.ctx) };
                    return llvm.Types.struct_(self.ctx, &result_fields, false);
                }
                // Hash trait: hash() returns i64
                if (std.mem.eql(u8, m.method_name, "hash")) {
                    return llvm.Types.int64(self.ctx);
                }
                // String methods
                // len() returns i32
                if (std.mem.eql(u8, m.method_name, "len")) {
                    return llvm.Types.int32(self.ctx);
                }
                // is_empty(), contains(), starts_with(), ends_with() return bool
                if (std.mem.eql(u8, m.method_name, "is_empty") or
                    std.mem.eql(u8, m.method_name, "contains") or
                    std.mem.eql(u8, m.method_name, "starts_with") or
                    std.mem.eql(u8, m.method_name, "ends_with"))
                {
                    return llvm.Types.int1(self.ctx);
                }
                // trim(), to_uppercase(), to_lowercase() return string (pointer)
                if (std.mem.eql(u8, m.method_name, "trim") or
                    std.mem.eql(u8, m.method_name, "to_uppercase") or
                    std.mem.eql(u8, m.method_name, "to_lowercase"))
                {
                    return llvm.Types.pointer(self.ctx);
                }
                // bytes() and chars() return slice struct { ptr, len }
                if (std.mem.eql(u8, m.method_name, "bytes") or
                    std.mem.eql(u8, m.method_name, "chars"))
                {
                    var slice_fields = [_]llvm.TypeRef{
                        llvm.Types.pointer(self.ctx), // data pointer
                        llvm.Types.int64(self.ctx), // length
                    };
                    return llvm.Types.struct_(self.ctx, &slice_fields, false);
                }

                // Default trait: TypeName.default() returns the type's default value
                if (std.mem.eql(u8, m.method_name, "default")) {
                    if (m.object == .identifier) {
                        const type_name = m.object.identifier.name;
                        // Map type name to LLVM type
                        if (std.mem.eql(u8, type_name, "i8")) return llvm.Types.int8(self.ctx);
                        if (std.mem.eql(u8, type_name, "i16")) return llvm.Types.int16(self.ctx);
                        if (std.mem.eql(u8, type_name, "i32")) return llvm.Types.int32(self.ctx);
                        if (std.mem.eql(u8, type_name, "i64")) return llvm.Types.int64(self.ctx);
                        if (std.mem.eql(u8, type_name, "i128")) return llvm.Types.int128(self.ctx);
                        if (std.mem.eql(u8, type_name, "isize")) return llvm.Types.int64(self.ctx);
                        if (std.mem.eql(u8, type_name, "u8")) return llvm.Types.int8(self.ctx);
                        if (std.mem.eql(u8, type_name, "u16")) return llvm.Types.int16(self.ctx);
                        if (std.mem.eql(u8, type_name, "u32")) return llvm.Types.int32(self.ctx);
                        if (std.mem.eql(u8, type_name, "u64")) return llvm.Types.int64(self.ctx);
                        if (std.mem.eql(u8, type_name, "u128")) return llvm.Types.int128(self.ctx);
                        if (std.mem.eql(u8, type_name, "usize")) return llvm.Types.int64(self.ctx);
                        if (std.mem.eql(u8, type_name, "f32")) return llvm.Types.float32(self.ctx);
                        if (std.mem.eql(u8, type_name, "f64")) return llvm.Types.float64(self.ctx);
                        if (std.mem.eql(u8, type_name, "bool")) return llvm.Types.int1(self.ctx);
                        if (std.mem.eql(u8, type_name, "char")) return llvm.Types.int32(self.ctx);
                        if (std.mem.eql(u8, type_name, "string")) return llvm.Types.pointer(self.ctx);
                        // Check for struct types
                        if (self.struct_types.get(type_name)) |struct_info| {
                            return struct_info.llvm_type;
                        }
                    }
                }

                // Range methods
                if (self.isRangeExpr(m.object)) {
                    // next() returns Optional[i32] - struct { i1 tag, i32 value }
                    if (std.mem.eql(u8, m.method_name, "next")) {
                        var opt_fields = [_]llvm.TypeRef{
                            llvm.Types.int1(self.ctx), // tag
                            llvm.Types.int32(self.ctx), // value (i32 for now)
                        };
                        return llvm.Types.struct_(self.ctx, &opt_fields, false);
                    }
                    // reset() returns void (represented as i32)
                    if (std.mem.eql(u8, m.method_name, "reset")) {
                        return llvm.Types.int32(self.ctx);
                    }
                    // is_empty() returns bool
                    if (std.mem.eql(u8, m.method_name, "is_empty")) {
                        return llvm.Types.int1(self.ctx);
                    }
                    // len() returns i32
                    if (std.mem.eql(u8, m.method_name, "len")) {
                        return llvm.Types.int32(self.ctx);
                    }
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
            .type_cast => |tc| {
                // Return the target type of the cast
                return self.getTypeByName(switch (tc.target_type) {
                    .named => |n| n.name,
                    else => return llvm.Types.int32(self.ctx),
                }) orelse llvm.Types.int32(self.ctx);
            },
            .block => |blk| {
                // Block type is the type of its final expression, or void if none
                if (blk.final_expr) |final| {
                    return try self.inferExprType(final);
                }
                return llvm.Types.void_(self.ctx);
            },
            .enum_literal => |lit| {
                // Look up the registered enum type
                const mangled_name = try self.getMangledEnumName(lit.enum_type);
                if (self.struct_types.get(mangled_name)) |enum_info| {
                    return enum_info.llvm_type;
                }
                // Try base name for non-generic enums
                const base_name = switch (lit.enum_type) {
                    .named => |n| n.name,
                    .generic_apply => |g| switch (g.base) {
                        .named => |n| n.name,
                        else => return llvm.Types.int32(self.ctx),
                    },
                    else => return llvm.Types.int32(self.ctx),
                };
                if (self.struct_types.get(base_name)) |enum_info| {
                    return enum_info.llvm_type;
                }
                return llvm.Types.int32(self.ctx);
            },
            // Comptime expressions
            .builtin_call => |bc| {
                // Check for user-defined comptime function call results first
                if (self.type_checker) |tc| {
                    if (tc.comptime_builtin_values.get(bc)) |comptime_value| {
                        return switch (comptime_value) {
                            .int => |i| if (i.is_i32) llvm.Types.int32(self.ctx) else llvm.Types.int64(self.ctx),
                            .float => llvm.Types.float64(self.ctx),
                            .bool_ => llvm.Types.int1(self.ctx),
                            .string => llvm.Types.pointer(self.ctx),
                            .void_ => llvm.Types.int32(self.ctx),
                            .struct_ => |cs| blk: {
                                // Get the LLVM struct type
                                if (self.struct_types.get(cs.type_name)) |struct_info| {
                                    break :blk struct_info.llvm_type;
                                }
                                break :blk llvm.Types.int32(self.ctx); // Fallback
                            },
                            .array => |arr| blk: {
                                // Get the LLVM array type
                                const elem_llvm_type = self.typeToLLVM(arr.element_type);
                                break :blk llvm.Types.array(elem_llvm_type, @intCast(arr.elements.len));
                            },
                        };
                    }
                }
                // Builtin calls that return strings
                if (std.mem.eql(u8, bc.name, "typeName") or
                    std.mem.eql(u8, bc.name, "typeInfo") or
                    std.mem.eql(u8, bc.name, "fields"))
                {
                    return llvm.Types.pointer(self.ctx); // string type
                }
                // @hasField returns bool
                if (std.mem.eql(u8, bc.name, "hasField")) {
                    return llvm.Types.int1(self.ctx); // bool type
                }
                // @sizeOf and @alignOf return i32
                if (std.mem.eql(u8, bc.name, "sizeOf") or std.mem.eql(u8, bc.name, "alignOf")) {
                    return llvm.Types.int32(self.ctx);
                }
                // Default to i32 for unknown builtins
                return llvm.Types.int32(self.ctx);
            },
            .comptime_block => |cb| blk: {
                // Look up the precomputed result from type checker
                if (self.type_checker) |tc| {
                    if (tc.comptime_values.get(cb)) |comptime_value| {
                        break :blk switch (comptime_value) {
                            .int => |i| if (i.is_i32) llvm.Types.int32(self.ctx) else llvm.Types.int64(self.ctx),
                            .float => llvm.Types.float64(self.ctx),
                            .bool_ => llvm.Types.int1(self.ctx),
                            .string => llvm.Types.pointer(self.ctx),
                            .void_ => llvm.Types.int32(self.ctx),
                            .struct_ => |cs| struct_blk: {
                                // Get the LLVM struct type
                                if (self.struct_types.get(cs.type_name)) |struct_info| {
                                    break :struct_blk struct_info.llvm_type;
                                }
                                break :struct_blk llvm.Types.int32(self.ctx); // Fallback
                            },
                            .array => |arr| arr_blk: {
                                // Get the LLVM array type
                                const elem_llvm_type = self.typeToLLVM(arr.element_type);
                                break :arr_blk llvm.Types.array(elem_llvm_type, @intCast(arr.elements.len));
                            },
                        };
                    }
                }
                break :blk llvm.Types.int32(self.ctx); // Fallback
            },
            .range => {
                // Range type: { start: T, end: T, current: T, inclusive: i1 }
                // For now, assume i32 element type
                const i32_type = llvm.Types.int32(self.ctx);
                const i1_type = llvm.Types.int1(self.ctx);
                var fields = [_]llvm.TypeRef{
                    i32_type, // start
                    i32_type, // end
                    i32_type, // current
                    i1_type, // inclusive
                };
                return llvm.Types.struct_(self.ctx, &fields, false);
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
    fn getStructTypeName(self: *Emitter, expr: ast.Expr) ?[]const u8 {
        return switch (expr) {
            .struct_literal => |lit| {
                if (lit.type_name) |type_name| {
                    return switch (type_name) {
                        .named => |n| n.name,
                        .generic_apply => |g| blk: {
                            // Build mangled name for generic struct
                            const base_name = switch (g.base) {
                                .named => |n| n.name,
                                else => break :blk null,
                            };
                            var mangled = std.ArrayListUnmanaged(u8){};
                            mangled.appendSlice(self.allocator, base_name) catch break :blk null;
                            for (g.args) |arg| {
                                mangled.append(self.allocator, '$') catch break :blk null;
                                self.appendTypeNameForMangling(&mangled, arg) catch break :blk null;
                            }
                            break :blk mangled.toOwnedSlice(self.allocator) catch null;
                        },
                        else => null,
                    };
                }
                return null;
            },
            .comptime_block => |cb| {
                // Look up the comptime value and extract struct type name
                if (self.type_checker) |tc| {
                    if (tc.comptime_values.get(cb)) |comptime_value| {
                        if (comptime_value == .struct_) {
                            return comptime_value.struct_.type_name;
                        }
                    }
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

    /// Append a type name to a buffer for name mangling (works with AST TypeExpr).
    fn appendTypeNameForMangling(self: *Emitter, buf: *std.ArrayListUnmanaged(u8), type_expr: ast.TypeExpr) !void {
        switch (type_expr) {
            .named => |n| try buf.appendSlice(self.allocator, n.name),
            .array => |a| {
                try buf.appendSlice(self.allocator, "arr_");
                try self.appendTypeNameForMangling(buf, a.element);
            },
            .slice => |s| {
                try buf.appendSlice(self.allocator, "slice_");
                try self.appendTypeNameForMangling(buf, s.element);
            },
            .optional => |o| {
                try buf.appendSlice(self.allocator, "opt_");
                try self.appendTypeNameForMangling(buf, o.inner);
            },
            .reference => |r| {
                try buf.appendSlice(self.allocator, if (r.mutable) "mutref_" else "ref_");
                try self.appendTypeNameForMangling(buf, r.inner);
            },
            .generic_apply => |g| {
                try self.appendTypeNameForMangling(buf, g.base);
                for (g.args) |arg| {
                    try buf.append(self.allocator, '_');
                    try self.appendTypeNameForMangling(buf, arg);
                }
            },
            .tuple => |t| {
                try buf.appendSlice(self.allocator, "tup");
                for (t.elements) |elem| {
                    try buf.append(self.allocator, '_');
                    try self.appendTypeNameForMangling(buf, elem);
                }
            },
            .result => {
                try buf.appendSlice(self.allocator, "result");
            },
            .function => {
                try buf.appendSlice(self.allocator, "fn");
            },
            .qualified => |q| {
                // For qualified types like Self.Item, mangle as Base_Member
                try self.appendTypeNameForMangling(buf, q.base);
                try buf.append(self.allocator, '_');
                try buf.appendSlice(self.allocator, q.member);
            },
        }
    }

    /// Append type name for mangling from a types.Type value.
    fn appendCheckerTypeNameForMangling(self: *Emitter, buf: *std.ArrayListUnmanaged(u8), ty: types.Type) !void {
        switch (ty) {
            .primitive => |p| {
                const name: []const u8 = switch (p) {
                    .i8_ => "i8",
                    .i16_ => "i16",
                    .i32_ => "i32",
                    .i64_ => "i64",
                    .i128_ => "i128",
                    .isize_ => "isize",
                    .u8_ => "u8",
                    .u16_ => "u16",
                    .u32_ => "u32",
                    .u64_ => "u64",
                    .u128_ => "u128",
                    .usize_ => "usize",
                    .f32_ => "f32",
                    .f64_ => "f64",
                    .bool_ => "bool",
                    .char_ => "char",
                    .string_ => "string",
                };
                try buf.appendSlice(self.allocator, name);
            },
            .void_ => try buf.appendSlice(self.allocator, "void"),
            .struct_ => |s| try buf.appendSlice(self.allocator, s.name),
            .enum_ => |e| try buf.appendSlice(self.allocator, e.name),
            .optional => |o| {
                try buf.appendSlice(self.allocator, "opt_");
                try self.appendCheckerTypeNameForMangling(buf, o.*);
            },
            .array => |a| {
                try buf.appendSlice(self.allocator, "arr_");
                try self.appendCheckerTypeNameForMangling(buf, a.element);
            },
            .slice => |s| {
                try buf.appendSlice(self.allocator, "slice_");
                try self.appendCheckerTypeNameForMangling(buf, s.element);
            },
            .reference => |r| {
                try buf.appendSlice(self.allocator, if (r.mutable) "mutref_" else "ref_");
                try self.appendCheckerTypeNameForMangling(buf, r.inner);
            },
            .applied => |a| {
                try self.appendCheckerTypeNameForMangling(buf, a.base);
                for (a.args) |arg| {
                    try buf.append(self.allocator, '$');
                    try self.appendCheckerTypeNameForMangling(buf, arg);
                }
            },
            .associated_type_ref => |atr| {
                // Format as TypeVar.AssocName for mangling (shouldn't normally reach here)
                try buf.appendSlice(self.allocator, atr.type_var.name);
                try buf.append(self.allocator, '_');
                try buf.appendSlice(self.allocator, atr.assoc_name);
            },
            else => try buf.appendSlice(self.allocator, "unknown"),
        }
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
        // Get the struct type name - could be simple or generic
        const type_expr = lit.type_name orelse return EmitError.InvalidAST;
        const type_name = switch (type_expr) {
            .named => |n| n.name,
            .generic_apply => |g| blk: {
                // For generic types, we need to use the mangled name
                // The type checker has registered monomorphized structs with mangled names
                // We need to reconstruct the mangled name here
                const base_name = switch (g.base) {
                    .named => |n| n.name,
                    else => return EmitError.UnsupportedFeature,
                };
                // Build mangled name: BaseName$Type1$Type2...
                var mangled = std.ArrayListUnmanaged(u8){};
                errdefer mangled.deinit(self.allocator);
                mangled.appendSlice(self.allocator, base_name) catch return EmitError.OutOfMemory;
                for (g.args) |arg| {
                    mangled.append(self.allocator, '$') catch return EmitError.OutOfMemory;
                    self.appendTypeNameForMangling(&mangled, arg) catch return EmitError.OutOfMemory;
                }
                break :blk mangled.toOwnedSlice(self.allocator) catch return EmitError.OutOfMemory;
            },
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
                    // For reference parameters, load the pointer first, then GEP into it
                    // For regular struct parameters, GEP directly from the alloca
                    const base_ptr = if (local.is_reference)
                        self.builder.buildLoad(local.ty, local.value, "ref.load")
                    else
                        local.value;

                    const gep_type = if (local.is_reference)
                        local.reference_inner_type.?
                    else
                        local.ty;

                    // First try numeric index (for tuples)
                    if (field_index) |idx| {
                        var indices = [_]llvm.ValueRef{
                            llvm.Const.int32(self.ctx, 0),
                            llvm.Const.int32(self.ctx, @intCast(idx)),
                        };
                        const field_ptr = self.builder.buildGEP(gep_type, base_ptr, &indices, "field.ptr");
                        // Get the field type from struct type
                        const field_type = llvm.c.LLVMStructGetTypeAtIndex(gep_type, idx);
                        return self.builder.buildLoad(field_type, field_ptr, "field.val");
                    }

                    // Named field access - look up field index from struct type name
                    if (local.struct_type_name) |struct_name| {
                        if (self.lookupFieldIndex(struct_name, field.field_name)) |idx| {
                            var indices = [_]llvm.ValueRef{
                                llvm.Const.int32(self.ctx, 0),
                                llvm.Const.int32(self.ctx, @intCast(idx)),
                            };
                            const field_ptr = self.builder.buildGEP(gep_type, base_ptr, &indices, "field.ptr");
                            const field_type = llvm.c.LLVMStructGetTypeAtIndex(gep_type, idx);
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
    // Optional and Result Type Support
    // =========================================================================

    /// Emit a postfix operator expression (? for safe unwrap, ! for force unwrap).
    /// Works for both Optional[T] and Result[T, E] types:
    /// - Optional layout: { i1 tag, T value } where tag: 0=None, 1=Some
    /// - Result layout: { i1 tag, T ok_value, E err_value } where tag: 0=Err, 1=Ok
    /// The ! operator traps on None/Err and returns the value/ok_value at index 1.
    fn emitPostfix(self: *Emitter, post: *ast.Postfix) EmitError!llvm.ValueRef {
        // Emit the operand (should be an optional or result type)
        const operand = try self.emitExpr(post.operand);
        const operand_type = llvm.typeOf(operand);

        // Optional/Result type layout is { i1, T, ... } where i1 is the tag (0=None/Err, 1=Some/Ok)
        // First, check if operand is a struct type (our optional/result representation)
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

    /// Create a Result Ok value: Result[T, E] = { tag: 1, ok_value: value, err_value: undef }
    fn emitOk(self: *Emitter, value: llvm.ValueRef, ok_type: llvm.TypeRef, err_type: llvm.TypeRef) llvm.ValueRef {
        // Result type is { i1, T, E }
        var result_fields = [_]llvm.TypeRef{
            llvm.Types.int1(self.ctx), // tag
            ok_type, // ok_value
            err_type, // err_value
        };
        const result_type = llvm.Types.struct_(self.ctx, &result_fields, false);

        // Allocate and populate
        const result_alloca = self.builder.buildAlloca(result_type, "ok.tmp");

        // Set tag to 1 (Ok)
        var tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const tag_ptr = self.builder.buildGEP(result_type, result_alloca, &tag_indices, "ok.tag.ptr");
        _ = self.builder.buildStore(llvm.Const.int1(self.ctx, true), tag_ptr);

        // Set ok_value
        var val_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 1),
        };
        const val_ptr = self.builder.buildGEP(result_type, result_alloca, &val_indices, "ok.val.ptr");
        _ = self.builder.buildStore(value, val_ptr);

        // Load and return (err_value field is undefined/uninitialized)
        return self.builder.buildLoad(result_type, result_alloca, "ok.result");
    }

    /// Create a Result Err value: Result[T, E] = { tag: 0, ok_value: undef, err_value: error }
    fn emitErr(self: *Emitter, error_val: llvm.ValueRef, ok_type: llvm.TypeRef, err_type: llvm.TypeRef) llvm.ValueRef {
        // Result type is { i1, T, E }
        var result_fields = [_]llvm.TypeRef{
            llvm.Types.int1(self.ctx), // tag
            ok_type, // ok_value
            err_type, // err_value
        };
        const result_type = llvm.Types.struct_(self.ctx, &result_fields, false);

        // Allocate and populate
        const result_alloca = self.builder.buildAlloca(result_type, "err.tmp");

        // Set tag to 0 (Err)
        var tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const tag_ptr = self.builder.buildGEP(result_type, result_alloca, &tag_indices, "err.tag.ptr");
        _ = self.builder.buildStore(llvm.Const.int1(self.ctx, false), tag_ptr);

        // Set err_value (index 2)
        var err_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 2),
        };
        const err_ptr = self.builder.buildGEP(result_type, result_alloca, &err_indices, "err.err.ptr");
        _ = self.builder.buildStore(error_val, err_ptr);

        // Load and return (ok_value field is undefined/uninitialized)
        return self.builder.buildLoad(result_type, result_alloca, "err.result");
    }

    /// Get the Result type from ok_type and err_type.
    fn getResultType(self: *Emitter, ok_type: llvm.TypeRef, err_type: llvm.TypeRef) llvm.TypeRef {
        var result_fields = [_]llvm.TypeRef{
            llvm.Types.int1(self.ctx), // tag
            ok_type, // ok_value
            err_type, // err_value
        };
        return llvm.Types.struct_(self.ctx, &result_fields, false);
    }

    /// Emit an enum literal: EnumType::VariantName(payload)
    /// Generates code to construct a tagged union value.
    fn emitEnumLiteral(self: *Emitter, lit: *ast.EnumLiteral) EmitError!llvm.ValueRef {
        // Get the mangled enum type name
        const mangled_name = try self.getMangledEnumName(lit.enum_type);

        // Look up the registered enum type
        const enum_info = self.struct_types.get(mangled_name) orelse {
            // Enum type not registered - this might be a non-generic enum
            // Try the base name if it's not a generic
            const base_name = switch (lit.enum_type) {
                .named => |n| n.name,
                .generic_apply => |g| switch (g.base) {
                    .named => |n| n.name,
                    else => return EmitError.UnsupportedFeature,
                },
                else => return EmitError.UnsupportedFeature,
            };

            // Try base name lookup
            if (self.struct_types.get(base_name)) |info| {
                return self.emitEnumLiteralWithInfo(lit, info, base_name);
            }
            return EmitError.UnsupportedFeature;
        };

        return self.emitEnumLiteralWithInfo(lit, enum_info, mangled_name);
    }

    /// Helper to emit enum literal with resolved type info
    fn emitEnumLiteralWithInfo(self: *Emitter, lit: *ast.EnumLiteral, enum_info: StructTypeInfo, enum_name: []const u8) EmitError!llvm.ValueRef {
        // Find the variant index
        var variant_index: ?u32 = null;
        var variant_payload: ?types.VariantPayload = null;

        // We need to look up the enum definition to find variant info
        // For now, try to find it through the type checker's monomorphized enums
        if (self.type_checker) |tc| {
            const monos = tc.getMonomorphizedEnums();
            for (monos) |mono| {
                if (std.mem.eql(u8, mono.mangled_name, enum_name)) {
                    for (mono.concrete_type.variants, 0..) |v, i| {
                        if (std.mem.eql(u8, v.name, lit.variant_name)) {
                            variant_index = @intCast(i);
                            variant_payload = v.payload;
                            break;
                        }
                    }
                    break;
                }
            }
        }

        // Also check non-monomorphized enums (registered directly)
        if (variant_index == null) {
            // For non-generic enums, look up in the regular enum registry
            // This is a simplified path - could be enhanced
            variant_index = 0; // Default for unit variants or simple cases
        }

        const idx = variant_index orelse return EmitError.InvalidAST;
        const enum_type = enum_info.llvm_type;

        // Allocate the enum value
        const enum_alloca = self.builder.buildAlloca(enum_type, "enum.tmp");

        // Determine tag type (i8 for small enums, i16 for larger)
        const tag_type = llvm.Types.int8(self.ctx); // Assume small enums for now

        // Set the tag field (index 0)
        var tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const tag_ptr = self.builder.buildGEP(enum_type, enum_alloca, &tag_indices, "enum.tag.ptr");
        _ = self.builder.buildStore(llvm.Const.int(tag_type, idx, false), tag_ptr);

        // Set the payload if present
        if (lit.payload.len > 0 and variant_payload != null) {
            // Get pointer to payload field (index 1) - stored as byte array
            var payload_indices = [_]llvm.ValueRef{
                llvm.Const.int32(self.ctx, 0),
                llvm.Const.int32(self.ctx, 1),
                llvm.Const.int32(self.ctx, 0), // Start of the byte array
            };
            const payload_ptr = self.builder.buildGEP(enum_type, enum_alloca, &payload_indices, "enum.payload.ptr");

            switch (variant_payload.?) {
                .tuple => |tuple_types| {
                    // For tuple payloads, store each element at its offset
                    var offset: usize = 0;
                    for (lit.payload, tuple_types) |payload_expr, payload_type| {
                        const value = try self.emitExpr(payload_expr);

                        // Get pointer at current offset in the byte array
                        // In opaque pointer mode, we can store directly
                        var elem_indices = [_]llvm.ValueRef{
                            llvm.Const.int32(self.ctx, @intCast(offset)),
                        };
                        const elem_ptr = self.builder.buildGEP(
                            llvm.Types.int8(self.ctx),
                            payload_ptr,
                            &elem_indices,
                            "enum.payload.elem",
                        );
                        // Store the value - opaque pointers don't need bitcast
                        _ = self.builder.buildStore(value, elem_ptr);

                        offset += self.getTypeSize(payload_type);
                    }
                },
                .struct_ => |fields| {
                    // For struct payloads, store fields by offset
                    var offset: usize = 0;
                    for (lit.payload, fields) |payload_expr, field| {
                        const value = try self.emitExpr(payload_expr);

                        var elem_indices = [_]llvm.ValueRef{
                            llvm.Const.int32(self.ctx, @intCast(offset)),
                        };
                        const elem_ptr = self.builder.buildGEP(
                            llvm.Types.int8(self.ctx),
                            payload_ptr,
                            &elem_indices,
                            "enum.payload.field",
                        );
                        _ = self.builder.buildStore(value, elem_ptr);

                        offset += self.getTypeSize(field.type_);
                    }
                },
            }
        }

        // Load and return the constructed enum value
        return self.builder.buildLoad(enum_type, enum_alloca, "enum.result");
    }

    /// Get the mangled name for an enum type expression
    fn getMangledEnumName(self: *Emitter, type_expr: ast.TypeExpr) EmitError![]const u8 {
        return switch (type_expr) {
            .named => |n| n.name,
            .generic_apply => |g| blk: {
                const base_name = switch (g.base) {
                    .named => |n| n.name,
                    else => return EmitError.UnsupportedFeature,
                };

                // Build mangled name: EnumName$Arg1$Arg2
                var result = std.ArrayListUnmanaged(u8){};
                errdefer result.deinit(self.allocator);

                result.appendSlice(self.allocator, base_name) catch return EmitError.OutOfMemory;

                for (g.args) |arg| {
                    result.append(self.allocator, '$') catch return EmitError.OutOfMemory;
                    const arg_name = try self.getTypeName(arg);
                    result.appendSlice(self.allocator, arg_name) catch return EmitError.OutOfMemory;
                }

                const mangled_name = result.toOwnedSlice(self.allocator) catch return EmitError.OutOfMemory;
                // Track allocation for cleanup in deinit
                self.mangled_enum_names.append(self.allocator, mangled_name) catch return EmitError.OutOfMemory;
                break :blk mangled_name;
            },
            else => return EmitError.UnsupportedFeature,
        };
    }

    /// Get a string name for a type expression
    fn getTypeName(self: *Emitter, type_expr: ast.TypeExpr) EmitError![]const u8 {
        return switch (type_expr) {
            .named => |n| n.name,
            .generic_apply => try self.getMangledEnumName(type_expr),
            else => "unknown", // For primitive types referenced in generic args
        };
    }

    /// Emit a method call expression.
    /// Handles special methods like Rc.new(), Cell.new(), Ok(), Err(), .clone(), .downgrade(), etc.
    fn emitMethodCall(self: *Emitter, method: *ast.MethodCall) EmitError!llvm.ValueRef {
        // Check for static constructors: Rc.new(value), Cell.new(value), Ok(value), Err(error)
        if (method.object == .identifier) {
            const obj_name = method.object.identifier.name;

            if (std.mem.eql(u8, obj_name, "Rc") and std.mem.eql(u8, method.method_name, "new")) {
                return self.emitRcNew(method);
            }

            if (std.mem.eql(u8, obj_name, "Arc") and std.mem.eql(u8, method.method_name, "new")) {
                return self.emitArcNew(method);
            }

            if (std.mem.eql(u8, obj_name, "Cell") and std.mem.eql(u8, method.method_name, "new")) {
                return self.emitCellNew(method);
            }

            // Result type constructors: Ok(value), Err(error)
            // These are parsed as method calls like Ok.new(value) style, but we handle them specially
            if (std.mem.eql(u8, obj_name, "Result")) {
                if (std.mem.eql(u8, method.method_name, "ok") or std.mem.eql(u8, method.method_name, "Ok")) {
                    return self.emitResultOk(method);
                }
                if (std.mem.eql(u8, method.method_name, "err") or std.mem.eql(u8, method.method_name, "Err")) {
                    return self.emitResultErr(method);
                }
            }

            // Default trait: TypeName.default() -> default value for the type
            // Primitives have builtin Default implementations
            if (std.mem.eql(u8, method.method_name, "default")) {
                return self.emitDefaultMethod(obj_name);
            }

            // List.new[T]() - creates an empty list
            if (std.mem.eql(u8, obj_name, "List") and std.mem.eql(u8, method.method_name, "new")) {
                return self.emitListNew(method);
            }

            // List.with_capacity[T](n) - creates a list with pre-allocated capacity
            if (std.mem.eql(u8, obj_name, "List") and std.mem.eql(u8, method.method_name, "with_capacity")) {
                return self.emitListWithCapacity(method);
            }

            // String.new() - creates an empty string
            if (std.mem.eql(u8, obj_name, "String") and std.mem.eql(u8, method.method_name, "new")) {
                return self.emitStringNew();
            }

            // String.from(s) - creates a string from a literal
            if (std.mem.eql(u8, obj_name, "String") and std.mem.eql(u8, method.method_name, "from")) {
                return self.emitStringFrom(method);
            }

            // String.with_capacity(n) - creates a string with pre-allocated capacity
            if (std.mem.eql(u8, obj_name, "String") and std.mem.eql(u8, method.method_name, "with_capacity")) {
                return self.emitStringWithCapacity(method);
            }
        }

        // Emit the object
        const object = try self.emitExpr(method.object);
        const object_type = llvm.typeOf(object);

        // Check for array/slice methods FIRST (before Cell methods which also have .get())
        if (self.isArrayExpr(method.object)) {
            // For arrays, we need the alloca pointer, not the loaded value
            // Get the alloca pointer for identifier expressions
            const array_ptr = if (method.object == .identifier) blk: {
                if (self.named_values.get(method.object.identifier.name)) |local| {
                    break :blk local.value; // This is the alloca pointer
                }
                break :blk object;
            } else object;

            if (std.mem.eql(u8, method.method_name, "len")) {
                return self.emitArrayLen(method.object, array_ptr);
            }
            if (std.mem.eql(u8, method.method_name, "is_empty")) {
                return self.emitArrayIsEmpty(method.object, array_ptr);
            }
            if (std.mem.eql(u8, method.method_name, "first")) {
                return self.emitArrayFirst(method.object, array_ptr);
            }
            if (std.mem.eql(u8, method.method_name, "last")) {
                return self.emitArrayLast(method.object, array_ptr);
            }
            if (std.mem.eql(u8, method.method_name, "get")) {
                if (method.args.len != 1) return EmitError.InvalidAST;
                const index = try self.emitExpr(method.args[0]);
                return self.emitArrayGet(method.object, array_ptr, index);
            }
            if (std.mem.eql(u8, method.method_name, "contains")) {
                if (method.args.len != 1) return EmitError.InvalidAST;
                const value = try self.emitExpr(method.args[0]);
                return self.emitArrayContains(method.object, array_ptr, value);
            }
        }

        // Check for List methods (BEFORE Cell methods which also have .get())
        if (self.isListExpr(method.object)) {
            // For List methods, we need the alloca pointer, not the loaded value
            const list_ptr_early = if (method.object == .identifier) blk: {
                if (self.named_values.get(method.object.identifier.name)) |local| {
                    break :blk local.value; // This is the alloca pointer
                }
                break :blk null;
            } else null;

            if (list_ptr_early) |ptr| {
                if (std.mem.eql(u8, method.method_name, "get")) {
                    if (method.args.len != 1) return EmitError.InvalidAST;
                    const index = try self.emitExpr(method.args[0]);
                    return self.emitListGet(ptr, method, index);
                }
                if (std.mem.eql(u8, method.method_name, "set")) {
                    if (method.args.len != 2) return EmitError.InvalidAST;
                    const index = try self.emitExpr(method.args[0]);
                    const value = try self.emitExpr(method.args[1]);
                    return self.emitListSet(ptr, method, index, value);
                }
                if (std.mem.eql(u8, method.method_name, "clone")) {
                    if (method.args.len != 0) return EmitError.InvalidAST;
                    return self.emitListClone(ptr, method);
                }
                if (std.mem.eql(u8, method.method_name, "drop")) {
                    if (method.args.len != 0) return EmitError.InvalidAST;
                    return self.emitListDrop(ptr, method);
                }
            }
        }

        // Check for String methods (heap-allocated string)
        if (self.isStringDataExpr(method.object)) {
            // For String methods, we need the alloca pointer, not the loaded value
            // If object is an identifier, get its alloca directly
            // Otherwise, emit the expression and store to a temporary alloca
            const ptr = if (method.object == .identifier) blk: {
                if (self.named_values.get(method.object.identifier.name)) |local| {
                    break :blk local.value; // This is the alloca pointer
                }
                // Identifier not found in named_values, fall through to emit
                const obj_val = try self.emitExpr(method.object);
                const string_type = self.getStringStructType();
                const tmp_alloca = self.builder.buildAlloca(string_type, "str.tmp");
                _ = self.builder.buildStore(obj_val, tmp_alloca);
                break :blk tmp_alloca;
            } else blk: {
                // Non-identifier object (e.g., method call result like a.concat(b))
                // Emit the expression and store to a temporary alloca
                const obj_val = try self.emitExpr(method.object);
                const string_type = self.getStringStructType();
                const tmp_alloca = self.builder.buildAlloca(string_type, "str.tmp");
                _ = self.builder.buildStore(obj_val, tmp_alloca);
                break :blk tmp_alloca;
            };

            {
                if (std.mem.eql(u8, method.method_name, "len")) {
                    return self.emitStringDataLen(ptr);
                }
                if (std.mem.eql(u8, method.method_name, "is_empty")) {
                    return self.emitStringDataIsEmpty(ptr);
                }
                if (std.mem.eql(u8, method.method_name, "capacity")) {
                    return self.emitStringCapacity(ptr);
                }
                if (std.mem.eql(u8, method.method_name, "push")) {
                    if (method.args.len != 1) return EmitError.InvalidAST;
                    const char_val = try self.emitExpr(method.args[0]);
                    return self.emitStringPush(ptr, char_val);
                }
                if (std.mem.eql(u8, method.method_name, "concat")) {
                    if (method.args.len != 1) return EmitError.InvalidAST;
                    // For concat we need the other string's pointer
                    // If argument is an identifier, get its alloca directly
                    // Otherwise, emit the expression and store to a temporary alloca
                    const other_ptr = if (method.args[0] == .identifier) other_blk: {
                        if (self.named_values.get(method.args[0].identifier.name)) |local| {
                            break :other_blk local.value;
                        }
                        // Identifier not found in named_values, fall through to emit
                        const other_val = try self.emitExpr(method.args[0]);
                        const string_type = self.getStringStructType();
                        const other_tmp = self.builder.buildAlloca(string_type, "str.arg.tmp");
                        _ = self.builder.buildStore(other_val, other_tmp);
                        break :other_blk other_tmp;
                    } else other_blk: {
                        const other_val = try self.emitExpr(method.args[0]);
                        const string_type = self.getStringStructType();
                        const other_tmp = self.builder.buildAlloca(string_type, "str.arg.tmp");
                        _ = self.builder.buildStore(other_val, other_tmp);
                        break :other_blk other_tmp;
                    };
                    return self.emitStringConcat(ptr, other_ptr);
                }
                if (std.mem.eql(u8, method.method_name, "append")) {
                    if (method.args.len != 1) return EmitError.InvalidAST;
                    const other_str = try self.emitExpr(method.args[0]);
                    const other_ptr = if (method.args[0] == .identifier) other_blk: {
                        if (self.named_values.get(method.args[0].identifier.name)) |local| {
                            break :other_blk local.value;
                        }
                        break :other_blk other_str;
                    } else other_str;
                    return self.emitStringAppend(ptr, other_ptr);
                }
                if (std.mem.eql(u8, method.method_name, "as_str")) {
                    return self.emitStringAsStr(ptr);
                }
                if (std.mem.eql(u8, method.method_name, "clear")) {
                    return self.emitStringClear(ptr);
                }
                if (std.mem.eql(u8, method.method_name, "clone")) {
                    return self.emitStringClone(ptr);
                }
                if (std.mem.eql(u8, method.method_name, "drop")) {
                    return self.emitStringDrop(ptr);
                }
                if (std.mem.eql(u8, method.method_name, "eq")) {
                    if (method.args.len != 1) return EmitError.InvalidAST;
                    const other_str = try self.emitExpr(method.args[0]);
                    const other_ptr = if (method.args[0] == .identifier) other_blk: {
                        if (self.named_values.get(method.args[0].identifier.name)) |local| {
                            break :other_blk local.value;
                        }
                        break :other_blk other_str;
                    } else other_str;
                    return self.emitStringEq(ptr, other_ptr);
                }
                if (std.mem.eql(u8, method.method_name, "hash")) {
                    return self.emitStringDataHash(ptr);
                }
            }
        }

        // Check for user-defined struct methods FIRST before builtin methods
        // This ensures user-defined methods like .get() take precedence over Cell.get()
        if (self.type_checker) |tc| {
            const struct_name = self.getStructNameFromExpr(method.object);
            if (struct_name) |name| {
                if (tc.lookupStructMethod(name, method.method_name)) |struct_method| {
                    return self.emitUserDefinedMethod(method, object, name, struct_method);
                }
            }
        }

        // Check for Cell methods: .get(), .set()
        // Only applies if no user-defined method was found above and not a List
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

        // Check for Rc/Arc methods
        if (std.mem.eql(u8, method.method_name, "clone")) {
            // Check if this is an Arc or Rc and dispatch accordingly
            if (self.isArcType(method.object)) {
                return self.emitArcClone(object, object_type);
            } else if (self.isRcType(method.object)) {
                return self.emitRcClone(object, object_type);
            }
            // For non-Rc/Arc types, fall through to Clone trait handler below
        }

        if (std.mem.eql(u8, method.method_name, "downgrade")) {
            // Check if this is an Arc or Rc and dispatch accordingly
            if (self.isArcType(method.object)) {
                return self.emitArcDowngrade(object, object_type);
            } else {
                return self.emitRcDowngrade(object, object_type);
            }
        }

        if (std.mem.eql(u8, method.method_name, "upgrade")) {
            // Check if this is a WeakArc or Weak and dispatch accordingly
            // For now, assume Weak (non-atomic) - WeakArc needs separate tracking
            return self.emitWeakUpgrade(object, object_type);
        }

        // Check for Result/Optional methods: is_ok, is_err, is_some, is_none
        if (std.mem.eql(u8, method.method_name, "is_ok")) {
            // Result.is_ok() - return true if tag == 1
            return self.emitResultIsOk(object, object_type);
        }

        if (std.mem.eql(u8, method.method_name, "is_err")) {
            // Result.is_err() - return true if tag == 0
            return self.emitResultIsErr(object, object_type);
        }

        if (std.mem.eql(u8, method.method_name, "is_some")) {
            // Optional.is_some() - return true if tag == 1
            return self.emitOptionalIsSome(object, object_type);
        }

        if (std.mem.eql(u8, method.method_name, "is_none")) {
            // Optional.is_none() - return true if tag == 0
            return self.emitOptionalIsNone(object, object_type);
        }

        if (std.mem.eql(u8, method.method_name, "unwrap")) {
            // Result.unwrap() or Optional.unwrap() - same as ! operator
            return self.emitResultUnwrap(object, object_type);
        }

        if (std.mem.eql(u8, method.method_name, "unwrap_err")) {
            // Result.unwrap_err() - return error value, trap if Ok
            return self.emitResultUnwrapErr(object, object_type);
        }

        if (std.mem.eql(u8, method.method_name, "unwrap_or")) {
            // Optional.unwrap_or(default) or Result.unwrap_or(default)
            // Returns the inner value if Some/Ok, otherwise returns default
            if (method.args.len != 1) {
                return EmitError.InvalidAST;
            }
            const default_val = try self.emitExpr(method.args[0]);
            return self.emitUnwrapOr(object, object_type, default_val);
        }

        if (std.mem.eql(u8, method.method_name, "expect")) {
            // Optional.expect(msg) or Result.expect(msg)
            // Like unwrap but with a custom message (message ignored for now - traps on None/Err)
            // TODO: Print the message before trapping when panic infrastructure is available
            return self.emitResultUnwrap(object, object_type);
        }

        if (std.mem.eql(u8, method.method_name, "ok")) {
            // Result.ok() -> ?T - converts Result to Optional of Ok value
            // Returns Some(value) if Ok, None if Err
            return self.emitResultOkMethod(object, object_type, method);
        }

        if (std.mem.eql(u8, method.method_name, "err")) {
            // Result.err() -> ?E - converts Result to Optional of Err value
            // Returns Some(error) if Err, None if Ok
            return self.emitResultErrMethod(object, object_type, method);
        }

        // map(f) - applies function f to inner value if Some/Ok
        if (std.mem.eql(u8, method.method_name, "map")) {
            if (method.args.len != 1) {
                return EmitError.InvalidAST;
            }
            const func_val = try self.emitExpr(method.args[0]);
            return self.emitMapMethod(object, object_type, func_val, method);
        }

        // and_then(f) - applies function f and flattens result
        if (std.mem.eql(u8, method.method_name, "and_then")) {
            if (method.args.len != 1) {
                return EmitError.InvalidAST;
            }
            const func_val = try self.emitExpr(method.args[0]);
            return self.emitAndThenMethod(object, object_type, func_val, method);
        }

        // map_err(f) - applies function f to error value if Err
        if (std.mem.eql(u8, method.method_name, "map_err")) {
            if (method.args.len != 1) {
                return EmitError.InvalidAST;
            }
            const func_val = try self.emitExpr(method.args[0]);
            return self.emitMapErrMethod(object, object_type, func_val, method);
        }

        // Eq trait: eq() method for equality comparison
        if (std.mem.eql(u8, method.method_name, "eq")) {
            return self.emitEqMethod(method, object, object_type);
        }

        // Ordered trait: lt(), le(), gt(), ge() methods for comparison
        if (std.mem.eql(u8, method.method_name, "lt") or
            std.mem.eql(u8, method.method_name, "le") or
            std.mem.eql(u8, method.method_name, "gt") or
            std.mem.eql(u8, method.method_name, "ge"))
        {
            return self.emitOrderedMethod(method, object, object_type);
        }

        // Clone trait: clone() method for explicit cloning
        if (std.mem.eql(u8, method.method_name, "clone")) {
            return self.emitCloneMethod(method, object, object_type);
        }

        // Drop trait: drop() method for explicit destruction
        // Only user-defined struct types can have drop() - primitives are trivially dropped
        if (std.mem.eql(u8, method.method_name, "drop")) {
            return self.emitDropMethod(method, object, object_type);
        }

        // Hash trait: hash() method for computing hash codes
        // All primitives have builtin Hash - structs need explicit impl
        if (std.mem.eql(u8, method.method_name, "hash")) {
            return self.emitHashMethod(method, object, object_type);
        }

        // Check for string methods
        // Strings are represented as pointers in LLVM, so we check the LLVM type
        if (self.isStringExpr(method.object)) {
            if (std.mem.eql(u8, method.method_name, "len")) {
                return self.emitStringLen(object);
            }
            if (std.mem.eql(u8, method.method_name, "is_empty")) {
                return self.emitStringIsEmpty(object);
            }
            if (std.mem.eql(u8, method.method_name, "contains")) {
                if (method.args.len != 1) return EmitError.InvalidAST;
                const pattern = try self.emitExpr(method.args[0]);
                return self.emitStringContains(object, pattern);
            }
            if (std.mem.eql(u8, method.method_name, "starts_with")) {
                if (method.args.len != 1) return EmitError.InvalidAST;
                const prefix = try self.emitExpr(method.args[0]);
                return self.emitStringStartsWith(object, prefix);
            }
            if (std.mem.eql(u8, method.method_name, "ends_with")) {
                if (method.args.len != 1) return EmitError.InvalidAST;
                const suffix = try self.emitExpr(method.args[0]);
                return self.emitStringEndsWith(object, suffix);
            }
            if (std.mem.eql(u8, method.method_name, "trim")) {
                return self.emitStringTrim(object);
            }
            if (std.mem.eql(u8, method.method_name, "to_uppercase")) {
                return self.emitStringToUppercase(object);
            }
            if (std.mem.eql(u8, method.method_name, "to_lowercase")) {
                return self.emitStringToLowercase(object);
            }
            if (std.mem.eql(u8, method.method_name, "bytes")) {
                return self.emitStringBytes(object);
            }
            if (std.mem.eql(u8, method.method_name, "chars")) {
                return self.emitStringChars(object);
            }
        }

        // Check for integer methods
        if (self.isIntegerExpr(method.object)) {
            if (std.mem.eql(u8, method.method_name, "abs")) {
                return self.emitIntAbs(object);
            }
            if (std.mem.eql(u8, method.method_name, "min")) {
                if (method.args.len != 1) return EmitError.InvalidAST;
                const other = try self.emitExpr(method.args[0]);
                return self.emitIntMin(object, other);
            }
            if (std.mem.eql(u8, method.method_name, "max")) {
                if (method.args.len != 1) return EmitError.InvalidAST;
                const other = try self.emitExpr(method.args[0]);
                return self.emitIntMax(object, other);
            }
        }

        // Check for Range methods
        if (self.isRangeExpr(method.object)) {
            // For Range methods, we need the alloca pointer, not the loaded value
            const range_ptr = if (method.object == .identifier) blk: {
                if (self.named_values.get(method.object.identifier.name)) |local| {
                    break :blk local.value; // This is the alloca pointer
                }
                break :blk null;
            } else null;

            if (std.mem.eql(u8, method.method_name, "next")) {
                if (range_ptr) |ptr| {
                    return self.emitRangeNext(ptr);
                }
            }
            if (std.mem.eql(u8, method.method_name, "reset")) {
                if (range_ptr) |ptr| {
                    return self.emitRangeReset(ptr);
                }
            }
            if (std.mem.eql(u8, method.method_name, "is_empty")) {
                if (range_ptr) |ptr| {
                    return self.emitRangeIsEmpty(ptr);
                }
            }
            if (std.mem.eql(u8, method.method_name, "len")) {
                if (range_ptr) |ptr| {
                    return self.emitRangeLen(ptr);
                }
            }
        }

        // Check for List methods
        if (self.isListExpr(method.object)) {
            // For List methods, we need the alloca pointer, not the loaded value
            const list_ptr = if (method.object == .identifier) blk: {
                if (self.named_values.get(method.object.identifier.name)) |local| {
                    break :blk local.value; // This is the alloca pointer
                }
                break :blk null;
            } else null;

            if (list_ptr) |ptr| {
                if (std.mem.eql(u8, method.method_name, "len")) {
                    return self.emitListLen(ptr);
                }
                if (std.mem.eql(u8, method.method_name, "is_empty")) {
                    return self.emitListIsEmpty(ptr);
                }
                if (std.mem.eql(u8, method.method_name, "capacity")) {
                    return self.emitListCapacity(ptr);
                }
                if (std.mem.eql(u8, method.method_name, "push")) {
                    if (method.args.len != 1) return EmitError.InvalidAST;
                    const value = try self.emitExpr(method.args[0]);
                    return self.emitListPush(ptr, method, value);
                }
                if (std.mem.eql(u8, method.method_name, "pop")) {
                    return self.emitListPop(ptr, method);
                }
                if (std.mem.eql(u8, method.method_name, "get")) {
                    if (method.args.len != 1) return EmitError.InvalidAST;
                    const index = try self.emitExpr(method.args[0]);
                    return self.emitListGet(ptr, method, index);
                }
                if (std.mem.eql(u8, method.method_name, "set")) {
                    if (method.args.len != 2) return EmitError.InvalidAST;
                    const index = try self.emitExpr(method.args[0]);
                    const value = try self.emitExpr(method.args[1]);
                    return self.emitListSet(ptr, method, index, value);
                }
                if (std.mem.eql(u8, method.method_name, "first")) {
                    return self.emitListFirst(ptr, method);
                }
                if (std.mem.eql(u8, method.method_name, "last")) {
                    return self.emitListLast(ptr, method);
                }
                if (std.mem.eql(u8, method.method_name, "clear")) {
                    return self.emitListClear(ptr);
                }
            }
        }

        // For other methods, fall back to a placeholder for now
        // (User-defined struct methods are checked earlier, before Cell methods)
        // TODO: Implement remaining method types
        return llvm.Const.int32(self.ctx, 0);
    }

    /// Get the struct name from an expression (for method call resolution).
    /// Looks up the struct type name from our cached struct_types map.
    fn getStructNameFromExpr(self: *Emitter, expr: ast.Expr) ?[]const u8 {
        switch (expr) {
            .identifier => |ident| {
                // Look up the variable to get its type
                if (self.named_values.get(ident.name)) |local| {
                    // Search our struct_types map to find which struct has this LLVM type
                    var it = self.struct_types.iterator();
                    while (it.next()) |entry| {
                        if (entry.value_ptr.llvm_type == local.ty) {
                            return entry.key_ptr.*;
                        }
                    }
                }
                return null;
            },
            .struct_literal => |lit| {
                // For struct literals, get the name from the type_name
                if (lit.type_name) |type_name| {
                    switch (type_name) {
                        .named => |n| return n.name,
                        .generic_apply => |g| {
                            // For generic types like Pair[i32, i32], we need the mangled name
                            // which would already be in struct_types
                            if (g.base == .named) {
                                return g.base.named.name;
                            }
                            return null;
                        },
                        else => return null,
                    }
                }
                return null;
            },
            .field => |_| {
                // Field access returns a value, harder to track type
                return null;
            },
            else => return null,
        }
    }

    /// Emit a call to a user-defined struct method.
    fn emitUserDefinedMethod(
        self: *Emitter,
        method: *ast.MethodCall,
        object: llvm.ValueRef,
        struct_name: []const u8,
        struct_method: TypeChecker.StructMethod,
    ) EmitError!llvm.ValueRef {
        // Determine the mangled function name
        // For generic structs, the struct_name already contains the monomorphization (e.g., "Pair$i32$i32")
        // The method name becomes "Pair$i32$i32_get_first"
        var fn_name_buf = std.ArrayListUnmanaged(u8){};
        defer fn_name_buf.deinit(self.allocator);
        fn_name_buf.appendSlice(self.allocator, struct_name) catch return EmitError.OutOfMemory;
        fn_name_buf.append(self.allocator, '_') catch return EmitError.OutOfMemory;
        fn_name_buf.appendSlice(self.allocator, method.method_name) catch return EmitError.OutOfMemory;

        const fn_name = self.allocator.dupeZ(u8, fn_name_buf.items) catch return EmitError.OutOfMemory;
        defer self.allocator.free(fn_name);

        // Get the function from the module
        const callee = self.module.getNamedFunction(fn_name) orelse {
            // Function not found - it might not have been emitted yet
            // This is an error in our compilation order
            return EmitError.InvalidAST;
        };

        // Build arguments: self (if needed) + actual arguments
        var args = std.ArrayListUnmanaged(llvm.ValueRef){};
        defer args.deinit(self.allocator);

        // Add 'self' if method has it
        if (struct_method.has_self) {
            // Check if self is a reference parameter (&self or &mut self)
            // If so, we need to pass a pointer instead of the loaded value
            const func_type = struct_method.func_type;
            const self_is_ref = func_type == .function and
                func_type.function.params.len > 0 and
                func_type.function.params[0] == .reference;

            if (self_is_ref) {
                // For reference self, get the alloca pointer for the object
                const self_ptr = switch (method.object) {
                    .identifier => |ident| blk: {
                        if (self.named_values.get(ident.name)) |local| {
                            break :blk local.value; // This is the alloca pointer
                        }
                        // Fallback: store object to a temp and return pointer
                        const obj_type = llvm.typeOf(object);
                        const alloca = self.builder.buildAlloca(obj_type, "self.tmp");
                        _ = self.builder.buildStore(object, alloca);
                        break :blk alloca;
                    },
                    else => blk: {
                        // For non-identifier objects (field access, etc.), create temp
                        const obj_type = llvm.typeOf(object);
                        const alloca = self.builder.buildAlloca(obj_type, "self.tmp");
                        _ = self.builder.buildStore(object, alloca);
                        break :blk alloca;
                    },
                };
                args.append(self.allocator, self_ptr) catch return EmitError.OutOfMemory;
            } else {
                // For value self, pass the loaded object
                args.append(self.allocator, object) catch return EmitError.OutOfMemory;
            }
        }

        // Add other arguments
        for (method.args) |arg| {
            const arg_val = try self.emitExpr(arg);
            args.append(self.allocator, arg_val) catch return EmitError.OutOfMemory;
        }

        // Call the method
        const fn_type = llvm.c.LLVMGlobalGetValueType(callee);

        // Check if return type is void - LLVM doesn't allow naming void results
        const ret_type = llvm.c.LLVMGetReturnType(fn_type);
        const is_void = llvm.c.LLVMGetTypeKind(ret_type) == llvm.c.LLVMVoidTypeKind;
        const call_name: [:0]const u8 = if (is_void) "" else "method.result";

        return self.builder.buildCall(
            fn_type,
            callee,
            args.items,
            call_name,
        );
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

    // ========================================================================
    // Arc (Atomic Reference Counting) - Thread-safe shared ownership
    // ========================================================================

    /// Emit Arc.new(value) - allocates an Arc with atomic reference counts.
    fn emitArcNew(self: *Emitter, method: *ast.MethodCall) EmitError!llvm.ValueRef {
        if (method.args.len != 1) {
            return EmitError.InvalidAST;
        }

        // Emit the value to be wrapped
        const value = try self.emitExpr(method.args[0]);
        const value_type = llvm.typeOf(value);

        // Declare the Arc runtime function if not already
        const arc_alloc_fn = self.getOrDeclareArcAlloc();

        // Calculate value size based on type
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
        const fn_type = llvm.c.LLVMGlobalGetValueType(arc_alloc_fn);
        const ptr = self.builder.buildCall(
            fn_type,
            arc_alloc_fn,
            &args,
            "arc.alloc",
        );

        // Store the value at the returned pointer
        _ = self.builder.buildStore(value, ptr);

        // Return the pointer (Arc[T] is represented as a pointer)
        return ptr;
    }

    /// Emit arc.clone() - atomically increments reference count.
    fn emitArcClone(self: *Emitter, object: llvm.ValueRef, object_type: llvm.TypeRef) EmitError!llvm.ValueRef {
        _ = object_type;

        // Declare the runtime function
        const arc_clone_fn = self.getOrDeclareArcClone();

        // Call klar_arc_clone(ptr)
        var args = [_]llvm.ValueRef{object};
        return self.builder.buildCall(
            llvm.c.LLVMGlobalGetValueType(arc_clone_fn),
            arc_clone_fn,
            &args,
            "arc.clone",
        );
    }

    /// Emit arc.downgrade() - creates a WeakArc reference.
    fn emitArcDowngrade(self: *Emitter, object: llvm.ValueRef, object_type: llvm.TypeRef) EmitError!llvm.ValueRef {
        _ = object_type;

        // Declare the runtime function
        const arc_downgrade_fn = self.getOrDeclareArcDowngrade();

        // Call klar_arc_downgrade(ptr)
        var args = [_]llvm.ValueRef{object};
        return self.builder.buildCall(
            llvm.c.LLVMGlobalGetValueType(arc_downgrade_fn),
            arc_downgrade_fn,
            &args,
            "arc.downgrade",
        );
    }

    /// Emit weak_arc.upgrade() - attempts to atomically get an Arc from WeakArc.
    fn emitWeakArcUpgrade(self: *Emitter, object: llvm.ValueRef, object_type: llvm.TypeRef) EmitError!llvm.ValueRef {
        _ = object_type;

        // Declare the runtime function
        const weak_arc_upgrade_fn = self.getOrDeclareWeakArcUpgrade();

        // Call klar_weak_arc_upgrade(ptr) - returns null if Arc is gone
        var args = [_]llvm.ValueRef{object};
        const result_ptr = self.builder.buildCall(
            llvm.c.LLVMGlobalGetValueType(weak_arc_upgrade_fn),
            weak_arc_upgrade_fn,
            &args,
            "weak_arc.upgrade",
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
        const return_type = try self.typeExprToLLVM(closure.return_type);

        // Build parameter types for the lifted function
        // First parameter is the environment pointer (for captured variables)
        var param_types = std.ArrayListUnmanaged(llvm.TypeRef){};
        defer param_types.deinit(self.allocator);

        // Add environment pointer as first parameter
        param_types.append(self.allocator, llvm.Types.pointer(self.ctx)) catch return EmitError.OutOfMemory;

        // Add closure's declared parameters
        for (closure.params) |param| {
            const param_ty = try self.typeExprToLLVM(param.type_);
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
            .is_optional = closure.return_type == .optional,
            .inner_type = if (closure.return_type == .optional)
                try self.typeExprToLLVM(closure.return_type.optional.inner)
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
    fn isTypeExprSigned(self: *Emitter, type_expr: ast.TypeExpr) bool {
        _ = self;
        return switch (type_expr) {
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

    fn getOrDeclareRealloc(self: *Emitter) llvm.ValueRef {
        const fn_name = "realloc";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // void* realloc(void* ptr, size_t size)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        var param_types = [_]llvm.TypeRef{ ptr_type, i64_type };
        const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 2, 0);
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

    // ========================================================================
    // Arc (Atomic Reference Counting) Runtime Function Implementations
    // ========================================================================
    // These implement thread-safe atomic reference counting using LLVM atomics.
    // Arc uses the same memory layout as Rc: [strong_count:i64][weak_count:i64][value]
    // but uses atomic operations for thread safety.

    fn getOrDeclareArcAlloc(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_arc_alloc";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // Define: *anyopaque klar_arc_alloc(usize value_size, usize value_align)
        // Same layout as Rc: [strong_count:i64][weak_count:i64][value:value_size]
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

        // Store strong_count = 1 at offset 0 (using atomic store for consistency)
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

    fn getOrDeclareArcClone(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_arc_clone";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // Define: *anyopaque klar_arc_clone(*anyopaque ptr)
        // Atomically increments strong_count and returns ptr
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

        // Atomic increment: atomicrmw add i64* %strong_ptr, i64 1 monotonic
        _ = llvm.c.LLVMBuildAtomicRMW(
            self.builder.ref,
            llvm.c.LLVMAtomicRMWBinOpAdd,
            strong_ptr,
            llvm.Const.int64(self.ctx, 1),
            llvm.c.LLVMAtomicOrderingMonotonic,
            0, // not single threaded
        );

        // Return the original pointer
        _ = llvm.c.LLVMBuildRet(self.builder.ref, value_ptr);

        // Restore builder position
        if (saved_bb) |bb| {
            llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, bb);
        }
        self.current_function = saved_func;

        return func;
    }

    fn getOrDeclareArcDowngrade(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_arc_downgrade";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // Define: *anyopaque klar_arc_downgrade(*anyopaque ptr)
        // Atomically increments weak_count (at header offset -8 from value ptr) and returns ptr
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

        // Calculate weak_count address: value_ptr - 8
        const ptr_int = llvm.c.LLVMBuildPtrToInt(self.builder.ref, value_ptr, i64_type, "ptr_int");
        const weak_off = llvm.c.LLVMBuildSub(self.builder.ref, ptr_int, llvm.Const.int64(self.ctx, 8), "weak_off");
        const weak_ptr = llvm.c.LLVMBuildIntToPtr(self.builder.ref, weak_off, llvm.c.LLVMPointerType(i64_type, 0), "weak_ptr");

        // Atomic increment weak_count
        _ = llvm.c.LLVMBuildAtomicRMW(
            self.builder.ref,
            llvm.c.LLVMAtomicRMWBinOpAdd,
            weak_ptr,
            llvm.Const.int64(self.ctx, 1),
            llvm.c.LLVMAtomicOrderingMonotonic,
            0,
        );

        // Return the original pointer
        _ = llvm.c.LLVMBuildRet(self.builder.ref, value_ptr);

        // Restore builder position
        if (saved_bb) |bb| {
            llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, bb);
        }
        self.current_function = saved_func;

        return func;
    }

    fn getOrDeclareWeakArcUpgrade(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_weak_arc_upgrade";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // Define: *anyopaque klar_weak_arc_upgrade(*anyopaque ptr)
        // Atomically increments strong_count if > 0, returns ptr or null
        // This uses a compare-and-swap loop for thread safety
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        var param_types = [_]llvm.TypeRef{ptr_type};
        const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, param_types.len, 0);
        const func = llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);

        // Create basic blocks for CAS loop
        const entry_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "entry");
        const loop_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "loop");
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

        // Initial load with acquire ordering
        const initial_count = llvm.c.LLVMBuildLoad2(self.builder.ref, i64_type, strong_ptr, "initial_count");
        llvm.c.LLVMSetOrdering(initial_count, llvm.c.LLVMAtomicOrderingAcquire);
        _ = llvm.c.LLVMBuildBr(self.builder.ref, loop_bb);

        // Loop: check if count > 0 and try CAS
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, loop_bb);
        const current_phi = llvm.c.LLVMBuildPhi(self.builder.ref, i64_type, "current");

        // Check if count > 0
        const is_alive = llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntUGT, current_phi, llvm.Const.int64(self.ctx, 0), "is_alive");
        _ = llvm.c.LLVMBuildCondBr(self.builder.ref, is_alive, success_bb, fail_bb);

        // Success: try to increment with CAS
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, success_bb);
        const new_count = llvm.c.LLVMBuildAdd(self.builder.ref, current_phi, llvm.Const.int64(self.ctx, 1), "new_count");

        // Use cmpxchg for thread-safe increment
        const cmpxchg_result = llvm.c.LLVMBuildAtomicCmpXchg(
            self.builder.ref,
            strong_ptr,
            current_phi,
            new_count,
            llvm.c.LLVMAtomicOrderingAcquireRelease,
            llvm.c.LLVMAtomicOrderingMonotonic,
            0, // not single threaded
        );

        // Extract success flag and value
        const success_flag = llvm.c.LLVMBuildExtractValue(self.builder.ref, cmpxchg_result, 1, "success_flag");
        const loaded_val = llvm.c.LLVMBuildExtractValue(self.builder.ref, cmpxchg_result, 0, "loaded_val");

        // If CAS succeeded, return ptr; otherwise retry with loaded value
        const ret_or_retry_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "ret_success");
        _ = llvm.c.LLVMBuildCondBr(self.builder.ref, success_flag, ret_or_retry_bb, loop_bb);

        // Return success
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, ret_or_retry_bb);
        _ = llvm.c.LLVMBuildRet(self.builder.ref, value_ptr);

        // Fail: return null
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, fail_bb);
        _ = llvm.c.LLVMBuildRet(self.builder.ref, llvm.c.LLVMConstNull(ptr_type));

        // Add phi incoming values
        var phi_values = [_]llvm.ValueRef{ initial_count, loaded_val };
        var phi_blocks = [_]llvm.c.LLVMBasicBlockRef{ entry_bb, success_bb };
        llvm.c.LLVMAddIncoming(current_phi, &phi_values, &phi_blocks, 2);

        // Restore builder position
        if (saved_bb) |bb| {
            llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, bb);
        }
        self.current_function = saved_func;

        return func;
    }

    fn getOrDeclareArcDrop(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_arc_drop";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // Define: void klar_arc_drop(*anyopaque ptr, usize size, usize align, *fn destructor)
        // Atomically decrements strong_count. If 0, calls destructor, decrements weak_count.
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
        _ = llvm.c.LLVMGetParam(func, 1); // value_size
        _ = llvm.c.LLVMGetParam(func, 2); // value_align
        const destructor = llvm.c.LLVMGetParam(func, 3);

        // Calculate strong_count address: value_ptr - 16
        const ptr_int = llvm.c.LLVMBuildPtrToInt(self.builder.ref, value_ptr, i64_type, "ptr_int");
        const header_off = llvm.c.LLVMBuildSub(self.builder.ref, ptr_int, llvm.Const.int64(self.ctx, 16), "header_off");
        const strong_ptr = llvm.c.LLVMBuildIntToPtr(self.builder.ref, header_off, llvm.c.LLVMPointerType(i64_type, 0), "strong_ptr");

        // Atomic decrement with release ordering
        const prev_strong = llvm.c.LLVMBuildAtomicRMW(
            self.builder.ref,
            llvm.c.LLVMAtomicRMWBinOpSub,
            strong_ptr,
            llvm.Const.int64(self.ctx, 1),
            llvm.c.LLVMAtomicOrderingRelease,
            0,
        );

        // Check if we were the last strong reference (prev was 1)
        const was_one = llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntEQ, prev_strong, llvm.Const.int64(self.ctx, 1), "was_one");
        _ = llvm.c.LLVMBuildCondBr(self.builder.ref, was_one, strong_zero_bb, done_bb);

        // strong_zero: acquire fence, call destructor if not null, decrement weak_count
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, strong_zero_bb);
        // Acquire fence to synchronize with other threads' releases
        _ = llvm.c.LLVMBuildFence(self.builder.ref, llvm.c.LLVMAtomicOrderingAcquire, 0, "");

        const dtor_null = llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntEQ, destructor, llvm.c.LLVMConstNull(ptr_type), "dtor_null");
        _ = llvm.c.LLVMBuildCondBr(self.builder.ref, dtor_null, after_dtor_bb, call_dtor_bb);

        // call_dtor: call destructor(value_ptr)
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, call_dtor_bb);
        var dtor_param_types = [_]llvm.TypeRef{ptr_type};
        const dtor_fn_type = llvm.c.LLVMFunctionType(void_type, &dtor_param_types, 1, 0);
        var dtor_args = [_]llvm.ValueRef{value_ptr};
        _ = llvm.c.LLVMBuildCall2(self.builder.ref, dtor_fn_type, destructor, &dtor_args, 1, "");
        _ = llvm.c.LLVMBuildBr(self.builder.ref, after_dtor_bb);

        // after_dtor: atomic decrement weak_count
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, after_dtor_bb);
        const weak_off = llvm.c.LLVMBuildSub(self.builder.ref, ptr_int, llvm.Const.int64(self.ctx, 8), "weak_off");
        const weak_ptr = llvm.c.LLVMBuildIntToPtr(self.builder.ref, weak_off, llvm.c.LLVMPointerType(i64_type, 0), "weak_ptr");

        const prev_weak = llvm.c.LLVMBuildAtomicRMW(
            self.builder.ref,
            llvm.c.LLVMAtomicRMWBinOpSub,
            weak_ptr,
            llvm.Const.int64(self.ctx, 1),
            llvm.c.LLVMAtomicOrderingRelease,
            0,
        );

        // Check if weak_count was 1 (now 0)
        const weak_was_one = llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntEQ, prev_weak, llvm.Const.int64(self.ctx, 1), "weak_was_one");
        _ = llvm.c.LLVMBuildCondBr(self.builder.ref, weak_was_one, free_bb, done_bb);

        // free: free the header pointer
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, free_bb);
        // Acquire fence before freeing
        _ = llvm.c.LLVMBuildFence(self.builder.ref, llvm.c.LLVMAtomicOrderingAcquire, 0, "");
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

    fn getOrDeclareWeakArcDrop(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_weak_arc_drop";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // Define: void klar_weak_arc_drop(*anyopaque ptr, usize size, usize align)
        // Atomically decrements weak_count. If 0 and strong_count is 0, frees allocation.
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        const void_type = llvm.Types.void_(self.ctx);
        var param_types = [_]llvm.TypeRef{ ptr_type, i64_type, i64_type };
        const fn_type = llvm.c.LLVMFunctionType(void_type, &param_types, 3, 0);
        const func = llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);

        // Create basic blocks
        const entry_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "entry");
        const check_strong_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "check_strong");
        const free_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "free");
        const done_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "done");

        const saved_bb = llvm.c.LLVMGetInsertBlock(self.builder.ref);
        const saved_func = self.current_function;

        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, entry_bb);

        // Get parameters
        const value_ptr = llvm.c.LLVMGetParam(func, 0);
        _ = llvm.c.LLVMGetParam(func, 1); // value_size
        _ = llvm.c.LLVMGetParam(func, 2); // value_align

        // Calculate addresses
        const ptr_int = llvm.c.LLVMBuildPtrToInt(self.builder.ref, value_ptr, i64_type, "ptr_int");
        const weak_off = llvm.c.LLVMBuildSub(self.builder.ref, ptr_int, llvm.Const.int64(self.ctx, 8), "weak_off");
        const weak_ptr = llvm.c.LLVMBuildIntToPtr(self.builder.ref, weak_off, llvm.c.LLVMPointerType(i64_type, 0), "weak_ptr");

        // Atomic decrement weak_count
        const prev_weak = llvm.c.LLVMBuildAtomicRMW(
            self.builder.ref,
            llvm.c.LLVMAtomicRMWBinOpSub,
            weak_ptr,
            llvm.Const.int64(self.ctx, 1),
            llvm.c.LLVMAtomicOrderingRelease,
            0,
        );

        // Check if weak_count was 1
        const weak_was_one = llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntEQ, prev_weak, llvm.Const.int64(self.ctx, 1), "weak_was_one");
        _ = llvm.c.LLVMBuildCondBr(self.builder.ref, weak_was_one, check_strong_bb, done_bb);

        // check_strong: check if strong_count is also 0
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, check_strong_bb);
        // Acquire fence
        _ = llvm.c.LLVMBuildFence(self.builder.ref, llvm.c.LLVMAtomicOrderingAcquire, 0, "");

        const header_off = llvm.c.LLVMBuildSub(self.builder.ref, ptr_int, llvm.Const.int64(self.ctx, 16), "header_off");
        const strong_ptr = llvm.c.LLVMBuildIntToPtr(self.builder.ref, header_off, llvm.c.LLVMPointerType(i64_type, 0), "strong_ptr");
        const strong_count = llvm.c.LLVMBuildLoad2(self.builder.ref, i64_type, strong_ptr, "strong_count");
        llvm.c.LLVMSetOrdering(strong_count, llvm.c.LLVMAtomicOrderingAcquire);

        const strong_zero = llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntEQ, strong_count, llvm.Const.int64(self.ctx, 0), "strong_zero");
        _ = llvm.c.LLVMBuildCondBr(self.builder.ref, strong_zero, free_bb, done_bb);

        // free: free the header pointer
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

    /// Emit an interpolated string expression.
    /// For each part, emit either a string literal or the expression converted to string.
    /// Uses snprintf to build the final string in a stack buffer.
    fn emitInterpolatedString(self: *Emitter, interp: *ast.InterpolatedString) EmitError!llvm.ValueRef {
        if (interp.parts.len == 0) {
            return self.builder.buildGlobalStringPtr("", "empty_interp");
        }

        // Simple case: single string literal part
        if (interp.parts.len == 1) {
            switch (interp.parts[0]) {
                .string => |s| {
                    const str_z = self.allocator.dupeZ(u8, s) catch return EmitError.OutOfMemory;
                    defer self.allocator.free(str_z);
                    return self.builder.buildGlobalStringPtr(str_z, "interp_str");
                },
                .expr => |e| {
                    // Single expression - emit as format string with printf-style format
                    return self.emitExprAsString(e);
                },
            }
        }

        // Multiple parts: build format string and args for snprintf
        // We'll use a fixed-size stack buffer and snprintf
        var format_parts = std.ArrayListUnmanaged(u8){};
        defer format_parts.deinit(self.allocator);

        var expr_values = std.ArrayListUnmanaged(llvm.ValueRef){};
        defer expr_values.deinit(self.allocator);

        for (interp.parts) |part| {
            switch (part) {
                .string => |s| {
                    // Escape any % in the string for printf format
                    for (s) |c| {
                        if (c == '%') {
                            format_parts.append(self.allocator, '%') catch return EmitError.OutOfMemory;
                            format_parts.append(self.allocator, '%') catch return EmitError.OutOfMemory;
                        } else {
                            format_parts.append(self.allocator, c) catch return EmitError.OutOfMemory;
                        }
                    }
                },
                .expr => |e| {
                    // Emit the expression and determine format specifier
                    const val = try self.emitExpr(e);
                    expr_values.append(self.allocator, val) catch return EmitError.OutOfMemory;

                    // Determine the format specifier based on the expression type
                    const val_type = llvm.c.LLVMTypeOf(val);
                    const type_kind = llvm.c.LLVMGetTypeKind(val_type);

                    const format_spec: []const u8 = switch (type_kind) {
                        llvm.c.LLVMIntegerTypeKind => blk: {
                            const bit_width = llvm.c.LLVMGetIntTypeWidth(val_type);
                            if (bit_width == 1) {
                                // Bool - we need special handling
                                break :blk "%d";
                            } else if (bit_width <= 32) {
                                break :blk "%d";
                            } else {
                                break :blk "%lld";
                            }
                        },
                        llvm.c.LLVMFloatTypeKind, llvm.c.LLVMDoubleTypeKind => "%g",
                        llvm.c.LLVMPointerTypeKind => "%s",
                        else => "%d", // Default to int
                    };

                    for (format_spec) |c| {
                        format_parts.append(self.allocator, c) catch return EmitError.OutOfMemory;
                    }
                },
            }
        }

        // Create the format string
        format_parts.append(self.allocator, 0) catch return EmitError.OutOfMemory; // null terminate
        const format_str = self.builder.buildGlobalStringPtr(format_parts.items[0 .. format_parts.items.len - 1 :0], "interp_fmt");

        // Allocate a buffer on the stack for the result
        const buffer_size: u64 = 1024;
        const i8_type = llvm.Types.int8(self.ctx);
        const buffer_type = llvm.Types.array(i8_type, buffer_size);
        const buffer = self.builder.buildAlloca(buffer_type, "interp_buf");

        // Get pointer to buffer start
        var indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const buffer_ptr = self.builder.buildGEP(buffer_type, buffer, &indices, "buf_ptr");

        // Call snprintf(buffer, size, format, args...)
        const snprintf_fn = self.getOrDeclareSnprintf();
        const i64_type = llvm.Types.int64(self.ctx);
        const size_val = llvm.Const.int(i64_type, buffer_size, false);

        var call_args = std.ArrayListUnmanaged(llvm.ValueRef){};
        defer call_args.deinit(self.allocator);
        call_args.append(self.allocator, buffer_ptr) catch return EmitError.OutOfMemory;
        call_args.append(self.allocator, size_val) catch return EmitError.OutOfMemory;
        call_args.append(self.allocator, format_str) catch return EmitError.OutOfMemory;
        for (expr_values.items) |val| {
            call_args.append(self.allocator, val) catch return EmitError.OutOfMemory;
        }

        const fn_type = llvm.c.LLVMGlobalGetValueType(snprintf_fn);
        _ = self.builder.buildCall(fn_type, snprintf_fn, call_args.items, "");

        return buffer_ptr;
    }

    /// Emit a builtin call like @typeName, @typeInfo, @hasField, etc.
    /// Also handles user-defined comptime function calls via @name(...) syntax.
    /// These are evaluated at compile time and the results are stored in type_checker.
    fn emitBuiltinCall(self: *Emitter, bc: *ast.BuiltinCall) EmitError!llvm.ValueRef {
        // Look up the precomputed result from the type checker
        if (self.type_checker) |tc| {
            // Check for user-defined comptime function call results first
            if (tc.comptime_builtin_values.get(bc)) |comptime_value| {
                return self.emitComptimeValue(comptime_value);
            }
            // Check for string results (e.g., @typeName)
            if (tc.comptime_strings.get(bc)) |computed_string| {
                // Emit as a global string constant
                const str_z = self.allocator.dupeZ(u8, computed_string) catch return EmitError.OutOfMemory;
                defer self.allocator.free(str_z);
                return self.builder.buildGlobalStringPtr(str_z, "comptime_str");
            }
            // Check for boolean results (e.g., @hasField)
            if (tc.comptime_bools.get(bc)) |computed_bool| {
                return llvm.Const.int1(self.ctx, computed_bool);
            }
            // Check for integer results (e.g., @sizeOf, @alignOf)
            if (tc.comptime_ints.get(bc)) |computed_int| {
                return llvm.Const.int32(self.ctx, @intCast(computed_int));
            }
        }
        // Fallback: return 0 if not found
        return llvm.Const.int32(self.ctx, 0);
    }

    /// Emit a ComptimeValue as an LLVM constant.
    fn emitComptimeValue(self: *Emitter, comptime_value: TypeChecker.ComptimeValue) EmitError!llvm.ValueRef {
        return switch (comptime_value) {
            .int => |i| if (i.is_i32)
                llvm.Const.int32(self.ctx, @intCast(i.value))
            else
                llvm.Const.int64(self.ctx, @intCast(i.value)),
            .float => |f| llvm.Const.float64(self.ctx, f),
            .bool_ => |b| llvm.Const.int1(self.ctx, b),
            .string => |s| blk: {
                const str_z = self.allocator.dupeZ(u8, s) catch return EmitError.OutOfMemory;
                defer self.allocator.free(str_z);
                break :blk self.builder.buildGlobalStringPtr(str_z, "comptime_str");
            },
            .void_ => llvm.Const.int32(self.ctx, 0),
            .struct_ => |cs| blk: {
                // Get the LLVM struct type
                const struct_info = self.struct_types.get(cs.type_name) orelse {
                    break :blk llvm.Const.int32(self.ctx, 0); // Fallback
                };
                const struct_type = struct_info.llvm_type;

                // Build constant field values in the correct order
                var field_values = std.ArrayListUnmanaged(llvm.ValueRef){};
                defer field_values.deinit(self.allocator);

                // Iterate through struct fields in declaration order
                for (struct_info.field_names) |field_name| {
                    if (cs.fields.get(field_name)) |field_cv| {
                        const field_val = try self.emitComptimeValue(field_cv);
                        field_values.append(self.allocator, field_val) catch return EmitError.OutOfMemory;
                    } else {
                        // Field not found - use zero
                        field_values.append(self.allocator, llvm.Const.int32(self.ctx, 0)) catch return EmitError.OutOfMemory;
                    }
                }

                // Create constant struct
                break :blk llvm.Const.namedStruct(struct_type, field_values.items);
            },
            .array => |arr| blk: {
                // Get the LLVM element type
                const element_llvm_type = self.typeToLLVM(arr.element_type);

                // Recursively emit each element as a constant
                var element_values = std.ArrayListUnmanaged(llvm.ValueRef){};
                defer element_values.deinit(self.allocator);

                for (arr.elements) |elem| {
                    const elem_val = try self.emitComptimeValue(elem);
                    element_values.append(self.allocator, elem_val) catch return EmitError.OutOfMemory;
                }

                // Create constant array
                break :blk llvm.Const.array(element_llvm_type, element_values.items);
            },
        };
    }

    /// Emit a comptime block.
    /// These are evaluated at compile time by the type checker and the results are stored.
    fn emitComptimeBlock(self: *Emitter, cb: *ast.ComptimeBlock) EmitError!llvm.ValueRef {
        // Look up the precomputed result from the type checker
        if (self.type_checker) |tc| {
            if (tc.comptime_values.get(cb)) |comptime_value| {
                return self.emitComptimeValue(comptime_value);
            }
        }
        // Fallback: return 0 if not found (should not happen if type checking passed)
        return llvm.Const.int32(self.ctx, 0);
    }

    /// Emit an expression and convert it to a string for interpolation.
    /// Returns a pointer to a string representation.
    fn emitExprAsString(self: *Emitter, expr: ast.Expr) EmitError!llvm.ValueRef {
        const val = try self.emitExpr(expr);
        const val_type = llvm.c.LLVMTypeOf(val);
        const type_kind = llvm.c.LLVMGetTypeKind(val_type);

        // If already a pointer (string), return as-is
        if (type_kind == llvm.c.LLVMPointerTypeKind) {
            return val;
        }

        // For other types, use sprintf to convert
        const buffer_size: u64 = 64;
        const i8_type = llvm.Types.int8(self.ctx);
        const buffer_type = llvm.Types.array(i8_type, buffer_size);
        const buffer = self.builder.buildAlloca(buffer_type, "tostr_buf");

        var indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const buffer_ptr = self.builder.buildGEP(buffer_type, buffer, &indices, "buf_ptr");

        const format_spec: [:0]const u8 = switch (type_kind) {
            llvm.c.LLVMIntegerTypeKind => blk: {
                const bit_width = llvm.c.LLVMGetIntTypeWidth(val_type);
                if (bit_width <= 32) {
                    break :blk "%d";
                } else {
                    break :blk "%lld";
                }
            },
            llvm.c.LLVMFloatTypeKind, llvm.c.LLVMDoubleTypeKind => "%g",
            else => "%d",
        };

        const format_str = self.builder.buildGlobalStringPtr(format_spec, "tostr_fmt");

        const snprintf_fn = self.getOrDeclareSnprintf();
        const i64_type = llvm.Types.int64(self.ctx);
        const size_val = llvm.Const.int(i64_type, buffer_size, false);

        var call_args = [_]llvm.ValueRef{ buffer_ptr, size_val, format_str, val };
        const fn_type = llvm.c.LLVMGlobalGetValueType(snprintf_fn);
        _ = self.builder.buildCall(fn_type, snprintf_fn, &call_args, "");

        return buffer_ptr;
    }

    fn getOrDeclareSnprintf(self: *Emitter) llvm.ValueRef {
        const fn_name = "snprintf";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // int snprintf(char *str, size_t size, const char *format, ...)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);
        var param_types = [_]llvm.TypeRef{ ptr_type, i64_type, ptr_type };
        // variadic = 1 (true)
        const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 3, 1);
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

    /// Emit assert_eq that compares two values and panics with details on failure
    fn emitAssertEq(self: *Emitter, args: []const ast.Expr) EmitError!llvm.ValueRef {
        if (args.len != 2) {
            return llvm.Const.int32(self.ctx, 0);
        }

        const func = self.current_function orelse return EmitError.InvalidAST;

        // Evaluate both arguments
        const left = try self.emitExpr(args[0]);
        const right = try self.emitExpr(args[1]);

        // Get the type for comparison
        const left_type = llvm.typeOf(left);
        const type_kind = llvm.c.LLVMGetTypeKind(left_type);

        // Create comparison based on type
        const cond = switch (type_kind) {
            llvm.c.LLVMIntegerTypeKind => self.builder.buildICmp(llvm.c.LLVMIntEQ, left, right, "assert_eq.cmp"),
            llvm.c.LLVMFloatTypeKind, llvm.c.LLVMDoubleTypeKind => self.builder.buildFCmp(llvm.c.LLVMRealOEQ, left, right, "assert_eq.cmp"),
            llvm.c.LLVMPointerTypeKind => self.builder.buildICmp(llvm.c.LLVMIntEQ, left, right, "assert_eq.cmp"),
            else => self.builder.buildICmp(llvm.c.LLVMIntEQ, left, right, "assert_eq.cmp"),
        };

        // Create blocks
        const fail_bb = llvm.appendBasicBlock(self.ctx, func, "assert_eq.fail");
        const continue_bb = llvm.appendBasicBlock(self.ctx, func, "assert_eq.continue");

        // Branch based on condition
        _ = self.builder.buildCondBr(cond, continue_bb, fail_bb);

        // Emit failure block
        self.builder.positionAtEnd(fail_bb);

        // Print assertion failure message to stderr with values
        const fprintf_fn = self.getOrDeclareFprintf();
        const stderr_fn = self.getOrDeclareStderr();
        const stderr = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(stderr_fn), stderr_fn, &[_]llvm.ValueRef{}, "stderr");

        // Format string based on type
        const fmt_str = switch (type_kind) {
            llvm.c.LLVMIntegerTypeKind => self.builder.buildGlobalStringPtr("assertion failed: left = %lld, right = %lld\n", "assert_eq_fmt"),
            llvm.c.LLVMFloatTypeKind, llvm.c.LLVMDoubleTypeKind => self.builder.buildGlobalStringPtr("assertion failed: left = %g, right = %g\n", "assert_eq_fmt"),
            llvm.c.LLVMPointerTypeKind => self.builder.buildGlobalStringPtr("assertion failed: left = %p, right = %p\n", "assert_eq_fmt"),
            else => self.builder.buildGlobalStringPtr("assertion failed: values not equal\n", "assert_eq_fmt"),
        };

        // For integer types, extend to i64 for printf compatibility
        var left_val = left;
        var right_val = right;
        if (type_kind == llvm.c.LLVMIntegerTypeKind) {
            const i64_type = llvm.Types.int64(self.ctx);
            const bit_width = llvm.c.LLVMGetIntTypeWidth(left_type);
            if (bit_width < 64) {
                left_val = llvm.c.LLVMBuildSExt(self.builder.ref, left, i64_type, "left_ext");
                right_val = llvm.c.LLVMBuildSExt(self.builder.ref, right, i64_type, "right_ext");
            }
        }

        var fail_args = [_]llvm.ValueRef{ stderr, fmt_str, left_val, right_val };
        _ = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(fprintf_fn), fprintf_fn, &fail_args, "");

        // Call abort
        const abort_fn = self.getOrDeclareAbort();
        _ = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(abort_fn), abort_fn, &[_]llvm.ValueRef{}, "");
        _ = self.builder.buildUnreachable();

        // Continue block - assertion passed
        self.builder.positionAtEnd(continue_bb);

        return llvm.Const.int32(self.ctx, 0);
    }

    /// Emit dbg(value) - prints value with debug info and returns value
    fn emitDbg(self: *Emitter, args: []const ast.Expr) EmitError!llvm.ValueRef {
        if (args.len != 1) {
            return llvm.Const.int32(self.ctx, 0);
        }

        // Emit the argument
        const value = try self.emitExpr(args[0]);
        const value_type = llvm.typeOf(value);
        const type_kind = llvm.c.LLVMGetTypeKind(value_type);

        // Print to stderr
        const fprintf_fn = self.getOrDeclareFprintf();
        const stderr_fn = self.getOrDeclareStderr();
        const stderr = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(stderr_fn), stderr_fn, &[_]llvm.ValueRef{}, "stderr");

        // Format string based on type
        const fmt_str = switch (type_kind) {
            llvm.c.LLVMIntegerTypeKind => blk: {
                const bit_width = llvm.c.LLVMGetIntTypeWidth(value_type);
                if (bit_width == 1) {
                    break :blk self.builder.buildGlobalStringPtr("[dbg] %s\n", "dbg_fmt");
                }
                break :blk self.builder.buildGlobalStringPtr("[dbg] %lld\n", "dbg_fmt");
            },
            llvm.c.LLVMFloatTypeKind, llvm.c.LLVMDoubleTypeKind => self.builder.buildGlobalStringPtr("[dbg] %g\n", "dbg_fmt"),
            llvm.c.LLVMPointerTypeKind => self.builder.buildGlobalStringPtr("[dbg] %p\n", "dbg_fmt"),
            else => self.builder.buildGlobalStringPtr("[dbg] <value>\n", "dbg_fmt"),
        };

        // For integer types, extend to i64 for printf compatibility (except bool)
        var print_val = value;
        if (type_kind == llvm.c.LLVMIntegerTypeKind) {
            const bit_width = llvm.c.LLVMGetIntTypeWidth(value_type);
            if (bit_width == 1) {
                // Bool - use conditional to select "true" or "false"
                const true_str = self.builder.buildGlobalStringPtr("true", "true_str");
                const false_str = self.builder.buildGlobalStringPtr("false", "false_str");
                print_val = self.builder.buildSelect(value, true_str, false_str, "bool_str");
            } else if (bit_width < 64) {
                const i64_type = llvm.Types.int64(self.ctx);
                print_val = llvm.c.LLVMBuildSExt(self.builder.ref, value, i64_type, "val_ext");
            }
        }

        var dbg_args = [_]llvm.ValueRef{ stderr, fmt_str, print_val };
        _ = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(fprintf_fn), fprintf_fn, &dbg_args, "");

        // Return the original value (pass-through)
        return value;
    }

    /// Emit type_name(value) - returns the type name as a string
    fn emitTypeName(self: *Emitter, args: []const ast.Expr) EmitError!llvm.ValueRef {
        if (args.len != 1) {
            return self.builder.buildGlobalStringPtr("unknown", "type_name");
        }

        // Emit the argument to get its type
        const value = try self.emitExpr(args[0]);
        const value_type = llvm.typeOf(value);
        const type_kind = llvm.c.LLVMGetTypeKind(value_type);

        // Return type name string based on LLVM type
        const type_name = switch (type_kind) {
            llvm.c.LLVMIntegerTypeKind => blk: {
                const bit_width = llvm.c.LLVMGetIntTypeWidth(value_type);
                break :blk switch (bit_width) {
                    1 => "bool",
                    8 => "i8",
                    16 => "i16",
                    32 => "i32",
                    64 => "i64",
                    128 => "i128",
                    else => "int",
                };
            },
            llvm.c.LLVMFloatTypeKind => "f32",
            llvm.c.LLVMDoubleTypeKind => "f64",
            llvm.c.LLVMPointerTypeKind => "ptr",
            llvm.c.LLVMStructTypeKind => "struct",
            llvm.c.LLVMArrayTypeKind => "array",
            llvm.c.LLVMVoidTypeKind => "void",
            else => "unknown",
        };

        return self.builder.buildGlobalStringPtr(type_name, "type_name_str");
    }

    /// Emit len(value) - returns the length of a string or array
    fn emitLen(self: *Emitter, args: []const ast.Expr) EmitError!llvm.ValueRef {
        if (args.len != 1) {
            return llvm.Const.int32(self.ctx, 0);
        }

        // Emit the argument
        const value = try self.emitExpr(args[0]);
        const value_type = llvm.typeOf(value);
        const type_kind = llvm.c.LLVMGetTypeKind(value_type);

        // For arrays, return the compile-time length
        if (type_kind == llvm.c.LLVMArrayTypeKind) {
            const arr_len = llvm.c.LLVMGetArrayLength2(value_type);
            return llvm.Const.int32(self.ctx, @intCast(arr_len));
        }

        // For strings (pointers), use strlen and truncate to i32
        if (type_kind == llvm.c.LLVMPointerTypeKind) {
            const strlen_fn = self.getOrDeclareStrlen();
            var strlen_args = [_]llvm.ValueRef{value};
            const len_i64 = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(strlen_fn), strlen_fn, &strlen_args, "strlen");
            // Truncate from size_t (i64) to i32
            const i32_type = llvm.Types.int32(self.ctx);
            return llvm.c.LLVMBuildTrunc(self.builder.ref, len_i64, i32_type, "strlen_i32");
        }

        // For other types, return 0
        return llvm.Const.int32(self.ctx, 0);
    }

    fn getOrDeclareStrlen(self: *Emitter) llvm.ValueRef {
        const fn_name = "strlen";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // size_t strlen(const char *s)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const size_type = llvm.Types.int64(self.ctx);
        var param_types = [_]llvm.TypeRef{ptr_type};
        const fn_type = llvm.c.LLVMFunctionType(size_type, &param_types, 1, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    /// Declare strstr if not already declared.
    fn getOrDeclareStrstr(self: *Emitter) llvm.ValueRef {
        const fn_name = "strstr";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // char *strstr(const char *haystack, const char *needle)
        const ptr_type = llvm.Types.pointer(self.ctx);
        var param_types = [_]llvm.TypeRef{ ptr_type, ptr_type };
        const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 2, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    /// Declare strncmp if not already declared.
    fn getOrDeclareStrncmp(self: *Emitter) llvm.ValueRef {
        const fn_name = "strncmp";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // int strncmp(const char *s1, const char *s2, size_t n)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);
        const size_type = llvm.Types.int64(self.ctx);
        var param_types = [_]llvm.TypeRef{ ptr_type, ptr_type, size_type };
        const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 3, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    /// Check if an expression is a string type.
    /// Checks string literals directly and uses named_values for identifiers.
    fn isStringExpr(self: *Emitter, expr: ast.Expr) bool {
        switch (expr) {
            .literal => |lit| {
                // String literals are always string type
                return lit.kind == .string;
            },
            .identifier => |id| {
                // Check if we have type info for this identifier in named_values
                if (self.named_values.get(id.name)) |local| {
                    // Check if the LLVM type is a pointer (strings are pointers)
                    // and we've marked this as a string type
                    if (local.is_string) {
                        return true;
                    }
                }
                // Fallback: use type checker if available
                if (self.type_checker) |tc| {
                    // We need to cast away const since checkExpr modifies internal state
                    // This is safe because the type checker has already processed the AST
                    const tc_mut = @constCast(tc);
                    const expr_type = tc_mut.checkExpr(expr);
                    return expr_type == .primitive and expr_type.primitive == .string_;
                }
                return false;
            },
            .interpolated_string => {
                // Interpolated strings are always string type
                return true;
            },
            .method_call => |m| {
                // If the method returns a string (like trim, to_uppercase, etc.),
                // the result is a string
                if (std.mem.eql(u8, m.method_name, "trim") or
                    std.mem.eql(u8, m.method_name, "to_uppercase") or
                    std.mem.eql(u8, m.method_name, "to_lowercase"))
                {
                    // Only if the object is also a string
                    return self.isStringExpr(m.object);
                }
                return false;
            },
            else => {
                // For other expressions, use type checker if available
                if (self.type_checker) |tc| {
                    const tc_mut = @constCast(tc);
                    const expr_type = tc_mut.checkExpr(expr);
                    return expr_type == .primitive and expr_type.primitive == .string_;
                }
                return false;
            },
        }
    }

    /// Check if an expression is an array or slice type.
    fn isArrayExpr(self: *Emitter, expr: ast.Expr) bool {
        switch (expr) {
            .identifier => |id| {
                // Check if we have array info for this identifier in named_values
                if (self.named_values.get(id.name)) |local| {
                    if (local.is_array) {
                        return true;
                    }
                }
            },
            .array_literal => {
                // Array literals are always array type
                return true;
            },
            else => {},
        }
        // Fallback: use type checker if available (may not work during emission)
        if (self.type_checker) |tc| {
            const tc_mut = @constCast(tc);
            const expr_type = tc_mut.checkExpr(expr);
            return expr_type == .array or expr_type == .slice;
        }
        return false;
    }

    /// Check if an expression is an integer type.
    fn isIntegerExpr(self: *Emitter, expr: ast.Expr) bool {
        switch (expr) {
            .literal => |lit| {
                return lit.kind == .int;
            },
            .identifier => |id| {
                if (self.named_values.get(id.name)) |local| {
                    // Check if the LLVM type is an integer type
                    const kind = llvm.getTypeKind(local.ty);
                    return kind == llvm.c.LLVMIntegerTypeKind;
                }
                return false;
            },
            .binary, .unary => {
                // Use type checker to determine if result is integer
                if (self.type_checker) |tc| {
                    const tc_mut = @constCast(tc);
                    const expr_type = tc_mut.checkExpr(expr);
                    return expr_type.isInteger();
                }
                return false;
            },
            .method_call => |m| {
                // Methods like abs, min, max on integers return integers
                if (std.mem.eql(u8, m.method_name, "abs") or
                    std.mem.eql(u8, m.method_name, "min") or
                    std.mem.eql(u8, m.method_name, "max"))
                {
                    return self.isIntegerExpr(m.object);
                }
                return false;
            },
            else => {
                // For other expressions, use type checker if available
                if (self.type_checker) |tc| {
                    const tc_mut = @constCast(tc);
                    const expr_type = tc_mut.checkExpr(expr);
                    return expr_type.isInteger();
                }
                return false;
            },
        }
    }

    /// Check if an expression is a Range type.
    fn isRangeExpr(self: *Emitter, expr: ast.Expr) bool {
        switch (expr) {
            .range => return true,
            .identifier => |id| {
                // Check the stored LLVM type from named_values
                if (self.named_values.get(id.name)) |local| {
                    return self.isRangeType(local.ty);
                }
                return false;
            },
            else => {
                // For other expressions, check via emitted type
                // This handles method chains and other complex expressions
                return false;
            },
        }
    }

    /// Check if an LLVM type is a Range type by checking its structure.
    /// Range has layout: { i32, i32, i32, i1 } (start, end, current, inclusive)
    fn isRangeType(self: *Emitter, ty: llvm.TypeRef) bool {
        _ = self;
        const type_kind = llvm.getTypeKind(ty);
        if (type_kind != llvm.c.LLVMStructTypeKind) return false;

        // Range struct has exactly 4 fields: i32, i32, i32, i1
        const num_fields = llvm.c.LLVMCountStructElementTypes(ty);
        if (num_fields != 4) return false;

        // Check field types
        const field0 = llvm.c.LLVMStructGetTypeAtIndex(ty, 0);
        const field1 = llvm.c.LLVMStructGetTypeAtIndex(ty, 1);
        const field2 = llvm.c.LLVMStructGetTypeAtIndex(ty, 2);
        const field3 = llvm.c.LLVMStructGetTypeAtIndex(ty, 3);

        // Fields 0-2 should be i32, field 3 should be i1
        const kind0 = llvm.getTypeKind(field0);
        const kind1 = llvm.getTypeKind(field1);
        const kind2 = llvm.getTypeKind(field2);
        const kind3 = llvm.getTypeKind(field3);

        if (kind0 != llvm.c.LLVMIntegerTypeKind or kind1 != llvm.c.LLVMIntegerTypeKind or
            kind2 != llvm.c.LLVMIntegerTypeKind or kind3 != llvm.c.LLVMIntegerTypeKind)
        {
            return false;
        }

        // Check bit widths: first 3 should be 32, last should be 1
        const width0 = llvm.c.LLVMGetIntTypeWidth(field0);
        const width1 = llvm.c.LLVMGetIntTypeWidth(field1);
        const width2 = llvm.c.LLVMGetIntTypeWidth(field2);
        const width3 = llvm.c.LLVMGetIntTypeWidth(field3);

        return width0 == 32 and width1 == 32 and width2 == 32 and width3 == 1;
    }

    /// Check if an expression is a List type.
    fn isListExpr(self: *Emitter, expr: ast.Expr) bool {
        switch (expr) {
            .identifier => |id| {
                // Check the stored LLVM type structure
                if (self.named_values.get(id.name)) |local| {
                    // Exclude String types - String and List have same LLVM structure
                    // but String has is_string_data flag set
                    if (local.is_string_data) {
                        return false;
                    }
                    // Check LLVM type structure first
                    if (self.isListType(local.ty)) {
                        return true;
                    }
                    // Fallback: check via type checker
                    if (self.type_checker) |tc| {
                        const tc_mut = @constCast(tc);
                        const expr_type = tc_mut.checkExpr(expr);
                        return expr_type == .list;
                    }
                }
                return false;
            },
            else => {
                // For other expressions, check via type checker
                if (self.type_checker) |tc| {
                    const tc_mut = @constCast(tc);
                    const expr_type = tc_mut.checkExpr(expr);
                    return expr_type == .list;
                }
                return false;
            },
        }
    }

    /// Check if an LLVM type is a List type by checking its structure.
    /// List has layout: { ptr, i32, i32 } (ptr, len, capacity)
    fn isListType(self: *Emitter, ty: llvm.TypeRef) bool {
        _ = self;
        const type_kind = llvm.getTypeKind(ty);
        if (type_kind != llvm.c.LLVMStructTypeKind) return false;

        // List struct has exactly 3 fields: ptr, i32, i32
        const num_fields = llvm.c.LLVMCountStructElementTypes(ty);
        if (num_fields != 3) return false;

        // Check field types
        const field0 = llvm.c.LLVMStructGetTypeAtIndex(ty, 0);
        const field1 = llvm.c.LLVMStructGetTypeAtIndex(ty, 1);
        const field2 = llvm.c.LLVMStructGetTypeAtIndex(ty, 2);

        // Field 0 should be pointer, fields 1-2 should be i32
        const kind0 = llvm.getTypeKind(field0);
        const kind1 = llvm.getTypeKind(field1);
        const kind2 = llvm.getTypeKind(field2);

        if (kind0 != llvm.c.LLVMPointerTypeKind) return false;
        if (kind1 != llvm.c.LLVMIntegerTypeKind or kind2 != llvm.c.LLVMIntegerTypeKind) return false;

        // Check bit widths: should be 32
        const width1 = llvm.c.LLVMGetIntTypeWidth(field1);
        const width2 = llvm.c.LLVMGetIntTypeWidth(field2);

        return width1 == 32 and width2 == 32;
    }

    /// Check if an expression is a String (heap-allocated) type.
    fn isStringDataExpr(self: *Emitter, expr: ast.Expr) bool {
        switch (expr) {
            // Check if this is an identifier with stored is_string_data flag
            .identifier => |id| {
                if (self.named_values.get(id.name)) |local| {
                    if (local.is_string_data) {
                        return true;
                    }
                }
            },
            // Check for String static constructors and String-returning methods
            .method_call => |mc| {
                // String static methods: String.new(), String.from(), String.with_capacity()
                if (mc.object == .identifier) {
                    const name = mc.object.identifier.name;
                    if (std.mem.eql(u8, name, "String")) {
                        if (std.mem.eql(u8, mc.method_name, "new") or
                            std.mem.eql(u8, mc.method_name, "from") or
                            std.mem.eql(u8, mc.method_name, "with_capacity"))
                        {
                            return true;
                        }
                    }
                }
                // String instance methods that return String: concat(), clone()
                if (self.isStringDataExpr(mc.object)) {
                    if (std.mem.eql(u8, mc.method_name, "concat") or
                        std.mem.eql(u8, mc.method_name, "clone"))
                    {
                        return true;
                    }
                }
            },
            else => {},
        }
        // Fallback: check via type checker (for non-identifier expressions)
        if (self.type_checker) |tc| {
            const tc_mut = @constCast(tc);
            const expr_type = tc_mut.checkExpr(expr);
            return expr_type == .string_data;
        }
        return false;
    }

    /// Get the element type for a List expression.
    fn getListElementType(self: *Emitter, expr: ast.Expr) ?types.Type {
        // First check if this is an identifier with stored list element type
        switch (expr) {
            .identifier => |id| {
                if (self.named_values.get(id.name)) |local| {
                    if (local.list_element_type) |elem_type| {
                        return elem_type;
                    }
                }
            },
            else => {},
        }
        // Fallback: use type checker to get the list element type
        if (self.type_checker) |tc| {
            const tc_mut = @constCast(tc);
            const expr_type = tc_mut.checkExpr(expr);
            if (expr_type == .list) {
                return expr_type.list.element;
            }
        }
        return null;
    }

    /// Get array element type from an expression.
    fn getArrayElementType(self: *Emitter, expr: ast.Expr) ?types.Type {
        // First check named_values for identifiers
        switch (expr) {
            .identifier => |id| {
                if (self.named_values.get(id.name)) |local| {
                    if (local.is_array) {
                        return local.array_element_type;
                    }
                }
            },
            else => {},
        }
        // Fallback: use type checker if available
        if (self.type_checker) |tc| {
            const tc_mut = @constCast(tc);
            const expr_type = tc_mut.checkExpr(expr);
            if (expr_type == .array) {
                return expr_type.array.element;
            }
            if (expr_type == .slice) {
                return expr_type.slice.element;
            }
        }
        return null;
    }

    /// Get array length from an expression (for fixed-size arrays).
    fn getArraySize(self: *Emitter, expr: ast.Expr) ?usize {
        // First check named_values for identifiers
        switch (expr) {
            .identifier => |id| {
                if (self.named_values.get(id.name)) |local| {
                    if (local.is_array) {
                        return local.array_size; // null for slices
                    }
                }
            },
            else => {},
        }
        // Fallback: use type checker if available
        if (self.type_checker) |tc| {
            const tc_mut = @constCast(tc);
            const expr_type = tc_mut.checkExpr(expr);
            if (expr_type == .array) {
                return expr_type.array.size;
            }
        }
        return null;
    }

    /// Emit string.len() - returns length as i32.
    fn emitStringLen(self: *Emitter, str: llvm.ValueRef) EmitError!llvm.ValueRef {
        const strlen_fn = self.getOrDeclareStrlen();
        var args = [_]llvm.ValueRef{str};
        const len_i64 = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(strlen_fn), strlen_fn, &args, "strlen");
        // Truncate from size_t (i64) to i32
        const i32_type = llvm.Types.int32(self.ctx);
        return llvm.c.LLVMBuildTrunc(self.builder.ref, len_i64, i32_type, "strlen_i32");
    }

    /// Emit string.is_empty() - returns true if len == 0.
    fn emitStringIsEmpty(self: *Emitter, str: llvm.ValueRef) EmitError!llvm.ValueRef {
        const strlen_fn = self.getOrDeclareStrlen();
        var args = [_]llvm.ValueRef{str};
        const len = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(strlen_fn), strlen_fn, &args, "strlen");
        // Compare len == 0
        const zero = llvm.Const.int64(self.ctx, 0);
        return self.builder.buildICmp(llvm.c.LLVMIntEQ, len, zero, "is_empty");
    }

    /// Emit string.contains(pattern) - returns true if pattern is found in string.
    fn emitStringContains(self: *Emitter, str: llvm.ValueRef, pattern: llvm.ValueRef) EmitError!llvm.ValueRef {
        const strstr_fn = self.getOrDeclareStrstr();
        var args = [_]llvm.ValueRef{ str, pattern };
        const result = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(strstr_fn), strstr_fn, &args, "strstr");
        // strstr returns NULL if not found, non-NULL if found
        const null_ptr = llvm.c.LLVMConstPointerNull(llvm.Types.pointer(self.ctx));
        return self.builder.buildICmp(llvm.c.LLVMIntNE, result, null_ptr, "contains");
    }

    /// Emit string.starts_with(prefix) - returns true if string starts with prefix.
    fn emitStringStartsWith(self: *Emitter, str: llvm.ValueRef, prefix: llvm.ValueRef) EmitError!llvm.ValueRef {
        // Get the length of the prefix
        const strlen_fn = self.getOrDeclareStrlen();
        var strlen_args = [_]llvm.ValueRef{prefix};
        const prefix_len = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(strlen_fn), strlen_fn, &strlen_args, "prefix_len");

        // Compare using strncmp(str, prefix, prefix_len) == 0
        const strncmp_fn = self.getOrDeclareStrncmp();
        var strncmp_args = [_]llvm.ValueRef{ str, prefix, prefix_len };
        const cmp_result = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(strncmp_fn), strncmp_fn, &strncmp_args, "strncmp");

        const zero = llvm.Const.int32(self.ctx, 0);
        return self.builder.buildICmp(llvm.c.LLVMIntEQ, cmp_result, zero, "starts_with");
    }

    /// Emit string.ends_with(suffix) - returns true if string ends with suffix.
    fn emitStringEndsWith(self: *Emitter, str: llvm.ValueRef, suffix: llvm.ValueRef) EmitError!llvm.ValueRef {
        // Get the lengths
        const strlen_fn = self.getOrDeclareStrlen();

        var str_args = [_]llvm.ValueRef{str};
        const str_len = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(strlen_fn), strlen_fn, &str_args, "str_len");

        var suffix_args = [_]llvm.ValueRef{suffix};
        const suffix_len = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(strlen_fn), strlen_fn, &suffix_args, "suffix_len");

        // Check if suffix is longer than string (would be false)
        const suffix_longer = self.builder.buildICmp(llvm.c.LLVMIntUGT, suffix_len, str_len, "suffix_longer");

        // Calculate offset = str_len - suffix_len
        const offset = llvm.c.LLVMBuildSub(self.builder.ref, str_len, suffix_len, "offset");

        // Get pointer to end of string: str + offset
        const i8_type = llvm.Types.int8(self.ctx);
        var indices = [_]llvm.ValueRef{offset};
        const end_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, i8_type, str, &indices, 1, "end_ptr");

        // Compare using strcmp(end_ptr, suffix) == 0
        const strcmp_fn = self.getOrDeclareStrcmp();
        var strcmp_args = [_]llvm.ValueRef{ end_ptr, suffix };
        const cmp_result = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(strcmp_fn), strcmp_fn, &strcmp_args, "strcmp");

        const zero = llvm.Const.int32(self.ctx, 0);
        const ends_match = self.builder.buildICmp(llvm.c.LLVMIntEQ, cmp_result, zero, "ends_match");

        // Result: !suffix_longer && ends_match
        const suffix_not_longer = llvm.c.LLVMBuildNot(self.builder.ref, suffix_longer, "suffix_not_longer");
        return self.builder.buildAnd(suffix_not_longer, ends_match, "ends_with");
    }

    /// Emit string.trim() - returns a new string with leading/trailing whitespace removed.
    /// Calls the klar_string_trim runtime function.
    fn emitStringTrim(self: *Emitter, str: llvm.ValueRef) EmitError!llvm.ValueRef {
        const trim_fn = self.getOrDeclareStringTrim();
        var args = [_]llvm.ValueRef{str};
        return llvm.c.LLVMBuildCall2(
            self.builder.ref,
            llvm.c.LLVMGlobalGetValueType(trim_fn),
            trim_fn,
            &args,
            1,
            "trimmed",
        );
    }

    /// Emit string.to_uppercase() - returns a new string with all characters uppercased.
    /// Calls the klar_string_to_uppercase runtime function.
    fn emitStringToUppercase(self: *Emitter, str: llvm.ValueRef) EmitError!llvm.ValueRef {
        const upper_fn = self.getOrDeclareStringToUppercase();
        var args = [_]llvm.ValueRef{str};
        return llvm.c.LLVMBuildCall2(
            self.builder.ref,
            llvm.c.LLVMGlobalGetValueType(upper_fn),
            upper_fn,
            &args,
            1,
            "uppercased",
        );
    }

    /// Emit string.to_lowercase() - returns a new string with all characters lowercased.
    /// Calls the klar_string_to_lowercase runtime function.
    fn emitStringToLowercase(self: *Emitter, str: llvm.ValueRef) EmitError!llvm.ValueRef {
        const lower_fn = self.getOrDeclareStringToLowercase();
        var args = [_]llvm.ValueRef{str};
        return llvm.c.LLVMBuildCall2(
            self.builder.ref,
            llvm.c.LLVMGlobalGetValueType(lower_fn),
            lower_fn,
            &args,
            1,
            "lowercased",
        );
    }

    /// Emit string.bytes() - returns a slice of u8 containing the string's bytes.
    /// The slice points directly to the string's data (no allocation).
    fn emitStringBytes(self: *Emitter, str: llvm.ValueRef) EmitError!llvm.ValueRef {
        // Get the string length using strlen
        const strlen_fn = self.getOrDeclareStrlen();
        var strlen_args = [_]llvm.ValueRef{str};
        const len = llvm.c.LLVMBuildCall2(
            self.builder.ref,
            llvm.c.LLVMGlobalGetValueType(strlen_fn),
            strlen_fn,
            &strlen_args,
            1,
            "str.len",
        );

        // Create a slice struct type: { ptr, i64 }
        var slice_fields = [_]llvm.TypeRef{
            llvm.Types.pointer(self.ctx), // data pointer
            llvm.Types.int64(self.ctx), // length
        };
        const slice_type = llvm.Types.struct_(self.ctx, &slice_fields, false);

        // Build the slice struct value using alloca + store + load pattern
        const slice_ptr = self.builder.buildAlloca(slice_type, "bytes_slice_ptr");

        // Store pointer field (index 0)
        const ptr_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, slice_type, slice_ptr, 0, "ptr_field");
        _ = self.builder.buildStore(str, ptr_field);

        // Store length field (index 1)
        const len_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, slice_type, slice_ptr, 1, "len_field");
        _ = self.builder.buildStore(len, len_field);

        // Load the complete slice struct
        return self.builder.buildLoad(slice_type, slice_ptr, "bytes_slice");
    }

    /// Emit string.chars() - returns a slice of char containing the string's unicode codepoints.
    /// This calls a runtime function that decodes UTF-8 and allocates the result.
    fn emitStringChars(self: *Emitter, str: llvm.ValueRef) EmitError!llvm.ValueRef {
        const chars_fn = self.getOrDeclareStringChars();
        var args = [_]llvm.ValueRef{str};
        return llvm.c.LLVMBuildCall2(
            self.builder.ref,
            llvm.c.LLVMGlobalGetValueType(chars_fn),
            chars_fn,
            &args,
            1,
            "chars_slice",
        );
    }

    // ========================================================================
    // Integer Methods
    // ========================================================================

    /// Emit integer.abs() - returns absolute value.
    /// For signed integers, computes abs(x) = x < 0 ? -x : x
    /// For unsigned integers, just returns x.
    fn emitIntAbs(self: *Emitter, value: llvm.ValueRef) EmitError!llvm.ValueRef {
        const value_type = llvm.typeOf(value);
        const bit_width = llvm.c.LLVMGetIntTypeWidth(value_type);

        // Check if this is a signed type by seeing if it's a standard signed width
        // Klar uses i8, i16, i32, i64 for signed and u8, u16, u32, u64 for unsigned
        // We can't directly tell from LLVM, so we check if the type checker says signed
        // For now, we'll assume it could be signed and compute abs safely

        // Create zero constant for comparison
        const zero = llvm.c.LLVMConstInt(value_type, 0, 0);

        // Check if value < 0 (signed comparison)
        const is_negative = self.builder.buildICmp(llvm.c.LLVMIntSLT, value, zero, "is_neg");

        // Compute -value using sub(0, value)
        const neg_value = llvm.c.LLVMBuildSub(self.builder.ref, zero, value, "neg");

        // Select: is_negative ? -value : value
        _ = bit_width;
        return llvm.c.LLVMBuildSelect(self.builder.ref, is_negative, neg_value, value, "abs");
    }

    /// Emit integer.min(other) - returns minimum of two values.
    fn emitIntMin(self: *Emitter, a: llvm.ValueRef, b: llvm.ValueRef) EmitError!llvm.ValueRef {
        // Compare a < b (signed comparison)
        const cmp = self.builder.buildICmp(llvm.c.LLVMIntSLT, a, b, "min_cmp");
        // Select: a < b ? a : b
        return llvm.c.LLVMBuildSelect(self.builder.ref, cmp, a, b, "min");
    }

    /// Emit integer.max(other) - returns maximum of two values.
    fn emitIntMax(self: *Emitter, a: llvm.ValueRef, b: llvm.ValueRef) EmitError!llvm.ValueRef {
        // Compare a > b (signed comparison)
        const cmp = self.builder.buildICmp(llvm.c.LLVMIntSGT, a, b, "max_cmp");
        // Select: a > b ? a : b
        return llvm.c.LLVMBuildSelect(self.builder.ref, cmp, a, b, "max");
    }

    // ========================================================================
    // Range Methods
    // ========================================================================

    /// Emit Range.next() - returns Optional[T], Some(current) and increments, or None if exhausted.
    /// Range layout: { start: T, end: T, current: T, inclusive: i1 }
    fn emitRangeNext(self: *Emitter, range_ptr: llvm.ValueRef) EmitError!llvm.ValueRef {
        const i32_type = llvm.Types.int32(self.ctx);
        const i1_type = llvm.Types.int1(self.ctx);

        // Build range struct type
        var range_fields = [_]llvm.TypeRef{
            i32_type, // start
            i32_type, // end
            i32_type, // current
            i1_type, // inclusive
        };
        const range_ty = llvm.Types.struct_(self.ctx, &range_fields, false);

        // Load current value
        const current_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, range_ty, range_ptr, 2, "range.current.ptr");
        const current = self.builder.buildLoad(i32_type, current_ptr, "range.current");

        // Load end value
        const end_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, range_ty, range_ptr, 1, "range.end.ptr");
        const end_val = self.builder.buildLoad(i32_type, end_ptr, "range.end");

        // Load inclusive flag
        const inclusive_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, range_ty, range_ptr, 3, "range.inclusive.ptr");
        const inclusive = self.builder.buildLoad(i1_type, inclusive_ptr, "range.inclusive");

        // Build Optional[i32] type: { i1 tag, i32 value }
        var opt_fields = [_]llvm.TypeRef{ i1_type, i32_type };
        const opt_ty = llvm.Types.struct_(self.ctx, &opt_fields, false);

        // Create basic blocks for branching
        const func = self.current_function orelse return EmitError.InvalidAST;
        const check_inclusive_bb = llvm.appendBasicBlock(self.ctx, func, "range.check_inclusive");
        const some_bb = llvm.appendBasicBlock(self.ctx, func, "range.some");
        const none_bb = llvm.appendBasicBlock(self.ctx, func, "range.none");
        const merge_bb = llvm.appendBasicBlock(self.ctx, func, "range.merge");

        // Check: current < end (non-inclusive case, always valid check)
        const lt_cmp = self.builder.buildICmp(llvm.c.LLVMIntSLT, current, end_val, "range.lt");
        _ = self.builder.buildCondBr(lt_cmp, some_bb, check_inclusive_bb);

        // Check inclusive case: current == end && inclusive
        self.builder.positionAtEnd(check_inclusive_bb);
        const eq_cmp = self.builder.buildICmp(llvm.c.LLVMIntEQ, current, end_val, "range.eq");
        const in_range = llvm.c.LLVMBuildAnd(self.builder.ref, eq_cmp, inclusive, "range.in_range");
        _ = self.builder.buildCondBr(in_range, some_bb, none_bb);

        // Some case: return Some(current), increment current
        self.builder.positionAtEnd(some_bb);
        // Increment current for next iteration
        const one = llvm.Const.int32(self.ctx, 1);
        const next_current = self.builder.buildAdd(current, one, "range.next_current");
        _ = self.builder.buildStore(next_current, current_ptr);
        // Build Some(current) - allocate, store, load
        const some_alloca = self.builder.buildAlloca(opt_ty, "opt.some");
        const some_tag_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_ty, some_alloca, 0, "opt.tag.ptr");
        _ = self.builder.buildStore(llvm.Const.int1(self.ctx, true), some_tag_ptr);
        const some_val_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_ty, some_alloca, 1, "opt.val.ptr");
        _ = self.builder.buildStore(current, some_val_ptr);
        const some_result = self.builder.buildLoad(opt_ty, some_alloca, "opt.some");
        _ = self.builder.buildBr(merge_bb);

        // None case: return None
        self.builder.positionAtEnd(none_bb);
        const none_alloca = self.builder.buildAlloca(opt_ty, "opt.none");
        const none_tag_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_ty, none_alloca, 0, "opt.tag.ptr");
        _ = self.builder.buildStore(llvm.Const.int1(self.ctx, false), none_tag_ptr);
        const none_val_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_ty, none_alloca, 1, "opt.val.ptr");
        _ = self.builder.buildStore(llvm.Const.int32(self.ctx, 0), none_val_ptr);
        const none_result = self.builder.buildLoad(opt_ty, none_alloca, "opt.none");
        _ = self.builder.buildBr(merge_bb);

        // Merge: phi node to select result
        self.builder.positionAtEnd(merge_bb);
        const phi = llvm.c.LLVMBuildPhi(self.builder.ref, opt_ty, "range.next.result");
        var incoming_values = [_]llvm.ValueRef{ some_result, none_result };
        var incoming_blocks = [_]llvm.BasicBlockRef{ some_bb, none_bb };
        llvm.c.LLVMAddIncoming(phi, &incoming_values, &incoming_blocks, 2);

        return phi;
    }

    /// Emit Range.reset() - resets current to start.
    fn emitRangeReset(self: *Emitter, range_ptr: llvm.ValueRef) EmitError!llvm.ValueRef {
        const i32_type = llvm.Types.int32(self.ctx);
        const i1_type = llvm.Types.int1(self.ctx);

        // Build range struct type
        var range_fields = [_]llvm.TypeRef{
            i32_type, // start
            i32_type, // end
            i32_type, // current
            i1_type, // inclusive
        };
        const range_ty = llvm.Types.struct_(self.ctx, &range_fields, false);

        // Load start value
        const start_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, range_ty, range_ptr, 0, "range.start.ptr");
        const start_val = self.builder.buildLoad(i32_type, start_ptr, "range.start");

        // Store start into current
        const current_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, range_ty, range_ptr, 2, "range.current.ptr");
        _ = self.builder.buildStore(start_val, current_ptr);

        // Return void (represented as 0 in LLVM for now)
        return llvm.Const.int32(self.ctx, 0);
    }

    /// Emit Range.is_empty() - returns true if current >= end (or > end for inclusive).
    fn emitRangeIsEmpty(self: *Emitter, range_ptr: llvm.ValueRef) EmitError!llvm.ValueRef {
        const i32_type = llvm.Types.int32(self.ctx);
        const i1_type = llvm.Types.int1(self.ctx);

        // Build range struct type
        var range_fields = [_]llvm.TypeRef{
            i32_type, // start
            i32_type, // end
            i32_type, // current
            i1_type, // inclusive
        };
        const range_ty = llvm.Types.struct_(self.ctx, &range_fields, false);

        // Load current value
        const current_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, range_ty, range_ptr, 2, "range.current.ptr");
        const current = self.builder.buildLoad(i32_type, current_ptr, "range.current");

        // Load end value
        const end_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, range_ty, range_ptr, 1, "range.end.ptr");
        const end_val = self.builder.buildLoad(i32_type, end_ptr, "range.end");

        // Load inclusive flag
        const inclusive_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, range_ty, range_ptr, 3, "range.inclusive.ptr");
        const inclusive = self.builder.buildLoad(i1_type, inclusive_ptr, "range.inclusive");

        // For non-inclusive: is_empty = current >= end
        // For inclusive: is_empty = current > end
        // Combined: is_empty = (current > end) || (!inclusive && current == end)
        const gt_cmp = self.builder.buildICmp(llvm.c.LLVMIntSGT, current, end_val, "range.gt");
        const eq_cmp = self.builder.buildICmp(llvm.c.LLVMIntEQ, current, end_val, "range.eq");
        const not_inclusive = llvm.c.LLVMBuildNot(self.builder.ref, inclusive, "range.not_incl");
        const eq_and_not_incl = llvm.c.LLVMBuildAnd(self.builder.ref, eq_cmp, not_inclusive, "range.eq_and_not_incl");
        return llvm.c.LLVMBuildOr(self.builder.ref, gt_cmp, eq_and_not_incl, "range.is_empty");
    }

    /// Emit Range.len() - returns remaining count as i32.
    fn emitRangeLen(self: *Emitter, range_ptr: llvm.ValueRef) EmitError!llvm.ValueRef {
        const i32_type = llvm.Types.int32(self.ctx);
        const i1_type = llvm.Types.int1(self.ctx);

        // Build range struct type
        var range_fields = [_]llvm.TypeRef{
            i32_type, // start
            i32_type, // end
            i32_type, // current
            i1_type, // inclusive
        };
        const range_ty = llvm.Types.struct_(self.ctx, &range_fields, false);

        // Load current value
        const current_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, range_ty, range_ptr, 2, "range.current.ptr");
        const current = self.builder.buildLoad(i32_type, current_ptr, "range.current");

        // Load end value
        const end_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, range_ty, range_ptr, 1, "range.end.ptr");
        const end_val = self.builder.buildLoad(i32_type, end_ptr, "range.end");

        // Load inclusive flag
        const inclusive_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, range_ty, range_ptr, 3, "range.inclusive.ptr");
        const inclusive = self.builder.buildLoad(i1_type, inclusive_ptr, "range.inclusive");

        // len = max(0, end - current + (inclusive ? 1 : 0))
        const diff = self.builder.buildSub(end_val, current, "range.diff");
        const inclusive_ext = llvm.c.LLVMBuildZExt(self.builder.ref, inclusive, i32_type, "range.incl_ext");
        const len_raw = self.builder.buildAdd(diff, inclusive_ext, "range.len_raw");

        // Clamp to 0 if negative
        const zero = llvm.Const.int32(self.ctx, 0);
        const is_neg = self.builder.buildICmp(llvm.c.LLVMIntSLT, len_raw, zero, "range.is_neg");
        return llvm.c.LLVMBuildSelect(self.builder.ref, is_neg, zero, len_raw, "range.len");
    }

    // ========================================================================
    // List Methods
    // ========================================================================

    /// Emit List.new[T]() - creates an empty list.
    /// Returns a List struct initialized to { ptr: null, len: 0, capacity: 0 }
    fn emitListNew(self: *Emitter, method: *ast.MethodCall) EmitError!llvm.ValueRef {
        _ = method;

        // Build list struct type
        const list_type = self.getListStructType();

        // Create an empty list struct { null, 0, 0 } directly
        const null_ptr = llvm.c.LLVMConstNull(llvm.Types.pointer(self.ctx));
        const zero_i32 = llvm.Const.int32(self.ctx, 0);

        // Build constant struct value
        var values = [_]llvm.ValueRef{ null_ptr, zero_i32, zero_i32 };
        return llvm.c.LLVMConstNamedStruct(list_type, &values, 3);
    }

    /// Emit List.with_capacity[T](n) - creates a list with pre-allocated capacity.
    /// Returns a List struct with allocated memory for n elements.
    /// Implemented inline: allocates memory using malloc, returns { ptr, 0, capacity }.
    fn emitListWithCapacity(self: *Emitter, method: *ast.MethodCall) EmitError!llvm.ValueRef {
        // Get element type from type_args
        const type_args = method.type_args orelse return EmitError.InvalidAST;
        if (type_args.len != 1) return EmitError.InvalidAST;

        // Resolve element type using type checker
        const element_type = if (self.type_checker) |tc| blk: {
            const tc_mut = @constCast(tc);
            break :blk tc_mut.resolveTypeExpr(type_args[0]) catch return EmitError.InvalidAST;
        } else return EmitError.InvalidAST;

        const element_llvm_type = self.typeToLLVM(element_type);
        const element_size = self.getLLVMTypeSize(element_llvm_type);

        // Emit the capacity argument
        if (method.args.len != 1) return EmitError.InvalidAST;
        const capacity = try self.emitExpr(method.args[0]);

        const i64_type = llvm.Types.int64(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);
        const list_type = self.getListStructType();

        // Convert capacity to i32 if needed (it could be i64)
        const cap_type = llvm.typeOf(capacity);
        const cap_i32 = if (llvm.c.LLVMGetIntTypeWidth(cap_type) > 32)
            llvm.c.LLVMBuildTrunc(self.builder.ref, capacity, i32_type, "cap_i32")
        else
            capacity;

        // Calculate allocation size: capacity * element_size
        const cap_i64 = llvm.c.LLVMBuildSExt(self.builder.ref, cap_i32, i64_type, "cap_i64");
        const elem_size_val = llvm.Const.int64(self.ctx, @intCast(element_size));
        const alloc_size = llvm.c.LLVMBuildMul(self.builder.ref, cap_i64, elem_size_val, "alloc_size");

        // Call malloc(size)
        const malloc_fn = self.getOrDeclareMalloc();
        var malloc_args = [_]llvm.ValueRef{alloc_size};
        const data_ptr = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(malloc_fn), malloc_fn, &malloc_args, "list.data_ptr");

        // Build the list struct { ptr, len: 0, capacity }
        const list_alloca = self.builder.buildAlloca(list_type, "list.with_cap");

        // Store ptr (field 0)
        const ptr_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, list_type, list_alloca, 0, "list.ptr_field");
        _ = self.builder.buildStore(data_ptr, ptr_field);

        // Store len = 0 (field 1)
        const len_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, list_type, list_alloca, 1, "list.len_field");
        _ = self.builder.buildStore(llvm.Const.int32(self.ctx, 0), len_field);

        // Store capacity (field 2)
        const cap_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, list_type, list_alloca, 2, "list.cap_field");
        _ = self.builder.buildStore(cap_i32, cap_field);

        // Load and return the struct value
        return self.builder.buildLoad(list_type, list_alloca, "list.with_cap_val");
    }

    /// Emit list.len() - returns length as i32.
    /// List layout: { ptr, len, capacity } - len is at index 1.
    fn emitListLen(self: *Emitter, list_ptr: llvm.ValueRef) EmitError!llvm.ValueRef {
        const list_type = self.getListStructType();
        const i32_type = llvm.Types.int32(self.ctx);

        // GEP to the len field (index 1)
        const len_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, list_type, list_ptr, 1, "list.len.ptr");
        return self.builder.buildLoad(i32_type, len_ptr, "list.len");
    }

    /// Emit list.is_empty() - returns true if len == 0.
    fn emitListIsEmpty(self: *Emitter, list_ptr: llvm.ValueRef) EmitError!llvm.ValueRef {
        const len = try self.emitListLen(list_ptr);
        const zero = llvm.Const.int32(self.ctx, 0);
        return self.builder.buildICmp(llvm.c.LLVMIntEQ, len, zero, "list.is_empty");
    }

    /// Emit list.capacity() - returns capacity as i32.
    /// List layout: { ptr, len, capacity } - capacity is at index 2.
    fn emitListCapacity(self: *Emitter, list_ptr: llvm.ValueRef) EmitError!llvm.ValueRef {
        const list_type = self.getListStructType();
        const i32_type = llvm.Types.int32(self.ctx);

        // GEP to the capacity field (index 2)
        const cap_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, list_type, list_ptr, 2, "list.cap.ptr");
        return self.builder.buildLoad(i32_type, cap_ptr, "list.capacity");
    }

    /// Emit list.push(value) - adds an element to the list.
    /// Implemented inline: checks capacity, grows if needed, stores value, increments len.
    fn emitListPush(self: *Emitter, list_ptr: llvm.ValueRef, method: *ast.MethodCall, value: llvm.ValueRef) EmitError!llvm.ValueRef {
        const func = self.current_function orelse return EmitError.InvalidAST;

        // Get element type info
        const element_type = self.getListElementType(method.object) orelse return EmitError.InvalidAST;
        const element_llvm_type = self.typeToLLVM(element_type);
        const element_size = self.getLLVMTypeSize(element_llvm_type);

        const list_type = self.getListStructType();
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);

        // Load current ptr, len, capacity
        const ptr_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, list_type, list_ptr, 0, "push.ptr_ptr");
        const len_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, list_type, list_ptr, 1, "push.len_ptr");
        const cap_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, list_type, list_ptr, 2, "push.cap_ptr");

        const current_ptr = self.builder.buildLoad(ptr_type, ptr_ptr, "push.current_ptr");
        const current_len = self.builder.buildLoad(i32_type, len_ptr, "push.current_len");
        const current_cap = self.builder.buildLoad(i32_type, cap_ptr, "push.current_cap");

        // Check if we need to grow: len >= capacity
        const need_grow = self.builder.buildICmp(llvm.c.LLVMIntSGE, current_len, current_cap, "push.need_grow");

        // Create basic blocks for growth path
        const grow_bb = llvm.appendBasicBlock(self.ctx, func, "push.grow");
        const store_bb = llvm.appendBasicBlock(self.ctx, func, "push.store");

        _ = self.builder.buildCondBr(need_grow, grow_bb, store_bb);

        // --- Grow block ---
        self.builder.positionAtEnd(grow_bb);

        // New capacity = max(8, capacity * 2)
        const doubled_cap = llvm.c.LLVMBuildMul(self.builder.ref, current_cap, llvm.Const.int32(self.ctx, 2), "push.doubled");
        const eight = llvm.Const.int32(self.ctx, 8);
        const cmp_eight = self.builder.buildICmp(llvm.c.LLVMIntSGT, doubled_cap, eight, "push.cmp_eight");
        const new_cap = llvm.c.LLVMBuildSelect(self.builder.ref, cmp_eight, doubled_cap, eight, "push.new_cap");

        // Calculate new size in bytes: new_cap * element_size
        const new_cap_i64 = llvm.c.LLVMBuildSExt(self.builder.ref, new_cap, i64_type, "push.new_cap_i64");
        const elem_size_val = llvm.Const.int64(self.ctx, @intCast(element_size));
        const new_size = llvm.c.LLVMBuildMul(self.builder.ref, new_cap_i64, elem_size_val, "push.new_size");

        // Call realloc(ptr, new_size) - realloc handles null ptr like malloc
        const realloc_fn = self.getOrDeclareRealloc();
        var realloc_args = [_]llvm.ValueRef{ current_ptr, new_size };
        const new_ptr = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(realloc_fn), realloc_fn, &realloc_args, "push.new_ptr");

        // Store new ptr and capacity
        _ = self.builder.buildStore(new_ptr, ptr_ptr);
        _ = self.builder.buildStore(new_cap, cap_ptr);

        _ = self.builder.buildBr(store_bb);

        // --- Store block ---
        self.builder.positionAtEnd(store_bb);

        // PHI for the data pointer (either current_ptr or new_ptr from grow)
        const phi = llvm.c.LLVMBuildPhi(self.builder.ref, ptr_type, "push.data_ptr");
        var incoming_values = [_]llvm.ValueRef{ current_ptr, new_ptr };
        var incoming_blocks = [_]llvm.BasicBlockRef{ llvm.c.LLVMGetPreviousBasicBlock(grow_bb), grow_bb };
        llvm.c.LLVMAddIncoming(phi, &incoming_values, &incoming_blocks, 2);

        // Calculate address: ptr + len * element_size
        const len_i64 = llvm.c.LLVMBuildSExt(self.builder.ref, current_len, i64_type, "push.len_i64");
        const offset = llvm.c.LLVMBuildMul(self.builder.ref, len_i64, elem_size_val, "push.offset");
        const i8_type = llvm.Types.int8(self.ctx);
        var gep_indices = [_]llvm.ValueRef{offset};
        const elem_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, i8_type, phi, &gep_indices, 1, "push.elem_ptr");

        // Store the value
        _ = self.builder.buildStore(value, elem_ptr);

        // Increment len and store back
        const new_len = llvm.c.LLVMBuildAdd(self.builder.ref, current_len, llvm.Const.int32(self.ctx, 1), "push.new_len");
        _ = self.builder.buildStore(new_len, len_ptr);

        // Return void (represented as i32 0 for now)
        return llvm.Const.int32(self.ctx, 0);
    }

    /// Emit list.pop() - removes and returns the last element as Optional.
    fn emitListPop(self: *Emitter, list_ptr: llvm.ValueRef, method: *ast.MethodCall) EmitError!llvm.ValueRef {
        const list_pop_fn = self.getOrDeclareListPop();

        // Get element type
        const element_type = self.getListElementType(method.object) orelse return EmitError.InvalidAST;
        const element_llvm_type = self.typeToLLVM(element_type);
        const element_size = self.getLLVMTypeSize(element_llvm_type);

        // Allocate stack space for the output value
        const value_out = self.builder.buildAlloca(element_llvm_type, "pop.value");

        var args = [_]llvm.ValueRef{
            list_ptr,
            llvm.Const.int64(self.ctx, @intCast(element_size)),
            value_out,
        };
        const success = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(list_pop_fn), list_pop_fn, &args, "pop.success");

        // Build optional type: { i1, T }
        const i1_type = llvm.Types.int1(self.ctx);
        var opt_fields = [_]llvm.TypeRef{ i1_type, element_llvm_type };
        const opt_type = llvm.Types.struct_(self.ctx, &opt_fields, false);

        // Allocate the optional result
        const opt_alloca = self.builder.buildAlloca(opt_type, "pop.optional");

        // Store the tag (success bool)
        const tag_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_type, opt_alloca, 0, "pop.tag_ptr");
        _ = self.builder.buildStore(success, tag_ptr);

        // Store the value (if success)
        const value_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_type, opt_alloca, 1, "pop.value_ptr");
        const loaded_value = self.builder.buildLoad(element_llvm_type, value_out, "pop.loaded");
        _ = self.builder.buildStore(loaded_value, value_ptr);

        // Load and return the optional
        return self.builder.buildLoad(opt_type, opt_alloca, "pop.result");
    }

    /// Emit list.get(index) - returns element at index as Optional.
    /// Uses inline LLVM codegen (no runtime function call).
    fn emitListGet(self: *Emitter, list_ptr: llvm.ValueRef, method: *ast.MethodCall, index: llvm.ValueRef) EmitError!llvm.ValueRef {
        const func = self.current_function orelse return EmitError.InvalidAST;

        // Get element type info
        const element_type = self.getListElementType(method.object) orelse return EmitError.InvalidAST;
        const element_llvm_type = self.typeToLLVM(element_type);
        const element_size = self.getLLVMTypeSize(element_llvm_type);

        const list_type = self.getListStructType();
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        const i1_type = llvm.Types.int1(self.ctx);
        const i8_type = llvm.Types.int8(self.ctx);

        // Load current ptr and len from the list struct
        const ptr_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, list_type, list_ptr, 0, "get.ptr_ptr");
        const len_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, list_type, list_ptr, 1, "get.len_ptr");

        const current_ptr = self.builder.buildLoad(ptr_type, ptr_ptr, "get.current_ptr");
        const current_len = self.builder.buildLoad(i32_type, len_ptr, "get.current_len");

        // Check bounds: index >= 0 and index < len
        const zero = llvm.Const.int32(self.ctx, 0);
        const idx_ge_zero = self.builder.buildICmp(llvm.c.LLVMIntSGE, index, zero, "get.idx_ge_zero");
        const idx_lt_len = self.builder.buildICmp(llvm.c.LLVMIntSLT, index, current_len, "get.idx_lt_len");
        const in_bounds = llvm.c.LLVMBuildAnd(self.builder.ref, idx_ge_zero, idx_lt_len, "get.in_bounds");

        // Build optional type: { i1, T }
        var opt_fields = [_]llvm.TypeRef{ i1_type, element_llvm_type };
        const opt_type = llvm.Types.struct_(self.ctx, &opt_fields, false);

        // Allocate the optional result on the stack
        const opt_alloca = self.builder.buildAlloca(opt_type, "get.optional");

        // Create basic blocks for valid/invalid cases
        const valid_bb = llvm.appendBasicBlock(self.ctx, func, "get.valid");
        const invalid_bb = llvm.appendBasicBlock(self.ctx, func, "get.invalid");
        const merge_bb = llvm.appendBasicBlock(self.ctx, func, "get.merge");

        _ = self.builder.buildCondBr(in_bounds, valid_bb, invalid_bb);

        // --- Valid block: index is in bounds ---
        self.builder.positionAtEnd(valid_bb);

        // Calculate address: ptr + index * element_size
        const idx_i64 = llvm.c.LLVMBuildSExt(self.builder.ref, index, i64_type, "get.idx_i64");
        const elem_size_val = llvm.Const.int64(self.ctx, @intCast(element_size));
        const offset = llvm.c.LLVMBuildMul(self.builder.ref, idx_i64, elem_size_val, "get.offset");
        var gep_indices = [_]llvm.ValueRef{offset};
        const elem_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, i8_type, current_ptr, &gep_indices, 1, "get.elem_ptr");

        // Load the value
        const loaded_value = self.builder.buildLoad(element_llvm_type, elem_ptr, "get.loaded");

        // Store success (true) and value
        const tag_ptr_valid = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_type, opt_alloca, 0, "get.tag_ptr_valid");
        _ = self.builder.buildStore(llvm.Const.int1(self.ctx, true), tag_ptr_valid);
        const value_ptr_valid = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_type, opt_alloca, 1, "get.value_ptr_valid");
        _ = self.builder.buildStore(loaded_value, value_ptr_valid);

        _ = self.builder.buildBr(merge_bb);

        // --- Invalid block: index is out of bounds ---
        self.builder.positionAtEnd(invalid_bb);

        // Store failure (false) - value field left uninitialized
        const tag_ptr_invalid = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_type, opt_alloca, 0, "get.tag_ptr_invalid");
        _ = self.builder.buildStore(llvm.Const.int1(self.ctx, false), tag_ptr_invalid);

        _ = self.builder.buildBr(merge_bb);

        // --- Merge block ---
        self.builder.positionAtEnd(merge_bb);

        // Load and return the optional
        return self.builder.buildLoad(opt_type, opt_alloca, "get.result");
    }

    /// Emit list.clone() - creates a deep copy of the list.
    /// Uses inline LLVM codegen (no runtime function call).
    fn emitListClone(self: *Emitter, list_ptr: llvm.ValueRef, method: *ast.MethodCall) EmitError!llvm.ValueRef {
        const func = self.current_function orelse return EmitError.InvalidAST;

        // Get element type info
        const element_type = self.getListElementType(method.object) orelse return EmitError.InvalidAST;
        const element_llvm_type = self.typeToLLVM(element_type);
        const element_size = self.getLLVMTypeSize(element_llvm_type);

        const list_type = self.getListStructType();
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);

        // Load current ptr and len from the source list
        const src_ptr_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, list_type, list_ptr, 0, "clone.src_ptr_ptr");
        const src_len_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, list_type, list_ptr, 1, "clone.src_len_ptr");

        const src_ptr = self.builder.buildLoad(ptr_type, src_ptr_ptr, "clone.src_ptr");
        const src_len = self.builder.buildLoad(i32_type, src_len_ptr, "clone.src_len");

        // Allocate result list on the stack
        const result_alloca = self.builder.buildAlloca(list_type, "clone.result");

        // Check if source list is empty
        const zero = llvm.Const.int32(self.ctx, 0);
        const is_empty = self.builder.buildICmp(llvm.c.LLVMIntEQ, src_len, zero, "clone.is_empty");

        // Create basic blocks
        const empty_bb = llvm.appendBasicBlock(self.ctx, func, "clone.empty");
        const copy_bb = llvm.appendBasicBlock(self.ctx, func, "clone.copy");
        const merge_bb = llvm.appendBasicBlock(self.ctx, func, "clone.merge");

        _ = self.builder.buildCondBr(is_empty, empty_bb, copy_bb);

        // --- Empty block: return empty list ---
        self.builder.positionAtEnd(empty_bb);

        // Store null ptr, 0 len, 0 capacity
        const empty_ptr_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, list_type, result_alloca, 0, "clone.empty_ptr_field");
        _ = self.builder.buildStore(llvm.c.LLVMConstPointerNull(ptr_type), empty_ptr_field);
        const empty_len_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, list_type, result_alloca, 1, "clone.empty_len_field");
        _ = self.builder.buildStore(zero, empty_len_field);
        const empty_cap_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, list_type, result_alloca, 2, "clone.empty_cap_field");
        _ = self.builder.buildStore(zero, empty_cap_field);

        _ = self.builder.buildBr(merge_bb);

        // --- Copy block: allocate and copy data ---
        self.builder.positionAtEnd(copy_bb);

        // Calculate allocation size: len * element_size
        const len_i64 = llvm.c.LLVMBuildSExt(self.builder.ref, src_len, i64_type, "clone.len_i64");
        const elem_size_val = llvm.Const.int64(self.ctx, @intCast(element_size));
        const alloc_size = llvm.c.LLVMBuildMul(self.builder.ref, len_i64, elem_size_val, "clone.alloc_size");

        // Allocate new memory
        const malloc_fn = self.getOrDeclareMalloc();
        var malloc_args = [_]llvm.ValueRef{alloc_size};
        const new_ptr = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(malloc_fn), malloc_fn, &malloc_args, "clone.new_ptr");

        // Copy data using memcpy
        const memcpy_fn = self.getOrDeclareMemcpy();
        var memcpy_args = [_]llvm.ValueRef{ new_ptr, src_ptr, alloc_size };
        _ = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(memcpy_fn), memcpy_fn, &memcpy_args, "");

        // Store new ptr, same len, capacity = len
        const copy_ptr_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, list_type, result_alloca, 0, "clone.copy_ptr_field");
        _ = self.builder.buildStore(new_ptr, copy_ptr_field);
        const copy_len_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, list_type, result_alloca, 1, "clone.copy_len_field");
        _ = self.builder.buildStore(src_len, copy_len_field);
        const copy_cap_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, list_type, result_alloca, 2, "clone.copy_cap_field");
        _ = self.builder.buildStore(src_len, copy_cap_field);

        _ = self.builder.buildBr(merge_bb);

        // --- Merge block ---
        self.builder.positionAtEnd(merge_bb);

        // Load and return the result list
        return self.builder.buildLoad(list_type, result_alloca, "clone.result_val");
    }

    /// Emit list.drop() - frees the list's memory.
    /// Uses inline LLVM codegen (no runtime function call).
    fn emitListDrop(self: *Emitter, list_ptr: llvm.ValueRef, method: *ast.MethodCall) EmitError!llvm.ValueRef {
        const func = self.current_function orelse return EmitError.InvalidAST;

        // Get element type info
        const element_type = self.getListElementType(method.object) orelse return EmitError.InvalidAST;
        const element_llvm_type = self.typeToLLVM(element_type);
        _ = self.getLLVMTypeSize(element_llvm_type); // validate element type

        const list_type = self.getListStructType();
        const ptr_type = llvm.Types.pointer(self.ctx);

        // Load the data pointer from the list
        const data_ptr_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, list_type, list_ptr, 0, "drop.data_ptr_ptr");
        const data_ptr = self.builder.buildLoad(ptr_type, data_ptr_ptr, "drop.data_ptr");

        // Check if pointer is null
        const null_ptr = llvm.c.LLVMConstPointerNull(ptr_type);
        const is_null = self.builder.buildICmp(llvm.c.LLVMIntEQ, data_ptr, null_ptr, "drop.is_null");

        // Create basic blocks
        const free_bb = llvm.appendBasicBlock(self.ctx, func, "drop.free");
        const done_bb = llvm.appendBasicBlock(self.ctx, func, "drop.done");

        _ = self.builder.buildCondBr(is_null, done_bb, free_bb);

        // --- Free block: ptr is not null, free the memory ---
        self.builder.positionAtEnd(free_bb);

        // Call free(ptr)
        const free_fn = self.getOrDeclareFree();
        var free_args = [_]llvm.ValueRef{data_ptr};
        _ = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(free_fn), free_fn, &free_args, "");

        // Reset the list to empty state
        const zero = llvm.Const.int32(self.ctx, 0);
        _ = self.builder.buildStore(null_ptr, data_ptr_ptr);
        const len_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, list_type, list_ptr, 1, "drop.len_ptr");
        _ = self.builder.buildStore(zero, len_ptr);
        const cap_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, list_type, list_ptr, 2, "drop.cap_ptr");
        _ = self.builder.buildStore(zero, cap_ptr);

        _ = self.builder.buildBr(done_bb);

        // --- Done block ---
        self.builder.positionAtEnd(done_bb);

        // Return void (represented as i32 0)
        return llvm.Const.int32(self.ctx, 0);
    }

    /// Emit list.set(index, value) - sets element at index.
    fn emitListSet(self: *Emitter, list_ptr: llvm.ValueRef, method: *ast.MethodCall, index: llvm.ValueRef, value: llvm.ValueRef) EmitError!llvm.ValueRef {
        const list_set_fn = self.getOrDeclareListSet();

        // Get element type
        const element_type = self.getListElementType(method.object) orelse return EmitError.InvalidAST;
        const element_llvm_type = self.typeToLLVM(element_type);
        const element_size = self.getLLVMTypeSize(element_llvm_type);

        // Allocate stack space for the value and get its pointer
        const value_alloca = self.builder.buildAlloca(element_llvm_type, "set.value");
        _ = self.builder.buildStore(value, value_alloca);

        var args = [_]llvm.ValueRef{
            list_ptr,
            llvm.Const.int64(self.ctx, @intCast(element_size)),
            index,
            value_alloca,
        };
        _ = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(list_set_fn), list_set_fn, &args, "");

        // Return void (represented as i32 0 for now)
        return llvm.Const.int32(self.ctx, 0);
    }

    /// Emit list.first() - returns first element as Optional.
    fn emitListFirst(self: *Emitter, list_ptr: llvm.ValueRef, method: *ast.MethodCall) EmitError!llvm.ValueRef {
        const list_first_fn = self.getOrDeclareListFirst();

        // Get element type
        const element_type = self.getListElementType(method.object) orelse return EmitError.InvalidAST;
        const element_llvm_type = self.typeToLLVM(element_type);
        const element_size = self.getLLVMTypeSize(element_llvm_type);

        // Allocate stack space for the output value
        const value_out = self.builder.buildAlloca(element_llvm_type, "first.value");

        var args = [_]llvm.ValueRef{
            list_ptr,
            llvm.Const.int64(self.ctx, @intCast(element_size)),
            value_out,
        };
        const success = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(list_first_fn), list_first_fn, &args, "first.success");

        // Build optional type: { i1, T }
        const i1_type = llvm.Types.int1(self.ctx);
        var opt_fields = [_]llvm.TypeRef{ i1_type, element_llvm_type };
        const opt_type = llvm.Types.struct_(self.ctx, &opt_fields, false);

        // Allocate the optional result
        const opt_alloca = self.builder.buildAlloca(opt_type, "first.optional");

        // Store the tag (success bool)
        const tag_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_type, opt_alloca, 0, "first.tag_ptr");
        _ = self.builder.buildStore(success, tag_ptr);

        // Store the value (if success)
        const value_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_type, opt_alloca, 1, "first.value_ptr");
        const loaded_value = self.builder.buildLoad(element_llvm_type, value_out, "first.loaded");
        _ = self.builder.buildStore(loaded_value, value_ptr);

        // Load and return the optional
        return self.builder.buildLoad(opt_type, opt_alloca, "first.result");
    }

    /// Emit list.last() - returns last element as Optional.
    fn emitListLast(self: *Emitter, list_ptr: llvm.ValueRef, method: *ast.MethodCall) EmitError!llvm.ValueRef {
        const list_last_fn = self.getOrDeclareListLast();

        // Get element type
        const element_type = self.getListElementType(method.object) orelse return EmitError.InvalidAST;
        const element_llvm_type = self.typeToLLVM(element_type);
        const element_size = self.getLLVMTypeSize(element_llvm_type);

        // Allocate stack space for the output value
        const value_out = self.builder.buildAlloca(element_llvm_type, "last.value");

        var args = [_]llvm.ValueRef{
            list_ptr,
            llvm.Const.int64(self.ctx, @intCast(element_size)),
            value_out,
        };
        const success = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(list_last_fn), list_last_fn, &args, "last.success");

        // Build optional type: { i1, T }
        const i1_type = llvm.Types.int1(self.ctx);
        var opt_fields = [_]llvm.TypeRef{ i1_type, element_llvm_type };
        const opt_type = llvm.Types.struct_(self.ctx, &opt_fields, false);

        // Allocate the optional result
        const opt_alloca = self.builder.buildAlloca(opt_type, "last.optional");

        // Store the tag (success bool)
        const tag_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_type, opt_alloca, 0, "last.tag_ptr");
        _ = self.builder.buildStore(success, tag_ptr);

        // Store the value (if success)
        const value_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_type, opt_alloca, 1, "last.value_ptr");
        const loaded_value = self.builder.buildLoad(element_llvm_type, value_out, "last.loaded");
        _ = self.builder.buildStore(loaded_value, value_ptr);

        // Load and return the optional
        return self.builder.buildLoad(opt_type, opt_alloca, "last.result");
    }

    /// Emit list.clear() - clears the list.
    fn emitListClear(self: *Emitter, list_ptr: llvm.ValueRef) EmitError!llvm.ValueRef {
        const list_clear_fn = self.getOrDeclareListClear();
        var args = [_]llvm.ValueRef{list_ptr};
        _ = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(list_clear_fn), list_clear_fn, &args, "");
        return llvm.Const.int32(self.ctx, 0);
    }

    // ========================================================================
    // String Methods (heap-allocated string)
    // ========================================================================

    /// Emit String.new() - creates an empty string.
    fn emitStringNew(self: *Emitter) EmitError!llvm.ValueRef {
        // Build String struct type (same as List)
        const string_type = self.getStringStructType();

        // Create an empty string struct { null, 0, 0 } directly
        const null_ptr = llvm.c.LLVMConstNull(llvm.Types.pointer(self.ctx));
        const zero_i32 = llvm.Const.int32(self.ctx, 0);

        var values = [_]llvm.ValueRef{ null_ptr, zero_i32, zero_i32 };
        return llvm.c.LLVMConstNamedStruct(string_type, &values, 3);
    }

    /// Emit String.from(s) - creates a string from a string literal.
    /// Implemented inline: calculates length, allocates memory, copies content.
    fn emitStringFrom(self: *Emitter, method: *ast.MethodCall) EmitError!llvm.ValueRef {
        if (method.args.len != 1) return EmitError.InvalidAST;

        // Emit the string literal argument (pointer to null-terminated string)
        const str_literal = try self.emitExpr(method.args[0]);

        const string_type = self.getStringStructType();
        const i32_type = llvm.Types.int32(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);

        // Call strlen to get the length
        const strlen_fn = self.getOrDeclareStrlen();
        var strlen_args = [_]llvm.ValueRef{str_literal};
        const len_i64 = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(strlen_fn), strlen_fn, &strlen_args, "str.len");

        // Convert to i32 (String uses i32 for len)
        const len_i32 = llvm.c.LLVMBuildTrunc(self.builder.ref, len_i64, i32_type, "str.len_i32");

        // Allocate memory: len + 1 for null terminator
        const one_i32 = llvm.Const.int32(self.ctx, 1);
        const alloc_len = llvm.c.LLVMBuildAdd(self.builder.ref, len_i32, one_i32, "str.alloc_len");
        const alloc_len_i64 = llvm.c.LLVMBuildSExt(self.builder.ref, alloc_len, i64_type, "str.alloc_len_i64");

        // Call malloc
        const malloc_fn = self.getOrDeclareMalloc();
        var malloc_args = [_]llvm.ValueRef{alloc_len_i64};
        const new_ptr = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(malloc_fn), malloc_fn, &malloc_args, "str.ptr");

        // Call memcpy to copy content (including null terminator)
        const memcpy_fn = self.getOrDeclareMemcpy();
        var memcpy_args = [_]llvm.ValueRef{ new_ptr, str_literal, alloc_len_i64 };
        _ = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(memcpy_fn), memcpy_fn, &memcpy_args, "");

        // Build the String struct: { ptr, len, len } (capacity = len for from())
        const string_alloca = self.builder.buildAlloca(string_type, "str.result");
        const ptr_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, string_alloca, 0, "str.ptr_field");
        const len_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, string_alloca, 1, "str.len_field");
        const cap_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, string_alloca, 2, "str.cap_field");

        _ = self.builder.buildStore(new_ptr, ptr_field);
        _ = self.builder.buildStore(len_i32, len_field);
        _ = self.builder.buildStore(len_i32, cap_field); // capacity = len for from()

        // Load and return the struct value
        return self.builder.buildLoad(string_type, string_alloca, "str.from.result");
    }

    /// Emit String.with_capacity(n) - creates a string with pre-allocated capacity.
    /// Implemented inline: allocates memory using malloc, returns { ptr, 0, capacity }.
    fn emitStringWithCapacity(self: *Emitter, method: *ast.MethodCall) EmitError!llvm.ValueRef {
        if (method.args.len != 1) return EmitError.InvalidAST;

        // Emit the capacity argument (i32)
        const capacity = try self.emitExpr(method.args[0]);

        const string_type = self.getStringStructType();
        const i64_type = llvm.Types.int64(self.ctx);

        // Allocate memory: capacity + 1 for null terminator
        const one_i32 = llvm.Const.int32(self.ctx, 1);
        const alloc_size_i32 = llvm.c.LLVMBuildAdd(self.builder.ref, capacity, one_i32, "str.alloc_size");
        const alloc_size = llvm.c.LLVMBuildSExt(self.builder.ref, alloc_size_i32, i64_type, "str.alloc_size_i64");

        // Call malloc
        const malloc_fn = self.getOrDeclareMalloc();
        var malloc_args = [_]llvm.ValueRef{alloc_size};
        const new_ptr = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(malloc_fn), malloc_fn, &malloc_args, "str.ptr");

        // Build the String struct: { ptr, 0, capacity }
        const string_alloca = self.builder.buildAlloca(string_type, "str.result");
        const ptr_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, string_alloca, 0, "str.ptr_field");
        const len_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, string_alloca, 1, "str.len_field");
        const cap_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, string_alloca, 2, "str.cap_field");

        _ = self.builder.buildStore(new_ptr, ptr_field);
        _ = self.builder.buildStore(llvm.Const.int32(self.ctx, 0), len_field); // len = 0
        _ = self.builder.buildStore(capacity, cap_field); // capacity from argument

        // Load and return the struct value
        return self.builder.buildLoad(string_type, string_alloca, "str.with_cap.result");
    }

    /// Emit string.len() for heap-allocated String type - returns the byte length.
    fn emitStringDataLen(self: *Emitter, str_ptr: llvm.ValueRef) EmitError!llvm.ValueRef {
        const string_type = self.getStringStructType();
        const len_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, str_ptr, 1, "str.len_ptr");
        return self.builder.buildLoad(llvm.Types.int32(self.ctx), len_ptr, "str.len");
    }

    /// Emit string.is_empty() for heap-allocated String type - checks if the string is empty.
    fn emitStringDataIsEmpty(self: *Emitter, str_ptr: llvm.ValueRef) EmitError!llvm.ValueRef {
        const len = try self.emitStringDataLen(str_ptr);
        const zero = llvm.Const.int32(self.ctx, 0);
        return self.builder.buildICmp(llvm.c.LLVMIntEQ, len, zero, "str.is_empty");
    }

    /// Emit string.capacity() - returns the allocated capacity.
    fn emitStringCapacity(self: *Emitter, str_ptr: llvm.ValueRef) EmitError!llvm.ValueRef {
        const string_type = self.getStringStructType();
        const cap_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, str_ptr, 2, "str.cap_ptr");
        return self.builder.buildLoad(llvm.Types.int32(self.ctx), cap_ptr, "str.cap");
    }

    /// Emit string.push(char) - appends a character (UTF-8 encoded).
    /// Implemented inline with growth logic similar to List.push.
    fn emitStringPush(self: *Emitter, str_ptr: llvm.ValueRef, char_val: llvm.ValueRef) EmitError!llvm.ValueRef {
        const func = self.current_function orelse return EmitError.InvalidAST;

        const string_type = self.getStringStructType();
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        const i8_type = llvm.Types.int8(self.ctx);

        // Load current ptr, len, capacity
        const ptr_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, str_ptr, 0, "push.ptr_ptr");
        const len_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, str_ptr, 1, "push.len_ptr");
        const cap_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, str_ptr, 2, "push.cap_ptr");

        const current_ptr = self.builder.buildLoad(ptr_type, ptr_ptr, "push.current_ptr");
        const current_len = self.builder.buildLoad(i32_type, len_ptr, "push.current_len");
        const current_cap = self.builder.buildLoad(i32_type, cap_ptr, "push.current_cap");

        // For now, assume single-byte character (ASCII). UTF-8 multi-byte would need more logic.
        // Need 1 extra byte for the character
        const one = llvm.Const.int32(self.ctx, 1);
        const new_len = llvm.c.LLVMBuildAdd(self.builder.ref, current_len, one, "push.new_len");

        // Check if we need to grow: new_len >= capacity (need space for null terminator)
        const need_grow = self.builder.buildICmp(llvm.c.LLVMIntSGE, new_len, current_cap, "push.need_grow");

        // Create basic blocks for growth path
        const grow_bb = llvm.appendBasicBlock(self.ctx, func, "push.grow");
        const store_bb = llvm.appendBasicBlock(self.ctx, func, "push.store");

        _ = self.builder.buildCondBr(need_grow, grow_bb, store_bb);

        // --- Grow block ---
        self.builder.positionAtEnd(grow_bb);

        // New capacity = max(16, capacity * 2)
        const doubled_cap = llvm.c.LLVMBuildMul(self.builder.ref, current_cap, llvm.Const.int32(self.ctx, 2), "push.doubled");
        const sixteen = llvm.Const.int32(self.ctx, 16);
        const cmp_sixteen = self.builder.buildICmp(llvm.c.LLVMIntSGT, doubled_cap, sixteen, "push.cmp_sixteen");
        const new_cap = llvm.c.LLVMBuildSelect(self.builder.ref, cmp_sixteen, doubled_cap, sixteen, "push.new_cap");

        // Calculate new size in bytes: new_cap (includes space for null terminator)
        const new_cap_i64 = llvm.c.LLVMBuildSExt(self.builder.ref, new_cap, i64_type, "push.new_cap_i64");

        // Call realloc(ptr, new_size)
        const realloc_fn = self.getOrDeclareRealloc();
        var realloc_args = [_]llvm.ValueRef{ current_ptr, new_cap_i64 };
        const new_ptr = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(realloc_fn), realloc_fn, &realloc_args, "push.new_ptr");

        // Store new ptr and capacity
        _ = self.builder.buildStore(new_ptr, ptr_ptr);
        _ = self.builder.buildStore(new_cap, cap_ptr);

        _ = self.builder.buildBr(store_bb);

        // --- Store block ---
        self.builder.positionAtEnd(store_bb);

        // PHI for the data pointer (either current_ptr or new_ptr from grow)
        const phi = llvm.c.LLVMBuildPhi(self.builder.ref, ptr_type, "push.data_ptr");
        var incoming_values = [_]llvm.ValueRef{ current_ptr, new_ptr };
        var incoming_blocks = [_]llvm.BasicBlockRef{ llvm.c.LLVMGetPreviousBasicBlock(grow_bb), grow_bb };
        llvm.c.LLVMAddIncoming(phi, &incoming_values, &incoming_blocks, 2);

        // Calculate address: ptr + len
        const len_i64 = llvm.c.LLVMBuildSExt(self.builder.ref, current_len, i64_type, "push.len_i64");
        var gep_indices = [_]llvm.ValueRef{len_i64};
        const char_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, i8_type, phi, &gep_indices, 1, "push.char_ptr");

        // Truncate char (i32) to i8 and store
        const char_i8 = llvm.c.LLVMBuildTrunc(self.builder.ref, char_val, i8_type, "push.char_i8");
        _ = self.builder.buildStore(char_i8, char_ptr);

        // Store null terminator at len + 1
        const null_offset = llvm.c.LLVMBuildAdd(self.builder.ref, len_i64, llvm.Const.int64(self.ctx, 1), "push.null_offset");
        var null_indices = [_]llvm.ValueRef{null_offset};
        const null_ptr_addr = llvm.c.LLVMBuildGEP2(self.builder.ref, i8_type, phi, &null_indices, 1, "push.null_ptr");
        _ = self.builder.buildStore(llvm.Const.int(i8_type, 0, false), null_ptr_addr);

        // Store new length
        _ = self.builder.buildStore(new_len, len_ptr);

        // Return void (represented as i32 0 for now)
        return llvm.Const.int32(self.ctx, 0);
    }

    /// Emit string.concat(other) - concatenates two strings, returns new string.
    /// Implemented inline: allocates new buffer, copies both strings, returns new String struct.
    fn emitStringConcat(self: *Emitter, str_ptr: llvm.ValueRef, other_ptr: llvm.ValueRef) EmitError!llvm.ValueRef {
        const string_type = self.getStringStructType();
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        const i8_type = llvm.Types.int8(self.ctx);

        // Load ptr1, len1 from first string
        const ptr1_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, str_ptr, 0, "concat.ptr1_ptr");
        const len1_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, str_ptr, 1, "concat.len1_ptr");
        const ptr1 = self.builder.buildLoad(ptr_type, ptr1_ptr, "concat.ptr1");
        const len1 = self.builder.buildLoad(i32_type, len1_ptr, "concat.len1");

        // Load ptr2, len2 from second string
        const ptr2_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, other_ptr, 0, "concat.ptr2_ptr");
        const len2_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, other_ptr, 1, "concat.len2_ptr");
        const ptr2 = self.builder.buildLoad(ptr_type, ptr2_ptr, "concat.ptr2");
        const len2 = self.builder.buildLoad(i32_type, len2_ptr, "concat.len2");

        // total_len = len1 + len2
        const total_len = llvm.c.LLVMBuildAdd(self.builder.ref, len1, len2, "concat.total_len");

        // new_cap = total_len + 1 (for null terminator)
        const one = llvm.Const.int32(self.ctx, 1);
        const new_cap = llvm.c.LLVMBuildAdd(self.builder.ref, total_len, one, "concat.new_cap");

        // Allocate: malloc(new_cap)
        const new_cap_i64 = llvm.c.LLVMBuildSExt(self.builder.ref, new_cap, i64_type, "concat.new_cap_i64");
        const malloc_fn = self.getOrDeclareMalloc();
        var malloc_args = [_]llvm.ValueRef{new_cap_i64};
        const new_ptr = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(malloc_fn), malloc_fn, &malloc_args, "concat.new_ptr");

        // memcpy(new_ptr, ptr1, len1)
        const len1_i64 = llvm.c.LLVMBuildSExt(self.builder.ref, len1, i64_type, "concat.len1_i64");
        const memcpy_fn = self.getOrDeclareMemcpy();
        var memcpy_args1 = [_]llvm.ValueRef{ new_ptr, ptr1, len1_i64 };
        _ = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(memcpy_fn), memcpy_fn, &memcpy_args1, "");

        // memcpy(new_ptr + len1, ptr2, len2)
        var gep_indices = [_]llvm.ValueRef{len1_i64};
        const dest2 = llvm.c.LLVMBuildGEP2(self.builder.ref, i8_type, new_ptr, &gep_indices, 1, "concat.dest2");
        const len2_i64 = llvm.c.LLVMBuildSExt(self.builder.ref, len2, i64_type, "concat.len2_i64");
        var memcpy_args2 = [_]llvm.ValueRef{ dest2, ptr2, len2_i64 };
        _ = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(memcpy_fn), memcpy_fn, &memcpy_args2, "");

        // Store null terminator at new_ptr + total_len
        const total_len_i64 = llvm.c.LLVMBuildSExt(self.builder.ref, total_len, i64_type, "concat.total_len_i64");
        var null_indices = [_]llvm.ValueRef{total_len_i64};
        const null_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, i8_type, new_ptr, &null_indices, 1, "concat.null_ptr");
        _ = self.builder.buildStore(llvm.Const.int(i8_type, 0, false), null_ptr);

        // Build result String struct: { new_ptr, total_len, new_cap }
        const result_alloca = self.builder.buildAlloca(string_type, "concat.result");
        const result_ptr_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, result_alloca, 0, "concat.result_ptr");
        const result_len_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, result_alloca, 1, "concat.result_len");
        const result_cap_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, result_alloca, 2, "concat.result_cap");

        _ = self.builder.buildStore(new_ptr, result_ptr_field);
        _ = self.builder.buildStore(total_len, result_len_field);
        _ = self.builder.buildStore(new_cap, result_cap_field);

        // Return loaded struct value
        return self.builder.buildLoad(string_type, result_alloca, "concat.result_val");
    }

    /// Emit string.append(other) - appends other string to this one (mutates).
    /// Implemented inline with growth logic similar to push but for multiple bytes.
    fn emitStringAppend(self: *Emitter, str_ptr: llvm.ValueRef, other_ptr: llvm.ValueRef) EmitError!llvm.ValueRef {
        const func = self.current_function orelse return EmitError.InvalidAST;

        const string_type = self.getStringStructType();
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        const i8_type = llvm.Types.int8(self.ctx);

        // Load other's len first to check if empty
        const other_len_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, other_ptr, 1, "append.other_len_ptr");
        const other_len = self.builder.buildLoad(i32_type, other_len_ptr, "append.other_len");

        // Early return if other is empty: if (other.len == 0) return
        const zero = llvm.Const.int32(self.ctx, 0);
        const other_empty = self.builder.buildICmp(llvm.c.LLVMIntEQ, other_len, zero, "append.other_empty");

        const do_append_bb = llvm.appendBasicBlock(self.ctx, func, "append.do");
        const end_bb = llvm.appendBasicBlock(self.ctx, func, "append.end");

        _ = self.builder.buildCondBr(other_empty, end_bb, do_append_bb);

        // --- Do append block ---
        self.builder.positionAtEnd(do_append_bb);

        // Load self's ptr, len, capacity
        const ptr_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, str_ptr, 0, "append.ptr_ptr");
        const len_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, str_ptr, 1, "append.len_ptr");
        const cap_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, str_ptr, 2, "append.cap_ptr");

        const current_ptr = self.builder.buildLoad(ptr_type, ptr_ptr, "append.current_ptr");
        const current_len = self.builder.buildLoad(i32_type, len_ptr, "append.current_len");
        const current_cap = self.builder.buildLoad(i32_type, cap_ptr, "append.current_cap");

        // Load other's ptr
        const other_ptr_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, other_ptr, 0, "append.other_ptr_ptr");
        const other_data_ptr = self.builder.buildLoad(ptr_type, other_ptr_ptr, "append.other_data");

        // new_len = self.len + other.len
        const new_len = llvm.c.LLVMBuildAdd(self.builder.ref, current_len, other_len, "append.new_len");

        // need_space = new_len + 1 (for null terminator)
        const one = llvm.Const.int32(self.ctx, 1);
        const need_space = llvm.c.LLVMBuildAdd(self.builder.ref, new_len, one, "append.need_space");

        // Check if we need to grow: need_space > capacity
        const need_grow = self.builder.buildICmp(llvm.c.LLVMIntSGT, need_space, current_cap, "append.need_grow");

        const grow_bb = llvm.appendBasicBlock(self.ctx, func, "append.grow");
        const copy_bb = llvm.appendBasicBlock(self.ctx, func, "append.copy");

        _ = self.builder.buildCondBr(need_grow, grow_bb, copy_bb);

        // --- Grow block ---
        self.builder.positionAtEnd(grow_bb);

        // Calculate new capacity: start with current cap (or 16 if 0), double until >= need_space
        // For simplicity, use: new_cap = max(16, need_space * 2)
        const sixteen = llvm.Const.int32(self.ctx, 16);
        const need_space_doubled = llvm.c.LLVMBuildMul(self.builder.ref, need_space, llvm.Const.int32(self.ctx, 2), "append.need_doubled");
        const cmp = self.builder.buildICmp(llvm.c.LLVMIntSGT, need_space_doubled, sixteen, "append.cmp");
        const new_cap = llvm.c.LLVMBuildSelect(self.builder.ref, cmp, need_space_doubled, sixteen, "append.new_cap");

        // realloc(ptr, new_cap)
        const new_cap_i64 = llvm.c.LLVMBuildSExt(self.builder.ref, new_cap, i64_type, "append.new_cap_i64");
        const realloc_fn = self.getOrDeclareRealloc();
        var realloc_args = [_]llvm.ValueRef{ current_ptr, new_cap_i64 };
        const grown_ptr = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(realloc_fn), realloc_fn, &realloc_args, "append.grown_ptr");

        // Store new ptr and capacity
        _ = self.builder.buildStore(grown_ptr, ptr_ptr);
        _ = self.builder.buildStore(new_cap, cap_ptr);

        _ = self.builder.buildBr(copy_bb);

        // --- Copy block ---
        self.builder.positionAtEnd(copy_bb);

        // PHI for data pointer
        const data_phi = llvm.c.LLVMBuildPhi(self.builder.ref, ptr_type, "append.data_ptr");
        var incoming_values = [_]llvm.ValueRef{ current_ptr, grown_ptr };
        var incoming_blocks = [_]llvm.BasicBlockRef{ do_append_bb, grow_bb };
        llvm.c.LLVMAddIncoming(data_phi, &incoming_values, &incoming_blocks, 2);

        // memcpy(data_ptr + current_len, other_data, other_len)
        const current_len_i64 = llvm.c.LLVMBuildSExt(self.builder.ref, current_len, i64_type, "append.cur_len_i64");
        var dest_indices = [_]llvm.ValueRef{current_len_i64};
        const dest_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, i8_type, data_phi, &dest_indices, 1, "append.dest");

        const other_len_i64 = llvm.c.LLVMBuildSExt(self.builder.ref, other_len, i64_type, "append.other_len_i64");
        const memcpy_fn = self.getOrDeclareMemcpy();
        var memcpy_args = [_]llvm.ValueRef{ dest_ptr, other_data_ptr, other_len_i64 };
        _ = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(memcpy_fn), memcpy_fn, &memcpy_args, "");

        // Store null terminator at data_ptr + new_len
        const new_len_i64 = llvm.c.LLVMBuildSExt(self.builder.ref, new_len, i64_type, "append.new_len_i64");
        var null_indices = [_]llvm.ValueRef{new_len_i64};
        const null_ptr_addr = llvm.c.LLVMBuildGEP2(self.builder.ref, i8_type, data_phi, &null_indices, 1, "append.null_ptr");
        _ = self.builder.buildStore(llvm.Const.int(i8_type, 0, false), null_ptr_addr);

        // Update len
        _ = self.builder.buildStore(new_len, len_ptr);

        _ = self.builder.buildBr(end_bb);

        // --- End block ---
        self.builder.positionAtEnd(end_bb);

        return llvm.Const.int32(self.ctx, 0);
    }

    /// Emit string.as_str() - returns pointer to null-terminated C string.
    /// Simply returns the ptr field (which is already null-terminated).
    fn emitStringAsStr(self: *Emitter, str_ptr: llvm.ValueRef) EmitError!llvm.ValueRef {
        const string_type = self.getStringStructType();
        const ptr_type = llvm.Types.pointer(self.ctx);

        const ptr_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, str_ptr, 0, "asstr.ptr_ptr");
        return self.builder.buildLoad(ptr_type, ptr_ptr, "asstr.ptr");
    }

    /// Emit string.clear() - clears the string (keeps capacity).
    /// Sets len to 0 and writes null terminator at start (if ptr is not null).
    fn emitStringClear(self: *Emitter, str_ptr: llvm.ValueRef) EmitError!llvm.ValueRef {
        const func = self.current_function orelse return EmitError.InvalidAST;

        const string_type = self.getStringStructType();
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i8_type = llvm.Types.int8(self.ctx);

        // Set len = 0
        const len_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, str_ptr, 1, "clear.len_ptr");
        _ = self.builder.buildStore(llvm.Const.int32(self.ctx, 0), len_ptr);

        // Load data pointer
        const ptr_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, str_ptr, 0, "clear.ptr_ptr");
        const data_ptr = self.builder.buildLoad(ptr_type, ptr_ptr, "clear.data_ptr");

        // Check if ptr is not null before writing null terminator
        const null_ptr = llvm.Const.null_(ptr_type);
        const is_not_null = self.builder.buildICmp(llvm.c.LLVMIntNE, data_ptr, null_ptr, "clear.is_not_null");

        const write_bb = llvm.appendBasicBlock(self.ctx, func, "clear.write");
        const end_bb = llvm.appendBasicBlock(self.ctx, func, "clear.end");

        _ = self.builder.buildCondBr(is_not_null, write_bb, end_bb);

        // --- Write block ---
        self.builder.positionAtEnd(write_bb);
        _ = self.builder.buildStore(llvm.Const.int(i8_type, 0, false), data_ptr);
        _ = self.builder.buildBr(end_bb);

        // --- End block ---
        self.builder.positionAtEnd(end_bb);

        return llvm.Const.int32(self.ctx, 0);
    }

    /// Emit string.clone() - creates a deep copy.
    /// Allocates new buffer, copies data, returns new String struct.
    fn emitStringClone(self: *Emitter, str_ptr: llvm.ValueRef) EmitError!llvm.ValueRef {
        const string_type = self.getStringStructType();
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        const i8_type = llvm.Types.int8(self.ctx);

        // Load source ptr, len
        const src_ptr_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, str_ptr, 0, "clone.src_ptr_ptr");
        const src_len_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, str_ptr, 1, "clone.src_len_ptr");
        const src_ptr = self.builder.buildLoad(ptr_type, src_ptr_ptr, "clone.src_ptr");
        const src_len = self.builder.buildLoad(i32_type, src_len_ptr, "clone.src_len");

        // new_cap = len + 1
        const one = llvm.Const.int32(self.ctx, 1);
        const new_cap = llvm.c.LLVMBuildAdd(self.builder.ref, src_len, one, "clone.new_cap");

        // malloc(new_cap)
        const new_cap_i64 = llvm.c.LLVMBuildSExt(self.builder.ref, new_cap, i64_type, "clone.new_cap_i64");
        const malloc_fn = self.getOrDeclareMalloc();
        var malloc_args = [_]llvm.ValueRef{new_cap_i64};
        const new_ptr = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(malloc_fn), malloc_fn, &malloc_args, "clone.new_ptr");

        // memcpy(new_ptr, src_ptr, len)
        const src_len_i64 = llvm.c.LLVMBuildSExt(self.builder.ref, src_len, i64_type, "clone.src_len_i64");
        const memcpy_fn = self.getOrDeclareMemcpy();
        var memcpy_args = [_]llvm.ValueRef{ new_ptr, src_ptr, src_len_i64 };
        _ = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(memcpy_fn), memcpy_fn, &memcpy_args, "");

        // Write null terminator
        var null_indices = [_]llvm.ValueRef{src_len_i64};
        const null_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, i8_type, new_ptr, &null_indices, 1, "clone.null_ptr");
        _ = self.builder.buildStore(llvm.Const.int(i8_type, 0, false), null_ptr);

        // Build result String struct: { new_ptr, len, new_cap }
        const result_alloca = self.builder.buildAlloca(string_type, "clone.result");
        const result_ptr_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, result_alloca, 0, "clone.result_ptr");
        const result_len_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, result_alloca, 1, "clone.result_len");
        const result_cap_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, result_alloca, 2, "clone.result_cap");

        _ = self.builder.buildStore(new_ptr, result_ptr_field);
        _ = self.builder.buildStore(src_len, result_len_field);
        _ = self.builder.buildStore(new_cap, result_cap_field);

        return self.builder.buildLoad(string_type, result_alloca, "clone.result_val");
    }

    /// Emit string.drop() - frees the string's memory.
    /// Calls free(ptr) and zeroes the struct.
    fn emitStringDrop(self: *Emitter, str_ptr: llvm.ValueRef) EmitError!llvm.ValueRef {
        const string_type = self.getStringStructType();
        const ptr_type = llvm.Types.pointer(self.ctx);

        // Load ptr
        const ptr_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, str_ptr, 0, "drop.ptr_ptr");
        const data_ptr = self.builder.buildLoad(ptr_type, ptr_ptr, "drop.data_ptr");

        // free(ptr)
        const free_fn = self.getOrDeclareFree();
        var free_args = [_]llvm.ValueRef{data_ptr};
        _ = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(free_fn), free_fn, &free_args, "");

        // Zero the struct: ptr=null, len=0, cap=0
        const null_ptr = llvm.Const.null_(ptr_type);
        const zero = llvm.Const.int32(self.ctx, 0);

        _ = self.builder.buildStore(null_ptr, ptr_ptr);
        const len_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, str_ptr, 1, "drop.len_ptr");
        _ = self.builder.buildStore(zero, len_ptr);
        const cap_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, str_ptr, 2, "drop.cap_ptr");
        _ = self.builder.buildStore(zero, cap_ptr);

        return llvm.Const.int32(self.ctx, 0);
    }

    /// Emit string.eq(other) - compares two strings for equality.
    /// Returns true if lengths match and all bytes match.
    fn emitStringEq(self: *Emitter, str_ptr: llvm.ValueRef, other_ptr: llvm.ValueRef) EmitError!llvm.ValueRef {
        const func = self.current_function orelse return EmitError.InvalidAST;

        const string_type = self.getStringStructType();
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);
        const i1_type = llvm.Types.int1(self.ctx);

        // Load len1, len2
        const len1_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, str_ptr, 1, "eq.len1_ptr");
        const len2_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, other_ptr, 1, "eq.len2_ptr");
        const len1 = self.builder.buildLoad(i32_type, len1_ptr, "eq.len1");
        const len2 = self.builder.buildLoad(i32_type, len2_ptr, "eq.len2");

        // First check: lengths must be equal
        const lens_equal = self.builder.buildICmp(llvm.c.LLVMIntEQ, len1, len2, "eq.lens_equal");

        const check_bytes_bb = llvm.appendBasicBlock(self.ctx, func, "eq.check_bytes");
        const end_bb = llvm.appendBasicBlock(self.ctx, func, "eq.end");

        _ = self.builder.buildCondBr(lens_equal, check_bytes_bb, end_bb);

        // --- Check bytes block ---
        self.builder.positionAtEnd(check_bytes_bb);

        // Use memcmp for byte comparison
        const ptr1_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, str_ptr, 0, "eq.ptr1_ptr");
        const ptr2_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, other_ptr, 0, "eq.ptr2_ptr");
        const ptr1 = self.builder.buildLoad(ptr_type, ptr1_ptr, "eq.ptr1");
        const ptr2 = self.builder.buildLoad(ptr_type, ptr2_ptr, "eq.ptr2");

        const i64_type = llvm.Types.int64(self.ctx);
        const len_i64 = llvm.c.LLVMBuildSExt(self.builder.ref, len1, i64_type, "eq.len_i64");
        const memcmp_fn = self.getOrDeclareMemcmp();
        var memcmp_args = [_]llvm.ValueRef{ ptr1, ptr2, len_i64 };
        const cmp_result = self.builder.buildCall(llvm.c.LLVMGlobalGetValueType(memcmp_fn), memcmp_fn, &memcmp_args, "eq.cmp_result");

        // memcmp returns 0 if equal
        const bytes_equal = self.builder.buildICmp(llvm.c.LLVMIntEQ, cmp_result, llvm.Const.int32(self.ctx, 0), "eq.bytes_equal");

        _ = self.builder.buildBr(end_bb);

        // --- End block ---
        self.builder.positionAtEnd(end_bb);

        // PHI: true from check_bytes if bytes_equal, false from entry if lens differ
        const result_phi = llvm.c.LLVMBuildPhi(self.builder.ref, i1_type, "eq.result");
        const false_val = llvm.Const.int(i1_type, 0, false);
        var incoming_values = [_]llvm.ValueRef{ false_val, bytes_equal };
        const entry_bb = llvm.c.LLVMGetPreviousBasicBlock(check_bytes_bb);
        var incoming_blocks = [_]llvm.BasicBlockRef{ entry_bb, check_bytes_bb };
        llvm.c.LLVMAddIncoming(result_phi, &incoming_values, &incoming_blocks, 2);

        return result_phi;
    }

    /// Emit string.hash() for heap-allocated String type - computes FNV-1a hash.
    fn emitStringDataHash(self: *Emitter, str_ptr: llvm.ValueRef) EmitError!llvm.ValueRef {
        const func = self.current_function orelse return EmitError.InvalidAST;

        const string_type = self.getStringStructType();
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        const i8_type = llvm.Types.int8(self.ctx);

        // FNV-1a constants
        const FNV_OFFSET: u64 = 14695981039346656037;
        const FNV_PRIME: u64 = 1099511628211;

        // Load ptr, len
        const ptr_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, str_ptr, 0, "hash.ptr_ptr");
        const len_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, string_type, str_ptr, 1, "hash.len_ptr");
        const data_ptr = self.builder.buildLoad(ptr_type, ptr_ptr, "hash.data_ptr");
        const len = self.builder.buildLoad(i32_type, len_ptr, "hash.len");

        // Initialize hash = FNV_OFFSET
        const hash_alloca = self.builder.buildAlloca(i64_type, "hash.val");
        _ = self.builder.buildStore(llvm.Const.int(i64_type, @bitCast(FNV_OFFSET), false), hash_alloca);

        // Loop index
        const idx_alloca = self.builder.buildAlloca(i32_type, "hash.idx");
        _ = self.builder.buildStore(llvm.Const.int32(self.ctx, 0), idx_alloca);

        // Loop blocks
        const loop_cond_bb = llvm.appendBasicBlock(self.ctx, func, "hash.loop_cond");
        const loop_body_bb = llvm.appendBasicBlock(self.ctx, func, "hash.loop_body");
        const loop_end_bb = llvm.appendBasicBlock(self.ctx, func, "hash.loop_end");

        _ = self.builder.buildBr(loop_cond_bb);

        // --- Loop condition ---
        self.builder.positionAtEnd(loop_cond_bb);
        const idx = self.builder.buildLoad(i32_type, idx_alloca, "hash.idx_val");
        const cond = self.builder.buildICmp(llvm.c.LLVMIntSLT, idx, len, "hash.cond");
        _ = self.builder.buildCondBr(cond, loop_body_bb, loop_end_bb);

        // --- Loop body ---
        self.builder.positionAtEnd(loop_body_bb);

        // Load byte at data_ptr[idx]
        const idx_i64 = llvm.c.LLVMBuildSExt(self.builder.ref, idx, i64_type, "hash.idx_i64");
        var gep_indices = [_]llvm.ValueRef{idx_i64};
        const byte_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, i8_type, data_ptr, &gep_indices, 1, "hash.byte_ptr");
        const byte = self.builder.buildLoad(i8_type, byte_ptr, "hash.byte");
        const byte_i64 = llvm.c.LLVMBuildZExt(self.builder.ref, byte, i64_type, "hash.byte_i64");

        // hash ^= byte
        const current_hash = self.builder.buildLoad(i64_type, hash_alloca, "hash.current");
        const xored = llvm.c.LLVMBuildXor(self.builder.ref, current_hash, byte_i64, "hash.xored");

        // hash *= FNV_PRIME (wrapping)
        const multiplied = llvm.c.LLVMBuildMul(self.builder.ref, xored, llvm.Const.int(i64_type, @bitCast(FNV_PRIME), false), "hash.multiplied");
        _ = self.builder.buildStore(multiplied, hash_alloca);

        // idx++
        const next_idx = llvm.c.LLVMBuildAdd(self.builder.ref, idx, llvm.Const.int32(self.ctx, 1), "hash.next_idx");
        _ = self.builder.buildStore(next_idx, idx_alloca);

        _ = self.builder.buildBr(loop_cond_bb);

        // --- Loop end ---
        self.builder.positionAtEnd(loop_end_bb);

        // Return hash as i64
        return self.builder.buildLoad(i64_type, hash_alloca, "hash.result");
    }

    /// Get the LLVM struct type for String: { ptr, i32, i32 }
    fn getStringStructType(self: *Emitter) llvm.TypeRef {
        var fields = [_]llvm.TypeRef{
            llvm.Types.pointer(self.ctx), // ptr
            llvm.Types.int32(self.ctx), // len
            llvm.Types.int32(self.ctx), // capacity
        };
        return llvm.Types.struct_(self.ctx, &fields, false);
    }

    /// Get the LLVM struct type for List: { ptr, i32, i32 }
    fn getListStructType(self: *Emitter) llvm.TypeRef {
        var fields = [_]llvm.TypeRef{
            llvm.Types.pointer(self.ctx), // ptr
            llvm.Types.int32(self.ctx), // len
            llvm.Types.int32(self.ctx), // capacity
        };
        return llvm.Types.struct_(self.ctx, &fields, false);
    }

    /// Get type size in bytes from LLVM type.
    fn getLLVMTypeSize(self: *Emitter, ty: llvm.TypeRef) u64 {
        const type_kind = llvm.getTypeKind(ty);
        return switch (type_kind) {
            llvm.c.LLVMIntegerTypeKind => @as(u64, llvm.c.LLVMGetIntTypeWidth(ty)) / 8,
            llvm.c.LLVMFloatTypeKind => 4,
            llvm.c.LLVMDoubleTypeKind => 8,
            llvm.c.LLVMPointerTypeKind => 8, // 64-bit pointers
            llvm.c.LLVMStructTypeKind => blk: {
                // Sum of field sizes (simplified - doesn't account for padding)
                const num_fields = llvm.c.LLVMCountStructElementTypes(ty);
                var total: u64 = 0;
                for (0..num_fields) |i| {
                    const field_ty = llvm.c.LLVMStructGetTypeAtIndex(ty, @intCast(i));
                    total += self.getLLVMTypeSize(field_ty);
                }
                break :blk total;
            },
            llvm.c.LLVMArrayTypeKind => blk: {
                const elem_ty = llvm.c.LLVMGetElementType(ty);
                const elem_count = llvm.c.LLVMGetArrayLength(ty);
                break :blk self.getLLVMTypeSize(elem_ty) * @as(u64, elem_count);
            },
            else => 8, // Default
        };
    }

    /// Get alignment as log2 for a type.
    fn getAlignLog2(self: *Emitter, ty: llvm.TypeRef) u8 {
        const size = self.getLLVMTypeSize(ty);
        // Alignment is typically min(8, size) for basic types
        const alignment: u8 = if (size >= 8) 3 else if (size >= 4) 2 else if (size >= 2) 1 else 0;
        return alignment;
    }

    // ---- List runtime function declarations ----

    fn getOrDeclareListNew(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_list_new";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // ListHeader klar_list_new()
        const list_type = self.getListStructType();
        var param_types = [_]llvm.TypeRef{};
        const fn_type = llvm.c.LLVMFunctionType(list_type, &param_types, 0, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareListLen(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_list_len";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // i32 klar_list_len(ListHeader*)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);
        var param_types = [_]llvm.TypeRef{ptr_type};
        const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 1, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareListIsEmpty(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_list_is_empty";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // bool klar_list_is_empty(ListHeader*)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i1_type = llvm.Types.int1(self.ctx);
        var param_types = [_]llvm.TypeRef{ptr_type};
        const fn_type = llvm.c.LLVMFunctionType(i1_type, &param_types, 1, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareListCapacity(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_list_capacity";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // i32 klar_list_capacity(ListHeader*)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);
        var param_types = [_]llvm.TypeRef{ptr_type};
        const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 1, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareListPush(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_list_push";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // void klar_list_push(ListHeader*, usize element_size, u8 align_log2, void* value)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        const i8_type = llvm.Types.int8(self.ctx);
        const void_type = llvm.Types.void_(self.ctx);
        var param_types = [_]llvm.TypeRef{ ptr_type, i64_type, i8_type, ptr_type };
        const fn_type = llvm.c.LLVMFunctionType(void_type, &param_types, 4, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareListPop(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_list_pop";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // bool klar_list_pop(ListHeader*, usize element_size, void* value_out)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        const i1_type = llvm.Types.int1(self.ctx);
        var param_types = [_]llvm.TypeRef{ ptr_type, i64_type, ptr_type };
        const fn_type = llvm.c.LLVMFunctionType(i1_type, &param_types, 3, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareListGet(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_list_get";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // bool klar_list_get(ListHeader*, usize element_size, i32 index, void* value_out)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);
        const i1_type = llvm.Types.int1(self.ctx);
        var param_types = [_]llvm.TypeRef{ ptr_type, i64_type, i32_type, ptr_type };
        const fn_type = llvm.c.LLVMFunctionType(i1_type, &param_types, 4, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareListSet(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_list_set";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // void klar_list_set(ListHeader*, usize element_size, i32 index, void* value)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);
        const void_type = llvm.Types.void_(self.ctx);
        var param_types = [_]llvm.TypeRef{ ptr_type, i64_type, i32_type, ptr_type };
        const fn_type = llvm.c.LLVMFunctionType(void_type, &param_types, 4, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareListFirst(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_list_first";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // bool klar_list_first(ListHeader*, usize element_size, void* value_out)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        const i1_type = llvm.Types.int1(self.ctx);
        var param_types = [_]llvm.TypeRef{ ptr_type, i64_type, ptr_type };
        const fn_type = llvm.c.LLVMFunctionType(i1_type, &param_types, 3, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareListLast(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_list_last";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // bool klar_list_last(ListHeader*, usize element_size, void* value_out)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        const i1_type = llvm.Types.int1(self.ctx);
        var param_types = [_]llvm.TypeRef{ ptr_type, i64_type, ptr_type };
        const fn_type = llvm.c.LLVMFunctionType(i1_type, &param_types, 3, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareListClear(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_list_clear";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // void klar_list_clear(ListHeader*)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const void_type = llvm.Types.void_(self.ctx);
        var param_types = [_]llvm.TypeRef{ptr_type};
        const fn_type = llvm.c.LLVMFunctionType(void_type, &param_types, 1, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareListWithCapacity(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_list_with_capacity";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // ListHeader klar_list_with_capacity(usize element_size, u8 align_log2, i32 capacity)
        const list_type = self.getListStructType();
        const i64_type = llvm.Types.int64(self.ctx);
        const i8_type = llvm.Types.int8(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);
        var param_types = [_]llvm.TypeRef{ i64_type, i8_type, i32_type };
        const fn_type = llvm.c.LLVMFunctionType(list_type, &param_types, 3, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareListClone(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_list_clone";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // ListHeader klar_list_clone(ListHeader*, usize element_size, u8 align_log2)
        const list_type = self.getListStructType();
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        const i8_type = llvm.Types.int8(self.ctx);
        var param_types = [_]llvm.TypeRef{ ptr_type, i64_type, i8_type };
        const fn_type = llvm.c.LLVMFunctionType(list_type, &param_types, 3, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareListDrop(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_list_drop";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // void klar_list_drop(ListHeader*, usize element_size, u8 align_log2)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        const i8_type = llvm.Types.int8(self.ctx);
        const void_type = llvm.Types.void_(self.ctx);
        var param_types = [_]llvm.TypeRef{ ptr_type, i64_type, i8_type };
        const fn_type = llvm.c.LLVMFunctionType(void_type, &param_types, 3, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    // ========================================================================
    // String Runtime Function Declarations
    // ========================================================================

    fn getOrDeclareStringFrom(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_string_from";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }
        // StringHeader klar_string_from(const char* src)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const string_type = self.getStringStructType();
        var param_types = [_]llvm.TypeRef{ptr_type};
        const fn_type = llvm.c.LLVMFunctionType(string_type, &param_types, 1, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareStringWithCapacity(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_string_with_capacity";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }
        // StringHeader klar_string_with_capacity(i32 capacity)
        const i32_type = llvm.Types.int32(self.ctx);
        const string_type = self.getStringStructType();
        var param_types = [_]llvm.TypeRef{i32_type};
        const fn_type = llvm.c.LLVMFunctionType(string_type, &param_types, 1, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareStringPushChar(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_string_push_char";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }
        // void klar_string_push_char(StringHeader* s, i32 codepoint)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);
        const void_type = llvm.Types.void_(self.ctx);
        var param_types = [_]llvm.TypeRef{ ptr_type, i32_type };
        const fn_type = llvm.c.LLVMFunctionType(void_type, &param_types, 2, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareStringConcat(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_string_concat";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }
        // StringHeader klar_string_concat(const StringHeader* a, const StringHeader* b)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const string_type = self.getStringStructType();
        var param_types = [_]llvm.TypeRef{ ptr_type, ptr_type };
        const fn_type = llvm.c.LLVMFunctionType(string_type, &param_types, 2, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareStringAppend(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_string_append";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }
        // void klar_string_append(StringHeader* s, const StringHeader* other)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const void_type = llvm.Types.void_(self.ctx);
        var param_types = [_]llvm.TypeRef{ ptr_type, ptr_type };
        const fn_type = llvm.c.LLVMFunctionType(void_type, &param_types, 2, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareStringAsPtr(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_string_as_ptr";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }
        // const char* klar_string_as_ptr(const StringHeader* s)
        const ptr_type = llvm.Types.pointer(self.ctx);
        var param_types = [_]llvm.TypeRef{ptr_type};
        const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 1, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareStringClear(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_string_clear";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }
        // void klar_string_clear(StringHeader* s)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const void_type = llvm.Types.void_(self.ctx);
        var param_types = [_]llvm.TypeRef{ptr_type};
        const fn_type = llvm.c.LLVMFunctionType(void_type, &param_types, 1, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareStringClone(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_string_clone_heap";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }
        // StringHeader klar_string_clone_heap(const StringHeader* s)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const string_type = self.getStringStructType();
        var param_types = [_]llvm.TypeRef{ptr_type};
        const fn_type = llvm.c.LLVMFunctionType(string_type, &param_types, 1, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareStringDrop(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_string_drop";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }
        // void klar_string_drop(StringHeader* s)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const void_type = llvm.Types.void_(self.ctx);
        var param_types = [_]llvm.TypeRef{ptr_type};
        const fn_type = llvm.c.LLVMFunctionType(void_type, &param_types, 1, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareStringEq(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_string_eq";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }
        // bool klar_string_eq(const StringHeader* a, const StringHeader* b)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const bool_type = llvm.Types.int1(self.ctx);
        var param_types = [_]llvm.TypeRef{ ptr_type, ptr_type };
        const fn_type = llvm.c.LLVMFunctionType(bool_type, &param_types, 2, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    fn getOrDeclareStringDataHash(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_string_hash_heap";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }
        // i64 klar_string_hash_heap(const StringHeader* s)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        var param_types = [_]llvm.TypeRef{ptr_type};
        const fn_type = llvm.c.LLVMFunctionType(i64_type, &param_types, 1, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    // ========================================================================
    // Array/Slice Methods
    // ========================================================================

    /// Emit array.len() - returns length as i32.
    /// For fixed-size arrays, returns the compile-time known size.
    /// For slices, loads the length field from the slice struct.
    fn emitArrayLen(self: *Emitter, expr: ast.Expr, object: llvm.ValueRef) EmitError!llvm.ValueRef {
        // For fixed-size arrays, we know the size at compile time
        if (self.getArraySize(expr)) |size| {
            return llvm.Const.int32(self.ctx, @intCast(size));
        }

        // For slices, load the length from the struct's second field
        // Slice is { ptr, i64 }
        const i64_type = llvm.Types.int64(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);

        // Build slice struct type
        var slice_fields = [_]llvm.TypeRef{
            llvm.Types.pointer(self.ctx), // data pointer
            i64_type, // length
        };
        const slice_type = llvm.Types.struct_(self.ctx, &slice_fields, false);

        // Need to get length from slice struct - field 1
        const len_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, slice_type, object, 1, "len_ptr");
        const len_i64 = self.builder.buildLoad(i64_type, len_ptr, "len");
        // Truncate from i64 to i32
        return llvm.c.LLVMBuildTrunc(self.builder.ref, len_i64, i32_type, "len_i32");
    }

    /// Emit array.is_empty() - returns true if len == 0.
    fn emitArrayIsEmpty(self: *Emitter, expr: ast.Expr, object: llvm.ValueRef) EmitError!llvm.ValueRef {
        // For fixed-size arrays, we know if it's empty at compile time
        if (self.getArraySize(expr)) |size| {
            return llvm.Const.int1(self.ctx, size == 0);
        }

        // For slices, compare length to 0
        const i64_type = llvm.Types.int64(self.ctx);

        // Build slice struct type
        var slice_fields = [_]llvm.TypeRef{
            llvm.Types.pointer(self.ctx),
            i64_type,
        };
        const slice_type = llvm.Types.struct_(self.ctx, &slice_fields, false);

        const len_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, slice_type, object, 1, "len_ptr");
        const len = self.builder.buildLoad(i64_type, len_ptr, "len");
        const zero = llvm.Const.int64(self.ctx, 0);
        return self.builder.buildICmp(llvm.c.LLVMIntEQ, len, zero, "is_empty");
    }

    /// Emit array.first() - returns Optional[T], Some(first element) or None if empty.
    fn emitArrayFirst(self: *Emitter, expr: ast.Expr, object: llvm.ValueRef) EmitError!llvm.ValueRef {
        const element_type = self.getArrayElementType(expr) orelse return EmitError.InvalidAST;
        const elem_llvm_type = self.typeToLLVM(element_type);
        const i64_type = llvm.Types.int64(self.ctx);

        // Build optional type: { i1 tag, T value } where tag 0 = None, 1 = Some
        // Uses i1 to match built-in Optional type
        var opt_fields = [_]llvm.TypeRef{
            llvm.Types.int1(self.ctx), // tag (bool)
            elem_llvm_type, // value
        };
        const opt_type = llvm.Types.struct_(self.ctx, &opt_fields, false);

        // Build slice struct type
        var slice_fields = [_]llvm.TypeRef{
            llvm.Types.pointer(self.ctx),
            i64_type,
        };
        const slice_type = llvm.Types.struct_(self.ctx, &slice_fields, false);

        // Allocate space for the result
        const result_ptr = llvm.c.LLVMBuildAlloca(self.builder.ref, opt_type, "first_result");

        // Get array length
        const array_size = self.getArraySize(expr);
        const is_fixed_array = array_size != null;

        // For fixed arrays with size > 0, we know first() will always succeed
        if (is_fixed_array) {
            const size = array_size.?;
            // Build the fixed array type for GEP
            const array_type = llvm.Types.array(elem_llvm_type, @intCast(size));

            if (size > 0) {
                // Set tag to 1 (Some)
                const tag_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_type, result_ptr, 0, "tag_ptr");
                _ = self.builder.buildStore(llvm.Const.int1(self.ctx, true), tag_ptr);

                // Get first element - object is the alloca pointer to the array
                const zero = llvm.Const.int64(self.ctx, 0);
                var indices = [_]llvm.ValueRef{ zero, zero };
                const elem_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, array_type, object, &indices, 2, "first_elem_ptr");
                const elem_val = self.builder.buildLoad(elem_llvm_type, elem_ptr, "first_elem");

                // Store value
                const val_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_type, result_ptr, 1, "val_ptr");
                _ = self.builder.buildStore(elem_val, val_ptr);
            } else {
                // Empty array - return None (tag = 0)
                const tag_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_type, result_ptr, 0, "tag_ptr");
                _ = self.builder.buildStore(llvm.Const.int1(self.ctx, false), tag_ptr);
            }
        } else {
            // For slices, we need runtime check
            const current_fn = self.current_function orelse return EmitError.InvalidAST;
            const some_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, current_fn, "first_some");
            const none_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, current_fn, "first_none");
            const merge_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, current_fn, "first_merge");

            // Check if length > 0
            const len_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, slice_type, object, 1, "len_ptr");
            const len = self.builder.buildLoad(i64_type, len_ptr, "len");
            const zero = llvm.Const.int64(self.ctx, 0);
            const is_nonempty = self.builder.buildICmp(llvm.c.LLVMIntUGT, len, zero, "is_nonempty");
            _ = llvm.c.LLVMBuildCondBr(self.builder.ref, is_nonempty, some_bb, none_bb);

            // Some block - get first element
            llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, some_bb);
            const tag_ptr_some = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_type, result_ptr, 0, "tag_ptr");
            _ = self.builder.buildStore(llvm.Const.int1(self.ctx, true), tag_ptr_some);

            // Get data pointer from slice
            const data_ptr_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, slice_type, object, 0, "data_ptr_ptr");
            const data_ptr = self.builder.buildLoad(llvm.Types.pointer(self.ctx), data_ptr_ptr, "data_ptr");
            const elem_val = self.builder.buildLoad(elem_llvm_type, data_ptr, "first_elem");

            const val_ptr_some = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_type, result_ptr, 1, "val_ptr");
            _ = self.builder.buildStore(elem_val, val_ptr_some);
            _ = llvm.c.LLVMBuildBr(self.builder.ref, merge_bb);

            // None block
            llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, none_bb);
            const tag_ptr_none = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_type, result_ptr, 0, "tag_ptr");
            _ = self.builder.buildStore(llvm.Const.int1(self.ctx, false), tag_ptr_none);
            _ = llvm.c.LLVMBuildBr(self.builder.ref, merge_bb);

            // Merge block
            llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, merge_bb);
        }

        // Load and return the result
        return self.builder.buildLoad(opt_type, result_ptr, "first_opt");
    }

    /// Emit array.last() - returns Optional[T], Some(last element) or None if empty.
    fn emitArrayLast(self: *Emitter, expr: ast.Expr, object: llvm.ValueRef) EmitError!llvm.ValueRef {
        const element_type = self.getArrayElementType(expr) orelse return EmitError.InvalidAST;
        const elem_llvm_type = self.typeToLLVM(element_type);
        const i64_type = llvm.Types.int64(self.ctx);

        // Build optional type: { i1 tag, T value } where tag 0 = None, 1 = Some
        // Uses i1 to match built-in Optional type
        var opt_fields = [_]llvm.TypeRef{
            llvm.Types.int1(self.ctx), // tag (bool)
            elem_llvm_type, // value
        };
        const opt_type = llvm.Types.struct_(self.ctx, &opt_fields, false);

        // Build slice struct type
        var slice_fields = [_]llvm.TypeRef{
            llvm.Types.pointer(self.ctx),
            i64_type,
        };
        const slice_type = llvm.Types.struct_(self.ctx, &slice_fields, false);

        const result_ptr = llvm.c.LLVMBuildAlloca(self.builder.ref, opt_type, "last_result");

        const array_size = self.getArraySize(expr);
        const is_fixed_array = array_size != null;

        if (is_fixed_array) {
            const size = array_size.?;
            const array_type = llvm.Types.array(elem_llvm_type, @intCast(size));

            if (size > 0) {
                const tag_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_type, result_ptr, 0, "tag_ptr");
                _ = self.builder.buildStore(llvm.Const.int1(self.ctx, true), tag_ptr);

                // Get last element (index = size - 1)
                const zero = llvm.Const.int64(self.ctx, 0);
                const last_idx = llvm.Const.int64(self.ctx, @intCast(size - 1));
                var indices = [_]llvm.ValueRef{ zero, last_idx };
                const elem_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, array_type, object, &indices, 2, "last_elem_ptr");
                const elem_val = self.builder.buildLoad(elem_llvm_type, elem_ptr, "last_elem");

                const val_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_type, result_ptr, 1, "val_ptr");
                _ = self.builder.buildStore(elem_val, val_ptr);
            } else {
                const tag_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_type, result_ptr, 0, "tag_ptr");
                _ = self.builder.buildStore(llvm.Const.int1(self.ctx, false), tag_ptr);
            }
        } else {
            // For slices, we need runtime check
            const current_fn = self.current_function orelse return EmitError.InvalidAST;
            const some_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, current_fn, "last_some");
            const none_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, current_fn, "last_none");
            const merge_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, current_fn, "last_merge");

            const len_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, slice_type, object, 1, "len_ptr");
            const len = self.builder.buildLoad(i64_type, len_ptr, "len");
            const zero = llvm.Const.int64(self.ctx, 0);
            const is_nonempty = self.builder.buildICmp(llvm.c.LLVMIntUGT, len, zero, "is_nonempty");
            _ = llvm.c.LLVMBuildCondBr(self.builder.ref, is_nonempty, some_bb, none_bb);

            // Some block
            llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, some_bb);
            const tag_ptr_some = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_type, result_ptr, 0, "tag_ptr");
            _ = self.builder.buildStore(llvm.Const.int1(self.ctx, true), tag_ptr_some);

            // Calculate last index = len - 1
            const one = llvm.Const.int64(self.ctx, 1);
            const last_idx = llvm.c.LLVMBuildSub(self.builder.ref, len, one, "last_idx");

            // Get data pointer and index into it
            const data_ptr_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, slice_type, object, 0, "data_ptr_ptr");
            const data_ptr = self.builder.buildLoad(llvm.Types.pointer(self.ctx), data_ptr_ptr, "data_ptr");
            var gep_idx = [_]llvm.ValueRef{last_idx};
            const elem_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, elem_llvm_type, data_ptr, &gep_idx, 1, "last_elem_ptr");
            const elem_val = self.builder.buildLoad(elem_llvm_type, elem_ptr, "last_elem");

            const val_ptr_some = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_type, result_ptr, 1, "val_ptr");
            _ = self.builder.buildStore(elem_val, val_ptr_some);
            _ = llvm.c.LLVMBuildBr(self.builder.ref, merge_bb);

            // None block
            llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, none_bb);
            const tag_ptr_none = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_type, result_ptr, 0, "tag_ptr");
            _ = self.builder.buildStore(llvm.Const.int1(self.ctx, false), tag_ptr_none);
            _ = llvm.c.LLVMBuildBr(self.builder.ref, merge_bb);

            // Merge block
            llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, merge_bb);
        }

        return self.builder.buildLoad(opt_type, result_ptr, "last_opt");
    }

    /// Emit array.get(index) - returns Optional[T], Some(element) if in bounds, None otherwise.
    fn emitArrayGet(self: *Emitter, expr: ast.Expr, object: llvm.ValueRef, index: llvm.ValueRef) EmitError!llvm.ValueRef {
        const element_type = self.getArrayElementType(expr) orelse return EmitError.InvalidAST;
        const elem_llvm_type = self.typeToLLVM(element_type);
        const i64_type = llvm.Types.int64(self.ctx);

        // Build optional type: { i1 tag, T value } where tag 0 = None, 1 = Some
        // Uses i1 to match built-in Optional type
        var opt_fields = [_]llvm.TypeRef{
            llvm.Types.int1(self.ctx), // tag (bool)
            elem_llvm_type, // value
        };
        const opt_type = llvm.Types.struct_(self.ctx, &opt_fields, false);

        // Build slice struct type
        var slice_fields = [_]llvm.TypeRef{
            llvm.Types.pointer(self.ctx),
            i64_type,
        };
        const slice_type = llvm.Types.struct_(self.ctx, &slice_fields, false);

        const result_ptr = llvm.c.LLVMBuildAlloca(self.builder.ref, opt_type, "get_result");

        const current_fn = self.current_function orelse return EmitError.InvalidAST;
        const some_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, current_fn, "get_some");
        const none_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, current_fn, "get_none");
        const merge_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, current_fn, "get_merge");

        // Convert index to i64 if needed
        const idx_type = llvm.typeOf(index);
        const idx_i64 = if (idx_type == i64_type)
            index
        else
            llvm.c.LLVMBuildSExt(self.builder.ref, index, i64_type, "idx_i64");

        // Get array length
        const array_size = self.getArraySize(expr);
        const len = if (array_size) |size|
            llvm.Const.int64(self.ctx, @intCast(size))
        else blk: {
            const len_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, slice_type, object, 1, "len_ptr");
            break :blk self.builder.buildLoad(i64_type, len_ptr, "len");
        };

        // Check if index < 0 or index >= len
        const zero = llvm.Const.int64(self.ctx, 0);
        const idx_negative = self.builder.buildICmp(llvm.c.LLVMIntSLT, idx_i64, zero, "idx_negative");
        const idx_ge_len = self.builder.buildICmp(llvm.c.LLVMIntSGE, idx_i64, len, "idx_ge_len");
        const out_of_bounds = self.builder.buildOr(idx_negative, idx_ge_len, "out_of_bounds");
        _ = llvm.c.LLVMBuildCondBr(self.builder.ref, out_of_bounds, none_bb, some_bb);

        // Some block - index is valid
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, some_bb);
        const tag_ptr_some = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_type, result_ptr, 0, "tag_ptr");
        _ = self.builder.buildStore(llvm.Const.int1(self.ctx, true), tag_ptr_some);

        // Get element at index
        const elem_val = if (array_size) |size| blk: {
            // Fixed array - use 2D GEP with explicit array type
            const array_type = llvm.Types.array(elem_llvm_type, @intCast(size));
            var indices = [_]llvm.ValueRef{ zero, idx_i64 };
            const elem_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, array_type, object, &indices, 2, "elem_ptr");
            break :blk self.builder.buildLoad(elem_llvm_type, elem_ptr, "elem");
        } else blk: {
            // Slice - get data pointer and index
            const data_ptr_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, slice_type, object, 0, "data_ptr_ptr");
            const data_ptr = self.builder.buildLoad(llvm.Types.pointer(self.ctx), data_ptr_ptr, "data_ptr");
            var gep_idx = [_]llvm.ValueRef{idx_i64};
            const elem_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, elem_llvm_type, data_ptr, &gep_idx, 1, "elem_ptr");
            break :blk self.builder.buildLoad(elem_llvm_type, elem_ptr, "elem");
        };

        const val_ptr_some = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_type, result_ptr, 1, "val_ptr");
        _ = self.builder.buildStore(elem_val, val_ptr_some);
        _ = llvm.c.LLVMBuildBr(self.builder.ref, merge_bb);

        // None block
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, none_bb);
        const tag_ptr_none = llvm.c.LLVMBuildStructGEP2(self.builder.ref, opt_type, result_ptr, 0, "tag_ptr");
        _ = self.builder.buildStore(llvm.Const.int1(self.ctx, false), tag_ptr_none);
        _ = llvm.c.LLVMBuildBr(self.builder.ref, merge_bb);

        // Merge block
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, merge_bb);

        return self.builder.buildLoad(opt_type, result_ptr, "get_opt");
    }

    /// Emit array.contains(value) - returns true if any element equals value.
    fn emitArrayContains(self: *Emitter, expr: ast.Expr, object: llvm.ValueRef, value: llvm.ValueRef) EmitError!llvm.ValueRef {
        const element_type = self.getArrayElementType(expr) orelse return EmitError.InvalidAST;
        const elem_llvm_type = self.typeToLLVM(element_type);
        const i64_type = llvm.Types.int64(self.ctx);

        const current_fn = self.current_function orelse return EmitError.InvalidAST;

        // Build slice struct type
        var slice_fields = [_]llvm.TypeRef{
            llvm.Types.pointer(self.ctx),
            i64_type,
        };
        const slice_type = llvm.Types.struct_(self.ctx, &slice_fields, false);

        // Allocate result variable
        const i1_type = llvm.Types.int1(self.ctx);
        const result_ptr = llvm.c.LLVMBuildAlloca(self.builder.ref, i1_type, "contains_result");
        _ = self.builder.buildStore(llvm.Const.int1(self.ctx, false), result_ptr); // Initialize to false

        // Get array length
        const array_size = self.getArraySize(expr);
        const len = if (array_size) |size|
            llvm.Const.int64(self.ctx, @intCast(size))
        else blk: {
            const len_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, slice_type, object, 1, "len_ptr");
            break :blk self.builder.buildLoad(i64_type, len_ptr, "len");
        };

        // Loop through array
        const loop_init_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, current_fn, "contains_init");
        const loop_cond_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, current_fn, "contains_cond");
        const loop_body_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, current_fn, "contains_body");
        const found_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, current_fn, "contains_found");
        const loop_inc_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, current_fn, "contains_inc");
        const loop_end_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, current_fn, "contains_end");

        _ = llvm.c.LLVMBuildBr(self.builder.ref, loop_init_bb);

        // Loop init - allocate index
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, loop_init_bb);
        const idx_ptr = llvm.c.LLVMBuildAlloca(self.builder.ref, i64_type, "idx_ptr");
        _ = self.builder.buildStore(llvm.Const.int64(self.ctx, 0), idx_ptr);
        _ = llvm.c.LLVMBuildBr(self.builder.ref, loop_cond_bb);

        // Loop condition - idx < len
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, loop_cond_bb);
        const idx = self.builder.buildLoad(i64_type, idx_ptr, "idx");
        const continue_loop = self.builder.buildICmp(llvm.c.LLVMIntULT, idx, len, "continue_loop");
        _ = llvm.c.LLVMBuildCondBr(self.builder.ref, continue_loop, loop_body_bb, loop_end_bb);

        // Loop body - compare element
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, loop_body_bb);
        const elem_val = if (array_size) |size| blk: {
            // Fixed array - use 2D GEP with explicit array type
            const array_type = llvm.Types.array(elem_llvm_type, @intCast(size));
            const zero = llvm.Const.int64(self.ctx, 0);
            var indices = [_]llvm.ValueRef{ zero, idx };
            const elem_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, array_type, object, &indices, 2, "elem_ptr");
            break :blk self.builder.buildLoad(elem_llvm_type, elem_ptr, "elem");
        } else blk: {
            const data_ptr_ptr = llvm.c.LLVMBuildStructGEP2(self.builder.ref, slice_type, object, 0, "data_ptr_ptr");
            const data_ptr = self.builder.buildLoad(llvm.Types.pointer(self.ctx), data_ptr_ptr, "data_ptr");
            var gep_idx = [_]llvm.ValueRef{idx};
            const elem_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, elem_llvm_type, data_ptr, &gep_idx, 1, "elem_ptr");
            break :blk self.builder.buildLoad(elem_llvm_type, elem_ptr, "elem");
        };

        // Compare with value
        const is_eq = if (element_type.isInteger() or element_type == .primitive and element_type.primitive == .bool_)
            self.builder.buildICmp(llvm.c.LLVMIntEQ, elem_val, value, "is_eq")
        else if (element_type.isFloat())
            self.builder.buildFCmp(llvm.c.LLVMRealOEQ, elem_val, value, "is_eq")
        else
            // For other types (strings, structs), use pointer comparison for now
            // TODO: Use proper equality
            self.builder.buildICmp(llvm.c.LLVMIntEQ, elem_val, value, "is_eq");

        _ = llvm.c.LLVMBuildCondBr(self.builder.ref, is_eq, found_bb, loop_inc_bb);

        // Found block - set result to true and exit
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, found_bb);
        _ = self.builder.buildStore(llvm.Const.int1(self.ctx, true), result_ptr);
        _ = llvm.c.LLVMBuildBr(self.builder.ref, loop_end_bb);

        // Loop increment
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, loop_inc_bb);
        const next_idx = llvm.c.LLVMBuildAdd(self.builder.ref, idx, llvm.Const.int64(self.ctx, 1), "next_idx");
        _ = self.builder.buildStore(next_idx, idx_ptr);
        _ = llvm.c.LLVMBuildBr(self.builder.ref, loop_cond_bb);

        // Loop end
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, loop_end_bb);

        return self.builder.buildLoad(i1_type, result_ptr, "contains");
    }

    /// Declare or get the klar_string_chars runtime function.
    /// This function treats each byte as a character (ASCII-only for now).
    /// TODO: Implement proper UTF-8 decoding.
    fn getOrDeclareStringChars(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_string_chars";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // Return type is a struct { ptr, i64 }
        const ptr_type = llvm.Types.pointer(self.ctx);
        var slice_fields = [_]llvm.TypeRef{
            ptr_type, // data pointer
            llvm.Types.int64(self.ctx), // length
        };
        const slice_type = llvm.Types.struct_(self.ctx, &slice_fields, false);

        var param_types = [_]llvm.TypeRef{ptr_type};
        const fn_type = llvm.c.LLVMFunctionType(slice_type, &param_types, 1, 0);
        const func = llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);

        // Build the function body inline
        const entry_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "entry");
        const loop_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "loop");
        const loop_body_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "loop_body");
        const loop_end_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "loop_end");
        const ret_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "return");

        const saved_bb = llvm.c.LLVMGetInsertBlock(self.builder.ref);
        const saved_func = self.current_function;

        const i8_type = llvm.Types.int8(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        const zero_i64 = llvm.Const.int64(self.ctx, 0);
        const one_i64 = llvm.Const.int64(self.ctx, 1);
        const four_i64 = llvm.Const.int64(self.ctx, 4); // sizeof(i32)

        // Entry block
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, entry_bb);
        const s = llvm.c.LLVMGetParam(func, 0);

        // Get string length using strlen
        const strlen_fn = self.getOrDeclareStrlen();
        var strlen_args = [_]llvm.ValueRef{s};
        const len = llvm.c.LLVMBuildCall2(
            self.builder.ref,
            llvm.c.LLVMGlobalGetValueType(strlen_fn),
            strlen_fn,
            &strlen_args,
            1,
            "len",
        );

        // Allocate array of i32 (len * 4 bytes)
        const alloc_size = llvm.c.LLVMBuildMul(self.builder.ref, len, four_i64, "alloc_size");
        const malloc_fn = self.getOrDeclareMalloc();
        var malloc_args = [_]llvm.ValueRef{alloc_size};
        const result = llvm.c.LLVMBuildCall2(
            self.builder.ref,
            llvm.c.LLVMGlobalGetValueType(malloc_fn),
            malloc_fn,
            &malloc_args,
            1,
            "result",
        );

        // Allocate index variable
        const idx_ptr = llvm.c.LLVMBuildAlloca(self.builder.ref, i64_type, "idx_ptr");
        _ = llvm.c.LLVMBuildStore(self.builder.ref, zero_i64, idx_ptr);

        _ = llvm.c.LLVMBuildBr(self.builder.ref, loop_bb);

        // Loop condition
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, loop_bb);
        const idx = llvm.c.LLVMBuildLoad2(self.builder.ref, i64_type, idx_ptr, "idx");
        const cond = llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntULT, idx, len, "cond");
        _ = llvm.c.LLVMBuildCondBr(self.builder.ref, cond, loop_body_bb, loop_end_bb);

        // Loop body: copy byte as i32
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, loop_body_bb);

        // Load source byte
        var src_indices = [_]llvm.ValueRef{idx};
        const src_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, i8_type, s, &src_indices, 1, "src_ptr");
        const byte = llvm.c.LLVMBuildLoad2(self.builder.ref, i8_type, src_ptr, "byte");

        // Zero-extend byte to i32
        const char_val = llvm.c.LLVMBuildZExt(self.builder.ref, byte, i32_type, "char_val");

        // Store to result array
        var dst_indices = [_]llvm.ValueRef{idx};
        const dst_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, i32_type, result, &dst_indices, 1, "dst_ptr");
        _ = llvm.c.LLVMBuildStore(self.builder.ref, char_val, dst_ptr);

        // Increment index
        const next_idx = llvm.c.LLVMBuildAdd(self.builder.ref, idx, one_i64, "next_idx");
        _ = llvm.c.LLVMBuildStore(self.builder.ref, next_idx, idx_ptr);
        _ = llvm.c.LLVMBuildBr(self.builder.ref, loop_bb);

        // Loop end: build result struct
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, loop_end_bb);
        _ = llvm.c.LLVMBuildBr(self.builder.ref, ret_bb);

        // Return the slice struct
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, ret_bb);

        // Build slice struct { ptr, len } using alloca + store + load
        const slice_ptr = llvm.c.LLVMBuildAlloca(self.builder.ref, slice_type, "slice_ptr");

        // Store pointer field (index 0)
        const ptr_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, slice_type, slice_ptr, 0, "ptr_field");
        _ = llvm.c.LLVMBuildStore(self.builder.ref, result, ptr_field);

        // Store length field (index 1)
        const len_field = llvm.c.LLVMBuildStructGEP2(self.builder.ref, slice_type, slice_ptr, 1, "len_field");
        _ = llvm.c.LLVMBuildStore(self.builder.ref, len, len_field);

        // Load the complete slice struct and return
        const slice_val = llvm.c.LLVMBuildLoad2(self.builder.ref, slice_type, slice_ptr, "slice_val");
        _ = llvm.c.LLVMBuildRet(self.builder.ref, slice_val);

        // Restore builder position
        if (saved_bb) |bb| {
            llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, bb);
        }
        self.current_function = saved_func;

        return func;
    }

    /// Declare or get the klar_string_trim runtime function.
    /// This function trims leading and trailing whitespace from a string.
    fn getOrDeclareStringTrim(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_string_trim";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // Declare: char* klar_string_trim(const char* s)
        const ptr_type = llvm.Types.pointer(self.ctx);
        var param_types = [_]llvm.TypeRef{ptr_type};
        const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 1, 0);
        const func = llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);

        // Build the function body inline
        const entry_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "entry");
        const loop_start_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "loop_start");
        const loop_end_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "loop_end");
        const copy_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "copy");
        const ret_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "return");

        const saved_bb = llvm.c.LLVMGetInsertBlock(self.builder.ref);
        const saved_func = self.current_function;

        const i8_type = llvm.Types.int8(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        const zero_i64 = llvm.Const.int64(self.ctx, 0);
        const one_i64 = llvm.Const.int64(self.ctx, 1);

        // Entry block
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, entry_bb);
        const s = llvm.c.LLVMGetParam(func, 0);

        // Get string length
        const strlen_fn = self.getOrDeclareStrlen();
        var strlen_args = [_]llvm.ValueRef{s};
        const len = llvm.c.LLVMBuildCall2(
            self.builder.ref,
            llvm.c.LLVMGlobalGetValueType(strlen_fn),
            strlen_fn,
            &strlen_args,
            1,
            "len",
        );

        // Allocate variables for start and end indices
        const start_ptr = llvm.c.LLVMBuildAlloca(self.builder.ref, i64_type, "start_ptr");
        const end_ptr = llvm.c.LLVMBuildAlloca(self.builder.ref, i64_type, "end_ptr");
        _ = llvm.c.LLVMBuildStore(self.builder.ref, zero_i64, start_ptr);
        _ = llvm.c.LLVMBuildStore(self.builder.ref, len, end_ptr);

        // Loop to find start (skip leading whitespace)
        _ = llvm.c.LLVMBuildBr(self.builder.ref, loop_start_bb);

        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, loop_start_bb);
        const start_val = llvm.c.LLVMBuildLoad2(self.builder.ref, i64_type, start_ptr, "start");
        const start_lt_len = llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntULT, start_val, len, "start_lt_len");

        // Get character at start position
        var gep_indices = [_]llvm.ValueRef{start_val};
        const char_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, i8_type, s, &gep_indices, 1, "char_ptr");
        const ch = llvm.c.LLVMBuildLoad2(self.builder.ref, i8_type, char_ptr, "ch");

        // Check if whitespace (space, tab, newline, carriage return)
        const is_space = llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntEQ, ch, llvm.Const.int8(self.ctx, ' '), "is_space");
        const is_tab = llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntEQ, ch, llvm.Const.int8(self.ctx, '\t'), "is_tab");
        const is_nl = llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntEQ, ch, llvm.Const.int8(self.ctx, '\n'), "is_nl");
        const is_cr = llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntEQ, ch, llvm.Const.int8(self.ctx, '\r'), "is_cr");
        const is_ws1 = llvm.c.LLVMBuildOr(self.builder.ref, is_space, is_tab, "is_ws1");
        const is_ws2 = llvm.c.LLVMBuildOr(self.builder.ref, is_nl, is_cr, "is_ws2");
        const is_ws = llvm.c.LLVMBuildOr(self.builder.ref, is_ws1, is_ws2, "is_ws");

        const continue_start = llvm.c.LLVMBuildAnd(self.builder.ref, start_lt_len, is_ws, "continue_start");

        // If still whitespace, increment start and loop
        const inc_start_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "inc_start");
        _ = llvm.c.LLVMBuildCondBr(self.builder.ref, continue_start, inc_start_bb, loop_end_bb);

        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, inc_start_bb);
        const new_start = llvm.c.LLVMBuildAdd(self.builder.ref, start_val, one_i64, "new_start");
        _ = llvm.c.LLVMBuildStore(self.builder.ref, new_start, start_ptr);
        _ = llvm.c.LLVMBuildBr(self.builder.ref, loop_start_bb);

        // Loop to find end (skip trailing whitespace)
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, loop_end_bb);
        const end_val = llvm.c.LLVMBuildLoad2(self.builder.ref, i64_type, end_ptr, "end");
        const start_val2 = llvm.c.LLVMBuildLoad2(self.builder.ref, i64_type, start_ptr, "start2");
        const end_gt_start = llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntUGT, end_val, start_val2, "end_gt_start");

        // Get character at end-1 position
        const end_minus1 = llvm.c.LLVMBuildSub(self.builder.ref, end_val, one_i64, "end_minus1");
        var gep_indices2 = [_]llvm.ValueRef{end_minus1};
        const char_ptr2 = llvm.c.LLVMBuildGEP2(self.builder.ref, i8_type, s, &gep_indices2, 1, "char_ptr2");
        const ch2 = llvm.c.LLVMBuildLoad2(self.builder.ref, i8_type, char_ptr2, "ch2");

        // Check if whitespace
        const is_space2 = llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntEQ, ch2, llvm.Const.int8(self.ctx, ' '), "is_space2");
        const is_tab2 = llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntEQ, ch2, llvm.Const.int8(self.ctx, '\t'), "is_tab2");
        const is_nl2 = llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntEQ, ch2, llvm.Const.int8(self.ctx, '\n'), "is_nl2");
        const is_cr2 = llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntEQ, ch2, llvm.Const.int8(self.ctx, '\r'), "is_cr2");
        const is_ws12 = llvm.c.LLVMBuildOr(self.builder.ref, is_space2, is_tab2, "is_ws12");
        const is_ws22 = llvm.c.LLVMBuildOr(self.builder.ref, is_nl2, is_cr2, "is_ws22");
        const is_ws_end = llvm.c.LLVMBuildOr(self.builder.ref, is_ws12, is_ws22, "is_ws_end");

        const continue_end = llvm.c.LLVMBuildAnd(self.builder.ref, end_gt_start, is_ws_end, "continue_end");

        const dec_end_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "dec_end");
        _ = llvm.c.LLVMBuildCondBr(self.builder.ref, continue_end, dec_end_bb, copy_bb);

        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, dec_end_bb);
        const new_end = llvm.c.LLVMBuildSub(self.builder.ref, end_val, one_i64, "new_end");
        _ = llvm.c.LLVMBuildStore(self.builder.ref, new_end, end_ptr);
        _ = llvm.c.LLVMBuildBr(self.builder.ref, loop_end_bb);

        // Copy the trimmed substring
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, copy_bb);
        const final_start = llvm.c.LLVMBuildLoad2(self.builder.ref, i64_type, start_ptr, "final_start");
        const final_end = llvm.c.LLVMBuildLoad2(self.builder.ref, i64_type, end_ptr, "final_end");
        const trimmed_len = llvm.c.LLVMBuildSub(self.builder.ref, final_end, final_start, "trimmed_len");
        const alloc_size = llvm.c.LLVMBuildAdd(self.builder.ref, trimmed_len, one_i64, "alloc_size");

        // Allocate new string
        const malloc_fn = self.getOrDeclareMalloc();
        var malloc_args = [_]llvm.ValueRef{alloc_size};
        const result = llvm.c.LLVMBuildCall2(
            self.builder.ref,
            llvm.c.LLVMGlobalGetValueType(malloc_fn),
            malloc_fn,
            &malloc_args,
            1,
            "result",
        );

        // Copy using memcpy
        const memcpy_fn = self.getOrDeclareMemcpy();
        var gep_indices3 = [_]llvm.ValueRef{final_start};
        const src_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, i8_type, s, &gep_indices3, 1, "src_ptr");
        var memcpy_args = [_]llvm.ValueRef{ result, src_ptr, trimmed_len };
        _ = llvm.c.LLVMBuildCall2(
            self.builder.ref,
            llvm.c.LLVMGlobalGetValueType(memcpy_fn),
            memcpy_fn,
            &memcpy_args,
            3,
            "",
        );

        // Add null terminator
        var gep_indices4 = [_]llvm.ValueRef{trimmed_len};
        const null_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, i8_type, result, &gep_indices4, 1, "null_ptr");
        _ = llvm.c.LLVMBuildStore(self.builder.ref, llvm.Const.int1(self.ctx, false), null_ptr);

        _ = llvm.c.LLVMBuildBr(self.builder.ref, ret_bb);

        // Return
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, ret_bb);
        _ = llvm.c.LLVMBuildRet(self.builder.ref, result);

        // Restore builder position
        if (saved_bb) |bb| {
            llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, bb);
        }
        self.current_function = saved_func;

        return func;
    }

    /// Declare or get the klar_string_to_uppercase runtime function.
    fn getOrDeclareStringToUppercase(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_string_to_uppercase";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // Declare: char* klar_string_to_uppercase(const char* s)
        const ptr_type = llvm.Types.pointer(self.ctx);
        var param_types = [_]llvm.TypeRef{ptr_type};
        const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 1, 0);
        const func = llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);

        // Build the function body
        const entry_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "entry");
        const loop_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "loop");
        const body_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "body");
        const ret_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "return");

        const saved_bb = llvm.c.LLVMGetInsertBlock(self.builder.ref);
        const saved_func = self.current_function;

        const i8_type = llvm.Types.int8(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        const zero_i64 = llvm.Const.int64(self.ctx, 0);
        const one_i64 = llvm.Const.int64(self.ctx, 1);

        // Entry block
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, entry_bb);
        const s = llvm.c.LLVMGetParam(func, 0);

        // Get string length
        const strlen_fn = self.getOrDeclareStrlen();
        var strlen_args = [_]llvm.ValueRef{s};
        const len = llvm.c.LLVMBuildCall2(
            self.builder.ref,
            llvm.c.LLVMGlobalGetValueType(strlen_fn),
            strlen_fn,
            &strlen_args,
            1,
            "len",
        );

        // Allocate new string
        const alloc_size = llvm.c.LLVMBuildAdd(self.builder.ref, len, one_i64, "alloc_size");
        const malloc_fn = self.getOrDeclareMalloc();
        var malloc_args = [_]llvm.ValueRef{alloc_size};
        const result = llvm.c.LLVMBuildCall2(
            self.builder.ref,
            llvm.c.LLVMGlobalGetValueType(malloc_fn),
            malloc_fn,
            &malloc_args,
            1,
            "result",
        );

        // Initialize loop counter
        const i_ptr = llvm.c.LLVMBuildAlloca(self.builder.ref, i64_type, "i_ptr");
        _ = llvm.c.LLVMBuildStore(self.builder.ref, zero_i64, i_ptr);
        _ = llvm.c.LLVMBuildBr(self.builder.ref, loop_bb);

        // Loop condition
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, loop_bb);
        const i = llvm.c.LLVMBuildLoad2(self.builder.ref, i64_type, i_ptr, "i");
        const cond = llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntULT, i, len, "cond");
        _ = llvm.c.LLVMBuildCondBr(self.builder.ref, cond, body_bb, ret_bb);

        // Loop body - convert to uppercase
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, body_bb);
        var gep_src = [_]llvm.ValueRef{i};
        const src_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, i8_type, s, &gep_src, 1, "src_ptr");
        const ch = llvm.c.LLVMBuildLoad2(self.builder.ref, i8_type, src_ptr, "ch");

        // Check if lowercase letter (a-z) and convert to uppercase
        const is_lower = llvm.c.LLVMBuildAnd(
            self.builder.ref,
            llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntUGE, ch, llvm.Const.int8(self.ctx, 'a'), "ge_a"),
            llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntULE, ch, llvm.Const.int8(self.ctx, 'z'), "le_z"),
            "is_lower",
        );

        // Subtract 32 to convert lowercase to uppercase
        const upper_ch = llvm.c.LLVMBuildSub(self.builder.ref, ch, llvm.Const.int8(self.ctx, 32), "upper_ch");
        const result_ch = llvm.c.LLVMBuildSelect(self.builder.ref, is_lower, upper_ch, ch, "result_ch");

        // Store result
        var gep_dst = [_]llvm.ValueRef{i};
        const dst_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, i8_type, result, &gep_dst, 1, "dst_ptr");
        _ = llvm.c.LLVMBuildStore(self.builder.ref, result_ch, dst_ptr);

        // Increment counter
        const next_i = llvm.c.LLVMBuildAdd(self.builder.ref, i, one_i64, "next_i");
        _ = llvm.c.LLVMBuildStore(self.builder.ref, next_i, i_ptr);
        _ = llvm.c.LLVMBuildBr(self.builder.ref, loop_bb);

        // Return block - add null terminator and return
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, ret_bb);
        var gep_null = [_]llvm.ValueRef{len};
        const null_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, i8_type, result, &gep_null, 1, "null_ptr");
        _ = llvm.c.LLVMBuildStore(self.builder.ref, llvm.Const.int1(self.ctx, false), null_ptr);
        _ = llvm.c.LLVMBuildRet(self.builder.ref, result);

        // Restore builder position
        if (saved_bb) |bb| {
            llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, bb);
        }
        self.current_function = saved_func;

        return func;
    }

    /// Declare or get the klar_string_to_lowercase runtime function.
    fn getOrDeclareStringToLowercase(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_string_to_lowercase";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // Declare: char* klar_string_to_lowercase(const char* s)
        const ptr_type = llvm.Types.pointer(self.ctx);
        var param_types = [_]llvm.TypeRef{ptr_type};
        const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 1, 0);
        const func = llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);

        // Build the function body
        const entry_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "entry");
        const loop_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "loop");
        const body_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "body");
        const ret_bb = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "return");

        const saved_bb = llvm.c.LLVMGetInsertBlock(self.builder.ref);
        const saved_func = self.current_function;

        const i8_type = llvm.Types.int8(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        const zero_i64 = llvm.Const.int64(self.ctx, 0);
        const one_i64 = llvm.Const.int64(self.ctx, 1);

        // Entry block
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, entry_bb);
        const s = llvm.c.LLVMGetParam(func, 0);

        // Get string length
        const strlen_fn = self.getOrDeclareStrlen();
        var strlen_args = [_]llvm.ValueRef{s};
        const len = llvm.c.LLVMBuildCall2(
            self.builder.ref,
            llvm.c.LLVMGlobalGetValueType(strlen_fn),
            strlen_fn,
            &strlen_args,
            1,
            "len",
        );

        // Allocate new string
        const alloc_size = llvm.c.LLVMBuildAdd(self.builder.ref, len, one_i64, "alloc_size");
        const malloc_fn = self.getOrDeclareMalloc();
        var malloc_args = [_]llvm.ValueRef{alloc_size};
        const result = llvm.c.LLVMBuildCall2(
            self.builder.ref,
            llvm.c.LLVMGlobalGetValueType(malloc_fn),
            malloc_fn,
            &malloc_args,
            1,
            "result",
        );

        // Initialize loop counter
        const i_ptr = llvm.c.LLVMBuildAlloca(self.builder.ref, i64_type, "i_ptr");
        _ = llvm.c.LLVMBuildStore(self.builder.ref, zero_i64, i_ptr);
        _ = llvm.c.LLVMBuildBr(self.builder.ref, loop_bb);

        // Loop condition
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, loop_bb);
        const i = llvm.c.LLVMBuildLoad2(self.builder.ref, i64_type, i_ptr, "i");
        const cond = llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntULT, i, len, "cond");
        _ = llvm.c.LLVMBuildCondBr(self.builder.ref, cond, body_bb, ret_bb);

        // Loop body - convert to lowercase
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, body_bb);
        var gep_src = [_]llvm.ValueRef{i};
        const src_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, i8_type, s, &gep_src, 1, "src_ptr");
        const ch = llvm.c.LLVMBuildLoad2(self.builder.ref, i8_type, src_ptr, "ch");

        // Check if uppercase letter (A-Z) and convert to lowercase
        const is_upper = llvm.c.LLVMBuildAnd(
            self.builder.ref,
            llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntUGE, ch, llvm.Const.int8(self.ctx, 'A'), "ge_A"),
            llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntULE, ch, llvm.Const.int8(self.ctx, 'Z'), "le_Z"),
            "is_upper",
        );

        // Add 32 to convert uppercase to lowercase
        const lower_ch = llvm.c.LLVMBuildAdd(self.builder.ref, ch, llvm.Const.int8(self.ctx, 32), "lower_ch");
        const result_ch = llvm.c.LLVMBuildSelect(self.builder.ref, is_upper, lower_ch, ch, "result_ch");

        // Store result
        var gep_dst = [_]llvm.ValueRef{i};
        const dst_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, i8_type, result, &gep_dst, 1, "dst_ptr");
        _ = llvm.c.LLVMBuildStore(self.builder.ref, result_ch, dst_ptr);

        // Increment counter
        const next_i = llvm.c.LLVMBuildAdd(self.builder.ref, i, one_i64, "next_i");
        _ = llvm.c.LLVMBuildStore(self.builder.ref, next_i, i_ptr);
        _ = llvm.c.LLVMBuildBr(self.builder.ref, loop_bb);

        // Return block - add null terminator and return
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, ret_bb);
        var gep_null = [_]llvm.ValueRef{len};
        const null_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, i8_type, result, &gep_null, 1, "null_ptr");
        _ = llvm.c.LLVMBuildStore(self.builder.ref, llvm.Const.int1(self.ctx, false), null_ptr);
        _ = llvm.c.LLVMBuildRet(self.builder.ref, result);

        // Restore builder position
        if (saved_bb) |bb| {
            llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, bb);
        }
        self.current_function = saved_func;

        return func;
    }

    /// Declare or get the memcpy function.
    fn getOrDeclareMemcpy(self: *Emitter) llvm.ValueRef {
        const fn_name = "memcpy";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        const ptr_type = llvm.Types.pointer(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        var param_types = [_]llvm.TypeRef{ ptr_type, ptr_type, i64_type };
        const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 3, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    /// Declare or get the memcmp function.
    fn getOrDeclareMemcmp(self: *Emitter) llvm.ValueRef {
        const fn_name = "memcmp";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // int memcmp(const void* s1, const void* s2, size_t n)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i64_type = llvm.Types.int64(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);
        var param_types = [_]llvm.TypeRef{ ptr_type, ptr_type, i64_type };
        const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 3, 0);
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    /// Emit Ok(value) - Result::Ok constructor.
    /// For now, we infer error type as i32 (simple error codes).
    /// Type annotation will provide actual types in full implementation.
    fn emitOkCall(self: *Emitter, args: []const ast.Expr) EmitError!llvm.ValueRef {
        if (args.len != 1) {
            return EmitError.InvalidAST;
        }

        // Emit the value
        const value = try self.emitExpr(args[0]);
        const ok_type = llvm.typeOf(value);

        // Default error type to i32 (can be refined with type inference)
        const err_type = llvm.Types.int32(self.ctx);

        return self.emitOk(value, ok_type, err_type);
    }

    /// Emit Err(error) - Result::Err constructor.
    /// For now, we infer ok type as i32 (placeholder).
    /// Type annotation will provide actual types in full implementation.
    fn emitErrCall(self: *Emitter, args: []const ast.Expr) EmitError!llvm.ValueRef {
        if (args.len != 1) {
            return EmitError.InvalidAST;
        }

        // Emit the error value
        const error_val = try self.emitExpr(args[0]);
        const err_type = llvm.typeOf(error_val);

        // Default ok type to i32 (can be refined with type inference)
        const ok_type = llvm.Types.int32(self.ctx);

        return self.emitErr(error_val, ok_type, err_type);
    }

    /// Emit Result.ok(value) via method call syntax.
    fn emitResultOk(self: *Emitter, method: *ast.MethodCall) EmitError!llvm.ValueRef {
        if (method.args.len != 1) {
            return EmitError.InvalidAST;
        }

        const value = try self.emitExpr(method.args[0]);
        const ok_type = llvm.typeOf(value);
        const err_type = llvm.Types.int32(self.ctx);

        return self.emitOk(value, ok_type, err_type);
    }

    /// Emit Result.err(error) via method call syntax.
    fn emitResultErr(self: *Emitter, method: *ast.MethodCall) EmitError!llvm.ValueRef {
        if (method.args.len != 1) {
            return EmitError.InvalidAST;
        }

        const error_val = try self.emitExpr(method.args[0]);
        const err_type = llvm.typeOf(error_val);
        const ok_type = llvm.Types.int32(self.ctx);

        return self.emitErr(error_val, ok_type, err_type);
    }

    /// Emit Result.is_ok() - returns true if tag == 1 (Ok).
    fn emitResultIsOk(self: *Emitter, result_val: llvm.ValueRef, result_type: llvm.TypeRef) EmitError!llvm.ValueRef {
        // Store result to temp for GEP access
        const result_alloca = self.builder.buildAlloca(result_type, "result.is_ok.tmp");
        _ = self.builder.buildStore(result_val, result_alloca);

        // Get the tag (index 0)
        var tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const tag_ptr = self.builder.buildGEP(result_type, result_alloca, &tag_indices, "result.tag.ptr");
        const tag = self.builder.buildLoad(llvm.Types.int1(self.ctx), tag_ptr, "result.tag");

        // Tag == 1 means Ok
        return tag;
    }

    /// Emit Result.is_err() - returns true if tag == 0 (Err).
    fn emitResultIsErr(self: *Emitter, result_val: llvm.ValueRef, result_type: llvm.TypeRef) EmitError!llvm.ValueRef {
        // Store result to temp for GEP access
        const result_alloca = self.builder.buildAlloca(result_type, "result.is_err.tmp");
        _ = self.builder.buildStore(result_val, result_alloca);

        // Get the tag (index 0)
        var tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const tag_ptr = self.builder.buildGEP(result_type, result_alloca, &tag_indices, "result.tag.ptr");
        const tag = self.builder.buildLoad(llvm.Types.int1(self.ctx), tag_ptr, "result.tag");

        // Tag == 0 means Err, so NOT tag
        return self.builder.buildNot(tag, "result.is_err");
    }

    /// Emit Optional.is_some() - returns true if tag == 1 (Some).
    fn emitOptionalIsSome(self: *Emitter, opt_val: llvm.ValueRef, opt_type: llvm.TypeRef) EmitError!llvm.ValueRef {
        // Store optional to temp for GEP access
        const opt_alloca = self.builder.buildAlloca(opt_type, "opt.is_some.tmp");
        _ = self.builder.buildStore(opt_val, opt_alloca);

        // Get the tag (index 0)
        var tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const tag_ptr = self.builder.buildGEP(opt_type, opt_alloca, &tag_indices, "opt.tag.ptr");
        const tag = self.builder.buildLoad(llvm.Types.int1(self.ctx), tag_ptr, "opt.tag");

        // Tag == 1 means Some
        return tag;
    }

    /// Emit Optional.is_none() - returns true if tag == 0 (None).
    fn emitOptionalIsNone(self: *Emitter, opt_val: llvm.ValueRef, opt_type: llvm.TypeRef) EmitError!llvm.ValueRef {
        // Store optional to temp for GEP access
        const opt_alloca = self.builder.buildAlloca(opt_type, "opt.is_none.tmp");
        _ = self.builder.buildStore(opt_val, opt_alloca);

        // Get the tag (index 0)
        var tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const tag_ptr = self.builder.buildGEP(opt_type, opt_alloca, &tag_indices, "opt.tag.ptr");
        const tag = self.builder.buildLoad(llvm.Types.int1(self.ctx), tag_ptr, "opt.tag");

        // Tag == 0 means None, so NOT tag
        return self.builder.buildNot(tag, "opt.is_none");
    }

    /// Emit Optional.eq(other) - compares two optionals for equality.
    /// Both None => true, both Some with equal values => true, otherwise => false.
    fn emitOptionalEq(self: *Emitter, left: llvm.ValueRef, right: llvm.ValueRef, opt_type: llvm.TypeRef) EmitError!llvm.ValueRef {
        const func = self.current_function orelse return EmitError.InvalidAST;

        // Store both optionals for GEP access
        const left_alloca = self.builder.buildAlloca(opt_type, "opt.eq.left");
        _ = self.builder.buildStore(left, left_alloca);
        const right_alloca = self.builder.buildAlloca(opt_type, "opt.eq.right");
        _ = self.builder.buildStore(right, right_alloca);

        // Get left tag
        var tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const left_tag_ptr = self.builder.buildGEP(opt_type, left_alloca, &tag_indices, "opt.eq.left.tag.ptr");
        const left_tag = self.builder.buildLoad(llvm.Types.int1(self.ctx), left_tag_ptr, "opt.eq.left.tag");

        // Get right tag
        const right_tag_ptr = self.builder.buildGEP(opt_type, right_alloca, &tag_indices, "opt.eq.right.tag.ptr");
        const right_tag = self.builder.buildLoad(llvm.Types.int1(self.ctx), right_tag_ptr, "opt.eq.right.tag");

        // Check if tags are equal
        const tags_equal = self.builder.buildICmp(llvm.c.LLVMIntEQ, left_tag, right_tag, "opt.eq.tags");

        // Basic blocks
        const tags_match_block = llvm.appendBasicBlock(self.ctx, func, "opt.eq.tags_match");
        const tags_differ_block = llvm.appendBasicBlock(self.ctx, func, "opt.eq.tags_differ");
        const both_some_block = llvm.appendBasicBlock(self.ctx, func, "opt.eq.both_some");
        const both_none_block = llvm.appendBasicBlock(self.ctx, func, "opt.eq.both_none");
        const merge_block = llvm.appendBasicBlock(self.ctx, func, "opt.eq.merge");

        // Branch on tags equality
        _ = self.builder.buildCondBr(tags_equal, tags_match_block, tags_differ_block);

        // Tags differ => false
        self.builder.positionAtEnd(tags_differ_block);
        _ = self.builder.buildBr(merge_block);

        // Tags match => check if both Some or both None
        self.builder.positionAtEnd(tags_match_block);
        _ = self.builder.buildCondBr(left_tag, both_some_block, both_none_block);

        // Both None => true
        self.builder.positionAtEnd(both_none_block);
        _ = self.builder.buildBr(merge_block);

        // Both Some => compare values
        self.builder.positionAtEnd(both_some_block);
        var val_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 1),
        };
        const left_val_ptr = self.builder.buildGEP(opt_type, left_alloca, &val_indices, "opt.eq.left.val.ptr");
        const right_val_ptr = self.builder.buildGEP(opt_type, right_alloca, &val_indices, "opt.eq.right.val.ptr");
        const inner_type = llvm.c.LLVMStructGetTypeAtIndex(opt_type, 1);
        const left_val = self.builder.buildLoad(inner_type, left_val_ptr, "opt.eq.left.val");
        const right_val = self.builder.buildLoad(inner_type, right_val_ptr, "opt.eq.right.val");

        // Compare inner values based on type
        const inner_kind = llvm.c.LLVMGetTypeKind(inner_type);
        const values_equal = switch (inner_kind) {
            llvm.c.LLVMIntegerTypeKind => self.builder.buildICmp(llvm.c.LLVMIntEQ, left_val, right_val, "opt.eq.val"),
            llvm.c.LLVMFloatTypeKind, llvm.c.LLVMDoubleTypeKind => self.builder.buildFCmp(llvm.c.LLVMRealOEQ, left_val, right_val, "opt.eq.val"),
            llvm.c.LLVMPointerTypeKind => blk: {
                // Likely string - use strcmp
                const strcmp_fn = self.getOrDeclareStrcmp();
                var args = [_]llvm.ValueRef{ left_val, right_val };
                const result = self.builder.buildCall(
                    llvm.c.LLVMGlobalGetValueType(strcmp_fn),
                    strcmp_fn,
                    &args,
                    "opt.eq.strcmp",
                );
                break :blk self.builder.buildICmp(llvm.c.LLVMIntEQ, result, llvm.Const.int32(self.ctx, 0), "opt.eq.str");
            },
            else => llvm.Const.int1(self.ctx, false),
        };
        _ = self.builder.buildBr(merge_block);
        const both_some_end = llvm.c.LLVMGetInsertBlock(self.builder.ref);

        // Merge block with phi
        self.builder.positionAtEnd(merge_block);
        const phi = self.builder.buildPhi(llvm.Types.int1(self.ctx), "opt.eq.result");
        var incoming_vals = [_]llvm.ValueRef{
            llvm.Const.int1(self.ctx, false), // tags differ
            llvm.Const.int1(self.ctx, true), // both none
            values_equal, // both some
        };
        var incoming_blocks = [_]llvm.c.LLVMBasicBlockRef{
            tags_differ_block,
            both_none_block,
            both_some_end,
        };
        llvm.c.LLVMAddIncoming(phi, &incoming_vals, &incoming_blocks, 3);

        return phi;
    }

    /// Emit Result.eq(other) - compares two results for equality.
    /// Both Ok with equal values => true, both Err with equal values => true, otherwise => false.
    fn emitResultEq(self: *Emitter, left: llvm.ValueRef, right: llvm.ValueRef, result_type: llvm.TypeRef) EmitError!llvm.ValueRef {
        const func = self.current_function orelse return EmitError.InvalidAST;

        // Store both results for GEP access
        const left_alloca = self.builder.buildAlloca(result_type, "res.eq.left");
        _ = self.builder.buildStore(left, left_alloca);
        const right_alloca = self.builder.buildAlloca(result_type, "res.eq.right");
        _ = self.builder.buildStore(right, right_alloca);

        // Get left tag
        var tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const left_tag_ptr = self.builder.buildGEP(result_type, left_alloca, &tag_indices, "res.eq.left.tag.ptr");
        const left_tag = self.builder.buildLoad(llvm.Types.int1(self.ctx), left_tag_ptr, "res.eq.left.tag");

        // Get right tag
        const right_tag_ptr = self.builder.buildGEP(result_type, right_alloca, &tag_indices, "res.eq.right.tag.ptr");
        const right_tag = self.builder.buildLoad(llvm.Types.int1(self.ctx), right_tag_ptr, "res.eq.right.tag");

        // Check if tags are equal
        const tags_equal = self.builder.buildICmp(llvm.c.LLVMIntEQ, left_tag, right_tag, "res.eq.tags");

        // Basic blocks
        const tags_match_block = llvm.appendBasicBlock(self.ctx, func, "res.eq.tags_match");
        const tags_differ_block = llvm.appendBasicBlock(self.ctx, func, "res.eq.tags_differ");
        const both_ok_block = llvm.appendBasicBlock(self.ctx, func, "res.eq.both_ok");
        const both_err_block = llvm.appendBasicBlock(self.ctx, func, "res.eq.both_err");
        const merge_block = llvm.appendBasicBlock(self.ctx, func, "res.eq.merge");

        // Branch on tags equality
        _ = self.builder.buildCondBr(tags_equal, tags_match_block, tags_differ_block);

        // Tags differ => false
        self.builder.positionAtEnd(tags_differ_block);
        _ = self.builder.buildBr(merge_block);

        // Tags match => check if both Ok or both Err
        self.builder.positionAtEnd(tags_match_block);
        _ = self.builder.buildCondBr(left_tag, both_ok_block, both_err_block);

        // Both Ok => compare ok values
        self.builder.positionAtEnd(both_ok_block);
        var ok_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 1),
        };
        const left_ok_ptr = self.builder.buildGEP(result_type, left_alloca, &ok_indices, "res.eq.left.ok.ptr");
        const right_ok_ptr = self.builder.buildGEP(result_type, right_alloca, &ok_indices, "res.eq.right.ok.ptr");
        const ok_type = llvm.c.LLVMStructGetTypeAtIndex(result_type, 1);
        const left_ok = self.builder.buildLoad(ok_type, left_ok_ptr, "res.eq.left.ok");
        const right_ok = self.builder.buildLoad(ok_type, right_ok_ptr, "res.eq.right.ok");

        // Compare ok values
        const ok_kind = llvm.c.LLVMGetTypeKind(ok_type);
        const ok_equal = switch (ok_kind) {
            llvm.c.LLVMIntegerTypeKind => self.builder.buildICmp(llvm.c.LLVMIntEQ, left_ok, right_ok, "res.eq.ok"),
            llvm.c.LLVMFloatTypeKind, llvm.c.LLVMDoubleTypeKind => self.builder.buildFCmp(llvm.c.LLVMRealOEQ, left_ok, right_ok, "res.eq.ok"),
            llvm.c.LLVMPointerTypeKind => blk: {
                const strcmp_fn = self.getOrDeclareStrcmp();
                var args = [_]llvm.ValueRef{ left_ok, right_ok };
                const result = self.builder.buildCall(
                    llvm.c.LLVMGlobalGetValueType(strcmp_fn),
                    strcmp_fn,
                    &args,
                    "res.eq.ok.strcmp",
                );
                break :blk self.builder.buildICmp(llvm.c.LLVMIntEQ, result, llvm.Const.int32(self.ctx, 0), "res.eq.ok.str");
            },
            else => llvm.Const.int1(self.ctx, false),
        };
        _ = self.builder.buildBr(merge_block);
        const both_ok_end = llvm.c.LLVMGetInsertBlock(self.builder.ref);

        // Both Err => compare err values
        self.builder.positionAtEnd(both_err_block);
        var err_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 2),
        };
        const left_err_ptr = self.builder.buildGEP(result_type, left_alloca, &err_indices, "res.eq.left.err.ptr");
        const right_err_ptr = self.builder.buildGEP(result_type, right_alloca, &err_indices, "res.eq.right.err.ptr");
        const err_type = llvm.c.LLVMStructGetTypeAtIndex(result_type, 2);
        const left_err = self.builder.buildLoad(err_type, left_err_ptr, "res.eq.left.err");
        const right_err = self.builder.buildLoad(err_type, right_err_ptr, "res.eq.right.err");

        // Compare err values
        const err_kind = llvm.c.LLVMGetTypeKind(err_type);
        const err_equal = switch (err_kind) {
            llvm.c.LLVMIntegerTypeKind => self.builder.buildICmp(llvm.c.LLVMIntEQ, left_err, right_err, "res.eq.err"),
            llvm.c.LLVMFloatTypeKind, llvm.c.LLVMDoubleTypeKind => self.builder.buildFCmp(llvm.c.LLVMRealOEQ, left_err, right_err, "res.eq.err"),
            llvm.c.LLVMPointerTypeKind => blk: {
                const strcmp_fn = self.getOrDeclareStrcmp();
                var args = [_]llvm.ValueRef{ left_err, right_err };
                const result = self.builder.buildCall(
                    llvm.c.LLVMGlobalGetValueType(strcmp_fn),
                    strcmp_fn,
                    &args,
                    "res.eq.err.strcmp",
                );
                break :blk self.builder.buildICmp(llvm.c.LLVMIntEQ, result, llvm.Const.int32(self.ctx, 0), "res.eq.err.str");
            },
            else => llvm.Const.int1(self.ctx, false),
        };
        _ = self.builder.buildBr(merge_block);
        const both_err_end = llvm.c.LLVMGetInsertBlock(self.builder.ref);

        // Merge block with phi
        self.builder.positionAtEnd(merge_block);
        const phi = self.builder.buildPhi(llvm.Types.int1(self.ctx), "res.eq.result");
        var incoming_vals = [_]llvm.ValueRef{
            llvm.Const.int1(self.ctx, false), // tags differ
            ok_equal, // both ok
            err_equal, // both err
        };
        var incoming_blocks = [_]llvm.c.LLVMBasicBlockRef{
            tags_differ_block,
            both_ok_end,
            both_err_end,
        };
        llvm.c.LLVMAddIncoming(phi, &incoming_vals, &incoming_blocks, 3);

        return phi;
    }

    /// Emit Optional.clone() - returns a clone of the optional.
    /// For value types (integers, floats, bools), this is just a copy.
    fn emitOptionalClone(_: *Emitter, opt_val: llvm.ValueRef, _: llvm.TypeRef) EmitError!llvm.ValueRef {
        // For value types, the optional is already a value - just return it
        // The struct itself is copied (shallow copy), which is correct for primitives
        return opt_val;
    }

    /// Emit Result.clone() - returns a clone of the result.
    /// For value types (integers, floats, bools), this is just a copy.
    fn emitResultClone(_: *Emitter, result_val: llvm.ValueRef, _: llvm.TypeRef) EmitError!llvm.ValueRef {
        // For value types, the result is already a value - just return it
        // The struct itself is copied (shallow copy), which is correct for primitives
        return result_val;
    }

    /// Emit Result.unwrap() / Optional.unwrap() - traps if Err/None, returns value if Ok/Some.
    fn emitResultUnwrap(self: *Emitter, value: llvm.ValueRef, value_type: llvm.TypeRef) EmitError!llvm.ValueRef {
        const func = self.current_function orelse return EmitError.InvalidAST;

        // Store to temp for GEP access
        const val_alloca = self.builder.buildAlloca(value_type, "unwrap.tmp");
        _ = self.builder.buildStore(value, val_alloca);

        // Get the tag (index 0)
        var tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const tag_ptr = self.builder.buildGEP(value_type, val_alloca, &tag_indices, "unwrap.tag.ptr");
        const tag = self.builder.buildLoad(llvm.Types.int1(self.ctx), tag_ptr, "unwrap.tag");

        // Get the value (index 1)
        var val_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 1),
        };
        const inner_ptr = self.builder.buildGEP(value_type, val_alloca, &val_indices, "unwrap.val.ptr");
        const inner_type = llvm.c.LLVMStructGetTypeAtIndex(value_type, 1);

        const ok_block = llvm.appendBasicBlock(self.ctx, func, "unwrap.ok");
        const fail_block = llvm.appendBasicBlock(self.ctx, func, "unwrap.fail");

        // Branch based on tag
        _ = self.builder.buildCondBr(tag, ok_block, fail_block);

        // Fail block: trap
        self.builder.positionAtEnd(fail_block);
        _ = self.builder.buildUnreachable();

        // Ok block: return value
        self.builder.positionAtEnd(ok_block);
        return self.builder.buildLoad(inner_type, inner_ptr, "unwrap.val");
    }

    /// Emit Result.unwrap_err() - traps if Ok, returns error value if Err.
    fn emitResultUnwrapErr(self: *Emitter, result_val: llvm.ValueRef, result_type: llvm.TypeRef) EmitError!llvm.ValueRef {
        const func = self.current_function orelse return EmitError.InvalidAST;

        // Store to temp for GEP access
        const result_alloca = self.builder.buildAlloca(result_type, "unwrap_err.tmp");
        _ = self.builder.buildStore(result_val, result_alloca);

        // Get the tag (index 0)
        var tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const tag_ptr = self.builder.buildGEP(result_type, result_alloca, &tag_indices, "unwrap_err.tag.ptr");
        const tag = self.builder.buildLoad(llvm.Types.int1(self.ctx), tag_ptr, "unwrap_err.tag");

        // Get the error value (index 2 for Result)
        var err_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 2),
        };
        const err_ptr = self.builder.buildGEP(result_type, result_alloca, &err_indices, "unwrap_err.err.ptr");
        const err_type = llvm.c.LLVMStructGetTypeAtIndex(result_type, 2);

        const err_block = llvm.appendBasicBlock(self.ctx, func, "unwrap_err.err");
        const fail_block = llvm.appendBasicBlock(self.ctx, func, "unwrap_err.fail");

        // Branch based on tag: tag==0 means Err, tag==1 means Ok
        _ = self.builder.buildCondBr(tag, fail_block, err_block);

        // Fail block (Ok case): trap
        self.builder.positionAtEnd(fail_block);
        _ = self.builder.buildUnreachable();

        // Err block: return error value
        self.builder.positionAtEnd(err_block);
        return self.builder.buildLoad(err_type, err_ptr, "unwrap_err.val");
    }

    /// Emit Optional.unwrap_or(default) or Result.unwrap_or(default).
    /// Returns the inner value if Some/Ok, otherwise returns the default value.
    fn emitUnwrapOr(self: *Emitter, value: llvm.ValueRef, value_type: llvm.TypeRef, default_val: llvm.ValueRef) EmitError!llvm.ValueRef {
        const func = self.current_function orelse return EmitError.InvalidAST;

        // Store to temp for GEP access
        const val_alloca = self.builder.buildAlloca(value_type, "unwrap_or.tmp");
        _ = self.builder.buildStore(value, val_alloca);

        // Get the tag (index 0)
        var tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const tag_ptr = self.builder.buildGEP(value_type, val_alloca, &tag_indices, "unwrap_or.tag.ptr");
        const tag = self.builder.buildLoad(llvm.Types.int1(self.ctx), tag_ptr, "unwrap_or.tag");

        // Get the inner value (index 1 for Optional/Result Ok value)
        var val_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 1),
        };
        const inner_ptr = self.builder.buildGEP(value_type, val_alloca, &val_indices, "unwrap_or.val.ptr");
        const inner_type = llvm.c.LLVMStructGetTypeAtIndex(value_type, 1);

        // Create basic blocks
        const some_block = llvm.appendBasicBlock(self.ctx, func, "unwrap_or.some");
        const none_block = llvm.appendBasicBlock(self.ctx, func, "unwrap_or.none");
        const merge_block = llvm.appendBasicBlock(self.ctx, func, "unwrap_or.merge");

        // Branch based on tag: tag==1 means Some/Ok, tag==0 means None/Err
        _ = self.builder.buildCondBr(tag, some_block, none_block);

        // Some block: load the inner value
        self.builder.positionAtEnd(some_block);
        const some_val = self.builder.buildLoad(inner_type, inner_ptr, "unwrap_or.inner");
        _ = self.builder.buildBr(merge_block);

        // None block: use default value (already computed)
        self.builder.positionAtEnd(none_block);
        _ = self.builder.buildBr(merge_block);

        // Merge block: phi to select the result
        self.builder.positionAtEnd(merge_block);
        var incoming_values = [_]llvm.ValueRef{ some_val, default_val };
        var incoming_blocks = [_]llvm.BasicBlockRef{ some_block, none_block };
        const result = self.builder.buildPhi(inner_type, "unwrap_or.result");
        llvm.c.LLVMAddIncoming(result, &incoming_values, &incoming_blocks, 2);

        return result;
    }

    /// Emit Result.ok() method - converts Result[T, E] to ?T.
    /// Returns Some(value) if Ok, None if Err.
    fn emitResultOkMethod(self: *Emitter, result_val: llvm.ValueRef, result_type: llvm.TypeRef, method: *ast.MethodCall) EmitError!llvm.ValueRef {
        const func = self.current_function orelse return EmitError.InvalidAST;

        // Store Result to temp for GEP access
        const result_alloca = self.builder.buildAlloca(result_type, "result.ok.tmp");
        _ = self.builder.buildStore(result_val, result_alloca);

        // Get the tag (index 0)
        var tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const tag_ptr = self.builder.buildGEP(result_type, result_alloca, &tag_indices, "result.ok.tag.ptr");
        const tag = self.builder.buildLoad(llvm.Types.int1(self.ctx), tag_ptr, "result.ok.tag");

        // Get the ok value (index 1)
        var ok_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 1),
        };
        const ok_ptr = self.builder.buildGEP(result_type, result_alloca, &ok_indices, "result.ok.val.ptr");
        const ok_type = llvm.c.LLVMStructGetTypeAtIndex(result_type, 1);

        // Build Optional type: struct { i1, T } where T is the ok_type
        var opt_field_types = [_]llvm.TypeRef{ llvm.Types.int1(self.ctx), ok_type };
        const opt_type = llvm.Types.struct_(self.ctx, &opt_field_types, false);
        _ = method; // Unused, type built from Result's inner type

        // Create basic blocks
        const is_ok_block = llvm.appendBasicBlock(self.ctx, func, "result.ok.some");
        const is_err_block = llvm.appendBasicBlock(self.ctx, func, "result.ok.none");
        const merge_block = llvm.appendBasicBlock(self.ctx, func, "result.ok.merge");

        // Alloca for the optional result
        const opt_alloca = self.builder.buildAlloca(opt_type, "result.ok.opt");

        // Branch based on tag: tag==1 means Ok, tag==0 means Err
        _ = self.builder.buildCondBr(tag, is_ok_block, is_err_block);

        // Ok block: create Some(value)
        self.builder.positionAtEnd(is_ok_block);
        const ok_val = self.builder.buildLoad(ok_type, ok_ptr, "result.ok.inner");
        // Set tag to 1 (Some)
        var opt_tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const opt_tag_ptr = self.builder.buildGEP(opt_type, opt_alloca, &opt_tag_indices, "opt.tag.ptr");
        _ = self.builder.buildStore(llvm.Const.int(llvm.Types.int1(self.ctx), 1, false), opt_tag_ptr);
        // Set value
        var opt_val_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 1),
        };
        const opt_val_ptr = self.builder.buildGEP(opt_type, opt_alloca, &opt_val_indices, "opt.val.ptr");
        _ = self.builder.buildStore(ok_val, opt_val_ptr);
        _ = self.builder.buildBr(merge_block);

        // Err block: create None
        self.builder.positionAtEnd(is_err_block);
        // Set tag to 0 (None)
        const none_tag_ptr = self.builder.buildGEP(opt_type, opt_alloca, &opt_tag_indices, "opt.tag.ptr.none");
        _ = self.builder.buildStore(llvm.Const.int(llvm.Types.int1(self.ctx), 0, false), none_tag_ptr);
        _ = self.builder.buildBr(merge_block);

        // Merge block: load and return the optional
        self.builder.positionAtEnd(merge_block);
        return self.builder.buildLoad(opt_type, opt_alloca, "result.ok.result");
    }

    /// Emit Result.err() method - converts Result[T, E] to ?E.
    /// Returns Some(error) if Err, None if Ok.
    fn emitResultErrMethod(self: *Emitter, result_val: llvm.ValueRef, result_type: llvm.TypeRef, method: *ast.MethodCall) EmitError!llvm.ValueRef {
        const func = self.current_function orelse return EmitError.InvalidAST;

        // Store Result to temp for GEP access
        const result_alloca = self.builder.buildAlloca(result_type, "result.err.tmp");
        _ = self.builder.buildStore(result_val, result_alloca);

        // Get the tag (index 0)
        var tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const tag_ptr = self.builder.buildGEP(result_type, result_alloca, &tag_indices, "result.err.tag.ptr");
        const tag = self.builder.buildLoad(llvm.Types.int1(self.ctx), tag_ptr, "result.err.tag");

        // Get the err value (index 2)
        var err_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 2),
        };
        const err_ptr = self.builder.buildGEP(result_type, result_alloca, &err_indices, "result.err.val.ptr");
        const err_type = llvm.c.LLVMStructGetTypeAtIndex(result_type, 2);

        // Build Optional type: struct { i1, E } where E is the err_type
        var opt_field_types = [_]llvm.TypeRef{ llvm.Types.int1(self.ctx), err_type };
        const opt_type = llvm.Types.struct_(self.ctx, &opt_field_types, false);
        _ = method; // Unused, type built from Result's inner type

        // Create basic blocks
        const is_err_block = llvm.appendBasicBlock(self.ctx, func, "result.err.some");
        const is_ok_block = llvm.appendBasicBlock(self.ctx, func, "result.err.none");
        const merge_block = llvm.appendBasicBlock(self.ctx, func, "result.err.merge");

        // Alloca for the optional result
        const opt_alloca = self.builder.buildAlloca(opt_type, "result.err.opt");

        // Branch based on tag: tag==0 means Err, tag==1 means Ok
        _ = self.builder.buildCondBr(tag, is_ok_block, is_err_block);

        // Err block: create Some(error)
        self.builder.positionAtEnd(is_err_block);
        const err_val = self.builder.buildLoad(err_type, err_ptr, "result.err.inner");
        // Set tag to 1 (Some)
        var opt_tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const opt_tag_ptr = self.builder.buildGEP(opt_type, opt_alloca, &opt_tag_indices, "opt.tag.ptr");
        _ = self.builder.buildStore(llvm.Const.int(llvm.Types.int1(self.ctx), 1, false), opt_tag_ptr);
        // Set value
        var opt_val_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 1),
        };
        const opt_val_ptr = self.builder.buildGEP(opt_type, opt_alloca, &opt_val_indices, "opt.val.ptr");
        _ = self.builder.buildStore(err_val, opt_val_ptr);
        _ = self.builder.buildBr(merge_block);

        // Ok block: create None
        self.builder.positionAtEnd(is_ok_block);
        // Set tag to 0 (None)
        const none_tag_ptr = self.builder.buildGEP(opt_type, opt_alloca, &opt_tag_indices, "opt.tag.ptr.none");
        _ = self.builder.buildStore(llvm.Const.int(llvm.Types.int1(self.ctx), 0, false), none_tag_ptr);
        _ = self.builder.buildBr(merge_block);

        // Merge block: load and return the optional
        self.builder.positionAtEnd(merge_block);
        return self.builder.buildLoad(opt_type, opt_alloca, "result.err.result");
    }

    /// Emit Optional.map(f) or Result.map(f) - applies f to inner value.
    /// For Optional[T].map(f: fn(T) -> U) -> Optional[U]: Some(v) -> Some(f(v)), None -> None
    /// For Result[T, E].map(f: fn(T) -> U) -> Result[U, E]: Ok(v) -> Ok(f(v)), Err(e) -> Err(e)
    fn emitMapMethod(self: *Emitter, value: llvm.ValueRef, value_type: llvm.TypeRef, func_val: llvm.ValueRef, method: *ast.MethodCall) EmitError!llvm.ValueRef {
        const func = self.current_function orelse return EmitError.InvalidAST;

        // Store value to temp for GEP access
        const val_alloca = self.builder.buildAlloca(value_type, "map.tmp");
        _ = self.builder.buildStore(value, val_alloca);

        // Get the tag (index 0)
        var tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const tag_ptr = self.builder.buildGEP(value_type, val_alloca, &tag_indices, "map.tag.ptr");
        const tag = self.builder.buildLoad(llvm.Types.int1(self.ctx), tag_ptr, "map.tag");

        // Get the inner value (index 1 for Optional/Result Ok value)
        var inner_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 1),
        };
        const inner_ptr = self.builder.buildGEP(value_type, val_alloca, &inner_indices, "map.inner.ptr");
        const inner_type = llvm.c.LLVMStructGetTypeAtIndex(value_type, 1);

        // Determine if this is a Result (has 3 fields) or Optional (has 2 fields)
        const num_fields = llvm.c.LLVMCountStructElementTypes(value_type);
        const is_result = num_fields == 3;

        // Create basic blocks
        const some_block = llvm.appendBasicBlock(self.ctx, func, "map.some");
        const none_block = llvm.appendBasicBlock(self.ctx, func, "map.none");
        const merge_block = llvm.appendBasicBlock(self.ctx, func, "map.merge");

        // Branch based on tag
        _ = self.builder.buildCondBr(tag, some_block, none_block);

        // Some/Ok block: call the function and wrap result
        self.builder.positionAtEnd(some_block);
        const inner_val = self.builder.buildLoad(inner_type, inner_ptr, "map.inner");

        // Call the closure with the inner value (map closure returns i32)
        const mapped_val = try self.emitClosureCallWithValue(func_val, inner_val, inner_type, llvm.Types.int32(self.ctx));
        const mapped_type = llvm.c.LLVMTypeOf(mapped_val);

        // Build result type and wrap the mapped value
        var some_result: llvm.ValueRef = undefined;
        var result_type: llvm.TypeRef = undefined;

        if (is_result) {
            // Result: build Result[U, E] with ok_type = mapped_type, err_type unchanged
            const err_type = llvm.c.LLVMStructGetTypeAtIndex(value_type, 2);
            var result_fields = [_]llvm.TypeRef{ llvm.Types.int1(self.ctx), mapped_type, err_type };
            result_type = llvm.Types.struct_(self.ctx, &result_fields, false);

            // Create Ok(mapped_val)
            const result_alloca = self.builder.buildAlloca(result_type, "map.result.ok");
            var res_tag_indices = [_]llvm.ValueRef{
                llvm.Const.int32(self.ctx, 0),
                llvm.Const.int32(self.ctx, 0),
            };
            const res_tag_ptr = self.builder.buildGEP(result_type, result_alloca, &res_tag_indices, "map.result.tag.ptr");
            _ = self.builder.buildStore(llvm.Const.int(llvm.Types.int1(self.ctx), 1, false), res_tag_ptr);

            var res_val_indices = [_]llvm.ValueRef{
                llvm.Const.int32(self.ctx, 0),
                llvm.Const.int32(self.ctx, 1),
            };
            const res_val_ptr = self.builder.buildGEP(result_type, result_alloca, &res_val_indices, "map.result.val.ptr");
            _ = self.builder.buildStore(mapped_val, res_val_ptr);

            some_result = self.builder.buildLoad(result_type, result_alloca, "map.result.ok.val");
        } else {
            // Optional: build Optional[U]
            var opt_fields = [_]llvm.TypeRef{ llvm.Types.int1(self.ctx), mapped_type };
            result_type = llvm.Types.struct_(self.ctx, &opt_fields, false);

            // Create Some(mapped_val)
            some_result = self.emitSome(mapped_val, mapped_type);
        }
        _ = self.builder.buildBr(merge_block);
        const some_end_block = llvm.c.LLVMGetInsertBlock(self.builder.ref);

        // None/Err block: return None or propagate Err
        self.builder.positionAtEnd(none_block);
        var none_result: llvm.ValueRef = undefined;

        if (is_result) {
            // Result: propagate the Err unchanged (but need to build with new ok_type)
            const err_type = llvm.c.LLVMStructGetTypeAtIndex(value_type, 2);
            var err_indices = [_]llvm.ValueRef{
                llvm.Const.int32(self.ctx, 0),
                llvm.Const.int32(self.ctx, 2),
            };
            const err_ptr = self.builder.buildGEP(value_type, val_alloca, &err_indices, "map.err.ptr");
            const err_val = self.builder.buildLoad(err_type, err_ptr, "map.err");

            // Create Err(err_val) with new result type
            const result_alloca = self.builder.buildAlloca(result_type, "map.result.err");
            var res_tag_indices = [_]llvm.ValueRef{
                llvm.Const.int32(self.ctx, 0),
                llvm.Const.int32(self.ctx, 0),
            };
            const res_tag_ptr = self.builder.buildGEP(result_type, result_alloca, &res_tag_indices, "map.result.err.tag.ptr");
            _ = self.builder.buildStore(llvm.Const.int(llvm.Types.int1(self.ctx), 0, false), res_tag_ptr);

            var res_err_indices = [_]llvm.ValueRef{
                llvm.Const.int32(self.ctx, 0),
                llvm.Const.int32(self.ctx, 2),
            };
            const res_err_ptr = self.builder.buildGEP(result_type, result_alloca, &res_err_indices, "map.result.err.ptr");
            _ = self.builder.buildStore(err_val, res_err_ptr);

            none_result = self.builder.buildLoad(result_type, result_alloca, "map.result.err.val");
        } else {
            // Optional: return None
            none_result = self.emitNone(result_type);
        }
        _ = self.builder.buildBr(merge_block);
        const none_end_block = llvm.c.LLVMGetInsertBlock(self.builder.ref);

        // Merge block
        self.builder.positionAtEnd(merge_block);
        var incoming_vals = [_]llvm.ValueRef{ some_result, none_result };
        var incoming_blocks = [_]llvm.BasicBlockRef{ some_end_block, none_end_block };
        const phi = self.builder.buildPhi(result_type, "map.result");
        llvm.addIncoming(phi, &incoming_vals, &incoming_blocks);

        _ = method; // AST node used for type info if needed
        return phi;
    }

    /// Emit Result.map_err(f) - applies f to error value.
    /// For Result[T, E].map_err(f: fn(E) -> F) -> Result[T, F]: Ok(v) -> Ok(v), Err(e) -> Err(f(e))
    fn emitMapErrMethod(self: *Emitter, value: llvm.ValueRef, value_type: llvm.TypeRef, func_val: llvm.ValueRef, method: *ast.MethodCall) EmitError!llvm.ValueRef {
        const func = self.current_function orelse return EmitError.InvalidAST;

        // Store value to temp for GEP access
        const val_alloca = self.builder.buildAlloca(value_type, "map_err.tmp");
        _ = self.builder.buildStore(value, val_alloca);

        // Get the tag (index 0)
        var tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const tag_ptr = self.builder.buildGEP(value_type, val_alloca, &tag_indices, "map_err.tag.ptr");
        const tag = self.builder.buildLoad(llvm.Types.int1(self.ctx), tag_ptr, "map_err.tag");

        // Get the ok value type (index 1) - unchanged in map_err
        const ok_type = llvm.c.LLVMStructGetTypeAtIndex(value_type, 1);

        // Get the error value (index 2)
        var err_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 2),
        };
        const err_ptr = self.builder.buildGEP(value_type, val_alloca, &err_indices, "map_err.err.ptr");
        const err_type = llvm.c.LLVMStructGetTypeAtIndex(value_type, 2);

        // Create basic blocks
        const ok_block = llvm.appendBasicBlock(self.ctx, func, "map_err.ok");
        const err_block = llvm.appendBasicBlock(self.ctx, func, "map_err.err");
        const merge_block = llvm.appendBasicBlock(self.ctx, func, "map_err.merge");

        // Branch based on tag (1 = Ok, 0 = Err)
        _ = self.builder.buildCondBr(tag, ok_block, err_block);

        // Ok block: propagate Ok unchanged (but need to build with new err_type)
        self.builder.positionAtEnd(ok_block);

        // Get the ok value
        var ok_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 1),
        };
        const ok_ptr = self.builder.buildGEP(value_type, val_alloca, &ok_indices, "map_err.ok.ptr");
        const ok_val = self.builder.buildLoad(ok_type, ok_ptr, "map_err.ok");

        // Err block: call the function and wrap result
        self.builder.positionAtEnd(err_block);
        const err_val = self.builder.buildLoad(err_type, err_ptr, "map_err.err");

        // Call the closure with the error value
        const mapped_err_val = try self.emitClosureCallWithValue(func_val, err_val, err_type, llvm.Types.int32(self.ctx));
        const mapped_err_type = llvm.c.LLVMTypeOf(mapped_err_val);

        // Build result type: Result[T, F] where T is unchanged, F is the mapped error type
        var result_fields = [_]llvm.TypeRef{ llvm.Types.int1(self.ctx), ok_type, mapped_err_type };
        const result_type = llvm.Types.struct_(self.ctx, &result_fields, false);

        // Go back to Ok block and create Ok(ok_val) with new result type
        self.builder.positionAtEnd(ok_block);
        const ok_result_alloca = self.builder.buildAlloca(result_type, "map_err.result.ok");
        var ok_res_tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const ok_res_tag_ptr = self.builder.buildGEP(result_type, ok_result_alloca, &ok_res_tag_indices, "map_err.result.ok.tag.ptr");
        _ = self.builder.buildStore(llvm.Const.int(llvm.Types.int1(self.ctx), 1, false), ok_res_tag_ptr);

        var ok_res_val_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 1),
        };
        const ok_res_val_ptr = self.builder.buildGEP(result_type, ok_result_alloca, &ok_res_val_indices, "map_err.result.ok.val.ptr");
        _ = self.builder.buildStore(ok_val, ok_res_val_ptr);

        const ok_result = self.builder.buildLoad(result_type, ok_result_alloca, "map_err.result.ok.val");
        _ = self.builder.buildBr(merge_block);
        const ok_end_block = llvm.c.LLVMGetInsertBlock(self.builder.ref);

        // Continue Err block: create Err(mapped_err_val)
        self.builder.positionAtEnd(err_block);
        const err_result_alloca = self.builder.buildAlloca(result_type, "map_err.result.err");
        var err_res_tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const err_res_tag_ptr = self.builder.buildGEP(result_type, err_result_alloca, &err_res_tag_indices, "map_err.result.err.tag.ptr");
        _ = self.builder.buildStore(llvm.Const.int(llvm.Types.int1(self.ctx), 0, false), err_res_tag_ptr);

        var err_res_val_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 2),
        };
        const err_res_val_ptr = self.builder.buildGEP(result_type, err_result_alloca, &err_res_val_indices, "map_err.result.err.val.ptr");
        _ = self.builder.buildStore(mapped_err_val, err_res_val_ptr);

        const err_result = self.builder.buildLoad(result_type, err_result_alloca, "map_err.result.err.val");
        _ = self.builder.buildBr(merge_block);
        const err_end_block = llvm.c.LLVMGetInsertBlock(self.builder.ref);

        // Merge block
        self.builder.positionAtEnd(merge_block);
        var incoming_vals = [_]llvm.ValueRef{ ok_result, err_result };
        var incoming_blocks = [_]llvm.BasicBlockRef{ ok_end_block, err_end_block };
        const phi = self.builder.buildPhi(result_type, "map_err.result");
        llvm.addIncoming(phi, &incoming_vals, &incoming_blocks);

        _ = method; // AST node used for type info if needed
        return phi;
    }

    /// Emit Optional.and_then(f) or Result.and_then(f) - applies f and flattens.
    /// For Optional[T].and_then(f: fn(T) -> ?U) -> ?U: Some(v) -> f(v), None -> None
    /// For Result[T, E].and_then(f: fn(T) -> Result[U, E]) -> Result[U, E]: Ok(v) -> f(v), Err(e) -> Err(e)
    fn emitAndThenMethod(self: *Emitter, value: llvm.ValueRef, value_type: llvm.TypeRef, func_val: llvm.ValueRef, method: *ast.MethodCall) EmitError!llvm.ValueRef {
        const func = self.current_function orelse return EmitError.InvalidAST;

        // Store value to temp for GEP access
        const val_alloca = self.builder.buildAlloca(value_type, "and_then.tmp");
        _ = self.builder.buildStore(value, val_alloca);

        // Get the tag (index 0)
        var tag_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const tag_ptr = self.builder.buildGEP(value_type, val_alloca, &tag_indices, "and_then.tag.ptr");
        const tag = self.builder.buildLoad(llvm.Types.int1(self.ctx), tag_ptr, "and_then.tag");

        // Get the inner value (index 1)
        var inner_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 1),
        };
        const inner_ptr = self.builder.buildGEP(value_type, val_alloca, &inner_indices, "and_then.inner.ptr");
        const inner_type = llvm.c.LLVMStructGetTypeAtIndex(value_type, 1);

        // Determine if this is a Result (has 3 fields) or Optional (has 2 fields)
        const num_fields = llvm.c.LLVMCountStructElementTypes(value_type);
        const is_result = num_fields == 3;

        // Create basic blocks
        const some_block = llvm.appendBasicBlock(self.ctx, func, "and_then.some");
        const none_block = llvm.appendBasicBlock(self.ctx, func, "and_then.none");
        const merge_block = llvm.appendBasicBlock(self.ctx, func, "and_then.merge");

        // Branch based on tag
        _ = self.builder.buildCondBr(tag, some_block, none_block);

        // Determine the result type BEFORE branching (for the closure return type)
        // For and_then, the closure returns Optional[U] or Result[U, E]
        // Since we're in the Some/Ok branch, the result type is the same structure as input
        // but possibly with different inner type U
        var result_type: llvm.TypeRef = undefined;
        if (is_result) {
            // Result[U, E] - for now assume U is i32 (same as original ok_type)
            const err_type = llvm.c.LLVMStructGetTypeAtIndex(value_type, 2);
            var result_fields = [_]llvm.TypeRef{ llvm.Types.int1(self.ctx), llvm.Types.int32(self.ctx), err_type };
            result_type = llvm.Types.struct_(self.ctx, &result_fields, false);
        } else {
            // Optional[U] - for now assume U is i32
            var opt_fields = [_]llvm.TypeRef{ llvm.Types.int1(self.ctx), llvm.Types.int32(self.ctx) };
            result_type = llvm.Types.struct_(self.ctx, &opt_fields, false);
        }

        // Some/Ok block: call the function (result is already Optional/Result)
        self.builder.positionAtEnd(some_block);
        const inner_val = self.builder.buildLoad(inner_type, inner_ptr, "and_then.inner");

        // Call the closure - returns the new Optional/Result directly
        const func_result = try self.emitClosureCallWithValue(func_val, inner_val, inner_type, result_type);
        _ = self.builder.buildBr(merge_block);
        const some_end_block = llvm.c.LLVMGetInsertBlock(self.builder.ref);

        // None/Err block: return None or propagate Err
        self.builder.positionAtEnd(none_block);
        var none_result: llvm.ValueRef = undefined;

        if (is_result) {
            // Result: propagate the Err with the new result type
            const err_type = llvm.c.LLVMStructGetTypeAtIndex(value_type, 2);
            var err_indices = [_]llvm.ValueRef{
                llvm.Const.int32(self.ctx, 0),
                llvm.Const.int32(self.ctx, 2),
            };
            const err_ptr = self.builder.buildGEP(value_type, val_alloca, &err_indices, "and_then.err.ptr");
            const err_val = self.builder.buildLoad(err_type, err_ptr, "and_then.err");

            // Create Err with result_type (already computed above)
            const result_alloca = self.builder.buildAlloca(result_type, "and_then.result.err");
            var res_tag_indices = [_]llvm.ValueRef{
                llvm.Const.int32(self.ctx, 0),
                llvm.Const.int32(self.ctx, 0),
            };
            const res_tag_ptr = self.builder.buildGEP(result_type, result_alloca, &res_tag_indices, "and_then.result.err.tag.ptr");
            _ = self.builder.buildStore(llvm.Const.int(llvm.Types.int1(self.ctx), 0, false), res_tag_ptr);

            var res_err_indices = [_]llvm.ValueRef{
                llvm.Const.int32(self.ctx, 0),
                llvm.Const.int32(self.ctx, 2),
            };
            const res_err_ptr = self.builder.buildGEP(result_type, result_alloca, &res_err_indices, "and_then.result.err.ptr");
            _ = self.builder.buildStore(err_val, res_err_ptr);

            none_result = self.builder.buildLoad(result_type, result_alloca, "and_then.result.err.val");
        } else {
            // Optional: return None of the result type
            none_result = self.emitNone(result_type);
        }
        _ = self.builder.buildBr(merge_block);
        const none_end_block = llvm.c.LLVMGetInsertBlock(self.builder.ref);

        // Merge block
        self.builder.positionAtEnd(merge_block);
        var incoming_vals = [_]llvm.ValueRef{ func_result, none_result };
        var incoming_blocks = [_]llvm.BasicBlockRef{ some_end_block, none_end_block };
        const phi = self.builder.buildPhi(result_type, "and_then.result");
        llvm.addIncoming(phi, &incoming_vals, &incoming_blocks);

        _ = method; // AST node for type info if needed
        return phi;
    }

    /// Helper to call a closure with a single value argument.
    /// return_type: the expected return type of the closure (use int32 for map, Optional/Result for and_then)
    fn emitClosureCallWithValue(self: *Emitter, closure_val: llvm.ValueRef, arg_val: llvm.ValueRef, arg_type: llvm.TypeRef, return_type: llvm.TypeRef) EmitError!llvm.ValueRef {
        const closure_struct_type = self.getClosureStructType();

        // Allocate space to store the closure value
        const closure_alloca = self.builder.buildAlloca(closure_struct_type, "map.closure.tmp");
        _ = self.builder.buildStore(closure_val, closure_alloca);

        // Load function pointer
        var fn_ptr_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 0),
        };
        const fn_ptr_gep = self.builder.buildGEP(closure_struct_type, closure_alloca, &fn_ptr_indices, "map.fn.ptr.ptr");
        const fn_ptr = self.builder.buildLoad(llvm.Types.pointer(self.ctx), fn_ptr_gep, "map.fn.ptr");

        // Load environment pointer
        var env_ptr_indices = [_]llvm.ValueRef{
            llvm.Const.int32(self.ctx, 0),
            llvm.Const.int32(self.ctx, 1),
        };
        const env_ptr_gep = self.builder.buildGEP(closure_struct_type, closure_alloca, &env_ptr_indices, "map.env.ptr.ptr");
        const env_ptr = self.builder.buildLoad(llvm.Types.pointer(self.ctx), env_ptr_gep, "map.env.ptr");

        // Build call args: env_ptr + arg_val
        var call_args = [_]llvm.ValueRef{ env_ptr, arg_val };

        // Build function type: fn(ptr, arg_type) -> return_type
        var param_types = [_]llvm.TypeRef{ llvm.Types.pointer(self.ctx), arg_type };
        const fn_type = llvm.Types.function(return_type, &param_types, false);

        return self.builder.buildCall(fn_type, fn_ptr, &call_args, "map.closure.call");
    }

    /// Emit the Eq trait's eq() method for equality comparison.
    /// Works for primitives (integers, floats, bools, chars, strings).
    fn emitEqMethod(self: *Emitter, method: *ast.MethodCall, left: llvm.ValueRef, left_type: llvm.TypeRef) EmitError!llvm.ValueRef {
        // Get the other argument
        if (method.args.len != 1) {
            return EmitError.InvalidAST;
        }

        var right = try self.emitExpr(method.args[0]);

        // Dispatch based on the LLVM type kind of the left operand
        const left_kind = llvm.c.LLVMGetTypeKind(left_type);

        // For non-pointer primitives (int, float, bool), if the right side is
        // a pointer (reference), dereference it. But for pointers (strings),
        // don't dereference - the pointer IS the value.
        if (left_kind != llvm.c.LLVMPointerTypeKind) {
            const right_type = llvm.typeOf(right);
            const right_kind = llvm.c.LLVMGetTypeKind(right_type);
            if (right_kind == llvm.c.LLVMPointerTypeKind) {
                // Dereference the pointer to get the actual value
                right = self.builder.buildLoad(left_type, right, "eq.deref");
            }
        }

        switch (left_kind) {
            llvm.c.LLVMIntegerTypeKind => {
                // Integer or bool comparison
                return self.builder.buildICmp(llvm.c.LLVMIntEQ, left, right, "eq.int");
            },
            llvm.c.LLVMFloatTypeKind, llvm.c.LLVMDoubleTypeKind => {
                // Float comparison (OEQ = ordered equal)
                return self.builder.buildFCmp(llvm.c.LLVMRealOEQ, left, right, "eq.float");
            },
            llvm.c.LLVMPointerTypeKind => {
                // This is likely a string (char*) - use strcmp
                const strcmp_fn = self.getOrDeclareStrcmp();
                var args = [_]llvm.ValueRef{ left, right };
                const result = self.builder.buildCall(
                    llvm.c.LLVMGlobalGetValueType(strcmp_fn),
                    strcmp_fn,
                    &args,
                    "eq.strcmp.result",
                );
                // strcmp returns 0 if equal, so compare result == 0
                return self.builder.buildICmp(
                    llvm.c.LLVMIntEQ,
                    result,
                    llvm.Const.int32(self.ctx, 0),
                    "eq.str",
                );
            },
            llvm.c.LLVMStructTypeKind => {
                // Check if this is Optional (2 fields) or Result (3 fields)
                const num_fields = llvm.c.LLVMCountStructElementTypes(left_type);
                if (num_fields == 2) {
                    // Optional[T] - compare: both None = true, both Some with equal values = true
                    return self.emitOptionalEq(left, right, left_type);
                } else if (num_fields == 3) {
                    // Result[T, E] - compare: both Ok with equal values = true, both Err with equal values = true
                    return self.emitResultEq(left, right, left_type);
                }
                // For user-defined structs, we need to call the user-defined eq method
                // This should be handled by the user-defined method lookup before we get here
                // For now, fall through to placeholder
                return llvm.Const.int1(self.ctx, false);
            },
            else => {
                // Unsupported type - return false
                return llvm.Const.int1(self.ctx, false);
            },
        }
    }

    /// Emit the Ordered trait's comparison methods (lt, le, gt, ge).
    /// Works for primitives (integers, floats, strings).
    fn emitOrderedMethod(self: *Emitter, method: *ast.MethodCall, left: llvm.ValueRef, left_type: llvm.TypeRef) EmitError!llvm.ValueRef {
        // Get the other argument
        if (method.args.len != 1) {
            return EmitError.InvalidAST;
        }

        var right = try self.emitExpr(method.args[0]);

        // Dispatch based on the LLVM type kind of the left operand
        const left_kind = llvm.c.LLVMGetTypeKind(left_type);

        // For non-pointer primitives (int, float), if the right side is
        // a pointer (reference), dereference it. But for pointers (strings),
        // don't dereference - the pointer IS the value.
        if (left_kind != llvm.c.LLVMPointerTypeKind) {
            const right_type = llvm.typeOf(right);
            const right_kind = llvm.c.LLVMGetTypeKind(right_type);
            if (right_kind == llvm.c.LLVMPointerTypeKind) {
                // Dereference the pointer to get the actual value
                right = self.builder.buildLoad(left_type, right, "ord.deref");
            }
        }

        // Determine the comparison predicate based on method name
        const method_name = method.method_name;

        switch (left_kind) {
            llvm.c.LLVMIntegerTypeKind => {
                // Integer comparison (signed)
                const predicate: llvm.c.LLVMIntPredicate = if (std.mem.eql(u8, method_name, "lt"))
                    llvm.c.LLVMIntSLT
                else if (std.mem.eql(u8, method_name, "le"))
                    llvm.c.LLVMIntSLE
                else if (std.mem.eql(u8, method_name, "gt"))
                    llvm.c.LLVMIntSGT
                else // ge
                    llvm.c.LLVMIntSGE;
                return self.builder.buildICmp(predicate, left, right, "ord.int");
            },
            llvm.c.LLVMFloatTypeKind, llvm.c.LLVMDoubleTypeKind => {
                // Float comparison (ordered)
                const predicate: llvm.c.LLVMRealPredicate = if (std.mem.eql(u8, method_name, "lt"))
                    llvm.c.LLVMRealOLT
                else if (std.mem.eql(u8, method_name, "le"))
                    llvm.c.LLVMRealOLE
                else if (std.mem.eql(u8, method_name, "gt"))
                    llvm.c.LLVMRealOGT
                else // ge
                    llvm.c.LLVMRealOGE;
                return self.builder.buildFCmp(predicate, left, right, "ord.float");
            },
            llvm.c.LLVMPointerTypeKind => {
                // This is likely a string (char*) - use strcmp
                const strcmp_fn = self.getOrDeclareStrcmp();
                var args = [_]llvm.ValueRef{ left, right };
                const result = self.builder.buildCall(
                    llvm.c.LLVMGlobalGetValueType(strcmp_fn),
                    strcmp_fn,
                    &args,
                    "ord.strcmp.result",
                );
                // strcmp returns: negative if s1 < s2, 0 if equal, positive if s1 > s2
                // lt: result < 0
                // le: result <= 0
                // gt: result > 0
                // ge: result >= 0
                const predicate: llvm.c.LLVMIntPredicate = if (std.mem.eql(u8, method_name, "lt"))
                    llvm.c.LLVMIntSLT
                else if (std.mem.eql(u8, method_name, "le"))
                    llvm.c.LLVMIntSLE
                else if (std.mem.eql(u8, method_name, "gt"))
                    llvm.c.LLVMIntSGT
                else // ge
                    llvm.c.LLVMIntSGE;
                return self.builder.buildICmp(
                    predicate,
                    result,
                    llvm.Const.int32(self.ctx, 0),
                    "ord.str",
                );
            },
            llvm.c.LLVMStructTypeKind => {
                // For structs that implement Ordered, we need to call the user-defined method
                // This should be handled by the user-defined method lookup before we get here
                // For now, fall through to placeholder
                return llvm.Const.int1(self.ctx, false);
            },
            else => {
                // Unsupported type - return false
                return llvm.Const.int1(self.ctx, false);
            },
        }
    }

    /// Emit the Clone trait's clone() method for explicit cloning.
    /// Works for primitives (integers, floats, bools, chars) and strings.
    fn emitCloneMethod(self: *Emitter, method: *ast.MethodCall, value: llvm.ValueRef, value_type: llvm.TypeRef) EmitError!llvm.ValueRef {
        _ = method; // clone() takes no arguments

        // Dispatch based on the LLVM type kind
        const kind = llvm.c.LLVMGetTypeKind(value_type);

        switch (kind) {
            llvm.c.LLVMIntegerTypeKind => {
                // Integers and bools are value types - cloning is just returning the value
                return value;
            },
            llvm.c.LLVMFloatTypeKind, llvm.c.LLVMDoubleTypeKind => {
                // Floats are value types - cloning is just returning the value
                return value;
            },
            llvm.c.LLVMPointerTypeKind => {
                // This is likely a string (char*) - use strdup to create a copy
                const strdup_fn = self.getOrDeclareStrdup();
                var args = [_]llvm.ValueRef{value};
                return self.builder.buildCall(
                    llvm.c.LLVMGlobalGetValueType(strdup_fn),
                    strdup_fn,
                    &args,
                    "clone.str",
                );
            },
            llvm.c.LLVMStructTypeKind => {
                // Check if this is Optional (2 fields) or Result (3 fields)
                const num_fields = llvm.c.LLVMCountStructElementTypes(value_type);
                if (num_fields == 2) {
                    // Optional[T] - clone the optional
                    return self.emitOptionalClone(value, value_type);
                } else if (num_fields == 3) {
                    // Result[T, E] - clone the result
                    return self.emitResultClone(value, value_type);
                }
                // For user-defined structs that implement Clone, we need to call the user-defined clone method
                // This should be handled by the user-defined method lookup before we get here
                // For now, just return the value (shallow copy)
                return value;
            },
            else => {
                // Unsupported type - just return the value
                return value;
            },
        }
    }

    /// Emit the Drop trait's drop() method for explicit destruction.
    /// This calls the user-defined drop method on structs that implement Drop.
    /// Primitives don't have drop() - they are trivially destroyed.
    fn emitDropMethod(self: *Emitter, method: *ast.MethodCall, object: llvm.ValueRef, object_type: llvm.TypeRef) EmitError!llvm.ValueRef {
        _ = object_type;

        // For structs that implement Drop, we look up the user-defined drop method
        if (self.type_checker) |tc| {
            const struct_name = self.getStructNameFromExpr(method.object);
            if (struct_name) |name| {
                // Look up the drop method in the struct's impl block
                if (tc.lookupStructMethod(name, "drop")) |drop_method| {
                    // Call the user-defined drop method with the object as self
                    return self.emitUserDefinedMethod(method, object, name, drop_method);
                }
            }
        }

        // No drop method found - return void (0)
        // The type checker should have caught this already
        return llvm.Const.int32(self.ctx, 0);
    }

    /// Emit the Hash trait's hash() method for computing hash codes.
    /// Returns i64 hash value. All primitives have builtin Hash.
    /// Uses FNV-1a hash algorithm for consistent, good distribution.
    fn emitHashMethod(self: *Emitter, method: *ast.MethodCall, value: llvm.ValueRef, value_type: llvm.TypeRef) EmitError!llvm.ValueRef {
        _ = method; // hash() takes no arguments

        // Dispatch based on the LLVM type kind
        const kind = llvm.c.LLVMGetTypeKind(value_type);
        const i64_type = llvm.Types.int64(self.ctx);

        switch (kind) {
            llvm.c.LLVMIntegerTypeKind => {
                // For integers: convert to i64 and XOR with FNV offset basis
                // This gives a simple but effective hash for integer values
                const bit_width = llvm.c.LLVMGetIntTypeWidth(value_type);
                if (bit_width <= 64) {
                    // Sign extend or zero extend to i64
                    const ext_value = if (bit_width < 64)
                        self.builder.buildSExt(value, i64_type, "hash.ext")
                    else
                        value;
                    // XOR with FNV-1a offset basis for better distribution
                    // FNV offset basis: 14695981039346656037 (0xcbf29ce484222325)
                    const fnv_offset = llvm.Const.int64(self.ctx, @bitCast(@as(u64, 0xcbf29ce484222325)));
                    return self.builder.buildXor(ext_value, fnv_offset, "hash.int");
                } else {
                    // For larger integers (i128), truncate to i64 and hash
                    const trunc_value = self.builder.buildTrunc(value, i64_type, "hash.trunc");
                    const fnv_offset = llvm.Const.int64(self.ctx, @bitCast(@as(u64, 0xcbf29ce484222325)));
                    return self.builder.buildXor(trunc_value, fnv_offset, "hash.bigint");
                }
            },
            llvm.c.LLVMFloatTypeKind => {
                // For floats: bitcast to i32, then extend to i64
                const i32_type = llvm.Types.int32(self.ctx);
                const bits = llvm.c.LLVMBuildBitCast(self.builder.ref, value, i32_type, "hash.f32bits");
                const ext_bits = self.builder.buildSExt(bits, i64_type, "hash.ext");
                const fnv_offset = llvm.Const.int64(self.ctx, @bitCast(@as(u64, 0xcbf29ce484222325)));
                return self.builder.buildXor(ext_bits, fnv_offset, "hash.float");
            },
            llvm.c.LLVMDoubleTypeKind => {
                // For doubles: bitcast directly to i64
                const bits = llvm.c.LLVMBuildBitCast(self.builder.ref, value, i64_type, "hash.f64bits");
                const fnv_offset = llvm.Const.int64(self.ctx, @bitCast(@as(u64, 0xcbf29ce484222325)));
                return self.builder.buildXor(bits, fnv_offset, "hash.double");
            },
            llvm.c.LLVMPointerTypeKind => {
                // This is likely a string (char*) - call string hash helper
                const hash_fn = self.getOrDeclareStringHash();
                var args = [_]llvm.ValueRef{value};
                return self.builder.buildCall(
                    llvm.c.LLVMGlobalGetValueType(hash_fn),
                    hash_fn,
                    &args,
                    "hash.str",
                );
            },
            llvm.c.LLVMStructTypeKind => {
                // For structs that implement Hash, we need to call the user-defined hash method
                // This should be handled by the user-defined method lookup before we get here
                // For now, return a constant (should not reach here if type checker is correct)
                return llvm.Const.int64(self.ctx, 0);
            },
            else => {
                // Unsupported type - return 0
                return llvm.Const.int64(self.ctx, 0);
            },
        }
    }

    /// Get or declare the klar_string_hash helper function for hashing strings.
    /// Uses FNV-1a algorithm: hash = ((hash ^ byte) * FNV_prime) for each byte
    fn getOrDeclareStringHash(self: *Emitter) llvm.ValueRef {
        const fn_name = "klar_string_hash";

        // Check if already declared
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |existing| {
            return existing;
        }

        // Declare: i64 klar_string_hash(i8* str)
        const i64_type = llvm.Types.int64(self.ctx);
        const i8_ptr_type = llvm.Types.pointer(self.ctx);
        var param_types = [_]llvm.TypeRef{i8_ptr_type};
        const fn_type = llvm.Types.function(i64_type, &param_types, false);
        const func = llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);

        // Create the function body implementing FNV-1a hash
        const entry = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "entry");
        const loop_block = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "loop");
        const loop_body = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "loop_body");
        const exit_block = llvm.c.LLVMAppendBasicBlockInContext(self.ctx.ref, func, "exit");

        // Save current builder state
        const saved_block = llvm.c.LLVMGetInsertBlock(self.builder.ref);

        const i8_type = llvm.Types.int8(self.ctx);
        const str_param = llvm.c.LLVMGetParam(func, 0);

        // Entry block: initialize hash with FNV offset basis, jump to loop
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, entry);
        _ = llvm.c.LLVMBuildBr(self.builder.ref, loop_block);

        // Loop block: PHI nodes for hash and pointer, then check for null terminator
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, loop_block);
        const hash_phi = llvm.c.LLVMBuildPhi(self.builder.ref, i64_type, "hash");
        const ptr_phi = llvm.c.LLVMBuildPhi(self.builder.ref, i8_ptr_type, "ptr");

        // Load current character
        const char_val = llvm.c.LLVMBuildLoad2(self.builder.ref, i8_type, ptr_phi, "char");

        // Check if null terminator
        const is_null = llvm.c.LLVMBuildICmp(self.builder.ref, llvm.c.LLVMIntEQ, char_val, llvm.Const.int(i8_type, 0, false), "is_null");
        _ = llvm.c.LLVMBuildCondBr(self.builder.ref, is_null, exit_block, loop_body);

        // Loop body: hash = (hash ^ byte) * FNV_prime
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, loop_body);
        const char_ext = llvm.c.LLVMBuildZExt(self.builder.ref, char_val, i64_type, "char.ext");
        const xored = llvm.c.LLVMBuildXor(self.builder.ref, hash_phi, char_ext, "xored");
        // FNV-1a prime: 1099511628211 (0x100000001b3)
        const fnv_prime = llvm.Const.int64(self.ctx, @bitCast(@as(u64, 0x100000001b3)));
        const new_hash = llvm.c.LLVMBuildMul(self.builder.ref, xored, fnv_prime, "new_hash");

        // Increment pointer
        var indices = [_]llvm.ValueRef{llvm.Const.int32(self.ctx, 1)};
        const next_ptr = llvm.c.LLVMBuildGEP2(self.builder.ref, i8_type, ptr_phi, &indices, 1, "next_ptr");
        _ = llvm.c.LLVMBuildBr(self.builder.ref, loop_block);

        // Set up PHI incoming values
        // hash_phi: entry -> FNV offset basis, loop_body -> new_hash
        const fnv_offset = llvm.Const.int64(self.ctx, @bitCast(@as(u64, 0xcbf29ce484222325)));
        var hash_values = [_]llvm.ValueRef{ fnv_offset, new_hash };
        var hash_blocks = [_]llvm.BasicBlockRef{ entry, loop_body };
        llvm.c.LLVMAddIncoming(hash_phi, &hash_values, &hash_blocks, 2);

        // ptr_phi: entry -> str_param, loop_body -> next_ptr
        var ptr_values = [_]llvm.ValueRef{ str_param, next_ptr };
        var ptr_blocks = [_]llvm.BasicBlockRef{ entry, loop_body };
        llvm.c.LLVMAddIncoming(ptr_phi, &ptr_values, &ptr_blocks, 2);

        // Exit block: return the hash
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, exit_block);
        _ = llvm.c.LLVMBuildRet(self.builder.ref, hash_phi);

        // Restore builder position
        llvm.c.LLVMPositionBuilderAtEnd(self.builder.ref, saved_block);

        return func;
    }

    /// Emit the Default trait's default() static method.
    /// Returns the default value for the given type name.
    /// Primitives have builtin defaults:
    /// - integers -> 0
    /// - floats -> 0.0
    /// - bool -> false
    /// - char -> '\0' (0)
    /// - string -> "" (empty string)
    fn emitDefaultMethod(self: *Emitter, type_name: []const u8) EmitError!llvm.ValueRef {
        // Handle primitive types
        if (std.mem.eql(u8, type_name, "i8")) {
            return llvm.Const.int(llvm.Types.int8(self.ctx), 0, true);
        }
        if (std.mem.eql(u8, type_name, "i16")) {
            return llvm.Const.int(llvm.Types.int16(self.ctx), 0, true);
        }
        if (std.mem.eql(u8, type_name, "i32")) {
            return llvm.Const.int32(self.ctx, 0);
        }
        if (std.mem.eql(u8, type_name, "i64")) {
            return llvm.Const.int64(self.ctx, 0);
        }
        if (std.mem.eql(u8, type_name, "i128")) {
            return llvm.Const.int(llvm.Types.int128(self.ctx), 0, true);
        }
        if (std.mem.eql(u8, type_name, "isize")) {
            return llvm.Const.int64(self.ctx, 0); // isize is i64 on 64-bit
        }
        if (std.mem.eql(u8, type_name, "u8")) {
            return llvm.Const.int(llvm.Types.int8(self.ctx), 0, false);
        }
        if (std.mem.eql(u8, type_name, "u16")) {
            return llvm.Const.int(llvm.Types.int16(self.ctx), 0, false);
        }
        if (std.mem.eql(u8, type_name, "u32")) {
            return llvm.Const.int(llvm.Types.int32(self.ctx), 0, false);
        }
        if (std.mem.eql(u8, type_name, "u64")) {
            return llvm.Const.int64(self.ctx, 0);
        }
        if (std.mem.eql(u8, type_name, "u128")) {
            return llvm.Const.int(llvm.Types.int128(self.ctx), 0, false);
        }
        if (std.mem.eql(u8, type_name, "usize")) {
            return llvm.Const.int64(self.ctx, 0); // usize is u64 on 64-bit
        }
        if (std.mem.eql(u8, type_name, "f32")) {
            return llvm.Const.float32(self.ctx, 0.0);
        }
        if (std.mem.eql(u8, type_name, "f64")) {
            return llvm.Const.float64(self.ctx, 0.0);
        }
        if (std.mem.eql(u8, type_name, "bool")) {
            return llvm.Const.int1(self.ctx, false);
        }
        if (std.mem.eql(u8, type_name, "char")) {
            // char is represented as i32 (Unicode code point), default is '\0'
            return llvm.Const.int32(self.ctx, 0);
        }
        if (std.mem.eql(u8, type_name, "string")) {
            // String default is empty string ""
            return self.builder.buildGlobalStringPtr("", "default_str");
        }

        // For struct types that implement Default, look up the user-defined default method
        if (self.type_checker) |tc| {
            if (tc.lookupStructMethod(type_name, "default")) |default_method| {
                // Get the function name for the static method
                var name_buf: [256]u8 = undefined;
                const fn_name = std.fmt.bufPrint(&name_buf, "{s}.default", .{type_name}) catch return EmitError.OutOfMemory;

                // Look up the function - it should have been emitted earlier
                if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name.ptr)) |func| {
                    // Call the static default() function with no arguments
                    var args = [_]llvm.ValueRef{};
                    return self.builder.buildCall(
                        llvm.c.LLVMGlobalGetValueType(func),
                        func,
                        &args,
                        "default",
                    );
                }

                // Function not found, try to emit it
                _ = default_method;
                // TODO: emit the user-defined default method if not already emitted
            }
        }

        // Unknown type - return 0 as fallback (type checker should have caught this)
        return llvm.Const.int32(self.ctx, 0);
    }

    /// Declare the C strdup function if not already declared.
    fn getOrDeclareStrdup(self: *Emitter) llvm.ValueRef {
        const fn_name = "strdup";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // char* strdup(const char *s)
        const ptr_type = llvm.Types.pointer(self.ctx);
        var param_types = [_]llvm.TypeRef{ptr_type};
        const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 1, 0); // not variadic
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
    }

    /// Declare the C strcmp function if not already declared.
    fn getOrDeclareStrcmp(self: *Emitter) llvm.ValueRef {
        const fn_name = "strcmp";
        if (llvm.c.LLVMGetNamedFunction(self.module.ref, fn_name)) |func| {
            return func;
        }

        // int strcmp(const char *s1, const char *s2)
        const ptr_type = llvm.Types.pointer(self.ctx);
        const i32_type = llvm.Types.int32(self.ctx);
        var param_types = [_]llvm.TypeRef{ ptr_type, ptr_type };
        const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 2, 0); // not variadic
        return llvm.c.LLVMAddFunction(self.module.ref, fn_name, fn_type);
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
            // Load the Rc/Arc pointer from the alloca
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

            // Call klar_rc_drop or klar_arc_drop depending on type
            const drop_fn = if (v.is_arc) self.getOrDeclareArcDrop() else self.getOrDeclareRcDrop();
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
    fn registerDroppable(self: *Emitter, name: []const u8, alloca: llvm.ValueRef, inner_type: llvm.TypeRef, is_rc: bool, is_arc: bool) EmitError!void {
        if (self.scope_stack.items.len == 0) return;

        const scope = &self.scope_stack.items[self.scope_stack.items.len - 1];
        scope.droppables.append(self.allocator, .{
            .name = name,
            .alloca = alloca,
            .inner_type = inner_type,
            .is_rc = is_rc,
            .is_arc = is_arc,
        }) catch return EmitError.OutOfMemory;
    }

    // =========================================================================
    // Type Conversion from Checker Types
    // =========================================================================

    /// Convert a checker Type to an LLVM type.
    /// Used for monomorphization when we have concrete types from type inference.
    fn typeToLLVM(self: *Emitter, ty: types.Type) llvm.TypeRef {
        return switch (ty) {
            .primitive => |prim| switch (prim) {
                .i8_, .u8_ => llvm.Types.int8(self.ctx),
                .i16_, .u16_ => llvm.Types.int16(self.ctx),
                .i32_, .u32_ => llvm.Types.int32(self.ctx),
                .i64_, .u64_, .isize_, .usize_ => llvm.Types.int64(self.ctx),
                .i128_, .u128_ => llvm.Types.int128(self.ctx),
                .f32_ => llvm.Types.float32(self.ctx),
                .f64_ => llvm.Types.float64(self.ctx),
                .bool_ => llvm.Types.int1(self.ctx),
                .char_ => llvm.Types.int32(self.ctx), // Unicode codepoint
                .string_ => llvm.Types.pointer(self.ctx),
            },
            .void_ => llvm.Types.void_(self.ctx),
            .never, .unknown, .error_type => llvm.Types.void_(self.ctx),
            .array => |arr| {
                const elem_ty = self.typeToLLVM(arr.element);
                return llvm.Types.array(elem_ty, @intCast(arr.size));
            },
            .slice => {
                // Slice is {ptr, len}
                var fields = [_]llvm.TypeRef{
                    llvm.Types.pointer(self.ctx),
                    llvm.Types.int64(self.ctx),
                };
                return llvm.Types.struct_(self.ctx, &fields, false);
            },
            .tuple => |tup| {
                var elem_types = std.ArrayListUnmanaged(llvm.TypeRef){};
                defer elem_types.deinit(self.allocator);
                for (tup.elements) |elem| {
                    elem_types.append(self.allocator, self.typeToLLVM(elem)) catch {
                        return llvm.Types.int32(self.ctx);
                    };
                }
                return llvm.Types.struct_(self.ctx, elem_types.items, false);
            },
            .optional => |opt| {
                // Optional is {tag: i1, value}, opt is *Type
                const inner_ty = self.typeToLLVM(opt.*);
                var fields = [_]llvm.TypeRef{
                    llvm.Types.int1(self.ctx),
                    inner_ty,
                };
                return llvm.Types.struct_(self.ctx, &fields, false);
            },
            .result => |res| {
                // Result is {tag: i1, ok_value, err_value}
                const ok_ty = self.typeToLLVM(res.ok_type);
                const err_ty = self.typeToLLVM(res.err_type);
                var fields = [_]llvm.TypeRef{
                    llvm.Types.int1(self.ctx),
                    ok_ty,
                    err_ty,
                };
                return llvm.Types.struct_(self.ctx, &fields, false);
            },
            .function => {
                // Function types use closure representation
                return self.getClosureStructType();
            },
            .reference => llvm.Types.pointer(self.ctx),
            .struct_ => |s| {
                // Look up struct type in struct_types cache
                if (self.struct_types.get(s.name)) |struct_info| {
                    return struct_info.llvm_type;
                }
                // Fallback to pointer if not found
                return llvm.Types.pointer(self.ctx);
            },
            .applied => |a| {
                // For applied types (e.g., Pair[i32]), construct mangled name and look up
                var name_buf = std.ArrayListUnmanaged(u8){};
                defer name_buf.deinit(self.allocator);

                // Get base name
                if (a.base == .struct_) {
                    name_buf.appendSlice(self.allocator, a.base.struct_.name) catch {
                        return llvm.Types.pointer(self.ctx);
                    };
                    // Append type args
                    for (a.args) |arg| {
                        name_buf.append(self.allocator, '$') catch {
                            return llvm.Types.pointer(self.ctx);
                        };
                        self.appendCheckerTypeNameForMangling(&name_buf, arg) catch {
                            return llvm.Types.pointer(self.ctx);
                        };
                    }

                    // Look up the mangled struct type
                    if (self.struct_types.get(name_buf.items)) |struct_info| {
                        return struct_info.llvm_type;
                    }
                }
                // Fallback to pointer if not found
                return llvm.Types.pointer(self.ctx);
            },
            .enum_, .trait_, .type_var => {
                // Complex types default to pointer
                return llvm.Types.pointer(self.ctx);
            },
            .rc, .arc, .weak_rc, .weak_arc => {
                // Reference-counted types are pointers
                return llvm.Types.pointer(self.ctx);
            },
            .cell => llvm.Types.pointer(self.ctx),
            .range => |r| {
                // Range LLVM layout: { start: T, end: T, current: T, inclusive: i1 }
                const elem_ty = self.typeToLLVM(r.element_type);
                var fields = [_]llvm.TypeRef{
                    elem_ty, // start
                    elem_ty, // end
                    elem_ty, // current
                    llvm.Types.int1(self.ctx), // inclusive
                };
                return llvm.Types.struct_(self.ctx, &fields, false);
            },
            .list => {
                // List LLVM layout: { ptr: *T, len: i32, capacity: i32 }
                var fields = [_]llvm.TypeRef{
                    llvm.Types.pointer(self.ctx), // ptr
                    llvm.Types.int32(self.ctx), // len
                    llvm.Types.int32(self.ctx), // capacity
                };
                return llvm.Types.struct_(self.ctx, &fields, false);
            },
            .string_data => {
                // String LLVM layout: { ptr: *u8, len: i32, capacity: i32 }
                // Same layout as List but fixed element type of u8
                var fields = [_]llvm.TypeRef{
                    llvm.Types.pointer(self.ctx), // ptr
                    llvm.Types.int32(self.ctx), // len
                    llvm.Types.int32(self.ctx), // capacity
                };
                return llvm.Types.struct_(self.ctx, &fields, false);
            },
            .associated_type_ref => {
                // Associated type refs should have been resolved during type checking.
                // If we get here, it's an unresolved associated type - use pointer as fallback.
                return llvm.Types.pointer(self.ctx);
            },
        };
    }

    /// Check if a checker Type is signed.
    fn isCheckerTypeSigned(_: *Emitter, ty: types.Type) bool {
        return switch (ty) {
            .primitive => |prim| switch (prim) {
                .u8_, .u16_, .u32_, .u64_, .u128_, .usize_ => false,
                else => true,
            },
            else => true, // Default to signed
        };
    }

    // =========================================================================
    // Monomorphized Struct Registration
    // =========================================================================

    /// Register all monomorphized struct types from the type checker.
    /// Must be called BEFORE emitModule so that struct literals can find the types.
    pub fn registerMonomorphizedStructs(self: *Emitter, type_checker: *const TypeChecker) EmitError!void {
        const monos = type_checker.getMonomorphizedStructs();

        for (monos) |mono| {
            try self.registerMonomorphizedStruct(mono);
        }
    }

    /// Register a single monomorphized struct type.
    fn registerMonomorphizedStruct(self: *Emitter, mono: TypeChecker.MonomorphizedStruct) EmitError!void {
        // Skip if already registered
        if (self.struct_types.contains(mono.mangled_name)) {
            return;
        }

        // Build LLVM field types from the concrete struct type
        var field_types = std.ArrayListUnmanaged(llvm.TypeRef){};
        defer field_types.deinit(self.allocator);

        const field_count = mono.concrete_type.fields.len;
        var field_indices = self.allocator.alloc(u32, field_count) catch return EmitError.OutOfMemory;
        errdefer self.allocator.free(field_indices);

        var field_names = self.allocator.alloc([]const u8, field_count) catch return EmitError.OutOfMemory;
        errdefer self.allocator.free(field_names);

        for (mono.concrete_type.fields, 0..) |field, i| {
            const field_llvm_type = self.typeToLLVM(field.type_);
            field_types.append(self.allocator, field_llvm_type) catch return EmitError.OutOfMemory;
            field_indices[i] = @intCast(i);
            field_names[i] = field.name;
        }

        // Create LLVM struct type
        const struct_type = llvm.Types.struct_(self.ctx, field_types.items, false);

        // Cache the struct type info
        self.struct_types.put(mono.mangled_name, .{
            .llvm_type = struct_type,
            .field_indices = field_indices,
            .field_names = field_names,
        }) catch return EmitError.OutOfMemory;
    }

    // =========================================================================
    // Monomorphized Enum Registration
    // =========================================================================

    /// Register all monomorphized enum types from the type checker.
    /// This tracks concrete enum instantiations for future codegen support.
    pub fn registerMonomorphizedEnums(self: *Emitter, type_checker: *const TypeChecker) EmitError!void {
        const monos = type_checker.getMonomorphizedEnums();

        for (monos) |mono| {
            try self.registerMonomorphizedEnum(mono);
        }
    }

    /// Register a single monomorphized enum type.
    /// Enums are represented as tagged unions: {tag: i8, payload_union}
    fn registerMonomorphizedEnum(self: *Emitter, mono: TypeChecker.MonomorphizedEnum) EmitError!void {
        // Skip if already registered (uses struct_types map since enum codegen
        // will reuse struct infrastructure for tagged unions)
        if (self.struct_types.contains(mono.mangled_name)) {
            return;
        }

        // Calculate the maximum payload size across all variants
        var max_payload_size: usize = 0;
        for (mono.concrete_type.variants) |variant| {
            if (variant.payload) |payload| {
                const payload_size = switch (payload) {
                    .tuple => |tuple_types| blk: {
                        var size: usize = 0;
                        for (tuple_types) |t| {
                            size += self.getTypeSize(t);
                        }
                        break :blk size;
                    },
                    .struct_ => |struct_fields| blk: {
                        var size: usize = 0;
                        for (struct_fields) |f| {
                            size += self.getTypeSize(f.type_);
                        }
                        break :blk size;
                    },
                };
                if (payload_size > max_payload_size) {
                    max_payload_size = payload_size;
                }
            }
        }

        // Create LLVM type: {tag: i8, payload: [max_size x i8]}
        // This is a simple tagged union representation
        const variant_count = mono.concrete_type.variants.len;
        const tag_type = if (variant_count <= 256) llvm.Types.int8(self.ctx) else llvm.Types.int16(self.ctx);

        var field_types = std.ArrayListUnmanaged(llvm.TypeRef){};
        defer field_types.deinit(self.allocator);

        // Tag field
        field_types.append(self.allocator, tag_type) catch return EmitError.OutOfMemory;

        // Payload field (if any variant has payload)
        if (max_payload_size > 0) {
            const payload_array = llvm.Types.array(llvm.Types.int8(self.ctx), @intCast(max_payload_size));
            field_types.append(self.allocator, payload_array) catch return EmitError.OutOfMemory;
        }

        // Create LLVM struct type for the enum
        const enum_type = llvm.Types.struct_(self.ctx, field_types.items, false);

        // Allocate field tracking data
        const field_count: usize = if (max_payload_size > 0) 2 else 1;
        const field_indices = self.allocator.alloc(u32, field_count) catch return EmitError.OutOfMemory;
        errdefer self.allocator.free(field_indices);

        const field_names = self.allocator.alloc([]const u8, field_count) catch return EmitError.OutOfMemory;
        errdefer self.allocator.free(field_names);

        field_indices[0] = 0;
        field_names[0] = "tag";
        if (max_payload_size > 0) {
            field_indices[1] = 1;
            field_names[1] = "payload";
        }

        // Cache the enum type info (reusing struct infrastructure)
        self.struct_types.put(mono.mangled_name, .{
            .llvm_type = enum_type,
            .field_indices = field_indices,
            .field_names = field_names,
        }) catch return EmitError.OutOfMemory;
    }

    /// Get the size in bytes for a type (simplified estimation for enum layout)
    fn getTypeSize(self: *Emitter, typ: types.Type) usize {
        return switch (typ) {
            .primitive => |p| switch (p) {
                .bool_ => 1,
                .i8_, .u8_, .char_ => 1,
                .i16_, .u16_ => 2,
                .i32_, .u32_, .f32_ => 4,
                .i64_, .u64_, .f64_, .isize_, .usize_ => 8,
                .i128_, .u128_ => 16,
                .string_ => 16, // Pointer + length
            },
            .void_ => 0,
            .optional => 16, // Conservative: has_value flag + payload pointer
            .struct_ => |s| blk: {
                var size: usize = 0;
                for (s.fields) |f| {
                    size += self.getTypeSize(f.type_);
                }
                break :blk if (size == 0) 8 else size;
            },
            else => 8, // Default pointer size for complex types
        };
    }

    // =========================================================================
    // Monomorphized Function Emission
    // =========================================================================

    /// Declare all monomorphized function signatures from the type checker.
    /// Must be called BEFORE emitModule so that call sites can find the functions.
    pub fn declareMonomorphizedFunctions(self: *Emitter, type_checker: *const TypeChecker) EmitError!void {
        const monos = type_checker.getMonomorphizedFunctions();

        for (monos) |mono| {
            // Skip if this function has no body (extern declaration)
            if (mono.original_decl.body == null) continue;

            // Declare the monomorphized function with its mangled name
            try self.declareMonomorphizedFunction(mono);
        }
    }

    /// Emit all monomorphized function bodies from the type checker.
    /// Must be called AFTER emitModule (or at least after declarations are done).
    pub fn emitMonomorphizedFunctions(self: *Emitter, type_checker: *const TypeChecker) EmitError!void {
        const monos = type_checker.getMonomorphizedFunctions();

        for (monos) |mono| {
            // Skip if this function has no body (extern declaration)
            if (mono.original_decl.body == null) continue;

            // Emit the function body
            try self.emitMonomorphizedFunction(mono);
        }
    }

    /// Declare a monomorphized function without emitting its body.
    fn declareMonomorphizedFunction(self: *Emitter, mono: TypeChecker.MonomorphizedFunction) EmitError!void {
        const func_type = mono.concrete_type.function;

        // Build parameter types from the concrete function type
        var param_types = std.ArrayListUnmanaged(llvm.TypeRef){};
        defer param_types.deinit(self.allocator);

        for (func_type.params) |param_ty| {
            const llvm_param_ty = self.typeToLLVM(param_ty);
            param_types.append(self.allocator, llvm_param_ty) catch return EmitError.OutOfMemory;
        }

        // Get return type
        const return_type = self.typeToLLVM(func_type.return_type);

        // Create LLVM function type
        const fn_type = llvm.Types.function(return_type, param_types.items, false);

        // Create function with mangled name
        const mangled_name = self.allocator.dupeZ(u8, mono.mangled_name) catch return EmitError.OutOfMemory;
        defer self.allocator.free(mangled_name);

        const llvm_func = llvm.addFunction(self.module, mangled_name, fn_type);

        // Set the calling convention
        llvm.setFunctionCallConv(llvm_func, self.calling_convention.toLLVM());
    }

    /// Emit a monomorphized function body.
    fn emitMonomorphizedFunction(self: *Emitter, mono: TypeChecker.MonomorphizedFunction) EmitError!void {
        const func = mono.original_decl;
        const func_type = mono.concrete_type.function;

        // Get the declared function
        const mangled_name = self.allocator.dupeZ(u8, mono.mangled_name) catch return EmitError.OutOfMemory;
        defer self.allocator.free(mangled_name);

        const function = self.module.getNamedFunction(mangled_name) orelse return EmitError.InvalidAST;
        self.current_function = function;

        // Set up return type info
        const return_llvm_type = self.typeToLLVM(func_type.return_type);
        const is_optional = func_type.return_type == .optional;
        self.current_return_type = .{
            .llvm_type = return_llvm_type,
            .is_optional = is_optional,
            .inner_type = if (is_optional) self.typeToLLVM(func_type.return_type.optional.*) else null,
        };
        defer self.current_return_type = null;

        // Create entry block
        const entry = llvm.appendBasicBlock(self.ctx, function, "entry");
        self.builder.positionAtEnd(entry);
        self.has_terminator = false;

        // Clear named values
        self.named_values.clearRetainingCapacity();

        // Add parameters to named values with concrete types
        for (func.params, 0..) |param, i| {
            const param_value = llvm.getParam(function, @intCast(i));
            const concrete_param_type = func_type.params[i];
            const param_ty = self.typeToLLVM(concrete_param_type);

            const param_name = self.allocator.dupeZ(u8, param.name) catch return EmitError.OutOfMemory;
            defer self.allocator.free(param_name);

            const alloca = self.builder.buildAlloca(param_ty, param_name);
            _ = self.builder.buildStore(param_value, alloca);

            // Check if this is a reference parameter
            const is_ref = concrete_param_type == .reference;
            const ref_inner_type: ?llvm.TypeRef = if (is_ref) blk: {
                break :blk self.typeToLLVM(concrete_param_type.reference.inner);
            } else null;

            // Get struct name for reference types
            const param_struct_name: ?[]const u8 = if (is_ref) blk: {
                const inner = concrete_param_type.reference.inner;
                if (inner == .struct_) {
                    break :blk inner.struct_.name;
                }
                break :blk null;
            } else null;

            const is_signed = self.isCheckerTypeSigned(concrete_param_type);
            self.named_values.put(param.name, .{
                .value = alloca,
                .is_alloca = true,
                .ty = param_ty,
                .is_signed = is_signed,
                .struct_type_name = param_struct_name,
                .is_reference = is_ref,
                .reference_inner_type = ref_inner_type,
            }) catch return EmitError.OutOfMemory;
        }

        // Emit function body
        if (func.body) |body| {
            try self.pushScope(false);

            const result = try self.emitBlock(body);

            // Handle return
            if (!self.has_terminator) {
                if (func.return_type == null) {
                    self.emitDropsForReturn();
                    _ = self.builder.buildRetVoid();
                } else if (result) |val| {
                    if (self.current_return_type) |rt_info| {
                        if (rt_info.is_optional) {
                            self.emitDropsForReturn();
                            const wrapped = self.emitSome(val, rt_info.inner_type.?);
                            _ = self.builder.buildRet(wrapped);
                        } else {
                            self.emitDropsForReturn();
                            _ = self.builder.buildRet(val);
                        }
                    } else {
                        self.emitDropsForReturn();
                        _ = self.builder.buildRet(val);
                    }
                } else {
                    self.emitDropsForReturn();
                    _ = self.builder.buildRetVoid();
                }
            }

            self.popScope();
        }

        self.current_function = null;
    }

    /// Declare all monomorphized methods from the type checker.
    /// Must be called BEFORE emitting method bodies.
    pub fn declareMonomorphizedMethods(self: *Emitter, type_checker: *const TypeChecker) EmitError!void {
        const monos = type_checker.monomorphized_methods.items;

        for (monos) |mono| {
            // Skip if this method has no body
            if (mono.original_decl.body == null) continue;

            try self.declareMonomorphizedMethod(mono);
        }
    }

    /// Emit all monomorphized method bodies from the type checker.
    /// Must be called AFTER declareMonomorphizedMethods.
    pub fn emitMonomorphizedMethods(self: *Emitter, type_checker: *const TypeChecker) EmitError!void {
        const monos = type_checker.monomorphized_methods.items;

        for (monos) |mono| {
            // Skip if this method has no body
            if (mono.original_decl.body == null) continue;

            try self.emitMonomorphizedMethod(mono);
        }
    }

    /// Declare a monomorphized method without emitting its body.
    fn declareMonomorphizedMethod(self: *Emitter, mono: TypeChecker.MonomorphizedMethod) EmitError!void {
        const func_type = mono.concrete_type.function;

        // Build parameter types from the concrete function type
        var param_types = std.ArrayListUnmanaged(llvm.TypeRef){};
        defer param_types.deinit(self.allocator);

        for (func_type.params) |param_ty| {
            const llvm_param_ty = self.typeToLLVM(param_ty);
            param_types.append(self.allocator, llvm_param_ty) catch return EmitError.OutOfMemory;
        }

        // Get return type
        const return_type = self.typeToLLVM(func_type.return_type);

        // Create LLVM function type
        const fn_type = llvm.Types.function(return_type, param_types.items, false);

        // Create function with mangled name
        const mangled_name = self.allocator.dupeZ(u8, mono.mangled_name) catch return EmitError.OutOfMemory;
        defer self.allocator.free(mangled_name);

        const llvm_func = llvm.addFunction(self.module, mangled_name, fn_type);

        // Set the calling convention
        llvm.setFunctionCallConv(llvm_func, self.calling_convention.toLLVM());
    }

    /// Emit a monomorphized method body.
    fn emitMonomorphizedMethod(self: *Emitter, mono: TypeChecker.MonomorphizedMethod) EmitError!void {
        const func = mono.original_decl;
        const func_type = mono.concrete_type.function;

        // Get the declared function
        const mangled_name = self.allocator.dupeZ(u8, mono.mangled_name) catch return EmitError.OutOfMemory;
        defer self.allocator.free(mangled_name);

        const function = self.module.getNamedFunction(mangled_name) orelse {
            return EmitError.InvalidAST;
        };
        self.current_function = function;

        // Set up return type info
        const return_llvm_type = self.typeToLLVM(func_type.return_type);
        const is_optional = func_type.return_type == .optional;
        self.current_return_type = .{
            .llvm_type = return_llvm_type,
            .is_optional = is_optional,
            .inner_type = if (is_optional) self.typeToLLVM(func_type.return_type.optional.*) else null,
        };
        defer self.current_return_type = null;

        // Create entry block
        const entry = llvm.appendBasicBlock(self.ctx, function, "entry");
        self.builder.positionAtEnd(entry);
        self.has_terminator = false;

        // Clear named values
        self.named_values.clearRetainingCapacity();

        // Construct the mangled struct name for field resolution (e.g., "Pair$i32")
        var struct_name_buf = std.ArrayListUnmanaged(u8){};
        defer struct_name_buf.deinit(self.allocator);
        struct_name_buf.appendSlice(self.allocator, mono.struct_name) catch return EmitError.OutOfMemory;
        for (mono.type_args) |type_arg| {
            struct_name_buf.append(self.allocator, '$') catch return EmitError.OutOfMemory;
            self.appendCheckerTypeNameForMangling(&struct_name_buf, type_arg) catch return EmitError.OutOfMemory;
        }
        const mangled_struct_name = self.allocator.dupe(u8, struct_name_buf.items) catch return EmitError.OutOfMemory;

        // Add parameters to named values with concrete types
        for (func.params, 0..) |param, i| {
            const param_value = llvm.getParam(function, @intCast(i));
            const concrete_param_type = func_type.params[i];
            const param_ty = self.typeToLLVM(concrete_param_type);

            const param_name = self.allocator.dupeZ(u8, param.name) catch return EmitError.OutOfMemory;
            defer self.allocator.free(param_name);

            const alloca = self.builder.buildAlloca(param_ty, param_name);
            _ = self.builder.buildStore(param_value, alloca);

            // Check if this is a reference parameter
            const is_ref = concrete_param_type == .reference;
            const ref_inner_type: ?llvm.TypeRef = if (is_ref) blk: {
                break :blk self.typeToLLVM(concrete_param_type.reference.inner);
            } else null;

            // Check if this is a struct parameter (like 'self') that needs struct_type_name
            // For reference parameters, use the mangled struct name for 'self'
            const param_struct_name: ?[]const u8 = blk: {
                // If this is the first param named "self", use the mangled struct name
                // This works for both `self: Self` and `self: &Self` or `self: &mut Self`
                if (std.mem.eql(u8, param.name, "self")) {
                    break :blk mangled_struct_name;
                }
                break :blk null;
            };

            const is_signed = self.isCheckerTypeSigned(concrete_param_type);
            self.named_values.put(param.name, .{
                .value = alloca,
                .is_alloca = true,
                .ty = param_ty,
                .is_signed = is_signed,
                .struct_type_name = param_struct_name,
                .is_reference = is_ref,
                .reference_inner_type = ref_inner_type,
            }) catch return EmitError.OutOfMemory;
        }

        // Emit function body
        if (func.body) |body| {
            try self.pushScope(false);

            const result = try self.emitBlock(body);

            // Handle return
            if (!self.has_terminator) {
                if (func.return_type == null) {
                    self.emitDropsForReturn();
                    _ = self.builder.buildRetVoid();
                } else if (result) |val| {
                    if (self.current_return_type) |rt_info| {
                        if (rt_info.is_optional) {
                            self.emitDropsForReturn();
                            const wrapped = self.emitSome(val, rt_info.inner_type.?);
                            _ = self.builder.buildRet(wrapped);
                        } else {
                            self.emitDropsForReturn();
                            _ = self.builder.buildRet(val);
                        }
                    } else {
                        self.emitDropsForReturn();
                        _ = self.builder.buildRet(val);
                    }
                } else {
                    self.emitDropsForReturn();
                    _ = self.builder.buildRetVoid();
                }
            }

            self.popScope();
        }

        self.current_function = null;
    }
};
