const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("ast.zig");
const types = @import("types.zig");
const Type = types.Type;
const Primitive = types.Primitive;
const TypeBuilder = types.TypeBuilder;
const Span = ast.Span;

// ============================================================================
// Type Checker Errors
// ============================================================================

pub const CheckError = struct {
    kind: Kind,
    span: Span,
    message: []const u8,

    pub const Kind = enum {
        type_mismatch,
        undefined_variable,
        undefined_type,
        undefined_field,
        undefined_method,
        undefined_variant,
        invalid_operation,
        invalid_assignment,
        invalid_call,
        invalid_index,
        not_iterable,
        break_outside_loop,
        continue_outside_loop,
        return_type_mismatch,
        missing_return,
        duplicate_definition,
        invalid_pattern,
        exhaustiveness,
        trait_not_implemented,
        invalid_conversion,
        mutability_error,
    };

    pub fn format(self: CheckError, allocator: Allocator) ![]u8 {
        return std.fmt.allocPrint(allocator, "{s} at {d}:{d}: {s}", .{
            @tagName(self.kind),
            self.span.line,
            self.span.column,
            self.message,
        });
    }
};

// ============================================================================
// Symbol Table
// ============================================================================

pub const Symbol = struct {
    name: []const u8,
    type_: Type,
    kind: Kind,
    mutable: bool,
    span: Span,

    pub const Kind = enum {
        variable,
        parameter,
        function,
        type_,
        trait_,
        module,
        constant,
    };
};

pub const Scope = struct {
    symbols: std.StringHashMapUnmanaged(Symbol),
    parent: ?*Scope,
    kind: Kind,
    allocator: Allocator,

    pub const Kind = enum {
        global,
        function,
        block,
        loop,
    };

    pub fn init(allocator: Allocator, parent: ?*Scope, kind: Kind) Scope {
        return .{
            .symbols = .{},
            .parent = parent,
            .kind = kind,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Scope) void {
        self.symbols.deinit(self.allocator);
    }

    pub fn define(self: *Scope, symbol: Symbol) !void {
        try self.symbols.put(self.allocator, symbol.name, symbol);
    }

    pub fn lookup(self: *const Scope, name: []const u8) ?Symbol {
        if (self.symbols.get(name)) |sym| {
            return sym;
        }
        if (self.parent) |p| {
            return p.lookup(name);
        }
        return null;
    }

    pub fn lookupLocal(self: *const Scope, name: []const u8) ?Symbol {
        return self.symbols.get(name);
    }

    pub fn isInLoop(self: *const Scope) bool {
        if (self.kind == .loop) return true;
        if (self.parent) |p| {
            return p.isInLoop();
        }
        return false;
    }

    pub fn getFunctionScope(self: *const Scope) ?*const Scope {
        if (self.kind == .function) return self;
        if (self.parent) |p| {
            return p.getFunctionScope();
        }
        return null;
    }
};

// ============================================================================
// Type Environment - manages type variables and substitutions
// ============================================================================

pub const TypeEnv = struct {
    substitutions: std.AutoHashMap(u32, Type),
    allocator: Allocator,

    pub fn init(allocator: Allocator) TypeEnv {
        return .{
            .substitutions = std.AutoHashMap(u32, Type).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *TypeEnv) void {
        self.substitutions.deinit();
    }

    pub fn bind(self: *TypeEnv, type_var_id: u32, t: Type) !void {
        try self.substitutions.put(type_var_id, t);
    }

    pub fn resolve(self: *const TypeEnv, t: Type) Type {
        switch (t) {
            .type_var => |tv| {
                if (self.substitutions.get(tv.id)) |resolved| {
                    return self.resolve(resolved);
                }
                return t;
            },
            else => return t,
        }
    }
};

// ============================================================================
// Type Checker
// ============================================================================

pub const TypeChecker = struct {
    allocator: Allocator,
    type_builder: TypeBuilder,
    type_env: TypeEnv,
    errors: std.ArrayListUnmanaged(CheckError),
    global_scope: *Scope,
    current_scope: *Scope,
    current_return_type: ?Type,
    /// Scope of the current closure being analyzed (for capture tracking).
    /// Null when not inside a closure.
    closure_scope: ?*Scope,
    /// Captured variables for the current closure being analyzed.
    closure_captures: std.StringHashMapUnmanaged(CaptureInfo),
    /// Stack of type parameter scopes for generic functions/structs.
    /// Each scope maps type parameter names to their TypeVar types.
    type_param_scopes: std.ArrayListUnmanaged(std.StringHashMapUnmanaged(types.TypeVar)),
    /// Cache of monomorphized function instances.
    /// Maps (function_name, type_args hash) to MonomorphizedFunction info.
    monomorphized_functions: std.ArrayListUnmanaged(MonomorphizedFunction),
    /// Storage for generic function declarations (needed for codegen monomorphization).
    generic_functions: std.StringHashMapUnmanaged(*ast.FunctionDecl),
    /// Maps call expression pointers to resolved mangled function names.
    /// Used by codegen to emit calls to monomorphized functions.
    call_resolutions: std.AutoHashMapUnmanaged(*ast.Call, []const u8),
    /// Cache of monomorphized struct instances.
    /// Maps (struct_name, type_args) to MonomorphizedStruct info.
    monomorphized_structs: std.ArrayListUnmanaged(MonomorphizedStruct),
    /// Cache of monomorphized enum instances.
    /// Maps (enum_name, type_args) to MonomorphizedEnum info.
    monomorphized_enums: std.ArrayListUnmanaged(MonomorphizedEnum),
    /// Track generic enum definitions for cleanup (created in checkEnum).
    generic_enum_types: std.ArrayListUnmanaged(*types.EnumType),
    /// Track generic struct definitions for cleanup (created in checkStruct).
    generic_struct_types: std.ArrayListUnmanaged(*types.StructType),
    /// Registry of methods defined in impl blocks.
    /// Maps struct name -> list of methods.
    struct_methods: std.StringHashMapUnmanaged(std.ArrayListUnmanaged(StructMethod)),
    /// Cache of monomorphized method instances for generic structs.
    /// Used by codegen to emit monomorphized methods.
    monomorphized_methods: std.ArrayListUnmanaged(MonomorphizedMethod),
    /// Registry of trait definitions.
    /// Maps trait name -> TraitInfo.
    trait_registry: std.StringHashMapUnmanaged(TraitInfo),
    /// Track trait type allocations for cleanup.
    trait_types: std.ArrayListUnmanaged(*types.TraitType),
    /// Registry of trait implementations.
    /// Maps (struct_name, trait_name) -> TraitImplInfo.
    trait_impls: std.StringHashMapUnmanaged(std.ArrayListUnmanaged(TraitImplInfo)),
    /// Track type var slices allocated by pushTypeParams for cleanup.
    type_var_slices: std.ArrayListUnmanaged([]const types.TypeVar),
    /// Track Type slices allocated by substituteTypeParams for cleanup.
    substituted_type_slices: std.ArrayListUnmanaged([]const Type),

    const CaptureInfo = struct {
        is_mutable: bool,
    };

    /// Information about a trait definition.
    pub const TraitInfo = struct {
        /// The trait type containing method signatures
        trait_type: *types.TraitType,
        /// The original AST declaration
        decl: *ast.TraitDecl,
    };

    /// Information about a trait implementation for a type.
    pub const TraitImplInfo = struct {
        /// Name of the trait being implemented
        trait_name: []const u8,
        /// Name of the implementing type
        impl_type_name: []const u8,
        /// Type parameters from the impl block (for generic impls)
        impl_type_params: []const types.TypeVar,
        /// The methods implementing the trait
        methods: []const StructMethod,
    };

    /// Information about a method defined in an impl block.
    pub const StructMethod = struct {
        /// Name of the method
        name: []const u8,
        /// The function declaration from the AST
        decl: *ast.FunctionDecl,
        /// Type parameters from the impl block (for generic impls)
        impl_type_params: []const types.TypeVar,
        /// The resolved function type (with type vars if generic)
        func_type: Type,
        /// Whether the first parameter is 'self'
        has_self: bool,
        /// Whether self is mutable (&mut self vs &self)
        self_is_mutable: bool,
    };

    /// Information about a monomorphized method instance.
    pub const MonomorphizedMethod = struct {
        /// Original struct name (e.g., "Pair")
        struct_name: []const u8,
        /// Original method name (e.g., "get_first")
        method_name: []const u8,
        /// Mangled name for this instantiation (e.g., "Pair$i32$string_get_first")
        mangled_name: []const u8,
        /// The concrete type arguments used for this instantiation
        type_args: []const Type,
        /// The concrete function type after substitution
        concrete_type: Type,
        /// Reference to the original method declaration (for codegen)
        original_decl: *ast.FunctionDecl,
    };

    /// Information about a monomorphized function instance.
    pub const MonomorphizedFunction = struct {
        /// Original generic function name
        original_name: []const u8,
        /// Mangled name for this instantiation (e.g., "identity$i32")
        mangled_name: []const u8,
        /// The concrete type arguments used for this instantiation
        type_args: []const Type,
        /// The concrete function type after substitution
        concrete_type: Type,
        /// Reference to the original generic function declaration (for codegen)
        original_decl: *ast.FunctionDecl,
    };

    /// Information about a monomorphized struct instance.
    pub const MonomorphizedStruct = struct {
        /// Original generic struct name (e.g., "Pair")
        original_name: []const u8,
        /// Mangled name for this instantiation (e.g., "Pair$i32$string")
        mangled_name: []const u8,
        /// The concrete type arguments used for this instantiation
        type_args: []const Type,
        /// The concrete struct type with substituted field types
        concrete_type: *types.StructType,
    };

    /// Information about a monomorphized enum instance.
    pub const MonomorphizedEnum = struct {
        /// Original generic enum name (e.g., "Option")
        original_name: []const u8,
        /// Mangled name for this instantiation (e.g., "Option$i32")
        mangled_name: []const u8,
        /// The concrete type arguments used for this instantiation
        type_args: []const Type,
        /// The concrete enum type with substituted variant payloads
        concrete_type: *types.EnumType,
    };

    pub fn init(allocator: Allocator) TypeChecker {
        // Allocate global scope on the heap so pointer remains stable
        const global_scope = allocator.create(Scope) catch @panic("Failed to allocate global scope");
        global_scope.* = Scope.init(allocator, null, .global);

        var checker = TypeChecker{
            .allocator = allocator,
            .type_builder = TypeBuilder.init(allocator),
            .type_env = TypeEnv.init(allocator),
            .errors = .{},
            .global_scope = global_scope,
            .current_scope = global_scope,
            .current_return_type = null,
            .closure_scope = null,
            .closure_captures = .{},
            .type_param_scopes = .{},
            .monomorphized_functions = .{},
            .generic_functions = .{},
            .call_resolutions = .{},
            .monomorphized_structs = .{},
            .monomorphized_enums = .{},
            .generic_enum_types = .{},
            .generic_struct_types = .{},
            .struct_methods = .{},
            .monomorphized_methods = .{},
            .trait_registry = .{},
            .trait_types = .{},
            .trait_impls = .{},
            .type_var_slices = .{},
            .substituted_type_slices = .{},
        };
        checker.initBuiltins() catch {};
        return checker;
    }

    pub fn deinit(self: *TypeChecker) void {
        self.type_builder.deinit();
        self.type_env.deinit();
        // Free error messages before freeing the errors list
        for (self.errors.items) |err| {
            // Only free if it was actually allocated (not the fallback literal)
            if (err.message.len > 0 and !std.mem.eql(u8, err.message, "error formatting message")) {
                self.allocator.free(err.message);
            }
        }
        self.errors.deinit(self.allocator);
        self.global_scope.deinit();
        self.allocator.destroy(self.global_scope);
        self.closure_captures.deinit(self.allocator);
        // Clean up type parameter scopes
        for (self.type_param_scopes.items) |*scope| {
            scope.deinit(self.allocator);
        }
        self.type_param_scopes.deinit(self.allocator);

        // Clean up monomorphized functions
        for (self.monomorphized_functions.items) |func| {
            self.allocator.free(func.mangled_name);
            self.allocator.free(func.type_args);
        }
        self.monomorphized_functions.deinit(self.allocator);

        self.generic_functions.deinit(self.allocator);
        self.call_resolutions.deinit(self.allocator);

        // Clean up monomorphized structs
        for (self.monomorphized_structs.items) |s| {
            self.allocator.free(s.mangled_name);
            self.allocator.free(s.type_args);
            self.allocator.free(s.concrete_type.fields);
            self.allocator.destroy(s.concrete_type);
        }
        self.monomorphized_structs.deinit(self.allocator);

        // Clean up monomorphized enums
        for (self.monomorphized_enums.items) |e| {
            self.allocator.free(e.mangled_name);
            self.allocator.free(e.type_args);
            // Free variant payloads
            for (e.concrete_type.variants) |variant| {
                if (variant.payload) |payload| {
                    switch (payload) {
                        .tuple => |tuple_types| self.allocator.free(tuple_types),
                        .struct_ => |struct_fields| self.allocator.free(struct_fields),
                    }
                }
            }
            self.allocator.free(e.concrete_type.variants);
            self.allocator.destroy(e.concrete_type);
        }
        self.monomorphized_enums.deinit(self.allocator);

        // Clean up generic enum definitions (from checkEnum)
        for (self.generic_enum_types.items) |enum_type| {
            // Free type params if any
            if (enum_type.type_params.len > 0) {
                self.allocator.free(enum_type.type_params);
            }
            // Free variant payloads
            for (enum_type.variants) |variant| {
                if (variant.payload) |payload| {
                    switch (payload) {
                        .tuple => |tuple_types| self.allocator.free(tuple_types),
                        .struct_ => |struct_fields| self.allocator.free(struct_fields),
                    }
                }
            }
            self.allocator.free(enum_type.variants);
            self.allocator.destroy(enum_type);
        }
        self.generic_enum_types.deinit(self.allocator);

        // Clean up generic struct definitions (from checkStruct)
        for (self.generic_struct_types.items) |struct_type| {
            if (struct_type.type_params.len > 0) {
                self.allocator.free(struct_type.type_params);
            }
            self.allocator.free(struct_type.fields);
            self.allocator.destroy(struct_type);
        }
        self.generic_struct_types.deinit(self.allocator);

        // Clean up struct methods registry
        var methods_iter = self.struct_methods.valueIterator();
        while (methods_iter.next()) |methods_list| {
            methods_list.deinit(self.allocator);
        }
        self.struct_methods.deinit(self.allocator);

        // Clean up monomorphized methods
        for (self.monomorphized_methods.items) |method| {
            self.allocator.free(method.mangled_name);
            self.allocator.free(method.type_args);
        }
        self.monomorphized_methods.deinit(self.allocator);

        // Clean up trait registry
        self.trait_registry.deinit(self.allocator);

        // Clean up trait type allocations
        for (self.trait_types.items) |trait_type| {
            if (trait_type.type_params.len > 0) {
                self.allocator.free(trait_type.type_params);
            }
            self.allocator.free(trait_type.methods);
            self.allocator.destroy(trait_type);
        }
        self.trait_types.deinit(self.allocator);

        // Clean up trait implementations (including allocated keys)
        var impls_key_iter = self.trait_impls.keyIterator();
        while (impls_key_iter.next()) |key| {
            // Keys are allocated strings, free them
            if (key.*.len > 0) {
                self.allocator.free(key.*);
            }
        }
        var impls_iter = self.trait_impls.valueIterator();
        while (impls_iter.next()) |impl_list| {
            impl_list.deinit(self.allocator);
        }
        self.trait_impls.deinit(self.allocator);

        // Clean up type var slices from pushTypeParams
        for (self.type_var_slices.items) |slice| {
            self.allocator.free(slice);
        }
        self.type_var_slices.deinit(self.allocator);

        // Clean up type slices from substituteTypeParams
        for (self.substituted_type_slices.items) |slice| {
            self.allocator.free(slice);
        }
        self.substituted_type_slices.deinit(self.allocator);
    }

    fn initBuiltins(self: *TypeChecker) !void {
        // Register built-in types
        const builtins = [_]struct { name: []const u8, prim: Primitive }{
            .{ .name = "i8", .prim = .i8_ },
            .{ .name = "i16", .prim = .i16_ },
            .{ .name = "i32", .prim = .i32_ },
            .{ .name = "i64", .prim = .i64_ },
            .{ .name = "i128", .prim = .i128_ },
            .{ .name = "isize", .prim = .isize_ },
            .{ .name = "u8", .prim = .u8_ },
            .{ .name = "u16", .prim = .u16_ },
            .{ .name = "u32", .prim = .u32_ },
            .{ .name = "u64", .prim = .u64_ },
            .{ .name = "u128", .prim = .u128_ },
            .{ .name = "usize", .prim = .usize_ },
            .{ .name = "f32", .prim = .f32_ },
            .{ .name = "f64", .prim = .f64_ },
            .{ .name = "bool", .prim = .bool_ },
            .{ .name = "char", .prim = .char_ },
            .{ .name = "string", .prim = .string_ },
        };

        for (builtins) |b| {
            try self.current_scope.define(.{
                .name = b.name,
                .type_ = .{ .primitive = b.prim },
                .kind = .type_,
                .mutable = false,
                .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
            });
        }

        // Register built-in functions
        const print_type = try self.type_builder.functionType(&.{self.type_builder.stringType()}, self.type_builder.voidType());
        try self.current_scope.define(.{
            .name = "print",
            .type_ = print_type,
            .kind = .function,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        const println_type = try self.type_builder.functionType(&.{self.type_builder.stringType()}, self.type_builder.voidType());
        try self.current_scope.define(.{
            .name = "println",
            .type_ = println_type,
            .kind = .function,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        const assert_type = try self.type_builder.functionType(&.{self.type_builder.boolType()}, self.type_builder.voidType());
        try self.current_scope.define(.{
            .name = "assert",
            .type_ = assert_type,
            .kind = .function,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // panic(message: string) -> ! (noreturn)
        const panic_type = try self.type_builder.functionType(&.{self.type_builder.stringType()}, self.type_builder.voidType());
        try self.current_scope.define(.{
            .name = "panic",
            .type_ = panic_type,
            .kind = .function,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // assert_eq(left: T, right: T) -> void - compares two values, panics with details on failure
        const assert_eq_type = try self.type_builder.functionType(&.{ self.type_builder.i32Type(), self.type_builder.i32Type() }, self.type_builder.voidType());
        try self.current_scope.define(.{
            .name = "assert_eq",
            .type_ = assert_eq_type,
            .kind = .function,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // dbg(value: T) -> T - prints value with debug info and returns it
        const dbg_type = try self.type_builder.functionType(&.{self.type_builder.i32Type()}, self.type_builder.i32Type());
        try self.current_scope.define(.{
            .name = "dbg",
            .type_ = dbg_type,
            .kind = .function,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // type_name(value: T) -> string - returns the type name as a string
        const type_name_type = try self.type_builder.functionType(&.{self.type_builder.i32Type()}, self.type_builder.stringType());
        try self.current_scope.define(.{
            .name = "type_name",
            .type_ = type_name_type,
            .kind = .function,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // len(value: T) -> i32 - returns the length of a string or array
        const len_type = try self.type_builder.functionType(&.{self.type_builder.stringType()}, self.type_builder.i32Type());
        try self.current_scope.define(.{
            .name = "len",
            .type_ = len_type,
            .kind = .function,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // Ok(value: T) -> Result[T, E] - Result constructor for success values
        // For now, use i32 -> Result[i32, i32] as a simple type
        // Full generic support would infer T from the argument
        const ok_type = try self.type_builder.functionType(
            &.{self.type_builder.i32Type()},
            try self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.i32Type()),
        );
        try self.current_scope.define(.{
            .name = "Ok",
            .type_ = ok_type,
            .kind = .function,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // Err(error: E) -> Result[T, E] - Result constructor for error values
        // For now, use i32 -> Result[i32, i32] as a simple type
        const err_type = try self.type_builder.functionType(
            &.{self.type_builder.i32Type()},
            try self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.i32Type()),
        );
        try self.current_scope.define(.{
            .name = "Err",
            .type_ = err_type,
            .kind = .function,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });
    }

    // ========================================================================
    // Error Reporting
    // ========================================================================

    fn addError(self: *TypeChecker, kind: CheckError.Kind, span: Span, comptime fmt: []const u8, args: anytype) void {
        const message = std.fmt.allocPrint(self.allocator, fmt, args) catch "error formatting message";
        self.errors.append(self.allocator, .{
            .kind = kind,
            .span = span,
            .message = message,
        }) catch {};
    }

    pub fn hasErrors(self: *const TypeChecker) bool {
        return self.errors.items.len > 0;
    }

    // ========================================================================
    // Scope Management
    // ========================================================================

    fn pushScope(self: *TypeChecker, kind: Scope.Kind) !*Scope {
        const scope = try self.allocator.create(Scope);
        scope.* = Scope.init(self.allocator, self.current_scope, kind);
        self.current_scope = scope;
        return scope;
    }

    fn popScope(self: *TypeChecker) void {
        if (self.current_scope.parent) |parent| {
            const old = self.current_scope;
            self.current_scope = parent;
            old.deinit();
            self.allocator.destroy(old);
        }
    }

    // ========================================================================
    // Type Parameter Scope Management
    // ========================================================================

    /// Push a new type parameter scope and register the given type parameters.
    /// Returns the TypeVar types for each parameter, in order.
    fn pushTypeParams(self: *TypeChecker, type_params: []const ast.TypeParam) ![]const types.TypeVar {
        var scope = std.StringHashMapUnmanaged(types.TypeVar){};
        var type_vars = std.ArrayListUnmanaged(types.TypeVar){};
        defer type_vars.deinit(self.allocator);

        for (type_params) |param| {
            // Resolve trait bounds
            var bounds = std.ArrayListUnmanaged(*types.TraitType){};
            defer bounds.deinit(self.allocator);

            for (param.bounds) |bound_expr| {
                const resolved = self.resolveTypeExpr(bound_expr) catch continue;
                if (resolved == .trait_) {
                    bounds.append(self.allocator, resolved.trait_) catch {};
                } else {
                    self.addError(.type_mismatch, param.span, "expected trait in bound, found '{s}'", .{@tagName(resolved)});
                }
            }

            const type_var = if (bounds.items.len > 0)
                try self.type_builder.newTypeVarWithBounds(param.name, bounds.items)
            else
                try self.type_builder.newTypeVar(param.name);

            try scope.put(self.allocator, param.name, type_var);
            try type_vars.append(self.allocator, type_var);
        }

        try self.type_param_scopes.append(self.allocator, scope);
        const result = try self.allocator.dupe(types.TypeVar, type_vars.items);
        // Track allocation for cleanup in deinit
        try self.type_var_slices.append(self.allocator, result);
        return result;
    }

    /// Pop the current type parameter scope.
    fn popTypeParams(self: *TypeChecker) void {
        if (self.type_param_scopes.pop()) |*scope| {
            var s = scope.*;
            s.deinit(self.allocator);
        }
    }

    /// Look up a type parameter by name across all active scopes (innermost first).
    fn lookupTypeParam(self: *const TypeChecker, name: []const u8) ?types.TypeVar {
        // Search from innermost scope to outermost
        var i: usize = self.type_param_scopes.items.len;
        while (i > 0) {
            i -= 1;
            if (self.type_param_scopes.items[i].get(name)) |type_var| {
                return type_var;
            }
        }
        return null;
    }

    /// Generate a mangled name for a monomorphized function.
    /// E.g., "identity" with [i32] becomes "identity$i32"
    fn mangleFunctionName(self: *TypeChecker, base_name: []const u8, type_args: []const Type) ![]const u8 {
        var buffer = std.ArrayListUnmanaged(u8){};
        defer buffer.deinit(self.allocator);

        try buffer.appendSlice(self.allocator, base_name);
        try buffer.append(self.allocator, '$');

        for (type_args, 0..) |arg, i| {
            if (i > 0) try buffer.append(self.allocator, '_');
            try self.appendTypeName(&buffer, arg);
        }

        return try self.allocator.dupe(u8, buffer.items);
    }

    /// Append a type's name to a buffer for mangling.
    fn appendTypeName(self: *TypeChecker, buffer: *std.ArrayListUnmanaged(u8), typ: Type) !void {
        switch (typ) {
            .primitive => |p| {
                const name = switch (p) {
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
                try buffer.appendSlice(self.allocator, name);
            },
            .void_ => try buffer.appendSlice(self.allocator, "void"),
            .struct_ => |s| try buffer.appendSlice(self.allocator, s.name),
            .enum_ => |e| try buffer.appendSlice(self.allocator, e.name),
            .optional => |inner| {
                try buffer.appendSlice(self.allocator, "opt_");
                try self.appendTypeName(buffer, inner.*);
            },
            .array => |arr| {
                try buffer.appendSlice(self.allocator, "arr_");
                try self.appendTypeName(buffer, arr.element);
            },
            .slice => |sl| {
                try buffer.appendSlice(self.allocator, "slice_");
                try self.appendTypeName(buffer, sl.element);
            },
            .rc => |rc| {
                try buffer.appendSlice(self.allocator, "rc_");
                try self.appendTypeName(buffer, rc.inner);
            },
            .tuple => |tup| {
                try buffer.appendSlice(self.allocator, "tup");
                for (tup.elements) |elem| {
                    try buffer.append(self.allocator, '_');
                    try self.appendTypeName(buffer, elem);
                }
            },
            else => try buffer.appendSlice(self.allocator, "unknown"),
        }
    }

    /// Record a monomorphized function instance and return its mangled name.
    /// If the same instantiation already exists, returns the existing entry.
    pub fn recordMonomorphization(
        self: *TypeChecker,
        func_name: []const u8,
        type_args: []const Type,
        concrete_type: Type,
    ) ![]const u8 {
        // Check if this instantiation already exists
        for (self.monomorphized_functions.items) |existing| {
            if (std.mem.eql(u8, existing.original_name, func_name) and
                existing.type_args.len == type_args.len)
            {
                var matches = true;
                for (existing.type_args, type_args) |e, t| {
                    if (!e.eql(t)) {
                        matches = false;
                        break;
                    }
                }
                if (matches) {
                    return existing.mangled_name;
                }
            }
        }

        // Look up the original generic function declaration
        const original_decl = self.generic_functions.get(func_name) orelse {
            // This shouldn't happen for valid generic function calls
            return error.OutOfMemory; // Function not found in generic registry
        };

        // Create new entry
        const mangled_name = try self.mangleFunctionName(func_name, type_args);
        const type_args_copy = try self.allocator.dupe(Type, type_args);

        try self.monomorphized_functions.append(self.allocator, .{
            .original_name = func_name,
            .mangled_name = mangled_name,
            .type_args = type_args_copy,
            .concrete_type = concrete_type,
            .original_decl = original_decl,
        });

        return mangled_name;
    }

    /// Get all monomorphized function instances.
    pub fn getMonomorphizedFunctions(self: *const TypeChecker) []const MonomorphizedFunction {
        return self.monomorphized_functions.items;
    }

    /// Get the resolved mangled name for a call expression, if it's a generic call.
    /// Returns null if the call is not to a generic function.
    pub fn getCallResolution(self: *const TypeChecker, call: *ast.Call) ?[]const u8 {
        return self.call_resolutions.get(call);
    }

    /// Get all monomorphized struct instances.
    pub fn getMonomorphizedStructs(self: *const TypeChecker) []const MonomorphizedStruct {
        return self.monomorphized_structs.items;
    }

    /// Get all monomorphized enum instances.
    pub fn getMonomorphizedEnums(self: *const TypeChecker) []const MonomorphizedEnum {
        return self.monomorphized_enums.items;
    }

    /// Record a monomorphized struct instance and return the concrete StructType.
    /// If the same instantiation already exists, returns the existing entry.
    pub fn recordStructMonomorphization(
        self: *TypeChecker,
        struct_name: []const u8,
        original_struct: *types.StructType,
        type_args: []const Type,
    ) !*types.StructType {
        // Check if this instantiation already exists
        for (self.monomorphized_structs.items) |existing| {
            if (std.mem.eql(u8, existing.original_name, struct_name) and
                existing.type_args.len == type_args.len)
            {
                var matches = true;
                for (existing.type_args, type_args) |e, t| {
                    if (!e.eql(t)) {
                        matches = false;
                        break;
                    }
                }
                if (matches) {
                    return existing.concrete_type;
                }
            }
        }

        // Create substitution map from type params to concrete types
        var substitutions = std.AutoHashMapUnmanaged(u32, Type){};
        defer substitutions.deinit(self.allocator);

        for (original_struct.type_params, 0..) |type_param, i| {
            if (i < type_args.len) {
                try substitutions.put(self.allocator, type_param.id, type_args[i]);
            }
        }

        // Create new struct type with substituted field types
        var concrete_fields = std.ArrayListUnmanaged(types.StructField){};
        defer concrete_fields.deinit(self.allocator);

        for (original_struct.fields) |field| {
            const concrete_field_type = try self.substituteTypeParams(field.type_, substitutions);
            try concrete_fields.append(self.allocator, .{
                .name = field.name,
                .type_ = concrete_field_type,
                .is_pub = field.is_pub,
            });
        }

        // Generate mangled name
        const mangled_name = try self.mangleStructName(struct_name, type_args);

        // Allocate and store the concrete struct type
        const concrete_struct = try self.allocator.create(types.StructType);
        concrete_struct.* = .{
            .name = mangled_name,
            .type_params = &.{}, // No type params - this is a concrete type
            .fields = try self.allocator.dupe(types.StructField, concrete_fields.items),
            .traits = original_struct.traits,
            .is_copy = original_struct.is_copy,
        };

        const type_args_copy = try self.allocator.dupe(Type, type_args);

        try self.monomorphized_structs.append(self.allocator, .{
            .original_name = struct_name,
            .mangled_name = mangled_name,
            .type_args = type_args_copy,
            .concrete_type = concrete_struct,
        });

        return concrete_struct;
    }

    /// Generate a mangled name for a struct instantiation.
    /// E.g., "Pair" with [i32, string] -> "Pair$i32$string"
    fn mangleStructName(self: *TypeChecker, struct_name: []const u8, type_args: []const Type) ![]const u8 {
        var result = std.ArrayListUnmanaged(u8){};
        errdefer result.deinit(self.allocator);

        try result.appendSlice(self.allocator, struct_name);

        for (type_args) |arg| {
            try result.append(self.allocator, '$');
            try self.appendTypeName(&result, arg);
        }

        return result.toOwnedSlice(self.allocator);
    }

    /// Record a monomorphized enum instance and return the concrete EnumType.
    /// If the same instantiation already exists, returns the existing entry.
    pub fn recordEnumMonomorphization(
        self: *TypeChecker,
        enum_name: []const u8,
        original_enum: *types.EnumType,
        type_args: []const Type,
    ) !*types.EnumType {
        // Check if this instantiation already exists
        for (self.monomorphized_enums.items) |existing| {
            if (std.mem.eql(u8, existing.original_name, enum_name) and
                existing.type_args.len == type_args.len)
            {
                var matches = true;
                for (existing.type_args, type_args) |e, t| {
                    if (!e.eql(t)) {
                        matches = false;
                        break;
                    }
                }
                if (matches) {
                    return existing.concrete_type;
                }
            }
        }

        // Create substitution map from type params to concrete types
        var substitutions = std.AutoHashMapUnmanaged(u32, Type){};
        defer substitutions.deinit(self.allocator);

        for (original_enum.type_params, 0..) |type_param, i| {
            if (i < type_args.len) {
                try substitutions.put(self.allocator, type_param.id, type_args[i]);
            }
        }

        // Create new enum type with substituted variant payloads
        var concrete_variants = std.ArrayListUnmanaged(types.EnumVariant){};
        defer concrete_variants.deinit(self.allocator);

        for (original_enum.variants) |variant| {
            const concrete_payload: ?types.VariantPayload = if (variant.payload) |payload| blk: {
                break :blk switch (payload) {
                    .tuple => |tuple_types| tup: {
                        var new_types = std.ArrayListUnmanaged(Type){};
                        defer new_types.deinit(self.allocator);
                        for (tuple_types) |t| {
                            const concrete_type = try self.substituteTypeParams(t, substitutions);
                            try new_types.append(self.allocator, concrete_type);
                        }
                        break :tup .{ .tuple = try self.allocator.dupe(Type, new_types.items) };
                    },
                    .struct_ => |struct_fields| str: {
                        var new_fields = std.ArrayListUnmanaged(types.StructField){};
                        defer new_fields.deinit(self.allocator);
                        for (struct_fields) |f| {
                            const concrete_type = try self.substituteTypeParams(f.type_, substitutions);
                            try new_fields.append(self.allocator, .{
                                .name = f.name,
                                .type_ = concrete_type,
                                .is_pub = f.is_pub,
                            });
                        }
                        break :str .{ .struct_ = try self.allocator.dupe(types.StructField, new_fields.items) };
                    },
                };
            } else null;

            try concrete_variants.append(self.allocator, .{
                .name = variant.name,
                .payload = concrete_payload,
            });
        }

        // Generate mangled name (same pattern as structs)
        const mangled_name = try self.mangleStructName(enum_name, type_args);

        // Allocate and store the concrete enum type
        const concrete_enum = try self.allocator.create(types.EnumType);
        concrete_enum.* = .{
            .name = mangled_name,
            .type_params = &.{}, // No type params - this is a concrete type
            .variants = try self.allocator.dupe(types.EnumVariant, concrete_variants.items),
        };

        const type_args_copy = try self.allocator.dupe(Type, type_args);

        try self.monomorphized_enums.append(self.allocator, .{
            .original_name = enum_name,
            .mangled_name = mangled_name,
            .type_args = type_args_copy,
            .concrete_type = concrete_enum,
        });

        return concrete_enum;
    }

    /// Substitute type variables in a type using the given mapping.
    /// The substitutions map TypeVar.id to the concrete Type to substitute.
    /// Returns a new type with all type variables replaced.
    pub fn substituteTypeParams(self: *TypeChecker, typ: Type, substitutions: std.AutoHashMapUnmanaged(u32, Type)) !Type {
        return switch (typ) {
            // Type variable - the core substitution case
            .type_var => |tv| {
                if (substitutions.get(tv.id)) |concrete_type| {
                    return concrete_type;
                }
                // Type variable not in substitution map - return unchanged
                return typ;
            },

            // Primitive types - no substitution needed
            .primitive, .void_, .never, .unknown, .error_type => typ,

            // Array - substitute element type
            .array => |arr| {
                const new_elem = try self.substituteTypeParams(arr.element, substitutions);
                if (arr.element.eql(new_elem)) return typ;
                return self.type_builder.arrayType(new_elem, arr.size);
            },

            // Slice - substitute element type
            .slice => |sl| {
                const new_elem = try self.substituteTypeParams(sl.element, substitutions);
                if (sl.element.eql(new_elem)) return typ;
                return self.type_builder.sliceType(new_elem);
            },

            // Tuple - substitute all element types
            .tuple => |tup| {
                var changed = false;
                var new_elements = std.ArrayListUnmanaged(Type){};
                defer new_elements.deinit(self.allocator);

                for (tup.elements) |elem| {
                    const new_elem = try self.substituteTypeParams(elem, substitutions);
                    if (!elem.eql(new_elem)) changed = true;
                    try new_elements.append(self.allocator, new_elem);
                }

                if (!changed) return typ;
                const elements_slice = try self.allocator.dupe(Type, new_elements.items);
                try self.substituted_type_slices.append(self.allocator, elements_slice);
                return self.type_builder.tupleType(elements_slice);
            },

            // Optional - substitute inner type
            .optional => |inner| {
                const new_inner = try self.substituteTypeParams(inner.*, substitutions);
                if (inner.eql(new_inner)) return typ;
                return self.type_builder.optionalType(new_inner);
            },

            // Result - substitute ok and err types
            .result => |res| {
                const new_ok = try self.substituteTypeParams(res.ok_type, substitutions);
                const new_err = try self.substituteTypeParams(res.err_type, substitutions);
                if (res.ok_type.eql(new_ok) and res.err_type.eql(new_err)) return typ;
                return self.type_builder.resultType(new_ok, new_err);
            },

            // Function - substitute param and return types
            .function => |func| {
                var changed = false;
                var new_params = std.ArrayListUnmanaged(Type){};
                defer new_params.deinit(self.allocator);

                for (func.params) |param_type| {
                    const new_param_type = try self.substituteTypeParams(param_type, substitutions);
                    if (!param_type.eql(new_param_type)) changed = true;
                    try new_params.append(self.allocator, new_param_type);
                }

                const new_return = try self.substituteTypeParams(func.return_type, substitutions);
                if (!func.return_type.eql(new_return)) changed = true;

                if (!changed) return typ;
                const params_slice = try self.allocator.dupe(Type, new_params.items);
                try self.substituted_type_slices.append(self.allocator, params_slice);
                return self.type_builder.functionType(params_slice, new_return);
            },

            // Reference - substitute inner type
            .reference => |ref| {
                const new_inner = try self.substituteTypeParams(ref.inner, substitutions);
                if (ref.inner.eql(new_inner)) return typ;
                return self.type_builder.referenceType(new_inner, ref.mutable);
            },

            // Applied type - substitute type arguments
            .applied => |app| {
                const new_base = try self.substituteTypeParams(app.base, substitutions);
                var changed = !app.base.eql(new_base);

                var new_args = std.ArrayListUnmanaged(Type){};
                defer new_args.deinit(self.allocator);

                for (app.args) |arg| {
                    const new_arg = try self.substituteTypeParams(arg, substitutions);
                    if (!arg.eql(new_arg)) changed = true;
                    try new_args.append(self.allocator, new_arg);
                }

                if (!changed) return typ;
                const args_slice = try self.allocator.dupe(Type, new_args.items);
                try self.substituted_type_slices.append(self.allocator, args_slice);
                return self.type_builder.appliedType(new_base, args_slice);
            },

            // Rc types - substitute inner type
            .rc => |rc| {
                const new_inner = try self.substituteTypeParams(rc.inner, substitutions);
                if (rc.inner.eql(new_inner)) return typ;
                return self.type_builder.rcType(new_inner);
            },

            .weak_rc => |wrc| {
                const new_inner = try self.substituteTypeParams(wrc.inner, substitutions);
                if (wrc.inner.eql(new_inner)) return typ;
                return self.type_builder.weakRcType(new_inner);
            },

            .arc => |arc| {
                const new_inner = try self.substituteTypeParams(arc.inner, substitutions);
                if (arc.inner.eql(new_inner)) return typ;
                return self.type_builder.arcType(new_inner);
            },

            .weak_arc => |warc| {
                const new_inner = try self.substituteTypeParams(warc.inner, substitutions);
                if (warc.inner.eql(new_inner)) return typ;
                return self.type_builder.weakArcType(new_inner);
            },

            // Cell - substitute inner type
            .cell => |c| {
                const new_inner = try self.substituteTypeParams(c.inner, substitutions);
                if (c.inner.eql(new_inner)) return typ;
                return self.type_builder.cellType(new_inner);
            },

            // Struct/Enum/Trait - return as-is (monomorphization creates applied types)
            .struct_, .enum_, .trait_ => typ,
        };
    }

    /// Check if a type contains any type variables.
    pub fn containsTypeVar(self: *TypeChecker, typ: Type) bool {
        return switch (typ) {
            .type_var => true,
            .primitive, .void_, .never, .unknown, .error_type => false,
            .array => |arr| self.containsTypeVar(arr.element),
            .slice => |sl| self.containsTypeVar(sl.element),
            .tuple => |tup| {
                for (tup.elements) |elem| {
                    if (self.containsTypeVar(elem)) return true;
                }
                return false;
            },
            .optional => |inner| self.containsTypeVar(inner.*),
            .result => |res| self.containsTypeVar(res.ok_type) or self.containsTypeVar(res.err_type),
            .function => |func| {
                for (func.params) |param_type| {
                    if (self.containsTypeVar(param_type)) return true;
                }
                return self.containsTypeVar(func.return_type);
            },
            .reference => |ref| self.containsTypeVar(ref.inner),
            .applied => |app| {
                if (self.containsTypeVar(app.base)) return true;
                for (app.args) |arg| {
                    if (self.containsTypeVar(arg)) return true;
                }
                return false;
            },
            .rc => |rc| self.containsTypeVar(rc.inner),
            .weak_rc => |wrc| self.containsTypeVar(wrc.inner),
            .arc => |arc| self.containsTypeVar(arc.inner),
            .weak_arc => |warc| self.containsTypeVar(warc.inner),
            .cell => |c| self.containsTypeVar(c.inner),
            .struct_, .enum_, .trait_ => false,
        };
    }

    /// Try to unify a type with type variables against a concrete type.
    /// Returns true if unification succeeded, populating the substitutions map.
    pub fn unifyTypes(
        self: *TypeChecker,
        pattern: Type,
        concrete: Type,
        substitutions: *std.AutoHashMapUnmanaged(u32, Type),
    ) !bool {
        switch (pattern) {
            .type_var => |tv| {
                // Check if we already have a binding for this type variable
                if (substitutions.get(tv.id)) |existing| {
                    // Must match the existing binding
                    return existing.eql(concrete);
                }
                // New binding - add it
                try substitutions.put(self.allocator, tv.id, concrete);
                return true;
            },

            .primitive, .void_, .never, .unknown, .error_type => {
                return pattern.eql(concrete);
            },

            .array => |arr| {
                if (concrete != .array) return false;
                const concrete_arr = concrete.array;
                if (arr.size != concrete_arr.size) return false;
                return self.unifyTypes(arr.element, concrete_arr.element, substitutions);
            },

            .slice => |sl| {
                if (concrete != .slice) return false;
                const concrete_sl = concrete.slice;
                return self.unifyTypes(sl.element, concrete_sl.element, substitutions);
            },

            .tuple => |tup| {
                if (concrete != .tuple) return false;
                const concrete_tup = concrete.tuple;
                if (tup.elements.len != concrete_tup.elements.len) return false;
                for (tup.elements, concrete_tup.elements) |pat_elem, conc_elem| {
                    if (!try self.unifyTypes(pat_elem, conc_elem, substitutions)) {
                        return false;
                    }
                }
                return true;
            },

            .optional => |inner| {
                if (concrete != .optional) return false;
                return self.unifyTypes(inner.*, concrete.optional.*, substitutions);
            },

            .result => |res| {
                if (concrete != .result) return false;
                const concrete_res = concrete.result;
                return try self.unifyTypes(res.ok_type, concrete_res.ok_type, substitutions) and
                    try self.unifyTypes(res.err_type, concrete_res.err_type, substitutions);
            },

            .function => |func| {
                if (concrete != .function) return false;
                const concrete_func = concrete.function;
                if (func.params.len != concrete_func.params.len) return false;
                for (func.params, concrete_func.params) |pat_param_type, conc_param_type| {
                    if (!try self.unifyTypes(pat_param_type, conc_param_type, substitutions)) {
                        return false;
                    }
                }
                return self.unifyTypes(func.return_type, concrete_func.return_type, substitutions);
            },

            .reference => |ref| {
                if (concrete != .reference) return false;
                const concrete_ref = concrete.reference;
                if (ref.mutable != concrete_ref.mutable) return false;
                return self.unifyTypes(ref.inner, concrete_ref.inner, substitutions);
            },

            .rc => |rc| {
                if (concrete != .rc) return false;
                return self.unifyTypes(rc.inner, concrete.rc.inner, substitutions);
            },

            .weak_rc => |wrc| {
                if (concrete != .weak_rc) return false;
                return self.unifyTypes(wrc.inner, concrete.weak_rc.inner, substitutions);
            },

            .arc => |arc| {
                if (concrete != .arc) return false;
                return self.unifyTypes(arc.inner, concrete.arc.inner, substitutions);
            },

            .weak_arc => |warc| {
                if (concrete != .weak_arc) return false;
                return self.unifyTypes(warc.inner, concrete.weak_arc.inner, substitutions);
            },

            .cell => |c| {
                if (concrete != .cell) return false;
                return self.unifyTypes(c.inner, concrete.cell.inner, substitutions);
            },

            .applied => |app| {
                if (concrete != .applied) return false;
                const concrete_app = concrete.applied;
                if (!try self.unifyTypes(app.base, concrete_app.base, substitutions)) {
                    return false;
                }
                if (app.args.len != concrete_app.args.len) return false;
                for (app.args, concrete_app.args) |pat_arg, conc_arg| {
                    if (!try self.unifyTypes(pat_arg, conc_arg, substitutions)) {
                        return false;
                    }
                }
                return true;
            },

            .struct_, .enum_, .trait_ => {
                return pattern.eql(concrete);
            },
        }
    }

    // ========================================================================
    // Type Resolution (AST TypeExpr -> Type)
    // ========================================================================

    pub fn resolveTypeExpr(self: *TypeChecker, type_expr: ast.TypeExpr) !Type {
        switch (type_expr) {
            .named => |n| {
                // Check for primitive types
                if (Primitive.fromName(n.name)) |prim| {
                    return .{ .primitive = prim };
                }
                // Check for void
                if (std.mem.eql(u8, n.name, "void")) {
                    return self.type_builder.voidType();
                }
                // Check for Self
                if (std.mem.eql(u8, n.name, "Self")) {
                    // TODO: resolve Self in impl context
                    return self.type_builder.unknownType();
                }
                // Check for type parameters (e.g., T in fn foo[T](x: T))
                if (self.lookupTypeParam(n.name)) |type_var| {
                    return .{ .type_var = type_var };
                }
                // Look up user-defined type or trait
                if (self.current_scope.lookup(n.name)) |sym| {
                    if (sym.kind == .type_ or sym.kind == .trait_) {
                        return sym.type_;
                    }
                }
                self.addError(.undefined_type, n.span, "undefined type '{s}'", .{n.name});
                return self.type_builder.unknownType();
            },
            .array => |a| {
                const elem_type = try self.resolveTypeExpr(a.element);
                // TODO: evaluate size expression at compile time
                return try self.type_builder.arrayType(elem_type, 0);
            },
            .slice => |s| {
                const elem_type = try self.resolveTypeExpr(s.element);
                return try self.type_builder.sliceType(elem_type);
            },
            .tuple => |t| {
                var elem_types: std.ArrayListUnmanaged(Type) = .{};
                defer elem_types.deinit(self.allocator);
                for (t.elements) |elem| {
                    try elem_types.append(self.allocator, try self.resolveTypeExpr(elem));
                }
                return try self.type_builder.tupleType(elem_types.items);
            },
            .optional => |o| {
                const inner = try self.resolveTypeExpr(o.inner);
                return try self.type_builder.optionalType(inner);
            },
            .result => |r| {
                const ok_type = try self.resolveTypeExpr(r.ok_type);
                const err_type = try self.resolveTypeExpr(r.err_type);
                return try self.type_builder.resultType(ok_type, err_type);
            },
            .function => |f| {
                var param_types: std.ArrayListUnmanaged(Type) = .{};
                defer param_types.deinit(self.allocator);
                for (f.params) |param| {
                    try param_types.append(self.allocator, try self.resolveTypeExpr(param));
                }
                const ret_type = try self.resolveTypeExpr(f.return_type);
                return try self.type_builder.functionType(param_types.items, ret_type);
            },
            .reference => |r| {
                const inner = try self.resolveTypeExpr(r.inner);
                return try self.type_builder.referenceType(inner, r.mutable);
            },
            .generic_apply => |g| {
                // Check for built-in generic types: Rc[T], Weak[T]
                if (g.base == .named) {
                    const base_name = g.base.named.name;

                    // Rc[T] - reference-counted type
                    if (std.mem.eql(u8, base_name, "Rc")) {
                        if (g.args.len != 1) {
                            self.addError(.type_mismatch, g.span, "Rc expects exactly 1 type argument", .{});
                            return self.type_builder.unknownType();
                        }
                        const inner = try self.resolveTypeExpr(g.args[0]);
                        return try self.type_builder.rcType(inner);
                    }

                    // Weak[T] - weak reference type
                    if (std.mem.eql(u8, base_name, "Weak")) {
                        if (g.args.len != 1) {
                            self.addError(.type_mismatch, g.span, "Weak expects exactly 1 type argument", .{});
                            return self.type_builder.unknownType();
                        }
                        const inner = try self.resolveTypeExpr(g.args[0]);
                        return try self.type_builder.weakRcType(inner);
                    }

                    // Arc[T] - atomic reference-counted type (thread-safe)
                    if (std.mem.eql(u8, base_name, "Arc")) {
                        if (g.args.len != 1) {
                            self.addError(.type_mismatch, g.span, "Arc expects exactly 1 type argument", .{});
                            return self.type_builder.unknownType();
                        }
                        const inner = try self.resolveTypeExpr(g.args[0]);
                        return try self.type_builder.arcType(inner);
                    }

                    // WeakArc[T] - weak atomic reference type (thread-safe)
                    if (std.mem.eql(u8, base_name, "WeakArc")) {
                        if (g.args.len != 1) {
                            self.addError(.type_mismatch, g.span, "WeakArc expects exactly 1 type argument", .{});
                            return self.type_builder.unknownType();
                        }
                        const inner = try self.resolveTypeExpr(g.args[0]);
                        return try self.type_builder.weakArcType(inner);
                    }
                }

                // Generic user-defined type
                const base = try self.resolveTypeExpr(g.base);
                var args: std.ArrayListUnmanaged(Type) = .{};
                defer args.deinit(self.allocator);
                for (g.args) |arg| {
                    try args.append(self.allocator, try self.resolveTypeExpr(arg));
                }

                // Check if base is a generic struct that needs monomorphization
                if (base == .struct_) {
                    const struct_type = base.struct_;
                    // Only monomorphize if this struct has type parameters
                    if (struct_type.type_params.len > 0) {
                        // Validate argument count
                        if (args.items.len != struct_type.type_params.len) {
                            self.addError(.type_mismatch, g.span, "wrong number of type arguments", .{});
                            return self.type_builder.unknownType();
                        }
                        // Check if any type args contain unresolved type variables
                        var has_type_vars = false;
                        for (args.items) |arg| {
                            if (self.containsTypeVar(arg)) {
                                has_type_vars = true;
                                break;
                            }
                        }
                        if (has_type_vars) {
                            // Inside a generic context - return an applied type for later resolution
                            return try self.type_builder.appliedType(base, args.items);
                        }
                        // All type args are concrete - monomorphize the struct
                        const concrete_struct = self.recordStructMonomorphization(
                            struct_type.name,
                            struct_type,
                            args.items,
                        ) catch return self.type_builder.unknownType();
                        return .{ .struct_ = concrete_struct };
                    }
                }

                // Check if base is a generic enum that needs monomorphization
                if (base == .enum_) {
                    const enum_type = base.enum_;
                    if (enum_type.type_params.len > 0) {
                        // Validate argument count
                        if (args.items.len != enum_type.type_params.len) {
                            self.addError(.type_mismatch, g.span, "wrong number of type arguments", .{});
                            return self.type_builder.unknownType();
                        }
                        // Check if any type args contain unresolved type variables
                        var has_type_vars = false;
                        for (args.items) |arg| {
                            if (self.containsTypeVar(arg)) {
                                has_type_vars = true;
                                break;
                            }
                        }
                        if (has_type_vars) {
                            // Inside a generic context - return an applied type for later resolution
                            return try self.type_builder.appliedType(base, args.items);
                        }
                        // All type args are concrete - monomorphize the enum
                        const concrete_enum = self.recordEnumMonomorphization(
                            enum_type.name,
                            enum_type,
                            args.items,
                        ) catch return self.type_builder.unknownType();
                        return .{ .enum_ = concrete_enum };
                    }
                }

                return try self.type_builder.appliedType(base, args.items);
            },
        }
    }

    // ========================================================================
    // Expression Type Checking
    // ========================================================================

    pub fn checkExpr(self: *TypeChecker, expr: ast.Expr) Type {
        return switch (expr) {
            .literal => |l| self.checkLiteral(l),
            .identifier => |i| self.checkIdentifier(i),
            .binary => |b| self.checkBinary(b),
            .unary => |u| self.checkUnary(u),
            .postfix => |p| self.checkPostfix(p),
            .call => |c| self.checkCall(c),
            .index => |i| self.checkIndex(i),
            .field => |f| self.checkField(f),
            .method_call => |m| self.checkMethodCall(m),
            .if_expr => |i| self.checkIfExpr(i),
            .match_expr => |m| self.checkMatchExpr(m),
            .block => |b| self.checkBlock(b),
            .closure => |c| self.checkClosure(c),
            .range => |r| self.checkRange(r),
            .struct_literal => |s| self.checkStructLiteral(s),
            .array_literal => |a| self.checkArrayLiteral(a),
            .tuple_literal => |t| self.checkTupleLiteral(t),
            .type_cast => |tc| self.checkTypeCast(tc),
            .grouped => |g| self.checkExpr(g.expr),
            .interpolated_string => |is| self.checkInterpolatedString(is),
            .enum_literal => |e| self.checkEnumLiteral(e),
        };
    }

    fn checkLiteral(self: *TypeChecker, lit: ast.Literal) Type {
        return switch (lit.kind) {
            .int => self.type_builder.i32Type(), // Default int type
            .float => self.type_builder.f64Type(), // Default float type
            .string => self.type_builder.stringType(),
            .char => self.type_builder.charType(),
            .bool_ => self.type_builder.boolType(),
        };
    }

    fn checkInterpolatedString(self: *TypeChecker, interp: *ast.InterpolatedString) Type {
        // Type-check all embedded expressions (any type is allowed, will be converted to string)
        for (interp.parts) |part| {
            switch (part) {
                .string => {},
                .expr => |e| _ = self.checkExpr(e),
            }
        }
        // Interpolated strings always produce a string
        return self.type_builder.stringType();
    }

    fn checkIdentifier(self: *TypeChecker, id: ast.Identifier) Type {
        if (self.current_scope.lookup(id.name)) |sym| {
            // Track captures if we're inside a closure
            if (self.closure_scope) |closure_scope| {
                // Check if this variable is from outside the closure's scope
                // It's a capture if not found in closure scope's local symbols
                // and not found in any child scope of the closure
                if (!self.isInClosureScope(closure_scope, id.name)) {
                    // Skip functions and types - they don't need to be captured
                    if (sym.kind == .variable or sym.kind == .parameter or sym.kind == .constant) {
                        self.closure_captures.put(self.allocator, id.name, .{
                            .is_mutable = sym.mutable,
                        }) catch {};
                    }
                }
            }
            return sym.type_;
        }
        self.addError(.undefined_variable, id.span, "undefined variable '{s}'", .{id.name});
        return self.type_builder.unknownType();
    }

    /// Check if a variable is defined within the closure scope (not a capture).
    fn isInClosureScope(self: *TypeChecker, closure_scope: *Scope, name: []const u8) bool {
        // Check the closure scope and all its child scopes (which are nested inside it)
        // Starting from current_scope, walk up to closure_scope checking each scope's locals
        var check_scope: ?*Scope = self.current_scope;
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

    fn checkBinary(self: *TypeChecker, bin: *ast.Binary) Type {
        const left_type = self.checkExpr(bin.left);
        const right_type = self.checkExpr(bin.right);

        // Handle assignment operators
        switch (bin.op) {
            .assign, .add_assign, .sub_assign, .mul_assign, .div_assign, .mod_assign => {
                // Check that left side is assignable and mutable
                if (!self.isAssignable(bin.left)) {
                    self.addError(.invalid_assignment, bin.span, "cannot assign to this expression", .{});
                }
                if (bin.op == .assign) {
                    if (!self.checkAssignmentCompatible(left_type, right_type)) {
                        self.addError(.type_mismatch, bin.span, "cannot assign different types", .{});
                    }
                    return self.type_builder.voidType();
                } else {
                    // Compound assignment: check arithmetic compatibility
                    if (!left_type.isNumeric() or !right_type.isNumeric()) {
                        self.addError(.invalid_operation, bin.span, "compound assignment requires numeric types", .{});
                    }
                    return self.type_builder.voidType();
                }
            },
            else => {},
        }

        // Arithmetic operators
        switch (bin.op) {
            .add, .sub, .mul, .div, .mod, .add_wrap, .sub_wrap, .mul_wrap, .add_sat, .sub_sat, .mul_sat => {
                if (!left_type.isNumeric()) {
                    self.addError(.invalid_operation, bin.span, "left operand must be numeric", .{});
                    return self.type_builder.unknownType();
                }
                if (!right_type.isNumeric()) {
                    self.addError(.invalid_operation, bin.span, "right operand must be numeric", .{});
                    return self.type_builder.unknownType();
                }
                // Types must match exactly (no implicit conversions)
                if (!left_type.eql(right_type)) {
                    self.addError(.type_mismatch, bin.span, "operand types must match", .{});
                }
                return left_type;
            },
            // Comparison operators
            .eq, .not_eq, .lt, .gt, .lt_eq, .gt_eq => {
                if (!left_type.eql(right_type)) {
                    self.addError(.type_mismatch, bin.span, "comparison operands must have same type", .{});
                }
                return self.type_builder.boolType();
            },
            // Logical operators
            .and_, .or_ => {
                if (!self.isBoolType(left_type)) {
                    self.addError(.type_mismatch, bin.span, "'and'/'or' requires bool operands", .{});
                }
                if (!self.isBoolType(right_type)) {
                    self.addError(.type_mismatch, bin.span, "'and'/'or' requires bool operands", .{});
                }
                return self.type_builder.boolType();
            },
            // Bitwise operators
            .bit_and, .bit_or, .bit_xor, .shl, .shr => {
                if (!left_type.isInteger()) {
                    self.addError(.invalid_operation, bin.span, "bitwise operations require integers", .{});
                    return self.type_builder.unknownType();
                }
                if (!right_type.isInteger()) {
                    self.addError(.invalid_operation, bin.span, "bitwise operations require integers", .{});
                    return self.type_builder.unknownType();
                }
                return left_type;
            },
            // Null coalescing
            .null_coalesce => {
                // Left must be optional
                if (left_type != .optional) {
                    self.addError(.invalid_operation, bin.span, "'??' requires optional on left", .{});
                    return right_type;
                }
                // Right must match inner type of optional
                const inner = left_type.optional.*;
                if (!inner.eql(right_type)) {
                    self.addError(.type_mismatch, bin.span, "'??' types must match", .{});
                }
                return right_type;
            },
            // 'is' operator for type checking
            .is => {
                return self.type_builder.boolType();
            },
            else => return self.type_builder.unknownType(),
        }
    }

    fn checkUnary(self: *TypeChecker, un: *ast.Unary) Type {
        const operand_type = self.checkExpr(un.operand);

        switch (un.op) {
            .negate => {
                if (!operand_type.isNumeric()) {
                    self.addError(.invalid_operation, un.span, "negation requires numeric type", .{});
                    return self.type_builder.unknownType();
                }
                if (!operand_type.isSigned()) {
                    self.addError(.invalid_operation, un.span, "negation requires signed type", .{});
                }
                return operand_type;
            },
            .not => {
                if (!self.isBoolType(operand_type)) {
                    self.addError(.invalid_operation, un.span, "'not' requires bool type", .{});
                    return self.type_builder.unknownType();
                }
                return self.type_builder.boolType();
            },
            .ref => {
                return self.type_builder.referenceType(operand_type, false) catch self.type_builder.unknownType();
            },
            .ref_mut => {
                if (!self.isMutable(un.operand)) {
                    self.addError(.mutability_error, un.span, "cannot take mutable reference to immutable value", .{});
                }
                return self.type_builder.referenceType(operand_type, true) catch self.type_builder.unknownType();
            },
            .deref => {
                // Only allow dereferencing reference types (&T, &mut T)
                // Rc[T] should use .get() or Cell/RefCell patterns instead
                if (operand_type == .reference) {
                    return operand_type.reference.inner;
                } else {
                    self.addError(.invalid_operation, un.span, "cannot dereference non-reference type", .{});
                    return self.type_builder.unknownType();
                }
            },
        }
    }

    fn checkPostfix(self: *TypeChecker, post: *ast.Postfix) Type {
        const operand_type = self.checkExpr(post.operand);

        switch (post.op) {
            .unwrap => {
                // ? operator: optional -> inner type (returns optional on None)
                if (operand_type != .optional) {
                    self.addError(.invalid_operation, post.span, "'?' requires optional type", .{});
                    return self.type_builder.unknownType();
                }
                return operand_type.optional.*;
            },
            .force_unwrap => {
                // ! operator: optional -> inner type (traps on None)
                if (operand_type != .optional and operand_type != .result) {
                    self.addError(.invalid_operation, post.span, "'!' requires optional or result type", .{});
                    return self.type_builder.unknownType();
                }
                if (operand_type == .optional) {
                    return operand_type.optional.*;
                }
                return operand_type.result.ok_type;
            },
        }
    }

    fn checkCall(self: *TypeChecker, call: *ast.Call) Type {
        const callee_type = self.checkExpr(call.callee);

        if (callee_type != .function) {
            self.addError(.invalid_call, call.span, "cannot call non-function type", .{});
            return self.type_builder.unknownType();
        }

        const func = callee_type.function;

        // Check argument count
        if (call.args.len != func.params.len) {
            self.addError(.invalid_call, call.span, "expected {d} arguments, got {d}", .{ func.params.len, call.args.len });
            return func.return_type;
        }

        // Check if this is a generic function call (has type variables)
        const is_generic = self.containsTypeVar(.{ .function = func });

        if (is_generic) {
            // Type inference for generic function calls
            var substitutions = std.AutoHashMapUnmanaged(u32, Type){};
            defer substitutions.deinit(self.allocator);

            // First pass: check all argument types and try to infer type variables
            var all_unified = true;
            for (call.args, func.params) |arg, param_type| {
                const arg_type = self.checkExpr(arg);
                // Try to unify the parameter type (which may contain type vars)
                // with the concrete argument type
                const unified = self.unifyTypes(param_type, arg_type, &substitutions) catch {
                    all_unified = false;
                    continue;
                };
                if (!unified) {
                    self.addError(.type_mismatch, arg.span(), "argument type mismatch", .{});
                    all_unified = false;
                }
            }

            if (all_unified) {
                // Successfully inferred all type variables, substitute in return type
                const concrete_return = self.substituteTypeParams(func.return_type, substitutions) catch {
                    return func.return_type;
                };

                // Record monomorphization if callee is a named function
                if (call.callee == .identifier) {
                    const func_name = call.callee.identifier.name;

                    // Collect inferred type arguments in order
                    var type_args = std.ArrayListUnmanaged(Type){};
                    defer type_args.deinit(self.allocator);

                    // Collect all inferred types from the substitution map
                    var iter = substitutions.iterator();
                    while (iter.next()) |entry| {
                        type_args.append(self.allocator, entry.value_ptr.*) catch {};
                    }

                    // Look up the original generic function declaration to check trait bounds
                    if (self.generic_functions.get(func_name)) |original_decl| {
                        // Check that each type argument satisfies the bounds
                        // The substitutions map maps type var id -> concrete type
                        // We need to match them with the type params in order
                        for (original_decl.type_params) |type_param| {
                            // Get the type var that was registered for this param
                            if (self.lookupTypeParam(type_param.name)) |type_var| {
                                // Get the concrete type that was inferred
                                if (substitutions.get(type_var.id)) |concrete_type| {
                                    // Check that the concrete type satisfies bounds
                                    _ = self.typeSatisfiesBounds(concrete_type, type_var.bounds, call.span);
                                }
                            }
                        }
                    }

                    // Get the concrete function type
                    const concrete_func_type = self.substituteTypeParams(.{ .function = func }, substitutions) catch {
                        return concrete_return;
                    };

                    // Record this monomorphization and get the mangled name
                    const mangled_name = self.recordMonomorphization(
                        func_name,
                        type_args.items,
                        concrete_func_type,
                    ) catch {
                        return concrete_return;
                    };

                    // Store the call resolution for codegen
                    self.call_resolutions.put(self.allocator, call, mangled_name) catch {};
                }

                return concrete_return;
            } else {
                // Could not fully infer types, return the generic return type
                return func.return_type;
            }
        } else {
            // Non-generic call: check argument types directly
            for (call.args, func.params) |arg, param_type| {
                const arg_type = self.checkExpr(arg);
                if (!arg_type.eql(param_type)) {
                    self.addError(.type_mismatch, arg.span(), "argument type mismatch", .{});
                }
            }
            return func.return_type;
        }
    }

    fn checkIndex(self: *TypeChecker, idx: *ast.Index) Type {
        const object_type = self.checkExpr(idx.object);
        const index_type = self.checkExpr(idx.index);

        // Index must be integer
        if (!index_type.isInteger()) {
            self.addError(.invalid_index, idx.span, "index must be integer type", .{});
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
                return self.type_builder.unknownType();
            },
            else => {
                self.addError(.invalid_index, idx.span, "cannot index this type", .{});
                return self.type_builder.unknownType();
            },
        }
    }

    fn checkField(self: *TypeChecker, fld: *ast.Field) Type {
        const object_type = self.checkExpr(fld.object);

        switch (object_type) {
            .struct_ => |s| {
                for (s.fields) |field| {
                    if (std.mem.eql(u8, field.name, fld.field_name)) {
                        return field.type_;
                    }
                }
                self.addError(.undefined_field, fld.span, "no field '{s}' on struct", .{fld.field_name});
                return self.type_builder.unknownType();
            },
            .applied => |a| {
                // For applied types like Pair[T], access fields from the base struct
                if (a.base == .struct_) {
                    const s = a.base.struct_;
                    for (s.fields) |field| {
                        if (std.mem.eql(u8, field.name, fld.field_name)) {
                            // The field type may contain type variables that map to the applied args
                            // For now, return the raw field type (type var substitution happens at monomorphization)
                            return field.type_;
                        }
                    }
                    self.addError(.undefined_field, fld.span, "no field '{s}' on struct", .{fld.field_name});
                    return self.type_builder.unknownType();
                }
                self.addError(.undefined_field, fld.span, "cannot access field on this type", .{});
                return self.type_builder.unknownType();
            },
            .tuple => |t| {
                // Tuple field access by index (e.g., tuple.0, tuple.1)
                const idx = std.fmt.parseInt(usize, fld.field_name, 10) catch {
                    self.addError(.undefined_field, fld.span, "invalid tuple field", .{});
                    return self.type_builder.unknownType();
                };
                if (idx >= t.elements.len) {
                    self.addError(.undefined_field, fld.span, "tuple index out of bounds", .{});
                    return self.type_builder.unknownType();
                }
                return t.elements[idx];
            },
            else => {
                self.addError(.undefined_field, fld.span, "cannot access field on this type", .{});
                return self.type_builder.unknownType();
            },
        }
    }

    fn checkMethodCall(self: *TypeChecker, method: *ast.MethodCall) Type {
        // Handle Rc.new(value) and Weak.new(value) static constructors
        if (method.object == .identifier) {
            const obj_name = method.object.identifier.name;

            // Rc.new(value) -> Rc[T] where T is the type of value
            if (std.mem.eql(u8, obj_name, "Rc") and std.mem.eql(u8, method.method_name, "new")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "Rc.new() expects exactly 1 argument", .{});
                    return self.type_builder.unknownType();
                }
                const value_type = self.checkExpr(method.args[0]);
                return self.type_builder.rcType(value_type) catch self.type_builder.unknownType();
            }

            // Weak.new() is not valid - Weak can only be created from Rc.downgrade()
            if (std.mem.eql(u8, obj_name, "Weak") and std.mem.eql(u8, method.method_name, "new")) {
                self.addError(.invalid_call, method.span, "Weak cannot be created directly; use rc_value.downgrade()", .{});
                return self.type_builder.unknownType();
            }

            // Arc.new(value) -> Arc[T] where T is the type of value (thread-safe)
            if (std.mem.eql(u8, obj_name, "Arc") and std.mem.eql(u8, method.method_name, "new")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "Arc.new() expects exactly 1 argument", .{});
                    return self.type_builder.unknownType();
                }
                const value_type = self.checkExpr(method.args[0]);
                return self.type_builder.arcType(value_type) catch self.type_builder.unknownType();
            }

            // WeakArc.new() is not valid - WeakArc can only be created from Arc.downgrade()
            if (std.mem.eql(u8, obj_name, "WeakArc") and std.mem.eql(u8, method.method_name, "new")) {
                self.addError(.invalid_call, method.span, "WeakArc cannot be created directly; use arc_value.downgrade()", .{});
                return self.type_builder.unknownType();
            }

            // Cell.new(value) -> Cell[T] where T is the type of value
            // Cell provides interior mutability for Copy types
            if (std.mem.eql(u8, obj_name, "Cell") and std.mem.eql(u8, method.method_name, "new")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "Cell.new() expects exactly 1 argument", .{});
                    return self.type_builder.unknownType();
                }
                const value_type = self.checkExpr(method.args[0]);
                // Cell should only wrap Copy types, but we'll allow it for flexibility
                return self.type_builder.cellType(value_type) catch self.type_builder.unknownType();
            }
        }

        const object_type = self.checkExpr(method.object);

        // Check for type conversion methods
        if (std.mem.eql(u8, method.method_name, "as") or
            std.mem.eql(u8, method.method_name, "to") or
            std.mem.eql(u8, method.method_name, "trunc"))
        {
            // Type conversion methods
            if (method.type_args) |type_args| {
                if (type_args.len == 1) {
                    return self.resolveTypeExpr(type_args[0]) catch self.type_builder.unknownType();
                }
            }
            self.addError(.invalid_call, method.span, "conversion requires type argument", .{});
            return self.type_builder.unknownType();
        }

        // Check for built-in methods on all types
        if (std.mem.eql(u8, method.method_name, "to_string")) {
            return self.type_builder.stringType();
        }

        // Check for len method on arrays, tuples, strings
        if (std.mem.eql(u8, method.method_name, "len")) {
            if (object_type == .primitive and object_type.primitive == .string_) {
                return .{ .primitive = .usize_ };
            }
            if (object_type == .array or object_type == .slice or object_type == .tuple) {
                return .{ .primitive = .usize_ };
            }
            self.addError(.undefined_method, method.span, "len() requires array, tuple, or string", .{});
            return self.type_builder.unknownType();
        }

        // Check for is_empty method on strings, arrays
        if (std.mem.eql(u8, method.method_name, "is_empty")) {
            if (object_type == .primitive and object_type.primitive == .string_) {
                return self.type_builder.boolType();
            }
            if (object_type == .array or object_type == .slice) {
                return self.type_builder.boolType();
            }
        }

        // String methods
        if (object_type == .primitive and object_type.primitive == .string_) {
            if (std.mem.eql(u8, method.method_name, "contains") or
                std.mem.eql(u8, method.method_name, "starts_with") or
                std.mem.eql(u8, method.method_name, "ends_with"))
            {
                return self.type_builder.boolType();
            }
            if (std.mem.eql(u8, method.method_name, "trim") or
                std.mem.eql(u8, method.method_name, "to_uppercase") or
                std.mem.eql(u8, method.method_name, "to_lowercase"))
            {
                return self.type_builder.stringType();
            }
            if (std.mem.eql(u8, method.method_name, "chars")) {
                // Return array of char - for now return unknown
                return self.type_builder.unknownType();
            }
            if (std.mem.eql(u8, method.method_name, "bytes")) {
                // Return array of u8 - for now return unknown
                return self.type_builder.unknownType();
            }
        }

        // Integer methods
        if (object_type.isInteger()) {
            if (std.mem.eql(u8, method.method_name, "abs") or
                std.mem.eql(u8, method.method_name, "min") or
                std.mem.eql(u8, method.method_name, "max"))
            {
                return object_type;
            }
        }

        // Array methods
        if (object_type == .array or object_type == .slice) {
            if (std.mem.eql(u8, method.method_name, "first") or
                std.mem.eql(u8, method.method_name, "last") or
                std.mem.eql(u8, method.method_name, "get"))
            {
                // Returns Optional[element_type] - for now return unknown
                return self.type_builder.unknownType();
            }
            if (std.mem.eql(u8, method.method_name, "contains")) {
                return self.type_builder.boolType();
            }
        }

        // Optional methods
        if (object_type == .optional) {
            if (std.mem.eql(u8, method.method_name, "is_some") or
                std.mem.eql(u8, method.method_name, "is_none"))
            {
                return self.type_builder.boolType();
            }
            if (std.mem.eql(u8, method.method_name, "unwrap") or
                std.mem.eql(u8, method.method_name, "unwrap_or") or
                std.mem.eql(u8, method.method_name, "expect"))
            {
                return object_type.optional.*;
            }
        }

        // Result methods
        if (object_type == .result) {
            const result_type = object_type.result;

            // is_ok() -> bool
            if (std.mem.eql(u8, method.method_name, "is_ok")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "is_ok() takes no arguments", .{});
                }
                return self.type_builder.boolType();
            }

            // is_err() -> bool
            if (std.mem.eql(u8, method.method_name, "is_err")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "is_err() takes no arguments", .{});
                }
                return self.type_builder.boolType();
            }

            // unwrap() -> T (panics on Err)
            if (std.mem.eql(u8, method.method_name, "unwrap")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "unwrap() takes no arguments", .{});
                }
                return result_type.ok_type;
            }

            // unwrap_err() -> E (panics on Ok)
            if (std.mem.eql(u8, method.method_name, "unwrap_err")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "unwrap_err() takes no arguments", .{});
                }
                return result_type.err_type;
            }

            // unwrap_or(default) -> T
            if (std.mem.eql(u8, method.method_name, "unwrap_or")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "unwrap_or() takes exactly 1 argument", .{});
                } else {
                    const arg_type = self.checkExpr(method.args[0]);
                    if (!arg_type.eql(result_type.ok_type)) {
                        self.addError(.type_mismatch, method.span, "unwrap_or argument must match Ok type", .{});
                    }
                }
                return result_type.ok_type;
            }

            // expect(msg) -> T (panics with message on Err)
            if (std.mem.eql(u8, method.method_name, "expect")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "expect() takes exactly 1 argument", .{});
                }
                return result_type.ok_type;
            }

            // ok() -> ?T (converts to Optional)
            if (std.mem.eql(u8, method.method_name, "ok")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "ok() takes no arguments", .{});
                }
                return self.type_builder.optionalType(result_type.ok_type) catch self.type_builder.unknownType();
            }

            // err() -> ?E (converts to Optional)
            if (std.mem.eql(u8, method.method_name, "err")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "err() takes no arguments", .{});
                }
                return self.type_builder.optionalType(result_type.err_type) catch self.type_builder.unknownType();
            }
        }

        // Rc methods
        if (object_type == .rc) {
            const inner_type = object_type.rc.inner;

            // clone() -> Rc[T] (increments reference count)
            if (std.mem.eql(u8, method.method_name, "clone")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "clone() takes no arguments", .{});
                }
                return object_type; // Returns same Rc[T] type
            }

            // downgrade() -> Weak[T] (creates weak reference)
            if (std.mem.eql(u8, method.method_name, "downgrade")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "downgrade() takes no arguments", .{});
                }
                return self.type_builder.weakRcType(inner_type) catch self.type_builder.unknownType();
            }

            // strong_count() -> usize
            if (std.mem.eql(u8, method.method_name, "strong_count")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "strong_count() takes no arguments", .{});
                }
                return .{ .primitive = .usize_ };
            }

            // weak_count() -> usize
            if (std.mem.eql(u8, method.method_name, "weak_count")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "weak_count() takes no arguments", .{});
                }
                return .{ .primitive = .usize_ };
            }

            // To access the inner value of Rc, use Rc.new(Cell.new(value))
            // then call cell.get() and cell.set() for interior mutability
        }

        // Weak methods
        if (object_type == .weak_rc) {
            const inner_type = object_type.weak_rc.inner;

            // clone() -> Weak[T] (increments weak count)
            if (std.mem.eql(u8, method.method_name, "clone")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "clone() takes no arguments", .{});
                }
                return object_type; // Returns same Weak[T] type
            }

            // upgrade() -> ?Rc[T] (attempts to get strong reference)
            if (std.mem.eql(u8, method.method_name, "upgrade")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "upgrade() takes no arguments", .{});
                }
                const rc_type = self.type_builder.rcType(inner_type) catch return self.type_builder.unknownType();
                return self.type_builder.optionalType(rc_type) catch self.type_builder.unknownType();
            }

            // strong_count() -> usize
            if (std.mem.eql(u8, method.method_name, "strong_count")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "strong_count() takes no arguments", .{});
                }
                return .{ .primitive = .usize_ };
            }

            // weak_count() -> usize
            if (std.mem.eql(u8, method.method_name, "weak_count")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "weak_count() takes no arguments", .{});
                }
                return .{ .primitive = .usize_ };
            }
        }

        // Arc methods (thread-safe atomic reference counting)
        if (object_type == .arc) {
            const inner_type = object_type.arc.inner;

            // clone() -> Arc[T] (atomically increments reference count)
            if (std.mem.eql(u8, method.method_name, "clone")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "clone() takes no arguments", .{});
                }
                return object_type; // Returns same Arc[T] type
            }

            // downgrade() -> WeakArc[T] (creates weak reference)
            if (std.mem.eql(u8, method.method_name, "downgrade")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "downgrade() takes no arguments", .{});
                }
                return self.type_builder.weakArcType(inner_type) catch self.type_builder.unknownType();
            }

            // strong_count() -> usize
            if (std.mem.eql(u8, method.method_name, "strong_count")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "strong_count() takes no arguments", .{});
                }
                return .{ .primitive = .usize_ };
            }

            // weak_count() -> usize
            if (std.mem.eql(u8, method.method_name, "weak_count")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "weak_count() takes no arguments", .{});
                }
                return .{ .primitive = .usize_ };
            }

            // To access the inner value of Arc, use Arc.new(Cell.new(value))
            // then call cell.get() and cell.set() for interior mutability
        }

        // WeakArc methods (thread-safe weak references)
        if (object_type == .weak_arc) {
            const inner_type = object_type.weak_arc.inner;

            // clone() -> WeakArc[T] (atomically increments weak count)
            if (std.mem.eql(u8, method.method_name, "clone")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "clone() takes no arguments", .{});
                }
                return object_type; // Returns same WeakArc[T] type
            }

            // upgrade() -> ?Arc[T] (attempts to atomically get strong reference)
            if (std.mem.eql(u8, method.method_name, "upgrade")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "upgrade() takes no arguments", .{});
                }
                const arc_type = self.type_builder.arcType(inner_type) catch return self.type_builder.unknownType();
                return self.type_builder.optionalType(arc_type) catch self.type_builder.unknownType();
            }

            // strong_count() -> usize
            if (std.mem.eql(u8, method.method_name, "strong_count")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "strong_count() takes no arguments", .{});
                }
                return .{ .primitive = .usize_ };
            }

            // weak_count() -> usize
            if (std.mem.eql(u8, method.method_name, "weak_count")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "weak_count() takes no arguments", .{});
                }
                return .{ .primitive = .usize_ };
            }
        }

        // Cell methods for interior mutability
        if (object_type == .cell) {
            const inner_type = object_type.cell.inner;

            // get() -> T (returns a copy of the inner value)
            if (std.mem.eql(u8, method.method_name, "get")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "get() takes no arguments", .{});
                }
                return inner_type;
            }

            // set(value: T) -> void (sets the inner value)
            if (std.mem.eql(u8, method.method_name, "set")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "set() expects exactly 1 argument", .{});
                    return self.type_builder.voidType();
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (!arg_type.eql(inner_type)) {
                    self.addError(.type_mismatch, method.span, "set() argument type mismatch", .{});
                }
                return self.type_builder.voidType();
            }

            // replace(value: T) -> T (sets new value, returns old value)
            if (std.mem.eql(u8, method.method_name, "replace")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "replace() expects exactly 1 argument", .{});
                    return inner_type;
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (!arg_type.eql(inner_type)) {
                    self.addError(.type_mismatch, method.span, "replace() argument type mismatch", .{});
                }
                return inner_type;
            }
        }

        // Look up user-defined method on struct types
        if (object_type == .struct_) {
            const struct_type = object_type.struct_;
            if (self.lookupStructMethod(struct_type.name, method.method_name)) |struct_method| {
                // Check argument count (excluding self if method has it)
                const expected_args = if (struct_method.has_self)
                    struct_method.func_type.function.params.len - 1
                else
                    struct_method.func_type.function.params.len;

                if (method.args.len != expected_args) {
                    self.addError(.invalid_call, method.span, "method expects {d} argument(s), got {d}", .{ expected_args, method.args.len });
                }

                // Type check arguments
                const param_start: usize = if (struct_method.has_self) 1 else 0;
                for (method.args, 0..) |arg, i| {
                    const arg_type = self.checkExpr(arg);
                    const param_idx = param_start + i;
                    if (param_idx < struct_method.func_type.function.params.len) {
                        const expected_type = struct_method.func_type.function.params[param_idx];
                        // Skip type var parameters for now (generic methods)
                        if (expected_type != .type_var and !arg_type.eql(expected_type)) {
                            self.addError(.type_mismatch, method.span, "argument type mismatch", .{});
                        }
                    }
                }

                // If the struct is a monomorphized generic struct, substitute type params in return type
                // Check if this is a monomorphized struct (has $ in name)
                if (std.mem.indexOf(u8, struct_type.name, "$")) |_| {
                    self.recordMethodMonomorphization(struct_type, struct_method) catch {};

                    // Find the monomorphized struct info to get the type substitutions
                    for (self.monomorphized_structs.items) |mono| {
                        if (std.mem.eql(u8, mono.mangled_name, struct_type.name)) {
                            // Build substitution map from impl type params to concrete types
                            var substitutions = std.AutoHashMapUnmanaged(u32, Type){};
                            defer substitutions.deinit(self.allocator);

                            // Map method's impl type params to the struct's concrete type args
                            for (struct_method.impl_type_params, 0..) |type_param, i| {
                                if (i < mono.type_args.len) {
                                    substitutions.put(self.allocator, type_param.id, mono.type_args[i]) catch {};
                                }
                            }

                            // Substitute in the return type
                            const return_type = struct_method.func_type.function.return_type;
                            const concrete_return = self.substituteTypeParams(return_type, substitutions) catch return_type;
                            return concrete_return;
                        }
                    }
                }

                return struct_method.func_type.function.return_type;
            }
        }

        self.addError(.undefined_method, method.span, "method '{s}' not found", .{method.method_name});
        return self.type_builder.unknownType();
    }

    /// Look up a method by name for a given struct type.
    /// Handles both direct struct names and monomorphized struct names (e.g., "Pair$i32$string").
    pub fn lookupStructMethod(self: *const TypeChecker, struct_name: []const u8, method_name: []const u8) ?StructMethod {
        // First try direct lookup
        if (self.struct_methods.get(struct_name)) |methods| {
            for (methods.items) |m| {
                if (std.mem.eql(u8, m.name, method_name)) {
                    return m;
                }
            }
        }

        // If this is a monomorphized struct name (contains $), try the original name
        if (std.mem.indexOf(u8, struct_name, "$")) |dollar_idx| {
            const original_name = struct_name[0..dollar_idx];
            if (self.struct_methods.get(original_name)) |methods| {
                for (methods.items) |m| {
                    if (std.mem.eql(u8, m.name, method_name)) {
                        return m;
                    }
                }
            }
        }

        return null;
    }

    /// Record a monomorphized method instance for a generic struct.
    fn recordMethodMonomorphization(self: *TypeChecker, struct_type: *types.StructType, method: StructMethod) !void {
        // Find the corresponding monomorphized struct to get type args
        var type_args: []const Type = &.{};
        for (self.monomorphized_structs.items) |ms| {
            if (std.mem.eql(u8, ms.mangled_name, struct_type.name)) {
                type_args = ms.type_args;
                break;
            }
        }

        // Check if this method instantiation already exists
        for (self.monomorphized_methods.items) |existing| {
            if (std.mem.eql(u8, existing.mangled_name, struct_type.name) and
                std.mem.eql(u8, existing.method_name, method.name))
            {
                // Already recorded
                return;
            }
        }

        // Create mangled method name: Pair$i32$string_get_first
        var mangled_name = std.ArrayListUnmanaged(u8){};
        errdefer mangled_name.deinit(self.allocator);
        try mangled_name.appendSlice(self.allocator, struct_type.name);
        try mangled_name.append(self.allocator, '_');
        try mangled_name.appendSlice(self.allocator, method.name);

        // Substitute type params in the method's function type
        var substitutions = std.AutoHashMapUnmanaged(u32, Type){};
        defer substitutions.deinit(self.allocator);

        for (method.impl_type_params, 0..) |type_param, i| {
            if (i < type_args.len) {
                try substitutions.put(self.allocator, type_param.id, type_args[i]);
            }
        }

        const concrete_func_type = try self.substituteTypeParams(method.func_type, substitutions);

        try self.monomorphized_methods.append(self.allocator, .{
            .struct_name = if (std.mem.indexOf(u8, struct_type.name, "$")) |idx| struct_type.name[0..idx] else struct_type.name,
            .method_name = method.name,
            .mangled_name = try mangled_name.toOwnedSlice(self.allocator),
            .type_args = try self.allocator.dupe(Type, type_args),
            .concrete_type = concrete_func_type,
            .original_decl = method.decl,
        });
    }

    fn checkIfExpr(self: *TypeChecker, if_expr: *ast.IfExpr) Type {
        const cond_type = self.checkExpr(if_expr.condition);
        if (!self.isBoolType(cond_type)) {
            self.addError(.type_mismatch, if_expr.condition.span(), "if condition must be bool", .{});
        }

        const then_type = self.checkExpr(if_expr.then_branch);

        if (if_expr.else_branch) |else_branch| {
            const else_type = self.checkExpr(else_branch);
            if (!then_type.eql(else_type)) {
                self.addError(.type_mismatch, if_expr.span, "if branches must have same type", .{});
            }
            return then_type;
        }

        // No else branch: return void or optional
        return self.type_builder.voidType();
    }

    fn checkMatchExpr(self: *TypeChecker, match: *ast.MatchExpr) Type {
        const subject_type = self.checkExpr(match.subject);
        var result_type: ?Type = null;

        for (match.arms) |arm| {
            // Check pattern against subject type
            self.checkPattern(arm.pattern, subject_type);

            // Check guard if present
            if (arm.guard) |guard| {
                const guard_type = self.checkExpr(guard);
                if (!self.isBoolType(guard_type)) {
                    self.addError(.type_mismatch, guard.span(), "match guard must be bool", .{});
                }
            }

            // Check arm body
            const arm_type = self.checkExpr(arm.body);

            if (result_type) |rt| {
                if (!rt.eql(arm_type)) {
                    self.addError(.type_mismatch, arm.span, "match arms must have same type", .{});
                }
            } else {
                result_type = arm_type;
            }
        }

        return result_type orelse self.type_builder.neverType();
    }

    fn checkBlock(self: *TypeChecker, block: *ast.Block) Type {
        _ = self.pushScope(.block) catch return self.type_builder.unknownType();
        defer self.popScope();

        for (block.statements) |stmt| {
            self.checkStmt(stmt);
        }

        if (block.final_expr) |final| {
            return self.checkExpr(final);
        }

        return self.type_builder.voidType();
    }

    fn checkClosure(self: *TypeChecker, closure: *ast.Closure) Type {
        const new_scope = self.pushScope(.function) catch return self.type_builder.unknownType();
        defer self.popScope();

        // Set up capture tracking for this closure
        const old_closure_scope = self.closure_scope;
        self.closure_scope = new_scope;
        self.closure_captures.clearRetainingCapacity();
        defer {
            self.closure_scope = old_closure_scope;
            // Note: captures are extracted before this defer runs
        }

        var param_types: std.ArrayListUnmanaged(Type) = .{};
        defer param_types.deinit(self.allocator);

        for (closure.params) |param| {
            const param_type = if (param.type_) |t|
                self.resolveTypeExpr(t) catch self.type_builder.unknownType()
            else
                self.type_builder.unknownType();

            param_types.append(self.allocator, param_type) catch {};

            self.current_scope.define(.{
                .name = param.name,
                .type_ = param_type,
                .kind = .parameter,
                .mutable = false,
                .span = param.span,
            }) catch {};
        }

        const return_type = if (closure.return_type) |rt|
            self.resolveTypeExpr(rt) catch self.type_builder.unknownType()
        else
            self.type_builder.unknownType();

        const old_return_type = self.current_return_type;
        self.current_return_type = return_type;
        defer self.current_return_type = old_return_type;

        const body_type = self.checkExpr(closure.body);

        // Infer return type if not specified
        const actual_return = if (closure.return_type == null) body_type else return_type;

        // Store captured variables in the AST node
        // Note: This allocation is intentionally not freed here as it becomes
        // part of the AST and is used during code generation. The memory will
        // be freed when the allocator is cleaned up (arena-style).
        if (self.closure_captures.count() > 0) {
            var captures = std.ArrayListUnmanaged(ast.CapturedVar){};
            var iter = self.closure_captures.iterator();
            while (iter.next()) |entry| {
                captures.append(self.allocator, .{
                    .name = entry.key_ptr.*,
                    .is_mutable = entry.value_ptr.is_mutable,
                }) catch {};
            }
            closure.captures = captures.toOwnedSlice(self.allocator) catch null;
        }

        return self.type_builder.functionType(param_types.items, actual_return) catch self.type_builder.unknownType();
    }

    fn checkRange(self: *TypeChecker, range: *ast.Range) Type {
        var elem_type: Type = self.type_builder.i32Type();

        if (range.start) |start| {
            const start_type = self.checkExpr(start);
            if (!start_type.isInteger()) {
                self.addError(.type_mismatch, range.span, "range bounds must be integer", .{});
            }
            elem_type = start_type;
        }

        if (range.end) |end| {
            const end_type = self.checkExpr(end);
            if (!end_type.isInteger()) {
                self.addError(.type_mismatch, range.span, "range bounds must be integer", .{});
            }
            if (range.start != null and !elem_type.eql(end_type)) {
                self.addError(.type_mismatch, range.span, "range bounds must have same type", .{});
            }
        }

        // Range produces an iterator-like type
        // For now, just return slice type as placeholder
        return self.type_builder.sliceType(elem_type) catch self.type_builder.unknownType();
    }

    fn checkStructLiteral(self: *TypeChecker, lit: *ast.StructLiteral) Type {
        if (lit.type_name) |type_name| {
            const resolved = self.resolveTypeExpr(type_name) catch return self.type_builder.unknownType();
            if (resolved != .struct_) {
                self.addError(.type_mismatch, lit.span, "expected struct type", .{});
                return self.type_builder.unknownType();
            }

            // Check field types
            const struct_type = resolved.struct_;
            for (lit.fields) |field_init| {
                const field_type = self.checkExpr(field_init.value);
                var found = false;
                for (struct_type.fields) |struct_field| {
                    if (std.mem.eql(u8, struct_field.name, field_init.name)) {
                        found = true;
                        if (!field_type.eql(struct_field.type_)) {
                            self.addError(.type_mismatch, field_init.span, "field type mismatch", .{});
                        }
                        break;
                    }
                }
                if (!found) {
                    self.addError(.undefined_field, field_init.span, "unknown field '{s}'", .{field_init.name});
                }
            }

            return resolved;
        }

        // Anonymous struct literal
        self.addError(.type_mismatch, lit.span, "struct literal requires type name", .{});
        return self.type_builder.unknownType();
    }

    fn checkArrayLiteral(self: *TypeChecker, arr: *ast.ArrayLiteral) Type {
        if (arr.elements.len == 0) {
            // Empty array: type unknown without context
            return self.type_builder.unknownType();
        }

        const first_type = self.checkExpr(arr.elements[0]);
        for (arr.elements[1..]) |elem| {
            const elem_type = self.checkExpr(elem);
            if (!elem_type.eql(first_type)) {
                self.addError(.type_mismatch, elem.span(), "array elements must have same type", .{});
            }
        }

        return self.type_builder.arrayType(first_type, arr.elements.len) catch self.type_builder.unknownType();
    }

    fn checkTupleLiteral(self: *TypeChecker, tup: *ast.TupleLiteral) Type {
        var elem_types: std.ArrayListUnmanaged(Type) = .{};
        defer elem_types.deinit(self.allocator);

        for (tup.elements) |elem| {
            elem_types.append(self.allocator, self.checkExpr(elem)) catch {};
        }

        return self.type_builder.tupleType(elem_types.items) catch self.type_builder.unknownType();
    }

    fn checkTypeCast(self: *TypeChecker, cast: *ast.TypeCast) Type {
        const expr_type = self.checkExpr(cast.expr);
        const target_type = self.resolveTypeExpr(cast.target_type) catch return self.type_builder.unknownType();

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
            self.addError(.invalid_conversion, cast.span, "invalid type cast", .{});
        }

        return target_type;
    }

    /// Type check enum literal construction: EnumType::VariantName(payload)
    fn checkEnumLiteral(self: *TypeChecker, lit: *ast.EnumLiteral) Type {
        // Resolve the enum type
        const enum_type = self.resolveTypeExpr(lit.enum_type) catch return self.type_builder.unknownType();

        if (enum_type != .enum_) {
            self.addError(.type_mismatch, lit.span, "expected enum type", .{});
            return self.type_builder.unknownType();
        }

        const enum_def = enum_type.enum_;

        // Find the variant
        var found_variant: ?types.EnumVariant = null;
        for (enum_def.variants) |variant| {
            if (std.mem.eql(u8, variant.name, lit.variant_name)) {
                found_variant = variant;
                break;
            }
        }

        if (found_variant == null) {
            self.addError(.undefined_variant, lit.span, "unknown variant '{s}'", .{lit.variant_name});
            return self.type_builder.unknownType();
        }

        const variant = found_variant.?;

        // Check payload matches variant definition
        if (variant.payload) |payload| {
            switch (payload) {
                .tuple => |tuple_types| {
                    if (lit.payload.len != tuple_types.len) {
                        self.addError(.type_mismatch, lit.span, "expected {d} payload values, got {d}", .{ tuple_types.len, lit.payload.len });
                    } else {
                        for (lit.payload, tuple_types) |payload_expr, expected_type| {
                            const actual_type = self.checkExpr(payload_expr);
                            if (!actual_type.eql(expected_type)) {
                                self.addError(.type_mismatch, payload_expr.span(), "payload type mismatch", .{});
                            }
                        }
                    }
                },
                .struct_ => |fields| {
                    // For struct payloads, check each field
                    if (lit.payload.len != fields.len) {
                        self.addError(.type_mismatch, lit.span, "expected {d} struct fields, got {d}", .{ fields.len, lit.payload.len });
                    }
                    // TODO: Proper struct field matching by name
                },
            }
        } else {
            // Unit variant - no payload expected
            if (lit.payload.len > 0) {
                self.addError(.type_mismatch, lit.span, "variant '{s}' takes no payload", .{lit.variant_name});
            }
        }

        // If this is a generic enum, record the monomorphization
        if (enum_def.type_params.len > 0) {
            // Extract type arguments from the resolved enum type
            if (lit.enum_type == .generic_apply) {
                const generic = lit.enum_type.generic_apply;
                var type_args = std.ArrayListUnmanaged(Type){};
                defer type_args.deinit(self.allocator);

                for (generic.args) |arg| {
                    const resolved_arg = self.resolveTypeExpr(arg) catch continue;
                    type_args.append(self.allocator, resolved_arg) catch {};
                }

                // Record enum monomorphization - pass original enum def
                _ = self.recordEnumMonomorphization(enum_def.name, enum_def, type_args.items) catch {};
            }
        }

        return enum_type;
    }

    // ========================================================================
    // Statement Type Checking
    // ========================================================================

    pub fn checkStmt(self: *TypeChecker, stmt: ast.Stmt) void {
        switch (stmt) {
            .let_decl => |l| self.checkLetDecl(l),
            .var_decl => |v| self.checkVarDecl(v),
            .assignment => |a| self.checkAssignment(a),
            .expr_stmt => |e| _ = self.checkExpr(e.expr),
            .return_stmt => |r| self.checkReturn(r),
            .break_stmt => |b| self.checkBreak(b),
            .continue_stmt => |c| self.checkContinue(c),
            .for_loop => |f| self.checkFor(f),
            .while_loop => |w| self.checkWhile(w),
            .loop_stmt => |l| self.checkLoop(l),
        }
    }

    fn checkAssignment(self: *TypeChecker, assign: *ast.Assignment) void {
        const target_type = self.checkExpr(assign.target);
        const value_type = self.checkExpr(assign.value);

        if (!self.isAssignable(assign.target)) {
            self.addError(.invalid_assignment, assign.span, "cannot assign to this expression", .{});
        }
        if (!self.isMutable(assign.target)) {
            self.addError(.mutability_error, assign.span, "cannot assign to immutable variable", .{});
        }

        if (assign.op == .assign) {
            if (!self.checkAssignmentCompatible(target_type, value_type)) {
                self.addError(.type_mismatch, assign.span, "cannot assign different types", .{});
            }
        } else {
            // Compound assignment
            if (!target_type.isNumeric() or !value_type.isNumeric()) {
                self.addError(.invalid_operation, assign.span, "compound assignment requires numeric types", .{});
            }
        }
    }

    fn checkLetDecl(self: *TypeChecker, decl: *ast.LetDecl) void {
        const value_type = self.checkExpr(decl.value);

        const declared_type = if (decl.type_) |t|
            self.resolveTypeExpr(t) catch self.type_builder.unknownType()
        else
            value_type;

        if (decl.type_ != null and !declared_type.eql(value_type)) {
            self.addError(.type_mismatch, decl.span, "initializer type doesn't match declared type", .{});
        }

        // Check for duplicate in current scope
        if (self.current_scope.lookupLocal(decl.name) != null) {
            self.addError(.duplicate_definition, decl.span, "'{s}' already defined in this scope", .{decl.name});
            return;
        }

        self.current_scope.define(.{
            .name = decl.name,
            .type_ = declared_type,
            .kind = .variable,
            .mutable = false,
            .span = decl.span,
        }) catch {};
    }

    fn checkVarDecl(self: *TypeChecker, decl: *ast.VarDecl) void {
        const value_type = self.checkExpr(decl.value);

        const declared_type = if (decl.type_) |t|
            self.resolveTypeExpr(t) catch self.type_builder.unknownType()
        else
            value_type;

        if (decl.type_ != null and !declared_type.eql(value_type)) {
            self.addError(.type_mismatch, decl.span, "initializer type doesn't match declared type", .{});
        }

        if (self.current_scope.lookupLocal(decl.name) != null) {
            self.addError(.duplicate_definition, decl.span, "'{s}' already defined in this scope", .{decl.name});
            return;
        }

        self.current_scope.define(.{
            .name = decl.name,
            .type_ = declared_type,
            .kind = .variable,
            .mutable = true,
            .span = decl.span,
        }) catch {};
    }

    fn checkReturn(self: *TypeChecker, ret: *ast.ReturnStmt) void {
        const func_scope = self.current_scope.getFunctionScope();
        if (func_scope == null) {
            self.addError(.invalid_operation, ret.span, "return outside function", .{});
            return;
        }

        if (ret.value) |value| {
            const value_type = self.checkExpr(value);
            if (self.current_return_type) |expected| {
                if (!value_type.eql(expected)) {
                    // Allow returning T when ?T is expected (implicit Some wrapping)
                    if (expected == .optional) {
                        if (value_type.eql(expected.optional.*)) {
                            return; // OK: T can be returned as ?T (becomes Some(T))
                        }
                    }
                    self.addError(.return_type_mismatch, ret.span, "return type mismatch", .{});
                }
            }
        } else {
            if (self.current_return_type) |expected| {
                // Allow returning nothing when ?T is expected (becomes None)
                if (expected == .optional) {
                    return; // OK: implicit None return
                }
                if (expected != .void_) {
                    self.addError(.return_type_mismatch, ret.span, "missing return value", .{});
                }
            }
        }
    }

    fn checkBreak(self: *TypeChecker, brk: *ast.BreakStmt) void {
        if (!self.current_scope.isInLoop()) {
            self.addError(.break_outside_loop, brk.span, "break outside loop", .{});
        }
        if (brk.value) |value| {
            _ = self.checkExpr(value);
        }
    }

    fn checkContinue(self: *TypeChecker, cont: *ast.ContinueStmt) void {
        if (!self.current_scope.isInLoop()) {
            self.addError(.continue_outside_loop, cont.span, "continue outside loop", .{});
        }
    }

    fn checkFor(self: *TypeChecker, for_loop: *ast.ForLoop) void {
        const iter_type = self.checkExpr(for_loop.iterable);

        // Get element type from iterable
        const elem_type: Type = switch (iter_type) {
            .array => |a| a.element,
            .slice => |s| s.element,
            else => blk: {
                self.addError(.not_iterable, for_loop.span, "cannot iterate over this type", .{});
                break :blk self.type_builder.unknownType();
            },
        };

        // Create loop scope and bind pattern
        _ = self.pushScope(.loop) catch return;
        defer self.popScope();

        self.bindPattern(for_loop.pattern, elem_type);

        _ = self.checkBlock(for_loop.body);
    }

    fn checkWhile(self: *TypeChecker, while_loop: *ast.WhileLoop) void {
        const cond_type = self.checkExpr(while_loop.condition);
        if (!self.isBoolType(cond_type)) {
            self.addError(.type_mismatch, while_loop.condition.span(), "while condition must be bool", .{});
        }

        _ = self.pushScope(.loop) catch return;
        defer self.popScope();

        _ = self.checkBlock(while_loop.body);
    }

    fn checkLoop(self: *TypeChecker, loop: *ast.LoopStmt) void {
        _ = self.pushScope(.loop) catch return;
        defer self.popScope();

        _ = self.checkBlock(loop.body);
    }

    // ========================================================================
    // Declaration Type Checking
    // ========================================================================

    pub fn checkDecl(self: *TypeChecker, decl: ast.Decl) void {
        switch (decl) {
            .function => |f| self.checkFunction(f),
            .struct_decl => |s| self.checkStruct(s),
            .enum_decl => |e| self.checkEnum(e),
            .trait_decl => |t| self.checkTrait(t),
            .impl_decl => |i| self.checkImpl(i),
            .type_alias => |t| self.checkTypeAlias(t),
            .const_decl => |c| self.checkConst(c),
            .import_decl => {}, // Imports handled separately
            .module_decl => {}, // Module declarations don't need type checking
        }
    }

    fn checkFunction(self: *TypeChecker, func: *ast.FunctionDecl) void {
        // Push type parameters into scope if this is a generic function
        const has_type_params = func.type_params.len > 0;
        if (has_type_params) {
            _ = self.pushTypeParams(func.type_params) catch return;
        }
        defer if (has_type_params) self.popTypeParams();

        // Build function type
        var param_types: std.ArrayListUnmanaged(Type) = .{};
        defer param_types.deinit(self.allocator);

        for (func.params) |param| {
            const param_type = self.resolveTypeExpr(param.type_) catch self.type_builder.unknownType();
            param_types.append(self.allocator, param_type) catch {};
        }

        const return_type = if (func.return_type) |rt|
            self.resolveTypeExpr(rt) catch self.type_builder.unknownType()
        else
            self.type_builder.voidType();

        const func_type = self.type_builder.functionType(param_types.items, return_type) catch {
            return;
        };

        // Register function in current scope
        self.current_scope.define(.{
            .name = func.name,
            .type_ = func_type,
            .kind = .function,
            .mutable = false,
            .span = func.span,
        }) catch {};

        // Check function body if present
        if (func.body) |body| {
            _ = self.pushScope(.function) catch return;
            defer self.popScope();

            // Bind parameters
            for (func.params) |param| {
                const param_type = self.resolveTypeExpr(param.type_) catch self.type_builder.unknownType();
                self.current_scope.define(.{
                    .name = param.name,
                    .type_ = param_type,
                    .kind = .parameter,
                    .mutable = false,
                    .span = param.span,
                }) catch {};
            }

            self.current_return_type = return_type;
            defer self.current_return_type = null;

            _ = self.checkBlock(body);
        }
    }

    fn checkStruct(self: *TypeChecker, struct_decl: *ast.StructDecl) void {
        // Push type parameters into scope if this is a generic struct
        const has_type_params = struct_decl.type_params.len > 0;
        var struct_type_params: []const types.TypeVar = &.{};
        if (has_type_params) {
            struct_type_params = self.pushTypeParams(struct_decl.type_params) catch return;
        }
        defer if (has_type_params) self.popTypeParams();

        var fields: std.ArrayListUnmanaged(types.StructField) = .{};
        defer fields.deinit(self.allocator);

        for (struct_decl.fields) |field| {
            const field_type = self.resolveTypeExpr(field.type_) catch self.type_builder.unknownType();
            fields.append(self.allocator, .{
                .name = field.name,
                .type_ = field_type,
                .is_pub = field.is_pub,
            }) catch {};
        }

        // Create struct type
        const struct_type = self.allocator.create(types.StructType) catch return;
        struct_type.* = .{
            .name = struct_decl.name,
            .type_params = struct_type_params,
            .fields = fields.toOwnedSlice(self.allocator) catch &.{},
            .traits = &.{},
        };

        // Track for cleanup in deinit
        self.generic_struct_types.append(self.allocator, struct_type) catch {};

        self.current_scope.define(.{
            .name = struct_decl.name,
            .type_ = .{ .struct_ = struct_type },
            .kind = .type_,
            .mutable = false,
            .span = struct_decl.span,
        }) catch {};
    }

    fn checkEnum(self: *TypeChecker, enum_decl: *ast.EnumDecl) void {
        // Push type parameters into scope if this is a generic enum
        const has_type_params = enum_decl.type_params.len > 0;
        var enum_type_params: []const types.TypeVar = &.{};
        if (has_type_params) {
            enum_type_params = self.pushTypeParams(enum_decl.type_params) catch return;
        }
        defer if (has_type_params) self.popTypeParams();

        var variants: std.ArrayListUnmanaged(types.EnumVariant) = .{};
        defer variants.deinit(self.allocator);

        for (enum_decl.variants) |variant| {
            const payload: ?types.VariantPayload = if (variant.payload) |p| blk: {
                break :blk switch (p) {
                    .tuple => |tuple_types| tup: {
                        var resolved_types: std.ArrayListUnmanaged(Type) = .{};
                        defer resolved_types.deinit(self.allocator);
                        for (tuple_types) |t| {
                            resolved_types.append(self.allocator, self.resolveTypeExpr(t) catch self.type_builder.unknownType()) catch {};
                        }
                        break :tup .{ .tuple = resolved_types.toOwnedSlice(self.allocator) catch &.{} };
                    },
                    .struct_ => |struct_fields| str: {
                        var resolved_fields: std.ArrayListUnmanaged(types.StructField) = .{};
                        defer resolved_fields.deinit(self.allocator);
                        for (struct_fields) |f| {
                            resolved_fields.append(self.allocator, .{
                                .name = f.name,
                                .type_ = self.resolveTypeExpr(f.type_) catch self.type_builder.unknownType(),
                                .is_pub = f.is_pub,
                            }) catch {};
                        }
                        break :str .{ .struct_ = resolved_fields.toOwnedSlice(self.allocator) catch &.{} };
                    },
                };
            } else null;

            variants.append(self.allocator, .{
                .name = variant.name,
                .payload = payload,
            }) catch {};
        }

        const enum_type = self.allocator.create(types.EnumType) catch return;
        enum_type.* = .{
            .name = enum_decl.name,
            .type_params = enum_type_params,
            .variants = variants.toOwnedSlice(self.allocator) catch &.{},
        };

        // Track for cleanup in deinit
        self.generic_enum_types.append(self.allocator, enum_type) catch {};

        self.current_scope.define(.{
            .name = enum_decl.name,
            .type_ = .{ .enum_ = enum_type },
            .kind = .type_,
            .mutable = false,
            .span = enum_decl.span,
        }) catch {};
    }

    fn checkTrait(self: *TypeChecker, trait_decl: *ast.TraitDecl) void {
        // Check for duplicate trait definition
        if (self.trait_registry.get(trait_decl.name) != null) {
            self.addError(.duplicate_definition, trait_decl.span, "duplicate trait definition '{s}'", .{trait_decl.name});
            return;
        }

        // Push type parameters into scope if this is a generic trait
        const has_type_params = trait_decl.type_params.len > 0;
        var trait_type_params: []const types.TypeVar = &.{};
        if (has_type_params) {
            trait_type_params = self.pushTypeParams(trait_decl.type_params) catch return;
        }
        defer if (has_type_params) self.popTypeParams();

        // Build trait methods
        var trait_methods: std.ArrayListUnmanaged(types.TraitMethod) = .{};
        defer trait_methods.deinit(self.allocator);

        var seen_method_names: std.StringHashMapUnmanaged(void) = .{};
        defer seen_method_names.deinit(self.allocator);

        for (trait_decl.methods) |method_decl| {
            // Check for duplicate method names
            if (seen_method_names.get(method_decl.name) != null) {
                self.addError(.duplicate_definition, method_decl.span, "duplicate method '{s}' in trait", .{method_decl.name});
                continue;
            }
            seen_method_names.put(self.allocator, method_decl.name, {}) catch {};

            // Build method signature
            var param_types: std.ArrayListUnmanaged(Type) = .{};
            defer param_types.deinit(self.allocator);

            for (method_decl.params) |param| {
                // Handle 'self' parameter - it represents the implementing type
                if (std.mem.eql(u8, param.name, "self") or std.mem.eql(u8, param.name, "Self")) {
                    // For 'self', we'll use a placeholder type that gets substituted during impl
                    // For now, use unknown type as a marker
                    param_types.append(self.allocator, self.type_builder.unknownType()) catch {};
                } else {
                    const param_type = self.resolveTypeExpr(param.type_) catch self.type_builder.unknownType();
                    param_types.append(self.allocator, param_type) catch {};
                }
            }

            const return_type = if (method_decl.return_type) |rt|
                self.resolveTypeExpr(rt) catch self.type_builder.unknownType()
            else
                self.type_builder.voidType();

            const func_type = self.type_builder.functionType(param_types.items, return_type) catch {
                continue;
            };

            // Check if method has a default implementation (has a body)
            const has_default = method_decl.body != null;

            // If there's a default implementation, type-check it
            if (method_decl.body) |body| {
                _ = self.pushScope(.function) catch continue;
                defer self.popScope();

                // Bind parameters
                for (method_decl.params, 0..) |param, idx| {
                    const param_type = if (idx < param_types.items.len) param_types.items[idx] else self.type_builder.unknownType();
                    self.current_scope.define(.{
                        .name = param.name,
                        .type_ = param_type,
                        .kind = .parameter,
                        .mutable = false,
                        .span = param.span,
                    }) catch {};
                }

                self.current_return_type = return_type;
                defer self.current_return_type = null;

                _ = self.checkBlock(body);
            }

            trait_methods.append(self.allocator, .{
                .name = method_decl.name,
                .signature = func_type.function.*,
                .has_default = has_default,
            }) catch {};
        }

        // Create and register the trait type
        const trait_type = self.allocator.create(types.TraitType) catch return;
        trait_type.* = .{
            .name = trait_decl.name,
            .type_params = trait_type_params,
            .methods = trait_methods.toOwnedSlice(self.allocator) catch &.{},
            .super_traits = &.{}, // TODO: Support trait inheritance
        };

        // Track for cleanup
        self.trait_types.append(self.allocator, trait_type) catch {};

        // Register in trait registry
        self.trait_registry.put(self.allocator, trait_decl.name, .{
            .trait_type = trait_type,
            .decl = @constCast(trait_decl),
        }) catch {};

        // Register in symbol table as a trait
        self.current_scope.define(.{
            .name = trait_decl.name,
            .type_ = .{ .trait_ = trait_type },
            .kind = .trait_,
            .mutable = false,
            .span = trait_decl.span,
        }) catch {};
    }

    fn checkImpl(self: *TypeChecker, impl_decl: *ast.ImplDecl) void {
        // Push type parameters into scope if this is a generic impl
        const has_type_params = impl_decl.type_params.len > 0;
        var impl_type_params: []const types.TypeVar = &.{};
        if (has_type_params) {
            impl_type_params = self.pushTypeParams(impl_decl.type_params) catch return;
        }
        defer if (has_type_params) self.popTypeParams();

        // Resolve the target type (e.g., Pair[A, B] or Point)
        const target_type = self.resolveTypeExpr(impl_decl.target_type) catch {
            self.addError(.undefined_type, impl_decl.span, "cannot resolve impl target type", .{});
            return;
        };

        // Get the struct name - we only support impl blocks for structs currently
        // For generic impls like impl[T] Pair[T], the target_type is an applied type
        const struct_name = switch (target_type) {
            .struct_ => |s| s.name,
            .applied => |a| blk: {
                // For generic impls, the base should be a struct
                if (a.base == .struct_) {
                    break :blk a.base.struct_.name;
                }
                self.addError(.invalid_operation, impl_decl.span, "impl blocks currently only support structs", .{});
                return;
            },
            else => {
                self.addError(.invalid_operation, impl_decl.span, "impl blocks currently only support structs", .{});
                return;
            },
        };

        // Handle trait implementations (impl Type: Trait { ... })
        var trait_info: ?TraitInfo = null;
        if (impl_decl.trait_type) |trait_type_expr| {
            // Resolve the trait type
            const resolved_trait = self.resolveTypeExpr(trait_type_expr) catch {
                self.addError(.undefined_type, impl_decl.span, "cannot resolve trait type", .{});
                return;
            };

            // Must be a trait
            if (resolved_trait != .trait_) {
                self.addError(.type_mismatch, impl_decl.span, "expected trait type", .{});
                return;
            }

            // Look up the trait in the registry
            const trait_name = resolved_trait.trait_.name;
            trait_info = self.trait_registry.get(trait_name);
            if (trait_info == null) {
                self.addError(.undefined_type, impl_decl.span, "trait '{s}' not found", .{trait_name});
                return;
            }
        }

        // Process each method in the impl block
        for (impl_decl.methods) |*method_decl_const| {
            // Need to cast to mutable pointer for registration
            const method_decl = @constCast(method_decl_const);

            // Determine if this method has 'self' as first parameter
            var has_self = false;
            var self_is_mutable = false;
            var param_start_idx: usize = 0;

            if (method_decl.params.len > 0) {
                const first_param = method_decl.params[0];
                if (std.mem.eql(u8, first_param.name, "self")) {
                    has_self = true;
                    param_start_idx = 1;
                    // Check if self is mutable (type would be &mut Self or similar)
                    // For now, we'll mark it based on the type expression
                    if (first_param.type_ == .reference) {
                        self_is_mutable = first_param.type_.reference.mutable;
                    }
                }
            }

            // Build the method's function type
            var param_types: std.ArrayListUnmanaged(Type) = .{};
            defer param_types.deinit(self.allocator);

            // Include all parameters (including self) in the function type
            for (method_decl.params) |param| {
                // For 'self' parameter, use the target struct type
                if (std.mem.eql(u8, param.name, "self")) {
                    // self could be: self, &self, &mut self
                    // For now, treat it as the struct type itself
                    param_types.append(self.allocator, target_type) catch {};
                } else {
                    const param_type = self.resolveTypeExpr(param.type_) catch self.type_builder.unknownType();
                    param_types.append(self.allocator, param_type) catch {};
                }
            }

            const return_type = if (method_decl.return_type) |rt|
                self.resolveTypeExpr(rt) catch self.type_builder.unknownType()
            else
                self.type_builder.voidType();

            const func_type = self.type_builder.functionType(param_types.items, return_type) catch {
                continue;
            };

            // Register the method in the struct_methods registry
            const result = self.struct_methods.getOrPut(self.allocator, struct_name) catch continue;
            if (!result.found_existing) {
                result.value_ptr.* = .{};
            }

            // Check for duplicate method
            var is_duplicate = false;
            for (result.value_ptr.items) |existing| {
                if (std.mem.eql(u8, existing.name, method_decl.name)) {
                    self.addError(.duplicate_definition, method_decl.span, "duplicate method definition", .{});
                    is_duplicate = true;
                    break;
                }
            }

            if (!is_duplicate) {
                // Duplicate the impl_type_params slice to own it
                const owned_type_params = self.allocator.dupe(types.TypeVar, impl_type_params) catch &.{};

                result.value_ptr.append(self.allocator, .{
                    .name = method_decl.name,
                    .decl = method_decl,
                    .impl_type_params = owned_type_params,
                    .func_type = func_type,
                    .has_self = has_self,
                    .self_is_mutable = self_is_mutable,
                }) catch {};
            }

            // Type-check the method body
            if (method_decl.body) |body| {
                _ = self.pushScope(.function) catch continue;
                defer self.popScope();

                // Bind parameters (starting from param_start_idx to skip self for binding purposes)
                // Actually, we should bind self too with the proper type
                for (method_decl.params, 0..) |param, idx| {
                    const param_type = if (idx < param_types.items.len) param_types.items[idx] else self.type_builder.unknownType();
                    self.current_scope.define(.{
                        .name = param.name,
                        .type_ = param_type,
                        .kind = .parameter,
                        .mutable = if (std.mem.eql(u8, param.name, "self")) self_is_mutable else false,
                        .span = param.span,
                    }) catch {};
                }

                self.current_return_type = return_type;
                defer self.current_return_type = null;

                _ = self.checkBlock(body);
            }
        }

        // If this is a trait implementation, verify all required methods are implemented
        if (trait_info) |info| {
            const trait_type = info.trait_type;

            // Get the list of implemented methods for this struct
            const impl_methods = self.struct_methods.get(struct_name);

            for (trait_type.methods) |trait_method| {
                // Check if method is implemented
                var found = false;
                if (impl_methods) |methods_list| {
                    for (methods_list.items) |impl_method| {
                        if (std.mem.eql(u8, impl_method.name, trait_method.name)) {
                            found = true;
                            // TODO: Verify method signature matches trait signature
                            break;
                        }
                    }
                }

                if (!found and !trait_method.has_default) {
                    self.addError(.trait_not_implemented, impl_decl.span, "missing implementation for required trait method '{s}'", .{trait_method.name});
                }
            }

            // Register this trait implementation
            const impl_key = self.makeTraitImplKey(struct_name, trait_type.name);
            const impl_result = self.trait_impls.getOrPut(self.allocator, impl_key) catch return;
            if (!impl_result.found_existing) {
                impl_result.value_ptr.* = .{};
            }

            impl_result.value_ptr.append(self.allocator, .{
                .trait_name = trait_type.name,
                .impl_type_name = struct_name,
                .impl_type_params = impl_type_params,
                .methods = if (impl_methods) |m| m.items else &.{},
            }) catch {};
        }
    }

    /// Create a unique key for trait implementation lookup
    fn makeTraitImplKey(self: *TypeChecker, type_name: []const u8, trait_name: []const u8) []const u8 {
        return std.fmt.allocPrint(self.allocator, "{s}:{s}", .{ type_name, trait_name }) catch "";
    }

    /// Check if a type implements a trait
    pub fn typeImplementsTrait(self: *TypeChecker, type_name: []const u8, trait_name: []const u8) bool {
        const key = self.makeTraitImplKey(type_name, trait_name);
        defer self.allocator.free(key);
        return self.trait_impls.get(key) != null;
    }

    /// Check if a concrete type satisfies all trait bounds.
    /// Returns true if the type satisfies all bounds, false otherwise.
    pub fn typeSatisfiesBounds(self: *TypeChecker, concrete_type: Type, bounds: []const *types.TraitType, span: Span) bool {
        if (bounds.len == 0) return true;

        // Get the type name for trait lookup
        const type_name: ?[]const u8 = switch (concrete_type) {
            .struct_ => |s| s.name,
            .enum_ => |e| e.name,
            .primitive => |p| @tagName(p),
            else => null,
        };

        if (type_name == null) {
            // Complex types (functions, references, etc.) don't support traits yet
            return true;
        }

        for (bounds) |trait| {
            if (!self.typeImplementsTrait(type_name.?, trait.name)) {
                self.addError(.trait_not_implemented, span, "type '{s}' does not implement trait '{s}'", .{ type_name.?, trait.name });
                return false;
            }
        }

        return true;
    }

    fn checkTypeAlias(self: *TypeChecker, alias: *ast.TypeAlias) void {
        const target_type = self.resolveTypeExpr(alias.target) catch return;

        self.current_scope.define(.{
            .name = alias.name,
            .type_ = target_type,
            .kind = .type_,
            .mutable = false,
            .span = alias.span,
        }) catch {};
    }

    fn checkConst(self: *TypeChecker, const_decl: *ast.ConstDecl) void {
        const value_type = self.checkExpr(const_decl.value);

        const declared_type = if (const_decl.type_) |t|
            self.resolveTypeExpr(t) catch self.type_builder.unknownType()
        else
            value_type;

        if (const_decl.type_ != null and !declared_type.eql(value_type)) {
            self.addError(.type_mismatch, const_decl.span, "constant type mismatch", .{});
        }

        self.current_scope.define(.{
            .name = const_decl.name,
            .type_ = declared_type,
            .kind = .constant,
            .mutable = false,
            .span = const_decl.span,
        }) catch {};
    }

    // ========================================================================
    // Pattern Checking
    // ========================================================================

    fn checkPattern(self: *TypeChecker, pattern: ast.Pattern, expected_type: Type) void {
        switch (pattern) {
            .wildcard => {}, // Always matches
            .literal => |lit| {
                const lit_type = self.checkLiteralPattern(lit);
                if (!lit_type.eql(expected_type)) {
                    self.addError(.type_mismatch, lit.span, "pattern type mismatch", .{});
                }
            },
            .binding => |bind| {
                self.current_scope.define(.{
                    .name = bind.name,
                    .type_ = expected_type,
                    .kind = .variable,
                    .mutable = bind.mutable,
                    .span = bind.span,
                }) catch {};
            },
            .variant => |v| {
                // Determine the enum type - either from pattern or from expected type
                var enum_type = expected_type;

                if (v.type_expr) |type_expr| {
                    // Pattern specifies a type - resolve it
                    enum_type = self.resolveTypeExpr(type_expr) catch {
                        self.addError(.undefined_type, v.span, "unknown type in pattern", .{});
                        return;
                    };
                }

                if (enum_type != .enum_) {
                    self.addError(.invalid_pattern, v.span, "variant pattern requires enum type", .{});
                    return;
                }

                const enum_def = enum_type.enum_;

                // Find the variant
                var found_variant: ?types.EnumVariant = null;
                for (enum_def.variants) |variant| {
                    if (std.mem.eql(u8, variant.name, v.variant_name)) {
                        found_variant = variant;
                        break;
                    }
                }

                if (found_variant == null) {
                    self.addError(.undefined_variant, v.span, "unknown variant '{s}'", .{v.variant_name});
                    return;
                }

                const variant = found_variant.?;

                // Check and bind payload pattern
                if (variant.payload) |payload| {
                    if (v.payload) |payload_pattern| {
                        switch (payload) {
                            .tuple => |tuple_types| {
                                if (tuple_types.len == 1) {
                                    // Single-element tuple: bind directly
                                    self.checkPattern(payload_pattern, tuple_types[0]);
                                } else {
                                    // Multi-element tuple: expect tuple pattern
                                    const payload_type = self.type_builder.tupleType(tuple_types) catch self.type_builder.unknownType();
                                    self.checkPattern(payload_pattern, payload_type);
                                }
                            },
                            .struct_ => |fields| {
                                // Build struct type for the payload
                                // For now, treat as unknown - struct patterns need more work
                                _ = fields;
                                self.checkPattern(payload_pattern, self.type_builder.unknownType());
                            },
                        }
                    } else {
                        self.addError(.invalid_pattern, v.span, "variant '{s}' expects payload", .{v.variant_name});
                    }
                } else {
                    // Unit variant - no payload expected
                    if (v.payload != null) {
                        self.addError(.invalid_pattern, v.span, "variant '{s}' takes no payload", .{v.variant_name});
                    }
                }

                // If this is a generic enum from pattern, record monomorphization
                if (v.type_expr) |type_expr| {
                    if (type_expr == .generic_apply and enum_def.type_params.len > 0) {
                        const generic = type_expr.generic_apply;
                        var type_args = std.ArrayListUnmanaged(Type){};
                        defer type_args.deinit(self.allocator);

                        for (generic.args) |arg| {
                            const resolved_arg = self.resolveTypeExpr(arg) catch continue;
                            type_args.append(self.allocator, resolved_arg) catch {};
                        }

                        // Record enum monomorphization
                        _ = self.recordEnumMonomorphization(enum_def.name, enum_def, type_args.items) catch {};
                    }
                }
            },
            .struct_pattern => |s| {
                if (expected_type != .struct_) {
                    self.addError(.invalid_pattern, s.span, "struct pattern requires struct type", .{});
                    return;
                }
                // TODO: check fields exist and match
            },
            .tuple_pattern => |t| {
                if (expected_type != .tuple) {
                    self.addError(.invalid_pattern, t.span, "tuple pattern requires tuple type", .{});
                    return;
                }
                const tuple_type = expected_type.tuple;
                if (t.elements.len != tuple_type.elements.len) {
                    self.addError(.invalid_pattern, t.span, "wrong number of tuple elements", .{});
                    return;
                }
                for (t.elements, tuple_type.elements) |elem_pattern, elem_type| {
                    self.checkPattern(elem_pattern, elem_type);
                }
            },
            .or_pattern => |o| {
                for (o.alternatives) |alt| {
                    self.checkPattern(alt, expected_type);
                }
            },
            .guarded => |g| {
                self.checkPattern(g.pattern, expected_type);
                const guard_type = self.checkExpr(g.guard);
                if (!self.isBoolType(guard_type)) {
                    self.addError(.type_mismatch, g.span, "guard must be bool", .{});
                }
            },
        }
    }

    fn checkLiteralPattern(self: *TypeChecker, lit: ast.PatternLiteral) Type {
        return switch (lit.kind) {
            .int => self.type_builder.i32Type(),
            .float => self.type_builder.f64Type(),
            .string => self.type_builder.stringType(),
            .char => self.type_builder.charType(),
            .bool_ => self.type_builder.boolType(),
        };
    }

    fn bindPattern(self: *TypeChecker, pattern: ast.Pattern, t: Type) void {
        switch (pattern) {
            .wildcard => {},
            .literal => {},
            .binding => |bind| {
                self.current_scope.define(.{
                    .name = bind.name,
                    .type_ = t,
                    .kind = .variable,
                    .mutable = bind.mutable,
                    .span = bind.span,
                }) catch {};
            },
            .variant => |v| {
                // Determine the enum type
                var enum_type = t;
                if (v.type_expr) |type_expr| {
                    enum_type = self.resolveTypeExpr(type_expr) catch return;
                }

                if (enum_type != .enum_) return;
                const enum_def = enum_type.enum_;

                // Find the variant
                var found_variant: ?types.EnumVariant = null;
                for (enum_def.variants) |variant| {
                    if (std.mem.eql(u8, variant.name, v.variant_name)) {
                        found_variant = variant;
                        break;
                    }
                }

                if (found_variant) |variant| {
                    if (variant.payload) |payload| {
                        if (v.payload) |payload_pattern| {
                            switch (payload) {
                                .tuple => |tuple_types| {
                                    if (tuple_types.len == 1) {
                                        // Single-element: bind directly
                                        self.bindPattern(payload_pattern, tuple_types[0]);
                                    } else {
                                        // Multi-element: build tuple type
                                        const payload_type = self.type_builder.tupleType(tuple_types) catch return;
                                        self.bindPattern(payload_pattern, payload_type);
                                    }
                                },
                                .struct_ => |fields| {
                                    _ = fields;
                                    // Struct payloads - handled by struct pattern binding
                                    self.bindPattern(payload_pattern, self.type_builder.unknownType());
                                },
                            }
                        }
                    }
                }
            },
            .struct_pattern => |s| {
                // TODO: bind struct field patterns
                _ = s;
            },
            .tuple_pattern => |tup| {
                if (t == .tuple) {
                    const tuple_type = t.tuple;
                    for (tup.elements, 0..) |elem_pattern, i| {
                        if (i < tuple_type.elements.len) {
                            self.bindPattern(elem_pattern, tuple_type.elements[i]);
                        }
                    }
                }
            },
            .or_pattern => |o| {
                // Bind variables from first alternative (all should have same bindings)
                if (o.alternatives.len > 0) {
                    self.bindPattern(o.alternatives[0], t);
                }
            },
            .guarded => |g| {
                self.bindPattern(g.pattern, t);
            },
        }
    }

    // ========================================================================
    // Helper Functions
    // ========================================================================

    fn isBoolType(self: *TypeChecker, t: Type) bool {
        _ = self;
        return t == .primitive and t.primitive == .bool_;
    }

    fn isAssignable(self: *TypeChecker, expr: ast.Expr) bool {
        _ = self;
        return switch (expr) {
            .identifier => true,
            .index => true,
            .field => true,
            .unary => |u| u.op == .deref,
            else => false,
        };
    }

    fn isMutable(self: *TypeChecker, expr: ast.Expr) bool {
        switch (expr) {
            .identifier => |id| {
                if (self.current_scope.lookup(id.name)) |sym| {
                    return sym.mutable;
                }
                return false;
            },
            .field => |f| return self.isMutable(f.object),
            .index => |i| return self.isMutable(i.object),
            .unary => |u| {
                if (u.op == .deref) {
                    const operand_type = self.checkExpr(u.operand);
                    if (operand_type == .reference) {
                        return operand_type.reference.mutable;
                    }
                }
                return false;
            },
            else => return false,
        }
    }

    fn checkAssignmentCompatible(self: *TypeChecker, target: Type, value: Type) bool {
        _ = self;
        // No implicit conversions - types must match exactly
        return target.eql(value) or target == .unknown or value == .unknown;
    }

    // ========================================================================
    // Module Checking Entry Point
    // ========================================================================

    pub fn checkModule(self: *TypeChecker, module: ast.Module) void {
        // First pass: register all type declarations
        for (module.declarations) |decl| {
            switch (decl) {
                .struct_decl, .enum_decl, .trait_decl, .type_alias => self.checkDecl(decl),
                else => {},
            }
        }

        // Second pass: check function signatures
        for (module.declarations) |decl| {
            switch (decl) {
                .function => |f| {
                    // Push type parameters for generic functions
                    const has_type_params = f.type_params.len > 0;
                    if (has_type_params) {
                        _ = self.pushTypeParams(f.type_params) catch continue;
                        // Store generic function declarations for later codegen monomorphization
                        self.generic_functions.put(self.allocator, f.name, f) catch {};
                    }
                    defer if (has_type_params) self.popTypeParams();

                    // Register function but don't check body yet
                    var param_types: std.ArrayListUnmanaged(Type) = .{};
                    defer param_types.deinit(self.allocator);

                    for (f.params) |param| {
                        const param_type = self.resolveTypeExpr(param.type_) catch self.type_builder.unknownType();
                        param_types.append(self.allocator, param_type) catch {};
                    }

                    const return_type = if (f.return_type) |rt|
                        self.resolveTypeExpr(rt) catch self.type_builder.unknownType()
                    else
                        self.type_builder.voidType();

                    const func_type = self.type_builder.functionType(param_types.items, return_type) catch continue;

                    self.current_scope.define(.{
                        .name = f.name,
                        .type_ = func_type,
                        .kind = .function,
                        .mutable = false,
                        .span = f.span,
                    }) catch {};
                },
                .const_decl => self.checkDecl(decl),
                else => {},
            }
        }

        // Third pass: check function bodies
        for (module.declarations) |decl| {
            switch (decl) {
                .function => |f| {
                    if (f.body) |body| {
                        // Push type parameters for generic functions
                        const has_type_params = f.type_params.len > 0;
                        if (has_type_params) {
                            _ = self.pushTypeParams(f.type_params) catch continue;
                        }
                        defer if (has_type_params) self.popTypeParams();

                        _ = self.pushScope(.function) catch continue;
                        defer self.popScope();

                        for (f.params) |param| {
                            const param_type = self.resolveTypeExpr(param.type_) catch self.type_builder.unknownType();
                            self.current_scope.define(.{
                                .name = param.name,
                                .type_ = param_type,
                                .kind = .parameter,
                                .mutable = false,
                                .span = param.span,
                            }) catch {};
                        }

                        self.current_return_type = if (f.return_type) |rt|
                            self.resolveTypeExpr(rt) catch self.type_builder.voidType()
                        else
                            self.type_builder.voidType();
                        defer self.current_return_type = null;

                        _ = self.checkBlock(body);
                    }
                },
                .impl_decl => self.checkDecl(decl),
                else => {},
            }
        }
    }
};

// ============================================================================
// Tests
// ============================================================================

test "TypeChecker initialization" {
    const testing = std.testing;
    var checker = TypeChecker.init(testing.allocator);
    defer checker.deinit();

    // Check builtin types are registered
    try testing.expect(checker.current_scope.lookup("i32") != null);
    try testing.expect(checker.current_scope.lookup("string") != null);
    try testing.expect(checker.current_scope.lookup("bool") != null);

    // Check builtin functions
    try testing.expect(checker.current_scope.lookup("print") != null);
    try testing.expect(checker.current_scope.lookup("println") != null);
}

test "Scope lookup" {
    const testing = std.testing;
    var checker = TypeChecker.init(testing.allocator);
    defer checker.deinit();

    // Define a variable in global scope
    try checker.current_scope.define(.{
        .name = "x",
        .type_ = checker.type_builder.i32Type(),
        .kind = .variable,
        .mutable = false,
        .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
    });

    // Create nested scope
    _ = try checker.pushScope(.block);

    // Should still find x
    try testing.expect(checker.current_scope.lookup("x") != null);

    // Define y in inner scope
    try checker.current_scope.define(.{
        .name = "y",
        .type_ = checker.type_builder.i32Type(),
        .kind = .variable,
        .mutable = true,
        .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
    });

    try testing.expect(checker.current_scope.lookup("y") != null);

    checker.popScope();

    // y should not be visible anymore
    try testing.expect(checker.current_scope.lookup("y") == null);
    // x should still be visible
    try testing.expect(checker.current_scope.lookup("x") != null);
}

test "Loop scope tracking" {
    const testing = std.testing;
    var checker = TypeChecker.init(testing.allocator);
    defer checker.deinit();

    try testing.expect(!checker.current_scope.isInLoop());

    _ = try checker.pushScope(.loop);
    try testing.expect(checker.current_scope.isInLoop());

    _ = try checker.pushScope(.block);
    try testing.expect(checker.current_scope.isInLoop()); // Still in loop

    checker.popScope();
    try testing.expect(checker.current_scope.isInLoop());

    checker.popScope();
    try testing.expect(!checker.current_scope.isInLoop());
}
