const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("ast.zig");
const types = @import("types.zig");
const Type = types.Type;
const Primitive = types.Primitive;
const TypeBuilder = types.TypeBuilder;
const Span = ast.Span;
const module_resolver = @import("module_resolver.zig");
const ModuleInfo = module_resolver.ModuleInfo;
const ModuleResolver = module_resolver.ModuleResolver;
const interpreter = @import("interpreter.zig");
const Interpreter = interpreter.Interpreter;
const values = @import("values.zig");
const Value = values.Value;

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
        undefined_function,
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
        wrong_number_of_args,
        // Module system errors
        undefined_module,
        visibility_error,
        circular_import,
        import_conflict,
        // Comptime errors
        comptime_error,
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
    /// For imported symbols with aliases, stores the original name.
    /// Used by codegen to find the correct LLVM function.
    original_name: ?[]const u8 = null,

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
    /// Track trait method calls through bounds for monomorphization.
    trait_method_calls: std.ArrayListUnmanaged(TraitMethodCall),
    /// Current trait being checked (for Self.Item resolution in trait method signatures)
    current_trait_type: ?*types.TraitType,

    // Module system fields
    /// Registry of symbols from other modules, keyed by module canonical name.
    module_registry: std.StringHashMapUnmanaged(ModuleSymbols),
    /// The current module being checked (null for single-file compilation).
    current_module: ?*ModuleInfo,
    /// The module resolver (null for single-file compilation).
    module_resolver_ref: ?*ModuleResolver,
    /// Module scopes created by prepareForNewModule (for cleanup).
    module_scopes: std.ArrayListUnmanaged(*Scope),

    // Comptime evaluation
    /// Storage for comptime-evaluated string values.
    /// Maps BuiltinCall AST node pointers to their computed string values.
    /// Used by codegen to emit the correct string literals.
    comptime_strings: std.AutoHashMapUnmanaged(*ast.BuiltinCall, []const u8),

    /// Storage for comptime-evaluated boolean values.
    /// Maps BuiltinCall AST node pointers to their computed boolean values.
    /// Used by codegen to emit the correct boolean literals.
    comptime_bools: std.AutoHashMapUnmanaged(*ast.BuiltinCall, bool),

    /// Storage for comptime-evaluated integer values.
    /// Maps BuiltinCall AST node pointers to their computed integer values.
    /// Used by codegen for @sizeOf, @alignOf, etc.
    comptime_ints: std.AutoHashMapUnmanaged(*ast.BuiltinCall, i64),

    /// Storage for @repeat(value, count) builtin info.
    /// Maps BuiltinCall AST node pointers to their repeat information.
    /// Used by codegen to emit array initialization.
    comptime_repeats: std.AutoHashMapUnmanaged(*ast.BuiltinCall, RepeatInfo),

    /// Storage for comptime block values.
    /// Maps ComptimeBlock AST node pointers to their evaluated values.
    /// Used by codegen to emit the correct compile-time constant values.
    comptime_values: std.AutoHashMapUnmanaged(*ast.ComptimeBlock, ComptimeValue),

    /// Storage for comptime function declarations.
    /// Maps function name to the function declaration for comptime evaluation.
    comptime_functions: std.StringHashMapUnmanaged(*ast.FunctionDecl),

    /// Storage for comptime function call results.
    /// Maps Call AST node pointers to their evaluated compile-time values.
    /// Used by codegen to emit constants instead of function calls.
    comptime_call_values: std.AutoHashMapUnmanaged(*ast.Call, ComptimeValue),

    /// Storage for comptime function calls via @name(...) builtin syntax.
    /// Maps BuiltinCall AST node pointers to their evaluated compile-time values.
    comptime_builtin_values: std.AutoHashMapUnmanaged(*ast.BuiltinCall, ComptimeValue),

    /// Storage for compile-time constant values.
    /// Maps constant names to their evaluated compile-time values.
    /// This allows comptime blocks and expressions to reference outer scope constants.
    constant_values: std.StringHashMapUnmanaged(ComptimeValue),

    /// Shared interpreter for comptime function evaluation.
    /// This allows recursive comptime function calls to share state.
    /// Owned by the outermost comptime call (set to null between top-level evaluations).
    comptime_interpreter: ?*Interpreter,

    /// Recursion depth for comptime function calls (prevents infinite recursion).
    comptime_depth: u32,

    /// True when we are inside a comptime function body during type checking.
    /// When true, calls to comptime functions within the body should be type-checked
    /// but not evaluated (the interpreter handles evaluation).
    checking_comptime_function_body: bool,

    /// Maps Postfix ? expressions to their error conversion info.
    /// Used by codegen to emit From::from calls when error types differ.
    error_conversions: std.AutoHashMapUnmanaged(*ast.Postfix, ErrorConversionInfo),

    /// Expected type context for type inference in Ok/Err constructors.
    /// Set when checking variable declarations with type annotations.
    expected_type: ?Type,

    /// Maps debug() call AST nodes to the argument type.
    /// Used by codegen to emit type-specific formatting code.
    debug_call_types: std.AutoHashMapUnmanaged(*ast.Call, Type),

    /// Maximum recursion depth for comptime functions.
    const max_comptime_depth: u32 = 1000;

    /// Represents a compile-time evaluated value.
    pub const ComptimeValue = union(enum) {
        /// Integer with type info preserved
        int: struct {
            value: i64,
            is_i32: bool, // true for i32, false for i64
        },
        float: f64,
        bool_: bool,
        string: []const u8,
        void_,
        /// Struct value with type name and field values (preserves order)
        struct_: ComptimeStruct,
        /// Array value with element type and elements
        array: ComptimeArray,
    };

    /// A compile-time struct value.
    pub const ComptimeStruct = struct {
        type_name: []const u8,
        /// Fields stored in declaration order using ArrayHashMap
        fields: std.StringArrayHashMapUnmanaged(ComptimeValue),
    };

    /// A compile-time array value.
    pub const ComptimeArray = struct {
        /// Element type for codegen
        element_type: Type,
        /// Array elements in order
        elements: []const ComptimeValue,
    };

    /// Information for @repeat(value, count) builtin.
    /// Stores the element type and count for codegen.
    pub const RepeatInfo = struct {
        /// The type of the repeated element
        element_type: Type,
        /// The number of times to repeat (comptime-known)
        count: usize,
        /// The value expression to repeat
        value_expr: ast.Expr,
    };

    /// Symbols exported from a module.
    pub const ModuleSymbols = struct {
        /// All exported symbols from this module.
        symbols: std.StringHashMapUnmanaged(ModuleSymbol),
        /// The module info (for re-export tracking).
        module_info: *ModuleInfo,
    };

    /// A symbol imported from another module.
    pub const ModuleSymbol = struct {
        /// Name of the symbol.
        name: []const u8,
        /// Kind of symbol.
        kind: Kind,
        /// Resolved type (null for types/traits).
        type_: ?Type,
        /// Whether the symbol is public.
        is_pub: bool,

        pub const Kind = enum {
            function,
            struct_type,
            enum_type,
            trait_type,
            type_alias,
            constant,
        };
    };

    const CaptureInfo = struct {
        is_mutable: bool,
    };

    /// Information about a trait definition.
    pub const TraitInfo = struct {
        /// The trait type containing method signatures
        trait_type: *types.TraitType,
        /// The original AST declaration (null for builtin traits)
        decl: ?*ast.TraitDecl,
    };

    /// Information about an associated type binding in an impl block.
    pub const AssociatedTypeBinding = struct {
        /// Name of the associated type
        name: []const u8,
        /// The concrete type provided
        concrete_type: Type,
    };

    /// Information about a trait implementation for a type.
    pub const TraitImplInfo = struct {
        /// Name of the trait being implemented
        trait_name: []const u8,
        /// Name of the implementing type
        impl_type_name: []const u8,
        /// Type parameters from the impl block (for generic impls)
        impl_type_params: []const types.TypeVar,
        /// Associated type bindings (type Item = ConcreteType)
        associated_type_bindings: []const AssociatedTypeBinding,
        /// The methods implementing the trait
        methods: []const StructMethod,
    };

    /// Information about an error conversion needed for the ? operator.
    /// When a function returns Result[T, TargetError] and uses ? on Result[U, SourceError],
    /// this records the From[SourceError] conversion to be generated in codegen.
    pub const ErrorConversionInfo = struct {
        /// The source error type (from the operand's Result type)
        source_type: Type,
        /// The target error type (from the function's return type)
        target_type: Type,
        /// Mangled name of the from method (e.g., "AppError_from_IoError")
        from_method_name: []const u8,
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

    /// Information about a trait method call through a generic bound.
    /// Used to track which trait methods are called on type variables,
    /// so codegen can emit the correct monomorphized method calls.
    pub const TraitMethodCall = struct {
        /// Name of the type variable (e.g., "T")
        type_var_name: []const u8,
        /// Name of the trait providing the method
        trait_name: []const u8,
        /// Name of the method being called
        method_name: []const u8,
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
            .trait_method_calls = .{},
            .current_trait_type = null,
            .module_registry = .{},
            .current_module = null,
            .module_resolver_ref = null,
            .module_scopes = .{},
            .comptime_strings = .{},
            .comptime_bools = .{},
            .comptime_ints = .{},
            .comptime_repeats = .{},
            .comptime_values = .{},
            .comptime_functions = .{},
            .comptime_call_values = .{},
            .comptime_builtin_values = .{},
            .constant_values = .{},
            .comptime_interpreter = null,
            .comptime_depth = 0,
            .checking_comptime_function_body = false,
            .error_conversions = .{},
            .expected_type = null,
            .debug_call_types = .{},
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
            // Note: type_params slices are freed separately via type_var_slices
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
        // Note: type_params slices are freed separately via type_var_slices
        for (self.generic_struct_types.items) |struct_type| {
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
        // Note: type_params slices are freed separately via type_var_slices
        // Note: method.signature.params are allocated from type_builder's arena, not self.allocator
        for (self.trait_types.items) |trait_type| {
            if (trait_type.super_traits.len > 0) {
                self.allocator.free(trait_type.super_traits);
            }
            // Free associated types and their bounds slices
            for (trait_type.associated_types) |assoc| {
                if (assoc.bounds.len > 0) {
                    self.allocator.free(assoc.bounds);
                }
            }
            if (trait_type.associated_types.len > 0) {
                self.allocator.free(trait_type.associated_types);
            }
            // The methods slice is allocated with self.allocator
            self.allocator.free(trait_type.methods);
            self.allocator.destroy(trait_type);
        }
        self.trait_types.deinit(self.allocator);

        // Clean up trait implementations (including allocated keys and associated type bindings)
        var impls_key_iter = self.trait_impls.keyIterator();
        while (impls_key_iter.next()) |key| {
            // Keys are allocated strings, free them
            if (key.*.len > 0) {
                self.allocator.free(key.*);
            }
        }
        var impls_iter = self.trait_impls.valueIterator();
        while (impls_iter.next()) |impl_list| {
            // Free associated_type_bindings slices in each TraitImplInfo
            for (impl_list.items) |impl_info| {
                if (impl_info.associated_type_bindings.len > 0) {
                    self.allocator.free(impl_info.associated_type_bindings);
                }
            }
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

        // Clean up trait method calls
        self.trait_method_calls.deinit(self.allocator);

        // Clean up module registry (including allocated canonical name keys)
        var module_key_iter = self.module_registry.keyIterator();
        while (module_key_iter.next()) |key| {
            self.allocator.free(key.*);
        }
        var module_iter = self.module_registry.valueIterator();
        while (module_iter.next()) |mod_symbols| {
            mod_symbols.symbols.deinit(self.allocator);
        }
        self.module_registry.deinit(self.allocator);

        // Clean up module scopes created by prepareForNewModule
        for (self.module_scopes.items) |scope| {
            scope.deinit();
            self.allocator.destroy(scope);
        }
        self.module_scopes.deinit(self.allocator);

        // Clean up comptime strings
        var comptime_iter = self.comptime_strings.valueIterator();
        while (comptime_iter.next()) |str| {
            self.allocator.free(str.*);
        }
        self.comptime_strings.deinit(self.allocator);

        // Clean up comptime bools (no values to free, just the map)
        self.comptime_bools.deinit(self.allocator);

        // Clean up comptime ints (no values to free, just the map)
        self.comptime_ints.deinit(self.allocator);

        // Clean up comptime repeats (no values to free, just the map)
        self.comptime_repeats.deinit(self.allocator);

        // Clean up comptime values (need to free strings)
        var comptime_values_iter = self.comptime_values.valueIterator();
        while (comptime_values_iter.next()) |cv| {
            if (cv.* == .string) {
                self.allocator.free(cv.string);
            }
        }
        self.comptime_values.deinit(self.allocator);
        self.comptime_functions.deinit(self.allocator);
        // Clean up comptime call values (free allocated strings)
        var call_iter = self.comptime_call_values.valueIterator();
        while (call_iter.next()) |cv| {
            if (cv.* == .string) {
                self.allocator.free(cv.string);
            }
        }
        self.comptime_call_values.deinit(self.allocator);
        // Clean up comptime builtin call values (free allocated strings)
        var builtin_iter = self.comptime_builtin_values.valueIterator();
        while (builtin_iter.next()) |cv| {
            if (cv.* == .string) {
                self.allocator.free(cv.string);
            }
        }
        self.comptime_builtin_values.deinit(self.allocator);

        // Clean up constant values (free allocated strings)
        var constant_iter = self.constant_values.valueIterator();
        while (constant_iter.next()) |cv| {
            if (cv.* == .string) {
                self.allocator.free(cv.string);
            }
        }
        self.constant_values.deinit(self.allocator);

        // Clean up error conversions (free allocated method names)
        var error_conv_iter = self.error_conversions.valueIterator();
        while (error_conv_iter.next()) |conv| {
            self.allocator.free(conv.from_method_name);
        }
        self.error_conversions.deinit(self.allocator);

        // Clean up debug call types (no values to free, just the map)
        self.debug_call_types.deinit(self.allocator);
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

        // readline() -> string - reads a line from stdin
        const readline_type = try self.type_builder.functionType(&.{}, self.type_builder.stringType());
        try self.current_scope.define(.{
            .name = "readline",
            .type_ = readline_type,
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

        // debug(value: T) -> string - returns debug string representation of any value
        // Note: Actual type checking done in checkCall, this is a placeholder signature
        const debug_type = try self.type_builder.functionType(&.{self.type_builder.i32Type()}, self.type_builder.stringType());
        try self.current_scope.define(.{
            .name = "debug",
            .type_ = debug_type,
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

        // Some(value: T) -> ?T - Optional constructor for present values
        // For now, use i32 -> ?i32 as a simple type
        // Full generic support infers T from the argument
        const some_type = try self.type_builder.functionType(
            &.{self.type_builder.i32Type()},
            try self.type_builder.optionalType(self.type_builder.i32Type()),
        );
        try self.current_scope.define(.{
            .name = "Some",
            .type_ = some_type,
            .kind = .function,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // None - Optional constructor for absent values
        // Returns ?i32 by default, but actual type is inferred from context
        const none_type = try self.type_builder.functionType(
            &.{},
            try self.type_builder.optionalType(self.type_builder.i32Type()),
        );
        try self.current_scope.define(.{
            .name = "None",
            .type_ = none_type,
            .kind = .function,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // ====================================================================
        // Register builtin traits
        // ====================================================================

        // Eq trait: trait Eq { fn eq(&self, other: &Self) -> bool; }
        // The signature uses 'unknown' as a placeholder for Self, which gets
        // substituted with the concrete type during impl checking.
        // Note: Use type_builder for function types so params are on the arena.
        const eq_self_type = self.type_builder.unknownType();
        const eq_self_ref = try self.type_builder.referenceType(eq_self_type, false);
        const eq_func_type = try self.type_builder.functionType(&.{ eq_self_ref, eq_self_ref }, self.type_builder.boolType());
        const eq_method = types.TraitMethod{
            .name = "eq",
            .signature = eq_func_type.function.*,
            .has_default = false,
        };

        const eq_trait_type = try self.allocator.create(types.TraitType);
        eq_trait_type.* = .{
            .name = "Eq",
            .type_params = &.{},
            .associated_types = &.{},
            .methods = try self.allocator.dupe(types.TraitMethod, &.{eq_method}),
            .super_traits = &.{},
        };

        // Track for cleanup
        try self.trait_types.append(self.allocator, eq_trait_type);

        // Register in trait registry
        try self.trait_registry.put(self.allocator, "Eq", .{
            .trait_type = eq_trait_type,
            .decl = null, // Builtin trait, no AST declaration
        });

        // Register in symbol table as a trait
        try self.current_scope.define(.{
            .name = "Eq",
            .type_ = .{ .trait_ = eq_trait_type },
            .kind = .trait_,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // Ordered trait: trait Ordered { fn lt(&self, other: &Self) -> bool; fn le(...); fn gt(...); fn ge(...); }
        // Provides comparison methods for ordering: less than, less than or equal, greater than, greater than or equal
        // Note: Use type_builder for function types so params are on the arena.
        const ord_self_type = self.type_builder.unknownType();
        const ord_self_ref = try self.type_builder.referenceType(ord_self_type, false);
        const ord_return_type = self.type_builder.boolType();

        const lt_func = try self.type_builder.functionType(&.{ ord_self_ref, ord_self_ref }, ord_return_type);
        const lt_method = types.TraitMethod{
            .name = "lt",
            .signature = lt_func.function.*,
            .has_default = false,
        };
        const le_func = try self.type_builder.functionType(&.{ ord_self_ref, ord_self_ref }, ord_return_type);
        const le_method = types.TraitMethod{
            .name = "le",
            .signature = le_func.function.*,
            .has_default = false,
        };
        const gt_func = try self.type_builder.functionType(&.{ ord_self_ref, ord_self_ref }, ord_return_type);
        const gt_method = types.TraitMethod{
            .name = "gt",
            .signature = gt_func.function.*,
            .has_default = false,
        };
        const ge_func = try self.type_builder.functionType(&.{ ord_self_ref, ord_self_ref }, ord_return_type);
        const ge_method = types.TraitMethod{
            .name = "ge",
            .signature = ge_func.function.*,
            .has_default = false,
        };

        const ordered_trait_type = try self.allocator.create(types.TraitType);
        ordered_trait_type.* = .{
            .name = "Ordered",
            .type_params = &.{},
            .associated_types = &.{},
            .methods = try self.allocator.dupe(types.TraitMethod, &.{ lt_method, le_method, gt_method, ge_method }),
            .super_traits = &.{},
        };

        // Track for cleanup
        try self.trait_types.append(self.allocator, ordered_trait_type);

        // Register in trait registry
        try self.trait_registry.put(self.allocator, "Ordered", .{
            .trait_type = ordered_trait_type,
            .decl = null, // Builtin trait, no AST declaration
        });

        // Register in symbol table as a trait
        try self.current_scope.define(.{
            .name = "Ordered",
            .type_ = .{ .trait_ = ordered_trait_type },
            .kind = .trait_,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // Clone trait: trait Clone { fn clone(&self) -> Self; }
        // Provides explicit cloning for types. All primitives implement Clone.
        // Create a proper Self type variable so the type_var handler can resolve it
        // Note: Use type_builder for function types so params are on the arena.
        const clone_self_type = Type{ .type_var = .{
            .id = 999, // Use a high unique ID to avoid conflicts
            .name = "Self",
            .bounds = &.{},
        } };
        const clone_self_ref = try self.type_builder.referenceType(clone_self_type, false);
        const clone_func = try self.type_builder.functionType(&.{clone_self_ref}, clone_self_type);
        const clone_method = types.TraitMethod{
            .name = "clone",
            .signature = clone_func.function.*,
            .has_default = false,
        };

        const clone_trait_type = try self.allocator.create(types.TraitType);
        clone_trait_type.* = .{
            .name = "Clone",
            .type_params = &.{},
            .associated_types = &.{},
            .methods = try self.allocator.dupe(types.TraitMethod, &.{clone_method}),
            .super_traits = &.{},
        };

        // Track for cleanup
        try self.trait_types.append(self.allocator, clone_trait_type);

        // Register in trait registry
        try self.trait_registry.put(self.allocator, "Clone", .{
            .trait_type = clone_trait_type,
            .decl = null, // Builtin trait, no AST declaration
        });

        // Register in symbol table as a trait
        try self.current_scope.define(.{
            .name = "Clone",
            .type_ = .{ .trait_ = clone_trait_type },
            .kind = .trait_,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // Drop trait: trait Drop { fn drop(self: Self) -> void; }
        // Provides custom destructor behavior. Called automatically when value goes out of scope.
        // Primitives don't need Drop (trivially dropped), but custom types can implement it.
        // Note: Use type_builder for function types so params are on the arena.
        const drop_self_type = self.type_builder.unknownType();
        const drop_func = try self.type_builder.functionType(&.{drop_self_type}, self.type_builder.voidType());
        const drop_method = types.TraitMethod{
            .name = "drop",
            .signature = drop_func.function.*,
            .has_default = false,
        };

        const drop_trait_type = try self.allocator.create(types.TraitType);
        drop_trait_type.* = .{
            .name = "Drop",
            .type_params = &.{},
            .associated_types = &.{},
            .methods = try self.allocator.dupe(types.TraitMethod, &.{drop_method}),
            .super_traits = &.{},
        };

        // Track for cleanup
        try self.trait_types.append(self.allocator, drop_trait_type);

        // Register in trait registry
        try self.trait_registry.put(self.allocator, "Drop", .{
            .trait_type = drop_trait_type,
            .decl = null, // Builtin trait, no AST declaration
        });

        // Register in symbol table as a trait
        try self.current_scope.define(.{
            .name = "Drop",
            .type_ = .{ .trait_ = drop_trait_type },
            .kind = .trait_,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // Default trait: trait Default { fn default() -> Self; }
        // Provides a default value for types. All primitives have builtin Default:
        // i32, i64, etc. -> 0, f32, f64 -> 0.0, bool -> false, string -> ""
        // Note: This is a static method (no &self), it returns a new value of type Self.
        const default_self_type = Type{ .type_var = .{
            .id = 998, // Use a unique ID to avoid conflicts
            .name = "Self",
            .bounds = &.{},
        } };
        const default_method_sig = types.FunctionType{
            .params = &.{}, // No parameters - static method
            .return_type = default_self_type, // Returns Self
            .is_async = false,
        };
        const default_method = types.TraitMethod{
            .name = "default",
            .signature = default_method_sig,
            .has_default = false,
        };

        const default_trait_type = try self.allocator.create(types.TraitType);
        default_trait_type.* = .{
            .name = "Default",
            .type_params = &.{},
            .associated_types = &.{},
            .methods = try self.allocator.dupe(types.TraitMethod, &.{default_method}),
            .super_traits = &.{},
        };

        // Track for cleanup
        try self.trait_types.append(self.allocator, default_trait_type);

        // Register in trait registry
        try self.trait_registry.put(self.allocator, "Default", .{
            .trait_type = default_trait_type,
            .decl = null, // Builtin trait, no AST declaration
        });

        // Register in symbol table as a trait
        try self.current_scope.define(.{
            .name = "Default",
            .type_ = .{ .trait_ = default_trait_type },
            .kind = .trait_,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // Hash trait: trait Hash { fn hash(&self) -> i64; }
        // Provides hashing for types, needed for HashMap/HashSet keys.
        // All primitives have builtin Hash implementation.
        // Returns i64 to allow for a large hash space.
        // Note: Use type_builder for function types so params are on the arena.
        const hash_self_type = self.type_builder.unknownType();
        const hash_self_ref = try self.type_builder.referenceType(hash_self_type, false);
        const hash_func = try self.type_builder.functionType(&.{hash_self_ref}, self.type_builder.i64Type());
        const hash_method = types.TraitMethod{
            .name = "hash",
            .signature = hash_func.function.*,
            .has_default = false,
        };

        const hash_trait_type = try self.allocator.create(types.TraitType);
        hash_trait_type.* = .{
            .name = "Hash",
            .type_params = &.{},
            .associated_types = &.{},
            .methods = try self.allocator.dupe(types.TraitMethod, &.{hash_method}),
            .super_traits = &.{},
        };

        // Track for cleanup
        try self.trait_types.append(self.allocator, hash_trait_type);

        // Register in trait registry
        try self.trait_registry.put(self.allocator, "Hash", .{
            .trait_type = hash_trait_type,
            .decl = null, // Builtin trait, no AST declaration
        });

        // Register in symbol table as a trait
        try self.current_scope.define(.{
            .name = "Hash",
            .type_ = .{ .trait_ = hash_trait_type },
            .kind = .trait_,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // Iterator trait: trait Iterator { type Item; fn next(&mut self) -> ?Self.Item; }
        // The core iterator protocol. Types that implement Iterator can be used in for loops.
        // The associated type `Item` specifies what type of values the iterator yields.
        // The `next` method returns Some(item) for each element, then None when exhausted.

        // First create the trait type (needed for AssociatedTypeRef)
        const iterator_trait_type = try self.allocator.create(types.TraitType);

        // Create Self type variable for method signatures
        const iter_self_type_var = types.TypeVar{
            .id = 997, // Unique ID to avoid conflicts
            .name = "Self",
            .bounds = &.{},
        };
        const iter_self_type = Type{ .type_var = iter_self_type_var };

        // Create &mut Self for the next() parameter
        const iter_self_mut_ref = try self.type_builder.referenceType(iter_self_type, true);

        // Create Self.Item (associated type reference)
        const iter_item_type = try self.type_builder.associatedTypeRefType(
            iter_self_type_var,
            "Item",
            iterator_trait_type,
        );

        // Create ?Self.Item (optional of the associated type)
        const iter_next_return = try self.type_builder.optionalType(iter_item_type);

        // Create next(&mut self) -> ?Self.Item
        const iter_next_func = try self.type_builder.functionType(&.{iter_self_mut_ref}, iter_next_return);
        const iter_next_method = types.TraitMethod{
            .name = "next",
            .signature = iter_next_func.function.*,
            .has_default = false,
        };

        // Create the Item associated type declaration (no bounds, no default)
        const iter_item_assoc = types.AssociatedType{
            .name = "Item",
            .bounds = &.{}, // No trait bounds on Item
            .default = null, // No default type
        };

        // Complete the trait type
        iterator_trait_type.* = .{
            .name = "Iterator",
            .type_params = &.{},
            .associated_types = try self.allocator.dupe(types.AssociatedType, &.{iter_item_assoc}),
            .methods = try self.allocator.dupe(types.TraitMethod, &.{iter_next_method}),
            .super_traits = &.{},
        };

        // Track for cleanup
        try self.trait_types.append(self.allocator, iterator_trait_type);

        // Register in trait registry
        try self.trait_registry.put(self.allocator, "Iterator", .{
            .trait_type = iterator_trait_type,
            .decl = null, // Builtin trait, no AST declaration
        });

        // Register in symbol table as a trait
        try self.current_scope.define(.{
            .name = "Iterator",
            .type_ = .{ .trait_ = iterator_trait_type },
            .kind = .trait_,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // IntoIterator trait: trait IntoIterator { type Item; type IntoIter: Iterator; fn into_iter(self) -> Self.IntoIter; }
        // Types that implement IntoIterator can be converted into iterators.
        // This is what for loops use: `for x in collection` desugars to `for x in collection.into_iter()`.
        // The `Item` associated type specifies what values are yielded.
        // The `IntoIter` associated type must implement Iterator.

        // Create the trait type first (needed for AssociatedTypeRef)
        const into_iter_trait_type = try self.allocator.create(types.TraitType);

        // Create Self type variable for method signatures
        const into_iter_self_type_var = types.TypeVar{
            .id = 996, // Unique ID to avoid conflicts
            .name = "Self",
            .bounds = &.{},
        };
        const into_iter_self_type = Type{ .type_var = into_iter_self_type_var };

        // Create Self.IntoIter (associated type reference for return type)
        const into_iter_return_type = try self.type_builder.associatedTypeRefType(
            into_iter_self_type_var,
            "IntoIter",
            into_iter_trait_type,
        );

        // Create into_iter(self) -> Self.IntoIter
        const into_iter_func = try self.type_builder.functionType(&.{into_iter_self_type}, into_iter_return_type);
        const into_iter_method = types.TraitMethod{
            .name = "into_iter",
            .signature = into_iter_func.function.*,
            .has_default = false,
        };

        // Create the Item associated type declaration (no bounds, no default)
        const into_iter_item_assoc = types.AssociatedType{
            .name = "Item",
            .bounds = &.{}, // No trait bounds on Item
            .default = null, // No default type
        };

        // Create the IntoIter associated type declaration (must implement Iterator)
        // Store pointer to Iterator trait for bounds checking
        const into_iter_bounds = try self.allocator.dupe(*types.TraitType, &.{iterator_trait_type});
        const into_iter_iter_assoc = types.AssociatedType{
            .name = "IntoIter",
            .bounds = into_iter_bounds, // Must implement Iterator
            .default = null, // No default type
        };

        // Complete the trait type
        into_iter_trait_type.* = .{
            .name = "IntoIterator",
            .type_params = &.{},
            .associated_types = try self.allocator.dupe(types.AssociatedType, &.{ into_iter_item_assoc, into_iter_iter_assoc }),
            .methods = try self.allocator.dupe(types.TraitMethod, &.{into_iter_method}),
            .super_traits = &.{},
        };

        // Track for cleanup
        try self.trait_types.append(self.allocator, into_iter_trait_type);

        // Register in trait registry
        try self.trait_registry.put(self.allocator, "IntoIterator", .{
            .trait_type = into_iter_trait_type,
            .decl = null, // Builtin trait, no AST declaration
        });

        // Register in symbol table as a trait
        try self.current_scope.define(.{
            .name = "IntoIterator",
            .type_ = .{ .trait_ = into_iter_trait_type },
            .kind = .trait_,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // From trait: trait From[E] { fn from(err: E) -> Self; }
        // Enables automatic error type conversion in the ? operator.
        // When a function returns Result[T, TargetError] and uses ? on Result[U, SourceError],
        // the compiler calls TargetError.from(source_error) if impl TargetError: From[SourceError] exists.

        // Create the trait type first
        const from_trait_type = try self.allocator.create(types.TraitType);

        // Create Self type variable for return type
        const from_self_type_var = types.TypeVar{
            .id = 995, // Unique ID to avoid conflicts
            .name = "Self",
            .bounds = &.{},
        };
        const from_self_type = Type{ .type_var = from_self_type_var };

        // Create E type parameter (the source error type)
        const from_e_type_var = types.TypeVar{
            .id = 994, // Unique ID to avoid conflicts
            .name = "E",
            .bounds = &.{},
        };
        const from_e_type = Type{ .type_var = from_e_type_var };

        // Create from(err: E) -> Self
        const from_func = try self.type_builder.functionType(&.{from_e_type}, from_self_type);
        const from_method = types.TraitMethod{
            .name = "from",
            .signature = from_func.function.*,
            .has_default = false,
        };

        // Store the type parameter E
        const from_type_params = try self.allocator.dupe(types.TypeVar, &.{from_e_type_var});
        try self.type_var_slices.append(self.allocator, from_type_params);

        // Complete the trait type
        from_trait_type.* = .{
            .name = "From",
            .type_params = from_type_params,
            .associated_types = &.{},
            .methods = try self.allocator.dupe(types.TraitMethod, &.{from_method}),
            .super_traits = &.{},
        };

        // Track for cleanup
        try self.trait_types.append(self.allocator, from_trait_type);

        // Register in trait registry
        try self.trait_registry.put(self.allocator, "From", .{
            .trait_type = from_trait_type,
            .decl = null, // Builtin trait, no AST declaration
        });

        // Register in symbol table as a trait
        try self.current_scope.define(.{
            .name = "From",
            .type_ = .{ .trait_ = from_trait_type },
            .kind = .trait_,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // Into trait: trait Into[T] { fn into(self) -> T; }
        // The inverse of From - converts Self into a target type T.
        // While From constructs Self from another type, Into converts Self to another type.

        // Create the trait type first
        const into_trait_type = try self.allocator.create(types.TraitType);

        // Create Self type variable for self parameter
        const into_self_type_var = types.TypeVar{
            .id = 993, // Unique ID to avoid conflicts with From's 995, 994
            .name = "Self",
            .bounds = &.{},
        };
        const into_self_type = Type{ .type_var = into_self_type_var };

        // Create T type parameter (the target type)
        const into_t_type_var = types.TypeVar{
            .id = 992, // Unique ID to avoid conflicts
            .name = "T",
            .bounds = &.{},
        };
        const into_t_type = Type{ .type_var = into_t_type_var };

        // Create into(self: Self) -> T
        const into_func = try self.type_builder.functionType(&.{into_self_type}, into_t_type);
        const into_method = types.TraitMethod{
            .name = "into",
            .signature = into_func.function.*,
            .has_default = false,
        };

        // Store the type parameter T
        const into_type_params = try self.allocator.dupe(types.TypeVar, &.{into_t_type_var});
        try self.type_var_slices.append(self.allocator, into_type_params);

        // Complete the trait type
        into_trait_type.* = .{
            .name = "Into",
            .type_params = into_type_params,
            .associated_types = &.{},
            .methods = try self.allocator.dupe(types.TraitMethod, &.{into_method}),
            .super_traits = &.{},
        };

        // Track for cleanup
        try self.trait_types.append(self.allocator, into_trait_type);

        // Register in trait registry
        try self.trait_registry.put(self.allocator, "Into", .{
            .trait_type = into_trait_type,
            .decl = null, // Builtin trait, no AST declaration
        });

        // Register in symbol table as a trait
        try self.current_scope.define(.{
            .name = "Into",
            .type_ = .{ .trait_ = into_trait_type },
            .kind = .trait_,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // ====================================================================
        // Write trait: trait Write { fn write(&mut self, buf: &[u8]) -> Result[i32, IoError]; fn flush(&mut self) -> Result[void, IoError]; }
        // ====================================================================

        // Create the trait type first
        const write_trait_type = try self.allocator.create(types.TraitType);

        // Create Self type variable for self parameter
        const write_self_type_var = types.TypeVar{
            .id = 990, // Unique ID
            .name = "Self",
            .bounds = &.{},
        };
        const write_self_type = Type{ .type_var = write_self_type_var };

        // Create &mut Self type for self parameter
        const write_self_mut_ref = try self.type_builder.referenceType(write_self_type, true);

        // Create &[u8] type for buffer parameter
        const u8_type = Type{ .primitive = .u8_ };
        const u8_slice = try self.type_builder.sliceType(u8_type);
        const u8_slice_ref = try self.type_builder.referenceType(u8_slice, false);

        // Create Result[i32, IoError] return type for write
        const write_result_type = try self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.ioErrorType());

        // Create Result[void, IoError] return type for flush
        const flush_result_type = try self.type_builder.resultType(self.type_builder.voidType(), self.type_builder.ioErrorType());

        // Create write(&mut self, buf: &[u8]) -> Result[i32, IoError]
        const write_func = try self.type_builder.functionType(&.{ write_self_mut_ref, u8_slice_ref }, write_result_type);
        const write_method = types.TraitMethod{
            .name = "write",
            .signature = write_func.function.*,
            .has_default = false,
        };

        // Create flush(&mut self) -> Result[void, IoError]
        const flush_func = try self.type_builder.functionType(&.{write_self_mut_ref}, flush_result_type);
        const flush_method = types.TraitMethod{
            .name = "flush",
            .signature = flush_func.function.*,
            .has_default = false,
        };

        // Complete the trait type (no type parameters)
        write_trait_type.* = .{
            .name = "Write",
            .type_params = &.{},
            .associated_types = &.{},
            .methods = try self.allocator.dupe(types.TraitMethod, &.{ write_method, flush_method }),
            .super_traits = &.{},
        };

        // Track for cleanup
        try self.trait_types.append(self.allocator, write_trait_type);

        // Register in trait registry
        try self.trait_registry.put(self.allocator, "Write", .{
            .trait_type = write_trait_type,
            .decl = null, // Builtin trait, no AST declaration
        });

        // Register in symbol table as a trait
        try self.current_scope.define(.{
            .name = "Write",
            .type_ = .{ .trait_ = write_trait_type },
            .kind = .trait_,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // ====================================================================
        // Read trait: trait Read { fn read(&mut self, buf: &mut [u8]) -> Result[i32, IoError]; }
        // ====================================================================

        // Create the trait type first
        const read_trait_type = try self.allocator.create(types.TraitType);

        // Create Self type variable for self parameter
        const read_self_type_var = types.TypeVar{
            .id = 989, // Unique ID
            .name = "Self",
            .bounds = &.{},
        };
        const read_self_type = Type{ .type_var = read_self_type_var };

        // Create &mut Self type for self parameter
        const read_self_mut_ref = try self.type_builder.referenceType(read_self_type, true);

        // Create &mut [u8] type for buffer parameter (mutable reference for reading into)
        const u8_slice_mut_ref = try self.type_builder.referenceType(u8_slice, true);

        // Create read(&mut self, buf: &mut [u8]) -> Result[i32, IoError]
        const read_func = try self.type_builder.functionType(&.{ read_self_mut_ref, u8_slice_mut_ref }, write_result_type);
        const read_method = types.TraitMethod{
            .name = "read",
            .signature = read_func.function.*,
            .has_default = false,
        };

        // Complete the trait type (no type parameters)
        read_trait_type.* = .{
            .name = "Read",
            .type_params = &.{},
            .associated_types = &.{},
            .methods = try self.allocator.dupe(types.TraitMethod, &.{read_method}),
            .super_traits = &.{},
        };

        // Track for cleanup
        try self.trait_types.append(self.allocator, read_trait_type);

        // Register in trait registry
        try self.trait_registry.put(self.allocator, "Read", .{
            .trait_type = read_trait_type,
            .decl = null, // Builtin trait, no AST declaration
        });

        // Register in symbol table as a trait
        try self.current_scope.define(.{
            .name = "Read",
            .type_ = .{ .trait_ = read_trait_type },
            .kind = .trait_,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // ====================================================================
        // Register I/O types
        // ====================================================================

        // File type - opaque handle for file I/O
        try self.current_scope.define(.{
            .name = "File",
            .type_ = self.type_builder.fileType(),
            .kind = .type_,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // IoError type - error enum for I/O operations
        try self.current_scope.define(.{
            .name = "IoError",
            .type_ = self.type_builder.ioErrorType(),
            .kind = .type_,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // Stdout type - handle for standard output
        try self.current_scope.define(.{
            .name = "Stdout",
            .type_ = self.type_builder.stdoutType(),
            .kind = .type_,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // Stderr type - handle for standard error
        try self.current_scope.define(.{
            .name = "Stderr",
            .type_ = self.type_builder.stderrType(),
            .kind = .type_,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // stdout() -> Stdout - builtin function to get stdout handle
        const stdout_fn_type = try self.type_builder.functionType(&.{}, self.type_builder.stdoutType());
        try self.current_scope.define(.{
            .name = "stdout",
            .type_ = stdout_fn_type,
            .kind = .function,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // stderr() -> Stderr - builtin function to get stderr handle
        const stderr_fn_type = try self.type_builder.functionType(&.{}, self.type_builder.stderrType());
        try self.current_scope.define(.{
            .name = "stderr",
            .type_ = stderr_fn_type,
            .kind = .function,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // Stdin type - handle for standard input
        try self.current_scope.define(.{
            .name = "Stdin",
            .type_ = self.type_builder.stdinType(),
            .kind = .type_,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // stdin() -> Stdin - builtin function to get stdin handle
        const stdin_fn_type = try self.type_builder.functionType(&.{}, self.type_builder.stdinType());
        try self.current_scope.define(.{
            .name = "stdin",
            .type_ = stdin_fn_type,
            .kind = .function,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // ====================================================================
        // Register builtin trait implementations for I/O types
        // ====================================================================

        // File: Write
        const file_write_key = try std.fmt.allocPrint(self.allocator, "File:Write", .{});
        const file_write_result = try self.trait_impls.getOrPut(self.allocator, file_write_key);
        if (!file_write_result.found_existing) {
            file_write_result.value_ptr.* = .{};
        }
        try file_write_result.value_ptr.append(self.allocator, .{
            .trait_name = "Write",
            .impl_type_name = "File",
            .impl_type_params = &.{},
            .associated_type_bindings = &.{},
            .methods = &.{},
        });

        // File: Read
        const file_read_key = try std.fmt.allocPrint(self.allocator, "File:Read", .{});
        const file_read_result = try self.trait_impls.getOrPut(self.allocator, file_read_key);
        if (!file_read_result.found_existing) {
            file_read_result.value_ptr.* = .{};
        }
        try file_read_result.value_ptr.append(self.allocator, .{
            .trait_name = "Read",
            .impl_type_name = "File",
            .impl_type_params = &.{},
            .associated_type_bindings = &.{},
            .methods = &.{},
        });

        // Stdout: Write
        const stdout_write_key = try std.fmt.allocPrint(self.allocator, "Stdout:Write", .{});
        const stdout_write_result = try self.trait_impls.getOrPut(self.allocator, stdout_write_key);
        if (!stdout_write_result.found_existing) {
            stdout_write_result.value_ptr.* = .{};
        }
        try stdout_write_result.value_ptr.append(self.allocator, .{
            .trait_name = "Write",
            .impl_type_name = "Stdout",
            .impl_type_params = &.{},
            .associated_type_bindings = &.{},
            .methods = &.{},
        });

        // Stderr: Write
        const stderr_write_key = try std.fmt.allocPrint(self.allocator, "Stderr:Write", .{});
        const stderr_write_result = try self.trait_impls.getOrPut(self.allocator, stderr_write_key);
        if (!stderr_write_result.found_existing) {
            stderr_write_result.value_ptr.* = .{};
        }
        try stderr_write_result.value_ptr.append(self.allocator, .{
            .trait_name = "Write",
            .impl_type_name = "Stderr",
            .impl_type_params = &.{},
            .associated_type_bindings = &.{},
            .methods = &.{},
        });

        // Stdin: Read
        const stdin_read_key = try std.fmt.allocPrint(self.allocator, "Stdin:Read", .{});
        const stdin_read_result = try self.trait_impls.getOrPut(self.allocator, stdin_read_key);
        if (!stdin_read_result.found_existing) {
            stdin_read_result.value_ptr.* = .{};
        }
        try stdin_read_result.value_ptr.append(self.allocator, .{
            .trait_name = "Read",
            .impl_type_name = "Stdin",
            .impl_type_params = &.{},
            .associated_type_bindings = &.{},
            .methods = &.{},
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

    /// Clear all accumulated errors (useful for REPL after reporting)
    pub fn clearErrors(self: *TypeChecker) void {
        // Free error messages
        for (self.errors.items) |err| {
            if (err.message.len > 0 and !std.mem.eql(u8, err.message, "error formatting message")) {
                self.allocator.free(err.message);
            }
        }
        self.errors.clearRetainingCapacity();
    }

    /// Resolve a type expression to a Type, returning void on error
    pub fn resolveType(self: *TypeChecker, type_expr: ast.TypeExpr) Type {
        return self.resolveTypeExpr(type_expr) catch {
            return .void_;
        };
    }

    /// Look up a symbol by name and return its type (if found)
    pub fn lookupType(self: *TypeChecker, name: []const u8) ?Type {
        if (self.current_scope.lookup(name)) |sym| {
            return sym.type_;
        }
        return null;
    }

    /// Look up a symbol by name in the current scope chain.
    /// Returns the full Symbol struct including original_name for aliased imports.
    pub fn lookupSymbol(self: *const TypeChecker, name: []const u8) ?Symbol {
        return self.current_scope.lookup(name);
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

    /// Get all enum types (both generic and non-generic).
    /// Non-generic enums have type_params.len == 0.
    pub fn getEnumTypes(self: *const TypeChecker) []*types.EnumType {
        return self.generic_enum_types.items;
    }

    /// Get a constant value by name.
    /// Returns null if the name doesn't refer to a constant.
    pub fn getConstantValue(self: *const TypeChecker, name: []const u8) ?ComptimeValue {
        return self.constant_values.get(name);
    }

    /// Record a monomorphized struct instance and return the concrete StructType.
    /// If the same instantiation already exists, returns the existing entry.
    pub fn recordStructMonomorphization(
        self: *TypeChecker,
        struct_name: []const u8,
        original_struct: *types.StructType,
        type_args: []const Type,
        span: Span,
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

        // Validate trait bounds for each type argument
        for (original_struct.type_params, 0..) |type_param, i| {
            if (i < type_args.len and type_param.bounds.len > 0) {
                if (!self.typeSatisfiesBounds(type_args[i], type_param.bounds, span)) {
                    return error.TypeCheckError;
                }
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
            // I/O types also don't need substitution (they have no type parameters)
            .primitive, .void_, .never, .unknown, .error_type, .file, .io_error, .stdout_handle, .stderr_handle, .stdin_handle => typ,

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

            // ContextError - substitute inner type
            .context_error => |ce| {
                const new_inner = try self.substituteTypeParams(ce.inner_type, substitutions);
                if (ce.inner_type.eql(new_inner)) return typ;
                return self.type_builder.contextErrorType(new_inner);
            },

            // Associated type reference - resolve using trait impls
            .associated_type_ref => |assoc_ref| {
                // Look up what concrete type was substituted for the type variable
                if (substitutions.get(assoc_ref.type_var.id)) |concrete_type| {
                    // Get the struct/enum name from the concrete type
                    const impl_type_name = switch (concrete_type) {
                        .struct_ => |s| s.name,
                        .enum_ => |e| e.name,
                        .applied => |app| switch (app.base) {
                            .struct_ => |s| s.name,
                            .enum_ => |e| e.name,
                            else => return typ, // Can't resolve associated type
                        },
                        else => return typ, // Can't resolve associated type for non-struct/enum
                    };

                    // Build the key for trait_impls lookup: "TypeName:TraitName"
                    const impl_key = self.makeTraitImplKey(impl_type_name, assoc_ref.trait.name);
                    defer self.allocator.free(impl_key);

                    // Look up the trait impl for this concrete type
                    if (self.trait_impls.get(impl_key)) |impls| {
                        for (impls.items) |impl_info| {
                            // Find the associated type binding
                            for (impl_info.associated_type_bindings) |binding| {
                                if (std.mem.eql(u8, binding.name, assoc_ref.assoc_name)) {
                                    return binding.concrete_type;
                                }
                            }
                        }
                    }
                }
                // Couldn't resolve - return as-is (will be an error later)
                return typ;
            },

            // Struct/Enum/Trait - return as-is (monomorphization creates applied types)
            .struct_, .enum_, .trait_ => typ,

            // Range - substitute element type
            .range => |r| {
                const new_elem = try self.substituteTypeParams(r.element_type, substitutions);
                if (r.element_type.eql(new_elem)) return typ;
                return self.type_builder.rangeType(new_elem, r.inclusive);
            },

            // List - substitute element type
            .list => |l| {
                const new_elem = try self.substituteTypeParams(l.element, substitutions);
                if (l.element.eql(new_elem)) return typ;
                return self.type_builder.listType(new_elem);
            },

            // Map - substitute key and value types
            .map => |m| {
                const new_key = try self.substituteTypeParams(m.key, substitutions);
                const new_value = try self.substituteTypeParams(m.value, substitutions);
                if (m.key.eql(new_key) and m.value.eql(new_value)) return typ;
                return self.type_builder.mapType(new_key, new_value);
            },

            // Set - substitute element type
            .set => |s| {
                const new_element = try self.substituteTypeParams(s.element, substitutions);
                if (s.element.eql(new_element)) return typ;
                return self.type_builder.setType(new_element);
            },

            // String - no type parameters to substitute
            .string_data => typ,

            // BufReader - substitute inner type
            .buf_reader => |br| {
                const new_inner = try self.substituteTypeParams(br.inner, substitutions);
                if (br.inner.eql(new_inner)) return typ;
                return self.type_builder.bufReaderType(new_inner);
            },

            // BufWriter - substitute inner type
            .buf_writer => |bw| {
                const new_inner = try self.substituteTypeParams(bw.inner, substitutions);
                if (bw.inner.eql(new_inner)) return typ;
                return self.type_builder.bufWriterType(new_inner);
            },
        };
    }

    /// Check if a type contains any type variables.
    pub fn containsTypeVar(self: *TypeChecker, typ: Type) bool {
        return switch (typ) {
            .type_var => true,
            .primitive, .void_, .never, .unknown, .error_type, .file, .io_error, .stdout_handle, .stderr_handle, .stdin_handle => false,
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
            .context_error => |ce| self.containsTypeVar(ce.inner_type),
            .range => |r| self.containsTypeVar(r.element_type),
            .list => |l| self.containsTypeVar(l.element),
            .map => |m| self.containsTypeVar(m.key) or self.containsTypeVar(m.value),
            .set => |s| self.containsTypeVar(s.element),
            // String has no type parameters
            .string_data => false,
            // Buffered I/O types have inner type that may contain type variables
            .buf_reader => |br| self.containsTypeVar(br.inner),
            .buf_writer => |bw| self.containsTypeVar(bw.inner),
            // Associated type ref contains a type variable reference
            .associated_type_ref => true,
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

            .primitive, .void_, .never, .unknown, .error_type, .file, .io_error, .stdout_handle, .stderr_handle, .stdin_handle => {
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

            .context_error => |ce| {
                if (concrete != .context_error) return false;
                return self.unifyTypes(ce.inner_type, concrete.context_error.inner_type, substitutions);
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

            .associated_type_ref => |assoc_ref| {
                // Associated type ref acts like a type variable for unification purposes
                // Try to resolve it first using existing substitutions
                if (substitutions.get(assoc_ref.type_var.id)) |_| {
                    // Resolve the associated type and compare
                    const resolved = self.substituteTypeParams(.{ .associated_type_ref = assoc_ref }, substitutions.*) catch {
                        return false;
                    };
                    if (resolved == .associated_type_ref) {
                        // Couldn't resolve - check if types match
                        return false;
                    }
                    return resolved.eql(concrete);
                }
                // No substitution for the type variable yet - can't unify
                return false;
            },

            .range => |r| {
                if (concrete != .range) return false;
                const concrete_range = concrete.range;
                if (r.inclusive != concrete_range.inclusive) return false;
                return self.unifyTypes(r.element_type, concrete_range.element_type, substitutions);
            },

            .list => |l| {
                if (concrete != .list) return false;
                return self.unifyTypes(l.element, concrete.list.element, substitutions);
            },

            .map => |m| {
                if (concrete != .map) return false;
                const concrete_map = concrete.map;
                if (!try self.unifyTypes(m.key, concrete_map.key, substitutions)) {
                    return false;
                }
                return self.unifyTypes(m.value, concrete_map.value, substitutions);
            },

            .set => |s| {
                if (concrete != .set) return false;
                return self.unifyTypes(s.element, concrete.set.element, substitutions);
            },

            // String has no type parameters - simple equality check
            .string_data => {
                return pattern.eql(concrete);
            },

            .buf_reader => |br| {
                if (concrete != .buf_reader) return false;
                return self.unifyTypes(br.inner, concrete.buf_reader.inner, substitutions);
            },

            .buf_writer => |bw| {
                if (concrete != .buf_writer) return false;
                return self.unifyTypes(bw.inner, concrete.buf_writer.inner, substitutions);
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
                // Check for String (heap-allocated string type)
                if (std.mem.eql(u8, n.name, "String")) {
                    return try self.type_builder.stringDataType();
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
                // Evaluate size expression at compile time
                if (self.evaluateComptimeExpr(a.size)) |val| {
                    if (val == .int) {
                        if (val.int.value < 0) {
                            self.addError(.comptime_error, a.span, "array size cannot be negative", .{});
                            return try self.type_builder.arrayType(elem_type, 0);
                        }
                        const size: usize = @intCast(val.int.value);
                        return try self.type_builder.arrayType(elem_type, size);
                    } else {
                        self.addError(.comptime_error, a.span, "array size must be an integer", .{});
                        return try self.type_builder.arrayType(elem_type, 0);
                    }
                } else {
                    self.addError(.comptime_error, a.span, "array size must be comptime-known", .{});
                    return try self.type_builder.arrayType(elem_type, 0);
                }
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

                    // Cell[T] - interior mutability wrapper
                    if (std.mem.eql(u8, base_name, "Cell")) {
                        if (g.args.len != 1) {
                            self.addError(.type_mismatch, g.span, "Cell expects exactly 1 type argument", .{});
                            return self.type_builder.unknownType();
                        }
                        const inner = try self.resolveTypeExpr(g.args[0]);
                        return try self.type_builder.cellType(inner);
                    }

                    // Result[T, E] - result type for error handling
                    if (std.mem.eql(u8, base_name, "Result")) {
                        if (g.args.len != 2) {
                            self.addError(.type_mismatch, g.span, "Result expects exactly 2 type arguments", .{});
                            return self.type_builder.unknownType();
                        }
                        const ok_type = try self.resolveTypeExpr(g.args[0]);
                        const err_type = try self.resolveTypeExpr(g.args[1]);
                        return try self.type_builder.resultType(ok_type, err_type);
                    }

                    // ContextError[E] - error wrapper with context message
                    if (std.mem.eql(u8, base_name, "ContextError")) {
                        if (g.args.len != 1) {
                            self.addError(.type_mismatch, g.span, "ContextError expects exactly 1 type argument", .{});
                            return self.type_builder.unknownType();
                        }
                        const inner = try self.resolveTypeExpr(g.args[0]);
                        return try self.type_builder.contextErrorType(inner);
                    }

                    // Option[T] - optional type (alternative syntax for ?T)
                    if (std.mem.eql(u8, base_name, "Option")) {
                        if (g.args.len != 1) {
                            self.addError(.type_mismatch, g.span, "Option expects exactly 1 type argument", .{});
                            return self.type_builder.unknownType();
                        }
                        const inner = try self.resolveTypeExpr(g.args[0]);
                        return try self.type_builder.optionalType(inner);
                    }

                    // Range[T] - range iterator type
                    if (std.mem.eql(u8, base_name, "Range")) {
                        if (g.args.len != 1) {
                            self.addError(.type_mismatch, g.span, "Range expects exactly 1 type argument", .{});
                            return self.type_builder.unknownType();
                        }
                        const inner = try self.resolveTypeExpr(g.args[0]);
                        if (!inner.isInteger()) {
                            self.addError(.type_mismatch, g.span, "Range element type must be an integer type", .{});
                        }
                        // Default to non-inclusive when created via type syntax
                        return try self.type_builder.rangeType(inner, false);
                    }

                    // List[T] - growable collection type
                    if (std.mem.eql(u8, base_name, "List")) {
                        if (g.args.len != 1) {
                            self.addError(.type_mismatch, g.span, "List expects exactly 1 type argument", .{});
                            return self.type_builder.unknownType();
                        }
                        const inner = try self.resolveTypeExpr(g.args[0]);
                        return try self.type_builder.listType(inner);
                    }

                    // Map[K,V] - hash map type
                    if (std.mem.eql(u8, base_name, "Map")) {
                        if (g.args.len != 2) {
                            self.addError(.type_mismatch, g.span, "Map expects exactly 2 type arguments (key and value types)", .{});
                            return self.type_builder.unknownType();
                        }
                        const key_type = try self.resolveTypeExpr(g.args[0]);
                        const value_type = try self.resolveTypeExpr(g.args[1]);
                        // Check that key type implements Hash + Eq
                        if (!self.typeImplementsHashAndEq(key_type, g.span)) {
                            // Error already reported by typeImplementsHashAndEq
                        }
                        return try self.type_builder.mapType(key_type, value_type);
                    }

                    // Set[T] - hash set type
                    if (std.mem.eql(u8, base_name, "Set")) {
                        if (g.args.len != 1) {
                            self.addError(.type_mismatch, g.span, "Set expects exactly 1 type argument (element type)", .{});
                            return self.type_builder.unknownType();
                        }
                        const element_type = try self.resolveTypeExpr(g.args[0]);
                        // Check that element type implements Hash + Eq
                        if (!self.typeImplementsHashAndEq(element_type, g.span)) {
                            // Error already reported by typeImplementsHashAndEq
                        }
                        return try self.type_builder.setType(element_type);
                    }

                    // BufReader[R] - buffered reader wrapper (R must implement Read)
                    if (std.mem.eql(u8, base_name, "BufReader")) {
                        if (g.args.len != 1) {
                            self.addError(.type_mismatch, g.span, "BufReader expects exactly 1 type argument (reader type)", .{});
                            return self.type_builder.unknownType();
                        }
                        const inner_type = try self.resolveTypeExpr(g.args[0]);
                        // Validate that inner type implements Read trait
                        if (self.trait_registry.get("Read")) |read_trait| {
                            if (!self.typeSatisfiesBounds(inner_type, &.{read_trait.trait_type}, g.span)) {
                                // Error already reported by typeSatisfiesBounds
                            }
                        }
                        return try self.type_builder.bufReaderType(inner_type);
                    }

                    // BufWriter[W] - buffered writer wrapper (W must implement Write)
                    if (std.mem.eql(u8, base_name, "BufWriter")) {
                        if (g.args.len != 1) {
                            self.addError(.type_mismatch, g.span, "BufWriter expects exactly 1 type argument (writer type)", .{});
                            return self.type_builder.unknownType();
                        }
                        const inner_type = try self.resolveTypeExpr(g.args[0]);
                        // Validate that inner type implements Write trait
                        if (self.trait_registry.get("Write")) |write_trait| {
                            if (!self.typeSatisfiesBounds(inner_type, &.{write_trait.trait_type}, g.span)) {
                                // Error already reported by typeSatisfiesBounds
                            }
                        }
                        return try self.type_builder.bufWriterType(inner_type);
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
                            g.span,
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
            .qualified => |q| {
                // Qualified type access like Self.Item or T.Associated
                const base = try self.resolveTypeExpr(q.base);

                // Currently we only support associated type access on Self or type variables
                switch (base) {
                    .type_var => |tv| {
                        // T.Item - look up the associated type in T's trait bounds
                        for (tv.bounds) |bound_trait| {
                            for (bound_trait.associated_types) |assoc| {
                                if (std.mem.eql(u8, assoc.name, q.member)) {
                                    // Found the associated type - create an AssociatedTypeRef
                                    // that will be resolved during monomorphization
                                    return self.type_builder.associatedTypeRefType(tv, q.member, bound_trait) catch {
                                        return self.type_builder.unknownType();
                                    };
                                }
                            }
                        }
                        self.addError(.undefined_type, q.span, "type variable has no associated type '{s}'", .{q.member});
                        return self.type_builder.unknownType();
                    },
                    .unknown => {
                        // Self.Item in trait context - look up in current trait's associated types
                        // For now, we need to track the current trait being checked
                        // and look up q.member in its associated types
                        if (self.current_trait_type) |trait_type| {
                            for (trait_type.associated_types) |assoc| {
                                if (std.mem.eql(u8, assoc.name, q.member)) {
                                    // Found the associated type declaration
                                    // In trait methods, Self.Item is a placeholder that will be
                                    // resolved to the concrete type during impl checking
                                    return self.type_builder.unknownType();
                                }
                            }
                            self.addError(.undefined_type, q.span, "trait has no associated type '{s}'", .{q.member});
                        } else {
                            self.addError(.undefined_type, q.span, "cannot use Self.{s} outside trait context", .{q.member});
                        }
                        return self.type_builder.unknownType();
                    },
                    else => {
                        self.addError(.undefined_type, q.span, "cannot access associated type on this type", .{});
                        return self.type_builder.unknownType();
                    },
                }
            },
        }
    }

    // ========================================================================
    // Expression Type Checking
    // ========================================================================

    pub fn checkExpr(self: *TypeChecker, expr: ast.Expr) Type {
        return self.checkExprWithHint(expr, null);
    }

    /// Check an expression with an optional type hint for contextual typing.
    /// The hint is used for numeric literals to adopt the expected type.
    pub fn checkExprWithHint(self: *TypeChecker, expr: ast.Expr, hint: ?Type) Type {
        return switch (expr) {
            .literal => |l| self.checkLiteralWithHint(l, hint),
            .identifier => |i| self.checkIdentifier(i),
            .binary => |b| self.checkBinary(b),
            .unary => |u| self.checkUnary(u),
            .postfix => |p| self.checkPostfix(p),
            .call => |c| blk: {
                // Set expected_type for Ok/Err type inference
                const prev_expected = self.expected_type;
                self.expected_type = hint;
                defer self.expected_type = prev_expected;
                break :blk self.checkCall(c);
            },
            .index => |i| self.checkIndex(i),
            .field => |f| self.checkField(f),
            .method_call => |m| self.checkMethodCall(m),
            .block => |b| self.checkBlock(b),
            .closure => |c| self.checkClosure(c),
            .range => |r| self.checkRange(r),
            .struct_literal => |s| self.checkStructLiteral(s),
            .array_literal => |a| self.checkArrayLiteral(a),
            .tuple_literal => |t| self.checkTupleLiteralWithHint(t, hint),
            .type_cast => |tc| self.checkTypeCast(tc),
            .grouped => |g| self.checkExprWithHint(g.expr, hint),
            .interpolated_string => |is| self.checkInterpolatedString(is),
            .enum_literal => |e| self.checkEnumLiteral(e),
            .comptime_block => |cb| self.checkComptimeBlock(cb),
            .builtin_call => |bc| self.checkBuiltinCall(bc),
        };
    }

    fn checkLiteralWithHint(self: *TypeChecker, lit: ast.Literal, hint: ?Type) Type {
        return switch (lit.kind) {
            .int => {
                // Use type hint if it's a compatible integer type
                if (hint) |h| {
                    if (h == .primitive and h.primitive.isInteger()) {
                        return h;
                    }
                }
                return self.type_builder.i32Type(); // Default int type
            },
            .float => {
                // Use type hint if it's a compatible float type
                if (hint) |h| {
                    if (h == .primitive and h.primitive.isFloat()) {
                        return h;
                    }
                }
                return self.type_builder.f64Type(); // Default float type
            },
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

        // String concatenation with +
        if (bin.op == .add) {
            const left_is_string = left_type == .primitive and left_type.primitive == .string_;
            const right_is_string = right_type == .primitive and right_type.primitive == .string_;
            if (left_is_string and right_is_string) {
                return self.type_builder.stringType();
            }
            if (left_is_string or right_is_string) {
                self.addError(.type_mismatch, bin.span, "cannot concatenate string with non-string", .{});
                return self.type_builder.unknownType();
            }
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
                // With new syntax, mutability is determined by whether the operand is mutable
                // `ref x` where x is `var` -> inout T (mutable reference)
                // `ref x` where x is `let` -> ref T (immutable reference)
                const is_mutable = self.isMutable(un.operand);
                return self.type_builder.referenceType(operand_type, is_mutable) catch self.type_builder.unknownType();
            },
            .ref_mut => {
                // Legacy syntax support (for backward compatibility during transition)
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
                // ? operator: propagate error/none via early return
                // Accepts both Optional and Result types

                if (operand_type != .optional and operand_type != .result) {
                    self.addError(.invalid_operation, post.span, "'?' requires optional or result type", .{});
                    return self.type_builder.unknownType();
                }

                // Validate function return type is compatible
                const return_type = self.current_return_type orelse {
                    self.addError(.invalid_operation, post.span, "'?' cannot be used outside a function", .{});
                    return self.type_builder.unknownType();
                };

                if (operand_type == .optional) {
                    // Optional? requires function to return Optional
                    if (return_type != .optional) {
                        self.addError(.type_mismatch, post.span, "'?' on optional requires function to return optional type", .{});
                    }
                    return operand_type.optional.*;
                } else {
                    // Result? requires function to return Result with same or convertible error type
                    if (return_type != .result) {
                        self.addError(.type_mismatch, post.span, "'?' on result requires function to return result type", .{});
                    } else if (!operand_type.result.err_type.eql(return_type.result.err_type)) {
                        // Error types differ - check if From trait conversion exists
                        const source_err = operand_type.result.err_type;
                        const target_err = return_type.result.err_type;

                        if (self.hasFromImpl(target_err, source_err)) {
                            // From impl exists - record the conversion for codegen
                            // Method name follows struct method naming: {TypeName}_{method_name}
                            const target_name = self.getTypeName(target_err) orelse "unknown";
                            const from_method_name = std.fmt.allocPrint(
                                self.allocator,
                                "{s}_from",
                                .{target_name},
                            ) catch "";

                            self.error_conversions.put(self.allocator, post, .{
                                .source_type = source_err,
                                .target_type = target_err,
                                .from_method_name = from_method_name,
                            }) catch {};
                        } else {
                            self.addError(.type_mismatch, post.span, "error type mismatch: cannot convert from '{s}' to '{s}'; consider implementing From[{s}] for {s}", .{
                                self.getTypeName(source_err) orelse "unknown",
                                self.getTypeName(target_err) orelse "unknown",
                                self.getTypeName(source_err) orelse "unknown",
                                self.getTypeName(target_err) orelse "unknown",
                            });
                        }
                    }
                    return operand_type.result.ok_type;
                }
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

    /// Special type checking for Ok/Err constructors that infers Result type from context.
    /// This enables Result[T, E] to work with any types, not just i32.
    fn checkOkErrCall(self: *TypeChecker, call: *ast.Call, is_ok: bool) Type {
        // Require exactly 1 argument
        if (call.args.len != 1) {
            const name = if (is_ok) "Ok" else "Err";
            self.addError(.invalid_call, call.span, "{s} requires exactly 1 argument, got {d}", .{ name, call.args.len });
            return self.type_builder.unknownType();
        }

        // Check the argument type
        const arg_type = self.checkExpr(call.args[0]);

        // If we have expected_type and it's a Result type, use its type parameters
        if (self.expected_type) |expected| {
            if (expected == .result) {
                const result = expected.result;
                if (is_ok) {
                    // Ok(value) - argument should match ok_type
                    if (!self.isTypeCompatible(result.ok_type, arg_type)) {
                        self.addError(.type_mismatch, call.args[0].span(), "argument type mismatch", .{});
                    }
                } else {
                    // Err(error) - argument should match err_type
                    if (!self.isTypeCompatible(result.err_type, arg_type)) {
                        self.addError(.type_mismatch, call.args[0].span(), "argument type mismatch", .{});
                    }
                }
                // Return the expected Result type (fully specified)
                return expected;
            }
        }

        // No expected type context - infer Result[arg_type, i32] for Ok, Result[i32, arg_type] for Err
        const i32_type = self.type_builder.i32Type();
        if (is_ok) {
            return self.type_builder.resultType(arg_type, i32_type) catch self.type_builder.unknownType();
        } else {
            return self.type_builder.resultType(i32_type, arg_type) catch self.type_builder.unknownType();
        }
    }

    /// Special type checking for Some/None constructors that infers Optional type from context.
    /// This enables ?T to work with any types, not just i32.
    fn checkSomeNoneCall(self: *TypeChecker, call: *ast.Call, is_some: bool) Type {
        if (is_some) {
            // Some(value) - requires exactly 1 argument
            if (call.args.len != 1) {
                self.addError(.invalid_call, call.span, "Some requires exactly 1 argument, got {d}", .{call.args.len});
                return self.type_builder.unknownType();
            }

            // Check the argument type
            const arg_type = self.checkExpr(call.args[0]);

            // If we have expected_type and it's an Optional type, validate it
            if (self.expected_type) |expected| {
                if (expected == .optional) {
                    const inner = expected.optional.*;
                    if (!self.isTypeCompatible(inner, arg_type)) {
                        self.addError(.type_mismatch, call.args[0].span(), "argument type mismatch", .{});
                    }
                    // Return the expected Optional type (fully specified)
                    return expected;
                }
            }

            // No expected type context - infer ?arg_type
            return self.type_builder.optionalType(arg_type) catch self.type_builder.unknownType();
        } else {
            // None - requires 0 arguments
            if (call.args.len != 0) {
                self.addError(.invalid_call, call.span, "None takes no arguments, got {d}", .{call.args.len});
                return self.type_builder.unknownType();
            }

            // None requires type context to know the inner type
            if (self.expected_type) |expected| {
                if (expected == .optional) {
                    return expected;
                }
            }

            // No type context - default to ?i32 (same pattern as Err)
            return self.type_builder.optionalType(self.type_builder.i32Type()) catch self.type_builder.unknownType();
        }
    }

    fn checkCall(self: *TypeChecker, call: *ast.Call) Type {
        // Special handling for Ok/Err constructors to infer Result type from context
        if (call.callee == .identifier) {
            const func_name = call.callee.identifier.name;
            if (std.mem.eql(u8, func_name, "Ok") or std.mem.eql(u8, func_name, "Err")) {
                return self.checkOkErrCall(call, std.mem.eql(u8, func_name, "Ok"));
            }

            // Special handling for Some/None constructors to infer Optional type from context
            if (std.mem.eql(u8, func_name, "Some") or std.mem.eql(u8, func_name, "None")) {
                return self.checkSomeNoneCall(call, std.mem.eql(u8, func_name, "Some"));
            }

            // Special handling for print/println - accept both string and String
            if (std.mem.eql(u8, func_name, "print") or std.mem.eql(u8, func_name, "println")) {
                if (call.args.len != 1) {
                    self.addError(.invalid_call, call.span, "{s} requires exactly 1 argument, got {d}", .{ func_name, call.args.len });
                    return self.type_builder.voidType();
                }
                const arg_type = self.checkExpr(call.args[0]);
                // Accept string (primitive) or String (string_data)
                const is_string = arg_type == .primitive and arg_type.primitive == .string_;
                const is_string_data = arg_type == .string_data;
                if (!is_string and !is_string_data) {
                    self.addError(.type_mismatch, call.args[0].span(), "print/println expects string or String", .{});
                }
                return self.type_builder.voidType();
            }

            // Special handling for debug - accepts any type, returns string
            if (std.mem.eql(u8, func_name, "debug")) {
                if (call.args.len != 1) {
                    self.addError(.invalid_call, call.span, "debug requires exactly 1 argument, got {d}", .{call.args.len});
                    return self.type_builder.stringType();
                }
                const arg_type = self.checkExpr(call.args[0]);
                // Store the argument type for codegen to use
                self.debug_call_types.put(self.allocator, call, arg_type) catch {};
                return self.type_builder.stringType();
            }
        }

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

        // Check if this is a comptime function call
        if (call.callee == .identifier) {
            const func_name = call.callee.identifier.name;
            if (self.comptime_functions.get(func_name)) |comptime_func| {
                // Evaluate comptime function at compile time
                const result = self.evaluateComptimeCall(call, comptime_func);
                if (result) |comptime_value| {
                    self.comptime_call_values.put(self.allocator, call, comptime_value) catch {};
                    return func.return_type;
                }
                // Evaluation failed, error already reported
                return func.return_type;
            }
        }

        // Check comptime parameters receive comptime-known arguments
        if (func.comptime_params != 0) {
            for (call.args, 0..) |arg, i| {
                if (i < 64 and (func.comptime_params & (@as(u64, 1) << @intCast(i))) != 0) {
                    // This parameter is comptime, verify argument is comptime-known
                    if (self.evaluateComptimeExpr(arg) == null) {
                        self.addError(.comptime_error, arg.span(), "argument to comptime parameter must be comptime-known", .{});
                    }
                }
            }
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
        var object_type = self.checkExpr(fld.object);

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
                self.addError(.undefined_field, fld.span, "no field '{s}' on struct", .{fld.field_name});
                return self.type_builder.unknownType();
            },
            .applied => |a| {
                // For applied types like Pair[T], access fields from the base struct
                if (a.base == .struct_) {
                    const s = a.base.struct_;
                    for (s.fields) |field| {
                        if (std.mem.eql(u8, field.name, fld.field_name)) {
                            // Build substitution map from struct's type params to applied args
                            var substitutions = std.AutoHashMapUnmanaged(u32, Type){};
                            defer substitutions.deinit(self.allocator);

                            // Map each struct type param to corresponding applied arg
                            const min_len = @min(s.type_params.len, a.args.len);
                            for (s.type_params[0..min_len], a.args[0..min_len]) |type_param, arg| {
                                substitutions.put(self.allocator, type_param.id, arg) catch {};
                            }

                            // Substitute type vars in field type
                            return self.substituteTypeParams(field.type_, substitutions) catch field.type_;
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

            // Default trait: TypeName.default() -> T where T implements Default
            // Primitives have builtin Default implementations:
            // i32, i64, etc. -> 0, f32, f64 -> 0.0, bool -> false, string -> ""
            if (std.mem.eql(u8, method.method_name, "default")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "default() takes no arguments", .{});
                    return self.type_builder.unknownType();
                }

                // Check if obj_name is a primitive type
                const primitive_types = [_]struct { name: []const u8, prim: types.Primitive }{
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

                for (primitive_types) |pt| {
                    if (std.mem.eql(u8, obj_name, pt.name)) {
                        return Type{ .primitive = pt.prim };
                    }
                }

                // Check if it's a struct that implements Default
                if (self.current_scope.lookup(obj_name)) |sym| {
                    if (sym.type_ == .struct_) {
                        if (self.typeImplementsTrait(obj_name, "Default")) {
                            // Return the struct type - the struct has implemented default()
                            return sym.type_;
                        }
                        self.addError(.undefined_method, method.span, "type '{s}' does not implement Default trait", .{obj_name});
                        return self.type_builder.unknownType();
                    }
                }

                self.addError(.undefined_method, method.span, "unknown type '{s}' for Default::default()", .{obj_name});
                return self.type_builder.unknownType();
            }

            // List.new[T]() -> List[T] - static constructor with type argument
            if (std.mem.eql(u8, obj_name, "List") and std.mem.eql(u8, method.method_name, "new")) {
                if (method.type_args) |type_args| {
                    if (type_args.len != 1) {
                        self.addError(.invalid_call, method.span, "List.new[T]() expects exactly 1 type argument", .{});
                        return self.type_builder.unknownType();
                    }
                    if (method.args.len != 0) {
                        self.addError(.invalid_call, method.span, "List.new[T]() takes no value arguments", .{});
                    }
                    const element_type = self.resolveTypeExpr(type_args[0]) catch {
                        return self.type_builder.unknownType();
                    };
                    return self.type_builder.listType(element_type) catch self.type_builder.unknownType();
                }
                self.addError(.invalid_call, method.span, "List.new() requires a type argument: List.new[i32]()", .{});
                return self.type_builder.unknownType();
            }

            // List.with_capacity[T](n) -> List[T] - static constructor with pre-allocated capacity
            if (std.mem.eql(u8, obj_name, "List") and std.mem.eql(u8, method.method_name, "with_capacity")) {
                if (method.type_args) |type_args| {
                    if (type_args.len != 1) {
                        self.addError(.invalid_call, method.span, "List.with_capacity[T](n) expects exactly 1 type argument", .{});
                        return self.type_builder.unknownType();
                    }
                    if (method.args.len != 1) {
                        self.addError(.invalid_call, method.span, "List.with_capacity[T](n) takes exactly 1 argument (capacity)", .{});
                        return self.type_builder.unknownType();
                    }
                    // Check that the argument is an integer (capacity)
                    const arg_type = self.checkExpr(method.args[0]);
                    if (arg_type != .primitive or (arg_type.primitive != .i32_ and arg_type.primitive != .i64_)) {
                        self.addError(.type_mismatch, method.span, "List.with_capacity expects an integer capacity argument", .{});
                    }
                    const element_type = self.resolveTypeExpr(type_args[0]) catch {
                        return self.type_builder.unknownType();
                    };
                    return self.type_builder.listType(element_type) catch self.type_builder.unknownType();
                }
                self.addError(.invalid_call, method.span, "List.with_capacity() requires a type argument: List.with_capacity[i32](10)", .{});
                return self.type_builder.unknownType();
            }

            // Map.new[K,V]() -> Map[K,V] - static constructor with type arguments
            // K must implement Hash + Eq traits
            if (std.mem.eql(u8, obj_name, "Map") and std.mem.eql(u8, method.method_name, "new")) {
                if (method.type_args) |type_args| {
                    if (type_args.len != 2) {
                        self.addError(.invalid_call, method.span, "Map.new[K,V]() expects exactly 2 type arguments", .{});
                        return self.type_builder.unknownType();
                    }
                    if (method.args.len != 0) {
                        self.addError(.invalid_call, method.span, "Map.new[K,V]() takes no value arguments", .{});
                    }
                    const key_type = self.resolveTypeExpr(type_args[0]) catch {
                        return self.type_builder.unknownType();
                    };
                    const value_type = self.resolveTypeExpr(type_args[1]) catch {
                        return self.type_builder.unknownType();
                    };
                    // Check that K implements Hash + Eq traits
                    if (!self.typeImplementsHashAndEq(key_type, method.span)) {
                        // Error already reported by typeImplementsHashAndEq
                    }
                    return self.type_builder.mapType(key_type, value_type) catch self.type_builder.unknownType();
                }
                self.addError(.invalid_call, method.span, "Map.new() requires type arguments: Map.new[i32, string]()", .{});
                return self.type_builder.unknownType();
            }

            // Map.with_capacity[K,V](n) -> Map[K,V] - static constructor with pre-allocated capacity
            if (std.mem.eql(u8, obj_name, "Map") and std.mem.eql(u8, method.method_name, "with_capacity")) {
                if (method.type_args) |type_args| {
                    if (type_args.len != 2) {
                        self.addError(.invalid_call, method.span, "Map.with_capacity[K,V](n) expects exactly 2 type arguments", .{});
                        return self.type_builder.unknownType();
                    }
                    if (method.args.len != 1) {
                        self.addError(.invalid_call, method.span, "Map.with_capacity[K,V](n) takes exactly 1 argument (capacity)", .{});
                        return self.type_builder.unknownType();
                    }
                    // Check that the argument is an integer (capacity)
                    const arg_type = self.checkExpr(method.args[0]);
                    if (arg_type != .primitive or (arg_type.primitive != .i32_ and arg_type.primitive != .i64_)) {
                        self.addError(.type_mismatch, method.span, "Map.with_capacity expects an integer capacity argument", .{});
                    }
                    const key_type = self.resolveTypeExpr(type_args[0]) catch {
                        return self.type_builder.unknownType();
                    };
                    const value_type = self.resolveTypeExpr(type_args[1]) catch {
                        return self.type_builder.unknownType();
                    };
                    // Check that K implements Hash + Eq traits
                    if (!self.typeImplementsHashAndEq(key_type, method.span)) {
                        // Error already reported by typeImplementsHashAndEq
                    }
                    return self.type_builder.mapType(key_type, value_type) catch self.type_builder.unknownType();
                }
                self.addError(.invalid_call, method.span, "Map.with_capacity() requires type arguments: Map.with_capacity[i32, string](10)", .{});
                return self.type_builder.unknownType();
            }

            // Set.new[T]() -> Set[T] - static constructor with type arguments
            // T must implement Hash + Eq traits
            if (std.mem.eql(u8, obj_name, "Set") and std.mem.eql(u8, method.method_name, "new")) {
                if (method.type_args) |type_args| {
                    if (type_args.len != 1) {
                        self.addError(.invalid_call, method.span, "Set.new[T]() expects exactly 1 type argument", .{});
                        return self.type_builder.unknownType();
                    }
                    if (method.args.len != 0) {
                        self.addError(.invalid_call, method.span, "Set.new[T]() takes no value arguments", .{});
                    }
                    const element_type = self.resolveTypeExpr(type_args[0]) catch {
                        return self.type_builder.unknownType();
                    };
                    // Check that T implements Hash + Eq traits
                    if (!self.typeImplementsHashAndEq(element_type, method.span)) {
                        // Error already reported by typeImplementsHashAndEq
                    }
                    return self.type_builder.setType(element_type) catch self.type_builder.unknownType();
                }
                self.addError(.invalid_call, method.span, "Set.new() requires type arguments: Set.new[i32]()", .{});
                return self.type_builder.unknownType();
            }

            // Set.with_capacity[T](n) -> Set[T] - static constructor with pre-allocated capacity
            if (std.mem.eql(u8, obj_name, "Set") and std.mem.eql(u8, method.method_name, "with_capacity")) {
                if (method.type_args) |type_args| {
                    if (type_args.len != 1) {
                        self.addError(.invalid_call, method.span, "Set.with_capacity[T](n) expects exactly 1 type argument", .{});
                        return self.type_builder.unknownType();
                    }
                    if (method.args.len != 1) {
                        self.addError(.invalid_call, method.span, "Set.with_capacity[T](n) takes exactly 1 argument (capacity)", .{});
                        return self.type_builder.unknownType();
                    }
                    // Check that the argument is an integer (capacity)
                    const arg_type = self.checkExpr(method.args[0]);
                    if (arg_type != .primitive or (arg_type.primitive != .i32_ and arg_type.primitive != .i64_)) {
                        self.addError(.type_mismatch, method.span, "Set.with_capacity expects an integer capacity argument", .{});
                    }
                    const element_type = self.resolveTypeExpr(type_args[0]) catch {
                        return self.type_builder.unknownType();
                    };
                    // Check that T implements Hash + Eq traits
                    if (!self.typeImplementsHashAndEq(element_type, method.span)) {
                        // Error already reported by typeImplementsHashAndEq
                    }
                    return self.type_builder.setType(element_type) catch self.type_builder.unknownType();
                }
                self.addError(.invalid_call, method.span, "Set.with_capacity() requires type arguments: Set.with_capacity[i32](10)", .{});
                return self.type_builder.unknownType();
            }

            // String.new() -> String - static constructor for empty string
            if (std.mem.eql(u8, obj_name, "String") and std.mem.eql(u8, method.method_name, "new")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "String.new() takes no arguments", .{});
                }
                if (method.type_args != null) {
                    self.addError(.invalid_call, method.span, "String.new() does not take type arguments", .{});
                }
                return self.type_builder.stringDataType() catch self.type_builder.unknownType();
            }

            // String.from(s: string) -> String - static constructor from string literal
            if (std.mem.eql(u8, obj_name, "String") and std.mem.eql(u8, method.method_name, "from")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "String.from() takes exactly 1 argument", .{});
                    return self.type_builder.unknownType();
                }
                if (method.type_args != null) {
                    self.addError(.invalid_call, method.span, "String.from() does not take type arguments", .{});
                }
                // Check that the argument is a string literal
                const arg_type = self.checkExpr(method.args[0]);
                if (arg_type != .primitive or arg_type.primitive != .string_) {
                    self.addError(.type_mismatch, method.span, "String.from() expects a string argument", .{});
                }
                return self.type_builder.stringDataType() catch self.type_builder.unknownType();
            }

            // String.with_capacity(n: i32) -> String - static constructor with pre-allocated capacity
            if (std.mem.eql(u8, obj_name, "String") and std.mem.eql(u8, method.method_name, "with_capacity")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "String.with_capacity() takes exactly 1 argument (capacity)", .{});
                    return self.type_builder.unknownType();
                }
                if (method.type_args != null) {
                    self.addError(.invalid_call, method.span, "String.with_capacity() does not take type arguments", .{});
                }
                // Check that the argument is an integer
                const arg_type = self.checkExpr(method.args[0]);
                if (arg_type != .primitive or (arg_type.primitive != .i32_ and arg_type.primitive != .i64_)) {
                    self.addError(.type_mismatch, method.span, "String.with_capacity() expects an integer argument", .{});
                }
                return self.type_builder.stringDataType() catch self.type_builder.unknownType();
            }

            // File.open(path: string, mode: string) -> Result[File, IoError]
            if (std.mem.eql(u8, obj_name, "File") and std.mem.eql(u8, method.method_name, "open")) {
                if (method.args.len != 2) {
                    self.addError(.invalid_call, method.span, "File.open() takes exactly 2 arguments (path, mode)", .{});
                    return self.type_builder.unknownType();
                }
                if (method.type_args != null) {
                    self.addError(.invalid_call, method.span, "File.open() does not take type arguments", .{});
                }
                // Check that both arguments are strings
                const path_type = self.checkExpr(method.args[0]);
                if (path_type != .primitive or path_type.primitive != .string_) {
                    self.addError(.type_mismatch, method.span, "File.open() expects a string path argument", .{});
                }
                const mode_type = self.checkExpr(method.args[1]);
                if (mode_type != .primitive or mode_type.primitive != .string_) {
                    self.addError(.type_mismatch, method.span, "File.open() expects a string mode argument", .{});
                }
                // Return Result[File, IoError]
                return self.type_builder.resultType(self.type_builder.fileType(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
            }

            // File.read_to_string(path: string) -> Result[String, IoError]
            if (std.mem.eql(u8, obj_name, "File") and std.mem.eql(u8, method.method_name, "read_to_string")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "File.read_to_string() takes exactly 1 argument (path)", .{});
                    return self.type_builder.unknownType();
                }
                if (method.type_args != null) {
                    self.addError(.invalid_call, method.span, "File.read_to_string() does not take type arguments", .{});
                }
                // Check that the argument is a string
                const path_type = self.checkExpr(method.args[0]);
                if (path_type != .primitive or path_type.primitive != .string_) {
                    self.addError(.type_mismatch, method.span, "File.read_to_string() expects a string path argument", .{});
                }
                // Return Result[String, IoError]
                const string_type = self.type_builder.stringDataType() catch return self.type_builder.unknownType();
                return self.type_builder.resultType(string_type, self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
            }

            // File.read_all(path: string) -> Result[List[u8], IoError]
            if (std.mem.eql(u8, obj_name, "File") and std.mem.eql(u8, method.method_name, "read_all")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "File.read_all() takes exactly 1 argument (path)", .{});
                    return self.type_builder.unknownType();
                }
                if (method.type_args != null) {
                    self.addError(.invalid_call, method.span, "File.read_all() does not take type arguments", .{});
                }
                // Check that the argument is a string
                const path_type = self.checkExpr(method.args[0]);
                if (path_type != .primitive or path_type.primitive != .string_) {
                    self.addError(.type_mismatch, method.span, "File.read_all() expects a string path argument", .{});
                }
                // Return Result[List[u8], IoError]
                const u8_type = Type{ .primitive = .u8_ };
                const list_u8_type = self.type_builder.listType(u8_type) catch return self.type_builder.unknownType();
                return self.type_builder.resultType(list_u8_type, self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
            }

            // BufReader.new[R](reader: R) -> BufReader[R]
            // Creates a new buffered reader wrapping the given reader
            if (std.mem.eql(u8, obj_name, "BufReader") and std.mem.eql(u8, method.method_name, "new")) {
                if (method.type_args) |type_args| {
                    if (type_args.len != 1) {
                        self.addError(.invalid_call, method.span, "BufReader.new[R]() expects exactly 1 type argument", .{});
                        return self.type_builder.unknownType();
                    }
                    if (method.args.len != 1) {
                        self.addError(.invalid_call, method.span, "BufReader.new[R](reader) takes exactly 1 argument", .{});
                        return self.type_builder.unknownType();
                    }
                    const inner_type = self.resolveTypeExpr(type_args[0]) catch {
                        return self.type_builder.unknownType();
                    };
                    // Validate that inner type implements Read trait
                    if (self.trait_registry.get("Read")) |read_trait| {
                        if (!self.typeSatisfiesBounds(inner_type, &.{read_trait.trait_type}, method.span)) {
                            // Error already reported
                        }
                    }
                    // Check that argument matches the type parameter
                    const arg_type = self.checkExpr(method.args[0]);
                    if (!arg_type.eql(inner_type)) {
                        self.addError(.type_mismatch, method.span, "argument type must match type parameter", .{});
                    }
                    return self.type_builder.bufReaderType(inner_type) catch self.type_builder.unknownType();
                }
                self.addError(.invalid_call, method.span, "BufReader.new() requires a type argument: BufReader.new[File](file)", .{});
                return self.type_builder.unknownType();
            }

            // BufWriter.new[W](writer: W) -> BufWriter[W]
            // Creates a new buffered writer wrapping the given writer
            if (std.mem.eql(u8, obj_name, "BufWriter") and std.mem.eql(u8, method.method_name, "new")) {
                if (method.type_args) |type_args| {
                    if (type_args.len != 1) {
                        self.addError(.invalid_call, method.span, "BufWriter.new[W]() expects exactly 1 type argument", .{});
                        return self.type_builder.unknownType();
                    }
                    if (method.args.len != 1) {
                        self.addError(.invalid_call, method.span, "BufWriter.new[W](writer) takes exactly 1 argument", .{});
                        return self.type_builder.unknownType();
                    }
                    const inner_type = self.resolveTypeExpr(type_args[0]) catch {
                        return self.type_builder.unknownType();
                    };
                    // Validate that inner type implements Write trait
                    if (self.trait_registry.get("Write")) |write_trait| {
                        if (!self.typeSatisfiesBounds(inner_type, &.{write_trait.trait_type}, method.span)) {
                            // Error already reported
                        }
                    }
                    // Check that argument matches the type parameter
                    const arg_type = self.checkExpr(method.args[0]);
                    if (!arg_type.eql(inner_type)) {
                        self.addError(.type_mismatch, method.span, "argument type must match type parameter", .{});
                    }
                    return self.type_builder.bufWriterType(inner_type) catch self.type_builder.unknownType();
                }
                self.addError(.invalid_call, method.span, "BufWriter.new() requires a type argument: BufWriter.new[File](file)", .{});
                return self.type_builder.unknownType();
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
        // to_string() returns String (heap-allocated), not string (primitive literal)
        if (std.mem.eql(u8, method.method_name, "to_string")) {
            return self.type_builder.stringDataType() catch self.type_builder.unknownType();
        }

        // Check for len method on arrays, tuples, strings, lists, maps, sets, and String
        // Returns i32 for ergonomic use with loop counters
        if (std.mem.eql(u8, method.method_name, "len")) {
            if (object_type == .primitive and object_type.primitive == .string_) {
                return .{ .primitive = .i32_ };
            }
            if (object_type == .array or object_type == .slice or object_type == .tuple or object_type == .list or object_type == .string_data or object_type == .map or object_type == .set) {
                return .{ .primitive = .i32_ };
            }
            self.addError(.undefined_method, method.span, "len() requires array, tuple, string, list, map, or set", .{});
            return self.type_builder.unknownType();
        }

        // Check for is_empty method on strings, arrays, lists, maps, sets, and String
        if (std.mem.eql(u8, method.method_name, "is_empty")) {
            if (object_type == .primitive and object_type.primitive == .string_) {
                return self.type_builder.boolType();
            }
            if (object_type == .list or object_type == .string_data or object_type == .map or object_type == .set) {
                return self.type_builder.boolType();
            }
            if (object_type == .array or object_type == .slice) {
                return self.type_builder.boolType();
            }
        }

        // Eq trait: eq() method on primitives and structs that implement Eq
        if (std.mem.eql(u8, method.method_name, "eq")) {
            // Check argument count - eq takes exactly one argument (other)
            if (method.args.len != 1) {
                self.addError(.invalid_call, method.span, "eq() expects exactly 1 argument", .{});
                return self.type_builder.boolType();
            }

            const arg_type = self.checkExpr(method.args[0]);

            // Primitives have builtin Eq implementation
            if (object_type == .primitive) {
                // Check that the argument type matches (or is a reference to the same type)
                const expected_type = object_type;
                var actual_type = arg_type;

                // Handle reference types - unwrap if necessary
                if (arg_type == .reference) {
                    actual_type = arg_type.reference.inner;
                }

                if (!actual_type.eql(expected_type)) {
                    self.addError(.type_mismatch, method.span, "eq() argument type mismatch: expected same type as receiver", .{});
                }
                return self.type_builder.boolType();
            }

            // For struct types, check if the struct implements Eq
            if (object_type == .struct_) {
                const struct_name = object_type.struct_.name;
                if (self.typeImplementsTrait(struct_name, "Eq")) {
                    // Check argument type matches
                    var actual_type = arg_type;
                    if (arg_type == .reference) {
                        actual_type = arg_type.reference.inner;
                    }
                    if (!actual_type.eql(object_type)) {
                        self.addError(.type_mismatch, method.span, "eq() argument type mismatch", .{});
                    }
                    return self.type_builder.boolType();
                }
            }

            // Type variable with Eq bound - handled below in the generic type_var section
        }

        // Ordered trait: lt(), le(), gt(), ge() methods on primitives and structs that implement Ordered
        if (std.mem.eql(u8, method.method_name, "lt") or
            std.mem.eql(u8, method.method_name, "le") or
            std.mem.eql(u8, method.method_name, "gt") or
            std.mem.eql(u8, method.method_name, "ge"))
        {
            // Check argument count - comparison methods take exactly one argument (other)
            if (method.args.len != 1) {
                self.addError(.invalid_call, method.span, "{s}() expects exactly 1 argument", .{method.method_name});
                return self.type_builder.boolType();
            }

            const arg_type = self.checkExpr(method.args[0]);

            // Primitives have builtin Ordered implementation (int, float, string)
            if (object_type == .primitive) {
                const prim = object_type.primitive;
                // Only numeric types and strings support ordering
                if (prim.isInteger() or prim.isFloat() or prim == .string_) {
                    // Check that the argument type matches (or is a reference to the same type)
                    const expected_type = object_type;
                    var actual_type = arg_type;

                    // Handle reference types - unwrap if necessary
                    if (arg_type == .reference) {
                        actual_type = arg_type.reference.inner;
                    }

                    if (!actual_type.eql(expected_type)) {
                        self.addError(.type_mismatch, method.span, "{s}() argument type mismatch: expected same type as receiver", .{method.method_name});
                    }
                    return self.type_builder.boolType();
                } else {
                    // bool type doesn't support ordering
                    self.addError(.invalid_call, method.span, "{s}() is not defined for type {s}", .{ method.method_name, @tagName(prim) });
                    return self.type_builder.boolType();
                }
            }

            // For struct types, check if the struct implements Ordered
            if (object_type == .struct_) {
                const struct_name = object_type.struct_.name;
                if (self.typeImplementsTrait(struct_name, "Ordered")) {
                    // Check argument type matches
                    var actual_type = arg_type;
                    if (arg_type == .reference) {
                        actual_type = arg_type.reference.inner;
                    }
                    if (!actual_type.eql(object_type)) {
                        self.addError(.type_mismatch, method.span, "{s}() argument type mismatch", .{method.method_name});
                    }
                    return self.type_builder.boolType();
                }
            }

            // Type variable with Ordered bound - handled below in the generic type_var section
        }

        // Clone trait: clone() method on primitives and structs that implement Clone
        if (std.mem.eql(u8, method.method_name, "clone")) {
            // clone() takes no arguments
            if (method.args.len != 0) {
                self.addError(.invalid_call, method.span, "clone() expects no arguments", .{});
                return self.type_builder.unknownType();
            }

            // All primitives have builtin Clone implementation (value types are trivially cloneable)
            if (object_type == .primitive) {
                return object_type;
            }

            // For struct types, check if the struct implements Clone
            if (object_type == .struct_) {
                const struct_name = object_type.struct_.name;
                if (self.typeImplementsTrait(struct_name, "Clone")) {
                    return object_type;
                }
            }

            // Type variable with Clone bound - handled below in the generic type_var section
        }

        // Drop trait: drop() method for explicit destruction
        // Unlike Clone/Eq/Ordered, Drop is NOT automatically provided for primitives.
        // Only types that explicitly implement Drop can have drop() called on them.
        if (std.mem.eql(u8, method.method_name, "drop")) {
            // drop() takes no arguments (only self)
            if (method.args.len != 0) {
                self.addError(.invalid_call, method.span, "drop() expects no arguments", .{});
                return self.type_builder.voidType();
            }

            // Primitives do NOT implement Drop - they are trivially destroyed
            if (object_type == .primitive) {
                self.addError(.undefined_method, method.span, "primitives do not implement Drop; they are trivially destroyed", .{});
                return self.type_builder.voidType();
            }

            // For struct types, check if the struct implements Drop
            if (object_type == .struct_) {
                const struct_name = object_type.struct_.name;
                if (self.typeImplementsTrait(struct_name, "Drop")) {
                    return self.type_builder.voidType();
                } else {
                    self.addError(.undefined_method, method.span, "type '{s}' does not implement Drop", .{struct_name});
                    return self.type_builder.voidType();
                }
            }

            // Type variable with Drop bound - handled below in the generic type_var section
        }

        // Hash trait: hash() method returns i64 hash code
        // All primitives have builtin Hash implementation.
        if (std.mem.eql(u8, method.method_name, "hash")) {
            // hash() takes no arguments (only self)
            if (method.args.len != 0) {
                self.addError(.invalid_call, method.span, "hash() expects no arguments", .{});
                return self.type_builder.i64Type();
            }

            // All primitives have builtin Hash implementation
            if (object_type == .primitive) {
                return self.type_builder.i64Type();
            }

            // For struct types, check if the struct implements Hash
            if (object_type == .struct_) {
                const struct_name = object_type.struct_.name;
                if (self.typeImplementsTrait(struct_name, "Hash")) {
                    return self.type_builder.i64Type();
                } else {
                    self.addError(.undefined_method, method.span, "type '{s}' does not implement Hash", .{struct_name});
                    return self.type_builder.i64Type();
                }
            }

            // Type variable with Hash bound - handled below in the generic type_var section
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
                // Return slice of char
                return self.type_builder.sliceType(self.type_builder.charType()) catch return self.type_builder.unknownType();
            }
            if (std.mem.eql(u8, method.method_name, "bytes")) {
                // Return slice of u8
                const u8_type: Type = .{ .primitive = .u8_ };
                return self.type_builder.sliceType(u8_type) catch return self.type_builder.unknownType();
            }
        }

        // Integer methods
        if (object_type.isInteger()) {
            if (std.mem.eql(u8, method.method_name, "abs")) {
                // abs() takes no arguments
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "abs() takes no arguments", .{});
                }
                return object_type;
            }
            if (std.mem.eql(u8, method.method_name, "min") or
                std.mem.eql(u8, method.method_name, "max"))
            {
                // min(other) and max(other) take exactly one argument of the same type
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "{s}() takes exactly 1 argument", .{method.method_name});
                } else {
                    const arg_type = self.checkExpr(method.args[0]);
                    if (!arg_type.eql(object_type)) {
                        self.addError(.type_mismatch, method.span, "{s}() argument must match receiver type", .{method.method_name});
                    }
                }
                return object_type;
            }
        }

        // Array methods
        if (object_type == .array or object_type == .slice) {
            const element_type = if (object_type == .array) object_type.array.element else object_type.slice.element;

            if (std.mem.eql(u8, method.method_name, "first") or
                std.mem.eql(u8, method.method_name, "last"))
            {
                // first() and last() take no arguments, return Optional[element_type]
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "{s}() takes no arguments", .{method.method_name});
                }
                return self.type_builder.optionalType(element_type) catch self.type_builder.unknownType();
            }
            if (std.mem.eql(u8, method.method_name, "get")) {
                // get(index) returns Optional[element_type]
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "get() takes exactly 1 argument (index)", .{});
                } else {
                    const arg_type = self.checkExpr(method.args[0]);
                    if (!arg_type.isInteger()) {
                        self.addError(.type_mismatch, method.span, "get() index must be an integer", .{});
                    }
                }
                return self.type_builder.optionalType(element_type) catch self.type_builder.unknownType();
            }
            if (std.mem.eql(u8, method.method_name, "contains")) {
                // contains(value) checks if array contains the value
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "contains() takes exactly 1 argument", .{});
                } else {
                    const arg_type = self.checkExpr(method.args[0]);
                    if (!arg_type.eql(element_type)) {
                        self.addError(.type_mismatch, method.span, "contains() argument must match array element type", .{});
                    }
                }
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
            // map(f: fn(T) -> U) -> ?U
            if (std.mem.eql(u8, method.method_name, "map")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "map() takes exactly 1 argument (a function)", .{});
                    return self.type_builder.unknownType();
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (arg_type == .function) {
                    // Return Optional of the function's return type
                    return self.type_builder.optionalType(arg_type.function.return_type) catch self.type_builder.unknownType();
                }
                self.addError(.type_mismatch, method.span, "map() argument must be a function", .{});
                return self.type_builder.unknownType();
            }
            // and_then(f: fn(T) -> ?U) -> ?U
            if (std.mem.eql(u8, method.method_name, "and_then")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "and_then() takes exactly 1 argument (a function)", .{});
                    return self.type_builder.unknownType();
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (arg_type == .function) {
                    // Return the function's return type directly (should be Optional)
                    return arg_type.function.return_type;
                }
                self.addError(.type_mismatch, method.span, "and_then() argument must be a function", .{});
                return self.type_builder.unknownType();
            }
            // eq(other: ?T) -> bool (Eq trait for Optional)
            if (std.mem.eql(u8, method.method_name, "eq")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "eq() expects exactly 1 argument", .{});
                    return self.type_builder.boolType();
                }
                const arg_type = self.checkExpr(method.args[0]);
                // Argument should be the same Optional type (or a reference to it)
                var actual_arg_type = arg_type;
                if (arg_type == .reference) {
                    actual_arg_type = arg_type.reference.inner;
                }
                if (!actual_arg_type.eql(object_type)) {
                    self.addError(.type_mismatch, method.span, "eq() argument type mismatch: expected same Optional type", .{});
                }
                return self.type_builder.boolType();
            }
            // clone() -> ?T (Clone trait for Optional)
            if (std.mem.eql(u8, method.method_name, "clone")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "clone() expects no arguments", .{});
                }
                return object_type;
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

            // map(f: fn(T) -> U) -> Result[U, E]
            if (std.mem.eql(u8, method.method_name, "map")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "map() takes exactly 1 argument (a function)", .{});
                    return self.type_builder.unknownType();
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (arg_type == .function) {
                    // Return Result[U, E] where U is the function's return type
                    return self.type_builder.resultType(arg_type.function.return_type, result_type.err_type) catch self.type_builder.unknownType();
                }
                self.addError(.type_mismatch, method.span, "map() argument must be a function", .{});
                return self.type_builder.unknownType();
            }

            // and_then(f: fn(T) -> Result[U, E]) -> Result[U, E]
            if (std.mem.eql(u8, method.method_name, "and_then")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "and_then() takes exactly 1 argument (a function)", .{});
                    return self.type_builder.unknownType();
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (arg_type == .function) {
                    // Return the function's return type directly (should be Result)
                    return arg_type.function.return_type;
                }
                self.addError(.type_mismatch, method.span, "and_then() argument must be a function", .{});
                return self.type_builder.unknownType();
            }

            // map_err(f: fn(E) -> F) -> Result[T, F]
            if (std.mem.eql(u8, method.method_name, "map_err")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "map_err() takes exactly 1 argument (a function)", .{});
                    return self.type_builder.unknownType();
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (arg_type == .function) {
                    // Return Result[T, F] where T is unchanged and F is the function's return type
                    return self.type_builder.resultType(result_type.ok_type, arg_type.function.return_type) catch self.type_builder.unknownType();
                }
                self.addError(.type_mismatch, method.span, "map_err() argument must be a function", .{});
                return self.type_builder.unknownType();
            }
            // eq(other: Result[T, E]) -> bool (Eq trait for Result)
            if (std.mem.eql(u8, method.method_name, "eq")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "eq() expects exactly 1 argument", .{});
                    return self.type_builder.boolType();
                }
                const arg_type = self.checkExpr(method.args[0]);
                // Argument should be the same Result type (or a reference to it)
                var actual_arg_type = arg_type;
                if (arg_type == .reference) {
                    actual_arg_type = arg_type.reference.inner;
                }
                if (!actual_arg_type.eql(object_type)) {
                    self.addError(.type_mismatch, method.span, "eq() argument type mismatch: expected same Result type", .{});
                }
                return self.type_builder.boolType();
            }
            // clone() -> Result[T, E] (Clone trait for Result)
            if (std.mem.eql(u8, method.method_name, "clone")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "clone() expects no arguments", .{});
                }
                return object_type;
            }

            // context(msg: string) -> Result[T, ContextError[E]]
            // Wraps the error type with a context message
            if (std.mem.eql(u8, method.method_name, "context")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "context() takes exactly 1 argument (a string message)", .{});
                    return self.type_builder.unknownType();
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (arg_type != .primitive or arg_type.primitive != .string_) {
                    self.addError(.type_mismatch, method.span, "context() argument must be a string", .{});
                }
                // Return Result[T, ContextError[E]]
                const context_err_type = self.type_builder.contextErrorType(result_type.err_type) catch return self.type_builder.unknownType();
                return self.type_builder.resultType(result_type.ok_type, context_err_type) catch self.type_builder.unknownType();
            }
        }

        // Range methods
        if (object_type == .range) {
            const range_type = object_type.range;
            const element_type = range_type.element_type;

            // next(&mut self) -> ?T (Iterator trait method)
            if (std.mem.eql(u8, method.method_name, "next")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "next() takes no arguments (only &mut self)", .{});
                }
                return self.type_builder.optionalType(element_type) catch self.type_builder.unknownType();
            }

            // reset(&mut self) -> void
            if (std.mem.eql(u8, method.method_name, "reset")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "reset() takes no arguments", .{});
                }
                return self.type_builder.voidType();
            }

            // is_empty(&self) -> bool
            if (std.mem.eql(u8, method.method_name, "is_empty")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "is_empty() takes no arguments", .{});
                }
                return self.type_builder.boolType();
            }

            // len(&self) -> i32
            if (std.mem.eql(u8, method.method_name, "len")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "len() takes no arguments", .{});
                }
                return self.type_builder.i32Type();
            }

            // clone() -> Range[T] (Clone trait)
            if (std.mem.eql(u8, method.method_name, "clone")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "clone() expects no arguments", .{});
                }
                return object_type;
            }
        }

        // ContextError methods
        if (object_type == .context_error) {
            const context_error_type = object_type.context_error;

            // message() -> string - returns the context message
            if (std.mem.eql(u8, method.method_name, "message")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "message() takes no arguments", .{});
                }
                return self.type_builder.stringType();
            }

            // cause() -> E - returns the original error
            if (std.mem.eql(u8, method.method_name, "cause")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "cause() takes no arguments", .{});
                }
                return context_error_type.inner_type;
            }

            // display_chain() -> string - returns formatted error chain
            if (std.mem.eql(u8, method.method_name, "display_chain")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "display_chain() takes no arguments", .{});
                }
                return self.type_builder.stringType();
            }
        }

        // List methods
        if (object_type == .list) {
            const list_type = object_type.list;
            const element_type = list_type.element;

            // push(&mut self, value: T) -> void
            if (std.mem.eql(u8, method.method_name, "push")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "push() expects exactly 1 argument", .{});
                    return self.type_builder.voidType();
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (!arg_type.eql(element_type)) {
                    self.addError(.type_mismatch, method.span, "push() argument type mismatch", .{});
                }
                return self.type_builder.voidType();
            }

            // pop(&mut self) -> ?T
            if (std.mem.eql(u8, method.method_name, "pop")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "pop() takes no arguments", .{});
                }
                return self.type_builder.optionalType(element_type) catch self.type_builder.unknownType();
            }

            // get(&self, index: i32) -> ?T
            if (std.mem.eql(u8, method.method_name, "get")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "get() expects exactly 1 argument", .{});
                    return self.type_builder.optionalType(element_type) catch self.type_builder.unknownType();
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (!arg_type.isInteger()) {
                    self.addError(.type_mismatch, method.span, "get() index must be an integer", .{});
                }
                return self.type_builder.optionalType(element_type) catch self.type_builder.unknownType();
            }

            // set(&mut self, index: i32, value: T) -> void
            if (std.mem.eql(u8, method.method_name, "set")) {
                if (method.args.len != 2) {
                    self.addError(.invalid_call, method.span, "set() expects exactly 2 arguments (index, value)", .{});
                    return self.type_builder.voidType();
                }
                const index_type = self.checkExpr(method.args[0]);
                if (!index_type.isInteger()) {
                    self.addError(.type_mismatch, method.span, "set() index must be an integer", .{});
                }
                const value_type = self.checkExpr(method.args[1]);
                if (!value_type.eql(element_type)) {
                    self.addError(.type_mismatch, method.span, "set() value type mismatch", .{});
                }
                return self.type_builder.voidType();
            }

            // len(&self) -> i32
            if (std.mem.eql(u8, method.method_name, "len")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "len() takes no arguments", .{});
                }
                return self.type_builder.i32Type();
            }

            // is_empty(&self) -> bool
            if (std.mem.eql(u8, method.method_name, "is_empty")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "is_empty() takes no arguments", .{});
                }
                return self.type_builder.boolType();
            }

            // first(&self) -> ?T
            if (std.mem.eql(u8, method.method_name, "first")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "first() takes no arguments", .{});
                }
                return self.type_builder.optionalType(element_type) catch self.type_builder.unknownType();
            }

            // last(&self) -> ?T
            if (std.mem.eql(u8, method.method_name, "last")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "last() takes no arguments", .{});
                }
                return self.type_builder.optionalType(element_type) catch self.type_builder.unknownType();
            }

            // clear(&mut self) -> void
            if (std.mem.eql(u8, method.method_name, "clear")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "clear() takes no arguments", .{});
                }
                return self.type_builder.voidType();
            }

            // capacity(&self) -> i32
            if (std.mem.eql(u8, method.method_name, "capacity")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "capacity() takes no arguments", .{});
                }
                return self.type_builder.i32Type();
            }

            // clone(&self) -> List[T] (creates a deep copy of the list)
            if (std.mem.eql(u8, method.method_name, "clone")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "clone() takes no arguments", .{});
                }
                return object_type; // Returns same List[T] type
            }

            // drop(&mut self) -> void (frees list memory)
            if (std.mem.eql(u8, method.method_name, "drop")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "drop() takes no arguments", .{});
                }
                return self.type_builder.voidType();
            }

            // take(n: i32) -> List[T] (returns first n elements)
            if (std.mem.eql(u8, method.method_name, "take")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "take() expects exactly 1 argument (count)", .{});
                    return object_type;
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (arg_type != .primitive or arg_type.primitive != .i32_) {
                    self.addError(.type_mismatch, method.span, "take() argument must be i32", .{});
                }
                return object_type; // Returns same List[T] type
            }

            // skip(n: i32) -> List[T] (skips first n elements)
            if (std.mem.eql(u8, method.method_name, "skip")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "skip() expects exactly 1 argument (count)", .{});
                    return object_type;
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (arg_type != .primitive or arg_type.primitive != .i32_) {
                    self.addError(.type_mismatch, method.span, "skip() argument must be i32", .{});
                }
                return object_type; // Returns same List[T] type
            }

            // filter(fn(T) -> bool) -> List[T] (keeps elements where predicate returns true)
            if (std.mem.eql(u8, method.method_name, "filter")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "filter() expects exactly 1 argument (predicate)", .{});
                    return object_type;
                }
                const arg_type = self.checkExpr(method.args[0]);
                // Validate it's a function fn(T) -> bool
                if (arg_type != .function) {
                    self.addError(.type_mismatch, method.span, "filter() argument must be a function", .{});
                    return object_type;
                }
                const fn_type = arg_type.function;
                if (fn_type.params.len != 1) {
                    self.addError(.type_mismatch, method.span, "filter() predicate must take exactly 1 parameter", .{});
                    return object_type;
                }
                if (!fn_type.params[0].eql(element_type)) {
                    self.addError(.type_mismatch, method.span, "filter() predicate parameter type must match list element type", .{});
                }
                if (fn_type.return_type != .primitive or fn_type.return_type.primitive != .bool_) {
                    self.addError(.type_mismatch, method.span, "filter() predicate must return bool", .{});
                }
                return object_type; // Returns same List[T] type
            }

            // map(fn(T) -> U) -> List[U] (transforms each element)
            if (std.mem.eql(u8, method.method_name, "map")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "map() expects exactly 1 argument (transform function)", .{});
                    return object_type;
                }
                const arg_type = self.checkExpr(method.args[0]);
                // Validate it's a function fn(T) -> U
                if (arg_type != .function) {
                    self.addError(.type_mismatch, method.span, "map() argument must be a function", .{});
                    return object_type;
                }
                const fn_type = arg_type.function;
                if (fn_type.params.len != 1) {
                    self.addError(.type_mismatch, method.span, "map() transform function must take exactly 1 parameter", .{});
                    return object_type;
                }
                if (!fn_type.params[0].eql(element_type)) {
                    self.addError(.type_mismatch, method.span, "map() transform function parameter type must match list element type", .{});
                }
                // Return List[U] where U is the function's return type
                return self.type_builder.listType(fn_type.return_type) catch self.type_builder.unknownType();
            }

            // enumerate() -> List[(i32, T)] (pairs each element with its index)
            if (std.mem.eql(u8, method.method_name, "enumerate")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "enumerate() takes no arguments", .{});
                }
                // Build tuple type (i32, T)
                const i32_type = self.type_builder.i32Type();
                const tuple_types = [_]Type{ i32_type, element_type };
                const tuple_type = self.type_builder.tupleType(&tuple_types) catch self.type_builder.unknownType();
                return self.type_builder.listType(tuple_type) catch self.type_builder.unknownType();
            }

            // zip(other: List[U]) -> List[(T, U)] (combines two lists element-wise)
            if (std.mem.eql(u8, method.method_name, "zip")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "zip() expects exactly 1 argument (other list)", .{});
                    return object_type;
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (arg_type != .list) {
                    self.addError(.type_mismatch, method.span, "zip() argument must be a List", .{});
                    return object_type;
                }
                const other_element_type = arg_type.list.element;
                // Build tuple type (T, U)
                const tuple_types = [_]Type{ element_type, other_element_type };
                const tuple_type = self.type_builder.tupleType(&tuple_types) catch self.type_builder.unknownType();
                return self.type_builder.listType(tuple_type) catch self.type_builder.unknownType();
            }
        }

        // Map methods
        if (object_type == .map) {
            const map_type = object_type.map;
            const key_type = map_type.key;
            const value_type = map_type.value;

            // insert(&mut self, key: K, value: V) -> void
            if (std.mem.eql(u8, method.method_name, "insert")) {
                if (method.args.len != 2) {
                    self.addError(.invalid_call, method.span, "insert() expects exactly 2 arguments (key, value)", .{});
                    return self.type_builder.voidType();
                }
                const arg_key_type = self.checkExpr(method.args[0]);
                if (!arg_key_type.eql(key_type)) {
                    self.addError(.type_mismatch, method.span, "insert() key type mismatch", .{});
                }
                const arg_value_type = self.checkExpr(method.args[1]);
                if (!arg_value_type.eql(value_type)) {
                    self.addError(.type_mismatch, method.span, "insert() value type mismatch", .{});
                }
                return self.type_builder.voidType();
            }

            // get(&self, key: K) -> ?V
            if (std.mem.eql(u8, method.method_name, "get")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "get() expects exactly 1 argument", .{});
                    return self.type_builder.optionalType(value_type) catch self.type_builder.unknownType();
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (!arg_type.eql(key_type)) {
                    self.addError(.type_mismatch, method.span, "get() key type mismatch", .{});
                }
                return self.type_builder.optionalType(value_type) catch self.type_builder.unknownType();
            }

            // remove(&mut self, key: K) -> ?V
            if (std.mem.eql(u8, method.method_name, "remove")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "remove() expects exactly 1 argument", .{});
                    return self.type_builder.optionalType(value_type) catch self.type_builder.unknownType();
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (!arg_type.eql(key_type)) {
                    self.addError(.type_mismatch, method.span, "remove() key type mismatch", .{});
                }
                return self.type_builder.optionalType(value_type) catch self.type_builder.unknownType();
            }

            // contains_key(&self, key: K) -> bool
            if (std.mem.eql(u8, method.method_name, "contains_key")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "contains_key() expects exactly 1 argument", .{});
                    return self.type_builder.boolType();
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (!arg_type.eql(key_type)) {
                    self.addError(.type_mismatch, method.span, "contains_key() key type mismatch", .{});
                }
                return self.type_builder.boolType();
            }

            // keys(&self) -> List[K]
            if (std.mem.eql(u8, method.method_name, "keys")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "keys() takes no arguments", .{});
                }
                return self.type_builder.listType(key_type) catch self.type_builder.unknownType();
            }

            // values(&self) -> List[V]
            if (std.mem.eql(u8, method.method_name, "values")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "values() takes no arguments", .{});
                }
                return self.type_builder.listType(value_type) catch self.type_builder.unknownType();
            }

            // len(&self) -> i32
            if (std.mem.eql(u8, method.method_name, "len")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "len() takes no arguments", .{});
                }
                return self.type_builder.i32Type();
            }

            // is_empty(&self) -> bool
            if (std.mem.eql(u8, method.method_name, "is_empty")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "is_empty() takes no arguments", .{});
                }
                return self.type_builder.boolType();
            }

            // capacity(&self) -> i32
            if (std.mem.eql(u8, method.method_name, "capacity")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "capacity() takes no arguments", .{});
                }
                return self.type_builder.i32Type();
            }

            // clear(&mut self) -> void
            if (std.mem.eql(u8, method.method_name, "clear")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "clear() takes no arguments", .{});
                }
                return self.type_builder.voidType();
            }

            // clone(&self) -> Map[K,V] (creates a deep copy of the map)
            if (std.mem.eql(u8, method.method_name, "clone")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "clone() takes no arguments", .{});
                }
                return object_type; // Returns same Map[K,V] type
            }

            // drop(&mut self) -> void (frees map memory)
            if (std.mem.eql(u8, method.method_name, "drop")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "drop() takes no arguments", .{});
                }
                return self.type_builder.voidType();
            }

            // take(n: i32) -> Map[K,V] (returns first n entries)
            if (std.mem.eql(u8, method.method_name, "take")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "take() expects exactly 1 argument (count)", .{});
                    return object_type;
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (arg_type != .primitive or arg_type.primitive != .i32_) {
                    self.addError(.type_mismatch, method.span, "take() argument must be i32", .{});
                }
                return object_type; // Returns same Map[K,V] type
            }

            // skip(n: i32) -> Map[K,V] (skips first n entries)
            if (std.mem.eql(u8, method.method_name, "skip")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "skip() expects exactly 1 argument (count)", .{});
                    return object_type;
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (arg_type != .primitive or arg_type.primitive != .i32_) {
                    self.addError(.type_mismatch, method.span, "skip() argument must be i32", .{});
                }
                return object_type; // Returns same Map[K,V] type
            }

            // filter(fn(K, V) -> bool) -> Map[K,V] (keeps entries where predicate returns true)
            if (std.mem.eql(u8, method.method_name, "filter")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "filter() expects exactly 1 argument (predicate)", .{});
                    return object_type;
                }
                const arg_type = self.checkExpr(method.args[0]);
                // Validate it's a function fn(K, V) -> bool
                if (arg_type != .function) {
                    self.addError(.type_mismatch, method.span, "filter() argument must be a function", .{});
                    return object_type;
                }
                const fn_type = arg_type.function;
                if (fn_type.params.len != 2) {
                    self.addError(.type_mismatch, method.span, "filter() predicate must take exactly 2 parameters (key, value)", .{});
                    return object_type;
                }
                if (!fn_type.params[0].eql(key_type)) {
                    self.addError(.type_mismatch, method.span, "filter() predicate first parameter type must match map key type", .{});
                }
                if (!fn_type.params[1].eql(value_type)) {
                    self.addError(.type_mismatch, method.span, "filter() predicate second parameter type must match map value type", .{});
                }
                if (fn_type.return_type != .primitive or fn_type.return_type.primitive != .bool_) {
                    self.addError(.type_mismatch, method.span, "filter() predicate must return bool", .{});
                }
                return object_type; // Returns same Map[K,V] type
            }

            // map_values(fn(V) -> U) -> Map[K,U] (transforms values, preserves keys)
            if (std.mem.eql(u8, method.method_name, "map_values")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "map_values() expects exactly 1 argument (transform function)", .{});
                    return self.type_builder.unknownType();
                }
                const arg_type = self.checkExpr(method.args[0]);
                // Validate it's a function fn(V) -> U
                if (arg_type != .function) {
                    self.addError(.type_mismatch, method.span, "map_values() argument must be a function", .{});
                    return self.type_builder.unknownType();
                }
                const fn_type = arg_type.function;
                if (fn_type.params.len != 1) {
                    self.addError(.type_mismatch, method.span, "map_values() transform function must take exactly 1 parameter", .{});
                    return self.type_builder.unknownType();
                }
                if (!fn_type.params[0].eql(value_type)) {
                    self.addError(.type_mismatch, method.span, "map_values() transform function parameter type must match map value type", .{});
                }
                // Return Map[K,U] where U is the function's return type
                return self.type_builder.mapType(key_type, fn_type.return_type) catch self.type_builder.unknownType();
            }
        }

        // Set methods
        if (object_type == .set) {
            const set_type = object_type.set;
            const element_type = set_type.element;

            // insert(&mut self, element: T) -> bool (returns true if newly inserted)
            if (std.mem.eql(u8, method.method_name, "insert")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "insert() expects exactly 1 argument (element)", .{});
                    return self.type_builder.boolType();
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (!arg_type.eql(element_type)) {
                    self.addError(.type_mismatch, method.span, "insert() element type mismatch", .{});
                }
                return self.type_builder.boolType();
            }

            // contains(&self, element: T) -> bool
            if (std.mem.eql(u8, method.method_name, "contains")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "contains() expects exactly 1 argument", .{});
                    return self.type_builder.boolType();
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (!arg_type.eql(element_type)) {
                    self.addError(.type_mismatch, method.span, "contains() element type mismatch", .{});
                }
                return self.type_builder.boolType();
            }

            // remove(&mut self, element: T) -> bool (returns true if removed)
            if (std.mem.eql(u8, method.method_name, "remove")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "remove() expects exactly 1 argument", .{});
                    return self.type_builder.boolType();
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (!arg_type.eql(element_type)) {
                    self.addError(.type_mismatch, method.span, "remove() element type mismatch", .{});
                }
                return self.type_builder.boolType();
            }

            // len(&self) -> i32
            if (std.mem.eql(u8, method.method_name, "len")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "len() takes no arguments", .{});
                }
                return self.type_builder.i32Type();
            }

            // is_empty(&self) -> bool
            if (std.mem.eql(u8, method.method_name, "is_empty")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "is_empty() takes no arguments", .{});
                }
                return self.type_builder.boolType();
            }

            // capacity(&self) -> i32
            if (std.mem.eql(u8, method.method_name, "capacity")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "capacity() takes no arguments", .{});
                }
                return self.type_builder.i32Type();
            }

            // clear(&mut self) -> void
            if (std.mem.eql(u8, method.method_name, "clear")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "clear() takes no arguments", .{});
                }
                return self.type_builder.voidType();
            }

            // clone(&self) -> Set[T] (creates a deep copy of the set)
            if (std.mem.eql(u8, method.method_name, "clone")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "clone() takes no arguments", .{});
                }
                return object_type; // Returns same Set[T] type
            }

            // drop(&mut self) -> void (frees set memory)
            if (std.mem.eql(u8, method.method_name, "drop")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "drop() takes no arguments", .{});
                }
                return self.type_builder.voidType();
            }

            // union(&self, other: Set[T]) -> Set[T] (elements in either set)
            if (std.mem.eql(u8, method.method_name, "union")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "union() expects exactly 1 argument", .{});
                    return object_type;
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (!arg_type.eql(object_type)) {
                    self.addError(.type_mismatch, method.span, "union() expects a Set with same element type", .{});
                }
                return object_type;
            }

            // intersection(&self, other: Set[T]) -> Set[T] (elements in both sets)
            if (std.mem.eql(u8, method.method_name, "intersection")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "intersection() expects exactly 1 argument", .{});
                    return object_type;
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (!arg_type.eql(object_type)) {
                    self.addError(.type_mismatch, method.span, "intersection() expects a Set with same element type", .{});
                }
                return object_type;
            }

            // difference(&self, other: Set[T]) -> Set[T] (elements in self but not other)
            if (std.mem.eql(u8, method.method_name, "difference")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "difference() expects exactly 1 argument", .{});
                    return object_type;
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (!arg_type.eql(object_type)) {
                    self.addError(.type_mismatch, method.span, "difference() expects a Set with same element type", .{});
                }
                return object_type;
            }

            // take(n: i32) -> Set[T] (returns first n elements)
            if (std.mem.eql(u8, method.method_name, "take")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "take() expects exactly 1 argument (count)", .{});
                    return object_type;
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (arg_type != .primitive or arg_type.primitive != .i32_) {
                    self.addError(.type_mismatch, method.span, "take() argument must be i32", .{});
                }
                return object_type; // Returns same Set[T] type
            }

            // skip(n: i32) -> Set[T] (skips first n elements)
            if (std.mem.eql(u8, method.method_name, "skip")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "skip() expects exactly 1 argument (count)", .{});
                    return object_type;
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (arg_type != .primitive or arg_type.primitive != .i32_) {
                    self.addError(.type_mismatch, method.span, "skip() argument must be i32", .{});
                }
                return object_type; // Returns same Set[T] type
            }

            // filter(fn(T) -> bool) -> Set[T] (keeps elements where predicate returns true)
            if (std.mem.eql(u8, method.method_name, "filter")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "filter() expects exactly 1 argument (predicate)", .{});
                    return object_type;
                }
                const arg_type = self.checkExpr(method.args[0]);
                // Validate it's a function fn(T) -> bool
                if (arg_type != .function) {
                    self.addError(.type_mismatch, method.span, "filter() argument must be a function", .{});
                    return object_type;
                }
                const fn_type = arg_type.function;
                if (fn_type.params.len != 1) {
                    self.addError(.type_mismatch, method.span, "filter() predicate must take exactly 1 parameter", .{});
                    return object_type;
                }
                if (!fn_type.params[0].eql(element_type)) {
                    self.addError(.type_mismatch, method.span, "filter() predicate parameter type must match set element type", .{});
                }
                if (fn_type.return_type != .primitive or fn_type.return_type.primitive != .bool_) {
                    self.addError(.type_mismatch, method.span, "filter() predicate must return bool", .{});
                }
                return object_type; // Returns same Set[T] type
            }

            // map(fn(T) -> U) -> List[U] (transforms each element, returns List since U may not be hashable)
            if (std.mem.eql(u8, method.method_name, "map")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "map() expects exactly 1 argument (transform function)", .{});
                    return self.type_builder.unknownType();
                }
                const arg_type = self.checkExpr(method.args[0]);
                // Validate it's a function fn(T) -> U
                if (arg_type != .function) {
                    self.addError(.type_mismatch, method.span, "map() argument must be a function", .{});
                    return self.type_builder.unknownType();
                }
                const fn_type = arg_type.function;
                if (fn_type.params.len != 1) {
                    self.addError(.type_mismatch, method.span, "map() transform function must take exactly 1 parameter", .{});
                    return self.type_builder.unknownType();
                }
                if (!fn_type.params[0].eql(element_type)) {
                    self.addError(.type_mismatch, method.span, "map() transform function parameter type must match set element type", .{});
                }
                // Return List[U] where U is the function's return type
                return self.type_builder.listType(fn_type.return_type) catch self.type_builder.unknownType();
            }

            // enumerate() -> List[(i32, T)] (pairs each element with its index)
            if (std.mem.eql(u8, method.method_name, "enumerate")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "enumerate() takes no arguments", .{});
                }
                // Build tuple type (i32, T)
                const i32_type = self.type_builder.i32Type();
                const tuple_types = [_]Type{ i32_type, element_type };
                const tuple_type = self.type_builder.tupleType(&tuple_types) catch self.type_builder.unknownType();
                return self.type_builder.listType(tuple_type) catch self.type_builder.unknownType();
            }

            // zip(other: Set[U]) -> List[(T, U)] (combines two sets element-wise)
            if (std.mem.eql(u8, method.method_name, "zip")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "zip() expects exactly 1 argument (other set)", .{});
                    return self.type_builder.unknownType();
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (arg_type != .set) {
                    self.addError(.type_mismatch, method.span, "zip() argument must be a Set", .{});
                    return self.type_builder.unknownType();
                }
                const other_element_type = arg_type.set.element;
                // Build tuple type (T, U)
                const tuple_types = [_]Type{ element_type, other_element_type };
                const tuple_type = self.type_builder.tupleType(&tuple_types) catch self.type_builder.unknownType();
                return self.type_builder.listType(tuple_type) catch self.type_builder.unknownType();
            }
        }

        // String methods (heap-allocated string)
        if (object_type == .string_data) {
            // len(&self) -> i32
            if (std.mem.eql(u8, method.method_name, "len")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "len() takes no arguments", .{});
                }
                return self.type_builder.i32Type();
            }

            // is_empty(&self) -> bool
            if (std.mem.eql(u8, method.method_name, "is_empty")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "is_empty() takes no arguments", .{});
                }
                return self.type_builder.boolType();
            }

            // capacity(&self) -> i32
            if (std.mem.eql(u8, method.method_name, "capacity")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "capacity() takes no arguments", .{});
                }
                return self.type_builder.i32Type();
            }

            // push(&mut self, c: char) -> void
            if (std.mem.eql(u8, method.method_name, "push")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "push() expects exactly 1 argument", .{});
                    return self.type_builder.voidType();
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (arg_type != .primitive or arg_type.primitive != .char_) {
                    self.addError(.type_mismatch, method.span, "push() expects a char argument", .{});
                }
                return self.type_builder.voidType();
            }

            // concat(&self, other: String) -> String
            if (std.mem.eql(u8, method.method_name, "concat")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "concat() expects exactly 1 argument", .{});
                    return object_type;
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (arg_type != .string_data) {
                    self.addError(.type_mismatch, method.span, "concat() expects a String argument", .{});
                }
                return object_type; // Returns new String
            }

            // append(&mut self, other: String) -> void
            if (std.mem.eql(u8, method.method_name, "append")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "append() expects exactly 1 argument", .{});
                    return self.type_builder.voidType();
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (arg_type != .string_data) {
                    self.addError(.type_mismatch, method.span, "append() expects a String argument", .{});
                }
                return self.type_builder.voidType();
            }

            // as_str(&self) -> string (get null-terminated C string)
            if (std.mem.eql(u8, method.method_name, "as_str")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "as_str() takes no arguments", .{});
                }
                return self.type_builder.stringType();
            }

            // clear(&mut self) -> void
            if (std.mem.eql(u8, method.method_name, "clear")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "clear() takes no arguments", .{});
                }
                return self.type_builder.voidType();
            }

            // clone(&self) -> String
            if (std.mem.eql(u8, method.method_name, "clone")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "clone() takes no arguments", .{});
                }
                return object_type;
            }

            // drop(&mut self) -> void
            if (std.mem.eql(u8, method.method_name, "drop")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "drop() takes no arguments", .{});
                }
                return self.type_builder.voidType();
            }

            // eq(&self, other: String) -> bool
            if (std.mem.eql(u8, method.method_name, "eq")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "eq() expects exactly 1 argument", .{});
                    return self.type_builder.boolType();
                }
                const arg_type = self.checkExpr(method.args[0]);
                if (arg_type != .string_data) {
                    self.addError(.type_mismatch, method.span, "eq() expects a String argument", .{});
                }
                return self.type_builder.boolType();
            }

            // hash(&self) -> i64
            if (std.mem.eql(u8, method.method_name, "hash")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "hash() takes no arguments", .{});
                }
                return self.type_builder.i64Type();
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

        // File methods
        if (object_type == .file) {
            // read(&mut self, buf: &mut [u8]) -> Result[i32, IoError]
            if (std.mem.eql(u8, method.method_name, "read")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "read() expects exactly 1 argument (buffer)", .{});
                    return self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
                }
                const buf_type = self.checkExpr(method.args[0]);
                // Check that argument is a mutable reference to a slice or array of u8
                const is_valid_buf = blk: {
                    if (buf_type != .reference or !buf_type.reference.mutable) break :blk false;
                    const inner = buf_type.reference.inner;
                    if (inner == .slice and inner.slice.element == .primitive and inner.slice.element.primitive == .u8_) break :blk true;
                    if (inner == .array and inner.array.element == .primitive and inner.array.element.primitive == .u8_) break :blk true;
                    break :blk false;
                };
                if (!is_valid_buf) {
                    self.addError(.type_mismatch, method.span, "read() expects a mutable reference to [u8] or [u8; N] buffer", .{});
                }
                return self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
            }

            // write(&mut self, buf: &[u8]) -> Result[i32, IoError]
            if (std.mem.eql(u8, method.method_name, "write")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "write() expects exactly 1 argument (buffer)", .{});
                    return self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
                }
                const buf_type = self.checkExpr(method.args[0]);
                // Check that argument is a reference to a slice or array of u8
                const is_valid_buf = blk: {
                    if (buf_type != .reference) break :blk false;
                    const inner = buf_type.reference.inner;
                    if (inner == .slice and inner.slice.element == .primitive and inner.slice.element.primitive == .u8_) break :blk true;
                    if (inner == .array and inner.array.element == .primitive and inner.array.element.primitive == .u8_) break :blk true;
                    break :blk false;
                };
                if (!is_valid_buf) {
                    self.addError(.type_mismatch, method.span, "write() expects a reference to [u8] or [u8; N] buffer", .{});
                }
                return self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
            }

            // write_string(&mut self, s: string) -> Result[i32, IoError]
            if (std.mem.eql(u8, method.method_name, "write_string")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "write_string() expects exactly 1 argument (string)", .{});
                    return self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
                }
                const str_type = self.checkExpr(method.args[0]);
                if (str_type != .primitive or str_type.primitive != .string_) {
                    self.addError(.type_mismatch, method.span, "write_string() expects a string argument", .{});
                }
                return self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
            }

            // close(self: File) -> Result[void, IoError]
            if (std.mem.eql(u8, method.method_name, "close")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "close() takes no arguments", .{});
                }
                return self.type_builder.resultType(self.type_builder.voidType(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
            }

            // flush(&mut self) -> Result[void, IoError]
            if (std.mem.eql(u8, method.method_name, "flush")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "flush() takes no arguments", .{});
                }
                return self.type_builder.resultType(self.type_builder.voidType(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
            }
        }

        // Stdout methods
        if (object_type == .stdout_handle) {
            // write(&mut self, buf: &[u8]) -> Result[i32, IoError]
            if (std.mem.eql(u8, method.method_name, "write")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "write() expects exactly 1 argument (buffer)", .{});
                    return self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
                }
                const buf_type = self.checkExpr(method.args[0]);
                // Check that argument is a reference to a slice or array of u8
                const is_valid_buf = blk: {
                    if (buf_type != .reference) break :blk false;
                    const inner = buf_type.reference.inner;
                    if (inner == .slice and inner.slice.element == .primitive and inner.slice.element.primitive == .u8_) break :blk true;
                    if (inner == .array and inner.array.element == .primitive and inner.array.element.primitive == .u8_) break :blk true;
                    break :blk false;
                };
                if (!is_valid_buf) {
                    self.addError(.type_mismatch, method.span, "write() expects a reference to [u8] or [u8; N] buffer", .{});
                }
                return self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
            }

            // write_string(&mut self, s: string) -> Result[i32, IoError]
            if (std.mem.eql(u8, method.method_name, "write_string")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "write_string() expects exactly 1 argument (string)", .{});
                    return self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
                }
                const str_type = self.checkExpr(method.args[0]);
                if (str_type != .primitive or str_type.primitive != .string_) {
                    self.addError(.type_mismatch, method.span, "write_string() expects a string argument", .{});
                }
                return self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
            }

            // flush(&mut self) -> Result[void, IoError]
            if (std.mem.eql(u8, method.method_name, "flush")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "flush() takes no arguments", .{});
                }
                return self.type_builder.resultType(self.type_builder.voidType(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
            }
        }

        // Stderr methods
        if (object_type == .stderr_handle) {
            // write(&mut self, buf: &[u8]) -> Result[i32, IoError]
            if (std.mem.eql(u8, method.method_name, "write")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "write() expects exactly 1 argument (buffer)", .{});
                    return self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
                }
                const buf_type = self.checkExpr(method.args[0]);
                // Check that argument is a reference to a slice or array of u8
                const is_valid_buf = blk: {
                    if (buf_type != .reference) break :blk false;
                    const inner = buf_type.reference.inner;
                    if (inner == .slice and inner.slice.element == .primitive and inner.slice.element.primitive == .u8_) break :blk true;
                    if (inner == .array and inner.array.element == .primitive and inner.array.element.primitive == .u8_) break :blk true;
                    break :blk false;
                };
                if (!is_valid_buf) {
                    self.addError(.type_mismatch, method.span, "write() expects a reference to [u8] or [u8; N] buffer", .{});
                }
                return self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
            }

            // write_string(&mut self, s: string) -> Result[i32, IoError]
            if (std.mem.eql(u8, method.method_name, "write_string")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "write_string() expects exactly 1 argument (string)", .{});
                    return self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
                }
                const str_type = self.checkExpr(method.args[0]);
                if (str_type != .primitive or str_type.primitive != .string_) {
                    self.addError(.type_mismatch, method.span, "write_string() expects a string argument", .{});
                }
                return self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
            }

            // flush(&mut self) -> Result[void, IoError]
            if (std.mem.eql(u8, method.method_name, "flush")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "flush() takes no arguments", .{});
                }
                return self.type_builder.resultType(self.type_builder.voidType(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
            }
        }

        // Stdin methods
        if (object_type == .stdin_handle) {
            // read(&mut self, buf: &mut [u8]) -> Result[i32, IoError]
            if (std.mem.eql(u8, method.method_name, "read")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "read() expects exactly 1 argument (buffer)", .{});
                    return self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
                }
                const buf_type = self.checkExpr(method.args[0]);
                // Check that argument is a mutable reference to a slice or array of u8
                const is_valid_buf = blk: {
                    if (buf_type != .reference or !buf_type.reference.mutable) break :blk false;
                    const inner = buf_type.reference.inner;
                    if (inner == .slice and inner.slice.element == .primitive and inner.slice.element.primitive == .u8_) break :blk true;
                    if (inner == .array and inner.array.element == .primitive and inner.array.element.primitive == .u8_) break :blk true;
                    break :blk false;
                };
                if (!is_valid_buf) {
                    self.addError(.type_mismatch, method.span, "read() expects a mutable reference to [u8] or [u8; N] buffer", .{});
                }
                return self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
            }
        }

        // BufReader methods
        if (object_type == .buf_reader) {
            const inner_type = object_type.buf_reader.inner;

            // read(&mut self, buf: &mut [u8]) -> Result[i32, IoError]
            if (std.mem.eql(u8, method.method_name, "read")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "read() expects exactly 1 argument (buffer)", .{});
                    return self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
                }
                const buf_type = self.checkExpr(method.args[0]);
                const is_valid_buf = blk: {
                    if (buf_type != .reference or !buf_type.reference.mutable) break :blk false;
                    const inner = buf_type.reference.inner;
                    if (inner == .slice and inner.slice.element == .primitive and inner.slice.element.primitive == .u8_) break :blk true;
                    if (inner == .array and inner.array.element == .primitive and inner.array.element.primitive == .u8_) break :blk true;
                    break :blk false;
                };
                if (!is_valid_buf) {
                    self.addError(.type_mismatch, method.span, "read() expects a mutable reference to [u8] or [u8; N] buffer", .{});
                }
                return self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
            }

            // read_line(&mut self) -> Result[String, IoError]
            if (std.mem.eql(u8, method.method_name, "read_line")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "read_line() takes no arguments", .{});
                }
                const string_type = self.type_builder.stringDataType() catch self.type_builder.unknownType();
                return self.type_builder.resultType(string_type, self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
            }

            // read_to_string(&mut self) -> Result[String, IoError]
            if (std.mem.eql(u8, method.method_name, "read_to_string")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "read_to_string() takes no arguments", .{});
                }
                const string_type = self.type_builder.stringDataType() catch self.type_builder.unknownType();
                return self.type_builder.resultType(string_type, self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
            }

            // fill_buf(&mut self) -> Result[[u8], IoError]
            if (std.mem.eql(u8, method.method_name, "fill_buf")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "fill_buf() takes no arguments", .{});
                }
                const slice_u8 = self.type_builder.sliceType(.{ .primitive = .u8_ }) catch self.type_builder.unknownType();
                return self.type_builder.resultType(slice_u8, self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
            }

            // consume(&mut self, n: i32)
            if (std.mem.eql(u8, method.method_name, "consume")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "consume() expects exactly 1 argument (n)", .{});
                    return self.type_builder.voidType();
                }
                const n_type = self.checkExpr(method.args[0]);
                if (n_type != .primitive or n_type.primitive != .i32_) {
                    self.addError(.type_mismatch, method.span, "consume() expects an i32 argument", .{});
                }
                return self.type_builder.voidType();
            }

            // into_inner(self) -> R
            if (std.mem.eql(u8, method.method_name, "into_inner")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "into_inner() takes no arguments", .{});
                }
                return inner_type;
            }
        }

        // BufWriter methods
        if (object_type == .buf_writer) {
            const inner_type = object_type.buf_writer.inner;

            // write(&mut self, buf: &[u8]) -> Result[i32, IoError]
            if (std.mem.eql(u8, method.method_name, "write")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "write() expects exactly 1 argument (buffer)", .{});
                    return self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
                }
                const buf_type = self.checkExpr(method.args[0]);
                const is_valid_buf = blk: {
                    if (buf_type != .reference) break :blk false;
                    const inner = buf_type.reference.inner;
                    if (inner == .slice and inner.slice.element == .primitive and inner.slice.element.primitive == .u8_) break :blk true;
                    if (inner == .array and inner.array.element == .primitive and inner.array.element.primitive == .u8_) break :blk true;
                    break :blk false;
                };
                if (!is_valid_buf) {
                    self.addError(.type_mismatch, method.span, "write() expects a reference to [u8] or [u8; N] buffer", .{});
                }
                return self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
            }

            // write_string(&mut self, s: string) -> Result[i32, IoError]
            if (std.mem.eql(u8, method.method_name, "write_string")) {
                if (method.args.len != 1) {
                    self.addError(.invalid_call, method.span, "write_string() expects exactly 1 argument (string)", .{});
                    return self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
                }
                const str_type = self.checkExpr(method.args[0]);
                if (str_type != .primitive or str_type.primitive != .string_) {
                    self.addError(.type_mismatch, method.span, "write_string() expects a string argument", .{});
                }
                return self.type_builder.resultType(self.type_builder.i32Type(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
            }

            // flush(&mut self) -> Result[void, IoError]
            if (std.mem.eql(u8, method.method_name, "flush")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "flush() takes no arguments", .{});
                }
                return self.type_builder.resultType(self.type_builder.voidType(), self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
            }

            // into_inner(self) -> Result[W, IoError]
            // Flushes buffer and returns the inner writer. Returns error if flush fails.
            if (std.mem.eql(u8, method.method_name, "into_inner")) {
                if (method.args.len != 0) {
                    self.addError(.invalid_call, method.span, "into_inner() takes no arguments", .{});
                }
                return self.type_builder.resultType(inner_type, self.type_builder.ioErrorType()) catch self.type_builder.unknownType();
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

        // Look up user-defined method on enum types
        if (object_type == .enum_) {
            const enum_type = object_type.enum_;
            if (self.lookupStructMethod(enum_type.name, method.method_name)) |enum_method| {
                // Check argument count (excluding self if method has it)
                const expected_args = if (enum_method.has_self)
                    enum_method.func_type.function.params.len - 1
                else
                    enum_method.func_type.function.params.len;

                if (method.args.len != expected_args) {
                    self.addError(.invalid_call, method.span, "method expects {d} argument(s), got {d}", .{ expected_args, method.args.len });
                }

                // Type check arguments
                const param_start: usize = if (enum_method.has_self) 1 else 0;
                for (method.args, 0..) |arg, i| {
                    const arg_type = self.checkExpr(arg);
                    const param_idx = param_start + i;
                    if (param_idx < enum_method.func_type.function.params.len) {
                        const expected_type = enum_method.func_type.function.params[param_idx];
                        // Skip type var parameters for now (generic methods)
                        if (expected_type != .type_var and !arg_type.eql(expected_type)) {
                            self.addError(.type_mismatch, method.span, "argument type mismatch", .{});
                        }
                    }
                }

                // If the enum is a monomorphized generic enum, substitute type params in return type
                if (std.mem.indexOf(u8, enum_type.name, "$")) |_| {
                    // Note: enum method monomorphization tracking not yet implemented
                    // The return type substitution below handles the essential case

                    for (self.monomorphized_enums.items) |mono| {
                        if (std.mem.eql(u8, mono.mangled_name, enum_type.name)) {
                            var substitutions = std.AutoHashMapUnmanaged(u32, Type){};
                            defer substitutions.deinit(self.allocator);

                            for (enum_method.impl_type_params, 0..) |type_param, i| {
                                if (i < mono.type_args.len) {
                                    substitutions.put(self.allocator, type_param.id, mono.type_args[i]) catch {};
                                }
                            }

                            const return_type = enum_method.func_type.function.return_type;
                            const concrete_return = self.substituteTypeParams(return_type, substitutions) catch return_type;
                            return concrete_return;
                        }
                    }
                }

                return enum_method.func_type.function.return_type;
            }
        }

        // Handle method calls on type variables with trait bounds
        // When T: SomeTrait, calling item.method() on an item: T should resolve
        // to the trait method from SomeTrait
        // Also handle ref T and inout T (references to type variables)
        var inner_type = object_type;
        if (inner_type == .reference) {
            inner_type = inner_type.reference.inner;
        }
        if (inner_type == .type_var) {
            const type_var = inner_type.type_var;

            // Search all trait bounds for the method
            var found_trait: ?*types.TraitType = null;
            var found_method: ?types.TraitMethod = null;
            var ambiguous = false;

            for (type_var.bounds) |trait| {
                for (trait.methods) |trait_method| {
                    if (std.mem.eql(u8, trait_method.name, method.method_name)) {
                        if (found_method != null) {
                            // Method found in multiple traits - ambiguous
                            ambiguous = true;
                        } else {
                            found_trait = trait;
                            found_method = trait_method;
                        }
                    }
                }
            }

            if (ambiguous) {
                self.addError(.type_mismatch, method.span, "method '{s}' is ambiguous: found in multiple trait bounds", .{method.method_name});
                return self.type_builder.unknownType();
            }

            if (found_method) |trait_method| {
                // Found the method in a trait bound
                // Check argument count (excluding self)
                const expected_args = if (trait_method.signature.params.len > 0)
                    trait_method.signature.params.len - 1 // First param is self
                else
                    0;

                if (method.args.len != expected_args) {
                    self.addError(.invalid_call, method.span, "method expects {d} argument(s), got {d}", .{ expected_args, method.args.len });
                }

                // Type check arguments (skip self parameter at index 0)
                for (method.args, 0..) |arg, i| {
                    const arg_type = self.checkExpr(arg);
                    const param_idx = i + 1; // Skip self
                    if (param_idx < trait_method.signature.params.len) {
                        const expected_type = trait_method.signature.params[param_idx];
                        // Type vars in params need special handling
                        if (expected_type != .type_var and !arg_type.eql(expected_type)) {
                            self.addError(.type_mismatch, method.span, "argument type mismatch", .{});
                        }
                    }
                }

                // Record this trait method call for monomorphization
                // Store: type_var name, trait name, method name
                self.recordTraitMethodCall(type_var.name, found_trait.?.name, method.method_name) catch {};

                // Return the trait method's return type
                // Note: If return type is Self, it should resolve to the type_var
                const return_type = trait_method.signature.return_type;
                if (return_type == .type_var and std.mem.eql(u8, return_type.type_var.name, "Self")) {
                    return object_type; // Self becomes the bounded type
                }

                // If return type is unknown, check if it's a Self.Item type
                // that should become T.Item (associated type ref) for the caller's type variable
                if (return_type == .unknown) {
                    // Check if this trait has associated types
                    // If so, create an AssociatedTypeRef for the first associated type
                    // (This handles the common case of Self.Item in trait methods)
                    if (found_trait.?.associated_types.len > 0) {
                        // For now, assume the unknown return type is the first associated type
                        // A more robust solution would track which associated type Self.X refers to
                        const assoc = found_trait.?.associated_types[0];
                        return self.type_builder.associatedTypeRefType(type_var, assoc.name, found_trait.?) catch {
                            return self.type_builder.unknownType();
                        };
                    }
                }

                return return_type;
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

    /// Record a trait method call through a generic bound.
    /// This tracks when a trait method is called on a type variable (e.g., T.method())
    /// so that codegen can emit the correct concrete method call for each monomorphization.
    fn recordTraitMethodCall(self: *TypeChecker, type_var_name: []const u8, trait_name: []const u8, method_name: []const u8) !void {
        // Check if this call is already recorded
        for (self.trait_method_calls.items) |existing| {
            if (std.mem.eql(u8, existing.type_var_name, type_var_name) and
                std.mem.eql(u8, existing.trait_name, trait_name) and
                std.mem.eql(u8, existing.method_name, method_name))
            {
                // Already recorded
                return;
            }
        }

        try self.trait_method_calls.append(self.allocator, .{
            .type_var_name = type_var_name,
            .trait_name = trait_name,
            .method_name = method_name,
        });
    }

    fn checkIfStmt(self: *TypeChecker, if_stmt: *ast.IfStmt) void {
        const cond_type = self.checkExpr(if_stmt.condition);
        if (!self.isBoolType(cond_type)) {
            self.addError(.type_mismatch, if_stmt.condition.span(), "if condition must be bool", .{});
        }

        // Check then branch as a block (doesn't produce a value)
        _ = self.checkBlock(if_stmt.then_branch);

        // Check else branch if present
        if (if_stmt.else_branch) |else_branch| {
            switch (else_branch.*) {
                .block => |block| _ = self.checkBlock(block),
                .if_stmt => |nested_if| self.checkIfStmt(nested_if),
            }
        }
    }

    fn checkMatchStmt(self: *TypeChecker, match_stmt: *ast.MatchStmt) void {
        const subject_type = self.checkExpr(match_stmt.subject);

        for (match_stmt.arms) |arm| {
            // Check pattern against subject type
            self.checkPattern(arm.pattern, subject_type);

            // Check guard if present
            if (arm.guard) |guard| {
                const guard_type = self.checkExpr(guard);
                if (!self.isBoolType(guard_type)) {
                    self.addError(.type_mismatch, guard.span(), "match guard must be bool", .{});
                }
            }

            // Check arm body as a block (doesn't produce a value)
            _ = self.checkBlock(arm.body);
        }
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
            const param_type = self.resolveTypeExpr(param.type_) catch self.type_builder.unknownType();

            param_types.append(self.allocator, param_type) catch {};

            self.current_scope.define(.{
                .name = param.name,
                .type_ = param_type,
                .kind = .parameter,
                .mutable = false,
                .span = param.span,
            }) catch {};
        }

        const return_type = self.resolveTypeExpr(closure.return_type) catch self.type_builder.unknownType();

        const old_return_type = self.current_return_type;
        self.current_return_type = return_type;
        defer self.current_return_type = old_return_type;

        _ = self.checkExpr(closure.body);

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

        return self.type_builder.functionType(param_types.items, return_type) catch self.type_builder.unknownType();
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
            // If no start was specified, infer element type from end
            if (range.start == null) {
                elem_type = end_type;
            }
        }

        // Range is now a proper Range[T] type
        return self.type_builder.rangeType(elem_type, range.inclusive) catch self.type_builder.unknownType();
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
        return self.checkTupleLiteralWithHint(tup, null);
    }

    fn checkTupleLiteralWithHint(self: *TypeChecker, tup: *ast.TupleLiteral, hint: ?Type) Type {
        var elem_types: std.ArrayListUnmanaged(Type) = .{};
        defer elem_types.deinit(self.allocator);

        // Extract expected element types from hint if it's a tuple with matching arity
        const hint_elem_types: ?[]const Type = if (hint) |h| blk: {
            if (h == .tuple and h.tuple.elements.len == tup.elements.len) {
                break :blk h.tuple.elements;
            }
            break :blk null;
        } else null;

        for (tup.elements, 0..) |elem, i| {
            const elem_hint: ?Type = if (hint_elem_types) |het| het[i] else null;
            elem_types.append(self.allocator, self.checkExprWithHint(elem, elem_hint)) catch {};
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

    /// Type check a comptime block expression
    /// Comptime blocks are evaluated at compile time using the interpreter.
    fn checkComptimeBlock(self: *TypeChecker, block: *ast.ComptimeBlock) Type {
        // First, type-check the block to ensure it's valid
        const block_type = self.checkBlock(block.body);

        // Create an interpreter instance for comptime evaluation
        var interp = Interpreter.init(self.allocator) catch {
            self.addError(.comptime_error, block.span, "failed to initialize comptime interpreter", .{});
            return self.type_builder.unknownType();
        };
        defer interp.deinit();

        // Populate the interpreter with constants from the current scope
        self.populateInterpreterEnv(&interp);

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
            self.addError(.comptime_error, block.span, "{s}", .{msg});
            return self.type_builder.unknownType();
        };

        // Convert the Value to ComptimeValue using the shared helper
        const comptime_value = self.valueToComptimeValue(result, block.span) orelse {
            return self.type_builder.unknownType();
        };

        // Store the evaluated value for codegen
        self.comptime_values.put(self.allocator, block, comptime_value) catch {
            self.addError(.comptime_error, block.span, "failed to store comptime value", .{});
            return self.type_builder.unknownType();
        };

        // Return the type based on the evaluated value
        return switch (comptime_value) {
            .int => |i| if (i.is_i32) self.type_builder.i32Type() else self.type_builder.i64Type(),
            .float => self.type_builder.f64Type(),
            .bool_ => self.type_builder.boolType(),
            .string => self.type_builder.stringType(),
            .void_ => block_type, // Return the block's inferred type
            .struct_ => |cs| blk: {
                // Look up the struct type by name
                if (self.lookupType(cs.type_name)) |t| {
                    if (t == .struct_) {
                        break :blk t;
                    }
                }
                // If struct not found, return unknown
                self.addError(.comptime_error, block.span, "unknown struct type '{s}' in comptime block", .{cs.type_name});
                break :blk self.type_builder.unknownType();
            },
            .array => |arr| blk: {
                // Build array type from element type and length
                break :blk self.type_builder.arrayType(arr.element_type, arr.elements.len) catch self.type_builder.unknownType();
            },
        };
    }

    /// Populate interpreter environment with constants from the current scope.
    /// Copies compile-time evaluated constants to the interpreter's global environment.
    fn populateInterpreterEnv(self: *TypeChecker, interp: *Interpreter) void {
        // Copy all evaluated constant values to the interpreter
        var iter = self.constant_values.iterator();
        while (iter.next()) |entry| {
            const name = entry.key_ptr.*;
            const cv = entry.value_ptr.*;
            const value = self.comptimeValueToInterpreterValue(cv);
            interp.global_env.define(name, value, false) catch {};
        }
    }

    /// Evaluate a call to a comptime function at compile time.
    /// Returns the computed ComptimeValue, or null if evaluation fails.
    /// Supports recursive comptime function calls by sharing the interpreter.
    fn evaluateComptimeCall(self: *TypeChecker, call: *ast.Call, func_decl: *ast.FunctionDecl) ?ComptimeValue {
        // Check recursion depth limit
        if (self.comptime_depth >= max_comptime_depth) {
            self.addError(.comptime_error, call.span, "comptime recursion depth limit exceeded (max {d})", .{max_comptime_depth});
            return null;
        }

        // First, check that all arguments are compile-time known
        var arg_values = std.ArrayListUnmanaged(Value){};
        defer arg_values.deinit(self.allocator);

        for (call.args) |arg| {
            // Try to evaluate the argument at compile time
            const arg_value = self.evaluateComptimeExpr(arg) orelse {
                self.addError(.comptime_error, arg.span(), "argument to comptime function must be comptime-known", .{});
                return null;
            };
            arg_values.append(self.allocator, arg_value) catch {
                return null;
            };
        }

        // Check if we need to create a new interpreter (outermost call)
        const is_outermost = self.comptime_interpreter == null;
        if (is_outermost) {
            // Create the shared interpreter
            const interp = self.allocator.create(Interpreter) catch {
                self.addError(.comptime_error, call.span, "failed to allocate interpreter for comptime function", .{});
                return null;
            };
            interp.* = Interpreter.init(self.allocator) catch {
                self.allocator.destroy(interp);
                self.addError(.comptime_error, call.span, "failed to create interpreter for comptime function", .{});
                return null;
            };
            self.comptime_interpreter = interp;

            // Populate with constants from outer scope
            self.populateInterpreterEnv(interp);

            // Register all comptime functions in the interpreter
            self.registerComptimeFunctionsInInterpreter(interp);
        }

        const interp = self.comptime_interpreter.?;

        // Track recursion depth
        self.comptime_depth += 1;
        defer self.comptime_depth -= 1;

        // Cleanup interpreter when outermost call completes
        defer if (is_outermost) {
            interp.deinit();
            self.allocator.destroy(interp);
            self.comptime_interpreter = null;
        };

        // Get the function body
        const body = func_decl.body orelse {
            self.addError(.comptime_error, call.span, "comptime function must have a body", .{});
            return null;
        };

        // Create a new environment for this function call (for proper parameter scoping)
        const old_env = interp.current_env;
        const func_env = self.allocator.create(values.Environment) catch {
            self.addError(.comptime_error, call.span, "failed to create function environment", .{});
            return null;
        };
        func_env.* = values.Environment.init(self.allocator, interp.global_env);
        interp.current_env = func_env;

        defer {
            interp.current_env = old_env;
            func_env.deinit();
            self.allocator.destroy(func_env);
        }

        // Bind parameters to argument values in the function environment
        for (func_decl.params, arg_values.items) |param, arg_val| {
            interp.current_env.define(param.name, arg_val, false) catch {
                self.addError(.comptime_error, call.span, "failed to bind comptime function parameter", .{});
                return null;
            };
        }

        // Clear return value from any previous call
        interp.return_value = null;

        // Evaluate function body
        const block_result = interp.evalBlock(body) catch |err| {
            // Convert interpreter errors to compile errors
            if (err == error.RuntimeError) {
                self.addError(.comptime_error, call.span, "comptime function evaluation failed", .{});
            } else if (err == error.ComptimeError) {
                // @compileError was called
                // Error message already written to stdout by interpreter
            }
            return null;
        };

        // Check for return value (set when return statement was executed)
        // If there's a return_value, use that; otherwise use the block result
        const result = if (interp.return_value) |rv| rv else block_result;

        // Clear the return value so it doesn't affect other calls
        interp.return_value = null;

        // Convert interpreter Value to ComptimeValue
        return self.valueToComptimeValue(result, call.span);
    }

    /// Register all comptime functions in the interpreter's environment.
    /// This enables recursive calls by making functions available for lookup.
    fn registerComptimeFunctionsInInterpreter(self: *TypeChecker, interp: *Interpreter) void {
        var iter = self.comptime_functions.iterator();
        while (iter.next()) |entry| {
            const func_name = entry.key_ptr.*;
            const func_decl = entry.value_ptr.*;

            if (func_decl.body) |body| {
                // Create a FunctionValue for this comptime function
                const func_val = self.allocator.create(values.FunctionValue) catch continue;

                // Convert params
                var params = std.ArrayListUnmanaged(values.FunctionValue.FunctionParam){};
                for (func_decl.params) |param| {
                    params.append(self.allocator, .{ .name = param.name }) catch continue;
                }

                const params_slice = params.toOwnedSlice(self.allocator) catch continue;

                func_val.* = .{
                    .name = func_name,
                    .params = params_slice,
                    .body = body,
                    .closure_env = interp.global_env,
                };

                // Track for cleanup
                interp.allocated_functions.append(self.allocator, func_val) catch continue;

                // Register in global environment
                interp.global_env.define(func_name, .{ .function = func_val }, false) catch continue;
            }
        }
    }

    /// Evaluate a comptime function call via @name(...) builtin syntax.
    /// Similar to evaluateComptimeCall but handles BuiltinCall arguments.
    fn evaluateComptimeCallBuiltin(self: *TypeChecker, bc: *ast.BuiltinCall, func_decl: *ast.FunctionDecl) ?ComptimeValue {
        // Check recursion depth limit
        if (self.comptime_depth >= max_comptime_depth) {
            self.addError(.comptime_error, bc.span, "comptime recursion depth limit exceeded (max {d})", .{max_comptime_depth});
            return null;
        }

        // First, check that all arguments are compile-time known
        var arg_values = std.ArrayListUnmanaged(Value){};
        defer arg_values.deinit(self.allocator);

        for (bc.args) |arg| {
            const arg_expr = switch (arg) {
                .expr_arg => |e| e,
                .type_arg => {
                    self.addError(.comptime_error, bc.span, "type arguments not supported in comptime function calls", .{});
                    return null;
                },
            };
            // Try to evaluate the argument at compile time
            const arg_value = self.evaluateComptimeExpr(arg_expr) orelse {
                self.addError(.comptime_error, arg_expr.span(), "argument to comptime function must be comptime-known", .{});
                return null;
            };
            arg_values.append(self.allocator, arg_value) catch {
                return null;
            };
        }

        // Check if we need to create a new interpreter (outermost call)
        const is_outermost = self.comptime_interpreter == null;
        if (is_outermost) {
            // Create the shared interpreter
            const interp = self.allocator.create(Interpreter) catch {
                self.addError(.comptime_error, bc.span, "failed to allocate interpreter for comptime function", .{});
                return null;
            };
            interp.* = Interpreter.init(self.allocator) catch {
                self.allocator.destroy(interp);
                self.addError(.comptime_error, bc.span, "failed to create interpreter for comptime function", .{});
                return null;
            };
            self.comptime_interpreter = interp;

            // Populate with constants from outer scope
            self.populateInterpreterEnv(interp);

            // Register all comptime functions in the interpreter
            self.registerComptimeFunctionsInInterpreter(interp);
        }

        const interp = self.comptime_interpreter.?;

        // Track recursion depth
        self.comptime_depth += 1;
        defer self.comptime_depth -= 1;

        // Cleanup interpreter when outermost call completes
        defer if (is_outermost) {
            interp.deinit();
            self.allocator.destroy(interp);
            self.comptime_interpreter = null;
        };

        // Get the function body
        const body = func_decl.body orelse {
            self.addError(.comptime_error, bc.span, "comptime function must have a body", .{});
            return null;
        };

        // Create a new environment for this function call (for proper parameter scoping)
        const old_env = interp.current_env;
        const func_env = self.allocator.create(values.Environment) catch {
            self.addError(.comptime_error, bc.span, "failed to create function environment", .{});
            return null;
        };
        func_env.* = values.Environment.init(self.allocator, interp.global_env);
        interp.current_env = func_env;

        defer {
            interp.current_env = old_env;
            func_env.deinit();
            self.allocator.destroy(func_env);
        }

        // Bind parameters to argument values in the function environment
        for (func_decl.params, arg_values.items) |param, arg_val| {
            interp.current_env.define(param.name, arg_val, false) catch {
                self.addError(.comptime_error, bc.span, "failed to bind comptime function parameter", .{});
                return null;
            };
        }

        // Clear return value from any previous call
        interp.return_value = null;

        // Evaluate function body
        const block_result = interp.evalBlock(body) catch |err| {
            // Convert interpreter errors to compile errors
            if (err == error.RuntimeError) {
                self.addError(.comptime_error, bc.span, "comptime function evaluation failed", .{});
            } else if (err == error.ComptimeError) {
                // @compileError was called
                // Error message already written to stdout by interpreter
            }
            return null;
        };

        // Check for return value (set when return statement was executed)
        // If there's a return_value, use that; otherwise use the block result
        const result = if (interp.return_value) |rv| rv else block_result;

        // Clear the return value so it doesn't affect other calls
        interp.return_value = null;

        // Convert interpreter Value to ComptimeValue
        return self.valueToComptimeValue(result, bc.span);
    }

    /// Evaluate an expression at compile time to get an interpreter Value.
    /// Returns null if the expression is not comptime-known.
    fn evaluateComptimeExpr(self: *TypeChecker, expr: ast.Expr) ?Value {
        return switch (expr) {
            .literal => |lit| switch (lit.kind) {
                .int => |v| .{ .int = values.Integer{ .value = v, .type_ = .i64_ } },
                .float => |v| .{ .float = values.Float{ .value = v, .type_ = .f64_ } },
                .bool_ => |v| .{ .bool_ = v },
                .string => |v| .{ .string = v },
                .char => |v| .{ .char_ = v },
            },
            .unary => |u| {
                const operand = self.evaluateComptimeExpr(u.operand) orelse return null;
                switch (u.op) {
                    .negate => {
                        if (operand == .int) return .{ .int = values.Integer{ .value = -operand.int.value, .type_ = operand.int.type_ } };
                        if (operand == .float) return .{ .float = values.Float{ .value = -operand.float.value, .type_ = operand.float.type_ } };
                        return null;
                    },
                    .not => {
                        if (operand == .bool_) return .{ .bool_ = !operand.bool_ };
                        return null;
                    },
                    else => return null,
                }
            },
            .binary => |b| {
                const left = self.evaluateComptimeExpr(b.left) orelse return null;
                const right = self.evaluateComptimeExpr(b.right) orelse return null;
                return self.evaluateComptimeBinaryOp(b.op, left, right);
            },
            .identifier => |ident| {
                // Check if this identifier refers to a const with a known comptime value
                if (self.constant_values.get(ident.name)) |cv| {
                    return self.comptimeValueToInterpreterValue(cv);
                }
                return null;
            },
            .call => |c| {
                // Check if calling another comptime function
                if (c.callee == .identifier) {
                    const func_name = c.callee.identifier.name;
                    if (self.comptime_functions.get(func_name)) |comptime_func| {
                        const result = self.evaluateComptimeCall(c, comptime_func);
                        if (result) |cv| {
                            return self.comptimeValueToInterpreterValue(cv);
                        }
                    }
                }
                return null;
            },
            .builtin_call => |bc| {
                // Check if this is a comptime function call via @name(...) syntax
                if (self.comptime_functions.get(bc.name)) |comptime_func| {
                    // Delegate to evaluateComptimeCallBuiltin which uses the shared interpreter
                    const result = self.evaluateComptimeCallBuiltin(bc, comptime_func);
                    if (result) |cv| {
                        return self.comptimeValueToInterpreterValue(cv);
                    }
                }
                return null;
            },
            .array_literal => |arr| {
                // Evaluate each element at compile time
                var elements = std.ArrayListUnmanaged(Value){};
                for (arr.elements) |elem| {
                    const val = self.evaluateComptimeExpr(elem) orelse return null;
                    elements.append(self.allocator, val) catch return null;
                }
                // Create an interpreter array value (heap-allocated)
                const array_val = self.allocator.create(values.ArrayValue) catch return null;
                array_val.* = .{ .elements = elements.items };
                return .{ .array = array_val };
            },
            else => null,
        };
    }

    /// Evaluate a binary operation at compile time.
    fn evaluateComptimeBinaryOp(self: *TypeChecker, op: ast.BinaryOp, left: Value, right: Value) ?Value {
        _ = self;
        // Integer operations
        if (left == .int and right == .int) {
            const l = left.int.value;
            const r = right.int.value;
            const t = left.int.type_;
            return switch (op) {
                .add => .{ .int = values.Integer{ .value = l + r, .type_ = t } },
                .sub => .{ .int = values.Integer{ .value = l - r, .type_ = t } },
                .mul => .{ .int = values.Integer{ .value = l * r, .type_ = t } },
                .div => if (r != 0) .{ .int = values.Integer{ .value = @divTrunc(l, r), .type_ = t } } else null,
                .mod => if (r != 0) .{ .int = values.Integer{ .value = @mod(l, r), .type_ = t } } else null,
                .eq => .{ .bool_ = l == r },
                .not_eq => .{ .bool_ = l != r },
                .lt => .{ .bool_ = l < r },
                .lt_eq => .{ .bool_ = l <= r },
                .gt => .{ .bool_ = l > r },
                .gt_eq => .{ .bool_ = l >= r },
                else => null,
            };
        }
        // Float operations
        if ((left == .float or left == .int) and (right == .float or right == .int)) {
            const l: f64 = if (left == .float) left.float.value else @floatFromInt(left.int.value);
            const r: f64 = if (right == .float) right.float.value else @floatFromInt(right.int.value);
            return switch (op) {
                .add => .{ .float = values.Float{ .value = l + r, .type_ = .f64_ } },
                .sub => .{ .float = values.Float{ .value = l - r, .type_ = .f64_ } },
                .mul => .{ .float = values.Float{ .value = l * r, .type_ = .f64_ } },
                .div => if (r != 0) .{ .float = values.Float{ .value = l / r, .type_ = .f64_ } } else null,
                .eq => .{ .bool_ = l == r },
                .not_eq => .{ .bool_ = l != r },
                .lt => .{ .bool_ = l < r },
                .lt_eq => .{ .bool_ = l <= r },
                .gt => .{ .bool_ = l > r },
                .gt_eq => .{ .bool_ = l >= r },
                else => null,
            };
        }
        // Boolean operations
        if (left == .bool_ and right == .bool_) {
            const l = left.bool_;
            const r = right.bool_;
            return switch (op) {
                .and_ => .{ .bool_ = l and r },
                .or_ => .{ .bool_ = l or r },
                .eq => .{ .bool_ = l == r },
                .not_eq => .{ .bool_ = l != r },
                else => null,
            };
        }
        // String operations
        if (left == .string and right == .string) {
            return switch (op) {
                .eq => .{ .bool_ = std.mem.eql(u8, left.string, right.string) },
                .not_eq => .{ .bool_ = !std.mem.eql(u8, left.string, right.string) },
                else => null,
            };
        }
        return null;
    }

    /// Convert an interpreter Value to a ComptimeValue.
    fn valueToComptimeValue(self: *TypeChecker, value: Value, span: ast.Span) ?ComptimeValue {
        return switch (value) {
            .int => |v| blk: {
                const int_val = v.value;
                // Clamp to i64 range for ComptimeValue
                const clamped: i64 = if (int_val > std.math.maxInt(i64))
                    std.math.maxInt(i64)
                else if (int_val < std.math.minInt(i64))
                    std.math.minInt(i64)
                else
                    @intCast(int_val);
                break :blk .{ .int = .{ .value = clamped, .is_i32 = clamped >= std.math.minInt(i32) and clamped <= std.math.maxInt(i32) } };
            },
            .float => |v| .{ .float = v.value },
            .bool_ => |v| .{ .bool_ = v },
            .string => |v| blk: {
                // Need to duplicate the string as interpreter may free it
                const duped = self.allocator.dupe(u8, v) catch {
                    self.addError(.comptime_error, span, "failed to allocate comptime string", .{});
                    break :blk null;
                };
                break :blk .{ .string = duped };
            },
            .void_ => .{ .void_ = {} },
            .struct_ => |sv| blk: {
                // Convert struct value to comptime struct
                var comptime_fields = std.StringArrayHashMapUnmanaged(ComptimeValue){};
                var iter = sv.fields.iterator();
                while (iter.next()) |entry| {
                    // Recursively convert field value
                    const field_comptime_value = self.valueToComptimeValue(entry.value_ptr.*, span) orelse {
                        // Clean up on failure
                        comptime_fields.deinit(self.allocator);
                        break :blk null;
                    };
                    // Duplicate field name to ensure ownership
                    const field_name = self.allocator.dupe(u8, entry.key_ptr.*) catch {
                        comptime_fields.deinit(self.allocator);
                        break :blk null;
                    };
                    comptime_fields.put(self.allocator, field_name, field_comptime_value) catch {
                        self.allocator.free(field_name);
                        comptime_fields.deinit(self.allocator);
                        break :blk null;
                    };
                }
                // Duplicate type name
                const type_name = self.allocator.dupe(u8, sv.type_name) catch {
                    comptime_fields.deinit(self.allocator);
                    break :blk null;
                };
                break :blk .{ .struct_ = .{ .type_name = type_name, .fields = comptime_fields } };
            },
            .array => |arr| blk: {
                // Convert array value to comptime array
                if (arr.elements.len == 0) {
                    self.addError(.comptime_error, span, "empty arrays not supported in comptime blocks", .{});
                    break :blk null;
                }

                // Recursively convert all elements
                var comptime_elements = std.ArrayListUnmanaged(ComptimeValue){};
                for (arr.elements) |elem| {
                    const elem_comptime = self.valueToComptimeValue(elem, span) orelse {
                        comptime_elements.deinit(self.allocator);
                        break :blk null;
                    };
                    comptime_elements.append(self.allocator, elem_comptime) catch {
                        comptime_elements.deinit(self.allocator);
                        break :blk null;
                    };
                }

                // Infer element type from first element
                const element_type = self.inferTypeFromComptimeValue(comptime_elements.items[0]);

                break :blk .{ .array = .{
                    .element_type = element_type,
                    .elements = comptime_elements.toOwnedSlice(self.allocator) catch {
                        comptime_elements.deinit(self.allocator);
                        break :blk null;
                    },
                } };
            },
            else => {
                self.addError(.comptime_error, span, "comptime block produced a non-primitive value", .{});
                return null;
            },
        };
    }

    /// Infer the Klar Type from a ComptimeValue.
    fn inferTypeFromComptimeValue(self: *TypeChecker, cv: ComptimeValue) Type {
        return switch (cv) {
            .int => |i| if (i.is_i32) self.type_builder.i32Type() else self.type_builder.i64Type(),
            .float => self.type_builder.f64Type(),
            .bool_ => self.type_builder.boolType(),
            .string => self.type_builder.stringType(),
            .void_ => self.type_builder.voidType(),
            .struct_ => |cs| blk: {
                if (self.lookupType(cs.type_name)) |t| {
                    break :blk t;
                }
                break :blk self.type_builder.unknownType();
            },
            .array => |arr| blk: {
                // Recursively infer element type
                const elem_type = self.inferTypeFromComptimeValue(arr.elements[0]);
                break :blk self.type_builder.arrayType(elem_type, arr.elements.len) catch self.type_builder.unknownType();
            },
        };
    }

    /// Convert a ComptimeValue back to an interpreter Value.
    fn comptimeValueToInterpreterValue(self: *TypeChecker, cv: ComptimeValue) Value {
        return switch (cv) {
            .int => |i| .{ .int = values.Integer{ .value = i.value, .type_ = if (i.is_i32) .i32_ else .i64_ } },
            .float => |f| .{ .float = values.Float{ .value = f, .type_ = .f64_ } },
            .bool_ => |b| .{ .bool_ = b },
            .string => |s| .{ .string = s },
            .void_ => .{ .void_ = {} },
            .struct_ => |cs| blk: {
                // Convert comptime struct back to interpreter struct value
                const sv = self.allocator.create(values.StructValue) catch {
                    break :blk .{ .void_ = {} }; // Fallback on allocation failure
                };
                sv.* = .{
                    .type_name = cs.type_name,
                    .fields = .{},
                };
                // Convert each field
                var iter = cs.fields.iterator();
                while (iter.next()) |entry| {
                    const field_value = self.comptimeValueToInterpreterValue(entry.value_ptr.*);
                    sv.fields.put(self.allocator, entry.key_ptr.*, field_value) catch {};
                }
                break :blk .{ .struct_ = sv };
            },
            .array => |arr| blk: {
                // Convert comptime array back to interpreter array value (heap-allocated)
                var elements = std.ArrayListUnmanaged(Value){};
                for (arr.elements) |elem| {
                    const elem_value = self.comptimeValueToInterpreterValue(elem);
                    elements.append(self.allocator, elem_value) catch {
                        break :blk .{ .void_ = {} };
                    };
                }
                const array_val = self.allocator.create(values.ArrayValue) catch {
                    break :blk .{ .void_ = {} };
                };
                array_val.* = .{ .elements = elements.items };
                break :blk .{ .array = array_val };
            },
        };
    }

    /// Type check a builtin function call (@typeName, @typeInfo, etc.)
    /// Also handles user-defined comptime function calls (@name syntax)
    fn checkBuiltinCall(self: *TypeChecker, builtin: *ast.BuiltinCall) Type {
        // First, check if this is a user-defined comptime function call
        if (self.comptime_functions.get(builtin.name)) |comptime_func| {
            return self.checkComptimeFunctionCallFromBuiltin(builtin, comptime_func);
        }

        if (std.mem.eql(u8, builtin.name, "typeName")) {
            return self.checkBuiltinTypeName(builtin);
        } else if (std.mem.eql(u8, builtin.name, "typeInfo")) {
            return self.checkBuiltinTypeInfo(builtin);
        } else if (std.mem.eql(u8, builtin.name, "fields")) {
            return self.checkBuiltinFields(builtin);
        } else if (std.mem.eql(u8, builtin.name, "compileError")) {
            return self.checkBuiltinCompileError(builtin);
        } else if (std.mem.eql(u8, builtin.name, "hasField")) {
            return self.checkBuiltinHasField(builtin);
        } else if (std.mem.eql(u8, builtin.name, "sizeOf")) {
            return self.checkBuiltinSizeOf(builtin);
        } else if (std.mem.eql(u8, builtin.name, "alignOf")) {
            return self.checkBuiltinAlignOf(builtin);
        } else if (std.mem.eql(u8, builtin.name, "assert")) {
            return self.checkBuiltinAssert(builtin);
        } else if (std.mem.eql(u8, builtin.name, "repeat")) {
            return self.checkBuiltinRepeat(builtin);
        } else {
            self.addError(.undefined_function, builtin.span, "unknown builtin '@{s}'", .{builtin.name});
            return self.type_builder.unknownType();
        }
    }

    /// Handle a user-defined comptime function call using @name(...) syntax
    fn checkComptimeFunctionCallFromBuiltin(self: *TypeChecker, builtin: *ast.BuiltinCall, func_decl: *ast.FunctionDecl) Type {

        // Check argument count
        if (builtin.args.len != func_decl.params.len) {
            self.addError(.wrong_number_of_args, builtin.span, "@{s} expects {d} argument(s), got {d}", .{ builtin.name, func_decl.params.len, builtin.args.len });
            return self.type_builder.unknownType();
        }

        // If we're inside a comptime function body, just type-check arguments
        // without trying to evaluate them. The interpreter will handle evaluation.
        if (self.checking_comptime_function_body) {
            // Type-check arguments against parameter types
            for (builtin.args, 0..) |arg, i| {
                const expr = switch (arg) {
                    .expr_arg => |e| e,
                    .type_arg => {
                        self.addError(.type_mismatch, builtin.span, "comptime function expects expression arguments, not types", .{});
                        return self.type_builder.unknownType();
                    },
                };

                const arg_type = self.checkExpr(expr);
                const param_type = self.resolveTypeExpr(func_decl.params[i].type_) catch self.type_builder.unknownType();
                if (!self.isTypeCompatible(arg_type, param_type)) {
                    self.addError(.type_mismatch, expr.span(), "argument type mismatch", .{});
                }
            }
            // Return the function's return type without evaluating
            return self.checkFunctionDeclReturnType(func_decl);
        }

        // Normal path: evaluate at compile time
        var arg_values = std.ArrayListUnmanaged(Value){};
        defer arg_values.deinit(self.allocator);

        for (builtin.args) |arg| {
            const expr = switch (arg) {
                .expr_arg => |e| e,
                .type_arg => {
                    self.addError(.type_mismatch, builtin.span, "comptime function expects expression arguments, not types", .{});
                    return self.type_builder.unknownType();
                },
            };

            const arg_value = self.evaluateComptimeExpr(expr) orelse {
                self.addError(.comptime_error, expr.span(), "argument to comptime function must be comptime-known", .{});
                return self.type_builder.unknownType();
            };
            arg_values.append(self.allocator, arg_value) catch {
                return self.type_builder.unknownType();
            };
        }

        // Use the shared interpreter approach from evaluateComptimeCallBuiltin
        const comptime_value = self.evaluateComptimeCallBuiltin(builtin, func_decl) orelse {
            return self.type_builder.unknownType();
        };

        // Store the result for codegen
        self.comptime_builtin_values.put(self.allocator, builtin, comptime_value) catch {};

        // Return the function's return type
        return self.checkFunctionDeclReturnType(func_decl);
    }

    /// Get the return type of a function declaration
    fn checkFunctionDeclReturnType(self: *TypeChecker, func_decl: *ast.FunctionDecl) Type {
        if (func_decl.return_type) |ret_type| {
            return self.resolveTypeExpr(ret_type) catch self.type_builder.unknownType();
        }
        return self.type_builder.voidType();
    }

    /// @typeName(T) -> string
    fn checkBuiltinTypeName(self: *TypeChecker, builtin: *ast.BuiltinCall) Type {
        if (builtin.args.len != 1) {
            self.addError(.wrong_number_of_args, builtin.span, "@typeName expects 1 argument, got {d}", .{builtin.args.len});
            return self.type_builder.unknownType();
        }

        // The argument must be a type
        const resolved_type: Type = switch (builtin.args[0]) {
            .type_arg => |type_expr| blk: {
                // Resolve the type expression to a concrete type
                const t = self.resolveTypeExpr(type_expr) catch {
                    return self.type_builder.unknownType();
                };
                break :blk t;
            },
            .expr_arg => {
                self.addError(.type_mismatch, builtin.span, "@typeName expects a type argument, not an expression", .{});
                return self.type_builder.unknownType();
            },
        };

        // Format the type to a string and store it for codegen
        var buf = std.ArrayListUnmanaged(u8){};
        types.formatType(buf.writer(self.allocator), resolved_type) catch {
            buf.deinit(self.allocator);
            return self.type_builder.stringType();
        };

        // Store the computed string value for codegen
        const type_name = buf.toOwnedSlice(self.allocator) catch {
            buf.deinit(self.allocator);
            return self.type_builder.stringType();
        };
        self.comptime_strings.put(self.allocator, builtin, type_name) catch {
            self.allocator.free(type_name);
        };

        return self.type_builder.stringType();
    }

    /// @typeInfo(T) -> string describing the type kind
    /// Returns one of: "primitive", "struct", "enum", "trait", "array", "slice",
    /// "tuple", "optional", "result", "function", "reference", "rc", "arc", "void", "unknown"
    fn checkBuiltinTypeInfo(self: *TypeChecker, builtin: *ast.BuiltinCall) Type {
        if (builtin.args.len != 1) {
            self.addError(.wrong_number_of_args, builtin.span, "@typeInfo expects 1 argument, got {d}", .{builtin.args.len});
            return self.type_builder.unknownType();
        }

        const type_expr = switch (builtin.args[0]) {
            .type_arg => |te| te,
            .expr_arg => {
                self.addError(.type_mismatch, builtin.span, "@typeInfo expects a type argument, not an expression", .{});
                return self.type_builder.unknownType();
            },
        };

        // Resolve the type expression
        const resolved_type = self.resolveTypeExpr(type_expr) catch {
            return self.type_builder.unknownType();
        };

        // Determine the type kind string
        const type_kind_literal: []const u8 = switch (resolved_type) {
            .primitive => "primitive",
            .struct_ => "struct",
            .enum_ => "enum",
            .trait_ => "trait",
            .array => "array",
            .slice => "slice",
            .tuple => "tuple",
            .optional => "optional",
            .result => "result",
            .function => "function",
            .reference => "reference",
            .type_var => "type_var",
            .applied => "applied",
            .associated_type_ref => "associated_type",
            .rc => "rc",
            .weak_rc => "weak_rc",
            .arc => "arc",
            .weak_arc => "weak_arc",
            .cell => "cell",
            .context_error => "context_error",
            .range => "range",
            .list => "list",
            .map => "map",
            .set => "set",
            .string_data => "string",
            .file => "file",
            .io_error => "io_error",
            .stdout_handle => "stdout",
            .stderr_handle => "stderr",
            .stdin_handle => "stdin",
            .buf_reader => "buf_reader",
            .buf_writer => "buf_writer",
            .void_ => "void",
            .never => "never",
            .unknown => "unknown",
            .error_type => "error",
        };

        // Duplicate the string literal so it can be freed during deinit
        const type_kind = self.allocator.dupe(u8, type_kind_literal) catch {
            return self.type_builder.stringType();
        };

        // Store the type kind string for codegen
        self.comptime_strings.put(self.allocator, builtin, type_kind) catch {
            self.allocator.free(type_kind);
        };

        return self.type_builder.stringType();
    }

    /// @fields(T) -> string with comma-separated field names
    /// Returns empty string for non-struct types
    fn checkBuiltinFields(self: *TypeChecker, builtin: *ast.BuiltinCall) Type {
        if (builtin.args.len != 1) {
            self.addError(.wrong_number_of_args, builtin.span, "@fields expects 1 argument, got {d}", .{builtin.args.len});
            return self.type_builder.unknownType();
        }

        const type_expr = switch (builtin.args[0]) {
            .type_arg => |te| te,
            .expr_arg => {
                self.addError(.type_mismatch, builtin.span, "@fields expects a type argument, not an expression", .{});
                return self.type_builder.unknownType();
            },
        };

        // Resolve the type expression
        const resolved_type = self.resolveTypeExpr(type_expr) catch {
            return self.type_builder.unknownType();
        };

        // Get fields for struct types - always allocate so deinit can free
        const fields_str: []u8 = switch (resolved_type) {
            .struct_ => |s| blk: {
                if (s.fields.len == 0) {
                    break :blk self.allocator.alloc(u8, 0) catch {
                        return self.type_builder.stringType();
                    };
                }
                // Build comma-separated field names
                var total_len: usize = 0;
                for (s.fields) |field| {
                    total_len += field.name.len + 1; // +1 for comma
                }
                total_len -= 1; // No trailing comma

                const result = self.allocator.alloc(u8, total_len) catch {
                    return self.type_builder.stringType();
                };
                var pos: usize = 0;
                for (s.fields, 0..) |field, i| {
                    @memcpy(result[pos..][0..field.name.len], field.name);
                    pos += field.name.len;
                    if (i < s.fields.len - 1) {
                        result[pos] = ',';
                        pos += 1;
                    }
                }
                break :blk result;
            },
            .enum_ => |e| blk: {
                // For enums, return variant names
                if (e.variants.len == 0) {
                    break :blk self.allocator.alloc(u8, 0) catch {
                        return self.type_builder.stringType();
                    };
                }
                var total_len: usize = 0;
                for (e.variants) |variant| {
                    total_len += variant.name.len + 1;
                }
                total_len -= 1;

                const result = self.allocator.alloc(u8, total_len) catch {
                    return self.type_builder.stringType();
                };
                var pos: usize = 0;
                for (e.variants, 0..) |variant, i| {
                    @memcpy(result[pos..][0..variant.name.len], variant.name);
                    pos += variant.name.len;
                    if (i < e.variants.len - 1) {
                        result[pos] = ',';
                        pos += 1;
                    }
                }
                break :blk result;
            },
            else => self.allocator.alloc(u8, 0) catch {
                return self.type_builder.stringType();
            },
        };

        // Store the fields string for codegen
        self.comptime_strings.put(self.allocator, builtin, fields_str) catch {
            self.allocator.free(fields_str);
        };

        return self.type_builder.stringType();
    }

    /// @compileError("message") -> never returns (compile error)
    fn checkBuiltinCompileError(self: *TypeChecker, builtin: *ast.BuiltinCall) Type {
        if (builtin.args.len != 1) {
            self.addError(.wrong_number_of_args, builtin.span, "@compileError expects 1 argument, got {d}", .{builtin.args.len});
            return self.type_builder.unknownType();
        }

        switch (builtin.args[0]) {
            .expr_arg => |expr| {
                // Check if the expression is a string literal
                switch (expr) {
                    .literal => |lit| {
                        switch (lit.kind) {
                            .string => |msg| {
                                // Emit the compile error with the user's message
                                self.addError(.comptime_error, builtin.span, "{s}", .{msg});
                            },
                            else => {
                                self.addError(.type_mismatch, builtin.span, "@compileError expects a string literal", .{});
                            },
                        }
                    },
                    else => {
                        self.addError(.type_mismatch, builtin.span, "@compileError expects a string literal", .{});
                    },
                }
            },
            .type_arg => {
                self.addError(.type_mismatch, builtin.span, "@compileError expects a string, not a type", .{});
            },
        }

        return self.type_builder.neverType();
    }

    /// @assert(condition) or @assert(condition, "message")
    /// Evaluates condition at compile time and emits error if false.
    fn checkBuiltinAssert(self: *TypeChecker, builtin: *ast.BuiltinCall) Type {
        if (builtin.args.len == 0 or builtin.args.len > 2) {
            self.addError(.wrong_number_of_args, builtin.span, "@assert expects 1 or 2 arguments, got {d}", .{builtin.args.len});
            return self.type_builder.unknownType();
        }

        // First arg must be a comptime-known boolean expression
        const condition_expr = switch (builtin.args[0]) {
            .expr_arg => |expr| expr,
            .type_arg => {
                self.addError(.type_mismatch, builtin.span, "@assert first argument must be a boolean expression, not a type", .{});
                return self.type_builder.voidType();
            },
        };

        // Try to evaluate the condition at compile time
        const condition_value = self.evaluateComptimeExpr(condition_expr);
        if (condition_value == null) {
            self.addError(.comptime_error, builtin.span, "@assert condition must be comptime-known", .{});
            return self.type_builder.voidType();
        }

        // Check that it's a boolean
        if (condition_value.? != .bool_) {
            self.addError(.type_mismatch, builtin.span, "@assert condition must be a boolean, not {s}", .{@tagName(condition_value.?)});
            return self.type_builder.voidType();
        }

        // If condition is false, emit compile error
        if (!condition_value.?.bool_) {
            // Check for optional message argument
            if (builtin.args.len == 2) {
                switch (builtin.args[1]) {
                    .expr_arg => |expr| {
                        switch (expr) {
                            .literal => |lit| {
                                switch (lit.kind) {
                                    .string => |msg| {
                                        self.addError(.comptime_error, builtin.span, "assertion failed: {s}", .{msg});
                                        return self.type_builder.voidType();
                                    },
                                    else => {},
                                }
                            },
                            else => {},
                        }
                    },
                    .type_arg => {},
                }
                // If we get here, message wasn't a string literal
                self.addError(.type_mismatch, builtin.span, "@assert message must be a string literal", .{});
            } else {
                self.addError(.comptime_error, builtin.span, "assertion failed", .{});
            }
        }

        return self.type_builder.voidType();
    }

    /// @repeat(value, count) -> [typeof(value); count]
    /// Creates an array of `count` elements, all initialized to `value`.
    /// `count` must be a comptime-known integer.
    fn checkBuiltinRepeat(self: *TypeChecker, builtin: *ast.BuiltinCall) Type {
        if (builtin.args.len != 2) {
            self.addError(.wrong_number_of_args, builtin.span, "@repeat expects 2 arguments (value, count), got {d}", .{builtin.args.len});
            return self.type_builder.unknownType();
        }

        // First arg is the value to repeat - must be an expression
        const value_expr = switch (builtin.args[0]) {
            .expr_arg => |expr| expr,
            .type_arg => {
                self.addError(.type_mismatch, builtin.span, "@repeat first argument must be a value, not a type", .{});
                return self.type_builder.unknownType();
            },
        };

        // Type-check the value expression
        const element_type = self.checkExpr(value_expr);
        if (element_type == .unknown or element_type == .error_type) {
            return self.type_builder.unknownType();
        }

        // Second arg is the count - must be a comptime-known integer
        const count_expr = switch (builtin.args[1]) {
            .expr_arg => |expr| expr,
            .type_arg => {
                self.addError(.type_mismatch, builtin.span, "@repeat second argument must be an integer, not a type", .{});
                return self.type_builder.unknownType();
            },
        };

        // Evaluate count at compile time
        const count_value = self.evaluateComptimeExpr(count_expr);
        if (count_value == null) {
            self.addError(.comptime_error, builtin.span, "@repeat count must be comptime-known", .{});
            return self.type_builder.unknownType();
        }

        // Check that it's an integer
        const count: usize = switch (count_value.?) {
            .int => |i| blk: {
                if (i.value < 0) {
                    self.addError(.type_mismatch, builtin.span, "@repeat count must be non-negative, got {d}", .{i.value});
                    return self.type_builder.unknownType();
                }
                break :blk @intCast(i.value);
            },
            else => {
                self.addError(.type_mismatch, builtin.span, "@repeat count must be an integer", .{});
                return self.type_builder.unknownType();
            },
        };

        // Store repeat info for codegen
        self.comptime_repeats.put(self.allocator, builtin, .{
            .element_type = element_type,
            .count = count,
            .value_expr = value_expr,
        }) catch {};

        // Return the array type
        return self.type_builder.arrayType(element_type, count) catch self.type_builder.unknownType();
    }

    /// @hasField(T, "field_name") -> bool
    fn checkBuiltinHasField(self: *TypeChecker, builtin: *ast.BuiltinCall) Type {
        if (builtin.args.len != 2) {
            self.addError(.wrong_number_of_args, builtin.span, "@hasField expects 2 arguments, got {d}", .{builtin.args.len});
            return self.type_builder.unknownType();
        }

        // First arg must be a type
        const type_arg = switch (builtin.args[0]) {
            .type_arg => |te| te,
            .expr_arg => {
                self.addError(.type_mismatch, builtin.span, "@hasField first argument must be a type", .{});
                return self.type_builder.unknownType();
            },
        };

        // Second arg must be a string literal
        const field_name: ?[]const u8 = switch (builtin.args[1]) {
            .expr_arg => |expr| blk: {
                switch (expr) {
                    .literal => |lit| {
                        switch (lit.kind) {
                            .string => |s| break :blk s,
                            else => {},
                        }
                    },
                    else => {},
                }
                self.addError(.type_mismatch, builtin.span, "@hasField second argument must be a string literal", .{});
                break :blk null;
            },
            .type_arg => blk: {
                self.addError(.type_mismatch, builtin.span, "@hasField second argument must be a string, not a type", .{});
                break :blk null;
            },
        };

        // Evaluate at compile time
        if (field_name) |name| {
            const resolved_type = self.resolveTypeExpr(type_arg) catch {
                return self.type_builder.boolType();
            };
            const has_field = switch (resolved_type) {
                .struct_ => |s| blk: {
                    for (s.fields) |field| {
                        if (std.mem.eql(u8, field.name, name)) {
                            break :blk true;
                        }
                    }
                    break :blk false;
                },
                else => false,
            };
            self.comptime_bools.put(self.allocator, builtin, has_field) catch {};
        }

        return self.type_builder.boolType();
    }

    /// @sizeOf(T) -> i32 (size of type in bytes)
    fn checkBuiltinSizeOf(self: *TypeChecker, builtin: *ast.BuiltinCall) Type {
        if (builtin.args.len != 1) {
            self.addError(.wrong_number_of_args, builtin.span, "@sizeOf expects 1 argument, got {d}", .{builtin.args.len});
            return self.type_builder.unknownType();
        }

        const type_arg = switch (builtin.args[0]) {
            .type_arg => |ta| ta,
            .expr_arg => {
                self.addError(.type_mismatch, builtin.span, "@sizeOf expects a type argument, not an expression", .{});
                return self.type_builder.unknownType();
            },
        };

        const resolved_type = self.resolveTypeExpr(type_arg) catch {
            return self.type_builder.i32Type();
        };

        const size: i64 = self.computeTypeSize(resolved_type);
        self.comptime_ints.put(self.allocator, builtin, size) catch {};

        return self.type_builder.i32Type();
    }

    /// @alignOf(T) -> i32 (alignment of type in bytes)
    fn checkBuiltinAlignOf(self: *TypeChecker, builtin: *ast.BuiltinCall) Type {
        if (builtin.args.len != 1) {
            self.addError(.wrong_number_of_args, builtin.span, "@alignOf expects 1 argument, got {d}", .{builtin.args.len});
            return self.type_builder.unknownType();
        }

        const type_arg = switch (builtin.args[0]) {
            .type_arg => |ta| ta,
            .expr_arg => {
                self.addError(.type_mismatch, builtin.span, "@alignOf expects a type argument, not an expression", .{});
                return self.type_builder.unknownType();
            },
        };

        const resolved_type = self.resolveTypeExpr(type_arg) catch {
            return self.type_builder.i32Type();
        };

        const alignment: i64 = self.computeTypeAlignment(resolved_type);
        self.comptime_ints.put(self.allocator, builtin, alignment) catch {};

        return self.type_builder.i32Type();
    }

    /// Compute the size of a type in bytes
    fn computeTypeSize(self: *TypeChecker, typ: Type) i64 {
        return switch (typ) {
            .primitive => |p| switch (p) {
                .i8_, .u8_ => 1,
                .i16_, .u16_ => 2,
                .i32_, .u32_, .f32_ => 4,
                .i64_, .u64_, .f64_ => 8,
                .bool_ => 1,
                else => 8, // default for other primitives (i128, isize, etc.)
            },
            .void_ => 0,
            .reference, .function => 8, // 64-bit pointers
            .struct_ => |s| blk: {
                var total: i64 = 0;
                for (s.fields) |field| {
                    total += self.computeTypeSize(field.type_);
                }
                break :blk if (total == 0) 1 else total; // empty struct has size 1
            },
            .enum_ => 4, // enum tag
            .array => |a| self.computeTypeSize(a.element) * @as(i64, @intCast(a.size)),
            .slice => 16, // pointer + length
            .optional => |o| self.computeTypeSize(o.*) + 1, // child + tag byte
            else => 8, // default for unknown types
        };
    }

    /// Compute the alignment of a type in bytes
    fn computeTypeAlignment(self: *TypeChecker, typ: Type) i64 {
        return switch (typ) {
            .primitive => |p| switch (p) {
                .i8_, .u8_, .bool_ => 1,
                .i16_, .u16_ => 2,
                .i32_, .u32_, .f32_ => 4,
                .i64_, .u64_, .f64_ => 8,
                else => 8, // default for other primitives (i128, isize, etc.)
            },
            .void_ => 1,
            .reference, .function => 8, // 64-bit pointers
            .struct_ => |s| blk: {
                var max_align: i64 = 1;
                for (s.fields) |field| {
                    const field_align = self.computeTypeAlignment(field.type_);
                    if (field_align > max_align) {
                        max_align = field_align;
                    }
                }
                break :blk max_align;
            },
            .enum_ => 4,
            .array => |a| self.computeTypeAlignment(a.element),
            .slice => 8, // pointer alignment
            .optional => |o| self.computeTypeAlignment(o.*),
            else => 8,
        };
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
            .if_stmt => |i| self.checkIfStmt(i),
            .match_stmt => |m| self.checkMatchStmt(m),
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
        // Type is always provided - resolve it first for use as hint
        const declared_type = self.resolveTypeExpr(decl.type_) catch self.type_builder.unknownType();

        // Pass declared type as hint for contextual typing of literals
        const value_type = self.checkExprWithHint(decl.value, declared_type);

        if (!self.isTypeCompatible(declared_type, value_type)) {
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
        // Type is always provided - resolve it first for use as hint
        const declared_type = self.resolveTypeExpr(decl.type_) catch self.type_builder.unknownType();

        // Pass declared type as hint for contextual typing of literals
        const value_type = self.checkExprWithHint(decl.value, declared_type);

        if (!self.isTypeCompatible(declared_type, value_type)) {
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
            // Pass expected return type as hint for contextual typing of literals
            const value_type = self.checkExprWithHint(value, self.current_return_type);
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
            .range => |r| r.element_type,
            .list => |l| l.element,
            .set => |s| s.element,
            .map => |m| blk: {
                // Map iteration yields (key, value) tuples
                const tuple_elems = [_]Type{ m.key, m.value };
                break :blk self.type_builder.tupleType(&tuple_elems) catch self.type_builder.unknownType();
            },
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

        // Build comptime parameter bitmask
        var comptime_params: u64 = 0;
        for (func.params, 0..) |param, i| {
            const param_type = self.resolveTypeExpr(param.type_) catch self.type_builder.unknownType();
            param_types.append(self.allocator, param_type) catch {};
            if (param.is_comptime and i < 64) {
                comptime_params |= @as(u64, 1) << @intCast(i);
            }
        }

        const return_type = if (func.return_type) |rt|
            self.resolveTypeExpr(rt) catch self.type_builder.unknownType()
        else
            self.type_builder.voidType();

        const func_type = self.type_builder.functionTypeWithComptime(param_types.items, return_type, comptime_params) catch {
            return;
        };

        // Validate main() signature
        if (std.mem.eql(u8, func.name, "main")) {
            // main() accepts at most one parameter
            if (func.params.len > 1) {
                self.addError(.invalid_operation, func.span, "main() accepts at most one parameter (args: [String])", .{});
            }
            // If one parameter, must be [String]
            if (func.params.len == 1) {
                const param_type = param_types.items[0];
                // Check for slice of String (string_data type, not primitive string_)
                const is_string_slice = param_type == .slice and
                    param_type.slice.element == .string_data;
                if (!is_string_slice) {
                    const type_str = types.typeToString(self.allocator, param_type) catch "unknown";
                    defer self.allocator.free(type_str);
                    self.addError(.type_mismatch, func.params[0].span, "main() parameter must be [String], found {s}", .{type_str});
                }
            }
            // Return type must be i32 or void
            const is_i32 = return_type == .primitive and return_type.primitive == .i32_;
            const is_void = return_type == .void_;
            if (!is_i32 and !is_void) {
                const error_span = if (func.return_type) |rt| rt.span() else func.span;
                self.addError(.type_mismatch, error_span, "main() must return i32 or void", .{});
            }
        }

        // Register function in current scope
        self.current_scope.define(.{
            .name = func.name,
            .type_ = func_type,
            .kind = .function,
            .mutable = false,
            .span = func.span,
        }) catch {};

        // Register comptime functions for compile-time evaluation
        if (func.is_comptime) {
            self.comptime_functions.put(self.allocator, func.name, func) catch {};
        }

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

            // For comptime functions, set the flag so nested comptime calls
            // are type-checked but not evaluated during this phase
            const was_checking_comptime_body = self.checking_comptime_function_body;
            if (func.is_comptime) {
                self.checking_comptime_function_body = true;
            }
            defer self.checking_comptime_function_body = was_checking_comptime_body;

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

        // Pre-register enum type BEFORE resolving variants to allow recursive types.
        // This enables patterns like: enum JsonValue { Array(List[JsonValue]) }
        const enum_type = self.allocator.create(types.EnumType) catch return;
        enum_type.* = .{
            .name = enum_decl.name,
            .type_params = enum_type_params,
            .variants = &.{}, // Will be filled in below
        };

        // Track for cleanup in deinit
        self.generic_enum_types.append(self.allocator, enum_type) catch {};

        // Register in scope FIRST so recursive references resolve
        self.current_scope.define(.{
            .name = enum_decl.name,
            .type_ = .{ .enum_ = enum_type },
            .kind = .type_,
            .mutable = false,
            .span = enum_decl.span,
        }) catch {};

        // NOW resolve variant types (enum name is in scope for self-references)
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

        // Update the enum type with resolved variants
        enum_type.variants = variants.toOwnedSlice(self.allocator) catch &.{};
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

        // Resolve super traits (trait inheritance)
        var super_trait_list: std.ArrayListUnmanaged(*types.TraitType) = .{};
        defer super_trait_list.deinit(self.allocator);

        for (trait_decl.super_traits) |super_trait_expr| {
            const resolved = self.resolveTypeExpr(super_trait_expr) catch {
                self.addError(.undefined_type, trait_decl.span, "cannot resolve super trait", .{});
                continue;
            };
            if (resolved != .trait_) {
                self.addError(.type_mismatch, trait_decl.span, "expected trait type in trait inheritance", .{});
                continue;
            }
            // Check that super trait is already defined (no forward references)
            if (self.trait_registry.get(resolved.trait_.name) == null) {
                self.addError(.undefined_type, trait_decl.span, "super trait '{s}' must be defined before use", .{resolved.trait_.name});
                continue;
            }
            super_trait_list.append(self.allocator, resolved.trait_) catch {};
        }

        // Build associated types list
        var associated_types: std.ArrayListUnmanaged(types.AssociatedType) = .{};
        defer associated_types.deinit(self.allocator);

        var seen_assoc_names: std.StringHashMapUnmanaged(void) = .{};
        defer seen_assoc_names.deinit(self.allocator);

        // First, inherit associated types from super traits
        for (super_trait_list.items) |super_trait| {
            for (super_trait.associated_types) |super_assoc| {
                if (seen_assoc_names.get(super_assoc.name) != null) {
                    // Associated type already defined (conflict from multiple inheritance)
                    // First one wins - could add more sophisticated handling later
                    continue;
                }
                seen_assoc_names.put(self.allocator, super_assoc.name, {}) catch {};
                associated_types.append(self.allocator, super_assoc) catch {};
            }
        }

        // Process associated type declarations from this trait
        for (trait_decl.associated_types) |assoc_decl| {
            // Check for duplicate associated type names
            if (seen_assoc_names.get(assoc_decl.name) != null) {
                self.addError(.duplicate_definition, assoc_decl.span, "duplicate associated type '{s}' in trait", .{assoc_decl.name});
                continue;
            }
            seen_assoc_names.put(self.allocator, assoc_decl.name, {}) catch {};

            // Resolve bounds (trait requirements for the associated type)
            var assoc_bounds: std.ArrayListUnmanaged(*types.TraitType) = .{};
            defer assoc_bounds.deinit(self.allocator);

            for (assoc_decl.bounds) |bound_expr| {
                const resolved = self.resolveTypeExpr(bound_expr) catch {
                    self.addError(.undefined_type, assoc_decl.span, "cannot resolve associated type bound", .{});
                    continue;
                };
                if (resolved != .trait_) {
                    self.addError(.type_mismatch, assoc_decl.span, "expected trait type for associated type bound", .{});
                    continue;
                }
                assoc_bounds.append(self.allocator, resolved.trait_) catch {};
            }

            // Resolve default type if provided
            var default_type: ?Type = null;
            if (assoc_decl.default) |default_expr| {
                default_type = self.resolveTypeExpr(default_expr) catch null;
            }

            associated_types.append(self.allocator, .{
                .name = assoc_decl.name,
                .bounds = assoc_bounds.toOwnedSlice(self.allocator) catch &.{},
                .default = default_type,
            }) catch {};
        }

        // Create a preliminary trait type with associated types for Self.Item resolution
        // Methods will be added later
        const trait_type = self.allocator.create(types.TraitType) catch return;
        trait_type.* = .{
            .name = trait_decl.name,
            .type_params = trait_type_params,
            .associated_types = associated_types.toOwnedSlice(self.allocator) catch &.{},
            .methods = &.{}, // Will be filled in after method processing
            .super_traits = super_trait_list.toOwnedSlice(self.allocator) catch &.{},
        };

        // Set current trait type for Self.Item resolution in method signatures
        self.current_trait_type = trait_type;
        defer self.current_trait_type = null;

        // Build trait methods
        var trait_methods: std.ArrayListUnmanaged(types.TraitMethod) = .{};
        defer trait_methods.deinit(self.allocator);

        var seen_method_names: std.StringHashMapUnmanaged(void) = .{};
        defer seen_method_names.deinit(self.allocator);

        // First, inherit methods from super traits
        for (trait_type.super_traits) |super_trait| {
            for (super_trait.methods) |super_method| {
                if (seen_method_names.get(super_method.name) != null) {
                    // Method already defined (conflict from multiple inheritance)
                    // For now, first one wins - could add more sophisticated handling later
                    continue;
                }
                seen_method_names.put(self.allocator, super_method.name, {}) catch {};
                trait_methods.append(self.allocator, super_method) catch {};
            }
        }

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

        // Update the trait type with the now-complete methods list
        trait_type.methods = trait_methods.toOwnedSlice(self.allocator) catch &.{};

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

        // Get the type name - we support impl blocks for structs and enums
        // For generic impls like impl[T] Pair[T], the target_type is an applied type
        const struct_name = switch (target_type) {
            .struct_ => |s| s.name,
            .enum_ => |e| e.name,
            .applied => |a| blk: {
                // For generic impls, the base should be a struct or enum
                if (a.base == .struct_) {
                    break :blk a.base.struct_.name;
                }
                if (a.base == .enum_) {
                    break :blk a.base.enum_.name;
                }
                self.addError(.invalid_operation, impl_decl.span, "impl blocks currently only support structs and enums", .{});
                return;
            },
            else => {
                self.addError(.invalid_operation, impl_decl.span, "impl blocks currently only support structs and enums", .{});
                return;
            },
        };

        // Handle trait implementations (impl Type: Trait { ... })
        var trait_info: ?TraitInfo = null;
        // For generic traits like From[E], store the type arguments
        var trait_type_args: []const Type = &.{};
        if (impl_decl.trait_type) |trait_type_expr| {
            // Check if this is a generic trait application (e.g., From[IoError])
            const resolved_trait: Type = if (trait_type_expr == .generic_apply) blk: {
                const generic_apply = trait_type_expr.generic_apply;
                // Resolve the base trait type
                const base_trait = self.resolveTypeExpr(generic_apply.base) catch {
                    self.addError(.undefined_type, impl_decl.span, "cannot resolve trait type", .{});
                    return;
                };

                // Resolve type arguments
                var resolved_args = std.ArrayListUnmanaged(Type){};
                for (generic_apply.args) |arg_expr| {
                    const arg_type = self.resolveTypeExpr(arg_expr) catch continue;
                    resolved_args.append(self.allocator, arg_type) catch {};
                }
                trait_type_args = resolved_args.toOwnedSlice(self.allocator) catch &.{};
                self.substituted_type_slices.append(self.allocator, trait_type_args) catch {};

                break :blk base_trait;
            } else self.resolveTypeExpr(trait_type_expr) catch {
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

        // Process associated type bindings from the impl block
        var assoc_bindings: std.ArrayListUnmanaged(AssociatedTypeBinding) = .{};
        defer assoc_bindings.deinit(self.allocator);

        for (impl_decl.associated_types) |binding| {
            // Resolve the concrete type
            const concrete_type = self.resolveTypeExpr(binding.value) catch {
                self.addError(.undefined_type, binding.span, "cannot resolve associated type binding", .{});
                continue;
            };

            // If implementing a trait, verify the binding corresponds to a trait associated type
            if (trait_info) |info| {
                const trait_type = info.trait_type;
                var found_in_trait = false;
                for (trait_type.associated_types) |assoc| {
                    if (std.mem.eql(u8, assoc.name, binding.name)) {
                        found_in_trait = true;

                        // TODO: Check that concrete_type satisfies assoc.bounds
                        // This requires implementing trait bound checking for arbitrary types

                        break;
                    }
                }
                if (!found_in_trait) {
                    self.addError(.undefined_type, binding.span, "'{s}' is not an associated type of trait '{s}'", .{ binding.name, trait_type.name });
                    continue;
                }
            }

            assoc_bindings.append(self.allocator, .{
                .name = binding.name,
                .concrete_type = concrete_type,
            }) catch {};
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
                // For 'self' parameter, use the target struct type (possibly wrapped in reference)
                if (std.mem.eql(u8, param.name, "self")) {
                    // Check if self is by reference (&self or &mut self)
                    if (param.type_ == .reference) {
                        // Preserve the reference wrapper with the correct mutability
                        const self_ref = self.type_builder.referenceType(target_type, param.type_.reference.mutable) catch {
                            param_types.append(self.allocator, target_type) catch {};
                            continue;
                        };
                        param_types.append(self.allocator, self_ref) catch {};
                    } else {
                        // self by value
                        param_types.append(self.allocator, target_type) catch {};
                    }
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

            // Check that all required associated types are provided
            for (trait_type.associated_types) |trait_assoc| {
                var found = false;
                for (assoc_bindings.items) |binding| {
                    if (std.mem.eql(u8, binding.name, trait_assoc.name)) {
                        found = true;
                        break;
                    }
                }

                if (!found and trait_assoc.default == null) {
                    self.addError(.trait_not_implemented, impl_decl.span, "missing associated type binding for '{s}'", .{trait_assoc.name});
                }
            }

            for (trait_type.methods) |trait_method| {
                // Check if method is implemented
                var found = false;
                if (impl_methods) |methods_list| {
                    for (methods_list.items) |impl_method| {
                        if (std.mem.eql(u8, impl_method.name, trait_method.name)) {
                            found = true;
                            // Verify method signature matches trait signature
                            // Pass trait type parameters and their bindings for substitution
                            _ = self.verifyMethodSignature(impl_method, trait_method, target_type, trait_type.type_params, trait_type_args, impl_decl.span);
                            break;
                        }
                    }
                }

                if (!found and !trait_method.has_default) {
                    self.addError(.trait_not_implemented, impl_decl.span, "missing implementation for required trait method '{s}'", .{trait_method.name});
                }
            }

            // Register this trait implementation
            // For generic traits like From[IoError], include type args in key
            const impl_key = if (trait_type_args.len > 0)
                self.makeGenericTraitImplKey(struct_name, trait_type.name, trait_type_args)
            else
                self.makeTraitImplKey(struct_name, trait_type.name);
            const impl_result = self.trait_impls.getOrPut(self.allocator, impl_key) catch return;
            if (!impl_result.found_existing) {
                impl_result.value_ptr.* = .{};
            }

            impl_result.value_ptr.append(self.allocator, .{
                .trait_name = trait_type.name,
                .impl_type_name = struct_name,
                .impl_type_params = impl_type_params,
                .associated_type_bindings = assoc_bindings.toOwnedSlice(self.allocator) catch &.{},
                .methods = if (impl_methods) |m| m.items else &.{},
            }) catch {};
        }
    }

    /// Create a unique key for trait implementation lookup
    fn makeTraitImplKey(self: *TypeChecker, type_name: []const u8, trait_name: []const u8) []const u8 {
        return std.fmt.allocPrint(self.allocator, "{s}:{s}", .{ type_name, trait_name }) catch "";
    }

    /// Create a key for generic trait implementations like From[IoError].
    /// Format: "TypeName:TraitName[TypeArg1,TypeArg2,...]"
    fn makeGenericTraitImplKey(self: *TypeChecker, type_name: []const u8, trait_name: []const u8, type_args: []const Type) []const u8 {
        // Build the type args string
        var args_buf = std.ArrayListUnmanaged(u8){};
        defer args_buf.deinit(self.allocator);

        args_buf.append(self.allocator, '[') catch {};
        for (type_args, 0..) |arg, i| {
            if (i > 0) args_buf.append(self.allocator, ',') catch {};
            const arg_name = self.getTypeName(arg) orelse "unknown";
            args_buf.appendSlice(self.allocator, arg_name) catch {};
        }
        args_buf.append(self.allocator, ']') catch {};

        return std.fmt.allocPrint(self.allocator, "{s}:{s}{s}", .{
            type_name,
            trait_name,
            args_buf.items,
        }) catch "";
    }

    /// Check if a type implements a trait
    pub fn typeImplementsTrait(self: *TypeChecker, type_name: []const u8, trait_name: []const u8) bool {
        const key = self.makeTraitImplKey(type_name, trait_name);
        defer self.allocator.free(key);
        return self.trait_impls.get(key) != null;
    }

    /// Check if target_type implements From[source_type].
    /// Used by the ? operator to determine if automatic error conversion is possible.
    /// Returns true if impl target_type: From[source_type] exists.
    pub fn hasFromImpl(self: *TypeChecker, target_type: Type, source_type: Type) bool {
        // Get the target type name (must be an enum for error types)
        const target_name = self.getTypeName(target_type) orelse return false;
        const source_name = self.getTypeName(source_type) orelse return false;

        // Build the key "TargetError:From[SourceError]"
        const key = std.fmt.allocPrint(self.allocator, "{s}:From[{s}]", .{ target_name, source_name }) catch return false;
        defer self.allocator.free(key);

        return self.trait_impls.get(key) != null;
    }

    /// Get the name of a type for trait implementation lookup.
    /// Returns null for types that cannot implement traits.
    fn getTypeName(self: *TypeChecker, t: Type) ?[]const u8 {
        _ = self;
        return switch (t) {
            .struct_ => |s| s.name,
            .enum_ => |e| e.name,
            else => null,
        };
    }

    /// Check if a type implements both Hash and Eq traits (required for Map keys).
    /// All primitives have builtin Hash and Eq implementations.
    /// For struct types, checks that the struct explicitly implements both traits.
    /// Reports an error if the type doesn't satisfy the requirements.
    fn typeImplementsHashAndEq(self: *TypeChecker, key_type: Type, span: Span) bool {
        // Primitives have builtin Hash and Eq implementations
        if (key_type == .primitive) {
            return true;
        }

        // For struct types, check that the struct implements both Hash and Eq
        if (key_type == .struct_) {
            const struct_name = key_type.struct_.name;
            var valid = true;

            if (!self.typeImplementsTrait(struct_name, "Hash")) {
                self.addError(.trait_not_implemented, span, "Map key type '{s}' must implement Hash", .{struct_name});
                valid = false;
            }
            if (!self.typeImplementsTrait(struct_name, "Eq")) {
                self.addError(.trait_not_implemented, span, "Map key type '{s}' must implement Eq", .{struct_name});
                valid = false;
            }
            return valid;
        }

        // For enum types, check if they implement Hash and Eq
        if (key_type == .enum_) {
            const enum_name = key_type.enum_.name;
            var valid = true;

            if (!self.typeImplementsTrait(enum_name, "Hash")) {
                self.addError(.trait_not_implemented, span, "Map key type '{s}' must implement Hash", .{enum_name});
                valid = false;
            }
            if (!self.typeImplementsTrait(enum_name, "Eq")) {
                self.addError(.trait_not_implemented, span, "Map key type '{s}' must implement Eq", .{enum_name});
                valid = false;
            }
            return valid;
        }

        // Other types (functions, references, etc.) are not valid map keys
        self.addError(.type_mismatch, span, "type is not valid as a Map key - must implement Hash + Eq", .{});
        return false;
    }

    /// Check if a type represents Self in a trait definition.
    /// Self can be represented as:
    /// - .unknown (used by Eq, Ordered, Hash, Drop)
    /// - .type_var with name "Self" (used by Clone, Default, Iterator)
    fn isSelfType(t: Type) bool {
        return t == .unknown or (t == .type_var and std.mem.eql(u8, t.type_var.name, "Self"));
    }

    /// Check if a type is a reference to Self.
    /// Returns the reference info if true, null otherwise.
    fn isRefToSelf(t: Type) ?struct { mutable: bool } {
        if (t != .reference) return null;
        if (isSelfType(t.reference.inner)) {
            return .{ .mutable = t.reference.mutable };
        }
        return null;
    }

    /// Verify that an impl method signature matches the trait method signature.
    /// Returns true if signatures match, false otherwise.
    /// The trait method uses 'unknown' or type_var "Self" for Self parameter.
    /// For generic traits like From[E], trait_type_params contains [E] and trait_type_args
    /// contains the concrete types [NetworkError]. Type variables in trait_sig are substituted.
    fn verifyMethodSignature(
        self: *TypeChecker,
        impl_method: StructMethod,
        trait_method: types.TraitMethod,
        impl_type: Type,
        trait_type_params: []const types.TypeVar,
        trait_type_args: []const Type,
        span: Span,
    ) bool {
        const impl_func = impl_method.func_type.function;
        const trait_sig = &trait_method.signature;

        // Check parameter count
        if (impl_func.params.len != trait_sig.params.len) {
            self.addError(.type_mismatch, span, "method '{s}' has {d} parameters, but trait requires {d}", .{
                trait_method.name,
                impl_func.params.len,
                trait_sig.params.len,
            });
            return false;
        }

        // Check each parameter type
        for (impl_func.params, trait_sig.params, 0..) |impl_param, trait_param, idx| {
            // Check if trait parameter is Self type (unknown or type_var "Self")
            if (isSelfType(trait_param)) {
                // Self parameter by value - impl should use the implementing type
                if (idx == 0) {
                    // Accept: impl_type, &impl_type, &mut impl_type
                    const matches = impl_param.eql(impl_type) or
                        (impl_param == .reference and impl_param.reference.inner.eql(impl_type));
                    if (!matches) {
                        self.addError(.type_mismatch, span, "method '{s}' parameter {d} should be Self type", .{
                            trait_method.name,
                            idx,
                        });
                        return false;
                    }
                }
                continue;
            }

            // Check if trait parameter is a reference to Self (&Self or &mut Self)
            if (isRefToSelf(trait_param)) |ref_info| {
                // Reference to Self - impl should use reference to implementing type
                if (idx == 0) {
                    // Must be a reference type
                    if (impl_param != .reference) {
                        self.addError(.type_mismatch, span, "method '{s}' parameter {d} should be reference to Self", .{
                            trait_method.name,
                            idx,
                        });
                        return false;
                    }
                    // Inner type must match implementing type
                    if (!impl_param.reference.inner.eql(impl_type)) {
                        self.addError(.type_mismatch, span, "method '{s}' parameter {d} should be reference to Self", .{
                            trait_method.name,
                            idx,
                        });
                        return false;
                    }
                    // Mutability must match
                    if (impl_param.reference.mutable != ref_info.mutable) {
                        self.addError(.type_mismatch, span, "method '{s}' parameter {d} mutability mismatch", .{
                            trait_method.name,
                            idx,
                        });
                        return false;
                    }
                }
                continue;
            }

            // Check if trait parameter is a type variable (like E in From[E])
            // Substitute with the concrete type from trait_type_args
            var expected_param = trait_param;
            if (trait_param == .type_var) {
                // Look up the type variable in trait_type_params to find its index
                for (trait_type_params, 0..) |tp, ti| {
                    if (tp.id == trait_param.type_var.id) {
                        if (ti < trait_type_args.len) {
                            expected_param = trait_type_args[ti];
                        }
                        break;
                    }
                }
            }

            // Regular parameter - types should match (after substitution)
            if (!impl_param.eql(expected_param)) {
                self.addError(.type_mismatch, span, "method '{s}' parameter {d} type mismatch", .{
                    trait_method.name,
                    idx,
                });
                return false;
            }
        }

        // Check return type
        // Note: 'unknown' is used for Self in user-defined traits AND for Self.Item,
        // so we skip validation when return type is 'unknown' (can't distinguish here).
        // For builtin traits, Self is represented as type_var with name "Self".
        if (trait_sig.return_type == .type_var and
            std.mem.eql(u8, trait_sig.return_type.type_var.name, "Self"))
        {
            // Self return type (builtin traits) - impl should return the implementing type
            if (!impl_func.return_type.eql(impl_type)) {
                self.addError(.type_mismatch, span, "method '{s}' return type should be Self", .{
                    trait_method.name,
                });
                return false;
            }
        } else if (trait_sig.return_type == .type_var) {
            // Non-Self type variable (like T in Into[T]) - substitute with concrete type
            var expected_return = trait_sig.return_type;
            for (trait_type_params, 0..) |tp, ti| {
                if (tp.id == trait_sig.return_type.type_var.id) {
                    if (ti < trait_type_args.len) {
                        expected_return = trait_type_args[ti];
                    }
                    break;
                }
            }
            if (!impl_func.return_type.eql(expected_return)) {
                self.addError(.type_mismatch, span, "method '{s}' return type mismatch", .{
                    trait_method.name,
                });
                return false;
            }
        } else if (trait_sig.return_type == .optional) {
            // Handle ?Self.Item - optional containing associated type ref or unknown
            const inner = trait_sig.return_type.optional.*;
            if (inner == .associated_type_ref or inner == .unknown) {
                // The impl's return type should be optional with the concrete associated type
                // For now, just verify it's an optional type - full validation would require
                // looking up the associated type binding
                if (impl_func.return_type != .optional) {
                    self.addError(.type_mismatch, span, "method '{s}' return type should be optional", .{
                        trait_method.name,
                    });
                    return false;
                }
                // Return type validated enough for now
            } else if (!impl_func.return_type.eql(trait_sig.return_type)) {
                self.addError(.type_mismatch, span, "method '{s}' return type mismatch", .{
                    trait_method.name,
                });
                return false;
            }
        } else if (trait_sig.return_type == .associated_type_ref) {
            // Handle Self.Item directly - the impl should return the concrete associated type
            // For now, accept any type - full validation would require looking up the binding
            _ = impl_func.return_type;
        } else if (trait_sig.return_type != .unknown) {
            // Skip validation for 'unknown' (used for both Self and Self.Item in user traits)
            if (!impl_func.return_type.eql(trait_sig.return_type)) {
                self.addError(.type_mismatch, span, "method '{s}' return type mismatch", .{
                    trait_method.name,
                });
                return false;
            }
        }

        return true;
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

        // Try to evaluate the constant at compile time and store it for comptime access
        if (self.evaluateComptimeExpr(const_decl.value)) |interp_value| {
            if (self.valueToComptimeValue(interp_value, const_decl.span)) |comptime_val| {
                self.constant_values.put(self.allocator, const_decl.name, comptime_val) catch {};
            }
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
                // Determine the type to match - either from pattern or from expected type
                var match_type = expected_type;

                if (v.type_expr) |type_expr| {
                    // Pattern specifies a type - resolve it
                    match_type = self.resolveTypeExpr(type_expr) catch {
                        self.addError(.undefined_type, v.span, "unknown type in pattern", .{});
                        return;
                    };
                }

                // Handle Result type patterns: Ok(v), Err(e)
                if (match_type == .result) {
                    const result_type = match_type.result;
                    if (std.mem.eql(u8, v.variant_name, "Ok")) {
                        if (v.payload) |payload_pattern| {
                            self.checkPattern(payload_pattern, result_type.ok_type);
                        } else {
                            self.addError(.invalid_pattern, v.span, "Ok variant expects payload", .{});
                        }
                    } else if (std.mem.eql(u8, v.variant_name, "Err")) {
                        if (v.payload) |payload_pattern| {
                            self.checkPattern(payload_pattern, result_type.err_type);
                        } else {
                            self.addError(.invalid_pattern, v.span, "Err variant expects payload", .{});
                        }
                    } else {
                        self.addError(.undefined_variant, v.span, "unknown Result variant '{s}'", .{v.variant_name});
                    }
                    return;
                }

                // Handle Optional type patterns: Some(v), None
                if (match_type == .optional) {
                    const inner_type = match_type.optional.*;
                    if (std.mem.eql(u8, v.variant_name, "Some")) {
                        if (v.payload) |payload_pattern| {
                            self.checkPattern(payload_pattern, inner_type);
                        } else {
                            self.addError(.invalid_pattern, v.span, "Some variant expects payload", .{});
                        }
                    } else if (std.mem.eql(u8, v.variant_name, "None")) {
                        if (v.payload != null) {
                            self.addError(.invalid_pattern, v.span, "None variant takes no payload", .{});
                        }
                    } else {
                        self.addError(.undefined_variant, v.span, "unknown Optional variant '{s}'", .{v.variant_name});
                    }
                    return;
                }

                // Handle regular enum types
                if (match_type != .enum_) {
                    self.addError(.invalid_pattern, v.span, "variant pattern requires enum type", .{});
                    return;
                }

                const enum_def = match_type.enum_;

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
                // If type annotation is present, verify it matches the inferred type
                var actual_type = t;
                if (bind.type_annotation) |type_expr| {
                    const declared_type = self.resolveTypeExpr(type_expr) catch self.type_builder.unknownType();
                    if (!declared_type.eql(t)) {
                        self.addError(.type_mismatch, bind.span, "declared type doesn't match inferred type", .{});
                    }
                    actual_type = declared_type;
                }
                self.current_scope.define(.{
                    .name = bind.name,
                    .type_ = actual_type,
                    .kind = .variable,
                    .mutable = bind.mutable,
                    .span = bind.span,
                }) catch {};
            },
            .variant => |v| {
                // Determine the type to match
                var match_type = t;
                if (v.type_expr) |type_expr| {
                    match_type = self.resolveTypeExpr(type_expr) catch return;
                }

                // Handle Result type patterns: Ok(v), Err(e)
                if (match_type == .result) {
                    const result_type = match_type.result;
                    if (std.mem.eql(u8, v.variant_name, "Ok")) {
                        if (v.payload) |payload_pattern| {
                            self.bindPattern(payload_pattern, result_type.ok_type);
                        }
                    } else if (std.mem.eql(u8, v.variant_name, "Err")) {
                        if (v.payload) |payload_pattern| {
                            self.bindPattern(payload_pattern, result_type.err_type);
                        }
                    }
                    return;
                }

                // Handle Optional type patterns: Some(v), None
                if (match_type == .optional) {
                    const inner_type = match_type.optional.*;
                    if (std.mem.eql(u8, v.variant_name, "Some")) {
                        if (v.payload) |payload_pattern| {
                            self.bindPattern(payload_pattern, inner_type);
                        }
                    }
                    // None has no payload to bind
                    return;
                }

                // Handle regular enum types
                if (match_type != .enum_) return;
                const enum_def = match_type.enum_;

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
        return self.isTypeCompatible(target, value);
    }

    /// Check if a value type is compatible with a target type.
    /// This allows:
    /// - Exact type equality
    /// - Unknown types (for error recovery)
    /// - Array to slice coercion (array[T, N] -> slice[T])
    fn isTypeCompatible(self: *TypeChecker, target: Type, value: Type) bool {
        _ = self;

        // Exact match
        if (target.eql(value)) return true;

        // Unknown types (error recovery)
        if (target == .unknown or value == .unknown) return true;

        // Array to slice coercion: [T; N] can be assigned to [T]
        if (target == .slice and value == .array) {
            return target.slice.element.eql(value.array.element);
        }

        return false;
    }

    // ========================================================================
    // Module System Support
    // ========================================================================

    /// Set the module resolver for multi-file compilation.
    pub fn setModuleResolver(self: *TypeChecker, resolver: *ModuleResolver) void {
        self.module_resolver_ref = resolver;
    }

    /// Set the current module being checked.
    pub fn setCurrentModule(self: *TypeChecker, mod: *ModuleInfo) void {
        self.current_module = mod;
    }

    /// Prepare for checking a new module by resetting to a fresh module scope.
    /// This keeps builtin types but clears module-level function/variable definitions.
    pub fn prepareForNewModule(self: *TypeChecker) void {
        // Create a new scope for this module that inherits from global (builtins)
        const module_scope = self.allocator.create(Scope) catch return;
        module_scope.* = Scope.init(self.allocator, self.global_scope, .global);
        self.current_scope = module_scope;
        // Track for cleanup
        self.module_scopes.append(self.allocator, module_scope) catch {};
    }

    /// Register exported symbols from a module.
    pub fn registerModuleExports(self: *TypeChecker, mod: *ModuleInfo) !void {
        const canonical = try mod.canonicalName(self.allocator);
        defer self.allocator.free(canonical);

        var symbols = ModuleSymbols{
            .symbols = .{},
            .module_info = mod,
        };

        // Collect exports from the module's AST
        if (mod.module_ast) |module_ast| {
            for (module_ast.declarations) |decl| {
                switch (decl) {
                    .function => |f| {
                        if (f.is_pub) {
                            const sym = ModuleSymbol{
                                .name = f.name,
                                .kind = .function,
                                .type_ = self.current_scope.lookup(f.name).?.type_,
                                .is_pub = true,
                            };
                            try symbols.symbols.put(self.allocator, f.name, sym);
                        }
                    },
                    .struct_decl => |s| {
                        if (s.is_pub) {
                            const sym = ModuleSymbol{
                                .name = s.name,
                                .kind = .struct_type,
                                .type_ = null, // Types don't have a Type value
                                .is_pub = true,
                            };
                            try symbols.symbols.put(self.allocator, s.name, sym);
                        }
                    },
                    .enum_decl => |e| {
                        if (e.is_pub) {
                            const sym = ModuleSymbol{
                                .name = e.name,
                                .kind = .enum_type,
                                .type_ = null,
                                .is_pub = true,
                            };
                            try symbols.symbols.put(self.allocator, e.name, sym);
                        }
                    },
                    .trait_decl => |t| {
                        if (t.is_pub) {
                            const sym = ModuleSymbol{
                                .name = t.name,
                                .kind = .trait_type,
                                .type_ = null,
                                .is_pub = true,
                            };
                            try symbols.symbols.put(self.allocator, t.name, sym);
                        }
                    },
                    .const_decl => |c| {
                        if (c.is_pub) {
                            const sym = ModuleSymbol{
                                .name = c.name,
                                .kind = .constant,
                                .type_ = self.current_scope.lookup(c.name).?.type_,
                                .is_pub = true,
                            };
                            try symbols.symbols.put(self.allocator, c.name, sym);
                        }
                    },
                    .type_alias => |t| {
                        if (t.is_pub) {
                            const sym = ModuleSymbol{
                                .name = t.name,
                                .kind = .type_alias,
                                .type_ = null,
                                .is_pub = true,
                            };
                            try symbols.symbols.put(self.allocator, t.name, sym);
                        }
                    },
                    else => {},
                }
            }
        }

        // Store in registry using duplicated canonical name
        const canonical_dup = try self.allocator.dupe(u8, canonical);
        try self.module_registry.put(self.allocator, canonical_dup, symbols);
    }

    /// Process all imports for a module.
    fn processImports(self: *TypeChecker, module: ast.Module) void {
        for (module.imports) |import_decl| {
            self.processImport(import_decl) catch {
                // Error already reported in processImport
            };
        }
    }

    /// Process a single import declaration.
    fn processImport(self: *TypeChecker, import_decl: ast.ImportDecl) !void {
        // Build canonical name for the imported module
        var canonical_buf = std.ArrayListUnmanaged(u8){};
        defer canonical_buf.deinit(self.allocator);

        for (import_decl.path, 0..) |segment, i| {
            try canonical_buf.appendSlice(self.allocator, segment);
            if (i < import_decl.path.len - 1) {
                try canonical_buf.append(self.allocator, '.');
            }
        }

        const canonical = canonical_buf.items;

        // Look up the module in the registry
        const mod_symbols = self.module_registry.get(canonical) orelse {
            self.addError(.undefined_module, import_decl.span, "module '{s}' not found", .{canonical});
            return error.UndefinedModule;
        };

        // Handle different import styles
        if (import_decl.items) |items| {
            switch (items) {
                .all => {
                    // import module.* - import all public symbols
                    self.importAllSymbols(mod_symbols, import_decl.span);
                },
                .specific => |specific_items| {
                    // import module.{ A, B } - import specific symbols
                    for (specific_items) |item| {
                        self.importSpecificSymbol(mod_symbols, item, import_decl.span);
                    }
                },
            }
        } else {
            // import module - just makes module name available as namespace
            // The last segment becomes the namespace name (or alias if provided)
            const namespace_name = import_decl.alias orelse import_decl.path[import_decl.path.len - 1];

            // Register the module as a namespace symbol
            self.current_scope.define(.{
                .name = namespace_name,
                .type_ = self.type_builder.unknownType(), // Module namespace type
                .kind = .module,
                .mutable = false,
                .span = import_decl.span,
            }) catch {};
        }
    }

    /// Import all public symbols from a module into current scope.
    fn importAllSymbols(self: *TypeChecker, mod_symbols: ModuleSymbols, span: Span) void {
        var iter = mod_symbols.symbols.iterator();
        while (iter.next()) |entry| {
            const sym = entry.value_ptr.*;
            if (!sym.is_pub) continue;

            // Check for conflicts
            if (self.current_scope.lookupLocal(sym.name)) |existing| {
                self.addError(.import_conflict, span, "import '{s}' conflicts with existing symbol defined at {d}:{d}", .{ sym.name, existing.span.line, existing.span.column });
                continue;
            }

            // Add to scope based on symbol kind
            const symbol_kind: Symbol.Kind = switch (sym.kind) {
                .function => .function,
                .struct_type, .enum_type, .trait_type, .type_alias => .type_,
                .constant => .constant,
            };

            self.current_scope.define(.{
                .name = sym.name,
                .type_ = sym.type_ orelse self.type_builder.unknownType(),
                .kind = symbol_kind,
                .mutable = false,
                .span = span,
            }) catch {};
        }
    }

    /// Import a specific symbol from a module into current scope.
    fn importSpecificSymbol(self: *TypeChecker, mod_symbols: ModuleSymbols, item: ast.ImportItem, span: Span) void {
        const sym = mod_symbols.symbols.get(item.name) orelse {
            self.addError(.undefined_variable, item.span, "symbol '{s}' not found in module", .{item.name});
            return;
        };

        if (!sym.is_pub) {
            self.addError(.visibility_error, item.span, "symbol '{s}' is not public", .{item.name});
            return;
        }

        // Use alias if provided, otherwise use original name
        const local_name = item.alias orelse item.name;

        // Check for conflicts
        if (self.current_scope.lookupLocal(local_name)) |existing| {
            self.addError(.import_conflict, span, "import '{s}' conflicts with existing symbol defined at {d}:{d}", .{ local_name, existing.span.line, existing.span.column });
            return;
        }

        // Add to scope
        const symbol_kind: Symbol.Kind = switch (sym.kind) {
            .function => .function,
            .struct_type, .enum_type, .trait_type, .type_alias => .type_,
            .constant => .constant,
        };

        self.current_scope.define(.{
            .name = local_name,
            .type_ = sym.type_ orelse self.type_builder.unknownType(),
            .kind = symbol_kind,
            .mutable = false,
            .span = span,
            // If aliased, store original name for codegen to find the LLVM function
            .original_name = if (item.alias != null) item.name else null,
        }) catch {};
    }

    // ========================================================================
    // Module Checking Entry Point
    // ========================================================================

    pub fn checkModule(self: *TypeChecker, module: ast.Module) void {
        // Process imports first (for multi-file compilation)
        self.processImports(module);

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

                    // Build comptime parameter bitmask
                    var comptime_params: u64 = 0;
                    for (f.params, 0..) |param, i| {
                        const param_type = self.resolveTypeExpr(param.type_) catch self.type_builder.unknownType();
                        param_types.append(self.allocator, param_type) catch {};
                        if (param.is_comptime and i < 64) {
                            comptime_params |= @as(u64, 1) << @intCast(i);
                        }
                    }

                    const return_type = if (f.return_type) |rt|
                        self.resolveTypeExpr(rt) catch self.type_builder.unknownType()
                    else
                        self.type_builder.voidType();

                    const func_type = self.type_builder.functionTypeWithComptime(param_types.items, return_type, comptime_params) catch continue;

                    // Validate main() signature
                    if (std.mem.eql(u8, f.name, "main")) {
                        // main() accepts at most one parameter
                        if (f.params.len > 1) {
                            self.addError(.invalid_operation, f.span, "main() accepts at most one parameter (args: [String])", .{});
                        }
                        // If one parameter, must be [String]
                        if (f.params.len == 1) {
                            const param_type = param_types.items[0];
                            // Check for slice of String (string_data type, not primitive string_)
                            const is_string_slice = param_type == .slice and
                                param_type.slice.element == .string_data;
                            if (!is_string_slice) {
                                const type_str = types.typeToString(self.allocator, param_type) catch "unknown";
                                defer self.allocator.free(type_str);
                                self.addError(.type_mismatch, f.params[0].span, "main() parameter must be [String], found {s}", .{type_str});
                            }
                        }
                        // Return type must be i32 or void
                        const is_i32 = return_type == .primitive and return_type.primitive == .i32_;
                        const is_void = return_type == .void_;
                        if (!is_i32 and !is_void) {
                            const error_span = if (f.return_type) |rt| rt.span() else f.span;
                            self.addError(.type_mismatch, error_span, "main() must return i32 or void", .{});
                        }
                    }

                    self.current_scope.define(.{
                        .name = f.name,
                        .type_ = func_type,
                        .kind = .function,
                        .mutable = false,
                        .span = f.span,
                    }) catch {};

                    // Register comptime functions for compile-time evaluation
                    if (f.is_comptime) {
                        self.comptime_functions.put(self.allocator, f.name, f) catch {};
                    }
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

                        // For comptime functions, set the flag so nested comptime calls
                        // are type-checked but not evaluated during this phase
                        const was_checking_comptime_body = self.checking_comptime_function_body;
                        if (f.is_comptime) {
                            self.checking_comptime_function_body = true;
                        }
                        defer self.checking_comptime_function_body = was_checking_comptime_body;

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
