const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("../ast.zig");
const types = @import("../types.zig");
const Type = types.Type;
const Primitive = types.Primitive;
const TypeBuilder = types.TypeBuilder;
const Span = ast.Span;
const module_resolver = @import("../module_resolver.zig");
const ModuleInfo = module_resolver.ModuleInfo;
const ModuleResolver = module_resolver.ModuleResolver;
const interpreter = @import("../interpreter.zig");
const Interpreter = interpreter.Interpreter;
const values = @import("../values.zig");
const Value = values.Value;

// Import submodules for type definitions
pub const generics = @import("generics.zig");
pub const traits = @import("traits.zig");
pub const comptime_ = @import("comptime.zig");
pub const builtins = @import("builtins.zig");
pub const modules = @import("modules.zig");

// Import implementation modules
const statements = @import("statements.zig");
const patterns = @import("patterns.zig");
const builtins_check = @import("builtins_check.zig");
const declarations = @import("declarations.zig");
const comptime_eval = @import("comptime_eval.zig");
const expressions = @import("expressions.zig");
const type_utils = @import("type_utils.zig");
const methods = @import("methods.zig");
const type_resolution = @import("type_resolution.zig");
const trait_checking = @import("trait_checking.zig");
const method_calls = @import("method_calls.zig");

// Re-export types from submodules
pub const MonomorphizedFunction = generics.MonomorphizedFunction;
pub const MonomorphizedStruct = generics.MonomorphizedStruct;
pub const MonomorphizedEnum = generics.MonomorphizedEnum;
pub const MonomorphizedMethod = generics.MonomorphizedMethod;
pub const TraitMethodCall = generics.TraitMethodCall;

pub const TraitInfo = traits.TraitInfo;
pub const AssociatedTypeBinding = traits.AssociatedTypeBinding;
pub const TraitImplInfo = traits.TraitImplInfo;
pub const StructMethod = traits.StructMethod;
pub const ErrorConversionInfo = traits.ErrorConversionInfo;

pub const ComptimeValue = comptime_.ComptimeValue;
pub const ComptimeStruct = comptime_.ComptimeStruct;
pub const ComptimeArray = comptime_.ComptimeArray;
pub const RepeatInfo = comptime_.RepeatInfo;
const max_comptime_depth = comptime_.max_comptime_depth;

pub const ModuleSymbols = modules.ModuleSymbols;
pub const ModuleSymbol = modules.ModuleSymbol;

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
        // Unsafe errors
        unsafe_trait_requires_unsafe_impl,
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
    /// Maps mangled_name to MonomorphizedFunction info for O(1) lookup.
    monomorphized_functions: std.StringHashMapUnmanaged(MonomorphizedFunction),
    /// Storage for generic function declarations (needed for codegen monomorphization).
    generic_functions: std.StringHashMapUnmanaged(*ast.FunctionDecl),
    /// Maps call expression pointers to resolved mangled function names.
    /// Used by codegen to emit calls to monomorphized functions.
    call_resolutions: std.AutoHashMapUnmanaged(*ast.Call, []const u8),
    /// Cache of monomorphized struct instances.
    /// Maps mangled_name to MonomorphizedStruct info for O(1) lookup.
    monomorphized_structs: std.StringHashMapUnmanaged(MonomorphizedStruct),
    /// Cache of monomorphized enum instances.
    /// Maps mangled_name to MonomorphizedEnum info for O(1) lookup.
    monomorphized_enums: std.StringHashMapUnmanaged(MonomorphizedEnum),
    /// Track generic enum definitions for cleanup (created in checkEnum).
    generic_enum_types: std.ArrayListUnmanaged(*types.EnumType),
    /// Track generic struct definitions for cleanup (created in checkStruct).
    generic_struct_types: std.ArrayListUnmanaged(*types.StructType),
    /// Registry of methods defined in impl blocks.
    /// Maps struct name -> list of methods.
    struct_methods: std.StringHashMapUnmanaged(std.ArrayListUnmanaged(StructMethod)),
    /// Cache of monomorphized method instances for generic structs.
    /// Maps mangled_name to MonomorphizedMethod info for O(1) lookup.
    monomorphized_methods: std.StringHashMapUnmanaged(MonomorphizedMethod),
    /// Registry of trait definitions.
    /// Maps trait name -> TraitInfo.
    trait_registry: std.StringHashMapUnmanaged(TraitInfo),
    /// Track trait type allocations for cleanup.
    trait_types: std.ArrayListUnmanaged(*types.TraitType),
    /// Track extern type allocations for cleanup (created in checkExternType).
    extern_types: std.ArrayListUnmanaged(*types.ExternType),
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
    /// Current impl target type (for resolving Self in impl blocks)
    current_impl_type: ?Type,

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

    /// Tracks whether we're inside an unsafe context (unsafe block or unsafe fn body).
    /// When true, unsafe operations like calling extern functions are allowed.
    in_unsafe_context: bool,

    /// Maps debug() call AST nodes to the argument type.
    /// Used by codegen to emit type-specific formatting code.
    debug_call_types: std.AutoHashMapUnmanaged(*ast.Call, Type),

    const CaptureInfo = struct {
        is_mutable: bool,
        type_: Type,
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
            .extern_types = .{},
            .trait_impls = .{},
            .type_var_slices = .{},
            .substituted_type_slices = .{},
            .trait_method_calls = .{},
            .current_trait_type = null,
            .current_impl_type = null,
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
            .in_unsafe_context = false,
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
        var func_iter = self.monomorphized_functions.valueIterator();
        while (func_iter.next()) |func| {
            self.allocator.free(func.mangled_name);
            self.allocator.free(func.type_args);
        }
        self.monomorphized_functions.deinit(self.allocator);

        self.generic_functions.deinit(self.allocator);
        self.call_resolutions.deinit(self.allocator);

        // Clean up monomorphized structs
        var struct_iter = self.monomorphized_structs.valueIterator();
        while (struct_iter.next()) |s| {
            self.allocator.free(s.mangled_name);
            self.allocator.free(s.type_args);
            self.allocator.free(s.concrete_type.fields);
            self.allocator.destroy(s.concrete_type);
        }
        self.monomorphized_structs.deinit(self.allocator);

        // Clean up monomorphized enums
        var enum_iter = self.monomorphized_enums.valueIterator();
        while (enum_iter.next()) |e| {
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
        var method_iter = self.monomorphized_methods.valueIterator();
        while (method_iter.next()) |method| {
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

        // Clean up extern type allocations
        for (self.extern_types.items) |extern_type| {
            self.allocator.destroy(extern_type);
        }
        self.extern_types.deinit(self.allocator);

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
        const builtin_types = [_]struct { name: []const u8, prim: Primitive }{
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

        for (builtin_types) |b| {
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

        // Path type - filesystem path wrapper around String
        try self.current_scope.define(.{
            .name = "Path",
            .type_ = self.type_builder.pathType(),
            .kind = .type_,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // ====================================================================
        // Register filesystem functions
        // ====================================================================

        // fs_exists(path: string) -> bool
        const fs_exists_fn_type = try self.type_builder.functionType(&.{self.type_builder.stringType()}, self.type_builder.boolType());
        try self.current_scope.define(.{
            .name = "fs_exists",
            .type_ = fs_exists_fn_type,
            .kind = .function,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // fs_is_file(path: string) -> bool
        const fs_is_file_fn_type = try self.type_builder.functionType(&.{self.type_builder.stringType()}, self.type_builder.boolType());
        try self.current_scope.define(.{
            .name = "fs_is_file",
            .type_ = fs_is_file_fn_type,
            .kind = .function,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // fs_is_dir(path: string) -> bool
        const fs_is_dir_fn_type = try self.type_builder.functionType(&.{self.type_builder.stringType()}, self.type_builder.boolType());
        try self.current_scope.define(.{
            .name = "fs_is_dir",
            .type_ = fs_is_dir_fn_type,
            .kind = .function,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // fs_create_dir(path: string) -> Result[void, IoError]
        const fs_create_dir_ret = try self.type_builder.resultType(self.type_builder.voidType(), self.type_builder.ioErrorType());
        const fs_create_dir_fn_type = try self.type_builder.functionType(&.{self.type_builder.stringType()}, fs_create_dir_ret);
        try self.current_scope.define(.{
            .name = "fs_create_dir",
            .type_ = fs_create_dir_fn_type,
            .kind = .function,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // fs_create_dir_all(path: string) -> Result[void, IoError]
        const fs_create_dir_all_fn_type = try self.type_builder.functionType(&.{self.type_builder.stringType()}, fs_create_dir_ret);
        try self.current_scope.define(.{
            .name = "fs_create_dir_all",
            .type_ = fs_create_dir_all_fn_type,
            .kind = .function,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // fs_remove_file(path: string) -> Result[void, IoError]
        const fs_remove_file_fn_type = try self.type_builder.functionType(&.{self.type_builder.stringType()}, fs_create_dir_ret);
        try self.current_scope.define(.{
            .name = "fs_remove_file",
            .type_ = fs_remove_file_fn_type,
            .kind = .function,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // fs_remove_dir(path: string) -> Result[void, IoError]
        const fs_remove_dir_fn_type = try self.type_builder.functionType(&.{self.type_builder.stringType()}, fs_create_dir_ret);
        try self.current_scope.define(.{
            .name = "fs_remove_dir",
            .type_ = fs_remove_dir_fn_type,
            .kind = .function,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // fs_read_string(path: string) -> Result[String, IoError]
        const string_type = try self.type_builder.stringDataType();
        const fs_read_string_ret = try self.type_builder.resultType(string_type, self.type_builder.ioErrorType());
        const fs_read_string_fn_type = try self.type_builder.functionType(&.{self.type_builder.stringType()}, fs_read_string_ret);
        try self.current_scope.define(.{
            .name = "fs_read_string",
            .type_ = fs_read_string_fn_type,
            .kind = .function,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // fs_write_string(path: string, content: string) -> Result[void, IoError]
        const fs_write_string_fn_type = try self.type_builder.functionType(&.{ self.type_builder.stringType(), self.type_builder.stringType() }, fs_create_dir_ret);
        try self.current_scope.define(.{
            .name = "fs_write_string",
            .type_ = fs_write_string_fn_type,
            .kind = .function,
            .mutable = false,
            .span = .{ .start = 0, .end = 0, .line = 0, .column = 0 },
        });

        // fs_read_dir(path: string) -> Result[List[String], IoError]
        const list_string_type = try self.type_builder.listType(string_type);
        const fs_read_dir_ret = try self.type_builder.resultType(list_string_type, self.type_builder.ioErrorType());
        const fs_read_dir_fn_type = try self.type_builder.functionType(&.{self.type_builder.stringType()}, fs_read_dir_ret);
        try self.current_scope.define(.{
            .name = "fs_read_dir",
            .type_ = fs_read_dir_fn_type,
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

    pub fn addError(self: *TypeChecker, kind: CheckError.Kind, span: Span, comptime fmt: []const u8, args: anytype) void {
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

    pub fn pushScope(self: *TypeChecker, kind: Scope.Kind) !*Scope {
        const scope = try self.allocator.create(Scope);
        scope.* = Scope.init(self.allocator, self.current_scope, kind);
        self.current_scope = scope;
        return scope;
    }

    pub fn popScope(self: *TypeChecker) void {
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
    pub fn pushTypeParams(self: *TypeChecker, type_params: []const ast.TypeParam) ![]const types.TypeVar {
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
    pub fn popTypeParams(self: *TypeChecker) void {
        if (self.type_param_scopes.pop()) |*scope| {
            var s = scope.*;
            s.deinit(self.allocator);
        }
    }

    /// Look up a type parameter by name across all active scopes (innermost first).
    pub fn lookupTypeParam(self: *const TypeChecker, name: []const u8) ?types.TypeVar {
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
        // Compute mangled name first for O(1) hash lookup
        const mangled_name = try self.mangleFunctionName(func_name, type_args);

        // O(1) lookup - check if this instantiation already exists
        if (self.monomorphized_functions.get(mangled_name)) |existing| {
            // Already recorded, free the duplicate mangled name and return existing
            self.allocator.free(mangled_name);
            return existing.mangled_name;
        }

        // Look up the original generic function declaration
        const original_decl = self.generic_functions.get(func_name) orelse {
            // This shouldn't happen for valid generic function calls
            self.allocator.free(mangled_name);
            return error.OutOfMemory; // Function not found in generic registry
        };

        // Create new entry
        const type_args_copy = try self.allocator.dupe(Type, type_args);

        try self.monomorphized_functions.put(self.allocator, mangled_name, .{
            .original_name = func_name,
            .mangled_name = mangled_name,
            .type_args = type_args_copy,
            .concrete_type = concrete_type,
            .original_decl = original_decl,
        });

        return mangled_name;
    }

    /// Get iterator over all monomorphized function instances.
    pub fn getMonomorphizedFunctions(self: *const TypeChecker) std.StringHashMapUnmanaged(MonomorphizedFunction).ValueIterator {
        return self.monomorphized_functions.valueIterator();
    }

    /// Get the resolved mangled name for a call expression, if it's a generic call.
    /// Returns null if the call is not to a generic function.
    pub fn getCallResolution(self: *const TypeChecker, call: *ast.Call) ?[]const u8 {
        return self.call_resolutions.get(call);
    }

    /// Get iterator over all monomorphized struct instances.
    pub fn getMonomorphizedStructs(self: *const TypeChecker) std.StringHashMapUnmanaged(MonomorphizedStruct).ValueIterator {
        return self.monomorphized_structs.valueIterator();
    }

    /// Get iterator over all monomorphized enum instances.
    pub fn getMonomorphizedEnums(self: *const TypeChecker) std.StringHashMapUnmanaged(MonomorphizedEnum).ValueIterator {
        return self.monomorphized_enums.valueIterator();
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
        // Generate mangled name first for O(1) hash lookup
        const mangled_name = try self.mangleStructName(struct_name, type_args);

        // O(1) lookup - check if this instantiation already exists
        if (self.monomorphized_structs.get(mangled_name)) |existing| {
            self.allocator.free(mangled_name);
            return existing.concrete_type;
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
                    self.allocator.free(mangled_name);
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

        try self.monomorphized_structs.put(self.allocator, mangled_name, .{
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
        // Generate mangled name first for O(1) hash lookup
        const mangled_name = try self.mangleStructName(enum_name, type_args);

        // O(1) lookup - check if this instantiation already exists
        if (self.monomorphized_enums.get(mangled_name)) |existing| {
            self.allocator.free(mangled_name);
            return existing.concrete_type;
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

        // Allocate and store the concrete enum type
        const concrete_enum = try self.allocator.create(types.EnumType);
        concrete_enum.* = .{
            .name = mangled_name,
            .type_params = &.{}, // No type params - this is a concrete type
            .variants = try self.allocator.dupe(types.EnumVariant, concrete_variants.items),
        };

        const type_args_copy = try self.allocator.dupe(Type, type_args);

        try self.monomorphized_enums.put(self.allocator, mangled_name, .{
            .original_name = enum_name,
            .mangled_name = mangled_name,
            .type_args = type_args_copy,
            .concrete_type = concrete_enum,
        });

        return concrete_enum;
    }

    // ========================================================================
    // Type Resolution (delegated to type_resolution.zig)
    // ========================================================================

    /// Substitute type variables in a type using the given mapping.
    pub fn substituteTypeParams(self: *TypeChecker, typ: Type, substitutions: std.AutoHashMapUnmanaged(u32, Type)) !Type {
        return type_resolution.substituteTypeParams(self, typ, substitutions);
    }

    /// Check if a type contains any type variables.
    pub fn containsTypeVar(self: *TypeChecker, typ: Type) bool {
        return type_resolution.containsTypeVar(self, typ);
    }

    /// Try to unify a type with type variables against a concrete type.
    /// Returns true if unification succeeded, populating the substitutions map.
    pub fn unifyTypes(
        self: *TypeChecker,
        pattern: Type,
        concrete: Type,
        substitutions: *std.AutoHashMapUnmanaged(u32, Type),
    ) !bool {
        return type_resolution.unifyTypes(self, pattern, concrete, substitutions);
    }

    /// Resolve a type expression from the AST to a concrete Type.
    pub fn resolveTypeExpr(self: *TypeChecker, type_expr: ast.TypeExpr) !Type {
        return type_resolution.resolveTypeExpr(self, type_expr);
    }

    // NOTE: resolveTypeExpr implementation moved to type_resolution.zig
    // The old implementation (400+ lines) is now in type_resolution.zig
    // ========================================================================
    // Expression Type Checking (delegated to expressions.zig)
    // ========================================================================

    pub fn checkExpr(self: *TypeChecker, expr: ast.Expr) Type {
        return expressions.checkExpr(self, expr);
    }

    /// Check an expression with an optional type hint for contextual typing.
    /// The hint is used for numeric literals to adopt the expected type.
    pub fn checkExprWithHint(self: *TypeChecker, expr: ast.Expr, hint: ?Type) Type {
        return expressions.checkExprWithHint(self, expr, hint);
    }

    // NOTE: Basic expression checking functions (checkLiteralWithHint, checkIdentifier,
    // checkBinary, checkUnary, checkPostfix) have been moved to expressions.zig.
    // The functions below are called by checkCallImpl and must stay here.

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

    /// Internal implementation of call checking.
    /// Called via expressions.checkCall -> tc.checkCallImpl
    pub fn checkCallImpl(self: *TypeChecker, call: *ast.Call) Type {
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

            // FFI Pointer Functions

            // is_null[T](ptr: COptPtr[T]) -> bool - SAFE, no unsafe required
            if (std.mem.eql(u8, func_name, "is_null")) {
                if (call.args.len != 1) {
                    self.addError(.invalid_call, call.span, "is_null requires exactly 1 argument", .{});
                    return self.type_builder.boolType();
                }
                const arg_type = self.checkExpr(call.args[0]);
                if (arg_type != .copt_ptr) {
                    self.addError(.type_mismatch, call.args[0].span(), "is_null expects COptPtr[T], got {s}", .{types.typeToString(self.allocator, arg_type) catch "unknown"});
                }
                return self.type_builder.boolType();
            }

            // unwrap_ptr[T](ptr: COptPtr[T]) -> CPtr[T] - UNSAFE
            if (std.mem.eql(u8, func_name, "unwrap_ptr")) {
                if (call.args.len != 1) {
                    self.addError(.invalid_call, call.span, "unwrap_ptr requires exactly 1 argument", .{});
                    return self.type_builder.unknownType();
                }
                if (!self.in_unsafe_context) {
                    self.addError(.invalid_call, call.span, "unwrap_ptr is unsafe and requires unsafe block or unsafe fn", .{});
                }
                const arg_type = self.checkExpr(call.args[0]);
                if (arg_type == .copt_ptr) {
                    // Return CPtr[T] where T is the inner type of COptPtr[T]
                    return self.type_builder.cptrType(arg_type.copt_ptr.inner) catch self.type_builder.unknownType();
                }
                self.addError(.type_mismatch, call.args[0].span(), "unwrap_ptr expects COptPtr[T]", .{});
                return self.type_builder.unknownType();
            }

            // offset[T](ptr: CPtr[T], count: isize) -> CPtr[T] - UNSAFE
            if (std.mem.eql(u8, func_name, "offset")) {
                if (call.args.len != 2) {
                    self.addError(.invalid_call, call.span, "offset requires exactly 2 arguments (ptr, count)", .{});
                    return self.type_builder.unknownType();
                }
                if (!self.in_unsafe_context) {
                    self.addError(.invalid_call, call.span, "offset is unsafe and requires unsafe block or unsafe fn", .{});
                }
                const ptr_type = self.checkExpr(call.args[0]);
                const count_type = self.checkExpr(call.args[1]);
                if (ptr_type != .cptr) {
                    self.addError(.type_mismatch, call.args[0].span(), "offset expects CPtr[T] as first argument", .{});
                    return self.type_builder.unknownType();
                }
                // count should be isize
                if (count_type != .primitive or count_type.primitive != .isize_) {
                    self.addError(.type_mismatch, call.args[1].span(), "offset expects isize as second argument", .{});
                }
                return ptr_type; // Returns same CPtr[T]
            }

            // read[T](ptr: CPtr[T]) -> T - UNSAFE
            if (std.mem.eql(u8, func_name, "read")) {
                if (call.args.len != 1) {
                    self.addError(.invalid_call, call.span, "read requires exactly 1 argument", .{});
                    return self.type_builder.unknownType();
                }
                if (!self.in_unsafe_context) {
                    self.addError(.invalid_call, call.span, "read is unsafe and requires unsafe block or unsafe fn", .{});
                }
                const ptr_type = self.checkExpr(call.args[0]);
                if (ptr_type == .cptr) {
                    // Return T, the inner type of CPtr[T]
                    return ptr_type.cptr.inner;
                }
                self.addError(.type_mismatch, call.args[0].span(), "read expects CPtr[T]", .{});
                return self.type_builder.unknownType();
            }

            // write[T](ptr: CPtr[T], value: T) -> void - UNSAFE
            if (std.mem.eql(u8, func_name, "write")) {
                if (call.args.len != 2) {
                    self.addError(.invalid_call, call.span, "write requires exactly 2 arguments (ptr, value)", .{});
                    return self.type_builder.voidType();
                }
                if (!self.in_unsafe_context) {
                    self.addError(.invalid_call, call.span, "write is unsafe and requires unsafe block or unsafe fn", .{});
                }
                const ptr_type = self.checkExpr(call.args[0]);
                const value_type = self.checkExpr(call.args[1]);
                if (ptr_type == .cptr) {
                    // Value type must match the inner type of CPtr[T]
                    if (!ptr_type.cptr.inner.eql(value_type)) {
                        const expected_str = types.typeToString(self.allocator, ptr_type.cptr.inner) catch "unknown";
                        const got_str = types.typeToString(self.allocator, value_type) catch "unknown";
                        self.addError(.type_mismatch, call.args[1].span(), "write: expected {s}, got {s}", .{ expected_str, got_str });
                    }
                } else {
                    self.addError(.type_mismatch, call.args[0].span(), "write expects CPtr[T] as first argument", .{});
                }
                return self.type_builder.voidType();
            }

            // ref_to_ptr[T](value: ref T) -> CPtr[T] - UNSAFE
            if (std.mem.eql(u8, func_name, "ref_to_ptr")) {
                if (call.args.len != 1) {
                    self.addError(.invalid_call, call.span, "ref_to_ptr requires exactly 1 argument", .{});
                    return self.type_builder.unknownType();
                }
                if (!self.in_unsafe_context) {
                    self.addError(.invalid_call, call.span, "ref_to_ptr is unsafe and requires unsafe block or unsafe fn", .{});
                }
                const arg_type = self.checkExpr(call.args[0]);
                if (arg_type == .reference) {
                    // Return CPtr[T] where T is the inner type of ref T
                    return self.type_builder.cptrType(arg_type.reference.inner) catch self.type_builder.unknownType();
                }
                self.addError(.type_mismatch, call.args[0].span(), "ref_to_ptr expects a reference type", .{});
                return self.type_builder.unknownType();
            }

            // ptr_cast[U](ptr: CPtr[T]) -> CPtr[U] - UNSAFE
            // Cast between pointer types. Requires explicit type argument.
            if (std.mem.eql(u8, func_name, "ptr_cast")) {
                if (call.args.len != 1) {
                    self.addError(.invalid_call, call.span, "ptr_cast requires exactly 1 argument", .{});
                    return self.type_builder.unknownType();
                }
                if (!self.in_unsafe_context) {
                    self.addError(.invalid_call, call.span, "ptr_cast is unsafe and requires unsafe block or unsafe fn", .{});
                }

                // Require explicit type argument: ptr_cast[TargetType](ptr)
                if (call.type_args == null or call.type_args.?.len != 1) {
                    self.addError(.invalid_call, call.span, "ptr_cast requires exactly 1 type argument: ptr_cast[TargetType](ptr)", .{});
                    return self.type_builder.unknownType();
                }

                const arg_type = self.checkExpr(call.args[0]);

                // Argument must be CPtr[T] or COptPtr[T]
                if (arg_type != .cptr and arg_type != .copt_ptr) {
                    self.addError(.type_mismatch, call.args[0].span(), "ptr_cast expects CPtr[T] or COptPtr[T]", .{});
                    return self.type_builder.unknownType();
                }

                // Resolve the target type from the type argument
                const target_type = self.resolveType(call.type_args.?[0]);

                // Return the appropriate pointer type with the new inner type
                if (arg_type == .cptr) {
                    return self.type_builder.cptrType(target_type) catch self.type_builder.unknownType();
                } else {
                    return self.type_builder.coptPtrType(target_type) catch self.type_builder.unknownType();
                }
            }

        }

        const callee_type = self.checkExpr(call.callee);

        // Handle extern function pointer calls
        if (callee_type == .extern_fn) {
            return self.checkExternFnCall(call, callee_type.extern_fn);
        }

        if (callee_type != .function) {
            self.addError(.invalid_call, call.span, "cannot call non-function type", .{});
            return self.type_builder.unknownType();
        }

        const func = callee_type.function;

        // Check if calling an unsafe function from a safe context
        if (func.is_unsafe and !self.in_unsafe_context) {
            self.addError(.invalid_call, call.span, "call to unsafe function requires unsafe block or unsafe fn", .{});
        }

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
            for (call.args, func.params, 0..) |arg, param_type, i| {
                // Validate out parameter/argument matching (generic functions don't typically have out params, but check anyway)
                const is_out_param = (i < 64) and ((func.out_params & (@as(u64, 1) << @intCast(i))) != 0);
                const is_out_arg = (arg == .out_arg);
                if (is_out_param and !is_out_arg) {
                    self.addError(.type_mismatch, arg.span(), "out parameter requires 'out' keyword at call site", .{});
                } else if (!is_out_param and is_out_arg) {
                    self.addError(.type_mismatch, arg.span(), "'out' keyword used for non-out parameter", .{});
                }

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
            for (call.args, func.params, 0..) |arg, param_type, i| {
                const is_out_param = (i < 64) and ((func.out_params & (@as(u64, 1) << @intCast(i))) != 0);
                const is_out_arg = (arg == .out_arg);

                // Validate out parameter/argument matching
                if (is_out_param and !is_out_arg) {
                    self.addError(.type_mismatch, arg.span(), "out parameter requires 'out' keyword at call site", .{});
                } else if (!is_out_param and is_out_arg) {
                    self.addError(.type_mismatch, arg.span(), "'out' keyword used for non-out parameter", .{});
                }

                const arg_type = self.checkExpr(arg);
                if (!arg_type.eql(param_type)) {
                    self.addError(.type_mismatch, arg.span(), "argument type mismatch", .{});
                }
            }
            return func.return_type;
        }
    }

    /// Type check a call to an extern function pointer (C function pointer).
    /// Extern function pointer calls always require unsafe context.
    fn checkExternFnCall(self: *TypeChecker, call: *ast.Call, extern_fn: *const types.ExternFnType) Type {
        // Calling through a C function pointer is unsafe (raw pointer dereference)
        if (!self.in_unsafe_context) {
            self.addError(.invalid_call, call.span, "call to extern fn pointer requires unsafe block or unsafe fn", .{});
        }

        // Check argument count
        if (call.args.len != extern_fn.params.len) {
            self.addError(.invalid_call, call.span, "expected {d} arguments, got {d}", .{ extern_fn.params.len, call.args.len });
            return extern_fn.return_type;
        }

        // Check argument types
        for (call.args, extern_fn.params) |arg, param_type| {
            const arg_type = self.checkExpr(arg);
            if (!arg_type.eql(param_type)) {
                const expected_str = types.typeToString(self.allocator, param_type) catch "unknown";
                const got_str = types.typeToString(self.allocator, arg_type) catch "unknown";
                self.addError(.type_mismatch, arg.span(), "expected {s}, got {s}", .{ expected_str, got_str });
            }
        }

        return extern_fn.return_type;
    }

    // NOTE: checkIndex and checkField have been moved to expressions.zig.

    /// Internal implementation of method call checking.
    /// Called via expressions.checkMethodCall -> tc.checkMethodCallImpl
    pub fn checkMethodCallImpl(self: *TypeChecker, method: *ast.MethodCall) Type {
        // Handle static constructors (Rc.new, List.new, Map.new, etc.)
        if (method_calls.checkStaticConstructor(self, method)) |result| {
            return result;
        }

        const object_type = self.checkExpr(method.object);

        // Check for builtin methods (len, clone, push, pop, etc.)
        if (method_calls.checkBuiltinMethod(self, method, object_type)) |result| {
            return result;
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
                    var mono_iter = self.monomorphized_structs.valueIterator();
                    while (mono_iter.next()) |mono| {
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

                    var enum_iter = self.monomorphized_enums.valueIterator();
                    while (enum_iter.next()) |mono| {
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
        return methods.lookupStructMethod(self, struct_name, method_name);
    }

    /// Record a monomorphized method instance for a generic struct.
    pub fn recordMethodMonomorphization(self: *TypeChecker, struct_type: *types.StructType, method: StructMethod) !void {
        return methods.recordMethodMonomorphization(self, struct_type, method);
    }

    /// Record a trait method call through a generic bound.
    /// This tracks when a trait method is called on a type variable (e.g., T.method())
    /// so that codegen can emit the correct concrete method call for each monomorphization.
    fn recordTraitMethodCall(self: *TypeChecker, type_var_name: []const u8, trait_name: []const u8, method_name: []const u8) !void {
        return methods.recordTraitMethodCall(self, type_var_name, trait_name, method_name);
    }

    fn checkIfStmt(self: *TypeChecker, if_stmt: *ast.IfStmt) void {
        statements.checkIfStmt(self, if_stmt);
    }

    fn checkMatchStmt(self: *TypeChecker, match_stmt: *ast.MatchStmt) void {
        statements.checkMatchStmt(self, match_stmt);
    }

    pub fn checkBlock(self: *TypeChecker, block: *ast.Block) Type {
        return expressions.checkBlock(self, block);
    }

    // NOTE: Expression checking helper functions (checkClosure, checkRange,
    // checkStructLiteral, checkArrayLiteral, checkTupleLiteral, checkTypeCast,
    // checkEnumLiteral, checkComptimeBlock, checkUnsafeBlock, checkOutArg)
    // have been moved to expressions.zig.

    // ========================================================================
    // Compile-Time Evaluation (delegated to comptime_eval.zig)
    // ========================================================================

    /// Evaluate an expression at compile time to get an interpreter Value.
    /// Returns null if the expression is not comptime-known.
    pub fn evaluateComptimeExpr(self: *TypeChecker, expr: ast.Expr) ?Value {
        return comptime_eval.evaluateComptimeExpr(self, expr);
    }

    /// Evaluate a call to a comptime function at compile time.
    fn evaluateComptimeCall(self: *TypeChecker, call: *ast.Call, func_decl: *ast.FunctionDecl) ?ComptimeValue {
        return comptime_eval.evaluateComptimeCall(self, call, func_decl);
    }

    /// Evaluate a comptime function call via @name(...) builtin syntax.
    pub fn evaluateComptimeCallBuiltin(self: *TypeChecker, bc: *ast.BuiltinCall, func_decl: *ast.FunctionDecl) ?ComptimeValue {
        return comptime_eval.evaluateComptimeCallBuiltin(self, bc, func_decl);
    }

    /// Populate interpreter environment with constants from the current scope.
    fn populateInterpreterEnv(self: *TypeChecker, interp: *Interpreter) void {
        comptime_eval.populateInterpreterEnv(self, interp);
    }

    /// Convert an interpreter Value to a ComptimeValue.
    pub fn valueToComptimeValue(self: *TypeChecker, value: Value, span: ast.Span) ?ComptimeValue {
        return comptime_eval.valueToComptimeValue(self, value, span);
    }

    fn checkBuiltinCall(self: *TypeChecker, builtin: *ast.BuiltinCall) Type {
        return builtins_check.checkBuiltinCall(self, builtin);
    }

    fn checkComptimeFunctionCallFromBuiltin(self: *TypeChecker, builtin: *ast.BuiltinCall, func_decl: *ast.FunctionDecl) Type {
        _ = self;
        _ = builtin;
        _ = func_decl;
        // Delegated to builtins_check via checkBuiltinCall
        unreachable;
    }

    fn checkFunctionDeclReturnType(self: *TypeChecker, func_decl: *ast.FunctionDecl) Type {
        return builtins_check.checkFunctionDeclReturnType(self, func_decl);
    }


    // ========================================================================
    // Statement Type Checking
    // ========================================================================

    pub fn checkStmt(self: *TypeChecker, stmt: ast.Stmt) void {
        statements.checkStmt(self, stmt);
    }

    fn checkAssignment(self: *TypeChecker, assign: *ast.Assignment) void {
        statements.checkAssignment(self, assign);
    }

    fn checkLetDecl(self: *TypeChecker, decl: *ast.LetDecl) void {
        statements.checkLetDecl(self, decl);
    }

    fn checkVarDecl(self: *TypeChecker, decl: *ast.VarDecl) void {
        statements.checkVarDecl(self, decl);
    }

    fn checkReturn(self: *TypeChecker, ret: *ast.ReturnStmt) void {
        statements.checkReturn(self, ret);
    }

    fn checkBreak(self: *TypeChecker, brk: *ast.BreakStmt) void {
        statements.checkBreak(self, brk);
    }

    fn checkContinue(self: *TypeChecker, cont: *ast.ContinueStmt) void {
        statements.checkContinue(self, cont);
    }

    fn checkFor(self: *TypeChecker, for_loop: *ast.ForLoop) void {
        statements.checkFor(self, for_loop);
    }

    fn checkWhile(self: *TypeChecker, while_loop: *ast.WhileLoop) void {
        statements.checkWhile(self, while_loop);
    }

    fn checkLoop(self: *TypeChecker, loop: *ast.LoopStmt) void {
        statements.checkLoop(self, loop);
    }

    // ========================================================================
    // Declaration Type Checking
    // ========================================================================

    pub fn checkDecl(self: *TypeChecker, decl: ast.Decl) void {
        declarations.checkDecl(self, decl);
    }


    // ========================================================================
    // Trait Checking (delegated to trait_checking.zig)
    // ========================================================================

    /// Create a unique key for trait implementation lookup
    pub fn makeTraitImplKey(self: *TypeChecker, type_name: []const u8, trait_name: []const u8) []const u8 {
        return trait_checking.makeTraitImplKey(self, type_name, trait_name);
    }

    /// Create a key for generic trait implementations like From[IoError].
    pub fn makeGenericTraitImplKey(self: *TypeChecker, type_name: []const u8, trait_name: []const u8, type_args: []const Type) []const u8 {
        return trait_checking.makeGenericTraitImplKey(self, type_name, trait_name, type_args);
    }

    /// Check if a type implements a trait
    pub fn typeImplementsTrait(self: *TypeChecker, type_name: []const u8, trait_name: []const u8) bool {
        return trait_checking.typeImplementsTrait(self, type_name, trait_name);
    }

    /// Check if target_type implements From[source_type].
    pub fn hasFromImpl(self: *TypeChecker, target_type: Type, source_type: Type) bool {
        return trait_checking.hasFromImpl(self, target_type, source_type);
    }

    /// Get the name of a type for trait implementation lookup.
    pub fn getTypeName(self: *TypeChecker, t: Type) ?[]const u8 {
        return trait_checking.getTypeName(self, t);
    }

    /// Check if a type implements both Hash and Eq traits (required for Map keys).
    pub fn typeImplementsHashAndEq(self: *TypeChecker, key_type: Type, span: Span) bool {
        return trait_checking.typeImplementsHashAndEq(self, key_type, span);
    }

    // ========================================================================
    // Method Utilities (delegated to methods.zig)
    // ========================================================================

    /// Verify that an impl method signature matches the trait method signature.
    pub fn verifyMethodSignature(
        self: *TypeChecker,
        impl_method: StructMethod,
        trait_method: types.TraitMethod,
        impl_type: Type,
        trait_type_params: []const types.TypeVar,
        trait_type_args: []const Type,
        span: Span,
    ) bool {
        return methods.verifyMethodSignature(self, impl_method, trait_method, impl_type, trait_type_params, trait_type_args, span);
    }

    /// Check if a concrete type satisfies all trait bounds.
    pub fn typeSatisfiesBounds(self: *TypeChecker, concrete_type: Type, bounds: []const *types.TraitType, span: Span) bool {
        return trait_checking.typeSatisfiesBounds(self, concrete_type, bounds, span);
    }


    // ========================================================================
    // Pattern Checking
    // ========================================================================

    pub fn checkPattern(self: *TypeChecker, pattern: ast.Pattern, expected_type: Type) void {
        patterns.checkPattern(self, pattern, expected_type);
    }

    fn checkLiteralPattern(self: *TypeChecker, lit: ast.PatternLiteral) Type {
        return patterns.checkLiteralPattern(self, lit);
    }

    pub fn bindPattern(self: *TypeChecker, pattern: ast.Pattern, t: Type) void {
        patterns.bindPattern(self, pattern, t);
    }

    // ========================================================================
    // Helper Functions
    // ========================================================================

    // ========================================================================
    // Type Utilities (delegated to type_utils.zig)
    // ========================================================================

    pub fn isBoolType(self: *TypeChecker, t: Type) bool {
        return type_utils.isBoolType(self, t);
    }

    /// Check if a type is FFI-compatible (can be used in extern structs and extern functions).
    pub fn isFfiCompatibleType(self: *TypeChecker, t: Type) bool {
        return type_utils.isFfiCompatibleType(self, t);
    }

    /// Check if a type is an unsized extern type (opaque type that must be behind a pointer).
    pub fn isUnsizedExternType(self: *TypeChecker, t: Type) bool {
        return type_utils.isUnsizedExternType(self, t);
    }

    /// Check if a primitive type is an integer type (suitable for extern enum repr).
    pub fn isIntegerPrimitive(self: *TypeChecker, prim: types.Primitive) bool {
        return type_utils.isIntegerPrimitive(self, prim);
    }

    /// Check if an i128 value fits within the range of a primitive integer type.
    pub fn valueInRange(self: *TypeChecker, value: i128, prim: types.Primitive) bool {
        return type_utils.valueInRange(self, value, prim);
    }

    pub fn isAssignable(self: *TypeChecker, expr: ast.Expr) bool {
        return type_utils.isAssignable(self, expr);
    }

    pub fn isMutable(self: *TypeChecker, expr: ast.Expr) bool {
        return type_utils.isMutable(self, expr);
    }

    pub fn checkAssignmentCompatible(self: *TypeChecker, target: Type, value: Type) bool {
        return type_utils.checkAssignmentCompatible(self, target, value);
    }

    /// Check if a value type is compatible with a target type.
    pub fn isTypeCompatible(self: *TypeChecker, target: Type, value: Type) bool {
        return type_utils.isTypeCompatible(self, target, value);
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
                            // Look up the struct type from scope (registered by checkStruct)
                            const type_info = self.current_scope.lookup(s.name);
                            const sym = ModuleSymbol{
                                .name = s.name,
                                .kind = .struct_type,
                                .type_ = if (type_info) |info| info.type_ else null,
                                .is_pub = true,
                            };
                            try symbols.symbols.put(self.allocator, s.name, sym);
                        }
                    },
                    .enum_decl => |e| {
                        if (e.is_pub) {
                            // Look up the enum type from scope (registered by checkEnum)
                            const type_info = self.current_scope.lookup(e.name);
                            const sym = ModuleSymbol{
                                .name = e.name,
                                .kind = .enum_type,
                                .type_ = if (type_info) |info| info.type_ else null,
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
                            // Look up the aliased type from scope (registered by checkTypeAlias)
                            const type_info = self.current_scope.lookup(t.name);
                            const sym = ModuleSymbol{
                                .name = t.name,
                                .kind = .type_alias,
                                .type_ = if (type_info) |info| info.type_ else null,
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
                .struct_decl, .enum_decl, .trait_decl, .type_alias, .extern_type_decl => self.checkDecl(decl),
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

                    const func_type = self.type_builder.functionTypeWithFlags(param_types.items, return_type, comptime_params, f.is_unsafe) catch continue;

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
                .extern_block => self.checkDecl(decl),
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

                        // For unsafe functions, the entire body is an unsafe context
                        const was_unsafe = self.in_unsafe_context;
                        if (f.is_unsafe) {
                            self.in_unsafe_context = true;
                        }
                        defer self.in_unsafe_context = was_unsafe;

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
