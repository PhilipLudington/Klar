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

    const CaptureInfo = struct {
        is_mutable: bool,
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
        };
        checker.initBuiltins() catch {};
        return checker;
    }

    pub fn deinit(self: *TypeChecker) void {
        self.type_builder.deinit();
        self.type_env.deinit();
        self.errors.deinit(self.allocator);
        self.global_scope.deinit();
        self.allocator.destroy(self.global_scope);
        self.closure_captures.deinit(self.allocator);
        // Clean up type parameter scopes
        for (self.type_param_scopes.items) |*scope| {
            scope.deinit(self.allocator);
        }
        self.type_param_scopes.deinit(self.allocator);
        self.monomorphized_functions.deinit(self.allocator);
        self.generic_functions.deinit(self.allocator);
        self.call_resolutions.deinit(self.allocator);
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
            const type_var = try self.type_builder.newTypeVar(param.name);
            try scope.put(self.allocator, param.name, type_var);
            try type_vars.append(self.allocator, type_var);
        }

        try self.type_param_scopes.append(self.allocator, scope);
        return try self.allocator.dupe(types.TypeVar, type_vars.items);
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
                return self.type_builder.tupleType(try self.allocator.dupe(Type, new_elements.items));
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
                return self.type_builder.functionType(
                    try self.allocator.dupe(Type, new_params.items),
                    new_return,
                );
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
                return self.type_builder.appliedType(new_base, try self.allocator.dupe(Type, new_args.items));
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
                // Look up user-defined type
                if (self.current_scope.lookup(n.name)) |sym| {
                    if (sym.kind == .type_) {
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

        // TODO: look up method on type's impl or trait
        self.addError(.undefined_method, method.span, "method '{s}' not found", .{method.method_name});
        return self.type_builder.unknownType();
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

        self.current_scope.define(.{
            .name = enum_decl.name,
            .type_ = .{ .enum_ = enum_type },
            .kind = .type_,
            .mutable = false,
            .span = enum_decl.span,
        }) catch {};
    }

    fn checkTrait(self: *TypeChecker, trait_decl: *ast.TraitDecl) void {
        // TODO: implement trait checking
        _ = self;
        _ = trait_decl;
    }

    fn checkImpl(self: *TypeChecker, impl_decl: *ast.ImplDecl) void {
        // TODO: implement impl checking
        _ = self;
        _ = impl_decl;
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
                if (expected_type != .enum_) {
                    self.addError(.invalid_pattern, v.span, "variant pattern requires enum type", .{});
                    return;
                }
                // TODO: check variant exists and payload matches
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
            .binding => |bind| {
                self.current_scope.define(.{
                    .name = bind.name,
                    .type_ = t,
                    .kind = .variable,
                    .mutable = bind.mutable,
                    .span = bind.span,
                }) catch {};
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
            else => {},
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
