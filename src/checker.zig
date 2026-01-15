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

    fn checkIdentifier(self: *TypeChecker, id: ast.Identifier) Type {
        if (self.current_scope.lookup(id.name)) |sym| {
            return sym.type_;
        }
        self.addError(.undefined_variable, id.span, "undefined variable '{s}'", .{id.name});
        return self.type_builder.unknownType();
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
                if (operand_type != .reference) {
                    self.addError(.invalid_operation, un.span, "cannot dereference non-reference type", .{});
                    return self.type_builder.unknownType();
                }
                return operand_type.reference.inner;
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

        // Check argument types
        for (call.args, func.params) |arg, param| {
            const arg_type = self.checkExpr(arg);
            if (!arg_type.eql(param)) {
                self.addError(.type_mismatch, arg.span(), "argument type mismatch", .{});
            }
        }

        return func.return_type;
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
        _ = self.pushScope(.function) catch return self.type_builder.unknownType();
        defer self.popScope();

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

        switch (cast.cast_kind) {
            .as => {
                // Safe widening conversion
                if (expr_type == .primitive and target_type == .primitive) {
                    if (!expr_type.primitive.canWidenTo(target_type.primitive)) {
                        self.addError(.invalid_conversion, cast.span, "'.as' requires widening conversion", .{});
                    }
                }
            },
            .to => {
                // Checked narrowing conversion (may trap)
                if (!expr_type.isNumeric() or !target_type.isNumeric()) {
                    self.addError(.invalid_conversion, cast.span, "'.to' requires numeric types", .{});
                }
            },
            .trunc => {
                // Truncating conversion (no trap)
                if (!expr_type.isInteger() or !target_type.isInteger()) {
                    self.addError(.invalid_conversion, cast.span, "'.trunc' requires integer types", .{});
                }
            },
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
                    self.addError(.return_type_mismatch, ret.span, "return type mismatch", .{});
                }
            }
        } else {
            if (self.current_return_type) |expected| {
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
            .type_params = &.{},
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
            .type_params = &.{},
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
