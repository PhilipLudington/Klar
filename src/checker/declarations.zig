//! Declaration type checking.
//!
//! This module contains functions for type-checking Klar declarations:
//! functions, structs, enums, traits, impls, type aliases, and constants.

const std = @import("std");
const ast = @import("../ast.zig");
const types = @import("../types.zig");
const Type = types.Type;

// Import type definitions from checker module
const traits_mod = @import("traits.zig");
const TraitInfo = traits_mod.TraitInfo;
const AssociatedTypeBinding = traits_mod.AssociatedTypeBinding;
const StructMethod = traits_mod.StructMethod;

// ============================================================================
// Declaration Checking
// ============================================================================

pub fn checkDecl(tc: anytype, decl: ast.Decl) void {
    switch (decl) {
        .function => |f| checkFunction(tc, f),
        .struct_decl => |s| checkStruct(tc, s),
        .enum_decl => |e| checkEnum(tc, e),
        .trait_decl => |t| checkTrait(tc, t),
        .impl_decl => |i| checkImpl(tc, i),
        .type_alias => |t| checkTypeAlias(tc, t),
        .const_decl => |c| checkConst(tc, c),
        .import_decl => {}, // Imports handled separately
        .module_decl => {}, // Module declarations don't need type checking
        .extern_type_decl => |e| checkExternType(tc, e),
        .extern_block => |b| checkExternBlock(tc, b),
    }
}

fn checkFunction(tc: anytype, func: *ast.FunctionDecl) void {
    // Push type parameters into scope if this is a generic function
    const has_type_params = func.type_params.len > 0;
    if (has_type_params) {
        _ = tc.pushTypeParams(func.type_params) catch return;
    }
    defer if (has_type_params) tc.popTypeParams();

    // Build function type
    var param_types: std.ArrayListUnmanaged(Type) = .{};
    defer param_types.deinit(tc.allocator);

    // Build comptime parameter bitmask
    var comptime_params: u64 = 0;
    for (func.params, 0..) |param, i| {
        const param_type = tc.resolveTypeExpr(param.type_) catch tc.type_builder.unknownType();
        param_types.append(tc.allocator, param_type) catch {};
        if (param.is_comptime and i < 64) {
            comptime_params |= @as(u64, 1) << @intCast(i);
        }
    }

    const return_type = if (func.return_type) |rt|
        tc.resolveTypeExpr(rt) catch tc.type_builder.unknownType()
    else
        tc.type_builder.voidType();

    const func_type = tc.type_builder.functionTypeWithFlags(param_types.items, return_type, comptime_params, func.is_unsafe) catch {
        return;
    };

    // Validate main() signature
    if (std.mem.eql(u8, func.name, "main")) {
        // main() accepts at most one parameter
        if (func.params.len > 1) {
            tc.addError(.invalid_operation, func.span, "main() accepts at most one parameter (args: [String])", .{});
        }
        // If one parameter, must be [String]
        if (func.params.len == 1) {
            const param_type = param_types.items[0];
            // Check for slice of String (string_data type, not primitive string_)
            const is_string_slice = param_type == .slice and
                param_type.slice.element == .string_data;
            if (!is_string_slice) {
                const type_str = types.typeToString(tc.allocator, param_type) catch "unknown";
                defer tc.allocator.free(type_str);
                tc.addError(.type_mismatch, func.params[0].span, "main() parameter must be [String], found {s}", .{type_str});
            }
        }
        // Return type must be i32 or void
        const is_i32 = return_type == .primitive and return_type.primitive == .i32_;
        const is_void = return_type == .void_;
        if (!is_i32 and !is_void) {
            const error_span = if (func.return_type) |rt| rt.span() else func.span;
            tc.addError(.type_mismatch, error_span, "main() must return i32 or void", .{});
        }
    }

    // Register function in current scope
    tc.current_scope.define(.{
        .name = func.name,
        .type_ = func_type,
        .kind = .function,
        .mutable = false,
        .span = func.span,
    }) catch {};

    // Register comptime functions for compile-time evaluation
    if (func.is_comptime) {
        tc.comptime_functions.put(tc.allocator, func.name, func) catch {};
    }

    // Check function body if present
    if (func.body) |body| {
        _ = tc.pushScope(.function) catch return;
        defer tc.popScope();

        // Bind parameters
        for (func.params) |param| {
            const param_type = tc.resolveTypeExpr(param.type_) catch tc.type_builder.unknownType();
            tc.current_scope.define(.{
                .name = param.name,
                .type_ = param_type,
                .kind = .parameter,
                .mutable = false,
                .span = param.span,
            }) catch {};
        }

        tc.current_return_type = return_type;
        defer tc.current_return_type = null;

        // For comptime functions, set the flag so nested comptime calls
        // are type-checked but not evaluated during this phase
        const was_checking_comptime_body = tc.checking_comptime_function_body;
        if (func.is_comptime) {
            tc.checking_comptime_function_body = true;
        }
        defer tc.checking_comptime_function_body = was_checking_comptime_body;

        // For unsafe functions, the entire body is an unsafe context
        const was_unsafe = tc.in_unsafe_context;
        if (func.is_unsafe) {
            tc.in_unsafe_context = true;
        }
        defer tc.in_unsafe_context = was_unsafe;

        _ = tc.checkBlock(body);
    }
}

fn checkStruct(tc: anytype, struct_decl: *ast.StructDecl) void {
    // Push type parameters into scope if this is a generic struct
    const has_type_params = struct_decl.type_params.len > 0;
    var struct_type_params: []const types.TypeVar = &.{};
    if (has_type_params) {
        struct_type_params = tc.pushTypeParams(struct_decl.type_params) catch return;
    }
    defer if (has_type_params) tc.popTypeParams();

    var fields: std.ArrayListUnmanaged(types.StructField) = .{};
    defer fields.deinit(tc.allocator);

    for (struct_decl.fields) |field| {
        const field_type = tc.resolveTypeExpr(field.type_) catch tc.type_builder.unknownType();

        // Validate FFI-compatible types for extern structs
        if (struct_decl.is_extern and !tc.isFfiCompatibleType(field_type)) {
            if (tc.isUnsizedExternType(field_type)) {
                tc.addError(.type_mismatch, field.span, "unsized extern type cannot be used in struct field; use CPtr[T] or COptPtr[T] instead", .{});
            } else {
                tc.addError(.type_mismatch, field.span, "extern struct fields must be FFI-compatible types (primitives, CPtr, COptPtr, CStr, or other extern types)", .{});
            }
        }

        fields.append(tc.allocator, .{
            .name = field.name,
            .type_ = field_type,
            .is_pub = field.is_pub,
        }) catch {};
    }

    // Create struct type
    const struct_type = tc.allocator.create(types.StructType) catch return;
    struct_type.* = .{
        .name = struct_decl.name,
        .type_params = struct_type_params,
        .fields = fields.toOwnedSlice(tc.allocator) catch &.{},
        .traits = &.{},
        .is_extern = struct_decl.is_extern,
        .is_packed = struct_decl.is_packed,
    };

    // Track for cleanup in deinit
    tc.generic_struct_types.append(tc.allocator, struct_type) catch {};

    tc.current_scope.define(.{
        .name = struct_decl.name,
        .type_ = .{ .struct_ = struct_type },
        .kind = .type_,
        .mutable = false,
        .span = struct_decl.span,
    }) catch {};
}

fn checkExternType(tc: anytype, extern_decl: *ast.ExternTypeDecl) void {
    // Create the extern type
    const extern_type = tc.allocator.create(types.ExternType) catch return;
    extern_type.* = .{
        .name = extern_decl.name,
        .size = extern_decl.size,
    };

    // Track for cleanup
    tc.extern_types.append(tc.allocator, extern_type) catch {};

    // Register in current scope
    tc.current_scope.define(.{
        .name = extern_decl.name,
        .type_ = .{ .extern_type = extern_type },
        .kind = .type_,
        .mutable = false,
        .span = extern_decl.span,
    }) catch {};
}

fn checkExternBlock(tc: anytype, block: *ast.ExternBlock) void {
    // Check each extern function in the block
    for (block.functions) |func| {
        checkExternFunction(tc, func);
    }
}

fn checkExternFunction(tc: anytype, func: *ast.FunctionDecl) void {
    // Build function type from parameters
    var param_types: std.ArrayListUnmanaged(Type) = .{};
    defer param_types.deinit(tc.allocator);

    for (func.params) |param| {
        const param_type = tc.resolveTypeExpr(param.type_) catch tc.type_builder.unknownType();
        param_types.append(tc.allocator, param_type) catch {};

        // Validate parameter type is FFI-compatible
        if (!tc.isFfiCompatibleType(param_type)) {
            if (tc.isUnsizedExternType(param_type)) {
                tc.addError(.type_mismatch, param.span, "unsized extern type cannot be passed by value; use CPtr[T] or COptPtr[T] instead", .{});
            } else {
                tc.addError(.type_mismatch, param.span, "extern function parameters must be FFI-compatible types", .{});
            }
        }
        // Note: out parameter info (is_out) is preserved in AST for codegen
    }

    // Resolve return type
    const return_type = if (func.return_type) |rt|
        tc.resolveTypeExpr(rt) catch tc.type_builder.unknownType()
    else
        tc.type_builder.voidType();

    // Validate return type is FFI-compatible (if not void)
    if (return_type != .void_ and !tc.isFfiCompatibleType(return_type)) {
        const error_span = if (func.return_type) |rt| rt.span() else func.span;
        if (tc.isUnsizedExternType(return_type)) {
            tc.addError(.type_mismatch, error_span, "unsized extern type cannot be returned by value; use CPtr[T] or COptPtr[T] instead", .{});
        } else {
            tc.addError(.type_mismatch, error_span, "extern function return type must be FFI-compatible", .{});
        }
    }

    // Create extern function type using type builder
    const extern_fn_type = tc.type_builder.externFnType(param_types.items, return_type) catch return;

    // Register in current scope
    tc.current_scope.define(.{
        .name = func.name,
        .type_ = extern_fn_type,
        .kind = .function,
        .mutable = false,
        .span = func.span,
    }) catch {};
}

fn checkEnum(tc: anytype, enum_decl: *ast.EnumDecl) void {
    // Push type parameters into scope if this is a generic enum
    const has_type_params = enum_decl.type_params.len > 0;
    var enum_type_params: []const types.TypeVar = &.{};
    if (has_type_params) {
        enum_type_params = tc.pushTypeParams(enum_decl.type_params) catch return;
    }
    defer if (has_type_params) tc.popTypeParams();

    // For extern enums, resolve and validate repr type
    var resolved_repr_type: ?Type = null;
    if (enum_decl.is_extern) {
        if (enum_decl.repr_type) |repr_expr| {
            const repr = tc.resolveTypeExpr(repr_expr) catch {
                tc.addError(.undefined_type, enum_decl.span, "cannot resolve repr type for extern enum", .{});
                return;
            };
            // Validate repr type is an integer primitive
            if (repr != .primitive or !tc.isIntegerPrimitive(repr.primitive)) {
                tc.addError(.type_mismatch, enum_decl.span, "extern enum repr type must be an integer type (i8, i16, i32, i64, u8, u16, u32, u64, isize, usize)", .{});
                return;
            }
            resolved_repr_type = repr;
        }
    }

    // Pre-register enum type BEFORE resolving variants to allow recursive types.
    // This enables patterns like: enum JsonValue { Array(List[JsonValue]) }
    const enum_type = tc.allocator.create(types.EnumType) catch return;
    enum_type.* = .{
        .name = enum_decl.name,
        .type_params = enum_type_params,
        .variants = &.{}, // Will be filled in below
        .is_extern = enum_decl.is_extern,
        .repr_type = resolved_repr_type,
    };

    // Track for cleanup in deinit
    tc.generic_enum_types.append(tc.allocator, enum_type) catch {};

    // Register in scope FIRST so recursive references resolve
    tc.current_scope.define(.{
        .name = enum_decl.name,
        .type_ = .{ .enum_ = enum_type },
        .kind = .type_,
        .mutable = false,
        .span = enum_decl.span,
    }) catch {};

    // NOW resolve variant types (enum name is in scope for self-references)
    var variants: std.ArrayListUnmanaged(types.EnumVariant) = .{};
    defer variants.deinit(tc.allocator);

    for (enum_decl.variants) |variant| {
        // For extern enums, validate variant constraints
        if (enum_decl.is_extern) {
            // Payload should already be forbidden by parser, but double-check
            if (variant.payload != null) {
                tc.addError(.invalid_operation, variant.span, "extern enum variants cannot have payloads", .{});
                continue;
            }
            // Explicit value should be required by parser, but double-check
            if (variant.value == null) {
                tc.addError(.invalid_operation, variant.span, "extern enum variants must have explicit values", .{});
                continue;
            }
            // Validate value fits in repr type
            if (resolved_repr_type) |repr| {
                if (!tc.valueInRange(variant.value.?, repr.primitive)) {
                    tc.addError(.type_mismatch, variant.span, "extern enum variant value '{d}' does not fit in repr type", .{variant.value.?});
                    continue;
                }
            }
        }

        const payload: ?types.VariantPayload = if (variant.payload) |p| blk: {
            break :blk switch (p) {
                .tuple => |tuple_types| tup: {
                    var resolved_types: std.ArrayListUnmanaged(Type) = .{};
                    defer resolved_types.deinit(tc.allocator);
                    for (tuple_types) |t| {
                        resolved_types.append(tc.allocator, tc.resolveTypeExpr(t) catch tc.type_builder.unknownType()) catch {};
                    }
                    break :tup .{ .tuple = resolved_types.toOwnedSlice(tc.allocator) catch &.{} };
                },
                .struct_ => |struct_fields| str: {
                    var resolved_fields: std.ArrayListUnmanaged(types.StructField) = .{};
                    defer resolved_fields.deinit(tc.allocator);
                    for (struct_fields) |f| {
                        resolved_fields.append(tc.allocator, .{
                            .name = f.name,
                            .type_ = tc.resolveTypeExpr(f.type_) catch tc.type_builder.unknownType(),
                            .is_pub = f.is_pub,
                        }) catch {};
                    }
                    break :str .{ .struct_ = resolved_fields.toOwnedSlice(tc.allocator) catch &.{} };
                },
            };
        } else null;

        variants.append(tc.allocator, .{
            .name = variant.name,
            .payload = payload,
            .value = variant.value,
        }) catch {};
    }

    // Update the enum type with resolved variants
    enum_type.variants = variants.toOwnedSlice(tc.allocator) catch &.{};
}

fn checkTrait(tc: anytype, trait_decl: *ast.TraitDecl) void {
    // Check for duplicate trait definition
    if (tc.trait_registry.get(trait_decl.name) != null) {
        tc.addError(.duplicate_definition, trait_decl.span, "duplicate trait definition '{s}'", .{trait_decl.name});
        return;
    }

    // Push type parameters into scope if this is a generic trait
    const has_type_params = trait_decl.type_params.len > 0;
    var trait_type_params: []const types.TypeVar = &.{};
    if (has_type_params) {
        trait_type_params = tc.pushTypeParams(trait_decl.type_params) catch return;
    }
    defer if (has_type_params) tc.popTypeParams();

    // Resolve super traits (trait inheritance)
    var super_trait_list: std.ArrayListUnmanaged(*types.TraitType) = .{};
    defer super_trait_list.deinit(tc.allocator);

    for (trait_decl.super_traits) |super_trait_expr| {
        const resolved = tc.resolveTypeExpr(super_trait_expr) catch {
            tc.addError(.undefined_type, trait_decl.span, "cannot resolve super trait", .{});
            continue;
        };
        if (resolved != .trait_) {
            tc.addError(.type_mismatch, trait_decl.span, "expected trait type in trait inheritance", .{});
            continue;
        }
        // Check that super trait is already defined (no forward references)
        if (tc.trait_registry.get(resolved.trait_.name) == null) {
            tc.addError(.undefined_type, trait_decl.span, "super trait '{s}' must be defined before use", .{resolved.trait_.name});
            continue;
        }
        super_trait_list.append(tc.allocator, resolved.trait_) catch {};
    }

    // Build associated types list
    var associated_types: std.ArrayListUnmanaged(types.AssociatedType) = .{};
    defer associated_types.deinit(tc.allocator);

    for (trait_decl.associated_types) |assoc| {
        // Resolve bounds for the associated type
        var bounds: std.ArrayListUnmanaged(*types.TraitType) = .{};
        defer bounds.deinit(tc.allocator);

        for (assoc.bounds) |bound_expr| {
            const bound_type = tc.resolveTypeExpr(bound_expr) catch continue;
            if (bound_type == .trait_) {
                bounds.append(tc.allocator, bound_type.trait_) catch {};
            }
        }

        // Resolve default type if present
        const default_type: ?Type = if (assoc.default) |default_expr|
            tc.resolveTypeExpr(default_expr) catch null
        else
            null;

        associated_types.append(tc.allocator, .{
            .name = assoc.name,
            .bounds = bounds.toOwnedSlice(tc.allocator) catch &.{},
            .default = default_type,
        }) catch {};
    }

    // Create trait type first (with empty methods) so Self.Item can be resolved
    const trait_type = tc.allocator.create(types.TraitType) catch return;
    trait_type.* = .{
        .name = trait_decl.name,
        .type_params = trait_type_params,
        .methods = &.{}, // Will be filled in below after setting trait context
        .super_traits = super_trait_list.toOwnedSlice(tc.allocator) catch &.{},
        .associated_types = associated_types.toOwnedSlice(tc.allocator) catch &.{},
        .is_unsafe = trait_decl.is_unsafe,
    };

    // Set current trait context so Self.Item can be resolved in method signatures
    const prev_trait = tc.current_trait_type;
    tc.current_trait_type = trait_type;
    defer tc.current_trait_type = prev_trait;

    // Build methods list (now Self.Item can be resolved)
    var methods: std.ArrayListUnmanaged(types.TraitMethod) = .{};
    defer methods.deinit(tc.allocator);

    for (trait_decl.methods) |method| {
        // Build parameter types for signature
        var sig_params: std.ArrayListUnmanaged(Type) = .{};
        defer sig_params.deinit(tc.allocator);

        for (method.params) |param| {
            if (std.mem.eql(u8, param.name, "self")) {
                // Self parameter - use 'unknown' as placeholder for implementing type
                // Check if it's a reference type (&self or &mut self)
                if (param.type_ == .reference) {
                    const ref_type = tc.type_builder.referenceType(tc.type_builder.unknownType(), param.type_.reference.mutable) catch tc.type_builder.unknownType();
                    sig_params.append(tc.allocator, ref_type) catch {};
                } else {
                    sig_params.append(tc.allocator, tc.type_builder.unknownType()) catch {};
                }
            } else {
                const param_type = tc.resolveTypeExpr(param.type_) catch tc.type_builder.unknownType();
                sig_params.append(tc.allocator, param_type) catch {};
            }
        }

        const return_type = if (method.return_type) |rt|
            tc.resolveTypeExpr(rt) catch tc.type_builder.unknownType()
        else
            tc.type_builder.voidType();

        methods.append(tc.allocator, .{
            .name = method.name,
            .signature = .{
                .params = sig_params.toOwnedSlice(tc.allocator) catch &.{},
                .return_type = return_type,
            },
            .has_default = method.body != null,
        }) catch {};
    }

    // Update trait type with resolved methods
    trait_type.methods = methods.toOwnedSlice(tc.allocator) catch &.{};

    // Track for cleanup
    tc.trait_types.append(tc.allocator, trait_type) catch {};

    // Register in trait registry
    tc.trait_registry.put(tc.allocator, trait_decl.name, .{
        .trait_type = trait_type,
        .decl = trait_decl,
    }) catch {};

    // Register in scope as a type
    tc.current_scope.define(.{
        .name = trait_decl.name,
        .type_ = .{ .trait_ = trait_type },
        .kind = .trait_,
        .mutable = false,
        .span = trait_decl.span,
    }) catch {};
}

fn checkImpl(tc: anytype, impl_decl: *ast.ImplDecl) void {
    // Push type parameters into scope if this is a generic impl
    const has_type_params = impl_decl.type_params.len > 0;
    var impl_type_params: []const types.TypeVar = &.{};
    if (has_type_params) {
        impl_type_params = tc.pushTypeParams(impl_decl.type_params) catch return;
    }
    defer if (has_type_params) tc.popTypeParams();

    // Resolve the target type (e.g., Pair[A, B] or Point)
    const target_type = tc.resolveTypeExpr(impl_decl.target_type) catch {
        tc.addError(.undefined_type, impl_decl.span, "cannot resolve impl target type", .{});
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
            tc.addError(.invalid_operation, impl_decl.span, "impl blocks currently only support structs and enums", .{});
            return;
        },
        else => {
            tc.addError(.invalid_operation, impl_decl.span, "impl blocks currently only support structs and enums", .{});
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
            const base_trait = tc.resolveTypeExpr(generic_apply.base) catch {
                tc.addError(.undefined_type, impl_decl.span, "cannot resolve trait type", .{});
                return;
            };

            // Resolve type arguments
            var resolved_args = std.ArrayListUnmanaged(Type){};
            for (generic_apply.args) |arg_expr| {
                const arg_type = tc.resolveTypeExpr(arg_expr) catch continue;
                resolved_args.append(tc.allocator, arg_type) catch {};
            }
            trait_type_args = resolved_args.toOwnedSlice(tc.allocator) catch &.{};
            tc.substituted_type_slices.append(tc.allocator, trait_type_args) catch {};

            break :blk base_trait;
        } else tc.resolveTypeExpr(trait_type_expr) catch {
            tc.addError(.undefined_type, impl_decl.span, "cannot resolve trait type", .{});
            return;
        };

        // Must be a trait
        if (resolved_trait != .trait_) {
            tc.addError(.type_mismatch, impl_decl.span, "expected trait type", .{});
            return;
        }

        // Look up the trait in the registry
        const trait_name = resolved_trait.trait_.name;
        trait_info = tc.trait_registry.get(trait_name);
        if (trait_info == null) {
            tc.addError(.undefined_type, impl_decl.span, "trait '{s}' not found", .{trait_name});
            return;
        }

        // Check that unsafe traits require unsafe impl
        if (trait_info.?.trait_type.is_unsafe and !impl_decl.is_unsafe) {
            tc.addError(.unsafe_trait_requires_unsafe_impl, impl_decl.span, "implementing unsafe trait '{s}' requires 'unsafe impl'", .{trait_name});
            return;
        }

        // Check that unsafe impl is only used for unsafe traits
        if (impl_decl.is_unsafe and !trait_info.?.trait_type.is_unsafe) {
            tc.addError(.invalid_operation, impl_decl.span, "'unsafe impl' can only be used when implementing an unsafe trait", .{});
            return;
        }
    }

    // Process associated type bindings from the impl block
    var assoc_bindings: std.ArrayListUnmanaged(AssociatedTypeBinding) = .{};
    defer assoc_bindings.deinit(tc.allocator);

    for (impl_decl.associated_types) |binding| {
        // Resolve the concrete type
        const concrete_type = tc.resolveTypeExpr(binding.value) catch {
            tc.addError(.undefined_type, binding.span, "cannot resolve associated type binding", .{});
            continue;
        };

        // If implementing a trait, verify the binding corresponds to a trait associated type
        if (trait_info) |info| {
            const trait_type = info.trait_type;
            var found_in_trait = false;
            for (trait_type.associated_types) |assoc| {
                if (std.mem.eql(u8, assoc.name, binding.name)) {
                    found_in_trait = true;
                    break;
                }
            }
            if (!found_in_trait) {
                tc.addError(.undefined_type, binding.span, "'{s}' is not an associated type of trait '{s}'", .{ binding.name, trait_type.name });
                continue;
            }
        }

        assoc_bindings.append(tc.allocator, .{
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

        if (method_decl.params.len > 0) {
            const first_param = method_decl.params[0];
            if (std.mem.eql(u8, first_param.name, "self")) {
                has_self = true;
                // Check if self is mutable (type would be &mut Self or similar)
                if (first_param.type_ == .reference) {
                    self_is_mutable = first_param.type_.reference.mutable;
                }
            }
        }

        // Build the method's function type
        var param_types: std.ArrayListUnmanaged(Type) = .{};
        defer param_types.deinit(tc.allocator);

        // Include all parameters (including self) in the function type
        for (method_decl.params) |param| {
            // For 'self' parameter, use the target struct type (possibly wrapped in reference)
            if (std.mem.eql(u8, param.name, "self")) {
                // Check if self is by reference (&self or &mut self)
                if (param.type_ == .reference) {
                    // Preserve the reference wrapper with the correct mutability
                    const self_ref = tc.type_builder.referenceType(target_type, param.type_.reference.mutable) catch {
                        param_types.append(tc.allocator, target_type) catch {};
                        continue;
                    };
                    param_types.append(tc.allocator, self_ref) catch {};
                } else {
                    // self by value
                    param_types.append(tc.allocator, target_type) catch {};
                }
            } else {
                const param_type = tc.resolveTypeExpr(param.type_) catch tc.type_builder.unknownType();
                param_types.append(tc.allocator, param_type) catch {};
            }
        }

        const return_type = if (method_decl.return_type) |rt|
            tc.resolveTypeExpr(rt) catch tc.type_builder.unknownType()
        else
            tc.type_builder.voidType();

        const func_type = tc.type_builder.functionType(param_types.items, return_type) catch {
            continue;
        };

        // Register the method in the struct_methods registry
        const result = tc.struct_methods.getOrPut(tc.allocator, struct_name) catch continue;
        if (!result.found_existing) {
            result.value_ptr.* = .{};
        }

        // Check for duplicate method
        var is_duplicate = false;
        for (result.value_ptr.items) |existing| {
            if (std.mem.eql(u8, existing.name, method_decl.name)) {
                tc.addError(.duplicate_definition, method_decl.span, "duplicate method definition", .{});
                is_duplicate = true;
                break;
            }
        }

        if (!is_duplicate) {
            // Duplicate the impl_type_params slice to own it
            const owned_type_params = tc.allocator.dupe(types.TypeVar, impl_type_params) catch &.{};

            result.value_ptr.append(tc.allocator, .{
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
            _ = tc.pushScope(.function) catch continue;
            defer tc.popScope();

            // Bind parameters
            for (method_decl.params, 0..) |param, idx| {
                const param_type = if (idx < param_types.items.len) param_types.items[idx] else tc.type_builder.unknownType();
                tc.current_scope.define(.{
                    .name = param.name,
                    .type_ = param_type,
                    .kind = .parameter,
                    .mutable = if (std.mem.eql(u8, param.name, "self")) self_is_mutable else false,
                    .span = param.span,
                }) catch {};
            }

            tc.current_return_type = return_type;
            defer tc.current_return_type = null;

            _ = tc.checkBlock(body);
        }
    }

    // If this is a trait implementation, verify all required methods are implemented
    if (trait_info) |info| {
        const trait_type = info.trait_type;

        // Get the list of implemented methods for this struct
        const impl_methods = tc.struct_methods.get(struct_name);

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
                tc.addError(.trait_not_implemented, impl_decl.span, "missing associated type binding for '{s}'", .{trait_assoc.name});
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
                        _ = tc.verifyMethodSignature(impl_method, trait_method, target_type, trait_type.type_params, trait_type_args, impl_decl.span);
                        break;
                    }
                }
            }

            if (!found and !trait_method.has_default) {
                tc.addError(.trait_not_implemented, impl_decl.span, "missing implementation for required trait method '{s}'", .{trait_method.name});
            }
        }

        // Register this trait implementation
        // For generic traits like From[IoError], include type args in key
        const impl_key = if (trait_type_args.len > 0)
            tc.makeGenericTraitImplKey(struct_name, trait_type.name, trait_type_args)
        else
            tc.makeTraitImplKey(struct_name, trait_type.name);
        const impl_result = tc.trait_impls.getOrPut(tc.allocator, impl_key) catch return;
        if (!impl_result.found_existing) {
            impl_result.value_ptr.* = .{};
        }

        impl_result.value_ptr.append(tc.allocator, .{
            .trait_name = trait_type.name,
            .impl_type_name = struct_name,
            .impl_type_params = impl_type_params,
            .associated_type_bindings = assoc_bindings.toOwnedSlice(tc.allocator) catch &.{},
            .methods = if (impl_methods) |m| m.items else &.{},
        }) catch {};
    }
}

fn checkTypeAlias(tc: anytype, alias: *ast.TypeAlias) void {
    const target_type = tc.resolveTypeExpr(alias.target) catch return;

    tc.current_scope.define(.{
        .name = alias.name,
        .type_ = target_type,
        .kind = .type_,
        .mutable = false,
        .span = alias.span,
    }) catch {};
}

fn checkConst(tc: anytype, const_decl: *ast.ConstDecl) void {
    const value_type = tc.checkExpr(const_decl.value);

    const declared_type = if (const_decl.type_) |t|
        tc.resolveTypeExpr(t) catch tc.type_builder.unknownType()
    else
        value_type;

    if (const_decl.type_ != null and !declared_type.eql(value_type)) {
        tc.addError(.type_mismatch, const_decl.span, "constant type mismatch", .{});
    }

    // Try to evaluate the constant at compile time and store it for comptime access
    if (tc.evaluateComptimeExpr(const_decl.value)) |interp_value| {
        if (tc.valueToComptimeValue(interp_value, const_decl.span)) |comptime_val| {
            tc.constant_values.put(tc.allocator, const_decl.name, comptime_val) catch {};
        }
    }

    tc.current_scope.define(.{
        .name = const_decl.name,
        .type_ = declared_type,
        .kind = .constant,
        .mutable = false,
        .span = const_decl.span,
    }) catch {};
}
