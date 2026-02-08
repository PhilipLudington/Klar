//! Method resolution and verification utilities.
//!
//! This module contains helper functions for method lookup, monomorphization tracking,
//! and trait method signature verification.
//!
//! Functions receive a type checker instance via `anytype` to avoid
//! circular imports while maintaining type safety through duck typing.

const std = @import("std");
const ast = @import("../ast.zig");
const types = @import("../types.zig");
const Type = types.Type;
const Span = ast.Span;

// Import from checker for type definitions
const checker_mod = @import("checker.zig");
const StructMethod = checker_mod.StructMethod;
const TraitMethodCall = checker_mod.TraitMethodCall;
const MonomorphizedMethod = checker_mod.MonomorphizedMethod;

// ============================================================================
// Method Lookup
// ============================================================================

/// Look up a method on a struct by name.
/// First tries direct lookup, then handles monomorphized struct names (containing $).
pub fn lookupStructMethod(tc: anytype, struct_name: []const u8, method_name: []const u8) ?StructMethod {
    // First try direct lookup
    if (tc.struct_methods.get(struct_name)) |methods| {
        for (methods.items) |m| {
            if (std.mem.eql(u8, m.name, method_name)) {
                return m;
            }
        }
    }

    // If this is a monomorphized struct name (contains $), try the original name
    if (std.mem.indexOf(u8, struct_name, "$")) |dollar_idx| {
        const original_name = struct_name[0..dollar_idx];
        if (tc.struct_methods.get(original_name)) |methods| {
            for (methods.items) |m| {
                if (std.mem.eql(u8, m.name, method_name)) {
                    return m;
                }
            }
        }
    }

    return null;
}

// ============================================================================
// Method Monomorphization
// ============================================================================

/// Record a monomorphized method instance for a generic struct.
pub fn recordMethodMonomorphization(tc: anytype, struct_type: *types.StructType, method: StructMethod) !void {
    // Create mangled method name first for O(1) hash lookup: Pair$i32$string_get_first
    var mangled_name_buf = std.ArrayListUnmanaged(u8){};
    errdefer mangled_name_buf.deinit(tc.allocator);
    try mangled_name_buf.appendSlice(tc.allocator, struct_type.name);
    try mangled_name_buf.append(tc.allocator, '_');
    try mangled_name_buf.appendSlice(tc.allocator, method.name);
    const mangled_name = try mangled_name_buf.toOwnedSlice(tc.allocator);

    // O(1) lookup - check if this method instantiation already exists
    if (tc.monomorphized_methods.contains(mangled_name)) {
        tc.allocator.free(mangled_name);
        return;
    }

    // Find the corresponding monomorphized struct to get type args
    var type_args: []const Type = &.{};
    var iter = tc.monomorphized_structs.valueIterator();
    while (iter.next()) |ms| {
        if (std.mem.eql(u8, ms.mangled_name, struct_type.name)) {
            type_args = ms.type_args;
            break;
        }
    }

    // Substitute type params in the method's function type
    var substitutions = std.AutoHashMapUnmanaged(u32, Type){};
    defer substitutions.deinit(tc.allocator);

    for (method.impl_type_params, 0..) |type_param, i| {
        if (i < type_args.len) {
            try substitutions.put(tc.allocator, type_param.id, type_args[i]);
        }
    }

    const concrete_func_type = try tc.substituteTypeParams(method.func_type, substitutions);

    try tc.monomorphized_methods.put(tc.allocator, mangled_name, .{
        .struct_name = if (std.mem.indexOf(u8, struct_type.name, "$")) |idx| struct_type.name[0..idx] else struct_type.name,
        .method_name = method.name,
        .mangled_name = mangled_name,
        .type_args = try tc.allocator.dupe(Type, type_args),
        .concrete_type = concrete_func_type,
        .original_decl = method.decl,
    });
}

/// Record a trait method call through a generic bound.
/// This tracks when a trait method is called on a type variable (e.g., T.method())
/// so that codegen can emit the correct concrete method call for each monomorphization.
pub fn recordTraitMethodCall(tc: anytype, type_var_name: []const u8, trait_name: []const u8, method_name: []const u8) !void {
    // Check if this call is already recorded
    for (tc.trait_method_calls.items) |existing| {
        if (std.mem.eql(u8, existing.type_var_name, type_var_name) and
            std.mem.eql(u8, existing.trait_name, trait_name) and
            std.mem.eql(u8, existing.method_name, method_name))
        {
            // Already recorded
            return;
        }
    }

    try tc.trait_method_calls.append(tc.allocator, .{
        .type_var_name = type_var_name,
        .trait_name = trait_name,
        .method_name = method_name,
    });
}

// ============================================================================
// Trait Method Signature Verification
// ============================================================================

/// Check if a type represents Self in a trait context.
/// Self can be represented as:
/// - .unknown (used by Eq, Ordered, Hash, Drop)
/// - .type_var with name "Self" (used by Clone, Default, Iterator)
pub fn isSelfType(t: Type) bool {
    return t == .unknown or (t == .type_var and std.mem.eql(u8, t.type_var.name, "Self"));
}

/// Check if a type is a reference to Self.
/// Returns the reference info if true, null otherwise.
pub fn isRefToSelf(t: Type) ?struct { mutable: bool } {
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
pub fn verifyMethodSignature(
    tc: anytype,
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
        tc.addError(.type_mismatch, span, "method '{s}' has {d} parameters, but trait requires {d}", .{
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
                    tc.addError(.type_mismatch, span, "method '{s}' parameter {d} should be Self type", .{
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
                    tc.addError(.type_mismatch, span, "method '{s}' parameter {d} should be reference to Self", .{
                        trait_method.name,
                        idx,
                    });
                    return false;
                }
                // Inner type must match implementing type
                if (!impl_param.reference.inner.eql(impl_type)) {
                    tc.addError(.type_mismatch, span, "method '{s}' parameter {d} should be reference to Self", .{
                        trait_method.name,
                        idx,
                    });
                    return false;
                }
                // Mutability must match
                if (impl_param.reference.mutable != ref_info.mutable) {
                    tc.addError(.type_mismatch, span, "method '{s}' parameter {d} mutability mismatch", .{
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
            tc.addError(.type_mismatch, span, "method '{s}' parameter {d} type mismatch", .{
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
            tc.addError(.type_mismatch, span, "method '{s}' return type should be Self", .{
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
            tc.addError(.type_mismatch, span, "method '{s}' return type mismatch", .{
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
                tc.addError(.type_mismatch, span, "method '{s}' return type should be optional", .{
                    trait_method.name,
                });
                return false;
            }
            // Return type validated enough for now
        } else if (!impl_func.return_type.eql(trait_sig.return_type)) {
            tc.addError(.type_mismatch, span, "method '{s}' return type mismatch", .{
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
            tc.addError(.type_mismatch, span, "method '{s}' return type mismatch", .{
                trait_method.name,
            });
            return false;
        }
    }

    return true;
}
