//! Trait Checking Module
//!
//! Functions for checking trait implementations and bounds.
//! Uses duck typing (`tc: anytype`) to avoid circular dependencies with checker.zig.

const std = @import("std");
const types = @import("../types.zig");
const Type = types.Type;
const Span = @import("../ast.zig").Span;

// ============================================================================
// Trait Implementation Keys
// ============================================================================

/// Create a unique key for trait implementation lookup.
/// Format: "TypeName:TraitName"
pub fn makeTraitImplKey(tc: anytype, type_name: []const u8, trait_name: []const u8) []const u8 {
    return std.fmt.allocPrint(tc.allocator, "{s}:{s}", .{ type_name, trait_name }) catch "";
}

/// Create a key for generic trait implementations like From[IoError].
/// Format: "TypeName:TraitName[TypeArg1,TypeArg2,...]"
pub fn makeGenericTraitImplKey(tc: anytype, type_name: []const u8, trait_name: []const u8, type_args: []const Type) []const u8 {
    // Build the type args string
    var args_buf = std.ArrayListUnmanaged(u8){};
    defer args_buf.deinit(tc.allocator);

    args_buf.append(tc.allocator, '[') catch {};
    for (type_args, 0..) |arg, i| {
        if (i > 0) args_buf.append(tc.allocator, ',') catch {};
        const arg_name = getTypeName(tc, arg) orelse "unknown";
        args_buf.appendSlice(tc.allocator, arg_name) catch {};
    }
    args_buf.append(tc.allocator, ']') catch {};

    return std.fmt.allocPrint(tc.allocator, "{s}:{s}{s}", .{
        type_name,
        trait_name,
        args_buf.items,
    }) catch "";
}

// ============================================================================
// Trait Implementation Checks
// ============================================================================

/// Check if a type implements a trait.
pub fn typeImplementsTrait(tc: anytype, type_name: []const u8, trait_name: []const u8) bool {
    const key = makeTraitImplKey(tc, type_name, trait_name);
    defer tc.allocator.free(key);
    return tc.trait_impls.get(key) != null;
}

/// Check if target_type implements From[source_type].
/// Used by the ? operator to determine if automatic error conversion is possible.
/// Returns true if impl target_type: From[source_type] exists.
pub fn hasFromImpl(tc: anytype, target_type: Type, source_type: Type) bool {
    // Get the target type name (must be an enum for error types)
    const target_name = getTypeName(tc, target_type) orelse return false;
    const source_name = getTypeName(tc, source_type) orelse return false;

    // Build the key "TargetError:From[SourceError]"
    const key = std.fmt.allocPrint(tc.allocator, "{s}:From[{s}]", .{ target_name, source_name }) catch return false;
    defer tc.allocator.free(key);

    return tc.trait_impls.get(key) != null;
}

/// Get the name of a type for trait implementation lookup.
/// Returns null for types that cannot implement traits.
pub fn getTypeName(tc: anytype, t: Type) ?[]const u8 {
    _ = tc;
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
pub fn typeImplementsHashAndEq(tc: anytype, key_type: Type, span: Span) bool {
    // Primitives have builtin Hash and Eq implementations
    if (key_type == .primitive) {
        return true;
    }

    // For struct types, check that the struct implements both Hash and Eq
    if (key_type == .struct_) {
        const struct_name = key_type.struct_.name;
        var valid = true;

        if (!typeImplementsTrait(tc, struct_name, "Hash")) {
            tc.addError(.trait_not_implemented, span, "Map key type '{s}' must implement Hash", .{struct_name});
            valid = false;
        }
        if (!typeImplementsTrait(tc, struct_name, "Eq")) {
            tc.addError(.trait_not_implemented, span, "Map key type '{s}' must implement Eq", .{struct_name});
            valid = false;
        }
        return valid;
    }

    // For enum types, check if they implement Hash and Eq
    if (key_type == .enum_) {
        const enum_name = key_type.enum_.name;
        var valid = true;

        if (!typeImplementsTrait(tc, enum_name, "Hash")) {
            tc.addError(.trait_not_implemented, span, "Map key type '{s}' must implement Hash", .{enum_name});
            valid = false;
        }
        if (!typeImplementsTrait(tc, enum_name, "Eq")) {
            tc.addError(.trait_not_implemented, span, "Map key type '{s}' must implement Eq", .{enum_name});
            valid = false;
        }
        return valid;
    }

    // Other types (functions, references, etc.) are not valid map keys
    tc.addError(.type_mismatch, span, "type is not valid as a Map key - must implement Hash + Eq", .{});
    return false;
}

// ============================================================================
// Trait Bounds Checking
// ============================================================================

/// Check if a concrete type satisfies all trait bounds.
/// Returns true if the type satisfies all bounds, false otherwise.
pub fn typeSatisfiesBounds(tc: anytype, concrete_type: Type, bounds: []const *types.TraitType, span: Span) bool {
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
        if (!typeImplementsTrait(tc, type_name.?, trait.name)) {
            tc.addError(.trait_not_implemented, span, "type '{s}' does not implement trait '{s}'", .{ type_name.?, trait.name });
            return false;
        }
    }

    return true;
}
