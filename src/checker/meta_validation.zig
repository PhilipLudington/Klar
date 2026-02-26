//! Meta annotation validation.
//!
//! This module validates meta annotations during type checking:
//! - Scope rules (meta module file-level only, meta guide file/impl only)
//! - Group name resolution (meta in references defined groups)
//! - Related path resolution (meta related references existing declarations)
//! - Deprecation tracking (meta deprecated functions)
//! - Module/guide field type validation

const std = @import("std");
const ast = @import("../ast.zig");

/// Kinds of declarations that can carry meta annotations.
pub const DeclKind = enum {
    function,
    struct_,
    enum_,
    trait_,
    impl_,
    field,
    variant,
    test_,
    type_alias,
    const_,
};

/// Collect group names from file-level meta annotations.
/// Must be called early in module checking, before any declaration validation.
/// Clears previous group names first to prevent cross-module leakage.
pub fn collectGroupNames(tc: anytype, file_meta: []const ast.MetaAnnotation) void {
    tc.meta_group_names.clearRetainingCapacity();
    for (file_meta) |annotation| {
        switch (annotation) {
            .group_def => |g| {
                tc.meta_group_names.put(tc.allocator, g.name, {}) catch {};
            },
            else => {},
        }
    }
}

/// Validate file-level meta annotations (meta module field types, etc).
pub fn validateFileMeta(tc: anytype, file_meta: []const ast.MetaAnnotation) void {
    for (file_meta) |annotation| {
        switch (annotation) {
            .module_meta => |block| {
                validateModuleMetaFields(tc, block);
            },
            else => {},
        }
    }
}

/// Validate meta annotations on a declaration.
pub fn validateDeclMeta(tc: anytype, meta: []const ast.MetaAnnotation, decl_kind: DeclKind) void {
    for (meta) |annotation| {
        switch (annotation) {
            .module_meta => {
                tc.addError(.meta_error, annotation.span(), "meta module can only appear at file level", .{});
            },
            .guide => {
                if (decl_kind != .impl_) {
                    tc.addError(.meta_error, annotation.span(), "meta guide can only appear at file level or on impl blocks", .{});
                }
            },
            .group_join => |gj| {
                if (!tc.meta_group_names.contains(gj.value)) {
                    tc.addError(.meta_error, gj.span, "meta in: group '{s}' is not defined", .{gj.value});
                }
            },
            .related => |rel| {
                validateRelatedPaths(tc, rel);
            },
            .group_def => {
                tc.addError(.meta_error, annotation.span(), "meta group can only appear at file level", .{});
            },
            else => {
                // intent, decision, tag, hint, deprecated, pure, define, custom — all valid on declarations
            },
        }
    }
}

/// Register a function as deprecated if it has a meta deprecated annotation.
pub fn registerDeprecatedFunction(tc: anytype, name: []const u8, meta: []const ast.MetaAnnotation) void {
    for (meta) |annotation| {
        switch (annotation) {
            .deprecated => |dep| {
                tc.deprecated_functions.put(tc.allocator, name, dep.value) catch {};
                return;
            },
            else => {},
        }
    }
}

/// Check if a call target is deprecated and emit a warning if so.
pub fn checkDeprecatedCall(tc: anytype, func_name: []const u8, call_span: ast.Span) void {
    if (tc.deprecated_functions.get(func_name)) |message| {
        tc.addWarning(call_span, "call to deprecated function '{s}': {s}", .{ func_name, message });
    }
}

/// Validate meta module block fields.
/// Known field 'depends' must be a string list. Other fields accept any value type.
fn validateModuleMetaFields(tc: anytype, block: *ast.MetaBlock) void {
    for (block.entries) |entry| {
        if (std.mem.eql(u8, entry.key, "depends")) {
            switch (entry.value) {
                .string_list => {},
                .string => {
                    tc.addError(.meta_error, entry.span, "meta module: 'depends' must be a list, e.g. depends: [\"a\", \"b\"]", .{});
                },
            }
        }
    }
}

/// Validate paths in a meta related annotation.
fn validateRelatedPaths(tc: anytype, related: *ast.MetaRelated) void {
    for (related.paths) |path| {
        if (path.segments.len == 0) continue;

        // Single-segment path: look up in current scope
        const name = path.segments[0];
        if (tc.current_scope.lookup(name) == null) {
            if (path.segments.len == 1) {
                tc.addError(.meta_error, path.span, "meta related: '{s}' does not refer to a known declaration", .{name});
            }
            // Multi-segment paths: validate first segment only for M.5
            // Full cross-module resolution deferred to later milestone
        }
    }
}
