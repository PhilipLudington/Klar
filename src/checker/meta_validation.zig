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

/// Check if a meta annotation list contains `meta pure`.
/// Returns the span of the pure annotation if found.
pub fn hasPureAnnotation(meta: []const ast.MetaAnnotation) ?ast.Span {
    for (meta) |annotation| {
        switch (annotation) {
            .pure => |span| return span,
            else => {},
        }
    }
    return null;
}

/// Register a function as pure if it has a `meta pure` annotation.
pub fn registerPureFunction(tc: anytype, name: []const u8, meta: []const ast.MetaAnnotation) void {
    if (hasPureAnnotation(meta) != null) {
        tc.pure_functions.put(tc.allocator, name, {}) catch {};
    }
}

/// Check if a function call violates purity constraints.
/// Called from checkCallImpl when in_pure_function is true.
pub fn checkPureCall(tc: anytype, func_name: []const u8, call_span: ast.Span) void {
    // Check if calling a known-impure builtin
    if (isImpureBuiltin(func_name)) {
        tc.addError(.meta_error, call_span, "pure function cannot call impure builtin '{s}'", .{func_name});
        return;
    }
    // Skip constructors — always pure
    if (std.mem.eql(u8, func_name, "Ok") or std.mem.eql(u8, func_name, "Err") or
        std.mem.eql(u8, func_name, "Some") or std.mem.eql(u8, func_name, "None"))
    {
        return;
    }
    // Skip compiler-handled builtins that are pure (no side effects)
    if (isPureBuiltin(func_name)) return;
    // Skip local variables (closures defined within the pure function's body)
    if (tc.current_scope.lookup(func_name)) |sym| {
        if (sym.kind == .variable or sym.kind == .parameter) return;
    }
    // User function — must be marked meta pure
    if (!tc.pure_functions.contains(func_name)) {
        tc.addError(.meta_error, call_span, "pure function cannot call non-pure function '{s}'", .{func_name});
    }
}

/// Returns true if the given builtin function name is known to be impure (performs I/O or side effects).
fn isImpureBuiltin(name: []const u8) bool {
    // I/O functions
    if (std.mem.eql(u8, name, "print")) return true;
    if (std.mem.eql(u8, name, "println")) return true;
    if (std.mem.eql(u8, name, "readline")) return true;
    if (std.mem.eql(u8, name, "dbg")) return true;
    // Assertion functions (side effects: abort on failure)
    if (std.mem.eql(u8, name, "panic")) return true;
    if (std.mem.eql(u8, name, "assert")) return true;
    if (std.mem.eql(u8, name, "assert_eq")) return true;
    if (std.mem.eql(u8, name, "assert_ne")) return true;
    if (std.mem.eql(u8, name, "assert_ok")) return true;
    if (std.mem.eql(u8, name, "assert_err")) return true;
    if (std.mem.eql(u8, name, "assert_some")) return true;
    if (std.mem.eql(u8, name, "assert_none")) return true;
    // Stream handles
    if (std.mem.eql(u8, name, "stdout")) return true;
    if (std.mem.eql(u8, name, "stderr")) return true;
    if (std.mem.eql(u8, name, "stdin")) return true;
    // Filesystem functions
    if (std.mem.eql(u8, name, "fs_exists")) return true;
    if (std.mem.eql(u8, name, "fs_is_file")) return true;
    if (std.mem.eql(u8, name, "fs_is_dir")) return true;
    if (std.mem.eql(u8, name, "fs_create_dir")) return true;
    if (std.mem.eql(u8, name, "fs_create_dir_all")) return true;
    if (std.mem.eql(u8, name, "fs_remove_file")) return true;
    if (std.mem.eql(u8, name, "fs_remove_dir")) return true;
    if (std.mem.eql(u8, name, "fs_read_string")) return true;
    if (std.mem.eql(u8, name, "fs_write_string")) return true;
    if (std.mem.eql(u8, name, "fs_read_dir")) return true;
    // Unsafe memory access (side effects)
    if (std.mem.eql(u8, name, "read")) return true;
    if (std.mem.eql(u8, name, "write")) return true;
    return false;
}

/// Returns true if the given function name is a compiler-handled builtin that is pure (no side effects).
/// These are special-cased in checkCallImpl but don't have meta annotations, so they need
/// explicit whitelisting to avoid false "non-pure function" errors.
fn isPureBuiltin(name: []const u8) bool {
    if (std.mem.eql(u8, name, "debug")) return true;
    if (std.mem.eql(u8, name, "is_null")) return true;
    if (std.mem.eql(u8, name, "unwrap_ptr")) return true;
    if (std.mem.eql(u8, name, "offset")) return true;
    if (std.mem.eql(u8, name, "ref_to_ptr")) return true;
    if (std.mem.eql(u8, name, "ptr_cast")) return true;
    return false;
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
