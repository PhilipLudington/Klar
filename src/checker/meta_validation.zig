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

/// Kinds of declarations that can carry meta annotations (for validation scope matching).
///
/// Separate from meta_query.zig's DeclKind which is for CLI display. Key differences:
/// - This enum: used by validateDeclMeta/validateCustomScope for scope checking
/// - meta_query.zig: adds file_meta, test_decl, const_decl, and splits struct_field/enum_variant
/// Both enums intentionally diverge because their use cases differ.
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

/// Collect custom meta annotation definitions from file-level meta annotations.
/// Must be called after processImports (which may add imported definitions).
/// Does NOT clear — use clearMetaDefinitions() before processImports to reset.
pub fn collectMetaDefinitions(tc: anytype, file_meta: []const ast.MetaAnnotation) void {
    for (file_meta) |annotation| {
        switch (annotation) {
            .define => |def| {
                if (tc.meta_definitions.contains(def.name)) {
                    tc.addError(.meta_error, def.span, "duplicate meta define '{s}'", .{def.name});
                } else {
                    tc.meta_definitions.put(tc.allocator, def.name, def) catch {};
                }
            },
            else => {},
        }
    }
}

/// Clear the meta definitions registry. Call before processImports when switching modules.
pub fn clearMetaDefinitions(tc: anytype) void {
    tc.meta_definitions.clearRetainingCapacity();
}

/// Clear the deprecated functions map, freeing allocated method keys.
/// Method keys (format "TypeName::method_name") are heap-allocated via registerDeprecatedMethod
/// and must be freed before clearing the map to avoid leaking memory in multi-module builds.
/// Plain function keys are borrowed slices into the AST source buffer and must NOT be freed.
///
/// Safety: We distinguish the two by checking for "::" in the key. This is safe because
/// Klar identifiers cannot contain "::" (it is a path separator token, not a valid identifier
/// character), so any key containing "::" was necessarily heap-allocated by registerDeprecatedMethod.
pub fn clearDeprecatedFunctions(tc: anytype) void {
    var key_iter = tc.deprecated_functions.keyIterator();
    while (key_iter.next()) |key| {
        if (std.mem.indexOf(u8, key.*, "::") != null) {
            tc.allocator.free(key.*);
        }
    }
    tc.deprecated_functions.clearRetainingCapacity();
}

/// Validate only custom meta annotations at file level.
/// Used in Phase 2 of two-phase checking when custom annotations were deferred.
pub fn validateFileMetaCustomOnly(tc: anytype, file_meta: []const ast.MetaAnnotation) void {
    for (file_meta) |annotation| {
        switch (annotation) {
            .custom => |cust| {
                validateCustomAnnotation(tc, cust, null);
            },
            else => {},
        }
    }
}

/// Validate file-level meta annotations (meta module field types, custom annotations, group joins, etc).
pub fn validateFileMeta(tc: anytype, file_meta: []const ast.MetaAnnotation) void {
    for (file_meta) |annotation| {
        switch (annotation) {
            .module_meta => |block| {
                validateModuleMetaFields(tc, block);
            },
            .group_join => |gj| {
                if (!tc.meta_group_names.contains(gj.value)) {
                    tc.addError(.meta_error, gj.span, "meta in: group '{s}' is not defined", .{gj.value});
                }
            },
            .custom => |cust| {
                if (!tc.lenient_custom_meta) {
                    validateCustomAnnotation(tc, cust, null);
                }
            },
            .group_def => |g| {
                // Validate group body structurally: nested annotations are templates
                // applied to declarations at join time, so scope-dependent checks
                // (custom annotation scope, meta guide on impl-only) are deferred.
                for (g.annotations) |nested| {
                    validateGroupBodyAnnotation(tc, nested);
                }
            },
            else => {},
        }
    }
}

/// Validate a single annotation inside a group definition body.
/// Group annotations are templates — they will be applied to declarations at join time.
/// Only structural errors are checked here (no meta module, no nested groups/defines).
/// Scope-dependent checks (custom annotation scope, meta guide on impl-only) are
/// deferred until the group is joined to a specific declaration.
fn validateGroupBodyAnnotation(tc: anytype, annotation: ast.MetaAnnotation) void {
    switch (annotation) {
        .module_meta => {
            tc.addError(.meta_error, annotation.span(), "meta module cannot appear inside a group definition", .{});
        },
        .group_def => {
            tc.addError(.meta_error, annotation.span(), "meta group cannot be nested inside another group definition", .{});
        },
        .define => {
            tc.addError(.meta_error, annotation.span(), "meta define cannot appear inside a group definition", .{});
        },
        .group_join => |gj| {
            if (!tc.meta_group_names.contains(gj.value)) {
                tc.addError(.meta_error, gj.span, "meta in: group '{s}' is not defined", .{gj.value});
            }
        },
        .related => |rel| {
            validateRelatedPaths(tc, rel);
        },
        .custom => |cust| {
            // Validate argument types/counts but NOT scope (scope depends on target declaration)
            if (!tc.lenient_custom_meta) {
                validateCustomAnnotationNoScope(tc, cust);
            }
        },
        else => {
            // intent, decision, tag, hint, deprecated, pure, guide — all structurally valid in groups
        },
    }
}

/// Validate a custom meta annotation's existence and argument types, but NOT its scope.
/// Used for group body annotations where the target declaration kind is not yet known.
fn validateCustomAnnotationNoScope(tc: anytype, cust: *ast.MetaCustom) void {
    const def = tc.meta_definitions.get(cust.name) orelse {
        tc.addError(.meta_error, cust.span, "unknown custom meta annotation '{s}'", .{cust.name});
        return;
    };

    if (cust.args.len != def.params.len) {
        tc.addError(.meta_error, cust.span, "meta {s} expects {d} argument(s), got {d}", .{ cust.name, def.params.len, cust.args.len });
        return;
    }

    for (cust.args, 0..) |arg, i| {
        const param = def.params[i];
        switch (param.type_constraint) {
            .string_type => {
                switch (arg) {
                    .string => {},
                    .path => |p| {
                        const joined = joinMetaPath(tc.allocator, p.segments);
                        defer joined.deinit();
                        tc.addError(.meta_error, cust.span, "meta {s}: expected string literal for parameter '{s}', got path '{s}'", .{ cust.name, param.name, joined.slice });
                    },
                }
            },
            .path_type => {
                switch (arg) {
                    .path => {},
                    .string => |value| {
                        tc.addError(.meta_error, cust.span, "meta {s}: expected path for parameter '{s}', got string literal \"{s}\"", .{ cust.name, param.name, value });
                    },
                }
            },
            .string_union => |allowed| {
                switch (arg) {
                    .string => |value| {
                        var found = false;
                        for (allowed) |valid| {
                            if (std.mem.eql(u8, value, valid)) {
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            tc.addError(.meta_error, cust.span, "meta {s}: argument '{s}' is not a valid value for parameter '{s}'", .{ cust.name, value, param.name });
                        }
                    },
                    .path => |p| {
                        const joined = joinMetaPath(tc.allocator, p.segments);
                        defer joined.deinit();
                        tc.addError(.meta_error, cust.span, "meta {s}: expected string literal for parameter '{s}', got path '{s}'", .{ cust.name, param.name, joined.slice });
                    },
                }
            },
        }
    }
    // NOTE: scope validation is intentionally skipped — see validateGroupBodyAnnotation.
}

/// Validate only custom meta annotations on a declaration.
/// Used in Phase 2 of two-phase checking to validate custom annotations that
/// were deferred from Phase 1 (when imports may have been incomplete).
pub fn validateDeclMetaCustomOnly(tc: anytype, meta: []const ast.MetaAnnotation, decl_kind: DeclKind) void {
    for (meta) |annotation| {
        switch (annotation) {
            .custom => |cust| {
                validateCustomAnnotation(tc, cust, decl_kind);
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
            .define => {
                tc.addError(.meta_error, annotation.span(), "meta define can only appear at file level", .{});
            },
            .custom => |cust| {
                if (!tc.lenient_custom_meta) {
                    validateCustomAnnotation(tc, cust, decl_kind);
                }
            },
            .require, .ensure => {
                if (decl_kind != .function and decl_kind != .impl_) {
                    tc.addError(.meta_error, annotation.span(), "meta require/ensure can only appear on functions and methods", .{});
                }
            },
            else => {
                // intent, decision, tag, hint, deprecated, pure — all valid on declarations
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

/// Register a method as deprecated using a qualified key "TypeName::method_name".
pub fn registerDeprecatedMethod(tc: anytype, type_name: []const u8, method_name: []const u8, meta: []const ast.MetaAnnotation) void {
    for (meta) |annotation| {
        switch (annotation) {
            .deprecated => |dep| {
                const key = std.fmt.allocPrint(tc.allocator, "{s}::{s}", .{ type_name, method_name }) catch return;
                tc.deprecated_functions.put(tc.allocator, key, dep.value) catch {
                    tc.allocator.free(key);
                    return;
                };
                return;
            },
            else => {},
        }
    }
}

/// Check if a method call target is deprecated and emit a warning if so.
pub fn checkDeprecatedMethodCall(tc: anytype, type_name: []const u8, method_name: []const u8, call_span: ast.Span) void {
    const key = std.fmt.allocPrint(tc.allocator, "{s}::{s}", .{ type_name, method_name }) catch return;
    defer tc.allocator.free(key);
    if (tc.deprecated_functions.get(key)) |message| {
        tc.addWarning(call_span, "call to deprecated method '{s}.{s}': {s}", .{ type_name, method_name, message });
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

/// Register a method as pure using a qualified key "TypeName::method_name".
pub fn registerPureMethod(tc: anytype, type_name: []const u8, method_name: []const u8, meta: []const ast.MetaAnnotation) void {
    if (hasPureAnnotation(meta) != null) {
        const key = std.fmt.allocPrint(tc.allocator, "{s}::{s}", .{ type_name, method_name }) catch return;
        tc.pure_functions.put(tc.allocator, key, {}) catch {
            tc.allocator.free(key);
        };
    }
}

/// Clear the pure functions map, freeing allocated method keys.
/// Method keys (format "TypeName::method_name") are heap-allocated via registerPureMethod
/// and must be freed before clearing the map to avoid leaking memory in multi-module builds.
pub fn clearPureFunctions(tc: anytype) void {
    var key_iter = tc.pure_functions.keyIterator();
    while (key_iter.next()) |key| {
        if (std.mem.indexOf(u8, key.*, "::") != null) {
            tc.allocator.free(key.*);
        }
    }
    tc.pure_functions.clearRetainingCapacity();
}

/// Check if a function call violates purity constraints.
/// Called from checkCallImpl when in_pure_function is true.
///
/// Current scope: Function/builtin calls, user-defined method calls, and builtin method calls
/// are checked. Local mutation (var bindings, accumulator patterns) is intentionally allowed —
/// purity here means "no external side effects," not "no mutation at all."
///
/// Known gaps (not yet checked):
/// - Function-typed parameters (callbacks) — could hold impure function references
pub fn checkPureCall(tc: anytype, func_name: []const u8, call_span: ast.Span) void {
    // Check if calling a known-impure builtin
    if (isImpureBuiltin(func_name)) {
        if (tc.pure_function_span) |ps| {
            tc.addError(.meta_error, call_span, "pure function (declared at line {d}) cannot call impure builtin '{s}'", .{ ps.line, func_name });
        } else {
            tc.addError(.meta_error, call_span, "pure function cannot call impure builtin '{s}'", .{func_name});
        }
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
    // Skip local variables and parameters. Local closures defined within the pure
    // function's body are safe. Function-typed parameters (callbacks) are a known
    // gap — they could hold impure function references but we cannot statically
    // verify this without a "pure callback" annotation on function types.
    // This is the same category of limitation as impure method calls.
    if (tc.current_scope.lookup(func_name)) |sym| {
        if (sym.kind == .variable or sym.kind == .parameter) return;
    }
    // User function — must be marked meta pure
    if (!tc.pure_functions.contains(func_name)) {
        if (tc.pure_function_span) |ps| {
            tc.addError(.meta_error, call_span, "pure function (declared at line {d}) cannot call non-pure function '{s}'", .{ ps.line, func_name });
        } else {
            tc.addError(.meta_error, call_span, "pure function cannot call non-pure function '{s}'", .{func_name});
        }
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
    if (std.mem.eql(u8, name, "fs_append_string")) return true;
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

/// Returns true if the given builtin method name is impure (mutates the receiver or performs I/O).
/// Used by checkPureBuiltinMethodCall to catch mutating collection methods and I/O methods
/// inside meta pure functions.
pub fn isImpureBuiltinMethod(method_name: []const u8) bool {
    // Collection mutation methods (List, Map, Set, String, Deque, PriorityQueue)
    if (std.mem.eql(u8, method_name, "push")) return true;
    if (std.mem.eql(u8, method_name, "pop")) return true;
    if (std.mem.eql(u8, method_name, "insert")) return true;
    if (std.mem.eql(u8, method_name, "remove")) return true;
    if (std.mem.eql(u8, method_name, "clear")) return true;
    if (std.mem.eql(u8, method_name, "set")) return true;
    if (std.mem.eql(u8, method_name, "sort")) return true;
    if (std.mem.eql(u8, method_name, "reverse")) return true;
    if (std.mem.eql(u8, method_name, "push_front")) return true;
    if (std.mem.eql(u8, method_name, "push_back")) return true;
    if (std.mem.eql(u8, method_name, "pop_front")) return true;
    if (std.mem.eql(u8, method_name, "pop_back")) return true;
    if (std.mem.eql(u8, method_name, "push_str")) return true;
    if (std.mem.eql(u8, method_name, "push_char")) return true;
    // I/O methods (File, Stream handles)
    if (std.mem.eql(u8, method_name, "write_all")) return true;
    if (std.mem.eql(u8, method_name, "flush")) return true;
    if (std.mem.eql(u8, method_name, "close")) return true;
    // Rc/Arc mutation
    if (std.mem.eql(u8, method_name, "replace")) return true;
    // Channel methods
    if (std.mem.eql(u8, method_name, "send")) return true;
    if (std.mem.eql(u8, method_name, "recv")) return true;
    return false;
}

/// Check if a builtin method call violates purity constraints.
/// Called from method_calls.checkBuiltinMethod when in_pure_function is true.
pub fn checkPureBuiltinMethodCall(tc: anytype, method_name: []const u8, call_span: ast.Span) void {
    if (isImpureBuiltinMethod(method_name)) {
        if (tc.pure_function_span) |ps| {
            tc.addError(.meta_error, call_span, "pure function (declared at line {d}) cannot call impure method '{s}'", .{ ps.line, method_name });
        } else {
            tc.addError(.meta_error, call_span, "pure function cannot call impure method '{s}'", .{method_name});
        }
    }
}

/// Check if a pure function has inout parameters, which would allow mutating external state.
/// Called when entering a meta pure function body.
/// Checks the AST type_expr directly (reference with mutable=true means inout).
pub fn checkPureInoutParams(tc: anytype, params: []const ast.FunctionParam, pure_span: ast.Span) void {
    for (params) |param| {
        const tag = std.meta.activeTag(param.type_);
        if (tag == .reference) {
            if (param.type_.reference.mutable) {
                tc.addError(.meta_error, param.span, "pure function (declared at line {d}) cannot have inout parameter '{s}': inout allows mutation of external state", .{ pure_span.line, param.name });
            }
        }
    }
}

/// Extract all `meta require` contracts from a meta annotation list.
pub fn getRequireContracts(meta: []const ast.MetaAnnotation) []const *ast.MetaContract {
    // Count first
    var count: usize = 0;
    for (meta) |annotation| {
        switch (annotation) {
            .require => count += 1,
            else => {},
        }
    }
    if (count == 0) return &.{};

    // This is called at type-check time, so we use a static buffer approach.
    // Max 16 require contracts per function should be sufficient.
    const S = struct {
        var buf: [16]*ast.MetaContract = undefined;
    };
    var i: usize = 0;
    for (meta) |annotation| {
        switch (annotation) {
            .require => |contract| {
                if (i < 16) {
                    S.buf[i] = contract;
                    i += 1;
                }
            },
            else => {},
        }
    }
    return S.buf[0..i];
}

/// Extract all `meta ensure` contracts from a meta annotation list.
pub fn getEnsureContracts(meta: []const ast.MetaAnnotation) []const *ast.MetaContract {
    var count: usize = 0;
    for (meta) |annotation| {
        switch (annotation) {
            .ensure => count += 1,
            else => {},
        }
    }
    if (count == 0) return &.{};

    const S = struct {
        var buf: [16]*ast.MetaContract = undefined;
    };
    var i: usize = 0;
    for (meta) |annotation| {
        switch (annotation) {
            .ensure => |contract| {
                if (i < 16) {
                    S.buf[i] = contract;
                    i += 1;
                }
            },
            else => {},
        }
    }
    return S.buf[0..i];
}

/// Type-check `meta require` contract expressions.
/// Require expressions are checked in the function's parameter scope — they can reference
/// parameters but not local variables. Each expression must evaluate to bool.
pub fn checkRequireContracts(tc: anytype, meta: []const ast.MetaAnnotation) void {
    for (meta) |annotation| {
        switch (annotation) {
            .require => |contract| {
                const expr_type = tc.checkExpr(contract.expr);
                if (!tc.isBoolType(expr_type)) {
                    tc.addError(.meta_error, contract.span, "meta require expression must be of type bool", .{});
                }
            },
            else => {},
        }
    }
}

/// Type-check `meta ensure` contract expressions.
/// Ensure expressions are checked with an additional `result` binding of the function's
/// return type. Each expression must evaluate to bool.
pub fn checkEnsureContracts(tc: anytype, meta: []const ast.MetaAnnotation, return_type: anytype) void {
    const checker = @import("checker.zig");
    for (meta) |annotation| {
        switch (annotation) {
            .ensure => |contract| {
                // Push a temporary scope with `result` binding for the ensure expression
                const scope = tc.pushScope(.block) catch return;
                scope.define(.{
                    .name = "result",
                    .type_ = return_type,
                    .kind = checker.Symbol.Kind.variable,
                    .mutable = false,
                    .span = contract.span,
                }) catch {};
                const expr_type = tc.checkExpr(contract.expr);
                if (!tc.isBoolType(expr_type)) {
                    tc.addError(.meta_error, contract.span, "meta ensure expression must be of type bool", .{});
                }
                tc.popScope();
            },
            else => {},
        }
    }
}

/// Validate a custom meta annotation against its definition.
/// `decl_kind` is null for file-level annotations.
fn validateCustomAnnotation(tc: anytype, cust: *ast.MetaCustom, decl_kind: ?DeclKind) void {
    const def = tc.meta_definitions.get(cust.name) orelse {
        tc.addError(.meta_error, cust.span, "unknown custom meta annotation '{s}'", .{cust.name});
        return;
    };

    // Check argument count
    if (cust.args.len != def.params.len) {
        tc.addError(.meta_error, cust.span, "meta {s} expects {d} argument(s), got {d}", .{ cust.name, def.params.len, cust.args.len });
        return;
    }

    // Validate each argument against its parameter's type constraint
    for (cust.args, 0..) |arg, i| {
        const param = def.params[i];
        switch (param.type_constraint) {
            .string_type => {
                switch (arg) {
                    .string => {},
                    .path => |p| {
                        const joined = joinMetaPath(tc.allocator, p.segments);
                        defer joined.deinit();
                        tc.addError(.meta_error, cust.span, "meta {s}: expected string literal for parameter '{s}', got path '{s}'", .{ cust.name, param.name, joined.slice });
                    },
                }
            },
            .path_type => {
                switch (arg) {
                    .path => {},
                    .string => |value| {
                        tc.addError(.meta_error, cust.span, "meta {s}: expected path for parameter '{s}', got string literal \"{s}\"", .{ cust.name, param.name, value });
                    },
                }
            },
            .string_union => |allowed| {
                // Arg must be a string and its value must be in the allowed set
                switch (arg) {
                    .string => |value| {
                        var found = false;
                        for (allowed) |valid| {
                            if (std.mem.eql(u8, value, valid)) {
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            tc.addError(.meta_error, cust.span, "meta {s}: argument '{s}' is not a valid value for parameter '{s}'", .{ cust.name, value, param.name });
                        }
                    },
                    .path => |p| {
                        // Path args don't match string union constraints
                        const joined = joinMetaPath(tc.allocator, p.segments);
                        defer joined.deinit();
                        tc.addError(.meta_error, cust.span, "meta {s}: expected string literal for parameter '{s}', got path '{s}'", .{ cust.name, param.name, joined.slice });
                    },
                }
            },
        }
    }

    // Validate scope restriction
    if (def.scope) |scope| {
        validateCustomScope(tc, cust, scope, decl_kind);
    }
}

/// Join meta path segments with "::" for error messages.
/// Returns a JoinedPath; call deinit() when done to free the heap buffer (if any).
const JoinedPath = struct {
    slice: []const u8,
    /// Non-null when slice is heap-allocated and must be freed.
    owned: ?[]const u8,
    allocator: std.mem.Allocator,

    fn deinit(self: JoinedPath) void {
        if (self.owned) |buf| self.allocator.free(buf);
    }
};

fn joinMetaPath(allocator: std.mem.Allocator, segments: []const []const u8) JoinedPath {
    if (segments.len == 0) return .{ .slice = "?", .owned = null, .allocator = allocator };
    if (segments.len == 1) return .{ .slice = segments[0], .owned = null, .allocator = allocator };
    var list = std.ArrayListUnmanaged(u8){};
    for (segments, 0..) |seg, i| {
        if (i > 0) list.appendSlice(allocator, "::") catch {
            list.deinit(allocator);
            return .{ .slice = segments[0], .owned = null, .allocator = allocator };
        };
        list.appendSlice(allocator, seg) catch {
            list.deinit(allocator);
            return .{ .slice = segments[0], .owned = null, .allocator = allocator };
        };
    }
    const owned_slice = list.toOwnedSlice(allocator) catch {
        list.deinit(allocator);
        return .{ .slice = segments[0], .owned = null, .allocator = allocator };
    };
    return .{ .slice = owned_slice, .owned = owned_slice, .allocator = allocator };
}

/// Validate that a custom annotation is used on the correct kind of declaration.
fn validateCustomScope(tc: anytype, cust: *ast.MetaCustom, scope: ast.MetaScope, decl_kind: ?DeclKind) void {
    const dk = decl_kind orelse {
        // File-level usage — only valid if scope is module_scope
        if (scope != .module_scope) {
            const scope_name = scopeName(scope);
            tc.addError(.meta_error, cust.span, "meta {s} can only appear on {s} declarations", .{ cust.name, scope_name });
        }
        return;
    };

    // fn_scope covers both functions and tests — tests are function-like executable units.
    // test_scope covers only tests. This means `for fn` is a superset of `for test`.
    const matches = switch (scope) {
        .fn_scope => dk == .function or dk == .test_,
        .module_scope => false, // module_scope means file-level only
        .struct_scope => dk == .struct_,
        .enum_scope => dk == .enum_,
        .trait_scope => dk == .trait_,
        .field_scope => dk == .field,
        .variant_scope => dk == .variant,
        .test_scope => dk == .test_,
    };

    if (!matches) {
        const scope_name = if (scope == .module_scope) "file-level" else scopeName(scope);
        tc.addError(.meta_error, cust.span, "meta {s} can only appear on {s} declarations", .{ cust.name, scope_name });
    }
}

/// Convert a MetaScope to a human-readable name.
fn scopeName(scope: ast.MetaScope) []const u8 {
    return switch (scope) {
        .fn_scope => "fn",
        .module_scope => "module",
        .struct_scope => "struct",
        .enum_scope => "enum",
        .trait_scope => "trait",
        .field_scope => "field",
        .variant_scope => "variant",
        .test_scope => "test",
    };
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
