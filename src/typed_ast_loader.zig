//! Loads a typed AST JSON file (produced by the selfhost `emit-typed-ast` command)
//! and populates both an `ast.Module` and a `TypeChecker` with declaration info.
//!
//! This enables the `--typed-ast-input` pipeline where the selfhost frontend performs
//! type checking and the Zig backend only handles LLVM codegen:
//!
//!   selfhost parser → selfhost checker → typed AST JSON → Zig codegen → binary

const std = @import("std");
const ast = @import("ast.zig");
const ast_from_json = @import("ast_from_json.zig");
const TypeChecker = @import("checker/checker.zig").TypeChecker;
const types = @import("types.zig");
const Allocator = std.mem.Allocator;
const Value = std.json.Value;
const ObjectMap = std.json.ObjectMap;
const Array = std.json.Array;

pub const TypedAstError = error{
    InvalidJson,
    NotTypedAst,
    UnsupportedVersion,
    MissingField,
    InvalidValue,
    OutOfMemory,
};

/// Load a typed AST JSON file. Builds the ast.Module and populates the
/// TypeChecker with declaration-level information (types, function signatures,
/// enum types, struct methods).
///
/// After this call, the TypeChecker is ready for codegen — no further
/// type checking is needed.
pub fn loadTypedAst(
    allocator: Allocator,
    arena: Allocator,
    json: []const u8,
    checker: *TypeChecker,
) !ast.Module {
    // Parse JSON
    const parsed = std.json.parseFromSlice(Value, allocator, json, .{}) catch {
        return TypedAstError.InvalidJson;
    };
    defer parsed.deinit();

    const root = parsed.value;
    if (root != .object) return TypedAstError.InvalidJson;
    const root_obj = root.object;

    // Validate format and version
    const format = getString(root_obj, "format") orelse return TypedAstError.NotTypedAst;
    if (!std.mem.eql(u8, format, "typed-ast")) return TypedAstError.NotTypedAst;

    const version = getInteger(root_obj, "version") orelse return TypedAstError.MissingField;
    if (version != 1) return TypedAstError.UnsupportedVersion;

    // Build the base AST module (reuses ast_from_json infrastructure).
    // The typed AST JSON is a superset of dump-ast JSON — extra fields
    // like "resolved_type", "id", monomorphized tables are ignored by buildModule.
    const module = ast_from_json.buildModule(arena, root_obj) catch {
        return TypedAstError.InvalidValue;
    };

    // Register declarations in the checker (types, function signatures, enums).
    // This populates the scope tables that codegen needs for symbol lookup.
    checker.prepareForNewModule();
    checker.checkModuleDeclarations(module);

    // Also process impl blocks to register struct methods.
    // checkModuleDeclarations skips impl_decl — we need methods for codegen.
    for (module.declarations) |decl| {
        switch (decl) {
            .impl_decl => checker.checkDecl(decl),
            else => {},
        }
    }

    // Clear any errors from declaration checking — the typed AST has already
    // been validated by the selfhost checker.
    checker.clearErrors();

    // Parse monomorphized tables and populate the checker.
    // For simple programs (no generics), these tables are empty.
    loadMonomorphizedFunctions(allocator, arena, root_obj, checker);
    loadMonomorphizedStructs(allocator, arena, root_obj, checker);
    loadMonomorphizedEnums(allocator, arena, root_obj, checker);
    loadMonomorphizedMethods(allocator, arena, root_obj, checker);

    return module;
}

/// Load a multi-module typed AST JSON file. Builds multiple ast.Modules and
/// populates the TypeChecker with all declarations and monomorphized tables.
///
/// The multi-module format uses a top-level "modules" array with per-module
/// declarations, and global monomorphized tables at the root level.
///
/// After this call, the caller should call checker.checkModuleBodies() on
/// each module to populate side tables for codegen.
pub fn loadMultiTypedAst(
    allocator: Allocator,
    arena: Allocator,
    json: []const u8,
    checker: *TypeChecker,
    modules_out: *std.ArrayListUnmanaged(ast.Module),
    prefixes_out: *std.ArrayListUnmanaged(?[]const u8),
) !void {
    // Parse JSON
    const parsed = std.json.parseFromSlice(Value, allocator, json, .{}) catch {
        return TypedAstError.InvalidJson;
    };
    defer parsed.deinit();

    const root = parsed.value;
    if (root != .object) return TypedAstError.InvalidJson;
    const root_obj = root.object;

    // Validate format and version
    const format = getString(root_obj, "format") orelse return TypedAstError.NotTypedAst;
    if (!std.mem.eql(u8, format, "typed-ast-multi")) return TypedAstError.NotTypedAst;

    const version = getInteger(root_obj, "version") orelse return TypedAstError.MissingField;
    if (version != 1) return TypedAstError.UnsupportedVersion;

    // Load each module from the "modules" array
    const modules_arr = getArray(root_obj, "modules") orelse return TypedAstError.MissingField;

    for (modules_arr.items) |mod_item| {
        if (mod_item != .object) continue;
        const mod_obj = mod_item.object;

        const name = getString(mod_obj, "name") orelse continue;
        const is_entry = getBool(mod_obj, "is_entry") orelse false;

        // Build AST module from this module entry's declarations/imports
        const module = ast_from_json.buildModule(arena, mod_obj) catch continue;

        // Register declarations in the checker's global scope (no module scoping
        // needed since the typed AST is already type-checked — we just need all
        // struct/enum/function declarations visible for codegen field resolution)
        checker.checkModuleDeclarations(module);

        // Register impl blocks (checkModuleDeclarations skips them)
        for (module.declarations) |decl| {
            switch (decl) {
                .impl_decl => checker.checkDecl(decl),
                else => {},
            }
        }
        checker.clearErrors();

        try modules_out.append(allocator, module);

        // Set module prefix: entry module gets null, others get the module name
        if (is_entry or name.len == 0) {
            try prefixes_out.append(allocator, null);
        } else {
            const prefix = try allocator.dupe(u8, name);
            try prefixes_out.append(allocator, prefix);
        }
    }

    // Load global monomorphized tables (shared across all modules)
    loadMonomorphizedFunctions(allocator, arena, root_obj, checker);
    loadMonomorphizedStructs(allocator, arena, root_obj, checker);
    loadMonomorphizedEnums(allocator, arena, root_obj, checker);
    loadMonomorphizedMethods(allocator, arena, root_obj, checker);
}

/// Detect whether a JSON string is multi-module typed AST format.
/// The selfhost emitter always writes `"format"` as the first JSON key,
/// so the discriminator appears within the first 200 bytes. We scan for
/// the exact value string "typed-ast-multi" to avoid false positives
/// from single-module format "typed-ast" (which is a prefix).
pub fn isMultiModuleFormat(json: []const u8) bool {
    const limit = @min(json.len, 200);
    const search = json[0..limit];
    return std.mem.indexOf(u8, search, "typed-ast-multi") != null;
}

// ============================================================================
// Monomorphized table loaders
// ============================================================================

fn loadMonomorphizedFunctions(
    allocator: Allocator,
    arena: Allocator,
    root: ObjectMap,
    checker: *TypeChecker,
) void {
    const arr = getArray(root, "monomorphized_functions") orelse return;

    for (arr.items) |item| {
        if (item != .object) continue;
        const obj = item.object;

        const original_name = getString(obj, "original_name") orelse continue;
        const mangled_name = getString(obj, "mangled_name") orelse continue;

        // Build the function declaration from the JSON body
        const func_decl = buildMonoFunctionDecl(arena, obj, mangled_name) catch continue;

        // Build concrete function type from params/return_type
        const concrete_type = buildFunctionTypeFromJson(allocator, checker, obj) catch continue;

        // Build type args
        const type_args = buildTypeArgs(allocator, checker, obj) catch continue;

        // mangled_name is freed by checker.deinit(); original_name is not,
        // so use the arena (freed when the arena is torn down in buildNative).
        const mangled_dupe = allocator.dupe(u8, mangled_name) catch continue;
        const original_dupe = arena.dupe(u8, original_name) catch continue;

        checker.monomorphized_functions.put(allocator, mangled_dupe, .{
            .original_name = original_dupe,
            .mangled_name = mangled_dupe,
            .type_args = type_args,
            .concrete_type = concrete_type,
            .original_decl = func_decl,
        }) catch continue;
    }
}

fn loadMonomorphizedStructs(
    allocator: Allocator,
    arena: Allocator,
    root: ObjectMap,
    checker: *TypeChecker,
) void {
    const arr = getArray(root, "monomorphized_structs") orelse return;

    for (arr.items) |item| {
        if (item != .object) continue;
        const obj = item.object;

        const original_name = getString(obj, "original_name") orelse continue;
        const mangled_name = getString(obj, "mangled_name") orelse continue;

        // Parse fields — the fields slice is freed by checker.deinit(),
        // but field.name is not, so use arena for names.
        const fields_arr = getArray(obj, "fields") orelse continue;
        const fields = allocator.alloc(types.StructField, fields_arr.items.len) catch continue;
        var valid = true;
        for (fields_arr.items, 0..) |field_val, i| {
            if (field_val != .object) {
                valid = false;
                break;
            }
            const field_obj = field_val.object;
            const field_name = getString(field_obj, "name") orelse {
                valid = false;
                break;
            };
            const field_type = resolveTypeJson(allocator, checker, field_obj, "type") catch {
                valid = false;
                break;
            };
            fields[i] = .{
                .name = arena.dupe(u8, field_name) catch {
                    valid = false;
                    break;
                },
                .type_ = field_type,
                .is_pub = getBool(field_obj, "is_pub") orelse false,
            };
        }
        if (!valid) {
            allocator.free(fields);
            continue;
        }
        // Parse type args
        const type_args = buildTypeArgs(allocator, checker, obj) catch {
            allocator.free(fields);
            continue;
        };

        // Create concrete struct type — struct_type.name is not freed by
        // checker.deinit() (only destroy is called), so use arena.
        const struct_type = allocator.create(types.StructType) catch {
            allocator.free(fields);
            continue;
        };
        struct_type.* = .{
            .name = arena.dupe(u8, mangled_name) catch {
                allocator.free(fields);
                allocator.destroy(struct_type);
                continue;
            },
            .fields = fields,
            .type_params = &.{},
            .traits = &.{},
            .is_extern = false,
        };

        // mangled_name is freed by checker.deinit(); original_name is not.
        const mangled_dupe = allocator.dupe(u8, mangled_name) catch continue;
        const original_dupe = arena.dupe(u8, original_name) catch continue;

        checker.monomorphized_structs.put(allocator, mangled_dupe, .{
            .original_name = original_dupe,
            .mangled_name = mangled_dupe,
            .type_args = type_args,
            .concrete_type = struct_type,
        }) catch continue;

    }
}

fn loadMonomorphizedEnums(
    allocator: Allocator,
    arena: Allocator,
    root: ObjectMap,
    checker: *TypeChecker,
) void {
    const arr = getArray(root, "monomorphized_enums") orelse return;

    for (arr.items) |item| {
        if (item != .object) continue;
        const obj = item.object;

        const original_name = getString(obj, "original_name") orelse continue;
        const mangled_name = getString(obj, "mangled_name") orelse continue;

        // Parse variants — the variants slice is freed by checker.deinit(),
        // but variant.name is not, so use arena for names.
        const variants_arr = getArray(obj, "variants") orelse continue;
        const variants = allocator.alloc(types.EnumVariant, variants_arr.items.len) catch continue;
        var valid = true;
        var valid_count: usize = 0;
        for (variants_arr.items, 0..) |var_val, i| {
            if (var_val != .object) {
                valid = false;
                break;
            }
            const var_obj = var_val.object;
            const variant_name = getString(var_obj, "name") orelse {
                valid = false;
                break;
            };
            const payload: ?types.VariantPayload = if (!isNull(var_obj, "payload")) blk: {
                const payload_type = resolveTypeJsonValue(allocator, checker, var_obj.get("payload").?) catch {
                    valid = false;
                    break;
                };
                const tuple = allocator.alloc(types.Type, 1) catch {
                    valid = false;
                    break;
                };
                tuple[0] = payload_type;
                break :blk .{ .tuple = tuple };
            } else null;
            if (!valid) break;
            variants[i] = .{
                .name = arena.dupe(u8, variant_name) catch {
                    valid = false;
                    break;
                },
                .payload = payload,
            };
            valid_count += 1;
        }
        if (!valid) {
            // Clean up any payload tuples already allocated
            for (variants[0..valid_count]) |v| {
                if (v.payload) |p| {
                    switch (p) {
                        .tuple => |t| allocator.free(t),
                        .struct_ => |s| allocator.free(s),
                    }
                }
            }
            allocator.free(variants);
            continue;
        }

        // Parse type args
        const type_args = buildTypeArgs(allocator, checker, obj) catch {
            allocator.free(variants);
            continue;
        };

        // Create concrete enum type — enum_type.name is not freed by
        // checker.deinit() (only destroy is called), so use arena.
        const enum_type = allocator.create(types.EnumType) catch {
            allocator.free(variants);
            continue;
        };
        enum_type.* = .{
            .name = arena.dupe(u8, mangled_name) catch {
                allocator.free(variants);
                allocator.destroy(enum_type);
                continue;
            },
            .variants = variants,
            .type_params = &.{},
            .is_extern = false,
            .repr_type = null,
        };

        // Do NOT append to generic_enum_types — codegen registers monomorphized
        // enums separately via registerMonomorphizedEnums, and checker.deinit()
        // would double-free if the same pointer is in both lists.

        // mangled_name is freed by checker.deinit(); original_name is not.
        const mangled_dupe = allocator.dupe(u8, mangled_name) catch continue;
        const original_dupe = arena.dupe(u8, original_name) catch continue;

        checker.monomorphized_enums.put(allocator, mangled_dupe, .{
            .original_name = original_dupe,
            .mangled_name = mangled_dupe,
            .type_args = type_args,
            .concrete_type = enum_type,
        }) catch continue;
    }
}

fn loadMonomorphizedMethods(
    allocator: Allocator,
    arena: Allocator,
    root: ObjectMap,
    checker: *TypeChecker,
) void {
    const generics = @import("checker/generics.zig");
    const arr = getArray(root, "monomorphized_methods") orelse return;

    for (arr.items) |item| {
        if (item != .object) continue;
        const obj = item.object;

        const struct_name = getString(obj, "struct_name") orelse continue;
        const method_name = getString(obj, "method_name") orelse continue;
        const mangled_name = getString(obj, "mangled_name") orelse continue;

        // Build the function declaration from the JSON body
        const func_decl = buildMonoFunctionDecl(arena, obj, mangled_name) catch continue;

        // Build concrete function type from params/return_type
        const concrete_type = buildFunctionTypeFromJson(allocator, checker, obj) catch continue;

        // Build type args (may be empty — we use mangled_struct_name directly)
        const type_args = buildTypeArgs(allocator, checker, obj) catch continue;

        const mangled_dupe = allocator.dupe(u8, mangled_name) catch continue;
        const struct_name_dupe = arena.dupe(u8, struct_name) catch continue;

        checker.monomorphized_methods.put(allocator, mangled_dupe, generics.MonomorphizedMethod{
            .struct_name = struct_name_dupe,
            .method_name = arena.dupe(u8, method_name) catch continue,
            .mangled_name = mangled_dupe,
            .type_args = type_args,
            .concrete_type = concrete_type,
            .original_decl = func_decl,
        }) catch continue;
    }
}

// ============================================================================
// Type resolution from JSON type objects
// ============================================================================

fn resolveTypeJson(allocator: Allocator, checker: *TypeChecker, obj: ObjectMap, key: []const u8) error{OutOfMemory}!types.Type {
    const val = obj.get(key) orelse return checker.type_builder.unknownType();
    return resolveTypeJsonValue(allocator, checker, val);
}

fn resolveTypeJsonValue(allocator: Allocator, checker: *TypeChecker, val: Value) error{OutOfMemory}!types.Type {
    if (val != .object) return checker.type_builder.unknownType();
    const obj = val.object;

    const kind = getString(obj, "kind") orelse return checker.type_builder.unknownType();

    // Primitives
    if (std.mem.eql(u8, kind, "i32")) return checker.type_builder.i32Type();
    if (std.mem.eql(u8, kind, "i64")) return checker.type_builder.i64Type();
    if (std.mem.eql(u8, kind, "f64")) return checker.type_builder.f64Type();
    if (std.mem.eql(u8, kind, "bool")) return checker.type_builder.boolType();
    if (std.mem.eql(u8, kind, "string")) return checker.type_builder.stringType();
    if (std.mem.eql(u8, kind, "char")) return checker.type_builder.charType();
    if (std.mem.eql(u8, kind, "u8")) return .{ .primitive = .u8_ };
    if (std.mem.eql(u8, kind, "void")) return checker.type_builder.voidType();
    if (std.mem.eql(u8, kind, "never")) return .never;

    // Composites
    if (std.mem.eql(u8, kind, "array")) {
        const element = try resolveTypeJson(allocator, checker, obj, "element");
        const size = getInteger(obj, "size") orelse return checker.type_builder.unknownType();
        return checker.type_builder.arrayType(element, @intCast(size)) catch return checker.type_builder.unknownType();
    }
    if (std.mem.eql(u8, kind, "slice")) {
        const element = try resolveTypeJson(allocator, checker, obj, "element");
        return checker.type_builder.sliceType(element) catch return checker.type_builder.unknownType();
    }
    if (std.mem.eql(u8, kind, "optional")) {
        const inner = try resolveTypeJson(allocator, checker, obj, "inner");
        return checker.type_builder.optionalType(inner) catch return checker.type_builder.unknownType();
    }
    if (std.mem.eql(u8, kind, "result")) {
        const ok = try resolveTypeJson(allocator, checker, obj, "ok");
        const err_type = try resolveTypeJson(allocator, checker, obj, "err");
        return checker.type_builder.resultType(ok, err_type) catch return checker.type_builder.unknownType();
    }
    if (std.mem.eql(u8, kind, "reference")) {
        const inner = try resolveTypeJson(allocator, checker, obj, "inner");
        const mutable = getBool(obj, "mutable") orelse false;
        return checker.type_builder.referenceType(inner, mutable) catch return checker.type_builder.unknownType();
    }
    if (std.mem.eql(u8, kind, "tuple")) {
        const elements_arr = getArray(obj, "elements") orelse return checker.type_builder.unknownType();
        const element_types = try allocator.alloc(types.Type, elements_arr.items.len);
        for (elements_arr.items, 0..) |elem, i| {
            element_types[i] = try resolveTypeJsonValue(allocator, checker, elem);
        }
        return checker.type_builder.tupleType(element_types) catch return checker.type_builder.unknownType();
    }
    if (std.mem.eql(u8, kind, "function")) {
        const params_arr = getArray(obj, "params") orelse return checker.type_builder.unknownType();
        const param_types = try allocator.alloc(types.Type, params_arr.items.len);
        for (params_arr.items, 0..) |p, i| {
            param_types[i] = try resolveTypeJsonValue(allocator, checker, p);
        }
        const ret = try resolveTypeJson(allocator, checker, obj, "return");
        return checker.type_builder.functionType(param_types, ret) catch return checker.type_builder.unknownType();
    }

    // User-defined types (look up in checker scope)
    if (std.mem.eql(u8, kind, "struct")) {
        const name = getString(obj, "name") orelse return checker.type_builder.unknownType();
        // Try checker's scope first
        if (checker.lookupSymbol(name)) |sym| {
            if (sym.type_ == .struct_) return sym.type_;
        }
        // Try monomorphized structs
        if (checker.monomorphized_structs.get(name)) |ms| {
            return types.Type{ .struct_ = ms.concrete_type };
        }
        return checker.type_builder.unknownType();
    }
    if (std.mem.eql(u8, kind, "enum")) {
        const name = getString(obj, "name") orelse return checker.type_builder.unknownType();
        if (checker.lookupSymbol(name)) |sym| {
            if (sym.type_ == .enum_) return sym.type_;
        }
        // Try monomorphized enums
        if (checker.monomorphized_enums.get(name)) |me| {
            return types.Type{ .enum_ = me.concrete_type };
        }
        return checker.type_builder.unknownType();
    }

    // Collections
    if (std.mem.eql(u8, kind, "list")) {
        const element = try resolveTypeJson(allocator, checker, obj, "element");
        return checker.type_builder.listType(element) catch return checker.type_builder.unknownType();
    }
    if (std.mem.eql(u8, kind, "map")) {
        const key_type = try resolveTypeJson(allocator, checker, obj, "key");
        const value_type = try resolveTypeJson(allocator, checker, obj, "value");
        return checker.type_builder.mapType(key_type, value_type) catch return checker.type_builder.unknownType();
    }
    if (std.mem.eql(u8, kind, "set")) {
        const element = try resolveTypeJson(allocator, checker, obj, "element");
        return checker.type_builder.setType(element) catch return checker.type_builder.unknownType();
    }
    if (std.mem.eql(u8, kind, "String")) {
        return checker.type_builder.stringDataType() catch return checker.type_builder.unknownType();
    }

    // Smart pointers
    if (std.mem.eql(u8, kind, "rc")) {
        const inner = try resolveTypeJson(allocator, checker, obj, "inner");
        return checker.type_builder.rcType(inner) catch return checker.type_builder.unknownType();
    }
    if (std.mem.eql(u8, kind, "arc")) {
        const inner = try resolveTypeJson(allocator, checker, obj, "inner");
        return checker.type_builder.arcType(inner) catch return checker.type_builder.unknownType();
    }

    // Future
    if (std.mem.eql(u8, kind, "future")) {
        const inner = try resolveTypeJson(allocator, checker, obj, "inner");
        return checker.type_builder.futureType(inner) catch return checker.type_builder.unknownType();
    }

    // Range
    if (std.mem.eql(u8, kind, "range")) {
        const elem = try resolveTypeJson(allocator, checker, obj, "element");
        return checker.type_builder.rangeType(elem, false) catch return checker.type_builder.unknownType();
    }

    // Smart pointer variants
    if (std.mem.eql(u8, kind, "weak_rc")) {
        const inner = try resolveTypeJson(allocator, checker, obj, "inner");
        return checker.type_builder.weakRcType(inner) catch return checker.type_builder.unknownType();
    }
    if (std.mem.eql(u8, kind, "weak_arc")) {
        const inner = try resolveTypeJson(allocator, checker, obj, "inner");
        return checker.type_builder.weakArcType(inner) catch return checker.type_builder.unknownType();
    }
    if (std.mem.eql(u8, kind, "cell")) {
        const inner = try resolveTypeJson(allocator, checker, obj, "inner");
        return checker.type_builder.cellType(inner) catch return checker.type_builder.unknownType();
    }
    if (std.mem.eql(u8, kind, "context_error")) {
        const inner = try resolveTypeJson(allocator, checker, obj, "inner");
        return checker.type_builder.contextErrorType(inner) catch return checker.type_builder.unknownType();
    }

    // Buffered I/O
    if (std.mem.eql(u8, kind, "buf_reader")) {
        const inner = try resolveTypeJson(allocator, checker, obj, "inner");
        return checker.type_builder.bufReaderType(inner) catch return checker.type_builder.unknownType();
    }
    if (std.mem.eql(u8, kind, "buf_writer")) {
        const inner = try resolveTypeJson(allocator, checker, obj, "inner");
        return checker.type_builder.bufWriterType(inner) catch return checker.type_builder.unknownType();
    }

    // FFI pointer types with inner
    if (std.mem.eql(u8, kind, "cptr")) {
        const inner = try resolveTypeJson(allocator, checker, obj, "inner");
        return checker.type_builder.cptrType(inner) catch return checker.type_builder.unknownType();
    }
    if (std.mem.eql(u8, kind, "copt_ptr")) {
        const inner = try resolveTypeJson(allocator, checker, obj, "inner");
        return checker.type_builder.coptPtrType(inner) catch return checker.type_builder.unknownType();
    }

    // I/O types
    if (std.mem.eql(u8, kind, "file")) return checker.type_builder.fileType();
    if (std.mem.eql(u8, kind, "io_error")) return checker.type_builder.ioErrorType();

    // FFI types
    if (std.mem.eql(u8, kind, "cstr")) return .{ .cstr = {} };
    if (std.mem.eql(u8, kind, "cstr_owned")) return .{ .cstr_owned = {} };

    return checker.type_builder.unknownType();
}

// ============================================================================
// Helper builders
// ============================================================================

fn buildFunctionTypeFromJson(allocator: Allocator, checker: *TypeChecker, obj: ObjectMap) !types.Type {
    const params_arr = getArray(obj, "params") orelse return checker.type_builder.unknownType();
    const param_types = try allocator.alloc(types.Type, params_arr.items.len);
    for (params_arr.items, 0..) |p, i| {
        if (p != .object) {
            param_types[i] = checker.type_builder.unknownType();
            continue;
        }
        param_types[i] = resolveTypeJson(allocator, checker, p.object, "type") catch checker.type_builder.unknownType();
    }
    const ret = resolveTypeJson(allocator, checker, obj, "return_type") catch checker.type_builder.unknownType();
    return checker.type_builder.functionType(param_types, ret) catch return checker.type_builder.unknownType();
}

fn buildTypeArgs(allocator: Allocator, checker: *TypeChecker, obj: ObjectMap) ![]const types.Type {
    const arr = getArray(obj, "type_args") orelse return &.{};
    const type_args = try allocator.alloc(types.Type, arr.items.len);
    for (arr.items, 0..) |ta, i| {
        type_args[i] = resolveTypeJsonValue(allocator, checker, ta) catch checker.type_builder.unknownType();
    }
    return type_args;
}

fn buildMonoFunctionDecl(arena: Allocator, obj: ObjectMap, mangled_name: []const u8) !*ast.FunctionDecl {
    const func = try arena.create(ast.FunctionDecl);

    // Parse params
    const params_arr = getArray(obj, "params") orelse return TypedAstError.MissingField;
    const params = try arena.alloc(ast.FunctionParam, params_arr.items.len);
    for (params_arr.items, 0..) |p, i| {
        if (p != .object) return TypedAstError.InvalidValue;
        const p_obj = p.object;
        const name = getString(p_obj, "name") orelse return TypedAstError.MissingField;
        const type_expr = buildTypeExprFromJson(arena, p_obj, "type");
        params[i] = .{
            .name = try arena.dupe(u8, name),
            .type_ = type_expr,
            .is_comptime = false,
            .is_out = false,
            .default_value = null,
            .span = ast_from_json.dummy_span,
        };
    }

    // Parse return type
    const ret_type = buildTypeExprFromJson(arena, obj, "return_type");

    // Parse body
    const body: ?*ast.Block = if (obj.get("body")) |body_val| blk: {
        if (body_val == .null) break :blk null;
        break :blk ast_from_json.buildBlock(arena, body_val) catch null;
    } else null;

    func.* = .{
        .name = try arena.dupe(u8, mangled_name),
        .params = params,
        .return_type = ret_type,
        .body = body,
        .type_params = &.{},
        .where_clause = null,
        .is_pub = false,
        .is_async = getBool(obj, "is_async") orelse false,
        .is_comptime = false,
        .is_unsafe = false,
        .is_extern = false,
        .is_variadic = false,
        .meta = &.{},
        .span = ast_from_json.dummy_span,
    };

    return func;
}

/// Build a simple TypeExpr from a type JSON object.
fn buildTypeExprFromJson(arena: Allocator, obj: ObjectMap, key: []const u8) ast.TypeExpr {
    const val = obj.get(key) orelse return .{ .named = .{ .name = "void", .span = ast_from_json.dummy_span } };
    if (val != .object) return .{ .named = .{ .name = "void", .span = ast_from_json.dummy_span } };
    const type_obj = val.object;
    const kind_str = getString(type_obj, "kind") orelse return .{ .named = .{ .name = "void", .span = ast_from_json.dummy_span } };

    // For struct/enum types, use the name field
    if (std.mem.eql(u8, kind_str, "struct") or std.mem.eql(u8, kind_str, "enum")) {
        const name = getString(type_obj, "name") orelse return .{ .named = .{ .name = "void", .span = ast_from_json.dummy_span } };
        return .{ .named = .{ .name = arena.dupe(u8, name) catch "void", .span = ast_from_json.dummy_span } };
    }

    // For primitive types, use the kind as the name
    return .{ .named = .{ .name = arena.dupe(u8, kind_str) catch "void", .span = ast_from_json.dummy_span } };
}

// ============================================================================
// JSON helpers
// ============================================================================

fn getString(obj: ObjectMap, key: []const u8) ?[]const u8 {
    const v = obj.get(key) orelse return null;
    return switch (v) {
        .string => |s| s,
        else => null,
    };
}

fn getBool(obj: ObjectMap, key: []const u8) ?bool {
    const v = obj.get(key) orelse return null;
    return switch (v) {
        .bool => |b| b,
        else => null,
    };
}

fn getArray(obj: ObjectMap, key: []const u8) ?Array {
    const v = obj.get(key) orelse return null;
    return switch (v) {
        .array => |a| a,
        else => null,
    };
}

fn isNull(obj: ObjectMap, key: []const u8) bool {
    const v = obj.get(key) orelse return true;
    return v == .null;
}

fn getInteger(obj: ObjectMap, key: []const u8) ?i64 {
    const v = obj.get(key) orelse return null;
    return switch (v) {
        .integer => |i| i,
        else => null,
    };
}
