//! Type resolution and unification.
//!
//! This module contains functions for:
//! - Type parameter substitution (generic instantiation)
//! - Type variable detection
//! - Type unification (pattern matching types against concrete types)
//! - Type expression resolution (AST TypeExpr -> Type)
//!
//! Functions receive a type checker instance via `anytype` to avoid
//! circular imports while maintaining type safety through duck typing.

const std = @import("std");
const ast = @import("../ast.zig");
const types = @import("../types.zig");
const Type = types.Type;
const Primitive = types.Primitive;

// ============================================================================
// Type Parameter Substitution
// ============================================================================

/// Substitute type parameters with concrete types.
/// Returns a new type with all type variables replaced.
pub fn substituteTypeParams(tc: anytype, typ: Type, substitutions: std.AutoHashMapUnmanaged(u32, Type)) !Type {
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
        // I/O types and extern types also don't need substitution (they have no type parameters)
        .primitive, .void_, .never, .unknown, .error_type, .file, .io_error, .stdout_handle, .stderr_handle, .stdin_handle, .path, .extern_type => typ,

        // Array - substitute element type
        .array => |arr| {
            const new_elem = try substituteTypeParams(tc, arr.element, substitutions);
            if (arr.element.eql(new_elem)) return typ;
            return tc.type_builder.arrayType(new_elem, arr.size);
        },

        // Slice - substitute element type
        .slice => |sl| {
            const new_elem = try substituteTypeParams(tc, sl.element, substitutions);
            if (sl.element.eql(new_elem)) return typ;
            return tc.type_builder.sliceType(new_elem);
        },

        // Tuple - substitute all element types
        .tuple => |tup| {
            var changed = false;
            var new_elements = std.ArrayListUnmanaged(Type){};
            defer new_elements.deinit(tc.allocator);

            for (tup.elements) |elem| {
                const new_elem = try substituteTypeParams(tc, elem, substitutions);
                if (!elem.eql(new_elem)) changed = true;
                try new_elements.append(tc.allocator, new_elem);
            }

            if (!changed) return typ;
            const elements_slice = try tc.allocator.dupe(Type, new_elements.items);
            try tc.substituted_type_slices.append(tc.allocator, elements_slice);
            return tc.type_builder.tupleType(elements_slice);
        },

        // Optional - substitute inner type
        .optional => |inner| {
            const new_inner = try substituteTypeParams(tc, inner.*, substitutions);
            if (inner.eql(new_inner)) return typ;
            return tc.type_builder.optionalType(new_inner);
        },

        // Result - substitute ok and err types
        .result => |res| {
            const new_ok = try substituteTypeParams(tc, res.ok_type, substitutions);
            const new_err = try substituteTypeParams(tc, res.err_type, substitutions);
            if (res.ok_type.eql(new_ok) and res.err_type.eql(new_err)) return typ;
            return tc.type_builder.resultType(new_ok, new_err);
        },

        // Function - substitute param and return types
        .function => |func| {
            var changed = false;
            var new_params = std.ArrayListUnmanaged(Type){};
            defer new_params.deinit(tc.allocator);

            for (func.params) |param_type| {
                const new_param_type = try substituteTypeParams(tc, param_type, substitutions);
                if (!param_type.eql(new_param_type)) changed = true;
                try new_params.append(tc.allocator, new_param_type);
            }

            const new_return = try substituteTypeParams(tc, func.return_type, substitutions);
            if (!func.return_type.eql(new_return)) changed = true;

            if (!changed) return typ;
            const params_slice = try tc.allocator.dupe(Type, new_params.items);
            try tc.substituted_type_slices.append(tc.allocator, params_slice);
            return tc.type_builder.functionType(params_slice, new_return);
        },

        // Reference - substitute inner type
        .reference => |ref| {
            const new_inner = try substituteTypeParams(tc, ref.inner, substitutions);
            if (ref.inner.eql(new_inner)) return typ;
            return tc.type_builder.referenceType(new_inner, ref.mutable);
        },

        // Applied type - substitute type arguments
        .applied => |app| {
            const new_base = try substituteTypeParams(tc, app.base, substitutions);
            var changed = !app.base.eql(new_base);

            var new_args = std.ArrayListUnmanaged(Type){};
            defer new_args.deinit(tc.allocator);

            for (app.args) |arg| {
                const new_arg = try substituteTypeParams(tc, arg, substitutions);
                if (!arg.eql(new_arg)) changed = true;
                try new_args.append(tc.allocator, new_arg);
            }

            if (!changed) return typ;
            const args_slice = try tc.allocator.dupe(Type, new_args.items);
            try tc.substituted_type_slices.append(tc.allocator, args_slice);
            return tc.type_builder.appliedType(new_base, args_slice);
        },

        // Rc types - substitute inner type
        .rc => |rc| {
            const new_inner = try substituteTypeParams(tc, rc.inner, substitutions);
            if (rc.inner.eql(new_inner)) return typ;
            return tc.type_builder.rcType(new_inner);
        },

        .weak_rc => |wrc| {
            const new_inner = try substituteTypeParams(tc, wrc.inner, substitutions);
            if (wrc.inner.eql(new_inner)) return typ;
            return tc.type_builder.weakRcType(new_inner);
        },

        .arc => |arc| {
            const new_inner = try substituteTypeParams(tc, arc.inner, substitutions);
            if (arc.inner.eql(new_inner)) return typ;
            return tc.type_builder.arcType(new_inner);
        },

        .weak_arc => |warc| {
            const new_inner = try substituteTypeParams(tc, warc.inner, substitutions);
            if (warc.inner.eql(new_inner)) return typ;
            return tc.type_builder.weakArcType(new_inner);
        },

        // Cell - substitute inner type
        .cell => |c| {
            const new_inner = try substituteTypeParams(tc, c.inner, substitutions);
            if (c.inner.eql(new_inner)) return typ;
            return tc.type_builder.cellType(new_inner);
        },

        // ContextError - substitute inner type
        .context_error => |ce| {
            const new_inner = try substituteTypeParams(tc, ce.inner_type, substitutions);
            if (ce.inner_type.eql(new_inner)) return typ;
            return tc.type_builder.contextErrorType(new_inner);
        },

        // Associated type reference - resolve using trait impls
        .associated_type_ref => |assoc_ref| {
            // Look up what concrete type was substituted for the type variable
            if (substitutions.get(assoc_ref.type_var.id)) |concrete_type| {
                // Get the struct/enum name from the concrete type
                const impl_type_name = switch (concrete_type) {
                    .struct_ => |s| s.name,
                    .enum_ => |e| e.name,
                    .applied => |app| switch (app.base) {
                        .struct_ => |s| s.name,
                        .enum_ => |e| e.name,
                        else => return typ, // Can't resolve associated type
                    },
                    else => return typ, // Can't resolve associated type for non-struct/enum
                };

                // Build the key for trait_impls lookup: "TypeName:TraitName"
                const impl_key = tc.makeTraitImplKey(impl_type_name, assoc_ref.trait.name);
                defer tc.allocator.free(impl_key);

                // Look up the trait impl for this concrete type
                if (tc.trait_impls.get(impl_key)) |impls| {
                    for (impls.items) |impl_info| {
                        // Find the associated type binding
                        for (impl_info.associated_type_bindings) |binding| {
                            if (std.mem.eql(u8, binding.name, assoc_ref.assoc_name)) {
                                return binding.concrete_type;
                            }
                        }
                    }
                }
            }
            // Couldn't resolve - return as-is (will be an error later)
            return typ;
        },

        // Struct/Enum/Trait - return as-is (monomorphization creates applied types)
        .struct_, .enum_, .trait_ => typ,

        // Range - substitute element type
        .range => |r| {
            const new_elem = try substituteTypeParams(tc, r.element_type, substitutions);
            if (r.element_type.eql(new_elem)) return typ;
            return tc.type_builder.rangeType(new_elem, r.inclusive);
        },

        // List - substitute element type
        .list => |l| {
            const new_elem = try substituteTypeParams(tc, l.element, substitutions);
            if (l.element.eql(new_elem)) return typ;
            return tc.type_builder.listType(new_elem);
        },

        // Map - substitute key and value types
        .map => |m| {
            const new_key = try substituteTypeParams(tc, m.key, substitutions);
            const new_value = try substituteTypeParams(tc, m.value, substitutions);
            if (m.key.eql(new_key) and m.value.eql(new_value)) return typ;
            return tc.type_builder.mapType(new_key, new_value);
        },

        // Set - substitute element type
        .set => |s| {
            const new_element = try substituteTypeParams(tc, s.element, substitutions);
            if (s.element.eql(new_element)) return typ;
            return tc.type_builder.setType(new_element);
        },

        // String - no type parameters to substitute
        .string_data => typ,

        // BufReader - substitute inner type
        .buf_reader => |br| {
            const new_inner = try substituteTypeParams(tc, br.inner, substitutions);
            if (br.inner.eql(new_inner)) return typ;
            return tc.type_builder.bufReaderType(new_inner);
        },

        // BufWriter - substitute inner type
        .buf_writer => |bw| {
            const new_inner = try substituteTypeParams(tc, bw.inner, substitutions);
            if (bw.inner.eql(new_inner)) return typ;
            return tc.type_builder.bufWriterType(new_inner);
        },

        // CPtr - substitute inner type
        .cptr => |cptr| {
            const new_inner = try substituteTypeParams(tc, cptr.inner, substitutions);
            if (cptr.inner.eql(new_inner)) return typ;
            return tc.type_builder.cptrType(new_inner);
        },

        // COptPtr - substitute inner type
        .copt_ptr => |coptptr| {
            const new_inner = try substituteTypeParams(tc, coptptr.inner, substitutions);
            if (coptptr.inner.eql(new_inner)) return typ;
            return tc.type_builder.coptPtrType(new_inner);
        },

        // CStr and CStrOwned - no type parameters to substitute
        .cstr, .cstr_owned => typ,

        // Extern fn - substitute param and return types
        .extern_fn => |ef| {
            var changed = false;
            var new_params = std.ArrayListUnmanaged(Type){};
            defer new_params.deinit(tc.allocator);

            for (ef.params) |param| {
                const new_param = try substituteTypeParams(tc, param, substitutions);
                if (!param.eql(new_param)) changed = true;
                try new_params.append(tc.allocator, new_param);
            }

            const new_ret = try substituteTypeParams(tc, ef.return_type, substitutions);
            if (!ef.return_type.eql(new_ret)) changed = true;

            if (!changed) return typ;
            const params_slice = try tc.allocator.dupe(Type, new_params.items);
            try tc.substituted_type_slices.append(tc.allocator, params_slice);
            return tc.type_builder.externFnType(params_slice, new_ret);
        },
    };
}

// ============================================================================
// Type Variable Detection
// ============================================================================

/// Check if a type contains any type variables.
pub fn containsTypeVar(tc: anytype, typ: Type) bool {
    return switch (typ) {
        .type_var => true,
        .primitive, .void_, .never, .unknown, .error_type, .file, .io_error, .stdout_handle, .stderr_handle, .stdin_handle, .path, .extern_type, .cstr, .cstr_owned => false,
        .array => |arr| containsTypeVar(tc, arr.element),
        .slice => |sl| containsTypeVar(tc, sl.element),
        .tuple => |tup| {
            for (tup.elements) |elem| {
                if (containsTypeVar(tc, elem)) return true;
            }
            return false;
        },
        .optional => |inner| containsTypeVar(tc, inner.*),
        .result => |res| containsTypeVar(tc, res.ok_type) or containsTypeVar(tc, res.err_type),
        .function => |func| {
            for (func.params) |param_type| {
                if (containsTypeVar(tc, param_type)) return true;
            }
            return containsTypeVar(tc, func.return_type);
        },
        .reference => |ref| containsTypeVar(tc, ref.inner),
        .applied => |app| {
            if (containsTypeVar(tc, app.base)) return true;
            for (app.args) |arg| {
                if (containsTypeVar(tc, arg)) return true;
            }
            return false;
        },
        .rc => |rc| containsTypeVar(tc, rc.inner),
        .weak_rc => |wrc| containsTypeVar(tc, wrc.inner),
        .arc => |arc| containsTypeVar(tc, arc.inner),
        .weak_arc => |warc| containsTypeVar(tc, warc.inner),
        .cell => |c| containsTypeVar(tc, c.inner),
        .context_error => |ce| containsTypeVar(tc, ce.inner_type),
        .range => |r| containsTypeVar(tc, r.element_type),
        .list => |l| containsTypeVar(tc, l.element),
        .map => |m| containsTypeVar(tc, m.key) or containsTypeVar(tc, m.value),
        .set => |s| containsTypeVar(tc, s.element),
        // String has no type parameters
        .string_data => false,
        // Buffered I/O types have inner type that may contain type variables
        .buf_reader => |br| containsTypeVar(tc, br.inner),
        .buf_writer => |bw| containsTypeVar(tc, bw.inner),
        // FFI pointer types may contain type variables in their inner type
        .cptr => |cptr| containsTypeVar(tc, cptr.inner),
        .copt_ptr => |coptptr| containsTypeVar(tc, coptptr.inner),
        // Extern fn types may contain type variables in their params/return type
        .extern_fn => |ef| {
            for (ef.params) |param| {
                if (containsTypeVar(tc, param)) return true;
            }
            return containsTypeVar(tc, ef.return_type);
        },
        // Associated type ref contains a type variable reference
        .associated_type_ref => true,
        .struct_, .enum_, .trait_ => false,
    };
}

// ============================================================================
// Type Unification
// ============================================================================

/// Try to unify a type with type variables against a concrete type.
/// Returns true if unification succeeded, populating the substitutions map.
pub fn unifyTypes(
    tc: anytype,
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
            try substitutions.put(tc.allocator, tv.id, concrete);
            return true;
        },

        .primitive, .void_, .never, .unknown, .error_type, .file, .io_error, .stdout_handle, .stderr_handle, .stdin_handle, .path, .extern_type, .cstr, .cstr_owned => {
            return pattern.eql(concrete);
        },

        .array => |arr| {
            if (concrete != .array) return false;
            const concrete_arr = concrete.array;
            if (arr.size != concrete_arr.size) return false;
            return unifyTypes(tc, arr.element, concrete_arr.element, substitutions);
        },

        .slice => |sl| {
            if (concrete != .slice) return false;
            const concrete_sl = concrete.slice;
            return unifyTypes(tc, sl.element, concrete_sl.element, substitutions);
        },

        .tuple => |tup| {
            if (concrete != .tuple) return false;
            const concrete_tup = concrete.tuple;
            if (tup.elements.len != concrete_tup.elements.len) return false;
            for (tup.elements, concrete_tup.elements) |pat_elem, conc_elem| {
                if (!try unifyTypes(tc, pat_elem, conc_elem, substitutions)) {
                    return false;
                }
            }
            return true;
        },

        .optional => |inner| {
            if (concrete != .optional) return false;
            return unifyTypes(tc, inner.*, concrete.optional.*, substitutions);
        },

        .result => |res| {
            if (concrete != .result) return false;
            const concrete_res = concrete.result;
            return try unifyTypes(tc, res.ok_type, concrete_res.ok_type, substitutions) and
                try unifyTypes(tc, res.err_type, concrete_res.err_type, substitutions);
        },

        .function => |func| {
            if (concrete != .function) return false;
            const concrete_func = concrete.function;
            if (func.params.len != concrete_func.params.len) return false;
            for (func.params, concrete_func.params) |pat_param_type, conc_param_type| {
                if (!try unifyTypes(tc, pat_param_type, conc_param_type, substitutions)) {
                    return false;
                }
            }
            return unifyTypes(tc, func.return_type, concrete_func.return_type, substitutions);
        },

        .reference => |ref| {
            if (concrete != .reference) return false;
            const concrete_ref = concrete.reference;
            if (ref.mutable != concrete_ref.mutable) return false;
            return unifyTypes(tc, ref.inner, concrete_ref.inner, substitutions);
        },

        .rc => |rc| {
            if (concrete != .rc) return false;
            return unifyTypes(tc, rc.inner, concrete.rc.inner, substitutions);
        },

        .weak_rc => |wrc| {
            if (concrete != .weak_rc) return false;
            return unifyTypes(tc, wrc.inner, concrete.weak_rc.inner, substitutions);
        },

        .arc => |arc| {
            if (concrete != .arc) return false;
            return unifyTypes(tc, arc.inner, concrete.arc.inner, substitutions);
        },

        .weak_arc => |warc| {
            if (concrete != .weak_arc) return false;
            return unifyTypes(tc, warc.inner, concrete.weak_arc.inner, substitutions);
        },

        .cell => |c| {
            if (concrete != .cell) return false;
            return unifyTypes(tc, c.inner, concrete.cell.inner, substitutions);
        },

        .context_error => |ce| {
            if (concrete != .context_error) return false;
            return unifyTypes(tc, ce.inner_type, concrete.context_error.inner_type, substitutions);
        },

        .applied => |app| {
            if (concrete != .applied) return false;
            const concrete_app = concrete.applied;
            if (!try unifyTypes(tc, app.base, concrete_app.base, substitutions)) {
                return false;
            }
            if (app.args.len != concrete_app.args.len) return false;
            for (app.args, concrete_app.args) |pat_arg, conc_arg| {
                if (!try unifyTypes(tc, pat_arg, conc_arg, substitutions)) {
                    return false;
                }
            }
            return true;
        },

        .associated_type_ref => |assoc_ref| {
            // Associated type ref acts like a type variable for unification purposes
            // Try to resolve it first using existing substitutions
            if (substitutions.get(assoc_ref.type_var.id)) |_| {
                // Resolve the associated type and compare
                const resolved = substituteTypeParams(tc, .{ .associated_type_ref = assoc_ref }, substitutions.*) catch {
                    return false;
                };
                if (resolved == .associated_type_ref) {
                    // Couldn't resolve - check if types match
                    return false;
                }
                return resolved.eql(concrete);
            }
            // No substitution for the type variable yet - can't unify
            return false;
        },

        .range => |r| {
            if (concrete != .range) return false;
            const concrete_range = concrete.range;
            if (r.inclusive != concrete_range.inclusive) return false;
            return unifyTypes(tc, r.element_type, concrete_range.element_type, substitutions);
        },

        .list => |l| {
            if (concrete != .list) return false;
            return unifyTypes(tc, l.element, concrete.list.element, substitutions);
        },

        .map => |m| {
            if (concrete != .map) return false;
            const concrete_map = concrete.map;
            if (!try unifyTypes(tc, m.key, concrete_map.key, substitutions)) {
                return false;
            }
            return unifyTypes(tc, m.value, concrete_map.value, substitutions);
        },

        .set => |s| {
            if (concrete != .set) return false;
            return unifyTypes(tc, s.element, concrete.set.element, substitutions);
        },

        // String has no type parameters - simple equality check
        .string_data => {
            return pattern.eql(concrete);
        },

        .buf_reader => |br| {
            if (concrete != .buf_reader) return false;
            return unifyTypes(tc, br.inner, concrete.buf_reader.inner, substitutions);
        },

        .buf_writer => |bw| {
            if (concrete != .buf_writer) return false;
            return unifyTypes(tc, bw.inner, concrete.buf_writer.inner, substitutions);
        },

        .cptr => |cptr| {
            if (concrete != .cptr) return false;
            return unifyTypes(tc, cptr.inner, concrete.cptr.inner, substitutions);
        },

        .copt_ptr => |coptptr| {
            if (concrete != .copt_ptr) return false;
            return unifyTypes(tc, coptptr.inner, concrete.copt_ptr.inner, substitutions);
        },

        .extern_fn => |ef| {
            if (concrete != .extern_fn) return false;
            const concrete_ef = concrete.extern_fn;
            if (ef.params.len != concrete_ef.params.len) return false;
            for (ef.params, concrete_ef.params) |pat_param, conc_param| {
                if (!try unifyTypes(tc, pat_param, conc_param, substitutions)) {
                    return false;
                }
            }
            return unifyTypes(tc, ef.return_type, concrete_ef.return_type, substitutions);
        },

        .struct_, .enum_, .trait_ => {
            return pattern.eql(concrete);
        },
    }
}

// ============================================================================
// Type Expression Resolution (AST TypeExpr -> Type)
// ============================================================================

/// Resolve a type expression from the AST to a concrete Type.
pub fn resolveTypeExpr(tc: anytype, type_expr: ast.TypeExpr) !Type {
    switch (type_expr) {
        .named => |n| {
            // Check for primitive types
            if (Primitive.fromName(n.name)) |prim| {
                return .{ .primitive = prim };
            }
            // Check for void
            if (std.mem.eql(u8, n.name, "void")) {
                return tc.type_builder.voidType();
            }
            // Check for String (heap-allocated string type)
            if (std.mem.eql(u8, n.name, "String")) {
                return try tc.type_builder.stringDataType();
            }
            // Check for CStr (FFI: null-terminated borrowed C string)
            if (std.mem.eql(u8, n.name, "CStr")) {
                return tc.type_builder.cstrType();
            }
            // Check for CStrOwned (FFI: null-terminated owned C string)
            if (std.mem.eql(u8, n.name, "CStrOwned")) {
                return tc.type_builder.cstrOwnedType();
            }
            // Check for Self
            if (std.mem.eql(u8, n.name, "Self")) {
                if (tc.current_impl_type) |impl_type| {
                    return impl_type;
                }
                if (tc.current_trait_type) |trait_type| {
                    return .{ .trait_ = trait_type };
                }
                tc.addError(.undefined_type, n.span, "'Self' can only be used inside impl or trait blocks", .{});
                return tc.type_builder.unknownType();
            }
            // Check for type parameters (e.g., T in fn foo[T](x: T))
            if (tc.lookupTypeParam(n.name)) |type_var| {
                return .{ .type_var = type_var };
            }
            // Look up user-defined type or trait
            if (tc.current_scope.lookup(n.name)) |sym| {
                if (sym.kind == .type_ or sym.kind == .trait_) {
                    return sym.type_;
                }
            }
            tc.addError(.undefined_type, n.span, "undefined type '{s}'", .{n.name});
            return tc.type_builder.unknownType();
        },
        .array => |a| {
            const elem_type = try resolveTypeExpr(tc, a.element);
            // Evaluate size expression at compile time
            if (tc.evaluateComptimeExpr(a.size)) |val| {
                if (val == .int) {
                    if (val.int.value < 0) {
                        tc.addError(.comptime_error, a.span, "array size cannot be negative", .{});
                        return try tc.type_builder.arrayType(elem_type, 0);
                    }
                    const size: usize = @intCast(val.int.value);
                    return try tc.type_builder.arrayType(elem_type, size);
                } else {
                    tc.addError(.comptime_error, a.span, "array size must be an integer", .{});
                    return try tc.type_builder.arrayType(elem_type, 0);
                }
            } else {
                tc.addError(.comptime_error, a.span, "array size must be comptime-known", .{});
                return try tc.type_builder.arrayType(elem_type, 0);
            }
        },
        .slice => |s| {
            const elem_type = try resolveTypeExpr(tc, s.element);
            return try tc.type_builder.sliceType(elem_type);
        },
        .tuple => |t| {
            var elem_types: std.ArrayListUnmanaged(Type) = .{};
            defer elem_types.deinit(tc.allocator);
            for (t.elements) |elem| {
                try elem_types.append(tc.allocator, try resolveTypeExpr(tc, elem));
            }
            return try tc.type_builder.tupleType(elem_types.items);
        },
        .optional => |o| {
            const inner = try resolveTypeExpr(tc, o.inner);
            return try tc.type_builder.optionalType(inner);
        },
        .result => |r| {
            const ok_type = try resolveTypeExpr(tc, r.ok_type);
            const err_type = try resolveTypeExpr(tc, r.err_type);
            return try tc.type_builder.resultType(ok_type, err_type);
        },
        .function => |f| {
            var param_types: std.ArrayListUnmanaged(Type) = .{};
            defer param_types.deinit(tc.allocator);
            for (f.params) |param| {
                try param_types.append(tc.allocator, try resolveTypeExpr(tc, param));
            }
            const ret_type = try resolveTypeExpr(tc, f.return_type);
            return try tc.type_builder.functionType(param_types.items, ret_type);
        },
        .extern_function => |ef| {
            var param_types: std.ArrayListUnmanaged(Type) = .{};
            defer param_types.deinit(tc.allocator);
            for (ef.params) |param| {
                try param_types.append(tc.allocator, try resolveTypeExpr(tc, param));
            }
            const ret_type = try resolveTypeExpr(tc, ef.return_type);
            return try tc.type_builder.externFnType(param_types.items, ret_type);
        },
        .reference => |r| {
            const inner = try resolveTypeExpr(tc, r.inner);
            return try tc.type_builder.referenceType(inner, r.mutable);
        },
        .generic_apply => |g| {
            // Check for built-in generic types: Rc[T], Weak[T]
            if (g.base == .named) {
                const base_name = g.base.named.name;

                // Rc[T] - reference-counted type
                if (std.mem.eql(u8, base_name, "Rc")) {
                    if (g.args.len != 1) {
                        tc.addError(.type_mismatch, g.span, "Rc expects exactly 1 type argument", .{});
                        return tc.type_builder.unknownType();
                    }
                    const inner = try resolveTypeExpr(tc, g.args[0]);
                    return try tc.type_builder.rcType(inner);
                }

                // Weak[T] - weak reference type
                if (std.mem.eql(u8, base_name, "Weak")) {
                    if (g.args.len != 1) {
                        tc.addError(.type_mismatch, g.span, "Weak expects exactly 1 type argument", .{});
                        return tc.type_builder.unknownType();
                    }
                    const inner = try resolveTypeExpr(tc, g.args[0]);
                    return try tc.type_builder.weakRcType(inner);
                }

                // Arc[T] - atomic reference-counted type (thread-safe)
                if (std.mem.eql(u8, base_name, "Arc")) {
                    if (g.args.len != 1) {
                        tc.addError(.type_mismatch, g.span, "Arc expects exactly 1 type argument", .{});
                        return tc.type_builder.unknownType();
                    }
                    const inner = try resolveTypeExpr(tc, g.args[0]);
                    return try tc.type_builder.arcType(inner);
                }

                // WeakArc[T] - weak atomic reference type (thread-safe)
                if (std.mem.eql(u8, base_name, "WeakArc")) {
                    if (g.args.len != 1) {
                        tc.addError(.type_mismatch, g.span, "WeakArc expects exactly 1 type argument", .{});
                        return tc.type_builder.unknownType();
                    }
                    const inner = try resolveTypeExpr(tc, g.args[0]);
                    return try tc.type_builder.weakArcType(inner);
                }

                // Cell[T] - interior mutability wrapper
                if (std.mem.eql(u8, base_name, "Cell")) {
                    if (g.args.len != 1) {
                        tc.addError(.type_mismatch, g.span, "Cell expects exactly 1 type argument", .{});
                        return tc.type_builder.unknownType();
                    }
                    const inner = try resolveTypeExpr(tc, g.args[0]);
                    return try tc.type_builder.cellType(inner);
                }

                // Result[T, E] - result type for error handling
                if (std.mem.eql(u8, base_name, "Result")) {
                    if (g.args.len != 2) {
                        tc.addError(.type_mismatch, g.span, "Result expects exactly 2 type arguments", .{});
                        return tc.type_builder.unknownType();
                    }
                    const ok_type = try resolveTypeExpr(tc, g.args[0]);
                    const err_type = try resolveTypeExpr(tc, g.args[1]);
                    return try tc.type_builder.resultType(ok_type, err_type);
                }

                // ContextError[E] - error wrapper with context message
                if (std.mem.eql(u8, base_name, "ContextError")) {
                    if (g.args.len != 1) {
                        tc.addError(.type_mismatch, g.span, "ContextError expects exactly 1 type argument", .{});
                        return tc.type_builder.unknownType();
                    }
                    const inner = try resolveTypeExpr(tc, g.args[0]);
                    return try tc.type_builder.contextErrorType(inner);
                }

                // Option[T] - optional type (alternative syntax for ?T)
                if (std.mem.eql(u8, base_name, "Option")) {
                    if (g.args.len != 1) {
                        tc.addError(.type_mismatch, g.span, "Option expects exactly 1 type argument", .{});
                        return tc.type_builder.unknownType();
                    }
                    const inner = try resolveTypeExpr(tc, g.args[0]);
                    return try tc.type_builder.optionalType(inner);
                }

                // Range[T] - range iterator type
                if (std.mem.eql(u8, base_name, "Range")) {
                    if (g.args.len != 1) {
                        tc.addError(.type_mismatch, g.span, "Range expects exactly 1 type argument", .{});
                        return tc.type_builder.unknownType();
                    }
                    const inner = try resolveTypeExpr(tc, g.args[0]);
                    if (!inner.isInteger()) {
                        tc.addError(.type_mismatch, g.span, "Range element type must be an integer type", .{});
                    }
                    // Default to non-inclusive when created via type syntax
                    return try tc.type_builder.rangeType(inner, false);
                }

                // List[T] - growable collection type
                if (std.mem.eql(u8, base_name, "List")) {
                    if (g.args.len != 1) {
                        tc.addError(.type_mismatch, g.span, "List expects exactly 1 type argument", .{});
                        return tc.type_builder.unknownType();
                    }
                    const inner = try resolveTypeExpr(tc, g.args[0]);
                    return try tc.type_builder.listType(inner);
                }

                // Map[K,V] - hash map type
                if (std.mem.eql(u8, base_name, "Map")) {
                    if (g.args.len != 2) {
                        tc.addError(.type_mismatch, g.span, "Map expects exactly 2 type arguments (key and value types)", .{});
                        return tc.type_builder.unknownType();
                    }
                    const key_type = try resolveTypeExpr(tc, g.args[0]);
                    const value_type = try resolveTypeExpr(tc, g.args[1]);
                    // Check that key type implements Hash + Eq
                    if (!tc.typeImplementsHashAndEq(key_type, g.span)) {
                        // Error already reported by typeImplementsHashAndEq
                    }
                    return try tc.type_builder.mapType(key_type, value_type);
                }

                // Set[T] - hash set type
                if (std.mem.eql(u8, base_name, "Set")) {
                    if (g.args.len != 1) {
                        tc.addError(.type_mismatch, g.span, "Set expects exactly 1 type argument (element type)", .{});
                        return tc.type_builder.unknownType();
                    }
                    const element_type = try resolveTypeExpr(tc, g.args[0]);
                    // Check that element type implements Hash + Eq
                    if (!tc.typeImplementsHashAndEq(element_type, g.span)) {
                        // Error already reported by typeImplementsHashAndEq
                    }
                    return try tc.type_builder.setType(element_type);
                }

                // BufReader[R] - buffered reader wrapper (R must implement Read)
                if (std.mem.eql(u8, base_name, "BufReader")) {
                    if (g.args.len != 1) {
                        tc.addError(.type_mismatch, g.span, "BufReader expects exactly 1 type argument (reader type)", .{});
                        return tc.type_builder.unknownType();
                    }
                    const inner_type = try resolveTypeExpr(tc, g.args[0]);
                    // Validate that inner type implements Read trait
                    if (tc.trait_registry.get("Read")) |read_trait| {
                        if (!tc.typeSatisfiesBounds(inner_type, &.{read_trait.trait_type}, g.span)) {
                            // Error already reported by typeSatisfiesBounds
                        }
                    }
                    return try tc.type_builder.bufReaderType(inner_type);
                }

                // BufWriter[W] - buffered writer wrapper (W must implement Write)
                if (std.mem.eql(u8, base_name, "BufWriter")) {
                    if (g.args.len != 1) {
                        tc.addError(.type_mismatch, g.span, "BufWriter expects exactly 1 type argument (writer type)", .{});
                        return tc.type_builder.unknownType();
                    }
                    const inner_type = try resolveTypeExpr(tc, g.args[0]);
                    // Validate that inner type implements Write trait
                    if (tc.trait_registry.get("Write")) |write_trait| {
                        if (!tc.typeSatisfiesBounds(inner_type, &.{write_trait.trait_type}, g.span)) {
                            // Error already reported by typeSatisfiesBounds
                        }
                    }
                    return try tc.type_builder.bufWriterType(inner_type);
                }

                // FFI Pointer Types

                // CPtr[T] - non-null raw pointer (FFI)
                if (std.mem.eql(u8, base_name, "CPtr")) {
                    if (g.args.len != 1) {
                        tc.addError(.type_mismatch, g.span, "CPtr expects exactly 1 type argument", .{});
                        return tc.type_builder.unknownType();
                    }
                    const inner = try resolveTypeExpr(tc, g.args[0]);
                    return try tc.type_builder.cptrType(inner);
                }

                // COptPtr[T] - nullable raw pointer (FFI)
                if (std.mem.eql(u8, base_name, "COptPtr")) {
                    if (g.args.len != 1) {
                        tc.addError(.type_mismatch, g.span, "COptPtr expects exactly 1 type argument", .{});
                        return tc.type_builder.unknownType();
                    }
                    const inner = try resolveTypeExpr(tc, g.args[0]);
                    return try tc.type_builder.coptPtrType(inner);
                }
            }

            // Generic user-defined type
            const base = try resolveTypeExpr(tc, g.base);
            var args: std.ArrayListUnmanaged(Type) = .{};
            defer args.deinit(tc.allocator);
            for (g.args) |arg| {
                try args.append(tc.allocator, try resolveTypeExpr(tc, arg));
            }

            // Check if base is a generic struct that needs monomorphization
            if (base == .struct_) {
                const struct_type = base.struct_;
                // Only monomorphize if this struct has type parameters
                if (struct_type.type_params.len > 0) {
                    // Validate argument count
                    if (args.items.len != struct_type.type_params.len) {
                        tc.addError(.type_mismatch, g.span, "wrong number of type arguments", .{});
                        return tc.type_builder.unknownType();
                    }
                    // Check if any type args contain unresolved type variables
                    var has_type_vars = false;
                    for (args.items) |arg| {
                        if (containsTypeVar(tc, arg)) {
                            has_type_vars = true;
                            break;
                        }
                    }
                    if (has_type_vars) {
                        // Inside a generic context - return an applied type for later resolution
                        return try tc.type_builder.appliedType(base, args.items);
                    }
                    // All type args are concrete - monomorphize the struct
                    const concrete_struct = tc.recordStructMonomorphization(
                        struct_type.name,
                        struct_type,
                        args.items,
                        g.span,
                    ) catch return tc.type_builder.unknownType();
                    return .{ .struct_ = concrete_struct };
                }
            }

            // Check if base is a generic enum that needs monomorphization
            if (base == .enum_) {
                const enum_type = base.enum_;
                if (enum_type.type_params.len > 0) {
                    // Validate argument count
                    if (args.items.len != enum_type.type_params.len) {
                        tc.addError(.type_mismatch, g.span, "wrong number of type arguments", .{});
                        return tc.type_builder.unknownType();
                    }
                    // Check if any type args contain unresolved type variables
                    var has_type_vars = false;
                    for (args.items) |arg| {
                        if (containsTypeVar(tc, arg)) {
                            has_type_vars = true;
                            break;
                        }
                    }
                    if (has_type_vars) {
                        // Inside a generic context - return an applied type for later resolution
                        return try tc.type_builder.appliedType(base, args.items);
                    }
                    // All type args are concrete - monomorphize the enum
                    const concrete_enum = tc.recordEnumMonomorphization(
                        enum_type.name,
                        enum_type,
                        args.items,
                    ) catch return tc.type_builder.unknownType();
                    return .{ .enum_ = concrete_enum };
                }
            }

            return try tc.type_builder.appliedType(base, args.items);
        },
        .qualified => |q| {
            // Qualified type access like Self.Item or T.Associated
            const base = try resolveTypeExpr(tc, q.base);

            // Currently we only support associated type access on Self or type variables
            switch (base) {
                .type_var => |tv| {
                    // T.Item - look up the associated type in T's trait bounds
                    for (tv.bounds) |bound_trait| {
                        for (bound_trait.associated_types) |assoc| {
                            if (std.mem.eql(u8, assoc.name, q.member)) {
                                // Found the associated type - create an AssociatedTypeRef
                                // that will be resolved during monomorphization
                                return tc.type_builder.associatedTypeRefType(tv, q.member, bound_trait) catch {
                                    return tc.type_builder.unknownType();
                                };
                            }
                        }
                    }
                    tc.addError(.undefined_type, q.span, "type variable has no associated type '{s}'", .{q.member});
                    return tc.type_builder.unknownType();
                },
                .unknown => {
                    // Self.Item in trait context - look up in current trait's associated types
                    // For now, we need to track the current trait being checked
                    // and look up q.member in its associated types
                    if (tc.current_trait_type) |trait_type| {
                        for (trait_type.associated_types) |assoc| {
                            if (std.mem.eql(u8, assoc.name, q.member)) {
                                // Found the associated type declaration
                                // In trait methods, Self.Item is a placeholder that will be
                                // resolved to the concrete type during impl checking
                                return tc.type_builder.unknownType();
                            }
                        }
                        tc.addError(.undefined_type, q.span, "trait has no associated type '{s}'", .{q.member});
                    } else {
                        tc.addError(.undefined_type, q.span, "cannot use Self.{s} outside trait context", .{q.member});
                    }
                    return tc.type_builder.unknownType();
                },
                .trait_ => |trait_type| {
                    // Self.Item where Self resolved to a trait type
                    // Look up the associated type in the trait's declarations
                    for (trait_type.associated_types) |assoc| {
                        if (std.mem.eql(u8, assoc.name, q.member)) {
                            // In trait method signatures, Self.Item is a placeholder
                            // resolved to the concrete type during impl checking
                            return tc.type_builder.unknownType();
                        }
                    }
                    tc.addError(.undefined_type, q.span, "trait '{s}' has no associated type '{s}'", .{ trait_type.name, q.member });
                    return tc.type_builder.unknownType();
                },
                else => {
                    tc.addError(.undefined_type, q.span, "cannot access associated type on this type", .{});
                    return tc.type_builder.unknownType();
                },
            }
        },
    }
}
