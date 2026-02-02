//! Pattern matching type checking.
//!
//! This module contains functions for type-checking Klar pattern matching.
//! Patterns are used in match statements, for loops, and let bindings.

const std = @import("std");
const ast = @import("../ast.zig");
const types = @import("../types.zig");
const Type = types.Type;

// ============================================================================
// Pattern Checking
// ============================================================================

pub fn checkPattern(tc: anytype, pattern: ast.Pattern, expected_type: Type) void {
    switch (pattern) {
        .wildcard => {}, // Always matches
        .literal => |lit| {
            const lit_type = checkLiteralPattern(tc, lit);
            if (!lit_type.eql(expected_type)) {
                tc.addError(.type_mismatch, lit.span, "pattern type mismatch", .{});
            }
        },
        .binding => |bind| {
            tc.current_scope.define(.{
                .name = bind.name,
                .type_ = expected_type,
                .kind = .variable,
                .mutable = bind.mutable,
                .span = bind.span,
            }) catch {};
        },
        .variant => |v| {
            checkVariantPattern(tc, v, expected_type);
        },
        .struct_pattern => |s| {
            if (expected_type != .struct_) {
                tc.addError(.invalid_pattern, s.span, "struct pattern requires struct type", .{});
                return;
            }
            const struct_type = expected_type.struct_;
            for (s.fields) |field| {
                // Verify field exists on the struct
                var found_field: ?types.StructField = null;
                for (struct_type.fields) |sf| {
                    if (std.mem.eql(u8, sf.name, field.name)) {
                        found_field = sf;
                        break;
                    }
                }
                if (found_field) |sf| {
                    if (field.pattern) |field_pattern| {
                        checkPattern(tc, field_pattern, sf.type_);
                    }
                    // If no nested pattern, the field name becomes a binding (handled in bindPattern)
                } else {
                    tc.addError(.undefined_field, field.span, "struct '{s}' has no field '{s}'", .{ struct_type.name, field.name });
                }
            }
        },
        .tuple_pattern => |t| {
            if (expected_type != .tuple) {
                tc.addError(.invalid_pattern, t.span, "tuple pattern requires tuple type", .{});
                return;
            }
            const tuple_type = expected_type.tuple;
            if (t.elements.len != tuple_type.elements.len) {
                tc.addError(.invalid_pattern, t.span, "wrong number of tuple elements", .{});
                return;
            }
            for (t.elements, tuple_type.elements) |elem_pattern, elem_type| {
                checkPattern(tc, elem_pattern, elem_type);
            }
        },
        .or_pattern => |o| {
            for (o.alternatives) |alt| {
                checkPattern(tc, alt, expected_type);
            }
        },
        .guarded => |g| {
            checkPattern(tc, g.pattern, expected_type);
            const guard_type = tc.checkExpr(g.guard);
            if (!tc.isBoolType(guard_type)) {
                tc.addError(.type_mismatch, g.span, "guard must be bool", .{});
            }
        },
    }
}

fn checkVariantPattern(tc: anytype, v: *ast.VariantPattern, expected_type: Type) void {
    // Determine the type to match - either from pattern or from expected type
    var match_type = expected_type;

    if (v.type_expr) |type_expr| {
        // Pattern specifies a type - resolve it
        match_type = tc.resolveTypeExpr(type_expr) catch {
            tc.addError(.undefined_type, v.span, "unknown type in pattern", .{});
            return;
        };
    }

    // Handle Result type patterns: Ok(v), Err(e)
    if (match_type == .result) {
        const result_type = match_type.result;
        if (std.mem.eql(u8, v.variant_name, "Ok")) {
            if (v.payload) |payload_pattern| {
                checkPattern(tc, payload_pattern, result_type.ok_type);
            } else {
                tc.addError(.invalid_pattern, v.span, "Ok variant expects payload", .{});
            }
        } else if (std.mem.eql(u8, v.variant_name, "Err")) {
            if (v.payload) |payload_pattern| {
                checkPattern(tc, payload_pattern, result_type.err_type);
            } else {
                tc.addError(.invalid_pattern, v.span, "Err variant expects payload", .{});
            }
        } else {
            tc.addError(.undefined_variant, v.span, "unknown Result variant '{s}'", .{v.variant_name});
        }
        return;
    }

    // Handle Optional type patterns: Some(v), None
    if (match_type == .optional) {
        const inner_type = match_type.optional.*;
        if (std.mem.eql(u8, v.variant_name, "Some")) {
            if (v.payload) |payload_pattern| {
                checkPattern(tc, payload_pattern, inner_type);
            } else {
                tc.addError(.invalid_pattern, v.span, "Some variant expects payload", .{});
            }
        } else if (std.mem.eql(u8, v.variant_name, "None")) {
            if (v.payload != null) {
                tc.addError(.invalid_pattern, v.span, "None variant takes no payload", .{});
            }
        } else {
            tc.addError(.undefined_variant, v.span, "unknown Optional variant '{s}'", .{v.variant_name});
        }
        return;
    }

    // Handle regular enum types
    if (match_type != .enum_) {
        tc.addError(.invalid_pattern, v.span, "variant pattern requires enum type", .{});
        return;
    }

    const enum_def = match_type.enum_;

    // Find the variant
    var found_variant: ?types.EnumVariant = null;
    for (enum_def.variants) |variant| {
        if (std.mem.eql(u8, variant.name, v.variant_name)) {
            found_variant = variant;
            break;
        }
    }

    if (found_variant == null) {
        tc.addError(.undefined_variant, v.span, "unknown variant '{s}'", .{v.variant_name});
        return;
    }

    const variant = found_variant.?;

    // Check and bind payload pattern
    if (variant.payload) |payload| {
        if (v.payload) |payload_pattern| {
            switch (payload) {
                .tuple => |tuple_types| {
                    if (tuple_types.len == 1) {
                        // Single-element tuple: bind directly
                        checkPattern(tc, payload_pattern, tuple_types[0]);
                    } else {
                        // Multi-element tuple: expect tuple pattern
                        const payload_type = tc.type_builder.tupleType(tuple_types) catch tc.type_builder.unknownType();
                        checkPattern(tc, payload_pattern, payload_type);
                    }
                },
                .struct_ => |fields| {
                    // Build struct type for the payload
                    // For now, treat as unknown - struct patterns need more work
                    _ = fields;
                    checkPattern(tc, payload_pattern, tc.type_builder.unknownType());
                },
            }
        } else {
            tc.addError(.invalid_pattern, v.span, "variant '{s}' expects payload", .{v.variant_name});
        }
    } else {
        // Unit variant - no payload expected
        if (v.payload != null) {
            tc.addError(.invalid_pattern, v.span, "variant '{s}' takes no payload", .{v.variant_name});
        }
    }

    // If this is a generic enum from pattern, record monomorphization
    if (v.type_expr) |type_expr| {
        if (type_expr == .generic_apply and enum_def.type_params.len > 0) {
            const generic = type_expr.generic_apply;
            var type_args = std.ArrayListUnmanaged(Type){};
            defer type_args.deinit(tc.allocator);

            for (generic.args) |arg| {
                const resolved_arg = tc.resolveTypeExpr(arg) catch continue;
                type_args.append(tc.allocator, resolved_arg) catch {};
            }

            // Record enum monomorphization
            _ = tc.recordEnumMonomorphization(enum_def.name, enum_def, type_args.items) catch {};
        }
    }
}

pub fn checkLiteralPattern(tc: anytype, lit: ast.PatternLiteral) Type {
    return switch (lit.kind) {
        .int => tc.type_builder.i32Type(),
        .float => tc.type_builder.f64Type(),
        .string => tc.type_builder.stringType(),
        .char => tc.type_builder.charType(),
        .bool_ => tc.type_builder.boolType(),
    };
}

pub fn bindPattern(tc: anytype, pattern: ast.Pattern, t: Type) void {
    switch (pattern) {
        .wildcard => {},
        .literal => {},
        .binding => |bind| {
            // If type annotation is present, verify it matches the inferred type
            var actual_type = t;
            if (bind.type_annotation) |type_expr| {
                const declared_type = tc.resolveTypeExpr(type_expr) catch tc.type_builder.unknownType();
                if (!declared_type.eql(t)) {
                    tc.addError(.type_mismatch, bind.span, "declared type doesn't match inferred type", .{});
                }
                actual_type = declared_type;
            }
            tc.current_scope.define(.{
                .name = bind.name,
                .type_ = actual_type,
                .kind = .variable,
                .mutable = bind.mutable,
                .span = bind.span,
            }) catch {};
        },
        .variant => |v| {
            bindVariantPattern(tc, v, t);
        },
        .struct_pattern => |s| {
            if (t != .struct_) return;
            const struct_type = t.struct_;
            for (s.fields) |field| {
                // Find the field type from the struct definition
                var field_type: Type = tc.type_builder.unknownType();
                for (struct_type.fields) |sf| {
                    if (std.mem.eql(u8, sf.name, field.name)) {
                        field_type = sf.type_;
                        break;
                    }
                }
                if (field.pattern) |field_pattern| {
                    // Bind nested pattern with the field's type
                    bindPattern(tc, field_pattern, field_type);
                } else {
                    // Shorthand: field name becomes a binding (e.g., Point { x, y })
                    tc.current_scope.define(.{
                        .name = field.name,
                        .type_ = field_type,
                        .kind = .variable,
                        .mutable = false,
                        .span = field.span,
                    }) catch {};
                }
            }
        },
        .tuple_pattern => |tup| {
            if (t == .tuple) {
                const tuple_type = t.tuple;
                for (tup.elements, 0..) |elem_pattern, i| {
                    if (i < tuple_type.elements.len) {
                        bindPattern(tc, elem_pattern, tuple_type.elements[i]);
                    }
                }
            }
        },
        .or_pattern => |o| {
            // Bind variables from first alternative (all should have same bindings)
            if (o.alternatives.len > 0) {
                bindPattern(tc, o.alternatives[0], t);
            }
        },
        .guarded => |g| {
            bindPattern(tc, g.pattern, t);
        },
    }
}

fn bindVariantPattern(tc: anytype, v: *ast.VariantPattern, t: Type) void {
    // Determine the type to match
    var match_type = t;
    if (v.type_expr) |type_expr| {
        match_type = tc.resolveTypeExpr(type_expr) catch return;
    }

    // Handle Result type patterns: Ok(v), Err(e)
    if (match_type == .result) {
        const result_type = match_type.result;
        if (std.mem.eql(u8, v.variant_name, "Ok")) {
            if (v.payload) |payload_pattern| {
                bindPattern(tc, payload_pattern, result_type.ok_type);
            }
        } else if (std.mem.eql(u8, v.variant_name, "Err")) {
            if (v.payload) |payload_pattern| {
                bindPattern(tc, payload_pattern, result_type.err_type);
            }
        }
        return;
    }

    // Handle Optional type patterns: Some(v), None
    if (match_type == .optional) {
        const inner_type = match_type.optional.*;
        if (std.mem.eql(u8, v.variant_name, "Some")) {
            if (v.payload) |payload_pattern| {
                bindPattern(tc, payload_pattern, inner_type);
            }
        }
        // None has no payload to bind
        return;
    }

    // Handle regular enum types
    if (match_type != .enum_) return;
    const enum_def = match_type.enum_;

    // Find the variant
    var found_variant: ?types.EnumVariant = null;
    for (enum_def.variants) |variant| {
        if (std.mem.eql(u8, variant.name, v.variant_name)) {
            found_variant = variant;
            break;
        }
    }

    if (found_variant) |variant| {
        if (variant.payload) |payload| {
            if (v.payload) |payload_pattern| {
                switch (payload) {
                    .tuple => |tuple_types| {
                        if (tuple_types.len == 1) {
                            // Single-element: bind directly
                            bindPattern(tc, payload_pattern, tuple_types[0]);
                        } else {
                            // Multi-element: build tuple type
                            const payload_type = tc.type_builder.tupleType(tuple_types) catch return;
                            bindPattern(tc, payload_pattern, payload_type);
                        }
                    },
                    .struct_ => |fields| {
                        _ = fields;
                        // Struct payloads - handled by struct pattern binding
                        bindPattern(tc, payload_pattern, tc.type_builder.unknownType());
                    },
                }
            }
        }
    }
}
