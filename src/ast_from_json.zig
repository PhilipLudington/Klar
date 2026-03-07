//! Deserializes JSON (from `klar dump-ast`) back into `ast.Module`.
//!
//! This enables a selfhost frontend to emit JSON that the Zig backend
//! can consume via `klar build --ast-input file.json`.

const std = @import("std");
const ast = @import("ast.zig");
const Allocator = std.mem.Allocator;
const ObjectMap = std.json.ObjectMap;
const Value = std.json.Value;
const Array = std.json.Array;

pub const JsonAstError = error{
    InvalidJson,
    MissingField,
    UnknownKind,
    InvalidValue,
    OutOfMemory,
};

pub const dummy_span = ast.Span{ .start = 0, .end = 0, .line = 0, .column = 0 };

// ============================================================================
// Entry point
// ============================================================================

/// Deserialize a JSON string (produced by `klar dump-ast`) into an `ast.Module`.
/// All AST nodes are allocated in `arena`. The `allocator` is used only for
/// the temporary JSON parse tree.
pub fn moduleFromJson(allocator: Allocator, arena: Allocator, json: []const u8) !ast.Module {
    const parsed = std.json.parseFromSlice(Value, allocator, json, .{}) catch {
        return JsonAstError.InvalidJson;
    };
    defer parsed.deinit();

    const root = parsed.value;
    if (root != .object) return JsonAstError.InvalidJson;

    return buildModule(arena, root.object);
}

// ============================================================================
// Helpers
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

fn getObject(obj: ObjectMap, key: []const u8) ?ObjectMap {
    const v = obj.get(key) orelse return null;
    return switch (v) {
        .object => |o| o,
        else => null,
    };
}

/// Returns true if `key` is absent or explicitly set to JSON `null`.
/// Callers rely on this conflation for optional AST fields where both
/// "missing" and "null" mean "not present".
fn isNull(obj: ObjectMap, key: []const u8) bool {
    const v = obj.get(key) orelse return true;
    return v == .null;
}

fn dupeStr(arena: Allocator, s: []const u8) ![]const u8 {
    return arena.dupe(u8, s);
}

fn getInteger(obj: ObjectMap, key: []const u8) ?i64 {
    const v = obj.get(key) orelse return null;
    return switch (v) {
        .integer => |i| i,
        else => null,
    };
}

fn getFloat(obj: ObjectMap, key: []const u8) ?f64 {
    const v = obj.get(key) orelse return null;
    return switch (v) {
        .float => |f| f,
        .integer => |i| @as(f64, @floatFromInt(i)),
        else => null,
    };
}

/// Map a tag-name string back to a BinaryOp enum value.
fn parseBinaryOp(name: []const u8) !ast.BinaryOp {
    inline for (std.meta.fields(ast.BinaryOp)) |field| {
        if (std.mem.eql(u8, name, field.name)) {
            return @enumFromInt(field.value);
        }
    }
    return JsonAstError.UnknownKind;
}

/// Map a tag-name string back to a UnaryOp enum value.
fn parseUnaryOp(name: []const u8) !ast.UnaryOp {
    inline for (std.meta.fields(ast.UnaryOp)) |field| {
        if (std.mem.eql(u8, name, field.name)) {
            return @enumFromInt(field.value);
        }
    }
    return JsonAstError.UnknownKind;
}

/// Map a tag-name string back to a PostfixOp enum value.
fn parsePostfixOp(name: []const u8) !ast.PostfixOp {
    inline for (std.meta.fields(ast.PostfixOp)) |field| {
        if (std.mem.eql(u8, name, field.name)) {
            return @enumFromInt(field.value);
        }
    }
    return JsonAstError.UnknownKind;
}

// ============================================================================
// TypeExpr builders
// ============================================================================

fn buildTypeExpr(arena: Allocator, val: Value) anyerror!ast.TypeExpr {
    if (val != .object) return JsonAstError.InvalidValue;
    const obj = val.object;
    const kind = getString(obj, "kind") orelse return JsonAstError.MissingField;

    if (std.mem.eql(u8, kind, "named")) {
        const name = getString(obj, "name") orelse return JsonAstError.MissingField;
        return .{ .named = .{ .name = try dupeStr(arena, name), .span = dummy_span } };
    } else if (std.mem.eql(u8, kind, "array")) {
        const element = try buildTypeExpr(arena, obj.get("element") orelse return JsonAstError.MissingField);
        const size = try buildExpr(arena, obj.get("size") orelse return JsonAstError.MissingField);
        const node = try arena.create(ast.ArrayType);
        node.* = .{ .element = element, .size = size, .span = dummy_span };
        return .{ .array = node };
    } else if (std.mem.eql(u8, kind, "slice")) {
        const element = try buildTypeExpr(arena, obj.get("element") orelse return JsonAstError.MissingField);
        const node = try arena.create(ast.SliceType);
        node.* = .{ .element = element, .span = dummy_span };
        return .{ .slice = node };
    } else if (std.mem.eql(u8, kind, "tuple")) {
        const elems = getArray(obj, "elements") orelse return JsonAstError.MissingField;
        const elements = try arena.alloc(ast.TypeExpr, elems.items.len);
        for (elems.items, 0..) |e, i| {
            elements[i] = try buildTypeExpr(arena, e);
        }
        const node = try arena.create(ast.TupleType);
        node.* = .{ .elements = elements, .span = dummy_span };
        return .{ .tuple = node };
    } else if (std.mem.eql(u8, kind, "optional")) {
        const inner = try buildTypeExpr(arena, obj.get("inner") orelse return JsonAstError.MissingField);
        const node = try arena.create(ast.OptionalType);
        node.* = .{ .inner = inner, .span = dummy_span };
        return .{ .optional = node };
    } else if (std.mem.eql(u8, kind, "result")) {
        const ok_type = try buildTypeExpr(arena, obj.get("ok_type") orelse return JsonAstError.MissingField);
        const err_type = try buildTypeExpr(arena, obj.get("err_type") orelse return JsonAstError.MissingField);
        const node = try arena.create(ast.ResultType);
        node.* = .{ .ok_type = ok_type, .err_type = err_type, .span = dummy_span };
        return .{ .result = node };
    } else if (std.mem.eql(u8, kind, "function")) {
        const params_arr = getArray(obj, "params") orelse return JsonAstError.MissingField;
        const params = try arena.alloc(ast.TypeExpr, params_arr.items.len);
        for (params_arr.items, 0..) |p, i| {
            params[i] = try buildTypeExpr(arena, p);
        }
        const return_type = try buildTypeExpr(arena, obj.get("return_type") orelse return JsonAstError.MissingField);
        const node = try arena.create(ast.FunctionType);
        node.* = .{ .params = params, .return_type = return_type, .span = dummy_span };
        return .{ .function = node };
    } else if (std.mem.eql(u8, kind, "extern_function")) {
        const params_arr = getArray(obj, "params") orelse return JsonAstError.MissingField;
        const params = try arena.alloc(ast.TypeExpr, params_arr.items.len);
        for (params_arr.items, 0..) |p, i| {
            params[i] = try buildTypeExpr(arena, p);
        }
        const return_type = try buildTypeExpr(arena, obj.get("return_type") orelse return JsonAstError.MissingField);
        const node = try arena.create(ast.ExternFunctionType);
        node.* = .{ .params = params, .return_type = return_type, .span = dummy_span };
        return .{ .extern_function = node };
    } else if (std.mem.eql(u8, kind, "reference")) {
        const mutable = getBool(obj, "mutable") orelse false;
        const inner = try buildTypeExpr(arena, obj.get("inner") orelse return JsonAstError.MissingField);
        const node = try arena.create(ast.ReferenceType);
        node.* = .{ .inner = inner, .mutable = mutable, .span = dummy_span };
        return .{ .reference = node };
    } else if (std.mem.eql(u8, kind, "generic_apply")) {
        const base = try buildTypeExpr(arena, obj.get("base") orelse return JsonAstError.MissingField);
        const args_arr = getArray(obj, "args") orelse return JsonAstError.MissingField;
        const type_args = try arena.alloc(ast.TypeExpr, args_arr.items.len);
        for (args_arr.items, 0..) |a, i| {
            type_args[i] = try buildTypeExpr(arena, a);
        }
        const node = try arena.create(ast.GenericApply);
        node.* = .{ .base = base, .args = type_args, .span = dummy_span };
        return .{ .generic_apply = node };
    } else if (std.mem.eql(u8, kind, "qualified")) {
        const base = try buildTypeExpr(arena, obj.get("base") orelse return JsonAstError.MissingField);
        const member = getString(obj, "member") orelse return JsonAstError.MissingField;
        const node = try arena.create(ast.QualifiedType);
        node.* = .{ .base = base, .member = try dupeStr(arena, member), .span = dummy_span };
        return .{ .qualified = node };
    } else {
        return JsonAstError.UnknownKind;
    }
}

// ============================================================================
// Pattern builders
// ============================================================================

fn buildPattern(arena: Allocator, val: Value) anyerror!ast.Pattern {
    if (val != .object) return JsonAstError.InvalidValue;
    const obj = val.object;
    const kind = getString(obj, "kind") orelse return JsonAstError.MissingField;

    if (std.mem.eql(u8, kind, "wildcard")) {
        return .{ .wildcard = .{ .span = dummy_span } };
    } else if (std.mem.eql(u8, kind, "literal")) {
        const lit_kind = try buildLiteralKind(obj);
        // Dupe string literals into arena (JSON tree is freed after moduleFromJson returns)
        const final_kind: ast.Literal.Kind = switch (lit_kind) {
            .string => |s| .{ .string = try dupeStr(arena, s) },
            else => lit_kind,
        };
        return .{ .literal = .{ .kind = final_kind, .span = dummy_span } };
    } else if (std.mem.eql(u8, kind, "binding")) {
        const name = getString(obj, "name") orelse return JsonAstError.MissingField;
        const mutable = getBool(obj, "mutable") orelse false;
        const type_annotation = if (!isNull(obj, "type_annotation"))
            try buildTypeExpr(arena, obj.get("type_annotation").?)
        else
            null;
        return .{ .binding = .{
            .name = try dupeStr(arena, name),
            .mutable = mutable,
            .type_annotation = type_annotation,
            .span = dummy_span,
        } };
    } else if (std.mem.eql(u8, kind, "variant")) {
        const type_expr = if (!isNull(obj, "type_expr"))
            try buildTypeExpr(arena, obj.get("type_expr").?)
        else
            null;
        const variant_name = getString(obj, "variant_name") orelse return JsonAstError.MissingField;
        const payload = if (!isNull(obj, "payload"))
            try buildPattern(arena, obj.get("payload").?)
        else
            null;
        const node = try arena.create(ast.VariantPattern);
        node.* = .{
            .type_expr = type_expr,
            .variant_name = try dupeStr(arena, variant_name),
            .payload = payload,
            .span = dummy_span,
        };
        return .{ .variant = node };
    } else if (std.mem.eql(u8, kind, "struct_pattern")) {
        const type_name = getString(obj, "type_name");
        const fields_arr = getArray(obj, "fields") orelse return JsonAstError.MissingField;
        const fields = try arena.alloc(ast.StructFieldPattern, fields_arr.items.len);
        for (fields_arr.items, 0..) |f, i| {
            if (f != .object) return JsonAstError.InvalidValue;
            const fo = f.object;
            const fname = getString(fo, "name") orelse return JsonAstError.MissingField;
            const pattern = if (!isNull(fo, "pattern"))
                try buildPattern(arena, fo.get("pattern").?)
            else
                null;
            fields[i] = .{
                .name = try dupeStr(arena, fname),
                .pattern = pattern,
                .span = dummy_span,
            };
        }
        const node = try arena.create(ast.StructPattern);
        node.* = .{
            .type_name = if (type_name) |tn| try dupeStr(arena, tn) else null,
            .fields = fields,
            .span = dummy_span,
        };
        return .{ .struct_pattern = node };
    } else if (std.mem.eql(u8, kind, "tuple_pattern")) {
        const elems = getArray(obj, "elements") orelse return JsonAstError.MissingField;
        const elements = try arena.alloc(ast.Pattern, elems.items.len);
        for (elems.items, 0..) |e, i| {
            elements[i] = try buildPattern(arena, e);
        }
        const node = try arena.create(ast.TuplePattern);
        node.* = .{ .elements = elements, .span = dummy_span };
        return .{ .tuple_pattern = node };
    } else if (std.mem.eql(u8, kind, "or_pattern")) {
        const alts = getArray(obj, "alternatives") orelse return JsonAstError.MissingField;
        const alternatives = try arena.alloc(ast.Pattern, alts.items.len);
        for (alts.items, 0..) |a, i| {
            alternatives[i] = try buildPattern(arena, a);
        }
        const node = try arena.create(ast.OrPattern);
        node.* = .{ .alternatives = alternatives, .span = dummy_span };
        return .{ .or_pattern = node };
    } else if (std.mem.eql(u8, kind, "guarded")) {
        const pattern = try buildPattern(arena, obj.get("pattern") orelse return JsonAstError.MissingField);
        const guard = try buildExpr(arena, obj.get("guard") orelse return JsonAstError.MissingField);
        const node = try arena.create(ast.GuardedPattern);
        node.* = .{ .pattern = pattern, .guard = guard, .span = dummy_span };
        return .{ .guarded = node };
    } else {
        return JsonAstError.UnknownKind;
    }
}

// ============================================================================
// Literal helper (shared by expr literal and pattern literal)
// ============================================================================

fn buildLiteralKind(obj: ObjectMap) anyerror!ast.Literal.Kind {
    const lit_type = getString(obj, "type") orelse return JsonAstError.MissingField;

    if (std.mem.eql(u8, lit_type, "int")) {
        // Int values are JSON-escaped strings (to handle i128)
        const val_str = getString(obj, "value") orelse return JsonAstError.MissingField;
        const val = std.fmt.parseInt(i128, val_str, 10) catch return JsonAstError.InvalidValue;
        return .{ .int = val };
    } else if (std.mem.eql(u8, lit_type, "float")) {
        const fval = getFloat(obj, "value") orelse return JsonAstError.MissingField;
        return .{ .float = fval };
    } else if (std.mem.eql(u8, lit_type, "string")) {
        const sval = getString(obj, "value") orelse return JsonAstError.MissingField;
        // Note: sval points into the JSON parse tree. Callers (buildExpr, buildPattern)
        // are responsible for duping into the arena before the JSON tree is freed.
        return .{ .string = sval };
    } else if (std.mem.eql(u8, lit_type, "char")) {
        const cval = getInteger(obj, "value") orelse return JsonAstError.MissingField;
        if (cval < 0 or cval > 0x10FFFF) return JsonAstError.InvalidValue;
        return .{ .char = @intCast(cval) };
    } else if (std.mem.eql(u8, lit_type, "bool")) {
        const bval = getBool(obj, "value") orelse return JsonAstError.MissingField;
        return .{ .bool_ = bval };
    } else {
        return JsonAstError.UnknownKind;
    }
}

// ============================================================================
// Expression builders
// ============================================================================

fn buildExpr(arena: Allocator, val: Value) anyerror!ast.Expr {
    if (val != .object) return JsonAstError.InvalidValue;
    const obj = val.object;
    const kind = getString(obj, "kind") orelse return JsonAstError.MissingField;

    if (std.mem.eql(u8, kind, "literal")) {
        const lit_kind = try buildLiteralKind(obj);
        // For string literals, dupe into arena since the JSON tree will be freed
        const final_kind: ast.Literal.Kind = switch (lit_kind) {
            .string => |s| .{ .string = try dupeStr(arena, s) },
            else => lit_kind,
        };
        return .{ .literal = .{ .kind = final_kind, .span = dummy_span } };
    } else if (std.mem.eql(u8, kind, "identifier")) {
        const name = getString(obj, "name") orelse return JsonAstError.MissingField;
        return .{ .identifier = .{ .name = try dupeStr(arena, name), .span = dummy_span } };
    } else if (std.mem.eql(u8, kind, "binary")) {
        const op_name = getString(obj, "op") orelse return JsonAstError.MissingField;
        const op = try parseBinaryOp(op_name);
        const left = try buildExpr(arena, obj.get("left") orelse return JsonAstError.MissingField);
        const right = try buildExpr(arena, obj.get("right") orelse return JsonAstError.MissingField);
        const node = try arena.create(ast.Binary);
        node.* = .{ .left = left, .op = op, .right = right, .span = dummy_span };
        return .{ .binary = node };
    } else if (std.mem.eql(u8, kind, "unary")) {
        const op_name = getString(obj, "op") orelse return JsonAstError.MissingField;
        const op = try parseUnaryOp(op_name);
        const operand = try buildExpr(arena, obj.get("operand") orelse return JsonAstError.MissingField);
        const node = try arena.create(ast.Unary);
        node.* = .{ .op = op, .operand = operand, .span = dummy_span };
        return .{ .unary = node };
    } else if (std.mem.eql(u8, kind, "postfix")) {
        const op_name = getString(obj, "op") orelse return JsonAstError.MissingField;
        const op = try parsePostfixOp(op_name);
        const operand = try buildExpr(arena, obj.get("operand") orelse return JsonAstError.MissingField);
        const node = try arena.create(ast.Postfix);
        node.* = .{ .operand = operand, .op = op, .span = dummy_span };
        return .{ .postfix = node };
    } else if (std.mem.eql(u8, kind, "call")) {
        const callee = try buildExpr(arena, obj.get("callee") orelse return JsonAstError.MissingField);
        const args_arr = getArray(obj, "args") orelse return JsonAstError.MissingField;
        const call_args = try arena.alloc(ast.Expr, args_arr.items.len);
        for (args_arr.items, 0..) |a, i| {
            call_args[i] = try buildExpr(arena, a);
        }
        const type_args = if (getArray(obj, "type_args")) |ta| blk: {
            const tas = try arena.alloc(ast.TypeExpr, ta.items.len);
            for (ta.items, 0..) |t, i| {
                tas[i] = try buildTypeExpr(arena, t);
            }
            break :blk tas;
        } else null;
        const node = try arena.create(ast.Call);
        node.* = .{ .callee = callee, .args = call_args, .type_args = type_args, .span = dummy_span };
        return .{ .call = node };
    } else if (std.mem.eql(u8, kind, "index")) {
        const object = try buildExpr(arena, obj.get("object") orelse return JsonAstError.MissingField);
        const index = try buildExpr(arena, obj.get("index") orelse return JsonAstError.MissingField);
        const node = try arena.create(ast.Index);
        node.* = .{ .object = object, .index = index, .span = dummy_span };
        return .{ .index = node };
    } else if (std.mem.eql(u8, kind, "field")) {
        const object = try buildExpr(arena, obj.get("object") orelse return JsonAstError.MissingField);
        const field_name = getString(obj, "field_name") orelse return JsonAstError.MissingField;
        const node = try arena.create(ast.Field);
        node.* = .{ .object = object, .field_name = try dupeStr(arena, field_name), .span = dummy_span };
        return .{ .field = node };
    } else if (std.mem.eql(u8, kind, "method_call")) {
        const object = try buildExpr(arena, obj.get("object") orelse return JsonAstError.MissingField);
        const method_name = getString(obj, "method_name") orelse return JsonAstError.MissingField;
        const type_args = if (getArray(obj, "type_args")) |ta| blk: {
            const tas = try arena.alloc(ast.TypeExpr, ta.items.len);
            for (ta.items, 0..) |t, i| {
                tas[i] = try buildTypeExpr(arena, t);
            }
            break :blk tas;
        } else null;
        const args_arr = getArray(obj, "args") orelse return JsonAstError.MissingField;
        const call_args = try arena.alloc(ast.Expr, args_arr.items.len);
        for (args_arr.items, 0..) |a, i| {
            call_args[i] = try buildExpr(arena, a);
        }
        const node = try arena.create(ast.MethodCall);
        node.* = .{
            .object = object,
            .method_name = try dupeStr(arena, method_name),
            .type_args = type_args,
            .args = call_args,
            .span = dummy_span,
        };
        return .{ .method_call = node };
    } else if (std.mem.eql(u8, kind, "block")) {
        const body_val = obj.get("body") orelse return JsonAstError.MissingField;
        const block = try buildBlock(arena, body_val);
        return .{ .block = block };
    } else if (std.mem.eql(u8, kind, "closure")) {
        const params_arr = getArray(obj, "params") orelse return JsonAstError.MissingField;
        const params = try arena.alloc(ast.ClosureParam, params_arr.items.len);
        for (params_arr.items, 0..) |p, i| {
            if (p != .object) return JsonAstError.InvalidValue;
            const po = p.object;
            const pname = getString(po, "name") orelse return JsonAstError.MissingField;
            const ptype = try buildTypeExpr(arena, po.get("type") orelse return JsonAstError.MissingField);
            params[i] = .{ .name = try dupeStr(arena, pname), .type_ = ptype, .span = dummy_span };
        }
        const return_type = try buildTypeExpr(arena, obj.get("return_type") orelse return JsonAstError.MissingField);
        const body = try buildExpr(arena, obj.get("body") orelse return JsonAstError.MissingField);
        const node = try arena.create(ast.Closure);
        node.* = .{ .params = params, .return_type = return_type, .body = body, .span = dummy_span };
        return .{ .closure = node };
    } else if (std.mem.eql(u8, kind, "range")) {
        const inclusive = getBool(obj, "inclusive") orelse false;
        const start = if (!isNull(obj, "start"))
            try buildExpr(arena, obj.get("start").?)
        else
            null;
        const end = if (!isNull(obj, "end"))
            try buildExpr(arena, obj.get("end").?)
        else
            null;
        const node = try arena.create(ast.Range);
        node.* = .{ .start = start, .end = end, .inclusive = inclusive, .span = dummy_span };
        return .{ .range = node };
    } else if (std.mem.eql(u8, kind, "struct_literal")) {
        const type_name = if (!isNull(obj, "type_name"))
            try buildTypeExpr(arena, obj.get("type_name").?)
        else
            null;
        const fields_arr = getArray(obj, "fields") orelse return JsonAstError.MissingField;
        const fields = try arena.alloc(ast.StructFieldInit, fields_arr.items.len);
        for (fields_arr.items, 0..) |f, i| {
            if (f != .object) return JsonAstError.InvalidValue;
            const fo = f.object;
            const fname = getString(fo, "name") orelse return JsonAstError.MissingField;
            const fval = try buildExpr(arena, fo.get("value") orelse return JsonAstError.MissingField);
            fields[i] = .{ .name = try dupeStr(arena, fname), .value = fval, .span = dummy_span };
        }
        const spread = if (!isNull(obj, "spread"))
            try buildExpr(arena, obj.get("spread").?)
        else
            null;
        const node = try arena.create(ast.StructLiteral);
        node.* = .{ .type_name = type_name, .fields = fields, .spread = spread, .span = dummy_span };
        return .{ .struct_literal = node };
    } else if (std.mem.eql(u8, kind, "array_literal")) {
        const elems = getArray(obj, "elements") orelse return JsonAstError.MissingField;
        const elements = try arena.alloc(ast.Expr, elems.items.len);
        for (elems.items, 0..) |e, i| {
            elements[i] = try buildExpr(arena, e);
        }
        const node = try arena.create(ast.ArrayLiteral);
        node.* = .{ .elements = elements, .span = dummy_span };
        return .{ .array_literal = node };
    } else if (std.mem.eql(u8, kind, "tuple_literal")) {
        const elems = getArray(obj, "elements") orelse return JsonAstError.MissingField;
        const elements = try arena.alloc(ast.Expr, elems.items.len);
        for (elems.items, 0..) |e, i| {
            elements[i] = try buildExpr(arena, e);
        }
        const node = try arena.create(ast.TupleLiteral);
        node.* = .{ .elements = elements, .span = dummy_span };
        return .{ .tuple_literal = node };
    } else if (std.mem.eql(u8, kind, "type_cast")) {
        const truncating = getBool(obj, "truncating") orelse false;
        const target_type = try buildTypeExpr(arena, obj.get("target_type") orelse return JsonAstError.MissingField);
        const expr = try buildExpr(arena, obj.get("expr") orelse return JsonAstError.MissingField);
        const node = try arena.create(ast.TypeCast);
        node.* = .{ .expr = expr, .target_type = target_type, .truncating = truncating, .span = dummy_span };
        return .{ .type_cast = node };
    } else if (std.mem.eql(u8, kind, "grouped")) {
        const expr = try buildExpr(arena, obj.get("expr") orelse return JsonAstError.MissingField);
        const node = try arena.create(ast.Grouped);
        node.* = .{ .expr = expr, .span = dummy_span };
        return .{ .grouped = node };
    } else if (std.mem.eql(u8, kind, "interpolated_string")) {
        const parts_arr = getArray(obj, "parts") orelse return JsonAstError.MissingField;
        const parts = try arena.alloc(ast.InterpolatedPart, parts_arr.items.len);
        for (parts_arr.items, 0..) |p, i| {
            if (p != .object) return JsonAstError.InvalidValue;
            const po = p.object;
            const part_kind = getString(po, "kind") orelse return JsonAstError.MissingField;
            if (std.mem.eql(u8, part_kind, "string")) {
                const sv = getString(po, "value") orelse return JsonAstError.MissingField;
                parts[i] = .{ .string = try dupeStr(arena, sv) };
            } else if (std.mem.eql(u8, part_kind, "expr")) {
                const ev = try buildExpr(arena, po.get("value") orelse return JsonAstError.MissingField);
                parts[i] = .{ .expr = ev };
            } else {
                return JsonAstError.UnknownKind;
            }
        }
        const node = try arena.create(ast.InterpolatedString);
        node.* = .{ .parts = parts, .span = dummy_span };
        return .{ .interpolated_string = node };
    } else if (std.mem.eql(u8, kind, "enum_literal")) {
        const enum_type = try buildTypeExpr(arena, obj.get("enum_type") orelse return JsonAstError.MissingField);
        const variant_name = getString(obj, "variant_name") orelse return JsonAstError.MissingField;
        const payload_arr = getArray(obj, "payload") orelse return JsonAstError.MissingField;
        const payload = try arena.alloc(ast.Expr, payload_arr.items.len);
        for (payload_arr.items, 0..) |p, i| {
            payload[i] = try buildExpr(arena, p);
        }
        const node = try arena.create(ast.EnumLiteral);
        node.* = .{
            .enum_type = enum_type,
            .variant_name = try dupeStr(arena, variant_name),
            .payload = payload,
            .span = dummy_span,
        };
        return .{ .enum_literal = node };
    } else if (std.mem.eql(u8, kind, "comptime_block")) {
        const body = try buildBlock(arena, obj.get("body") orelse return JsonAstError.MissingField);
        const node = try arena.create(ast.ComptimeBlock);
        node.* = .{ .body = body, .span = dummy_span };
        return .{ .comptime_block = node };
    } else if (std.mem.eql(u8, kind, "builtin_call")) {
        const name = getString(obj, "name") orelse return JsonAstError.MissingField;
        const args_arr = getArray(obj, "args") orelse return JsonAstError.MissingField;
        const builtin_args = try arena.alloc(ast.BuiltinArg, args_arr.items.len);
        for (args_arr.items, 0..) |a, i| {
            if (a != .object) return JsonAstError.InvalidValue;
            const ao = a.object;
            const arg_kind = getString(ao, "kind") orelse return JsonAstError.MissingField;
            if (std.mem.eql(u8, arg_kind, "type")) {
                builtin_args[i] = .{ .type_arg = try buildTypeExpr(arena, ao.get("value") orelse return JsonAstError.MissingField) };
            } else {
                builtin_args[i] = .{ .expr_arg = try buildExpr(arena, ao.get("value") orelse return JsonAstError.MissingField) };
            }
        }
        const node = try arena.create(ast.BuiltinCall);
        node.* = .{ .name = try dupeStr(arena, name), .args = builtin_args, .span = dummy_span };
        return .{ .builtin_call = node };
    } else if (std.mem.eql(u8, kind, "unsafe_block")) {
        const body = try buildBlock(arena, obj.get("body") orelse return JsonAstError.MissingField);
        const node = try arena.create(ast.UnsafeBlock);
        node.* = .{ .body = body, .span = dummy_span };
        return .{ .unsafe_block = node };
    } else if (std.mem.eql(u8, kind, "out_arg")) {
        const name = getString(obj, "name") orelse return JsonAstError.MissingField;
        const node = try arena.create(ast.OutArg);
        node.* = .{ .name = try dupeStr(arena, name), .span = dummy_span };
        return .{ .out_arg = node };
    } else {
        return JsonAstError.UnknownKind;
    }
}

// ============================================================================
// Block builder
// ============================================================================

pub fn buildBlock(arena: Allocator, val: Value) anyerror!*ast.Block {
    if (val != .object) return JsonAstError.InvalidValue;
    const obj = val.object;
    const stmts_arr = getArray(obj, "statements") orelse return JsonAstError.MissingField;
    const statements = try arena.alloc(ast.Stmt, stmts_arr.items.len);
    for (stmts_arr.items, 0..) |s, i| {
        statements[i] = try buildStmt(arena, s);
    }
    const final_expr = if (!isNull(obj, "final_expr"))
        try buildExpr(arena, obj.get("final_expr").?)
    else
        null;
    const block = try arena.create(ast.Block);
    block.* = .{ .statements = statements, .final_expr = final_expr, .span = dummy_span };
    return block;
}

// ============================================================================
// Statement builders
// ============================================================================

fn buildStmt(arena: Allocator, val: Value) anyerror!ast.Stmt {
    if (val != .object) return JsonAstError.InvalidValue;
    const obj = val.object;
    const kind = getString(obj, "kind") orelse return JsonAstError.MissingField;

    if (std.mem.eql(u8, kind, "let_decl")) {
        const name = getString(obj, "name") orelse return JsonAstError.MissingField;
        const is_shadow = getBool(obj, "is_shadow") orelse false;
        const type_ = try buildTypeExpr(arena, obj.get("type") orelse return JsonAstError.MissingField);
        const value = try buildExpr(arena, obj.get("value") orelse return JsonAstError.MissingField);
        const node = try arena.create(ast.LetDecl);
        node.* = .{ .name = try dupeStr(arena, name), .type_ = type_, .value = value, .is_shadow = is_shadow, .span = dummy_span };
        return .{ .let_decl = node };
    } else if (std.mem.eql(u8, kind, "var_decl")) {
        const name = getString(obj, "name") orelse return JsonAstError.MissingField;
        const is_shadow = getBool(obj, "is_shadow") orelse false;
        const type_ = try buildTypeExpr(arena, obj.get("type") orelse return JsonAstError.MissingField);
        const value = try buildExpr(arena, obj.get("value") orelse return JsonAstError.MissingField);
        const node = try arena.create(ast.VarDecl);
        node.* = .{ .name = try dupeStr(arena, name), .type_ = type_, .value = value, .is_shadow = is_shadow, .span = dummy_span };
        return .{ .var_decl = node };
    } else if (std.mem.eql(u8, kind, "assignment")) {
        const op_name = getString(obj, "op") orelse return JsonAstError.MissingField;
        const op = try parseBinaryOp(op_name);
        const target = try buildExpr(arena, obj.get("target") orelse return JsonAstError.MissingField);
        const value = try buildExpr(arena, obj.get("value") orelse return JsonAstError.MissingField);
        const node = try arena.create(ast.Assignment);
        node.* = .{ .target = target, .op = op, .value = value, .span = dummy_span };
        return .{ .assignment = node };
    } else if (std.mem.eql(u8, kind, "expr_stmt")) {
        const expr = try buildExpr(arena, obj.get("expr") orelse return JsonAstError.MissingField);
        const node = try arena.create(ast.ExprStmt);
        node.* = .{ .expr = expr, .span = dummy_span };
        return .{ .expr_stmt = node };
    } else if (std.mem.eql(u8, kind, "return_stmt")) {
        const value = if (!isNull(obj, "value"))
            try buildExpr(arena, obj.get("value").?)
        else
            null;
        const node = try arena.create(ast.ReturnStmt);
        node.* = .{ .value = value, .span = dummy_span };
        return .{ .return_stmt = node };
    } else if (std.mem.eql(u8, kind, "break_stmt")) {
        const value = if (!isNull(obj, "value"))
            try buildExpr(arena, obj.get("value").?)
        else
            null;
        const node = try arena.create(ast.BreakStmt);
        node.* = .{ .value = value, .span = dummy_span };
        return .{ .break_stmt = node };
    } else if (std.mem.eql(u8, kind, "continue_stmt")) {
        const node = try arena.create(ast.ContinueStmt);
        node.* = .{ .span = dummy_span };
        return .{ .continue_stmt = node };
    } else if (std.mem.eql(u8, kind, "for_loop")) {
        const pattern = try buildPattern(arena, obj.get("pattern") orelse return JsonAstError.MissingField);
        const iterable = try buildExpr(arena, obj.get("iterable") orelse return JsonAstError.MissingField);
        const body = try buildBlock(arena, obj.get("body") orelse return JsonAstError.MissingField);
        const node = try arena.create(ast.ForLoop);
        node.* = .{ .pattern = pattern, .iterable = iterable, .body = body, .span = dummy_span };
        return .{ .for_loop = node };
    } else if (std.mem.eql(u8, kind, "while_loop")) {
        const condition = try buildExpr(arena, obj.get("condition") orelse return JsonAstError.MissingField);
        const body = try buildBlock(arena, obj.get("body") orelse return JsonAstError.MissingField);
        const node = try arena.create(ast.WhileLoop);
        node.* = .{ .condition = condition, .body = body, .span = dummy_span };
        return .{ .while_loop = node };
    } else if (std.mem.eql(u8, kind, "loop_stmt")) {
        const body = try buildBlock(arena, obj.get("body") orelse return JsonAstError.MissingField);
        const node = try arena.create(ast.LoopStmt);
        node.* = .{ .body = body, .span = dummy_span };
        return .{ .loop_stmt = node };
    } else if (std.mem.eql(u8, kind, "if_stmt")) {
        return .{ .if_stmt = try buildIfStmt(arena, obj) };
    } else if (std.mem.eql(u8, kind, "match_stmt")) {
        const subject = try buildExpr(arena, obj.get("subject") orelse return JsonAstError.MissingField);
        const arms_arr = getArray(obj, "arms") orelse return JsonAstError.MissingField;
        const arms = try arena.alloc(ast.MatchArmStmt, arms_arr.items.len);
        for (arms_arr.items, 0..) |a, i| {
            if (a != .object) return JsonAstError.InvalidValue;
            const ao = a.object;
            const pattern = try buildPattern(arena, ao.get("pattern") orelse return JsonAstError.MissingField);
            const guard = if (!isNull(ao, "guard"))
                try buildExpr(arena, ao.get("guard").?)
            else
                null;
            const body = try buildBlock(arena, ao.get("body") orelse return JsonAstError.MissingField);
            arms[i] = .{ .pattern = pattern, .guard = guard, .body = body, .span = dummy_span };
        }
        const node = try arena.create(ast.MatchStmt);
        node.* = .{ .subject = subject, .arms = arms, .span = dummy_span };
        return .{ .match_stmt = node };
    } else {
        return JsonAstError.UnknownKind;
    }
}

fn buildIfStmt(arena: Allocator, obj: ObjectMap) anyerror!*ast.IfStmt {
    const condition = try buildExpr(arena, obj.get("condition") orelse return JsonAstError.MissingField);
    const then_branch = try buildBlock(arena, obj.get("then_branch") orelse return JsonAstError.MissingField);

    const else_branch: ?*ast.ElseBranch = if (!isNull(obj, "else_branch")) blk: {
        const eb_val = obj.get("else_branch").?;
        if (eb_val != .object) return JsonAstError.InvalidValue;
        const eb_obj = eb_val.object;
        const eb_kind = getString(eb_obj, "kind") orelse return JsonAstError.MissingField;
        const eb = try arena.create(ast.ElseBranch);
        if (std.mem.eql(u8, eb_kind, "block")) {
            // Else block: {"kind":"block","body":{...}}
            eb.* = .{ .block = try buildBlock(arena, eb_obj.get("body") orelse return JsonAstError.MissingField) };
        } else if (std.mem.eql(u8, eb_kind, "if_stmt")) {
            // Else-if chain
            eb.* = .{ .if_stmt = try buildIfStmt(arena, eb_obj) };
        } else {
            return JsonAstError.UnknownKind;
        }
        break :blk eb;
    } else null;

    const node = try arena.create(ast.IfStmt);
    node.* = .{ .condition = condition, .then_branch = then_branch, .else_branch = else_branch, .span = dummy_span };
    return node;
}

// ============================================================================
// Declaration builders
// ============================================================================

fn buildTypeParams(arena: Allocator, obj: ObjectMap) anyerror![]const ast.TypeParam {
    const arr = getArray(obj, "type_params") orelse return &.{};
    const result = try arena.alloc(ast.TypeParam, arr.items.len);
    for (arr.items, 0..) |tp, i| {
        if (tp != .object) return JsonAstError.InvalidValue;
        const tpo = tp.object;
        const name = getString(tpo, "name") orelse return JsonAstError.MissingField;
        const bounds_arr = getArray(tpo, "bounds") orelse return JsonAstError.MissingField;
        const bounds = try arena.alloc(ast.TypeExpr, bounds_arr.items.len);
        for (bounds_arr.items, 0..) |b, j| {
            bounds[j] = try buildTypeExpr(arena, b);
        }
        result[i] = .{ .name = try dupeStr(arena, name), .bounds = bounds, .span = dummy_span };
    }
    return result;
}

fn buildFunctionParams(arena: Allocator, obj: ObjectMap) anyerror![]const ast.FunctionParam {
    const arr = getArray(obj, "params") orelse return &.{};
    const result = try arena.alloc(ast.FunctionParam, arr.items.len);
    for (arr.items, 0..) |p, i| {
        if (p != .object) return JsonAstError.InvalidValue;
        const po = p.object;
        const name = getString(po, "name") orelse return JsonAstError.MissingField;
        const type_ = try buildTypeExpr(arena, po.get("type") orelse return JsonAstError.MissingField);
        const is_comptime = getBool(po, "is_comptime") orelse false;
        const is_out = getBool(po, "is_out") orelse false;
        const default_value = if (!isNull(po, "default_value"))
            try buildExpr(arena, po.get("default_value").?)
        else
            null;
        result[i] = .{
            .name = try dupeStr(arena, name),
            .type_ = type_,
            .default_value = default_value,
            .is_comptime = is_comptime,
            .is_out = is_out,
            .span = dummy_span,
        };
    }
    return result;
}

fn buildWhereClause(arena: Allocator, val: Value) anyerror!?[]const ast.WhereConstraint {
    if (val == .null) return null;
    if (val != .array) return JsonAstError.InvalidValue;
    const arr = val.array;
    const result = try arena.alloc(ast.WhereConstraint, arr.items.len);
    for (arr.items, 0..) |c, i| {
        if (c != .object) return JsonAstError.InvalidValue;
        const co = c.object;
        const type_param = getString(co, "type_param") orelse return JsonAstError.MissingField;
        const bounds_arr = getArray(co, "bounds") orelse return JsonAstError.MissingField;
        const bounds = try arena.alloc(ast.TypeExpr, bounds_arr.items.len);
        for (bounds_arr.items, 0..) |b, j| {
            bounds[j] = try buildTypeExpr(arena, b);
        }
        result[i] = .{
            .type_param = try dupeStr(arena, type_param),
            .bounds = bounds,
            .span = dummy_span,
        };
    }
    return result;
}

fn buildFunctionDecl(arena: Allocator, obj: ObjectMap) anyerror!ast.FunctionDecl {
    const name = getString(obj, "name") orelse return JsonAstError.MissingField;
    const is_pub = getBool(obj, "is_pub") orelse false;
    const is_async = getBool(obj, "is_async") orelse false;
    const is_comptime = getBool(obj, "is_comptime") orelse false;
    const is_unsafe = getBool(obj, "is_unsafe") orelse false;
    const is_extern = getBool(obj, "is_extern") orelse false;
    const is_variadic = getBool(obj, "is_variadic") orelse false;
    const type_params = try buildTypeParams(arena, obj);
    const params = try buildFunctionParams(arena, obj);
    const return_type = if (!isNull(obj, "return_type"))
        try buildTypeExpr(arena, obj.get("return_type").?)
    else
        null;
    const where_clause = try buildWhereClause(arena, obj.get("where_clause") orelse .null);
    const body = if (!isNull(obj, "body"))
        try buildBlock(arena, obj.get("body").?)
    else
        null;
    const meta = try buildMetaAnnotations(arena, obj);

    return .{
        .name = try dupeStr(arena, name),
        .type_params = type_params,
        .params = params,
        .return_type = return_type,
        .where_clause = where_clause,
        .body = body,
        .is_pub = is_pub,
        .is_async = is_async,
        .is_comptime = is_comptime,
        .is_unsafe = is_unsafe,
        .is_extern = is_extern,
        .is_variadic = is_variadic,
        .span = dummy_span,
        .meta = meta,
    };
}

fn buildStructField(arena: Allocator, val: Value) anyerror!ast.StructField {
    if (val != .object) return JsonAstError.InvalidValue;
    const obj = val.object;
    const name = getString(obj, "name") orelse return JsonAstError.MissingField;
    const type_ = try buildTypeExpr(arena, obj.get("type") orelse return JsonAstError.MissingField);
    const is_pub = getBool(obj, "is_pub") orelse false;
    const meta = try buildMetaAnnotations(arena, obj);
    return .{
        .name = try dupeStr(arena, name),
        .type_ = type_,
        .is_pub = is_pub,
        .span = dummy_span,
        .meta = meta,
    };
}

fn buildEnumVariant(arena: Allocator, val: Value) anyerror!ast.EnumVariant {
    if (val != .object) return JsonAstError.InvalidValue;
    const obj = val.object;
    const name = getString(obj, "name") orelse return JsonAstError.MissingField;

    const value: ?i128 = if (getInteger(obj, "value")) |v| @intCast(v) else null;

    const payload: ?ast.VariantPayload = if (!isNull(obj, "payload")) blk: {
        const pval = obj.get("payload").?;
        if (pval != .object) return JsonAstError.InvalidValue;
        const po = pval.object;
        const pk = getString(po, "kind") orelse return JsonAstError.MissingField;
        if (std.mem.eql(u8, pk, "tuple")) {
            const types_arr = getArray(po, "types") orelse return JsonAstError.MissingField;
            const types = try arena.alloc(ast.TypeExpr, types_arr.items.len);
            for (types_arr.items, 0..) |t, i| {
                types[i] = try buildTypeExpr(arena, t);
            }
            break :blk .{ .tuple = types };
        } else if (std.mem.eql(u8, pk, "struct")) {
            const fields_arr = getArray(po, "fields") orelse return JsonAstError.MissingField;
            const fields = try arena.alloc(ast.StructField, fields_arr.items.len);
            for (fields_arr.items, 0..) |f, i| {
                fields[i] = try buildStructField(arena, f);
            }
            break :blk .{ .struct_ = fields };
        } else {
            return JsonAstError.UnknownKind;
        }
    } else null;

    const meta = try buildMetaAnnotations(arena, obj);

    return .{
        .name = try dupeStr(arena, name),
        .payload = payload,
        .value = value,
        .span = dummy_span,
        .meta = meta,
    };
}

fn buildDecl(arena: Allocator, val: Value) anyerror!ast.Decl {
    if (val != .object) return JsonAstError.InvalidValue;
    const obj = val.object;
    const kind = getString(obj, "kind") orelse return JsonAstError.MissingField;

    if (std.mem.eql(u8, kind, "function")) {
        const func = try buildFunctionDecl(arena, obj);
        const node = try arena.create(ast.FunctionDecl);
        node.* = func;
        return .{ .function = node };
    } else if (std.mem.eql(u8, kind, "test_decl")) {
        const name = getString(obj, "name") orelse return JsonAstError.MissingField;
        const body = try buildBlock(arena, obj.get("body") orelse return JsonAstError.MissingField);
        const meta = try buildMetaAnnotations(arena, obj);
        const node = try arena.create(ast.TestDecl);
        node.* = .{ .name = try dupeStr(arena, name), .body = body, .span = dummy_span, .meta = meta };
        return .{ .test_decl = node };
    } else if (std.mem.eql(u8, kind, "struct_decl")) {
        const name = getString(obj, "name") orelse return JsonAstError.MissingField;
        const is_pub = getBool(obj, "is_pub") orelse false;
        const is_extern = getBool(obj, "is_extern") orelse false;
        const is_packed = getBool(obj, "is_packed") orelse false;
        const type_params = try buildTypeParams(arena, obj);
        const fields_arr = getArray(obj, "fields") orelse return JsonAstError.MissingField;
        const fields = try arena.alloc(ast.StructField, fields_arr.items.len);
        for (fields_arr.items, 0..) |f, i| {
            fields[i] = try buildStructField(arena, f);
        }
        const traits_arr = getArray(obj, "traits") orelse return JsonAstError.MissingField;
        const traits = try arena.alloc(ast.TypeExpr, traits_arr.items.len);
        for (traits_arr.items, 0..) |t, i| {
            traits[i] = try buildTypeExpr(arena, t);
        }
        const meta = try buildMetaAnnotations(arena, obj);
        const node = try arena.create(ast.StructDecl);
        node.* = .{
            .name = try dupeStr(arena, name),
            .type_params = type_params,
            .fields = fields,
            .traits = traits,
            .is_pub = is_pub,
            .is_extern = is_extern,
            .is_packed = is_packed,
            .span = dummy_span,
            .meta = meta,
        };
        return .{ .struct_decl = node };
    } else if (std.mem.eql(u8, kind, "enum_decl")) {
        const name = getString(obj, "name") orelse return JsonAstError.MissingField;
        const is_pub = getBool(obj, "is_pub") orelse false;
        const is_extern = getBool(obj, "is_extern") orelse false;
        const type_params = try buildTypeParams(arena, obj);
        const repr_type = if (!isNull(obj, "repr_type"))
            try buildTypeExpr(arena, obj.get("repr_type").?)
        else
            null;
        const variants_arr = getArray(obj, "variants") orelse return JsonAstError.MissingField;
        const variants = try arena.alloc(ast.EnumVariant, variants_arr.items.len);
        for (variants_arr.items, 0..) |v, i| {
            variants[i] = try buildEnumVariant(arena, v);
        }
        const meta = try buildMetaAnnotations(arena, obj);
        const node = try arena.create(ast.EnumDecl);
        node.* = .{
            .name = try dupeStr(arena, name),
            .type_params = type_params,
            .variants = variants,
            .is_pub = is_pub,
            .is_extern = is_extern,
            .repr_type = repr_type,
            .span = dummy_span,
            .meta = meta,
        };
        return .{ .enum_decl = node };
    } else if (std.mem.eql(u8, kind, "trait_decl")) {
        const name = getString(obj, "name") orelse return JsonAstError.MissingField;
        const is_pub = getBool(obj, "is_pub") orelse false;
        const is_unsafe = getBool(obj, "is_unsafe") orelse false;
        const type_params = try buildTypeParams(arena, obj);

        // Super traits
        const super_arr = getArray(obj, "super_traits") orelse return JsonAstError.MissingField;
        const super_traits = try arena.alloc(ast.TypeExpr, super_arr.items.len);
        for (super_arr.items, 0..) |st, i| {
            super_traits[i] = try buildTypeExpr(arena, st);
        }

        // Associated types
        const at_arr = getArray(obj, "associated_types") orelse return JsonAstError.MissingField;
        const associated_types = try arena.alloc(ast.AssociatedTypeDecl, at_arr.items.len);
        for (at_arr.items, 0..) |at, i| {
            if (at != .object) return JsonAstError.InvalidValue;
            const ato = at.object;
            const at_name = getString(ato, "name") orelse return JsonAstError.MissingField;
            const at_bounds_arr = getArray(ato, "bounds") orelse return JsonAstError.MissingField;
            const at_bounds = try arena.alloc(ast.TypeExpr, at_bounds_arr.items.len);
            for (at_bounds_arr.items, 0..) |b, j| {
                at_bounds[j] = try buildTypeExpr(arena, b);
            }
            const at_default = if (!isNull(ato, "default"))
                try buildTypeExpr(arena, ato.get("default").?)
            else
                null;
            associated_types[i] = .{
                .name = try dupeStr(arena, at_name),
                .bounds = at_bounds,
                .default = at_default,
                .span = dummy_span,
            };
        }

        // Methods
        const methods_arr = getArray(obj, "methods") orelse return JsonAstError.MissingField;
        const methods = try arena.alloc(ast.FunctionDecl, methods_arr.items.len);
        for (methods_arr.items, 0..) |m, i| {
            if (m != .object) return JsonAstError.InvalidValue;
            methods[i] = try buildFunctionDecl(arena, m.object);
        }

        const meta = try buildMetaAnnotations(arena, obj);
        const node = try arena.create(ast.TraitDecl);
        node.* = .{
            .name = try dupeStr(arena, name),
            .type_params = type_params,
            .super_traits = super_traits,
            .associated_types = associated_types,
            .methods = methods,
            .is_pub = is_pub,
            .is_unsafe = is_unsafe,
            .span = dummy_span,
            .meta = meta,
        };
        return .{ .trait_decl = node };
    } else if (std.mem.eql(u8, kind, "impl_decl")) {
        const is_unsafe = getBool(obj, "is_unsafe") orelse false;
        const type_params = try buildTypeParams(arena, obj);
        const target_type = try buildTypeExpr(arena, obj.get("target_type") orelse return JsonAstError.MissingField);
        const trait_type = if (!isNull(obj, "trait_type"))
            try buildTypeExpr(arena, obj.get("trait_type").?)
        else
            null;

        // Associated type bindings
        const atb_arr = getArray(obj, "associated_types") orelse return JsonAstError.MissingField;
        const associated_types = try arena.alloc(ast.AssociatedTypeBinding, atb_arr.items.len);
        for (atb_arr.items, 0..) |at, i| {
            if (at != .object) return JsonAstError.InvalidValue;
            const ato = at.object;
            const at_name = getString(ato, "name") orelse return JsonAstError.MissingField;
            const at_value = try buildTypeExpr(arena, ato.get("value") orelse return JsonAstError.MissingField);
            associated_types[i] = .{
                .name = try dupeStr(arena, at_name),
                .value = at_value,
                .span = dummy_span,
            };
        }

        const where_clause = try buildWhereClause(arena, obj.get("where_clause") orelse .null);

        // Methods
        const methods_arr = getArray(obj, "methods") orelse return JsonAstError.MissingField;
        const methods = try arena.alloc(ast.FunctionDecl, methods_arr.items.len);
        for (methods_arr.items, 0..) |m, i| {
            if (m != .object) return JsonAstError.InvalidValue;
            methods[i] = try buildFunctionDecl(arena, m.object);
        }

        const meta = try buildMetaAnnotations(arena, obj);
        const node = try arena.create(ast.ImplDecl);
        node.* = .{
            .type_params = type_params,
            .target_type = target_type,
            .trait_type = trait_type,
            .associated_types = associated_types,
            .where_clause = where_clause,
            .methods = methods,
            .is_unsafe = is_unsafe,
            .span = dummy_span,
            .meta = meta,
        };
        return .{ .impl_decl = node };
    } else if (std.mem.eql(u8, kind, "type_alias")) {
        const name = getString(obj, "name") orelse return JsonAstError.MissingField;
        const is_pub = getBool(obj, "is_pub") orelse false;
        const type_params = try buildTypeParams(arena, obj);
        const target = try buildTypeExpr(arena, obj.get("target") orelse return JsonAstError.MissingField);
        const meta = try buildMetaAnnotations(arena, obj);
        const node = try arena.create(ast.TypeAlias);
        node.* = .{
            .name = try dupeStr(arena, name),
            .type_params = type_params,
            .target = target,
            .is_pub = is_pub,
            .span = dummy_span,
            .meta = meta,
        };
        return .{ .type_alias = node };
    } else if (std.mem.eql(u8, kind, "const_decl")) {
        const name = getString(obj, "name") orelse return JsonAstError.MissingField;
        const is_pub = getBool(obj, "is_pub") orelse false;
        const type_ = if (!isNull(obj, "type"))
            try buildTypeExpr(arena, obj.get("type").?)
        else
            null;
        const value = try buildExpr(arena, obj.get("value") orelse return JsonAstError.MissingField);
        const meta = try buildMetaAnnotations(arena, obj);
        const node = try arena.create(ast.ConstDecl);
        node.* = .{
            .name = try dupeStr(arena, name),
            .type_ = type_,
            .value = value,
            .is_pub = is_pub,
            .span = dummy_span,
            .meta = meta,
        };
        return .{ .const_decl = node };
    } else if (std.mem.eql(u8, kind, "import_decl")) {
        // Import decl wraps its data in a "data" field
        const data_val = obj.get("data") orelse return JsonAstError.MissingField;
        const imp = try buildImportDecl(arena, data_val);
        const node = try arena.create(ast.ImportDecl);
        node.* = imp;
        return .{ .import_decl = node };
    } else if (std.mem.eql(u8, kind, "module_decl")) {
        const data_val = obj.get("data") orelse return JsonAstError.MissingField;
        if (data_val != .object) return JsonAstError.InvalidValue;
        const data_obj = data_val.object;
        const path_arr = getArray(data_obj, "path") orelse return JsonAstError.MissingField;
        const path = try arena.alloc([]const u8, path_arr.items.len);
        for (path_arr.items, 0..) |seg, i| {
            if (seg != .string) return JsonAstError.InvalidValue;
            path[i] = try dupeStr(arena, seg.string);
        }
        const node = try arena.create(ast.ModuleDecl);
        node.* = .{ .path = path, .span = dummy_span };
        return .{ .module_decl = node };
    } else if (std.mem.eql(u8, kind, "extern_type_decl")) {
        const name = getString(obj, "name") orelse return JsonAstError.MissingField;
        const is_pub = getBool(obj, "is_pub") orelse false;
        const size: ?u64 = if (getInteger(obj, "size")) |s| @intCast(s) else null;
        const node = try arena.create(ast.ExternTypeDecl);
        node.* = .{
            .name = try dupeStr(arena, name),
            .size = size,
            .is_pub = is_pub,
            .span = dummy_span,
        };
        return .{ .extern_type_decl = node };
    } else if (std.mem.eql(u8, kind, "extern_block")) {
        const funcs_arr = getArray(obj, "functions") orelse return JsonAstError.MissingField;
        const functions = try arena.alloc(*ast.FunctionDecl, funcs_arr.items.len);
        for (funcs_arr.items, 0..) |f, i| {
            if (f != .object) return JsonAstError.InvalidValue;
            const func = try buildFunctionDecl(arena, f.object);
            const fnode = try arena.create(ast.FunctionDecl);
            fnode.* = func;
            functions[i] = fnode;
        }
        const node = try arena.create(ast.ExternBlock);
        node.* = .{ .functions = functions, .span = dummy_span };
        return .{ .extern_block = node };
    } else {
        return JsonAstError.UnknownKind;
    }
}

// ============================================================================
// Import builder
// ============================================================================

fn buildImportDecl(arena: Allocator, val: Value) anyerror!ast.ImportDecl {
    if (val != .object) return JsonAstError.InvalidValue;
    const obj = val.object;

    // Path
    const path_arr = getArray(obj, "path") orelse return JsonAstError.MissingField;
    const path = try arena.alloc([]const u8, path_arr.items.len);
    for (path_arr.items, 0..) |seg, i| {
        if (seg != .string) return JsonAstError.InvalidValue;
        path[i] = try dupeStr(arena, seg.string);
    }

    // Items
    const items_val = obj.get("items") orelse .null;
    const items: ?ast.ImportItems = switch (items_val) {
        .null => null,
        .string => |s| if (std.mem.eql(u8, s, "*")) .all else null,
        .array => |arr| blk: {
            const specs = try arena.alloc(ast.ImportItem, arr.items.len);
            for (arr.items, 0..) |item, i| {
                if (item != .object) return JsonAstError.InvalidValue;
                const io = item.object;
                const iname = getString(io, "name") orelse return JsonAstError.MissingField;
                const ialias = getString(io, "alias");
                specs[i] = .{
                    .name = try dupeStr(arena, iname),
                    .alias = if (ialias) |a| try dupeStr(arena, a) else null,
                    .span = dummy_span,
                };
            }
            break :blk .{ .specific = specs };
        },
        else => return JsonAstError.InvalidValue,
    };

    // Alias
    const alias = if (getString(obj, "alias")) |a| try dupeStr(arena, a) else null;

    return .{
        .path = path,
        .items = items,
        .alias = alias,
        .span = dummy_span,
    };
}

// ============================================================================
// Meta annotation builders
// ============================================================================

fn buildMetaAnnotations(arena: Allocator, obj: ObjectMap) anyerror![]const ast.MetaAnnotation {
    const arr = getArray(obj, "meta") orelse return &.{};
    const result = try arena.alloc(ast.MetaAnnotation, arr.items.len);
    for (arr.items, 0..) |ann, i| {
        result[i] = try buildOneMetaAnnotation(arena, ann);
    }
    return result;
}

fn buildOneMetaAnnotation(arena: Allocator, val: Value) anyerror!ast.MetaAnnotation {
    if (val != .object) return JsonAstError.InvalidValue;
    const obj = val.object;
    const kind = getString(obj, "kind") orelse return JsonAstError.MissingField;

    if (std.mem.eql(u8, kind, "intent")) {
        const value = getString(obj, "value") orelse return JsonAstError.MissingField;
        return .{ .intent = .{ .value = try dupeStr(arena, value), .span = dummy_span } };
    } else if (std.mem.eql(u8, kind, "decision")) {
        const value = getString(obj, "value") orelse return JsonAstError.MissingField;
        return .{ .decision = .{ .value = try dupeStr(arena, value), .span = dummy_span } };
    } else if (std.mem.eql(u8, kind, "tag")) {
        const value = getString(obj, "value") orelse return JsonAstError.MissingField;
        return .{ .tag = .{ .value = try dupeStr(arena, value), .span = dummy_span } };
    } else if (std.mem.eql(u8, kind, "hint")) {
        const value = getString(obj, "value") orelse return JsonAstError.MissingField;
        return .{ .hint = .{ .value = try dupeStr(arena, value), .span = dummy_span } };
    } else if (std.mem.eql(u8, kind, "deprecated")) {
        const value = getString(obj, "value") orelse return JsonAstError.MissingField;
        return .{ .deprecated = .{ .value = try dupeStr(arena, value), .span = dummy_span } };
    } else if (std.mem.eql(u8, kind, "pure")) {
        return .{ .pure = dummy_span };
    } else if (std.mem.eql(u8, kind, "module") or std.mem.eql(u8, kind, "guide")) {
        const entries = try buildMetaBlockEntries(arena, obj);
        const block = try arena.create(ast.MetaBlock);
        block.* = .{ .entries = entries, .span = dummy_span };
        if (std.mem.eql(u8, kind, "module")) {
            return .{ .module_meta = block };
        } else {
            return .{ .guide = block };
        }
    } else if (std.mem.eql(u8, kind, "related")) {
        const paths_arr = getArray(obj, "paths") orelse return JsonAstError.MissingField;
        const paths = try arena.alloc(ast.MetaPath, paths_arr.items.len);
        for (paths_arr.items, 0..) |p, i| {
            paths[i] = try buildMetaPath(arena, p);
        }
        const description = if (getString(obj, "description")) |d| try dupeStr(arena, d) else null;
        const node = try arena.create(ast.MetaRelated);
        node.* = .{ .paths = paths, .description = description, .span = dummy_span };
        return .{ .related = node };
    } else if (std.mem.eql(u8, kind, "group")) {
        const name = getString(obj, "name") orelse return JsonAstError.MissingField;
        const anns_arr = getArray(obj, "annotations") orelse return JsonAstError.MissingField;
        const annotations = try arena.alloc(ast.MetaAnnotation, anns_arr.items.len);
        for (anns_arr.items, 0..) |a, i| {
            annotations[i] = try buildOneMetaAnnotation(arena, a);
        }
        const node = try arena.create(ast.MetaGroupDef);
        node.* = .{ .name = try dupeStr(arena, name), .annotations = annotations, .span = dummy_span };
        return .{ .group_def = node };
    } else if (std.mem.eql(u8, kind, "in")) {
        const value = getString(obj, "value") orelse return JsonAstError.MissingField;
        return .{ .group_join = .{ .value = try dupeStr(arena, value), .span = dummy_span } };
    } else if (std.mem.eql(u8, kind, "define")) {
        const name = getString(obj, "name") orelse return JsonAstError.MissingField;
        const params_arr = getArray(obj, "params") orelse return JsonAstError.MissingField;
        const define_params = try arena.alloc(ast.MetaDefineParam, params_arr.items.len);
        for (params_arr.items, 0..) |p, i| {
            if (p != .object) return JsonAstError.InvalidValue;
            const po = p.object;
            const pname = getString(po, "name") orelse return JsonAstError.MissingField;
            const ptype_val = po.get("type") orelse return JsonAstError.MissingField;
            const type_constraint: ast.MetaParamType = switch (ptype_val) {
                .string => |s| blk: {
                    if (std.mem.eql(u8, s, "string")) break :blk .string_type;
                    if (std.mem.eql(u8, s, "path")) break :blk .path_type;
                    return JsonAstError.InvalidValue;
                },
                .array => |arr| blk: {
                    const vals = try arena.alloc([]const u8, arr.items.len);
                    for (arr.items, 0..) |v, j| {
                        if (v != .string) return JsonAstError.InvalidValue;
                        vals[j] = try dupeStr(arena, v.string);
                    }
                    break :blk .{ .string_union = vals };
                },
                else => return JsonAstError.InvalidValue,
            };
            define_params[i] = .{
                .name = try dupeStr(arena, pname),
                .type_constraint = type_constraint,
                .span = dummy_span,
            };
        }

        const scope: ?ast.MetaScope = if (getString(obj, "scope")) |s| blk: {
            if (std.mem.eql(u8, s, "fn")) break :blk .fn_scope;
            if (std.mem.eql(u8, s, "module")) break :blk .module_scope;
            if (std.mem.eql(u8, s, "struct")) break :blk .struct_scope;
            if (std.mem.eql(u8, s, "enum")) break :blk .enum_scope;
            if (std.mem.eql(u8, s, "trait")) break :blk .trait_scope;
            if (std.mem.eql(u8, s, "field")) break :blk .field_scope;
            if (std.mem.eql(u8, s, "variant")) break :blk .variant_scope;
            if (std.mem.eql(u8, s, "test")) break :blk .test_scope;
            return JsonAstError.InvalidValue;
        } else null;

        const node = try arena.create(ast.MetaDefine);
        node.* = .{
            .name = try dupeStr(arena, name),
            .params = define_params,
            .scope = scope,
            .span = dummy_span,
        };
        return .{ .define = node };
    } else if (std.mem.eql(u8, kind, "custom")) {
        const name = getString(obj, "name") orelse return JsonAstError.MissingField;
        const args_arr = getArray(obj, "args") orelse return JsonAstError.MissingField;
        const custom_args = try arena.alloc(ast.MetaCustomArg, args_arr.items.len);
        for (args_arr.items, 0..) |a, i| {
            switch (a) {
                .string => |s| {
                    custom_args[i] = .{ .string = try dupeStr(arena, s) };
                },
                .object => {
                    custom_args[i] = .{ .path = try buildMetaPath(arena, a) };
                },
                else => return JsonAstError.InvalidValue,
            }
        }
        const node = try arena.create(ast.MetaCustom);
        node.* = .{
            .name = try dupeStr(arena, name),
            .args = custom_args,
            .span = dummy_span,
        };
        return .{ .custom = node };
    } else {
        return JsonAstError.UnknownKind;
    }
}

fn buildMetaBlockEntries(arena: Allocator, obj: ObjectMap) anyerror![]const ast.MetaKeyValue {
    const arr = getArray(obj, "entries") orelse return &.{};
    const result = try arena.alloc(ast.MetaKeyValue, arr.items.len);
    for (arr.items, 0..) |entry, i| {
        if (entry != .object) return JsonAstError.InvalidValue;
        const eo = entry.object;
        const key = getString(eo, "key") orelse return JsonAstError.MissingField;
        const value_val = eo.get("value") orelse return JsonAstError.MissingField;
        const meta_value: ast.MetaValue = switch (value_val) {
            .string => |s| .{ .string = try dupeStr(arena, s) },
            .array => |arr_val| blk: {
                const strs = try arena.alloc([]const u8, arr_val.items.len);
                for (arr_val.items, 0..) |s, j| {
                    if (s != .string) return JsonAstError.InvalidValue;
                    strs[j] = try dupeStr(arena, s.string);
                }
                break :blk .{ .string_list = strs };
            },
            else => return JsonAstError.InvalidValue,
        };
        result[i] = .{
            .key = try dupeStr(arena, key),
            .value = meta_value,
            .span = dummy_span,
        };
    }
    return result;
}

fn buildMetaPath(arena: Allocator, val: Value) anyerror!ast.MetaPath {
    if (val != .object) return JsonAstError.InvalidValue;
    const obj = val.object;
    const segs_arr = getArray(obj, "segments") orelse return JsonAstError.MissingField;
    const segments = try arena.alloc([]const u8, segs_arr.items.len);
    for (segs_arr.items, 0..) |s, i| {
        if (s != .string) return JsonAstError.InvalidValue;
        segments[i] = try dupeStr(arena, s.string);
    }
    return .{ .segments = segments, .span = dummy_span };
}

// ============================================================================
// Module builder
// ============================================================================

pub fn buildModule(arena: Allocator, root: ObjectMap) anyerror!ast.Module {
    // Module declaration
    const module_decl: ?ast.ModuleDecl = if (!isNull(root, "module_decl")) blk: {
        const md_val = root.get("module_decl").?;
        if (md_val != .object) return JsonAstError.InvalidValue;
        const md_obj = md_val.object;
        const path_arr = getArray(md_obj, "path") orelse return JsonAstError.MissingField;
        const path = try arena.alloc([]const u8, path_arr.items.len);
        for (path_arr.items, 0..) |seg, i| {
            if (seg != .string) return JsonAstError.InvalidValue;
            path[i] = try dupeStr(arena, seg.string);
        }
        break :blk .{ .path = path, .span = dummy_span };
    } else null;

    // Imports
    const imports_arr = getArray(root, "imports") orelse return JsonAstError.MissingField;
    const imports = try arena.alloc(ast.ImportDecl, imports_arr.items.len);
    for (imports_arr.items, 0..) |imp, i| {
        imports[i] = try buildImportDecl(arena, imp);
    }

    // Declarations
    const decls_arr = getArray(root, "declarations") orelse return JsonAstError.MissingField;
    const declarations = try arena.alloc(ast.Decl, decls_arr.items.len);
    for (decls_arr.items, 0..) |d, i| {
        declarations[i] = try buildDecl(arena, d);
    }

    // File-level meta annotations
    const file_meta = try buildMetaAnnotations(arena, root);

    return .{
        .module_decl = module_decl,
        .imports = imports,
        .declarations = declarations,
        .file_meta = file_meta,
    };
}
