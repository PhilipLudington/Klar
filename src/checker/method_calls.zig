//! Method Call Checking Module
//!
//! Handles type checking for method calls on built-in types.
//! Split from checker.zig to reduce file size.
//!
//! ## Structure
//! - `checkStaticConstructor` - Rc.new, List.new, Map.new, etc.
//! - `checkBuiltinMethod` - len, clone, push, pop, etc.
//!
//! Uses duck typing (`tc: anytype`) to avoid circular dependencies.

const std = @import("std");
const ast = @import("../ast.zig");
const types = @import("../types.zig");
const Type = types.Type;
const Span = ast.Span;

/// Check for static constructor calls like Rc.new(), List.new[T](), etc.
/// Returns the result type if this is a static constructor, null otherwise.
pub fn checkStaticConstructor(tc: anytype, method: *ast.MethodCall) ?Type {
    if (method.object != .identifier) return null;

    const obj_name = method.object.identifier.name;

    // Rc.new(value) -> Rc[T] where T is the type of value
    if (std.mem.eql(u8, obj_name, "Rc") and std.mem.eql(u8, method.method_name, "new")) {
        if (method.args.len != 1) {
            tc.addError(.invalid_call, method.span, "Rc.new() expects exactly 1 argument", .{});
            return tc.type_builder.unknownType();
        }
        const value_type = tc.checkExpr(method.args[0]);
        return tc.type_builder.rcType(value_type) catch tc.type_builder.unknownType();
    }

    // Weak.new() is not valid - Weak can only be created from Rc.downgrade()
    if (std.mem.eql(u8, obj_name, "Weak") and std.mem.eql(u8, method.method_name, "new")) {
        tc.addError(.invalid_call, method.span, "Weak cannot be created directly; use rc_value.downgrade()", .{});
        return tc.type_builder.unknownType();
    }

    // Arc.new(value) -> Arc[T] where T is the type of value (thread-safe)
    if (std.mem.eql(u8, obj_name, "Arc") and std.mem.eql(u8, method.method_name, "new")) {
        if (method.args.len != 1) {
            tc.addError(.invalid_call, method.span, "Arc.new() expects exactly 1 argument", .{});
            return tc.type_builder.unknownType();
        }
        const value_type = tc.checkExpr(method.args[0]);
        return tc.type_builder.arcType(value_type) catch tc.type_builder.unknownType();
    }

    // WeakArc.new() is not valid - WeakArc can only be created from Arc.downgrade()
    if (std.mem.eql(u8, obj_name, "WeakArc") and std.mem.eql(u8, method.method_name, "new")) {
        tc.addError(.invalid_call, method.span, "WeakArc cannot be created directly; use arc_value.downgrade()", .{});
        return tc.type_builder.unknownType();
    }

    // Cell.new(value) -> Cell[T] where T is the type of value
    if (std.mem.eql(u8, obj_name, "Cell") and std.mem.eql(u8, method.method_name, "new")) {
        if (method.args.len != 1) {
            tc.addError(.invalid_call, method.span, "Cell.new() expects exactly 1 argument", .{});
            return tc.type_builder.unknownType();
        }
        const value_type = tc.checkExpr(method.args[0]);
        return tc.type_builder.cellType(value_type) catch tc.type_builder.unknownType();
    }

    // Default trait: TypeName.default() -> T where T implements Default
    if (std.mem.eql(u8, method.method_name, "default")) {
        return checkDefaultConstructor(tc, method, obj_name);
    }

    // List.new[T]() -> List[T]
    if (std.mem.eql(u8, obj_name, "List") and std.mem.eql(u8, method.method_name, "new")) {
        return checkListNew(tc, method);
    }

    // List.with_capacity[T](n) -> List[T]
    if (std.mem.eql(u8, obj_name, "List") and std.mem.eql(u8, method.method_name, "with_capacity")) {
        return checkListWithCapacity(tc, method);
    }

    // Map.new[K,V]() -> Map[K,V]
    if (std.mem.eql(u8, obj_name, "Map") and std.mem.eql(u8, method.method_name, "new")) {
        return checkMapNew(tc, method);
    }

    // Map.with_capacity[K,V](n) -> Map[K,V]
    if (std.mem.eql(u8, obj_name, "Map") and std.mem.eql(u8, method.method_name, "with_capacity")) {
        return checkMapWithCapacity(tc, method);
    }

    // Set.new[T]() -> Set[T]
    if (std.mem.eql(u8, obj_name, "Set") and std.mem.eql(u8, method.method_name, "new")) {
        return checkSetNew(tc, method);
    }

    // Set.with_capacity[T](n) -> Set[T]
    if (std.mem.eql(u8, obj_name, "Set") and std.mem.eql(u8, method.method_name, "with_capacity")) {
        return checkSetWithCapacity(tc, method);
    }

    // String constructors
    if (std.mem.eql(u8, obj_name, "String")) {
        if (std.mem.eql(u8, method.method_name, "new")) return checkStringNew(tc, method);
        if (std.mem.eql(u8, method.method_name, "from")) return checkStringFrom(tc, method);
        if (std.mem.eql(u8, method.method_name, "with_capacity")) return checkStringWithCapacity(tc, method);
    }

    // Path.new(s: string) -> Path
    if (std.mem.eql(u8, obj_name, "Path") and std.mem.eql(u8, method.method_name, "new")) {
        return checkPathNew(tc, method);
    }

    // File static methods
    if (std.mem.eql(u8, obj_name, "File")) {
        if (std.mem.eql(u8, method.method_name, "open")) return checkFileOpen(tc, method);
        if (std.mem.eql(u8, method.method_name, "read_to_string")) return checkFileReadToString(tc, method);
        if (std.mem.eql(u8, method.method_name, "read_all")) return checkFileReadAll(tc, method);
    }

    // BufReader.new[R](reader: R) -> BufReader[R]
    if (std.mem.eql(u8, obj_name, "BufReader") and std.mem.eql(u8, method.method_name, "new")) {
        return checkBufReaderNew(tc, method);
    }

    // BufWriter.new[W](writer: W) -> BufWriter[W]
    if (std.mem.eql(u8, obj_name, "BufWriter") and std.mem.eql(u8, method.method_name, "new")) {
        return checkBufWriterNew(tc, method);
    }

    // CStr.from_ptr(ptr: CPtr[i8]) -> CStr
    if (std.mem.eql(u8, obj_name, "CStr") and std.mem.eql(u8, method.method_name, "from_ptr")) {
        return checkCStrFromPtr(tc, method);
    }

    return null;
}

// ============================================================================
// Static Constructor Helpers
// ============================================================================

fn checkDefaultConstructor(tc: anytype, method: *ast.MethodCall, obj_name: []const u8) Type {
    if (method.args.len != 0) {
        tc.addError(.invalid_call, method.span, "default() takes no arguments", .{});
        return tc.type_builder.unknownType();
    }

    // Check if obj_name is a primitive type
    const primitive_types = [_]struct { name: []const u8, prim: types.Primitive }{
        .{ .name = "i8", .prim = .i8_ },
        .{ .name = "i16", .prim = .i16_ },
        .{ .name = "i32", .prim = .i32_ },
        .{ .name = "i64", .prim = .i64_ },
        .{ .name = "i128", .prim = .i128_ },
        .{ .name = "isize", .prim = .isize_ },
        .{ .name = "u8", .prim = .u8_ },
        .{ .name = "u16", .prim = .u16_ },
        .{ .name = "u32", .prim = .u32_ },
        .{ .name = "u64", .prim = .u64_ },
        .{ .name = "u128", .prim = .u128_ },
        .{ .name = "usize", .prim = .usize_ },
        .{ .name = "f32", .prim = .f32_ },
        .{ .name = "f64", .prim = .f64_ },
        .{ .name = "bool", .prim = .bool_ },
        .{ .name = "char", .prim = .char_ },
        .{ .name = "string", .prim = .string_ },
    };

    for (primitive_types) |pt| {
        if (std.mem.eql(u8, obj_name, pt.name)) {
            return Type{ .primitive = pt.prim };
        }
    }

    // Check if it's a struct that implements Default
    if (tc.current_scope.lookup(obj_name)) |sym| {
        if (sym.type_ == .struct_) {
            if (tc.typeImplementsTrait(obj_name, "Default")) {
                return sym.type_;
            }
            tc.addError(.undefined_method, method.span, "type '{s}' does not implement Default trait", .{obj_name});
            return tc.type_builder.unknownType();
        }
    }

    tc.addError(.undefined_method, method.span, "unknown type '{s}' for Default::default()", .{obj_name});
    return tc.type_builder.unknownType();
}

fn checkListNew(tc: anytype, method: *ast.MethodCall) Type {
    if (method.type_args) |type_args| {
        if (type_args.len != 1) {
            tc.addError(.invalid_call, method.span, "List.new[T]() expects exactly 1 type argument", .{});
            return tc.type_builder.unknownType();
        }
        if (method.args.len != 0) {
            tc.addError(.invalid_call, method.span, "List.new[T]() takes no value arguments", .{});
        }
        const element_type = tc.resolveTypeExpr(type_args[0]) catch {
            return tc.type_builder.unknownType();
        };
        return tc.type_builder.listType(element_type) catch tc.type_builder.unknownType();
    }
    tc.addError(.invalid_call, method.span, "List.new() requires a type argument: List.new[i32]()", .{});
    return tc.type_builder.unknownType();
}

fn checkListWithCapacity(tc: anytype, method: *ast.MethodCall) Type {
    if (method.type_args) |type_args| {
        if (type_args.len != 1) {
            tc.addError(.invalid_call, method.span, "List.with_capacity[T](n) expects exactly 1 type argument", .{});
            return tc.type_builder.unknownType();
        }
        if (method.args.len != 1) {
            tc.addError(.invalid_call, method.span, "List.with_capacity[T](n) takes exactly 1 argument (capacity)", .{});
            return tc.type_builder.unknownType();
        }
        const arg_type = tc.checkExpr(method.args[0]);
        if (arg_type != .primitive or (arg_type.primitive != .i32_ and arg_type.primitive != .i64_)) {
            tc.addError(.type_mismatch, method.span, "List.with_capacity expects an integer capacity argument", .{});
        }
        const element_type = tc.resolveTypeExpr(type_args[0]) catch {
            return tc.type_builder.unknownType();
        };
        return tc.type_builder.listType(element_type) catch tc.type_builder.unknownType();
    }
    tc.addError(.invalid_call, method.span, "List.with_capacity() requires a type argument: List.with_capacity[i32](10)", .{});
    return tc.type_builder.unknownType();
}

fn checkMapNew(tc: anytype, method: *ast.MethodCall) Type {
    if (method.type_args) |type_args| {
        if (type_args.len != 2) {
            tc.addError(.invalid_call, method.span, "Map.new[K,V]() expects exactly 2 type arguments", .{});
            return tc.type_builder.unknownType();
        }
        if (method.args.len != 0) {
            tc.addError(.invalid_call, method.span, "Map.new[K,V]() takes no value arguments", .{});
        }
        const key_type = tc.resolveTypeExpr(type_args[0]) catch {
            return tc.type_builder.unknownType();
        };
        const value_type = tc.resolveTypeExpr(type_args[1]) catch {
            return tc.type_builder.unknownType();
        };
        _ = tc.typeImplementsHashAndEq(key_type, method.span);
        return tc.type_builder.mapType(key_type, value_type) catch tc.type_builder.unknownType();
    }
    tc.addError(.invalid_call, method.span, "Map.new() requires type arguments: Map.new[i32, string]()", .{});
    return tc.type_builder.unknownType();
}

fn checkMapWithCapacity(tc: anytype, method: *ast.MethodCall) Type {
    if (method.type_args) |type_args| {
        if (type_args.len != 2) {
            tc.addError(.invalid_call, method.span, "Map.with_capacity[K,V](n) expects exactly 2 type arguments", .{});
            return tc.type_builder.unknownType();
        }
        if (method.args.len != 1) {
            tc.addError(.invalid_call, method.span, "Map.with_capacity[K,V](n) takes exactly 1 argument (capacity)", .{});
            return tc.type_builder.unknownType();
        }
        const arg_type = tc.checkExpr(method.args[0]);
        if (arg_type != .primitive or (arg_type.primitive != .i32_ and arg_type.primitive != .i64_)) {
            tc.addError(.type_mismatch, method.span, "Map.with_capacity expects an integer capacity argument", .{});
        }
        const key_type = tc.resolveTypeExpr(type_args[0]) catch {
            return tc.type_builder.unknownType();
        };
        const value_type = tc.resolveTypeExpr(type_args[1]) catch {
            return tc.type_builder.unknownType();
        };
        _ = tc.typeImplementsHashAndEq(key_type, method.span);
        return tc.type_builder.mapType(key_type, value_type) catch tc.type_builder.unknownType();
    }
    tc.addError(.invalid_call, method.span, "Map.with_capacity() requires type arguments: Map.with_capacity[i32, string](10)", .{});
    return tc.type_builder.unknownType();
}

fn checkSetNew(tc: anytype, method: *ast.MethodCall) Type {
    if (method.type_args) |type_args| {
        if (type_args.len != 1) {
            tc.addError(.invalid_call, method.span, "Set.new[T]() expects exactly 1 type argument", .{});
            return tc.type_builder.unknownType();
        }
        if (method.args.len != 0) {
            tc.addError(.invalid_call, method.span, "Set.new[T]() takes no value arguments", .{});
        }
        const element_type = tc.resolveTypeExpr(type_args[0]) catch {
            return tc.type_builder.unknownType();
        };
        _ = tc.typeImplementsHashAndEq(element_type, method.span);
        return tc.type_builder.setType(element_type) catch tc.type_builder.unknownType();
    }
    tc.addError(.invalid_call, method.span, "Set.new() requires type arguments: Set.new[i32]()", .{});
    return tc.type_builder.unknownType();
}

fn checkSetWithCapacity(tc: anytype, method: *ast.MethodCall) Type {
    if (method.type_args) |type_args| {
        if (type_args.len != 1) {
            tc.addError(.invalid_call, method.span, "Set.with_capacity[T](n) expects exactly 1 type argument", .{});
            return tc.type_builder.unknownType();
        }
        if (method.args.len != 1) {
            tc.addError(.invalid_call, method.span, "Set.with_capacity[T](n) takes exactly 1 argument (capacity)", .{});
            return tc.type_builder.unknownType();
        }
        const arg_type = tc.checkExpr(method.args[0]);
        if (arg_type != .primitive or (arg_type.primitive != .i32_ and arg_type.primitive != .i64_)) {
            tc.addError(.type_mismatch, method.span, "Set.with_capacity expects an integer capacity argument", .{});
        }
        const element_type = tc.resolveTypeExpr(type_args[0]) catch {
            return tc.type_builder.unknownType();
        };
        _ = tc.typeImplementsHashAndEq(element_type, method.span);
        return tc.type_builder.setType(element_type) catch tc.type_builder.unknownType();
    }
    tc.addError(.invalid_call, method.span, "Set.with_capacity() requires type arguments: Set.with_capacity[i32](10)", .{});
    return tc.type_builder.unknownType();
}

fn checkStringNew(tc: anytype, method: *ast.MethodCall) Type {
    if (method.args.len != 0) {
        tc.addError(.invalid_call, method.span, "String.new() takes no arguments", .{});
    }
    if (method.type_args != null) {
        tc.addError(.invalid_call, method.span, "String.new() does not take type arguments", .{});
    }
    return tc.type_builder.stringDataType() catch tc.type_builder.unknownType();
}

fn checkStringFrom(tc: anytype, method: *ast.MethodCall) Type {
    if (method.args.len != 1) {
        tc.addError(.invalid_call, method.span, "String.from() takes exactly 1 argument", .{});
        return tc.type_builder.unknownType();
    }
    if (method.type_args != null) {
        tc.addError(.invalid_call, method.span, "String.from() does not take type arguments", .{});
    }
    const arg_type = tc.checkExpr(method.args[0]);
    if (arg_type != .primitive or arg_type.primitive != .string_) {
        tc.addError(.type_mismatch, method.span, "String.from() expects a string argument", .{});
    }
    return tc.type_builder.stringDataType() catch tc.type_builder.unknownType();
}

fn checkStringWithCapacity(tc: anytype, method: *ast.MethodCall) Type {
    if (method.args.len != 1) {
        tc.addError(.invalid_call, method.span, "String.with_capacity() takes exactly 1 argument (capacity)", .{});
        return tc.type_builder.unknownType();
    }
    if (method.type_args != null) {
        tc.addError(.invalid_call, method.span, "String.with_capacity() does not take type arguments", .{});
    }
    const arg_type = tc.checkExpr(method.args[0]);
    if (arg_type != .primitive or (arg_type.primitive != .i32_ and arg_type.primitive != .i64_)) {
        tc.addError(.type_mismatch, method.span, "String.with_capacity() expects an integer argument", .{});
    }
    return tc.type_builder.stringDataType() catch tc.type_builder.unknownType();
}

fn checkPathNew(tc: anytype, method: *ast.MethodCall) Type {
    if (method.args.len != 1) {
        tc.addError(.invalid_call, method.span, "Path.new() takes exactly 1 argument (path string)", .{});
        return tc.type_builder.unknownType();
    }
    if (method.type_args != null) {
        tc.addError(.invalid_call, method.span, "Path.new() does not take type arguments", .{});
    }
    const arg_type = tc.checkExpr(method.args[0]);
    if (arg_type != .primitive or arg_type.primitive != .string_) {
        tc.addError(.type_mismatch, method.span, "Path.new() expects a string argument", .{});
    }
    return tc.type_builder.pathType();
}

fn checkFileOpen(tc: anytype, method: *ast.MethodCall) Type {
    if (method.args.len != 2) {
        tc.addError(.invalid_call, method.span, "File.open() takes exactly 2 arguments (path, mode)", .{});
        return tc.type_builder.unknownType();
    }
    if (method.type_args != null) {
        tc.addError(.invalid_call, method.span, "File.open() does not take type arguments", .{});
    }
    const path_type = tc.checkExpr(method.args[0]);
    if (path_type != .primitive or path_type.primitive != .string_) {
        tc.addError(.type_mismatch, method.span, "File.open() expects a string path argument", .{});
    }
    const mode_type = tc.checkExpr(method.args[1]);
    if (mode_type != .primitive or mode_type.primitive != .string_) {
        tc.addError(.type_mismatch, method.span, "File.open() expects a string mode argument", .{});
    }
    return tc.type_builder.resultType(tc.type_builder.fileType(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
}

fn checkFileReadToString(tc: anytype, method: *ast.MethodCall) Type {
    if (method.args.len != 1) {
        tc.addError(.invalid_call, method.span, "File.read_to_string() takes exactly 1 argument (path)", .{});
        return tc.type_builder.unknownType();
    }
    if (method.type_args != null) {
        tc.addError(.invalid_call, method.span, "File.read_to_string() does not take type arguments", .{});
    }
    const path_type = tc.checkExpr(method.args[0]);
    if (path_type != .primitive or path_type.primitive != .string_) {
        tc.addError(.type_mismatch, method.span, "File.read_to_string() expects a string path argument", .{});
    }
    const string_type = tc.type_builder.stringDataType() catch return tc.type_builder.unknownType();
    return tc.type_builder.resultType(string_type, tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
}

fn checkFileReadAll(tc: anytype, method: *ast.MethodCall) Type {
    if (method.args.len != 1) {
        tc.addError(.invalid_call, method.span, "File.read_all() takes exactly 1 argument (path)", .{});
        return tc.type_builder.unknownType();
    }
    if (method.type_args != null) {
        tc.addError(.invalid_call, method.span, "File.read_all() does not take type arguments", .{});
    }
    const path_type = tc.checkExpr(method.args[0]);
    if (path_type != .primitive or path_type.primitive != .string_) {
        tc.addError(.type_mismatch, method.span, "File.read_all() expects a string path argument", .{});
    }
    const u8_type = Type{ .primitive = .u8_ };
    const list_u8_type = tc.type_builder.listType(u8_type) catch return tc.type_builder.unknownType();
    return tc.type_builder.resultType(list_u8_type, tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
}

fn checkBufReaderNew(tc: anytype, method: *ast.MethodCall) Type {
    if (method.type_args) |type_args| {
        if (type_args.len != 1) {
            tc.addError(.invalid_call, method.span, "BufReader.new[R]() expects exactly 1 type argument", .{});
            return tc.type_builder.unknownType();
        }
        if (method.args.len != 1) {
            tc.addError(.invalid_call, method.span, "BufReader.new[R](reader) takes exactly 1 argument", .{});
            return tc.type_builder.unknownType();
        }
        const inner_type = tc.resolveTypeExpr(type_args[0]) catch {
            return tc.type_builder.unknownType();
        };
        if (tc.trait_registry.get("Read")) |read_trait| {
            _ = tc.typeSatisfiesBounds(inner_type, &.{read_trait.trait_type}, method.span);
        }
        const arg_type = tc.checkExpr(method.args[0]);
        if (!arg_type.eql(inner_type)) {
            tc.addError(.type_mismatch, method.span, "argument type must match type parameter", .{});
        }
        return tc.type_builder.bufReaderType(inner_type) catch tc.type_builder.unknownType();
    }
    tc.addError(.invalid_call, method.span, "BufReader.new() requires a type argument: BufReader.new[File](file)", .{});
    return tc.type_builder.unknownType();
}

fn checkBufWriterNew(tc: anytype, method: *ast.MethodCall) Type {
    if (method.type_args) |type_args| {
        if (type_args.len != 1) {
            tc.addError(.invalid_call, method.span, "BufWriter.new[W]() expects exactly 1 type argument", .{});
            return tc.type_builder.unknownType();
        }
        if (method.args.len != 1) {
            tc.addError(.invalid_call, method.span, "BufWriter.new[W](writer) takes exactly 1 argument", .{});
            return tc.type_builder.unknownType();
        }
        const inner_type = tc.resolveTypeExpr(type_args[0]) catch {
            return tc.type_builder.unknownType();
        };
        if (tc.trait_registry.get("Write")) |write_trait| {
            _ = tc.typeSatisfiesBounds(inner_type, &.{write_trait.trait_type}, method.span);
        }
        const arg_type = tc.checkExpr(method.args[0]);
        if (!arg_type.eql(inner_type)) {
            tc.addError(.type_mismatch, method.span, "argument type must match type parameter", .{});
        }
        return tc.type_builder.bufWriterType(inner_type) catch tc.type_builder.unknownType();
    }
    tc.addError(.invalid_call, method.span, "BufWriter.new() requires a type argument: BufWriter.new[File](file)", .{});
    return tc.type_builder.unknownType();
}

fn checkCStrFromPtr(tc: anytype, method: *ast.MethodCall) Type {
    if (method.args.len != 1) {
        tc.addError(.invalid_call, method.span, "CStr.from_ptr() takes exactly 1 argument", .{});
        return tc.type_builder.cstrType();
    }
    if (method.type_args != null) {
        tc.addError(.invalid_call, method.span, "CStr.from_ptr() does not take type arguments", .{});
    }
    const arg_type = tc.checkExpr(method.args[0]);
    if (arg_type == .cptr) {
        if (arg_type.cptr.inner != .primitive or arg_type.cptr.inner.primitive != .i8_) {
            tc.addError(.type_mismatch, method.span, "CStr.from_ptr() expects CPtr[i8]", .{});
        }
    } else {
        tc.addError(.type_mismatch, method.span, "CStr.from_ptr() expects CPtr[i8]", .{});
    }
    return tc.type_builder.cstrType();
}

/// Check for builtin method calls on various types.
/// Returns the result type if this is a builtin method, null otherwise.
pub fn checkBuiltinMethod(tc: anytype, method: *ast.MethodCall, object_type: Type) ?Type {
        // Check for type conversion methods
        if (std.mem.eql(u8, method.method_name, "as") or
            std.mem.eql(u8, method.method_name, "to") or
            std.mem.eql(u8, method.method_name, "trunc"))
        {
            // Type conversion methods
            if (method.type_args) |type_args| {
                if (type_args.len == 1) {
                    const target_type = tc.resolveTypeExpr(type_args[0]) catch return tc.type_builder.unknownType();
                    // .to[T] returns ?T (optional), .as[T] and .trunc[T] return T
                    if (std.mem.eql(u8, method.method_name, "to")) {
                        return tc.type_builder.optionalType(target_type) catch tc.type_builder.unknownType();
                    }
                    return target_type;
                }
            }
            tc.addError(.invalid_call, method.span, "conversion requires type argument", .{});
            return tc.type_builder.unknownType();
        }

        // Check for built-in methods on all types
        // to_string() returns string (primitive) for char and Path, String (heap-allocated) for other types
        if (std.mem.eql(u8, method.method_name, "to_string")) {
            // char.to_string() returns primitive string (single char doesn't need heap allocation)
            if (object_type == .primitive and object_type.primitive == .char_) {
                return tc.type_builder.stringType();
            }
            // Path.to_string() returns primitive string (returns pointer to internal data)
            if (object_type == .path) {
                return tc.type_builder.stringType();
            }
            return tc.type_builder.stringDataType() catch tc.type_builder.unknownType();
        }

        // Check for len method on arrays, tuples, strings, lists, maps, sets, String, and CStr
        // Returns i32 for ergonomic use with loop counters (except CStr which returns usize)
        if (std.mem.eql(u8, method.method_name, "len")) {
            if (object_type == .primitive and object_type.primitive == .string_) {
                return .{ .primitive = .i32_ };
            }
            if (object_type == .array or object_type == .slice or object_type == .tuple or object_type == .list or object_type == .string_data or object_type == .map or object_type == .set) {
                return .{ .primitive = .i32_ };
            }
            // CStr.len() returns usize and requires unsafe
            if (object_type == .cstr) {
                if (!tc.in_unsafe_context) {
                    tc.addError(.invalid_call, method.span, "CStr.len() is unsafe and requires unsafe block or unsafe fn", .{});
                }
                return Type{ .primitive = .usize_ };
            }
            tc.addError(.undefined_method, method.span, "len() requires array, tuple, string, list, map, or set", .{});
            return tc.type_builder.unknownType();
        }

        // Check for is_empty method on strings, arrays, lists, maps, sets, and String
        if (std.mem.eql(u8, method.method_name, "is_empty")) {
            if (object_type == .primitive and object_type.primitive == .string_) {
                return tc.type_builder.boolType();
            }
            if (object_type == .list or object_type == .string_data or object_type == .map or object_type == .set) {
                return tc.type_builder.boolType();
            }
            if (object_type == .array or object_type == .slice) {
                return tc.type_builder.boolType();
            }
        }

        // Eq trait: eq() method on primitives and structs that implement Eq
        if (std.mem.eql(u8, method.method_name, "eq")) {
            // Check argument count - eq takes exactly one argument (other)
            if (method.args.len != 1) {
                tc.addError(.invalid_call, method.span, "eq() expects exactly 1 argument", .{});
                return tc.type_builder.boolType();
            }

            const arg_type = tc.checkExpr(method.args[0]);

            // Primitives have builtin Eq implementation
            if (object_type == .primitive) {
                // Check that the argument type matches (or is a reference to the same type)
                const expected_type = object_type;
                var actual_type = arg_type;

                // Handle reference types - unwrap if necessary
                if (arg_type == .reference) {
                    actual_type = arg_type.reference.inner;
                }

                if (!actual_type.eql(expected_type)) {
                    tc.addError(.type_mismatch, method.span, "eq() argument type mismatch: expected same type as receiver", .{});
                }
                return tc.type_builder.boolType();
            }

            // For struct types, check if the struct implements Eq
            if (object_type == .struct_) {
                const struct_name = object_type.struct_.name;
                if (tc.typeImplementsTrait(struct_name, "Eq")) {
                    // Check argument type matches
                    var actual_type = arg_type;
                    if (arg_type == .reference) {
                        actual_type = arg_type.reference.inner;
                    }
                    if (!actual_type.eql(object_type)) {
                        tc.addError(.type_mismatch, method.span, "eq() argument type mismatch", .{});
                    }
                    return tc.type_builder.boolType();
                }
            }

            // Type variable with Eq bound - handled below in the generic type_var section
        }

        // Ordered trait: lt(), le(), gt(), ge() methods on primitives and structs that implement Ordered
        if (std.mem.eql(u8, method.method_name, "lt") or
            std.mem.eql(u8, method.method_name, "le") or
            std.mem.eql(u8, method.method_name, "gt") or
            std.mem.eql(u8, method.method_name, "ge"))
        {
            // Check argument count - comparison methods take exactly one argument (other)
            if (method.args.len != 1) {
                tc.addError(.invalid_call, method.span, "{s}() expects exactly 1 argument", .{method.method_name});
                return tc.type_builder.boolType();
            }

            const arg_type = tc.checkExpr(method.args[0]);

            // Primitives have builtin Ordered implementation (int, float, string)
            if (object_type == .primitive) {
                const prim = object_type.primitive;
                // Only numeric types and strings support ordering
                if (prim.isInteger() or prim.isFloat() or prim == .string_) {
                    // Check that the argument type matches (or is a reference to the same type)
                    const expected_type = object_type;
                    var actual_type = arg_type;

                    // Handle reference types - unwrap if necessary
                    if (arg_type == .reference) {
                        actual_type = arg_type.reference.inner;
                    }

                    if (!actual_type.eql(expected_type)) {
                        tc.addError(.type_mismatch, method.span, "{s}() argument type mismatch: expected same type as receiver", .{method.method_name});
                    }
                    return tc.type_builder.boolType();
                } else {
                    // bool type doesn't support ordering
                    tc.addError(.invalid_call, method.span, "{s}() is not defined for type {s}", .{ method.method_name, @tagName(prim) });
                    return tc.type_builder.boolType();
                }
            }

            // For struct types, check if the struct implements Ordered
            if (object_type == .struct_) {
                const struct_name = object_type.struct_.name;
                if (tc.typeImplementsTrait(struct_name, "Ordered")) {
                    // Check argument type matches
                    var actual_type = arg_type;
                    if (arg_type == .reference) {
                        actual_type = arg_type.reference.inner;
                    }
                    if (!actual_type.eql(object_type)) {
                        tc.addError(.type_mismatch, method.span, "{s}() argument type mismatch", .{method.method_name});
                    }
                    return tc.type_builder.boolType();
                }
            }

            // Type variable with Ordered bound - handled below in the generic type_var section
        }

        // Clone trait: clone() method on primitives and structs that implement Clone
        if (std.mem.eql(u8, method.method_name, "clone")) {
            // clone() takes no arguments
            if (method.args.len != 0) {
                tc.addError(.invalid_call, method.span, "clone() expects no arguments", .{});
                return tc.type_builder.unknownType();
            }

            // All primitives have builtin Clone implementation (value types are trivially cloneable)
            if (object_type == .primitive) {
                return object_type;
            }

            // For struct types, check if the struct implements Clone
            if (object_type == .struct_) {
                const struct_name = object_type.struct_.name;
                if (tc.typeImplementsTrait(struct_name, "Clone")) {
                    return object_type;
                }
            }

            // Type variable with Clone bound - handled below in the generic type_var section
        }

        // Drop trait: drop() method for explicit destruction
        // Unlike Clone/Eq/Ordered, Drop is NOT automatically provided for primitives.
        // Only types that explicitly implement Drop can have drop() called on them.
        if (std.mem.eql(u8, method.method_name, "drop")) {
            // drop() takes no arguments (only self)
            if (method.args.len != 0) {
                tc.addError(.invalid_call, method.span, "drop() expects no arguments", .{});
                return tc.type_builder.voidType();
            }

            // Primitives do NOT implement Drop - they are trivially destroyed
            if (object_type == .primitive) {
                tc.addError(.undefined_method, method.span, "primitives do not implement Drop; they are trivially destroyed", .{});
                return tc.type_builder.voidType();
            }

            // For struct types, check if the struct implements Drop
            if (object_type == .struct_) {
                const struct_name = object_type.struct_.name;
                if (tc.typeImplementsTrait(struct_name, "Drop")) {
                    return tc.type_builder.voidType();
                } else {
                    tc.addError(.undefined_method, method.span, "type '{s}' does not implement Drop", .{struct_name});
                    return tc.type_builder.voidType();
                }
            }

            // Type variable with Drop bound - handled below in the generic type_var section
        }

        // Hash trait: hash() method returns i64 hash code
        // All primitives have builtin Hash implementation.
        if (std.mem.eql(u8, method.method_name, "hash")) {
            // hash() takes no arguments (only self)
            if (method.args.len != 0) {
                tc.addError(.invalid_call, method.span, "hash() expects no arguments", .{});
                return tc.type_builder.i64Type();
            }

            // All primitives have builtin Hash implementation
            if (object_type == .primitive) {
                return tc.type_builder.i64Type();
            }

            // For struct types, check if the struct implements Hash
            if (object_type == .struct_) {
                const struct_name = object_type.struct_.name;
                if (tc.typeImplementsTrait(struct_name, "Hash")) {
                    return tc.type_builder.i64Type();
                } else {
                    tc.addError(.undefined_method, method.span, "type '{s}' does not implement Hash", .{struct_name});
                    return tc.type_builder.i64Type();
                }
            }

            // Type variable with Hash bound - handled below in the generic type_var section
        }

        // String methods
        if (object_type == .primitive and object_type.primitive == .string_) {
            if (std.mem.eql(u8, method.method_name, "contains") or
                std.mem.eql(u8, method.method_name, "starts_with") or
                std.mem.eql(u8, method.method_name, "ends_with"))
            {
                return tc.type_builder.boolType();
            }
            if (std.mem.eql(u8, method.method_name, "trim") or
                std.mem.eql(u8, method.method_name, "to_uppercase") or
                std.mem.eql(u8, method.method_name, "to_lowercase"))
            {
                return tc.type_builder.stringType();
            }
            if (std.mem.eql(u8, method.method_name, "chars")) {
                // Return slice of char
                return tc.type_builder.sliceType(tc.type_builder.charType()) catch return tc.type_builder.unknownType();
            }
            if (std.mem.eql(u8, method.method_name, "bytes")) {
                // Return slice of u8
                const u8_type: Type = .{ .primitive = .u8_ };
                return tc.type_builder.sliceType(u8_type) catch return tc.type_builder.unknownType();
            }
            if (std.mem.eql(u8, method.method_name, "slice")) {
                // slice(start, end) takes exactly 2 integer arguments
                if (method.args.len != 2) {
                    tc.addError(.invalid_call, method.span, "slice() takes exactly 2 arguments (start, end)", .{});
                } else {
                    const start_type = tc.checkExpr(method.args[0]);
                    const end_type = tc.checkExpr(method.args[1]);
                    if (!start_type.isInteger()) {
                        tc.addError(.type_mismatch, method.span, "slice() start index must be an integer", .{});
                    }
                    if (!end_type.isInteger()) {
                        tc.addError(.type_mismatch, method.span, "slice() end index must be an integer", .{});
                    }
                }
                return tc.type_builder.stringType();
            }
            // FFI: as_cstr() -> CStr - borrows string as C string (string literals are already null-terminated)
            if (std.mem.eql(u8, method.method_name, "as_cstr")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "as_cstr() takes no arguments", .{});
                }
                return tc.type_builder.cstrType();
            }
            // FFI: to_cstr() -> CStrOwned - allocates owned null-terminated copy
            if (std.mem.eql(u8, method.method_name, "to_cstr")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "to_cstr() takes no arguments", .{});
                }
                return tc.type_builder.cstrOwnedType();
            }
        }

        // CStr methods (FFI: null-terminated C string)
        if (object_type == .cstr) {
            // CStr.to_string() -> string - unsafe, copies C string to Klar string
            if (std.mem.eql(u8, method.method_name, "to_string")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "to_string() takes no arguments", .{});
                }
                if (!tc.in_unsafe_context) {
                    tc.addError(.invalid_call, method.span, "CStr.to_string() is unsafe and requires unsafe block or unsafe fn", .{});
                }
                return tc.type_builder.stringDataType() catch tc.type_builder.unknownType();
            }
            // CStr.len() -> usize - unsafe, reads from pointer to find null terminator
            if (std.mem.eql(u8, method.method_name, "len")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "len() takes no arguments", .{});
                }
                if (!tc.in_unsafe_context) {
                    tc.addError(.invalid_call, method.span, "CStr.len() is unsafe and requires unsafe block or unsafe fn", .{});
                }
                return Type{ .primitive = .usize_ };
            }
        }

        // CStrOwned methods (FFI: owned null-terminated C string)
        if (object_type == .cstr_owned) {
            // CStrOwned.as_cstr() -> CStr - borrow as CStr for passing to C functions
            if (std.mem.eql(u8, method.method_name, "as_cstr")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "as_cstr() takes no arguments", .{});
                }
                return tc.type_builder.cstrType();
            }
            // CStrOwned.to_string() -> String - unsafe, copies to Klar String
            if (std.mem.eql(u8, method.method_name, "to_string")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "to_string() takes no arguments", .{});
                }
                if (!tc.in_unsafe_context) {
                    tc.addError(.invalid_call, method.span, "CStrOwned.to_string() is unsafe and requires unsafe block or unsafe fn", .{});
                }
                return tc.type_builder.stringDataType() catch tc.type_builder.unknownType();
            }
            // CStrOwned.len() -> usize - unsafe, reads from pointer to find null terminator
            if (std.mem.eql(u8, method.method_name, "len")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "len() takes no arguments", .{});
                }
                if (!tc.in_unsafe_context) {
                    tc.addError(.invalid_call, method.span, "CStrOwned.len() is unsafe and requires unsafe block or unsafe fn", .{});
                }
                return Type{ .primitive = .usize_ };
            }
        }

        // Integer methods
        if (object_type.isInteger()) {
            if (std.mem.eql(u8, method.method_name, "abs")) {
                // abs() takes no arguments
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "abs() takes no arguments", .{});
                }
                return object_type;
            }
            if (std.mem.eql(u8, method.method_name, "min") or
                std.mem.eql(u8, method.method_name, "max"))
            {
                // min(other) and max(other) take exactly one argument of the same type
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "{s}() takes exactly 1 argument", .{method.method_name});
                } else {
                    const arg_type = tc.checkExpr(method.args[0]);
                    if (!arg_type.eql(object_type)) {
                        tc.addError(.type_mismatch, method.span, "{s}() argument must match receiver type", .{method.method_name});
                    }
                }
                return object_type;
            }
        }

        // Array methods
        if (object_type == .array or object_type == .slice) {
            const element_type = if (object_type == .array) object_type.array.element else object_type.slice.element;

            if (std.mem.eql(u8, method.method_name, "first") or
                std.mem.eql(u8, method.method_name, "last"))
            {
                // first() and last() take no arguments, return Optional[element_type]
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "{s}() takes no arguments", .{method.method_name});
                }
                return tc.type_builder.optionalType(element_type) catch tc.type_builder.unknownType();
            }
            if (std.mem.eql(u8, method.method_name, "get")) {
                // get(index) returns Optional[element_type]
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "get() takes exactly 1 argument (index)", .{});
                } else {
                    const arg_type = tc.checkExpr(method.args[0]);
                    if (!arg_type.isInteger()) {
                        tc.addError(.type_mismatch, method.span, "get() index must be an integer", .{});
                    }
                }
                return tc.type_builder.optionalType(element_type) catch tc.type_builder.unknownType();
            }
            if (std.mem.eql(u8, method.method_name, "contains")) {
                // contains(value) checks if array contains the value
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "contains() takes exactly 1 argument", .{});
                } else {
                    const arg_type = tc.checkExpr(method.args[0]);
                    if (!arg_type.eql(element_type)) {
                        tc.addError(.type_mismatch, method.span, "contains() argument must match array element type", .{});
                    }
                }
                return tc.type_builder.boolType();
            }
        }

        // Optional methods
        if (object_type == .optional) {
            if (std.mem.eql(u8, method.method_name, "is_some") or
                std.mem.eql(u8, method.method_name, "is_none"))
            {
                return tc.type_builder.boolType();
            }
            if (std.mem.eql(u8, method.method_name, "unwrap") or
                std.mem.eql(u8, method.method_name, "unwrap_or") or
                std.mem.eql(u8, method.method_name, "expect"))
            {
                return object_type.optional.*;
            }
            // map(f: fn(T) -> U) -> ?U
            if (std.mem.eql(u8, method.method_name, "map")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "map() takes exactly 1 argument (a function)", .{});
                    return tc.type_builder.unknownType();
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (arg_type == .function) {
                    // Return Optional of the function's return type
                    return tc.type_builder.optionalType(arg_type.function.return_type) catch tc.type_builder.unknownType();
                }
                tc.addError(.type_mismatch, method.span, "map() argument must be a function", .{});
                return tc.type_builder.unknownType();
            }
            // and_then(f: fn(T) -> ?U) -> ?U
            if (std.mem.eql(u8, method.method_name, "and_then")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "and_then() takes exactly 1 argument (a function)", .{});
                    return tc.type_builder.unknownType();
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (arg_type == .function) {
                    // Return the function's return type directly (should be Optional)
                    return arg_type.function.return_type;
                }
                tc.addError(.type_mismatch, method.span, "and_then() argument must be a function", .{});
                return tc.type_builder.unknownType();
            }
            // eq(other: ?T) -> bool (Eq trait for Optional)
            if (std.mem.eql(u8, method.method_name, "eq")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "eq() expects exactly 1 argument", .{});
                    return tc.type_builder.boolType();
                }
                const arg_type = tc.checkExpr(method.args[0]);
                // Argument should be the same Optional type (or a reference to it)
                var actual_arg_type = arg_type;
                if (arg_type == .reference) {
                    actual_arg_type = arg_type.reference.inner;
                }
                if (!actual_arg_type.eql(object_type)) {
                    tc.addError(.type_mismatch, method.span, "eq() argument type mismatch: expected same Optional type", .{});
                }
                return tc.type_builder.boolType();
            }
            // clone() -> ?T (Clone trait for Optional)
            if (std.mem.eql(u8, method.method_name, "clone")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "clone() expects no arguments", .{});
                }
                return object_type;
            }
        }

        // Result methods
        if (object_type == .result) {
            const result_type = object_type.result;

            // is_ok() -> bool
            if (std.mem.eql(u8, method.method_name, "is_ok")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "is_ok() takes no arguments", .{});
                }
                return tc.type_builder.boolType();
            }

            // is_err() -> bool
            if (std.mem.eql(u8, method.method_name, "is_err")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "is_err() takes no arguments", .{});
                }
                return tc.type_builder.boolType();
            }

            // unwrap() -> T (panics on Err)
            if (std.mem.eql(u8, method.method_name, "unwrap")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "unwrap() takes no arguments", .{});
                }
                return result_type.ok_type;
            }

            // unwrap_err() -> E (panics on Ok)
            if (std.mem.eql(u8, method.method_name, "unwrap_err")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "unwrap_err() takes no arguments", .{});
                }
                return result_type.err_type;
            }

            // unwrap_or(default) -> T
            if (std.mem.eql(u8, method.method_name, "unwrap_or")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "unwrap_or() takes exactly 1 argument", .{});
                } else {
                    const arg_type = tc.checkExpr(method.args[0]);
                    if (!arg_type.eql(result_type.ok_type)) {
                        tc.addError(.type_mismatch, method.span, "unwrap_or argument must match Ok type", .{});
                    }
                }
                return result_type.ok_type;
            }

            // expect(msg) -> T (panics with message on Err)
            if (std.mem.eql(u8, method.method_name, "expect")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "expect() takes exactly 1 argument", .{});
                }
                return result_type.ok_type;
            }

            // ok() -> ?T (converts to Optional)
            if (std.mem.eql(u8, method.method_name, "ok")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "ok() takes no arguments", .{});
                }
                return tc.type_builder.optionalType(result_type.ok_type) catch tc.type_builder.unknownType();
            }

            // err() -> ?E (converts to Optional)
            if (std.mem.eql(u8, method.method_name, "err")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "err() takes no arguments", .{});
                }
                return tc.type_builder.optionalType(result_type.err_type) catch tc.type_builder.unknownType();
            }

            // map(f: fn(T) -> U) -> Result[U, E]
            if (std.mem.eql(u8, method.method_name, "map")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "map() takes exactly 1 argument (a function)", .{});
                    return tc.type_builder.unknownType();
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (arg_type == .function) {
                    // Return Result[U, E] where U is the function's return type
                    return tc.type_builder.resultType(arg_type.function.return_type, result_type.err_type) catch tc.type_builder.unknownType();
                }
                tc.addError(.type_mismatch, method.span, "map() argument must be a function", .{});
                return tc.type_builder.unknownType();
            }

            // and_then(f: fn(T) -> Result[U, E]) -> Result[U, E]
            if (std.mem.eql(u8, method.method_name, "and_then")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "and_then() takes exactly 1 argument (a function)", .{});
                    return tc.type_builder.unknownType();
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (arg_type == .function) {
                    // Return the function's return type directly (should be Result)
                    return arg_type.function.return_type;
                }
                tc.addError(.type_mismatch, method.span, "and_then() argument must be a function", .{});
                return tc.type_builder.unknownType();
            }

            // map_err(f: fn(E) -> F) -> Result[T, F]
            if (std.mem.eql(u8, method.method_name, "map_err")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "map_err() takes exactly 1 argument (a function)", .{});
                    return tc.type_builder.unknownType();
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (arg_type == .function) {
                    // Return Result[T, F] where T is unchanged and F is the function's return type
                    return tc.type_builder.resultType(result_type.ok_type, arg_type.function.return_type) catch tc.type_builder.unknownType();
                }
                tc.addError(.type_mismatch, method.span, "map_err() argument must be a function", .{});
                return tc.type_builder.unknownType();
            }
            // eq(other: Result[T, E]) -> bool (Eq trait for Result)
            if (std.mem.eql(u8, method.method_name, "eq")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "eq() expects exactly 1 argument", .{});
                    return tc.type_builder.boolType();
                }
                const arg_type = tc.checkExpr(method.args[0]);
                // Argument should be the same Result type (or a reference to it)
                var actual_arg_type = arg_type;
                if (arg_type == .reference) {
                    actual_arg_type = arg_type.reference.inner;
                }
                if (!actual_arg_type.eql(object_type)) {
                    tc.addError(.type_mismatch, method.span, "eq() argument type mismatch: expected same Result type", .{});
                }
                return tc.type_builder.boolType();
            }
            // clone() -> Result[T, E] (Clone trait for Result)
            if (std.mem.eql(u8, method.method_name, "clone")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "clone() expects no arguments", .{});
                }
                return object_type;
            }

            // context(msg: string) -> Result[T, ContextError[E]]
            // Wraps the error type with a context message
            if (std.mem.eql(u8, method.method_name, "context")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "context() takes exactly 1 argument (a string message)", .{});
                    return tc.type_builder.unknownType();
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (arg_type != .primitive or arg_type.primitive != .string_) {
                    tc.addError(.type_mismatch, method.span, "context() argument must be a string", .{});
                }
                // Return Result[T, ContextError[E]]
                const context_err_type = tc.type_builder.contextErrorType(result_type.err_type) catch return tc.type_builder.unknownType();
                return tc.type_builder.resultType(result_type.ok_type, context_err_type) catch tc.type_builder.unknownType();
            }
        }

        // Range methods
        if (object_type == .range) {
            const range_type = object_type.range;
            const element_type = range_type.element_type;

            // next(&mut self) -> ?T (Iterator trait method)
            if (std.mem.eql(u8, method.method_name, "next")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "next() takes no arguments (only &mut self)", .{});
                }
                return tc.type_builder.optionalType(element_type) catch tc.type_builder.unknownType();
            }

            // reset(&mut self) -> void
            if (std.mem.eql(u8, method.method_name, "reset")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "reset() takes no arguments", .{});
                }
                return tc.type_builder.voidType();
            }

            // is_empty(&self) -> bool
            if (std.mem.eql(u8, method.method_name, "is_empty")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "is_empty() takes no arguments", .{});
                }
                return tc.type_builder.boolType();
            }

            // len(&self) -> i32
            if (std.mem.eql(u8, method.method_name, "len")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "len() takes no arguments", .{});
                }
                return tc.type_builder.i32Type();
            }

            // clone() -> Range[T] (Clone trait)
            if (std.mem.eql(u8, method.method_name, "clone")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "clone() expects no arguments", .{});
                }
                return object_type;
            }
        }

        // ContextError methods
        if (object_type == .context_error) {
            const context_error_type = object_type.context_error;

            // message() -> string - returns the context message
            if (std.mem.eql(u8, method.method_name, "message")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "message() takes no arguments", .{});
                }
                return tc.type_builder.stringType();
            }

            // cause() -> E - returns the original error
            if (std.mem.eql(u8, method.method_name, "cause")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "cause() takes no arguments", .{});
                }
                return context_error_type.inner_type;
            }

            // display_chain() -> string - returns formatted error chain
            if (std.mem.eql(u8, method.method_name, "display_chain")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "display_chain() takes no arguments", .{});
                }
                return tc.type_builder.stringType();
            }
        }

        // List methods
        if (object_type == .list) {
            const list_type = object_type.list;
            const element_type = list_type.element;

            // push(&mut self, value: T) -> void
            if (std.mem.eql(u8, method.method_name, "push")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "push() expects exactly 1 argument", .{});
                    return tc.type_builder.voidType();
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (!arg_type.eql(element_type)) {
                    tc.addError(.type_mismatch, method.span, "push() argument type mismatch", .{});
                }
                return tc.type_builder.voidType();
            }

            // pop(&mut self) -> ?T
            if (std.mem.eql(u8, method.method_name, "pop")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "pop() takes no arguments", .{});
                }
                return tc.type_builder.optionalType(element_type) catch tc.type_builder.unknownType();
            }

            // get(&self, index: i32) -> ?T
            if (std.mem.eql(u8, method.method_name, "get")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "get() expects exactly 1 argument", .{});
                    return tc.type_builder.optionalType(element_type) catch tc.type_builder.unknownType();
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (!arg_type.isInteger()) {
                    tc.addError(.type_mismatch, method.span, "get() index must be an integer", .{});
                }
                return tc.type_builder.optionalType(element_type) catch tc.type_builder.unknownType();
            }

            // set(&mut self, index: i32, value: T) -> void
            if (std.mem.eql(u8, method.method_name, "set")) {
                if (method.args.len != 2) {
                    tc.addError(.invalid_call, method.span, "set() expects exactly 2 arguments (index, value)", .{});
                    return tc.type_builder.voidType();
                }
                const index_type = tc.checkExpr(method.args[0]);
                if (!index_type.isInteger()) {
                    tc.addError(.type_mismatch, method.span, "set() index must be an integer", .{});
                }
                const value_type = tc.checkExpr(method.args[1]);
                if (!value_type.eql(element_type)) {
                    tc.addError(.type_mismatch, method.span, "set() value type mismatch", .{});
                }
                return tc.type_builder.voidType();
            }

            // len(&self) -> i32
            if (std.mem.eql(u8, method.method_name, "len")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "len() takes no arguments", .{});
                }
                return tc.type_builder.i32Type();
            }

            // is_empty(&self) -> bool
            if (std.mem.eql(u8, method.method_name, "is_empty")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "is_empty() takes no arguments", .{});
                }
                return tc.type_builder.boolType();
            }

            // first(&self) -> ?T
            if (std.mem.eql(u8, method.method_name, "first")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "first() takes no arguments", .{});
                }
                return tc.type_builder.optionalType(element_type) catch tc.type_builder.unknownType();
            }

            // last(&self) -> ?T
            if (std.mem.eql(u8, method.method_name, "last")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "last() takes no arguments", .{});
                }
                return tc.type_builder.optionalType(element_type) catch tc.type_builder.unknownType();
            }

            // clear(&mut self) -> void
            if (std.mem.eql(u8, method.method_name, "clear")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "clear() takes no arguments", .{});
                }
                return tc.type_builder.voidType();
            }

            // capacity(&self) -> i32
            if (std.mem.eql(u8, method.method_name, "capacity")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "capacity() takes no arguments", .{});
                }
                return tc.type_builder.i32Type();
            }

            // clone(&self) -> List[T] (creates a deep copy of the list)
            if (std.mem.eql(u8, method.method_name, "clone")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "clone() takes no arguments", .{});
                }
                return object_type; // Returns same List[T] type
            }

            // drop(&mut self) -> void (frees list memory)
            if (std.mem.eql(u8, method.method_name, "drop")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "drop() takes no arguments", .{});
                }
                return tc.type_builder.voidType();
            }

            // take(n: i32) -> List[T] (returns first n elements)
            if (std.mem.eql(u8, method.method_name, "take")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "take() expects exactly 1 argument (count)", .{});
                    return object_type;
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (arg_type != .primitive or arg_type.primitive != .i32_) {
                    tc.addError(.type_mismatch, method.span, "take() argument must be i32", .{});
                }
                return object_type; // Returns same List[T] type
            }

            // skip(n: i32) -> List[T] (skips first n elements)
            if (std.mem.eql(u8, method.method_name, "skip")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "skip() expects exactly 1 argument (count)", .{});
                    return object_type;
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (arg_type != .primitive or arg_type.primitive != .i32_) {
                    tc.addError(.type_mismatch, method.span, "skip() argument must be i32", .{});
                }
                return object_type; // Returns same List[T] type
            }

            // filter(fn(T) -> bool) -> List[T] (keeps elements where predicate returns true)
            if (std.mem.eql(u8, method.method_name, "filter")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "filter() expects exactly 1 argument (predicate)", .{});
                    return object_type;
                }
                const arg_type = tc.checkExpr(method.args[0]);
                // Validate it's a function fn(T) -> bool
                if (arg_type != .function) {
                    tc.addError(.type_mismatch, method.span, "filter() argument must be a function", .{});
                    return object_type;
                }
                const fn_type = arg_type.function;
                if (fn_type.params.len != 1) {
                    tc.addError(.type_mismatch, method.span, "filter() predicate must take exactly 1 parameter", .{});
                    return object_type;
                }
                if (!fn_type.params[0].eql(element_type)) {
                    tc.addError(.type_mismatch, method.span, "filter() predicate parameter type must match list element type", .{});
                }
                if (fn_type.return_type != .primitive or fn_type.return_type.primitive != .bool_) {
                    tc.addError(.type_mismatch, method.span, "filter() predicate must return bool", .{});
                }
                return object_type; // Returns same List[T] type
            }

            // map(fn(T) -> U) -> List[U] (transforms each element)
            if (std.mem.eql(u8, method.method_name, "map")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "map() expects exactly 1 argument (transform function)", .{});
                    return object_type;
                }
                const arg_type = tc.checkExpr(method.args[0]);
                // Validate it's a function fn(T) -> U
                if (arg_type != .function) {
                    tc.addError(.type_mismatch, method.span, "map() argument must be a function", .{});
                    return object_type;
                }
                const fn_type = arg_type.function;
                if (fn_type.params.len != 1) {
                    tc.addError(.type_mismatch, method.span, "map() transform function must take exactly 1 parameter", .{});
                    return object_type;
                }
                if (!fn_type.params[0].eql(element_type)) {
                    tc.addError(.type_mismatch, method.span, "map() transform function parameter type must match list element type", .{});
                }
                // Return List[U] where U is the function's return type
                return tc.type_builder.listType(fn_type.return_type) catch tc.type_builder.unknownType();
            }

            // enumerate() -> List[(i32, T)] (pairs each element with its index)
            if (std.mem.eql(u8, method.method_name, "enumerate")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "enumerate() takes no arguments", .{});
                }
                // Build tuple type (i32, T)
                const i32_type = tc.type_builder.i32Type();
                const tuple_types = [_]Type{ i32_type, element_type };
                const tuple_type = tc.type_builder.tupleType(&tuple_types) catch tc.type_builder.unknownType();
                return tc.type_builder.listType(tuple_type) catch tc.type_builder.unknownType();
            }

            // zip(other: List[U]) -> List[(T, U)] (combines two lists element-wise)
            if (std.mem.eql(u8, method.method_name, "zip")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "zip() expects exactly 1 argument (other list)", .{});
                    return object_type;
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (arg_type != .list) {
                    tc.addError(.type_mismatch, method.span, "zip() argument must be a List", .{});
                    return object_type;
                }
                const other_element_type = arg_type.list.element;
                // Build tuple type (T, U)
                const tuple_types = [_]Type{ element_type, other_element_type };
                const tuple_type = tc.type_builder.tupleType(&tuple_types) catch tc.type_builder.unknownType();
                return tc.type_builder.listType(tuple_type) catch tc.type_builder.unknownType();
            }
        }

        // Map methods
        if (object_type == .map) {
            const map_type = object_type.map;
            const key_type = map_type.key;
            const value_type = map_type.value;

            // insert(&mut self, key: K, value: V) -> void
            if (std.mem.eql(u8, method.method_name, "insert")) {
                if (method.args.len != 2) {
                    tc.addError(.invalid_call, method.span, "insert() expects exactly 2 arguments (key, value)", .{});
                    return tc.type_builder.voidType();
                }
                const arg_key_type = tc.checkExpr(method.args[0]);
                if (!arg_key_type.eql(key_type)) {
                    tc.addError(.type_mismatch, method.span, "insert() key type mismatch", .{});
                }
                const arg_value_type = tc.checkExpr(method.args[1]);
                if (!arg_value_type.eql(value_type)) {
                    tc.addError(.type_mismatch, method.span, "insert() value type mismatch", .{});
                }
                return tc.type_builder.voidType();
            }

            // get(&self, key: K) -> ?V
            if (std.mem.eql(u8, method.method_name, "get")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "get() expects exactly 1 argument", .{});
                    return tc.type_builder.optionalType(value_type) catch tc.type_builder.unknownType();
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (!arg_type.eql(key_type)) {
                    tc.addError(.type_mismatch, method.span, "get() key type mismatch", .{});
                }
                return tc.type_builder.optionalType(value_type) catch tc.type_builder.unknownType();
            }

            // remove(&mut self, key: K) -> ?V
            if (std.mem.eql(u8, method.method_name, "remove")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "remove() expects exactly 1 argument", .{});
                    return tc.type_builder.optionalType(value_type) catch tc.type_builder.unknownType();
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (!arg_type.eql(key_type)) {
                    tc.addError(.type_mismatch, method.span, "remove() key type mismatch", .{});
                }
                return tc.type_builder.optionalType(value_type) catch tc.type_builder.unknownType();
            }

            // contains_key(&self, key: K) -> bool
            if (std.mem.eql(u8, method.method_name, "contains_key")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "contains_key() expects exactly 1 argument", .{});
                    return tc.type_builder.boolType();
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (!arg_type.eql(key_type)) {
                    tc.addError(.type_mismatch, method.span, "contains_key() key type mismatch", .{});
                }
                return tc.type_builder.boolType();
            }

            // keys(&self) -> List[K]
            if (std.mem.eql(u8, method.method_name, "keys")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "keys() takes no arguments", .{});
                }
                return tc.type_builder.listType(key_type) catch tc.type_builder.unknownType();
            }

            // values(&self) -> List[V]
            if (std.mem.eql(u8, method.method_name, "values")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "values() takes no arguments", .{});
                }
                return tc.type_builder.listType(value_type) catch tc.type_builder.unknownType();
            }

            // len(&self) -> i32
            if (std.mem.eql(u8, method.method_name, "len")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "len() takes no arguments", .{});
                }
                return tc.type_builder.i32Type();
            }

            // is_empty(&self) -> bool
            if (std.mem.eql(u8, method.method_name, "is_empty")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "is_empty() takes no arguments", .{});
                }
                return tc.type_builder.boolType();
            }

            // capacity(&self) -> i32
            if (std.mem.eql(u8, method.method_name, "capacity")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "capacity() takes no arguments", .{});
                }
                return tc.type_builder.i32Type();
            }

            // clear(&mut self) -> void
            if (std.mem.eql(u8, method.method_name, "clear")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "clear() takes no arguments", .{});
                }
                return tc.type_builder.voidType();
            }

            // clone(&self) -> Map[K,V] (creates a deep copy of the map)
            if (std.mem.eql(u8, method.method_name, "clone")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "clone() takes no arguments", .{});
                }
                return object_type; // Returns same Map[K,V] type
            }

            // drop(&mut self) -> void (frees map memory)
            if (std.mem.eql(u8, method.method_name, "drop")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "drop() takes no arguments", .{});
                }
                return tc.type_builder.voidType();
            }

            // take(n: i32) -> Map[K,V] (returns first n entries)
            if (std.mem.eql(u8, method.method_name, "take")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "take() expects exactly 1 argument (count)", .{});
                    return object_type;
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (arg_type != .primitive or arg_type.primitive != .i32_) {
                    tc.addError(.type_mismatch, method.span, "take() argument must be i32", .{});
                }
                return object_type; // Returns same Map[K,V] type
            }

            // skip(n: i32) -> Map[K,V] (skips first n entries)
            if (std.mem.eql(u8, method.method_name, "skip")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "skip() expects exactly 1 argument (count)", .{});
                    return object_type;
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (arg_type != .primitive or arg_type.primitive != .i32_) {
                    tc.addError(.type_mismatch, method.span, "skip() argument must be i32", .{});
                }
                return object_type; // Returns same Map[K,V] type
            }

            // filter(fn(K, V) -> bool) -> Map[K,V] (keeps entries where predicate returns true)
            if (std.mem.eql(u8, method.method_name, "filter")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "filter() expects exactly 1 argument (predicate)", .{});
                    return object_type;
                }
                const arg_type = tc.checkExpr(method.args[0]);
                // Validate it's a function fn(K, V) -> bool
                if (arg_type != .function) {
                    tc.addError(.type_mismatch, method.span, "filter() argument must be a function", .{});
                    return object_type;
                }
                const fn_type = arg_type.function;
                if (fn_type.params.len != 2) {
                    tc.addError(.type_mismatch, method.span, "filter() predicate must take exactly 2 parameters (key, value)", .{});
                    return object_type;
                }
                if (!fn_type.params[0].eql(key_type)) {
                    tc.addError(.type_mismatch, method.span, "filter() predicate first parameter type must match map key type", .{});
                }
                if (!fn_type.params[1].eql(value_type)) {
                    tc.addError(.type_mismatch, method.span, "filter() predicate second parameter type must match map value type", .{});
                }
                if (fn_type.return_type != .primitive or fn_type.return_type.primitive != .bool_) {
                    tc.addError(.type_mismatch, method.span, "filter() predicate must return bool", .{});
                }
                return object_type; // Returns same Map[K,V] type
            }

            // map_values(fn(V) -> U) -> Map[K,U] (transforms values, preserves keys)
            if (std.mem.eql(u8, method.method_name, "map_values")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "map_values() expects exactly 1 argument (transform function)", .{});
                    return tc.type_builder.unknownType();
                }
                const arg_type = tc.checkExpr(method.args[0]);
                // Validate it's a function fn(V) -> U
                if (arg_type != .function) {
                    tc.addError(.type_mismatch, method.span, "map_values() argument must be a function", .{});
                    return tc.type_builder.unknownType();
                }
                const fn_type = arg_type.function;
                if (fn_type.params.len != 1) {
                    tc.addError(.type_mismatch, method.span, "map_values() transform function must take exactly 1 parameter", .{});
                    return tc.type_builder.unknownType();
                }
                if (!fn_type.params[0].eql(value_type)) {
                    tc.addError(.type_mismatch, method.span, "map_values() transform function parameter type must match map value type", .{});
                }
                // Return Map[K,U] where U is the function's return type
                return tc.type_builder.mapType(key_type, fn_type.return_type) catch tc.type_builder.unknownType();
            }
        }

        // Set methods
        if (object_type == .set) {
            const set_type = object_type.set;
            const element_type = set_type.element;

            // insert(&mut self, element: T) -> bool (returns true if newly inserted)
            if (std.mem.eql(u8, method.method_name, "insert")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "insert() expects exactly 1 argument (element)", .{});
                    return tc.type_builder.boolType();
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (!arg_type.eql(element_type)) {
                    tc.addError(.type_mismatch, method.span, "insert() element type mismatch", .{});
                }
                return tc.type_builder.boolType();
            }

            // contains(&self, element: T) -> bool
            if (std.mem.eql(u8, method.method_name, "contains")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "contains() expects exactly 1 argument", .{});
                    return tc.type_builder.boolType();
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (!arg_type.eql(element_type)) {
                    tc.addError(.type_mismatch, method.span, "contains() element type mismatch", .{});
                }
                return tc.type_builder.boolType();
            }

            // remove(&mut self, element: T) -> bool (returns true if removed)
            if (std.mem.eql(u8, method.method_name, "remove")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "remove() expects exactly 1 argument", .{});
                    return tc.type_builder.boolType();
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (!arg_type.eql(element_type)) {
                    tc.addError(.type_mismatch, method.span, "remove() element type mismatch", .{});
                }
                return tc.type_builder.boolType();
            }

            // len(&self) -> i32
            if (std.mem.eql(u8, method.method_name, "len")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "len() takes no arguments", .{});
                }
                return tc.type_builder.i32Type();
            }

            // is_empty(&self) -> bool
            if (std.mem.eql(u8, method.method_name, "is_empty")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "is_empty() takes no arguments", .{});
                }
                return tc.type_builder.boolType();
            }

            // capacity(&self) -> i32
            if (std.mem.eql(u8, method.method_name, "capacity")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "capacity() takes no arguments", .{});
                }
                return tc.type_builder.i32Type();
            }

            // clear(&mut self) -> void
            if (std.mem.eql(u8, method.method_name, "clear")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "clear() takes no arguments", .{});
                }
                return tc.type_builder.voidType();
            }

            // clone(&self) -> Set[T] (creates a deep copy of the set)
            if (std.mem.eql(u8, method.method_name, "clone")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "clone() takes no arguments", .{});
                }
                return object_type; // Returns same Set[T] type
            }

            // drop(&mut self) -> void (frees set memory)
            if (std.mem.eql(u8, method.method_name, "drop")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "drop() takes no arguments", .{});
                }
                return tc.type_builder.voidType();
            }

            // union(&self, other: Set[T]) -> Set[T] (elements in either set)
            if (std.mem.eql(u8, method.method_name, "union")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "union() expects exactly 1 argument", .{});
                    return object_type;
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (!arg_type.eql(object_type)) {
                    tc.addError(.type_mismatch, method.span, "union() expects a Set with same element type", .{});
                }
                return object_type;
            }

            // intersection(&self, other: Set[T]) -> Set[T] (elements in both sets)
            if (std.mem.eql(u8, method.method_name, "intersection")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "intersection() expects exactly 1 argument", .{});
                    return object_type;
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (!arg_type.eql(object_type)) {
                    tc.addError(.type_mismatch, method.span, "intersection() expects a Set with same element type", .{});
                }
                return object_type;
            }

            // difference(&self, other: Set[T]) -> Set[T] (elements in self but not other)
            if (std.mem.eql(u8, method.method_name, "difference")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "difference() expects exactly 1 argument", .{});
                    return object_type;
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (!arg_type.eql(object_type)) {
                    tc.addError(.type_mismatch, method.span, "difference() expects a Set with same element type", .{});
                }
                return object_type;
            }

            // take(n: i32) -> Set[T] (returns first n elements)
            if (std.mem.eql(u8, method.method_name, "take")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "take() expects exactly 1 argument (count)", .{});
                    return object_type;
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (arg_type != .primitive or arg_type.primitive != .i32_) {
                    tc.addError(.type_mismatch, method.span, "take() argument must be i32", .{});
                }
                return object_type; // Returns same Set[T] type
            }

            // skip(n: i32) -> Set[T] (skips first n elements)
            if (std.mem.eql(u8, method.method_name, "skip")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "skip() expects exactly 1 argument (count)", .{});
                    return object_type;
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (arg_type != .primitive or arg_type.primitive != .i32_) {
                    tc.addError(.type_mismatch, method.span, "skip() argument must be i32", .{});
                }
                return object_type; // Returns same Set[T] type
            }

            // filter(fn(T) -> bool) -> Set[T] (keeps elements where predicate returns true)
            if (std.mem.eql(u8, method.method_name, "filter")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "filter() expects exactly 1 argument (predicate)", .{});
                    return object_type;
                }
                const arg_type = tc.checkExpr(method.args[0]);
                // Validate it's a function fn(T) -> bool
                if (arg_type != .function) {
                    tc.addError(.type_mismatch, method.span, "filter() argument must be a function", .{});
                    return object_type;
                }
                const fn_type = arg_type.function;
                if (fn_type.params.len != 1) {
                    tc.addError(.type_mismatch, method.span, "filter() predicate must take exactly 1 parameter", .{});
                    return object_type;
                }
                if (!fn_type.params[0].eql(element_type)) {
                    tc.addError(.type_mismatch, method.span, "filter() predicate parameter type must match set element type", .{});
                }
                if (fn_type.return_type != .primitive or fn_type.return_type.primitive != .bool_) {
                    tc.addError(.type_mismatch, method.span, "filter() predicate must return bool", .{});
                }
                return object_type; // Returns same Set[T] type
            }

            // map(fn(T) -> U) -> List[U] (transforms each element, returns List since U may not be hashable)
            if (std.mem.eql(u8, method.method_name, "map")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "map() expects exactly 1 argument (transform function)", .{});
                    return tc.type_builder.unknownType();
                }
                const arg_type = tc.checkExpr(method.args[0]);
                // Validate it's a function fn(T) -> U
                if (arg_type != .function) {
                    tc.addError(.type_mismatch, method.span, "map() argument must be a function", .{});
                    return tc.type_builder.unknownType();
                }
                const fn_type = arg_type.function;
                if (fn_type.params.len != 1) {
                    tc.addError(.type_mismatch, method.span, "map() transform function must take exactly 1 parameter", .{});
                    return tc.type_builder.unknownType();
                }
                if (!fn_type.params[0].eql(element_type)) {
                    tc.addError(.type_mismatch, method.span, "map() transform function parameter type must match set element type", .{});
                }
                // Return List[U] where U is the function's return type
                return tc.type_builder.listType(fn_type.return_type) catch tc.type_builder.unknownType();
            }

            // enumerate() -> List[(i32, T)] (pairs each element with its index)
            if (std.mem.eql(u8, method.method_name, "enumerate")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "enumerate() takes no arguments", .{});
                }
                // Build tuple type (i32, T)
                const i32_type = tc.type_builder.i32Type();
                const tuple_types = [_]Type{ i32_type, element_type };
                const tuple_type = tc.type_builder.tupleType(&tuple_types) catch tc.type_builder.unknownType();
                return tc.type_builder.listType(tuple_type) catch tc.type_builder.unknownType();
            }

            // zip(other: Set[U]) -> List[(T, U)] (combines two sets element-wise)
            if (std.mem.eql(u8, method.method_name, "zip")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "zip() expects exactly 1 argument (other set)", .{});
                    return tc.type_builder.unknownType();
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (arg_type != .set) {
                    tc.addError(.type_mismatch, method.span, "zip() argument must be a Set", .{});
                    return tc.type_builder.unknownType();
                }
                const other_element_type = arg_type.set.element;
                // Build tuple type (T, U)
                const tuple_types = [_]Type{ element_type, other_element_type };
                const tuple_type = tc.type_builder.tupleType(&tuple_types) catch tc.type_builder.unknownType();
                return tc.type_builder.listType(tuple_type) catch tc.type_builder.unknownType();
            }
        }

        // String methods (heap-allocated string)
        if (object_type == .string_data) {
            // len(&self) -> i32
            if (std.mem.eql(u8, method.method_name, "len")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "len() takes no arguments", .{});
                }
                return tc.type_builder.i32Type();
            }

            // is_empty(&self) -> bool
            if (std.mem.eql(u8, method.method_name, "is_empty")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "is_empty() takes no arguments", .{});
                }
                return tc.type_builder.boolType();
            }

            // capacity(&self) -> i32
            if (std.mem.eql(u8, method.method_name, "capacity")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "capacity() takes no arguments", .{});
                }
                return tc.type_builder.i32Type();
            }

            // push(&mut self, c: char) -> void
            if (std.mem.eql(u8, method.method_name, "push")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "push() expects exactly 1 argument", .{});
                    return tc.type_builder.voidType();
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (arg_type != .primitive or arg_type.primitive != .char_) {
                    tc.addError(.type_mismatch, method.span, "push() expects a char argument", .{});
                }
                return tc.type_builder.voidType();
            }

            // concat(&self, other: String) -> String
            if (std.mem.eql(u8, method.method_name, "concat")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "concat() expects exactly 1 argument", .{});
                    return object_type;
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (arg_type != .string_data) {
                    tc.addError(.type_mismatch, method.span, "concat() expects a String argument", .{});
                }
                return object_type; // Returns new String
            }

            // append(&mut self, other: String) -> void
            if (std.mem.eql(u8, method.method_name, "append")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "append() expects exactly 1 argument", .{});
                    return tc.type_builder.voidType();
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (arg_type != .string_data) {
                    tc.addError(.type_mismatch, method.span, "append() expects a String argument", .{});
                }
                return tc.type_builder.voidType();
            }

            // as_str(&self) -> string (get null-terminated C string)
            if (std.mem.eql(u8, method.method_name, "as_str")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "as_str() takes no arguments", .{});
                }
                return tc.type_builder.stringType();
            }

            // as_cstr(&self) -> CStr (FFI: borrow as C string)
            if (std.mem.eql(u8, method.method_name, "as_cstr")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "as_cstr() takes no arguments", .{});
                }
                return tc.type_builder.cstrType();
            }

            // to_cstr(&self) -> CStrOwned (FFI: allocate owned null-terminated copy)
            if (std.mem.eql(u8, method.method_name, "to_cstr")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "to_cstr() takes no arguments", .{});
                }
                return tc.type_builder.cstrOwnedType();
            }

            // clear(&mut self) -> void
            if (std.mem.eql(u8, method.method_name, "clear")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "clear() takes no arguments", .{});
                }
                return tc.type_builder.voidType();
            }

            // clone(&self) -> String
            if (std.mem.eql(u8, method.method_name, "clone")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "clone() takes no arguments", .{});
                }
                return object_type;
            }

            // drop(&mut self) -> void
            if (std.mem.eql(u8, method.method_name, "drop")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "drop() takes no arguments", .{});
                }
                return tc.type_builder.voidType();
            }

            // eq(&self, other: String) -> bool
            if (std.mem.eql(u8, method.method_name, "eq")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "eq() expects exactly 1 argument", .{});
                    return tc.type_builder.boolType();
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (arg_type != .string_data) {
                    tc.addError(.type_mismatch, method.span, "eq() expects a String argument", .{});
                }
                return tc.type_builder.boolType();
            }

            // hash(&self) -> i64
            if (std.mem.eql(u8, method.method_name, "hash")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "hash() takes no arguments", .{});
                }
                return tc.type_builder.i64Type();
            }
        }

        // Rc methods
        if (object_type == .rc) {
            const inner_type = object_type.rc.inner;

            // clone() -> Rc[T] (increments reference count)
            if (std.mem.eql(u8, method.method_name, "clone")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "clone() takes no arguments", .{});
                }
                return object_type; // Returns same Rc[T] type
            }

            // downgrade() -> Weak[T] (creates weak reference)
            if (std.mem.eql(u8, method.method_name, "downgrade")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "downgrade() takes no arguments", .{});
                }
                return tc.type_builder.weakRcType(inner_type) catch tc.type_builder.unknownType();
            }

            // strong_count() -> usize
            if (std.mem.eql(u8, method.method_name, "strong_count")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "strong_count() takes no arguments", .{});
                }
                return .{ .primitive = .usize_ };
            }

            // weak_count() -> usize
            if (std.mem.eql(u8, method.method_name, "weak_count")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "weak_count() takes no arguments", .{});
                }
                return .{ .primitive = .usize_ };
            }

            // To access the inner value of Rc, use Rc.new(Cell.new(value))
            // then call cell.get() and cell.set() for interior mutability
        }

        // Weak methods
        if (object_type == .weak_rc) {
            const inner_type = object_type.weak_rc.inner;

            // clone() -> Weak[T] (increments weak count)
            if (std.mem.eql(u8, method.method_name, "clone")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "clone() takes no arguments", .{});
                }
                return object_type; // Returns same Weak[T] type
            }

            // upgrade() -> ?Rc[T] (attempts to get strong reference)
            if (std.mem.eql(u8, method.method_name, "upgrade")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "upgrade() takes no arguments", .{});
                }
                const rc_type = tc.type_builder.rcType(inner_type) catch return tc.type_builder.unknownType();
                return tc.type_builder.optionalType(rc_type) catch tc.type_builder.unknownType();
            }

            // strong_count() -> usize
            if (std.mem.eql(u8, method.method_name, "strong_count")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "strong_count() takes no arguments", .{});
                }
                return .{ .primitive = .usize_ };
            }

            // weak_count() -> usize
            if (std.mem.eql(u8, method.method_name, "weak_count")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "weak_count() takes no arguments", .{});
                }
                return .{ .primitive = .usize_ };
            }
        }

        // Arc methods (thread-safe atomic reference counting)
        if (object_type == .arc) {
            const inner_type = object_type.arc.inner;

            // clone() -> Arc[T] (atomically increments reference count)
            if (std.mem.eql(u8, method.method_name, "clone")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "clone() takes no arguments", .{});
                }
                return object_type; // Returns same Arc[T] type
            }

            // downgrade() -> WeakArc[T] (creates weak reference)
            if (std.mem.eql(u8, method.method_name, "downgrade")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "downgrade() takes no arguments", .{});
                }
                return tc.type_builder.weakArcType(inner_type) catch tc.type_builder.unknownType();
            }

            // strong_count() -> usize
            if (std.mem.eql(u8, method.method_name, "strong_count")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "strong_count() takes no arguments", .{});
                }
                return .{ .primitive = .usize_ };
            }

            // weak_count() -> usize
            if (std.mem.eql(u8, method.method_name, "weak_count")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "weak_count() takes no arguments", .{});
                }
                return .{ .primitive = .usize_ };
            }

            // To access the inner value of Arc, use Arc.new(Cell.new(value))
            // then call cell.get() and cell.set() for interior mutability
        }

        // WeakArc methods (thread-safe weak references)
        if (object_type == .weak_arc) {
            const inner_type = object_type.weak_arc.inner;

            // clone() -> WeakArc[T] (atomically increments weak count)
            if (std.mem.eql(u8, method.method_name, "clone")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "clone() takes no arguments", .{});
                }
                return object_type; // Returns same WeakArc[T] type
            }

            // upgrade() -> ?Arc[T] (attempts to atomically get strong reference)
            if (std.mem.eql(u8, method.method_name, "upgrade")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "upgrade() takes no arguments", .{});
                }
                const arc_type = tc.type_builder.arcType(inner_type) catch return tc.type_builder.unknownType();
                return tc.type_builder.optionalType(arc_type) catch tc.type_builder.unknownType();
            }

            // strong_count() -> usize
            if (std.mem.eql(u8, method.method_name, "strong_count")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "strong_count() takes no arguments", .{});
                }
                return .{ .primitive = .usize_ };
            }

            // weak_count() -> usize
            if (std.mem.eql(u8, method.method_name, "weak_count")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "weak_count() takes no arguments", .{});
                }
                return .{ .primitive = .usize_ };
            }
        }

        // Cell methods for interior mutability
        if (object_type == .cell) {
            const inner_type = object_type.cell.inner;

            // get() -> T (returns a copy of the inner value)
            if (std.mem.eql(u8, method.method_name, "get")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "get() takes no arguments", .{});
                }
                return inner_type;
            }

            // set(value: T) -> void (sets the inner value)
            if (std.mem.eql(u8, method.method_name, "set")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "set() expects exactly 1 argument", .{});
                    return tc.type_builder.voidType();
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (!arg_type.eql(inner_type)) {
                    tc.addError(.type_mismatch, method.span, "set() argument type mismatch", .{});
                }
                return tc.type_builder.voidType();
            }

            // replace(value: T) -> T (sets new value, returns old value)
            if (std.mem.eql(u8, method.method_name, "replace")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "replace() expects exactly 1 argument", .{});
                    return inner_type;
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (!arg_type.eql(inner_type)) {
                    tc.addError(.type_mismatch, method.span, "replace() argument type mismatch", .{});
                }
                return inner_type;
            }
        }

        // File methods
        if (object_type == .file) {
            // read(&mut self, buf: &mut [u8]) -> Result[i32, IoError]
            if (std.mem.eql(u8, method.method_name, "read")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "read() expects exactly 1 argument (buffer)", .{});
                    return tc.type_builder.resultType(tc.type_builder.i32Type(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
                }
                const buf_type = tc.checkExpr(method.args[0]);
                // Check that argument is a mutable reference to a slice or array of u8
                const is_valid_buf = blk: {
                    if (buf_type != .reference or !buf_type.reference.mutable) break :blk false;
                    const inner = buf_type.reference.inner;
                    if (inner == .slice and inner.slice.element == .primitive and inner.slice.element.primitive == .u8_) break :blk true;
                    if (inner == .array and inner.array.element == .primitive and inner.array.element.primitive == .u8_) break :blk true;
                    break :blk false;
                };
                if (!is_valid_buf) {
                    tc.addError(.type_mismatch, method.span, "read() expects a mutable reference to [u8] or [u8; N] buffer", .{});
                }
                return tc.type_builder.resultType(tc.type_builder.i32Type(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
            }

            // write(&mut self, buf: &[u8]) -> Result[i32, IoError]
            if (std.mem.eql(u8, method.method_name, "write")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "write() expects exactly 1 argument (buffer)", .{});
                    return tc.type_builder.resultType(tc.type_builder.i32Type(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
                }
                const buf_type = tc.checkExpr(method.args[0]);
                // Check that argument is a reference to a slice or array of u8
                const is_valid_buf = blk: {
                    if (buf_type != .reference) break :blk false;
                    const inner = buf_type.reference.inner;
                    if (inner == .slice and inner.slice.element == .primitive and inner.slice.element.primitive == .u8_) break :blk true;
                    if (inner == .array and inner.array.element == .primitive and inner.array.element.primitive == .u8_) break :blk true;
                    break :blk false;
                };
                if (!is_valid_buf) {
                    tc.addError(.type_mismatch, method.span, "write() expects a reference to [u8] or [u8; N] buffer", .{});
                }
                return tc.type_builder.resultType(tc.type_builder.i32Type(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
            }

            // write_string(&mut self, s: string) -> Result[i32, IoError]
            if (std.mem.eql(u8, method.method_name, "write_string")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "write_string() expects exactly 1 argument (string)", .{});
                    return tc.type_builder.resultType(tc.type_builder.i32Type(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
                }
                const str_type = tc.checkExpr(method.args[0]);
                if (str_type != .primitive or str_type.primitive != .string_) {
                    tc.addError(.type_mismatch, method.span, "write_string() expects a string argument", .{});
                }
                return tc.type_builder.resultType(tc.type_builder.i32Type(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
            }

            // close(self: File) -> Result[void, IoError]
            if (std.mem.eql(u8, method.method_name, "close")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "close() takes no arguments", .{});
                }
                return tc.type_builder.resultType(tc.type_builder.voidType(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
            }

            // flush(&mut self) -> Result[void, IoError]
            if (std.mem.eql(u8, method.method_name, "flush")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "flush() takes no arguments", .{});
                }
                return tc.type_builder.resultType(tc.type_builder.voidType(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
            }
        }

        // Path methods
        if (object_type == .path) {
            // join(other: string) -> Path
            if (std.mem.eql(u8, method.method_name, "join")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "join() expects exactly 1 argument (path component)", .{});
                    return tc.type_builder.pathType();
                }
                const arg_type = tc.checkExpr(method.args[0]);
                if (arg_type != .primitive or arg_type.primitive != .string_) {
                    tc.addError(.type_mismatch, method.span, "join() expects a string argument", .{});
                }
                return tc.type_builder.pathType();
            }

            // parent() -> ?Path
            if (std.mem.eql(u8, method.method_name, "parent")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "parent() takes no arguments", .{});
                }
                return tc.type_builder.optionalType(tc.type_builder.pathType()) catch tc.type_builder.unknownType();
            }

            // file_name() -> ?string
            if (std.mem.eql(u8, method.method_name, "file_name")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "file_name() takes no arguments", .{});
                }
                return tc.type_builder.optionalType(tc.type_builder.stringType()) catch tc.type_builder.unknownType();
            }

            // extension() -> ?string
            if (std.mem.eql(u8, method.method_name, "extension")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "extension() takes no arguments", .{});
                }
                return tc.type_builder.optionalType(tc.type_builder.stringType()) catch tc.type_builder.unknownType();
            }

            // to_string() -> string
            if (std.mem.eql(u8, method.method_name, "to_string")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "to_string() takes no arguments", .{});
                }
                return tc.type_builder.stringType();
            }

            // exists() -> bool
            if (std.mem.eql(u8, method.method_name, "exists")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "exists() takes no arguments", .{});
                }
                return tc.type_builder.boolType();
            }

            // is_file() -> bool
            if (std.mem.eql(u8, method.method_name, "is_file")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "is_file() takes no arguments", .{});
                }
                return tc.type_builder.boolType();
            }

            // is_dir() -> bool
            if (std.mem.eql(u8, method.method_name, "is_dir")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "is_dir() takes no arguments", .{});
                }
                return tc.type_builder.boolType();
            }
        }

        // Stdout methods
        if (object_type == .stdout_handle) {
            // write(&mut self, buf: &[u8]) -> Result[i32, IoError]
            if (std.mem.eql(u8, method.method_name, "write")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "write() expects exactly 1 argument (buffer)", .{});
                    return tc.type_builder.resultType(tc.type_builder.i32Type(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
                }
                const buf_type = tc.checkExpr(method.args[0]);
                // Check that argument is a reference to a slice or array of u8
                const is_valid_buf = blk: {
                    if (buf_type != .reference) break :blk false;
                    const inner = buf_type.reference.inner;
                    if (inner == .slice and inner.slice.element == .primitive and inner.slice.element.primitive == .u8_) break :blk true;
                    if (inner == .array and inner.array.element == .primitive and inner.array.element.primitive == .u8_) break :blk true;
                    break :blk false;
                };
                if (!is_valid_buf) {
                    tc.addError(.type_mismatch, method.span, "write() expects a reference to [u8] or [u8; N] buffer", .{});
                }
                return tc.type_builder.resultType(tc.type_builder.i32Type(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
            }

            // write_string(&mut self, s: string) -> Result[i32, IoError]
            if (std.mem.eql(u8, method.method_name, "write_string")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "write_string() expects exactly 1 argument (string)", .{});
                    return tc.type_builder.resultType(tc.type_builder.i32Type(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
                }
                const str_type = tc.checkExpr(method.args[0]);
                if (str_type != .primitive or str_type.primitive != .string_) {
                    tc.addError(.type_mismatch, method.span, "write_string() expects a string argument", .{});
                }
                return tc.type_builder.resultType(tc.type_builder.i32Type(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
            }

            // flush(&mut self) -> Result[void, IoError]
            if (std.mem.eql(u8, method.method_name, "flush")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "flush() takes no arguments", .{});
                }
                return tc.type_builder.resultType(tc.type_builder.voidType(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
            }
        }

        // Stderr methods
        if (object_type == .stderr_handle) {
            // write(&mut self, buf: &[u8]) -> Result[i32, IoError]
            if (std.mem.eql(u8, method.method_name, "write")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "write() expects exactly 1 argument (buffer)", .{});
                    return tc.type_builder.resultType(tc.type_builder.i32Type(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
                }
                const buf_type = tc.checkExpr(method.args[0]);
                // Check that argument is a reference to a slice or array of u8
                const is_valid_buf = blk: {
                    if (buf_type != .reference) break :blk false;
                    const inner = buf_type.reference.inner;
                    if (inner == .slice and inner.slice.element == .primitive and inner.slice.element.primitive == .u8_) break :blk true;
                    if (inner == .array and inner.array.element == .primitive and inner.array.element.primitive == .u8_) break :blk true;
                    break :blk false;
                };
                if (!is_valid_buf) {
                    tc.addError(.type_mismatch, method.span, "write() expects a reference to [u8] or [u8; N] buffer", .{});
                }
                return tc.type_builder.resultType(tc.type_builder.i32Type(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
            }

            // write_string(&mut self, s: string) -> Result[i32, IoError]
            if (std.mem.eql(u8, method.method_name, "write_string")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "write_string() expects exactly 1 argument (string)", .{});
                    return tc.type_builder.resultType(tc.type_builder.i32Type(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
                }
                const str_type = tc.checkExpr(method.args[0]);
                if (str_type != .primitive or str_type.primitive != .string_) {
                    tc.addError(.type_mismatch, method.span, "write_string() expects a string argument", .{});
                }
                return tc.type_builder.resultType(tc.type_builder.i32Type(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
            }

            // flush(&mut self) -> Result[void, IoError]
            if (std.mem.eql(u8, method.method_name, "flush")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "flush() takes no arguments", .{});
                }
                return tc.type_builder.resultType(tc.type_builder.voidType(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
            }
        }

        // Stdin methods
        if (object_type == .stdin_handle) {
            // read(&mut self, buf: &mut [u8]) -> Result[i32, IoError]
            if (std.mem.eql(u8, method.method_name, "read")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "read() expects exactly 1 argument (buffer)", .{});
                    return tc.type_builder.resultType(tc.type_builder.i32Type(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
                }
                const buf_type = tc.checkExpr(method.args[0]);
                // Check that argument is a mutable reference to a slice or array of u8
                const is_valid_buf = blk: {
                    if (buf_type != .reference or !buf_type.reference.mutable) break :blk false;
                    const inner = buf_type.reference.inner;
                    if (inner == .slice and inner.slice.element == .primitive and inner.slice.element.primitive == .u8_) break :blk true;
                    if (inner == .array and inner.array.element == .primitive and inner.array.element.primitive == .u8_) break :blk true;
                    break :blk false;
                };
                if (!is_valid_buf) {
                    tc.addError(.type_mismatch, method.span, "read() expects a mutable reference to [u8] or [u8; N] buffer", .{});
                }
                return tc.type_builder.resultType(tc.type_builder.i32Type(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
            }
        }

        // BufReader methods
        if (object_type == .buf_reader) {
            const inner_type = object_type.buf_reader.inner;

            // read(&mut self, buf: &mut [u8]) -> Result[i32, IoError]
            if (std.mem.eql(u8, method.method_name, "read")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "read() expects exactly 1 argument (buffer)", .{});
                    return tc.type_builder.resultType(tc.type_builder.i32Type(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
                }
                const buf_type = tc.checkExpr(method.args[0]);
                const is_valid_buf = blk: {
                    if (buf_type != .reference or !buf_type.reference.mutable) break :blk false;
                    const inner = buf_type.reference.inner;
                    if (inner == .slice and inner.slice.element == .primitive and inner.slice.element.primitive == .u8_) break :blk true;
                    if (inner == .array and inner.array.element == .primitive and inner.array.element.primitive == .u8_) break :blk true;
                    break :blk false;
                };
                if (!is_valid_buf) {
                    tc.addError(.type_mismatch, method.span, "read() expects a mutable reference to [u8] or [u8; N] buffer", .{});
                }
                return tc.type_builder.resultType(tc.type_builder.i32Type(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
            }

            // read_line(&mut self) -> Result[String, IoError]
            if (std.mem.eql(u8, method.method_name, "read_line")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "read_line() takes no arguments", .{});
                }
                const string_type = tc.type_builder.stringDataType() catch tc.type_builder.unknownType();
                return tc.type_builder.resultType(string_type, tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
            }

            // read_to_string(&mut self) -> Result[String, IoError]
            if (std.mem.eql(u8, method.method_name, "read_to_string")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "read_to_string() takes no arguments", .{});
                }
                const string_type = tc.type_builder.stringDataType() catch tc.type_builder.unknownType();
                return tc.type_builder.resultType(string_type, tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
            }

            // fill_buf(&mut self) -> Result[[u8], IoError]
            if (std.mem.eql(u8, method.method_name, "fill_buf")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "fill_buf() takes no arguments", .{});
                }
                const slice_u8 = tc.type_builder.sliceType(.{ .primitive = .u8_ }) catch tc.type_builder.unknownType();
                return tc.type_builder.resultType(slice_u8, tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
            }

            // consume(&mut self, n: i32)
            if (std.mem.eql(u8, method.method_name, "consume")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "consume() expects exactly 1 argument (n)", .{});
                    return tc.type_builder.voidType();
                }
                const n_type = tc.checkExpr(method.args[0]);
                if (n_type != .primitive or n_type.primitive != .i32_) {
                    tc.addError(.type_mismatch, method.span, "consume() expects an i32 argument", .{});
                }
                return tc.type_builder.voidType();
            }

            // into_inner(self) -> R
            if (std.mem.eql(u8, method.method_name, "into_inner")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "into_inner() takes no arguments", .{});
                }
                return inner_type;
            }
        }

        // BufWriter methods
        if (object_type == .buf_writer) {
            const inner_type = object_type.buf_writer.inner;

            // write(&mut self, buf: &[u8]) -> Result[i32, IoError]
            if (std.mem.eql(u8, method.method_name, "write")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "write() expects exactly 1 argument (buffer)", .{});
                    return tc.type_builder.resultType(tc.type_builder.i32Type(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
                }
                const buf_type = tc.checkExpr(method.args[0]);
                const is_valid_buf = blk: {
                    if (buf_type != .reference) break :blk false;
                    const inner = buf_type.reference.inner;
                    if (inner == .slice and inner.slice.element == .primitive and inner.slice.element.primitive == .u8_) break :blk true;
                    if (inner == .array and inner.array.element == .primitive and inner.array.element.primitive == .u8_) break :blk true;
                    break :blk false;
                };
                if (!is_valid_buf) {
                    tc.addError(.type_mismatch, method.span, "write() expects a reference to [u8] or [u8; N] buffer", .{});
                }
                return tc.type_builder.resultType(tc.type_builder.i32Type(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
            }

            // write_string(&mut self, s: string) -> Result[i32, IoError]
            if (std.mem.eql(u8, method.method_name, "write_string")) {
                if (method.args.len != 1) {
                    tc.addError(.invalid_call, method.span, "write_string() expects exactly 1 argument (string)", .{});
                    return tc.type_builder.resultType(tc.type_builder.i32Type(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
                }
                const str_type = tc.checkExpr(method.args[0]);
                if (str_type != .primitive or str_type.primitive != .string_) {
                    tc.addError(.type_mismatch, method.span, "write_string() expects a string argument", .{});
                }
                return tc.type_builder.resultType(tc.type_builder.i32Type(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
            }

            // flush(&mut self) -> Result[void, IoError]
            if (std.mem.eql(u8, method.method_name, "flush")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "flush() takes no arguments", .{});
                }
                return tc.type_builder.resultType(tc.type_builder.voidType(), tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
            }

            // into_inner(self) -> Result[W, IoError]
            // Flushes buffer and returns the inner writer. Returns error if flush fails.
            if (std.mem.eql(u8, method.method_name, "into_inner")) {
                if (method.args.len != 0) {
                    tc.addError(.invalid_call, method.span, "into_inner() takes no arguments", .{});
                }
                return tc.type_builder.resultType(inner_type, tc.type_builder.ioErrorType()) catch tc.type_builder.unknownType();
            }
        }


    return null;
}
