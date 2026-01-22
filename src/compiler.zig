const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("ast.zig");
const Span = ast.Span;
const bytecode = @import("bytecode.zig");
const OpCode = bytecode.OpCode;
const TypeTag = bytecode.TypeTag;
const chunk_mod = @import("chunk.zig");
const Chunk = chunk_mod.Chunk;
const Constant = chunk_mod.Constant;
const Function = chunk_mod.Function;

// ============================================================================
// Compiler Errors
// ============================================================================

pub const CompileError = struct {
    kind: Kind,
    span: Span,
    message: []const u8,

    pub const Kind = enum {
        // Scope errors
        undefined_variable,
        variable_already_defined,
        too_many_locals,
        too_many_upvalues,

        // Constant errors
        too_many_constants,

        // Control flow errors
        jump_too_large,
        loop_too_large,
        break_outside_loop,
        continue_outside_loop,

        // Function errors
        too_many_parameters,

        // Internal errors
        internal_error,
    };
};

/// Error type for compilation operations.
pub const Error = error{
    CompilationFailed,
    OutOfMemory,
};

// ============================================================================
// Local Variable
// ============================================================================

/// Represents a local variable in the current scope.
const Local = struct {
    /// Name of the variable (slice into source)
    name: []const u8,

    /// Depth of the scope where this variable was declared.
    /// -1 means the variable is uninitialized (being declared).
    depth: i32,

    /// Whether this variable is mutable (var vs let).
    is_mutable: bool,

    /// Whether this local is captured by a closure.
    is_captured: bool,
};

// ============================================================================
// Upvalue
// ============================================================================

/// Describes an upvalue captured by a closure.
const Upvalue = struct {
    /// Index of the local in the enclosing function, or
    /// index into the enclosing function's upvalues.
    index: u8,

    /// True if capturing a local from the immediate enclosing function.
    /// False if capturing an upvalue from the enclosing function.
    is_local: bool,
};

// ============================================================================
// Compiler Scope
// ============================================================================

/// Maximum number of local variables per function.
const max_locals: usize = 256;

/// Maximum number of upvalues per function.
const max_upvalues: usize = 256;

/// Represents a function being compiled.
const CompilerScope = struct {
    /// The function being compiled.
    function: *Function,

    /// Type of function (script, function, method, closure).
    function_type: FunctionType,

    /// Local variables in this scope.
    locals: [max_locals]Local,

    /// Number of locals currently in scope.
    local_count: usize,

    /// Upvalues captured by this function.
    upvalues: [max_upvalues]Upvalue,

    /// Number of upvalues captured.
    upvalue_count: usize,

    /// Current scope depth (0 = global, 1+ = local).
    scope_depth: i32,

    /// Enclosing compiler scope (for closures).
    enclosing: ?*CompilerScope,

    /// Number of nested loops (for break/continue).
    loop_depth: usize,

    /// Stack of loop start offsets for continue.
    loop_starts: [256]usize,

    /// Stack of scope depths at loop entry (for break cleanup).
    loop_scope_depths: [256]i32,

    /// Stack of break jump locations to patch.
    break_jumps: std.ArrayListUnmanaged(usize),

    /// Whether the current function has an optional return type.
    /// When true, explicit return statements wrap the value in Some.
    returns_optional: bool,

    fn init(allocator: Allocator, function: *Function, function_type: FunctionType, enclosing: ?*CompilerScope) CompilerScope {
        var scope = CompilerScope{
            .function = function,
            .function_type = function_type,
            .locals = undefined,
            .local_count = 0,
            .upvalues = undefined,
            .upvalue_count = 0,
            .scope_depth = 0,
            .enclosing = enclosing,
            .returns_optional = false,
            .loop_depth = 0,
            .loop_starts = undefined,
            .loop_scope_depths = undefined,
            .break_jumps = .{},
        };

        // Reserve slot 0 for the function itself (or 'this' in methods).
        scope.locals[0] = .{
            .name = if (function_type == .method) "self" else "",
            .depth = 0,
            .is_mutable = false,
            .is_captured = false,
        };
        scope.local_count = 1;

        _ = allocator;
        return scope;
    }

    fn deinit(self: *CompilerScope, allocator: Allocator) void {
        self.break_jumps.deinit(allocator);
    }
};

const FunctionType = enum {
    script, // Top-level script
    function, // Named function
    method, // Method on a type
    closure, // Anonymous closure
};

// ============================================================================
// Helper Functions
// ============================================================================

/// Extracts the base type name from a TypeExpr.
fn extractTypeName(te: ast.TypeExpr) []const u8 {
    return switch (te) {
        .named => |n| n.name,
        .generic_apply => |g| extractTypeName(g.base),
        else => "anonymous",
    };
}

// ============================================================================
// Compiler
// ============================================================================

/// Compiles AST to bytecode.
pub const Compiler = struct {
    allocator: Allocator,

    /// Current compiler scope (function being compiled).
    current: *CompilerScope,

    /// List of compilation errors.
    errors: std.ArrayListUnmanaged(CompileError),

    /// Whether we've encountered an error.
    had_error: bool,

    /// Arena for allocating compiler-internal structures.
    arena: std.heap.ArenaAllocator,

    // -------------------------------------------------------------------------
    // Initialization
    // -------------------------------------------------------------------------

    pub fn init(allocator: Allocator) Compiler {
        return .{
            .allocator = allocator,
            .current = undefined,
            .errors = .{},
            .had_error = false,
            .arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.errors.deinit(self.allocator);
        self.arena.deinit();
    }

    // -------------------------------------------------------------------------
    // Main compilation entry point
    // -------------------------------------------------------------------------

    /// Compile a module (top-level declarations and statements).
    pub fn compile(self: *Compiler, module: ast.Module) !*Function {
        // Create the top-level script function.
        const script_func = try self.allocator.create(Function);
        script_func.* = Function.init(self.allocator, "<script>", 0);

        // Initialize the compiler scope.
        var scope = CompilerScope.init(self.allocator, script_func, .script, null);
        defer scope.deinit(self.allocator);
        self.current = &scope;

        // Compile all declarations.
        for (module.declarations) |decl| {
            try self.compileDecl(decl);
        }

        // Emit implicit return.
        try self.emitReturn();

        if (self.had_error) {
            return error.CompilationFailed;
        }

        return script_func;
    }

    // -------------------------------------------------------------------------
    // Declaration compilation
    // -------------------------------------------------------------------------

    fn compileDecl(self: *Compiler, decl: ast.Decl) Error!void {
        switch (decl) {
            .function => |func| try self.compileFunction(func),
            .const_decl => |c| try self.compileConstDecl(c),
            .struct_decl => |s| try self.compileStructDecl(s),
            .enum_decl => |e| try self.compileEnumDecl(e),
            // TODO: Implement remaining declarations
            .trait_decl, .impl_decl, .type_alias, .import_decl, .module_decl => {},
        }
    }

    fn compileFunction(self: *Compiler, func: *ast.FunctionDecl) Error!void {
        // Declare the function name in the current scope.
        const line = func.span.line;

        // Create the function object.
        const new_func = try self.allocator.create(Function);
        new_func.* = Function.init(self.allocator, func.name, @intCast(func.params.len));
        errdefer {
            new_func.deinit();
            self.allocator.destroy(new_func);
        }

        // Create a new compiler scope for this function.
        var scope = CompilerScope.init(self.allocator, new_func, .function, self.current);
        defer scope.deinit(self.allocator);
        const previous = self.current;
        self.current = &scope;

        // Track if this function returns an optional type
        if (func.return_type) |ret_type| {
            if (ret_type == .optional) {
                self.current.returns_optional = true;
            }
        }

        // Begin a new scope for the function body.
        self.beginScope();

        // Compile parameters as local variables.
        for (func.params) |param| {
            try self.declareVariable(param.name, false, param.span);
            try self.defineVariable();
        }

        // Compile the function body.
        if (func.body) |body| {
            try self.compileBlock(body);
        }

        // Emit implicit return if needed.
        // If the function has a return type, emit op_return (which pops and returns a value).
        // Otherwise, emit op_return_void.
        // For optional return types, emit op_none first (implicit None).
        if (func.return_type) |ret_type| {
            if (ret_type == .optional) {
                // Implicit None for optional return types
                try self.emitOp(.op_none, line);
            }
            try self.emitOp(.op_return, line);
        } else {
            try self.emitOp(.op_return_void, line);
        }

        // End the function scope.
        self.endScope();

        // Restore the previous compiler scope.
        self.current = previous;

        // Emit closure instruction - this pushes the closure onto the stack.
        const func_const = try self.makeConstant(.{ .function = new_func });
        try self.emitOp2(.op_closure, @intCast(func_const), line);

        // Emit upvalue descriptors.
        for (0..scope.upvalue_count) |i| {
            try self.emitByte(if (scope.upvalues[i].is_local) 1 else 0, line);
            try self.emitByte(scope.upvalues[i].index, line);
        }

        // Store the function in a global or local.
        // op_define_global pops the closure from the stack.
        if (previous.scope_depth == 0) {
            const global_idx = try self.makeConstant(.{ .string = func.name });
            try self.emitOp2(.op_define_global, @intCast(global_idx), line);
        } else {
            try self.declareVariable(func.name, false, func.span);
            try self.defineVariable();
        }
    }

    fn compileConstDecl(self: *Compiler, c: *ast.ConstDecl) Error!void {
        const line = c.span.line;

        // Compile the value.
        try self.compileExpr(c.value);

        if (self.current.scope_depth == 0) {
            // Global constant
            const name_idx = try self.makeConstant(.{ .string = c.name });
            try self.emitOp2(.op_define_global, @intCast(name_idx), line);
        } else {
            // Local constant
            try self.declareVariable(c.name, false, c.span);
            try self.defineVariable();
        }
    }

    fn compileStructDecl(_: *Compiler, _: *ast.StructDecl) Error!void {
        // TODO: Implement struct compilation
    }

    fn compileEnumDecl(_: *Compiler, _: *ast.EnumDecl) Error!void {
        // TODO: Implement enum compilation
    }

    // -------------------------------------------------------------------------
    // Statement compilation
    // -------------------------------------------------------------------------

    fn compileStmt(self: *Compiler, stmt: ast.Stmt) Error!void {
        switch (stmt) {
            .let_decl => |l| try self.compileLetDecl(l),
            .var_decl => |v| try self.compileVarDecl(v),
            .expr_stmt => |e| try self.compileExprStmt(e),
            .return_stmt => |r| try self.compileReturnStmt(r),
            .break_stmt => |b| try self.compileBreakStmt(b),
            .continue_stmt => |c| try self.compileContinueStmt(c),
            .while_loop => |w| try self.compileWhileLoop(w),
            .for_loop => |f| try self.compileForLoop(f),
            .loop_stmt => |l| try self.compileLoopStmt(l),
            .assignment => |a| try self.compileAssignment(a),
            .if_stmt => |i| try self.compileIfStmt(i),
            .match_stmt => |m| try self.compileMatchStmt(m),
        }
    }

    fn compileLetDecl(self: *Compiler, l: *ast.LetDecl) Error!void {
        const line = l.span.line;

        // Compile the initializer.
        try self.compileExpr(l.value);

        if (self.current.scope_depth == 0) {
            // Global variable
            const name_idx = try self.makeConstant(.{ .string = l.name });
            try self.emitOp2(.op_define_global, @intCast(name_idx), line);
        } else {
            // Local variable
            try self.declareVariable(l.name, false, l.span);
            try self.defineVariable();
        }
    }

    fn compileVarDecl(self: *Compiler, v: *ast.VarDecl) Error!void {
        const line = v.span.line;

        // Compile the initializer.
        try self.compileExpr(v.value);

        if (self.current.scope_depth == 0) {
            // Global variable
            const name_idx = try self.makeConstant(.{ .string = v.name });
            try self.emitOp2(.op_define_global, @intCast(name_idx), line);
        } else {
            // Local variable (mutable)
            try self.declareVariable(v.name, true, v.span);
            try self.defineVariable();
        }
    }

    fn compileExprStmt(self: *Compiler, e: *ast.ExprStmt) Error!void {
        try self.compileExpr(e.expr);
        try self.emitOp(.op_pop, e.span.line);
    }

    fn compileReturnStmt(self: *Compiler, r: *ast.ReturnStmt) Error!void {
        if (r.value) |value| {
            try self.compileExpr(value);
            // If the function returns ?T, wrap the value in Some
            if (self.current.returns_optional) {
                try self.emitOp(.op_some, r.span.line);
            }
            try self.emitOp(.op_return, r.span.line);
        } else {
            // Empty return in optional function => return None
            if (self.current.returns_optional) {
                try self.emitOp(.op_none, r.span.line);
                try self.emitOp(.op_return, r.span.line);
            } else {
                try self.emitOp(.op_return_void, r.span.line);
            }
        }
    }

    fn compileBreakStmt(self: *Compiler, b: *ast.BreakStmt) Error!void {
        if (self.current.loop_depth == 0) {
            try self.addError(.break_outside_loop, b.span, "break outside of loop");
            return;
        }

        if (b.value) |value| {
            try self.compileExpr(value);
        }

        // Pop locals that will go out of scope when we break.
        // We need to restore to the scope depth at loop entry.
        const target_scope_depth = self.current.loop_scope_depths[self.current.loop_depth - 1];
        var i = self.current.local_count;
        while (i > 0) {
            i -= 1;
            const local = &self.current.locals[i];
            if (local.depth != -1 and local.depth > target_scope_depth) {
                if (local.is_captured) {
                    try self.emitOp(.op_close_upvalue, b.span.line);
                } else {
                    try self.emitOp(.op_pop, b.span.line);
                }
            } else {
                break;
            }
        }

        // Emit a jump to be patched when the loop ends.
        const jump = try self.emitJump(.op_jump, b.span.line);
        try self.current.break_jumps.append(self.allocator, jump);
    }

    fn compileContinueStmt(self: *Compiler, c: *ast.ContinueStmt) Error!void {
        if (self.current.loop_depth == 0) {
            try self.addError(.continue_outside_loop, c.span, "continue outside of loop");
            return;
        }

        // Emit a loop instruction back to the start of the loop.
        const loop_start = self.current.loop_starts[self.current.loop_depth - 1];
        try self.emitLoop(loop_start, c.span.line);
    }

    fn compileWhileLoop(self: *Compiler, w: *ast.WhileLoop) Error!void {
        const line = w.span.line;

        // Mark the loop start and scope depth for break cleanup.
        const loop_start = self.currentChunk().currentOffset();
        self.current.loop_starts[self.current.loop_depth] = loop_start;
        self.current.loop_scope_depths[self.current.loop_depth] = self.current.scope_depth;
        self.current.loop_depth += 1;

        // Compile condition.
        try self.compileExpr(w.condition);

        // Jump out if false.
        const exit_jump = try self.emitJump(.op_jump_if_false, line);

        // Begin a new scope for the loop body.
        self.beginScope();

        // Compile body (no value produced by loop body).
        try self.compileBlockStatements(w.body);

        // End the loop scope (pops locals declared in loop body).
        self.endScope();

        // Loop back to condition.
        try self.emitLoop(loop_start, line);

        // Patch exit jump.
        try self.patchJump(exit_jump);

        // Patch break jumps.
        for (self.current.break_jumps.items) |jump| {
            try self.patchJump(jump);
        }
        self.current.break_jumps.clearRetainingCapacity();

        self.current.loop_depth -= 1;
    }

    fn compileForLoop(self: *Compiler, f: *ast.ForLoop) Error!void {
        const line = f.span.line;

        // Begin a new scope for the loop variable.
        self.beginScope();

        // Compile the iterable and get an iterator.
        try self.compileExpr(f.iterable);
        try self.emitOp(.op_get_iter, line);

        // Declare the iterator as a hidden local.
        try self.declareVariable("$iter", false, f.span);
        try self.defineVariable();

        // Mark the loop start and scope depth for break cleanup.
        const loop_start = self.currentChunk().currentOffset();
        self.current.loop_starts[self.current.loop_depth] = loop_start;
        self.current.loop_scope_depths[self.current.loop_depth] = self.current.scope_depth;
        self.current.loop_depth += 1;

        // Get next value from iterator.
        try self.emitOp(.op_iter_next, line);

        // Jump out if no more values.
        const exit_jump = try self.emitJump(.op_jump_if_false, line);

        // Bind the loop variable.
        try self.compilePattern(f.pattern, line);

        // Compile body (no value produced by loop body).
        try self.compileBlockStatements(f.body);

        // Loop back.
        try self.emitLoop(loop_start, line);

        // Patch exit jump.
        try self.patchJump(exit_jump);

        // Patch break jumps.
        for (self.current.break_jumps.items) |jump| {
            try self.patchJump(jump);
        }
        self.current.break_jumps.clearRetainingCapacity();

        self.current.loop_depth -= 1;

        // End the loop scope.
        self.endScope();
    }

    fn compileLoopStmt(self: *Compiler, l: *ast.LoopStmt) Error!void {
        const line = l.span.line;

        // Begin a new scope for the loop body.
        self.beginScope();

        // Mark the loop start and scope depth for break cleanup.
        const loop_start = self.currentChunk().currentOffset();
        self.current.loop_starts[self.current.loop_depth] = loop_start;
        self.current.loop_scope_depths[self.current.loop_depth] = self.current.scope_depth;
        self.current.loop_depth += 1;

        // Compile body (no value produced by loop body).
        try self.compileBlockStatements(l.body);

        // End the loop scope (pops locals declared in loop body).
        self.endScope();

        // Loop back unconditionally.
        try self.emitLoop(loop_start, line);

        // Patch break jumps.
        for (self.current.break_jumps.items) |jump| {
            try self.patchJump(jump);
        }
        self.current.break_jumps.clearRetainingCapacity();

        self.current.loop_depth -= 1;
    }

    fn compileAssignment(self: *Compiler, a: *ast.Assignment) Error!void {
        const line = a.span.line;

        // Handle compound assignments.
        if (a.op != .assign) {
            // Load the target first.
            try self.compileExpr(a.target);

            // Compile the value.
            try self.compileExpr(a.value);

            // Emit the operation.
            const op: OpCode = switch (a.op) {
                .add_assign => .op_add,
                .sub_assign => .op_sub,
                .mul_assign => .op_mul,
                .div_assign => .op_div,
                .mod_assign => .op_mod,
                else => unreachable,
            };
            try self.emitOp(op, line);
        } else {
            // Simple assignment: compile the value.
            try self.compileExpr(a.value);
        }

        // Store to the target.
        try self.compileAssignmentTarget(a.target, line);

        // Assignment statements don't produce a value, so pop the leftover.
        // (op_set_local peeks rather than pops, leaving the value on stack)
        try self.emitOp(.op_pop, line);
    }

    fn compileAssignmentTarget(self: *Compiler, target: ast.Expr, line: u32) Error!void {
        switch (target) {
            .identifier => |id| {
                // Check if it's a local.
                if (self.resolveLocal(self.current, id.name)) |slot| {
                    if (slot <= 255) {
                        try self.emitOp1(.op_set_local, @intCast(slot), line);
                    } else {
                        try self.emitOp2(.op_set_local_long, @intCast(slot), line);
                    }
                } else if (self.resolveUpvalue(self.current, id.name)) |slot| {
                    try self.emitOp1(.op_set_upvalue, @intCast(slot), line);
                } else {
                    // Global
                    const name_idx = try self.makeConstant(.{ .string = id.name });
                    try self.emitOp2(.op_set_global, @intCast(name_idx), line);
                }
            },
            .index => |idx| {
                // Value is already on stack from compileAssignmentStatement.
                // Stack: [value]
                // Compile object and index, then swap to get correct order for op_set_index.
                try self.compileExpr(idx.object); // Stack: [value, object]
                try self.compileExpr(idx.index); // Stack: [value, object, index]
                try self.emitOp(.op_swap, line); // Stack: [value, index, object]
                // op_set_index expects: [value, index, container] with container on top
                try self.emitOp(.op_set_index, line);
            },
            .field => |fld| {
                // Compile object, value is already on stack.
                try self.compileExpr(fld.object);
                try self.emitOp(.op_swap, line);
                const name_idx = try self.makeConstant(.{ .string = fld.field_name });
                try self.emitOp2(.op_set_field, @intCast(name_idx), line);
            },
            else => {
                try self.addError(.internal_error, target.span(), "invalid assignment target");
            },
        }
    }

    // -------------------------------------------------------------------------
    // Expression compilation
    // -------------------------------------------------------------------------

    fn compileExpr(self: *Compiler, expr: ast.Expr) Error!void {
        switch (expr) {
            .literal => |l| try self.compileLiteral(l),
            .identifier => |id| try self.compileIdentifier(id),
            .binary => |b| try self.compileBinary(b),
            .unary => |u| try self.compileUnary(u),
            .postfix => |p| try self.compilePostfix(p),
            .call => |c| try self.compileCall(c),
            .index => |i| try self.compileIndex(i),
            .field => |f| try self.compileField(f),
            .method_call => |m| try self.compileMethodCall(m),
            .block => |b| try self.compileBlockExpr(b),
            .closure => |c| try self.compileClosure(c),
            .range => |r| try self.compileRange(r),
            .struct_literal => |s| try self.compileStructLiteral(s),
            .array_literal => |a| try self.compileArrayLiteral(a),
            .tuple_literal => |t| try self.compileTupleLiteral(t),
            .type_cast => |tc| try self.compileTypeCast(tc),
            .grouped => |g| try self.compileExpr(g.expr),
            .interpolated_string => |i| try self.compileInterpolatedString(i),
            .enum_literal => {
                // TODO: Enum literal compilation for bytecode VM
                // For now, enum literals are only supported in native compilation
            },
            .comptime_block => {
                // Comptime blocks are evaluated at compile time by the type checker
                // The bytecode VM should never see them - they are replaced by their evaluated values
                try self.addError(.internal_error, expr.span(), "comptime blocks not yet supported in bytecode VM");
            },
            .builtin_call => |bc| {
                // Handle @repeat builtin specially - compile it as an array literal
                if (std.mem.eql(u8, bc.name, "repeat") and bc.args.len == 2) {
                    // Get count from the second argument (must be an integer literal)
                    const count_expr = switch (bc.args[1]) {
                        .expr_arg => |e| e,
                        .type_arg => {
                            try self.addError(.internal_error, expr.span(), "@repeat count must be an integer");
                            return;
                        },
                    };

                    // Extract the count value from the literal
                    const count: usize = switch (count_expr) {
                        .literal => |lit| switch (lit.kind) {
                            .int => |v| blk: {
                                if (v < 0) {
                                    try self.addError(.internal_error, expr.span(), "@repeat count must be non-negative");
                                    return;
                                }
                                break :blk @intCast(v);
                            },
                            else => {
                                try self.addError(.internal_error, expr.span(), "@repeat count must be an integer literal");
                                return;
                            },
                        },
                        else => {
                            try self.addError(.internal_error, expr.span(), "@repeat count must be a literal");
                            return;
                        },
                    };

                    if (count == 0) {
                        try self.addError(.internal_error, expr.span(), "@repeat count must be > 0 for bytecode VM");
                        return;
                    }

                    // Compile the value expression count times
                    const value_expr = switch (bc.args[0]) {
                        .expr_arg => |e| e,
                        .type_arg => {
                            try self.addError(.internal_error, expr.span(), "@repeat first argument must be a value");
                            return;
                        },
                    };

                    const line = bc.span.line;
                    for (0..count) |_| {
                        try self.compileExpr(value_expr);
                    }

                    // Emit array instruction
                    if (count > 255) {
                        try self.addError(.too_many_constants, bc.span, "too many array elements");
                        return;
                    }
                    try self.emitOp2(.op_array, @intCast(count), line);
                } else {
                    // Other builtin calls are evaluated at compile time
                    try self.addError(.internal_error, expr.span(), "builtin calls not yet supported in bytecode VM");
                }
            },
        }
    }

    fn compileLiteral(self: *Compiler, lit: ast.Literal) Error!void {
        const line = lit.span.line;

        switch (lit.kind) {
            .int => |value| {
                // Use immediate opcodes for small integers.
                if (value >= -1 and value <= 5) {
                    const op: OpCode = switch (value) {
                        -1 => .op_int_neg1,
                        0 => .op_int_0,
                        1 => .op_int_1,
                        2 => .op_int_2,
                        3 => .op_int_3,
                        4 => .op_int_4,
                        5 => .op_int_5,
                        else => unreachable,
                    };
                    try self.emitOp(op, line);
                } else {
                    try self.emitConstant(.{ .int = value }, line);
                }
            },
            .float => |value| {
                try self.emitConstant(.{ .float = value }, line);
            },
            .string => |value| {
                try self.emitConstant(.{ .string = value }, line);
            },
            .char => |value| {
                try self.emitConstant(.{ .char = value }, line);
            },
            .bool_ => |value| {
                try self.emitOp(if (value) .op_true else .op_false, line);
            },
        }
    }

    fn compileIdentifier(self: *Compiler, id: ast.Identifier) Error!void {
        const line = id.span.line;

        // Check if it's a local variable.
        if (self.resolveLocal(self.current, id.name)) |slot| {
            if (slot <= 255) {
                try self.emitOp1(.op_get_local, @intCast(slot), line);
            } else {
                try self.emitOp2(.op_get_local_long, @intCast(slot), line);
            }
        } else if (self.resolveUpvalue(self.current, id.name)) |slot| {
            try self.emitOp1(.op_get_upvalue, @intCast(slot), line);
        } else {
            // Global variable
            const name_idx = try self.makeConstant(.{ .string = id.name });
            try self.emitOp2(.op_get_global, @intCast(name_idx), line);
        }
    }

    fn compileBinary(self: *Compiler, bin: *ast.Binary) Error!void {
        const line = bin.span.line;

        // Handle assignment operators - convert to assignment statement logic.
        switch (bin.op) {
            .assign => {
                // Simple assignment: evaluate RHS, assign to LHS.
                try self.compileExpr(bin.right);
                try self.compileAssignmentTarget(bin.left, line);
                return;
            },
            .add_assign, .sub_assign, .mul_assign, .div_assign, .mod_assign => {
                // Compound assignment: read LHS, apply op with RHS, assign back.
                try self.compileExpr(bin.left);
                try self.compileExpr(bin.right);
                const op: OpCode = switch (bin.op) {
                    .add_assign => .op_add,
                    .sub_assign => .op_sub,
                    .mul_assign => .op_mul,
                    .div_assign => .op_div,
                    .mod_assign => .op_mod,
                    else => unreachable,
                };
                try self.emitOp(op, line);
                try self.compileAssignmentTarget(bin.left, line);
                return;
            },
            else => {},
        }

        // Handle short-circuit operators specially.
        switch (bin.op) {
            .and_ => {
                try self.compileExpr(bin.left);
                const end_jump = try self.emitJump(.op_jump_if_false_no_pop, line);
                try self.emitOp(.op_pop, line);
                try self.compileExpr(bin.right);
                try self.patchJump(end_jump);
                return;
            },
            .or_ => {
                try self.compileExpr(bin.left);
                const end_jump = try self.emitJump(.op_jump_if_true, line);
                try self.emitOp(.op_pop, line);
                try self.compileExpr(bin.right);
                try self.patchJump(end_jump);
                return;
            },
            .null_coalesce => {
                try self.compileExpr(bin.left);
                try self.emitOp(.op_dup, line);
                try self.emitOp(.op_is_none, line);
                const end_jump = try self.emitJump(.op_jump_if_false, line);
                try self.emitOp(.op_pop, line);
                try self.compileExpr(bin.right);
                try self.patchJump(end_jump);
                return;
            },
            else => {},
        }

        // Compile operands left-to-right.
        try self.compileExpr(bin.left);
        try self.compileExpr(bin.right);

        // Emit the operation.
        const op: OpCode = switch (bin.op) {
            .add => .op_add,
            .sub => .op_sub,
            .mul => .op_mul,
            .div => .op_div,
            .mod => .op_mod,
            .add_wrap => .op_add_wrap,
            .sub_wrap => .op_sub_wrap,
            .mul_wrap => .op_mul_wrap,
            .add_sat => .op_add_sat,
            .sub_sat => .op_sub_sat,
            .mul_sat => .op_mul_sat,
            .eq => .op_eq,
            .not_eq => .op_ne,
            .lt => .op_lt,
            .gt => .op_gt,
            .lt_eq => .op_le,
            .gt_eq => .op_ge,
            .bit_and => .op_bit_and,
            .bit_or => .op_bit_or,
            .bit_xor => .op_bit_xor,
            .shl => .op_shl,
            .shr => .op_shr,
            // Handled above (short-circuit operators and assignment)
            .and_, .or_, .null_coalesce => unreachable,
            .assign, .add_assign, .sub_assign, .mul_assign, .div_assign, .mod_assign => unreachable,
            // 'is' type check
            .is => .op_is_type, // TODO: needs type operand
        };

        try self.emitOp(op, line);
    }

    fn compileUnary(self: *Compiler, unary: *ast.Unary) Error!void {
        const line = unary.span.line;

        // Compile the operand.
        try self.compileExpr(unary.operand);

        // Emit the operation.
        const op: OpCode = switch (unary.op) {
            .negate => .op_neg,
            .not => .op_not,
            .ref => .op_nop, // TODO: implement references
            .ref_mut => .op_nop,
            .deref => .op_nop,
        };

        try self.emitOp(op, line);
    }

    fn compilePostfix(self: *Compiler, postfix: *ast.Postfix) Error!void {
        const line = postfix.span.line;

        // Compile the operand.
        try self.compileExpr(postfix.operand);

        // Emit the operation.
        const op: OpCode = switch (postfix.op) {
            .unwrap => .op_unwrap,
            .force_unwrap => .op_unwrap,
        };

        try self.emitOp(op, line);
    }

    fn compileCall(self: *Compiler, call: *ast.Call) Error!void {
        const line = call.span.line;

        // Compile the callee.
        try self.compileExpr(call.callee);

        // Compile arguments.
        for (call.args) |arg| {
            try self.compileExpr(arg);
        }

        // Emit call instruction.
        if (call.args.len > 255) {
            try self.addError(.too_many_parameters, call.span, "too many arguments");
            return;
        }
        try self.emitOp1(.op_call, @intCast(call.args.len), line);
    }

    fn compileIndex(self: *Compiler, idx: *ast.Index) Error!void {
        const line = idx.span.line;

        // Compile object and index.
        try self.compileExpr(idx.object);
        try self.compileExpr(idx.index);

        try self.emitOp(.op_get_index, line);
    }

    fn compileField(self: *Compiler, field: *ast.Field) Error!void {
        const line = field.span.line;

        // Compile the object.
        try self.compileExpr(field.object);

        // Emit field access.
        const name_idx = try self.makeConstant(.{ .string = field.field_name });
        try self.emitOp2(.op_get_field, @intCast(name_idx), line);
    }

    fn compileMethodCall(self: *Compiler, method: *ast.MethodCall) Error!void {
        const line = method.span.line;

        // Compile the object (receiver).
        try self.compileExpr(method.object);

        // Compile arguments.
        for (method.args) |arg| {
            try self.compileExpr(arg);
        }

        // Emit invoke instruction.
        const name_idx = try self.makeConstant(.{ .string = method.method_name });
        if (method.args.len > 255) {
            try self.addError(.too_many_parameters, method.span, "too many arguments");
            return;
        }

        try self.emitOp(.op_invoke, line);
        try self.emitByte(@truncate(name_idx >> 8), line);
        try self.emitByte(@truncate(name_idx), line);
        try self.emitByte(@intCast(method.args.len), line);
    }

    fn compileIfStmt(self: *Compiler, if_stmt: *ast.IfStmt) Error!void {
        const line = if_stmt.span.line;

        // Compile condition.
        try self.compileExpr(if_stmt.condition);

        // Jump to else branch if false.
        const then_jump = try self.emitJump(.op_jump_if_false, line);

        // Compile then branch (as a block, doesn't leave value on stack).
        self.beginScope();
        try self.compileBlock(if_stmt.then_branch);
        self.endScope();

        // Jump over else branch if present.
        var else_jump: ?usize = null;
        if (if_stmt.else_branch != null) {
            else_jump = try self.emitJump(.op_jump, line);
        }

        // Patch the then jump.
        try self.patchJump(then_jump);

        // Compile else branch if present.
        if (if_stmt.else_branch) |else_branch| {
            switch (else_branch.*) {
                .block => |block| {
                    self.beginScope();
                    try self.compileBlock(block);
                    self.endScope();
                },
                .if_stmt => |nested_if| try self.compileIfStmt(nested_if),
            }
            // Patch the else jump.
            if (else_jump) |jump| {
                try self.patchJump(jump);
            }
        }
    }

    fn compileMatchStmt(self: *Compiler, match_stmt: *ast.MatchStmt) Error!void {
        const line = match_stmt.span.line;

        // Compile the subject.
        try self.compileExpr(match_stmt.subject);

        // We'll emit a series of:
        // 1. Duplicate subject
        // 2. Test pattern
        // 3. If match, pop subject and compile body, then jump to end
        // 4. If no match, try next arm

        var end_jumps = std.ArrayListUnmanaged(usize){};
        defer end_jumps.deinit(self.allocator);

        for (match_stmt.arms) |arm| {
            // Duplicate the subject for pattern matching.
            try self.emitOp(.op_dup, line);

            // Compile pattern test (leaves bool on stack).
            try self.compilePatternTest(arm.pattern, line);

            // Jump to next arm if pattern doesn't match.
            const next_arm = try self.emitJump(.op_jump_if_false, line);

            // Pop the duplicated subject (pattern matched).
            try self.emitOp(.op_pop, line);

            // Compile guard if present.
            if (arm.guard) |guard| {
                try self.compileExpr(guard);
                const guard_fail = try self.emitJump(.op_jump_if_false, line);
                // Guard passed, compile body (as a block).
                self.beginScope();
                try self.compileBlock(arm.body);
                self.endScope();
                const end_jump = try self.emitJump(.op_jump, line);
                try end_jumps.append(self.allocator, end_jump);
                try self.patchJump(guard_fail);
            } else {
                // Compile body (as a block).
                self.beginScope();
                try self.compileBlock(arm.body);
                self.endScope();
                const end_jump = try self.emitJump(.op_jump, line);
                try end_jumps.append(self.allocator, end_jump);
            }

            // Patch next arm jump.
            try self.patchJump(next_arm);
        }

        // Pop the subject if no arm matched.
        try self.emitOp(.op_pop, line);

        // Emit a panic for non-exhaustive match.
        const msg_idx = try self.makeConstant(.{ .string = "non-exhaustive match" });
        try self.emitOp2(.op_const, @intCast(msg_idx), line);
        try self.emitOp(.op_panic, line);

        // Patch all end jumps.
        for (end_jumps.items) |jump| {
            try self.patchJump(jump);
        }
    }

    fn compileBlockExpr(self: *Compiler, block: *ast.Block) Error!void {
        self.beginScope();
        try self.compileBlock(block);
        self.endScope();
    }

    fn compileBlock(self: *Compiler, block: *ast.Block) Error!void {
        // Compile all statements.
        for (block.statements) |stmt| {
            try self.compileStmt(stmt);
        }

        // Compile the final expression if present.
        if (block.final_expr) |final| {
            try self.compileExpr(final);
        } else {
            try self.emitOp(.op_void, block.span.line);
        }
    }

    /// Compile block statements without producing a value (for loop bodies).
    /// Creates a new scope so local variables are properly cleaned up between iterations.
    fn compileBlockStatements(self: *Compiler, block: *ast.Block) Error!void {
        self.beginScope();

        // Compile all statements.
        for (block.statements) |stmt| {
            try self.compileStmt(stmt);
        }

        // If there's a final expression, compile it and discard the result.
        if (block.final_expr) |final| {
            try self.compileExpr(final);
            try self.emitOp(.op_pop, block.span.line);
        }

        self.endScope();
    }

    fn compileClosure(self: *Compiler, closure: *ast.Closure) Error!void {
        const line = closure.span.line;

        // Create the function object.
        const new_func = try self.allocator.create(Function);
        new_func.* = Function.init(self.allocator, "<closure>", @intCast(closure.params.len));
        errdefer {
            new_func.deinit();
            self.allocator.destroy(new_func);
        }

        // Create a new compiler scope.
        var scope = CompilerScope.init(self.allocator, new_func, .closure, self.current);
        defer scope.deinit(self.allocator);
        const previous = self.current;
        self.current = &scope;

        // Begin scope.
        self.beginScope();

        // Compile parameters.
        for (closure.params) |param| {
            try self.declareVariable(param.name, false, param.span);
            try self.defineVariable();
        }

        // Compile body.
        try self.compileExpr(closure.body);
        try self.emitOp(.op_return, line);

        // End scope and restore.
        self.endScope();
        self.current = previous;

        // Record upvalue count.
        new_func.upvalue_count = @intCast(scope.upvalue_count);

        // Emit closure instruction.
        const func_const = try self.makeConstant(.{ .function = new_func });
        try self.emitOp2(.op_closure, @intCast(func_const), line);

        // Emit upvalue descriptors.
        for (0..scope.upvalue_count) |i| {
            try self.emitByte(if (scope.upvalues[i].is_local) 1 else 0, line);
            try self.emitByte(scope.upvalues[i].index, line);
        }
    }

    fn compileRange(self: *Compiler, range: *ast.Range) Error!void {
        const line = range.span.line;

        // Compile start and end.
        if (range.start) |start| {
            try self.compileExpr(start);
        } else {
            try self.emitOp(.op_int_0, line);
        }

        if (range.end) |end| {
            try self.compileExpr(end);
        } else {
            // TODO: Handle unbounded ranges
            try self.emitOp(.op_int_0, line);
        }

        // Emit range instruction.
        const op: OpCode = if (range.inclusive) .op_range_inclusive else .op_range_exclusive;
        try self.emitOp(op, line);
    }

    fn compileStructLiteral(self: *Compiler, struc: *ast.StructLiteral) Error!void {
        const line = struc.span.line;

        // Compile field values in order.
        for (struc.fields) |field| {
            try self.compileExpr(field.value);
        }

        // TODO: Handle spread operator

        const field_count = struc.fields.len;
        if (field_count > std.math.maxInt(u16)) {
            try self.addError(.too_many_constants, struc.span, "too many struct fields");
            return;
        }

        // Create struct descriptor with field names for the VM.
        const field_names = self.allocator.alloc([]const u8, field_count) catch {
            try self.addError(.internal_error, struc.span, "out of memory");
            return;
        };
        for (struc.fields, 0..) |field, i| {
            field_names[i] = field.name;
        }

        const descriptor = self.allocator.create(chunk_mod.StructDescriptor) catch {
            self.allocator.free(field_names);
            try self.addError(.internal_error, struc.span, "out of memory");
            return;
        };

        // Extract struct type name from TypeExpr if available.
        const type_name: []const u8 = if (struc.type_name) |te|
            extractTypeName(te)
        else
            "anonymous";

        descriptor.* = .{
            .name = type_name,
            .field_names = field_names,
        };

        const desc_idx = try self.makeConstant(.{ .struct_type = descriptor });
        try self.emitOp2(.op_struct, @intCast(desc_idx), line);
    }

    fn compileArrayLiteral(self: *Compiler, arr: *ast.ArrayLiteral) Error!void {
        const line = arr.span.line;

        // Compile all elements.
        for (arr.elements) |elem| {
            try self.compileExpr(elem);
        }

        // Emit array instruction.
        if (arr.elements.len > std.math.maxInt(u16)) {
            try self.addError(.too_many_constants, arr.span, "too many array elements");
            return;
        }
        try self.emitOp2(.op_array, @intCast(arr.elements.len), line);
    }

    fn compileTupleLiteral(self: *Compiler, tuple: *ast.TupleLiteral) Error!void {
        const line = tuple.span.line;

        // Compile all elements.
        for (tuple.elements) |elem| {
            try self.compileExpr(elem);
        }

        // Emit tuple instruction.
        if (tuple.elements.len > 255) {
            try self.addError(.too_many_constants, tuple.span, "too many tuple elements");
            return;
        }
        try self.emitOp1(.op_tuple, @intCast(tuple.elements.len), line);
    }

    fn compileTypeCast(self: *Compiler, cast: *ast.TypeCast) Error!void {
        const line = cast.span.line;

        // Compile the expression.
        try self.compileExpr(cast.expr);

        // Determine target type tag.
        const type_tag = self.typeExprToTag(cast.target_type);

        // Emit appropriate cast instruction.
        const op: OpCode = if (cast.truncating) .op_cast_trunc else .op_cast;
        try self.emitOp1(op, @intFromEnum(type_tag), line);
    }

    fn compileInterpolatedString(self: *Compiler, interp: *ast.InterpolatedString) Error!void {
        const line = interp.span.line;

        // Compile each part.
        for (interp.parts) |part| {
            switch (part) {
                .string => |s| {
                    try self.emitConstant(.{ .string = s }, line);
                },
                .expr => |e| {
                    try self.compileExpr(e);
                    // Convert to string if needed (VM will handle this).
                },
            }
        }

        // Emit string build instruction.
        if (interp.parts.len > 255) {
            try self.addError(.too_many_constants, interp.span, "too many interpolation parts");
            return;
        }
        try self.emitOp1(.op_string_build, @intCast(interp.parts.len), line);
    }

    // -------------------------------------------------------------------------
    // Pattern compilation
    // -------------------------------------------------------------------------

    fn compilePattern(self: *Compiler, pattern: ast.Pattern, line: u32) Error!void {
        switch (pattern) {
            .binding => |b| {
                // Bind the value on stack to a new local.
                try self.declareVariable(b.name, b.mutable, b.span);
                try self.defineVariable();
            },
            .wildcard => {
                // Pop and discard the value.
                try self.emitOp(.op_pop, line);
            },
            .literal => {
                // Pop and discard - value was matched in pattern test.
                try self.emitOp(.op_pop, line);
            },
            .tuple_pattern => |t| {
                // Destructure tuple.
                for (t.elements, 0..) |elem, i| {
                    try self.emitOp(.op_dup, line);
                    try self.emitConstant(.{ .int = @intCast(i) }, line);
                    try self.emitOp(.op_get_index, line);
                    try self.compilePattern(elem, line);
                }
                try self.emitOp(.op_pop, line);
            },
            .struct_pattern => |s| {
                // Destructure struct.
                for (s.fields) |field| {
                    try self.emitOp(.op_dup, line);
                    const name_idx = try self.makeConstant(.{ .string = field.name });
                    try self.emitOp2(.op_get_field, @intCast(name_idx), line);
                    if (field.pattern) |p| {
                        try self.compilePattern(p, line);
                    } else {
                        // Bind to variable with same name as field.
                        try self.declareVariable(field.name, false, field.span);
                        try self.defineVariable();
                    }
                }
                try self.emitOp(.op_pop, line);
            },
            .variant => |v| {
                // TODO: Implement variant pattern destructuring
                if (v.payload) |payload| {
                    try self.compilePattern(payload, line);
                }
            },
            .or_pattern => {
                // Already handled in pattern test.
                try self.emitOp(.op_pop, line);
            },
            .guarded => |g| {
                try self.compilePattern(g.pattern, line);
            },
        }
    }

    fn compilePatternTest(self: *Compiler, pattern: ast.Pattern, line: u32) Error!void {
        switch (pattern) {
            .wildcard => {
                // Always matches.
                try self.emitOp(.op_true, line);
            },
            .binding => {
                // Always matches (binding just captures).
                try self.emitOp(.op_true, line);
            },
            .literal => |lit| {
                // Compare with literal value.
                try self.compileLiteral(.{ .kind = lit.kind, .span = lit.span });
                try self.emitOp(.op_eq, line);
            },
            .variant => |v| {
                // Test if value is the expected variant.
                const name_idx = try self.makeConstant(.{ .string = v.variant_name });
                try self.emitOp2(.op_match_variant, @intCast(name_idx), line);
            },
            .tuple_pattern => |t| {
                // Test length and each element.
                // For now, just check it's a tuple.
                _ = t;
                try self.emitOp(.op_true, line);
            },
            .struct_pattern => {
                // Test structure matches.
                try self.emitOp(.op_true, line);
            },
            .or_pattern => |o| {
                // Try each alternative.
                for (o.alternatives) |alt| {
                    try self.emitOp(.op_dup, line);
                    try self.compilePatternTest(alt, line);
                    const next = try self.emitJump(.op_jump_if_false, line);
                    try self.emitOp(.op_true, line);
                    const end = try self.emitJump(.op_jump, line);
                    try self.patchJump(next);
                    try self.emitOp(.op_pop, line);
                    // Continue to next alternative if any
                    _ = end;
                }
                try self.emitOp(.op_false, line);
            },
            .guarded => |g| {
                try self.compilePatternTest(g.pattern, line);
            },
        }
    }

    // -------------------------------------------------------------------------
    // Scope management
    // -------------------------------------------------------------------------

    fn beginScope(self: *Compiler) void {
        self.current.scope_depth += 1;
    }

    fn endScope(self: *Compiler) void {
        self.current.scope_depth -= 1;

        // Pop locals that go out of scope.
        while (self.current.local_count > 0 and
            self.current.locals[self.current.local_count - 1].depth > self.current.scope_depth)
        {
            if (self.current.locals[self.current.local_count - 1].is_captured) {
                self.emitOp(.op_close_upvalue, 0) catch {};
            } else {
                self.emitOp(.op_pop, 0) catch {};
            }
            self.current.local_count -= 1;
        }
    }

    fn declareVariable(self: *Compiler, name: []const u8, is_mutable: bool, span: Span) Error!void {
        // Globals are handled differently.
        if (self.current.scope_depth == 0) return;

        // Check for duplicate in current scope.
        var i = self.current.local_count;
        while (i > 0) {
            i -= 1;
            const local = &self.current.locals[i];
            if (local.depth != -1 and local.depth < self.current.scope_depth) {
                break;
            }
            if (std.mem.eql(u8, local.name, name)) {
                try self.addError(.variable_already_defined, span, "variable already defined in this scope");
                return;
            }
        }

        // Add the local.
        if (self.current.local_count >= max_locals) {
            try self.addError(.too_many_locals, span, "too many local variables");
            return;
        }

        self.current.locals[self.current.local_count] = .{
            .name = name,
            .depth = -1, // Uninitialized marker
            .is_mutable = is_mutable,
            .is_captured = false,
        };
        self.current.local_count += 1;
    }

    fn defineVariable(self: *Compiler) Error!void {
        // For locals, mark as initialized.
        if (self.current.scope_depth > 0) {
            self.current.locals[self.current.local_count - 1].depth = self.current.scope_depth;
            return;
        }

        // Globals are defined via op_define_global (already emitted).
    }

    fn resolveLocal(_: *Compiler, scope: *CompilerScope, name: []const u8) ?usize {
        var i = scope.local_count;
        while (i > 0) {
            i -= 1;
            const local = &scope.locals[i];
            if (std.mem.eql(u8, local.name, name)) {
                if (local.depth == -1) {
                    // Variable is being used in its own initializer.
                    return null;
                }
                return i;
            }
        }
        return null;
    }

    fn resolveUpvalue(self: *Compiler, scope: *CompilerScope, name: []const u8) ?usize {
        if (scope.enclosing == null) return null;

        // Check if it's a local in the enclosing function.
        if (self.resolveLocal(scope.enclosing.?, name)) |local| {
            scope.enclosing.?.locals[local].is_captured = true;
            return self.addUpvalue(scope, @intCast(local), true);
        }

        // Check if it's an upvalue in the enclosing function.
        if (self.resolveUpvalue(scope.enclosing.?, name)) |upvalue| {
            return self.addUpvalue(scope, @intCast(upvalue), false);
        }

        return null;
    }

    fn addUpvalue(self: *Compiler, scope: *CompilerScope, index: u8, is_local: bool) ?usize {
        const upvalue_count = scope.upvalue_count;

        // Check if we already have this upvalue.
        for (0..upvalue_count) |i| {
            const upvalue = &scope.upvalues[i];
            if (upvalue.index == index and upvalue.is_local == is_local) {
                return i;
            }
        }

        if (upvalue_count >= max_upvalues) {
            self.addError(.too_many_upvalues, .{ .start = 0, .end = 0, .line = 0, .column = 0 }, "too many upvalues") catch {};
            return null;
        }

        scope.upvalues[upvalue_count] = .{
            .index = index,
            .is_local = is_local,
        };
        scope.upvalue_count += 1;

        return upvalue_count;
    }

    // -------------------------------------------------------------------------
    // Bytecode emission helpers
    // -------------------------------------------------------------------------

    fn currentChunk(self: *Compiler) *Chunk {
        return &self.current.function.chunk;
    }

    fn emitByte(self: *Compiler, byte: u8, line: u32) Error!void {
        try self.currentChunk().writeByte(byte, line);
    }

    fn emitOp(self: *Compiler, op: OpCode, line: u32) Error!void {
        try self.currentChunk().writeOp(op, line);
    }

    fn emitOp1(self: *Compiler, op: OpCode, operand: u8, line: u32) Error!void {
        try self.currentChunk().writeOp1(op, operand, line);
    }

    fn emitOp2(self: *Compiler, op: OpCode, operand: u16, line: u32) Error!void {
        try self.currentChunk().writeOp2(op, operand, line);
    }

    fn emitConstant(self: *Compiler, value: Constant, line: u32) Error!void {
        try self.currentChunk().writeConstant(value, line);
    }

    fn makeConstant(self: *Compiler, value: Constant) Error!usize {
        const index = try self.currentChunk().addConstant(value);
        if (index > std.math.maxInt(u24)) {
            try self.addError(.too_many_constants, .{ .start = 0, .end = 0, .line = 0, .column = 0 }, "too many constants");
            return 0;
        }
        return index;
    }

    fn emitJump(self: *Compiler, op: OpCode, line: u32) Error!usize {
        return try self.currentChunk().writeJump(op, line);
    }

    fn patchJump(self: *Compiler, offset: usize) Error!void {
        self.currentChunk().patchJump(offset) catch {
            try self.addError(.jump_too_large, .{ .start = 0, .end = 0, .line = 0, .column = 0 }, "jump too large");
        };
    }

    fn emitLoop(self: *Compiler, loop_start: usize, line: u32) Error!void {
        self.currentChunk().writeLoop(loop_start, line) catch {
            try self.addError(.loop_too_large, .{ .start = 0, .end = 0, .line = 0, .column = 0 }, "loop too large");
        };
    }

    fn emitReturn(self: *Compiler) Error!void {
        try self.emitOp(.op_return_void, 0);
    }

    // -------------------------------------------------------------------------
    // Type helpers
    // -------------------------------------------------------------------------

    fn typeExprToTag(self: *Compiler, type_expr: ast.TypeExpr) TypeTag {
        _ = self;
        switch (type_expr) {
            .named => |n| {
                if (std.mem.eql(u8, n.name, "i8")) return .i8_;
                if (std.mem.eql(u8, n.name, "i16")) return .i16_;
                if (std.mem.eql(u8, n.name, "i32")) return .i32_;
                if (std.mem.eql(u8, n.name, "i64")) return .i64_;
                if (std.mem.eql(u8, n.name, "i128")) return .i128_;
                if (std.mem.eql(u8, n.name, "isize")) return .isize_;
                if (std.mem.eql(u8, n.name, "u8")) return .u8_;
                if (std.mem.eql(u8, n.name, "u16")) return .u16_;
                if (std.mem.eql(u8, n.name, "u32")) return .u32_;
                if (std.mem.eql(u8, n.name, "u64")) return .u64_;
                if (std.mem.eql(u8, n.name, "u128")) return .u128_;
                if (std.mem.eql(u8, n.name, "usize")) return .usize_;
                if (std.mem.eql(u8, n.name, "f32")) return .f32_;
                if (std.mem.eql(u8, n.name, "f64")) return .f64_;
                if (std.mem.eql(u8, n.name, "bool")) return .bool_;
                if (std.mem.eql(u8, n.name, "char")) return .char_;
                if (std.mem.eql(u8, n.name, "string")) return .string_;
                // Default to i32 for unknown types
                return .i32_;
            },
            else => return .i32_,
        }
    }

    // -------------------------------------------------------------------------
    // Error handling
    // -------------------------------------------------------------------------

    fn addError(self: *Compiler, kind: CompileError.Kind, span: Span, message: []const u8) Error!void {
        try self.errors.append(self.allocator, .{
            .kind = kind,
            .span = span,
            .message = message,
        });
        self.had_error = true;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "Compiler init/deinit" {
    const testing = std.testing;

    var compiler = Compiler.init(testing.allocator);
    defer compiler.deinit();

    try testing.expect(!compiler.had_error);
    try testing.expectEqual(@as(usize, 0), compiler.errors.items.len);
}

test "Compiler compiles empty module" {
    const testing = std.testing;

    var compiler = Compiler.init(testing.allocator);
    defer compiler.deinit();

    const module = ast.Module{
        .module_decl = null,
        .imports = &[_]ast.ImportDecl{},
        .declarations = &[_]ast.Decl{},
    };

    const result = compiler.compile(module);
    if (result) |func| {
        defer {
            func.deinit();
            testing.allocator.destroy(func);
        }

        try testing.expectEqualStrings("<script>", func.name);
        try testing.expectEqual(@as(u8, 0), func.arity);
        // Should have at least a return instruction
        try testing.expect(func.chunk.code.items.len > 0);
    } else |err| {
        try testing.expect(err != error.CompilationFailed);
    }
}

test "Local variable resolution" {
    const testing = std.testing;

    var compiler = Compiler.init(testing.allocator);
    defer compiler.deinit();

    // Create a mock function and scope.
    var func = Function.init(testing.allocator, "test", 0);
    defer func.deinit();

    var scope = CompilerScope.init(testing.allocator, &func, .function, null);
    defer scope.deinit(testing.allocator);

    compiler.current = &scope;

    // Begin a scope and add a variable.
    compiler.beginScope();

    const span = Span{ .start = 0, .end = 0, .line = 1, .column = 1 };
    try compiler.declareVariable("x", false, span);
    try compiler.defineVariable();

    // Should be able to resolve "x".
    const slot = compiler.resolveLocal(&scope, "x");
    try testing.expect(slot != null);
    try testing.expectEqual(@as(usize, 1), slot.?); // Slot 0 is reserved

    // Should not be able to resolve "y".
    const no_slot = compiler.resolveLocal(&scope, "y");
    try testing.expect(no_slot == null);

    compiler.endScope();
}
