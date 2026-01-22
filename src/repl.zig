const std = @import("std");
const version = @import("version.zig");
const Allocator = std.mem.Allocator;
const Lexer = @import("lexer.zig").Lexer;
const parser_mod = @import("parser.zig");
const Parser = parser_mod.Parser;
const ParseError = parser_mod.ParseError;
const ast = @import("ast.zig");
const types = @import("types.zig");
const TypeChecker = @import("checker.zig").TypeChecker;
const Interpreter = @import("interpreter.zig").Interpreter;
const values = @import("values.zig");
const Value = values.Value;

// Zig 0.15 IO helpers
fn getStdIn() std.fs.File {
    return .{ .handle = std.posix.STDIN_FILENO };
}

fn getStdOut() std.fs.File {
    return .{ .handle = std.posix.STDOUT_FILENO };
}

fn getStdErr() std.fs.File {
    return .{ .handle = std.posix.STDERR_FILENO };
}

/// Read a line from a file, returning null on EOF
fn readLine(file: std.fs.File, buffer: []u8) !?[]const u8 {
    var len: usize = 0;
    while (len < buffer.len) {
        var byte: [1]u8 = undefined;
        const bytes_read = file.read(&byte) catch |err| {
            if (len > 0) return buffer[0..len];
            return err;
        };
        if (bytes_read == 0) {
            // EOF
            if (len > 0) return buffer[0..len];
            return null;
        }
        if (byte[0] == '\n') {
            return buffer[0..len];
        }
        buffer[len] = byte[0];
        len += 1;
    }
    // Line too long, return what we have
    return buffer[0..len];
}

/// Result of evaluating REPL input
pub const EvalResult = struct {
    /// The resulting value (null for statements/declarations with no result)
    value: ?Value,
    /// The type of the result (null if unknown/void)
    type_: ?types.Type,
    /// Whether this was a declaration that added a binding
    is_binding: bool,
    /// The name of the binding (if is_binding is true)
    binding_name: ?[]const u8,
};

/// Re-export parser's ReplInput for use
pub const ReplInput = Parser.ReplInput;

/// Re-export parser's Command for use
pub const Command = Parser.Command;

/// Interactive REPL for Klar
pub const Repl = struct {
    allocator: Allocator,
    arena: std.heap.ArenaAllocator,
    interpreter: *Interpreter,
    type_checker: *TypeChecker,
    /// Track binding names for :list command
    bindings: std.StringHashMapUnmanaged(BindingInfo),
    /// Source strings that need to persist (AST nodes reference them)
    source_strings: std.ArrayListUnmanaged([]const u8),

    const BindingInfo = struct {
        type_: types.Type,
        is_mutable: bool,
    };

    pub fn init(allocator: Allocator) !Repl {
        var arena = std.heap.ArenaAllocator.init(allocator);
        errdefer arena.deinit();

        const interpreter = try allocator.create(Interpreter);
        errdefer allocator.destroy(interpreter);
        interpreter.* = try Interpreter.init(allocator);
        errdefer interpreter.deinit();

        const type_checker = try allocator.create(TypeChecker);
        errdefer allocator.destroy(type_checker);
        type_checker.* = TypeChecker.init(allocator);

        return Repl{
            .allocator = allocator,
            .arena = arena,
            .interpreter = interpreter,
            .type_checker = type_checker,
            .bindings = .{},
            .source_strings = .{},
        };
    }

    pub fn deinit(self: *Repl) void {
        // Free persisted source strings
        for (self.source_strings.items) |s| {
            self.allocator.free(s);
        }
        self.source_strings.deinit(self.allocator);
        self.bindings.deinit(self.allocator);
        self.interpreter.deinit();
        self.allocator.destroy(self.interpreter);
        self.type_checker.deinit();
        self.allocator.destroy(self.type_checker);
        self.arena.deinit();
    }

    /// Main REPL loop
    pub fn run(self: *Repl) !void {
        const stdout = getStdOut();
        const stdin = getStdIn();

        try stdout.writeAll(version.repl_banner ++ "\n");
        try stdout.writeAll("Type :help for commands, :quit to exit\n\n");

        var line_buf: [4096]u8 = undefined;

        while (true) {
            try stdout.writeAll("klar> ");

            const line = readLine(stdin, &line_buf) catch |err| {
                if (err == error.EndOfStream) break;
                var err_buf: [256]u8 = undefined;
                const msg = std.fmt.bufPrint(&err_buf, "Read error: {s}\n", .{@errorName(err)}) catch "Read error\n";
                try getStdErr().writeAll(msg);
                continue;
            };

            if (line == null) break; // EOF

            const input = std.mem.trim(u8, line.?, " \t\r\n");
            if (input.len == 0) continue;

            const result = self.evalLine(input) catch |err| {
                self.printError(err);
                continue;
            };

            if (result) |r| {
                self.printResult(r) catch {};
            }
        }

        try stdout.writeAll("\nGoodbye!\n");
    }

    /// Evaluate a single line of input
    pub fn evalLine(self: *Repl, line: []const u8) !?EvalResult {
        // For statements/declarations that create bindings, we need to persist
        // the source string because AST nodes (and type checker symbols) may
        // reference slices of it. Make a copy that stays alive during evaluation.
        const source = try self.allocator.dupe(u8, line);
        var source_freed = false;
        defer if (!source_freed) self.allocator.free(source);

        // Parse the input
        const parsed = try self.parseInput(source);

        // Determine if we need to keep the source after evaluation
        const needs_persistence = switch (parsed) {
            .stmt => |stmt| switch (stmt) {
                .let_decl, .var_decl => true,
                else => false,
            },
            .decl => true,
            else => false,
        };

        // Execute and get result
        const result: ?EvalResult = switch (parsed) {
            .command => |cmd| try self.executeCommand(cmd),
            .empty => null,
            .expr => |expr| try self.evalExpr(expr),
            .stmt => |stmt| try self.evalStmt(stmt),
            .decl => |decl| try self.evalDecl(decl),
        };

        // Persist source if needed (don't let defer free it)
        if (needs_persistence) {
            try self.source_strings.append(self.allocator, source);
            source_freed = true; // Mark as persisted, don't free in defer
        }

        return result;
    }

    /// Parse REPL input (source string is handled by caller via current_source)
    fn parseInput(self: *Repl, source: []const u8) !ReplInput {
        // Check for meta-commands first
        if (source.len > 0 and source[0] == ':') {
            return .{ .command = parseCommand(source) };
        }

        // Reset arena for AST nodes
        _ = self.arena.reset(.retain_capacity);
        const arena_alloc = self.arena.allocator();

        var lexer = Lexer.init(source);
        var parser = Parser.init(arena_alloc, &lexer, source);
        defer parser.deinit();

        // Try to parse as REPL input (expression, statement, or declaration)
        return parser.parseReplInput() catch |err| {
            // Report parse errors
            for (parser.errors.items) |parse_err| {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Parse error at {d}:{d}: {s}\n", .{
                    parse_err.span.line,
                    parse_err.span.column,
                    parse_err.message,
                }) catch "Parse error\n";
                try getStdErr().writeAll(msg);
            }
            return err;
        };
    }

    /// Evaluate an expression
    fn evalExpr(self: *Repl, expr: ast.Expr) !EvalResult {
        // Type check the expression
        const type_ = self.type_checker.checkExpr(expr);

        if (self.type_checker.hasErrors()) {
            defer self.type_checker.clearErrors();
            for (self.type_checker.errors.items) |err| {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Type error: {s}\n", .{err.message}) catch "Type error\n";
                try getStdErr().writeAll(msg);
            }
            return error.TypeError;
        }

        // Evaluate the expression
        const value = self.interpreter.evaluate(expr) catch |err| {
            return err;
        };

        return EvalResult{
            .value = value,
            .type_ = type_,
            .is_binding = false,
            .binding_name = null,
        };
    }

    /// Evaluate a statement
    fn evalStmt(self: *Repl, stmt: ast.Stmt) !EvalResult {
        // Type check the statement
        self.type_checker.checkStmt(stmt);

        if (self.type_checker.hasErrors()) {
            defer self.type_checker.clearErrors();
            for (self.type_checker.errors.items) |err| {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Type error: {s}\n", .{err.message}) catch "Type error\n";
                try getStdErr().writeAll(msg);
            }
            return error.TypeError;
        }

        // Track binding info for let/var
        var binding_name: ?[]const u8 = null;
        var binding_type: ?types.Type = null;
        var is_mutable = false;

        switch (stmt) {
            .let_decl => |l| {
                binding_name = l.name;
                binding_type = self.type_checker.resolveType(l.type_);
            },
            .var_decl => |v| {
                binding_name = v.name;
                is_mutable = true;
                binding_type = self.type_checker.resolveType(v.type_);
            },
            else => {},
        }

        // Execute the statement
        self.interpreter.execute(stmt) catch |err| {
            return err;
        };

        // Track binding for :list command
        if (binding_name) |name| {
            const name_copy = try self.allocator.dupe(u8, name);
            errdefer self.allocator.free(name_copy);

            try self.bindings.put(self.allocator, name_copy, .{
                .type_ = binding_type orelse .void_,
                .is_mutable = is_mutable,
            });
        }

        return EvalResult{
            .value = null,
            .type_ = null,
            .is_binding = binding_name != null,
            .binding_name = binding_name,
        };
    }

    /// Evaluate a declaration
    fn evalDecl(self: *Repl, decl: ast.Decl) !EvalResult {
        // Type check the declaration
        self.type_checker.checkDecl(decl);

        if (self.type_checker.hasErrors()) {
            defer self.type_checker.clearErrors();
            for (self.type_checker.errors.items) |err| {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Type error: {s}\n", .{err.message}) catch "Type error\n";
                try getStdErr().writeAll(msg);
            }
            return error.TypeError;
        }

        var binding_name: ?[]const u8 = null;
        var binding_type: ?types.Type = null;

        switch (decl) {
            .function => |f| {
                binding_name = f.name;
                // Get the function type
                binding_type = self.type_checker.lookupType(f.name);
            },
            .const_decl => |c| {
                binding_name = c.name;
                if (c.type_) |t| {
                    binding_type = self.type_checker.resolveType(t);
                }
            },
            else => {},
        }

        // Execute the declaration
        self.interpreter.executeDecl(decl) catch |err| {
            return err;
        };

        // Track binding for :list command
        if (binding_name) |name| {
            const name_copy = try self.allocator.dupe(u8, name);
            errdefer self.allocator.free(name_copy);

            try self.bindings.put(self.allocator, name_copy, .{
                .type_ = binding_type orelse .void_,
                .is_mutable = false,
            });
        }

        return EvalResult{
            .value = null,
            .type_ = null,
            .is_binding = binding_name != null,
            .binding_name = binding_name,
        };
    }

    /// Execute a REPL meta-command
    fn executeCommand(self: *Repl, cmd: Command) !?EvalResult {
        const stdout = getStdOut();

        switch (cmd.kind) {
            .help => {
                try stdout.writeAll(
                    \\REPL Commands:
                    \\  :help          Show this help message
                    \\  :quit, :q      Exit the REPL
                    \\  :reset         Clear all bindings and start fresh
                    \\  :type <expr>   Show the type of an expression without evaluating
                    \\  :list          Show all current bindings
                    \\  :load <file>   Load and execute a Klar file
                    \\
                    \\Examples:
                    \\  let x: i32 = 42
                    \\  x + 1
                    \\  fn double(n: i32) -> i32 { return n * 2 }
                    \\  double(x)
                    \\
                );
                return null;
            },
            .quit => {
                return error.QuitRepl;
            },
            .reset => {
                self.resetState();
                try stdout.writeAll("State cleared.\n");
                return null;
            },
            .type_ => {
                if (cmd.arg) |expr_text| {
                    try self.showType(expr_text);
                } else {
                    try getStdErr().writeAll("Usage: :type <expression>\n");
                }
                return null;
            },
            .list => {
                try self.listBindings();
                return null;
            },
            .load => {
                if (cmd.arg) |path| {
                    try self.loadFile(path);
                } else {
                    try getStdErr().writeAll("Usage: :load <filename>\n");
                }
                return null;
            },
        }
    }

    /// Parse a meta-command
    fn parseCommand(line: []const u8) Command {
        const trimmed = std.mem.trim(u8, line[1..], " \t");

        // Check for commands with arguments
        if (std.mem.indexOf(u8, trimmed, " ")) |space_idx| {
            const cmd_name = trimmed[0..space_idx];
            const arg = std.mem.trim(u8, trimmed[space_idx + 1 ..], " \t");
            const arg_val = if (arg.len > 0) arg else null;

            if (std.mem.eql(u8, cmd_name, "type")) {
                return .{ .kind = .type_, .arg = arg_val };
            } else if (std.mem.eql(u8, cmd_name, "load")) {
                return .{ .kind = .load, .arg = arg_val };
            }
        }

        // Commands without arguments
        if (std.mem.eql(u8, trimmed, "help") or std.mem.eql(u8, trimmed, "h") or std.mem.eql(u8, trimmed, "?")) {
            return .{ .kind = .help, .arg = null };
        } else if (std.mem.eql(u8, trimmed, "quit") or std.mem.eql(u8, trimmed, "q") or std.mem.eql(u8, trimmed, "exit")) {
            return .{ .kind = .quit, .arg = null };
        } else if (std.mem.eql(u8, trimmed, "reset") or std.mem.eql(u8, trimmed, "clear")) {
            return .{ .kind = .reset, .arg = null };
        } else if (std.mem.eql(u8, trimmed, "list") or std.mem.eql(u8, trimmed, "ls") or std.mem.eql(u8, trimmed, "bindings")) {
            return .{ .kind = .list, .arg = null };
        } else if (std.mem.startsWith(u8, trimmed, "type ")) {
            return .{ .kind = .type_, .arg = std.mem.trim(u8, trimmed[5..], " \t") };
        } else if (std.mem.startsWith(u8, trimmed, "load ")) {
            return .{ .kind = .load, .arg = std.mem.trim(u8, trimmed[5..], " \t") };
        }

        // Unknown command - treat as help
        return .{ .kind = .help, .arg = null };
    }

    /// Reset the REPL state
    fn resetState(self: *Repl) void {
        // Clear bindings tracking
        var iter = self.bindings.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.bindings.clearRetainingCapacity();

        // Clear persisted source strings
        for (self.source_strings.items) |s| {
            self.allocator.free(s);
        }
        self.source_strings.clearRetainingCapacity();

        // Reinitialize interpreter and type checker
        self.interpreter.deinit();
        self.interpreter.* = Interpreter.init(self.allocator) catch return;

        self.type_checker.deinit();
        self.type_checker.* = TypeChecker.init(self.allocator);
    }

    /// Show the type of an expression without evaluating
    fn showType(self: *Repl, expr_text: []const u8) !void {
        _ = self.arena.reset(.retain_capacity);
        const arena_alloc = self.arena.allocator();

        var lexer = Lexer.init(expr_text);
        var parser = Parser.init(arena_alloc, &lexer, expr_text);
        defer parser.deinit();

        const expr = parser.parseExpression() catch |err| {
            for (parser.errors.items) |parse_err| {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Parse error: {s}\n", .{parse_err.message}) catch "Parse error\n";
                try getStdErr().writeAll(msg);
            }
            return err;
        };

        const type_ = self.type_checker.checkExpr(expr);

        if (self.type_checker.hasErrors()) {
            defer self.type_checker.clearErrors();
            for (self.type_checker.errors.items) |err| {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Type error: {s}\n", .{err.message}) catch "Type error\n";
                try getStdErr().writeAll(msg);
            }
            return;
        }

        const type_str = types.typeToString(self.allocator, type_) catch "?";
        defer if (type_str.len > 0 and type_str.ptr != "?".ptr) self.allocator.free(type_str);

        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "{s}\n", .{type_str}) catch "?\n";
        try getStdOut().writeAll(msg);
    }

    /// List all current bindings
    fn listBindings(self: *Repl) !void {
        const stdout = getStdOut();

        if (self.bindings.count() == 0) {
            try stdout.writeAll("No bindings defined.\n");
            return;
        }

        var iter = self.bindings.iterator();
        while (iter.next()) |entry| {
            const type_str = types.typeToString(self.allocator, entry.value_ptr.type_) catch "?";
            defer if (type_str.len > 0 and type_str.ptr != "?".ptr) self.allocator.free(type_str);

            const mutability = if (entry.value_ptr.is_mutable) "var" else "let";
            var buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "{s} {s}: {s}\n", .{
                mutability,
                entry.key_ptr.*,
                type_str,
            }) catch continue;
            try stdout.writeAll(msg);
        }
    }

    /// Load and execute a file
    fn loadFile(self: *Repl, path: []const u8) !void {
        const file = std.fs.cwd().openFile(path, .{}) catch |err| {
            var buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Error opening '{s}': {s}\n", .{ path, @errorName(err) }) catch "Error opening file\n";
            try getStdErr().writeAll(msg);
            return;
        };
        defer file.close();

        // Source must be persisted because symbols will reference slices of it
        const source = file.readToEndAlloc(self.allocator, 10 * 1024 * 1024) catch |err| {
            var buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Error reading '{s}': {s}\n", .{ path, @errorName(err) }) catch "Error reading file\n";
            try getStdErr().writeAll(msg);
            return;
        };
        errdefer self.allocator.free(source);

        // Parse the file as a module (use main allocator, not arena, so AST persists)
        var lexer = Lexer.init(source);
        var parser = Parser.init(self.allocator, &lexer, source);
        defer parser.deinit();

        const module = parser.parseModule() catch |err| {
            for (parser.errors.items) |parse_err| {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Parse error at {d}:{d}: {s}\n", .{
                    parse_err.span.line,
                    parse_err.span.column,
                    parse_err.message,
                }) catch "Parse error\n";
                try getStdErr().writeAll(msg);
            }
            return err;
        };

        // Type check the module
        self.type_checker.checkModule(module);

        if (self.type_checker.hasErrors()) {
            defer self.type_checker.clearErrors();
            for (self.type_checker.errors.items) |err| {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Type error: {s}\n", .{err.message}) catch "Type error\n";
                try getStdErr().writeAll(msg);
            }
            self.allocator.free(source);
            return;
        }

        // Execute the module
        self.interpreter.executeModule(module) catch |err| {
            var buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Runtime error: {s}\n", .{@errorName(err)}) catch "Runtime error\n";
            try getStdErr().writeAll(msg);
            self.allocator.free(source);
            return;
        };

        // Persist source string (symbols reference it)
        try self.source_strings.append(self.allocator, source);

        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Loaded '{s}'\n", .{path}) catch "Loaded file\n";
        try getStdOut().writeAll(msg);
    }

    /// Print an evaluation result
    fn printResult(self: *Repl, result: EvalResult) !void {
        const stdout = getStdOut();

        if (result.value) |value| {
            // Don't print void values
            if (value == .void_) return;

            // Format the value
            const value_str = values.valueToString(self.allocator, value) catch "?";
            defer if (value_str.len > 0 and value_str.ptr != "?".ptr) self.allocator.free(value_str);

            if (result.type_) |t| {
                const type_str = types.typeToString(self.allocator, t) catch "?";
                defer if (type_str.len > 0 and type_str.ptr != "?".ptr) self.allocator.free(type_str);

                var buf: [1024]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "{s}: {s}\n", .{ value_str, type_str }) catch {
                    try stdout.writeAll(value_str);
                    try stdout.writeAll("\n");
                    return;
                };
                try stdout.writeAll(msg);
            } else {
                try stdout.writeAll(value_str);
                try stdout.writeAll("\n");
            }
        } else if (result.is_binding) {
            // Binding defined
            if (result.binding_name) |name| {
                var buf: [256]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "{s} defined\n", .{name}) catch "";
                if (msg.len > 0) try stdout.writeAll(msg);
            }
        }
    }

    /// Print an error
    fn printError(self: *Repl, err: anyerror) void {
        _ = self;
        // QuitRepl is not an error, it's a normal exit
        if (err == error.QuitRepl) {
            std.process.exit(0);
        }

        const stderr = getStdErr();
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Error: {s}\n", .{@errorName(err)}) catch "Error\n";
        stderr.writeAll(msg) catch {};
    }
};

// Error for quitting REPL
const ReplError = error{
    QuitRepl,
    TypeError,
};

// ============================================================================
// Tests
// ============================================================================

test "Repl initialization" {
    const testing = std.testing;
    var repl = try Repl.init(testing.allocator);
    defer repl.deinit();
}

test "parseCommand help" {
    const cmd = Repl.parseCommand(":help");
    try std.testing.expectEqual(Command.Kind.help, cmd.kind);
    try std.testing.expect(cmd.arg == null);
}

test "parseCommand quit" {
    const cmd = Repl.parseCommand(":quit");
    try std.testing.expectEqual(Command.Kind.quit, cmd.kind);

    const cmd2 = Repl.parseCommand(":q");
    try std.testing.expectEqual(Command.Kind.quit, cmd2.kind);
}

test "parseCommand type with arg" {
    const cmd = Repl.parseCommand(":type x + 1");
    try std.testing.expectEqual(Command.Kind.type_, cmd.kind);
    try std.testing.expectEqualStrings("x + 1", cmd.arg.?);
}

test "parseCommand list" {
    const cmd = Repl.parseCommand(":list");
    try std.testing.expectEqual(Command.Kind.list, cmd.kind);
}
