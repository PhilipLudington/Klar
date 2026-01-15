const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;
const Parser = @import("parser.zig").Parser;
const ast = @import("ast.zig");
const TypeChecker = @import("checker.zig").TypeChecker;
const Interpreter = @import("interpreter.zig").Interpreter;
const values = @import("values.zig");
const Compiler = @import("compiler.zig").Compiler;
const Disassembler = @import("disasm.zig").Disassembler;
const VM = @import("vm.zig").VM;

// Zig 0.15 IO helpers
fn getStdOut() std.fs.File {
    return .{ .handle = std.posix.STDOUT_FILENO };
}

fn getStdErr() std.fs.File {
    return .{ .handle = std.posix.STDERR_FILENO };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        try printUsage();
        return;
    }

    const command = args[1];

    if (std.mem.eql(u8, command, "run") or std.mem.eql(u8, command, "run-vm")) {
        if (args.len < 3) {
            try getStdErr().writeAll("Error: no input file\n");
            return;
        }
        // Check for flags
        var debug_mode = false;
        var use_interpreter = false;
        for (args) |arg| {
            if (std.mem.eql(u8, arg, "--debug")) {
                debug_mode = true;
            } else if (std.mem.eql(u8, arg, "--interpret")) {
                use_interpreter = true;
            }
        }
        if (use_interpreter) {
            try runInterpreterFile(allocator, args[2]);
        } else {
            try runVmFile(allocator, args[2], debug_mode);
        }
    } else if (std.mem.eql(u8, command, "tokenize")) {
        if (args.len < 3) {
            try getStdErr().writeAll("Error: no input file\n");
            return;
        }
        try tokenizeFile(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "parse")) {
        if (args.len < 3) {
            try getStdErr().writeAll("Error: no input file\n");
            return;
        }
        try parseFile(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "build")) {
        try getStdErr().writeAll("Build not yet implemented\n");
    } else if (std.mem.eql(u8, command, "check")) {
        if (args.len < 3) {
            try getStdErr().writeAll("Error: no input file\n");
            return;
        }
        try checkFile(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "disasm")) {
        if (args.len < 3) {
            try getStdErr().writeAll("Error: no input file\n");
            return;
        }
        try disasmFile(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "test")) {
        try getStdErr().writeAll("Test not yet implemented\n");
    } else if (std.mem.eql(u8, command, "fmt")) {
        try getStdErr().writeAll("Format not yet implemented\n");
    } else if (std.mem.eql(u8, command, "help") or std.mem.eql(u8, command, "--help") or std.mem.eql(u8, command, "-h")) {
        try printUsage();
    } else if (std.mem.eql(u8, command, "version") or std.mem.eql(u8, command, "--version") or std.mem.eql(u8, command, "-v")) {
        try printVersion();
    } else {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Unknown command: {s}\n\n", .{command}) catch "Unknown command\n\n";
        try getStdErr().writeAll(msg);
        try printUsage();
    }
}

fn runInterpreterFile(allocator: std.mem.Allocator, path: []const u8) !void {
    const source = readSourceFile(allocator, path) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Error opening file '{s}': {}\n", .{ path, err }) catch "Error opening file\n";
        try getStdErr().writeAll(msg);
        return;
    };
    defer allocator.free(source);

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const stderr = getStdErr();

    // Parse the source
    var lexer = Lexer.init(source);
    var parser = Parser.init(arena.allocator(), &lexer, source);

    const module = parser.parseModule() catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Parse error: {}\n", .{err}) catch "Parse error\n";
        try stderr.writeAll(msg);

        for (parser.errors.items) |parse_err| {
            const err_msg = std.fmt.bufPrint(&buf, "  {d}:{d}: {s}\n", .{
                parse_err.span.line,
                parse_err.span.column,
                parse_err.message,
            }) catch continue;
            try stderr.writeAll(err_msg);
        }
        return;
    };

    // Type check
    var checker = TypeChecker.init(allocator);
    defer checker.deinit();

    checker.checkModule(module);

    if (checker.hasErrors()) {
        var buf: [512]u8 = undefined;
        const header = std.fmt.bufPrint(&buf, "Type error(s):\n", .{}) catch "Type errors:\n";
        try stderr.writeAll(header);

        for (checker.errors.items) |check_err| {
            const err_msg = std.fmt.bufPrint(&buf, "  {d}:{d}: {s}\n", .{
                check_err.span.line,
                check_err.span.column,
                check_err.message,
            }) catch continue;
            try stderr.writeAll(err_msg);
        }
        return;
    }

    // Execute
    var interp = Interpreter.init(allocator) catch {
        try stderr.writeAll("Failed to initialize interpreter\n");
        return;
    };
    defer interp.deinit();

    interp.executeModule(module) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Runtime error: {s}\n", .{@errorName(err)}) catch "Runtime error\n";
        try stderr.writeAll(msg);
        return;
    };

    // Look for main function and call it
    if (interp.global_env.get("main")) |main_val| {
        if (main_val == .function) {
            _ = interp.callFunction(main_val.function, &.{}) catch |err| {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Runtime error in main: {s}\n", .{@errorName(err)}) catch "Runtime error in main\n";
                try stderr.writeAll(msg);
            };
        }
    }
}

fn tokenizeFile(allocator: std.mem.Allocator, path: []const u8) !void {
    const source = readSourceFile(allocator, path) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Error opening file '{s}': {}\n", .{ path, err }) catch "Error opening file\n";
        try getStdErr().writeAll(msg);
        return;
    };
    defer allocator.free(source);

    const stdout = getStdOut();
    var lexer = Lexer.init(source);
    var buf: [512]u8 = undefined;

    while (true) {
        const token = lexer.next();
        const text = source[token.loc.start..token.loc.end];

        // Skip newlines for cleaner output
        if (token.kind == .newline) continue;

        const msg = std.fmt.bufPrint(&buf, "{d}:{d}\t{s}\t{s}\n", .{
            token.loc.line,
            token.loc.column,
            @tagName(token.kind),
            text,
        }) catch continue;
        try stdout.writeAll(msg);

        if (token.kind == .eof) break;
    }
}

fn readSourceFile(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    return try file.readToEndAlloc(allocator, 10 * 1024 * 1024);
}

fn parseFile(allocator: std.mem.Allocator, path: []const u8) !void {
    const source = readSourceFile(allocator, path) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Error opening file '{s}': {}\n", .{ path, err }) catch "Error opening file\n";
        try getStdErr().writeAll(msg);
        return;
    };
    defer allocator.free(source);

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const stdout = getStdOut();
    var lexer = Lexer.init(source);
    var parser = Parser.init(arena.allocator(), &lexer, source);

    try stdout.writeAll("=== Parsing Expression ===\n");

    const expr = parser.parseExpression() catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Parse error: {}\n", .{err}) catch "Parse error\n";
        try getStdErr().writeAll(msg);

        // Print any accumulated errors
        for (parser.errors.items) |parse_err| {
            const err_msg = std.fmt.bufPrint(&buf, "  {d}:{d}: {s}\n", .{
                parse_err.span.line,
                parse_err.span.column,
                parse_err.message,
            }) catch continue;
            try getStdErr().writeAll(err_msg);
        }
        return;
    };

    // Print the parsed expression
    try printExpr(stdout, source, expr, 0);
    try stdout.writeAll("\n");
}

fn printExpr(out: std.fs.File, source: []const u8, expr: ast.Expr, indent: usize) !void {
    var indent_buf: [64]u8 = undefined;
    const indent_str = indent_buf[0..@min(indent * 2, 64)];
    @memset(indent_str, ' ');

    var buf: [512]u8 = undefined;

    switch (expr) {
        .literal => |lit| {
            const kind_str = switch (lit.kind) {
                .int => |v| std.fmt.bufPrint(&buf, "int({d})", .{v}) catch "int(?)",
                .float => |v| std.fmt.bufPrint(&buf, "float({d})", .{v}) catch "float(?)",
                .string => |v| std.fmt.bufPrint(&buf, "string(\"{s}\")", .{v}) catch "string(?)",
                .char => |v| std.fmt.bufPrint(&buf, "char('{u}')", .{v}) catch "char(?)",
                .bool_ => |v| if (v) "bool(true)" else "bool(false)",
            };
            try out.writeAll(indent_str);
            try out.writeAll("Literal: ");
            try out.writeAll(kind_str);
            try out.writeAll("\n");
        },
        .identifier => |id| {
            try out.writeAll(indent_str);
            const msg = std.fmt.bufPrint(&buf, "Identifier: {s}\n", .{id.name}) catch "Identifier: ?\n";
            try out.writeAll(msg);
        },
        .binary => |bin| {
            try out.writeAll(indent_str);
            const msg = std.fmt.bufPrint(&buf, "Binary: {s}\n", .{@tagName(bin.op)}) catch "Binary: ?\n";
            try out.writeAll(msg);
            try printExpr(out, source, bin.left, indent + 1);
            try printExpr(out, source, bin.right, indent + 1);
        },
        .unary => |un| {
            try out.writeAll(indent_str);
            const msg = std.fmt.bufPrint(&buf, "Unary: {s}\n", .{@tagName(un.op)}) catch "Unary: ?\n";
            try out.writeAll(msg);
            try printExpr(out, source, un.operand, indent + 1);
        },
        .call => |call| {
            try out.writeAll(indent_str);
            try out.writeAll("Call:\n");
            try out.writeAll(indent_str);
            try out.writeAll("  callee:\n");
            try printExpr(out, source, call.callee, indent + 2);
            try out.writeAll(indent_str);
            const msg = std.fmt.bufPrint(&buf, "  args: ({d})\n", .{call.args.len}) catch "  args: ?\n";
            try out.writeAll(msg);
            for (call.args) |arg| {
                try printExpr(out, source, arg, indent + 2);
            }
        },
        .if_expr => |if_e| {
            try out.writeAll(indent_str);
            try out.writeAll("If:\n");
            try out.writeAll(indent_str);
            try out.writeAll("  condition:\n");
            try printExpr(out, source, if_e.condition, indent + 2);
            try out.writeAll(indent_str);
            try out.writeAll("  then:\n");
            try printExpr(out, source, if_e.then_branch, indent + 2);
            if (if_e.else_branch) |else_b| {
                try out.writeAll(indent_str);
                try out.writeAll("  else:\n");
                try printExpr(out, source, else_b, indent + 2);
            }
        },
        .block => |blk| {
            try out.writeAll(indent_str);
            const msg = std.fmt.bufPrint(&buf, "Block: ({d} stmts)\n", .{blk.statements.len}) catch "Block: ?\n";
            try out.writeAll(msg);
            if (blk.final_expr) |final| {
                try out.writeAll(indent_str);
                try out.writeAll("  final:\n");
                try printExpr(out, source, final, indent + 2);
            }
        },
        .closure => |cls| {
            try out.writeAll(indent_str);
            const msg = std.fmt.bufPrint(&buf, "Closure: ({d} params)\n", .{cls.params.len}) catch "Closure: ?\n";
            try out.writeAll(msg);
            try out.writeAll(indent_str);
            try out.writeAll("  body:\n");
            try printExpr(out, source, cls.body, indent + 2);
        },
        .field => |fld| {
            try out.writeAll(indent_str);
            const msg = std.fmt.bufPrint(&buf, "Field: .{s}\n", .{fld.field_name}) catch "Field: ?\n";
            try out.writeAll(msg);
            try printExpr(out, source, fld.object, indent + 1);
        },
        .method_call => |meth| {
            try out.writeAll(indent_str);
            const msg = std.fmt.bufPrint(&buf, "MethodCall: .{s}()\n", .{meth.method_name}) catch "MethodCall: ?\n";
            try out.writeAll(msg);
            try printExpr(out, source, meth.object, indent + 1);
        },
        .index => |idx| {
            try out.writeAll(indent_str);
            try out.writeAll("Index:\n");
            try printExpr(out, source, idx.object, indent + 1);
            try printExpr(out, source, idx.index, indent + 1);
        },
        .range => |rng| {
            try out.writeAll(indent_str);
            const msg = std.fmt.bufPrint(&buf, "Range: (inclusive={s})\n", .{if (rng.inclusive) "true" else "false"}) catch "Range: ?\n";
            try out.writeAll(msg);
            if (rng.start) |s| try printExpr(out, source, s, indent + 1);
            if (rng.end) |e| try printExpr(out, source, e, indent + 1);
        },
        .array_literal => |arr| {
            try out.writeAll(indent_str);
            const msg = std.fmt.bufPrint(&buf, "Array: [{d} elements]\n", .{arr.elements.len}) catch "Array: ?\n";
            try out.writeAll(msg);
        },
        .tuple_literal => |tup| {
            try out.writeAll(indent_str);
            const msg = std.fmt.bufPrint(&buf, "Tuple: ({d} elements)\n", .{tup.elements.len}) catch "Tuple: ?\n";
            try out.writeAll(msg);
        },
        else => {
            try out.writeAll(indent_str);
            try out.writeAll("(other expression)\n");
        },
    }
}

fn checkFile(allocator: std.mem.Allocator, path: []const u8) !void {
    const source = readSourceFile(allocator, path) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Error opening file '{s}': {}\n", .{ path, err }) catch "Error opening file\n";
        try getStdErr().writeAll(msg);
        return;
    };
    defer allocator.free(source);

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const stdout = getStdOut();
    const stderr = getStdErr();
    var lexer = Lexer.init(source);
    var parser = Parser.init(arena.allocator(), &lexer, source);

    // Parse the module
    const module = parser.parseModule() catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Parse error: {}\n", .{err}) catch "Parse error\n";
        try stderr.writeAll(msg);

        for (parser.errors.items) |parse_err| {
            const err_msg = std.fmt.bufPrint(&buf, "  {d}:{d}: {s}\n", .{
                parse_err.span.line,
                parse_err.span.column,
                parse_err.message,
            }) catch continue;
            try stderr.writeAll(err_msg);
        }
        return;
    };

    // Type check the module
    var checker = TypeChecker.init(allocator);
    defer checker.deinit();

    checker.checkModule(module);

    var buf: [512]u8 = undefined;

    if (checker.hasErrors()) {
        const header = std.fmt.bufPrint(&buf, "Type check failed with {d} error(s):\n", .{checker.errors.items.len}) catch "Type check failed:\n";
        try stderr.writeAll(header);

        for (checker.errors.items) |check_err| {
            const err_msg = std.fmt.bufPrint(&buf, "  {d}:{d} [{s}]: {s}\n", .{
                check_err.span.line,
                check_err.span.column,
                @tagName(check_err.kind),
                check_err.message,
            }) catch continue;
            try stderr.writeAll(err_msg);
        }
    } else {
        const msg = std.fmt.bufPrint(&buf, "Type check passed for '{s}'\n", .{path}) catch "Type check passed\n";
        try stdout.writeAll(msg);

        // Print some stats
        const decl_count = module.declarations.len;
        const stats = std.fmt.bufPrint(&buf, "  {d} declaration(s)\n", .{decl_count}) catch "";
        try stdout.writeAll(stats);
    }
}

fn runVmFile(allocator: std.mem.Allocator, path: []const u8, debug_mode: bool) !void {
    const source = readSourceFile(allocator, path) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Error opening file '{s}': {}\n", .{ path, err }) catch "Error opening file\n";
        try getStdErr().writeAll(msg);
        return;
    };
    defer allocator.free(source);

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const stderr = getStdErr();

    // Parse the source
    var lexer = Lexer.init(source);
    var parser = Parser.init(arena.allocator(), &lexer, source);

    const module = parser.parseModule() catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Parse error: {}\n", .{err}) catch "Parse error\n";
        try stderr.writeAll(msg);

        for (parser.errors.items) |parse_err| {
            const err_msg = std.fmt.bufPrint(&buf, "  {d}:{d}: {s}\n", .{
                parse_err.span.line,
                parse_err.span.column,
                parse_err.message,
            }) catch continue;
            try stderr.writeAll(err_msg);
        }
        return;
    };

    // Type check
    var checker = TypeChecker.init(allocator);
    defer checker.deinit();

    checker.checkModule(module);

    if (checker.hasErrors()) {
        var buf: [512]u8 = undefined;
        const header = std.fmt.bufPrint(&buf, "Type error(s):\n", .{}) catch "Type errors:\n";
        try stderr.writeAll(header);

        for (checker.errors.items) |check_err| {
            const err_msg = std.fmt.bufPrint(&buf, "  {d}:{d}: {s}\n", .{
                check_err.span.line,
                check_err.span.column,
                check_err.message,
            }) catch continue;
            try stderr.writeAll(err_msg);
        }
        return;
    }

    // Compile to bytecode
    var compiler = Compiler.init(allocator);
    defer compiler.deinit();

    const function = compiler.compile(module) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Compilation error: {s}\n", .{@errorName(err)}) catch "Compilation error\n";
        try stderr.writeAll(msg);

        for (compiler.errors.items) |comp_err| {
            const err_msg = std.fmt.bufPrint(&buf, "  {d}:{d}: {s}\n", .{
                comp_err.span.line,
                comp_err.span.column,
                comp_err.message,
            }) catch continue;
            try stderr.writeAll(err_msg);
        }
        return;
    };
    defer {
        function.deinit();
        allocator.destroy(function);
    }

    // Run in VM
    var vm = VM.init(allocator) catch {
        try stderr.writeAll("Failed to initialize VM\n");
        return;
    };
    defer vm.deinit();

    // Set up GC roots and register builtins
    vm.setup() catch {
        try stderr.writeAll("Failed to set up VM\n");
        return;
    };

    // Set debug mode if requested
    vm.debug_trace = debug_mode;

    _ = vm.interpret(function) catch |err| {
        var buf: [512]u8 = undefined;

        // Check if we have detailed error context
        if (vm.getLastError()) |ctx| {
            var err_buf: [2048]u8 = undefined;
            const detailed_msg = ctx.format(&err_buf);
            try stderr.writeAll(detailed_msg);
        } else {
            const msg = std.fmt.bufPrint(&buf, "Runtime error: {s}\n", .{@errorName(err)}) catch "Runtime error\n";
            try stderr.writeAll(msg);
        }
        return;
    };

    // Look for main function and call it
    if (vm.globals.get("main")) |main_val| {
        if (main_val == .closure) {
            _ = vm.callMain(main_val.closure) catch |err| {
                var buf: [512]u8 = undefined;
                if (vm.getLastError()) |ctx| {
                    var err_buf: [2048]u8 = undefined;
                    const detailed_msg = ctx.format(&err_buf);
                    try stderr.writeAll(detailed_msg);
                } else {
                    const msg = std.fmt.bufPrint(&buf, "Runtime error in main: {s}\n", .{@errorName(err)}) catch "Runtime error in main\n";
                    try stderr.writeAll(msg);
                }
            };
        }
    }
}

fn disasmFile(allocator: std.mem.Allocator, path: []const u8) !void {
    const source = readSourceFile(allocator, path) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Error opening file '{s}': {}\n", .{ path, err }) catch "Error opening file\n";
        try getStdErr().writeAll(msg);
        return;
    };
    defer allocator.free(source);

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const stderr = getStdErr();
    const stdout = getStdOut();

    // Parse the source
    var lexer = Lexer.init(source);
    var parser = Parser.init(arena.allocator(), &lexer, source);

    const module = parser.parseModule() catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Parse error: {}\n", .{err}) catch "Parse error\n";
        try stderr.writeAll(msg);

        for (parser.errors.items) |parse_err| {
            const err_msg = std.fmt.bufPrint(&buf, "  {d}:{d}: {s}\n", .{
                parse_err.span.line,
                parse_err.span.column,
                parse_err.message,
            }) catch continue;
            try stderr.writeAll(err_msg);
        }
        return;
    };

    // Type check
    var checker = TypeChecker.init(allocator);
    defer checker.deinit();

    checker.checkModule(module);

    if (checker.hasErrors()) {
        var buf: [512]u8 = undefined;
        const header = std.fmt.bufPrint(&buf, "Type error(s):\n", .{}) catch "Type errors:\n";
        try stderr.writeAll(header);

        for (checker.errors.items) |check_err| {
            const err_msg = std.fmt.bufPrint(&buf, "  {d}:{d}: {s}\n", .{
                check_err.span.line,
                check_err.span.column,
                check_err.message,
            }) catch continue;
            try stderr.writeAll(err_msg);
        }
        return;
    }

    // Compile to bytecode
    var compiler = Compiler.init(allocator);
    defer compiler.deinit();

    const function = compiler.compile(module) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Compilation error: {s}\n", .{@errorName(err)}) catch "Compilation error\n";
        try stderr.writeAll(msg);

        for (compiler.errors.items) |comp_err| {
            const err_msg = std.fmt.bufPrint(&buf, "  {d}:{d}: {s}\n", .{
                comp_err.span.line,
                comp_err.span.column,
                comp_err.message,
            }) catch continue;
            try stderr.writeAll(err_msg);
        }
        return;
    };
    defer {
        function.deinit();
        allocator.destroy(function);
    }

    // Disassemble
    var disasm = Disassembler.init(allocator);
    defer disasm.deinit();

    disasm.disassembleFunction(function) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Disassembly error: {s}\n", .{@errorName(err)}) catch "Disassembly error\n";
        try stderr.writeAll(msg);
        return;
    };

    try stdout.writeAll(disasm.getOutput());
}

fn printUsage() !void {
    try getStdOut().writeAll(
        \\
        \\  Klar - No ambiguity. No surprises.
        \\
        \\Usage: klar <command> [options]
        \\
        \\Commands:
        \\  run <file>           Run a Klar program (bytecode VM)
        \\  tokenize <file>      Tokenize a file (lexer output)
        \\  parse <file>         Parse a file (AST output)
        \\  check <file>         Type check a file
        \\  disasm <file>        Disassemble bytecode
        \\  build                Build a Klar project
        \\  test                 Run tests
        \\  fmt                  Format source files
        \\  help                 Show this help
        \\  version              Show version
        \\
        \\Options:
        \\  --debug              Enable instruction tracing
        \\  --interpret          Use tree-walking interpreter instead of VM
        \\
        \\Examples:
        \\  klar run hello.kl
        \\  klar run hello.kl --debug
        \\  klar run hello.kl --interpret
        \\  klar tokenize example.kl
        \\  klar check example.kl
        \\  klar disasm example.kl
        \\
    );
}

fn printVersion() !void {
    try getStdOut().writeAll("Klar 0.2.0-dev\n");
}

// Re-export tests from all modules
test {
    _ = @import("token.zig");
    _ = @import("lexer.zig");
    _ = @import("ast.zig");
    _ = @import("parser.zig");
    _ = @import("types.zig");
    _ = @import("checker.zig");
    _ = @import("values.zig");
    _ = @import("interpreter.zig");
    _ = @import("bytecode.zig");
    _ = @import("chunk.zig");
    _ = @import("compiler.zig");
    _ = @import("vm.zig");
    _ = @import("vm_value.zig");
    _ = @import("vm_builtins.zig");
    _ = @import("gc.zig");
    _ = @import("disasm.zig");
}
