const std = @import("std");
const version = @import("version.zig");
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
const codegen = @import("codegen/mod.zig");
const ir = @import("ir/mod.zig");
const ownership = @import("ownership/mod.zig");
const opt = @import("opt/mod.zig");
const module_resolver = @import("module_resolver.zig");
const ModuleResolver = module_resolver.ModuleResolver;
const ModuleInfo = module_resolver.ModuleInfo;
const Repl = @import("repl.zig").Repl;

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

    if (std.mem.eql(u8, command, "run")) {
        if (args.len < 3) {
            try getStdErr().writeAll("Error: no input file\n");
            return;
        }
        // Check for flags
        var use_vm = false;
        var debug_mode = false;
        var use_interpreter = false;
        for (args) |arg| {
            if (std.mem.eql(u8, arg, "--vm")) {
                use_vm = true;
            } else if (std.mem.eql(u8, arg, "--debug")) {
                debug_mode = true;
            } else if (std.mem.eql(u8, arg, "--interpret")) {
                use_interpreter = true;
            }
        }
        if (use_vm or use_interpreter) {
            if (use_interpreter) {
                try runInterpreterFile(allocator, args[2]);
            } else {
                try runVmFile(allocator, args[2], debug_mode);
            }
        } else {
            // Default: compile to native and run
            try runNativeFile(allocator, args[2]);
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
        if (args.len < 3) {
            try getStdErr().writeAll("Error: no input file\n");
            return;
        }
        // Parse options
        var output_path: ?[]const u8 = null;
        var emit_llvm = false;
        var emit_asm = false;
        var emit_ir = false;
        var opt_level: opt.OptLevel = .O0;
        var verbose_opt = false;
        var debug_info = false;
        var target_triple: ?[]const u8 = null;

        var i: usize = 3;
        while (i < args.len) : (i += 1) {
            const arg = args[i];
            if (std.mem.eql(u8, arg, "-o") and i + 1 < args.len) {
                output_path = args[i + 1];
                i += 1;
            } else if (std.mem.eql(u8, arg, "--emit-llvm")) {
                emit_llvm = true;
            } else if (std.mem.eql(u8, arg, "--emit-asm")) {
                emit_asm = true;
            } else if (std.mem.eql(u8, arg, "--emit-ir")) {
                emit_ir = true;
            } else if (std.mem.eql(u8, arg, "-O0")) {
                opt_level = .O0;
            } else if (std.mem.eql(u8, arg, "-O1")) {
                opt_level = .O1;
            } else if (std.mem.eql(u8, arg, "-O2")) {
                opt_level = .O2;
            } else if (std.mem.eql(u8, arg, "-O3")) {
                opt_level = .O3;
            } else if (std.mem.eql(u8, arg, "--verbose-opt")) {
                verbose_opt = true;
            } else if (std.mem.eql(u8, arg, "-g")) {
                debug_info = true;
            } else if (std.mem.eql(u8, arg, "--target") and i + 1 < args.len) {
                target_triple = args[i + 1];
                i += 1;
            } else if (std.mem.startsWith(u8, arg, "--target=")) {
                target_triple = arg["--target=".len..];
            }
        }

        try buildNative(allocator, args[2], .{
            .output_path = output_path,
            .emit_llvm_ir = emit_llvm,
            .emit_assembly = emit_asm,
            .emit_klar_ir = emit_ir,
            .opt_level = opt_level,
            .verbose_opt = verbose_opt,
            .debug_info = debug_info,
            .source_path = args[2],
            .target_triple = target_triple,
        });
    } else if (std.mem.eql(u8, command, "check")) {
        if (args.len < 3) {
            try getStdErr().writeAll("Error: no input file\n");
            return;
        }
        // Parse flags
        var dump_ownership = false;
        for (args) |arg| {
            if (std.mem.eql(u8, arg, "--dump-ownership")) {
                dump_ownership = true;
            }
        }
        try checkFile(allocator, args[2], dump_ownership);
    } else if (std.mem.eql(u8, command, "disasm")) {
        if (args.len < 3) {
            try getStdErr().writeAll("Error: no input file\n");
            return;
        }
        try disasmFile(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "repl")) {
        try runRepl(allocator);
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

fn runRepl(allocator: std.mem.Allocator) !void {
    var repl = Repl.init(allocator) catch {
        try getStdErr().writeAll("Failed to initialize REPL\n");
        return;
    };
    defer repl.deinit();

    repl.run() catch |err| {
        // Normal exit via :quit doesn't need error message
        if (err != error.QuitRepl) {
            var buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "REPL error: {s}\n", .{@errorName(err)}) catch "REPL error\n";
            try getStdErr().writeAll(msg);
        }
    };
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

/// Try to find the standard library path relative to the compiler binary.
fn findStdLibPath(allocator: std.mem.Allocator) ?[]const u8 {
    // Get the path to the current executable
    var exe_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const exe_path = std.fs.selfExePath(&exe_path_buf) catch return null;

    const exe_dir = std.fs.path.dirname(exe_path) orelse return null;

    // Try relative paths from the executable
    const candidates = [_][]const u8{
        "std", // <exe_dir>/std
        "../std", // <exe_dir>/../std
        "../lib/std", // <exe_dir>/../lib/std
        "../../std", // <exe_dir>/../../std (for zig-out/bin)
    };

    for (candidates) |candidate| {
        const full_path = std.fs.path.join(allocator, &.{ exe_dir, candidate }) catch continue;

        // Check if this path exists and contains mod.kl
        const mod_path = std.fs.path.join(allocator, &.{ full_path, "mod.kl" }) catch {
            allocator.free(full_path);
            continue;
        };
        defer allocator.free(mod_path);

        std.fs.cwd().access(mod_path, .{}) catch {
            allocator.free(full_path);
            continue;
        };

        return full_path;
    }

    return null;
}

/// Parse and load a module's source file.
/// Returns the source string which must be kept alive until after codegen.
fn parseModuleSource(
    allocator: std.mem.Allocator,
    arena: std.mem.Allocator,
    mod: *ModuleInfo,
) ![]const u8 {
    // Read source
    const source = readSourceFile(allocator, mod.file_path) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Error opening file '{s}': {}\n", .{ mod.file_path, err }) catch "Error opening file\n";
        try getStdErr().writeAll(msg);
        return error.FileNotFound;
    };
    // Don't store source in ModuleInfo - caller will track it for cleanup

    // Parse
    var lexer = Lexer.init(source);
    var parser = Parser.init(arena, &lexer, source);

    const module_ast = parser.parseModule() catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Parse error in '{s}': {}\n", .{ mod.file_path, err }) catch "Parse error\n";
        try getStdErr().writeAll(msg);

        for (parser.errors.items) |parse_err| {
            const err_msg = std.fmt.bufPrint(&buf, "  {d}:{d}: {s}\n", .{
                parse_err.span.line,
                parse_err.span.column,
                parse_err.message,
            }) catch continue;
            try getStdErr().writeAll(err_msg);
        }
        allocator.free(source);
        return error.ParseError;
    };

    mod.module_ast = module_ast;
    mod.state = .parsed;
    return source;
}

/// Discover all modules imported by a module.
fn discoverImports(
    resolver: *ModuleResolver,
    mod: *ModuleInfo,
) !void {
    if (mod.module_ast == null) return;

    for (mod.module_ast.?.imports) |import_decl| {
        _ = resolver.resolve(import_decl.path, mod) catch |err| {
            // Error already recorded in resolver
            _ = err;
            continue;
        };
    }
}

fn buildNative(allocator: std.mem.Allocator, path: []const u8, options: codegen.CompileOptions) !void {
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

    // Collect all modules to emit (for multi-file compilation)
    var modules_to_emit = std.ArrayListUnmanaged(ast.Module){};
    defer modules_to_emit.deinit(allocator);

    // Track source strings that need to be freed after codegen
    var module_sources = std.ArrayListUnmanaged([]const u8){};
    defer {
        for (module_sources.items) |src| {
            allocator.free(src);
        }
        module_sources.deinit(allocator);
    }

    // Check if this module has imports (multi-file compilation)
    if (module.imports.len > 0) {
        // Set up module resolver for multi-file compilation
        var resolver = ModuleResolver.init(allocator);
        defer resolver.deinit();

        // Try to find standard library
        if (findStdLibPath(allocator)) |std_path| {
            resolver.setStdLibPath(std_path);
            defer allocator.free(std_path);
        }

        // Register entry module
        const entry = resolver.resolveEntry(path) catch {
            try stderr.writeAll("Error: could not resolve entry module\n");
            return;
        };
        entry.module_ast = module;
        entry.state = .parsed;

        // Discover all imported modules (breadth-first)
        var modules_to_process = std.ArrayListUnmanaged(*ModuleInfo){};
        defer modules_to_process.deinit(allocator);
        try modules_to_process.append(allocator, entry);

        var processed_idx: usize = 0;
        while (processed_idx < modules_to_process.items.len) {
            const mod = modules_to_process.items[processed_idx];
            processed_idx += 1;

            // Parse if not already parsed
            if (mod.state == .discovered) {
                const src = parseModuleSource(allocator, arena.allocator(), mod) catch continue;
                try module_sources.append(allocator, src);
            }

            // Discover imports
            if (mod.module_ast) |mod_ast| {
                for (mod_ast.imports) |import_decl| {
                    const dep = resolver.resolve(import_decl.path, mod) catch continue;
                    if (dep.state == .discovered) {
                        try modules_to_process.append(allocator, dep);
                    }
                }
            }
        }

        // Check for resolution errors
        if (resolver.hasErrors()) {
            var buf: [512]u8 = undefined;
            for (resolver.errors.items) |err| {
                const err_msg = std.fmt.bufPrint(&buf, "Module error: {s}\n", .{err.message}) catch continue;
                try stderr.writeAll(err_msg);
            }
            return;
        }

        // Get topological order
        const compilation_order = resolver.getCompilationOrder() catch |err| {
            if (err == error.CircularImport) {
                try stderr.writeAll("Error: circular import detected\n");
            }
            return;
        };
        defer allocator.free(compilation_order);

        // Set up checker with module resolver
        checker.setModuleResolver(&resolver);

        // Type check all modules in order and collect for emission
        for (compilation_order) |mod| {
            if (mod.module_ast) |mod_ast| {
                // Prepare fresh scope for this module (keeps builtins and type registry)
                checker.prepareForNewModule();
                checker.setCurrentModule(mod);
                checker.checkModule(mod_ast);

                // Register exports WHILE in this module's scope (before switching)
                checker.registerModuleExports(mod) catch {};

                // Collect for emission
                try modules_to_emit.append(allocator, mod_ast);
            }
        }
    } else {
        // Single-file compilation (no imports)
        checker.checkModule(module);
        try modules_to_emit.append(allocator, module);
    }

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

    // Initialize LLVM targets
    codegen.llvm.initializeNativeTarget();
    // Also initialize all targets for cross-compilation
    codegen.llvm.initializeAllTargets();

    // Parse target triple if specified
    var target_info: ?codegen.TargetInfo = null;
    defer if (target_info) |ti| ti.deinit(allocator);

    if (options.target_triple) |triple_str| {
        target_info = codegen.TargetInfo.parse(allocator, triple_str) catch |err| {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Invalid target triple '{s}': {s}\n", .{ triple_str, @errorName(err) }) catch "Invalid target triple\n";
            try stderr.writeAll(msg);
            return;
        };

        if (target_info.?.isCrossCompile()) {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Cross-compiling for {s}\n", .{target_info.?.triple}) catch "Cross-compiling\n";
            try stdout.writeAll(msg);
        }
    }

    // Determine base name for output files
    const base_name = std.fs.path.stem(path);
    const module_name = allocator.dupeZ(u8, base_name) catch {
        try stderr.writeAll("Out of memory\n");
        return;
    };
    defer allocator.free(module_name);

    // Emit Klar IR if requested
    if (options.emit_klar_ir) {
        var ir_module = ir.inst.Module.init(arena.allocator(), base_name);
        var lowerer = ir.lower.Lowerer.init(arena.allocator(), &ir_module);
        defer lowerer.deinit();

        lowerer.lowerModule(module) catch |err| {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "IR lowering error: {s}\n", .{@errorName(err)}) catch "IR lowering error\n";
            try stderr.writeAll(msg);
            return;
        };

        // Write IR to file
        const ir_path_str = std.fmt.allocPrint(allocator, "{s}.ir", .{base_name}) catch {
            try stderr.writeAll("Out of memory\n");
            return;
        };
        defer allocator.free(ir_path_str);

        const ir_file = std.fs.cwd().createFile(ir_path_str, .{}) catch |err| {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Failed to create IR file: {s}\n", .{@errorName(err)}) catch "Failed to create IR file\n";
            try stderr.writeAll(msg);
            return;
        };
        defer ir_file.close();

        // Write IR to string first, then to file
        const ir_output = ir.printer.moduleToString(arena.allocator(), &ir_module) catch |err| {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Failed to generate IR: {s}\n", .{@errorName(err)}) catch "Failed to generate IR\n";
            try stderr.writeAll(msg);
            return;
        };
        ir_file.writeAll(ir_output) catch |err| {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Failed to write IR: {s}\n", .{@errorName(err)}) catch "Failed to write IR\n";
            try stderr.writeAll(msg);
            return;
        };

        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Wrote Klar IR to {s}.ir\n", .{base_name}) catch "Wrote Klar IR\n";
        try stdout.writeAll(msg);
    }

    // Create emitter and emit LLVM IR
    var emitter = codegen.Emitter.init(allocator, module_name);
    defer emitter.deinit();

    // Initialize debug info if requested
    if (options.debug_info) {
        if (options.source_path) |source_path_str| {
            // Extract filename and directory from path
            const filename = std.fs.path.basename(source_path_str);
            const directory = std.fs.path.dirname(source_path_str) orelse ".";
            emitter.initDebugInfo(filename, directory);
        }
    }

    // Set the type checker for generic function call resolution
    emitter.setTypeChecker(&checker);

    // Register all struct declarations first so their types are available
    // when declaring monomorphized function signatures (for all modules)
    for (modules_to_emit.items) |mod_to_emit| {
        emitter.registerAllStructDecls(mod_to_emit) catch |err| {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Codegen error (struct registration): {s}\n", .{@errorName(err)}) catch "Codegen error\n";
            try stderr.writeAll(msg);
            return;
        };
    }

    // Register monomorphized struct types BEFORE emitModule
    // so struct literals can find them
    emitter.registerMonomorphizedStructs(&checker) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Codegen error (struct monomorphization): {s}\n", .{@errorName(err)}) catch "Codegen error\n";
        try stderr.writeAll(msg);
        return;
    };

    // Register monomorphized enum types BEFORE emitModule
    // so enum literals can find them
    emitter.registerMonomorphizedEnums(&checker) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Codegen error (enum monomorphization): {s}\n", .{@errorName(err)}) catch "Codegen error\n";
        try stderr.writeAll(msg);
        return;
    };

    // Register non-generic enum types BEFORE emitModule
    // so enum literals can find them
    emitter.registerNonGenericEnums(&checker) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Codegen error (non-generic enum registration): {s}\n", .{@errorName(err)}) catch "Codegen error\n";
        try stderr.writeAll(msg);
        return;
    };

    // Declare monomorphized function signatures BEFORE emitModule
    // so call sites can find them
    emitter.declareMonomorphizedFunctions(&checker) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Codegen error (monomorphization decl): {s}\n", .{@errorName(err)}) catch "Codegen error\n";
        try stderr.writeAll(msg);
        return;
    };

    // Declare monomorphized methods for generic structs BEFORE emitModule
    // so method calls can find them
    emitter.declareMonomorphizedMethods(&checker) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Codegen error (method decl): {s}\n", .{@errorName(err)}) catch "Codegen error\n";
        try stderr.writeAll(msg);
        return;
    };

    // Emit all modules
    for (modules_to_emit.items) |mod_to_emit| {
        emitter.emitModule(mod_to_emit) catch |err| {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Codegen error: {s}\n", .{@errorName(err)}) catch "Codegen error\n";
            try stderr.writeAll(msg);
            return;
        };
    }

    // Emit monomorphized generic function bodies
    emitter.emitMonomorphizedFunctions(&checker) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Codegen error (monomorphization): {s}\n", .{@errorName(err)}) catch "Codegen error\n";
        try stderr.writeAll(msg);
        return;
    };

    // Emit monomorphized method bodies
    emitter.emitMonomorphizedMethods(&checker) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Codegen error (methods): {s}\n", .{@errorName(err)}) catch "Codegen error\n";
        try stderr.writeAll(msg);
        return;
    };

    // Finalize debug info before verification
    emitter.finalizeDebugInfo();

    emitter.verify() catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "LLVM verification failed: {s}\n", .{@errorName(err)}) catch "LLVM verification failed\n";
        try stderr.writeAll(msg);
        return;
    };

    // Emit LLVM IR if requested
    if (options.emit_llvm_ir) {
        const ll_path_str = std.fmt.allocPrint(allocator, "{s}.ll", .{base_name}) catch {
            try stderr.writeAll("Out of memory\n");
            return;
        };
        defer allocator.free(ll_path_str);
        const ll_path = allocator.dupeZ(u8, ll_path_str) catch {
            try stderr.writeAll("Out of memory\n");
            return;
        };
        defer allocator.free(ll_path);

        emitter.getModule().printToFile(ll_path) catch |err| {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Failed to write LLVM IR: {s}\n", .{@errorName(err)}) catch "Failed to write LLVM IR\n";
            try stderr.writeAll(msg);
            return;
        };

        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Wrote LLVM IR to {s}.ll\n", .{base_name}) catch "Wrote LLVM IR\n";
        try stdout.writeAll(msg);
    }

    // Get target info for code generation
    const triple: [:0]const u8 = if (target_info) |ti| ti.triple else codegen.target.getDefaultTriple();
    const target_ref = codegen.llvm.getTargetFromTriple(triple) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Failed to get target for '{s}': {s}\n", .{ triple, @errorName(err) }) catch "Failed to get target\n";
        try stderr.writeAll(msg);
        return;
    };

    // Map Klar optimization level to LLVM codegen level
    const llvm_opt_level: codegen.llvm.c.LLVMCodeGenOptLevel = switch (options.opt_level) {
        .O0 => codegen.llvm.c.LLVMCodeGenLevelNone,
        .O1 => codegen.llvm.c.LLVMCodeGenLevelLess,
        .O2 => codegen.llvm.c.LLVMCodeGenLevelDefault,
        .O3 => codegen.llvm.c.LLVMCodeGenLevelAggressive,
    };

    // Use generic CPU for cross-compilation
    const cpu = if (target_info != null and target_info.?.isCrossCompile())
        "generic"
    else
        codegen.target.getDefaultCPU();

    const tm = codegen.llvm.createTargetMachine(
        target_ref,
        triple,
        cpu,
        codegen.target.getDefaultFeatures(),
        llvm_opt_level,
        codegen.llvm.c.LLVMRelocDefault,
        codegen.llvm.c.LLVMCodeModelDefault,
    ) orelse {
        try stderr.writeAll("Failed to create target machine\n");
        return;
    };
    defer codegen.llvm.disposeTargetMachine(tm);

    // Emit assembly if requested
    if (options.emit_assembly) {
        const asm_path_str = std.fmt.allocPrint(allocator, "{s}.s", .{base_name}) catch {
            try stderr.writeAll("Out of memory\n");
            return;
        };
        defer allocator.free(asm_path_str);
        const asm_path = allocator.dupeZ(u8, asm_path_str) catch {
            try stderr.writeAll("Out of memory\n");
            return;
        };
        defer allocator.free(asm_path);

        codegen.llvm.targetMachineEmitToFile(tm, emitter.getModule(), asm_path, codegen.llvm.c.LLVMAssemblyFile) catch |err| {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Failed to write assembly: {s}\n", .{@errorName(err)}) catch "Failed to write assembly\n";
            try stderr.writeAll(msg);
            return;
        };

        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Wrote assembly to {s}.s\n", .{base_name}) catch "Wrote assembly\n";
        try stdout.writeAll(msg);
    }

    // Generate object file
    const obj_path_str = std.fmt.allocPrint(allocator, "{s}.o", .{base_name}) catch {
        try stderr.writeAll("Out of memory\n");
        return;
    };
    defer allocator.free(obj_path_str);
    const obj_path = allocator.dupeZ(u8, obj_path_str) catch {
        try stderr.writeAll("Out of memory\n");
        return;
    };
    defer allocator.free(obj_path);

    // Print optimization info if verbose
    if (options.verbose_opt and options.opt_level != .O0) {
        var buf2: [256]u8 = undefined;
        const opt_msg = std.fmt.bufPrint(&buf2, "Optimization level: {s}\n", .{options.opt_level.toString()}) catch "Optimization enabled\n";
        try stdout.writeAll(opt_msg);
    }

    codegen.llvm.targetMachineEmitToFile(tm, emitter.getModule(), obj_path, codegen.llvm.c.LLVMObjectFile) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Failed to emit object file: {s}\n", .{@errorName(err)}) catch "Failed to emit object file\n";
        try stderr.writeAll(msg);
        return;
    };

    // Link to create executable (default: build/<base_name>)
    const exe_path = options.output_path orelse blk: {
        // Create build directory if it doesn't exist
        std.fs.cwd().makePath("build") catch |err| {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Failed to create build directory: {s}\n", .{@errorName(err)}) catch "Failed to create build directory\n";
            try stderr.writeAll(msg);
            return;
        };
        break :blk std.fmt.allocPrint(allocator, "build/{s}", .{base_name}) catch {
            try stderr.writeAll("Out of memory\n");
            return;
        };
    };
    const owns_exe_path = options.output_path == null;
    defer if (owns_exe_path) allocator.free(exe_path);

    // Use cross-compilation linker if target specified
    const link_result = if (target_info) |ti|
        codegen.linker.linkForTarget(allocator, obj_path, exe_path, ti)
    else
        codegen.linker.link(allocator, obj_path, exe_path);

    link_result catch |err| {
        var buf: [512]u8 = undefined;
        const err_msg = switch (err) {
            error.CrossCompilationNotSupported => "Cross-compilation not supported for this target. Install the appropriate cross-toolchain.",
            error.LinkerNotFound => "Linker not found. Make sure the appropriate toolchain is installed.",
            error.LinkerFailed => "Linker failed. Check that all required libraries are available.",
            else => @errorName(err),
        };
        const msg = std.fmt.bufPrint(&buf, "Linker error: {s}\n", .{err_msg}) catch "Linker failed\n";
        try stderr.writeAll(msg);
        // Clean up object file
        std.fs.cwd().deleteFile(obj_path) catch {};
        return;
    };

    // Clean up object file (only on success)
    std.fs.cwd().deleteFile(obj_path) catch {};

    if (!options.quiet) {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Built {s}\n", .{exe_path}) catch "Built executable\n";
        try stdout.writeAll(msg);
    }
}

fn runNativeFile(allocator: std.mem.Allocator, path: []const u8) !void {
    // Generate a unique temp path for the executable
    const timestamp = std.time.timestamp();
    var temp_path_buf: [256]u8 = undefined;
    const temp_path = std.fmt.bufPrint(&temp_path_buf, "/tmp/klar-run-{d}", .{timestamp}) catch {
        try getStdErr().writeAll("Failed to create temp path\n");
        return;
    };

    // Build to temp path quietly
    const options = codegen.CompileOptions{
        .output_path = temp_path,
        .source_path = path,
        .quiet = true,
    };

    // Build the executable
    buildNative(allocator, path, options) catch {
        // Errors already printed by buildNative
        return;
    };

    // Execute the compiled binary
    const argv = [_][]const u8{temp_path};
    var child = std.process.Child.init(&argv, allocator);
    child.spawn() catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Failed to execute: {s}\n", .{@errorName(err)}) catch "Failed to execute\n";
        try getStdErr().writeAll(msg);
        // Clean up temp file
        std.fs.cwd().deleteFile(temp_path) catch {};
        return;
    };

    // Wait for completion
    const result = child.wait() catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Failed to wait: {s}\n", .{@errorName(err)}) catch "Failed to wait\n";
        try getStdErr().writeAll(msg);
        // Clean up temp file
        std.fs.cwd().deleteFile(temp_path) catch {};
        return;
    };

    // Clean up temp file
    std.fs.cwd().deleteFile(temp_path) catch {};

    // Exit with the program's exit code
    const exit_code: u8 = switch (result) {
        .Exited => |code| code,
        .Signal => |sig| blk: {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Process terminated by signal: {d}\n", .{sig}) catch "Process terminated by signal\n";
            getStdErr().writeAll(msg) catch {};
            break :blk 128 +| @as(u8, @intCast(@min(sig, 127)));
        },
        .Stopped => |sig| blk: {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Process stopped by signal: {d}\n", .{sig}) catch "Process stopped by signal\n";
            getStdErr().writeAll(msg) catch {};
            break :blk 128 +| @as(u8, @intCast(@min(sig, 127)));
        },
        .Unknown => |val| blk: {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Process terminated with unknown status: {d}\n", .{val}) catch "Process terminated\n";
            getStdErr().writeAll(msg) catch {};
            break :blk 1;
        },
    };
    if (exit_code != 0) {
        std.process.exit(exit_code);
    }
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

fn checkFile(allocator: std.mem.Allocator, path: []const u8, dump_ownership_flag: bool) !void {
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
        return; // Don't proceed to ownership check if type check fails
    }

    // Ownership analysis
    var ownership_checker = ownership.OwnershipChecker.init(allocator);
    defer ownership_checker.deinit();

    ownership_checker.analyze(module) catch |err| {
        const msg = std.fmt.bufPrint(&buf, "Ownership analysis error: {}\n", .{err}) catch "Ownership analysis error\n";
        try stderr.writeAll(msg);
        return;
    };

    // Dump ownership state if requested
    if (dump_ownership_flag) {
        try stdout.writeAll("\n=== Ownership State ===\n");
        try stdout.writeAll("(No detailed state dumped in this version)\n\n");
    }

    // Report ownership errors
    if (ownership_checker.hasErrors()) {
        const header = std.fmt.bufPrint(&buf, "Ownership check failed with {d} error(s):\n", .{ownership_checker.errors.items.len}) catch "Ownership check failed:\n";
        try stderr.writeAll(header);

        for (ownership_checker.errors.items) |own_err| {
            const err_msg = std.fmt.bufPrint(&buf, "  {d}:{d} [{s}]: {s}\n", .{
                own_err.span.line,
                own_err.span.column,
                @tagName(own_err.kind),
                own_err.message,
            }) catch continue;
            try stderr.writeAll(err_msg);
        }
    } else {
        const msg = std.fmt.bufPrint(&buf, "All checks passed for '{s}'\n", .{path}) catch "All checks passed\n";
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
        \\  run <file>           Run a Klar program (native compilation)
        \\  repl                 Interactive REPL (Read-Eval-Print Loop)
        \\  tokenize <file>      Tokenize a file (lexer output)
        \\  parse <file>         Parse a file (AST output)
        \\  check <file>         Type check a file
        \\  disasm <file>        Disassemble bytecode
        \\  build <file>         Build native executable
        \\  test                 Run tests
        \\  fmt                  Format source files
        \\  help                 Show this help
        \\  version              Show version
        \\
        \\Build Options:
        \\  -o <name>            Output file name
        \\  --target <triple>    Cross-compile for target (e.g., x86_64-linux-gnu)
        \\  -O0                  No optimizations (default)
        \\  -O1                  Basic optimizations (constant folding, DCE)
        \\  -O2                  Standard optimizations (O1 + simplification)
        \\  -O3                  Aggressive optimizations (O2 + LLVM aggressive)
        \\  -g                   Generate debug information (DWARF)
        \\  --emit-llvm          Output LLVM IR (.ll file)
        \\  --emit-asm           Output assembly (.s file)
        \\  --emit-ir            Output Klar IR (.ir file)
        \\  --verbose-opt        Show optimization statistics
        \\
        \\Run Options:
        \\  --vm                 Use bytecode VM instead of native
        \\  --debug              Enable instruction tracing (VM only)
        \\  --interpret          Use tree-walking interpreter (VM only)
        \\
        \\Target Triples:
        \\  x86_64-linux-gnu     Linux on x86_64
        \\  aarch64-linux-gnu    Linux on ARM64
        \\  x86_64-apple-macosx  macOS on Intel
        \\  arm64-apple-macosx   macOS on Apple Silicon
        \\  x86_64-windows-msvc  Windows on x86_64
        \\
        \\Examples:
        \\  klar run hello.kl
        \\  klar run hello.kl --debug
        \\  klar build hello.kl
        \\  klar build hello.kl -o myapp
        \\  klar build hello.kl -O2
        \\  klar build hello.kl --target x86_64-linux-gnu
        \\  klar build hello.kl --emit-llvm
        \\  klar build hello.kl --emit-ir
        \\  klar check example.kl
        \\
    );
}

fn printVersion() !void {
    try getStdOut().writeAll(version.display ++ "\n");
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
    _ = @import("codegen/mod.zig");
    _ = @import("gc.zig");
    _ = @import("disasm.zig");
    _ = @import("ir/mod.zig");
    _ = @import("opt/mod.zig");
    _ = @import("repl.zig");
}
