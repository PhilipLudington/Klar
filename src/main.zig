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
const manifest = @import("pkg/manifest.zig");

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
        // Parse flags and collect program arguments
        // Format: klar run [file.kl] [--vm] [--debug] [--interpret] [-- program_args...]
        // Or: klar run [file.kl] [flags] arg1 arg2 (non-flag args go to program)
        // If no file argument, look for klar.json in current directory
        var use_vm = false;
        var debug_mode = false;
        var use_interpreter = false;
        var source_file: ?[]const u8 = null;
        var program_args_start: usize = args.len; // Default: no program args

        var i: usize = 2;
        while (i < args.len) : (i += 1) {
            const arg = args[i];
            if (std.mem.eql(u8, arg, "--")) {
                // Everything after -- goes to the program
                program_args_start = i + 1;
                break;
            } else if (std.mem.eql(u8, arg, "--vm")) {
                use_vm = true;
            } else if (std.mem.eql(u8, arg, "--debug")) {
                debug_mode = true;
            } else if (std.mem.eql(u8, arg, "--interpret")) {
                use_interpreter = true;
            } else if (!std.mem.startsWith(u8, arg, "-") and source_file == null) {
                // First non-flag argument is the source file
                source_file = arg;
            } else if (!std.mem.startsWith(u8, arg, "-")) {
                // Subsequent non-flag arguments go to the program
                program_args_start = i;
                break;
            }
        }
        const program_args = if (program_args_start < args.len) args[program_args_start..] else &[_][]const u8{};

        // Determine source file: explicit argument or from klar.json
        var loaded_manifest: ?manifest.Manifest = null;
        var entry_path_buf: ?[]const u8 = null;
        defer if (entry_path_buf) |p| allocator.free(p);
        defer if (loaded_manifest) |*m| m.deinit();

        const final_source = if (source_file) |sf| sf else blk: {
            // Try to load klar.json from current directory
            loaded_manifest = manifest.loadManifest(allocator, "klar.json") catch |err| switch (err) {
                error.FileNotFound => {
                    try getStdErr().writeAll("Error: no input file and no klar.json found\n");
                    try getStdErr().writeAll("Usage: klar run <file.kl> or run from a project directory\n");
                    return;
                },
                error.InvalidJson => {
                    try getStdErr().writeAll("Error: invalid JSON in klar.json\n");
                    return;
                },
                error.MissingPackageSection => {
                    try getStdErr().writeAll("Error: klar.json missing 'package' section\n");
                    return;
                },
                error.MissingPackageName => {
                    try getStdErr().writeAll("Error: klar.json missing 'package.name'\n");
                    return;
                },
                else => {
                    try getStdErr().writeAll("Error: failed to load klar.json\n");
                    return;
                },
            };

            entry_path_buf = loaded_manifest.?.getEntryPath(allocator) catch {
                try getStdErr().writeAll("Error: failed to resolve entry path\n");
                return;
            };
            break :blk entry_path_buf.?;
        };

        // Resolve dependency paths for module search
        var dep_paths = std.ArrayListUnmanaged([]const u8){};
        defer {
            for (dep_paths.items) |p| allocator.free(p);
            dep_paths.deinit(allocator);
        }

        if (loaded_manifest) |m| {
            var dep_it = m.dependencies.iterator();
            while (dep_it.next()) |entry| {
                const dep = entry.value_ptr.*;
                if (dep.path) |rel_path| {
                    // Resolve path relative to manifest directory
                    const abs_path = std.fs.path.join(allocator, &.{ m.root_dir, rel_path }) catch continue;

                    // Resolve to canonical path
                    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
                    if (std.fs.cwd().realpath(abs_path, &path_buf)) |resolved| {
                        allocator.free(abs_path);
                        dep_paths.append(allocator, allocator.dupe(u8, resolved) catch continue) catch continue;
                    } else |_| {
                        dep_paths.append(allocator, abs_path) catch {
                            allocator.free(abs_path);
                            continue;
                        };
                    }
                }
            }
        }

        if (use_vm or use_interpreter) {
            if (use_interpreter) {
                try runInterpreterFile(allocator, final_source, program_args);
            } else {
                try runVmFile(allocator, final_source, debug_mode, program_args);
            }
        } else {
            // Default: compile to native and run
            try runNativeFileWithOptions(allocator, final_source, program_args, dep_paths.items);
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
        // Parse options - need to determine if first arg after "build" is a file or a flag
        var output_path: ?[]const u8 = null;
        var emit_llvm = false;
        var emit_asm = false;
        var emit_ir = false;
        var opt_level: opt.OptLevel = .O0;
        var verbose_opt = false;
        var debug_info = false;
        var target_triple: ?[]const u8 = null;
        var link_libs = std.ArrayListUnmanaged([]const u8){};
        defer link_libs.deinit(allocator);
        var link_paths = std.ArrayListUnmanaged([]const u8){};
        defer link_paths.deinit(allocator);
        var freestanding = false;
        var entry_point: ?[]const u8 = null;
        var linker_script: ?[]const u8 = null;
        var compile_only = false;
        var source_file: ?[]const u8 = null;

        var i: usize = 2;
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
            } else if (std.mem.eql(u8, arg, "-l") and i + 1 < args.len) {
                // -l libname (separate argument)
                try link_libs.append(allocator, args[i + 1]);
                i += 1;
            } else if (std.mem.startsWith(u8, arg, "-l")) {
                // -llibname (combined)
                try link_libs.append(allocator, arg[2..]);
            } else if (std.mem.eql(u8, arg, "-L") and i + 1 < args.len) {
                // -L path (separate argument)
                try link_paths.append(allocator, args[i + 1]);
                i += 1;
            } else if (std.mem.startsWith(u8, arg, "-L")) {
                // -Lpath (combined)
                try link_paths.append(allocator, arg[2..]);
            } else if (std.mem.eql(u8, arg, "--freestanding")) {
                freestanding = true;
            } else if (std.mem.eql(u8, arg, "--entry") and i + 1 < args.len) {
                entry_point = args[i + 1];
                i += 1;
            } else if (std.mem.startsWith(u8, arg, "--entry=")) {
                entry_point = arg["--entry=".len..];
            } else if (std.mem.eql(u8, arg, "-T") and i + 1 < args.len) {
                linker_script = args[i + 1];
                i += 1;
            } else if (std.mem.eql(u8, arg, "--linker-script") and i + 1 < args.len) {
                linker_script = args[i + 1];
                i += 1;
            } else if (std.mem.startsWith(u8, arg, "--linker-script=")) {
                linker_script = arg["--linker-script=".len..];
            } else if (std.mem.eql(u8, arg, "-c")) {
                compile_only = true;
            } else if (!std.mem.startsWith(u8, arg, "-") and source_file == null) {
                // Non-flag argument is the source file
                source_file = arg;
            }
        }

        // Determine source file: explicit argument or from klar.json
        var loaded_manifest: ?manifest.Manifest = null;
        var entry_path_buf: ?[]const u8 = null;
        defer if (entry_path_buf) |p| allocator.free(p);
        defer if (loaded_manifest) |*m| m.deinit();

        const final_source = if (source_file) |sf| sf else blk: {
            // Try to load klar.json from current directory
            loaded_manifest = manifest.loadManifest(allocator, "klar.json") catch |err| switch (err) {
                error.FileNotFound => {
                    try getStdErr().writeAll("Error: no input file and no klar.json found\n");
                    try getStdErr().writeAll("Usage: klar build <file.kl> or run from a project directory\n");
                    return;
                },
                error.InvalidJson => {
                    try getStdErr().writeAll("Error: invalid JSON in klar.json\n");
                    return;
                },
                error.MissingPackageSection => {
                    try getStdErr().writeAll("Error: klar.json missing 'package' section\n");
                    return;
                },
                error.MissingPackageName => {
                    try getStdErr().writeAll("Error: klar.json missing 'package.name'\n");
                    return;
                },
                else => {
                    try getStdErr().writeAll("Error: failed to load klar.json\n");
                    return;
                },
            };

            entry_path_buf = loaded_manifest.?.getEntryPath(allocator) catch {
                try getStdErr().writeAll("Error: failed to resolve entry path\n");
                return;
            };
            break :blk entry_path_buf.?;
        };

        // Warn if --entry is used without --freestanding
        if (entry_point != null and !freestanding) {
            try getStdErr().writeAll("Warning: --entry has no effect without --freestanding\n");
        }

        // Use project name as output if building from manifest and no -o specified
        // Put in build/ directory for project builds
        var project_output_buf: ?[]const u8 = null;
        defer if (project_output_buf) |p| allocator.free(p);
        const final_output = if (output_path) |o| o else if (loaded_manifest) |m| blk: {
            project_output_buf = std.fmt.allocPrint(allocator, "build/{s}", .{m.package.name}) catch null;
            break :blk project_output_buf;
        } else null;

        // Resolve dependency paths for module search
        var dep_paths = std.ArrayListUnmanaged([]const u8){};
        defer {
            for (dep_paths.items) |p| allocator.free(p);
            dep_paths.deinit(allocator);
        }

        if (loaded_manifest) |m| {
            var dep_it = m.dependencies.iterator();
            while (dep_it.next()) |entry| {
                const dep = entry.value_ptr.*;
                if (dep.path) |rel_path| {
                    // Resolve path relative to manifest directory
                    const abs_path = std.fs.path.join(allocator, &.{ m.root_dir, rel_path }) catch continue;

                    // Resolve to canonical path
                    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
                    if (std.fs.cwd().realpath(abs_path, &path_buf)) |resolved| {
                        allocator.free(abs_path);
                        dep_paths.append(allocator, allocator.dupe(u8, resolved) catch continue) catch continue;
                    } else |_| {
                        // Path doesn't exist, but still add it (will error during compilation)
                        dep_paths.append(allocator, abs_path) catch {
                            allocator.free(abs_path);
                            continue;
                        };
                    }
                }
            }
        }

        try buildNative(allocator, final_source, .{
            .output_path = final_output,
            .emit_llvm_ir = emit_llvm,
            .emit_assembly = emit_asm,
            .emit_klar_ir = emit_ir,
            .opt_level = opt_level,
            .verbose_opt = verbose_opt,
            .debug_info = debug_info,
            .source_path = final_source,
            .target_triple = target_triple,
            .link_libs = link_libs.items,
            .link_paths = link_paths.items,
            .freestanding = freestanding,
            .entry_point = entry_point,
            .linker_script = linker_script,
            .compile_only = compile_only,
            .search_paths = dep_paths.items,
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
    } else if (std.mem.eql(u8, command, "init")) {
        // Parse init options
        var is_lib = false;
        var project_name: ?[]const u8 = null;

        var i: usize = 2;
        while (i < args.len) : (i += 1) {
            const arg = args[i];
            if (std.mem.eql(u8, arg, "--lib")) {
                is_lib = true;
            } else if (!std.mem.startsWith(u8, arg, "-")) {
                project_name = arg;
            }
        }

        try initProject(allocator, project_name, is_lib);
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

fn runInterpreterFile(allocator: std.mem.Allocator, path: []const u8, program_args: []const []const u8) !void {
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

    // Track source strings for imported modules
    var module_sources = std.ArrayListUnmanaged([]const u8){};
    defer {
        for (module_sources.items) |src| {
            allocator.free(src);
        }
        module_sources.deinit(allocator);
    }

    // Check if this module has imports (multi-file compilation)
    if (module.imports.len > 0) {
        // Set up module resolver for multi-file checking
        var resolver = ModuleResolver.init(allocator);
        defer resolver.deinit();

        // Try to find standard library
        if (findStdLibPath(allocator)) |std_path| {
            resolver.setStdLibPath(std_path);
            defer allocator.free(std_path);
        }

        // Add current working directory as a search path
        var cwd_buf: [std.fs.max_path_bytes]u8 = undefined;
        if (std.fs.cwd().realpath(".", &cwd_buf)) |cwd_path| {
            resolver.addSearchPath(cwd_path) catch {};
        } else |_| {}

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

        // Type check all modules in order
        for (compilation_order) |mod| {
            if (mod.module_ast) |mod_ast| {
                checker.prepareForNewModule();
                checker.setCurrentModule(mod);
                checker.checkModule(mod_ast);
                checker.registerModuleExports(mod) catch {};
            }
        }
    } else {
        // Single-file compilation (no imports)
        checker.checkModule(module);
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
            const func = main_val.function;

            // Check if main takes args
            if (func.params.len > 0) {
                // Build args array: [path, program_args...]
                // Use arena allocator - will be cleaned up when function exits
                const arena_alloc = arena.allocator();

                // First, create string Values for each arg
                var string_values = std.ArrayListUnmanaged(values.Value){};
                string_values.ensureTotalCapacity(arena_alloc, program_args.len + 1) catch {
                    try stderr.writeAll("Failed to allocate args array\n");
                    return;
                };

                // Add the source file path as first arg (like argv[1] in native)
                string_values.appendAssumeCapacity(.{ .string = path });

                // Add program args
                for (program_args) |arg| {
                    string_values.appendAssumeCapacity(.{ .string = arg });
                }

                // Create ArrayValue
                const arr = arena_alloc.create(values.ArrayValue) catch {
                    try stderr.writeAll("Failed to allocate args array\n");
                    return;
                };
                arr.* = .{ .elements = string_values.items };

                const args_value = values.Value{ .array = arr };

                _ = interp.callFunction(func, &.{args_value}) catch |err| {
                    var buf: [512]u8 = undefined;
                    const msg = std.fmt.bufPrint(&buf, "Runtime error in main: {s}\n", .{@errorName(err)}) catch "Runtime error in main\n";
                    try stderr.writeAll(msg);
                };
            } else {
                // main() with no args
                _ = interp.callFunction(func, &.{}) catch |err| {
                    var buf: [512]u8 = undefined;
                    const msg = std.fmt.bufPrint(&buf, "Runtime error in main: {s}\n", .{@errorName(err)}) catch "Runtime error in main\n";
                    try stderr.writeAll(msg);
                };
            }
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

        // Add current working directory as a search path
        // This allows imports to resolve relative to where the command is run,
        // enabling test files in subdirectories to import from sibling directories
        var cwd_buf: [std.fs.max_path_bytes]u8 = undefined;
        if (std.fs.cwd().realpath(".", &cwd_buf)) |cwd_path| {
            resolver.addSearchPath(cwd_path) catch {};
        } else |_| {}

        // Add additional search paths (e.g., from package dependencies)
        for (options.search_paths) |search_path| {
            resolver.addSearchPath(search_path) catch {};
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

    // Set freestanding mode if requested
    if (options.freestanding) {
        emitter.setFreestanding(true);
        emitter.setEntryPoint(options.entry_point);
    }

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

    // If compile only (-c), don't link - just keep the object file
    if (options.compile_only) {
        // Optionally rename object file to output path
        if (options.output_path) |out_path| {
            std.fs.cwd().rename(obj_path, out_path) catch |err| {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Failed to rename object file: {s}\n", .{@errorName(err)}) catch "Failed to rename object file\n";
                try stderr.writeAll(msg);
                return;
            };
            if (!options.quiet) {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Compiled {s}\n", .{out_path}) catch "Compiled object file\n";
                try stdout.writeAll(msg);
            }
        } else {
            if (!options.quiet) {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Compiled {s}\n", .{obj_path_str}) catch "Compiled object file\n";
                try stdout.writeAll(msg);
            }
        }
        return;
    }

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

    // Create parent directory if it doesn't exist (for explicit -o path)
    if (options.output_path != null) {
        if (std.fs.path.dirname(exe_path)) |parent_dir| {
            std.fs.cwd().makePath(parent_dir) catch |err| {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Failed to create output directory: {s}\n", .{@errorName(err)}) catch "Failed to create output directory\n";
                try stderr.writeAll(msg);
                return;
            };
        }
    }

    // Use cross-compilation linker if target specified
    const linker_options = codegen.linker.LinkerOptions{
        .link_libs = options.link_libs,
        .link_paths = options.link_paths,
        .linker_script = options.linker_script,
        .freestanding = options.freestanding,
    };
    const link_result = if (target_info) |ti|
        codegen.linker.linkForTargetWithOptions(allocator, obj_path, exe_path, ti, linker_options)
    else
        codegen.linker.linkWithOptions(allocator, obj_path, exe_path, linker_options);

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

fn runNativeFile(allocator: std.mem.Allocator, path: []const u8, program_args: []const []const u8) !void {
    return runNativeFileWithOptions(allocator, path, program_args, &.{});
}

fn runNativeFileWithOptions(allocator: std.mem.Allocator, path: []const u8, program_args: []const []const u8, search_paths: []const []const u8) !void {
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
        .search_paths = search_paths,
    };

    // Build the executable
    buildNative(allocator, path, options) catch {
        // Errors already printed by buildNative
        return;
    };

    // Execute the compiled binary with args
    // We need argv = [source_path, arg1, arg2, ...] but execute temp_path.
    // The Klar runtime includes all of argv, so program sees [source_path, arg1, arg2, ...].
    // For standalone binaries, argv = [binary_path, arg1, ...] from the OS.
    // This keeps args[0] as the "program identifier" (source or binary path).
    //
    // Since std.process.Child uses argv[0] as the executable, we need to use
    // fork/exec directly to have a different executable path and argv[0].
    var argv_list = std.ArrayListUnmanaged(?[*:0]const u8){};
    defer argv_list.deinit(allocator);

    // Convert source path to null-terminated
    const path_z = allocator.dupeZ(u8, path) catch {
        try getStdErr().writeAll("Failed to allocate argv\n");
        std.fs.cwd().deleteFile(temp_path) catch {};
        return;
    };
    defer allocator.free(path_z);
    argv_list.append(allocator, path_z) catch {
        try getStdErr().writeAll("Failed to allocate argv\n");
        std.fs.cwd().deleteFile(temp_path) catch {};
        return;
    };

    // Convert user-provided arguments to null-terminated
    var arg_bufs = std.ArrayListUnmanaged([:0]const u8){};
    defer {
        for (arg_bufs.items) |buf| allocator.free(buf);
        arg_bufs.deinit(allocator);
    }
    for (program_args) |arg| {
        const arg_z = allocator.dupeZ(u8, arg) catch {
            try getStdErr().writeAll("Failed to allocate argv\n");
            std.fs.cwd().deleteFile(temp_path) catch {};
            return;
        };
        arg_bufs.append(allocator, arg_z) catch {
            allocator.free(arg_z);
            try getStdErr().writeAll("Failed to allocate argv\n");
            std.fs.cwd().deleteFile(temp_path) catch {};
            return;
        };
        argv_list.append(allocator, arg_z) catch {
            try getStdErr().writeAll("Failed to allocate argv\n");
            std.fs.cwd().deleteFile(temp_path) catch {};
            return;
        };
    }
    // Null terminator for argv array
    argv_list.append(allocator, null) catch {
        try getStdErr().writeAll("Failed to allocate argv\n");
        std.fs.cwd().deleteFile(temp_path) catch {};
        return;
    };

    // Convert temp_path to null-terminated for execve
    const temp_path_z = allocator.dupeZ(u8, temp_path) catch {
        try getStdErr().writeAll("Failed to allocate temp path\n");
        std.fs.cwd().deleteFile(temp_path) catch {};
        return;
    };
    defer allocator.free(temp_path_z);

    // Fork and exec
    const pid = std.posix.fork() catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Failed to fork: {s}\n", .{@errorName(err)}) catch "Failed to fork\n";
        try getStdErr().writeAll(msg);
        std.fs.cwd().deleteFile(temp_path) catch {};
        return;
    };

    if (pid == 0) {
        // Child process - exec the binary
        const argv_ptr: [*:null]const ?[*:0]const u8 = @ptrCast(argv_list.items.ptr);
        const envp = std.c.environ;
        // execveZ doesn't return on success - if it does, it's an error
        std.posix.execveZ(temp_path_z, argv_ptr, envp) catch {};
        std.posix.exit(127);
    }

    // Parent process - wait for child
    const result = std.posix.waitpid(pid, 0);

    // Clean up temp file
    std.fs.cwd().deleteFile(temp_path) catch {};

    // Decode the wait status using POSIX macros
    const status = result.status;
    // WIFEXITED: (status & 0x7F) == 0
    if ((status & 0x7F) == 0) {
        // Exited normally - WEXITSTATUS: (status >> 8) & 0xFF
        const exit_code: u8 = @truncate((status >> 8) & 0xFF);
        std.posix.exit(exit_code);
    }
    // WIFSIGNALED: ((status & 0x7F) + 1) >> 1 > 0
    if (((status & 0x7F) + 1) >> 1 > 0) {
        // Killed by signal - WTERMSIG: status & 0x7F
        const sig: u8 = @truncate(status & 0x7F);
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Process terminated by signal: {d}\n", .{sig}) catch "Process terminated by signal\n";
        getStdErr().writeAll(msg) catch {};
        std.posix.exit(128 +| sig);
    }
    // Unknown status
    std.posix.exit(1);
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

    // Track source strings for imported modules
    var module_sources = std.ArrayListUnmanaged([]const u8){};
    defer {
        for (module_sources.items) |src| {
            allocator.free(src);
        }
        module_sources.deinit(allocator);
    }

    // Check if this module has imports (multi-file compilation)
    if (module.imports.len > 0) {
        // Set up module resolver for multi-file checking
        var resolver = ModuleResolver.init(allocator);
        defer resolver.deinit();

        // Try to find standard library
        if (findStdLibPath(allocator)) |std_path| {
            resolver.setStdLibPath(std_path);
            defer allocator.free(std_path);
        }

        // Add current working directory as a search path
        var cwd_buf: [std.fs.max_path_bytes]u8 = undefined;
        if (std.fs.cwd().realpath(".", &cwd_buf)) |cwd_path| {
            resolver.addSearchPath(cwd_path) catch {};
        } else |_| {}

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

        // Type check all modules in order
        for (compilation_order) |mod| {
            if (mod.module_ast) |mod_ast| {
                checker.prepareForNewModule();
                checker.setCurrentModule(mod);
                checker.checkModule(mod_ast);
                checker.registerModuleExports(mod) catch {};
            }
        }
    } else {
        // Single-file compilation (no imports)
        checker.checkModule(module);
    }

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

fn runVmFile(allocator: std.mem.Allocator, path: []const u8, debug_mode: bool, program_args: []const []const u8) !void {
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

    // Track source strings for imported modules
    var module_sources = std.ArrayListUnmanaged([]const u8){};
    defer {
        for (module_sources.items) |src| {
            allocator.free(src);
        }
        module_sources.deinit(allocator);
    }

    // Check if this module has imports (multi-file compilation)
    if (module.imports.len > 0) {
        // Set up module resolver for multi-file checking
        var resolver = ModuleResolver.init(allocator);
        defer resolver.deinit();

        // Try to find standard library
        if (findStdLibPath(allocator)) |std_path| {
            resolver.setStdLibPath(std_path);
            defer allocator.free(std_path);
        }

        // Add current working directory as a search path
        var cwd_buf: [std.fs.max_path_bytes]u8 = undefined;
        if (std.fs.cwd().realpath(".", &cwd_buf)) |cwd_path| {
            resolver.addSearchPath(cwd_path) catch {};
        } else |_| {}

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

        // Type check all modules in order
        for (compilation_order) |mod| {
            if (mod.module_ast) |mod_ast| {
                checker.prepareForNewModule();
                checker.setCurrentModule(mod);
                checker.checkModule(mod_ast);
                checker.registerModuleExports(mod) catch {};
            }
        }
    } else {
        // Single-file compilation (no imports)
        checker.checkModule(module);
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
            // Build full args: [path] + program_args (matching native/interpreter behavior)
            var full_args = std.ArrayListUnmanaged([]const u8){};
            defer full_args.deinit(allocator);
            full_args.append(allocator, path) catch {
                try stderr.writeAll("Failed to allocate args\n");
                return;
            };
            for (program_args) |arg| {
                full_args.append(allocator, arg) catch {
                    try stderr.writeAll("Failed to allocate args\n");
                    return;
                };
            }

            _ = vm.callMain(main_val.closure, full_args.items) catch |err| {
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

fn initProject(allocator: std.mem.Allocator, name_arg: ?[]const u8, is_lib: bool) !void {
    const stdout = getStdOut();
    const stderr = getStdErr();
    const cwd = std.fs.cwd();

    // Determine project name: use argument, or current directory name
    const project_name = if (name_arg) |n| n else blk: {
        var path_buf: [std.fs.max_path_bytes]u8 = undefined;
        const cwd_path = cwd.realpath(".", &path_buf) catch {
            try stderr.writeAll("Error: could not determine current directory\n");
            return;
        };
        break :blk std.fs.path.basename(cwd_path);
    };

    // Check if klar.json already exists
    if (cwd.access("klar.json", .{})) |_| {
        try stderr.writeAll("Error: klar.json already exists in this directory\n");
        return;
    } else |_| {}

    // Generate manifest content
    const manifest_content = manifest.generateDefault(allocator, project_name, is_lib) catch {
        try stderr.writeAll("Error: failed to generate manifest\n");
        return;
    };
    defer allocator.free(manifest_content);

    // Create src/ directory if it doesn't exist
    cwd.makeDir("src") catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => {
            try stderr.writeAll("Error: failed to create src/ directory\n");
            return;
        },
    };

    // Create the entry point file
    const entry_file = if (is_lib) "src/lib.kl" else "src/main.kl";
    const entry_content = if (is_lib)
        \\// Library entry point
        \\
        \\pub fn greet(name: string) -> string {
        \\    return "Hello, " + name + "!"
        \\}
        \\
    else
        \\// Main entry point
        \\
        \\fn main(args: [String]) -> i32 {
        \\    println("Hello, Klar!")
        \\    return 0
        \\}
        \\
    ;

    // Only create entry file if it doesn't exist
    if (cwd.access(entry_file, .{})) |_| {
        // File exists, don't overwrite
    } else |_| {
        const entry = cwd.createFile(entry_file, .{}) catch {
            try stderr.writeAll("Error: failed to create entry point file\n");
            return;
        };
        defer entry.close();
        entry.writeAll(entry_content) catch {
            try stderr.writeAll("Error: failed to write entry point file\n");
            return;
        };
    }

    // Write klar.json
    const manifest_file = cwd.createFile("klar.json", .{}) catch {
        try stderr.writeAll("Error: failed to create klar.json\n");
        return;
    };
    defer manifest_file.close();
    manifest_file.writeAll(manifest_content) catch {
        try stderr.writeAll("Error: failed to write klar.json\n");
        return;
    };

    // Success message
    var buf: [512]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, "Created Klar {s} '{s}'\n  klar.json\n  {s}\n", .{
        if (is_lib) "library" else "project",
        project_name,
        entry_file,
    }) catch "Project created\n";
    try stdout.writeAll(msg);
}

fn printUsage() !void {
    try getStdOut().writeAll(
        \\
        \\  Klar - No ambiguity. No surprises.
        \\
        \\Usage: klar <command> [options]
        \\
        \\Commands:
        \\  init [name]          Create a new Klar project
        \\  build [file]         Build project or file to native executable
        \\  run [file]           Build and run project or file
        \\  check <file>         Type check a file
        \\  repl                 Interactive REPL (Read-Eval-Print Loop)
        \\  test                 Run tests (not implemented)
        \\  fmt                  Format source files (not implemented)
        \\  help                 Show this help
        \\  version              Show version
        \\  tokenize <file>      Tokenize a file (debug)
        \\  parse <file>         Parse a file (debug)
        \\  disasm <file>        Disassemble bytecode (debug)
        \\
        \\Build Options:
        \\  -o <name>            Output file name
        \\  -c                   Compile only (produce object file, don't link)
        \\  --target <triple>    Cross-compile for target (e.g., x86_64-linux-gnu)
        \\  -O0                  No optimizations (default)
        \\  -O1                  Basic optimizations (constant folding, DCE)
        \\  -O2                  Standard optimizations (O1 + simplification)
        \\  -O3                  Aggressive optimizations (O2 + LLVM aggressive)
        \\  -g                   Generate debug information (DWARF)
        \\  -l <lib>             Link with library (e.g., -lm, -lcurl)
        \\  -L <path>            Add library search path
        \\  --emit-llvm          Output LLVM IR (.ll file)
        \\  --emit-asm           Output assembly (.s file)
        \\  --emit-ir            Output Klar IR (.ir file)
        \\  --verbose-opt        Show optimization statistics
        \\
        \\Bare-Metal Options:
        \\  --freestanding       Compile without standard library (no libc)
        \\  --entry <symbol>     Set entry point symbol (default: main)
        \\  -T <path>            Use custom linker script
        \\  --linker-script <path>  Same as -T
        \\
        \\Run Options:
        \\  --vm                 Use bytecode VM instead of native
        \\  --debug              Enable instruction tracing (VM only)
        \\  --interpret          Use tree-walking interpreter (VM only)
        \\
        \\Init Options:
        \\  --lib                Create a library project (src/lib.kl instead of src/main.kl)
        \\
        \\Target Triples:
        \\  x86_64-linux-gnu     Linux on x86_64
        \\  aarch64-linux-gnu    Linux on ARM64
        \\  x86_64-apple-macosx  macOS on Intel
        \\  arm64-apple-macosx   macOS on Apple Silicon
        \\  x86_64-windows-msvc  Windows on x86_64
        \\  aarch64-none-elf     Bare-metal ARM64 (ELF)
        \\  aarch64-none-eabi    Bare-metal ARM64 (EABI)
        \\
        \\Examples:
        \\  klar init                               Create project in current directory
        \\  klar init my-project                    Create project with specific name
        \\  klar init --lib                         Create library project
        \\  klar build                              Build project (requires klar.json)
        \\  klar run                                Build and run project
        \\  klar run hello.kl                       Run a single file
        \\  klar run hello.kl --debug               Run with debug output
        \\  klar build hello.kl                     Build a single file
        \\  klar build hello.kl -o myapp            Build with custom output name
        \\  klar build hello.kl -O2                 Build with optimizations
        \\  klar build hello.kl --target x86_64-linux-gnu  Cross-compile
        \\  klar build hello.kl --emit-llvm         Emit LLVM IR
        \\  klar build hello.kl -lm -L/usr/local/lib  Link libraries
        \\  klar check example.kl                   Type-check a file
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
    _ = @import("pkg/manifest.zig");
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
