const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const has_llvm = build_options.has_llvm;
const version = @import("version.zig");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;
const Parser = @import("parser.zig").Parser;
const ast = @import("ast.zig");
const types = @import("types.zig");
const checker_mod = @import("checker/mod.zig");
const TypeChecker = checker_mod.TypeChecker;
const interp_mod = @import("interpreter.zig");
const Interpreter = interp_mod.Interpreter;
const AssertionRecorder = interp_mod.AssertionRecorder;
const AssertionRecord = interp_mod.AssertionRecord;
const values = @import("values.zig");
const Compiler = @import("compiler.zig").Compiler;
const Disassembler = @import("disasm.zig").Disassembler;
const VM = @import("vm.zig").VM;
const codegen = if (has_llvm) @import("codegen/mod.zig") else struct {};
const ir = if (has_llvm) @import("ir/mod.zig") else struct {};
const ownership = @import("ownership/mod.zig");
const opt = if (has_llvm) @import("opt/mod.zig") else struct {};
const module_resolver = @import("module_resolver.zig");
const ModuleResolver = module_resolver.ModuleResolver;
const ModuleInfo = module_resolver.ModuleInfo;
const Repl = @import("repl.zig").Repl;
const manifest = @import("pkg/manifest.zig");
const lockfile = @import("pkg/lockfile.zig");
const formatter = @import("formatter.zig");
const lsp = @import("lsp.zig");
const meta_query = @import("meta_query.zig");

// Cross-platform IO helpers
fn getStdOut() std.fs.File {
    if (comptime builtin.os.tag == .windows) {
        const handle = std.os.windows.kernel32.GetStdHandle(std.os.windows.STD_OUTPUT_HANDLE).?;
        return .{ .handle = handle };
    } else {
        return .{ .handle = std.posix.STDOUT_FILENO };
    }
}

fn getStdErr() std.fs.File {
    if (comptime builtin.os.tag == .windows) {
        const handle = std.os.windows.kernel32.GetStdHandle(std.os.windows.STD_ERROR_HANDLE).?;
        return .{ .handle = handle };
    } else {
        return .{ .handle = std.posix.STDERR_FILENO };
    }
}

const TestRunOptions = struct {
    fn_filter: ?[]const u8 = null,
    strict_tests: bool = false,
    require_tests: bool = false,
    json_output: bool = false,
    include_source: bool = false,
    emit_output: bool = true,
};

const CheckCommandOptions = struct {
    dump_ownership: bool = false,
    scope_line: ?usize = null,
    scope_column: ?usize = null,
    scope_json: bool = false,
    partial_mode: bool = false,
    expected_line: ?usize = null,
    expected_column: ?usize = null,
};

const TestFileResult = struct {
    tests_discovered: usize,
    tests_passed: usize,
    tests_failed: usize,
    test_results: []JsonTestSummary = &.{},
    compile_errors: []JsonCompileError = &.{},
    file_failed: bool = false,
};

const JsonCompileError = struct {
    stage: []const u8,
    message: []const u8,
    line: ?usize = null,
    column: ?usize = null,
};

const JsonAssertionSummary = struct {
    assertion_type: []const u8,
    passed: bool,
    expected: ?[]const u8 = null,
    actual: ?[]const u8 = null,
};

const JsonTestSummary = struct {
    name: []const u8,
    passed: bool,
    error_name: ?[]const u8 = null,
    assertions: []JsonAssertionSummary = &.{},
    source: ?[]const u8 = null,
};

const JsonFileSummary = struct {
    path: []const u8,
    total: usize,
    passed: usize,
    failed: usize,
    test_results: []JsonTestSummary = &.{},
    compile_errors: []JsonCompileError = &.{},
};

fn writeJsonEscaped(out: std.fs.File, s: []const u8) !void {
    try out.writeAll("\"");
    for (s) |c| {
        switch (c) {
            '"' => try out.writeAll("\\\""),
            '\\' => try out.writeAll("\\\\"),
            '\n' => try out.writeAll("\\n"),
            '\r' => try out.writeAll("\\r"),
            '\t' => try out.writeAll("\\t"),
            else => {
                if (c < 0x20) {
                    // RFC 8259: control characters U+0000–U+001F must be \uXXXX escaped
                    var esc_buf: [6]u8 = undefined;
                    const esc = std.fmt.bufPrint(&esc_buf, "\\u{x:0>4}", .{@as(u16, c)}) catch unreachable;
                    try out.writeAll(esc);
                } else {
                    try out.writeAll(&[_]u8{c});
                }
            },
        }
    }
    try out.writeAll("\"");
}

fn writeJsonUsize(out: std.fs.File, value: usize) !void {
    var buf: [32]u8 = undefined;
    const n = std.fmt.bufPrint(&buf, "{d}", .{value}) catch unreachable;
    try out.writeAll(n);
}

fn writeJsonBool(out: std.fs.File, value: bool) !void {
    if (value) {
        try out.writeAll("true");
    } else {
        try out.writeAll("false");
    }
}

const LineColumn = struct {
    line: usize,
    column: usize,
};

fn parseLineColumn(value: []const u8) !LineColumn {
    const colon_idx = std.mem.indexOfScalar(u8, value, ':') orelse return error.InvalidScopePosition;
    const line_text = value[0..colon_idx];
    const col_text = value[colon_idx + 1 ..];
    if (line_text.len == 0 or col_text.len == 0) return error.InvalidScopePosition;

    const line = std.fmt.parseInt(usize, line_text, 10) catch return error.InvalidScopePosition;
    const column = std.fmt.parseInt(usize, col_text, 10) catch return error.InvalidScopePosition;
    if (line == 0 or column == 0) return error.InvalidScopePosition;

    return .{ .line = line, .column = column };
}

/// Print meta warnings (deprecation notices, etc.) to stderr.
/// Warnings don't block compilation but inform the user of potential issues.
fn printMetaWarnings(checker: *const TypeChecker, stderr: anytype) !void {
    if (!checker.hasWarnings()) return;
    for (checker.errors.items) |check_err| {
        if (check_err.kind != .meta_warning) continue;
        const warn_msg = std.fmt.allocPrint(checker.allocator, "  warning: {d}:{d} {s}\n", .{
            check_err.span.line,
            check_err.span.column,
            check_err.message,
        }) catch {
            try stderr.writeAll("  warning: (message too long to display)\n");
            continue;
        };
        defer checker.allocator.free(warn_msg);
        try stderr.writeAll(warn_msg);
    }
}

fn sourceOffsetFromLineColumn(source: []const u8, line: usize, column: usize) ?usize {
    var current_line: usize = 1;
    var line_start: usize = 0;
    var i: usize = 0;

    while (i < source.len and current_line < line) : (i += 1) {
        if (source[i] == '\n') {
            current_line += 1;
            line_start = i + 1;
        }
    }

    if (current_line != line) return null;

    var line_end = source.len;
    i = line_start;
    while (i < source.len) : (i += 1) {
        if (source[i] == '\n') {
            line_end = i;
            break;
        }
    }

    const offset = line_start + (column - 1);
    if (offset > line_end) return null;
    return offset;
}

fn symbolKindName(kind: checker_mod.Symbol.Kind) []const u8 {
    return switch (kind) {
        .variable => "variable",
        .parameter => "parameter",
        .function => "function",
        .type_ => "type",
        .trait_ => "trait",
        .module => "module",
        .constant => "constant",
    };
}

fn freeJsonTestResults(allocator: std.mem.Allocator, test_results: []JsonTestSummary) void {
    for (test_results) |test_result| {
        allocator.free(test_result.name);
        if (test_result.error_name) |error_name| allocator.free(error_name);
        if (test_result.source) |source| allocator.free(source);
        for (test_result.assertions) |assertion| {
            if (assertion.expected) |expected| allocator.free(expected);
            if (assertion.actual) |actual| allocator.free(actual);
        }
        allocator.free(test_result.assertions);
    }
    allocator.free(test_results);
}

fn freeJsonCompilerErrors(allocator: std.mem.Allocator, compile_errors: []JsonCompileError) void {
    for (compile_errors) |compile_error| {
        allocator.free(compile_error.message);
    }
    allocator.free(compile_errors);
}

/// Detect circular imports and emit warnings to stderr.
/// Returns true if cycles were found (for callers that want to know).
fn warnCircularImports(resolver: *ModuleResolver, stderr: std.fs.File) bool {
    const cycle = resolver.detectCycle() catch return false;
    if (cycle) |modules| {
        defer resolver.allocator.free(modules);
        var buf: [512]u8 = undefined;
        if (modules.len > 0) {
            const name = modules[0].canonicalName(resolver.allocator) catch "?";
            defer if (!std.mem.eql(u8, name, "?")) resolver.allocator.free(name);
            const msg = std.fmt.bufPrint(&buf, "Warning: circular import detected involving '{s}'\n", .{name}) catch
                "Warning: circular import detected\n";
            stderr.writeAll(msg) catch {};
        } else {
            stderr.writeAll("Warning: circular import detected\n") catch {};
        }
        return true;
    }
    return false;
}

fn makeSingleCompileErrorResult(
    allocator: std.mem.Allocator,
    stage: []const u8,
    message: []const u8,
    line: ?usize,
    column: ?usize,
) !TestFileResult {
    const compile_errors = try allocator.alloc(JsonCompileError, 1);
    errdefer allocator.free(compile_errors);
    compile_errors[0] = .{
        .stage = stage,
        .message = try allocator.dupe(u8, message),
        .line = line,
        .column = column,
    };
    errdefer allocator.free(compile_errors[0].message);
    const empty_test_results = try allocator.alloc(JsonTestSummary, 0);
    return .{
        .tests_discovered = 0,
        .tests_passed = 0,
        .tests_failed = 0,
        .test_results = empty_test_results,
        .compile_errors = compile_errors,
        .file_failed = true,
    };
}

fn duplicateAssertionRecords(
    allocator: std.mem.Allocator,
    assertion_records: []const AssertionRecord,
) ![]JsonAssertionSummary {
    const assertions = try allocator.alloc(JsonAssertionSummary, assertion_records.len);
    var initialized: usize = 0;
    errdefer {
        for (assertions[0..initialized]) |assertion| {
            if (assertion.expected) |expected| allocator.free(expected);
            if (assertion.actual) |actual| allocator.free(actual);
        }
        allocator.free(assertions);
    }

    for (assertion_records, 0..) |record, i| {
        const expected = if (record.expected) |value| try allocator.dupe(u8, value) else null;
        errdefer if (expected) |value| allocator.free(value);
        const actual = if (record.actual) |value| try allocator.dupe(u8, value) else null;
        errdefer if (actual) |value| allocator.free(value);

        assertions[i] = .{
            .assertion_type = record.assertion_type,
            .passed = record.passed,
            .expected = expected,
            .actual = actual,
        };
        initialized += 1;
    }
    return assertions;
}

fn findFunctionSource(module: ast.Module, source: []const u8, function_name: []const u8) ?[]const u8 {
    for (module.declarations) |decl| {
        if (decl != .function) continue;
        const function = decl.function;
        if (!std.mem.eql(u8, function.name, function_name)) continue;
        if (function.span.end > source.len or function.span.start >= function.span.end) return null;
        return source[function.span.start..function.span.end];
    }
    return null;
}

/// Result of attempting to load a manifest with user-friendly error messages.
const ManifestLoadResult = union(enum) {
    success: manifest.Manifest,
    failure,
};

/// Load a manifest file with user-friendly error reporting.
/// Returns the manifest on success, or .failure after printing error to stderr.
fn loadManifestWithErrors(allocator: std.mem.Allocator, path: []const u8, command_hint: []const u8) ManifestLoadResult {
    const stderr = getStdErr();
    return .{
        .success = manifest.loadManifest(allocator, path) catch |err| {
            const msg = switch (err) {
                error.FileNotFound => blk: {
                    stderr.writeAll("Error: no input file and no klar.json found\n") catch {};
                    var buf: [256]u8 = undefined;
                    break :blk std.fmt.bufPrint(&buf, "Usage: klar {s} <file.kl> or run from a project directory\n", .{command_hint}) catch "";
                },
                error.InvalidJson => "Error: invalid JSON in klar.json\n",
                error.MissingPackageSection => "Error: klar.json missing 'package' section\n",
                error.MissingPackageName => "Error: klar.json missing 'package.name'\n",
                else => "Error: failed to load klar.json\n",
            };
            stderr.writeAll(msg) catch {};
            return .failure;
        },
    };
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
            loaded_manifest = switch (loadManifestWithErrors(allocator, "klar.json", "run")) {
                .success => |m| m,
                .failure => return,
            };

            entry_path_buf = loaded_manifest.?.getEntryPath(allocator) catch {
                try getStdErr().writeAll("Error: failed to resolve entry path\n");
                return;
            };
            break :blk entry_path_buf.?;
        };

        // Resolve dependency paths for module search (with lock file support)
        var dep_resolution: ?DependencyResolution = null;
        defer if (dep_resolution) |*dr| dr.deinit();

        if (loaded_manifest) |*m| {
            dep_resolution = resolveDependencies(allocator, m, m.root_dir) catch |err| {
                // Error already printed by resolveDependencies
                if (err == error.LockfileMismatch or err == error.DependencyNotFound) {
                    return;
                }
                return err;
            };
        }

        const search_paths = if (dep_resolution) |dr| dr.paths.items else &[_][]const u8{};

        if (use_vm or use_interpreter) {
            if (use_interpreter) {
                try runInterpreterFile(allocator, final_source, program_args);
            } else {
                try runVmFile(allocator, final_source, debug_mode, program_args);
            }
        } else if (comptime has_llvm) {
            // Default: compile to native and run
            try runNativeFileWithOptions(allocator, final_source, program_args, search_paths);
        } else {
            // LLVM not available, fall back to VM
            try runVmFile(allocator, final_source, debug_mode, program_args);
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
    } else if (std.mem.eql(u8, command, "dump-tokens")) {
        if (args.len < 3) {
            try getStdErr().writeAll("Error: no input file\n");
            return;
        }
        try dumpTokensFile(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "dump-ast")) {
        if (args.len < 3) {
            try getStdErr().writeAll("Error: no input file\n");
            return;
        }
        try dumpAstFile(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "build")) {
        if (comptime !has_llvm) {
            try getStdErr().writeAll("Error: 'klar build' requires LLVM. Install LLVM development headers and rebuild the compiler.\n");
            return;
        }
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
                const raw = args[i + 1];
                // Expand shorthand target names
                if (std.mem.eql(u8, raw, "wasm")) {
                    target_triple = "wasm32-unknown-unknown";
                } else if (std.mem.eql(u8, raw, "wasi")) {
                    target_triple = "wasm32-unknown-wasi";
                } else {
                    target_triple = raw;
                }
                i += 1;
            } else if (std.mem.startsWith(u8, arg, "--target=")) {
                const raw = arg["--target=".len..];
                if (std.mem.eql(u8, raw, "wasm")) {
                    target_triple = "wasm32-unknown-unknown";
                } else if (std.mem.eql(u8, raw, "wasi")) {
                    target_triple = "wasm32-unknown-wasi";
                } else {
                    target_triple = raw;
                }
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
            loaded_manifest = switch (loadManifestWithErrors(allocator, "klar.json", "build")) {
                .success => |m| m,
                .failure => return,
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

        // Resolve dependency paths for module search (with lock file support)
        var dep_resolution: ?DependencyResolution = null;
        defer if (dep_resolution) |*dr| dr.deinit();

        if (loaded_manifest) |*m| {
            dep_resolution = resolveDependencies(allocator, m, m.root_dir) catch |err| {
                // Error already printed by resolveDependencies
                if (err == error.LockfileMismatch or err == error.DependencyNotFound) {
                    return;
                }
                return err;
            };
        }

        const search_paths = if (dep_resolution) |dr| dr.paths.items else &[_][]const u8{};

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
            .search_paths = search_paths,
        });
    } else if (std.mem.eql(u8, command, "check")) {
        var options = CheckCommandOptions{};
        var input_path: ?[]const u8 = null;

        var i: usize = 2;
        while (i < args.len) : (i += 1) {
            const arg = args[i];
            if (std.mem.eql(u8, arg, "--dump-ownership")) {
                options.dump_ownership = true;
            } else if (std.mem.eql(u8, arg, "--scope-at")) {
                if (i + 1 >= args.len) {
                    try getStdErr().writeAll("Error: missing value for --scope-at (expected <line:col>)\n");
                    return;
                }
                const parsed = parseLineColumn(args[i + 1]) catch {
                    try getStdErr().writeAll("Error: invalid --scope-at value, expected <line:col>\n");
                    return;
                };
                options.scope_line = parsed.line;
                options.scope_column = parsed.column;
                i += 1;
            } else if (std.mem.startsWith(u8, arg, "--scope-at=")) {
                const parsed = parseLineColumn(arg["--scope-at=".len..]) catch {
                    try getStdErr().writeAll("Error: invalid --scope-at value, expected <line:col>\n");
                    return;
                };
                options.scope_line = parsed.line;
                options.scope_column = parsed.column;
            } else if (std.mem.eql(u8, arg, "--scope-json")) {
                options.scope_json = true;
            } else if (std.mem.eql(u8, arg, "--partial")) {
                options.partial_mode = true;
            } else if (std.mem.eql(u8, arg, "--expected-type-at")) {
                if (i + 1 >= args.len) {
                    try getStdErr().writeAll("Error: missing value for --expected-type-at (expected <line:col>)\n");
                    return;
                }
                const parsed = parseLineColumn(args[i + 1]) catch {
                    try getStdErr().writeAll("Error: invalid --expected-type-at value, expected <line:col>\n");
                    return;
                };
                options.expected_line = parsed.line;
                options.expected_column = parsed.column;
                i += 1;
            } else if (std.mem.startsWith(u8, arg, "--expected-type-at=")) {
                const parsed = parseLineColumn(arg["--expected-type-at=".len..]) catch {
                    try getStdErr().writeAll("Error: invalid --expected-type-at value, expected <line:col>\n");
                    return;
                };
                options.expected_line = parsed.line;
                options.expected_column = parsed.column;
            } else if (!std.mem.startsWith(u8, arg, "-") and input_path == null) {
                input_path = arg;
            } else {
                var buf: [256]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Error: unknown check option '{s}'\n", .{arg}) catch "Error: unknown check option\n";
                try getStdErr().writeAll(msg);
                return;
            }
        }

        const path = input_path orelse {
            try getStdErr().writeAll("Error: no input file\n");
            return;
        };

        if ((options.scope_line == null) != (options.scope_column == null)) {
            try getStdErr().writeAll("Error: --scope-at requires both line and column\n");
            return;
        }
        if ((options.expected_line == null) != (options.expected_column == null)) {
            try getStdErr().writeAll("Error: --expected-type-at requires both line and column\n");
            return;
        }
        if (options.scope_json and options.scope_line == null) {
            try getStdErr().writeAll("Error: --scope-json requires --scope-at <line:col>\n");
            return;
        }
        if (options.scope_line != null and options.expected_line != null) {
            try getStdErr().writeAll("Error: --scope-at and --expected-type-at cannot be used together\n");
            return;
        }

        try checkFile(allocator, path, options);
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
    } else if (std.mem.eql(u8, command, "update")) {
        try updateLockfile(allocator);
    } else if (std.mem.eql(u8, command, "test")) {
        var input_path: ?[]const u8 = null;
        var fn_filter: ?[]const u8 = null;
        var strict_tests = false;
        var require_tests = false;
        var json_output = false;
        var include_source = false;

        var i: usize = 2;
        while (i < args.len) : (i += 1) {
            const arg = args[i];
            if (std.mem.eql(u8, arg, "--fn")) {
                if (i + 1 >= args.len) {
                    try getStdErr().writeAll("Error: missing value for --fn\n");
                    return;
                }
                fn_filter = args[i + 1];
                i += 1;
            } else if (std.mem.startsWith(u8, arg, "--fn=")) {
                fn_filter = arg["--fn=".len..];
            } else if (std.mem.eql(u8, arg, "--strict-tests")) {
                strict_tests = true;
            } else if (std.mem.eql(u8, arg, "--require-tests")) {
                require_tests = true;
            } else if (std.mem.eql(u8, arg, "--json")) {
                json_output = true;
            } else if (std.mem.eql(u8, arg, "--include-source")) {
                include_source = true;
            } else if (!std.mem.startsWith(u8, arg, "-") and input_path == null) {
                input_path = arg;
            } else {
                var buf: [256]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Error: unknown test option '{s}'\n", .{arg}) catch "Error: unknown test option\n";
                try getStdErr().writeAll(msg);
                return;
            }
        }

        const final_path = input_path orelse {
            try getStdErr().writeAll("Error: no input path\n");
            return;
        };

        runTestPath(allocator, final_path, .{
            .fn_filter = fn_filter,
            .strict_tests = strict_tests,
            .require_tests = require_tests,
            .json_output = json_output,
            .include_source = include_source,
        }) catch |err| switch (err) {
            error.TestsFailed => std.process.exit(1),
            else => return err,
        };
    } else if (std.mem.eql(u8, command, "fmt")) {
        try fmtCommand(allocator, args);
    } else if (std.mem.eql(u8, command, "meta")) {
        try meta_query.metaCommand(allocator, args);
    } else if (std.mem.eql(u8, command, "lsp")) {
        try lsp.runStdio(allocator);
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

/// Execute the main() function on an interpreter that has already had its module executed.
/// Used by both single-module and multi-module interpreter paths.
fn executeMainFunction(
    interp: *Interpreter,
    path: []const u8,
    program_args: []const []const u8,
    arena_alloc: std.mem.Allocator,
    stderr: std.fs.File,
) !void {
    const main_val = interp.global_env.get("main") orelse return;
    if (main_val != .function) return;
    const func = main_val.function;

    if (func.params.len > 0) {
        // Build args array: [path, program_args...]
        var string_values = std.ArrayListUnmanaged(values.Value){};
        string_values.ensureTotalCapacity(arena_alloc, program_args.len + 1) catch {
            try stderr.writeAll("Failed to allocate args array\n");
            return;
        };

        // Add the source file path as first arg
        string_values.appendAssumeCapacity(.{ .string = path });
        for (program_args) |arg| {
            string_values.appendAssumeCapacity(.{ .string = arg });
        }

        const arr = arena_alloc.create(values.ArrayValue) catch {
            try stderr.writeAll("Failed to allocate args array\n");
            return;
        };
        arr.* = .{ .elements = string_values.items };
        const args_value = values.Value{ .array = arr };

        _ = interp.callFunction(func, &.{args_value}) catch |err| {
            var buf: [512]u8 = undefined;
            if (interp.consumeLastErrorMessage()) |msg_text| {
                const msg = std.fmt.bufPrint(&buf, "{s}\n", .{msg_text}) catch "runtime error\n";
                try stderr.writeAll(msg);
                std.process.exit(1);
            }
            const msg = std.fmt.bufPrint(&buf, "Runtime error in main: {s}\n", .{@errorName(err)}) catch "Runtime error in main\n";
            try stderr.writeAll(msg);
            std.process.exit(1);
        };
    } else {
        _ = interp.callFunction(func, &.{}) catch |err| {
            var buf: [512]u8 = undefined;
            if (interp.consumeLastErrorMessage()) |msg_text| {
                const msg = std.fmt.bufPrint(&buf, "{s}\n", .{msg_text}) catch "runtime error\n";
                try stderr.writeAll(msg);
                std.process.exit(1);
            }
            const msg = std.fmt.bufPrint(&buf, "Runtime error in main: {s}\n", .{@errorName(err)}) catch "Runtime error in main\n";
            try stderr.writeAll(msg);
            std.process.exit(1);
        };
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
        const compilation_order = resolver.getCompilationOrder() catch {
            return;
        };
        defer allocator.free(compilation_order);

        _ = warnCircularImports(&resolver, stderr);

        // Set up checker with module resolver
        checker.setModuleResolver(&resolver);

        // Phase 1: Register all declarations and export symbols
        for (compilation_order) |mod| {
            if (mod.module_ast) |mod_ast| {
                checker.prepareForNewModule();
                checker.setCurrentModule(mod);
                checker.checkModuleDeclarations(mod_ast);
                checker.registerModuleExports(mod) catch {};
                checker.saveModuleScope(mod);
            }
        }
        // Phase 2: Check all bodies (all exports now available)
        for (compilation_order) |mod| {
            if (mod.module_ast) |_| {
                checker.restoreModuleScope(mod);
                checker.setCurrentModule(mod);
                checker.checkModuleBodies(mod.module_ast.?);
            }
        }

        // Error check (before multi-module execution)
        if (checker.hasErrors()) {
            var buf: [512]u8 = undefined;
            const header = std.fmt.bufPrint(&buf, "Type error(s):\n", .{}) catch "Type errors:\n";
            try stderr.writeAll(header);

            for (checker.errors.items) |check_err| {
                if (check_err.kind == .meta_warning) continue;
                const err_msg = std.fmt.bufPrint(&buf, "  {d}:{d}: {s}\n", .{
                    check_err.span.line,
                    check_err.span.column,
                    check_err.message,
                }) catch continue;
                try stderr.writeAll(err_msg);
            }
            try printMetaWarnings(&checker, stderr);
            return;
        }
        try printMetaWarnings(&checker, stderr);

        // Multi-module execution: create per-module interpreters in dependency order
        var module_interpreters = std.AutoHashMapUnmanaged(*ModuleInfo, *Interpreter){};
        defer module_interpreters.deinit(allocator);

        var owned_interpreters = std.ArrayListUnmanaged(*Interpreter){};
        defer {
            for (owned_interpreters.items) |interp_ptr| {
                interp_ptr.deinit();
                allocator.destroy(interp_ptr);
            }
            owned_interpreters.deinit(allocator);
        }

        // Pass 1: Create interpreters, bind available imports (skip circular), execute modules
        for (compilation_order) |mod| {
            const mod_ast = mod.module_ast orelse continue;

            const interp_ptr = allocator.create(Interpreter) catch {
                try stderr.writeAll("Failed to initialize interpreter\n");
                return;
            };
            interp_ptr.* = Interpreter.init(allocator) catch {
                allocator.destroy(interp_ptr);
                try stderr.writeAll("Failed to initialize interpreter\n");
                return;
            };
            owned_interpreters.append(allocator, interp_ptr) catch {
                interp_ptr.deinit();
                allocator.destroy(interp_ptr);
                try stderr.writeAll("Failed to allocate interpreter list\n");
                return;
            };

            // skip_unexecuted=true: skip deps whose interpreters aren't created yet (circular deps)
            bindRuntimeImportsForModuleEx(interp_ptr, mod, &resolver, &module_interpreters, true) catch |err| {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Runtime import error: {s}\n", .{@errorName(err)}) catch "Runtime import error\n";
                try stderr.writeAll(msg);
                std.process.exit(1);
            };

            interp_ptr.executeModule(mod_ast) catch |err| {
                var buf: [512]u8 = undefined;
                if (interp_ptr.consumeLastErrorMessage()) |msg_text| {
                    const msg = std.fmt.bufPrint(&buf, "{s}\n", .{msg_text}) catch "runtime error\n";
                    try stderr.writeAll(msg);
                    std.process.exit(1);
                }
                const msg = std.fmt.bufPrint(&buf, "Runtime error: {s}\n", .{@errorName(err)}) catch "Runtime error\n";
                try stderr.writeAll(msg);
                std.process.exit(1);
            };

            module_interpreters.put(allocator, mod, interp_ptr) catch {
                try stderr.writeAll("Failed to register module interpreter\n");
                return;
            };
        }

        // Pass 2: Bind remaining imports from circular dependencies (all modules now available)
        for (compilation_order) |mod| {
            _ = mod.module_ast orelse continue;
            const interp_ptr = module_interpreters.get(mod) orelse continue;

            bindRuntimeImportsForModuleEx(interp_ptr, mod, &resolver, &module_interpreters, false) catch |err| {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Runtime import error (pass 2): {s}\n", .{@errorName(err)}) catch "Runtime import error\n";
                try stderr.writeAll(msg);
                std.process.exit(1);
            };
        }

        // Get entry module interpreter and call main
        const entry_interp = module_interpreters.get(entry) orelse {
            try stderr.writeAll("Missing entry module interpreter\n");
            return;
        };

        try executeMainFunction(entry_interp, path, program_args, arena.allocator(), stderr);
        return;
    } else {
        // Single-file compilation (no imports)
        checker.checkModule(module);
    }

    // Single-module error check
    if (checker.hasErrors()) {
        var buf: [512]u8 = undefined;
        const header = std.fmt.bufPrint(&buf, "Type error(s):\n", .{}) catch "Type errors:\n";
        try stderr.writeAll(header);

        for (checker.errors.items) |check_err| {
            if (check_err.kind == .meta_warning) continue;
            const err_msg = std.fmt.bufPrint(&buf, "  {d}:{d}: {s}\n", .{
                check_err.span.line,
                check_err.span.column,
                check_err.message,
            }) catch continue;
            try stderr.writeAll(err_msg);
        }
        try printMetaWarnings(&checker, stderr);
        return;
    }
    try printMetaWarnings(&checker, stderr);

    // Single-module execution
    var interp = Interpreter.init(allocator) catch {
        try stderr.writeAll("Failed to initialize interpreter\n");
        return;
    };
    defer interp.deinit();

    interp.executeModule(module) catch |err| {
        var buf: [512]u8 = undefined;
        if (interp.consumeLastErrorMessage()) |msg_text| {
            const msg = std.fmt.bufPrint(&buf, "{s}\n", .{msg_text}) catch "runtime error\n";
            try stderr.writeAll(msg);
            std.process.exit(1);
        }
        const msg = std.fmt.bufPrint(&buf, "Runtime error: {s}\n", .{@errorName(err)}) catch "Runtime error\n";
        try stderr.writeAll(msg);
        std.process.exit(1);
    };

    try executeMainFunction(&interp, path, program_args, arena.allocator(), stderr);
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

fn dumpTokensFile(allocator: std.mem.Allocator, path: []const u8) !void {
    const source = readSourceFile(allocator, path) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Error opening file '{s}': {}", .{ path, err }) catch "Error opening file";
        try getStdErr().writeAll(msg);
        try getStdErr().writeAll("\n");
        const out = getStdOut();
        try out.writeAll("{\"error\":\"file_not_found\",\"message\":");
        try writeJsonEscaped(out, msg);
        try out.writeAll("}\n");
        std.process.exit(1);
    };
    defer allocator.free(source);

    const out = getStdOut();
    var lexer = Lexer.init(source);
    var first = true;

    try out.writeAll("[");

    while (true) {
        const token = lexer.next();
        const text = source[token.loc.start..token.loc.end];

        if (!first) try out.writeAll(",");
        first = false;

        try out.writeAll("{\"kind\":");
        try writeJsonEscaped(out, @tagName(token.kind));
        try out.writeAll(",\"text\":");
        try writeJsonEscaped(out, text);
        try out.writeAll(",\"line\":");
        try writeJsonUsize(out, token.loc.line);
        try out.writeAll(",\"column\":");
        try writeJsonUsize(out, token.loc.column);
        try out.writeAll(",\"start\":");
        try writeJsonUsize(out, token.loc.start);
        try out.writeAll(",\"end\":");
        try writeJsonUsize(out, token.loc.end);
        try out.writeAll("}");

        if (token.kind == .eof) break;
    }

    try out.writeAll("]\n");
}

fn dumpAstFile(allocator: std.mem.Allocator, path: []const u8) !void {
    const source = readSourceFile(allocator, path) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Error opening file '{s}': {}", .{ path, err }) catch "Error opening file";
        try getStdErr().writeAll(msg);
        try getStdErr().writeAll("\n");
        const out = getStdOut();
        try out.writeAll("{\"error\":\"file_not_found\",\"message\":");
        try writeJsonEscaped(out, msg);
        try out.writeAll("}\n");
        std.process.exit(1);
    };
    defer allocator.free(source);

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var lexer = Lexer.init(source);
    var parser = Parser.init(arena.allocator(), &lexer, source);

    const module = parser.parseModule() catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Parse error: {}", .{err}) catch "Parse error";
        try getStdErr().writeAll(msg);
        try getStdErr().writeAll("\n");
        for (parser.errors.items) |parse_err| {
            const err_msg = std.fmt.bufPrint(&buf, "  {d}:{d}: {s}\n", .{
                parse_err.span.line,
                parse_err.span.column,
                parse_err.message,
            }) catch continue;
            try getStdErr().writeAll(err_msg);
        }
        const out = getStdOut();
        try out.writeAll("{\"error\":\"parse_failed\",\"message\":");
        try writeJsonEscaped(out, msg);
        try out.writeAll("}\n");
        std.process.exit(1);
    };

    const out = getStdOut();
    try dumpModule(out, module);
    try out.writeAll("\n");
}

fn dumpMetaAnnotations(out: std.fs.File, annotations: []const ast.MetaAnnotation) anyerror!void {
    if (annotations.len == 0) return;
    try out.writeAll(",\"meta\":[");
    for (annotations, 0..) |ann, i| {
        if (i > 0) try out.writeAll(",");
        try dumpOneMetaAnnotation(out, ann);
    }
    try out.writeAll("]");
}

fn dumpOneMetaAnnotation(out: std.fs.File, ann: ast.MetaAnnotation) anyerror!void {
    switch (ann) {
        .intent => |s| {
            try out.writeAll("{\"kind\":\"intent\",\"value\":");
            try writeJsonEscaped(out, s.value);
            try out.writeAll("}");
        },
        .decision => |s| {
            try out.writeAll("{\"kind\":\"decision\",\"value\":");
            try writeJsonEscaped(out, s.value);
            try out.writeAll("}");
        },
        .tag => |s| {
            try out.writeAll("{\"kind\":\"tag\",\"value\":");
            try writeJsonEscaped(out, s.value);
            try out.writeAll("}");
        },
        .hint => |s| {
            try out.writeAll("{\"kind\":\"hint\",\"value\":");
            try writeJsonEscaped(out, s.value);
            try out.writeAll("}");
        },
        .deprecated => |s| {
            try out.writeAll("{\"kind\":\"deprecated\",\"value\":");
            try writeJsonEscaped(out, s.value);
            try out.writeAll("}");
        },
        .pure => {
            try out.writeAll("{\"kind\":\"pure\"}");
        },
        .module_meta => |b| {
            try out.writeAll("{\"kind\":\"module\"");
            try dumpMetaBlockEntries(out, b.entries);
            try out.writeAll("}");
        },
        .guide => |b| {
            try out.writeAll("{\"kind\":\"guide\"");
            try dumpMetaBlockEntries(out, b.entries);
            try out.writeAll("}");
        },
        .related => |r| {
            try out.writeAll("{\"kind\":\"related\",\"paths\":[");
            for (r.paths, 0..) |p, j| {
                if (j > 0) try out.writeAll(",");
                try dumpMetaPath(out, p);
            }
            try out.writeAll("],\"description\":");
            if (r.description) |desc| {
                try writeJsonEscaped(out, desc);
            } else {
                try out.writeAll("null");
            }
            try out.writeAll("}");
        },
        .group_def => |g| {
            try out.writeAll("{\"kind\":\"group\",\"name\":");
            try writeJsonEscaped(out, g.name);
            try out.writeAll(",\"annotations\":[");
            for (g.annotations, 0..) |nested, j| {
                if (j > 0) try out.writeAll(",");
                try dumpOneMetaAnnotation(out, nested);
            }
            try out.writeAll("]}");
        },
        .group_join => |s| {
            try out.writeAll("{\"kind\":\"in\",\"value\":");
            try writeJsonEscaped(out, s.value);
            try out.writeAll("}");
        },
        .define => |d| {
            try out.writeAll("{\"kind\":\"define\",\"name\":");
            try writeJsonEscaped(out, d.name);
            try out.writeAll(",\"params\":[");
            for (d.params, 0..) |p, j| {
                if (j > 0) try out.writeAll(",");
                try out.writeAll("{\"name\":");
                try writeJsonEscaped(out, p.name);
                try out.writeAll(",\"type\":");
                switch (p.type_constraint) {
                    .string_type => try out.writeAll("\"string\""),
                    .path_type => try out.writeAll("\"path\""),
                    .string_union => |vals| {
                        try out.writeAll("[");
                        for (vals, 0..) |v, k| {
                            if (k > 0) try out.writeAll(",");
                            try writeJsonEscaped(out, v);
                        }
                        try out.writeAll("]");
                    },
                }
                try out.writeAll("}");
            }
            try out.writeAll("],\"scope\":");
            if (d.scope) |scope| {
                try writeJsonEscaped(out, metaScopeString(scope));
            } else {
                try out.writeAll("null");
            }
            try out.writeAll("}");
        },
        .custom => |c| {
            try out.writeAll("{\"kind\":\"custom\",\"name\":");
            try writeJsonEscaped(out, c.name);
            try out.writeAll(",\"args\":[");
            for (c.args, 0..) |arg, j| {
                if (j > 0) try out.writeAll(",");
                switch (arg) {
                    .string => |s| try writeJsonEscaped(out, s),
                    .path => |p| try dumpMetaPath(out, p),
                }
            }
            try out.writeAll("]}");
        },
    }
}

fn metaScopeString(scope: ast.MetaScope) []const u8 {
    return switch (scope) {
        .fn_scope => "fn",
        .module_scope => "module",
        .struct_scope => "struct",
        .enum_scope => "enum",
        .trait_scope => "trait",
        .field_scope => "field",
        .variant_scope => "variant",
        .test_scope => "test",
    };
}

fn dumpMetaBlockEntries(out: std.fs.File, entries: []const ast.MetaKeyValue) anyerror!void {
    try out.writeAll(",\"entries\":[");
    for (entries, 0..) |entry, i| {
        if (i > 0) try out.writeAll(",");
        try out.writeAll("{\"key\":");
        try writeJsonEscaped(out, entry.key);
        try out.writeAll(",\"value\":");
        switch (entry.value) {
            .string => |s| try writeJsonEscaped(out, s),
            .string_list => |list| {
                try out.writeAll("[");
                for (list, 0..) |item, j| {
                    if (j > 0) try out.writeAll(",");
                    try writeJsonEscaped(out, item);
                }
                try out.writeAll("]");
            },
        }
        try out.writeAll("}");
    }
    try out.writeAll("]");
}

fn dumpMetaPath(out: std.fs.File, path: ast.MetaPath) anyerror!void {
    try out.writeAll("{\"segments\":[");
    for (path.segments, 0..) |seg, i| {
        if (i > 0) try out.writeAll(",");
        try writeJsonEscaped(out, seg);
    }
    try out.writeAll("]}");
}

fn dumpModule(out: std.fs.File, module: ast.Module) anyerror!void {
    try out.writeAll("{\"module_decl\":");
    if (module.module_decl) |md| {
        try dumpModuleDecl(out, md);
    } else {
        try out.writeAll("null");
    }
    try out.writeAll(",\"imports\":[");
    for (module.imports, 0..) |imp, i| {
        if (i > 0) try out.writeAll(",");
        try dumpImportDecl(out, imp);
    }
    try out.writeAll("],\"declarations\":[");
    for (module.declarations, 0..) |decl, i| {
        if (i > 0) try out.writeAll(",");
        try dumpDecl(out, decl);
    }
    try out.writeAll("]");
    try dumpMetaAnnotations(out, module.file_meta);
    try out.writeAll("}");
}

fn dumpModuleDecl(out: std.fs.File, md: ast.ModuleDecl) anyerror!void {
    try out.writeAll("{\"path\":[");
    for (md.path, 0..) |seg, i| {
        if (i > 0) try out.writeAll(",");
        try writeJsonEscaped(out, seg);
    }
    try out.writeAll("]}");
}

fn dumpImportDecl(out: std.fs.File, imp: ast.ImportDecl) anyerror!void {
    try out.writeAll("{\"path\":[");
    for (imp.path, 0..) |seg, i| {
        if (i > 0) try out.writeAll(",");
        try writeJsonEscaped(out, seg);
    }
    try out.writeAll("],\"items\":");
    if (imp.items) |items| {
        switch (items) {
            .all => try out.writeAll("\"*\""),
            .specific => |specs| {
                try out.writeAll("[");
                for (specs, 0..) |item, i| {
                    if (i > 0) try out.writeAll(",");
                    try out.writeAll("{\"name\":");
                    try writeJsonEscaped(out, item.name);
                    try out.writeAll(",\"alias\":");
                    if (item.alias) |a| {
                        try writeJsonEscaped(out, a);
                    } else {
                        try out.writeAll("null");
                    }
                    try out.writeAll("}");
                }
                try out.writeAll("]");
            },
        }
    } else {
        try out.writeAll("null");
    }
    try out.writeAll(",\"alias\":");
    if (imp.alias) |a| {
        try writeJsonEscaped(out, a);
    } else {
        try out.writeAll("null");
    }
    try out.writeAll("}");
}

fn dumpDecl(out: std.fs.File, decl: ast.Decl) anyerror!void {
    switch (decl) {
        .function => |f| {
            try out.writeAll("{\"kind\":\"function\",\"name\":");
            try writeJsonEscaped(out, f.name);
            try out.writeAll(",\"is_pub\":");
            try writeJsonBool(out, f.is_pub);
            try out.writeAll(",\"is_async\":");
            try writeJsonBool(out, f.is_async);
            try out.writeAll(",\"is_comptime\":");
            try writeJsonBool(out, f.is_comptime);
            try out.writeAll(",\"is_unsafe\":");
            try writeJsonBool(out, f.is_unsafe);
            try out.writeAll(",\"is_extern\":");
            try writeJsonBool(out, f.is_extern);
            try out.writeAll(",\"is_variadic\":");
            try writeJsonBool(out, f.is_variadic);
            try dumpTypeParams(out, f.type_params);
            try dumpFunctionParams(out, f.params);
            try out.writeAll(",\"return_type\":");
            if (f.return_type) |rt| {
                try dumpTypeExpr(out, rt);
            } else {
                try out.writeAll("null");
            }
            try out.writeAll(",\"where_clause\":");
            if (f.where_clause) |wc| {
                try dumpWhereClause(out, wc);
            } else {
                try out.writeAll("null");
            }
            try out.writeAll(",\"body\":");
            if (f.body) |body| {
                try dumpBlock(out, body);
            } else {
                try out.writeAll("null");
            }
            try dumpMetaAnnotations(out, f.meta);
            try out.writeAll("}");
        },
        .test_decl => |t| {
            try out.writeAll("{\"kind\":\"test_decl\",\"name\":");
            try writeJsonEscaped(out, t.name);
            try out.writeAll(",\"body\":");
            try dumpBlock(out, t.body);
            try dumpMetaAnnotations(out, t.meta);
            try out.writeAll("}");
        },
        .struct_decl => |s| {
            try out.writeAll("{\"kind\":\"struct_decl\",\"name\":");
            try writeJsonEscaped(out, s.name);
            try out.writeAll(",\"is_pub\":");
            try writeJsonBool(out, s.is_pub);
            try out.writeAll(",\"is_extern\":");
            try writeJsonBool(out, s.is_extern);
            try out.writeAll(",\"is_packed\":");
            try writeJsonBool(out, s.is_packed);
            try dumpTypeParams(out, s.type_params);
            try out.writeAll(",\"fields\":[");
            for (s.fields, 0..) |field, i| {
                if (i > 0) try out.writeAll(",");
                try dumpStructField(out, field);
            }
            try out.writeAll("],\"traits\":[");
            for (s.traits, 0..) |tr, i| {
                if (i > 0) try out.writeAll(",");
                try dumpTypeExpr(out, tr);
            }
            try out.writeAll("]");
            try dumpMetaAnnotations(out, s.meta);
            try out.writeAll("}");
        },
        .enum_decl => |e| {
            try out.writeAll("{\"kind\":\"enum_decl\",\"name\":");
            try writeJsonEscaped(out, e.name);
            try out.writeAll(",\"is_pub\":");
            try writeJsonBool(out, e.is_pub);
            try out.writeAll(",\"is_extern\":");
            try writeJsonBool(out, e.is_extern);
            try dumpTypeParams(out, e.type_params);
            try out.writeAll(",\"repr_type\":");
            if (e.repr_type) |rt| {
                try dumpTypeExpr(out, rt);
            } else {
                try out.writeAll("null");
            }
            try out.writeAll(",\"variants\":[");
            for (e.variants, 0..) |v, i| {
                if (i > 0) try out.writeAll(",");
                try dumpEnumVariant(out, v);
            }
            try out.writeAll("]");
            try dumpMetaAnnotations(out, e.meta);
            try out.writeAll("}");
        },
        .trait_decl => |t| {
            try out.writeAll("{\"kind\":\"trait_decl\",\"name\":");
            try writeJsonEscaped(out, t.name);
            try out.writeAll(",\"is_pub\":");
            try writeJsonBool(out, t.is_pub);
            try out.writeAll(",\"is_unsafe\":");
            try writeJsonBool(out, t.is_unsafe);
            try dumpTypeParams(out, t.type_params);
            try out.writeAll(",\"super_traits\":[");
            for (t.super_traits, 0..) |st, i| {
                if (i > 0) try out.writeAll(",");
                try dumpTypeExpr(out, st);
            }
            try out.writeAll("],\"associated_types\":[");
            for (t.associated_types, 0..) |at, i| {
                if (i > 0) try out.writeAll(",");
                try out.writeAll("{\"name\":");
                try writeJsonEscaped(out, at.name);
                try out.writeAll(",\"bounds\":[");
                for (at.bounds, 0..) |b, j| {
                    if (j > 0) try out.writeAll(",");
                    try dumpTypeExpr(out, b);
                }
                try out.writeAll("],\"default\":");
                if (at.default) |d| {
                    try dumpTypeExpr(out, d);
                } else {
                    try out.writeAll("null");
                }
                try out.writeAll("}");
            }
            try out.writeAll("],\"methods\":[");
            for (t.methods, 0..) |m, i| {
                if (i > 0) try out.writeAll(",");
                try dumpFunctionDeclInline(out, m);
            }
            try out.writeAll("]");
            try dumpMetaAnnotations(out, t.meta);
            try out.writeAll("}");
        },
        .impl_decl => |imp| {
            try out.writeAll("{\"kind\":\"impl_decl\",\"is_unsafe\":");
            try writeJsonBool(out, imp.is_unsafe);
            try dumpTypeParams(out, imp.type_params);
            try out.writeAll(",\"target_type\":");
            try dumpTypeExpr(out, imp.target_type);
            try out.writeAll(",\"trait_type\":");
            if (imp.trait_type) |tt| {
                try dumpTypeExpr(out, tt);
            } else {
                try out.writeAll("null");
            }
            try out.writeAll(",\"associated_types\":[");
            for (imp.associated_types, 0..) |at, i| {
                if (i > 0) try out.writeAll(",");
                try out.writeAll("{\"name\":");
                try writeJsonEscaped(out, at.name);
                try out.writeAll(",\"value\":");
                try dumpTypeExpr(out, at.value);
                try out.writeAll("}");
            }
            try out.writeAll("],\"where_clause\":");
            if (imp.where_clause) |wc| {
                try dumpWhereClause(out, wc);
            } else {
                try out.writeAll("null");
            }
            try out.writeAll(",\"methods\":[");
            for (imp.methods, 0..) |m, i| {
                if (i > 0) try out.writeAll(",");
                try dumpFunctionDeclInline(out, m);
            }
            try out.writeAll("]");
            try dumpMetaAnnotations(out, imp.meta);
            try out.writeAll("}");
        },
        .type_alias => |t| {
            try out.writeAll("{\"kind\":\"type_alias\",\"name\":");
            try writeJsonEscaped(out, t.name);
            try out.writeAll(",\"is_pub\":");
            try writeJsonBool(out, t.is_pub);
            try dumpTypeParams(out, t.type_params);
            try out.writeAll(",\"target\":");
            try dumpTypeExpr(out, t.target);
            try dumpMetaAnnotations(out, t.meta);
            try out.writeAll("}");
        },
        .const_decl => |c| {
            try out.writeAll("{\"kind\":\"const_decl\",\"name\":");
            try writeJsonEscaped(out, c.name);
            try out.writeAll(",\"is_pub\":");
            try writeJsonBool(out, c.is_pub);
            try out.writeAll(",\"type\":");
            if (c.type_) |t| {
                try dumpTypeExpr(out, t);
            } else {
                try out.writeAll("null");
            }
            try out.writeAll(",\"value\":");
            try dumpExpr(out, c.value);
            try dumpMetaAnnotations(out, c.meta);
            try out.writeAll("}");
        },
        .import_decl => |imp| {
            try out.writeAll("{\"kind\":\"import_decl\"");
            try out.writeAll(",\"data\":");
            try dumpImportDecl(out, imp.*);
            try out.writeAll("}");
        },
        .module_decl => |md| {
            try out.writeAll("{\"kind\":\"module_decl\"");
            try out.writeAll(",\"data\":");
            try dumpModuleDecl(out, md.*);
            try out.writeAll("}");
        },
        .extern_type_decl => |e| {
            try out.writeAll("{\"kind\":\"extern_type_decl\",\"name\":");
            try writeJsonEscaped(out, e.name);
            try out.writeAll(",\"is_pub\":");
            try writeJsonBool(out, e.is_pub);
            try out.writeAll(",\"size\":");
            if (e.size) |sz| {
                var buf: [32]u8 = undefined;
                const n = std.fmt.bufPrint(&buf, "{d}", .{sz}) catch "0";
                try out.writeAll(n);
            } else {
                try out.writeAll("null");
            }
            try out.writeAll("}");
        },
        .extern_block => |eb| {
            try out.writeAll("{\"kind\":\"extern_block\",\"functions\":[");
            for (eb.functions, 0..) |f, i| {
                if (i > 0) try out.writeAll(",");
                try dumpFunctionDeclInline(out, f.*);
            }
            try out.writeAll("]}");
        },
    }
}

fn dumpFunctionDeclInline(out: std.fs.File, f: ast.FunctionDecl) anyerror!void {
    try out.writeAll("{\"name\":");
    try writeJsonEscaped(out, f.name);
    try out.writeAll(",\"is_pub\":");
    try writeJsonBool(out, f.is_pub);
    try out.writeAll(",\"is_async\":");
    try writeJsonBool(out, f.is_async);
    try out.writeAll(",\"is_comptime\":");
    try writeJsonBool(out, f.is_comptime);
    try out.writeAll(",\"is_unsafe\":");
    try writeJsonBool(out, f.is_unsafe);
    try out.writeAll(",\"is_extern\":");
    try writeJsonBool(out, f.is_extern);
    try out.writeAll(",\"is_variadic\":");
    try writeJsonBool(out, f.is_variadic);
    try dumpTypeParams(out, f.type_params);
    try dumpFunctionParams(out, f.params);
    try out.writeAll(",\"return_type\":");
    if (f.return_type) |rt| {
        try dumpTypeExpr(out, rt);
    } else {
        try out.writeAll("null");
    }
    try out.writeAll(",\"where_clause\":");
    if (f.where_clause) |wc| {
        try dumpWhereClause(out, wc);
    } else {
        try out.writeAll("null");
    }
    try out.writeAll(",\"body\":");
    if (f.body) |body| {
        try dumpBlock(out, body);
    } else {
        try out.writeAll("null");
    }
    try dumpMetaAnnotations(out, f.meta);
    try out.writeAll("}");
}

fn dumpTypeParams(out: std.fs.File, type_params: []const ast.TypeParam) anyerror!void {
    try out.writeAll(",\"type_params\":[");
    for (type_params, 0..) |tp, i| {
        if (i > 0) try out.writeAll(",");
        try out.writeAll("{\"name\":");
        try writeJsonEscaped(out, tp.name);
        try out.writeAll(",\"bounds\":[");
        for (tp.bounds, 0..) |b, j| {
            if (j > 0) try out.writeAll(",");
            try dumpTypeExpr(out, b);
        }
        try out.writeAll("]}");
    }
    try out.writeAll("]");
}

fn dumpFunctionParams(out: std.fs.File, params: []const ast.FunctionParam) anyerror!void {
    try out.writeAll(",\"params\":[");
    for (params, 0..) |p, i| {
        if (i > 0) try out.writeAll(",");
        try out.writeAll("{\"name\":");
        try writeJsonEscaped(out, p.name);
        try out.writeAll(",\"type\":");
        try dumpTypeExpr(out, p.type_);
        try out.writeAll(",\"is_comptime\":");
        try writeJsonBool(out, p.is_comptime);
        try out.writeAll(",\"is_out\":");
        try writeJsonBool(out, p.is_out);
        try out.writeAll(",\"default_value\":");
        if (p.default_value) |dv| {
            try dumpExpr(out, dv);
        } else {
            try out.writeAll("null");
        }
        try out.writeAll("}");
    }
    try out.writeAll("]");
}

fn dumpWhereClause(out: std.fs.File, wc: []const ast.WhereConstraint) anyerror!void {
    try out.writeAll("[");
    for (wc, 0..) |c, i| {
        if (i > 0) try out.writeAll(",");
        try out.writeAll("{\"type_param\":");
        try writeJsonEscaped(out, c.type_param);
        try out.writeAll(",\"bounds\":[");
        for (c.bounds, 0..) |b, j| {
            if (j > 0) try out.writeAll(",");
            try dumpTypeExpr(out, b);
        }
        try out.writeAll("]}");
    }
    try out.writeAll("]");
}

fn dumpStructField(out: std.fs.File, field: ast.StructField) anyerror!void {
    try out.writeAll("{\"name\":");
    try writeJsonEscaped(out, field.name);
    try out.writeAll(",\"type\":");
    try dumpTypeExpr(out, field.type_);
    try out.writeAll(",\"is_pub\":");
    try writeJsonBool(out, field.is_pub);
    try dumpMetaAnnotations(out, field.meta);
    try out.writeAll("}");
}

fn dumpEnumVariant(out: std.fs.File, v: ast.EnumVariant) anyerror!void {
    try out.writeAll("{\"name\":");
    try writeJsonEscaped(out, v.name);
    try out.writeAll(",\"value\":");
    if (v.value) |val| {
        var buf: [64]u8 = undefined;
        const n = std.fmt.bufPrint(&buf, "{d}", .{val}) catch "0";
        try out.writeAll(n);
    } else {
        try out.writeAll("null");
    }
    try out.writeAll(",\"payload\":");
    if (v.payload) |payload| {
        switch (payload) {
            .tuple => |tuple_types| {
                try out.writeAll("{\"kind\":\"tuple\",\"types\":[");
                for (tuple_types, 0..) |t, i| {
                    if (i > 0) try out.writeAll(",");
                    try dumpTypeExpr(out, t);
                }
                try out.writeAll("]}");
            },
            .struct_ => |fields| {
                try out.writeAll("{\"kind\":\"struct\",\"fields\":[");
                for (fields, 0..) |f, i| {
                    if (i > 0) try out.writeAll(",");
                    try dumpStructField(out, f);
                }
                try out.writeAll("]}");
            },
        }
    } else {
        try out.writeAll("null");
    }
    try dumpMetaAnnotations(out, v.meta);
    try out.writeAll("}");
}

fn dumpBlock(out: std.fs.File, block: *const ast.Block) anyerror!void {
    try out.writeAll("{\"statements\":[");
    for (block.statements, 0..) |stmt, i| {
        if (i > 0) try out.writeAll(",");
        try dumpStmt(out, stmt);
    }
    try out.writeAll("],\"final_expr\":");
    if (block.final_expr) |fe| {
        try dumpExpr(out, fe);
    } else {
        try out.writeAll("null");
    }
    try out.writeAll("}");
}

fn dumpStmt(out: std.fs.File, stmt: ast.Stmt) anyerror!void {
    switch (stmt) {
        .let_decl => |l| {
            try out.writeAll("{\"kind\":\"let_decl\",\"name\":");
            try writeJsonEscaped(out, l.name);
            try out.writeAll(",\"is_shadow\":");
            try writeJsonBool(out, l.is_shadow);
            try out.writeAll(",\"type\":");
            try dumpTypeExpr(out, l.type_);
            try out.writeAll(",\"value\":");
            try dumpExpr(out, l.value);
            try out.writeAll("}");
        },
        .var_decl => |v| {
            try out.writeAll("{\"kind\":\"var_decl\",\"name\":");
            try writeJsonEscaped(out, v.name);
            try out.writeAll(",\"is_shadow\":");
            try writeJsonBool(out, v.is_shadow);
            try out.writeAll(",\"type\":");
            try dumpTypeExpr(out, v.type_);
            try out.writeAll(",\"value\":");
            try dumpExpr(out, v.value);
            try out.writeAll("}");
        },
        .assignment => |a| {
            try out.writeAll("{\"kind\":\"assignment\",\"op\":");
            try writeJsonEscaped(out, @tagName(a.op));
            try out.writeAll(",\"target\":");
            try dumpExpr(out, a.target);
            try out.writeAll(",\"value\":");
            try dumpExpr(out, a.value);
            try out.writeAll("}");
        },
        .expr_stmt => |e| {
            try out.writeAll("{\"kind\":\"expr_stmt\",\"expr\":");
            try dumpExpr(out, e.expr);
            try out.writeAll("}");
        },
        .return_stmt => |r| {
            try out.writeAll("{\"kind\":\"return_stmt\",\"value\":");
            if (r.value) |v| {
                try dumpExpr(out, v);
            } else {
                try out.writeAll("null");
            }
            try out.writeAll("}");
        },
        .break_stmt => |b| {
            try out.writeAll("{\"kind\":\"break_stmt\",\"value\":");
            if (b.value) |v| {
                try dumpExpr(out, v);
            } else {
                try out.writeAll("null");
            }
            try out.writeAll("}");
        },
        .continue_stmt => {
            try out.writeAll("{\"kind\":\"continue_stmt\"}");
        },
        .for_loop => |f| {
            try out.writeAll("{\"kind\":\"for_loop\",\"pattern\":");
            try dumpPattern(out, f.pattern);
            try out.writeAll(",\"iterable\":");
            try dumpExpr(out, f.iterable);
            try out.writeAll(",\"body\":");
            try dumpBlock(out, f.body);
            try out.writeAll("}");
        },
        .while_loop => |w| {
            try out.writeAll("{\"kind\":\"while_loop\",\"condition\":");
            try dumpExpr(out, w.condition);
            try out.writeAll(",\"body\":");
            try dumpBlock(out, w.body);
            try out.writeAll("}");
        },
        .loop_stmt => |l| {
            try out.writeAll("{\"kind\":\"loop_stmt\",\"body\":");
            try dumpBlock(out, l.body);
            try out.writeAll("}");
        },
        .if_stmt => |i| {
            try dumpIfStmt(out, i);
        },
        .match_stmt => |m| {
            try out.writeAll("{\"kind\":\"match_stmt\",\"subject\":");
            try dumpExpr(out, m.subject);
            try out.writeAll(",\"arms\":[");
            for (m.arms, 0..) |arm, j| {
                if (j > 0) try out.writeAll(",");
                try out.writeAll("{\"pattern\":");
                try dumpPattern(out, arm.pattern);
                try out.writeAll(",\"guard\":");
                if (arm.guard) |g| {
                    try dumpExpr(out, g);
                } else {
                    try out.writeAll("null");
                }
                try out.writeAll(",\"body\":");
                try dumpBlock(out, arm.body);
                try out.writeAll("}");
            }
            try out.writeAll("]}");
        },
    }
}

fn dumpIfStmt(out: std.fs.File, i: *const ast.IfStmt) anyerror!void {
    try out.writeAll("{\"kind\":\"if_stmt\",\"condition\":");
    try dumpExpr(out, i.condition);
    try out.writeAll(",\"then_branch\":");
    try dumpBlock(out, i.then_branch);
    try out.writeAll(",\"else_branch\":");
    if (i.else_branch) |eb| {
        switch (eb.*) {
            .block => |b| {
                try out.writeAll("{\"kind\":\"block\",\"body\":");
                try dumpBlock(out, b);
                try out.writeAll("}");
            },
            .if_stmt => |nested| {
                try dumpIfStmt(out, nested);
            },
        }
    } else {
        try out.writeAll("null");
    }
    try out.writeAll("}");
}

fn dumpExpr(out: std.fs.File, expr: ast.Expr) anyerror!void {
    switch (expr) {
        .literal => |lit| {
            try out.writeAll("{\"kind\":\"literal\"");
            switch (lit.kind) {
                .int => |v| {
                    try out.writeAll(",\"type\":\"int\",\"value\":");
                    var buf: [64]u8 = undefined;
                    const n = std.fmt.bufPrint(&buf, "{d}", .{v}) catch "0";
                    try writeJsonEscaped(out, n);
                },
                .float => |v| {
                    try out.writeAll(",\"type\":\"float\",\"value\":");
                    var buf: [64]u8 = undefined;
                    const n = std.fmt.bufPrint(&buf, "{d}", .{v}) catch "0";
                    try out.writeAll(n);
                },
                .string => |v| {
                    try out.writeAll(",\"type\":\"string\",\"value\":");
                    try writeJsonEscaped(out, v);
                },
                .char => |v| {
                    try out.writeAll(",\"type\":\"char\",\"value\":");
                    var buf: [32]u8 = undefined;
                    const n = std.fmt.bufPrint(&buf, "{d}", .{v}) catch "0";
                    try out.writeAll(n);
                },
                .bool_ => |v| {
                    try out.writeAll(",\"type\":\"bool\",\"value\":");
                    try writeJsonBool(out, v);
                },
            }
            try out.writeAll("}");
        },
        .identifier => |id| {
            try out.writeAll("{\"kind\":\"identifier\",\"name\":");
            try writeJsonEscaped(out, id.name);
            try out.writeAll("}");
        },
        .binary => |bin| {
            try out.writeAll("{\"kind\":\"binary\",\"op\":");
            try writeJsonEscaped(out, @tagName(bin.op));
            try out.writeAll(",\"left\":");
            try dumpExpr(out, bin.left);
            try out.writeAll(",\"right\":");
            try dumpExpr(out, bin.right);
            try out.writeAll("}");
        },
        .unary => |un| {
            try out.writeAll("{\"kind\":\"unary\",\"op\":");
            try writeJsonEscaped(out, @tagName(un.op));
            try out.writeAll(",\"operand\":");
            try dumpExpr(out, un.operand);
            try out.writeAll("}");
        },
        .postfix => |p| {
            try out.writeAll("{\"kind\":\"postfix\",\"op\":");
            try writeJsonEscaped(out, @tagName(p.op));
            try out.writeAll(",\"operand\":");
            try dumpExpr(out, p.operand);
            try out.writeAll("}");
        },
        .call => |c| {
            try out.writeAll("{\"kind\":\"call\",\"callee\":");
            try dumpExpr(out, c.callee);
            try out.writeAll(",\"args\":[");
            for (c.args, 0..) |arg, i| {
                if (i > 0) try out.writeAll(",");
                try dumpExpr(out, arg);
            }
            try out.writeAll("],\"type_args\":");
            if (c.type_args) |ta| {
                try out.writeAll("[");
                for (ta, 0..) |t, i| {
                    if (i > 0) try out.writeAll(",");
                    try dumpTypeExpr(out, t);
                }
                try out.writeAll("]");
            } else {
                try out.writeAll("null");
            }
            try out.writeAll("}");
        },
        .index => |idx| {
            try out.writeAll("{\"kind\":\"index\",\"object\":");
            try dumpExpr(out, idx.object);
            try out.writeAll(",\"index\":");
            try dumpExpr(out, idx.index);
            try out.writeAll("}");
        },
        .field => |f| {
            try out.writeAll("{\"kind\":\"field\",\"object\":");
            try dumpExpr(out, f.object);
            try out.writeAll(",\"field_name\":");
            try writeJsonEscaped(out, f.field_name);
            try out.writeAll("}");
        },
        .method_call => |m| {
            try out.writeAll("{\"kind\":\"method_call\",\"object\":");
            try dumpExpr(out, m.object);
            try out.writeAll(",\"method_name\":");
            try writeJsonEscaped(out, m.method_name);
            try out.writeAll(",\"type_args\":");
            if (m.type_args) |ta| {
                try out.writeAll("[");
                for (ta, 0..) |t, i| {
                    if (i > 0) try out.writeAll(",");
                    try dumpTypeExpr(out, t);
                }
                try out.writeAll("]");
            } else {
                try out.writeAll("null");
            }
            try out.writeAll(",\"args\":[");
            for (m.args, 0..) |arg, i| {
                if (i > 0) try out.writeAll(",");
                try dumpExpr(out, arg);
            }
            try out.writeAll("]}");
        },
        .block => |b| {
            try out.writeAll("{\"kind\":\"block\",\"body\":");
            try dumpBlock(out, b);
            try out.writeAll("}");
        },
        .closure => |c| {
            try out.writeAll("{\"kind\":\"closure\",\"params\":[");
            for (c.params, 0..) |p, i| {
                if (i > 0) try out.writeAll(",");
                try out.writeAll("{\"name\":");
                try writeJsonEscaped(out, p.name);
                try out.writeAll(",\"type\":");
                try dumpTypeExpr(out, p.type_);
                try out.writeAll("}");
            }
            try out.writeAll("],\"return_type\":");
            try dumpTypeExpr(out, c.return_type);
            try out.writeAll(",\"body\":");
            try dumpExpr(out, c.body);
            try out.writeAll("}");
        },
        .range => |r| {
            try out.writeAll("{\"kind\":\"range\",\"inclusive\":");
            try writeJsonBool(out, r.inclusive);
            try out.writeAll(",\"start\":");
            if (r.start) |s| {
                try dumpExpr(out, s);
            } else {
                try out.writeAll("null");
            }
            try out.writeAll(",\"end\":");
            if (r.end) |e| {
                try dumpExpr(out, e);
            } else {
                try out.writeAll("null");
            }
            try out.writeAll("}");
        },
        .struct_literal => |s| {
            try out.writeAll("{\"kind\":\"struct_literal\",\"type_name\":");
            if (s.type_name) |tn| {
                try dumpTypeExpr(out, tn);
            } else {
                try out.writeAll("null");
            }
            try out.writeAll(",\"fields\":[");
            for (s.fields, 0..) |f, i| {
                if (i > 0) try out.writeAll(",");
                try out.writeAll("{\"name\":");
                try writeJsonEscaped(out, f.name);
                try out.writeAll(",\"value\":");
                try dumpExpr(out, f.value);
                try out.writeAll("}");
            }
            try out.writeAll("],\"spread\":");
            if (s.spread) |sp| {
                try dumpExpr(out, sp);
            } else {
                try out.writeAll("null");
            }
            try out.writeAll("}");
        },
        .array_literal => |a| {
            try out.writeAll("{\"kind\":\"array_literal\",\"elements\":[");
            for (a.elements, 0..) |e, i| {
                if (i > 0) try out.writeAll(",");
                try dumpExpr(out, e);
            }
            try out.writeAll("]}");
        },
        .tuple_literal => |t| {
            try out.writeAll("{\"kind\":\"tuple_literal\",\"elements\":[");
            for (t.elements, 0..) |e, i| {
                if (i > 0) try out.writeAll(",");
                try dumpExpr(out, e);
            }
            try out.writeAll("]}");
        },
        .type_cast => |tc| {
            try out.writeAll("{\"kind\":\"type_cast\",\"truncating\":");
            try writeJsonBool(out, tc.truncating);
            try out.writeAll(",\"target_type\":");
            try dumpTypeExpr(out, tc.target_type);
            try out.writeAll(",\"expr\":");
            try dumpExpr(out, tc.expr);
            try out.writeAll("}");
        },
        .grouped => |g| {
            try out.writeAll("{\"kind\":\"grouped\",\"expr\":");
            try dumpExpr(out, g.expr);
            try out.writeAll("}");
        },
        .interpolated_string => |is| {
            try out.writeAll("{\"kind\":\"interpolated_string\",\"parts\":[");
            for (is.parts, 0..) |part, i| {
                if (i > 0) try out.writeAll(",");
                switch (part) {
                    .string => |s| {
                        try out.writeAll("{\"kind\":\"string\",\"value\":");
                        try writeJsonEscaped(out, s);
                        try out.writeAll("}");
                    },
                    .expr => |e| {
                        try out.writeAll("{\"kind\":\"expr\",\"value\":");
                        try dumpExpr(out, e);
                        try out.writeAll("}");
                    },
                }
            }
            try out.writeAll("]}");
        },
        .enum_literal => |el| {
            try out.writeAll("{\"kind\":\"enum_literal\",\"enum_type\":");
            try dumpTypeExpr(out, el.enum_type);
            try out.writeAll(",\"variant_name\":");
            try writeJsonEscaped(out, el.variant_name);
            try out.writeAll(",\"payload\":[");
            for (el.payload, 0..) |p, i| {
                if (i > 0) try out.writeAll(",");
                try dumpExpr(out, p);
            }
            try out.writeAll("]}");
        },
        .comptime_block => |cb| {
            try out.writeAll("{\"kind\":\"comptime_block\",\"body\":");
            try dumpBlock(out, cb.body);
            try out.writeAll("}");
        },
        .builtin_call => |bc| {
            try out.writeAll("{\"kind\":\"builtin_call\",\"name\":");
            try writeJsonEscaped(out, bc.name);
            try out.writeAll(",\"args\":[");
            for (bc.args, 0..) |arg, i| {
                if (i > 0) try out.writeAll(",");
                switch (arg) {
                    .type_arg => |t| {
                        try out.writeAll("{\"kind\":\"type\",\"value\":");
                        try dumpTypeExpr(out, t);
                        try out.writeAll("}");
                    },
                    .expr_arg => |e| {
                        try out.writeAll("{\"kind\":\"expr\",\"value\":");
                        try dumpExpr(out, e);
                        try out.writeAll("}");
                    },
                }
            }
            try out.writeAll("]}");
        },
        .unsafe_block => |ub| {
            try out.writeAll("{\"kind\":\"unsafe_block\",\"body\":");
            try dumpBlock(out, ub.body);
            try out.writeAll("}");
        },
        .out_arg => |oa| {
            try out.writeAll("{\"kind\":\"out_arg\",\"name\":");
            try writeJsonEscaped(out, oa.name);
            try out.writeAll("}");
        },
    }
}

fn dumpPattern(out: std.fs.File, pattern: ast.Pattern) anyerror!void {
    switch (pattern) {
        .wildcard => {
            try out.writeAll("{\"kind\":\"wildcard\"}");
        },
        .literal => |lit| {
            try out.writeAll("{\"kind\":\"literal\"");
            switch (lit.kind) {
                .int => |v| {
                    try out.writeAll(",\"type\":\"int\",\"value\":");
                    var buf: [64]u8 = undefined;
                    const n = std.fmt.bufPrint(&buf, "{d}", .{v}) catch "0";
                    try writeJsonEscaped(out, n);
                },
                .float => |v| {
                    try out.writeAll(",\"type\":\"float\",\"value\":");
                    var buf: [64]u8 = undefined;
                    const n = std.fmt.bufPrint(&buf, "{d}", .{v}) catch "0";
                    try out.writeAll(n);
                },
                .string => |v| {
                    try out.writeAll(",\"type\":\"string\",\"value\":");
                    try writeJsonEscaped(out, v);
                },
                .char => |v| {
                    try out.writeAll(",\"type\":\"char\",\"value\":");
                    var buf: [32]u8 = undefined;
                    const n = std.fmt.bufPrint(&buf, "{d}", .{v}) catch "0";
                    try out.writeAll(n);
                },
                .bool_ => |v| {
                    try out.writeAll(",\"type\":\"bool\",\"value\":");
                    try writeJsonBool(out, v);
                },
            }
            try out.writeAll("}");
        },
        .binding => |b| {
            try out.writeAll("{\"kind\":\"binding\",\"name\":");
            try writeJsonEscaped(out, b.name);
            try out.writeAll(",\"mutable\":");
            try writeJsonBool(out, b.mutable);
            try out.writeAll(",\"type_annotation\":");
            if (b.type_annotation) |ta| {
                try dumpTypeExpr(out, ta);
            } else {
                try out.writeAll("null");
            }
            try out.writeAll("}");
        },
        .variant => |v| {
            try out.writeAll("{\"kind\":\"variant\",\"type_expr\":");
            if (v.type_expr) |te| {
                try dumpTypeExpr(out, te);
            } else {
                try out.writeAll("null");
            }
            try out.writeAll(",\"variant_name\":");
            try writeJsonEscaped(out, v.variant_name);
            try out.writeAll(",\"payload\":");
            if (v.payload) |p| {
                try dumpPattern(out, p);
            } else {
                try out.writeAll("null");
            }
            try out.writeAll("}");
        },
        .struct_pattern => |sp| {
            try out.writeAll("{\"kind\":\"struct_pattern\",\"type_name\":");
            if (sp.type_name) |tn| {
                try writeJsonEscaped(out, tn);
            } else {
                try out.writeAll("null");
            }
            try out.writeAll(",\"fields\":[");
            for (sp.fields, 0..) |f, i| {
                if (i > 0) try out.writeAll(",");
                try out.writeAll("{\"name\":");
                try writeJsonEscaped(out, f.name);
                try out.writeAll(",\"pattern\":");
                if (f.pattern) |p| {
                    try dumpPattern(out, p);
                } else {
                    try out.writeAll("null");
                }
                try out.writeAll("}");
            }
            try out.writeAll("]}");
        },
        .tuple_pattern => |tp| {
            try out.writeAll("{\"kind\":\"tuple_pattern\",\"elements\":[");
            for (tp.elements, 0..) |e, i| {
                if (i > 0) try out.writeAll(",");
                try dumpPattern(out, e);
            }
            try out.writeAll("]}");
        },
        .or_pattern => |op| {
            try out.writeAll("{\"kind\":\"or_pattern\",\"alternatives\":[");
            for (op.alternatives, 0..) |a, i| {
                if (i > 0) try out.writeAll(",");
                try dumpPattern(out, a);
            }
            try out.writeAll("]}");
        },
        .guarded => |g| {
            try out.writeAll("{\"kind\":\"guarded\",\"pattern\":");
            try dumpPattern(out, g.pattern);
            try out.writeAll(",\"guard\":");
            try dumpExpr(out, g.guard);
            try out.writeAll("}");
        },
    }
}

fn dumpTypeExpr(out: std.fs.File, te: ast.TypeExpr) anyerror!void {
    switch (te) {
        .named => |n| {
            try out.writeAll("{\"kind\":\"named\",\"name\":");
            try writeJsonEscaped(out, n.name);
            try out.writeAll("}");
        },
        .array => |a| {
            try out.writeAll("{\"kind\":\"array\",\"element\":");
            try dumpTypeExpr(out, a.element);
            try out.writeAll(",\"size\":");
            try dumpExpr(out, a.size);
            try out.writeAll("}");
        },
        .slice => |s| {
            try out.writeAll("{\"kind\":\"slice\",\"element\":");
            try dumpTypeExpr(out, s.element);
            try out.writeAll("}");
        },
        .tuple => |t| {
            try out.writeAll("{\"kind\":\"tuple\",\"elements\":[");
            for (t.elements, 0..) |e, i| {
                if (i > 0) try out.writeAll(",");
                try dumpTypeExpr(out, e);
            }
            try out.writeAll("]}");
        },
        .optional => |o| {
            try out.writeAll("{\"kind\":\"optional\",\"inner\":");
            try dumpTypeExpr(out, o.inner);
            try out.writeAll("}");
        },
        .result => |r| {
            try out.writeAll("{\"kind\":\"result\",\"ok_type\":");
            try dumpTypeExpr(out, r.ok_type);
            try out.writeAll(",\"err_type\":");
            try dumpTypeExpr(out, r.err_type);
            try out.writeAll("}");
        },
        .function => |f| {
            try out.writeAll("{\"kind\":\"function\",\"params\":[");
            for (f.params, 0..) |p, i| {
                if (i > 0) try out.writeAll(",");
                try dumpTypeExpr(out, p);
            }
            try out.writeAll("],\"return_type\":");
            try dumpTypeExpr(out, f.return_type);
            try out.writeAll("}");
        },
        .extern_function => |ef| {
            try out.writeAll("{\"kind\":\"extern_function\",\"params\":[");
            for (ef.params, 0..) |p, i| {
                if (i > 0) try out.writeAll(",");
                try dumpTypeExpr(out, p);
            }
            try out.writeAll("],\"return_type\":");
            try dumpTypeExpr(out, ef.return_type);
            try out.writeAll("}");
        },
        .reference => |r| {
            try out.writeAll("{\"kind\":\"reference\",\"mutable\":");
            try writeJsonBool(out, r.mutable);
            try out.writeAll(",\"inner\":");
            try dumpTypeExpr(out, r.inner);
            try out.writeAll("}");
        },
        .generic_apply => |g| {
            try out.writeAll("{\"kind\":\"generic_apply\",\"base\":");
            try dumpTypeExpr(out, g.base);
            try out.writeAll(",\"args\":[");
            for (g.args, 0..) |a, i| {
                if (i > 0) try out.writeAll(",");
                try dumpTypeExpr(out, a);
            }
            try out.writeAll("]}");
        },
        .qualified => |q| {
            try out.writeAll("{\"kind\":\"qualified\",\"base\":");
            try dumpTypeExpr(out, q.base);
            try out.writeAll(",\"member\":");
            try writeJsonEscaped(out, q.member);
            try out.writeAll("}");
        },
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
        const compilation_order = resolver.getCompilationOrder() catch {
            return;
        };
        defer allocator.free(compilation_order);

        _ = warnCircularImports(&resolver, stderr);

        // Set up checker with module resolver
        checker.setModuleResolver(&resolver);

        // Phase 1: Register all declarations, export symbols, and collect for emission
        for (compilation_order) |mod| {
            if (mod.module_ast) |mod_ast| {
                // Prepare fresh scope for this module (keeps builtins and type registry)
                checker.prepareForNewModule();
                checker.setCurrentModule(mod);
                checker.checkModuleDeclarations(mod_ast);

                // Register exports WHILE in this module's scope (before switching)
                checker.registerModuleExports(mod) catch {};
                checker.saveModuleScope(mod);

                // Collect for emission
                try modules_to_emit.append(allocator, mod_ast);
            }
        }
        // Phase 2: Check all bodies (all exports now available)
        for (compilation_order) |mod| {
            if (mod.module_ast) |_| {
                checker.restoreModuleScope(mod);
                checker.setCurrentModule(mod);
                checker.checkModuleBodies(mod.module_ast.?);
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
            if (check_err.kind == .meta_warning) continue;
            const err_msg = std.fmt.bufPrint(&buf, "  {d}:{d}: {s}\n", .{
                check_err.span.line,
                check_err.span.column,
                check_err.message,
            }) catch continue;
            try stderr.writeAll(err_msg);
        }
        try printMetaWarnings(&checker, stderr);
        return;
    }
    try printMetaWarnings(&checker, stderr);

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

    // Set target platform for cross-compilation
    if (target_info) |ti| {
        const target_platform = ti.toPlatform();
        emitter.setTargetPlatform(target_platform, ti.triple);
    }

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
    if (modules_to_emit.items.len > 1) {
        // Multi-module: three phases to handle cross-module dependencies.
        // Skip main from non-entry modules (entry module is last in list).
        const last_idx = modules_to_emit.items.len - 1;

        // Phase 1: Register struct declarations for field name resolution
        for (modules_to_emit.items) |mod_to_emit| {
            for (mod_to_emit.declarations) |decl| {
                switch (decl) {
                    .struct_decl => |s| {
                        emitter.registerStructDecl(s) catch |err| {
                            var buf: [512]u8 = undefined;
                            const msg = std.fmt.bufPrint(&buf, "Codegen error (struct): {s}\n", .{@errorName(err)}) catch "Codegen error\n";
                            try stderr.writeAll(msg);
                            return;
                        };
                    },
                    else => {},
                }
            }
        }

        // Phase 2: Declare all functions across all modules so cross-module
        // calls can find their targets even with circular dependencies.
        for (modules_to_emit.items, 0..) |mod_to_emit, i| {
            emitter.skip_main = (i != last_idx);
            emitter.declareModuleFunctions(mod_to_emit) catch |err| {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Codegen error (decl): {s}\n", .{@errorName(err)}) catch "Codegen error\n";
                try stderr.writeAll(msg);
                return;
            };
        }

        // Phase 3: Emit all function bodies.
        for (modules_to_emit.items, 0..) |mod_to_emit, i| {
            emitter.skip_main = (i != last_idx);
            emitter.emitModuleBodies(mod_to_emit) catch |err| {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Codegen error: {s}\n", .{@errorName(err)}) catch "Codegen error\n";
                try stderr.writeAll(msg);
                return;
            };
        }
        emitter.skip_main = false;

        // Generate main wrapper after all modules are emitted (once only)
        if (emitter.main_takes_args and !emitter.freestanding) {
            emitter.emitMainArgsWrapper() catch |err| {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Codegen error (main wrapper): {s}\n", .{@errorName(err)}) catch "Codegen error\n";
                try stderr.writeAll(msg);
                return;
            };
        }
    } else {
        // Single-module: use the combined approach
        for (modules_to_emit.items) |mod_to_emit| {
            emitter.emitModule(mod_to_emit) catch |err| {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Codegen error: {s}\n", .{@errorName(err)}) catch "Codegen error\n";
                try stderr.writeAll(msg);
                return;
            };
        }
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

    // Generate object file (use target platform extension, not host)
    const target_platform = if (target_info) |ti| ti.toPlatform() else codegen.target.Platform.current();
    const obj_ext = target_platform.getObjectExtension();
    const obj_path_str = std.fmt.allocPrint(allocator, "{s}{s}", .{ base_name, obj_ext }) catch {
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

    // Link to create executable (default: build/<base_name>[.wasm])
    const exe_ext = target_platform.getExecutableExtension();
    const exe_path = options.output_path orelse blk: {
        // Create build directory if it doesn't exist
        std.fs.cwd().makePath("build") catch |err| {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Failed to create build directory: {s}\n", .{@errorName(err)}) catch "Failed to create build directory\n";
            try stderr.writeAll(msg);
            return;
        };
        break :blk std.fmt.allocPrint(allocator, "build/{s}{s}", .{ base_name, exe_ext }) catch {
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
    // Generate a unique temp path for the executable (cross-platform)
    const timestamp = std.time.timestamp();
    const exe_ext = comptime if (builtin.os.tag == .windows) ".exe" else "";
    var temp_path_buf: [512]u8 = undefined;
    const temp_path = if (builtin.os.tag == .windows) blk: {
        // On Windows, use %TEMP% or %TMP% or fall back to C:\Temp
        const temp_dir = std.process.getEnvVarOwned(allocator, "TEMP") catch
            std.process.getEnvVarOwned(allocator, "TMP") catch
            null;
        defer if (temp_dir) |d| allocator.free(d);
        const dir = temp_dir orelse "C:\\Temp";
        break :blk std.fmt.bufPrint(&temp_path_buf, "{s}\\klar-run-{d}{s}", .{ dir, timestamp, exe_ext }) catch {
            try getStdErr().writeAll("Failed to create temp path\n");
            return;
        };
    } else std.fmt.bufPrint(&temp_path_buf, "/tmp/klar-run-{d}{s}", .{ timestamp, exe_ext }) catch {
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

    // Execute the compiled binary with args.
    // On POSIX, we use fork/exec to set argv[0] to the source path while
    // executing the temp binary. On Windows, we use std.process.Child
    // (argv[0] will be the temp binary path — minor behavioral difference).
    if (builtin.os.tag == .windows) {
        // Windows: use std.process.Child
        var child_argv = std.ArrayListUnmanaged([]const u8){};
        defer child_argv.deinit(allocator);

        child_argv.append(allocator, temp_path) catch {
            try getStdErr().writeAll("Failed to allocate argv\n");
            std.fs.cwd().deleteFile(temp_path) catch {};
            return;
        };
        for (program_args) |arg| {
            child_argv.append(allocator, arg) catch {
                try getStdErr().writeAll("Failed to allocate argv\n");
                std.fs.cwd().deleteFile(temp_path) catch {};
                return;
            };
        }

        var child = std.process.Child.init(child_argv.items, allocator);
        const term = child.spawnAndWait() catch |err| {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Failed to execute: {s}\n", .{@errorName(err)}) catch "Failed to execute\n";
            getStdErr().writeAll(msg) catch {};
            std.fs.cwd().deleteFile(temp_path) catch {};
            return;
        };

        // Clean up temp file
        std.fs.cwd().deleteFile(temp_path) catch {};

        switch (term) {
            .Exited => |code| std.process.exit(code),
            .Signal => |sig| {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Process terminated by signal: {d}\n", .{sig}) catch "Process terminated by signal\n";
                getStdErr().writeAll(msg) catch {};
                std.process.exit(128 +| @as(u8, @truncate(sig)));
            },
            else => std.process.exit(1),
        }
    } else {
        // POSIX: use fork/exec to set argv[0] to source path
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

fn fmtCommand(allocator: std.mem.Allocator, args: []const []const u8) !void {
    const stderr = getStdErr();

    // Parse flags
    var in_place = false;
    var check_only = false;
    var targets = std.ArrayListUnmanaged([]const u8){};
    defer targets.deinit(allocator);

    var i: usize = 2;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            try getStdOut().writeAll(
                \\Usage: klar fmt [options] <file.kl|dir> ...
                \\
                \\Format Klar source files to canonical style.
                \\
                \\Options:
                \\  -i         Format files in-place (modify files)
                \\  --check    Check if files are formatted (exit 1 if not)
                \\  -h, --help Show this help message
                \\
                \\Without -i, formatted output is written to stdout.
                \\When given a directory, recursively formats all .kl files.
                \\
            );
            return;
        } else if (std.mem.eql(u8, arg, "-i")) {
            in_place = true;
        } else if (std.mem.eql(u8, arg, "--check")) {
            check_only = true;
        } else if (std.mem.startsWith(u8, arg, "-")) {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Error: unknown flag '{s}'\nUsage: klar fmt [-i] [--check] <file.kl|dir>\n", .{arg}) catch "Error: unknown flag\n";
            try stderr.writeAll(msg);
            return;
        } else {
            try targets.append(allocator, arg);
        }
    }

    if (targets.items.len == 0) {
        try stderr.writeAll("Error: no input file or directory\nUsage: klar fmt [-i] [--check] <file.kl|dir>\n");
        return;
    }

    var had_error = false;
    var had_unformatted = false;

    for (targets.items) |target| {
        // Check if target is a file or directory
        // Note: on Windows, statFile returns error.IsDir for directories
        const is_dir = blk: {
            const stat = std.fs.cwd().statFile(target) catch |err| {
                if (err == error.IsDir) break :blk true;
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Error: cannot access '{s}': {}\n", .{ target, err }) catch "Error accessing file\n";
                try stderr.writeAll(msg);
                had_error = true;
                continue;
            };
            break :blk stat.kind == .directory;
        };

        if (is_dir) {
            // Recursively format all .kl files
            try fmtDirectory(allocator, target, in_place, check_only, &had_error, &had_unformatted);
        } else {
            try fmtFile(allocator, target, in_place, check_only, &had_error, &had_unformatted);
        }
    }

    if (check_only and had_unformatted) {
        std.process.exit(1);
    }
    if (had_error) {
        std.process.exit(1);
    }
}

fn fmtFile(allocator: std.mem.Allocator, path: []const u8, in_place: bool, check_only: bool, had_error: *bool, had_unformatted: *bool) !void {
    const stderr = getStdErr();
    const stdout = getStdOut();

    const source = readSourceFile(allocator, path) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Error reading '{s}': {}\n", .{ path, err }) catch "Error reading file\n";
        try stderr.writeAll(msg);
        had_error.* = true;
        return;
    };
    defer allocator.free(source);

    const formatted = formatter.format(allocator, source) catch |err| {
        switch (err) {
            error.ParseError => {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Error: parse error in '{s}', skipping\n", .{path}) catch "Parse error\n";
                try stderr.writeAll(msg);
                had_error.* = true;
                return;
            },
            else => return err,
        }
    };
    defer allocator.free(formatted);

    if (check_only) {
        if (!std.mem.eql(u8, source, formatted)) {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "{s}\n", .{path}) catch "unformatted file\n";
            try stdout.writeAll(msg);
            had_unformatted.* = true;
        }
        return;
    }

    if (in_place) {
        if (!std.mem.eql(u8, source, formatted)) {
            // Write to temp file then rename for atomic write
            const dir = std.fs.cwd();
            const nonce = std.crypto.random.int(u64);
            const tmp_path = std.fmt.allocPrint(allocator, "{s}.fmt-tmp.{x}", .{ path, nonce }) catch {
                try stderr.writeAll("Error: out of memory\n");
                had_error.* = true;
                return;
            };
            defer allocator.free(tmp_path);

            const tmp_file = dir.createFile(tmp_path, .{}) catch |err| {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Error writing '{s}': {}\n", .{ path, err }) catch "Error writing\n";
                try stderr.writeAll(msg);
                had_error.* = true;
                return;
            };
            tmp_file.writeAll(formatted) catch |err| {
                tmp_file.close();
                dir.deleteFile(tmp_path) catch {};
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Error writing '{s}': {}\n", .{ path, err }) catch "Error writing\n";
                try stderr.writeAll(msg);
                had_error.* = true;
                return;
            };
            tmp_file.close();

            dir.rename(tmp_path, path) catch |err| {
                dir.deleteFile(tmp_path) catch {};
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Error renaming '{s}': {}\n", .{ path, err }) catch "Error renaming\n";
                try stderr.writeAll(msg);
                had_error.* = true;
                return;
            };
        }
    } else {
        // Print to stdout
        try stdout.writeAll(formatted);
    }
}

fn fmtDirectory(allocator: std.mem.Allocator, dir_path: []const u8, in_place: bool, check_only: bool, had_error: *bool, had_unformatted: *bool) !void {
    const stderr = getStdErr();

    var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Error opening directory '{s}': {}\n", .{ dir_path, err }) catch "Error opening directory\n";
        try stderr.writeAll(msg);
        had_error.* = true;
        return;
    };
    defer dir.close();

    var walker = dir.walk(allocator) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Error walking directory '{s}': {}\n", .{ dir_path, err }) catch "Error walking directory\n";
        try stderr.writeAll(msg);
        had_error.* = true;
        return;
    };
    defer walker.deinit();

    while (walker.next() catch |err| blk: {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Warning: error walking '{s}': {}\n", .{ dir_path, err }) catch "Warning: directory walk error\n";
        stderr.writeAll(msg) catch {};
        had_error.* = true;
        break :blk null;
    }) |entry| {
        // Skip hidden directories and known non-source dirs
        if (entry.kind == .directory) continue;

        // Skip files in hidden directories, build/, zig-out/, scratch/
        const path_str = entry.path;
        if (shouldSkipPath(path_str)) continue;

        // Only format .kl files
        if (!std.mem.endsWith(u8, path_str, ".kl")) continue;

        // Build full path
        const full_path = std.fs.path.join(allocator, &.{ dir_path, path_str }) catch continue;
        defer allocator.free(full_path);

        try fmtFile(allocator, full_path, in_place, check_only, had_error, had_unformatted);
    }
}

fn shouldSkipPath(path: []const u8) bool {
    // Skip hidden directories
    if (std.mem.startsWith(u8, path, ".")) return true;

    // Skip known directories
    const skip_dirs = [_][]const u8{ "build/", "zig-out/", "scratch/", "zig-cache/", ".zig-cache/" };
    for (skip_dirs) |skip| {
        if (std.mem.startsWith(u8, path, skip)) return true;
    }

    // Skip paths containing hidden directory segments (handle both / and \ separators)
    var it = std.mem.tokenizeAny(u8, path, "/\\");
    while (it.next()) |segment| {
        if (segment.len > 0 and segment[0] == '.') return true;
    }

    return false;
}

fn checkFile(allocator: std.mem.Allocator, path: []const u8, options: CheckCommandOptions) !void {
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
    var parse_had_errors = false;

    // Parse the module
    const module = if (options.partial_mode) blk: {
        const parsed = parser.parseModuleRecovering() catch |err| {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Parse error: {}\n", .{err}) catch "Parse error\n";
            try stderr.writeAll(msg);
            return;
        };
        parse_had_errors = parser.errors.items.len > 0;
        break :blk parsed;
    } else parser.parseModule() catch |err| {
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

    if (options.partial_mode and parser.errors.items.len > 0) {
        var buf: [512]u8 = undefined;
        const header = std.fmt.bufPrint(&buf, "Parse diagnostics ({d}):\n", .{parser.errors.items.len}) catch "Parse diagnostics:\n";
        try stderr.writeAll(header);
        for (parser.errors.items) |parse_err| {
            const err_msg = std.fmt.bufPrint(&buf, "  {d}:{d}: {s}\n", .{
                parse_err.span.line,
                parse_err.span.column,
                parse_err.message,
            }) catch continue;
            try stderr.writeAll(err_msg);
        }
    }

    // Type check the module
    var checker = TypeChecker.init(allocator);
    defer checker.deinit();
    checker.setTestMode(true);
    var scope_root: ?*const checker_mod.Scope = null;

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
        const compilation_order = resolver.getCompilationOrder() catch {
            return;
        };
        defer allocator.free(compilation_order);

        _ = warnCircularImports(&resolver, stderr);

        // Set up checker with module resolver
        checker.setModuleResolver(&resolver);

        // Phase 1: Register all declarations and export symbols
        for (compilation_order) |mod| {
            if (mod.module_ast) |mod_ast| {
                checker.prepareForNewModule();
                checker.setCurrentModule(mod);
                checker.checkModuleDeclarations(mod_ast);
                checker.registerModuleExports(mod) catch {};
                checker.saveModuleScope(mod);
            }
        }
        // Phase 2: Check all bodies (all exports now available)
        for (compilation_order) |mod| {
            if (mod.module_ast) |_| {
                checker.restoreModuleScope(mod);
                checker.setCurrentModule(mod);
                checker.checkModuleBodies(mod.module_ast.?);
                if (mod == entry) {
                    scope_root = checker.current_scope;
                }
            }
        }
    } else {
        // Single-file compilation (no imports)
        checker.checkModule(module);
        scope_root = checker.current_scope;
    }

    var buf: [512]u8 = undefined;

    const has_type_errors = checker.hasErrors();
    if (has_type_errors) {
        const header = std.fmt.bufPrint(&buf, "Type check failed with {d} error(s):\n", .{checker.error_count}) catch "Type check failed:\n";
        try stderr.writeAll(header);

        for (checker.errors.items) |check_err| {
            if (check_err.kind == .meta_warning) continue;
            const err_msg = std.fmt.bufPrint(&buf, "  {d}:{d} [{s}]: {s}\n", .{
                check_err.span.line,
                check_err.span.column,
                @tagName(check_err.kind),
                check_err.message,
            }) catch continue;
            try stderr.writeAll(err_msg);
        }
        try printMetaWarnings(&checker, stderr);
        if (options.scope_line == null and options.expected_line == null) return; // Query modes can proceed for tooling.
    }

    if (options.scope_line) |scope_line| {
        const scope_column = options.scope_column.?;
        const cursor_offset = sourceOffsetFromLineColumn(source, scope_line, scope_column) orelse {
            var out_buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&out_buf, "Error: position {d}:{d} is outside file bounds\n", .{ scope_line, scope_column }) catch "Error: invalid scope position\n";
            try stderr.writeAll(msg);
            return;
        };

        const root_scope = scope_root orelse checker.current_scope;
        const scope_entries = checker.extractScopeAtOffsetInScope(module, cursor_offset, root_scope) catch {
            try stderr.writeAll("Error: failed to extract scope at cursor\n");
            return;
        };
        defer allocator.free(scope_entries);

        if (options.scope_json) {
            try stdout.writeAll("{\"line\":");
            try writeJsonUsize(stdout, scope_line);
            try stdout.writeAll(",\"column\":");
            try writeJsonUsize(stdout, scope_column);
            try stdout.writeAll(",\"bindings\":[");
            for (scope_entries, 0..) |entry, idx| {
                if (idx > 0) try stdout.writeAll(",");
                const type_text = types.typeToString(allocator, entry.type_) catch null;
                try stdout.writeAll("{\"name\":");
                try writeJsonEscaped(stdout, entry.name);
                try stdout.writeAll(",\"type\":");
                try writeJsonEscaped(stdout, type_text orelse "unknown");
                try stdout.writeAll(",\"kind\":");
                try writeJsonEscaped(stdout, symbolKindName(entry.kind));
                try stdout.writeAll(",\"mutable\":");
                try writeJsonBool(stdout, entry.mutable);
                try stdout.writeAll("}");
                if (type_text) |owned| allocator.free(owned);
            }
            try stdout.writeAll("]}\n");
        } else {
            var out_buf: [256]u8 = undefined;
            const header = std.fmt.bufPrint(&out_buf, "Scope at {d}:{d} ({d} binding(s)):\n", .{
                scope_line,
                scope_column,
                scope_entries.len,
            }) catch "Scope:\n";
            try stdout.writeAll(header);

            for (scope_entries) |entry| {
                const type_text = types.typeToString(allocator, entry.type_) catch null;
                const line = std.fmt.bufPrint(&out_buf, "  {s}: {s} [{s}{s}]\n", .{
                    entry.name,
                    type_text orelse "unknown",
                    symbolKindName(entry.kind),
                    if (entry.mutable) ", mutable" else "",
                }) catch continue;
                try stdout.writeAll(line);
                if (type_text) |owned| allocator.free(owned);
            }
        }
        return;
    }

    if (options.expected_line) |expected_line| {
        const expected_column = options.expected_column.?;
        const cursor_offset = sourceOffsetFromLineColumn(source, expected_line, expected_column) orelse {
            var out_buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&out_buf, "Error: position {d}:{d} is outside file bounds\n", .{ expected_line, expected_column }) catch "Error: invalid expected-type position\n";
            try stderr.writeAll(msg);
            return;
        };

        const root_scope = scope_root orelse checker.current_scope;
        const expected_type = checker.expectedTypeAtOffsetInScope(module, cursor_offset, root_scope);
        if (expected_type) |typ| {
            const type_text = types.typeToString(allocator, typ) catch null;
            defer if (type_text) |owned| allocator.free(owned);
            var out_buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&out_buf, "Expected type at {d}:{d}: {s}\n", .{
                expected_line,
                expected_column,
                type_text orelse "unknown",
            }) catch "Expected type: unknown\n";
            try stdout.writeAll(msg);
        } else {
            var out_buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&out_buf, "Expected type at {d}:{d}: <unknown>\n", .{
                expected_line,
                expected_column,
            }) catch "Expected type: <unknown>\n";
            try stdout.writeAll(msg);
        }
        return;
    }

    if (options.partial_mode) {
        if (!parse_had_errors and !has_type_errors) {
            try stdout.writeAll("Partial check completed with no diagnostics\n");
        }
        return;
    }

    if (has_type_errors) return;

    // Ownership analysis
    var ownership_checker = ownership.OwnershipChecker.init(allocator);
    defer ownership_checker.deinit();

    ownership_checker.analyze(module) catch |err| {
        const msg = std.fmt.bufPrint(&buf, "Ownership analysis error: {}\n", .{err}) catch "Ownership analysis error\n";
        try stderr.writeAll(msg);
        return;
    };

    // Dump ownership state if requested
    if (options.dump_ownership) {
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

        // Print meta warnings (don't block checks, but inform the user)
        try printMetaWarnings(&checker, stderr);

        // Print some stats
        const decl_count = module.declarations.len;
        const stats = std.fmt.bufPrint(&buf, "  {d} declaration(s)\n", .{decl_count}) catch "";
        try stdout.writeAll(stats);
    }
}

fn bindRuntimeImportsForModule(
    interp: *Interpreter,
    mod: *ModuleInfo,
    resolver: *ModuleResolver,
    module_interpreters: *std.AutoHashMapUnmanaged(*ModuleInfo, *Interpreter),
) !void {
    return bindRuntimeImportsForModuleEx(interp, mod, resolver, module_interpreters, false);
}

/// Bind runtime imports for a module. When skip_unexecuted is true, dependencies
/// whose interpreters haven't been created yet are silently skipped (for circular
/// import support during pass 1). Resolution and AST errors always propagate.
fn bindRuntimeImportsForModuleEx(
    interp: *Interpreter,
    mod: *ModuleInfo,
    resolver: *ModuleResolver,
    module_interpreters: *std.AutoHashMapUnmanaged(*ModuleInfo, *Interpreter),
    skip_unexecuted: bool,
) !void {
    const module_ast = mod.module_ast orelse return;

    for (module_ast.imports) |import_decl| {
        const dep = resolver.resolve(import_decl.path, mod) catch {
            return error.ImportModuleResolveFailed;
        };
        const dep_ast = dep.module_ast orelse {
            return error.ImportModuleResolveFailed;
        };
        const dep_interp = module_interpreters.get(dep) orelse {
            if (skip_unexecuted) continue;
            return error.ImportInterpreterMissing;
        };

        if (import_decl.items) |items| {
            switch (items) {
                .all => {
                    for (dep_ast.declarations) |decl| {
                        switch (decl) {
                            .function => |f| {
                                if (!f.is_pub) continue;
                                const export_value = dep_interp.global_env.get(f.name) orelse {
                                    return error.ImportValueMissing;
                                };
                                try interp.current_env.define(f.name, export_value, false);
                            },
                            .const_decl => |c| {
                                if (!c.is_pub) continue;
                                const export_value = dep_interp.global_env.get(c.name) orelse {
                                    return error.ImportValueMissing;
                                };
                                try interp.current_env.define(c.name, export_value, false);
                            },
                            .struct_decl => |s| {
                                if (!s.is_pub) continue;
                                // Types may have runtime namespace values (from impl blocks)
                                if (dep_interp.global_env.get(s.name)) |type_ns| {
                                    try interp.current_env.define(s.name, type_ns, false);
                                }
                            },
                            .enum_decl => |e| {
                                if (!e.is_pub) continue;
                                if (dep_interp.global_env.get(e.name)) |type_ns| {
                                    try interp.current_env.define(e.name, type_ns, false);
                                }
                            },
                            else => continue,
                        }
                    }
                },
                .specific => |specific_items| {
                    for (specific_items) |item| {
                        const SymbolKind = enum { runtime_value, type_only, missing };
                        var symbol_kind: SymbolKind = .missing;
                        for (dep_ast.declarations) |decl| {
                            switch (decl) {
                                .function => |f| {
                                    if (f.is_pub and std.mem.eql(u8, f.name, item.name)) {
                                        symbol_kind = .runtime_value;
                                        break;
                                    }
                                },
                                .const_decl => |c| {
                                    if (c.is_pub and std.mem.eql(u8, c.name, item.name)) {
                                        symbol_kind = .runtime_value;
                                        break;
                                    }
                                },
                                .struct_decl => |s| {
                                    if (s.is_pub and std.mem.eql(u8, s.name, item.name)) {
                                        symbol_kind = .type_only;
                                        break;
                                    }
                                },
                                .enum_decl => |e| {
                                    if (e.is_pub and std.mem.eql(u8, e.name, item.name)) {
                                        symbol_kind = .type_only;
                                        break;
                                    }
                                },
                                .trait_decl => |t| {
                                    if (t.is_pub and std.mem.eql(u8, t.name, item.name)) {
                                        symbol_kind = .type_only;
                                        break;
                                    }
                                },
                                .type_alias => |ta| {
                                    if (ta.is_pub and std.mem.eql(u8, ta.name, item.name)) {
                                        symbol_kind = .type_only;
                                        break;
                                    }
                                },
                                else => {},
                            }
                        }

                        if (symbol_kind == .missing) {
                            return error.ImportSymbolMissing;
                        }
                        if (symbol_kind == .type_only) {
                            // Types may have runtime namespace values (from impl blocks)
                            // Bind them if present in the dependency interpreter
                            if (dep_interp.global_env.get(item.name)) |type_ns| {
                                const local_name = item.alias orelse item.name;
                                try interp.current_env.define(local_name, type_ns, false);
                            }
                            continue;
                        }

                        const export_value = dep_interp.global_env.get(item.name) orelse {
                            return error.ImportValueMissing;
                        };

                        const local_name = item.alias orelse item.name;
                        try interp.current_env.define(local_name, export_value, false);
                    }
                },
            }
        } else {
            // Namespace import: import foo / import foo as bar
            const namespace_name = import_decl.alias orelse import_decl.path[import_decl.path.len - 1];
            const namespace_struct = try interp.allocator.create(values.StructValue);
            namespace_struct.* = .{
                .type_name = namespace_name,
                .fields = .{},
            };

            for (dep_ast.declarations) |decl| {
                const export_name = switch (decl) {
                    .function => |f| if (f.is_pub) f.name else continue,
                    .const_decl => |c| if (c.is_pub) c.name else continue,
                    else => continue,
                };

                const export_value = dep_interp.global_env.get(export_name) orelse {
                    return error.ImportValueMissing;
                };
                try namespace_struct.fields.put(interp.allocator, export_name, export_value);
            }

            try interp.current_env.define(namespace_name, .{ .struct_ = namespace_struct }, false);
        }

        // Propagate type methods from dependency interpreter for instance method dispatch.
        // When functions from the dependency are called, they may invoke instance methods
        // on types defined in the dependency module. The importing interpreter needs these
        // type_methods entries for dispatch.
        var dep_type_iter = dep_interp.type_methods.iterator();
        while (dep_type_iter.next()) |entry| {
            const gop = try interp.type_methods.getOrPut(interp.allocator, entry.key_ptr.*);
            if (!gop.found_existing) {
                gop.value_ptr.* = .{};
            }
            // Merge individual methods
            var method_iter = entry.value_ptr.iterator();
            while (method_iter.next()) |method_entry| {
                try gop.value_ptr.put(interp.allocator, method_entry.key_ptr.*, method_entry.value_ptr.*);
            }
        }
    }
}

fn runTestPath(allocator: std.mem.Allocator, path: []const u8, options: TestRunOptions) !void {
    const stdout = getStdOut();
    const stderr = getStdErr();

    // Note: on Windows, statFile returns error.IsDir for directories
    const is_dir = blk: {
        const stat = std.fs.cwd().statFile(path) catch |err| {
            if (err == error.IsDir) break :blk true;
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Error accessing path '{s}': {}\n", .{ path, err }) catch "Error accessing path\n";
            try stderr.writeAll(msg);
            return error.TestsFailed;
        };
        break :blk stat.kind == .directory;
    };

    if (!is_dir) {
        const result = runTestFile(allocator, path, options) catch |err| {
            if (err == error.TestsFailed and options.json_output) {
                try stdout.writeAll("{\"file\":");
                try writeJsonEscaped(stdout, path);
                try stdout.writeAll(",\"total\":0,\"passed\":0,\"failed\":0,\"tests\":[]}\n");
            }
            return err;
        };
        defer freeJsonTestResults(allocator, result.test_results);
        defer freeJsonCompilerErrors(allocator, result.compile_errors);
        if (options.json_output and result.file_failed) {
            try stdout.writeAll("{\"file\":");
            try writeJsonEscaped(stdout, path);
            try stdout.writeAll(",\"total\":");
            try writeJsonUsize(stdout, result.tests_discovered);
            try stdout.writeAll(",\"passed\":");
            try writeJsonUsize(stdout, result.tests_passed);
            try stdout.writeAll(",\"failed\":");
            try writeJsonUsize(stdout, result.tests_failed);
            try stdout.writeAll(",\"tests\":[],\"errors\":[");
            for (result.compile_errors, 0..) |compile_error, idx| {
                if (idx > 0) try stdout.writeAll(",");
                try stdout.writeAll("{\"stage\":");
                try writeJsonEscaped(stdout, compile_error.stage);
                try stdout.writeAll(",\"message\":");
                try writeJsonEscaped(stdout, compile_error.message);
                if (compile_error.line) |line| {
                    try stdout.writeAll(",\"line\":");
                    try writeJsonUsize(stdout, line);
                }
                if (compile_error.column) |column| {
                    try stdout.writeAll(",\"column\":");
                    try writeJsonUsize(stdout, column);
                }
                try stdout.writeAll("}");
            }
            try stdout.writeAll("]}\n");
        }
        if (result.tests_failed > 0 or result.file_failed) return error.TestsFailed;
        return;
    } else {
        var dir = std.fs.cwd().openDir(path, .{ .iterate = true }) catch |err| {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Error opening directory '{s}': {}\n", .{ path, err }) catch "Error opening directory\n";
                try stderr.writeAll(msg);
                return error.TestsFailed;
            };
            defer dir.close();

            var walker = try dir.walk(allocator);
            defer walker.deinit();

            var files_run: usize = 0;
            var files_failed: usize = 0;
            var tests_run: usize = 0;
            var per_file_options = options;
            per_file_options.require_tests = false;
            per_file_options.emit_output = !options.json_output;

            var json_file_results = std.ArrayListUnmanaged(JsonFileSummary){};
            defer {
                for (json_file_results.items) |item| {
                    allocator.free(item.path);
                    freeJsonTestResults(allocator, item.test_results);
                    freeJsonCompilerErrors(allocator, item.compile_errors);
                }
                json_file_results.deinit(allocator);
            }

            while (try walker.next()) |entry| {
                if (entry.kind != .file) continue;
                if (!std.mem.endsWith(u8, entry.path, ".kl")) continue;

                const full_path = try std.fs.path.join(allocator, &.{ path, entry.path });
                defer allocator.free(full_path);

                files_run += 1;
                const file_result = runTestFile(allocator, full_path, per_file_options) catch |err| {
                    if (err == error.TestsFailed) {
                        files_failed += 1;
                        if (options.json_output) {
                            const owned_path = try allocator.dupe(u8, full_path);
                            const compile_errors = try allocator.alloc(JsonCompileError, 1);
                            compile_errors[0] = .{
                                .stage = "runtime_setup",
                                .message = try allocator.dupe(u8, @errorName(err)),
                            };
                            try json_file_results.append(allocator, .{
                                .path = owned_path,
                                .total = 0,
                                .passed = 0,
                                // Compile/setup errors are file failures, not failed test assertions.
                                .failed = 0,
                                .test_results = try allocator.alloc(JsonTestSummary, 0),
                                .compile_errors = compile_errors,
                            });
                        }
                        continue;
                    }
                    return err;
                };
                tests_run += file_result.tests_discovered;
                if (file_result.tests_failed > 0 or file_result.file_failed) {
                    files_failed += 1;
                }
                if (options.json_output) {
                    const owned_path = try allocator.dupe(u8, full_path);
                    try json_file_results.append(allocator, .{
                        .path = owned_path,
                        .total = file_result.tests_discovered,
                        .passed = file_result.tests_passed,
                        .failed = file_result.tests_failed,
                        .test_results = file_result.test_results,
                        .compile_errors = file_result.compile_errors,
                    });
                } else {
                    freeJsonTestResults(allocator, file_result.test_results);
                    freeJsonCompilerErrors(allocator, file_result.compile_errors);
                }
            }

            if (files_run == 0) {
                var buf: [512]u8 = undefined;
                if (options.json_output) {
                    try stdout.writeAll("{\"path\":");
                    try writeJsonEscaped(stdout, path);
                    try stdout.writeAll(",\"files\":[],\"summary\":{\"files\":0,\"failed_files\":0,\"tests\":0,\"passed\":0,\"failed\":0}}\n");
                } else {
                    const msg = std.fmt.bufPrint(&buf, "No .kl files found in '{s}'\n", .{path}) catch "No .kl files found\n";
                    try stdout.writeAll(msg);
                }
                if (options.require_tests) {
                    const err_msg = std.fmt.bufPrint(&buf, "Error: --require-tests enabled, but no tests found in '{s}'\n", .{path}) catch "Error: no tests found\n";
                    try stderr.writeAll(err_msg);
                    return error.TestsFailed;
                }
                if (options.strict_tests) {
                    const warn_msg = std.fmt.bufPrint(&buf, "Warning: no tests found in '{s}'\n", .{path}) catch "Warning: no tests found\n";
                    try stderr.writeAll(warn_msg);
                }
                return;
            }

            var buf: [512]u8 = undefined;
            if (options.json_output) {
                var total_passed: usize = 0;
                var total_failed: usize = 0;
                for (json_file_results.items) |item| {
                    total_passed += item.passed;
                    total_failed += item.failed;
                }

                try stdout.writeAll("{\"path\":");
                try writeJsonEscaped(stdout, path);
                try stdout.writeAll(",\"files\":[");
                for (json_file_results.items, 0..) |item, idx| {
                    if (idx > 0) try stdout.writeAll(",");
                    try stdout.writeAll("{\"file\":");
                    try writeJsonEscaped(stdout, item.path);
                    try stdout.writeAll(",\"total\":");
                    try writeJsonUsize(stdout, item.total);
                    try stdout.writeAll(",\"passed\":");
                    try writeJsonUsize(stdout, item.passed);
                    try stdout.writeAll(",\"failed\":");
                    try writeJsonUsize(stdout, item.failed);
                    try stdout.writeAll(",\"tests\":[");
                    for (item.test_results, 0..) |test_result, test_idx| {
                        if (test_idx > 0) try stdout.writeAll(",");
                        try stdout.writeAll("{\"name\":");
                        try writeJsonEscaped(stdout, test_result.name);
                        try stdout.writeAll(",\"status\":");
                        try writeJsonEscaped(stdout, if (test_result.passed) "PASS" else "FAIL");
                        if (test_result.error_name) |error_name| {
                            try stdout.writeAll(",\"error\":");
                            try writeJsonEscaped(stdout, error_name);
                        }
                        if (test_result.source) |test_source| {
                            try stdout.writeAll(",\"source\":");
                            try writeJsonEscaped(stdout, test_source);
                        }
                        try stdout.writeAll(",\"assertions\":[");
                        for (test_result.assertions, 0..) |assertion, assertion_idx| {
                            if (assertion_idx > 0) try stdout.writeAll(",");
                            try stdout.writeAll("{\"type\":");
                            try writeJsonEscaped(stdout, assertion.assertion_type);
                            try stdout.writeAll(",\"passed\":");
                            try writeJsonBool(stdout, assertion.passed);
                            if (assertion.expected) |expected| {
                                try stdout.writeAll(",\"expected\":");
                                try writeJsonEscaped(stdout, expected);
                            }
                            if (assertion.actual) |actual| {
                                try stdout.writeAll(",\"actual\":");
                                try writeJsonEscaped(stdout, actual);
                            }
                            try stdout.writeAll("}");
                        }
                        try stdout.writeAll("]}");
                    }
                    try stdout.writeAll("]");
                    try stdout.writeAll(",\"errors\":[");
                    for (item.compile_errors, 0..) |compile_error, err_idx| {
                        if (err_idx > 0) try stdout.writeAll(",");
                        try stdout.writeAll("{\"stage\":");
                        try writeJsonEscaped(stdout, compile_error.stage);
                        try stdout.writeAll(",\"message\":");
                        try writeJsonEscaped(stdout, compile_error.message);
                        if (compile_error.line) |line| {
                            try stdout.writeAll(",\"line\":");
                            try writeJsonUsize(stdout, line);
                        }
                        if (compile_error.column) |column| {
                            try stdout.writeAll(",\"column\":");
                            try writeJsonUsize(stdout, column);
                        }
                        try stdout.writeAll("}");
                    }
                    try stdout.writeAll("]");
                    try stdout.writeAll("}");
                }
                try stdout.writeAll("],\"summary\":{\"files\":");
                try writeJsonUsize(stdout, files_run);
                try stdout.writeAll(",\"failed_files\":");
                try writeJsonUsize(stdout, files_failed);
                try stdout.writeAll(",\"tests\":");
                try writeJsonUsize(stdout, tests_run);
                try stdout.writeAll(",\"passed\":");
                try writeJsonUsize(stdout, total_passed);
                try stdout.writeAll(",\"failed\":");
                try writeJsonUsize(stdout, total_failed);
                try stdout.writeAll("}}\n");
            } else {
                const summary = std.fmt.bufPrint(&buf, "Directory test result: {d} file(s), {d} failed\n", .{ files_run, files_failed }) catch "Directory test result\n";
                try stdout.writeAll(summary);
            }

            if (tests_run == 0 and files_failed == 0) {
                if (options.require_tests) {
                    const err_msg = std.fmt.bufPrint(&buf, "Error: --require-tests enabled, but no tests found in '{s}'\n", .{path}) catch "Error: no tests found\n";
                    try stderr.writeAll(err_msg);
                    return error.TestsFailed;
                }
                if (options.strict_tests) {
                    const warn_msg = std.fmt.bufPrint(&buf, "Warning: no tests found in '{s}'\n", .{path}) catch "Warning: no tests found\n";
                    try stderr.writeAll(warn_msg);
                }
            }

            if (files_failed > 0) {
                return error.TestsFailed;
            }
    }
}

fn runTestFile(allocator: std.mem.Allocator, path: []const u8, options: TestRunOptions) !TestFileResult {
    const source = readSourceFile(allocator, path) catch |err| {
        if (options.json_output) {
            var msg_buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&msg_buf, "Error opening file '{s}': {}", .{ path, err }) catch "Error opening file";
            return makeSingleCompileErrorResult(allocator, "io", msg, null, null);
        }
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Error opening file '{s}': {}\n", .{ path, err }) catch "Error opening file\n";
        try getStdErr().writeAll(msg);
        return error.TestsFailed;
    };
    defer allocator.free(source);

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const stdout = getStdOut();
    const stderr = getStdErr();
    var lexer = Lexer.init(source);
    var parser = Parser.init(arena.allocator(), &lexer, source);

    const module = parser.parseModule() catch |err| {
        if (options.json_output) {
            const compile_errors = try allocator.alloc(JsonCompileError, parser.errors.items.len + 1);
            var compile_errors_initialized: usize = 0;
            errdefer {
                for (compile_errors[0..compile_errors_initialized]) |compile_error| {
                    allocator.free(compile_error.message);
                }
                allocator.free(compile_errors);
            }
            var header_buf: [256]u8 = undefined;
            const header = std.fmt.bufPrint(&header_buf, "Parse error: {}", .{err}) catch "Parse error";
            compile_errors[0] = .{
                .stage = "parse",
                .message = try allocator.dupe(u8, header),
            };
            compile_errors_initialized = 1;
            for (parser.errors.items, 0..) |parse_err, idx| {
                compile_errors[idx + 1] = .{
                    .stage = "parse",
                    .message = try allocator.dupe(u8, parse_err.message),
                    .line = parse_err.span.line,
                    .column = parse_err.span.column,
                };
                compile_errors_initialized += 1;
            }
            const empty_test_results = try allocator.alloc(JsonTestSummary, 0);
            return .{
                .tests_discovered = 0,
                .tests_passed = 0,
                .tests_failed = 0,
                .test_results = empty_test_results,
                .compile_errors = compile_errors,
                .file_failed = true,
            };
        }
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
        return error.TestsFailed;
    };

    var checker = TypeChecker.init(allocator);
    defer checker.deinit();
    checker.setTestMode(true);

    var module_sources = std.ArrayListUnmanaged([]const u8){};
    defer {
        for (module_sources.items) |src| {
            allocator.free(src);
        }
        module_sources.deinit(allocator);
    }

    if (module.imports.len > 0) {
        var resolver = ModuleResolver.init(allocator);
        defer resolver.deinit();

        if (findStdLibPath(allocator)) |std_path| {
            resolver.setStdLibPath(std_path);
            defer allocator.free(std_path);
        }

        var cwd_buf: [std.fs.max_path_bytes]u8 = undefined;
        if (std.fs.cwd().realpath(".", &cwd_buf)) |cwd_path| {
            resolver.addSearchPath(cwd_path) catch {};
        } else |_| {}

        const entry = resolver.resolveEntry(path) catch {
            if (options.json_output) {
                return makeSingleCompileErrorResult(allocator, "module_resolve", "could not resolve entry module", null, null);
            }
            try stderr.writeAll("Error: could not resolve entry module\n");
            return error.TestsFailed;
        };
        entry.module_ast = module;
        entry.state = .parsed;

        var modules_to_process = std.ArrayListUnmanaged(*ModuleInfo){};
        defer modules_to_process.deinit(allocator);
        try modules_to_process.append(allocator, entry);

        var processed_idx: usize = 0;
        while (processed_idx < modules_to_process.items.len) {
            const mod = modules_to_process.items[processed_idx];
            processed_idx += 1;

            if (mod.state == .discovered) {
                const src = parseModuleSource(allocator, arena.allocator(), mod) catch |err| {
                    if (options.json_output) {
                        var msg_buf: [256]u8 = undefined;
                        const msg = std.fmt.bufPrint(&msg_buf, "failed to parse module '{s}': {s}", .{ mod.file_path, @errorName(err) }) catch "failed to parse module";
                        return makeSingleCompileErrorResult(allocator, "module_parse", msg, null, null);
                    }
                    return error.TestsFailed;
                };
                try module_sources.append(allocator, src);
            }

            if (mod.module_ast) |mod_ast| {
                for (mod_ast.imports) |import_decl| {
                    const dep = resolver.resolve(import_decl.path, mod) catch continue;
                    if (dep.state == .discovered) {
                        try modules_to_process.append(allocator, dep);
                    }
                }
            }
        }

        if (resolver.hasErrors()) {
            if (options.json_output) {
                const compile_errors = try allocator.alloc(JsonCompileError, resolver.errors.items.len);
                var compile_errors_initialized: usize = 0;
                errdefer {
                    for (compile_errors[0..compile_errors_initialized]) |compile_error| {
                        allocator.free(compile_error.message);
                    }
                    allocator.free(compile_errors);
                }
                for (resolver.errors.items, 0..) |resolve_err, idx| {
                    compile_errors[idx] = .{
                        .stage = "module_resolve",
                        .message = try allocator.dupe(u8, resolve_err.message),
                    };
                    compile_errors_initialized += 1;
                }
                const empty_test_results = try allocator.alloc(JsonTestSummary, 0);
                return .{
                    .tests_discovered = 0,
                    .tests_passed = 0,
                    .tests_failed = 0,
                    .test_results = empty_test_results,
                    .compile_errors = compile_errors,
                    .file_failed = true,
                };
            }
            var buf: [512]u8 = undefined;
            for (resolver.errors.items) |err| {
                const err_msg = std.fmt.bufPrint(&buf, "Module error: {s}\n", .{err.message}) catch continue;
                try stderr.writeAll(err_msg);
            }
            return error.TestsFailed;
        }

        const compilation_order = resolver.getCompilationOrder() catch |err| {
            if (options.json_output) {
                var msg_buf: [256]u8 = undefined;
                const msg = std.fmt.bufPrint(&msg_buf, "failed to compute compilation order: {s}", .{@errorName(err)}) catch "failed to compute compilation order";
                return makeSingleCompileErrorResult(allocator, "module_resolve", msg, null, null);
            }
            return error.TestsFailed;
        };
        defer allocator.free(compilation_order);

        _ = warnCircularImports(&resolver, stderr);

        checker.setModuleResolver(&resolver);

        // Phase 1: Register all declarations and export symbols
        for (compilation_order) |mod| {
            if (mod.module_ast) |mod_ast| {
                checker.prepareForNewModule();
                checker.setCurrentModule(mod);
                checker.checkModuleDeclarations(mod_ast);
                checker.registerModuleExports(mod) catch {};
                checker.saveModuleScope(mod);
            }
        }
        // Phase 2: Check all bodies (all exports now available)
        for (compilation_order) |mod| {
            if (mod.module_ast) |_| {
                checker.restoreModuleScope(mod);
                checker.setCurrentModule(mod);
                checker.checkModuleBodies(mod.module_ast.?);
            }
        }

        if (checker.hasErrors()) {
            if (options.json_output) {
                const compile_errors = try allocator.alloc(JsonCompileError, checker.error_count);
                var compile_errors_initialized: usize = 0;
                errdefer {
                    for (compile_errors[0..compile_errors_initialized]) |compile_error| {
                        allocator.free(compile_error.message);
                    }
                    allocator.free(compile_errors);
                }
                for (checker.errors.items) |check_err| {
                    if (check_err.kind == .meta_warning) continue;
                    compile_errors[compile_errors_initialized] = .{
                        .stage = "type_check",
                        .message = try allocator.dupe(u8, check_err.message),
                        .line = check_err.span.line,
                        .column = check_err.span.column,
                    };
                    compile_errors_initialized += 1;
                }
                const empty_test_results = try allocator.alloc(JsonTestSummary, 0);
                return .{
                    .tests_discovered = 0,
                    .tests_passed = 0,
                    .tests_failed = 0,
                    .test_results = empty_test_results,
                    .compile_errors = compile_errors,
                    .file_failed = true,
                };
            }
            var buf: [512]u8 = undefined;
            const header = std.fmt.bufPrint(&buf, "Type error(s):\n", .{}) catch "Type errors:\n";
            try stderr.writeAll(header);

            for (checker.errors.items) |check_err| {
                if (check_err.kind == .meta_warning) continue;
                const err_msg = std.fmt.bufPrint(&buf, "  {d}:{d}: {s}\n", .{
                    check_err.span.line,
                    check_err.span.column,
                    check_err.message,
                }) catch continue;
                try stderr.writeAll(err_msg);
            }
            return error.TestsFailed;
        }

        var module_interpreters = std.AutoHashMapUnmanaged(*ModuleInfo, *Interpreter){};
        defer module_interpreters.deinit(allocator);

        var owned_interpreters = std.ArrayListUnmanaged(*Interpreter){};
        defer {
            for (owned_interpreters.items) |interp_ptr| {
                interp_ptr.deinit();
                allocator.destroy(interp_ptr);
            }
            owned_interpreters.deinit(allocator);
        }

        // Build one interpreter per module in dependency order.
        for (compilation_order) |mod| {
            const mod_ast = mod.module_ast orelse continue;

            const interp_ptr = try allocator.create(Interpreter);
            interp_ptr.* = try Interpreter.init(allocator);
            try owned_interpreters.append(allocator, interp_ptr);

            bindRuntimeImportsForModule(interp_ptr, mod, &resolver, &module_interpreters) catch |err| {
                if (options.json_output) {
                    var msg_buf: [256]u8 = undefined;
                    const module_name = mod.canonicalName(allocator) catch "<module>";
                    defer if (!std.mem.eql(u8, module_name, "<module>")) allocator.free(module_name);
                    const msg = std.fmt.bufPrint(&msg_buf, "Runtime setup error in module '{s}': {s}", .{ module_name, @errorName(err) }) catch "Runtime setup error";
                    return makeSingleCompileErrorResult(allocator, "runtime_setup", msg, null, null);
                }
                var buf: [512]u8 = undefined;
                const module_name = mod.canonicalName(allocator) catch "<module>";
                defer if (!std.mem.eql(u8, module_name, "<module>")) allocator.free(module_name);
                const msg = std.fmt.bufPrint(&buf, "Runtime setup error in module '{s}': {s}\n", .{ module_name, @errorName(err) }) catch "Runtime setup error\n";
                try stderr.writeAll(msg);
                return error.TestsFailed;
            };

            interp_ptr.executeModule(mod_ast) catch |err| {
                if (options.json_output) {
                    var msg_buf: [256]u8 = undefined;
                    const module_name = mod.canonicalName(allocator) catch "<module>";
                    defer if (!std.mem.eql(u8, module_name, "<module>")) allocator.free(module_name);
                    const msg = std.fmt.bufPrint(&msg_buf, "Runtime setup error in module '{s}': {s}", .{ module_name, @errorName(err) }) catch "Runtime setup error";
                    return makeSingleCompileErrorResult(allocator, "runtime_setup", msg, null, null);
                }
                var buf: [512]u8 = undefined;
                const module_name = mod.canonicalName(allocator) catch "<module>";
                defer if (!std.mem.eql(u8, module_name, "<module>")) allocator.free(module_name);
                const msg = std.fmt.bufPrint(&buf, "Runtime setup error in module '{s}': {s}\n", .{ module_name, @errorName(err) }) catch "Runtime setup error\n";
                try stderr.writeAll(msg);
                return error.TestsFailed;
            };

            try module_interpreters.put(allocator, mod, interp_ptr);
        }

        const entry_mod = resolver.entry_module orelse {
            if (options.json_output) {
                return makeSingleCompileErrorResult(allocator, "runtime_setup", "missing entry module", null, null);
            }
            try stderr.writeAll("Runtime setup error: missing entry module\n");
            return error.TestsFailed;
        };
        const entry_ast = entry_mod.module_ast orelse {
            if (options.json_output) {
                return makeSingleCompileErrorResult(allocator, "runtime_setup", "missing entry module AST", null, null);
            }
            try stderr.writeAll("Runtime setup error: missing entry module AST\n");
            return error.TestsFailed;
        };
        const entry_interp = module_interpreters.get(entry_mod) orelse {
            if (options.json_output) {
                return makeSingleCompileErrorResult(allocator, "runtime_setup", "missing entry interpreter", null, null);
            }
            try stderr.writeAll("Runtime setup error: missing entry interpreter\n");
            return error.TestsFailed;
        };

        var total_tests: usize = 0;
        for (entry_ast.declarations) |decl| {
            if (decl != .test_decl) continue;
            const test_decl = decl.test_decl;
            if (options.fn_filter) |fn_name| {
                if (!std.mem.eql(u8, test_decl.name, fn_name)) continue;
            }
            total_tests += 1;
        }

        if (total_tests == 0) {
            var buf: [512]u8 = undefined;
            const msg = if (options.fn_filter) |fn_name|
                std.fmt.bufPrint(&buf, "No tests matched '{s}' in '{s}'\n", .{ fn_name, path }) catch "No tests matched\n"
            else
                std.fmt.bufPrint(&buf, "No tests found in '{s}'\n", .{path}) catch "No tests found\n";
            if (options.emit_output and !options.json_output) {
                try stdout.writeAll(msg);
            }
            if (options.require_tests) {
                const err_msg = std.fmt.bufPrint(&buf, "Error: --require-tests enabled, but no tests found in '{s}'\n", .{path}) catch "Error: no tests found\n";
                try stderr.writeAll(err_msg);
                return error.TestsFailed;
            }
            if (options.strict_tests) {
                const warn_msg = std.fmt.bufPrint(&buf, "Warning: no tests found in '{s}'\n", .{path}) catch "Warning: no tests found\n";
                try stderr.writeAll(warn_msg);
            }
            if (options.emit_output and options.json_output) {
                try stdout.writeAll("{\"file\":");
                try writeJsonEscaped(stdout, path);
                try stdout.writeAll(",\"total\":0,\"passed\":0,\"failed\":0,\"tests\":[]}\n");
            }
            return .{
                .tests_discovered = 0,
                .tests_passed = 0,
                .tests_failed = 0,
                .test_results = try allocator.alloc(JsonTestSummary, 0),
                .compile_errors = try allocator.alloc(JsonCompileError, 0),
            };
        }

        var passed: usize = 0;
        var failed: usize = 0;
        var assertion_recorder = AssertionRecorder.init(allocator);
        defer assertion_recorder.deinit();
        const record_assertions = options.json_output;
        if (record_assertions) {
            interp_mod.setAssertionRecorder(&assertion_recorder);
        }
        defer if (record_assertions) interp_mod.setAssertionRecorder(null);

        var test_results = std.ArrayListUnmanaged(JsonTestSummary){};
        var test_results_transferred = false;
        defer {
            if (!test_results_transferred) {
                for (test_results.items) |test_result| {
                    allocator.free(test_result.name);
                    if (test_result.error_name) |error_name| allocator.free(error_name);
                    if (test_result.source) |test_source| allocator.free(test_source);
                    for (test_result.assertions) |assertion| {
                        if (assertion.expected) |expected| allocator.free(expected);
                        if (assertion.actual) |actual| allocator.free(actual);
                    }
                    allocator.free(test_result.assertions);
                }
            }
            test_results.deinit(allocator);
        }

        var buf: [512]u8 = undefined;
        if (options.emit_output and !options.json_output) {
            const start_msg = std.fmt.bufPrint(&buf, "Running {d} test(s)\n", .{total_tests}) catch "Running tests\n";
            try stdout.writeAll(start_msg);
        }

        for (entry_ast.declarations) |decl| {
            if (decl != .test_decl) continue;
            const test_decl = decl.test_decl;
            if (options.fn_filter) |fn_name| {
                if (!std.mem.eql(u8, test_decl.name, fn_name)) continue;
            }

            if (record_assertions) {
                const test_source = if (options.include_source) blk: {
                    if (findFunctionSource(entry_ast, source, test_decl.name)) |function_source| {
                        break :blk try allocator.dupe(u8, function_source);
                    }
                    break :blk null;
                } else null;
                errdefer if (test_source) |owned_source| allocator.free(owned_source);
                assertion_recorder.clear();
                if (entry_interp.evalBlock(test_decl.body)) |_| {
                    const assertion_results = try duplicateAssertionRecords(allocator, assertion_recorder.records.items);
                    const pass_msg = std.fmt.bufPrint(&buf, "  PASS {s}\n", .{test_decl.name}) catch "  PASS\n";
                    if (options.emit_output and !options.json_output) {
                        try stdout.writeAll(pass_msg);
                    }
                    try test_results.append(allocator, .{
                        .name = try allocator.dupe(u8, test_decl.name),
                        .passed = true,
                        .assertions = assertion_results,
                        .source = test_source,
                    });
                    passed += 1;
                } else |err| {
                    const assertion_results = try duplicateAssertionRecords(allocator, assertion_recorder.records.items);
                    const fail_msg = std.fmt.bufPrint(&buf, "  FAIL {s}: {s}\n", .{ test_decl.name, @errorName(err) }) catch "  FAIL\n";
                    if (options.emit_output and !options.json_output) {
                        try stdout.writeAll(fail_msg);
                    }
                    try test_results.append(allocator, .{
                        .name = try allocator.dupe(u8, test_decl.name),
                        .passed = false,
                        .error_name = try allocator.dupe(u8, @errorName(err)),
                        .assertions = assertion_results,
                        .source = test_source,
                    });
                    failed += 1;
                }
            } else {
                if (entry_interp.evalBlock(test_decl.body)) |_| {
                    const pass_msg = std.fmt.bufPrint(&buf, "  PASS {s}\n", .{test_decl.name}) catch "  PASS\n";
                    if (options.emit_output) {
                        try stdout.writeAll(pass_msg);
                    }
                    passed += 1;
                } else |err| {
                    const fail_msg = std.fmt.bufPrint(&buf, "  FAIL {s}: {s}\n", .{ test_decl.name, @errorName(err) }) catch "  FAIL\n";
                    if (options.emit_output) {
                        try stdout.writeAll(fail_msg);
                    }
                    failed += 1;
                }
            }
        }

        if (options.emit_output and options.json_output) {
            try stdout.writeAll("{\"file\":");
            try writeJsonEscaped(stdout, path);
            try stdout.writeAll(",\"total\":");
            try writeJsonUsize(stdout, total_tests);
            try stdout.writeAll(",\"passed\":");
            try writeJsonUsize(stdout, passed);
            try stdout.writeAll(",\"failed\":");
            try writeJsonUsize(stdout, failed);
            try stdout.writeAll(",\"tests\":[");
            for (test_results.items, 0..) |test_result, idx| {
                if (idx > 0) try stdout.writeAll(",");
                try stdout.writeAll("{\"name\":");
                try writeJsonEscaped(stdout, test_result.name);
                try stdout.writeAll(",\"status\":");
                try writeJsonEscaped(stdout, if (test_result.passed) "PASS" else "FAIL");
                if (test_result.error_name) |error_name| {
                    try stdout.writeAll(",\"error\":");
                    try writeJsonEscaped(stdout, error_name);
                }
                if (test_result.source) |test_source| {
                    try stdout.writeAll(",\"source\":");
                    try writeJsonEscaped(stdout, test_source);
                }
                try stdout.writeAll(",\"assertions\":[");
                for (test_result.assertions, 0..) |assertion, assertion_idx| {
                    if (assertion_idx > 0) try stdout.writeAll(",");
                    try stdout.writeAll("{\"type\":");
                    try writeJsonEscaped(stdout, assertion.assertion_type);
                    try stdout.writeAll(",\"passed\":");
                    try writeJsonBool(stdout, assertion.passed);
                    if (assertion.expected) |expected| {
                        try stdout.writeAll(",\"expected\":");
                        try writeJsonEscaped(stdout, expected);
                    }
                    if (assertion.actual) |actual| {
                        try stdout.writeAll(",\"actual\":");
                        try writeJsonEscaped(stdout, actual);
                    }
                    try stdout.writeAll("}");
                }
                try stdout.writeAll("]}");
            }
            try stdout.writeAll("],\"errors\":[]}\n");
        } else if (options.emit_output) {
            const summary = std.fmt.bufPrint(&buf, "Test result: {d} passed, {d} failed\n", .{ passed, failed }) catch "Test result\n";
            try stdout.writeAll(summary);
        }

        test_results_transferred = true;
        const owned_test_results = if (record_assertions)
            try test_results.toOwnedSlice(allocator)
        else
            try allocator.alloc(JsonTestSummary, 0);
        return .{
            .tests_discovered = total_tests,
            .tests_passed = passed,
            .tests_failed = failed,
            .test_results = owned_test_results,
            .compile_errors = try allocator.alloc(JsonCompileError, 0),
        };
    } else {
        checker.checkModule(module);

        if (checker.hasErrors()) {
            if (options.json_output) {
                const compile_errors = try allocator.alloc(JsonCompileError, checker.error_count);
                var compile_errors_initialized: usize = 0;
                errdefer {
                    for (compile_errors[0..compile_errors_initialized]) |compile_error| {
                        allocator.free(compile_error.message);
                    }
                    allocator.free(compile_errors);
                }
                for (checker.errors.items) |check_err| {
                    if (check_err.kind == .meta_warning) continue;
                    compile_errors[compile_errors_initialized] = .{
                        .stage = "type_check",
                        .message = try allocator.dupe(u8, check_err.message),
                        .line = check_err.span.line,
                        .column = check_err.span.column,
                    };
                    compile_errors_initialized += 1;
                }
                const empty_test_results = try allocator.alloc(JsonTestSummary, 0);
                return .{
                    .tests_discovered = 0,
                    .tests_passed = 0,
                    .tests_failed = 0,
                    .test_results = empty_test_results,
                    .compile_errors = compile_errors,
                    .file_failed = true,
                };
            }
            var buf: [512]u8 = undefined;
            const header = std.fmt.bufPrint(&buf, "Type error(s):\n", .{}) catch "Type errors:\n";
            try stderr.writeAll(header);

            for (checker.errors.items) |check_err| {
                if (check_err.kind == .meta_warning) continue;
                const err_msg = std.fmt.bufPrint(&buf, "  {d}:{d}: {s}\n", .{
                    check_err.span.line,
                    check_err.span.column,
                    check_err.message,
                }) catch continue;
                try stderr.writeAll(err_msg);
            }
            return error.TestsFailed;
        }

        var interp = try Interpreter.init(allocator);
        defer interp.deinit();

        interp.executeModule(module) catch |err| {
            if (options.json_output) {
                var msg_buf: [128]u8 = undefined;
                const msg = std.fmt.bufPrint(&msg_buf, "Runtime setup error: {s}", .{@errorName(err)}) catch "Runtime setup error";
                return makeSingleCompileErrorResult(allocator, "runtime_setup", msg, null, null);
            }
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Runtime setup error: {s}\n", .{@errorName(err)}) catch "Runtime setup error\n";
            try stderr.writeAll(msg);
            return error.TestsFailed;
        };

        var total_tests: usize = 0;
        for (module.declarations) |decl| {
            if (decl != .test_decl) continue;
            const test_decl = decl.test_decl;
            if (options.fn_filter) |fn_name| {
                if (!std.mem.eql(u8, test_decl.name, fn_name)) continue;
            }
            total_tests += 1;
        }

        if (total_tests == 0) {
            var buf: [512]u8 = undefined;
            const msg = if (options.fn_filter) |fn_name|
                std.fmt.bufPrint(&buf, "No tests matched '{s}' in '{s}'\n", .{ fn_name, path }) catch "No tests matched\n"
            else
                std.fmt.bufPrint(&buf, "No tests found in '{s}'\n", .{path}) catch "No tests found\n";
            if (options.emit_output and !options.json_output) {
                try stdout.writeAll(msg);
            }
            if (options.require_tests) {
                const err_msg = std.fmt.bufPrint(&buf, "Error: --require-tests enabled, but no tests found in '{s}'\n", .{path}) catch "Error: no tests found\n";
                try stderr.writeAll(err_msg);
                return error.TestsFailed;
            }
            if (options.strict_tests) {
                const warn_msg = std.fmt.bufPrint(&buf, "Warning: no tests found in '{s}'\n", .{path}) catch "Warning: no tests found\n";
                try stderr.writeAll(warn_msg);
            }
            if (options.emit_output and options.json_output) {
                try stdout.writeAll("{\"file\":");
                try writeJsonEscaped(stdout, path);
                try stdout.writeAll(",\"total\":0,\"passed\":0,\"failed\":0,\"tests\":[]}\n");
            }
            return .{
                .tests_discovered = 0,
                .tests_passed = 0,
                .tests_failed = 0,
                .test_results = try allocator.alloc(JsonTestSummary, 0),
                .compile_errors = try allocator.alloc(JsonCompileError, 0),
            };
        }

        var passed: usize = 0;
        var failed: usize = 0;
        var assertion_recorder = AssertionRecorder.init(allocator);
        defer assertion_recorder.deinit();
        const record_assertions = options.json_output;
        if (record_assertions) {
            interp_mod.setAssertionRecorder(&assertion_recorder);
        }
        defer if (record_assertions) interp_mod.setAssertionRecorder(null);

        var test_results = std.ArrayListUnmanaged(JsonTestSummary){};
        var test_results_transferred = false;
        defer {
            if (!test_results_transferred) {
                for (test_results.items) |test_result| {
                    allocator.free(test_result.name);
                    if (test_result.error_name) |error_name| allocator.free(error_name);
                    if (test_result.source) |test_source| allocator.free(test_source);
                    for (test_result.assertions) |assertion| {
                        if (assertion.expected) |expected| allocator.free(expected);
                        if (assertion.actual) |actual| allocator.free(actual);
                    }
                    allocator.free(test_result.assertions);
                }
            }
            test_results.deinit(allocator);
        }

        var buf: [512]u8 = undefined;

        if (options.emit_output and !options.json_output) {
            const start_msg = std.fmt.bufPrint(&buf, "Running {d} test(s)\n", .{total_tests}) catch "Running tests\n";
            try stdout.writeAll(start_msg);
        }

        for (module.declarations) |decl| {
            if (decl != .test_decl) continue;
            const test_decl = decl.test_decl;
            if (options.fn_filter) |fn_name| {
                if (!std.mem.eql(u8, test_decl.name, fn_name)) continue;
            }

            if (record_assertions) {
                const test_source = if (options.include_source) blk: {
                    if (findFunctionSource(module, source, test_decl.name)) |function_source| {
                        break :blk try allocator.dupe(u8, function_source);
                    }
                    break :blk null;
                } else null;
                errdefer if (test_source) |owned_source| allocator.free(owned_source);
                assertion_recorder.clear();
                if (interp.evalBlock(test_decl.body)) |_| {
                    const assertion_results = try duplicateAssertionRecords(allocator, assertion_recorder.records.items);
                    const pass_msg = std.fmt.bufPrint(&buf, "  PASS {s}\n", .{test_decl.name}) catch "  PASS\n";
                    if (options.emit_output and !options.json_output) {
                        try stdout.writeAll(pass_msg);
                    }
                    try test_results.append(allocator, .{
                        .name = try allocator.dupe(u8, test_decl.name),
                        .passed = true,
                        .assertions = assertion_results,
                        .source = test_source,
                    });
                    passed += 1;
                } else |err| {
                    const assertion_results = try duplicateAssertionRecords(allocator, assertion_recorder.records.items);
                    const fail_msg = std.fmt.bufPrint(&buf, "  FAIL {s}: {s}\n", .{ test_decl.name, @errorName(err) }) catch "  FAIL\n";
                    if (options.emit_output and !options.json_output) {
                        try stdout.writeAll(fail_msg);
                    }
                    try test_results.append(allocator, .{
                        .name = try allocator.dupe(u8, test_decl.name),
                        .passed = false,
                        .error_name = try allocator.dupe(u8, @errorName(err)),
                        .assertions = assertion_results,
                        .source = test_source,
                    });
                    failed += 1;
                }
            } else {
                if (interp.evalBlock(test_decl.body)) |_| {
                    const pass_msg = std.fmt.bufPrint(&buf, "  PASS {s}\n", .{test_decl.name}) catch "  PASS\n";
                    if (options.emit_output) {
                        try stdout.writeAll(pass_msg);
                    }
                    passed += 1;
                } else |err| {
                    const fail_msg = std.fmt.bufPrint(&buf, "  FAIL {s}: {s}\n", .{ test_decl.name, @errorName(err) }) catch "  FAIL\n";
                    if (options.emit_output) {
                        try stdout.writeAll(fail_msg);
                    }
                    failed += 1;
                }
            }
        }

        if (options.emit_output and options.json_output) {
            try stdout.writeAll("{\"file\":");
            try writeJsonEscaped(stdout, path);
            try stdout.writeAll(",\"total\":");
            try writeJsonUsize(stdout, total_tests);
            try stdout.writeAll(",\"passed\":");
            try writeJsonUsize(stdout, passed);
            try stdout.writeAll(",\"failed\":");
            try writeJsonUsize(stdout, failed);
            try stdout.writeAll(",\"tests\":[");
            for (test_results.items, 0..) |test_result, idx| {
                if (idx > 0) try stdout.writeAll(",");
                try stdout.writeAll("{\"name\":");
                try writeJsonEscaped(stdout, test_result.name);
                try stdout.writeAll(",\"status\":");
                try writeJsonEscaped(stdout, if (test_result.passed) "PASS" else "FAIL");
                if (test_result.error_name) |error_name| {
                    try stdout.writeAll(",\"error\":");
                    try writeJsonEscaped(stdout, error_name);
                }
                if (test_result.source) |test_source| {
                    try stdout.writeAll(",\"source\":");
                    try writeJsonEscaped(stdout, test_source);
                }
                try stdout.writeAll(",\"assertions\":[");
                for (test_result.assertions, 0..) |assertion, assertion_idx| {
                    if (assertion_idx > 0) try stdout.writeAll(",");
                    try stdout.writeAll("{\"type\":");
                    try writeJsonEscaped(stdout, assertion.assertion_type);
                    try stdout.writeAll(",\"passed\":");
                    try writeJsonBool(stdout, assertion.passed);
                    if (assertion.expected) |expected| {
                        try stdout.writeAll(",\"expected\":");
                        try writeJsonEscaped(stdout, expected);
                    }
                    if (assertion.actual) |actual| {
                        try stdout.writeAll(",\"actual\":");
                        try writeJsonEscaped(stdout, actual);
                    }
                    try stdout.writeAll("}");
                }
                try stdout.writeAll("]}");
            }
                try stdout.writeAll("],\"errors\":[]}\n");
        } else if (options.emit_output) {
            const summary = std.fmt.bufPrint(&buf, "Test result: {d} passed, {d} failed\n", .{ passed, failed }) catch "Test result\n";
            try stdout.writeAll(summary);
        }

        test_results_transferred = true;
        const owned_test_results = if (record_assertions)
            try test_results.toOwnedSlice(allocator)
        else
            try allocator.alloc(JsonTestSummary, 0);
        return .{
            .tests_discovered = total_tests,
            .tests_passed = passed,
            .tests_failed = failed,
            .test_results = owned_test_results,
            .compile_errors = try allocator.alloc(JsonCompileError, 0),
        };
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
        const compilation_order = resolver.getCompilationOrder() catch {
            return;
        };
        defer allocator.free(compilation_order);

        _ = warnCircularImports(&resolver, stderr);

        // Set up checker with module resolver
        checker.setModuleResolver(&resolver);

        // Phase 1: Register all declarations and export symbols
        for (compilation_order) |mod| {
            if (mod.module_ast) |mod_ast| {
                checker.prepareForNewModule();
                checker.setCurrentModule(mod);
                checker.checkModuleDeclarations(mod_ast);
                checker.registerModuleExports(mod) catch {};
                checker.saveModuleScope(mod);
            }
        }
        // Phase 2: Check all bodies (all exports now available)
        for (compilation_order) |mod| {
            if (mod.module_ast) |_| {
                checker.restoreModuleScope(mod);
                checker.setCurrentModule(mod);
                checker.checkModuleBodies(mod.module_ast.?);
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
            if (check_err.kind == .meta_warning) continue;
            const err_msg = std.fmt.bufPrint(&buf, "  {d}:{d}: {s}\n", .{
                check_err.span.line,
                check_err.span.column,
                check_err.message,
            }) catch continue;
            try stderr.writeAll(err_msg);
        }
        try printMetaWarnings(&checker, stderr);
        return;
    }
    try printMetaWarnings(&checker, stderr);

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
        std.process.exit(1);
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
                std.process.exit(1);
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
            if (check_err.kind == .meta_warning) continue;
            const err_msg = std.fmt.bufPrint(&buf, "  {d}:{d}: {s}\n", .{
                check_err.span.line,
                check_err.span.column,
                check_err.message,
            }) catch continue;
            try stderr.writeAll(err_msg);
        }
        try printMetaWarnings(&checker, stderr);
        return;
    }
    try printMetaWarnings(&checker, stderr);

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

/// Result of dependency resolution with lock file handling.
const DependencyResolution = struct {
    allocator: std.mem.Allocator,
    /// Resolved paths for module search.
    paths: std.ArrayListUnmanaged([]const u8),
    /// Whether lock file was generated this run.
    generated_lockfile: bool,

    pub fn deinit(self: *DependencyResolution) void {
        for (self.paths.items) |p| self.allocator.free(p);
        self.paths.deinit(self.allocator);
    }
};

/// Get version string from a dependency's klar.json manifest.
/// Returns null if manifest doesn't exist or version can't be read.
/// Caller owns the returned string and must free it.
fn getDepVersion(allocator: std.mem.Allocator, dep_path: []const u8) ?[]const u8 {
    const dep_manifest_path = std.fs.path.join(allocator, &.{ dep_path, "klar.json" }) catch return null;
    defer allocator.free(dep_manifest_path);

    var dm = manifest.loadManifest(allocator, dep_manifest_path) catch return null;
    defer dm.deinit();

    return dm.package.version.toString(allocator) catch null;
}

/// Resolve dependencies using lock file for reproducibility.
/// If lock file doesn't exist, generates it. If it exists, uses locked versions.
/// Returns resolved paths and whether lock file was newly generated.
fn resolveDependencies(
    allocator: std.mem.Allocator,
    loaded_manifest: *const manifest.Manifest,
    root_dir: []const u8,
) !DependencyResolution {
    const stderr = getStdErr();
    var result = DependencyResolution{
        .allocator = allocator,
        .paths = .{},
        .generated_lockfile = false,
    };
    errdefer result.deinit();

    // Check if klar.lock exists
    const lockfile_path = std.fs.path.join(allocator, &.{ root_dir, "klar.lock" }) catch {
        return resolveDependenciesWithoutLock(allocator, loaded_manifest);
    };
    defer allocator.free(lockfile_path);

    var existing_lock: ?lockfile.Lockfile = null;
    defer if (existing_lock) |*lf| lf.deinit();

    existing_lock = lockfile.loadLockfile(allocator, lockfile_path) catch |err| blk: {
        switch (err) {
            error.FileNotFound => {},
            error.UnsupportedVersion => {
                stderr.writeAll("Warning: klar.lock version is newer than this Klar version, regenerating\n") catch {};
            },
            else => {
                stderr.writeAll("Warning: failed to read klar.lock, regenerating\n") catch {};
            },
        }
        break :blk null;
    };

    if (existing_lock) |*lf| {
        // Check for mismatches between manifest and lock file
        const mismatched = lf.checkMismatch(&loaded_manifest.dependencies, allocator) catch {
            try stderr.writeAll("Warning: failed to check lock file consistency\n");
            return resolveDependenciesWithoutLock(allocator, loaded_manifest);
        };
        defer {
            for (mismatched) |m| allocator.free(m);
            allocator.free(mismatched);
        }

        if (mismatched.len > 0) {
            try stderr.writeAll("Error: klar.lock is out of date with klar.json\n");
            try stderr.writeAll("  Changed dependencies: ");
            for (mismatched, 0..) |name, i| {
                if (i > 0) try stderr.writeAll(", ");
                try stderr.writeAll(name);
            }
            try stderr.writeAll("\n  Run 'klar update' to regenerate the lock file\n");
            return error.LockfileMismatch;
        }

        // Use locked paths
        var lock_it = lf.dependencies.iterator();
        while (lock_it.next()) |entry| {
            const dep = entry.value_ptr.*;
            // Verify the resolved path still exists
            var path_buf: [std.fs.max_path_bytes]u8 = undefined;
            if (std.fs.cwd().realpath(dep.resolved, &path_buf)) |resolved| {
                try result.paths.append(allocator, try allocator.dupe(u8, resolved));
            } else |_| {
                // Path no longer exists, re-resolve from original
                if (dep.source == .path) {
                    const abs_path = std.fs.path.join(allocator, &.{ root_dir, dep.original }) catch continue;
                    defer allocator.free(abs_path);

                    if (std.fs.cwd().realpath(abs_path, &path_buf)) |resolved| {
                        try result.paths.append(allocator, try allocator.dupe(u8, resolved));
                    } else |_| {
                        var buf: [512]u8 = undefined;
                        const msg = std.fmt.bufPrint(&buf, "Warning: dependency path not found: {s}\n", .{dep.original}) catch "Warning: dependency path not found\n";
                        try stderr.writeAll(msg);
                    }
                }
            }
        }
    } else {
        // No lock file exists, resolve and generate one
        var new_lock = lockfile.Lockfile.init(allocator);
        errdefer new_lock.deinit();

        var dep_it = loaded_manifest.dependencies.iterator();
        while (dep_it.next()) |entry| {
            const name = entry.key_ptr.*;
            const dep = entry.value_ptr.*;

            if (dep.path) |rel_path| {
                // Resolve path relative to manifest directory
                const abs_path = std.fs.path.join(allocator, &.{ root_dir, rel_path }) catch continue;
                defer allocator.free(abs_path);

                // Resolve to canonical path
                var path_buf: [std.fs.max_path_bytes]u8 = undefined;
                if (std.fs.cwd().realpath(abs_path, &path_buf)) |resolved| {
                    try result.paths.append(allocator, try allocator.dupe(u8, resolved));

                    // Try to get version from dependency's manifest
                    const dep_version = getDepVersion(allocator, resolved);
                    defer if (dep_version) |v| allocator.free(v);

                    try new_lock.addDependency(name, .{
                        .source = .path,
                        .original = rel_path,
                        .resolved = resolved,
                        .version = dep_version,
                    });
                } else |_| {
                    // Path doesn't exist - this is an error, not a warning
                    var buf: [512]u8 = undefined;
                    const msg = std.fmt.bufPrint(&buf, "Error: dependency '{s}' path not found: {s}\n", .{ name, rel_path }) catch "Error: dependency path not found\n";
                    try stderr.writeAll(msg);
                    return error.DependencyNotFound;
                }
            }
        }

        // Save lock file
        if (new_lock.dependencies.count() > 0) {
            lockfile.saveLockfile(&new_lock, lockfile_path) catch |err| {
                var buf: [256]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Warning: failed to save klar.lock: {s}\n", .{@errorName(err)}) catch "Warning: failed to save klar.lock\n";
                try stderr.writeAll(msg);
            };
            result.generated_lockfile = true;
        }
        new_lock.deinit();
    }

    return result;
}

/// Fallback: resolve dependencies without lock file.
fn resolveDependenciesWithoutLock(
    allocator: std.mem.Allocator,
    loaded_manifest: *const manifest.Manifest,
) !DependencyResolution {
    var result = DependencyResolution{
        .allocator = allocator,
        .paths = .{},
        .generated_lockfile = false,
    };
    errdefer result.deinit();

    var dep_it = loaded_manifest.dependencies.iterator();
    while (dep_it.next()) |entry| {
        const dep = entry.value_ptr.*;
        if (dep.path) |rel_path| {
            const abs_path = std.fs.path.join(allocator, &.{ loaded_manifest.root_dir, rel_path }) catch continue;
            defer allocator.free(abs_path);

            var path_buf: [std.fs.max_path_bytes]u8 = undefined;
            if (std.fs.cwd().realpath(abs_path, &path_buf)) |resolved| {
                try result.paths.append(allocator, try allocator.dupe(u8, resolved));
            } else |_| {
                try result.paths.append(allocator, try allocator.dupe(u8, abs_path));
            }
        }
    }

    return result;
}

/// Update (regenerate) the lock file from klar.json.
fn updateLockfile(allocator: std.mem.Allocator) !void {
    const stdout = getStdOut();
    const stderr = getStdErr();

    // Load manifest
    var loaded_manifest = switch (loadManifestWithErrors(allocator, "klar.json", "update")) {
        .success => |m| m,
        .failure => return,
    };
    defer loaded_manifest.deinit();

    // Delete existing lock file if it exists
    std.fs.cwd().deleteFile("klar.lock") catch |err| switch (err) {
        error.FileNotFound => {}, // OK, doesn't exist
        else => {
            try stderr.writeAll("Warning: could not delete existing klar.lock\n");
        },
    };

    // Generate new lock file
    var new_lock = lockfile.Lockfile.init(allocator);
    defer new_lock.deinit();

    var updated_count: usize = 0;
    var dep_it = loaded_manifest.dependencies.iterator();
    while (dep_it.next()) |entry| {
        const name = entry.key_ptr.*;
        const dep = entry.value_ptr.*;

        if (dep.path) |rel_path| {
            // Resolve path relative to manifest directory
            const abs_path = std.fs.path.join(allocator, &.{ loaded_manifest.root_dir, rel_path }) catch continue;
            defer allocator.free(abs_path);

            // Resolve to canonical path
            var path_buf: [std.fs.max_path_bytes]u8 = undefined;
            if (std.fs.cwd().realpath(abs_path, &path_buf)) |resolved| {
                // Try to get version from dependency's manifest
                const dep_version = getDepVersion(allocator, resolved);
                defer if (dep_version) |v| allocator.free(v);

                new_lock.addDependency(name, .{
                    .source = .path,
                    .original = rel_path,
                    .resolved = resolved,
                    .version = dep_version,
                }) catch continue;

                updated_count += 1;

                // Print update message
                var buf: [512]u8 = undefined;
                if (dep_version) |v| {
                    const msg = std.fmt.bufPrint(&buf, "  {s} ({s}) -> {s}\n", .{ name, v, resolved }) catch continue;
                    stdout.writeAll(msg) catch {};
                } else {
                    const msg = std.fmt.bufPrint(&buf, "  {s} -> {s}\n", .{ name, resolved }) catch continue;
                    stdout.writeAll(msg) catch {};
                }
            } else |_| {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Error: dependency '{s}' path not found: {s}\n", .{ name, rel_path }) catch "Error: dependency path not found\n";
                stderr.writeAll(msg) catch {};
            }
        }
    }

    // Save lock file
    if (updated_count > 0) {
        lockfile.saveLockfile(&new_lock, "klar.lock") catch |err| {
            var buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Error: failed to save klar.lock: {s}\n", .{@errorName(err)}) catch "Error: failed to save klar.lock\n";
            stderr.writeAll(msg) catch {};
            return;
        };

        var buf: [128]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Updated klar.lock with {d} dependencies\n", .{updated_count}) catch "Updated klar.lock\n";
        stdout.writeAll(msg) catch {};
    } else if (loaded_manifest.dependencies.count() == 0) {
        stdout.writeAll("No dependencies to lock\n") catch {};
    } else {
        stderr.writeAll("Warning: no dependencies could be resolved\n") catch {};
    }
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

// Meta command is in meta_query.zig


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
        \\  update               Regenerate klar.lock from klar.json
        \\  check <file>         Type check a file
        \\    --dump-ownership   Include ownership analysis details
        \\    --partial          Parse with recovery and type-check partial declarations
        \\    --scope-at L:C     Print in-scope bindings at line/column
        \\    --expected-type-at L:C  Print expected type at line/column
        \\    --scope-json       Emit scope output as JSON (with --scope-at)
        \\  repl                 Interactive REPL (Read-Eval-Print Loop)
        \\  test <path>          Run inline test blocks in a file or directory
        \\    --fn <name>        Run only tests matching <name>
        \\    --strict-tests     Warn when no tests are found
        \\    --require-tests    Fail when no tests are found
        \\    --json             Emit machine-readable JSON summary
        \\    --include-source   Include matched function source in JSON test results
        \\  fmt [file|dir]       Format source files
        \\    -i                  Format files in-place
        \\    --check             Check formatting (exit 1 if unformatted)
        \\  meta [path]          Query meta annotations across files
        \\    --tag "name"       Find declarations with a specific tag
        \\    --module           List all module descriptions
        \\    --related fn_name  Follow cross-references for a function
        \\    --deprecated       List all deprecated items
        \\    --hints            List all AI hints
        \\    --json             Machine-readable JSON output
        \\  lsp                  Run LSP JSON-RPC transport over stdio
        \\  help                 Show this help
        \\  version              Show version
        \\  tokenize <file>      Tokenize a file (debug)
        \\  parse <file>         Parse a file (debug)
        \\  dump-tokens <file>   Dump token stream as JSON (parity testing)
        \\  dump-ast <file>      Dump AST as JSON (parity testing)
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
    _ = @import("checker/mod.zig");
    _ = @import("values.zig");
    _ = @import("interpreter.zig");
    _ = @import("bytecode.zig");
    _ = @import("pkg/manifest.zig");
    _ = @import("pkg/lockfile.zig");
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
    _ = @import("lsp.zig");
    _ = @import("meta_query.zig");
}
