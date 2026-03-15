const std = @import("std");
const builtin = @import("builtin");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Detect LLVM installation
    // Skip LLVM when cross-compiling: host LLVM libraries can't be used for a
    // different target (e.g., Linux LLVM can't produce Windows COFF objects).
    const is_cross_compiling = target.result.os.tag != builtin.os.tag or
        target.result.cpu.arch != builtin.cpu.arch;
    const llvm_prefix = if (is_cross_compiling) null else detectLLVMPrefix();
    const has_llvm = llvm_prefix != null;

    // Build options module (pass has_llvm to source code)
    const build_options = b.addOptions();
    build_options.addOption(bool, "has_llvm", has_llvm);

    // Create the main module
    const main_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "build_options", .module = build_options.createModule() },
        },
    });

    // Create executable using the module
    const exe = b.addExecutable(.{
        .name = "klar",
        .root_module = main_mod,
    });

    // Add LLVM include and library paths
    if (llvm_prefix) |prefix| {
        exe.addIncludePath(.{ .cwd_relative = b.fmt("{s}/include", .{prefix}) });
        exe.addLibraryPath(.{ .cwd_relative = b.fmt("{s}/lib", .{prefix}) });
        // Windows: LLVM-C.lib/dll (vovkos packages); macOS/Linux: libLLVM.dylib/.so
        if (builtin.os.tag == .windows) {
            exe.linkSystemLibrary("LLVM-C");
        } else {
            exe.linkSystemLibrary("LLVM");
        }
        exe.linkLibC();
    }

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the Klar compiler");
    run_step.dependOn(&run_cmd.step);

    // Create test module
    const test_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "build_options", .module = build_options.createModule() },
        },
    });

    const unit_tests = b.addTest(.{
        .root_module = test_mod,
    });

    // Add LLVM to tests as well
    if (llvm_prefix) |prefix| {
        unit_tests.addIncludePath(.{ .cwd_relative = b.fmt("{s}/include", .{prefix}) });
        unit_tests.addLibraryPath(.{ .cwd_relative = b.fmt("{s}/lib", .{prefix}) });
        if (builtin.os.tag == .windows) {
            unit_tests.linkSystemLibrary("LLVM-C");
        } else {
            unit_tests.linkSystemLibrary("LLVM");
        }
        unit_tests.linkLibC();
    }

    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);

    // Fuzz tests — parser/lexer only (no LLVM dependency)
    // Share module instances so each .zig file belongs to exactly one module.
    // Use "file.zig" names to match @import("file.zig") in source files.
    const token_mod = b.createModule(.{ .root_source_file = b.path("src/token.zig"), .target = target, .optimize = optimize });
    const ast_mod = b.createModule(.{ .root_source_file = b.path("src/ast.zig"), .target = target, .optimize = optimize, .imports = &.{.{ .name = "token.zig", .module = token_mod }} });
    const lexer_mod = b.createModule(.{ .root_source_file = b.path("src/lexer.zig"), .target = target, .optimize = optimize, .imports = &.{.{ .name = "token.zig", .module = token_mod }} });
    const parser_mod = b.createModule(.{ .root_source_file = b.path("src/parser.zig"), .target = target, .optimize = optimize, .imports = &.{ .{ .name = "token.zig", .module = token_mod }, .{ .name = "lexer.zig", .module = lexer_mod }, .{ .name = "ast.zig", .module = ast_mod } } });
    const fuzz_mod = b.createModule(.{
        .root_source_file = b.path("tests/fuzz/parser_fuzz.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "lexer", .module = lexer_mod },
            .{ .name = "parser", .module = parser_mod },
            .{ .name = "token", .module = token_mod },
        },
    });

    const fuzz_tests = b.addTest(.{
        .root_module = fuzz_mod,
    });

    const run_fuzz_tests = b.addRunArtifact(fuzz_tests);
    const fuzz_step = b.step("fuzz", "Run parser fuzz tests");
    fuzz_step.dependOn(&run_fuzz_tests.step);

    // Standalone stress test — generates random inputs (no --fuzz dependency)
    const stress_mod = b.createModule(.{
        .root_source_file = b.path("tests/fuzz/parser_stress.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "lexer", .module = lexer_mod },
            .{ .name = "parser", .module = parser_mod },
            .{ .name = "token", .module = token_mod },
        },
    });

    const stress_exe = b.addExecutable(.{
        .name = "parser-stress",
        .root_module = stress_mod,
    });

    const run_stress = b.addRunArtifact(stress_exe);
    if (b.args) |args| {
        run_stress.addArgs(args);
    }
    const stress_step = b.step("fuzz-stress", "Run parser stress test (default 1M inputs)");
    stress_step.dependOn(&run_stress.step);

    // Property-based type checker test (standalone, uses klar binary via subprocess)
    const property_exe = b.addExecutable(.{
        .name = "checker-property",
        .root_module = b.createModule(.{
            .root_source_file = b.path("tests/property/checker_property.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const run_property = b.addRunArtifact(property_exe);
    if (b.args) |args| {
        run_property.addArgs(args);
    }
    const property_step = b.step("property-test", "Run type checker property tests (default 10K programs)");
    property_step.dependOn(&run_property.step);
}

/// Detect LLVM installation path.
/// Checks environment variable first, then known locations.
fn detectLLVMPrefix() ?[]const u8 {
    // Try environment variable first
    if (std.process.getEnvVarOwned(std.heap.page_allocator, "LLVM_PREFIX")) |prefix| {
        return prefix;
    } else |_| {}

    // macOS ARM64 (Apple Silicon) - Homebrew (unversioned)
    if (std.fs.accessAbsolute("/opt/homebrew/opt/llvm/include/llvm-c/Core.h", .{})) |_| {
        return "/opt/homebrew/opt/llvm";
    } else |_| {}

    // macOS x86_64 (Intel) - Homebrew (unversioned)
    if (std.fs.accessAbsolute("/usr/local/opt/llvm/include/llvm-c/Core.h", .{})) |_| {
        return "/usr/local/opt/llvm";
    } else |_| {}

    // macOS - Homebrew versioned (e.g., brew install llvm@17)
    {
        const versions = [_][]const u8{ "20", "19", "18", "17", "16", "15", "14" };
        const prefixes = [_][]const u8{ "/opt/homebrew/opt/llvm@", "/usr/local/opt/llvm@" };
        for (prefixes) |prefix| {
            for (versions) |ver| {
                const path = std.fmt.allocPrint(std.heap.page_allocator, "{s}{s}/include/llvm-c/Core.h", .{ prefix, ver }) catch continue;
                if (std.fs.accessAbsolute(path, .{})) |_| {
                    return std.fmt.allocPrint(std.heap.page_allocator, "{s}{s}", .{ prefix, ver }) catch continue;
                } else |_| {}
            }
        }
    }

    // Linux - check common paths
    if (std.fs.accessAbsolute("/usr/include/llvm-c/Core.h", .{})) |_| {
        return "/usr";
    } else |_| {}

    if (std.fs.accessAbsolute("/usr/local/include/llvm-c/Core.h", .{})) |_| {
        return "/usr/local";
    } else |_| {}

    // Linux - versioned LLVM paths (Ubuntu/Debian: /usr/lib/llvm-XX/)
    {
        const versions = [_][]const u8{ "20", "19", "18", "17", "16", "15", "14" };
        for (versions) |ver| {
            const path = std.fmt.allocPrint(std.heap.page_allocator, "/usr/lib/llvm-{s}/include/llvm-c/Core.h", .{ver}) catch continue;
            if (std.fs.accessAbsolute(path, .{})) |_| {
                return std.fmt.allocPrint(std.heap.page_allocator, "/usr/lib/llvm-{s}", .{ver}) catch continue;
            } else |_| {}
        }
    }

    // Windows - check common LLVM installation paths
    if (builtin.os.tag == .windows) {
        const win_paths = [_][]const u8{
            "C:\\Program Files\\LLVM",
            "C:\\ProgramData\\chocolatey\\lib\\llvm",
        };
        for (win_paths) |p| {
            const check = std.fmt.allocPrint(std.heap.page_allocator, "{s}\\include\\llvm-c\\Core.h", .{p}) catch continue;
            if (std.fs.accessAbsolute(check, .{})) |_| {
                return p;
            } else |_| {}
        }
    }

    // LLVM not found - will compile without native codegen support
    return null;
}
