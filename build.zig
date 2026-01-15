const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Detect LLVM installation
    const llvm_prefix = detectLLVMPrefix();

    // Create the main module
    const main_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
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
        exe.linkSystemLibrary("LLVM");
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
    });

    const unit_tests = b.addTest(.{
        .root_module = test_mod,
    });

    // Add LLVM to tests as well
    if (llvm_prefix) |prefix| {
        unit_tests.addIncludePath(.{ .cwd_relative = b.fmt("{s}/include", .{prefix}) });
        unit_tests.addLibraryPath(.{ .cwd_relative = b.fmt("{s}/lib", .{prefix}) });
        unit_tests.linkSystemLibrary("LLVM");
        unit_tests.linkLibC();
    }

    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}

/// Detect LLVM installation path.
/// Checks environment variable first, then known locations.
fn detectLLVMPrefix() ?[]const u8 {
    // Try environment variable first
    if (std.process.getEnvVarOwned(std.heap.page_allocator, "LLVM_PREFIX")) |prefix| {
        return prefix;
    } else |_| {}

    // macOS ARM64 (Apple Silicon) - Homebrew
    if (std.fs.accessAbsolute("/opt/homebrew/opt/llvm/include/llvm-c/Core.h", .{})) |_| {
        return "/opt/homebrew/opt/llvm";
    } else |_| {}

    // macOS x86_64 (Intel) - Homebrew
    if (std.fs.accessAbsolute("/usr/local/opt/llvm/include/llvm-c/Core.h", .{})) |_| {
        return "/usr/local/opt/llvm";
    } else |_| {}

    // Linux - check common paths
    if (std.fs.accessAbsolute("/usr/include/llvm-c/Core.h", .{})) |_| {
        return "/usr";
    } else |_| {}

    if (std.fs.accessAbsolute("/usr/local/include/llvm-c/Core.h", .{})) |_| {
        return "/usr/local";
    } else |_| {}

    // LLVM not found - will compile without native codegen support
    return null;
}
