//! CarbideZig Standard Build Configuration
//!
//! This template provides a well-structured build.zig for new projects.
//!
//! ## Build Commands
//! - `zig build` - Build library and executable
//! - `zig build run` - Build and run executable
//! - `zig build test` - Run all tests
//! - `zig build docs` - Generate documentation
//!
//! ## Build Options
//! - `-Doptimize=<mode>` - Debug, ReleaseSafe, ReleaseFast, ReleaseSmall
//! - `-Dtarget=<triple>` - Cross-compilation target
//! - `-Dlog=<bool>` - Enable debug logging

const std = @import("std");

pub fn build(b: *std.Build) void {
    // ===== Standard Options =====
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // ===== Custom Options =====
    const enable_logging = b.option(
        bool,
        "log",
        "Enable debug logging",
    ) orelse (optimize == .Debug);

    // Create options module for compile-time configuration
    const options = b.addOptions();
    options.addOption(bool, "enable_logging", enable_logging);

    // ===== Library Module =====
    const lib_mod = b.addModule("mylib", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    // ===== Executable =====
    const exe = b.addExecutable(.{
        .name = "myapp",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "mylib", .module = lib_mod },
                .{ .name = "config", .module = options.createModule() },
            },
        }),
    });
    b.installArtifact(exe);

    // ===== Run Step =====
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the application");
    run_step.dependOn(&run_cmd.step);

    // ===== Tests =====
    const lib_tests = b.addTest(.{
        .root_module = lib_mod,
    });

    const exe_tests = b.addTest(.{
        .root_module = exe.root_module,
    });

    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&b.addRunArtifact(lib_tests).step);
    test_step.dependOn(&b.addRunArtifact(exe_tests).step);
}
