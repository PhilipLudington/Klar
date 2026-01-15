//! Build configuration for the Hello example project.
//!
//! ## Build Commands
//! - `zig build` - Build the library and executable
//! - `zig build run` - Build and run the example
//! - `zig build test` - Run all tests

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Library module
    const lib_mod = b.addModule("hello", .{
        .root_source_file = b.path("src/hello.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Executable
    const exe = b.addExecutable(.{
        .name = "hello",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "hello", .module = lib_mod },
            },
        }),
    });
    b.installArtifact(exe);

    // Run step
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the hello example");
    run_step.dependOn(&run_cmd.step);

    // Library tests
    const lib_tests = b.addTest(.{
        .root_module = lib_mod,
    });

    const run_lib_tests = b.addRunArtifact(lib_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_tests.step);
}
