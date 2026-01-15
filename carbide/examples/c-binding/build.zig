const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Main executable
    const exe = b.addExecutable(.{
        .name = "c-binding-demo",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Add C source files
    exe.addCSourceFiles(.{
        .files = &.{"vendor/mathlib.c"},
        .flags = &.{ "-std=c11", "-O2" },
    });

    // Add include path for C headers
    exe.addIncludePath(b.path("vendor"));

    // Link libc (required for C interop)
    exe.linkLibC();

    b.installArtifact(exe);

    // Run step
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the C binding demo");
    run_step.dependOn(&run_cmd.step);

    // Tests
    const tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    tests.addCSourceFiles(.{
        .files = &.{"vendor/mathlib.c"},
        .flags = &.{ "-std=c11" },
    });
    tests.addIncludePath(b.path("vendor"));
    tests.linkLibC();

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&b.addRunArtifact(tests).step);
}
