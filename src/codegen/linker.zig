//! System linker invocation.
//!
//! Invokes the platform's system linker to create native executables.

const std = @import("std");
const builtin = @import("builtin");
const target = @import("target.zig");

pub const LinkerError = error{
    LinkerFailed,
    LinkerNotFound,
    OutputWriteFailed,
    OutOfMemory,
};

/// Invoke the system linker to create an executable.
pub fn link(
    allocator: std.mem.Allocator,
    object_file: []const u8,
    output_file: []const u8,
) LinkerError!void {
    const platform = target.Platform.current();

    var args = std.ArrayListUnmanaged([]const u8){};
    defer args.deinit(allocator);

    switch (platform.os) {
        .macos => {
            // macOS linker invocation
            args.appendSlice(allocator, &.{
                "/usr/bin/ld",
                "-o",
                output_file,
                object_file,
                "-lSystem",
                "-syslibroot",
                findMacOSSDK(),
                "-arch",
                platform.getLinkerArch(),
                "-platform_version",
                "macos",
                "11.0",
                "11.0",
            }) catch return LinkerError.OutOfMemory;
        },
        .linux => {
            // Linux linker invocation using gcc as frontend
            // This handles CRT objects and library paths automatically
            args.appendSlice(allocator, &.{
                "gcc",
                "-o",
                output_file,
                object_file,
                "-no-pie",
            }) catch return LinkerError.OutOfMemory;
        },
        else => return LinkerError.LinkerNotFound,
    }

    var child = std.process.Child.init(args.items, allocator);
    child.stderr_behavior = .Pipe;
    child.stdout_behavior = .Pipe;

    child.spawn() catch return LinkerError.LinkerFailed;

    // Wait for completion and get result
    const result = child.wait() catch return LinkerError.LinkerFailed;

    switch (result) {
        .Exited => |code| {
            if (code != 0) {
                return LinkerError.LinkerFailed;
            }
        },
        else => return LinkerError.LinkerFailed,
    }
}

/// Find the macOS SDK path.
fn findMacOSSDK() []const u8 {
    // Try Xcode Command Line Tools first (most common)
    const paths = [_][]const u8{
        "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk",
        "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk",
    };

    for (paths) |path| {
        if (std.fs.accessAbsolute(path, .{})) |_| {
            return path;
        } else |_| {}
    }

    // Fallback - might not work but let the linker complain
    return "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk";
}

/// Link using clang driver (alternative approach).
pub fn linkWithClang(
    allocator: std.mem.Allocator,
    object_file: []const u8,
    output_file: []const u8,
) LinkerError!void {
    const platform = target.Platform.current();

    var args = std.ArrayListUnmanaged([]const u8){};
    defer args.deinit(allocator);

    // Use clang as the linker driver - handles all platform specifics
    args.appendSlice(allocator, &.{
        "clang",
        "-o",
        output_file,
        object_file,
    }) catch return LinkerError.OutOfMemory;

    // Add platform-specific flags
    switch (platform.os) {
        .macos => {
            args.append(allocator, "-arch") catch return LinkerError.OutOfMemory;
            args.append(allocator, platform.getLinkerArch()) catch return LinkerError.OutOfMemory;
        },
        .linux => {
            args.append(allocator, "-no-pie") catch return LinkerError.OutOfMemory;
        },
        else => {},
    }

    var child = std.process.Child.init(args.items, allocator);
    child.stderr_behavior = .Pipe;
    child.stdout_behavior = .Pipe;

    child.spawn() catch return LinkerError.LinkerFailed;

    // Wait for completion and get result
    const result = child.wait() catch return LinkerError.LinkerFailed;

    switch (result) {
        .Exited => |code| {
            if (code != 0) {
                return LinkerError.LinkerFailed;
            }
        },
        else => return LinkerError.LinkerFailed,
    }
}
