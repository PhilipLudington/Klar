//! System linker invocation.
//!
//! Invokes the platform's system linker to create native executables.
//! Supports cross-compilation by using appropriate linker flags for target platforms.

const std = @import("std");
const builtin = @import("builtin");
const target = @import("target.zig");

pub const LinkerError = error{
    LinkerFailed,
    LinkerNotFound,
    OutputWriteFailed,
    OutOfMemory,
    CrossCompilationNotSupported,
};

/// Linker options for additional libraries and search paths.
pub const LinkerOptions = struct {
    /// Libraries to link (e.g., "m" for libm, "curl" for libcurl).
    link_libs: []const []const u8 = &.{},
    /// Library search paths.
    link_paths: []const []const u8 = &.{},
    /// Custom linker script path (for bare-metal targets).
    linker_script: ?[]const u8 = null,
    /// Freestanding mode: no libc, no standard startup.
    freestanding: bool = false,
};

/// Invoke the system linker to create an executable for the host platform.
pub fn link(
    allocator: std.mem.Allocator,
    object_file: []const u8,
    output_file: []const u8,
) LinkerError!void {
    const platform = target.Platform.current();
    return linkForPlatform(allocator, object_file, output_file, platform, null, .{});
}

/// Invoke the system linker with additional library options.
pub fn linkWithOptions(
    allocator: std.mem.Allocator,
    object_file: []const u8,
    output_file: []const u8,
    options: LinkerOptions,
) LinkerError!void {
    const platform = target.Platform.current();
    return linkForPlatform(allocator, object_file, output_file, platform, null, options);
}

/// Invoke the system linker to create an executable for a specific target.
pub fn linkForTarget(
    allocator: std.mem.Allocator,
    object_file: []const u8,
    output_file: []const u8,
    target_info: target.TargetInfo,
) LinkerError!void {
    const platform = target_info.toPlatform();
    return linkForPlatform(allocator, object_file, output_file, platform, target_info, .{});
}

/// Invoke the system linker for a specific target with additional library options.
pub fn linkForTargetWithOptions(
    allocator: std.mem.Allocator,
    object_file: []const u8,
    output_file: []const u8,
    target_info: target.TargetInfo,
    options: LinkerOptions,
) LinkerError!void {
    const platform = target_info.toPlatform();
    return linkForPlatform(allocator, object_file, output_file, platform, target_info, options);
}

/// Core linking implementation.
fn linkForPlatform(
    allocator: std.mem.Allocator,
    object_file: []const u8,
    output_file: []const u8,
    platform: target.Platform,
    target_info_opt: ?target.TargetInfo,
    options: LinkerOptions,
) LinkerError!void {
    // Check for cross-compilation scenarios
    const is_cross = if (target_info_opt) |ti| ti.isCrossCompile() else false;

    var args = std.ArrayListUnmanaged([]const u8){};
    defer args.deinit(allocator);

    // Bare-metal/freestanding targets use a different linking approach
    if (platform.isFreestanding() or options.freestanding) {
        return linkBareMetalTarget(allocator, object_file, output_file, target_info_opt, options);
    }

    switch (platform.os) {
        .macos => {
            if (is_cross and builtin.os.tag != .macos) {
                // Cross-compiling to macOS from non-macOS requires special setup
                // For now, only support native and macOS-to-macOS cross-arch compilation
                return LinkerError.CrossCompilationNotSupported;
            }

            // Get the architecture from target_info or platform
            const arch_str = if (target_info_opt) |ti|
                ti.getLinkerArch()
            else
                platform.getLinkerArch();

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
                arch_str,
                "-platform_version",
                "macos",
                "11.0",
                "11.0",
            }) catch return LinkerError.OutOfMemory;
        },
        .linux => {
            // Determine the right GCC to use
            const gcc_name: []const u8 = if (target_info_opt) |ti| blk: {
                const host_arch: target.TargetInfo.Arch = switch (builtin.cpu.arch) {
                    .x86_64 => .x86_64,
                    .aarch64 => .aarch64,
                    else => .unknown,
                };
                const host_os: target.TargetInfo.Os = switch (builtin.os.tag) {
                    .linux => .linux,
                    else => .unknown,
                };

                // If not on Linux, or different architecture, use cross-compiler
                if (host_os != .linux or ti.arch != host_arch) {
                    break :blk switch (ti.arch) {
                        .x86_64 => "x86_64-linux-gnu-gcc",
                        .aarch64 => "aarch64-linux-gnu-gcc",
                        .unknown => return LinkerError.CrossCompilationNotSupported,
                    };
                }
                break :blk "gcc";
            } else "gcc";

            // Linux linker invocation using gcc as frontend
            args.appendSlice(allocator, &.{
                gcc_name,
                "-o",
                output_file,
                object_file,
                "-no-pie",
            }) catch return LinkerError.OutOfMemory;
        },
        .windows => {
            // Windows cross-compilation requires MinGW or MSVC cross-tools
            if (is_cross or builtin.os.tag != .windows) {
                // Try MinGW cross-compiler
                const mingw_gcc = if (target_info_opt) |ti| switch (ti.arch) {
                    .x86_64 => "x86_64-w64-mingw32-gcc",
                    .aarch64 => "aarch64-w64-mingw32-gcc",
                    .unknown => return LinkerError.CrossCompilationNotSupported,
                } else "x86_64-w64-mingw32-gcc";

                args.appendSlice(allocator, &.{
                    mingw_gcc,
                    "-o",
                    output_file,
                    object_file,
                }) catch return LinkerError.OutOfMemory;
            } else {
                // Native Windows linking
                args.appendSlice(allocator, &.{
                    "link.exe",
                    "/OUT:" ++ output_file,
                    object_file,
                    "/SUBSYSTEM:CONSOLE",
                }) catch return LinkerError.OutOfMemory;
            }
        },
        else => return LinkerError.LinkerNotFound,
    }

    // Track allocated strings so we can free them after linking
    var allocated_strings = std.ArrayListUnmanaged([]const u8){};
    defer {
        for (allocated_strings.items) |s| {
            allocator.free(s);
        }
        allocated_strings.deinit(allocator);
    }

    // Add library search paths (-L flags)
    // These must come before -l flags so the linker can find the libraries
    for (options.link_paths) |path| {
        switch (platform.os) {
            .windows => {
                if (is_cross or builtin.os.tag != .windows) {
                    // MinGW uses -L
                    const formatted = std.fmt.allocPrint(allocator, "-L{s}", .{path}) catch return LinkerError.OutOfMemory;
                    allocated_strings.append(allocator, formatted) catch return LinkerError.OutOfMemory;
                    args.append(allocator, formatted) catch return LinkerError.OutOfMemory;
                } else {
                    // MSVC uses /LIBPATH:
                    const formatted = std.fmt.allocPrint(allocator, "/LIBPATH:{s}", .{path}) catch return LinkerError.OutOfMemory;
                    allocated_strings.append(allocator, formatted) catch return LinkerError.OutOfMemory;
                    args.append(allocator, formatted) catch return LinkerError.OutOfMemory;
                }
            },
            else => {
                // macOS and Linux both use -L
                const formatted = std.fmt.allocPrint(allocator, "-L{s}", .{path}) catch return LinkerError.OutOfMemory;
                allocated_strings.append(allocator, formatted) catch return LinkerError.OutOfMemory;
                args.append(allocator, formatted) catch return LinkerError.OutOfMemory;
            },
        }
    }

    // Add libraries to link (-l flags)
    for (options.link_libs) |lib| {
        switch (platform.os) {
            .windows => {
                if (is_cross or builtin.os.tag != .windows) {
                    // MinGW uses -l
                    const formatted = std.fmt.allocPrint(allocator, "-l{s}", .{lib}) catch return LinkerError.OutOfMemory;
                    allocated_strings.append(allocator, formatted) catch return LinkerError.OutOfMemory;
                    args.append(allocator, formatted) catch return LinkerError.OutOfMemory;
                } else {
                    // MSVC: just add the library name with .lib extension
                    const formatted = std.fmt.allocPrint(allocator, "{s}.lib", .{lib}) catch return LinkerError.OutOfMemory;
                    allocated_strings.append(allocator, formatted) catch return LinkerError.OutOfMemory;
                    args.append(allocator, formatted) catch return LinkerError.OutOfMemory;
                }
            },
            else => {
                // macOS and Linux both use -l
                const formatted = std.fmt.allocPrint(allocator, "-l{s}", .{lib}) catch return LinkerError.OutOfMemory;
                allocated_strings.append(allocator, formatted) catch return LinkerError.OutOfMemory;
                args.append(allocator, formatted) catch return LinkerError.OutOfMemory;
            },
        }
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

/// Link for bare-metal/freestanding targets.
/// Uses ld directly without libc or standard startup.
fn linkBareMetalTarget(
    allocator: std.mem.Allocator,
    object_file: []const u8,
    output_file: []const u8,
    target_info_opt: ?target.TargetInfo,
    options: LinkerOptions,
) LinkerError!void {
    // Try multiple linker names in order of preference:
    // 1. Common toolchain names (Homebrew, apt packages)
    // 2. ARM Developer toolchain names
    // 3. Generic ld as last resort
    const linker_names: []const []const u8 = if (target_info_opt) |ti| switch (ti.arch) {
        .aarch64 => &.{ "aarch64-elf-ld", "aarch64-none-elf-ld", "ld" },
        .x86_64 => &.{ "x86_64-elf-ld", "ld" },
        .unknown => &.{"ld"},
    } else &.{"ld"};

    for (linker_names) |ld_name| {
        if (tryLinkBareMetal(allocator, ld_name, object_file, output_file, options)) |_| {
            return; // Success
        } else |err| {
            if (err == LinkerError.LinkerNotFound) {
                continue; // Try next linker
            }
            return err; // Other error, propagate
        }
    }

    return LinkerError.LinkerNotFound;
}

/// Attempt to link with a specific linker.
fn tryLinkBareMetal(
    allocator: std.mem.Allocator,
    ld_name: []const u8,
    object_file: []const u8,
    output_file: []const u8,
    options: LinkerOptions,
) LinkerError!void {
    var args = std.ArrayListUnmanaged([]const u8){};
    defer args.deinit(allocator);

    args.append(allocator, ld_name) catch return LinkerError.OutOfMemory;

    // Add linker script if provided
    if (options.linker_script) |script| {
        args.append(allocator, "-T") catch return LinkerError.OutOfMemory;
        args.append(allocator, script) catch return LinkerError.OutOfMemory;
    }

    // Output file
    args.append(allocator, "-o") catch return LinkerError.OutOfMemory;
    args.append(allocator, output_file) catch return LinkerError.OutOfMemory;

    // Input object file
    args.append(allocator, object_file) catch return LinkerError.OutOfMemory;

    // No standard libraries for bare-metal
    args.append(allocator, "--nostdlib") catch return LinkerError.OutOfMemory;

    // Track allocated strings for cleanup
    var allocated_strings = std.ArrayListUnmanaged([]const u8){};
    defer {
        for (allocated_strings.items) |s| {
            allocator.free(s);
        }
        allocated_strings.deinit(allocator);
    }

    // Add library search paths (-L flags)
    for (options.link_paths) |path| {
        const formatted = std.fmt.allocPrint(allocator, "-L{s}", .{path}) catch return LinkerError.OutOfMemory;
        allocated_strings.append(allocator, formatted) catch return LinkerError.OutOfMemory;
        args.append(allocator, formatted) catch return LinkerError.OutOfMemory;
    }

    // Add libraries to link (for bare-metal, these would be static archives)
    for (options.link_libs) |lib| {
        const formatted = std.fmt.allocPrint(allocator, "-l{s}", .{lib}) catch return LinkerError.OutOfMemory;
        allocated_strings.append(allocator, formatted) catch return LinkerError.OutOfMemory;
        args.append(allocator, formatted) catch return LinkerError.OutOfMemory;
    }

    var child = std.process.Child.init(args.items, allocator);
    child.stderr_behavior = .Inherit;
    child.stdout_behavior = .Inherit;

    child.spawn() catch |err| {
        if (err == error.FileNotFound) {
            return LinkerError.LinkerNotFound;
        }
        return LinkerError.LinkerFailed;
    };

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
