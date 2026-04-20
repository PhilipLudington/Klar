//! Kira dependency build orchestration.
//!
//! During `klar build`, this module detects Kira dependencies from the project
//! manifest, invokes `kira build --lib` for each, compiles the generated C code
//! to object files, runs `klar import-kira` to generate Klar extern blocks, and
//! returns the paths needed for linking and module resolution.

const std = @import("std");
const compat = @import("../compat.zig");
const Allocator = std.mem.Allocator;
const manifest = @import("../pkg/manifest.zig");
const kira_manifest = @import("kira_manifest.zig");

/// Result of building all Kira dependencies.
pub const KiraBuildResult = struct {
    allocator: Allocator,

    /// Object files to pass to the linker.
    extra_objects: std.ArrayListUnmanaged([]const u8),

    /// Search paths for generated .kl files (for module resolution).
    search_paths: std.ArrayListUnmanaged([]const u8),

    /// All allocated strings (for cleanup).
    allocated_strings: std.ArrayListUnmanaged([]const u8),

    pub fn deinit(self: *KiraBuildResult) void {
        for (self.allocated_strings.items) |s| {
            self.allocator.free(s);
        }
        self.allocated_strings.deinit(self.allocator);
        self.extra_objects.deinit(self.allocator);
        self.search_paths.deinit(self.allocator);
    }
};

pub const KiraBuildError = error{
    KiraNotFound,
    KiraBuildFailed,
    CCompilerNotFound,
    CCompileFailed,
    ImportKiraFailed,
    OutOfMemory,
    DependencyPathNotFound,
    EntryFileNotFound,
};

/// Build all Kira dependencies declared in the manifest.
///
/// For each Kira dependency:
/// 1. Resolve the path to the Kira project directory
/// 2. Find the entry .ki file
/// 3. Run `kira build --lib` to generate .c, .h, .kl, .json
/// 4. Compile the .c to a .o object file using `cc`
/// 5. Run `klar import-kira` on the .json to generate a Klar extern block
/// 6. Return object files for linking and search paths for module resolution
pub fn buildKiraDependencies(
    allocator: Allocator,
    kira_deps: std.StringHashMapUnmanaged(manifest.KiraDependency),
    project_root: []const u8,
    build_dir: []const u8,
    stderr: anytype,
) KiraBuildError!KiraBuildResult {
    var result = KiraBuildResult{
        .allocator = allocator,
        .extra_objects = .empty,
        .search_paths = .empty,
        .allocated_strings = .empty,
    };

    // Ensure build directory exists
    compat.cwd().makePath(build_dir) catch return KiraBuildError.OutOfMemory;

    // Ensure deps directory exists for generated .kl files
    const deps_dir = std.fmt.allocPrint(allocator, "{s}/deps", .{project_root}) catch return KiraBuildError.OutOfMemory;
    result.allocated_strings.append(allocator, deps_dir) catch return KiraBuildError.OutOfMemory;
    compat.cwd().makePath(deps_dir) catch return KiraBuildError.OutOfMemory;

    // Add deps directory as search path for module resolution
    result.search_paths.append(allocator, deps_dir) catch return KiraBuildError.OutOfMemory;

    var iter = kira_deps.iterator();
    while (iter.next()) |entry| {
        const dep_name = entry.key_ptr.*;
        const dep = entry.value_ptr.*;

        buildSingleKiraDependency(
            allocator,
            dep_name,
            dep,
            project_root,
            build_dir,
            deps_dir,
            &result,
            stderr,
        ) catch |err| {
            return err;
        };
    }

    return result;
}

/// Build a single Kira dependency.
fn buildSingleKiraDependency(
    allocator: Allocator,
    dep_name: []const u8,
    dep: manifest.KiraDependency,
    project_root: []const u8,
    build_dir: []const u8,
    deps_dir: []const u8,
    result: *KiraBuildResult,
    stderr: anytype,
) KiraBuildError!void {
    // Step 1: Resolve the Kira project directory
    if (!dep.isPath()) {
        // Git dependencies not yet supported for build orchestration
        printError(stderr, "Error: Kira dependency '{s}' uses git source, which is not yet supported for automatic builds. Use path dependencies.\n", .{dep_name});
        return KiraBuildError.DependencyPathNotFound;
    }

    const kira_path = dep.path.?;
    const abs_kira_dir = if (std.fs.path.isAbsolute(kira_path))
        std.fmt.allocPrint(allocator, "{s}", .{kira_path}) catch return KiraBuildError.OutOfMemory
    else
        std.fmt.allocPrint(allocator, "{s}/{s}", .{ project_root, kira_path }) catch return KiraBuildError.OutOfMemory;
    result.allocated_strings.append(allocator, abs_kira_dir) catch return KiraBuildError.OutOfMemory;

    // Verify the directory exists
    compat.cwd().access(abs_kira_dir, .{}) catch {
        printError(stderr, "Error: Kira dependency '{s}' path not found: {s}\n", .{ dep_name, abs_kira_dir });
        return KiraBuildError.DependencyPathNotFound;
    };

    // Step 2: Find the entry .ki file
    const entry_file = findKiraEntry(allocator, abs_kira_dir, dep_name) catch |err| {
        switch (err) {
            error.OutOfMemory => return KiraBuildError.OutOfMemory,
            else => {
                printError(stderr, "Error: Could not find entry file for Kira dependency '{s}' in {s}\n", .{ dep_name, abs_kira_dir });
                return KiraBuildError.EntryFileNotFound;
            },
        }
    };
    result.allocated_strings.append(allocator, entry_file) catch return KiraBuildError.OutOfMemory;

    // Derive output file paths from the entry file base name
    const entry_basename = std.fs.path.stem(entry_file);
    const c_file = std.fmt.allocPrint(allocator, "{s}/{s}.c", .{ abs_kira_dir, entry_basename }) catch return KiraBuildError.OutOfMemory;
    result.allocated_strings.append(allocator, c_file) catch return KiraBuildError.OutOfMemory;
    const json_file = std.fmt.allocPrint(allocator, "{s}/{s}.json", .{ abs_kira_dir, entry_basename }) catch return KiraBuildError.OutOfMemory;
    result.allocated_strings.append(allocator, json_file) catch return KiraBuildError.OutOfMemory;
    const obj_file = std.fmt.allocPrint(allocator, "{s}/kira_{s}.o", .{ build_dir, dep_name }) catch return KiraBuildError.OutOfMemory;
    result.allocated_strings.append(allocator, obj_file) catch return KiraBuildError.OutOfMemory;
    const kl_file = std.fmt.allocPrint(allocator, "{s}/kira_{s}.kl", .{ deps_dir, dep_name }) catch return KiraBuildError.OutOfMemory;
    result.allocated_strings.append(allocator, kl_file) catch return KiraBuildError.OutOfMemory;

    // Step 3: Check if rebuild is needed (cache check)
    if (!needsRebuild(entry_file, obj_file)) {
        // Cached — still need to add the object to the link list
        result.extra_objects.append(allocator, obj_file) catch return KiraBuildError.OutOfMemory;

        // Regenerate .kl if missing (import-kira output)
        if (needsRebuild(json_file, kl_file)) {
            try runImportKira(allocator, json_file, kl_file, dep_name, stderr);
        }
        return;
    }

    // Step 4: Run `kira build --lib <entry.ki>`
    try runKiraBuild(allocator, entry_file, stderr);

    // Step 5: Compile the .c to .o using cc
    try compileCToObject(allocator, c_file, obj_file, stderr);

    // Step 6: Run `klar import-kira` on the .json to generate the .kl extern block
    try runImportKira(allocator, json_file, kl_file, dep_name, stderr);

    // Add object file to link list
    result.extra_objects.append(allocator, obj_file) catch return KiraBuildError.OutOfMemory;
}

/// Find the entry .ki file in a Kira project directory.
/// Looks for: src/main.ki, main.ki, src/lib.ki, lib.ki, <dep_name>.ki
fn findKiraEntry(allocator: Allocator, kira_dir: []const u8, dep_name: []const u8) ![]const u8 {
    const candidates = [_][]const u8{
        "src/main.ki",
        "main.ki",
        "src/lib.ki",
        "lib.ki",
    };

    for (candidates) |candidate| {
        const path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ kira_dir, candidate });
        if (compat.cwd().access(path, .{})) |_| {
            return path;
        } else |_| {
            allocator.free(path);
        }
    }

    // Try <dep_name>.ki
    const dep_path = try std.fmt.allocPrint(allocator, "{s}/{s}.ki", .{ kira_dir, dep_name });
    if (compat.cwd().access(dep_path, .{})) |_| {
        return dep_path;
    } else |_| {
        allocator.free(dep_path);
    }

    return error.FileNotFound;
}

/// Check if a target file needs rebuilding based on source file modification time.
fn needsRebuild(source: []const u8, target_file: []const u8) bool {
    const source_stat = compat.cwd().statFile(source) catch return true;
    const target_stat = compat.cwd().statFile(target_file) catch return true;

    // Rebuild if source is newer than target
    return source_stat.mtime > target_stat.mtime;
}

/// Run `kira build --lib <entry.ki>`.
fn runKiraBuild(allocator: Allocator, entry_file: []const u8, stderr: anytype) KiraBuildError!void {
    const argv = [_][]const u8{ "kira", "build", "--lib", entry_file };

    var child = compat.Child.init(&argv, allocator);
    child.stderr_behavior = .Inherit;
    child.stdout_behavior = .Inherit;

    child.spawn() catch |err| {
        if (err == error.FileNotFound) {
            printError(stderr, "Error: 'kira' command not found. Install Kira to build Kira dependencies.\n", .{});
            return KiraBuildError.KiraNotFound;
        }
        printError(stderr, "Error: Failed to start 'kira build': {s}\n", .{@errorName(err)});
        return KiraBuildError.KiraBuildFailed;
    };

    const result = child.wait() catch {
        printError(stderr, "Error: Failed to wait for 'kira build'\n", .{});
        return KiraBuildError.KiraBuildFailed;
    };

    switch (result) {
        .Exited => |code| {
            if (code != 0) {
                printError(stderr, "Error: 'kira build --lib' failed with exit code {d}\n", .{code});
                return KiraBuildError.KiraBuildFailed;
            }
        },
        else => {
            printError(stderr, "Error: 'kira build --lib' terminated abnormally\n", .{});
            return KiraBuildError.KiraBuildFailed;
        },
    }
}

/// Compile a C file to an object file using the system C compiler.
fn compileCToObject(allocator: Allocator, c_file: []const u8, obj_file: []const u8, stderr: anytype) KiraBuildError!void {
    const cc_owned = if (@import("builtin").os.tag == .windows) null else (compat.getEnvVarOwned(allocator, "CC") catch null);
    defer if (cc_owned) |owned| allocator.free(owned);
    const cc: []const u8 = cc_owned orelse if (@import("builtin").os.tag == .windows) "cl.exe" else "cc";
    const argv = [_][]const u8{ cc, "-c", "-o", obj_file, c_file };

    var child = compat.Child.init(&argv, allocator);
    child.stderr_behavior = .Inherit;
    child.stdout_behavior = .Inherit;

    child.spawn() catch |err| {
        if (err == error.FileNotFound) {
            printError(stderr, "Error: C compiler '{s}' not found. Install a C compiler to build Kira dependencies.\n", .{cc});
            return KiraBuildError.CCompilerNotFound;
        }
        printError(stderr, "Error: Failed to start C compiler: {s}\n", .{@errorName(err)});
        return KiraBuildError.CCompileFailed;
    };

    const result = child.wait() catch {
        printError(stderr, "Error: Failed to wait for C compiler\n", .{});
        return KiraBuildError.CCompileFailed;
    };

    switch (result) {
        .Exited => |code| {
            if (code != 0) {
                printError(stderr, "Error: C compilation failed with exit code {d}\n", .{code});
                return KiraBuildError.CCompileFailed;
            }
        },
        else => {
            printError(stderr, "Error: C compiler terminated abnormally\n", .{});
            return KiraBuildError.CCompileFailed;
        },
    }
}

/// Run `klar import-kira` to generate a .kl extern block from a Kira manifest JSON.
/// Uses the in-process manifest parser (same as the `import-kira` CLI command).
fn runImportKira(allocator: Allocator, json_file: []const u8, kl_file: []const u8, dep_name: []const u8, stderr: anytype) KiraBuildError!void {
    // Use the in-process manifest parser and code generator
    const json_data = compat.cwd().readFileAlloc(allocator, json_file, 1024 * 1024) catch {
        printError(stderr, "Error: Failed to read Kira manifest: {s}\n", .{json_file});
        return KiraBuildError.ImportKiraFailed;
    };
    defer allocator.free(json_data);

    var parsed_manifest = kira_manifest.parseManifest(allocator, json_data) catch {
        printError(stderr, "Error: Failed to parse Kira manifest: {s}\n", .{json_file});
        return KiraBuildError.ImportKiraFailed;
    };
    defer parsed_manifest.deinit(allocator);

    const generated = kira_manifest.generateKlarSource(allocator, &parsed_manifest) catch {
        printError(stderr, "Error: Failed to generate Klar extern block for '{s}'\n", .{dep_name});
        return KiraBuildError.ImportKiraFailed;
    };
    defer allocator.free(generated);

    // Write to output file
    var file = compat.cwd().createFile(kl_file, .{}) catch {
        printError(stderr, "Error: Failed to write generated file: {s}\n", .{kl_file});
        return KiraBuildError.ImportKiraFailed;
    };
    defer file.close();

    file.writeAll(generated) catch {
        printError(stderr, "Error: Failed to write generated file: {s}\n", .{kl_file});
        return KiraBuildError.ImportKiraFailed;
    };
}

/// Print an error message to stderr, silently ignoring write failures.
fn printError(stderr: anytype, comptime fmt: []const u8, args: anytype) void {
    var buf: [1024]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, fmt, args) catch return;
    stderr.writeAll(msg) catch {};
}
