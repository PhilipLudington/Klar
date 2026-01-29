//! Package manifest (klar.json) parsing and representation.
//!
//! A manifest defines a Klar package with its metadata, dependencies, and build
//! configuration. The format is JSON for compatibility with Zig's std.json.
//!
//! Example klar.json:
//! ```json
//! {
//!   "package": {
//!     "name": "my-project",
//!     "version": "0.1.0",
//!     "authors": ["Author Name"],
//!     "entry": "src/main.kl"
//!   },
//!   "dependencies": {
//!     "utils": { "path": "../utils" }
//!   }
//! }
//! ```

const std = @import("std");
const Allocator = std.mem.Allocator;
const fs = std.fs;

/// Error types for manifest operations.
pub const ManifestError = error{
    FileNotFound,
    InvalidJson,
    MissingPackageSection,
    MissingPackageName,
    InvalidVersion,
    InvalidDependency,
    IoError,
};

/// Semantic version representation.
pub const Version = struct {
    major: u32,
    minor: u32,
    patch: u32,
    prerelease: ?[]const u8 = null,

    pub fn parse(allocator: Allocator, str: []const u8) !Version {
        _ = allocator;
        var it = std.mem.splitScalar(u8, str, '.');
        const major_str = it.next() orelse return error.InvalidVersion;
        const minor_str = it.next() orelse return error.InvalidVersion;

        // Handle patch with optional prerelease suffix
        const patch_part = it.next() orelse return error.InvalidVersion;
        var patch_str = patch_part;
        var prerelease: ?[]const u8 = null;

        if (std.mem.indexOfScalar(u8, patch_part, '-')) |dash_pos| {
            patch_str = patch_part[0..dash_pos];
            prerelease = patch_part[dash_pos + 1 ..];
        }

        return .{
            .major = std.fmt.parseInt(u32, major_str, 10) catch return error.InvalidVersion,
            .minor = std.fmt.parseInt(u32, minor_str, 10) catch return error.InvalidVersion,
            .patch = std.fmt.parseInt(u32, patch_str, 10) catch return error.InvalidVersion,
            .prerelease = prerelease,
        };
    }

    pub fn format(self: Version, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("{d}.{d}.{d}", .{ self.major, self.minor, self.patch });
        if (self.prerelease) |pre| {
            try writer.print("-{s}", .{pre});
        }
    }

    /// Convert version to an owned string. Caller must free the result.
    pub fn toString(self: Version, allocator: Allocator) ![]const u8 {
        var buf: [64]u8 = undefined;
        const len = if (self.prerelease) |pre|
            (std.fmt.bufPrint(&buf, "{d}.{d}.{d}-{s}", .{ self.major, self.minor, self.patch, pre }) catch return error.InvalidVersion).len
        else
            (std.fmt.bufPrint(&buf, "{d}.{d}.{d}", .{ self.major, self.minor, self.patch }) catch return error.InvalidVersion).len;
        return try allocator.dupe(u8, buf[0..len]);
    }
};

/// A dependency specification.
pub const Dependency = struct {
    /// Local path dependency (relative to manifest directory).
    path: ?[]const u8 = null,

    /// Git repository URL.
    git: ?[]const u8 = null,

    /// Git ref (tag, branch, or commit hash).
    ref: ?[]const u8 = null,

    /// Version constraint (for registry dependencies, future use).
    version: ?[]const u8 = null,

    pub fn isPath(self: Dependency) bool {
        return self.path != null;
    }

    pub fn isGit(self: Dependency) bool {
        return self.git != null;
    }
};

/// Package metadata section.
pub const PackageInfo = struct {
    /// Package name (required).
    name: []const u8,

    /// Package version (required, semver format).
    version: Version,

    /// Package authors (optional).
    authors: []const []const u8 = &.{},

    /// Entry point file (optional, defaults to src/main.kl or src/lib.kl).
    entry: ?[]const u8 = null,

    /// Package description (optional).
    description: ?[]const u8 = null,

    /// License identifier (optional).
    license: ?[]const u8 = null,
};

/// Complete manifest representation.
pub const Manifest = struct {
    allocator: Allocator,

    /// Package metadata.
    package: PackageInfo,

    /// Runtime dependencies.
    dependencies: std.StringHashMapUnmanaged(Dependency),

    /// Development-only dependencies.
    dev_dependencies: std.StringHashMapUnmanaged(Dependency),

    /// Directory containing the manifest (for resolving relative paths).
    root_dir: []const u8,

    pub fn deinit(self: *Manifest) void {
        // Free package info strings
        self.allocator.free(self.package.name);
        for (self.package.authors) |author| {
            self.allocator.free(author);
        }
        self.allocator.free(self.package.authors);
        if (self.package.entry) |e| self.allocator.free(e);
        if (self.package.description) |d| self.allocator.free(d);
        if (self.package.license) |l| self.allocator.free(l);

        // Free dependencies
        var dep_it = self.dependencies.iterator();
        while (dep_it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            freeDependency(self.allocator, entry.value_ptr.*);
        }
        self.dependencies.deinit(self.allocator);

        // Free dev-dependencies
        var dev_dep_it = self.dev_dependencies.iterator();
        while (dev_dep_it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            freeDependency(self.allocator, entry.value_ptr.*);
        }
        self.dev_dependencies.deinit(self.allocator);

        // Free root_dir
        self.allocator.free(self.root_dir);
    }

    fn freeDependency(allocator: Allocator, dep: Dependency) void {
        if (dep.path) |p| allocator.free(p);
        if (dep.git) |g| allocator.free(g);
        if (dep.ref) |r| allocator.free(r);
        if (dep.version) |v| allocator.free(v);
    }

    /// Get the entry point path, with default fallback.
    pub fn getEntryPoint(self: *const Manifest) []const u8 {
        return self.package.entry orelse "src/main.kl";
    }

    /// Get full path to entry point.
    pub fn getEntryPath(self: *const Manifest, allocator: Allocator) ![]const u8 {
        const entry = self.getEntryPoint();
        if (std.fs.path.isAbsolute(entry)) {
            return try allocator.dupe(u8, entry);
        }

        // Join root_dir with entry
        const path = try std.fs.path.join(allocator, &.{ self.root_dir, entry });
        return path;
    }
};

/// Load and parse a manifest from a klar.json file.
pub fn loadManifest(allocator: Allocator, manifest_path: []const u8) !Manifest {
    // Read the file
    const file = fs.cwd().openFile(manifest_path, .{}) catch |err| {
        return switch (err) {
            error.FileNotFound => ManifestError.FileNotFound,
            else => ManifestError.IoError,
        };
    };
    defer file.close();

    const content = file.readToEndAlloc(allocator, 1024 * 1024) catch {
        return ManifestError.IoError;
    };
    defer allocator.free(content);

    return parseManifest(allocator, content, std.fs.path.dirname(manifest_path) orelse ".");
}

/// Parse manifest from JSON content.
pub fn parseManifest(allocator: Allocator, content: []const u8, root_dir: []const u8) !Manifest {
    const parsed = std.json.parseFromSlice(std.json.Value, allocator, content, .{}) catch {
        return ManifestError.InvalidJson;
    };
    defer parsed.deinit();

    const root = parsed.value;
    if (root != .object) {
        return ManifestError.InvalidJson;
    }

    // Parse package section (required)
    const pkg_value = root.object.get("package") orelse {
        return ManifestError.MissingPackageSection;
    };
    if (pkg_value != .object) {
        return ManifestError.MissingPackageSection;
    }
    const pkg = pkg_value.object;

    // Package name (required)
    const name_value = pkg.get("name") orelse {
        return ManifestError.MissingPackageName;
    };
    const name = switch (name_value) {
        .string => |s| try allocator.dupe(u8, s),
        else => return ManifestError.MissingPackageName,
    };

    // Package version (required)
    const version_value = pkg.get("version") orelse {
        return ManifestError.InvalidVersion;
    };
    const version = switch (version_value) {
        .string => |s| try Version.parse(allocator, s),
        else => return ManifestError.InvalidVersion,
    };

    // Authors (optional)
    var authors = std.ArrayListUnmanaged([]const u8){};
    if (pkg.get("authors")) |authors_value| {
        if (authors_value == .array) {
            for (authors_value.array.items) |item| {
                if (item == .string) {
                    try authors.append(allocator, try allocator.dupe(u8, item.string));
                }
            }
        }
    }

    // Entry point (optional)
    const entry: ?[]const u8 = if (pkg.get("entry")) |e| switch (e) {
        .string => |s| try allocator.dupe(u8, s),
        else => null,
    } else null;

    // Description (optional)
    const description: ?[]const u8 = if (pkg.get("description")) |d| switch (d) {
        .string => |s| try allocator.dupe(u8, s),
        else => null,
    } else null;

    // License (optional)
    const license: ?[]const u8 = if (pkg.get("license")) |l| switch (l) {
        .string => |s| try allocator.dupe(u8, s),
        else => null,
    } else null;

    // Parse dependencies
    var deps = std.StringHashMapUnmanaged(Dependency){};
    if (root.object.get("dependencies")) |deps_value| {
        if (deps_value == .object) {
            var it = deps_value.object.iterator();
            while (it.next()) |kv| {
                const dep = try parseDependency(allocator, kv.value_ptr.*);
                const dep_name = try allocator.dupe(u8, kv.key_ptr.*);
                try deps.put(allocator, dep_name, dep);
            }
        }
    }

    // Parse dev-dependencies
    var dev_deps = std.StringHashMapUnmanaged(Dependency){};
    if (root.object.get("dev-dependencies")) |deps_value| {
        if (deps_value == .object) {
            var it = deps_value.object.iterator();
            while (it.next()) |kv| {
                const dep = try parseDependency(allocator, kv.value_ptr.*);
                const dep_name = try allocator.dupe(u8, kv.key_ptr.*);
                try dev_deps.put(allocator, dep_name, dep);
            }
        }
    }

    return .{
        .allocator = allocator,
        .package = .{
            .name = name,
            .version = version,
            .authors = try authors.toOwnedSlice(allocator),
            .entry = entry,
            .description = description,
            .license = license,
        },
        .dependencies = deps,
        .dev_dependencies = dev_deps,
        .root_dir = try allocator.dupe(u8, root_dir),
    };
}

fn parseDependency(allocator: Allocator, value: std.json.Value) !Dependency {
    if (value != .object) {
        return ManifestError.InvalidDependency;
    }
    const obj = value.object;

    return .{
        .path = if (obj.get("path")) |p| switch (p) {
            .string => |s| try allocator.dupe(u8, s),
            else => null,
        } else null,
        .git = if (obj.get("git")) |g| switch (g) {
            .string => |s| try allocator.dupe(u8, s),
            else => null,
        } else null,
        .ref = if (obj.get("ref")) |r| switch (r) {
            .string => |s| try allocator.dupe(u8, s),
            else => null,
        } else null,
        .version = if (obj.get("version")) |v| switch (v) {
            .string => |s| try allocator.dupe(u8, s),
            else => null,
        } else null,
    };
}

/// Generate a default manifest for a new project.
pub fn generateDefault(allocator: Allocator, name: []const u8, is_lib: bool) ![]const u8 {
    var buffer = std.ArrayListUnmanaged(u8){};
    const writer = buffer.writer(allocator);

    const entry = if (is_lib) "src/lib.kl" else "src/main.kl";

    try writer.print(
        \\{{
        \\  "package": {{
        \\    "name": "{s}",
        \\    "version": "0.1.0",
        \\    "entry": "{s}"
        \\  }},
        \\  "dependencies": {{}}
        \\}}
        \\
    , .{ name, entry });

    return buffer.toOwnedSlice(allocator);
}

// Tests
test "Version.parse basic" {
    const v = try Version.parse(std.testing.allocator, "1.2.3");
    try std.testing.expectEqual(@as(u32, 1), v.major);
    try std.testing.expectEqual(@as(u32, 2), v.minor);
    try std.testing.expectEqual(@as(u32, 3), v.patch);
    try std.testing.expect(v.prerelease == null);
}

test "Version.parse with prerelease" {
    const v = try Version.parse(std.testing.allocator, "1.0.0-beta");
    try std.testing.expectEqual(@as(u32, 1), v.major);
    try std.testing.expectEqual(@as(u32, 0), v.minor);
    try std.testing.expectEqual(@as(u32, 0), v.patch);
    try std.testing.expectEqualStrings("beta", v.prerelease.?);
}

test "Version.toString basic" {
    const v = Version{ .major = 1, .minor = 2, .patch = 3 };
    const str = try v.toString(std.testing.allocator);
    defer std.testing.allocator.free(str);
    try std.testing.expectEqualStrings("1.2.3", str);
}

test "Version.toString with prerelease" {
    const v = Version{ .major = 2, .minor = 0, .patch = 0, .prerelease = "rc1" };
    const str = try v.toString(std.testing.allocator);
    defer std.testing.allocator.free(str);
    try std.testing.expectEqualStrings("2.0.0-rc1", str);
}

test "parseManifest minimal" {
    const json =
        \\{
        \\  "package": {
        \\    "name": "test-pkg",
        \\    "version": "0.1.0"
        \\  }
        \\}
    ;

    var manifest = try parseManifest(std.testing.allocator, json, ".");
    defer manifest.deinit();

    try std.testing.expectEqualStrings("test-pkg", manifest.package.name);
    try std.testing.expectEqual(@as(u32, 0), manifest.package.version.major);
    try std.testing.expectEqual(@as(u32, 1), manifest.package.version.minor);
    try std.testing.expectEqual(@as(u32, 0), manifest.package.version.patch);
}

test "parseManifest with dependencies" {
    const json =
        \\{
        \\  "package": {
        \\    "name": "test-pkg",
        \\    "version": "1.0.0"
        \\  },
        \\  "dependencies": {
        \\    "utils": { "path": "../utils" },
        \\    "remote": { "git": "https://example.com/repo.git", "ref": "v1.0.0" }
        \\  }
        \\}
    ;

    var manifest = try parseManifest(std.testing.allocator, json, "/project");
    defer manifest.deinit();

    try std.testing.expectEqual(@as(usize, 2), manifest.dependencies.count());

    const utils_dep = manifest.dependencies.get("utils").?;
    try std.testing.expect(utils_dep.isPath());
    try std.testing.expectEqualStrings("../utils", utils_dep.path.?);

    const remote_dep = manifest.dependencies.get("remote").?;
    try std.testing.expect(remote_dep.isGit());
    try std.testing.expectEqualStrings("https://example.com/repo.git", remote_dep.git.?);
    try std.testing.expectEqualStrings("v1.0.0", remote_dep.ref.?);
}

test "generateDefault" {
    const content = try generateDefault(std.testing.allocator, "my-project", false);
    defer std.testing.allocator.free(content);

    // Verify it's valid JSON by parsing it
    var manifest = try parseManifest(std.testing.allocator, content, ".");
    defer manifest.deinit();

    try std.testing.expectEqualStrings("my-project", manifest.package.name);
    try std.testing.expectEqualStrings("src/main.kl", manifest.getEntryPoint());
}
