//! Lock file (klar.lock) parsing and generation.
//!
//! A lock file records the exact resolved state of all dependencies,
//! ensuring reproducible builds across different machines and times.
//!
//! Example klar.lock:
//! ```json
//! {
//!   "version": 1,
//!   "dependencies": {
//!     "utils": {
//!       "source": "path",
//!       "path": "../utils",
//!       "resolved": "/absolute/path/to/utils",
//!       "version": "0.2.1"
//!     }
//!   }
//! }
//! ```

const std = @import("std");
const Allocator = std.mem.Allocator;
const fs = std.fs;
const manifest = @import("manifest.zig");

/// Lock file format version. Increment when making breaking changes.
pub const LOCKFILE_VERSION: u32 = 1;

/// Error types for lock file operations.
pub const LockfileError = error{
    FileNotFound,
    InvalidJson,
    UnsupportedVersion,
    InvalidDependency,
    IoError,
    MismatchedDependency,
};

/// Source type for a dependency.
pub const DependencySource = enum {
    path,
    git,

    pub fn toString(self: DependencySource) []const u8 {
        return switch (self) {
            .path => "path",
            .git => "git",
        };
    }

    pub fn fromString(str: []const u8) ?DependencySource {
        if (std.mem.eql(u8, str, "path")) return .path;
        if (std.mem.eql(u8, str, "git")) return .git;
        return null;
    }
};

/// A resolved dependency in the lock file.
pub const ResolvedDependency = struct {
    /// The source type (path, git).
    source: DependencySource,

    /// Original path or URL from manifest.
    original: []const u8,

    /// Resolved absolute path on disk.
    resolved: []const u8,

    /// Version of the dependency (if available).
    version: ?[]const u8 = null,

    /// Git commit hash (for git dependencies).
    commit: ?[]const u8 = null,
};

/// Complete lock file representation.
pub const Lockfile = struct {
    allocator: Allocator,

    /// Lock file format version.
    version: u32,

    /// Resolved dependencies.
    dependencies: std.StringHashMapUnmanaged(ResolvedDependency),

    pub fn init(allocator: Allocator) Lockfile {
        return .{
            .allocator = allocator,
            .version = LOCKFILE_VERSION,
            .dependencies = .{},
        };
    }

    pub fn deinit(self: *Lockfile) void {
        var it = self.dependencies.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            freeResolvedDependency(self.allocator, entry.value_ptr.*);
        }
        self.dependencies.deinit(self.allocator);
    }

    fn freeResolvedDependency(allocator: Allocator, dep: ResolvedDependency) void {
        allocator.free(dep.original);
        allocator.free(dep.resolved);
        if (dep.version) |v| allocator.free(v);
        if (dep.commit) |c| allocator.free(c);
    }

    /// Add a resolved dependency to the lock file.
    pub fn addDependency(
        self: *Lockfile,
        name: []const u8,
        dep: ResolvedDependency,
    ) !void {
        const name_copy = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(name_copy);

        // Copy the dependency strings
        const dep_copy = ResolvedDependency{
            .source = dep.source,
            .original = try self.allocator.dupe(u8, dep.original),
            .resolved = try self.allocator.dupe(u8, dep.resolved),
            .version = if (dep.version) |v| try self.allocator.dupe(u8, v) else null,
            .commit = if (dep.commit) |c| try self.allocator.dupe(u8, c) else null,
        };

        try self.dependencies.put(self.allocator, name_copy, dep_copy);
    }

    /// Check if the lock file matches the manifest dependencies.
    /// Returns list of dependency names that don't match.
    pub fn checkMismatch(
        self: *const Lockfile,
        manifest_deps: *const std.StringHashMapUnmanaged(manifest.Dependency),
        allocator: Allocator,
    ) ![]const []const u8 {
        var mismatched = std.ArrayListUnmanaged([]const u8){};
        errdefer {
            for (mismatched.items) |m| allocator.free(m);
            mismatched.deinit(allocator);
        }

        // Check for deps in manifest but not in lock file
        var manifest_it = manifest_deps.iterator();
        while (manifest_it.next()) |entry| {
            const name = entry.key_ptr.*;
            if (!self.dependencies.contains(name)) {
                try mismatched.append(allocator, try allocator.dupe(u8, name));
                continue;
            }

            // Check if source matches
            const locked = self.dependencies.get(name).?;
            const dep = entry.value_ptr.*;

            if (dep.isPath() and locked.source != .path) {
                try mismatched.append(allocator, try allocator.dupe(u8, name));
            } else if (dep.isGit() and locked.source != .git) {
                try mismatched.append(allocator, try allocator.dupe(u8, name));
            } else if (dep.isPath()) {
                // Check if path changed
                if (!std.mem.eql(u8, dep.path.?, locked.original)) {
                    try mismatched.append(allocator, try allocator.dupe(u8, name));
                }
            }
        }

        // Check for deps in lock file but not in manifest
        var lock_it = self.dependencies.iterator();
        while (lock_it.next()) |entry| {
            const name = entry.key_ptr.*;
            if (!manifest_deps.contains(name)) {
                // Only add if not already in mismatched
                var found = false;
                for (mismatched.items) |m| {
                    if (std.mem.eql(u8, m, name)) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    try mismatched.append(allocator, try allocator.dupe(u8, name));
                }
            }
        }

        return mismatched.toOwnedSlice(allocator);
    }
};

/// Load and parse a lock file from klar.lock.
pub fn loadLockfile(allocator: Allocator, lockfile_path: []const u8) !Lockfile {
    const file = fs.cwd().openFile(lockfile_path, .{}) catch |err| {
        return switch (err) {
            error.FileNotFound => LockfileError.FileNotFound,
            else => LockfileError.IoError,
        };
    };
    defer file.close();

    const content = file.readToEndAlloc(allocator, 1024 * 1024) catch {
        return LockfileError.IoError;
    };
    defer allocator.free(content);

    return parseLockfile(allocator, content);
}

/// Parse lock file from JSON content.
pub fn parseLockfile(allocator: Allocator, content: []const u8) !Lockfile {
    const parsed = std.json.parseFromSlice(std.json.Value, allocator, content, .{}) catch {
        return LockfileError.InvalidJson;
    };
    defer parsed.deinit();

    const root = parsed.value;
    if (root != .object) {
        return LockfileError.InvalidJson;
    }

    // Check version
    const version = if (root.object.get("version")) |v| switch (v) {
        .integer => |i| @as(u32, @intCast(i)),
        else => return LockfileError.InvalidJson,
    } else return LockfileError.InvalidJson;

    if (version > LOCKFILE_VERSION) {
        return LockfileError.UnsupportedVersion;
    }

    var lockfile = Lockfile.init(allocator);
    lockfile.version = version;
    errdefer lockfile.deinit();

    // Parse dependencies
    if (root.object.get("dependencies")) |deps_value| {
        if (deps_value != .object) {
            return LockfileError.InvalidJson;
        }

        var it = deps_value.object.iterator();
        while (it.next()) |kv| {
            const dep = try parseResolvedDependency(allocator, kv.value_ptr.*);
            const name = try allocator.dupe(u8, kv.key_ptr.*);
            try lockfile.dependencies.put(allocator, name, dep);
        }
    }

    return lockfile;
}

fn parseResolvedDependency(allocator: Allocator, value: std.json.Value) !ResolvedDependency {
    if (value != .object) {
        return LockfileError.InvalidDependency;
    }
    const obj = value.object;

    // Source is required
    const source_str = switch (obj.get("source") orelse return LockfileError.InvalidDependency) {
        .string => |s| s,
        else => return LockfileError.InvalidDependency,
    };
    const source = DependencySource.fromString(source_str) orelse return LockfileError.InvalidDependency;

    // Original path/URL is required (stored as "path" or "git" depending on source)
    const original_key = switch (source) {
        .path => "path",
        .git => "git",
    };
    const original = switch (obj.get(original_key) orelse return LockfileError.InvalidDependency) {
        .string => |s| try allocator.dupe(u8, s),
        else => return LockfileError.InvalidDependency,
    };
    errdefer allocator.free(original);

    // Resolved path is required
    const resolved = switch (obj.get("resolved") orelse return LockfileError.InvalidDependency) {
        .string => |s| try allocator.dupe(u8, s),
        else => return LockfileError.InvalidDependency,
    };
    errdefer allocator.free(resolved);

    // Version is optional
    const version: ?[]const u8 = if (obj.get("version")) |v| switch (v) {
        .string => |s| try allocator.dupe(u8, s),
        else => null,
    } else null;

    // Commit is optional (for git deps)
    const commit: ?[]const u8 = if (obj.get("commit")) |c| switch (c) {
        .string => |s| try allocator.dupe(u8, s),
        else => null,
    } else null;

    return .{
        .source = source,
        .original = original,
        .resolved = resolved,
        .version = version,
        .commit = commit,
    };
}

/// Generate lock file JSON content.
pub fn generateLockfile(allocator: Allocator, lockfile: *const Lockfile) ![]const u8 {
    var buffer = std.ArrayListUnmanaged(u8){};
    const writer = buffer.writer(allocator);

    try writer.writeAll("{\n");
    try writer.print("  \"version\": {d},\n", .{lockfile.version});
    try writer.writeAll("  \"dependencies\": {");

    var first = true;
    var it = lockfile.dependencies.iterator();
    while (it.next()) |entry| {
        if (!first) {
            try writer.writeAll(",");
        }
        first = false;

        try writer.writeAll("\n    \"");
        try writer.writeAll(entry.key_ptr.*);
        try writer.writeAll("\": {\n");

        const dep = entry.value_ptr.*;
        try writer.print("      \"source\": \"{s}\",\n", .{dep.source.toString()});

        // Write original path/URL
        const key = switch (dep.source) {
            .path => "path",
            .git => "git",
        };
        try writer.print("      \"{s}\": \"{s}\",\n", .{ key, escapeJsonString(dep.original) });

        try writer.print("      \"resolved\": \"{s}\"", .{escapeJsonString(dep.resolved)});

        if (dep.version) |v| {
            try writer.print(",\n      \"version\": \"{s}\"", .{v});
        }

        if (dep.commit) |c| {
            try writer.print(",\n      \"commit\": \"{s}\"", .{c});
        }

        try writer.writeAll("\n    }");
    }

    if (!first) {
        try writer.writeAll("\n  ");
    }
    try writer.writeAll("}\n}\n");

    return buffer.toOwnedSlice(allocator);
}

/// Save lock file to disk.
pub fn saveLockfile(lockfile: *const Lockfile, lockfile_path: []const u8) !void {
    const content = try generateLockfile(lockfile.allocator, lockfile);
    defer lockfile.allocator.free(content);

    const file = try fs.cwd().createFile(lockfile_path, .{});
    defer file.close();

    try file.writeAll(content);
}

/// Escape special characters for JSON string.
fn escapeJsonString(str: []const u8) []const u8 {
    // For simplicity, we assume paths don't contain special characters
    // that need escaping. A full implementation would escape \, ", etc.
    return str;
}

// Tests
test "Lockfile init and deinit" {
    var lockfile = Lockfile.init(std.testing.allocator);
    defer lockfile.deinit();

    try std.testing.expectEqual(LOCKFILE_VERSION, lockfile.version);
    try std.testing.expectEqual(@as(usize, 0), lockfile.dependencies.count());
}

test "Lockfile add dependency" {
    var lockfile = Lockfile.init(std.testing.allocator);
    defer lockfile.deinit();

    try lockfile.addDependency("utils", .{
        .source = .path,
        .original = "../utils",
        .resolved = "/home/user/projects/utils",
        .version = "0.1.0",
    });

    try std.testing.expectEqual(@as(usize, 1), lockfile.dependencies.count());

    const dep = lockfile.dependencies.get("utils").?;
    try std.testing.expectEqual(DependencySource.path, dep.source);
    try std.testing.expectEqualStrings("../utils", dep.original);
    try std.testing.expectEqualStrings("/home/user/projects/utils", dep.resolved);
    try std.testing.expectEqualStrings("0.1.0", dep.version.?);
}

test "parseLockfile basic" {
    const json =
        \\{
        \\  "version": 1,
        \\  "dependencies": {
        \\    "utils": {
        \\      "source": "path",
        \\      "path": "../utils",
        \\      "resolved": "/home/user/projects/utils"
        \\    }
        \\  }
        \\}
    ;

    var lockfile = try parseLockfile(std.testing.allocator, json);
    defer lockfile.deinit();

    try std.testing.expectEqual(@as(u32, 1), lockfile.version);
    try std.testing.expectEqual(@as(usize, 1), lockfile.dependencies.count());

    const dep = lockfile.dependencies.get("utils").?;
    try std.testing.expectEqual(DependencySource.path, dep.source);
    try std.testing.expectEqualStrings("../utils", dep.original);
    try std.testing.expectEqualStrings("/home/user/projects/utils", dep.resolved);
}

test "parseLockfile with version and commit" {
    const json =
        \\{
        \\  "version": 1,
        \\  "dependencies": {
        \\    "http": {
        \\      "source": "git",
        \\      "git": "https://github.com/klar/http.git",
        \\      "resolved": "/home/user/.klar/cache/http-abc123",
        \\      "version": "1.0.0",
        \\      "commit": "abc123def456"
        \\    }
        \\  }
        \\}
    ;

    var lockfile = try parseLockfile(std.testing.allocator, json);
    defer lockfile.deinit();

    const dep = lockfile.dependencies.get("http").?;
    try std.testing.expectEqual(DependencySource.git, dep.source);
    try std.testing.expectEqualStrings("https://github.com/klar/http.git", dep.original);
    try std.testing.expectEqualStrings("1.0.0", dep.version.?);
    try std.testing.expectEqualStrings("abc123def456", dep.commit.?);
}

test "generateLockfile roundtrip" {
    var lockfile = Lockfile.init(std.testing.allocator);
    defer lockfile.deinit();

    try lockfile.addDependency("utils", .{
        .source = .path,
        .original = "../utils",
        .resolved = "/home/user/projects/utils",
        .version = "0.1.0",
    });

    const content = try generateLockfile(std.testing.allocator, &lockfile);
    defer std.testing.allocator.free(content);

    // Parse it back
    var parsed = try parseLockfile(std.testing.allocator, content);
    defer parsed.deinit();

    try std.testing.expectEqual(@as(usize, 1), parsed.dependencies.count());

    const dep = parsed.dependencies.get("utils").?;
    try std.testing.expectEqualStrings("../utils", dep.original);
    try std.testing.expectEqualStrings("/home/user/projects/utils", dep.resolved);
}

test "Lockfile checkMismatch detects new dependency" {
    var lockfile = Lockfile.init(std.testing.allocator);
    defer lockfile.deinit();

    // Lock file has only "utils"
    try lockfile.addDependency("utils", .{
        .source = .path,
        .original = "../utils",
        .resolved = "/path/to/utils",
    });

    // Manifest has "utils" and "http"
    var manifest_deps = std.StringHashMapUnmanaged(manifest.Dependency){};
    defer manifest_deps.deinit(std.testing.allocator);

    try manifest_deps.put(std.testing.allocator, "utils", .{ .path = "../utils" });
    try manifest_deps.put(std.testing.allocator, "http", .{ .path = "../http" });

    const mismatched = try lockfile.checkMismatch(&manifest_deps, std.testing.allocator);
    defer {
        for (mismatched) |m| std.testing.allocator.free(m);
        std.testing.allocator.free(mismatched);
    }

    try std.testing.expectEqual(@as(usize, 1), mismatched.len);
    try std.testing.expectEqualStrings("http", mismatched[0]);
}
