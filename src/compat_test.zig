//! Tests for the POSIX half of `compat.zig` that pin the target's own libc
//! constants (Bugs 68, 69). They live beside the shim, not in it, because
//! `compat.zig` is already over the file-size limit (DEBT.md Debt 9).

const std = @import("std");
const builtin = @import("builtin");
const compat = @import("compat.zig");

const is_windows = builtin.os.tag == .windows;

fn scratchName(buf: []u8, comptime stem: []const u8) ![]const u8 {
    return std.fmt.bufPrint(buf, "klar-{s}-{d}.tmp", .{ stem, std.c.getpid() });
}

test "Dir.createFile truncates: a shorter rewrite leaves only the new bytes" {
    // Pins Bug 68: on macOS the Linux literal for O_TRUNC set O_CREAT instead,
    // so a rewrite kept the old file's tail.
    if (is_windows) return error.SkipZigTest;
    const dir = compat.cwd();
    var name_buf: [64]u8 = undefined;
    const name = try scratchName(&name_buf, "bug68-trunc");
    defer dir.deleteFile(name) catch {};
    try dir.writeFile(.{ .sub_path = name, .data = "LONG CONTENT HERE 1234567890" });
    try dir.writeFile(.{ .sub_path = name, .data = "short" });
    const got = try dir.readFileAlloc(std.testing.allocator, name, 64);
    defer std.testing.allocator.free(got);
    try std.testing.expectEqualStrings("short", got);
}

test "Dir.createFile exclusive refuses an existing file with PathAlreadyExists" {
    // Pins Bug 68: on macOS the Linux literal for O_EXCL set O_FSYNC instead,
    // so an exclusive create opened the existing file.
    if (is_windows) return error.SkipZigTest;
    const dir = compat.cwd();
    var name_buf: [64]u8 = undefined;
    const name = try scratchName(&name_buf, "bug68-excl");
    defer dir.deleteFile(name) catch {};
    try dir.writeFile(.{ .sub_path = name, .data = "existing" });
    if (dir.createFile(name, .{ .exclusive = true })) |file| {
        file.close();
        return error.TestExpectedError;
    } else |err| try std.testing.expectEqual(error.PathAlreadyExists, err);
    const got = try dir.readFileAlloc(std.testing.allocator, name, 64);
    defer std.testing.allocator.free(got);
    try std.testing.expectEqualStrings("existing", got);
}

test "Dir.createFile exclusive creates a file that does not exist" {
    // The other half of O_EXCL: it refuses only an existing path.
    if (is_windows) return error.SkipZigTest;
    const dir = compat.cwd();
    var name_buf: [64]u8 = undefined;
    const name = try scratchName(&name_buf, "bug68-excl-new");
    dir.deleteFile(name) catch {};
    defer dir.deleteFile(name) catch {};
    const file = try dir.createFile(name, .{ .exclusive = true });
    try file.writeAll("fresh");
    file.close();
    const got = try dir.readFileAlloc(std.testing.allocator, name, 64);
    defer std.testing.allocator.free(got);
    try std.testing.expectEqualStrings("fresh", got);
}

test "Dir.deleteTree removes nested directories, not only their files" {
    // Pins Bug 69: on macOS the Linux value of AT_REMOVEDIR made each final
    // unlinkat a plain file unlink, so every directory stayed on disk.
    if (is_windows) return error.SkipZigTest;
    const dir = compat.cwd();
    var root_buf: [64]u8 = undefined;
    const root = try scratchName(&root_buf, "bug69-tree");
    var sub_buf: [128]u8 = undefined;
    const sub = try std.fmt.bufPrint(&sub_buf, "{s}/sub/deeper", .{root});
    var file_buf: [160]u8 = undefined;
    const file = try std.fmt.bufPrint(&file_buf, "{s}/leaf.txt", .{sub});
    try dir.makePath(sub);
    try dir.writeFile(.{ .sub_path = file, .data = "leaf" });
    try dir.deleteTree(root);
    try std.testing.expectError(error.FileNotFound, dir.access(root, .{}));
}
