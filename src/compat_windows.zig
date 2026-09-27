//! Windows half of `compat.zig` (Bug 66).
//!
//! `compat.zig` implements the Zig 0.15 file, directory and process surface
//! over libc and POSIX file descriptors, which Windows does not have (no
//! `openat`, `fstatat`, `fork`, `readdir`, and the aarch64-windows build does
//! not link libc at all). On Windows each `compat` entry point forwards here,
//! and this file implements it over Zig 0.16's own cross-platform `std.Io`
//! (`std.Io.Dir`, `std.Io.File`, `std.process.spawn`), run on the process-wide
//! single-threaded `Io`. `compat.File.handle` and `compat.Dir.handle` are
//! `std.posix.fd_t`, which is a Windows `HANDLE` there, so a compat handle and
//! an `std.Io` handle are the same value.
//!
//! Only `compat.zig` imports this file, and only from branches taken when the
//! target is Windows; Zig analyzes it for no other target.

const std = @import("std");
const compat = @import("compat.zig");

const Io = std.Io;
const Handle = std.posix.fd_t;

/// The single-threaded `Io` every Windows compat call runs on. It is std's
/// `init_single_threaded` (the process environment block, no worker threads)
/// with a real allocator: `global_single_threaded` has `Allocator.failing`,
/// and `std.process.spawn` builds the command line and searches PATH in an
/// arena over it, so every spawn there failed with `OutOfMemory` (Bug 72).
var threaded: Io.Threaded = init: {
    var t = Io.Threaded.init_single_threaded;
    t.allocator = std.heap.page_allocator;
    break :init t;
};

fn io() Io {
    return threaded.io();
}

/// Maps a `std.Io` error onto the compat error set `E` by name.
/// `PermissionDenied` becomes `AccessDenied` (0.15 had only the latter); any
/// error `E` does not name becomes `error.Unexpected`, which every compat
/// error set passed here contains.
fn mapErr(comptime E: type, err: anyerror) E {
    const name = if (err == error.PermissionDenied) "AccessDenied" else @errorName(err);
    inline for (@typeInfo(E).error_set.?) |field| {
        if (std.mem.eql(u8, field.name, name)) return @field(E, field.name);
    }
    return error.Unexpected;
}

fn ioFile(handle: Handle) Io.File {
    return .{ .handle = handle, .flags = .{ .nonblocking = false } };
}

fn ioDir(handle: Handle) Io.Dir {
    return .{ .handle = handle };
}

fn kindOf(kind: Io.File.Kind) compat.Stat.Kind {
    return std.meta.stringToEnum(compat.Stat.Kind, @tagName(kind)) orelse .unknown;
}

fn statOf(st: Io.File.Stat) compat.Stat {
    return .{
        .size = st.size,
        .mtime = @intCast(st.mtime.nanoseconds),
        .kind = kindOf(st.kind),
    };
}

// -----------------------------------------------------------------------------
// File
// -----------------------------------------------------------------------------

pub fn fileClose(handle: Handle) void {
    ioFile(handle).close(io());
}

pub fn fileWriteAll(handle: Handle, bytes: []const u8) compat.WriteError!void {
    ioFile(handle).writeStreamingAll(io(), bytes) catch |err| return mapErr(compat.WriteError, err);
}

pub fn fileWrite(handle: Handle, bytes: []const u8) compat.WriteError!usize {
    try fileWriteAll(handle, bytes);
    return bytes.len;
}

pub fn fileRead(handle: Handle, buffer: []u8) compat.ReadError!usize {
    if (buffer.len == 0) return 0;
    var bufs = [_][]u8{buffer};
    return ioFile(handle).readStreaming(io(), &bufs) catch |err| switch (err) {
        error.EndOfStream => 0,
        else => mapErr(compat.ReadError, err),
    };
}

pub fn fileReadAll(handle: Handle, buffer: []u8) compat.ReadError!usize {
    var total: usize = 0;
    while (total < buffer.len) {
        const n = try fileRead(handle, buffer[total..]);
        if (n == 0) break;
        total += n;
    }
    return total;
}

/// The file's length. Unlike the POSIX path this does not move the file
/// position, so it does not need to restore it.
pub fn fileGetEndPos(handle: Handle) !u64 {
    return ioFile(handle).length(io()) catch error.Unexpected;
}

pub fn fileStat(handle: Handle) !compat.Stat {
    const st = ioFile(handle).stat(io()) catch return error.Unexpected;
    return statOf(st);
}

pub fn fileSeekTo(handle: Handle, pos: u64) !void {
    const windows = std.os.windows;
    var distance: windows.LARGE_INTEGER = @intCast(pos);
    _ = &distance;
    if (windows.kernel32.SetFilePointerEx(handle, distance, null, windows.FILE_BEGIN) == 0)
        return error.Unexpected;
}

// -----------------------------------------------------------------------------
// Dir
// -----------------------------------------------------------------------------

pub fn cwd() Handle {
    return Io.Dir.cwd().handle;
}

pub fn dirOpenFile(handle: Handle, sub_path: []const u8, flags: compat.OpenFlags) compat.OpenError!Handle {
    const mode: Io.Dir.OpenFileOptions.Mode = switch (flags.mode) {
        .read_only => .read_only,
        .write_only => .write_only,
        .read_write => .read_write,
    };
    const file = ioDir(handle).openFile(io(), sub_path, .{ .mode = mode }) catch |err|
        return mapErr(compat.OpenError, err);
    return file.handle;
}

pub fn dirCreateFile(handle: Handle, sub_path: []const u8, flags: compat.CreateFlags) compat.OpenError!Handle {
    const file = ioDir(handle).createFile(io(), sub_path, .{
        .read = flags.read,
        .truncate = flags.truncate,
        .exclusive = flags.exclusive,
    }) catch |err| return mapErr(compat.OpenError, err);
    return file.handle;
}

pub fn dirOpenDir(handle: Handle, sub_path: []const u8, options: compat.OpenDirOptions) compat.OpenError!Handle {
    const dir = ioDir(handle).openDir(io(), sub_path, .{
        .iterate = options.iterate,
        .access_sub_paths = options.access_sub_paths,
        .follow_symlinks = !options.no_follow,
    }) catch |err| return mapErr(compat.OpenError, err);
    return dir.handle;
}

pub fn dirClose(handle: Handle) void {
    ioDir(handle).close(io());
}

pub fn dirAccess(handle: Handle, sub_path: []const u8) compat.AccessError!void {
    ioDir(handle).access(io(), sub_path, .{}) catch return error.FileNotFound;
}

pub fn dirStatFile(handle: Handle, sub_path: []const u8) !compat.Stat {
    const st = ioDir(handle).statFile(io(), sub_path, .{}) catch return error.FileNotFound;
    return statOf(st);
}

pub fn dirMakePath(handle: Handle, sub_path: []const u8) !void {
    ioDir(handle).createDirPath(io(), sub_path) catch return error.AccessDenied;
}

pub fn dirMakeDir(handle: Handle, sub_path: []const u8) !void {
    ioDir(handle).createDir(io(), sub_path, .default_dir) catch |err| switch (err) {
        error.PathAlreadyExists => return error.PathAlreadyExists,
        else => return error.AccessDenied,
    };
}

pub fn dirDeleteFile(handle: Handle, sub_path: []const u8) compat.DeleteError!void {
    ioDir(handle).deleteFile(io(), sub_path) catch |err| return mapErr(compat.DeleteError, err);
}

pub fn dirRename(handle: Handle, old_sub_path: []const u8, new_sub_path: []const u8) compat.RenameError!void {
    Io.Dir.rename(ioDir(handle), old_sub_path, ioDir(handle), new_sub_path, io()) catch |err|
        return mapErr(compat.RenameError, err);
}

pub fn dirDeleteTree(handle: Handle, sub_path: []const u8) !void {
    try ioDir(handle).deleteTree(io(), sub_path);
}

pub fn dirRealpath(handle: Handle, sub_path: []const u8, out_buffer: []u8) ![]u8 {
    var resolved: [std.fs.max_path_bytes]u8 = undefined;
    const n = ioDir(handle).realPathFile(io(), sub_path, &resolved) catch return error.FileNotFound;
    if (n > out_buffer.len) return error.NameTooLong;
    @memcpy(out_buffer[0..n], resolved[0..n]);
    return out_buffer[0..n];
}

pub fn dirRealpathAlloc(handle: Handle, allocator: std.mem.Allocator, sub_path: []const u8) ![]u8 {
    var resolved: [std.fs.max_path_bytes]u8 = undefined;
    const n = ioDir(handle).realPathFile(io(), sub_path, &resolved) catch return error.FileNotFound;
    return allocator.dupe(u8, resolved[0..n]);
}

/// Directory iteration state for `compat.Dir.Iterator` on Windows.
pub const IterState = Io.Dir.Iterator;

pub fn iterInit(handle: Handle) IterState {
    return ioDir(handle).iterate();
}

pub fn iterNext(state: *IterState) !?compat.Dir.Iterator.Entry {
    const entry = (state.next(io()) catch return error.Unexpected) orelse return null;
    return .{ .name = entry.name, .kind = kindOf(entry.kind) };
}

// -----------------------------------------------------------------------------
// Process, environment, time
// -----------------------------------------------------------------------------

pub fn accessAbsolute(absolute_path: []const u8) compat.AccessError!void {
    Io.Dir.accessAbsolute(io(), absolute_path, .{}) catch return error.FileNotFound;
}

pub fn selfExePath(out_buffer: []u8) ![]u8 {
    const n = std.process.executablePath(io(), out_buffer) catch |err| switch (err) {
        error.NameTooLong => return error.NameTooLong,
        else => return error.Unexpected,
    };
    return out_buffer[0..n];
}

var cached_args: std.process.Args = .{ .vector = &.{} };

pub fn initArgs(minimal: std.process.Init.Minimal) void {
    cached_args = minimal.args;
}

/// argv as WTF-8, one allocation per argument, same shape as the POSIX path.
pub fn argsAlloc(allocator: std.mem.Allocator) ![][:0]u8 {
    var it = try std.process.Args.Iterator.initAllocator(cached_args, allocator);
    defer it.deinit();
    var out: std.ArrayListUnmanaged([:0]u8) = .empty;
    errdefer {
        for (out.items) |a| allocator.free(a);
        out.deinit(allocator);
    }
    while (it.next()) |arg| {
        try out.append(allocator, try allocator.dupeZ(u8, arg));
    }
    return out.toOwnedSlice(allocator);
}

pub fn getEnvVarOwned(allocator: std.mem.Allocator, key: []const u8) ![]u8 {
    const environ: std.process.Environ = .{ .block = .global };
    return environ.getAlloc(allocator, key) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.EnvironmentVariableNotFound,
    };
}

pub fn exit(code: u8) noreturn {
    std.process.exit(code);
}

pub fn nanoTimestamp() i128 {
    return Io.Clock.real.now(io()).nanoseconds;
}

// -----------------------------------------------------------------------------
// Child process
// -----------------------------------------------------------------------------

fn stdIoOf(behavior: compat.Child.StdIo) std.process.SpawnOptions.StdIo {
    return switch (behavior) {
        .Inherit => .inherit,
        .Pipe => .pipe,
        .Ignore => .ignore,
        .Close => .close,
    };
}

pub fn childSpawn(self: *compat.Child) !void {
    const child = try std.process.spawn(io(), .{
        .argv = self.argv,
        .stdin = stdIoOf(self.stdin_behavior),
        .stdout = stdIoOf(self.stdout_behavior),
        .stderr = stdIoOf(self.stderr_behavior),
    });
    self.win_child = child;
    if (child.stdin) |f| self.stdin = .{ .handle = f.handle };
    if (child.stdout) |f| self.stdout = .{ .handle = f.handle };
    if (child.stderr) |f| self.stderr = .{ .handle = f.handle };
}

pub fn childWait(self: *compat.Child) !compat.Child.Term {
    var child = &(self.win_child orelse return error.WaitFailed);
    const term = child.wait(io()) catch return error.WaitFailed;
    // `std.process.Child.wait` closes the pipes it opened; drop the copies so
    // no caller closes a handle Windows may already have reused.
    self.win_child = null;
    self.stdin = null;
    self.stdout = null;
    self.stderr = null;
    return switch (term) {
        .exited => |code| .{ .Exited = code },
        .signal => |sig| .{ .Signal = @intFromEnum(sig) },
        .stopped => |sig| .{ .Stopped = @intFromEnum(sig) },
        .unknown => |code| .{ .Unknown = code },
    };
}
