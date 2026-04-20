//! Zig 0.16 compatibility shims for APIs that were removed or gutted.
//!
//! This module provides drop-in replacements for std library APIs that existed
//! in Zig 0.15 but were either removed or moved behind a mandatory `Io`
//! context in Zig 0.16. The implementations here use libc directly (the
//! project already links libc) to keep the source changes minimal.
//!
//! Covers:
//!   * `std.fs.cwd()` / `std.fs.File` / `std.fs.Dir`
//!   * `std.fs.accessAbsolute`, `std.fs.selfExePath`
//!   * `std.process.argsAlloc` / `argsFree` / `getEnvVarOwned` / `exit`
//!   * `std.process.Child`
//!   * `std.heap.GeneralPurposeAllocator` (trivial rename to DebugAllocator)
//!
//! The goal is API compatibility with the Zig 0.15 surface the Klar compiler
//! was using, not full feature parity with the Zig stdlib.

const std = @import("std");
const builtin = @import("builtin");
const posix = std.posix;

// -----------------------------------------------------------------------------
// ArrayList writer shim — 0.16 removed `.writer(allocator)` on
// ArrayListUnmanaged. Provide a tiny GenericWriter-compatible wrapper that
// forwards `writeAll`/`print`/`writeByte` to the underlying list.
// -----------------------------------------------------------------------------

pub const ArrayListWriterError = std.mem.Allocator.Error;

pub fn ArrayListWriter(comptime T: type) type {
    _ = T;
    return struct {
        list: *std.ArrayListUnmanaged(u8),
        allocator: std.mem.Allocator,

        pub const Error = std.mem.Allocator.Error;

        pub fn writeAll(self: @This(), bytes: []const u8) Error!void {
            try self.list.appendSlice(self.allocator, bytes);
        }

        pub fn writeByte(self: @This(), byte: u8) Error!void {
            try self.list.append(self.allocator, byte);
        }

        pub fn print(self: @This(), comptime fmt: []const u8, args: anytype) Error!void {
            const formatted = try std.fmt.allocPrint(self.allocator, fmt, args);
            defer self.allocator.free(formatted);
            try self.list.appendSlice(self.allocator, formatted);
        }
    };
}

pub fn listWriter(list: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator) ArrayListWriter(u8) {
    return .{ .list = list, .allocator = allocator };
}

// -----------------------------------------------------------------------------
// Path helpers — re-exports from remaining stdlib pieces.
// -----------------------------------------------------------------------------

pub const path = std.fs.path;
pub const max_path_bytes = std.fs.max_path_bytes;

// -----------------------------------------------------------------------------
// Error sets matching what the 0.15 stdlib produced. Callers pattern-match
// on a handful of specific error names (FileNotFound, AccessDenied, ...).
// -----------------------------------------------------------------------------

pub const OpenError = error{
    FileNotFound,
    AccessDenied,
    IsDir,
    NotDir,
    SymLinkLoop,
    NameTooLong,
    SystemResources,
    Unexpected,
    OutOfMemory,
};

pub const ReadError = error{
    InputOutput,
    SystemResources,
    BrokenPipe,
    Unexpected,
    OutOfMemory,
};

pub const WriteError = error{
    DiskQuota,
    NoSpaceLeft,
    InputOutput,
    BrokenPipe,
    AccessDenied,
    Unexpected,
};

pub const AccessError = error{
    FileNotFound,
    AccessDenied,
    Unexpected,
};

pub const StatError = error{
    FileNotFound,
    AccessDenied,
    SystemResources,
    Unexpected,
};

pub const MakeDirError = error{
    PathAlreadyExists,
    AccessDenied,
    FileNotFound,
    NameTooLong,
    NoSpaceLeft,
    NotDir,
    Unexpected,
};

pub const DeleteError = error{
    FileNotFound,
    AccessDenied,
    IsDir,
    FileBusy,
    Unexpected,
};

pub const RenameError = error{
    FileNotFound,
    AccessDenied,
    Unexpected,
};

pub const RealPathError = error{
    FileNotFound,
    AccessDenied,
    NameTooLong,
    SystemResources,
    OutOfMemory,
    Unexpected,
};

pub const EnvVarError = error{
    EnvironmentVariableNotFound,
    OutOfMemory,
};

// -----------------------------------------------------------------------------
// Options structs — keep field names from 0.15.
// -----------------------------------------------------------------------------

pub const OpenFlags = struct {
    pub const Mode = enum { read_only, write_only, read_write };
    mode: Mode = .read_only,
};

pub const CreateFlags = struct {
    read: bool = false,
    truncate: bool = true,
    exclusive: bool = false,
};

pub const OpenDirOptions = struct {
    iterate: bool = false,
    access_sub_paths: bool = true,
    no_follow: bool = false,
};

pub const AccessOptions = struct {
    mode: u32 = 0, // F_OK
};

pub const CopyFileOptions = struct {
    override_mode: ?std.c.mode_t = null,
};

pub const Stat = struct {
    size: u64,
    mtime: i128,
    kind: Kind,

    pub const Kind = enum {
        file,
        directory,
        sym_link,
        named_pipe,
        character_device,
        block_device,
        unix_domain_socket,
        whiteout,
        door,
        event_port,
        unknown,
    };
};

// -----------------------------------------------------------------------------
// libc helpers
// -----------------------------------------------------------------------------

// Some libc extern declarations that aren't in std.c directly on all platforms.
extern "c" fn execvp(file: [*:0]const u8, argv: [*:null]const ?[*:0]const u8) c_int;
extern "c" fn dup2(oldfd: c_int, newfd: c_int) c_int;
extern "c" fn pipe(fds: *[2]c_int) c_int;
extern "c" fn _NSGetExecutablePath(buf: [*]u8, bufsize: *u32) c_int;

// Returns errno threaded through errno global. Zig's std.c has this under the
// name `_errno_location` or similar; fallback uses the `.getErrno` wrapper.
fn errnoOf(rc: anytype) posix.E {
    _ = rc;
    return posix.errno(-1);
}

fn dupeZPath(allocator: std.mem.Allocator, p: []const u8) ![:0]u8 {
    return allocator.dupeZ(u8, p);
}

// Tiny allocator-less dupeZ into a fixed buffer. Returns a sentinel slice.
fn tmpZPath(buf: *[4096]u8, p: []const u8) ![:0]u8 {
    if (p.len >= buf.len) return error.NameTooLong;
    @memcpy(buf[0..p.len], p);
    buf[p.len] = 0;
    return buf[0..p.len :0];
}

// -----------------------------------------------------------------------------
// File
// -----------------------------------------------------------------------------

pub const File = struct {
    handle: posix.fd_t,

    pub const Kind = Stat.Kind;

    pub fn close(self: File) void {
        _ = std.c.close(self.handle);
    }

    pub fn writeAll(self: File, bytes: []const u8) WriteError!void {
        var index: usize = 0;
        while (index < bytes.len) {
            const rc = std.c.write(self.handle, bytes.ptr + index, bytes.len - index);
            if (rc < 0) {
                switch (posix.errno(rc)) {
                    .INTR => continue,
                    .AGAIN => continue,
                    .BADF => return WriteError.InputOutput,
                    .DQUOT => return WriteError.DiskQuota,
                    .NOSPC => return WriteError.NoSpaceLeft,
                    .PIPE => return WriteError.BrokenPipe,
                    .IO => return WriteError.InputOutput,
                    .PERM, .ACCES => return WriteError.AccessDenied,
                    else => return WriteError.Unexpected,
                }
            }
            index += @intCast(rc);
        }
    }

    pub fn write(self: File, bytes: []const u8) WriteError!usize {
        const rc = std.c.write(self.handle, bytes.ptr, bytes.len);
        if (rc < 0) {
            switch (posix.errno(rc)) {
                .INTR, .AGAIN => return 0,
                .DQUOT => return WriteError.DiskQuota,
                .NOSPC => return WriteError.NoSpaceLeft,
                .PIPE => return WriteError.BrokenPipe,
                .IO => return WriteError.InputOutput,
                .PERM, .ACCES => return WriteError.AccessDenied,
                else => return WriteError.Unexpected,
            }
        }
        return @intCast(rc);
    }

    pub fn readAll(self: File, buffer: []u8) ReadError!usize {
        var total: usize = 0;
        while (total < buffer.len) {
            const rc = std.c.read(self.handle, buffer.ptr + total, buffer.len - total);
            if (rc < 0) {
                switch (posix.errno(rc)) {
                    .INTR => continue,
                    .AGAIN => continue,
                    .IO => return ReadError.InputOutput,
                    else => return ReadError.Unexpected,
                }
            }
            if (rc == 0) break;
            total += @intCast(rc);
        }
        return total;
    }

    pub fn read(self: File, buffer: []u8) ReadError!usize {
        while (true) {
            const rc = std.c.read(self.handle, buffer.ptr, buffer.len);
            if (rc < 0) {
                switch (posix.errno(rc)) {
                    .INTR => continue,
                    .AGAIN => continue,
                    .IO => return ReadError.InputOutput,
                    else => return ReadError.Unexpected,
                }
            }
            return @intCast(rc);
        }
    }

    pub fn getEndPos(self: File) !u64 {
        const SEEK_SET: std.c.whence_t = 0;
        const SEEK_CUR: std.c.whence_t = 1;
        const SEEK_END: std.c.whence_t = 2;
        const cur = std.c.lseek(self.handle, 0, SEEK_CUR);
        if (cur < 0) return error.Unexpected;
        const end = std.c.lseek(self.handle, 0, SEEK_END);
        if (end < 0) return error.Unexpected;
        _ = std.c.lseek(self.handle, cur, SEEK_SET);
        return @intCast(end);
    }

    pub fn stat(self: File) !Stat {
        var st: std.c.Stat = undefined;
        const rc = std.c.fstat(self.handle, &st);
        if (rc != 0) return error.Unexpected;
        return statFromCStat(st);
    }

    pub fn readToEndAlloc(self: File, allocator: std.mem.Allocator, max_bytes: usize) ![]u8 {
        const size = self.getEndPos() catch {
            // Fallback: grow-on-read.
            return readAllAllocGrow(self, allocator, max_bytes);
        };
        if (size > max_bytes) return error.FileTooBig;
        const buf = try allocator.alloc(u8, @intCast(size));
        errdefer allocator.free(buf);
        const n = try self.readAll(buf);
        return buf[0..n];
    }

    pub fn readToEndAllocOptions(
        self: File,
        allocator: std.mem.Allocator,
        max_bytes: usize,
        size_hint: ?usize,
        comptime alignment: u29,
        comptime sentinel: ?u8,
    ) ![]align(alignment) u8 {
        _ = size_hint;
        _ = sentinel;
        const size = self.getEndPos() catch {
            return error.Unexpected;
        };
        if (size > max_bytes) return error.FileTooBig;
        const raw = try allocator.alignedAlloc(u8, @enumFromInt(std.math.log2_int(u29, alignment)), @intCast(size));
        errdefer allocator.free(raw);
        const n = try self.readAll(raw);
        return raw[0..n];
    }

    pub fn seekTo(self: File, pos: u64) !void {
        const SEEK_SET: std.c.whence_t = 0;
        const rc = std.c.lseek(self.handle, @intCast(pos), SEEK_SET);
        if (rc < 0) return error.Unexpected;
    }

    pub const Writer = struct {
        file: File,
        pub fn writeAll(self: Writer, bytes: []const u8) !void {
            try self.file.writeAll(bytes);
        }
        pub fn writeByte(self: Writer, byte: u8) !void {
            try self.file.writeAll(&[_]u8{byte});
        }
        pub fn print(self: Writer, comptime fmt: []const u8, args: anytype) !void {
            var buf: [1024]u8 = undefined;
            const s = std.fmt.bufPrint(&buf, fmt, args) catch return WriteError.Unexpected;
            try self.file.writeAll(s);
        }
    };

    pub const Reader = struct {
        file: File,
        pub fn read(self: Reader, buf: []u8) !usize {
            return self.file.read(buf);
        }
        pub fn readAll(self: Reader, buf: []u8) !usize {
            return self.file.readAll(buf);
        }
    };

    pub fn writer(self: File) Writer {
        return .{ .file = self };
    }

    pub fn reader(self: File) Reader {
        return .{ .file = self };
    }
};

fn readAllAllocGrow(file: File, allocator: std.mem.Allocator, max_bytes: usize) ![]u8 {
    var buf = try allocator.alloc(u8, 4096);
    errdefer allocator.free(buf);
    var len: usize = 0;
    while (true) {
        if (len == buf.len) {
            if (buf.len >= max_bytes) return error.FileTooBig;
            const new_len = @min(buf.len * 2, max_bytes);
            buf = try allocator.realloc(buf, new_len);
        }
        const n = try file.read(buf[len..]);
        if (n == 0) break;
        len += n;
    }
    return allocator.realloc(buf, len) catch buf[0..len];
}

fn statFromCStat(st: std.c.Stat) Stat {
    const S = std.c.S;
    const kind: Stat.Kind = blk: {
        const mode = st.mode;
        if (S.ISDIR(mode)) break :blk .directory;
        if (S.ISREG(mode)) break :blk .file;
        if (S.ISLNK(mode)) break :blk .sym_link;
        if (S.ISFIFO(mode)) break :blk .named_pipe;
        if (S.ISCHR(mode)) break :blk .character_device;
        if (S.ISBLK(mode)) break :blk .block_device;
        if (S.ISSOCK(mode)) break :blk .unix_domain_socket;
        break :blk .unknown;
    };
    // On macOS, stat has st_mtimespec; on Linux it's st_mtim.
    const mtime_ns: i128 = blk: {
        if (@hasField(std.c.Stat, "mtimespec")) {
            break :blk @as(i128, st.mtimespec.sec) * std.time.ns_per_s + st.mtimespec.nsec;
        } else if (@hasField(std.c.Stat, "mtim")) {
            break :blk @as(i128, st.mtim.sec) * std.time.ns_per_s + st.mtim.nsec;
        } else {
            break :blk 0;
        }
    };
    return .{
        .size = @intCast(st.size),
        .mtime = mtime_ns,
        .kind = kind,
    };
}

// -----------------------------------------------------------------------------
// Dir
// -----------------------------------------------------------------------------

pub const Dir = struct {
    handle: posix.fd_t,

    pub fn close(self: *Dir) void {
        _ = self;
        // We don't keep track of which handles are AT_FDCWD; callers should
        // pass their own opened dirs. Most call sites don't close `cwd()`.
    }

    pub fn openFile(self: Dir, sub_path: []const u8, flags: OpenFlags) !File {
        var buf: [4096]u8 = undefined;
        const zpath = try tmpZPath(&buf, sub_path);
        const o_flag: c_int = switch (flags.mode) {
            .read_only => @as(c_int, 0),
            .write_only => @as(c_int, 1),
            .read_write => @as(c_int, 2),
        };
        const fd = std.c.openat(self.handle, zpath.ptr, @bitCast(o_flag), @as(std.c.mode_t, 0));
        if (fd < 0) {
            return mapOpenErrno();
        }
        return File{ .handle = fd };
    }

    pub fn createFile(self: Dir, sub_path: []const u8, flags: CreateFlags) !File {
        var buf: [4096]u8 = undefined;
        const zpath = try tmpZPath(&buf, sub_path);
        var o_flag: c_int = if (flags.read) 2 else 1; // O_RDWR or O_WRONLY
        o_flag |= 0o100; // O_CREAT
        if (flags.truncate) o_flag |= 0o1000; // O_TRUNC
        if (flags.exclusive) o_flag |= 0o200; // O_EXCL
        const mode: std.c.mode_t = @as(std.c.mode_t, 0o644);
        const fd = std.c.openat(self.handle, zpath.ptr, @bitCast(o_flag), mode);
        if (fd < 0) {
            return mapOpenErrno();
        }
        return File{ .handle = fd };
    }

    pub fn openDir(self: Dir, sub_path: []const u8, options: OpenDirOptions) !Dir {
        _ = options;
        var buf: [4096]u8 = undefined;
        const zpath = try tmpZPath(&buf, sub_path);
        const O_DIRECTORY: c_int = if (builtin.os.tag == .macos) 0x100000 else 0o200000;
        const fd = std.c.openat(self.handle, zpath.ptr, @bitCast(O_DIRECTORY), @as(std.c.mode_t, 0));
        if (fd < 0) {
            return mapOpenErrno();
        }
        return Dir{ .handle = fd };
    }

    pub fn access(self: Dir, sub_path: []const u8, options: AccessOptions) AccessError!void {
        _ = options;
        var buf: [4096]u8 = undefined;
        const zpath = tmpZPath(&buf, sub_path) catch return AccessError.Unexpected;
        // Use faccessat via libc; absolute paths resolve ignoring dirfd.
        // libc has `faccessat(dirfd, path, mode, flags)`; declare extern.
        const rc = faccessat(self.handle, zpath.ptr, 0, 0);
        if (rc != 0) return error.FileNotFound;
    }

    pub fn statFile(self: Dir, sub_path: []const u8) !Stat {
        var buf: [4096]u8 = undefined;
        const zpath = try tmpZPath(&buf, sub_path);
        var st: std.c.Stat = undefined;
        const rc = fstatat(self.handle, zpath.ptr, &st, 0);
        if (rc != 0) return error.FileNotFound;
        return statFromCStat(st);
    }

    pub fn makePath(self: Dir, sub_path: []const u8) !void {
        // Recursive mkdir -p behavior.
        var buf: [4096]u8 = undefined;
        if (sub_path.len >= buf.len) return error.NameTooLong;
        @memcpy(buf[0..sub_path.len], sub_path);

        var i: usize = 0;
        while (i < sub_path.len) : (i += 1) {
            if (buf[i] == '/' and i > 0) {
                buf[i] = 0;
                _ = mkdirat_wrap(self.handle, @ptrCast(&buf[0]), 0o755);
                buf[i] = '/';
            }
        }
        buf[sub_path.len] = 0;
        const rc = mkdirat_wrap(self.handle, @ptrCast(&buf[0]), 0o755);
        if (rc != 0) {
            // EEXIST is fine; other errors bubble up.
            const errno = posix.errno(rc);
            if (errno != .EXIST) return error.AccessDenied;
        }
    }

    pub fn makeDir(self: Dir, sub_path: []const u8) !void {
        var buf: [4096]u8 = undefined;
        const zpath = try tmpZPath(&buf, sub_path);
        const rc = mkdirat_wrap(self.handle, zpath.ptr, 0o755);
        if (rc != 0) {
            const errno = posix.errno(rc);
            if (errno == .EXIST) return error.PathAlreadyExists;
            return error.AccessDenied;
        }
    }

    pub fn deleteFile(self: Dir, sub_path: []const u8) DeleteError!void {
        var buf: [4096]u8 = undefined;
        const zpath = tmpZPath(&buf, sub_path) catch return DeleteError.AccessDenied;
        const rc = std.c.unlinkat(self.handle, zpath.ptr, 0);
        if (rc != 0) {
            const errno = posix.errno(rc);
            switch (errno) {
                .NOENT => return DeleteError.FileNotFound,
                .ACCES, .PERM => return DeleteError.AccessDenied,
                .ISDIR => return DeleteError.IsDir,
                .BUSY => return DeleteError.FileBusy,
                else => return DeleteError.Unexpected,
            }
        }
    }

    pub fn rename(self: Dir, old_sub_path: []const u8, new_sub_path: []const u8) RenameError!void {
        var old_buf: [4096]u8 = undefined;
        var new_buf: [4096]u8 = undefined;
        const old_z = tmpZPath(&old_buf, old_sub_path) catch return RenameError.Unexpected;
        const new_z = tmpZPath(&new_buf, new_sub_path) catch return RenameError.Unexpected;
        const rc = std.c.renameat(self.handle, old_z.ptr, self.handle, new_z.ptr);
        if (rc != 0) {
            const errno = posix.errno(rc);
            switch (errno) {
                .NOENT => return RenameError.FileNotFound,
                .ACCES, .PERM => return RenameError.AccessDenied,
                else => return RenameError.Unexpected,
            }
        }
    }

    pub fn deleteTree(self: Dir, sub_path: []const u8) !void {
        // Best-effort recursive delete using POSIX nftw-style open+iterate.
        var dir = self.openDir(sub_path, .{ .iterate = true }) catch |err| switch (err) {
            else => return err,
        };
        defer _ = std.c.close(dir.handle);

        var it = dir.iterate();
        while (try it.next()) |entry| {
            switch (entry.kind) {
                .directory => try dir.deleteTree(entry.name),
                else => dir.deleteFile(entry.name) catch {},
            }
        }
        var parent_buf: [4096]u8 = undefined;
        const zpath = tmpZPath(&parent_buf, sub_path) catch return;
        // Use unlinkat with AT_REMOVEDIR flag (0x200 on Linux/macOS common).
        const AT_REMOVEDIR: c_int = 0x200;
        _ = std.c.unlinkat(self.handle, zpath.ptr, AT_REMOVEDIR);
    }

    pub fn copyFile(src_dir: Dir, src_sub_path: []const u8, dest_dir: Dir, dest_sub_path: []const u8, options: CopyFileOptions) !void {
        _ = options;
        const src_file = try src_dir.openFile(src_sub_path, .{});
        defer src_file.close();
        const dst_file = try dest_dir.createFile(dest_sub_path, .{});
        defer dst_file.close();

        var buf: [8192]u8 = undefined;
        while (true) {
            const n = try src_file.read(&buf);
            if (n == 0) break;
            try dst_file.writeAll(buf[0..n]);
        }
    }

    pub fn realpath(self: Dir, sub_path: []const u8, out_buffer: []u8) ![]u8 {
        _ = self;
        var zbuf: [4096]u8 = undefined;
        const zpath = try tmpZPath(&zbuf, sub_path);
        var resolved: [4096]u8 = undefined;
        const rc_opt = realpath_libc(zpath.ptr, &resolved);
        if (rc_opt == null) return error.FileNotFound;
        const len = std.mem.indexOfScalar(u8, &resolved, 0) orelse resolved.len;
        if (len > out_buffer.len) return error.NameTooLong;
        @memcpy(out_buffer[0..len], resolved[0..len]);
        return out_buffer[0..len];
    }

    pub fn realpathAlloc(self: Dir, allocator: std.mem.Allocator, sub_path: []const u8) ![]u8 {
        var zbuf: [4096]u8 = undefined;
        const zpath = try tmpZPath(&zbuf, sub_path);
        _ = self;
        var resolved: [4096]u8 = undefined;
        const rc_opt = realpath_libc(zpath.ptr, &resolved);
        if (rc_opt == null) return error.FileNotFound;
        const len = std.mem.indexOfScalar(u8, &resolved, 0) orelse resolved.len;
        const out = try allocator.alloc(u8, len);
        @memcpy(out, resolved[0..len]);
        return out;
    }

    pub fn readFileAlloc(self: Dir, allocator: std.mem.Allocator, sub_path: []const u8, max_bytes: usize) ![]u8 {
        const file = try self.openFile(sub_path, .{});
        defer file.close();
        return try file.readToEndAlloc(allocator, max_bytes);
    }

    pub fn writeFile(self: Dir, options: WriteFileOptions) !void {
        const file = try self.createFile(options.sub_path, .{});
        defer file.close();
        try file.writeAll(options.data);
    }

    pub const WriteFileOptions = struct {
        sub_path: []const u8,
        data: []const u8,
        flags: CreateFlags = .{},
    };

    pub fn iterate(self: Dir) Iterator {
        // Open a fresh DIR* via dup to avoid consuming the caller's fd.
        const dup_fd = std.c.dup(self.handle);
        const dir_ptr: ?*std.c.DIR = if (dup_fd >= 0) std.c.fdopendir(dup_fd) else null;
        return Iterator{ .dir = dir_ptr, .first = true };
    }

    pub const Iterator = struct {
        dir: ?*std.c.DIR,
        first: bool,

        pub const Entry = struct {
            name: []const u8,
            kind: Stat.Kind,
        };

        pub fn next(self: *Iterator) !?Entry {
            const dir = self.dir orelse return null;
            while (true) {
                const ent = std.c.readdir(dir) orelse {
                    return null;
                };
                const name_ptr: [*:0]const u8 = @ptrCast(&ent.name);
                const name = std.mem.span(name_ptr);
                if (std.mem.eql(u8, name, ".") or std.mem.eql(u8, name, "..")) continue;
                const kind: Stat.Kind = switch (ent.type) {
                    // POSIX DT_ constants
                    1 => .named_pipe, // DT_FIFO
                    2 => .character_device, // DT_CHR
                    4 => .directory, // DT_DIR
                    6 => .block_device, // DT_BLK
                    8 => .file, // DT_REG
                    10 => .sym_link, // DT_LNK
                    12 => .unix_domain_socket, // DT_SOCK
                    else => .unknown,
                };
                return Entry{ .name = name, .kind = kind };
            }
        }

        pub fn deinit(self: *Iterator) void {
            if (self.dir) |d| _ = std.c.closedir(d);
            self.dir = null;
        }
    };

    pub fn walk(self: Dir, allocator: std.mem.Allocator) !Walker {
        var stack = std.ArrayListUnmanaged(Walker.StackItem).empty;
        try stack.append(allocator, .{
            .iter = self.iterate(),
            .dir_handle = self.handle,
            .owned = false,
            .path = try allocator.dupe(u8, ""),
        });
        return Walker{
            .allocator = allocator,
            .stack = stack,
            .name_buffer = std.ArrayListUnmanaged(u8).empty,
        };
    }

    pub const Walker = struct {
        allocator: std.mem.Allocator,
        stack: std.ArrayListUnmanaged(StackItem),
        name_buffer: std.ArrayListUnmanaged(u8),

        pub const StackItem = struct {
            iter: Iterator,
            dir_handle: posix.fd_t,
            owned: bool,
            path: []const u8,
        };

        pub const Entry = struct {
            basename: []const u8,
            path: []const u8,
            kind: Stat.Kind,
            dir: Dir,
        };

        pub fn next(self: *Walker) !?Entry {
            while (self.stack.items.len > 0) {
                const top = &self.stack.items[self.stack.items.len - 1];
                const maybe_entry = try top.iter.next();
                if (maybe_entry) |e| {
                    self.name_buffer.clearRetainingCapacity();
                    try self.name_buffer.appendSlice(self.allocator, top.path);
                    if (top.path.len > 0) try self.name_buffer.append(self.allocator, '/');
                    try self.name_buffer.appendSlice(self.allocator, e.name);
                    const full_path = self.name_buffer.items;

                    if (e.kind == .directory) {
                        const parent_dir = Dir{ .handle = top.dir_handle };
                        const subdir = parent_dir.openDir(e.name, .{ .iterate = true }) catch {
                            return Entry{
                                .basename = e.name,
                                .path = full_path,
                                .kind = e.kind,
                                .dir = Dir{ .handle = -1 },
                            };
                        };
                        const path_dup = try self.allocator.dupe(u8, full_path);
                        try self.stack.append(self.allocator, .{
                            .iter = subdir.iterate(),
                            .dir_handle = subdir.handle,
                            .owned = true,
                            .path = path_dup,
                        });
                        return Entry{
                            .basename = e.name,
                            .path = full_path,
                            .kind = e.kind,
                            .dir = subdir,
                        };
                    }
                    return Entry{
                        .basename = e.name,
                        .path = full_path,
                        .kind = e.kind,
                        .dir = Dir{ .handle = top.dir_handle },
                    };
                }
                var done = self.stack.pop() orelse break;
                done.iter.deinit();
                if (done.owned and done.dir_handle >= 0) _ = std.c.close(done.dir_handle);
                self.allocator.free(done.path);
            }
            return null;
        }

        pub fn deinit(self: *Walker) void {
            while (self.stack.items.len > 0) {
                var item = self.stack.pop() orelse break;
                item.iter.deinit();
                if (item.owned and item.dir_handle >= 0) _ = std.c.close(item.dir_handle);
                self.allocator.free(item.path);
            }
            self.stack.deinit(self.allocator);
            self.name_buffer.deinit(self.allocator);
        }
    };
};

fn mapOpenErrno() anyerror {
    return switch (posix.errno(-1)) {
        .NOENT => error.FileNotFound,
        .ACCES, .PERM => error.AccessDenied,
        .ISDIR => error.IsDir,
        .NOTDIR => error.NotDir,
        .LOOP => error.SymLinkLoop,
        .NAMETOOLONG => error.NameTooLong,
        else => error.Unexpected,
    };
}

extern "c" fn faccessat(dirfd: c_int, path: [*:0]const u8, mode: c_int, flags: c_int) c_int;
extern "c" fn fstatat(dirfd: c_int, path: [*:0]const u8, buf: *std.c.Stat, flags: c_int) c_int;
const realpath_libc = @extern(*const fn ([*:0]const u8, [*]u8) callconv(.c) ?[*:0]u8, .{ .name = "realpath" });

fn mkdirat_wrap(dirfd: posix.fd_t, p: [*:0]const u8, mode: std.c.mode_t) c_int {
    return std.c.mkdirat(dirfd, p, mode);
}

// -----------------------------------------------------------------------------
// cwd()
// -----------------------------------------------------------------------------

pub fn cwd() Dir {
    // AT_FDCWD is -2 on macOS/BSD, -100 on Linux.
    const AT_FDCWD: posix.fd_t = if (builtin.os.tag == .macos or
        builtin.os.tag == .freebsd or
        builtin.os.tag == .netbsd or
        builtin.os.tag == .openbsd or
        builtin.os.tag == .dragonfly) -2 else -100;
    return Dir{ .handle = AT_FDCWD };
}

pub fn accessAbsolute(absolute_path: []const u8, options: AccessOptions) AccessError!void {
    _ = options;
    var buf: [4096]u8 = undefined;
    const zpath = tmpZPath(&buf, absolute_path) catch return AccessError.Unexpected;
    const rc = std.c.access(zpath.ptr, 0);
    if (rc != 0) return AccessError.FileNotFound;
}

pub fn selfExePath(out_buffer: []u8) ![]u8 {
    if (builtin.os.tag == .macos) {
        var size: u32 = @intCast(out_buffer.len);
        const rc = _NSGetExecutablePath(out_buffer.ptr, &size);
        if (rc != 0) return error.NameTooLong;
        const len = std.mem.indexOfScalar(u8, out_buffer, 0) orelse size;
        return out_buffer[0..len];
    } else if (builtin.os.tag == .linux) {
        const rc = std.c.readlink("/proc/self/exe", out_buffer.ptr, out_buffer.len);
        if (rc < 0) return error.Unexpected;
        return out_buffer[0..@intCast(rc)];
    } else {
        return error.Unsupported;
    }
}

// -----------------------------------------------------------------------------
// Process: args, env, exit
// -----------------------------------------------------------------------------

/// Cached argv captured once in main(). Stored as raw C-style strings so we
/// can lazily materialise allocator-owned slices on demand. On posix targets
/// the values point into the Minimal.args.vector from Zig 0.16; the
/// lifetime of the pointed-to bytes is guaranteed by `Minimal`, which lives
/// for the entire process.
var cached_argv: []const [*:0]const u8 = &.{};

pub fn initArgs(minimal: std.process.Init.Minimal) void {
    // On non-posix platforms (Windows/Wasi) the Vector type differs; this
    // compat module only targets the platforms currently exercised by the
    // Klar build (macOS, Linux).
    cached_argv = minimal.args.vector;
}

pub fn argsAllocFromMinimal(minimal: std.process.Init.Minimal, allocator: std.mem.Allocator) ![][:0]u8 {
    initArgs(minimal);
    return argsAlloc(allocator);
}

pub fn argsAlloc(allocator: std.mem.Allocator) ![][:0]u8 {
    const out = try allocator.alloc([:0]u8, cached_argv.len);
    errdefer allocator.free(out);
    for (cached_argv, 0..) |arg, i| {
        const slice = std.mem.span(arg);
        out[i] = try allocator.dupeZ(u8, slice);
    }
    return out;
}

pub fn argsFree(allocator: std.mem.Allocator, args: [][:0]u8) void {
    for (args) |a| allocator.free(a);
    allocator.free(args);
}

pub fn getEnvVarOwned(allocator: std.mem.Allocator, key: []const u8) ![]u8 {
    var buf: [4096]u8 = undefined;
    const zkey = try tmpZPath(&buf, key);
    const val = std.c.getenv(zkey.ptr) orelse return EnvVarError.EnvironmentVariableNotFound;
    const slice = std.mem.span(val);
    return allocator.dupe(u8, slice);
}

pub fn exit(code: u8) noreturn {
    std.c.exit(code);
    unreachable;
}

// -----------------------------------------------------------------------------
// Time helpers
// -----------------------------------------------------------------------------

extern "c" fn time(tloc: ?*c_longlong) c_longlong;

pub fn timestamp() i64 {
    return @intCast(time(null));
}

pub fn milliTimestamp() i64 {
    var ts: std.c.timespec = undefined;
    const rc = std.c.clock_gettime(.REALTIME, &ts);
    if (rc != 0) {
        return @as(i64, @intCast(time(null))) * 1000;
    }
    return @as(i64, @intCast(ts.sec)) * 1000 + @divTrunc(@as(i64, @intCast(ts.nsec)), 1_000_000);
}

pub fn posixFork() !i32 {
    const pid = std.c.fork();
    if (pid < 0) return error.ForkFailed;
    return @intCast(pid);
}

pub fn posixExecve(exe_path: [*:0]const u8, argv: [*:null]const ?[*:0]const u8, envp: [*:null]const ?[*:0]const u8) !void {
    _ = std.c.execve(exe_path, argv, envp);
    return error.ExecFailed;
}

pub fn posixWaitpid(pid: i32) i32 {
    var status: c_int = 0;
    while (true) {
        const rc = std.c.waitpid(@intCast(pid), &status, 0);
        if (rc < 0) {
            if (posix.errno(rc) == .INTR) continue;
            return -1;
        }
        return status;
    }
}

pub fn nanoTimestamp() i128 {
    var ts: std.c.timespec = undefined;
    const rc = std.c.clock_gettime(.REALTIME, &ts);
    if (rc != 0) {
        return @as(i128, @intCast(time(null))) * std.time.ns_per_s;
    }
    return @as(i128, @intCast(ts.sec)) * std.time.ns_per_s + @as(i128, @intCast(ts.nsec));
}

// -----------------------------------------------------------------------------
// Child process
// -----------------------------------------------------------------------------

pub const Child = struct {
    pub const StdIo = enum { Inherit, Pipe, Ignore, Close };
    pub const Term = union(enum) {
        Exited: u32,
        Signal: u32,
        Stopped: u32,
        Unknown: u32,
    };

    argv: []const []const u8,
    allocator: std.mem.Allocator,
    stdin_behavior: StdIo = .Inherit,
    stdout_behavior: StdIo = .Inherit,
    stderr_behavior: StdIo = .Inherit,
    stdin: ?File = null,
    stdout: ?File = null,
    stderr: ?File = null,
    pid: i32 = 0,

    pub fn init(argv: []const []const u8, allocator: std.mem.Allocator) Child {
        return .{ .argv = argv, .allocator = allocator };
    }

    pub fn spawn(self: *Child) !void {
        // Convert argv to null-terminated C strings.
        var argv_buf = try self.allocator.alloc(?[*:0]const u8, self.argv.len + 1);
        defer self.allocator.free(argv_buf);
        var owned: std.ArrayListUnmanaged([:0]u8) = .empty;
        defer {
            for (owned.items) |s| self.allocator.free(s);
            owned.deinit(self.allocator);
        }
        for (self.argv, 0..) |a, i| {
            const dup = try self.allocator.dupeZ(u8, a);
            try owned.append(self.allocator, dup);
            argv_buf[i] = dup.ptr;
        }
        argv_buf[self.argv.len] = null;

        // Set up pipes for stdout/stderr if needed.
        var stdout_pipe: [2]c_int = .{ -1, -1 };
        var stderr_pipe: [2]c_int = .{ -1, -1 };
        if (self.stdout_behavior == .Pipe) {
            if (pipe(&stdout_pipe) != 0) return error.PipeFailed;
        }
        if (self.stderr_behavior == .Pipe) {
            if (pipe(&stderr_pipe) != 0) return error.PipeFailed;
        }

        const pid = std.c.fork();
        if (pid < 0) return error.ForkFailed;
        if (pid == 0) {
            // Child process.
            if (self.stdout_behavior == .Pipe) {
                _ = std.c.close(stdout_pipe[0]);
                _ = dup2(stdout_pipe[1], 1);
                _ = std.c.close(stdout_pipe[1]);
            }
            if (self.stderr_behavior == .Pipe) {
                _ = std.c.close(stderr_pipe[0]);
                _ = dup2(stderr_pipe[1], 2);
                _ = std.c.close(stderr_pipe[1]);
            }
            const argv_ptr: [*:null]const ?[*:0]const u8 = @ptrCast(argv_buf.ptr);
            _ = execvp(argv_buf[0].?, argv_ptr);
            std.c.exit(127);
            unreachable;
        }

        self.pid = @intCast(pid);
        if (self.stdout_behavior == .Pipe) {
            _ = std.c.close(stdout_pipe[1]);
            self.stdout = File{ .handle = stdout_pipe[0] };
        }
        if (self.stderr_behavior == .Pipe) {
            _ = std.c.close(stderr_pipe[1]);
            self.stderr = File{ .handle = stderr_pipe[0] };
        }
    }

    pub fn wait(self: *Child) !Term {
        var status: c_int = 0;
        while (true) {
            const rc = std.c.waitpid(self.pid, &status, 0);
            if (rc < 0) {
                if (posix.errno(rc) == .INTR) continue;
                return error.WaitFailed;
            }
            break;
        }
        // Decode exit status.
        if ((status & 0x7F) == 0) {
            return Term{ .Exited = @intCast((status >> 8) & 0xFF) };
        } else if (((status & 0x7F) + 1) >> 1 > 0) {
            return Term{ .Signal = @intCast(status & 0x7F) };
        }
        return Term{ .Unknown = @intCast(status) };
    }

    pub fn spawnAndWait(self: *Child) !Term {
        try self.spawn();
        return self.wait();
    }
};
