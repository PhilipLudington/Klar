# C Interoperability Patterns

Patterns for interfacing Zig code with C libraries and codebases.

## Importing C Headers

### Basic Import

```zig
const c = @cImport({
    @cInclude("stdio.h");
    @cInclude("stdlib.h");
});

pub fn main() void {
    _ = c.printf("Hello from C!\n");
}
```

### With Defines and Include Paths

```zig
const c = @cImport({
    @cDefine("_GNU_SOURCE", {});
    @cDefine("DEBUG", "1");
    @cInclude("mylib.h");
});
```

In build.zig:
```zig
exe.addIncludePath(.{ .path = "vendor/include" });
exe.linkSystemLibrary("mylib");
```

## String Handling

### C Strings to Zig Slices

```zig
// C string (null-terminated pointer) to Zig slice
fn cStringToSlice(c_str: [*:0]const u8) []const u8 {
    return std.mem.span(c_str);
}

// Usage
const c_str: [*:0]const u8 = c.get_string();
const zig_slice = std.mem.span(c_str);
std.debug.print("String: {s}\n", .{zig_slice});
```

### Zig Slices to C Strings

```zig
// Temporary: use sentinel-terminated literal
const msg: [:0]const u8 = "Hello";
c.puts(msg.ptr);

// Dynamic: allocate with null terminator
fn toC(allocator: Allocator, slice: []const u8) ![:0]u8 {
    return allocator.dupeZ(u8, slice);
}

// Usage
const c_str = try toC(allocator, zig_slice);
defer allocator.free(c_str);
c.process(c_str.ptr);
```

### Preserving Sentinel in Function Signatures

```zig
// GOOD: Accept sentinel-terminated when needed for C
pub fn callCApi(str: [:0]const u8) void {
    c.c_function(str.ptr);  // ptr is guaranteed null-terminated
}

// BAD: Loses sentinel
pub fn callCApi(str: []const u8) void {
    c.c_function(str.ptr);  // NOT null-terminated!
}
```

## Memory Management Across Boundaries

### Pattern: Zig Allocates, Zig Frees

Safest approach - keep ownership on Zig side:

```zig
pub fn processWithC(allocator: Allocator, input: []const u8) ![]u8 {
    // Allocate buffer in Zig
    const buffer = try allocator.alloc(u8, 4096);
    errdefer allocator.free(buffer);

    // Pass to C for filling
    const written = c.fill_buffer(buffer.ptr, buffer.len);
    if (written < 0) return error.CFailed;

    // Return Zig-owned slice
    return buffer[0..@intCast(written)];
}
```

### Pattern: C Allocates, C Frees

When C library manages memory:

```zig
const CString = struct {
    ptr: [*:0]u8,

    pub fn deinit(self: CString) void {
        c.free_string(self.ptr);  // Use library's free function
    }

    pub fn slice(self: CString) []const u8 {
        return std.mem.span(self.ptr);
    }
};

pub fn getCString() !CString {
    const ptr = c.get_allocated_string() orelse return error.CAllocFailed;
    return CString{ .ptr = ptr };
}

// Usage
const str = try getCString();
defer str.deinit();
std.debug.print("{s}\n", .{str.slice()});
```

### Pattern: Using c_allocator for C Interop

When C code expects malloc/free:

```zig
const c_allocator = std.heap.c_allocator;

pub fn createForC() !*MyStruct {
    // Allocate with C's malloc
    const ptr = try c_allocator.create(MyStruct);
    ptr.* = .{};
    return ptr;
}

// C code can free with free()
```

## Callbacks

### Zig Function as C Callback

```zig
// C typedef: typedef void (*callback_fn)(void* user_data, int value);

fn zigCallback(user_data: ?*anyopaque, value: c_int) callconv(.C) void {
    const context: *MyContext = @ptrCast(@alignCast(user_data));
    context.handleValue(@intCast(value));
}

pub fn registerCallback(ctx: *MyContext) void {
    c.register_callback(zigCallback, ctx);
}
```

### Callback with Error Handling

```zig
fn safeCallback(user_data: ?*anyopaque, data: [*]const u8, len: c.size_t) callconv(.C) c_int {
    const ctx: *Context = @ptrCast(@alignCast(user_data)) orelse return -1;

    const slice = data[0..len];
    ctx.process(slice) catch |err| {
        ctx.last_error = err;
        return -1;
    };
    return 0;
}
```

## Type Conversions

### Integer Types

```zig
// C int to Zig
const zig_int: i32 = @intCast(c_int_value);

// Zig to C int (check bounds)
const c_int: c_int = @intCast(zig_value);

// Size types
const size: usize = @intCast(c_size);
const c_size: c.size_t = @intCast(zig_size);
```

### Pointer Types

```zig
// Many-pointer to slice (need length)
fn manyToSlice(ptr: [*]const u8, len: usize) []const u8 {
    return ptr[0..len];
}

// Slice to many-pointer
fn sliceToMany(slice: []const u8) [*]const u8 {
    return slice.ptr;
}

// Optional pointer (C can be NULL)
fn handleOptional(ptr: ?*c.SomeStruct) void {
    if (ptr) |p| {
        // Safe to use p
    }
}
```

### Struct Alignment

```zig
// Match C struct layout
const CCompatible = extern struct {
    x: c_int,
    y: c_int,
    name: [*:0]const u8,
};

// For binary-compatible packed structs
const Packed = packed struct {
    flags: u8,
    length: u16,
};
```

## Error Handling

### Converting C Errors to Zig

```zig
const CError = error{
    InvalidArgument,
    OutOfMemory,
    IoError,
    Unknown,
};

fn mapCError(code: c_int) CError {
    return switch (code) {
        c.ERR_INVALID => error.InvalidArgument,
        c.ERR_NOMEM => error.OutOfMemory,
        c.ERR_IO => error.IoError,
        else => error.Unknown,
    };
}

pub fn wrapCFunction(arg: i32) CError!i32 {
    const result = c.c_function(@intCast(arg));
    if (result < 0) {
        return mapCError(result);
    }
    return @intCast(result);
}
```

### Using errno

```zig
const errno = std.c.getErrno();

pub fn openFile(path: [:0]const u8) !c_int {
    const fd = c.open(path.ptr, c.O_RDONLY);
    if (fd < 0) {
        return switch (std.c.getErrno()) {
            .ENOENT => error.FileNotFound,
            .EACCES => error.AccessDenied,
            else => error.Unexpected,
        };
    }
    return fd;
}
```

## Building with C Libraries

### build.zig Configuration

```zig
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "myapp",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Add C source files
    exe.addCSourceFiles(.{
        .files = &.{ "vendor/lib.c", "vendor/util.c" },
        .flags = &.{ "-std=c11", "-O2" },
    });

    // Include paths
    exe.addIncludePath(b.path("vendor/include"));

    // Link system library
    exe.linkSystemLibrary("ssl");
    exe.linkSystemLibrary("crypto");

    // Link libc
    exe.linkLibC();

    b.installArtifact(exe);
}
```

### Static Library Linking

```zig
exe.addObjectFile(b.path("vendor/libfoo.a"));

// Or search in paths
exe.addLibraryPath(b.path("vendor/lib"));
exe.linkSystemLibrary("foo");
```

## Common Patterns

### Wrapping a C Library

```zig
// c_wrapper.zig
const std = @import("std");
const c = @cImport(@cInclude("mylib.h"));

pub const Handle = struct {
    raw: *c.handle_t,

    pub fn init() !Handle {
        const raw = c.handle_create() orelse return error.CreateFailed;
        return Handle{ .raw = raw };
    }

    pub fn deinit(self: Handle) void {
        c.handle_destroy(self.raw);
    }

    pub fn process(self: Handle, data: []const u8) ![]const u8 {
        var out_len: c.size_t = 0;
        const result = c.handle_process(
            self.raw,
            data.ptr,
            data.len,
            &out_len,
        );
        if (result == null) return error.ProcessFailed;
        return result[0..out_len];
    }
};
```

### Handling Variable-Length C Arrays

```zig
// C: struct { int count; item_t items[]; }
const CList = extern struct {
    count: c_int,

    pub fn items(self: *const CList) []const c.item_t {
        const ptr: [*]const c.item_t = @ptrFromInt(
            @intFromPtr(self) + @sizeOf(CList)
        );
        return ptr[0..@intCast(self.count)];
    }
};
```

### Thread-Safe C Wrapper

```zig
pub const ThreadSafeWrapper = struct {
    mutex: std.Thread.Mutex = .{},
    handle: *c.handle_t,

    pub fn call(self: *ThreadSafeWrapper, arg: i32) !i32 {
        self.mutex.lock();
        defer self.mutex.unlock();

        const result = c.handle_call(self.handle, @intCast(arg));
        if (result < 0) return error.CallFailed;
        return @intCast(result);
    }
};
```

## Quick Reference

| C Type | Zig Type | Conversion |
|--------|----------|------------|
| `char*` (null-term) | `[*:0]const u8` | Direct |
| `char*` + len | `[]const u8` | `ptr[0..len]` |
| `void*` | `?*anyopaque` | `@ptrCast(@alignCast(...))` |
| `int` | `c_int` | Direct, or `@intCast` |
| `size_t` | `usize` or `c.size_t` | `@intCast` |
| `NULL` | `null` | Optional types |
| `struct Foo` | `extern struct` | Match layout |
| `enum` | `c_int` or typed enum | `@intCast` |
| callback | `fn(...) callconv(.C)` | Explicit callconv |
