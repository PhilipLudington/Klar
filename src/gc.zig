const std = @import("std");
const Allocator = std.mem.Allocator;
const vm_value = @import("vm_value.zig");
const Value = vm_value.Value;

// ============================================================================
// GC Configuration
// ============================================================================

/// Initial heap size threshold before triggering GC (bytes).
const initial_gc_threshold: usize = 1024 * 1024; // 1 MB

/// Growth factor for heap threshold after collection.
const gc_heap_grow_factor: usize = 2;

// ============================================================================
// Object Type Tags
// ============================================================================

/// Type tag for heap-allocated objects.
/// Must be kept in sync with object types in vm_value.zig.
pub const ObjType = enum(u8) {
    string,
    array,
    tuple,
    struct_,
    closure,
    upvalue,
    function,
    native,
    optional,
    range,
};

// ============================================================================
// Object Header
// ============================================================================

/// Common header for all GC-managed objects.
/// This header is placed at the start of every heap-allocated object.
/// The object's data immediately follows this header in memory.
pub const ObjHeader = struct {
    /// Type of the object (for traversal during mark phase).
    obj_type: ObjType,

    /// GC mark flag - true if object is reachable.
    is_marked: bool,

    /// Next object in the linked list of all objects.
    next: ?*ObjHeader,

    /// Initialize a new object header.
    pub fn init(obj_type: ObjType) ObjHeader {
        return .{
            .obj_type = obj_type,
            .is_marked = false,
            .next = null,
        };
    }
};

// ============================================================================
// Garbage Collector
// ============================================================================

/// Mark-and-sweep garbage collector for the Klar VM.
pub const GC = struct {
    /// Backing allocator for actual memory operations.
    backing_allocator: Allocator,

    /// Head of linked list of all allocated objects.
    objects: ?*ObjHeader,

    /// Total bytes allocated.
    bytes_allocated: usize,

    /// Threshold for triggering next GC.
    next_gc: usize,

    /// String interning table (hash -> ObjString pointer).
    /// Key is the string hash, value is the interned string object.
    strings: std.AutoHashMapUnmanaged(u32, *vm_value.ObjString),

    /// Gray stack for tri-color marking (worklist of objects to traverse).
    gray_stack: std.ArrayListUnmanaged(*ObjHeader),

    /// Roots provided by the VM for marking.
    roots: Roots,

    /// Debug mode - log GC operations.
    debug_gc: bool,

    /// Stress test mode - GC on every allocation.
    stress_gc: bool,

    /// Root set for GC traversal.
    pub const Roots = struct {
        /// Pointer to VM stack array.
        stack: ?[*]Value,
        /// Current stack top index.
        stack_top: *usize,
        /// Global variables table.
        globals: ?*std.StringHashMapUnmanaged(Value),
        /// Open upvalues linked list head.
        open_upvalues: *?*vm_value.ObjUpvalue,
        /// Currently executing call frames (for closure references).
        frames: ?[*]CallFrame,
        /// Current frame count.
        frame_count: *usize,
    };

    /// Placeholder for CallFrame - will be properly imported when integrating.
    pub const CallFrame = extern struct {
        closure: *vm_value.ObjClosure,
        ip: usize,
        slots_base: usize,
    };

    // -------------------------------------------------------------------------
    // Initialization
    // -------------------------------------------------------------------------

    pub fn init(backing_allocator: Allocator) GC {
        return .{
            .backing_allocator = backing_allocator,
            .objects = null,
            .bytes_allocated = 0,
            .next_gc = initial_gc_threshold,
            .strings = .{},
            .gray_stack = .{},
            .roots = .{
                .stack = null,
                .stack_top = undefined,
                .globals = null,
                .open_upvalues = undefined,
                .frames = null,
                .frame_count = undefined,
            },
            .debug_gc = false,
            .stress_gc = false,
        };
    }

    pub fn deinit(self: *GC) void {
        // Free all objects
        self.freeObjects();

        // Free intern table
        self.strings.deinit(self.backing_allocator);

        // Free gray stack
        self.gray_stack.deinit(self.backing_allocator);
    }

    /// Set root pointers for GC traversal.
    /// Must be called before any allocations that might trigger GC.
    pub fn setRoots(
        self: *GC,
        stack: [*]Value,
        stack_top: *usize,
        globals: *std.StringHashMapUnmanaged(Value),
        open_upvalues: *?*vm_value.ObjUpvalue,
        frames: [*]CallFrame,
        frame_count: *usize,
    ) void {
        self.roots = .{
            .stack = stack,
            .stack_top = stack_top,
            .globals = globals,
            .open_upvalues = open_upvalues,
            .frames = frames,
            .frame_count = frame_count,
        };
    }

    // -------------------------------------------------------------------------
    // Object Allocation
    // -------------------------------------------------------------------------

    /// Allocate a new object of the given type.
    /// The object is automatically registered with the GC.
    pub fn allocObject(self: *GC, comptime T: type, obj_type: ObjType) !*T {
        // Stress test: collect on every allocation
        if (self.stress_gc) {
            self.collectGarbage();
        } else if (self.bytes_allocated > self.next_gc) {
            self.collectGarbage();
        }

        const obj = try self.backing_allocator.create(T);
        const size = @sizeOf(T);

        // Initialize the header (assuming T has a 'header' field)
        obj.header = ObjHeader.init(obj_type);

        // Link into object list
        obj.header.next = self.objects;
        self.objects = &obj.header;

        self.bytes_allocated += size;

        if (self.debug_gc) {
            std.debug.print("GC: Allocated {d} bytes for {s} at {*}\n", .{
                size,
                @tagName(obj_type),
                obj,
            });
        }

        return obj;
    }

    /// Allocate memory tracked by GC (for variable-sized data like arrays).
    pub fn allocBytes(self: *GC, comptime T: type, n: usize) ![]T {
        // May trigger GC
        if (self.stress_gc) {
            self.collectGarbage();
        } else if (self.bytes_allocated > self.next_gc) {
            self.collectGarbage();
        }

        const bytes = try self.backing_allocator.alloc(T, n);
        self.bytes_allocated += n * @sizeOf(T);
        return bytes;
    }

    /// Free memory tracked by GC.
    pub fn freeBytes(self: *GC, comptime T: type, slice: []T) void {
        const size = slice.len * @sizeOf(T);
        self.bytes_allocated -= size;
        self.backing_allocator.free(slice);
    }

    // -------------------------------------------------------------------------
    // String Interning
    // -------------------------------------------------------------------------

    /// Intern a string - returns existing interned string if found,
    /// or creates and interns a new one.
    pub fn internString(self: *GC, chars: []const u8) !*vm_value.ObjString {
        const hash = hashString(chars);

        // Check if already interned
        if (self.strings.get(hash)) |existing| {
            // Verify it's the same string (handle hash collisions)
            if (std.mem.eql(u8, existing.chars, chars)) {
                return existing;
            }
        }

        // Create new interned string
        const obj = try self.allocObject(vm_value.ObjString, .string);
        const owned = try self.allocBytes(u8, chars.len);
        @memcpy(owned, chars);

        obj.chars = owned;
        obj.hash = hash;
        obj.owns_data = true;

        // Add to intern table
        try self.strings.put(self.backing_allocator, hash, obj);

        return obj;
    }

    /// Take ownership of an existing string slice (for concatenation results).
    pub fn takeString(self: *GC, chars: []u8) !*vm_value.ObjString {
        const hash = hashString(chars);

        // Check if already interned
        if (self.strings.get(hash)) |existing| {
            if (std.mem.eql(u8, existing.chars, chars)) {
                // Free the duplicate and return existing
                self.freeBytes(u8, chars);
                return existing;
            }
        }

        // Create new interned string
        const obj = try self.allocObject(vm_value.ObjString, .string);
        obj.chars = chars;
        obj.hash = hash;
        obj.owns_data = true;

        try self.strings.put(self.backing_allocator, hash, obj);
        return obj;
    }

    // -------------------------------------------------------------------------
    // Garbage Collection - Mark Phase
    // -------------------------------------------------------------------------

    /// Perform garbage collection.
    pub fn collectGarbage(self: *GC) void {
        if (self.debug_gc) {
            std.debug.print("GC: -- gc begin\n", .{});
        }

        const before = self.bytes_allocated;

        // Mark phase
        self.markRoots();
        self.traceReferences();

        // Remove unreachable interned strings
        self.removeWhiteStrings();

        // Sweep phase
        self.sweep();

        // Update threshold
        self.next_gc = self.bytes_allocated * gc_heap_grow_factor;

        if (self.debug_gc) {
            std.debug.print("GC: -- gc end\n", .{});
            std.debug.print("GC:    collected {d} bytes (from {d} to {d}), next at {d}\n", .{
                before - self.bytes_allocated,
                before,
                self.bytes_allocated,
                self.next_gc,
            });
        }
    }

    /// Mark all root objects.
    fn markRoots(self: *GC) void {
        // Mark stack values
        if (self.roots.stack) |stack| {
            const top = self.roots.stack_top.*;
            for (0..top) |i| {
                self.markValue(stack[i]);
            }
        }

        // Mark global variables
        if (self.roots.globals) |globals| {
            var iter = globals.valueIterator();
            while (iter.next()) |value| {
                self.markValue(value.*);
            }
        }

        // Mark closures in call frames
        if (self.roots.frames) |frames| {
            const count = self.roots.frame_count.*;
            for (0..count) |i| {
                self.markObject(&frames[i].closure.header);
            }
        }

        // Mark open upvalues
        var upvalue = self.roots.open_upvalues.*;
        while (upvalue) |uv| {
            self.markObject(&uv.header);
            upvalue = uv.next;
        }
    }

    /// Mark a single value (if it contains an object reference).
    pub fn markValue(self: *GC, value: Value) void {
        switch (value) {
            .string => |s| self.markObject(&s.header),
            .array => |a| self.markObject(&a.header),
            .tuple => |t| self.markObject(&t.header),
            .struct_ => |s| self.markObject(&s.header),
            .closure => |c| self.markObject(&c.header),
            .function => |f| self.markObject(&f.header),
            .native => |n| self.markObject(&n.header),
            .upvalue => |u| self.markObject(&u.header),
            .optional => |o| self.markObject(&o.header),
            .range => |r| self.markObject(&r.header),
            // Primitive values don't need marking
            .int, .float, .bool_, .char_, .void_ => {},
        }
    }

    /// Mark an object as reachable.
    fn markObject(self: *GC, header: *ObjHeader) void {
        if (header.is_marked) return;

        if (self.debug_gc) {
            std.debug.print("GC: mark {*} ({s})\n", .{ header, @tagName(header.obj_type) });
        }

        header.is_marked = true;

        // Add to gray stack for later traversal
        self.gray_stack.append(self.backing_allocator, header) catch {
            // If we can't grow the gray stack, we have bigger problems
            @panic("GC: Out of memory for gray stack");
        };
    }

    /// Trace references from gray objects.
    fn traceReferences(self: *GC) void {
        while (self.gray_stack.pop()) |obj| {
            self.blackenObject(obj);
        }
    }

    /// Process a gray object - mark all objects it references.
    fn blackenObject(self: *GC, header: *ObjHeader) void {
        if (self.debug_gc) {
            std.debug.print("GC: blacken {*} ({s})\n", .{ header, @tagName(header.obj_type) });
        }

        switch (header.obj_type) {
            .string, .native, .range => {
                // No outgoing references
            },
            .array => {
                const arr: *vm_value.ObjArray = @alignCast(@fieldParentPtr("header", header));
                for (arr.items) |item| {
                    self.markValue(item);
                }
            },
            .tuple => {
                const tup: *vm_value.ObjTuple = @alignCast(@fieldParentPtr("header", header));
                for (tup.items) |item| {
                    self.markValue(item);
                }
            },
            .struct_ => {
                const s: *vm_value.ObjStruct = @alignCast(@fieldParentPtr("header", header));
                var iter = s.fields.valueIterator();
                while (iter.next()) |value| {
                    self.markValue(value.*);
                }
            },
            .closure => {
                const closure: *vm_value.ObjClosure = @alignCast(@fieldParentPtr("header", header));
                // Mark constants in the function's chunk (function prototypes are not GC-managed)
                self.markFunctionConstants(closure.function);
                // Mark upvalues
                for (closure.upvalues) |uv| {
                    self.markObject(&uv.header);
                }
            },
            .upvalue => {
                const uv: *vm_value.ObjUpvalue = @alignCast(@fieldParentPtr("header", header));
                self.markValue(uv.closed);
            },
            .function => {
                const func: *vm_value.ObjFunction = @alignCast(@fieldParentPtr("header", header));
                // Mark constants in the function's chunk
                self.markFunctionConstants(func.function);
            },
            .optional => {
                const opt: *vm_value.ObjOptional = @alignCast(@fieldParentPtr("header", header));
                if (opt.value) |v| {
                    self.markValue(v.*);
                }
            },
        }
    }

    /// Mark constants in a function's chunk.
    fn markFunctionConstants(self: *GC, function: *chunk_mod.Function) void {
        // Mark string constants and nested functions
        for (function.chunk.constants.items) |constant| {
            switch (constant) {
                .string => |s| {
                    // String constant - look up in intern table and mark if found
                    const hash = hashString(s);
                    if (self.strings.get(hash)) |str_obj| {
                        self.markObject(&str_obj.header);
                    }
                },
                .function => |f| {
                    // Nested function - recursively mark
                    self.markFunctionConstants(f);
                },
                else => {},
            }
        }
    }

    // Import chunk_mod for function constant marking
    const chunk_mod = @import("chunk.zig");

    // -------------------------------------------------------------------------
    // Garbage Collection - Sweep Phase
    // -------------------------------------------------------------------------

    /// Remove unreachable strings from intern table.
    fn removeWhiteStrings(self: *GC) void {
        var to_remove = std.ArrayListUnmanaged(u32){};
        defer to_remove.deinit(self.backing_allocator);

        var iter = self.strings.iterator();
        while (iter.next()) |entry| {
            if (!entry.value_ptr.*.header.is_marked) {
                to_remove.append(self.backing_allocator, entry.key_ptr.*) catch continue;
            }
        }

        for (to_remove.items) |hash| {
            _ = self.strings.remove(hash);
        }
    }

    /// Sweep unreachable objects.
    fn sweep(self: *GC) void {
        var previous: ?*ObjHeader = null;
        var object = self.objects;

        while (object) |obj| {
            if (obj.is_marked) {
                // Object is reachable - unmark for next cycle
                obj.is_marked = false;
                previous = obj;
                object = obj.next;
            } else {
                // Object is unreachable - free it
                const unreached = obj;
                object = obj.next;

                if (previous) |prev| {
                    prev.next = object;
                } else {
                    self.objects = object;
                }

                self.freeObject(unreached);
            }
        }
    }

    /// Free a single object.
    fn freeObject(self: *GC, header: *ObjHeader) void {
        if (self.debug_gc) {
            std.debug.print("GC: free {*} ({s})\n", .{ header, @tagName(header.obj_type) });
        }

        switch (header.obj_type) {
            .string => {
                const obj: *vm_value.ObjString = @alignCast(@fieldParentPtr("header", header));
                if (obj.owns_data) {
                    self.freeBytes(u8, @constCast(obj.chars));
                }
                self.bytes_allocated -= @sizeOf(vm_value.ObjString);
                self.backing_allocator.destroy(obj);
            },
            .array => {
                const obj: *vm_value.ObjArray = @alignCast(@fieldParentPtr("header", header));
                self.freeBytes(Value, obj.items);
                self.bytes_allocated -= @sizeOf(vm_value.ObjArray);
                self.backing_allocator.destroy(obj);
            },
            .tuple => {
                const obj: *vm_value.ObjTuple = @alignCast(@fieldParentPtr("header", header));
                self.freeBytes(Value, obj.items);
                self.bytes_allocated -= @sizeOf(vm_value.ObjTuple);
                self.backing_allocator.destroy(obj);
            },
            .struct_ => {
                const obj: *vm_value.ObjStruct = @alignCast(@fieldParentPtr("header", header));
                obj.fields.deinit(self.backing_allocator);
                self.bytes_allocated -= @sizeOf(vm_value.ObjStruct);
                self.backing_allocator.destroy(obj);
            },
            .closure => {
                const obj: *vm_value.ObjClosure = @alignCast(@fieldParentPtr("header", header));
                self.freeBytes(*vm_value.ObjUpvalue, obj.upvalues);
                self.bytes_allocated -= @sizeOf(vm_value.ObjClosure);
                self.backing_allocator.destroy(obj);
            },
            .upvalue => {
                const obj: *vm_value.ObjUpvalue = @alignCast(@fieldParentPtr("header", header));
                self.bytes_allocated -= @sizeOf(vm_value.ObjUpvalue);
                self.backing_allocator.destroy(obj);
            },
            .function => {
                const obj: *vm_value.ObjFunction = @alignCast(@fieldParentPtr("header", header));
                self.bytes_allocated -= @sizeOf(vm_value.ObjFunction);
                self.backing_allocator.destroy(obj);
            },
            .native => {
                const obj: *vm_value.ObjNative = @alignCast(@fieldParentPtr("header", header));
                self.bytes_allocated -= @sizeOf(vm_value.ObjNative);
                self.backing_allocator.destroy(obj);
            },
            .optional => {
                const obj: *vm_value.ObjOptional = @alignCast(@fieldParentPtr("header", header));
                if (obj.value) |v| {
                    self.backing_allocator.destroy(v);
                }
                self.bytes_allocated -= @sizeOf(vm_value.ObjOptional);
                self.backing_allocator.destroy(obj);
            },
            .range => {
                const obj: *vm_value.ObjRange = @alignCast(@fieldParentPtr("header", header));
                self.bytes_allocated -= @sizeOf(vm_value.ObjRange);
                self.backing_allocator.destroy(obj);
            },
        }
    }

    /// Free all objects (for VM shutdown).
    fn freeObjects(self: *GC) void {
        var object = self.objects;
        while (object) |obj| {
            const next = obj.next;
            self.freeObject(obj);
            object = next;
        }
        self.objects = null;
    }
};

// ============================================================================
// Hash Function (FNV-1a)
// ============================================================================

pub fn hashString(chars: []const u8) u32 {
    var hash: u32 = 2166136261;
    for (chars) |c| {
        hash ^= c;
        hash *%= 16777619;
    }
    return hash;
}

// ============================================================================
// Tests
// ============================================================================

test "GC initialization" {
    var gc = GC.init(std.testing.allocator);
    defer gc.deinit();

    try std.testing.expect(gc.objects == null);
    try std.testing.expectEqual(@as(usize, 0), gc.bytes_allocated);
}

test "hash string" {
    const hash1 = hashString("hello");
    const hash2 = hashString("hello");
    const hash3 = hashString("world");

    try std.testing.expectEqual(hash1, hash2);
    try std.testing.expect(hash1 != hash3);
}

test "string interning" {
    var gc = GC.init(std.testing.allocator);
    defer gc.deinit();

    // Intern the same string twice - should return same object
    const str1 = try gc.internString("hello");
    const str2 = try gc.internString("hello");

    try std.testing.expectEqual(str1, str2);
    try std.testing.expectEqualStrings("hello", str1.chars);
}

test "string interning different strings" {
    var gc = GC.init(std.testing.allocator);
    defer gc.deinit();

    const str1 = try gc.internString("hello");
    const str2 = try gc.internString("world");

    try std.testing.expect(str1 != str2);
    try std.testing.expectEqualStrings("hello", str1.chars);
    try std.testing.expectEqualStrings("world", str2.chars);
}

test "GC tracks allocated bytes" {
    var gc = GC.init(std.testing.allocator);
    defer gc.deinit();

    const initial_bytes = gc.bytes_allocated;
    _ = try gc.internString("test");

    try std.testing.expect(gc.bytes_allocated > initial_bytes);
}

test "GC object linked list" {
    var gc = GC.init(std.testing.allocator);
    defer gc.deinit();

    // Create multiple objects
    _ = try gc.internString("first");
    _ = try gc.internString("second");
    _ = try gc.internString("third");

    // Verify objects are linked
    try std.testing.expect(gc.objects != null);

    // Count objects in list
    var count: usize = 0;
    var obj = gc.objects;
    while (obj) |o| : (obj = o.next) {
        count += 1;
    }
    try std.testing.expectEqual(@as(usize, 3), count);
}
