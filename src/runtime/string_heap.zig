// ============================================================================
// Klar Heap-Allocated String Runtime
// ============================================================================
//
// Implements the String type - a heap-allocated, growable UTF-8 string.
//
// Memory layout:
// StringHeader { ptr: *u8, len: i32, capacity: i32 }
//
// The string data is null-terminated for C interop compatibility.
// Length (len) does NOT include the null terminator.
// Capacity (capacity) DOES include space for the null terminator.

const std = @import("std");
const alloc = @import("alloc.zig");

/// Runtime representation of a String.
/// Layout: { ptr: *u8, len: i32, capacity: i32 }
pub const StringHeader = extern struct {
    ptr: ?[*]u8,
    len: i32,
    capacity: i32,
};

const INITIAL_CAPACITY: i32 = 16;

/// Create a new empty String.
/// Returns a StringHeader struct with ptr=null, len=0, capacity=0.
export fn klar_string_new() StringHeader {
    return StringHeader{
        .ptr = null,
        .len = 0,
        .capacity = 0,
    };
}

/// Create a new String with pre-allocated capacity.
/// capacity: initial capacity to allocate (not including null terminator)
export fn klar_string_with_capacity(capacity: i32) StringHeader {
    if (capacity <= 0) {
        return StringHeader{
            .ptr = null,
            .len = 0,
            .capacity = 0,
        };
    }

    // Allocate capacity + 1 for null terminator
    const size = @as(usize, @intCast(capacity)) + 1;
    const ptr = alloc.klar_alloc(size, 0); // alignment log2 = 0 for u8

    if (ptr) |p| {
        const typed_ptr: [*]u8 = @ptrCast(p);
        typed_ptr[0] = 0; // Null terminate
        return StringHeader{
            .ptr = typed_ptr,
            .len = 0,
            .capacity = capacity + 1,
        };
    }

    return StringHeader{
        .ptr = null,
        .len = 0,
        .capacity = 0,
    };
}

/// Create a String from a null-terminated C string (string literal).
/// Makes a copy of the input data.
export fn klar_string_from(src: [*:0]const u8) StringHeader {
    var len: usize = 0;
    while (src[len] != 0) : (len += 1) {}

    if (len == 0) {
        return klar_string_new();
    }

    // Allocate len + 1 for null terminator
    const size = len + 1;
    const ptr = alloc.klar_alloc(size, 0);

    if (ptr) |p| {
        const typed_ptr: [*]u8 = @ptrCast(p);
        // Copy including null terminator
        for (0..size) |i| {
            typed_ptr[i] = src[i];
        }
        return StringHeader{
            .ptr = typed_ptr,
            .len = @intCast(len),
            .capacity = @intCast(size),
        };
    }

    return StringHeader{
        .ptr = null,
        .len = 0,
        .capacity = 0,
    };
}

/// Clone a String, creating a deep copy.
export fn klar_string_clone_heap(s: *const StringHeader) StringHeader {
    if (s.len == 0 or s.ptr == null) {
        return StringHeader{
            .ptr = null,
            .len = 0,
            .capacity = 0,
        };
    }

    // Allocate exactly what's needed + null terminator
    const size = @as(usize, @intCast(s.len)) + 1;
    const new_ptr = alloc.klar_alloc(size, 0);

    if (new_ptr) |p| {
        const typed_ptr: [*]u8 = @ptrCast(p);
        const src_ptr = s.ptr.?;
        // Copy including null terminator
        for (0..size) |i| {
            typed_ptr[i] = src_ptr[i];
        }
        return StringHeader{
            .ptr = typed_ptr,
            .len = s.len,
            .capacity = @intCast(size),
        };
    }

    return StringHeader{
        .ptr = null,
        .len = 0,
        .capacity = 0,
    };
}

/// Get the length of the String (in bytes, not including null terminator).
export fn klar_string_len_heap(s: *const StringHeader) i32 {
    return s.len;
}

/// Check if the String is empty.
export fn klar_string_is_empty_heap(s: *const StringHeader) bool {
    return s.len == 0;
}

/// Get the capacity of the String (including null terminator space).
export fn klar_string_capacity(s: *const StringHeader) i32 {
    return s.capacity;
}

/// Push a single byte onto the String.
/// Note: This is for ASCII characters. For full UTF-8, use push_char.
export fn klar_string_push_byte(s: *StringHeader, byte: u8) void {
    // Check if we need to grow (need space for byte + null terminator)
    if (s.len + 2 > s.capacity) {
        const new_capacity: i32 = if (s.capacity == 0) INITIAL_CAPACITY else s.capacity * 2;
        const new_size = @as(usize, @intCast(new_capacity));

        if (s.ptr) |old_ptr| {
            const old_size = @as(usize, @intCast(s.capacity));
            const maybe_new = alloc.klar_realloc(@ptrCast(old_ptr), old_size, new_size, 0);
            if (maybe_new) |new| {
                s.ptr = @ptrCast(new);
            } else {
                return; // Allocation failed
            }
        } else {
            const maybe_new = alloc.klar_alloc(new_size, 0);
            if (maybe_new) |new| {
                s.ptr = @ptrCast(new);
            } else {
                return; // Allocation failed
            }
        }
        s.capacity = new_capacity;
    }

    // Add the byte
    if (s.ptr) |p| {
        p[@intCast(s.len)] = byte;
        s.len += 1;
        p[@intCast(s.len)] = 0; // Null terminate
    }
}

/// Push a Unicode codepoint (char) onto the String, encoding as UTF-8.
export fn klar_string_push_char(s: *StringHeader, codepoint: i32) void {
    if (codepoint < 0) return;
    const cp: u32 = @intCast(codepoint);

    if (cp < 0x80) {
        // ASCII: 1 byte
        klar_string_push_byte(s, @intCast(cp));
    } else if (cp < 0x800) {
        // 2 bytes
        klar_string_push_byte(s, @intCast(0xC0 | (cp >> 6)));
        klar_string_push_byte(s, @intCast(0x80 | (cp & 0x3F)));
    } else if (cp < 0x10000) {
        // 3 bytes
        klar_string_push_byte(s, @intCast(0xE0 | (cp >> 12)));
        klar_string_push_byte(s, @intCast(0x80 | ((cp >> 6) & 0x3F)));
        klar_string_push_byte(s, @intCast(0x80 | (cp & 0x3F)));
    } else if (cp < 0x110000) {
        // 4 bytes
        klar_string_push_byte(s, @intCast(0xF0 | (cp >> 18)));
        klar_string_push_byte(s, @intCast(0x80 | ((cp >> 12) & 0x3F)));
        klar_string_push_byte(s, @intCast(0x80 | ((cp >> 6) & 0x3F)));
        klar_string_push_byte(s, @intCast(0x80 | (cp & 0x3F)));
    }
    // Invalid codepoints are silently ignored
}

/// Append another String to this one.
export fn klar_string_append(s: *StringHeader, other: *const StringHeader) void {
    if (other.len == 0 or other.ptr == null) return;

    // Ensure we have enough capacity
    const new_len = s.len + other.len;
    if (new_len + 1 > s.capacity) {
        var new_capacity = if (s.capacity == 0) INITIAL_CAPACITY else s.capacity;
        while (new_capacity < new_len + 1) {
            new_capacity *= 2;
        }
        const new_size = @as(usize, @intCast(new_capacity));

        if (s.ptr) |old_ptr| {
            const old_size = @as(usize, @intCast(s.capacity));
            const maybe_new = alloc.klar_realloc(@ptrCast(old_ptr), old_size, new_size, 0);
            if (maybe_new) |new| {
                s.ptr = @ptrCast(new);
            } else {
                return;
            }
        } else {
            const maybe_new = alloc.klar_alloc(new_size, 0);
            if (maybe_new) |new| {
                s.ptr = @ptrCast(new);
            } else {
                return;
            }
        }
        s.capacity = new_capacity;
    }

    // Copy the other string's bytes (not including its null terminator)
    if (s.ptr) |p| {
        const other_ptr = other.ptr.?;
        const start = @as(usize, @intCast(s.len));
        for (0..@intCast(other.len)) |i| {
            p[start + i] = other_ptr[i];
        }
        s.len = new_len;
        p[@intCast(s.len)] = 0; // Null terminate
    }
}

/// Concatenate two Strings, returning a new String.
export fn klar_string_concat(a: *const StringHeader, b: *const StringHeader) StringHeader {
    if (a.len == 0 and b.len == 0) {
        return klar_string_new();
    }

    const total_len = a.len + b.len;
    const size = @as(usize, @intCast(total_len)) + 1;
    const ptr = alloc.klar_alloc(size, 0);

    if (ptr) |p| {
        const typed_ptr: [*]u8 = @ptrCast(p);
        var idx: usize = 0;

        // Copy first string
        if (a.ptr) |a_ptr| {
            for (0..@intCast(a.len)) |i| {
                typed_ptr[idx] = a_ptr[i];
                idx += 1;
            }
        }

        // Copy second string
        if (b.ptr) |b_ptr| {
            for (0..@intCast(b.len)) |i| {
                typed_ptr[idx] = b_ptr[i];
                idx += 1;
            }
        }

        typed_ptr[idx] = 0; // Null terminate

        return StringHeader{
            .ptr = typed_ptr,
            .len = total_len,
            .capacity = @intCast(size),
        };
    }

    return StringHeader{
        .ptr = null,
        .len = 0,
        .capacity = 0,
    };
}

/// Get a pointer to the underlying null-terminated C string.
/// Returns null if the string is empty.
export fn klar_string_as_ptr(s: *const StringHeader) ?[*:0]const u8 {
    if (s.ptr) |p| {
        return @ptrCast(p);
    }
    return null;
}

/// Clear the String (set len to 0 but keep capacity).
export fn klar_string_clear(s: *StringHeader) void {
    s.len = 0;
    if (s.ptr) |p| {
        p[0] = 0; // Null terminate at start
    }
}

/// Free the String's memory.
export fn klar_string_drop(s: *StringHeader) void {
    if (s.ptr) |p| {
        const size = @as(usize, @intCast(s.capacity));
        alloc.klar_free(@ptrCast(p), size, 0);
        s.ptr = null;
        s.len = 0;
        s.capacity = 0;
    }
}

/// Check if two Strings are equal.
export fn klar_string_eq(a: *const StringHeader, b: *const StringHeader) bool {
    if (a.len != b.len) return false;
    if (a.len == 0) return true; // Both empty

    const a_ptr = a.ptr orelse return false;
    const b_ptr = b.ptr orelse return false;

    for (0..@intCast(a.len)) |i| {
        if (a_ptr[i] != b_ptr[i]) return false;
    }
    return true;
}

/// Compare a String with a string literal.
export fn klar_string_eq_literal(s: *const StringHeader, lit: [*:0]const u8) bool {
    // Get literal length
    var lit_len: usize = 0;
    while (lit[lit_len] != 0) : (lit_len += 1) {}

    if (@as(usize, @intCast(s.len)) != lit_len) return false;
    if (lit_len == 0) return true;

    const s_ptr = s.ptr orelse return false;
    for (0..lit_len) |i| {
        if (s_ptr[i] != lit[i]) return false;
    }
    return true;
}

/// Hash a String using FNV-1a.
export fn klar_string_hash_heap(s: *const StringHeader) i64 {
    const FNV_OFFSET: u64 = 14695981039346656037;
    const FNV_PRIME: u64 = 1099511628211;

    var hash: u64 = FNV_OFFSET;

    if (s.ptr) |p| {
        for (0..@intCast(s.len)) |i| {
            hash ^= p[i];
            hash *%= FNV_PRIME;
        }
    }

    return @bitCast(hash);
}

// ============================================================================
// Tests
// ============================================================================

test "String new creates empty string" {
    const s = klar_string_new();
    try std.testing.expectEqual(@as(i32, 0), s.len);
    try std.testing.expectEqual(@as(i32, 0), s.capacity);
    try std.testing.expect(s.ptr == null);
}

test "String from copies literal" {
    var s = klar_string_from("hello");
    defer klar_string_drop(&s);

    try std.testing.expectEqual(@as(i32, 5), s.len);
    try std.testing.expect(s.ptr != null);
    try std.testing.expect(klar_string_eq_literal(&s, "hello"));
}

test "String push_byte grows" {
    var s = klar_string_new();
    defer klar_string_drop(&s);

    klar_string_push_byte(&s, 'a');
    try std.testing.expectEqual(@as(i32, 1), s.len);
    try std.testing.expect(klar_string_eq_literal(&s, "a"));

    klar_string_push_byte(&s, 'b');
    klar_string_push_byte(&s, 'c');
    try std.testing.expectEqual(@as(i32, 3), s.len);
    try std.testing.expect(klar_string_eq_literal(&s, "abc"));
}

test "String push_char handles UTF-8" {
    var s = klar_string_new();
    defer klar_string_drop(&s);

    // Push ASCII
    klar_string_push_char(&s, 'h');
    // Push é (U+00E9) - 2-byte UTF-8
    klar_string_push_char(&s, 0xe9);
    // Push ASCII
    klar_string_push_char(&s, 'l');
    klar_string_push_char(&s, 'l');
    klar_string_push_char(&s, 'o');

    try std.testing.expectEqual(@as(i32, 6), s.len); // 1 + 2 + 1 + 1 + 1
}

test "String concat" {
    var a = klar_string_from("hello");
    defer klar_string_drop(&a);
    var b = klar_string_from(" world");
    defer klar_string_drop(&b);

    var c = klar_string_concat(&a, &b);
    defer klar_string_drop(&c);

    try std.testing.expectEqual(@as(i32, 11), c.len);
    try std.testing.expect(klar_string_eq_literal(&c, "hello world"));
}

test "String clone" {
    var s = klar_string_from("test");
    defer klar_string_drop(&s);

    var clone = klar_string_clone_heap(&s);
    defer klar_string_drop(&clone);

    try std.testing.expect(klar_string_eq(&s, &clone));
    try std.testing.expect(s.ptr != clone.ptr); // Different memory
}

test "String equality" {
    var a = klar_string_from("test");
    defer klar_string_drop(&a);
    var b = klar_string_from("test");
    defer klar_string_drop(&b);
    var c = klar_string_from("other");
    defer klar_string_drop(&c);

    try std.testing.expect(klar_string_eq(&a, &b));
    try std.testing.expect(!klar_string_eq(&a, &c));
}

test "String hash" {
    var a = klar_string_from("test");
    defer klar_string_drop(&a);
    var b = klar_string_from("test");
    defer klar_string_drop(&b);
    var c = klar_string_from("other");
    defer klar_string_drop(&c);

    try std.testing.expectEqual(klar_string_hash_heap(&a), klar_string_hash_heap(&b));
    try std.testing.expect(klar_string_hash_heap(&a) != klar_string_hash_heap(&c));
}

test "String clear" {
    var s = klar_string_from("hello");
    defer klar_string_drop(&s);

    const old_capacity = s.capacity;
    klar_string_clear(&s);

    try std.testing.expectEqual(@as(i32, 0), s.len);
    try std.testing.expectEqual(old_capacity, s.capacity);
    try std.testing.expect(klar_string_eq_literal(&s, ""));
}
