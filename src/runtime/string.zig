// ============================================================================
// Klar String Runtime Library
// ============================================================================
//
// This module provides runtime support for string methods that require
// memory allocation (trim, to_uppercase, to_lowercase).

const std = @import("std");
const c = @cImport({
    @cInclude("stdlib.h");
    @cInclude("string.h");
    @cInclude("ctype.h");
});

/// Trim leading and trailing whitespace from a string.
/// Returns a newly allocated string that must be freed by the caller.
export fn klar_string_trim(s: [*:0]const u8) [*:0]u8 {
    const len = c.strlen(s);
    if (len == 0) {
        // Return a copy of the empty string
        const result = @as([*]u8, @ptrCast(c.malloc(1) orelse return @ptrCast(@constCast(""))));
        result[0] = 0;
        return @ptrCast(result);
    }

    // Find the start (skip leading whitespace)
    var start: usize = 0;
    while (start < len and isWhitespace(s[start])) {
        start += 1;
    }

    // Find the end (skip trailing whitespace)
    var end: usize = len;
    while (end > start and isWhitespace(s[end - 1])) {
        end -= 1;
    }

    const trimmed_len = end - start;
    const result = @as([*]u8, @ptrCast(c.malloc(trimmed_len + 1) orelse return @ptrCast(@constCast(""))));

    // Copy the trimmed portion
    var i: usize = 0;
    while (i < trimmed_len) : (i += 1) {
        result[i] = s[start + i];
    }
    result[trimmed_len] = 0;

    return @ptrCast(result);
}

/// Convert a string to uppercase.
/// Returns a newly allocated string that must be freed by the caller.
export fn klar_string_to_uppercase(s: [*:0]const u8) [*:0]u8 {
    const len = c.strlen(s);
    const result = @as([*]u8, @ptrCast(c.malloc(len + 1) orelse return @ptrCast(@constCast(""))));

    var i: usize = 0;
    while (i < len) : (i += 1) {
        result[i] = toUpper(s[i]);
    }
    result[len] = 0;

    return @ptrCast(result);
}

/// Convert a string to lowercase.
/// Returns a newly allocated string that must be freed by the caller.
export fn klar_string_to_lowercase(s: [*:0]const u8) [*:0]u8 {
    const len = c.strlen(s);
    const result = @as([*]u8, @ptrCast(c.malloc(len + 1) orelse return @ptrCast(@constCast(""))));

    var i: usize = 0;
    while (i < len) : (i += 1) {
        result[i] = toLower(s[i]);
    }
    result[len] = 0;

    return @ptrCast(result);
}

/// Slice struct returned by chars() and similar functions.
const Slice = extern struct {
    ptr: ?*anyopaque,
    len: i64,
};

/// Decode a UTF-8 string into an array of unicode codepoints (chars).
/// Returns a slice containing the allocated array and its length.
/// The caller is responsible for freeing the returned memory.
export fn klar_string_chars(s: [*:0]const u8) Slice {
    const str_len = c.strlen(s);
    if (str_len == 0) {
        return .{ .ptr = null, .len = 0 };
    }

    // First pass: count the number of codepoints
    var count: usize = 0;
    var i: usize = 0;
    while (i < str_len) {
        const byte = s[i];
        const cp_len = utf8ByteLength(byte);
        if (i + cp_len > str_len) {
            // Invalid sequence at end, count as 1 replacement char
            count += 1;
            break;
        }
        count += 1;
        i += cp_len;
    }

    // Allocate array of i32 (unicode codepoints)
    const result = @as([*]i32, @ptrCast(@alignCast(c.malloc(count * @sizeOf(i32)) orelse return .{ .ptr = null, .len = 0 })));

    // Second pass: decode codepoints
    var out_idx: usize = 0;
    i = 0;
    while (i < str_len and out_idx < count) {
        const byte = s[i];
        const cp_len = utf8ByteLength(byte);
        if (i + cp_len > str_len) {
            // Invalid sequence at end, use replacement char
            result[out_idx] = 0xFFFD;
            out_idx += 1;
            break;
        }

        // Decode the UTF-8 sequence
        const codepoint = decodeUtf8(s[i .. i + cp_len]);
        result[out_idx] = codepoint;
        out_idx += 1;
        i += cp_len;
    }

    return .{ .ptr = result, .len = @intCast(out_idx) };
}

/// Get the length of a UTF-8 sequence from its first byte.
fn utf8ByteLength(first_byte: u8) usize {
    if (first_byte & 0x80 == 0) return 1; // ASCII
    if (first_byte & 0xE0 == 0xC0) return 2; // 2-byte sequence
    if (first_byte & 0xF0 == 0xE0) return 3; // 3-byte sequence
    if (first_byte & 0xF8 == 0xF0) return 4; // 4-byte sequence
    return 1; // Invalid, treat as 1 byte
}

/// Decode a UTF-8 sequence into a unicode codepoint.
fn decodeUtf8(bytes: []const u8) i32 {
    if (bytes.len == 0) return 0xFFFD; // Replacement char

    const first = bytes[0];
    if (first & 0x80 == 0) {
        // ASCII
        return first;
    }
    if (bytes.len >= 2 and first & 0xE0 == 0xC0) {
        // 2-byte sequence
        const b1 = bytes[1];
        if (b1 & 0xC0 != 0x80) return 0xFFFD;
        return (@as(i32, first & 0x1F) << 6) | @as(i32, b1 & 0x3F);
    }
    if (bytes.len >= 3 and first & 0xF0 == 0xE0) {
        // 3-byte sequence
        const b1 = bytes[1];
        const b2 = bytes[2];
        if (b1 & 0xC0 != 0x80 or b2 & 0xC0 != 0x80) return 0xFFFD;
        return (@as(i32, first & 0x0F) << 12) | (@as(i32, b1 & 0x3F) << 6) | @as(i32, b2 & 0x3F);
    }
    if (bytes.len >= 4 and first & 0xF8 == 0xF0) {
        // 4-byte sequence
        const b1 = bytes[1];
        const b2 = bytes[2];
        const b3 = bytes[3];
        if (b1 & 0xC0 != 0x80 or b2 & 0xC0 != 0x80 or b3 & 0xC0 != 0x80) return 0xFFFD;
        return (@as(i32, first & 0x07) << 18) | (@as(i32, b1 & 0x3F) << 12) | (@as(i32, b2 & 0x3F) << 6) | @as(i32, b3 & 0x3F);
    }
    return 0xFFFD; // Invalid, use replacement char
}

fn isWhitespace(ch: u8) bool {
    return ch == ' ' or ch == '\t' or ch == '\n' or ch == '\r';
}

fn toUpper(ch: u8) u8 {
    if (ch >= 'a' and ch <= 'z') {
        return ch - ('a' - 'A');
    }
    return ch;
}

fn toLower(ch: u8) u8 {
    if (ch >= 'A' and ch <= 'Z') {
        return ch + ('a' - 'A');
    }
    return ch;
}

test "trim empty string" {
    const result = klar_string_trim("");
    try std.testing.expectEqualStrings("", std.mem.span(result));
    c.free(result);
}

test "trim no whitespace" {
    const result = klar_string_trim("hello");
    try std.testing.expectEqualStrings("hello", std.mem.span(result));
    c.free(result);
}

test "trim leading whitespace" {
    const result = klar_string_trim("  hello");
    try std.testing.expectEqualStrings("hello", std.mem.span(result));
    c.free(result);
}

test "trim trailing whitespace" {
    const result = klar_string_trim("hello  ");
    try std.testing.expectEqualStrings("hello", std.mem.span(result));
    c.free(result);
}

test "trim both sides" {
    const result = klar_string_trim("  hello  ");
    try std.testing.expectEqualStrings("hello", std.mem.span(result));
    c.free(result);
}

test "to_uppercase" {
    const result = klar_string_to_uppercase("Hello World");
    try std.testing.expectEqualStrings("HELLO WORLD", std.mem.span(result));
    c.free(result);
}

test "to_lowercase" {
    const result = klar_string_to_lowercase("Hello World");
    try std.testing.expectEqualStrings("hello world", std.mem.span(result));
    c.free(result);
}

test "chars empty string" {
    const result = klar_string_chars("");
    try std.testing.expectEqual(@as(i64, 0), result.len);
    try std.testing.expectEqual(@as(?*anyopaque, null), result.ptr);
}

test "chars ASCII string" {
    const result = klar_string_chars("abc");
    try std.testing.expectEqual(@as(i64, 3), result.len);
    const chars: [*]i32 = @ptrCast(@alignCast(result.ptr));
    try std.testing.expectEqual(@as(i32, 'a'), chars[0]);
    try std.testing.expectEqual(@as(i32, 'b'), chars[1]);
    try std.testing.expectEqual(@as(i32, 'c'), chars[2]);
    c.free(result.ptr);
}

test "chars UTF-8 string" {
    // "héllo" - é is a 2-byte UTF-8 sequence (C3 A9)
    const result = klar_string_chars("h\xc3\xa9llo");
    try std.testing.expectEqual(@as(i64, 5), result.len);
    const chars: [*]i32 = @ptrCast(@alignCast(result.ptr));
    try std.testing.expectEqual(@as(i32, 'h'), chars[0]);
    try std.testing.expectEqual(@as(i32, 0xe9), chars[1]); // é = U+00E9
    try std.testing.expectEqual(@as(i32, 'l'), chars[2]);
    try std.testing.expectEqual(@as(i32, 'l'), chars[3]);
    try std.testing.expectEqual(@as(i32, 'o'), chars[4]);
    c.free(result.ptr);
}
