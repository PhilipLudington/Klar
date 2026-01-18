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
