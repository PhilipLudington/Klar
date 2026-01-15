//! JSON Parsing Example
//!
//! Demonstrates:
//! - Using std.json for parsing
//! - Typed parsing with structs
//! - Dynamic JSON handling
//! - JSON serialization
//! - Error handling for malformed input

const std = @import("std");
const json = std.json;
const Allocator = std.mem.Allocator;

// ============================================================================
// Typed JSON Parsing
// ============================================================================

/// User configuration loaded from JSON.
/// Fields with defaults are optional in the JSON.
pub const UserConfig = struct {
    name: []const u8,
    email: []const u8,
    age: ?u32 = null,
    settings: Settings = .{},

    pub const Settings = struct {
        theme: []const u8 = "light",
        notifications: bool = true,
        max_items: u32 = 100,
    };
};

/// Parse JSON string into a typed struct.
/// Caller owns returned value and must call deinit on the ParseResult.
pub fn parseConfig(allocator: Allocator, json_str: []const u8) !json.Parsed(UserConfig) {
    return json.parseFromSlice(
        UserConfig,
        allocator,
        json_str,
        .{
            .allocate = .alloc_always,
        },
    );
}

// ============================================================================
// Dynamic JSON Handling
// ============================================================================

/// Process JSON without knowing structure at compile time.
pub fn processDynamicJson(allocator: Allocator, json_str: []const u8) !void {
    const parsed = try json.parseFromSlice(
        json.Value,
        allocator,
        json_str,
        .{},
    );
    defer parsed.deinit();

    const root = parsed.value;

    // Navigate JSON dynamically
    switch (root) {
        .object => |obj| {
            std.debug.print("JSON object with {d} keys:\n", .{obj.count()});

            var iter = obj.iterator();
            while (iter.next()) |entry| {
                const key = entry.key_ptr.*;
                const value = entry.value_ptr.*;
                std.debug.print("  {s}: {s}\n", .{ key, @tagName(value) });
            }
        },
        .array => |arr| {
            std.debug.print("JSON array with {d} elements\n", .{arr.items.len});
        },
        else => {
            std.debug.print("JSON primitive: {s}\n", .{@tagName(root)});
        },
    }
}

/// Extract a value from a JSON path like "user.settings.theme".
pub fn getJsonPath(value: json.Value, path: []const u8) ?json.Value {
    var current = value;
    var iter = std.mem.splitScalar(u8, path, '.');

    while (iter.next()) |key| {
        switch (current) {
            .object => |obj| {
                current = obj.get(key) orelse return null;
            },
            else => return null,
        }
    }

    return current;
}

// ============================================================================
// JSON Serialization
// ============================================================================

/// Serialize a struct to JSON string.
/// Caller owns returned string and must free it.
pub fn toJson(allocator: Allocator, value: anytype) ![]u8 {
    var buffer = std.ArrayList(u8).init(allocator);
    errdefer buffer.deinit();

    try json.stringify(value, .{}, buffer.writer());

    return buffer.toOwnedSlice();
}

/// Serialize with pretty formatting.
pub fn toJsonPretty(allocator: Allocator, value: anytype) ![]u8 {
    var buffer = std.ArrayList(u8).init(allocator);
    errdefer buffer.deinit();

    try json.stringify(value, .{
        .whitespace = .indent_2,
    }, buffer.writer());

    return buffer.toOwnedSlice();
}

// ============================================================================
// Validation
// ============================================================================

pub const ValidationError = error{
    MissingRequiredField,
    InvalidFieldType,
    ValueOutOfRange,
    MalformedJson,
};

/// Validate JSON structure before parsing.
pub fn validateUserConfig(allocator: Allocator, json_str: []const u8) ValidationError!void {
    const parsed = json.parseFromSlice(
        json.Value,
        allocator,
        json_str,
        .{},
    ) catch return error.MalformedJson;
    defer parsed.deinit();

    const root = parsed.value;

    // Must be an object
    const obj = switch (root) {
        .object => |o| o,
        else => return error.InvalidFieldType,
    };

    // Required fields
    if (obj.get("name") == null) return error.MissingRequiredField;
    if (obj.get("email") == null) return error.MissingRequiredField;

    // Type checks
    if (obj.get("name")) |name| {
        if (name != .string) return error.InvalidFieldType;
    }

    if (obj.get("age")) |age| {
        switch (age) {
            .integer => |n| {
                if (n < 0 or n > 150) return error.ValueOutOfRange;
            },
            else => return error.InvalidFieldType,
        }
    }
}

// ============================================================================
// Main Demo
// ============================================================================

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const status = gpa.deinit();
        if (status == .leak) {
            std.log.err("Memory leak detected!", .{});
        }
    }
    const allocator = gpa.allocator();

    // Example 1: Typed parsing
    const config_json =
        \\{
        \\  "name": "Alice",
        \\  "email": "alice@example.com",
        \\  "age": 30,
        \\  "settings": {
        \\    "theme": "dark",
        \\    "notifications": false
        \\  }
        \\}
    ;

    std.debug.print("=== Typed Parsing ===\n", .{});
    const config = try parseConfig(allocator, config_json);
    defer config.deinit();

    std.debug.print("Name: {s}\n", .{config.value.name});
    std.debug.print("Email: {s}\n", .{config.value.email});
    if (config.value.age) |age| {
        std.debug.print("Age: {d}\n", .{age});
    }
    std.debug.print("Theme: {s}\n", .{config.value.settings.theme});

    // Example 2: Serialization
    std.debug.print("\n=== Serialization ===\n", .{});
    const new_config = UserConfig{
        .name = "Bob",
        .email = "bob@example.com",
        .age = 25,
        .settings = .{
            .theme = "light",
            .notifications = true,
            .max_items = 50,
        },
    };

    const json_output = try toJsonPretty(allocator, new_config);
    defer allocator.free(json_output);
    std.debug.print("{s}\n", .{json_output});

    // Example 3: Dynamic JSON
    std.debug.print("\n=== Dynamic JSON ===\n", .{});
    try processDynamicJson(allocator, config_json);

    // Example 4: Path extraction
    std.debug.print("\n=== Path Extraction ===\n", .{});
    const parsed = try json.parseFromSlice(json.Value, allocator, config_json, .{});
    defer parsed.deinit();

    if (getJsonPath(parsed.value, "settings.theme")) |theme| {
        if (theme == .string) {
            std.debug.print("Theme via path: {s}\n", .{theme.string});
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

test "parse valid config" {
    const json_str =
        \\{"name": "Test", "email": "test@test.com"}
    ;

    const result = try parseConfig(std.testing.allocator, json_str);
    defer result.deinit();

    try std.testing.expectEqualStrings("Test", result.value.name);
    try std.testing.expectEqualStrings("test@test.com", result.value.email);
    try std.testing.expectEqual(@as(?u32, null), result.value.age);
}

test "parse config with all fields" {
    const json_str =
        \\{
        \\  "name": "Alice",
        \\  "email": "alice@example.com",
        \\  "age": 30,
        \\  "settings": {"theme": "dark", "notifications": false, "max_items": 200}
        \\}
    ;

    const result = try parseConfig(std.testing.allocator, json_str);
    defer result.deinit();

    try std.testing.expectEqual(@as(?u32, 30), result.value.age);
    try std.testing.expectEqualStrings("dark", result.value.settings.theme);
    try std.testing.expectEqual(false, result.value.settings.notifications);
}

test "serialize and parse roundtrip" {
    const original = UserConfig{
        .name = "Test",
        .email = "test@test.com",
        .age = 42,
    };

    const json_str = try toJson(std.testing.allocator, original);
    defer std.testing.allocator.free(json_str);

    const parsed = try parseConfig(std.testing.allocator, json_str);
    defer parsed.deinit();

    try std.testing.expectEqualStrings(original.name, parsed.value.name);
    try std.testing.expectEqualStrings(original.email, parsed.value.email);
    try std.testing.expectEqual(original.age, parsed.value.age);
}

test "validate missing required field" {
    const json_str =
        \\{"name": "Test"}
    ;

    const result = validateUserConfig(std.testing.allocator, json_str);
    try std.testing.expectError(error.MissingRequiredField, result);
}

test "validate invalid field type" {
    const json_str =
        \\{"name": 123, "email": "test@test.com"}
    ;

    const result = validateUserConfig(std.testing.allocator, json_str);
    try std.testing.expectError(error.InvalidFieldType, result);
}

test "getJsonPath extracts nested values" {
    const json_str =
        \\{"a": {"b": {"c": 42}}}
    ;

    const parsed = try json.parseFromSlice(json.Value, std.testing.allocator, json_str, .{});
    defer parsed.deinit();

    const value = getJsonPath(parsed.value, "a.b.c");
    try std.testing.expect(value != null);
    try std.testing.expectEqual(@as(i64, 42), value.?.integer);
}

test "getJsonPath returns null for missing path" {
    const json_str =
        \\{"a": {"b": 1}}
    ;

    const parsed = try json.parseFromSlice(json.Value, std.testing.allocator, json_str, .{});
    defer parsed.deinit();

    const value = getJsonPath(parsed.value, "a.b.c");
    try std.testing.expectEqual(@as(?json.Value, null), value);
}
