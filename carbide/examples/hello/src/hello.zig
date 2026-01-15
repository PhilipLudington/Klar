//! # Hello Library
//!
//! A simple greeting library demonstrating CarbideZig patterns.
//!
//! ## Features
//! - Configurable greeting message
//! - Memory-safe string handling
//! - Proper error handling
//!
//! ## Example
//! ```zig
//! var greeter = try Greeter.init(allocator, .{});
//! defer greeter.deinit();
//!
//! const message = try greeter.greet("World");
//! defer allocator.free(message);
//! ```

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Maximum allowed name length in bytes.
pub const max_name_length = 256;

/// Maximum greeting/suffix length in bytes.
pub const max_text_length = 128;

/// Errors that can occur during greeting operations.
pub const GreetError = error{
    /// Name exceeds maximum allowed length.
    NameTooLong,
    /// Name is empty or contains only whitespace.
    EmptyName,
    /// Memory allocation failed.
    OutOfMemory,
};

/// Configuration for creating a Greeter.
///
/// All fields have sensible defaults, so you can create a default
/// configuration with `.{}`.
pub const Config = struct {
    /// The greeting word (e.g., "Hello", "Hi", "Welcome").
    greeting: []const u8 = "Hello",

    /// The suffix after the name (e.g., "!", ".", " - welcome!").
    suffix: []const u8 = "!",

    /// Whether to capitalize the first letter of the name.
    capitalize_name: bool = true,

    /// Optional prefix to add before the greeting.
    prefix: ?[]const u8 = null,
};

/// A configurable greeter that generates greeting messages.
///
/// ## Thread Safety
/// Not thread-safe. Each thread should have its own Greeter instance.
///
/// ## Example
/// ```zig
/// var greeter = try Greeter.init(allocator, .{ .greeting = "Hi" });
/// defer greeter.deinit();
///
/// const msg = try greeter.greet("Alice");
/// defer allocator.free(msg);
/// std.debug.print("{s}\n", .{msg});
/// ```
pub const Greeter = struct {
    allocator: Allocator,
    greeting: []u8,
    suffix: []u8,
    capitalize_name: bool,
    prefix: ?[]u8,
    greet_count: usize,

    const Self = @This();

    /// Creates a new Greeter with the given configuration.
    ///
    /// The greeter owns copies of all string data from the config.
    /// Call `deinit()` to release resources when done.
    ///
    /// ## Arguments
    /// - `allocator`: Allocator for internal buffers and greet() output
    /// - `config`: Configuration options (all fields have defaults)
    ///
    /// ## Returns
    /// A new Greeter instance, or an error if allocation fails.
    pub fn init(allocator: Allocator, config: Config) GreetError!Self {
        // Validate lengths
        if (config.greeting.len > max_text_length) {
            return GreetError.NameTooLong;
        }
        if (config.suffix.len > max_text_length) {
            return GreetError.NameTooLong;
        }

        // Allocate and copy greeting
        const greeting = allocator.dupe(u8, config.greeting) catch {
            return GreetError.OutOfMemory;
        };
        errdefer allocator.free(greeting);

        // Allocate and copy suffix
        const suffix = allocator.dupe(u8, config.suffix) catch {
            return GreetError.OutOfMemory;
        };
        errdefer allocator.free(suffix);

        // Allocate and copy prefix if present
        const prefix: ?[]u8 = if (config.prefix) |p| blk: {
            break :blk allocator.dupe(u8, p) catch {
                return GreetError.OutOfMemory;
            };
        } else null;
        errdefer if (prefix) |p| allocator.free(p);

        return Self{
            .allocator = allocator,
            .greeting = greeting,
            .suffix = suffix,
            .capitalize_name = config.capitalize_name,
            .prefix = prefix,
            .greet_count = 0,
        };
    }

    /// Releases all resources owned by this Greeter.
    ///
    /// After calling deinit(), the Greeter must not be used.
    pub fn deinit(self: *Self) void {
        if (self.prefix) |p| {
            self.allocator.free(p);
        }
        self.allocator.free(self.greeting);
        self.allocator.free(self.suffix);
        self.* = undefined;
    }

    /// Generates a greeting message for the given name.
    ///
    /// Format: "[prefix] greeting, name suffix"
    /// Example: "Hello, World!"
    ///
    /// ## Arguments
    /// - `name`: The name to greet. Must not be empty or exceed `max_name_length`.
    ///
    /// ## Returns
    /// A newly allocated greeting string. **Caller owns the returned memory**
    /// and must free it with the same allocator used to create this Greeter.
    ///
    /// ## Errors
    /// - `NameTooLong`: Name exceeds 256 bytes
    /// - `EmptyName`: Name is empty or whitespace-only
    /// - `OutOfMemory`: Allocation failed
    pub fn greet(self: *Self, name: []const u8) GreetError![]u8 {
        // Validate name
        const trimmed = std.mem.trim(u8, name, " \t\n\r");
        if (trimmed.len == 0) {
            return GreetError.EmptyName;
        }
        if (trimmed.len > max_name_length) {
            return GreetError.NameTooLong;
        }

        // Prepare name (optionally capitalize)
        var name_buf: [max_name_length]u8 = undefined;
        const processed_name = if (self.capitalize_name)
            self.capitalizeName(trimmed, &name_buf)
        else
            trimmed;

        // Calculate output size: "[prefix ] greeting, name suffix"
        const prefix_len = if (self.prefix) |p| p.len + 1 else 0; // +1 for space
        const comma_space_len: usize = 2; // ", "
        const total_len = prefix_len + self.greeting.len + comma_space_len + processed_name.len + self.suffix.len;

        // Allocate result buffer
        var result = self.allocator.alloc(u8, total_len) catch {
            return GreetError.OutOfMemory;
        };
        errdefer self.allocator.free(result);

        // Build the message
        var offset: usize = 0;

        // Add prefix if present
        if (self.prefix) |p| {
            @memcpy(result[offset..][0..p.len], p);
            offset += p.len;
            result[offset] = ' ';
            offset += 1;
        }

        // Add greeting
        @memcpy(result[offset..][0..self.greeting.len], self.greeting);
        offset += self.greeting.len;

        // Add ", "
        result[offset] = ',';
        result[offset + 1] = ' ';
        offset += 2;

        // Add name
        @memcpy(result[offset..][0..processed_name.len], processed_name);
        offset += processed_name.len;

        // Add suffix
        @memcpy(result[offset..][0..self.suffix.len], self.suffix);

        self.greet_count += 1;
        return result;
    }

    /// Returns the number of times greet() has been called successfully.
    pub fn greetCount(self: Self) usize {
        return self.greet_count;
    }

    /// Returns whether the greeter has generated any greetings.
    pub fn hasGreeted(self: Self) bool {
        return self.greet_count > 0;
    }

    /// Returns the current greeting word.
    ///
    /// The returned slice is valid until this Greeter is deinitialized.
    pub fn getGreeting(self: Self) []const u8 {
        return self.greeting;
    }

    // -- Private methods --

    fn capitalizeName(self: Self, name: []const u8, buf: []u8) []const u8 {
        _ = self;
        if (name.len == 0) return name;

        @memcpy(buf[0..name.len], name);
        buf[0] = std.ascii.toUpper(buf[0]);
        return buf[0..name.len];
    }
};

// -- Unit Tests --

test "Greeter creates default greeting" {
    var greeter = try Greeter.init(std.testing.allocator, .{});
    defer greeter.deinit();

    const message = try greeter.greet("world");
    defer std.testing.allocator.free(message);

    try std.testing.expectEqualStrings("Hello, World!", message);
}

test "Greeter uses custom greeting" {
    var greeter = try Greeter.init(std.testing.allocator, .{
        .greeting = "Hi",
    });
    defer greeter.deinit();

    const message = try greeter.greet("alice");
    defer std.testing.allocator.free(message);

    try std.testing.expectEqualStrings("Hi, Alice!", message);
}

test "Greeter uses custom suffix" {
    var greeter = try Greeter.init(std.testing.allocator, .{
        .suffix = " - welcome!",
    });
    defer greeter.deinit();

    const message = try greeter.greet("alice");
    defer std.testing.allocator.free(message);

    try std.testing.expectEqualStrings("Hello, Alice - welcome!", message);
}

test "Greeter respects capitalize_name option" {
    var greeter = try Greeter.init(std.testing.allocator, .{
        .capitalize_name = false,
    });
    defer greeter.deinit();

    const message = try greeter.greet("alice");
    defer std.testing.allocator.free(message);

    try std.testing.expectEqualStrings("Hello, alice!", message);
}

test "Greeter includes prefix when configured" {
    var greeter = try Greeter.init(std.testing.allocator, .{
        .prefix = "[INFO]",
    });
    defer greeter.deinit();

    const message = try greeter.greet("bob");
    defer std.testing.allocator.free(message);

    try std.testing.expectEqualStrings("[INFO] Hello, Bob!", message);
}

test "Greeter rejects empty name" {
    var greeter = try Greeter.init(std.testing.allocator, .{});
    defer greeter.deinit();

    const result = greeter.greet("");
    try std.testing.expectError(GreetError.EmptyName, result);
}

test "Greeter rejects whitespace-only name" {
    var greeter = try Greeter.init(std.testing.allocator, .{});
    defer greeter.deinit();

    const result = greeter.greet("   \t\n  ");
    try std.testing.expectError(GreetError.EmptyName, result);
}

test "Greeter trims whitespace from name" {
    var greeter = try Greeter.init(std.testing.allocator, .{});
    defer greeter.deinit();

    const message = try greeter.greet("  alice  ");
    defer std.testing.allocator.free(message);

    try std.testing.expectEqualStrings("Hello, Alice!", message);
}

test "Greeter tracks greet count" {
    var greeter = try Greeter.init(std.testing.allocator, .{});
    defer greeter.deinit();

    try std.testing.expect(!greeter.hasGreeted());
    try std.testing.expectEqual(@as(usize, 0), greeter.greetCount());

    const msg1 = try greeter.greet("a");
    defer std.testing.allocator.free(msg1);

    try std.testing.expect(greeter.hasGreeted());
    try std.testing.expectEqual(@as(usize, 1), greeter.greetCount());

    const msg2 = try greeter.greet("b");
    defer std.testing.allocator.free(msg2);

    try std.testing.expectEqual(@as(usize, 2), greeter.greetCount());
}

test "Greeter has no memory leaks" {
    // std.testing.allocator automatically checks for leaks
    var greeter = try Greeter.init(std.testing.allocator, .{
        .prefix = "Test",
    });
    defer greeter.deinit();

    for (0..10) |_| {
        const msg = try greeter.greet("test");
        std.testing.allocator.free(msg);
    }
}
