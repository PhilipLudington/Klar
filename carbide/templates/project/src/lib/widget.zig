//! Example widget demonstrating CarbideZig patterns.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Configuration for creating a Widget.
pub const Config = struct {
    /// Initial capacity for the internal buffer.
    initial_capacity: usize = 16,

    /// Maximum allowed capacity.
    max_capacity: usize = 1024,
};

/// A simple widget that demonstrates CarbideZig patterns.
///
/// ## Thread Safety
/// Not thread-safe. Use external synchronization for concurrent access.
///
/// ## Example
/// ```zig
/// var widget = try Widget.init(allocator, .{});
/// defer widget.deinit();
///
/// try widget.add(42);
/// const items = widget.items();
/// ```
pub const Widget = struct {
    allocator: Allocator,
    buffer: []u8,
    len: usize,
    config: Config,

    const Self = @This();

    /// Creates a new Widget.
    ///
    /// Caller must call `deinit()` to release resources.
    pub fn init(allocator: Allocator, config: Config) !Self {
        const buffer = try allocator.alloc(u8, config.initial_capacity);
        errdefer allocator.free(buffer);

        return Self{
            .allocator = allocator,
            .buffer = buffer,
            .len = 0,
            .config = config,
        };
    }

    /// Releases all resources.
    pub fn deinit(self: *Self) void {
        self.allocator.free(self.buffer);
        self.* = undefined;
    }

    /// Adds a value to the widget.
    pub fn add(self: *Self, value: u8) !void {
        if (self.len >= self.buffer.len) {
            try self.grow();
        }
        self.buffer[self.len] = value;
        self.len += 1;
    }

    /// Returns the current items.
    ///
    /// **Valid until**: next call to `add()` or `deinit()`.
    pub fn items(self: Self) []const u8 {
        return self.buffer[0..self.len];
    }

    /// Returns the number of items.
    pub fn count(self: Self) usize {
        return self.len;
    }

    /// Returns whether the widget is empty.
    pub fn isEmpty(self: Self) bool {
        return self.len == 0;
    }

    fn grow(self: *Self) !void {
        const new_capacity = @min(self.buffer.len * 2, self.config.max_capacity);
        if (new_capacity <= self.buffer.len) {
            return error.CapacityExceeded;
        }

        const new_buffer = try self.allocator.realloc(self.buffer, new_capacity);
        self.buffer = new_buffer;
    }
};

// Tests
test "Widget basic usage" {
    var widget = try Widget.init(std.testing.allocator, .{});
    defer widget.deinit();

    try std.testing.expect(widget.isEmpty());

    try widget.add(1);
    try widget.add(2);
    try widget.add(3);

    try std.testing.expectEqual(@as(usize, 3), widget.count());
    try std.testing.expectEqualSlices(u8, &.{ 1, 2, 3 }, widget.items());
}

test "Widget grows capacity" {
    var widget = try Widget.init(std.testing.allocator, .{
        .initial_capacity = 2,
        .max_capacity = 100,
    });
    defer widget.deinit();

    // Add more than initial capacity
    for (0..10) |i| {
        try widget.add(@intCast(i));
    }

    try std.testing.expectEqual(@as(usize, 10), widget.count());
}

test "Widget respects max capacity" {
    var widget = try Widget.init(std.testing.allocator, .{
        .initial_capacity = 2,
        .max_capacity = 4,
    });
    defer widget.deinit();

    try widget.add(1);
    try widget.add(2);
    try widget.add(3);
    try widget.add(4);

    // Should fail when exceeding max capacity
    try std.testing.expectError(error.CapacityExceeded, widget.add(5));
}
