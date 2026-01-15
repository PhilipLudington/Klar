//! Application entry point.
//!
//! This is the main executable for the project.

const std = @import("std");
const mylib = @import("mylib");
const config = @import("config");

pub fn main() !void {
    // Use GeneralPurposeAllocator for leak detection in debug builds
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const status = gpa.deinit();
        if (status == .leak) {
            std.debug.print("Memory leak detected!\n", .{});
        }
    }
    const allocator = gpa.allocator();

    // Parse command-line arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (config.enable_logging) {
        std.debug.print("Starting with {d} arguments\n", .{args.len});
    }

    // Run the application
    try run(allocator, args);
}

fn run(allocator: std.mem.Allocator, args: []const [:0]const u8) !void {
    _ = allocator;

    if (args.len > 1) {
        for (args[1..]) |arg| {
            std.debug.print("Argument: {s}\n", .{arg});
        }
    } else {
        std.debug.print("Hello from CarbideZig!\n", .{});
    }
}

test "basic functionality" {
    try std.testing.expect(true);
}
