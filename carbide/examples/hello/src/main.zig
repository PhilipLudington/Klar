//! Example application demonstrating the Hello library.
//!
//! Run with: `zig build run`
//! Run with args: `zig build run -- Alice Bob Charlie`

const std = @import("std");
const hello = @import("hello");
const Greeter = hello.Greeter;

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

    // Get command line arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // Skip program name, use remaining args as names to greet
    const names = if (args.len > 1) args[1..] else &[_][:0]const u8{"World"};

    // Create greeter with custom configuration
    var greeter = Greeter.init(allocator, .{
        .greeting = "Hello",
        .suffix = "! Welcome to CarbideZig.",
        .capitalize_name = true,
    }) catch |err| {
        std.debug.print("Failed to initialize greeter: {}\n", .{err});
        return;
    };
    defer greeter.deinit();

    // Greet each name
    for (names) |name| {
        const message = greeter.greet(name) catch |err| {
            switch (err) {
                hello.GreetError.EmptyName => {
                    std.debug.print("Skipping empty name\n", .{});
                    continue;
                },
                hello.GreetError.NameTooLong => {
                    std.debug.print("Name too long, skipping\n", .{});
                    continue;
                },
                hello.GreetError.OutOfMemory => {
                    std.debug.print("Out of memory!\n", .{});
                    return;
                },
            }
        };
        defer allocator.free(message);

        std.debug.print("{s}\n", .{message});
    }

    // Print statistics
    std.debug.print("\nGenerated {d} greeting(s).\n", .{greeter.greetCount()});
}
