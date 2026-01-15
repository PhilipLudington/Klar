//! CLI Todo Application Example
//!
//! Demonstrates:
//! - Command-line argument parsing
//! - File I/O for persistence
//! - Structured output formatting
//! - Error handling with user-friendly messages
//! - Configuration via environment variables

const std = @import("std");
const fs = std.fs;
const json = std.json;
const Allocator = std.mem.Allocator;

// ============================================================================
// Data Types
// ============================================================================

pub const TodoItem = struct {
    id: u32,
    title: []const u8,
    completed: bool = false,
    created_at: i64,
};

pub const TodoList = struct {
    allocator: Allocator,
    items: std.ArrayList(TodoItem),
    next_id: u32 = 1,

    pub fn init(allocator: Allocator) TodoList {
        return .{
            .allocator = allocator,
            .items = std.ArrayList(TodoItem).init(allocator),
        };
    }

    pub fn deinit(self: *TodoList) void {
        for (self.items.items) |item| {
            self.allocator.free(item.title);
        }
        self.items.deinit();
    }

    pub fn add(self: *TodoList, title: []const u8) !u32 {
        const id = self.next_id;
        self.next_id += 1;

        const title_copy = try self.allocator.dupe(u8, title);
        errdefer self.allocator.free(title_copy);

        try self.items.append(.{
            .id = id,
            .title = title_copy,
            .completed = false,
            .created_at = std.time.timestamp(),
        });

        return id;
    }

    pub fn complete(self: *TodoList, id: u32) !void {
        for (self.items.items) |*item| {
            if (item.id == id) {
                item.completed = true;
                return;
            }
        }
        return error.NotFound;
    }

    pub fn remove(self: *TodoList, id: u32) !void {
        for (self.items.items, 0..) |item, i| {
            if (item.id == id) {
                self.allocator.free(item.title);
                _ = self.items.orderedRemove(i);
                return;
            }
        }
        return error.NotFound;
    }

    pub fn findById(self: *const TodoList, id: u32) ?*const TodoItem {
        for (self.items.items) |*item| {
            if (item.id == id) return item;
        }
        return null;
    }
};

// ============================================================================
// Persistence
// ============================================================================

const StorageFormat = struct {
    items: []const StoredItem,
    next_id: u32,

    const StoredItem = struct {
        id: u32,
        title: []const u8,
        completed: bool,
        created_at: i64,
    };
};

pub fn saveTodoList(allocator: Allocator, list: *const TodoList, path: []const u8) !void {
    // Convert to storable format
    var stored_items = try allocator.alloc(StorageFormat.StoredItem, list.items.items.len);
    defer allocator.free(stored_items);

    for (list.items.items, 0..) |item, i| {
        stored_items[i] = .{
            .id = item.id,
            .title = item.title,
            .completed = item.completed,
            .created_at = item.created_at,
        };
    }

    const storage = StorageFormat{
        .items = stored_items,
        .next_id = list.next_id,
    };

    // Serialize to JSON
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    try json.stringify(storage, .{ .whitespace = .indent_2 }, buffer.writer());

    // Write to file
    const file = try fs.cwd().createFile(path, .{});
    defer file.close();
    try file.writeAll(buffer.items);
}

pub fn loadTodoList(allocator: Allocator, path: []const u8) !TodoList {
    // Read file
    const content = fs.cwd().readFileAlloc(allocator, path, 1024 * 1024) catch |err| switch (err) {
        error.FileNotFound => return TodoList.init(allocator),
        else => return err,
    };
    defer allocator.free(content);

    // Parse JSON
    const parsed = try json.parseFromSlice(StorageFormat, allocator, content, .{
        .allocate = .alloc_always,
    });
    defer parsed.deinit();

    // Build TodoList
    var list = TodoList.init(allocator);
    errdefer list.deinit();

    list.next_id = parsed.value.next_id;

    for (parsed.value.items) |item| {
        const title_copy = try allocator.dupe(u8, item.title);
        errdefer allocator.free(title_copy);

        try list.items.append(.{
            .id = item.id,
            .title = title_copy,
            .completed = item.completed,
            .created_at = item.created_at,
        });
    }

    return list;
}

// ============================================================================
// CLI Interface
// ============================================================================

const Command = enum {
    add,
    list,
    complete,
    remove,
    help,
};

const CliError = error{
    MissingArgument,
    InvalidCommand,
    InvalidId,
};

fn parseCommand(arg: []const u8) CliError!Command {
    return std.meta.stringToEnum(Command, arg) orelse error.InvalidCommand;
}

fn parseId(arg: []const u8) CliError!u32 {
    return std.fmt.parseInt(u32, arg, 10) catch error.InvalidId;
}

fn printUsage(writer: anytype) !void {
    try writer.writeAll(
        \\Usage: todo <command> [arguments]
        \\
        \\Commands:
        \\  add <title>     Add a new todo item
        \\  list            List all todo items
        \\  complete <id>   Mark a todo item as completed
        \\  remove <id>     Remove a todo item
        \\  help            Show this help message
        \\
        \\Examples:
        \\  todo add "Buy groceries"
        \\  todo list
        \\  todo complete 1
        \\  todo remove 1
        \\
    );
}

fn printTodoItem(writer: anytype, item: *const TodoItem) !void {
    const status = if (item.completed) "[x]" else "[ ]";
    try writer.print("{s} {d}: {s}\n", .{ status, item.id, item.title });
}

fn getDataPath(allocator: Allocator) ![]const u8 {
    // Check environment variable first
    if (std.process.getEnvVarOwned(allocator, "TODO_FILE")) |path| {
        return path;
    } else |_| {}

    // Default to home directory
    if (std.process.getEnvVarOwned(allocator, "HOME")) |home| {
        defer allocator.free(home);
        return std.fs.path.join(allocator, &.{ home, ".todo.json" });
    } else |_| {}

    // Fallback to current directory
    return allocator.dupe(u8, ".todo.json");
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const status = gpa.deinit();
        if (status == .leak) {
            std.log.err("Memory leak detected!", .{});
        }
    }
    const allocator = gpa.allocator();

    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    // Get data file path
    const data_path = try getDataPath(allocator);
    defer allocator.free(data_path);

    // Parse arguments
    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    _ = args.skip(); // Skip program name

    const command_str = args.next() orelse {
        try printUsage(stdout);
        return;
    };

    const command = parseCommand(command_str) catch {
        try stderr.print("Unknown command: {s}\n", .{command_str});
        try printUsage(stderr);
        std.process.exit(1);
    };

    // Load existing data
    var list = try loadTodoList(allocator, data_path);
    defer list.deinit();

    // Execute command
    switch (command) {
        .help => {
            try printUsage(stdout);
        },
        .add => {
            const title = args.next() orelse {
                try stderr.writeAll("Error: Missing title argument\n");
                std.process.exit(1);
            };

            const id = try list.add(title);
            try saveTodoList(allocator, &list, data_path);
            try stdout.print("Added todo #{d}: {s}\n", .{ id, title });
        },
        .list => {
            if (list.items.items.len == 0) {
                try stdout.writeAll("No todo items. Use 'todo add <title>' to create one.\n");
            } else {
                try stdout.print("Todo items ({d}):\n", .{list.items.items.len});
                for (list.items.items) |*item| {
                    try printTodoItem(stdout, item);
                }
            }
        },
        .complete => {
            const id_str = args.next() orelse {
                try stderr.writeAll("Error: Missing id argument\n");
                std.process.exit(1);
            };

            const id = parseId(id_str) catch {
                try stderr.print("Error: Invalid id: {s}\n", .{id_str});
                std.process.exit(1);
            };

            list.complete(id) catch |err| switch (err) {
                error.NotFound => {
                    try stderr.print("Error: Todo #{d} not found\n", .{id});
                    std.process.exit(1);
                },
                else => return err,
            };

            try saveTodoList(allocator, &list, data_path);
            try stdout.print("Completed todo #{d}\n", .{id});
        },
        .remove => {
            const id_str = args.next() orelse {
                try stderr.writeAll("Error: Missing id argument\n");
                std.process.exit(1);
            };

            const id = parseId(id_str) catch {
                try stderr.print("Error: Invalid id: {s}\n", .{id_str});
                std.process.exit(1);
            };

            list.remove(id) catch |err| switch (err) {
                error.NotFound => {
                    try stderr.print("Error: Todo #{d} not found\n", .{id});
                    std.process.exit(1);
                },
                else => return err,
            };

            try saveTodoList(allocator, &list, data_path);
            try stdout.print("Removed todo #{d}\n", .{id});
        },
    }
}

// ============================================================================
// Tests
// ============================================================================

test "TodoList add and find" {
    var list = TodoList.init(std.testing.allocator);
    defer list.deinit();

    const id = try list.add("Test item");
    try std.testing.expectEqual(@as(u32, 1), id);

    const item = list.findById(id);
    try std.testing.expect(item != null);
    try std.testing.expectEqualStrings("Test item", item.?.title);
    try std.testing.expectEqual(false, item.?.completed);
}

test "TodoList complete" {
    var list = TodoList.init(std.testing.allocator);
    defer list.deinit();

    const id = try list.add("Test item");
    try list.complete(id);

    const item = list.findById(id);
    try std.testing.expect(item != null);
    try std.testing.expectEqual(true, item.?.completed);
}

test "TodoList complete non-existent returns error" {
    var list = TodoList.init(std.testing.allocator);
    defer list.deinit();

    try std.testing.expectError(error.NotFound, list.complete(999));
}

test "TodoList remove" {
    var list = TodoList.init(std.testing.allocator);
    defer list.deinit();

    const id = try list.add("Test item");
    try std.testing.expectEqual(@as(usize, 1), list.items.items.len);

    try list.remove(id);
    try std.testing.expectEqual(@as(usize, 0), list.items.items.len);
}

test "parseCommand valid" {
    try std.testing.expectEqual(Command.add, try parseCommand("add"));
    try std.testing.expectEqual(Command.list, try parseCommand("list"));
    try std.testing.expectEqual(Command.complete, try parseCommand("complete"));
    try std.testing.expectEqual(Command.remove, try parseCommand("remove"));
    try std.testing.expectEqual(Command.help, try parseCommand("help"));
}

test "parseCommand invalid" {
    try std.testing.expectError(error.InvalidCommand, parseCommand("invalid"));
}

test "parseId valid" {
    try std.testing.expectEqual(@as(u32, 42), try parseId("42"));
    try std.testing.expectEqual(@as(u32, 0), try parseId("0"));
}

test "parseId invalid" {
    try std.testing.expectError(error.InvalidId, parseId("abc"));
    try std.testing.expectError(error.InvalidId, parseId("-1"));
}
