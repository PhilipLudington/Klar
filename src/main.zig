const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;

// Zig 0.15 IO helpers
fn getStdOut() std.fs.File {
    return .{ .handle = std.posix.STDOUT_FILENO };
}

fn getStdErr() std.fs.File {
    return .{ .handle = std.posix.STDERR_FILENO };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        try printUsage();
        return;
    }

    const command = args[1];

    if (std.mem.eql(u8, command, "run")) {
        if (args.len < 3) {
            try getStdErr().writeAll("Error: no input file\n");
            return;
        }
        try runFile(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "tokenize")) {
        if (args.len < 3) {
            try getStdErr().writeAll("Error: no input file\n");
            return;
        }
        try tokenizeFile(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "build")) {
        try getStdErr().writeAll("Build not yet implemented\n");
    } else if (std.mem.eql(u8, command, "check")) {
        try getStdErr().writeAll("Check not yet implemented\n");
    } else if (std.mem.eql(u8, command, "test")) {
        try getStdErr().writeAll("Test not yet implemented\n");
    } else if (std.mem.eql(u8, command, "fmt")) {
        try getStdErr().writeAll("Format not yet implemented\n");
    } else if (std.mem.eql(u8, command, "help") or std.mem.eql(u8, command, "--help") or std.mem.eql(u8, command, "-h")) {
        try printUsage();
    } else if (std.mem.eql(u8, command, "version") or std.mem.eql(u8, command, "--version") or std.mem.eql(u8, command, "-v")) {
        try printVersion();
    } else {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Unknown command: {s}\n\n", .{command}) catch "Unknown command\n\n";
        try getStdErr().writeAll(msg);
        try printUsage();
    }
}

fn runFile(allocator: std.mem.Allocator, path: []const u8) !void {
    const source = readSourceFile(allocator, path) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Error opening file '{s}': {}\n", .{ path, err }) catch "Error opening file\n";
        try getStdErr().writeAll(msg);
        return;
    };
    defer allocator.free(source);

    const stdout = getStdOut();
    try stdout.writeAll("=== Lexer Output ===\n");

    var lexer = Lexer.init(source);
    var buf: [512]u8 = undefined;

    while (true) {
        const token = lexer.next();
        const text = source[token.loc.start..token.loc.end];
        const msg = std.fmt.bufPrint(&buf, "{d}:{d} {s}: \"{s}\"\n", .{
            token.loc.line,
            token.loc.column,
            @tagName(token.kind),
            text,
        }) catch continue;
        try stdout.writeAll(msg);
        if (token.kind == .eof) break;
    }

    try stdout.writeAll("\n=== TODO: Parse and Execute ===\n");
}

fn tokenizeFile(allocator: std.mem.Allocator, path: []const u8) !void {
    const source = readSourceFile(allocator, path) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Error opening file '{s}': {}\n", .{ path, err }) catch "Error opening file\n";
        try getStdErr().writeAll(msg);
        return;
    };
    defer allocator.free(source);

    const stdout = getStdOut();
    var lexer = Lexer.init(source);
    var buf: [512]u8 = undefined;

    while (true) {
        const token = lexer.next();
        const text = source[token.loc.start..token.loc.end];

        // Skip newlines for cleaner output
        if (token.kind == .newline) continue;

        const msg = std.fmt.bufPrint(&buf, "{d}:{d}\t{s}\t{s}\n", .{
            token.loc.line,
            token.loc.column,
            @tagName(token.kind),
            text,
        }) catch continue;
        try stdout.writeAll(msg);

        if (token.kind == .eof) break;
    }
}

fn readSourceFile(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    return try file.readToEndAlloc(allocator, 10 * 1024 * 1024);
}

fn printUsage() !void {
    try getStdOut().writeAll(
        \\
        \\  Klar - No ambiguity. No surprises.
        \\
        \\Usage: klar <command> [options]
        \\
        \\Commands:
        \\  run <file>       Run a Klar program
        \\  tokenize <file>  Tokenize a file (lexer output)
        \\  build            Build a Klar project
        \\  check            Type check without building
        \\  test             Run tests
        \\  fmt              Format source files
        \\  help             Show this help
        \\  version          Show version
        \\
        \\Examples:
        \\  klar run hello.kl
        \\  klar tokenize example.kl
        \\  klar build --release
        \\  klar test
        \\
    );
}

fn printVersion() !void {
    try getStdOut().writeAll("Klar 0.1.0-dev\n");
}

// Re-export tests from lexer
test {
    _ = @import("lexer.zig");
}
