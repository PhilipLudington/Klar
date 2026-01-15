//! Simple HTTP Server Example
//!
//! Demonstrates:
//! - TCP socket handling with std.net
//! - Arena allocator for per-request memory
//! - Error handling patterns
//! - Buffered I/O (Zig 0.15+)

const std = @import("std");
const net = std.net;
const Allocator = std.mem.Allocator;

/// Server configuration with sensible defaults.
pub const Config = struct {
    port: u16 = 8080,
    max_connections: u32 = 128,
    read_timeout_ms: u32 = 30_000,
    max_request_size: usize = 1024 * 1024, // 1MB
};

/// HTTP request parsed from raw bytes.
pub const Request = struct {
    method: Method,
    path: []const u8,
    headers: std.StringHashMap([]const u8),
    body: []const u8,

    pub const Method = enum {
        GET,
        POST,
        PUT,
        DELETE,
        HEAD,
        OPTIONS,
    };

    pub fn deinit(self: *Request) void {
        self.headers.deinit();
    }
};

/// HTTP response builder.
pub const Response = struct {
    status: u16 = 200,
    status_text: []const u8 = "OK",
    headers: std.ArrayList(Header),
    body: []const u8 = "",

    const Header = struct {
        name: []const u8,
        value: []const u8,
    };

    pub fn init(allocator: Allocator) Response {
        return .{
            .headers = std.ArrayList(Header).init(allocator),
        };
    }

    pub fn deinit(self: *Response) void {
        self.headers.deinit();
    }

    pub fn setHeader(self: *Response, name: []const u8, value: []const u8) !void {
        try self.headers.append(.{ .name = name, .value = value });
    }

    /// Write response to stream.
    pub fn write(self: *const Response, writer: anytype) !void {
        // Status line
        try writer.print("HTTP/1.1 {d} {s}\r\n", .{ self.status, self.status_text });

        // Headers
        for (self.headers.items) |header| {
            try writer.print("{s}: {s}\r\n", .{ header.name, header.value });
        }

        // Content-Length if body present
        if (self.body.len > 0) {
            try writer.print("Content-Length: {d}\r\n", .{self.body.len});
        }

        // End headers
        try writer.writeAll("\r\n");

        // Body
        if (self.body.len > 0) {
            try writer.writeAll(self.body);
        }
    }
};

/// Parse HTTP request from raw bytes.
/// Caller owns returned Request and must call deinit().
pub fn parseRequest(allocator: Allocator, data: []const u8) !Request {
    var headers = std.StringHashMap([]const u8).init(allocator);
    errdefer headers.deinit();

    // Find end of headers
    const header_end = std.mem.indexOf(u8, data, "\r\n\r\n") orelse
        return error.InvalidRequest;

    const header_section = data[0..header_end];
    const body = data[header_end + 4 ..];

    // Parse request line
    var lines = std.mem.splitSequence(u8, header_section, "\r\n");
    const request_line = lines.next() orelse return error.InvalidRequest;

    var parts = std.mem.splitScalar(u8, request_line, ' ');
    const method_str = parts.next() orelse return error.InvalidRequest;
    const path = parts.next() orelse return error.InvalidRequest;

    const method = std.meta.stringToEnum(Request.Method, method_str) orelse
        return error.UnsupportedMethod;

    // Parse headers
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        const colon = std.mem.indexOf(u8, line, ":") orelse continue;
        const name = std.mem.trim(u8, line[0..colon], " ");
        const value = std.mem.trim(u8, line[colon + 1 ..], " ");
        try headers.put(name, value);
    }

    return Request{
        .method = method,
        .path = path,
        .headers = headers,
        .body = body,
    };
}

/// Handle a single HTTP connection.
fn handleConnection(
    allocator: Allocator,
    conn: net.Server.Connection,
    handler: *const fn (Allocator, *const Request) Response,
) void {
    defer conn.stream.close();

    // Read request with timeout
    var buffer: [8192]u8 = undefined;
    const bytes_read = conn.stream.read(&buffer) catch |err| {
        std.log.err("Read error: {}", .{err});
        return;
    };

    if (bytes_read == 0) return;

    // Parse and handle request
    var request = parseRequest(allocator, buffer[0..bytes_read]) catch |err| {
        std.log.err("Parse error: {}", .{err});
        sendError(conn.stream, 400, "Bad Request");
        return;
    };
    defer request.deinit();

    // Call user handler
    var response = handler(allocator, &request);
    defer response.deinit();

    // Send response
    var write_buffer: [4096]u8 = undefined;
    var buffered = std.io.bufferedWriter(conn.stream.writer());
    response.write(buffered.writer()) catch |err| {
        std.log.err("Write error: {}", .{err});
        return;
    };
    _ = write_buffer;
    buffered.flush() catch {};
}

fn sendError(stream: net.Stream, code: u16, message: []const u8) void {
    var buf: [256]u8 = undefined;
    const response = std.fmt.bufPrint(&buf, "HTTP/1.1 {d} {s}\r\nContent-Length: 0\r\n\r\n", .{ code, message }) catch return;
    stream.writeAll(response) catch {};
}

/// Run the HTTP server.
pub fn run(allocator: Allocator, config: Config, handler: *const fn (Allocator, *const Request) Response) !void {
    const address = net.Address.initIp4(.{ 0, 0, 0, 0 }, config.port);

    var server = try address.listen(.{
        .reuse_address = true,
    });
    defer server.deinit();

    std.log.info("Server listening on port {d}", .{config.port});

    while (true) {
        const conn = server.accept() catch |err| {
            std.log.err("Accept error: {}", .{err});
            continue;
        };

        // Use arena for per-request allocations
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();

        handleConnection(arena.allocator(), conn, handler);
    }
}

// Example request handler
fn exampleHandler(allocator: Allocator, request: *const Request) Response {
    _ = allocator;

    var response = Response.init(std.heap.page_allocator);

    response.setHeader("Content-Type", "text/plain") catch {};
    response.setHeader("Server", "CarbideZig-Example/0.1") catch {};

    switch (request.method) {
        .GET => {
            if (std.mem.eql(u8, request.path, "/")) {
                response.body = "Hello from CarbideZig HTTP Server!";
            } else if (std.mem.eql(u8, request.path, "/health")) {
                response.body = "OK";
            } else {
                response.status = 404;
                response.status_text = "Not Found";
                response.body = "Not Found";
            }
        },
        else => {
            response.status = 405;
            response.status_text = "Method Not Allowed";
            response.body = "Method Not Allowed";
        },
    }

    return response;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const status = gpa.deinit();
        if (status == .leak) {
            std.log.err("Memory leak detected!", .{});
        }
    }

    const config = Config{
        .port = 8080,
    };

    std.log.info("Starting HTTP server...", .{});
    try run(gpa.allocator(), config, exampleHandler);
}

// Tests
test "parse simple GET request" {
    const request_data =
        "GET /path HTTP/1.1\r\n" ++
        "Host: localhost\r\n" ++
        "Content-Type: text/plain\r\n" ++
        "\r\n";

    var request = try parseRequest(std.testing.allocator, request_data);
    defer request.deinit();

    try std.testing.expectEqual(Request.Method.GET, request.method);
    try std.testing.expectEqualStrings("/path", request.path);
    try std.testing.expectEqualStrings("localhost", request.headers.get("Host").?);
}

test "parse request with body" {
    const request_data =
        "POST /api/data HTTP/1.1\r\n" ++
        "Content-Length: 13\r\n" ++
        "\r\n" ++
        "Hello, World!";

    var request = try parseRequest(std.testing.allocator, request_data);
    defer request.deinit();

    try std.testing.expectEqual(Request.Method.POST, request.method);
    try std.testing.expectEqualStrings("Hello, World!", request.body);
}

test "response formatting" {
    var response = Response.init(std.testing.allocator);
    defer response.deinit();

    response.status = 200;
    response.body = "Hello";
    try response.setHeader("Content-Type", "text/plain");

    var buffer: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buffer);
    try response.write(stream.writer());

    const output = stream.getWritten();
    try std.testing.expect(std.mem.startsWith(u8, output, "HTTP/1.1 200 OK\r\n"));
    try std.testing.expect(std.mem.indexOf(u8, output, "Content-Type: text/plain") != null);
}
