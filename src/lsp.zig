const std = @import("std");
const version = @import("version.zig");

const LspAction = enum {
    continue_running,
    stop,
};

const max_payload_bytes: usize = 8 * 1024 * 1024;

fn readContentLength(reader: *std.Io.Reader) !?usize {
    var content_length: ?usize = null;

    while (true) {
        const line_opt = try reader.takeDelimiter('\n');
        if (line_opt == null) {
            if (content_length == null) return null;
            return error.InvalidHeader;
        }

        const line = std.mem.trimRight(u8, line_opt.?, "\r");
        if (line.len == 0) break;

        if (std.mem.startsWith(u8, line, "Content-Length:")) {
            const value_text = std.mem.trim(u8, line["Content-Length:".len..], " \t");
            if (value_text.len == 0) return error.InvalidHeader;
            content_length = std.fmt.parseInt(usize, value_text, 10) catch return error.InvalidHeader;
            if (content_length.? > max_payload_bytes) return error.PayloadTooLarge;
        }
    }

    return content_length orelse error.InvalidHeader;
}

fn writeMessage(out: std.fs.File, payload: []const u8) !void {
    var header_buf: [128]u8 = undefined;
    const header = std.fmt.bufPrint(&header_buf, "Content-Length: {d}\r\n\r\n", .{payload.len}) catch return error.WriteFailed;
    try out.writeAll(header);
    try out.writeAll(payload);
}

fn writeJsonString(writer: anytype, value: []const u8) !void {
    try writer.writeAll("\"");
    for (value) |c| {
        switch (c) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            else => try writer.writeAll(&[_]u8{c}),
        }
    }
    try writer.writeAll("\"");
}

fn writeJsonId(writer: anytype, id: std.json.Value) !void {
    switch (id) {
        .integer => |v| try writer.print("{d}", .{v}),
        .float => |v| try writer.print("{d}", .{v}),
        .number_string => |v| try writer.writeAll(v),
        .string => |v| try writeJsonString(writer, v),
        else => try writer.writeAll("null"),
    }
}

fn sendResult(allocator: std.mem.Allocator, out: std.fs.File, id: std.json.Value, result_json: []const u8) !void {
    var payload = std.ArrayListUnmanaged(u8){};
    defer payload.deinit(allocator);
    const writer = payload.writer(allocator);

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try writeJsonId(writer, id);
    try writer.writeAll(",\"result\":");
    try writer.writeAll(result_json);
    try writer.writeAll("}");
    try writeMessage(out, payload.items);
}

fn sendError(allocator: std.mem.Allocator, out: std.fs.File, id: std.json.Value, code: i64, message: []const u8) !void {
    var payload = std.ArrayListUnmanaged(u8){};
    defer payload.deinit(allocator);
    const writer = payload.writer(allocator);

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try writeJsonId(writer, id);
    try writer.print(",\"error\":{{\"code\":{d},\"message\":", .{code});
    try writeJsonString(writer, message);
    try writer.writeAll("}}");
    try writeMessage(out, payload.items);
}

fn jsonNullId() std.json.Value {
    return .{ .null = {} };
}

fn handleMessage(
    allocator: std.mem.Allocator,
    out: std.fs.File,
    payload: []const u8,
    shutdown_requested: *bool,
) !LspAction {
    const parsed = std.json.parseFromSlice(std.json.Value, allocator, payload, .{}) catch {
        try sendError(allocator, out, jsonNullId(), -32700, "Parse error");
        return .continue_running;
    };
    defer parsed.deinit();

    if (parsed.value != .object) {
        try sendError(allocator, out, jsonNullId(), -32600, "Invalid Request");
        return .continue_running;
    }
    const object = parsed.value.object;
    const method_val = object.get("method") orelse {
        const id = object.get("id") orelse jsonNullId();
        try sendError(allocator, out, id, -32600, "Invalid Request");
        return .continue_running;
    };
    if (method_val != .string) {
        const id = object.get("id") orelse jsonNullId();
        try sendError(allocator, out, id, -32600, "Invalid Request");
        return .continue_running;
    }
    const method = method_val.string;
    const id_opt = object.get("id");

    if (std.mem.eql(u8, method, "initialize")) {
        if (id_opt) |id| {
            try sendResult(
                allocator,
                out,
                id,
                "{\"capabilities\":{},\"serverInfo\":{\"name\":\"klar-lsp\",\"version\":\"" ++ version.display ++ "\"}}",
            );
        }
        return .continue_running;
    }

    if (std.mem.eql(u8, method, "shutdown")) {
        shutdown_requested.* = true;
        if (id_opt) |id| {
            try sendResult(allocator, out, id, "null");
        }
        return .continue_running;
    }

    if (std.mem.eql(u8, method, "exit")) {
        return .stop;
    }

    if (id_opt) |id| {
        try sendError(allocator, out, id, -32601, "Method not found");
    }
    return .continue_running;
}

pub fn runStdio(allocator: std.mem.Allocator) !void {
    const stdin_file: std.fs.File = .{ .handle = std.posix.STDIN_FILENO };
    const stdout_file: std.fs.File = .{ .handle = std.posix.STDOUT_FILENO };

    var read_buffer: [4096]u8 = undefined;
    var reader = stdin_file.reader(&read_buffer);
    var shutdown_requested = false;

    while (true) {
        const content_length_opt = readContentLength(&reader.interface) catch |err| switch (err) {
            error.InvalidHeader, error.PayloadTooLarge => std.process.exit(1),
            else => return err,
        };
        if (content_length_opt == null) break;
        const content_length = content_length_opt.?;

        const payload = try allocator.alloc(u8, content_length);
        defer allocator.free(payload);
        reader.interface.readSliceAll(payload) catch std.process.exit(1);

        const action = handleMessage(allocator, stdout_file, payload, &shutdown_requested) catch std.process.exit(1);
        if (action == .stop) {
            std.process.exit(if (shutdown_requested) 0 else 1);
        }
    }
}
