const std = @import("std");
const version = @import("version.zig");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const TypeChecker = @import("checker/mod.zig").TypeChecker;
const types = @import("types.zig");

const LspAction = enum {
    continue_running,
    stop,
};

const max_payload_bytes: usize = 8 * 1024 * 1024;
const max_source_bytes: usize = 8 * 1024 * 1024;
const max_open_documents: usize = 256;
const max_total_document_bytes: usize = 32 * 1024 * 1024;

const DocumentEntry = struct {
    text: []u8,
    generation: u64,
};

const CompletionCacheEntry = struct {
    generation: u64,
    line: usize,
    character: usize,
    result_json: []u8,
};

const ServerState = struct {
    documents: std.StringHashMapUnmanaged(DocumentEntry) = .{},
    completion_cache: std.StringHashMapUnmanaged(CompletionCacheEntry) = .{},
    total_document_bytes: usize = 0,

    fn deinit(self: *ServerState, allocator: std.mem.Allocator) void {
        var it = self.documents.iterator();
        while (it.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            allocator.free(entry.value_ptr.*.text);
        }
        self.documents.deinit(allocator);

        var cache_it = self.completion_cache.iterator();
        while (cache_it.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            allocator.free(entry.value_ptr.*.result_json);
        }
        self.completion_cache.deinit(allocator);
    }

    fn upsertDocument(self: *ServerState, allocator: std.mem.Allocator, uri: []const u8, text: []const u8) !void {
        if (text.len > max_source_bytes) return error.DocumentTooLarge;

        if (self.documents.getEntry(uri)) |entry| {
            const old_len = entry.value_ptr.*.text.len;
            const new_total = self.total_document_bytes - old_len + text.len;
            if (new_total > max_total_document_bytes) return error.DocumentStoreFull;

            allocator.free(entry.value_ptr.*.text);
            entry.value_ptr.* = .{
                .text = try allocator.dupe(u8, text),
                .generation = entry.value_ptr.*.generation + 1,
            };
            self.total_document_bytes = new_total;
            self.clearCompletionCacheForUri(allocator, uri);
            return;
        }

        if (self.documents.count() >= max_open_documents) return error.DocumentLimitReached;
        if (self.total_document_bytes + text.len > max_total_document_bytes) return error.DocumentStoreFull;

        const owned_uri = try allocator.dupe(u8, uri);
        errdefer allocator.free(owned_uri);
        const entry = DocumentEntry{
            .text = try allocator.dupe(u8, text),
            .generation = 1,
        };
        errdefer allocator.free(entry.text);
        try self.documents.put(allocator, owned_uri, entry);
        self.total_document_bytes += text.len;
    }

    fn clearCompletionCacheForUri(self: *ServerState, allocator: std.mem.Allocator, uri: []const u8) void {
        if (self.completion_cache.fetchRemove(uri)) |entry| {
            allocator.free(entry.key);
            allocator.free(entry.value.result_json);
        }
    }

    fn getCompletionCache(self: *const ServerState, uri: []const u8) ?CompletionCacheEntry {
        return self.completion_cache.get(uri);
    }

    fn upsertCompletionCache(
        self: *ServerState,
        allocator: std.mem.Allocator,
        uri: []const u8,
        cache: CompletionCacheEntry,
    ) !void {
        self.clearCompletionCacheForUri(allocator, uri);
        const owned_uri = try allocator.dupe(u8, uri);
        errdefer allocator.free(owned_uri);
        const owned_json = try allocator.dupe(u8, cache.result_json);
        errdefer allocator.free(owned_json);
        try self.completion_cache.put(allocator, owned_uri, .{
            .generation = cache.generation,
            .line = cache.line,
            .character = cache.character,
            .result_json = owned_json,
        });
    }

    fn removeDocument(self: *ServerState, allocator: std.mem.Allocator, uri: []const u8) void {
        if (self.documents.fetchRemove(uri)) |entry| {
            self.total_document_bytes -= entry.value.text.len;
            allocator.free(entry.key);
            allocator.free(entry.value.text);
        }
        self.clearCompletionCacheForUri(allocator, uri);
    }
};

const Diagnostic = struct {
    line: usize,
    column: usize,
    message: []const u8,
    source: []const u8,
};

fn freeDiagnostics(allocator: std.mem.Allocator, diagnostics: []Diagnostic) void {
    for (diagnostics) |diag| allocator.free(diag.message);
    allocator.free(diagnostics);
}

const CompletionItem = struct {
    label: []u8,
    detail: []u8,
    kind: usize,
    sort_text: []u8,
};

fn freeCompletionItems(allocator: std.mem.Allocator, items: []CompletionItem) void {
    for (items) |item| {
        allocator.free(item.label);
        allocator.free(item.detail);
        allocator.free(item.sort_text);
    }
    allocator.free(items);
}

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

fn jsonNullId() std.json.Value {
    return .{ .null = {} };
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

fn uriToPath(allocator: std.mem.Allocator, uri: []const u8) !?[]u8 {
    if (uri.len == 0) return null;

    const hex = "0123456789abcdefABCDEF";
    for (uri, 0..) |c, i| {
        if (c != '%') continue;
        if (i + 2 >= uri.len) return error.InvalidUri;
        if (std.mem.indexOfScalar(u8, hex, uri[i + 1]) == null) return error.InvalidUri;
        if (std.mem.indexOfScalar(u8, hex, uri[i + 2]) == null) return error.InvalidUri;
    }

    if (std.mem.startsWith(u8, uri, "file://")) {
        const rest = uri["file://".len..];
        if (rest.len == 0) return null;

        const path_slice = if (rest[0] == '/') rest else blk: {
            const slash_idx = std.mem.indexOfScalar(u8, rest, '/') orelse return null;
            const authority = rest[0..slash_idx];
            if (!std.mem.eql(u8, authority, "localhost")) return null;
            break :blk rest[slash_idx..];
        };

        var decoded = std.ArrayListUnmanaged(u8){};
        errdefer decoded.deinit(allocator);

        var i: usize = 0;
        while (i < path_slice.len) : (i += 1) {
            if (path_slice[i] == '%') {
                const hi = std.fmt.charToDigit(path_slice[i + 1], 16) catch return error.InvalidUri;
                const lo = std.fmt.charToDigit(path_slice[i + 2], 16) catch return error.InvalidUri;
                const byte: u8 = @as(u8, @intCast(hi * 16 + lo));
                try decoded.append(allocator, byte);
                i += 2;
            } else {
                try decoded.append(allocator, path_slice[i]);
            }
        }
        const decoded_path = try decoded.toOwnedSlice(allocator);
        return decoded_path;
    }
    if (std.mem.indexOf(u8, uri, "://") != null) return null;
    return try allocator.dupe(u8, uri);
}

fn readSourceFile(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    const file = std.fs.cwd().openFile(path, .{}) catch return error.FileNotFound;
    defer file.close();
    return try file.readToEndAlloc(allocator, max_source_bytes);
}

fn sourceOffsetFromZeroBasedLineColumn(source: []const u8, line: usize, column: usize) ?usize {
    var current_line: usize = 0;
    var line_start: usize = 0;
    var i: usize = 0;

    while (i < source.len and current_line < line) : (i += 1) {
        if (source[i] == '\n') {
            current_line += 1;
            line_start = i + 1;
        }
    }

    if (current_line != line) return null;

    var line_end = source.len;
    i = line_start;
    while (i < source.len) : (i += 1) {
        if (source[i] == '\n') {
            line_end = i;
            break;
        }
    }

    const offset = line_start + column;
    if (offset > line_end) return null;
    return offset;
}

fn collectDiagnosticsFromSource(allocator: std.mem.Allocator, source: []const u8) ![]Diagnostic {
    var diagnostics = std.ArrayListUnmanaged(Diagnostic){};
    errdefer {
        for (diagnostics.items) |diag| allocator.free(diag.message);
        diagnostics.deinit(allocator);
    }

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var lexer = Lexer.init(source);
    var parser = Parser.init(arena.allocator(), &lexer, source);
    defer parser.deinit();

    const module = parser.parseModuleRecovering() catch return diagnostics.toOwnedSlice(allocator);

    for (parser.errors.items) |parse_err| {
        try diagnostics.append(allocator, .{
            .line = parse_err.span.line,
            .column = parse_err.span.column,
            .message = try allocator.dupe(u8, parse_err.message),
            .source = "parser",
        });
    }

    var checker = TypeChecker.init(allocator);
    defer checker.deinit();
    checker.setTestMode(true);
    checker.checkModule(module);

    for (checker.errors.items) |check_err| {
        try diagnostics.append(allocator, .{
            .line = check_err.span.line,
            .column = check_err.span.column,
            .message = try allocator.dupe(u8, check_err.message),
            .source = "checker",
        });
    }

    return diagnostics.toOwnedSlice(allocator);
}

fn writeDiagnosticResultJson(allocator: std.mem.Allocator, diagnostics: []const Diagnostic) ![]u8 {
    var out = std.ArrayListUnmanaged(u8){};
    errdefer out.deinit(allocator);
    const writer = out.writer(allocator);

    try writer.writeAll("{\"kind\":\"full\",\"items\":[");
    for (diagnostics, 0..) |diag, idx| {
        if (idx > 0) try writer.writeAll(",");
        const start_line: usize = if (diag.line > 0) diag.line - 1 else 0;
        const start_col: usize = if (diag.column > 0) diag.column - 1 else 0;
        const end_col = start_col + 1;
        try writer.print(
            "{{\"range\":{{\"start\":{{\"line\":{d},\"character\":{d}}},\"end\":{{\"line\":{d},\"character\":{d}}}}},\"severity\":1,\"source\":",
            .{ start_line, start_col, start_line, end_col },
        );
        try writeJsonString(writer, diag.source);
        try writer.writeAll(",\"message\":");
        try writeJsonString(writer, diag.message);
        try writer.writeAll("}");
    }
    try writer.writeAll("]}");
    return out.toOwnedSlice(allocator);
}

fn getDiagnosticUri(params: std.json.Value) ?[]const u8 {
    if (params != .object) return null;
    const text_document = params.object.get("textDocument") orelse return null;
    if (text_document != .object) return null;
    const uri = text_document.object.get("uri") orelse return null;
    if (uri != .string) return null;
    return uri.string;
}

fn parseJsonUsize(value: std.json.Value) ?usize {
    return switch (value) {
        .integer => |v| if (v >= 0) @as(usize, @intCast(v)) else null,
        .number_string => |s| std.fmt.parseInt(usize, s, 10) catch null,
        else => null,
    };
}

const CompletionRequest = struct {
    uri: []const u8,
    line: usize,
    character: usize,
};

fn parseCompletionRequest(params: std.json.Value) ?CompletionRequest {
    if (params != .object) return null;
    const uri = getTextDocumentUri(params) orelse return null;
    const pos_val = params.object.get("position") orelse return null;
    if (pos_val != .object) return null;
    const line = parseJsonUsize(pos_val.object.get("line") orelse return null) orelse return null;
    const character = parseJsonUsize(pos_val.object.get("character") orelse return null) orelse return null;
    return .{ .uri = uri, .line = line, .character = character };
}

fn completionItemKind(kind: @import("checker/mod.zig").Symbol.Kind) usize {
    return switch (kind) {
        .function => 3,
        .type_, .trait_ => 7,
        .module => 9,
        .variable, .parameter, .constant => 6,
    };
}

fn isRuntimeCompletionKind(kind: @import("checker/mod.zig").Symbol.Kind) bool {
    return switch (kind) {
        .variable, .parameter, .constant, .function => true,
        .type_, .trait_, .module => false,
    };
}

fn collectCompletionsFromSource(allocator: std.mem.Allocator, source: []const u8, line: usize, character: usize) ![]CompletionItem {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var lexer = Lexer.init(source);
    var parser = Parser.init(arena.allocator(), &lexer, source);
    defer parser.deinit();

    const module = parser.parseModuleRecovering() catch {
        return try allocator.alloc(CompletionItem, 0);
    };

    var checker = TypeChecker.init(allocator);
    defer checker.deinit();
    checker.setTestMode(true);
    checker.checkModule(module);

    const cursor_offset = sourceOffsetFromZeroBasedLineColumn(source, line, character) orelse {
        return try allocator.alloc(CompletionItem, 0);
    };

    const expected_type = checker.expectedTypeAtOffset(module, cursor_offset);
    const scope_entries = checker.extractScopeAtOffset(module, cursor_offset) catch {
        return try allocator.alloc(CompletionItem, 0);
    };
    defer allocator.free(scope_entries);

    var items = std.ArrayListUnmanaged(CompletionItem){};
    errdefer {
        for (items.items) |item| {
            allocator.free(item.label);
            allocator.free(item.detail);
            allocator.free(item.sort_text);
        }
        items.deinit(allocator);
    }

    for (scope_entries) |entry| {
        const detail = types.typeToString(allocator, entry.type_) catch try allocator.dupe(u8, "unknown");
        errdefer allocator.free(detail);
        const label = try allocator.dupe(u8, entry.name);
        errdefer allocator.free(label);

        const rank: u8 = if (expected_type != null and isRuntimeCompletionKind(entry.kind) and entry.type_.eql(expected_type.?)) '0' else '1';
        const sort_text = try std.fmt.allocPrint(allocator, "{c}_{s}", .{ rank, entry.name });
        errdefer allocator.free(sort_text);

        try items.append(allocator, .{
            .label = label,
            .detail = detail,
            .kind = completionItemKind(entry.kind),
            .sort_text = sort_text,
        });
    }

    return items.toOwnedSlice(allocator);
}

fn writeCompletionResultJson(allocator: std.mem.Allocator, items: []const CompletionItem) ![]u8 {
    var out = std.ArrayListUnmanaged(u8){};
    errdefer out.deinit(allocator);
    const writer = out.writer(allocator);

    try writer.writeAll("{\"isIncomplete\":false,\"items\":[");
    for (items, 0..) |item, idx| {
        if (idx > 0) try writer.writeAll(",");
        try writer.writeAll("{\"label\":");
        try writeJsonString(writer, item.label);
        try writer.print(",\"kind\":{d},\"detail\":", .{item.kind});
        try writeJsonString(writer, item.detail);
        try writer.writeAll(",\"sortText\":");
        try writeJsonString(writer, item.sort_text);
        try writer.writeAll("}");
    }
    try writer.writeAll("]}");
    return out.toOwnedSlice(allocator);
}

fn handleCompletionRequest(
    allocator: std.mem.Allocator,
    state: *ServerState,
    out: std.fs.File,
    id: std.json.Value,
    params_opt: ?std.json.Value,
) !void {
    const params = params_opt orelse {
        try sendError(allocator, out, id, -32602, "Missing params");
        return;
    };
    const req = parseCompletionRequest(params) orelse {
        try sendError(allocator, out, id, -32602, "Missing textDocument/position");
        return;
    };

    const path = uriToPath(allocator, req.uri) catch {
        try sendError(allocator, out, id, -32603, "Failed to parse URI");
        return;
    } orelse {
        try sendError(allocator, out, id, -32602, "Only file:// URIs are supported");
        return;
    };
    defer allocator.free(path);

    const doc_generation = if (state.documents.get(req.uri)) |doc| doc.generation else 0;
    if (state.getCompletionCache(req.uri)) |cached| {
        if (cached.generation == doc_generation and cached.line == req.line and cached.character == req.character) {
            try sendResult(allocator, out, id, cached.result_json);
            return;
        }
    }

    const source = if (state.documents.get(req.uri)) |doc| blk: {
        break :blk try allocator.dupe(u8, doc.text);
    } else readSourceFile(allocator, path) catch {
        try sendError(allocator, out, id, -32603, "Failed to read source file");
        return;
    };
    defer allocator.free(source);

    const items = collectCompletionsFromSource(allocator, source, req.line, req.character) catch {
        try sendError(allocator, out, id, -32603, "Failed to compute completions");
        return;
    };
    defer freeCompletionItems(allocator, items);

    const result_json = writeCompletionResultJson(allocator, items) catch {
        try sendError(allocator, out, id, -32603, "Failed to encode completions");
        return;
    };
    defer allocator.free(result_json);

    state.upsertCompletionCache(allocator, req.uri, .{
        .generation = doc_generation,
        .line = req.line,
        .character = req.character,
        .result_json = result_json,
    }) catch {};

    try sendResult(allocator, out, id, result_json);
}

fn handleDiagnosticRequest(
    allocator: std.mem.Allocator,
    state: *ServerState,
    out: std.fs.File,
    id: std.json.Value,
    params_opt: ?std.json.Value,
) !void {
    const params = params_opt orelse {
        try sendError(allocator, out, id, -32602, "Missing params");
        return;
    };
    const uri = getDiagnosticUri(params) orelse {
        try sendError(allocator, out, id, -32602, "Missing textDocument.uri");
        return;
    };

    const path = uriToPath(allocator, uri) catch {
        try sendError(allocator, out, id, -32603, "Failed to parse URI");
        return;
    } orelse {
        try sendError(allocator, out, id, -32602, "Only file:// URIs are supported");
        return;
    };
    defer allocator.free(path);

    const source = if (state.documents.get(uri)) |doc| blk: {
        break :blk try allocator.dupe(u8, doc.text);
    } else readSourceFile(allocator, path) catch {
        try sendError(allocator, out, id, -32603, "Failed to read source file");
        return;
    };
    defer allocator.free(source);

    const diagnostics = collectDiagnosticsFromSource(allocator, source) catch {
        try sendError(allocator, out, id, -32603, "Failed to compute diagnostics");
        return;
    };
    defer freeDiagnostics(allocator, diagnostics);

    const result_json = writeDiagnosticResultJson(allocator, diagnostics) catch {
        try sendError(allocator, out, id, -32603, "Failed to encode diagnostics");
        return;
    };
    defer allocator.free(result_json);

    try sendResult(allocator, out, id, result_json);
}

fn getTextDocumentUri(params: std.json.Value) ?[]const u8 {
    if (params != .object) return null;
    const text_document = params.object.get("textDocument") orelse return null;
    if (text_document != .object) return null;
    const uri = text_document.object.get("uri") orelse return null;
    if (uri != .string) return null;
    return uri.string;
}

fn handleDidOpen(
    allocator: std.mem.Allocator,
    state: *ServerState,
    params_opt: ?std.json.Value,
) void {
    const params = params_opt orelse return;
    if (params != .object) return;
    const text_document = params.object.get("textDocument") orelse return;
    if (text_document != .object) return;

    const uri_val = text_document.object.get("uri") orelse return;
    const text_val = text_document.object.get("text") orelse return;
    if (uri_val != .string or text_val != .string) return;

    state.upsertDocument(allocator, uri_val.string, text_val.string) catch {};
}

fn handleDidChange(
    allocator: std.mem.Allocator,
    state: *ServerState,
    params_opt: ?std.json.Value,
) void {
    const params = params_opt orelse return;
    if (params != .object) return;
    const uri = getTextDocumentUri(params) orelse return;
    const changes = params.object.get("contentChanges") orelse return;
    if (changes != .array or changes.array.items.len == 0) return;

    const first_change = changes.array.items[0];
    if (first_change != .object) return;
    const text_val = first_change.object.get("text") orelse return;
    if (text_val != .string) return;

    state.upsertDocument(allocator, uri, text_val.string) catch {};
}

fn handleDidClose(
    allocator: std.mem.Allocator,
    state: *ServerState,
    params_opt: ?std.json.Value,
) void {
    const params = params_opt orelse return;
    const uri = getTextDocumentUri(params) orelse return;
    state.removeDocument(allocator, uri);
}

fn handleMessage(
    allocator: std.mem.Allocator,
    state: *ServerState,
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
    const params_opt = object.get("params");

    if (std.mem.eql(u8, method, "initialize")) {
        if (id_opt) |id| {
            try sendResult(
                allocator,
                out,
                id,
                "{\"capabilities\":{\"textDocumentSync\":{\"openClose\":true,\"change\":1},\"diagnosticProvider\":{\"interFileDependencies\":false,\"workspaceDiagnostics\":false},\"completionProvider\":{\"resolveProvider\":false}},\"serverInfo\":{\"name\":\"klar-lsp\",\"version\":\"" ++ version.display ++ "\"}}",
            );
        }
        return .continue_running;
    }

    if (std.mem.eql(u8, method, "textDocument/didOpen")) {
        handleDidOpen(allocator, state, params_opt);
        return .continue_running;
    }

    if (std.mem.eql(u8, method, "textDocument/didChange")) {
        handleDidChange(allocator, state, params_opt);
        return .continue_running;
    }

    if (std.mem.eql(u8, method, "textDocument/didClose")) {
        handleDidClose(allocator, state, params_opt);
        return .continue_running;
    }

    if (std.mem.eql(u8, method, "textDocument/diagnostic")) {
        if (id_opt) |id| {
            try handleDiagnosticRequest(allocator, state, out, id, params_opt);
        }
        return .continue_running;
    }

    if (std.mem.eql(u8, method, "textDocument/completion")) {
        if (id_opt) |id| {
            try handleCompletionRequest(allocator, state, out, id, params_opt);
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
    var state = ServerState{};
    defer state.deinit(allocator);

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

        const action = handleMessage(allocator, &state, stdout_file, payload, &shutdown_requested) catch std.process.exit(1);
        if (action == .stop) {
            std.process.exit(if (shutdown_requested) 0 else 1);
        }
    }
}
