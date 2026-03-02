const std = @import("std");
const builtin = @import("builtin");
const version = @import("version.zig");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const TypeChecker = @import("checker/mod.zig").TypeChecker;
const checker_mod = @import("checker/mod.zig");
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

const RequestResultCacheEntry = struct {
    generation: u64,
    line: usize,
    character: usize,
    result_json: []u8,
};

const ServerState = struct {
    documents: std.StringHashMapUnmanaged(DocumentEntry) = .{},
    completion_cache: std.StringHashMapUnmanaged(CompletionCacheEntry) = .{},
    hover_cache: std.StringHashMapUnmanaged(RequestResultCacheEntry) = .{},
    definition_cache: std.StringHashMapUnmanaged(RequestResultCacheEntry) = .{},
    total_document_bytes: usize = 0,
    /// Workspace root path extracted from initialize rootUri.
    /// Used to restrict file reads to the workspace directory.
    workspace_root: ?[]u8 = null,

    fn deinit(self: *ServerState, allocator: std.mem.Allocator) void {
        if (self.workspace_root) |root| allocator.free(root);
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

        var hover_it = self.hover_cache.iterator();
        while (hover_it.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            allocator.free(entry.value_ptr.*.result_json);
        }
        self.hover_cache.deinit(allocator);

        var definition_it = self.definition_cache.iterator();
        while (definition_it.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            allocator.free(entry.value_ptr.*.result_json);
        }
        self.definition_cache.deinit(allocator);
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
            self.clearHoverCacheForUri(allocator, uri);
            self.clearDefinitionCacheForUri(allocator, uri);
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

    fn clearHoverCacheForUri(self: *ServerState, allocator: std.mem.Allocator, uri: []const u8) void {
        if (self.hover_cache.fetchRemove(uri)) |entry| {
            allocator.free(entry.key);
            allocator.free(entry.value.result_json);
        }
    }

    fn clearDefinitionCacheForUri(self: *ServerState, allocator: std.mem.Allocator, uri: []const u8) void {
        if (self.definition_cache.fetchRemove(uri)) |entry| {
            allocator.free(entry.key);
            allocator.free(entry.value.result_json);
        }
    }

    fn getCompletionCache(self: *const ServerState, uri: []const u8) ?CompletionCacheEntry {
        return self.completion_cache.get(uri);
    }

    fn getHoverCache(self: *const ServerState, uri: []const u8) ?RequestResultCacheEntry {
        return self.hover_cache.get(uri);
    }

    fn getDefinitionCache(self: *const ServerState, uri: []const u8) ?RequestResultCacheEntry {
        return self.definition_cache.get(uri);
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

    fn upsertHoverCache(
        self: *ServerState,
        allocator: std.mem.Allocator,
        uri: []const u8,
        cache: RequestResultCacheEntry,
    ) !void {
        self.clearHoverCacheForUri(allocator, uri);
        const owned_uri = try allocator.dupe(u8, uri);
        errdefer allocator.free(owned_uri);
        const owned_json = try allocator.dupe(u8, cache.result_json);
        errdefer allocator.free(owned_json);
        try self.hover_cache.put(allocator, owned_uri, .{
            .generation = cache.generation,
            .line = cache.line,
            .character = cache.character,
            .result_json = owned_json,
        });
    }

    fn upsertDefinitionCache(
        self: *ServerState,
        allocator: std.mem.Allocator,
        uri: []const u8,
        cache: RequestResultCacheEntry,
    ) !void {
        self.clearDefinitionCacheForUri(allocator, uri);
        const owned_uri = try allocator.dupe(u8, uri);
        errdefer allocator.free(owned_uri);
        const owned_json = try allocator.dupe(u8, cache.result_json);
        errdefer allocator.free(owned_json);
        try self.definition_cache.put(allocator, owned_uri, .{
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
        self.clearHoverCacheForUri(allocator, uri);
        self.clearDefinitionCacheForUri(allocator, uri);
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

const KeywordCompletionSpec = struct {
    keyword: []const u8,
    detail: []const u8,
};

const keyword_completion_specs = [_]KeywordCompletionSpec{
    .{ .keyword = "async", .detail = "keyword: async function modifier" },
    .{ .keyword = "await", .detail = "keyword: await async result" },
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
        var decoded_path = try decoded.toOwnedSlice(allocator);

        // Windows: file:///C:/foo decodes to /C:/foo — strip the leading /
        if (builtin.os.tag == .windows and decoded_path.len >= 3 and
            decoded_path[0] == '/' and std.ascii.isAlphabetic(decoded_path[1]) and decoded_path[2] == ':')
        {
            std.mem.copyForwards(u8, decoded_path[0 .. decoded_path.len - 1], decoded_path[1..]);
            decoded_path = try allocator.realloc(decoded_path, decoded_path.len - 1);
        }

        return decoded_path;
    }
    if (std.mem.indexOf(u8, uri, "://") != null) return null;
    return try allocator.dupe(u8, uri);
}

/// Returns true if path contains ".." directory traversal components.
fn containsPathTraversal(path: []const u8) bool {
    var start: usize = 0;
    for (path, 0..) |c, i| {
        if (c == '/' or c == '\\') {
            if (std.mem.eql(u8, path[start..i], "..")) return true;
            start = i + 1;
        }
    }
    // Check final component
    if (path.len > start and std.mem.eql(u8, path[start..], "..")) return true;
    return false;
}

fn readSourceFile(allocator: std.mem.Allocator, path: []const u8, workspace_root: ?[]const u8) ![]u8 {
    // Reject paths with directory traversal components
    if (containsPathTraversal(path)) return error.AccessDenied;

    // If workspace root is set, ensure the resolved path is within it
    if (workspace_root) |root| {
        const real_path = std.fs.cwd().realpathAlloc(allocator, path) catch return error.FileNotFound;
        defer allocator.free(real_path);
        if (!std.mem.startsWith(u8, real_path, root)) return error.AccessDenied;
    }

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

const LineCharacter = struct {
    line: usize,
    character: usize,
};

fn zeroBasedLineCharacterFromOffset(source: []const u8, offset: usize) ?LineCharacter {
    if (offset > source.len) return null;

    var line: usize = 0;
    var character: usize = 0;
    var i: usize = 0;
    while (i < offset) : (i += 1) {
        if (source[i] == '\n') {
            line += 1;
            character = 0;
        } else {
            character += 1;
        }
    }
    return .{ .line = line, .character = character };
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

fn completionItemKind(kind: checker_mod.Symbol.Kind) usize {
    return switch (kind) {
        .function => 3,
        .type_, .trait_ => 7,
        .module => 9,
        .variable, .parameter, .constant => 6,
    };
}

fn isRuntimeCompletionKind(kind: checker_mod.Symbol.Kind) bool {
    return switch (kind) {
        .variable, .parameter, .constant, .function => true,
        .type_, .trait_, .module => false,
    };
}

fn symbolKindLabel(kind: checker_mod.Symbol.Kind) []const u8 {
    return switch (kind) {
        .variable => "variable",
        .parameter => "parameter",
        .function => "function",
        .type_ => "type",
        .trait_ => "trait",
        .module => "module",
        .constant => "constant",
    };
}

fn isIdentifierChar(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '_';
}

fn identifierAtOffset(source: []const u8, offset: usize) ?[]const u8 {
    if (source.len == 0) return null;

    var probe = offset;
    if (probe >= source.len) {
        if (source.len == 0) return null;
        probe = source.len - 1;
    }

    if (!isIdentifierChar(source[probe])) {
        if (offset > 0 and isIdentifierChar(source[offset - 1])) {
            probe = offset - 1;
        } else {
            return null;
        }
    }

    var start = probe;
    while (start > 0 and isIdentifierChar(source[start - 1])) : (start -= 1) {}

    var end = probe + 1;
    while (end < source.len and isIdentifierChar(source[end])) : (end += 1) {}

    if (start >= end) return null;
    return source[start..end];
}

fn identifierPrefixBeforeOffset(source: []const u8, offset: usize) []const u8 {
    if (source.len == 0) return "";

    const clamped_offset = @min(offset, source.len);
    var start = clamped_offset;
    while (start > 0 and isIdentifierChar(source[start - 1])) : (start -= 1) {}
    return source[start..clamped_offset];
}

fn sliceStartsWithIgnoreCase(haystack: []const u8, prefix: []const u8) bool {
    if (prefix.len > haystack.len) return false;
    return std.ascii.eqlIgnoreCase(haystack[0..prefix.len], prefix);
}

fn hasCompletionLabel(items: []const CompletionItem, label: []const u8) bool {
    for (items) |item| {
        if (std.mem.eql(u8, item.label, label)) return true;
    }
    return false;
}

fn appendKeywordCompletions(
    allocator: std.mem.Allocator,
    items: *std.ArrayListUnmanaged(CompletionItem),
    source: []const u8,
    cursor_offset: usize,
) !void {
    const prefix = identifierPrefixBeforeOffset(source, cursor_offset);
    for (keyword_completion_specs) |spec| {
        if (prefix.len > 0 and !sliceStartsWithIgnoreCase(spec.keyword, prefix)) continue;
        if (hasCompletionLabel(items.items, spec.keyword)) continue;

        try items.append(allocator, .{
            .label = try allocator.dupe(u8, spec.keyword),
            .detail = try allocator.dupe(u8, spec.detail),
            .kind = 14, // CompletionItemKind.Keyword
            .sort_text = try std.fmt.allocPrint(allocator, "2_{s}", .{spec.keyword}),
        });
    }
}

fn keywordHoverDescription(keyword: []const u8) ?[]const u8 {
    if (std.mem.eql(u8, keyword, "async")) {
        return "Marks a function as asynchronous. Use with `fn` declarations.";
    }
    if (std.mem.eql(u8, keyword, "await")) {
        return "Waits for an async computation result. Valid only inside async functions.";
    }
    return null;
}

fn keywordHoverResultJson(allocator: std.mem.Allocator, keyword: []const u8, description: []const u8) ![]u8 {
    var out = std.ArrayListUnmanaged(u8){};
    errdefer out.deinit(allocator);
    const writer = out.writer(allocator);

    const value = try std.fmt.allocPrint(allocator, "`{s}`: `keyword`\n\n{s}", .{
        keyword,
        description,
    });
    defer allocator.free(value);

    try writer.writeAll("{\"contents\":{\"kind\":\"markdown\",\"value\":");
    try writeJsonString(writer, value);
    try writer.writeAll("}}");
    return out.toOwnedSlice(allocator);
}

fn findScopeEntryByName(scope_entries: []const checker_mod.ScopeEntry, name: []const u8) ?checker_mod.ScopeEntry {
    var i = scope_entries.len;
    while (i > 0) {
        i -= 1;
        const entry = scope_entries[i];
        if (std.mem.eql(u8, entry.name, name)) return entry;
    }
    return null;
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

    try appendKeywordCompletions(allocator, &items, source, cursor_offset);

    return items.toOwnedSlice(allocator);
}

fn hoverResultJson(
    allocator: std.mem.Allocator,
    name: []const u8,
    type_name: []const u8,
    kind: checker_mod.Symbol.Kind,
) ![]u8 {
    var out = std.ArrayListUnmanaged(u8){};
    errdefer out.deinit(allocator);
    const writer = out.writer(allocator);

    const value = try std.fmt.allocPrint(allocator, "`{s}`: `{s}` ({s})", .{
        name,
        type_name,
        symbolKindLabel(kind),
    });
    defer allocator.free(value);

    try writer.writeAll("{\"contents\":{\"kind\":\"markdown\",\"value\":");
    try writeJsonString(writer, value);
    try writer.writeAll("}}");
    return out.toOwnedSlice(allocator);
}

fn collectHoverFromSource(
    allocator: std.mem.Allocator,
    source: []const u8,
    line: usize,
    character: usize,
) !?[]u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var lexer = Lexer.init(source);
    var parser = Parser.init(arena.allocator(), &lexer, source);
    defer parser.deinit();

    const module = parser.parseModuleRecovering() catch return null;

    var checker = TypeChecker.init(allocator);
    defer checker.deinit();
    checker.setTestMode(true);
    checker.checkModule(module);

    const cursor_offset = sourceOffsetFromZeroBasedLineColumn(source, line, character) orelse return null;
    const ident = identifierAtOffset(source, cursor_offset) orelse return null;

    const scope_entries = checker.extractScopeAtOffset(module, cursor_offset) catch return null;
    defer allocator.free(scope_entries);

    if (findScopeEntryByName(scope_entries, ident)) |scope_entry| {
        const type_name = types.typeToString(allocator, scope_entry.type_) catch try allocator.dupe(u8, "unknown");
        defer allocator.free(type_name);
        return try hoverResultJson(allocator, scope_entry.name, type_name, scope_entry.kind);
    }

    if (keywordHoverDescription(ident)) |description| {
        return try keywordHoverResultJson(allocator, ident, description);
    }

    return null;
}

const OffsetRange = struct {
    start: usize,
    end: usize,
};

fn symbolNameOffsetRange(source: []const u8, symbol: checker_mod.ScopeEntry) ?OffsetRange {
    if (symbol.span.start == 0 and symbol.span.end == 0) return null;
    if (symbol.span.start >= source.len) return null;

    const span_end = @min(symbol.span.end, source.len);
    if (span_end <= symbol.span.start) return null;

    const span_slice = source[symbol.span.start..span_end];
    if (std.mem.indexOf(u8, span_slice, symbol.name)) |rel| {
        const start = symbol.span.start + rel;
        const end = start + symbol.name.len;
        if (end <= source.len) {
            return .{ .start = start, .end = end };
        }
    }

    return null;
}

fn definitionResultJson(
    allocator: std.mem.Allocator,
    uri: []const u8,
    source: []const u8,
    symbol: checker_mod.ScopeEntry,
) !?[]u8 {
    const name_range = symbolNameOffsetRange(source, symbol) orelse return null;
    const start = zeroBasedLineCharacterFromOffset(source, name_range.start) orelse return null;
    const end = zeroBasedLineCharacterFromOffset(source, name_range.end) orelse return null;

    var out = std.ArrayListUnmanaged(u8){};
    errdefer out.deinit(allocator);
    const writer = out.writer(allocator);

    try writer.writeAll("{\"uri\":");
    try writeJsonString(writer, uri);
    try writer.writeAll(",\"range\":{\"start\":{\"line\":");
    try writer.print("{d}", .{start.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{start.character});
    try writer.writeAll("},\"end\":{\"line\":");
    try writer.print("{d}", .{end.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{end.character});
    try writer.writeAll("}}}");

    return @as(?[]u8, try out.toOwnedSlice(allocator));
}

fn collectDefinitionFromSource(
    allocator: std.mem.Allocator,
    uri: []const u8,
    source: []const u8,
    line: usize,
    character: usize,
) !?[]u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var lexer = Lexer.init(source);
    var parser = Parser.init(arena.allocator(), &lexer, source);
    defer parser.deinit();

    const module = parser.parseModuleRecovering() catch return null;

    var checker = TypeChecker.init(allocator);
    defer checker.deinit();
    checker.setTestMode(true);
    checker.checkModule(module);

    const cursor_offset = sourceOffsetFromZeroBasedLineColumn(source, line, character) orelse return null;
    const ident = identifierAtOffset(source, cursor_offset) orelse return null;

    const scope_entries = checker.extractScopeAtOffset(module, cursor_offset) catch return null;
    defer allocator.free(scope_entries);

    const scope_entry = findScopeEntryByName(scope_entries, ident) orelse return null;
    return try definitionResultJson(allocator, uri, source, scope_entry);
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
    } else readSourceFile(allocator, path, state.workspace_root) catch |err| {
        const msg = if (err == error.AccessDenied) "Access denied: path outside workspace" else "Failed to read source file";
        try sendError(allocator, out, id, -32603, msg);
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

fn handleHoverRequest(
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
    if (state.getHoverCache(req.uri)) |cached| {
        if (cached.generation == doc_generation and cached.line == req.line and cached.character == req.character) {
            try sendResult(allocator, out, id, cached.result_json);
            return;
        }
    }

    const source = if (state.documents.get(req.uri)) |doc| blk: {
        break :blk try allocator.dupe(u8, doc.text);
    } else readSourceFile(allocator, path, state.workspace_root) catch |err| {
        const msg = if (err == error.AccessDenied) "Access denied: path outside workspace" else "Failed to read source file";
        try sendError(allocator, out, id, -32603, msg);
        return;
    };
    defer allocator.free(source);

    const hover_result_opt = collectHoverFromSource(allocator, source, req.line, req.character) catch {
        try sendError(allocator, out, id, -32603, "Failed to compute hover");
        return;
    };

    const result_json = if (hover_result_opt) |hover_result| hover_result else try allocator.dupe(u8, "null");
    defer allocator.free(result_json);

    state.upsertHoverCache(allocator, req.uri, .{
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
    } else readSourceFile(allocator, path, state.workspace_root) catch |err| {
        const msg = if (err == error.AccessDenied) "Access denied: path outside workspace" else "Failed to read source file";
        try sendError(allocator, out, id, -32603, msg);
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

fn handleDefinitionRequest(
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
    if (state.getDefinitionCache(req.uri)) |cached| {
        if (cached.generation == doc_generation and cached.line == req.line and cached.character == req.character) {
            try sendResult(allocator, out, id, cached.result_json);
            return;
        }
    }

    const source = if (state.documents.get(req.uri)) |doc| blk: {
        break :blk try allocator.dupe(u8, doc.text);
    } else readSourceFile(allocator, path, state.workspace_root) catch |err| {
        const msg = if (err == error.AccessDenied) "Access denied: path outside workspace" else "Failed to read source file";
        try sendError(allocator, out, id, -32603, msg);
        return;
    };
    defer allocator.free(source);

    const definition_result_opt = collectDefinitionFromSource(allocator, req.uri, source, req.line, req.character) catch {
        try sendError(allocator, out, id, -32603, "Failed to compute definition");
        return;
    };

    const result_json = if (definition_result_opt) |definition_result| definition_result else try allocator.dupe(u8, "null");
    defer allocator.free(result_json);

    state.upsertDefinitionCache(allocator, req.uri, .{
        .generation = doc_generation,
        .line = req.line,
        .character = req.character,
        .result_json = result_json,
    }) catch {};

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
        // Extract workspace root from rootUri for path validation
        if (params_opt) |params| {
            if (params == .object) {
                if (params.object.get("rootUri")) |root_uri_val| {
                    if (root_uri_val == .string) {
                        if (uriToPath(allocator, root_uri_val.string) catch null) |root_path| {
                            const resolved = std.fs.cwd().realpathAlloc(allocator, root_path) catch null;
                            if (state.workspace_root) |old| allocator.free(old);
                            if (resolved) |r| {
                                allocator.free(root_path);
                                state.workspace_root = r;
                            } else {
                                state.workspace_root = root_path;
                            }
                        }
                    }
                }
            }
        }
        if (id_opt) |id| {
            try sendResult(
                allocator,
                out,
                id,
                "{\"capabilities\":{\"textDocumentSync\":{\"openClose\":true,\"change\":1},\"diagnosticProvider\":{\"interFileDependencies\":false,\"workspaceDiagnostics\":false},\"completionProvider\":{\"resolveProvider\":false},\"hoverProvider\":true,\"definitionProvider\":true},\"serverInfo\":{\"name\":\"klar-lsp\",\"version\":\"" ++ version.display ++ "\"}}",
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

    if (std.mem.eql(u8, method, "textDocument/hover")) {
        if (id_opt) |id| {
            try handleHoverRequest(allocator, state, out, id, params_opt);
        }
        return .continue_running;
    }

    if (std.mem.eql(u8, method, "textDocument/definition")) {
        if (id_opt) |id| {
            try handleDefinitionRequest(allocator, state, out, id, params_opt);
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
    const stdin_file: std.fs.File = if (comptime builtin.os.tag == .windows)
        .{ .handle = std.os.windows.kernel32.GetStdHandle(std.os.windows.STD_INPUT_HANDLE).? }
    else
        .{ .handle = std.posix.STDIN_FILENO };
    const stdout_file: std.fs.File = if (comptime builtin.os.tag == .windows)
        .{ .handle = std.os.windows.kernel32.GetStdHandle(std.os.windows.STD_OUTPUT_HANDLE).? }
    else
        .{ .handle = std.posix.STDOUT_FILENO };

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
