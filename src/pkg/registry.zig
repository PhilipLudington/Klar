//! Package registry client for fetching and publishing packages.
//!
//! Uses raw TCP + HTTP/1.1 to communicate with a Klar package registry.
//! Registry URL is read from KLAR_REGISTRY env var (default: http://localhost:8080).

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Registry-specific errors.
pub const RegistryError = error{
    ConnectionFailed,
    InvalidResponse,
    PackageNotFound,
    PublishFailed,
    VersionNotFound,
    InvalidUrl,
    ReadFailed,
};

/// Parsed HTTP URL components.
const UrlComponents = struct {
    host: []const u8,
    port: u16,
    path: []const u8,
};

/// HTTP response from the registry.
pub const HttpResponse = struct {
    status: u16,
    body: []const u8,
};

/// Package metadata returned by GET /packages/{name}.
pub const PackageMetadata = struct {
    allocator: Allocator,
    name: []const u8,
    versions: []const []const u8,
    latest: []const u8,

    pub fn deinit(self: *PackageMetadata) void {
        self.allocator.free(self.name);
        for (self.versions) |v| self.allocator.free(v);
        self.allocator.free(self.versions);
        self.allocator.free(self.latest);
    }
};

/// Package archive with file contents.
pub const PackageArchive = struct {
    allocator: Allocator,
    name: []const u8,
    version: []const u8,
    file_paths: []const []const u8,
    file_contents: []const []const u8,

    pub fn deinit(self: *PackageArchive) void {
        self.allocator.free(self.name);
        self.allocator.free(self.version);
        for (self.file_paths) |p| self.allocator.free(p);
        self.allocator.free(self.file_paths);
        for (self.file_contents) |c| self.allocator.free(c);
        self.allocator.free(self.file_contents);
    }
};

/// Get the registry URL from KLAR_REGISTRY env var or use default.
pub fn getRegistryUrl(allocator: Allocator) []const u8 {
    return std.process.getEnvVarOwned(allocator, "KLAR_REGISTRY") catch {
        return "http://localhost:8080";
    };
}

/// Parse an HTTP URL into host, port, path components.
fn parseUrl(url: []const u8) RegistryError!UrlComponents {
    // Reject https
    if (url.len > 8 and std.mem.eql(u8, url[0..8], "https://")) {
        return RegistryError.InvalidUrl;
    }

    var start: usize = 0;
    if (url.len > 7 and std.mem.eql(u8, url[0..7], "http://")) {
        start = 7;
    }

    // Find path separator
    const slash_pos = std.mem.indexOfScalarPos(u8, url, start, '/');
    const host_port = if (slash_pos) |pos| url[start..pos] else url[start..];
    const path = if (slash_pos) |pos| url[pos..] else "/";

    // Split host:port
    const colon_pos = std.mem.indexOfScalar(u8, host_port, ':');
    const host = if (colon_pos) |pos| host_port[0..pos] else host_port;
    const port: u16 = if (colon_pos) |pos| blk: {
        break :blk std.fmt.parseInt(u16, host_port[pos + 1 ..], 10) catch 80;
    } else 80;

    if (host.len == 0) return RegistryError.InvalidUrl;

    return .{ .host = host, .port = port, .path = path };
}

/// Make an HTTP GET request and return the response.
pub fn httpGet(allocator: Allocator, url: []const u8) !HttpResponse {
    const components = try parseUrl(url);

    const stream = std.net.tcpConnectToHost(allocator, components.host, components.port) catch {
        return RegistryError.ConnectionFailed;
    };
    defer stream.close();

    // Build request
    var request_buf = std.ArrayListUnmanaged(u8){};
    defer request_buf.deinit(allocator);
    const writer = request_buf.writer(allocator);

    try writer.print("GET {s} HTTP/1.1\r\nHost: {s}\r\nConnection: close\r\n\r\n", .{ components.path, components.host });

    stream.writeAll(request_buf.items) catch return RegistryError.ConnectionFailed;

    // Read response
    return readHttpResponse(allocator, stream);
}

/// Make an HTTP POST request with body and return the response.
pub fn httpPost(allocator: Allocator, url: []const u8, body: []const u8) !HttpResponse {
    const components = try parseUrl(url);

    const stream = std.net.tcpConnectToHost(allocator, components.host, components.port) catch {
        return RegistryError.ConnectionFailed;
    };
    defer stream.close();

    // Build request
    var request_buf = std.ArrayListUnmanaged(u8){};
    defer request_buf.deinit(allocator);
    const writer = request_buf.writer(allocator);

    try writer.print("POST {s} HTTP/1.1\r\nHost: {s}\r\nContent-Type: application/json\r\nContent-Length: {d}\r\nConnection: close\r\n\r\n", .{ components.path, components.host, body.len });
    try writer.writeAll(body);

    stream.writeAll(request_buf.items) catch return RegistryError.ConnectionFailed;

    // Read response
    return readHttpResponse(allocator, stream);
}

/// Read and parse an HTTP response from a stream.
fn readHttpResponse(allocator: Allocator, stream: std.net.Stream) !HttpResponse {
    var response_buf = std.ArrayListUnmanaged(u8){};
    defer response_buf.deinit(allocator);

    // Read until connection closes or Content-Length is satisfied
    var read_buf: [65536]u8 = undefined;
    for (0..10000) |_| {
        const n = stream.read(&read_buf) catch break;
        if (n == 0) break;
        try response_buf.appendSlice(allocator, read_buf[0..n]);

        // Check if we have full response based on Content-Length
        if (checkResponseComplete(response_buf.items)) break;
    }

    if (response_buf.items.len == 0) return RegistryError.ReadFailed;

    // Parse status code
    const status = parseStatusCode(response_buf.items) orelse return RegistryError.InvalidResponse;

    // Find body
    const header_end = std.mem.indexOf(u8, response_buf.items, "\r\n\r\n") orelse return RegistryError.InvalidResponse;
    const body_start = header_end + 4;

    const body = try allocator.dupe(u8, response_buf.items[body_start..]);

    return .{ .status = status, .body = body };
}

/// Check if an HTTP response is complete (headers received and Content-Length body fully read).
fn checkResponseComplete(data: []const u8) bool {
    const header_end = std.mem.indexOf(u8, data, "\r\n\r\n") orelse return false;
    const body_start = header_end + 4;

    // Look for Content-Length header
    const cl_marker = "Content-Length: ";
    const cl_pos = std.mem.indexOf(u8, data[0..header_end], cl_marker) orelse {
        // Also try lowercase
        const cl_lower = "content-length: ";
        const cl_lower_pos = std.mem.indexOf(u8, data[0..header_end], cl_lower) orelse return true;
        return checkContentLengthComplete(data, cl_lower_pos + cl_lower.len, body_start);
    };
    return checkContentLengthComplete(data, cl_pos + cl_marker.len, body_start);
}

fn checkContentLengthComplete(data: []const u8, cl_val_start: usize, body_start: usize) bool {
    const cl_end = std.mem.indexOfPos(u8, data, cl_val_start, "\r\n") orelse return true;
    const cl_str = data[cl_val_start..cl_end];
    const content_length = std.fmt.parseInt(usize, cl_str, 10) catch return true;
    return data.len >= body_start + content_length;
}

/// Parse HTTP status code from response first line.
fn parseStatusCode(data: []const u8) ?u16 {
    // "HTTP/1.1 200 OK\r\n"
    const sp1 = std.mem.indexOfScalar(u8, data, ' ') orelse return null;
    const rest = data[sp1 + 1 ..];
    const sp2 = std.mem.indexOfScalar(u8, rest, ' ') orelse {
        const cr = std.mem.indexOfScalar(u8, rest, '\r') orelse return null;
        return std.fmt.parseInt(u16, rest[0..cr], 10) catch null;
    };
    return std.fmt.parseInt(u16, rest[0..sp2], 10) catch null;
}

/// Fetch package metadata from the registry.
pub fn fetchPackageMetadata(allocator: Allocator, registry_url: []const u8, name: []const u8) !PackageMetadata {
    // Build URL: {registry}/packages/{name}
    const url = try std.fmt.allocPrint(allocator, "{s}/packages/{s}", .{ registry_url, name });
    defer allocator.free(url);

    const response = try httpGet(allocator, url);
    defer allocator.free(response.body);

    if (response.status == 404) return RegistryError.PackageNotFound;
    if (response.status != 200) return RegistryError.InvalidResponse;

    // Parse JSON: {"name":"...","versions":["..."],"latest":"..."}
    const parsed = std.json.parseFromSlice(std.json.Value, allocator, response.body, .{}) catch {
        return RegistryError.InvalidResponse;
    };
    defer parsed.deinit();

    const root = parsed.value;
    if (root != .object) return RegistryError.InvalidResponse;

    const pkg_name = switch (root.object.get("name") orelse return RegistryError.InvalidResponse) {
        .string => |s| try allocator.dupe(u8, s),
        else => return RegistryError.InvalidResponse,
    };
    errdefer allocator.free(pkg_name);

    const latest = switch (root.object.get("latest") orelse return RegistryError.InvalidResponse) {
        .string => |s| try allocator.dupe(u8, s),
        else => return RegistryError.InvalidResponse,
    };
    errdefer allocator.free(latest);

    var versions = std.ArrayListUnmanaged([]const u8){};
    errdefer {
        for (versions.items) |v| allocator.free(v);
        versions.deinit(allocator);
    }

    const versions_val = root.object.get("versions") orelse return RegistryError.InvalidResponse;
    if (versions_val != .array) return RegistryError.InvalidResponse;

    for (versions_val.array.items) |item| {
        if (item == .string) {
            try versions.append(allocator, try allocator.dupe(u8, item.string));
        }
    }

    return .{
        .allocator = allocator,
        .name = pkg_name,
        .versions = try versions.toOwnedSlice(allocator),
        .latest = latest,
    };
}

/// Fetch a package archive from the registry.
pub fn fetchPackage(allocator: Allocator, registry_url: []const u8, name: []const u8, version: []const u8) !PackageArchive {
    const url = try std.fmt.allocPrint(allocator, "{s}/packages/{s}/{s}", .{ registry_url, name, version });
    defer allocator.free(url);

    const response = try httpGet(allocator, url);
    defer allocator.free(response.body);

    if (response.status == 404) return RegistryError.VersionNotFound;
    if (response.status != 200) return RegistryError.InvalidResponse;

    return parsePackageArchive(allocator, response.body);
}

/// Parse a package archive JSON into a PackageArchive struct.
pub fn parsePackageArchive(allocator: Allocator, json_data: []const u8) !PackageArchive {
    const parsed = std.json.parseFromSlice(std.json.Value, allocator, json_data, .{}) catch {
        return RegistryError.InvalidResponse;
    };
    defer parsed.deinit();

    const root = parsed.value;
    if (root != .object) return RegistryError.InvalidResponse;

    const name = switch (root.object.get("name") orelse return RegistryError.InvalidResponse) {
        .string => |s| try allocator.dupe(u8, s),
        else => return RegistryError.InvalidResponse,
    };
    errdefer allocator.free(name);

    const version = switch (root.object.get("version") orelse return RegistryError.InvalidResponse) {
        .string => |s| try allocator.dupe(u8, s),
        else => return RegistryError.InvalidResponse,
    };
    errdefer allocator.free(version);

    const files_val = root.object.get("files") orelse return RegistryError.InvalidResponse;
    if (files_val != .object) return RegistryError.InvalidResponse;

    var paths = std.ArrayListUnmanaged([]const u8){};
    errdefer {
        for (paths.items) |p| allocator.free(p);
        paths.deinit(allocator);
    }

    var contents = std.ArrayListUnmanaged([]const u8){};
    errdefer {
        for (contents.items) |c| allocator.free(c);
        contents.deinit(allocator);
    }

    var it = files_val.object.iterator();
    while (it.next()) |entry| {
        try paths.append(allocator, try allocator.dupe(u8, entry.key_ptr.*));
        const content = switch (entry.value_ptr.*) {
            .string => |s| try allocator.dupe(u8, s),
            else => try allocator.dupe(u8, ""),
        };
        try contents.append(allocator, content);
    }

    return .{
        .allocator = allocator,
        .name = name,
        .version = version,
        .file_paths = try paths.toOwnedSlice(allocator),
        .file_contents = try contents.toOwnedSlice(allocator),
    };
}

/// Publish a package archive to the registry.
pub fn publishPackage(allocator: Allocator, registry_url: []const u8, archive_json: []const u8) !void {
    const url = try std.fmt.allocPrint(allocator, "{s}/publish", .{registry_url});
    defer allocator.free(url);

    const response = try httpPost(allocator, url, archive_json);
    defer allocator.free(response.body);

    if (response.status == 409) return RegistryError.PublishFailed; // Version conflict
    if (response.status != 200 and response.status != 201) return RegistryError.PublishFailed;
}

/// Build a package archive JSON from file paths and contents.
pub fn buildArchiveJson(
    allocator: Allocator,
    name: []const u8,
    version: []const u8,
    file_paths: []const []const u8,
    file_contents: []const []const u8,
) ![]const u8 {
    var buffer = std.ArrayListUnmanaged(u8){};
    errdefer buffer.deinit(allocator);
    const writer = buffer.writer(allocator);

    try writer.writeAll("{\"name\":\"");
    try writeJsonEscaped(writer, name);
    try writer.writeAll("\",\"version\":\"");
    try writeJsonEscaped(writer, version);
    try writer.writeAll("\",\"files\":{");

    for (file_paths, 0..) |path, i| {
        if (i > 0) try writer.writeByte(',');
        try writer.writeByte('"');
        try writeJsonEscaped(writer, path);
        try writer.writeAll("\":\"");
        try writeJsonEscaped(writer, file_contents[i]);
        try writer.writeByte('"');
    }

    try writer.writeAll("}}");
    return buffer.toOwnedSlice(allocator);
}

/// Write a string with JSON escaping.
fn writeJsonEscaped(writer: anytype, str: []const u8) !void {
    for (str) |c| {
        switch (c) {
            '\\' => try writer.writeAll("\\\\"),
            '"' => try writer.writeAll("\\\""),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            0x00...0x08, 0x0B, 0x0C, 0x0E...0x1F => {
                try writer.print("\\u{x:0>4}", .{c});
            },
            else => try writer.writeByte(c),
        }
    }
}

// =============================================================================
// Tests
// =============================================================================

test "parseUrl basic" {
    const result = try parseUrl("http://localhost:8080/packages/foo");
    try std.testing.expectEqualStrings("localhost", result.host);
    try std.testing.expectEqual(@as(u16, 8080), result.port);
    try std.testing.expectEqualStrings("/packages/foo", result.path);
}

test "parseUrl default port" {
    const result = try parseUrl("http://example.com/api");
    try std.testing.expectEqualStrings("example.com", result.host);
    try std.testing.expectEqual(@as(u16, 80), result.port);
    try std.testing.expectEqualStrings("/api", result.path);
}

test "parseUrl no path" {
    const result = try parseUrl("http://localhost:9090");
    try std.testing.expectEqualStrings("localhost", result.host);
    try std.testing.expectEqual(@as(u16, 9090), result.port);
    try std.testing.expectEqualStrings("/", result.path);
}

test "parseUrl rejects https" {
    const result = parseUrl("https://example.com/api");
    try std.testing.expectError(RegistryError.InvalidUrl, result);
}

test "parseStatusCode" {
    try std.testing.expectEqual(@as(?u16, 200), parseStatusCode("HTTP/1.1 200 OK\r\n"));
    try std.testing.expectEqual(@as(?u16, 404), parseStatusCode("HTTP/1.1 404 Not Found\r\n"));
    try std.testing.expectEqual(@as(?u16, 201), parseStatusCode("HTTP/1.1 201 Created\r\n"));
}

test "buildArchiveJson" {
    const paths = &[_][]const u8{ "klar.json", "src/lib.kl" };
    const contents = &[_][]const u8{ "{\"package\":{\"name\":\"test\"}}", "pub fn hello() -> string { return \"hi\" }" };

    const json = try buildArchiveJson(std.testing.allocator, "test-pkg", "1.0.0", paths, contents);
    defer std.testing.allocator.free(json);

    // Verify it's valid JSON by parsing it
    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, json, .{});
    defer parsed.deinit();

    const root = parsed.value;
    try std.testing.expectEqualStrings("test-pkg", root.object.get("name").?.string);
    try std.testing.expectEqualStrings("1.0.0", root.object.get("version").?.string);

    const files = root.object.get("files").?.object;
    try std.testing.expectEqual(@as(usize, 2), files.count());
}

test "parsePackageArchive" {
    const json =
        \\{"name":"mylib","version":"0.2.0","files":{"klar.json":"{\"package\":{\"name\":\"mylib\"}}","src/lib.kl":"pub fn greet() -> string { return \"hello\" }"}}
    ;

    var archive = try parsePackageArchive(std.testing.allocator, json);
    defer archive.deinit();

    try std.testing.expectEqualStrings("mylib", archive.name);
    try std.testing.expectEqualStrings("0.2.0", archive.version);
    try std.testing.expectEqual(@as(usize, 2), archive.file_paths.len);
}

test "buildArchiveJson roundtrip" {
    const paths = &[_][]const u8{"src/main.kl"};
    const contents = &[_][]const u8{"fn main() -> i32 { return 0 }"};

    const json = try buildArchiveJson(std.testing.allocator, "roundtrip", "0.1.0", paths, contents);
    defer std.testing.allocator.free(json);

    var archive = try parsePackageArchive(std.testing.allocator, json);
    defer archive.deinit();

    try std.testing.expectEqualStrings("roundtrip", archive.name);
    try std.testing.expectEqualStrings("0.1.0", archive.version);
    try std.testing.expectEqual(@as(usize, 1), archive.file_paths.len);
    try std.testing.expectEqualStrings("src/main.kl", archive.file_paths[0]);
    try std.testing.expectEqualStrings("fn main() -> i32 { return 0 }", archive.file_contents[0]);
}

test "checkResponseComplete with content-length" {
    try std.testing.expect(checkResponseComplete("HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nhello"));
    try std.testing.expect(!checkResponseComplete("HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\nhello"));
    try std.testing.expect(!checkResponseComplete("HTTP/1.1 200 OK\r\n")); // no header end
}

test "checkResponseComplete without content-length" {
    try std.testing.expect(checkResponseComplete("HTTP/1.1 200 OK\r\n\r\nhello"));
}
