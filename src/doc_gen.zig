//! HTML documentation generator for Klar source files.
//!
//! Parses `///` doc comments attached to `pub` declarations and generates
//! per-module HTML pages with an index. Does not use the compiler pipeline —
//! reads source files directly.

const std = @import("std");
const compat = @import("compat.zig");
const Allocator = std.mem.Allocator;

/// A documented item (function, struct, enum, or trait).
pub const DocItem = struct {
    kind: Kind,
    name: []const u8,
    signature: []const u8,
    doc_comment: []const u8,
    line: usize,

    pub const Kind = enum {
        function,
        struct_decl,
        enum_decl,
        trait_decl,
        constant,
        type_alias,
    };

    pub fn kindName(self: *const DocItem) []const u8 {
        return switch (self.kind) {
            .function => "function",
            .struct_decl => "struct",
            .enum_decl => "enum",
            .trait_decl => "trait",
            .constant => "constant",
            .type_alias => "type",
        };
    }
};

/// A documented module (one source file).
pub const DocModule = struct {
    name: []const u8,
    path: []const u8,
    items: []const DocItem,
    module_doc: []const u8,
};

/// Extract doc items from a Klar source file.
pub fn extractDocs(allocator: Allocator, source: []const u8, module_name: []const u8, file_path: []const u8) !DocModule {
    var items = std.ArrayListUnmanaged(DocItem).empty;
    defer items.deinit(allocator);

    var lines_iter = std.mem.splitScalar(u8, source, '\n');
    var pending_doc = std.ArrayListUnmanaged(u8).empty;
    defer pending_doc.deinit(allocator);

    var module_doc = std.ArrayListUnmanaged(u8).empty;
    defer module_doc.deinit(allocator);

    var line_num: usize = 0;
    var seen_non_doc = false;

    while (lines_iter.next()) |line| {
        line_num += 1;
        const trimmed = std.mem.trim(u8, line, " \t\r");

        // Module-level doc comment (//!)
        if (std.mem.startsWith(u8, trimmed, "//!")) {
            const doc_text = if (trimmed.len > 3 and trimmed[3] == ' ')
                trimmed[4..]
            else
                trimmed[3..];
            if (module_doc.items.len > 0) try module_doc.append(allocator, '\n');
            try module_doc.appendSlice(allocator, doc_text);
            continue;
        }

        // Item doc comment (///)
        if (std.mem.startsWith(u8, trimmed, "///")) {
            const doc_text = if (trimmed.len > 3 and trimmed[3] == ' ')
                trimmed[4..]
            else
                trimmed[3..];
            if (pending_doc.items.len > 0) try pending_doc.append(allocator, '\n');
            try pending_doc.appendSlice(allocator, doc_text);
            continue;
        }

        // Skip blank lines (preserve pending doc comments)
        if (trimmed.len == 0) continue;

        // Skip regular comments (not ///)
        if (std.mem.startsWith(u8, trimmed, "//")) {
            // Discard pending docs — regular comments break the association
            discardPendingDoc(allocator, &pending_doc, &module_doc, seen_non_doc, items.items.len);
            continue;
        }

        // Skip imports
        if (std.mem.startsWith(u8, trimmed, "import ")) {
            discardPendingDoc(allocator, &pending_doc, &module_doc, seen_non_doc, items.items.len);
            continue;
        }

        // Check for pub declarations — these consume pending doc comments
        if (std.mem.startsWith(u8, trimmed, "pub ")) {
            const after_pub = trimmed[4..];

            const item = if (std.mem.startsWith(u8, after_pub, "fn "))
                extractFunction(allocator, trimmed, line_num)
            else if (std.mem.startsWith(u8, after_pub, "struct "))
                extractStructOrEnum(allocator, trimmed, .struct_decl, line_num)
            else if (std.mem.startsWith(u8, after_pub, "enum "))
                extractStructOrEnum(allocator, trimmed, .enum_decl, line_num)
            else if (std.mem.startsWith(u8, after_pub, "trait "))
                extractStructOrEnum(allocator, trimmed, .trait_decl, line_num)
            else if (std.mem.startsWith(u8, after_pub, "let ") or std.mem.startsWith(u8, after_pub, "const "))
                extractConstant(allocator, trimmed, line_num)
            else if (std.mem.startsWith(u8, after_pub, "type "))
                extractTypeAlias(allocator, trimmed, line_num)
            else
                null;

            if (item) |it| {
                var doc_item = it catch {
                    pending_doc.clearRetainingCapacity();
                    seen_non_doc = true;
                    continue;
                };
                doc_item.doc_comment = if (pending_doc.items.len > 0)
                    try allocator.dupe(u8, pending_doc.items)
                else
                    "";
                try items.append(allocator, doc_item);
            }
            pending_doc.clearRetainingCapacity();
            seen_non_doc = true;
            continue;
        }

        // Non-pub, non-import, non-comment line — discard pending docs
        discardPendingDoc(allocator, &pending_doc, &module_doc, seen_non_doc, items.items.len);
        seen_non_doc = true;
    }

    return .{
        .name = try allocator.dupe(u8, module_name),
        .path = try allocator.dupe(u8, file_path),
        .items = try allocator.dupe(DocItem, items.items),
        .module_doc = if (module_doc.items.len > 0) try allocator.dupe(u8, module_doc.items) else "",
    };
}

/// If pending doc comments exist but aren't consumed by a pub declaration,
/// they might be module-level docs (if they appeared before any declarations).
fn discardPendingDoc(
    allocator: Allocator,
    pending_doc: *std.ArrayListUnmanaged(u8),
    module_doc: *std.ArrayListUnmanaged(u8),
    seen_non_doc: bool,
    item_count: usize,
) void {
    if (pending_doc.items.len > 0) {
        if (!seen_non_doc and item_count == 0 and module_doc.items.len == 0) {
            module_doc.appendSlice(allocator, pending_doc.items) catch {};
        }
        pending_doc.clearRetainingCapacity();
    }
}

fn extractFunction(allocator: Allocator, line: []const u8, line_num: usize) ?Allocator.Error!DocItem {
    // "pub fn name(args) -> ret {"  or  "pub fn name#[T](args) -> ret {"
    const fn_start = std.mem.indexOf(u8, line, "fn ") orelse return null;
    const after_fn = line[fn_start + 3 ..];

    // Find the name end (space, (, #, or {)
    var name_end: usize = 0;
    for (after_fn, 0..) |c, i| {
        if (c == '(' or c == '#' or c == '{' or c == ' ') {
            name_end = i;
            break;
        }
    } else {
        name_end = after_fn.len;
    }

    if (name_end == 0) return null;
    const name = after_fn[0..name_end];

    // Signature is everything up to the opening brace
    const brace_pos = std.mem.indexOfScalar(u8, line, '{');
    const sig = if (brace_pos) |pos|
        std.mem.trim(u8, line[0..pos], " \t")
    else
        std.mem.trim(u8, line, " \t");

    return DocItem{
        .kind = .function,
        .name = try allocator.dupe(u8, name),
        .signature = try allocator.dupe(u8, sig),
        .doc_comment = "",
        .line = line_num,
    };
}

fn extractStructOrEnum(allocator: Allocator, line: []const u8, kind: DocItem.Kind, line_num: usize) ?Allocator.Error!DocItem {
    const keyword = switch (kind) {
        .struct_decl => "struct ",
        .enum_decl => "enum ",
        .trait_decl => "trait ",
        else => return null,
    };

    const kw_start = std.mem.indexOf(u8, line, keyword) orelse return null;
    const after_kw = line[kw_start + keyword.len ..];

    // Name ends at space, {, or #
    var name_end: usize = 0;
    for (after_kw, 0..) |c, i| {
        if (c == ' ' or c == '{' or c == '#') {
            name_end = i;
            break;
        }
    } else {
        name_end = after_kw.len;
    }

    if (name_end == 0) return null;
    const name = after_kw[0..name_end];

    const brace_pos = std.mem.indexOfScalar(u8, line, '{');
    const sig = if (brace_pos) |pos|
        std.mem.trim(u8, line[0..pos], " \t")
    else
        std.mem.trim(u8, line, " \t");

    return DocItem{
        .kind = kind,
        .name = try allocator.dupe(u8, name),
        .signature = try allocator.dupe(u8, sig),
        .doc_comment = "",
        .line = line_num,
    };
}

fn extractConstant(allocator: Allocator, line: []const u8, line_num: usize) ?Allocator.Error!DocItem {
    const let_start = std.mem.indexOf(u8, line, "let ") orelse
        (std.mem.indexOf(u8, line, "const ") orelse return null);
    const is_const = std.mem.indexOf(u8, line, "const ") != null;
    const after_kw = if (is_const) line[let_start + 6 ..] else line[let_start + 4 ..];

    var name_end: usize = 0;
    for (after_kw, 0..) |c, i| {
        if (c == ':' or c == ' ' or c == '=') {
            name_end = i;
            break;
        }
    } else {
        name_end = after_kw.len;
    }

    if (name_end == 0) return null;
    const name = after_kw[0..name_end];

    return DocItem{
        .kind = .constant,
        .name = try allocator.dupe(u8, name),
        .signature = try allocator.dupe(u8, std.mem.trim(u8, line, " \t")),
        .doc_comment = "",
        .line = line_num,
    };
}

fn extractTypeAlias(allocator: Allocator, line: []const u8, line_num: usize) ?Allocator.Error!DocItem {
    const type_start = std.mem.indexOf(u8, line, "type ") orelse return null;
    const after_type = line[type_start + 5 ..];

    var name_end: usize = 0;
    for (after_type, 0..) |c, i| {
        if (c == ' ' or c == '=' or c == '#') {
            name_end = i;
            break;
        }
    } else {
        name_end = after_type.len;
    }

    if (name_end == 0) return null;
    const name = after_type[0..name_end];

    return DocItem{
        .kind = .type_alias,
        .name = try allocator.dupe(u8, name),
        .signature = try allocator.dupe(u8, std.mem.trim(u8, line, " \t")),
        .doc_comment = "",
        .line = line_num,
    };
}

/// Generate HTML for a single module.
pub fn generateModuleHtml(allocator: Allocator, module: *const DocModule) ![]const u8 {
    var buf = std.ArrayListUnmanaged(u8).empty;
    defer buf.deinit(allocator);
    const w = compat.listWriter(&buf, allocator);

    try w.print(
        \\<!DOCTYPE html>
        \\<html lang="en">
        \\<head>
        \\<meta charset="UTF-8">
        \\<meta name="viewport" content="width=device-width, initial-scale=1.0">
        \\<title>{s} — Klar Documentation</title>
        \\<style>
        \\body {{ font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; max-width: 900px; margin: 0 auto; padding: 2rem; color: #1a1a1a; }}
        \\h1 {{ border-bottom: 2px solid #0066cc; padding-bottom: 0.5rem; }}
        \\h2 {{ color: #0066cc; margin-top: 2rem; }}
        \\.item {{ background: #f5f7fa; border: 1px solid #e1e4e8; border-radius: 6px; padding: 1rem; margin: 1rem 0; }}
        \\.signature {{ font-family: 'JetBrains Mono', 'Fira Code', monospace; font-size: 0.9rem; background: #1e1e2e; color: #cdd6f4; padding: 0.75rem 1rem; border-radius: 4px; overflow-x: auto; }}
        \\.doc {{ margin-top: 0.5rem; line-height: 1.6; }}
        \\.kind {{ display: inline-block; background: #0066cc; color: white; padding: 0.15rem 0.5rem; border-radius: 3px; font-size: 0.75rem; font-weight: 600; text-transform: uppercase; margin-right: 0.5rem; }}
        \\.kind-struct {{ background: #7c3aed; }}
        \\.kind-enum {{ background: #059669; }}
        \\.kind-trait {{ background: #d97706; }}
        \\.kind-constant {{ background: #6b7280; }}
        \\.kind-type {{ background: #dc2626; }}
        \\nav {{ background: #f5f7fa; padding: 1rem; border-radius: 6px; margin-bottom: 2rem; }}
        \\nav a {{ color: #0066cc; text-decoration: none; margin-right: 1rem; }}
        \\nav a:hover {{ text-decoration: underline; }}
        \\.module-doc {{ margin: 1rem 0 2rem 0; line-height: 1.6; color: #374151; }}
        \\</style>
        \\</head>
        \\<body>
        \\<nav><a href="index.html">Index</a></nav>
        \\<h1>{s}</h1>
        \\<p><code>{s}</code></p>
        \\
    , .{ module.name, module.name, module.path });

    // Module doc comment
    if (module.module_doc.len > 0) {
        try w.writeAll("<div class=\"module-doc\">");
        try writeHtmlEscaped(w, module.module_doc);
        try w.writeAll("</div>\n");
    }

    // Group items by kind
    const sections = [_]struct { kind: DocItem.Kind, title: []const u8 }{
        .{ .kind = .function, .title = "Functions" },
        .{ .kind = .struct_decl, .title = "Structs" },
        .{ .kind = .enum_decl, .title = "Enums" },
        .{ .kind = .trait_decl, .title = "Traits" },
        .{ .kind = .type_alias, .title = "Types" },
        .{ .kind = .constant, .title = "Constants" },
    };

    for (sections) |section| {
        var count: usize = 0;
        for (module.items) |item| {
            if (item.kind == section.kind) count += 1;
        }
        if (count == 0) continue;

        try w.print("<h2>{s}</h2>\n", .{section.title});

        for (module.items) |item| {
            if (item.kind != section.kind) continue;

            const kind_class = switch (item.kind) {
                .struct_decl => " kind-struct",
                .enum_decl => " kind-enum",
                .trait_decl => " kind-trait",
                .constant => " kind-constant",
                .type_alias => " kind-type",
                else => "",
            };

            try w.print("<div class=\"item\" id=\"{s}\">\n", .{item.name});
            try w.print("<span class=\"kind{s}\">{s}</span> <strong>{s}</strong>\n", .{ kind_class, item.kindName(), item.name });
            try w.writeAll("<pre class=\"signature\">");
            try writeHtmlEscaped(w, item.signature);
            try w.writeAll("</pre>\n");

            if (item.doc_comment.len > 0) {
                try w.writeAll("<div class=\"doc\">");
                try writeDocCommentHtml(w, item.doc_comment);
                try w.writeAll("</div>\n");
            }

            try w.writeAll("</div>\n");
        }
    }

    try w.writeAll("</body>\n</html>\n");
    return buf.toOwnedSlice(allocator);
}

/// Generate the index HTML page listing all modules.
pub fn generateIndexHtml(allocator: Allocator, modules: []const DocModule) ![]const u8 {
    var buf = std.ArrayListUnmanaged(u8).empty;
    defer buf.deinit(allocator);
    const w = compat.listWriter(&buf, allocator);

    try w.writeAll(
        \\<!DOCTYPE html>
        \\<html lang="en">
        \\<head>
        \\<meta charset="UTF-8">
        \\<meta name="viewport" content="width=device-width, initial-scale=1.0">
        \\<title>Klar Documentation</title>
        \\<style>
        \\body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; max-width: 900px; margin: 0 auto; padding: 2rem; color: #1a1a1a; }
        \\h1 { border-bottom: 2px solid #0066cc; padding-bottom: 0.5rem; }
        \\.module-list { list-style: none; padding: 0; }
        \\.module-list li { padding: 0.75rem 1rem; border: 1px solid #e1e4e8; border-radius: 6px; margin: 0.5rem 0; }
        \\.module-list a { color: #0066cc; text-decoration: none; font-weight: 600; font-size: 1.1rem; }
        \\.module-list a:hover { text-decoration: underline; }
        \\.module-path { color: #6b7280; font-size: 0.85rem; font-family: monospace; }
        \\.module-count { color: #6b7280; font-size: 0.85rem; float: right; }
        \\.module-doc-preview { color: #374151; font-size: 0.9rem; margin-top: 0.25rem; }
        \\</style>
        \\</head>
        \\<body>
        \\<h1>Klar Documentation</h1>
        \\<ul class="module-list">
        \\
    );

    for (modules) |module| {
        const html_name = try std.fmt.allocPrint(allocator, "{s}.html", .{module.name});
        defer allocator.free(html_name);

        try w.print("<li><a href=\"{s}\">{s}</a>", .{ html_name, module.name });
        try w.print("<span class=\"module-count\">{d} items</span>", .{module.items.len});
        try w.print("<br><span class=\"module-path\">{s}</span>", .{module.path});

        if (module.module_doc.len > 0) {
            // Show first line of module doc as preview
            const first_line_end = std.mem.indexOfScalar(u8, module.module_doc, '\n') orelse module.module_doc.len;
            const preview = module.module_doc[0..first_line_end];
            try w.writeAll("<div class=\"module-doc-preview\">");
            try writeHtmlEscaped(w, preview);
            try w.writeAll("</div>");
        }

        try w.writeAll("</li>\n");
    }

    try w.writeAll("</ul>\n</body>\n</html>\n");
    return buf.toOwnedSlice(allocator);
}

fn writeHtmlEscaped(w: anytype, text: []const u8) !void {
    for (text) |c| {
        switch (c) {
            '<' => try w.writeAll("&lt;"),
            '>' => try w.writeAll("&gt;"),
            '&' => try w.writeAll("&amp;"),
            '"' => try w.writeAll("&quot;"),
            '\n' => try w.writeAll("<br>"),
            else => try w.writeByte(c),
        }
    }
}

/// Write doc comment content as HTML, handling basic markdown.
fn writeDocCommentHtml(w: anytype, text: []const u8) !void {
    var lines = std.mem.splitScalar(u8, text, '\n');
    var in_code_block = false;

    while (lines.next()) |line| {
        if (std.mem.startsWith(u8, line, "```")) {
            if (in_code_block) {
                try w.writeAll("</code></pre>");
                in_code_block = false;
            } else {
                try w.writeAll("<pre><code>");
                in_code_block = true;
            }
            continue;
        }

        if (in_code_block) {
            try writeHtmlEscaped(w, line);
            try w.writeByte('\n');
            continue;
        }

        // Handle inline code
        var i: usize = 0;
        while (i < line.len) {
            if (line[i] == '`') {
                const end = std.mem.indexOfScalarPos(u8, line, i + 1, '`') orelse {
                    try w.writeByte('`');
                    i += 1;
                    continue;
                };
                try w.writeAll("<code>");
                try writeHtmlEscaped(w, line[i + 1 .. end]);
                try w.writeAll("</code>");
                i = end + 1;
            } else {
                switch (line[i]) {
                    '<' => try w.writeAll("&lt;"),
                    '>' => try w.writeAll("&gt;"),
                    '&' => try w.writeAll("&amp;"),
                    else => try w.writeByte(line[i]),
                }
                i += 1;
            }
        }
        try w.writeAll("<br>\n");
    }

    if (in_code_block) {
        try w.writeAll("</code></pre>");
    }
}

// =============================================================================
// Tests
// =============================================================================

test "extractDocs basic function" {
    const source =
        \\/// Greet someone by name.
        \\pub fn greet(name: string) -> string {
        \\    return "Hello, " + name
        \\}
    ;

    const module = try extractDocs(std.testing.allocator, source, "test", "test.kl");
    defer std.testing.allocator.free(module.name);
    defer std.testing.allocator.free(module.path);
    defer {
        for (module.items) |item| {
            std.testing.allocator.free(item.name);
            std.testing.allocator.free(item.signature);
            if (item.doc_comment.len > 0) std.testing.allocator.free(item.doc_comment);
        }
        std.testing.allocator.free(module.items);
    }

    try std.testing.expectEqual(@as(usize, 1), module.items.len);
    try std.testing.expectEqualStrings("greet", module.items[0].name);
    try std.testing.expectEqual(DocItem.Kind.function, module.items[0].kind);
    try std.testing.expectEqualStrings("Greet someone by name.", module.items[0].doc_comment);
}

test "extractDocs struct and enum" {
    const source =
        \\/// A 2D point.
        \\pub struct Point { x: f64, y: f64 }
        \\
        \\/// Traffic light colors.
        \\pub enum Color { Red, Green, Blue }
    ;

    const module = try extractDocs(std.testing.allocator, source, "shapes", "shapes.kl");
    defer std.testing.allocator.free(module.name);
    defer std.testing.allocator.free(module.path);
    defer {
        for (module.items) |item| {
            std.testing.allocator.free(item.name);
            std.testing.allocator.free(item.signature);
            if (item.doc_comment.len > 0) std.testing.allocator.free(item.doc_comment);
        }
        std.testing.allocator.free(module.items);
    }

    try std.testing.expectEqual(@as(usize, 2), module.items.len);
    try std.testing.expectEqualStrings("Point", module.items[0].name);
    try std.testing.expectEqual(DocItem.Kind.struct_decl, module.items[0].kind);
    try std.testing.expectEqualStrings("Color", module.items[1].name);
    try std.testing.expectEqual(DocItem.Kind.enum_decl, module.items[1].kind);
}

test "extractDocs skips non-pub" {
    const source =
        \\fn private_fn() -> void {}
        \\pub fn public_fn() -> void {}
    ;

    const module = try extractDocs(std.testing.allocator, source, "test", "test.kl");
    defer std.testing.allocator.free(module.name);
    defer std.testing.allocator.free(module.path);
    defer {
        for (module.items) |item| {
            std.testing.allocator.free(item.name);
            std.testing.allocator.free(item.signature);
            if (item.doc_comment.len > 0) std.testing.allocator.free(item.doc_comment);
        }
        std.testing.allocator.free(module.items);
    }

    try std.testing.expectEqual(@as(usize, 1), module.items.len);
    try std.testing.expectEqualStrings("public_fn", module.items[0].name);
}

test "extractDocs module doc comment" {
    const source =
        \\//! This is a module-level doc comment.
        \\//! It describes the whole module.
        \\
        \\pub fn foo() -> void {}
    ;

    const module = try extractDocs(std.testing.allocator, source, "mymod", "mymod.kl");
    defer std.testing.allocator.free(module.name);
    defer std.testing.allocator.free(module.path);
    defer {
        for (module.items) |item| {
            std.testing.allocator.free(item.name);
            std.testing.allocator.free(item.signature);
        }
        std.testing.allocator.free(module.items);
        if (module.module_doc.len > 0) std.testing.allocator.free(module.module_doc);
    }

    try std.testing.expectEqualStrings("This is a module-level doc comment.\nIt describes the whole module.", module.module_doc);
}

test "extractDocs generic function" {
    const source =
        \\/// Sort a list in place.
        \\pub fn sort#[T: Ordered](list: List#[T]) -> List#[T] {
        \\    return list
        \\}
    ;

    const module = try extractDocs(std.testing.allocator, source, "algo", "algo.kl");
    defer std.testing.allocator.free(module.name);
    defer std.testing.allocator.free(module.path);
    defer {
        for (module.items) |item| {
            std.testing.allocator.free(item.name);
            std.testing.allocator.free(item.signature);
            if (item.doc_comment.len > 0) std.testing.allocator.free(item.doc_comment);
        }
        std.testing.allocator.free(module.items);
    }

    try std.testing.expectEqual(@as(usize, 1), module.items.len);
    try std.testing.expectEqualStrings("sort", module.items[0].name);
    try std.testing.expect(std.mem.indexOf(u8, module.items[0].signature, "List#[T]") != null);
}
