//! Meta annotation query engine for the `klar meta` CLI command.
//!
//! Extracted from main.zig to keep the CLI entry point lean.
//! Provides parsing, collection, and output (human-readable + JSON)
//! for querying meta annotations across Klar source files.

const std = @import("std");
const ast = @import("ast.zig");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;

// ============================================================================
// Types
// ============================================================================

pub const MetaQueryMode = union(enum) {
    tag: []const u8,
    module,
    related: []const u8,
    deprecated,
    hints,
};

/// Kind of declaration carrying meta annotations.
/// Shared between meta CLI and meta validation.
pub const DeclKind = enum {
    function,
    test_decl,
    struct_decl,
    struct_field,
    enum_decl,
    enum_variant,
    trait_decl,
    impl_decl,
    type_alias,
    const_decl,
    file_meta,

    pub fn toString(self: DeclKind) []const u8 {
        return switch (self) {
            .function => "function",
            .test_decl => "test",
            .struct_decl => "struct",
            .struct_field => "field",
            .enum_decl => "enum",
            .enum_variant => "variant",
            .trait_decl => "trait",
            .impl_decl => "impl",
            .type_alias => "type_alias",
            .const_decl => "const",
            .file_meta => "file",
        };
    }
};

pub const MetaMatch = struct {
    path: []const u8,
    name: []const u8,
    kind: DeclKind,
    line: usize,
    meta: []const ast.MetaAnnotation,
};

// ============================================================================
// Entry point
// ============================================================================

pub fn metaCommand(allocator: std.mem.Allocator, args: []const []const u8) !void {
    const stderr = getStdErr();

    var query_mode: ?MetaQueryMode = null;
    var json_output = false;
    var target_path: ?[]const u8 = null;

    var i: usize = 2;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            try getStdOut().writeAll(
                \\Usage: klar meta [options] [path]
                \\
                \\Query meta annotations across Klar source files.
                \\
                \\Options:
                \\  --tag "name"       Find declarations with a specific tag
                \\  --module           List all module descriptions
                \\  --related fn_name  Follow cross-references for a function
                \\  --deprecated       List all deprecated items
                \\  --hints            List all AI hints
                \\  --json             Machine-readable JSON output
                \\  -h, --help         Show this help message
                \\
                \\If no path is given, searches the current directory recursively.
                \\
            );
            return;
        } else if (std.mem.eql(u8, arg, "--json")) {
            json_output = true;
        } else if (std.mem.eql(u8, arg, "--tag")) {
            if (query_mode != null) {
                try stderr.writeAll("Error: multiple query modes specified\n");
                return;
            }
            i += 1;
            if (i >= args.len) {
                try stderr.writeAll("Error: --tag requires a value\n");
                return;
            }
            query_mode = .{ .tag = args[i] };
        } else if (std.mem.eql(u8, arg, "--module")) {
            if (query_mode != null) {
                try stderr.writeAll("Error: multiple query modes specified\n");
                return;
            }
            query_mode = .module;
        } else if (std.mem.eql(u8, arg, "--related")) {
            if (query_mode != null) {
                try stderr.writeAll("Error: multiple query modes specified\n");
                return;
            }
            i += 1;
            if (i >= args.len) {
                try stderr.writeAll("Error: --related requires a function name\n");
                return;
            }
            query_mode = .{ .related = args[i] };
        } else if (std.mem.eql(u8, arg, "--deprecated")) {
            if (query_mode != null) {
                try stderr.writeAll("Error: multiple query modes specified\n");
                return;
            }
            query_mode = .deprecated;
        } else if (std.mem.eql(u8, arg, "--hints")) {
            if (query_mode != null) {
                try stderr.writeAll("Error: multiple query modes specified\n");
                return;
            }
            query_mode = .hints;
        } else if (std.mem.startsWith(u8, arg, "-")) {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Error: unknown flag '{s}'\n", .{arg}) catch "Error: unknown flag\n";
            try stderr.writeAll(msg);
            return;
        } else {
            target_path = arg;
        }
    }

    const mode = query_mode orelse {
        try stderr.writeAll("Error: no query mode specified\nUsage: klar meta --tag|--module|--related|--deprecated|--hints [path]\n");
        return;
    };

    const path = target_path orelse ".";

    // We need an arena for parsed ASTs to live long enough for output
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    // Collect matches (arena-allocated, freed with arena)
    var matches = std.ArrayListUnmanaged(MetaMatch){};

    // Determine if path is a file or directory
    const stat = std.fs.cwd().statFile(path) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Error: cannot access '{s}': {}\n", .{ path, err }) catch "Error accessing path\n";
        try stderr.writeAll(msg);
        return;
    };

    if (stat.kind == .directory) {
        try metaQueryDirectory(arena_alloc, path, mode, &matches);
    } else {
        metaQueryFile(arena_alloc, path, mode, &matches) catch |err| {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Warning: error processing '{s}': {}\n", .{ path, err }) catch "Warning: error processing file\n";
            stderr.writeAll(msg) catch {};
        };
    }

    // Sort matches by path then line
    std.mem.sort(MetaMatch, matches.items, {}, struct {
        fn lessThan(_: void, a: MetaMatch, b: MetaMatch) bool {
            const path_cmp = std.mem.order(u8, a.path, b.path);
            if (path_cmp == .lt) return true;
            if (path_cmp == .gt) return false;
            return a.line < b.line;
        }
    }.lessThan);

    // Output results
    if (json_output) {
        try metaOutputJson(mode, matches.items);
    } else {
        try metaOutputHuman(mode, matches.items);
    }
}

// ============================================================================
// Directory / file walking
// ============================================================================

fn metaQueryDirectory(
    allocator: std.mem.Allocator,
    dir_path: []const u8,
    mode: MetaQueryMode,
    matches: *std.ArrayListUnmanaged(MetaMatch),
) !void {
    const stderr = getStdErr();

    var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Error opening directory '{s}': {}\n", .{ dir_path, err }) catch "Error opening directory\n";
        try stderr.writeAll(msg);
        return;
    };
    defer dir.close();

    var walker = dir.walk(allocator) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Error walking directory '{s}': {}\n", .{ dir_path, err }) catch "Error walking directory\n";
        try stderr.writeAll(msg);
        return;
    };
    defer walker.deinit();

    while (walker.next() catch |err| blk: {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Warning: error walking '{s}': {}\n", .{ dir_path, err }) catch "Warning: directory walk error\n";
        stderr.writeAll(msg) catch {};
        break :blk null;
    }) |entry| {
        if (entry.kind == .directory) continue;
        if (shouldSkipPath(entry.path)) continue;
        if (!std.mem.endsWith(u8, entry.path, ".kl")) continue;

        const full_path = std.fs.path.join(allocator, &.{ dir_path, entry.path }) catch continue;

        metaQueryFile(allocator, full_path, mode, matches) catch |err| {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Warning: error processing '{s}': {}\n", .{ full_path, err }) catch "Warning: error processing file\n";
            stderr.writeAll(msg) catch {};
        };
    }
}

fn metaQueryFile(
    allocator: std.mem.Allocator,
    path: []const u8,
    mode: MetaQueryMode,
    matches: *std.ArrayListUnmanaged(MetaMatch),
) !void {
    const source = readSourceFile(allocator, path) catch return;
    // Source is arena-allocated; AST string slices reference it, so it must
    // stay alive until output is complete. The arena handles cleanup.

    var lexer = Lexer.init(source);
    var parser = Parser.init(allocator, &lexer, source);
    const module = parser.parseModule() catch return; // skip files with parse errors

    switch (mode) {
        .tag => |tag_name| metaCollectTag(allocator, path, module, tag_name, matches),
        .module => metaCollectModule(allocator, path, module, matches),
        .deprecated => metaCollectDeprecated(allocator, path, module, matches),
        .hints => metaCollectHints(allocator, path, module, matches),
        .related => |fn_name| metaCollectRelated(allocator, path, module, fn_name, matches),
    }
}

// ============================================================================
// Helpers
// ============================================================================

fn getDeclNameAndMeta(decl: ast.Decl) struct { name: []const u8, kind: DeclKind, meta: []const ast.MetaAnnotation, line: usize } {
    return switch (decl) {
        .function => |f| .{ .name = f.name, .kind = .function, .meta = f.meta, .line = f.span.line },
        .test_decl => |t| .{ .name = t.name, .kind = .test_decl, .meta = t.meta, .line = t.span.line },
        .struct_decl => |s| .{ .name = s.name, .kind = .struct_decl, .meta = s.meta, .line = s.span.line },
        .enum_decl => |e| .{ .name = e.name, .kind = .enum_decl, .meta = e.meta, .line = e.span.line },
        .trait_decl => |t| .{ .name = t.name, .kind = .trait_decl, .meta = t.meta, .line = t.span.line },
        .impl_decl => |impl_d| .{
            .name = getImplName(impl_d.target_type),
            .kind = .impl_decl,
            .meta = impl_d.meta,
            .line = impl_d.span.line,
        },
        .type_alias => |t| .{ .name = t.name, .kind = .type_alias, .meta = t.meta, .line = t.span.line },
        .const_decl => |c| .{ .name = c.name, .kind = .const_decl, .meta = c.meta, .line = c.span.line },
        .import_decl => .{ .name = "", .kind = .function, .meta = &.{}, .line = 0 },
        .module_decl => .{ .name = "", .kind = .function, .meta = &.{}, .line = 0 },
        .extern_type_decl => |e| .{ .name = e.name, .kind = .struct_decl, .meta = &.{}, .line = e.span.line },
        .extern_block => .{ .name = "", .kind = .function, .meta = &.{}, .line = 0 },
    };
}

fn getImplName(type_expr: ast.TypeExpr) []const u8 {
    return switch (type_expr) {
        .named => |n| n.name,
        .generic_apply => |g| getImplName(g.base),
        else => "impl",
    };
}

/// Check if a meta annotation slice has a tag matching the given name.
/// Also expands group membership: if the declaration has `meta in("group")`,
/// check if the group's annotations include a matching tag.
fn hasMatchingTag(meta: []const ast.MetaAnnotation, tag_name: []const u8, file_meta: []const ast.MetaAnnotation) bool {
    for (meta) |ann| {
        switch (ann) {
            .tag => |t| {
                if (std.mem.eql(u8, t.value, tag_name)) return true;
            },
            .group_join => |gj| {
                // Find the group definition in file_meta and check its annotations
                for (file_meta) |fm| {
                    switch (fm) {
                        .group_def => |gd| {
                            if (std.mem.eql(u8, gd.name, gj.value)) {
                                for (gd.annotations) |ga| {
                                    switch (ga) {
                                        .tag => |gt| {
                                            if (std.mem.eql(u8, gt.value, tag_name)) return true;
                                        },
                                        else => {},
                                    }
                                }
                            }
                        },
                        else => {},
                    }
                }
            },
            else => {},
        }
    }
    return false;
}

/// Check if a meta annotation slice (or its group) has a deprecated annotation.
/// Returns the deprecated message if found.
fn hasDeprecatedViaGroup(meta: []const ast.MetaAnnotation, file_meta: []const ast.MetaAnnotation) ?[]const u8 {
    for (meta) |ann| {
        switch (ann) {
            .deprecated => |d| return d.value,
            .group_join => |gj| {
                for (file_meta) |fm| {
                    switch (fm) {
                        .group_def => |gd| {
                            if (std.mem.eql(u8, gd.name, gj.value)) {
                                for (gd.annotations) |ga| {
                                    switch (ga) {
                                        .deprecated => |d| return d.value,
                                        else => {},
                                    }
                                }
                            }
                        },
                        else => {},
                    }
                }
            },
            else => {},
        }
    }
    return null;
}

/// Check if a meta annotation slice (or its group) has a hint annotation.
fn hasHintViaGroup(meta: []const ast.MetaAnnotation, file_meta: []const ast.MetaAnnotation) bool {
    for (meta) |ann| {
        switch (ann) {
            .hint => return true,
            .group_join => |gj| {
                for (file_meta) |fm| {
                    switch (fm) {
                        .group_def => |gd| {
                            if (std.mem.eql(u8, gd.name, gj.value)) {
                                for (gd.annotations) |ga| {
                                    switch (ga) {
                                        .hint => return true,
                                        else => {},
                                    }
                                }
                            }
                        },
                        else => {},
                    }
                }
            },
            else => {},
        }
    }
    return false;
}

/// Collect all meta annotations for a match, expanding group membership.
/// Falls back to unexpanded meta on allocation failure.
fn collectExpandedMeta(
    allocator: std.mem.Allocator,
    meta: []const ast.MetaAnnotation,
    file_meta: []const ast.MetaAnnotation,
) []const ast.MetaAnnotation {
    // Check if we need group expansion
    var has_group_join = false;
    for (meta) |ann| {
        switch (ann) {
            .group_join => {
                has_group_join = true;
                break;
            },
            else => {},
        }
    }
    if (!has_group_join) return meta;

    // Build expanded list — fall back to unexpanded on OOM
    var expanded = std.ArrayListUnmanaged(ast.MetaAnnotation){};
    for (meta) |ann| {
        switch (ann) {
            .group_join => |gj| {
                expanded.append(allocator, ann) catch return meta;
                // Find and inline group annotations
                for (file_meta) |fm| {
                    switch (fm) {
                        .group_def => |gd| {
                            if (std.mem.eql(u8, gd.name, gj.value)) {
                                for (gd.annotations) |ga| {
                                    expanded.append(allocator, ga) catch return meta;
                                }
                            }
                        },
                        else => {},
                    }
                }
            },
            else => expanded.append(allocator, ann) catch return meta,
        }
    }
    return expanded.items;
}

fn shouldSkipPath(path: []const u8) bool {
    // Skip hidden directories
    if (std.mem.startsWith(u8, path, ".")) return true;

    // Skip known directories
    const skip_dirs = [_][]const u8{ "build/", "zig-out/", "scratch/", "zig-cache/", ".zig-cache/" };
    for (skip_dirs) |skip| {
        if (std.mem.startsWith(u8, path, skip)) return true;
    }

    // Skip paths containing hidden directory segments (handle both / and \ separators)
    var it = std.mem.tokenizeAny(u8, path, "/\\");
    while (it.next()) |segment| {
        if (segment.len > 0 and segment[0] == '.') return true;
    }

    return false;
}

fn readSourceFile(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    return try file.readToEndAlloc(allocator, 10 * 1024 * 1024);
}

// ============================================================================
// Collection: tag
// ============================================================================

fn metaCollectTag(
    allocator: std.mem.Allocator,
    path: []const u8,
    module: ast.Module,
    tag_name: []const u8,
    matches: *std.ArrayListUnmanaged(MetaMatch),
) void {
    // Check file-level meta
    if (hasMatchingTag(module.file_meta, tag_name, module.file_meta)) {
        matches.append(allocator, .{
            .path = path,
            .name = path,
            .kind = .file_meta,
            .line = if (module.file_meta.len > 0) module.file_meta[0].span().line else 1,
            .meta = collectExpandedMeta(allocator, module.file_meta, module.file_meta),
        }) catch {};
    }

    for (module.declarations) |decl| {
        const info = getDeclNameAndMeta(decl);
        if (info.meta.len == 0) continue;

        if (hasMatchingTag(info.meta, tag_name, module.file_meta)) {
            matches.append(allocator, .{
                .path = path,
                .name = info.name,
                .kind = info.kind,
                .line = info.line,
                .meta = collectExpandedMeta(allocator, info.meta, module.file_meta),
            }) catch {};
        }

        // Descend into struct fields
        if (decl == .struct_decl) {
            for (decl.struct_decl.fields) |field| {
                if (hasMatchingTag(field.meta, tag_name, module.file_meta)) {
                    matches.append(allocator, .{
                        .path = path,
                        .name = field.name,
                        .kind = .struct_field,
                        .line = field.span.line,
                        .meta = collectExpandedMeta(allocator, field.meta, module.file_meta),
                    }) catch {};
                }
            }
        }

        // Descend into enum variants
        if (decl == .enum_decl) {
            for (decl.enum_decl.variants) |variant| {
                if (hasMatchingTag(variant.meta, tag_name, module.file_meta)) {
                    matches.append(allocator, .{
                        .path = path,
                        .name = variant.name,
                        .kind = .enum_variant,
                        .line = variant.span.line,
                        .meta = collectExpandedMeta(allocator, variant.meta, module.file_meta),
                    }) catch {};
                }
            }
        }

        // Descend into impl/trait methods
        metaCollectMethodsTag(allocator, path, decl, tag_name, module.file_meta, matches);
    }
}

fn metaCollectMethodsTag(
    allocator: std.mem.Allocator,
    path: []const u8,
    decl: ast.Decl,
    tag_name: []const u8,
    file_meta: []const ast.MetaAnnotation,
    matches: *std.ArrayListUnmanaged(MetaMatch),
) void {
    const methods = switch (decl) {
        .impl_decl => |impl_d| impl_d.methods,
        .trait_decl => |t| t.methods,
        else => return,
    };
    for (methods) |method| {
        if (hasMatchingTag(method.meta, tag_name, file_meta)) {
            matches.append(allocator, .{
                .path = path,
                .name = method.name,
                .kind = .function,
                .line = method.span.line,
                .meta = collectExpandedMeta(allocator, method.meta, file_meta),
            }) catch {};
        }
    }
}

// ============================================================================
// Collection: module
// ============================================================================

fn metaCollectModule(
    allocator: std.mem.Allocator,
    path: []const u8,
    module: ast.Module,
    matches: *std.ArrayListUnmanaged(MetaMatch),
) void {
    for (module.file_meta) |ann| {
        switch (ann) {
            .module_meta, .guide => {
                matches.append(allocator, .{
                    .path = path,
                    .name = path,
                    .kind = .file_meta,
                    .line = ann.span().line,
                    .meta = module.file_meta,
                }) catch {};
                return; // One match per file
            },
            else => {},
        }
    }
}

// ============================================================================
// Collection: deprecated (with group expansion)
// ============================================================================

fn metaCollectDeprecated(
    allocator: std.mem.Allocator,
    path: []const u8,
    module: ast.Module,
    matches: *std.ArrayListUnmanaged(MetaMatch),
) void {
    // Check file-level meta
    if (hasDeprecatedViaGroup(module.file_meta, module.file_meta) != null) {
        matches.append(allocator, .{
            .path = path,
            .name = path,
            .kind = .file_meta,
            .line = if (module.file_meta.len > 0) module.file_meta[0].span().line else 1,
            .meta = collectExpandedMeta(allocator, module.file_meta, module.file_meta),
        }) catch {};
    }

    for (module.declarations) |decl| {
        const info = getDeclNameAndMeta(decl);
        if (hasDeprecatedViaGroup(info.meta, module.file_meta) != null) {
            matches.append(allocator, .{
                .path = path,
                .name = info.name,
                .kind = info.kind,
                .line = info.line,
                .meta = collectExpandedMeta(allocator, info.meta, module.file_meta),
            }) catch {};
        }

        // Descend into struct fields
        if (decl == .struct_decl) {
            for (decl.struct_decl.fields) |field| {
                if (hasDeprecatedViaGroup(field.meta, module.file_meta) != null) {
                    matches.append(allocator, .{
                        .path = path,
                        .name = field.name,
                        .kind = .struct_field,
                        .line = field.span.line,
                        .meta = collectExpandedMeta(allocator, field.meta, module.file_meta),
                    }) catch {};
                }
            }
        }

        // Descend into enum variants
        if (decl == .enum_decl) {
            for (decl.enum_decl.variants) |variant| {
                if (hasDeprecatedViaGroup(variant.meta, module.file_meta) != null) {
                    matches.append(allocator, .{
                        .path = path,
                        .name = variant.name,
                        .kind = .enum_variant,
                        .line = variant.span.line,
                        .meta = collectExpandedMeta(allocator, variant.meta, module.file_meta),
                    }) catch {};
                }
            }
        }

        // Descend into impl/trait methods
        const methods = switch (decl) {
            .impl_decl => |impl_d| impl_d.methods,
            .trait_decl => |t| t.methods,
            else => continue,
        };
        for (methods) |method| {
            if (hasDeprecatedViaGroup(method.meta, module.file_meta) != null) {
                matches.append(allocator, .{
                    .path = path,
                    .name = method.name,
                    .kind = .function,
                    .line = method.span.line,
                    .meta = collectExpandedMeta(allocator, method.meta, module.file_meta),
                }) catch {};
            }
        }
    }
}

// ============================================================================
// Collection: hints (with group expansion)
// ============================================================================

fn metaCollectHints(
    allocator: std.mem.Allocator,
    path: []const u8,
    module: ast.Module,
    matches: *std.ArrayListUnmanaged(MetaMatch),
) void {
    // Check file-level hints
    if (hasHintViaGroup(module.file_meta, module.file_meta)) {
        matches.append(allocator, .{
            .path = path,
            .name = path,
            .kind = .file_meta,
            .line = if (module.file_meta.len > 0) module.file_meta[0].span().line else 1,
            .meta = collectExpandedMeta(allocator, module.file_meta, module.file_meta),
        }) catch {};
    }

    for (module.declarations) |decl| {
        const info = getDeclNameAndMeta(decl);
        if (hasHintViaGroup(info.meta, module.file_meta)) {
            matches.append(allocator, .{
                .path = path,
                .name = info.name,
                .kind = info.kind,
                .line = info.line,
                .meta = collectExpandedMeta(allocator, info.meta, module.file_meta),
            }) catch {};
        }

        // Descend into impl/trait methods
        const methods = switch (decl) {
            .impl_decl => |impl_d| impl_d.methods,
            .trait_decl => |t| t.methods,
            else => continue,
        };
        for (methods) |method| {
            if (hasHintViaGroup(method.meta, module.file_meta)) {
                matches.append(allocator, .{
                    .path = path,
                    .name = method.name,
                    .kind = .function,
                    .line = method.span.line,
                    .meta = collectExpandedMeta(allocator, method.meta, module.file_meta),
                }) catch {};
            }
        }
    }
}

// ============================================================================
// Collection: related
// ============================================================================

fn metaCollectRelated(
    allocator: std.mem.Allocator,
    path: []const u8,
    module: ast.Module,
    fn_name: []const u8,
    matches: *std.ArrayListUnmanaged(MetaMatch),
) void {
    for (module.declarations) |decl| {
        const info = getDeclNameAndMeta(decl);
        metaCheckRelated(allocator, path, info.name, info.kind, info.line, info.meta, fn_name, matches);

        // Descend into impl/trait methods
        const methods = switch (decl) {
            .impl_decl => |impl_d| impl_d.methods,
            .trait_decl => |t| t.methods,
            else => continue,
        };
        for (methods) |method| {
            metaCheckRelated(allocator, path, method.name, .function, method.span.line, method.meta, fn_name, matches);
        }
    }
}

fn metaCheckRelated(
    allocator: std.mem.Allocator,
    path: []const u8,
    name: []const u8,
    kind: DeclKind,
    line: usize,
    meta: []const ast.MetaAnnotation,
    fn_name: []const u8,
    matches: *std.ArrayListUnmanaged(MetaMatch),
) void {
    // Match by name (the target itself)
    if (std.mem.eql(u8, name, fn_name)) {
        matches.append(allocator, .{
            .path = path,
            .name = name,
            .kind = kind,
            .line = line,
            .meta = meta,
        }) catch {};
        return;
    }
    // Match by related paths referencing the target
    if (meta.len == 0) return;
    for (meta) |ann| {
        switch (ann) {
            .related => |rel| {
                for (rel.paths) |rel_path| {
                    if (metaPathMatchesName(rel_path, fn_name)) {
                        matches.append(allocator, .{
                            .path = path,
                            .name = name,
                            .kind = kind,
                            .line = line,
                            .meta = meta,
                        }) catch {};
                        return;
                    }
                }
            },
            else => {},
        }
    }
}

fn metaPathMatchesName(path: ast.MetaPath, name: []const u8) bool {
    // Match if the last segment equals the name
    if (path.segments.len == 0) return false;
    if (std.mem.eql(u8, path.segments[path.segments.len - 1], name)) return true;
    // Also check full :: -separated path (e.g., "lexer::next_token")
    if (path.segments.len == 1) return false;
    // Compute expected length: sum of segments + (n-1) * 2 for "::" separators
    var total_len: usize = 0;
    for (path.segments, 0..) |seg, idx| {
        total_len += seg.len;
        if (idx < path.segments.len - 1) total_len += 2; // "::"
    }
    if (total_len != name.len) return false;
    var pos: usize = 0;
    for (path.segments, 0..) |seg, idx| {
        if (pos + seg.len > name.len) return false;
        if (!std.mem.eql(u8, name[pos..][0..seg.len], seg)) return false;
        pos += seg.len;
        if (idx < path.segments.len - 1) {
            if (pos + 2 > name.len) return false;
            if (name[pos] != ':' or name[pos + 1] != ':') return false;
            pos += 2;
        }
    }
    return true;
}

// ============================================================================
// IO helpers
// ============================================================================

const builtin = @import("builtin");

fn getStdOut() std.fs.File {
    if (comptime builtin.os.tag == .windows) {
        const handle = std.os.windows.kernel32.GetStdHandle(std.os.windows.STD_OUTPUT_HANDLE);
        return .{ .handle = handle };
    } else {
        return .{ .handle = std.posix.STDOUT_FILENO };
    }
}

fn getStdErr() std.fs.File {
    if (comptime builtin.os.tag == .windows) {
        const handle = std.os.windows.kernel32.GetStdHandle(std.os.windows.STD_ERROR_HANDLE);
        return .{ .handle = handle };
    } else {
        return .{ .handle = std.posix.STDERR_FILENO };
    }
}

/// Write a MetaPath with :: separators (human-readable output)
fn metaWritePath(out: std.fs.File, p: ast.MetaPath) !void {
    for (p.segments, 0..) |seg, sidx| {
        if (sidx > 0) try out.writeAll("::");
        try out.writeAll(seg);
    }
}

fn writeJsonEscaped(out: std.fs.File, s: []const u8) !void {
    try out.writeAll("\"");
    for (s) |c| {
        switch (c) {
            '"' => try out.writeAll("\\\""),
            '\\' => try out.writeAll("\\\\"),
            '\n' => try out.writeAll("\\n"),
            '\r' => try out.writeAll("\\r"),
            '\t' => try out.writeAll("\\t"),
            else => {
                if (c < 0x20) {
                    var esc_buf: [6]u8 = undefined;
                    const esc = std.fmt.bufPrint(&esc_buf, "\\u{x:0>4}", .{@as(u16, c)}) catch unreachable;
                    try out.writeAll(esc);
                } else {
                    try out.writeAll(&[_]u8{c});
                }
            },
        }
    }
    try out.writeAll("\"");
}

/// Write a string with JSON escaping (no surrounding quotes).
fn writeJsonEscapedRaw(out: std.fs.File, s: []const u8) !void {
    for (s) |c| {
        switch (c) {
            '"' => try out.writeAll("\\\""),
            '\\' => try out.writeAll("\\\\"),
            '\n' => try out.writeAll("\\n"),
            '\r' => try out.writeAll("\\r"),
            '\t' => try out.writeAll("\\t"),
            else => {
                if (c < 0x20) {
                    var esc_buf: [6]u8 = undefined;
                    const esc = std.fmt.bufPrint(&esc_buf, "\\u{x:0>4}", .{@as(u16, c)}) catch unreachable;
                    try out.writeAll(esc);
                } else {
                    try out.writeAll(&[_]u8{c});
                }
            },
        }
    }
}

// ============================================================================
// Output: Human-readable
// ============================================================================

fn metaOutputHuman(mode: MetaQueryMode, matches: []const MetaMatch) !void {
    const out = getStdOut();

    if (matches.len == 0) {
        try out.writeAll("0 matches found.\n");
        return;
    }

    switch (mode) {
        .tag => |tag_name| {
            try out.writeAll("Tag: \"");
            try out.writeAll(tag_name);
            try out.writeAll("\"\n");
        },
        .module => try out.writeAll(""),
        .deprecated => try out.writeAll("Deprecated items:\n"),
        .hints => try out.writeAll("AI Hints:\n"),
        .related => |fn_name| {
            try out.writeAll("Cross-references for: ");
            try out.writeAll(fn_name);
            try out.writeAll("\n");
        },
    }

    for (matches) |m| {
        try out.writeAll("\n  ");
        try out.writeAll(m.path);
        try out.writeAll(":");
        var line_buf: [16]u8 = undefined;
        const line_str = std.fmt.bufPrint(&line_buf, "{d}", .{m.line}) catch "?";
        try out.writeAll(line_str);
        try out.writeAll("  ");
        try out.writeAll(m.kind.toString());
        try out.writeAll("  ");
        try out.writeAll(m.name);
        try out.writeAll("\n");

        // Print relevant meta annotations
        for (m.meta) |ann| {
            switch (ann) {
                .intent => |s| {
                    try out.writeAll("    intent: \"");
                    try out.writeAll(s.value);
                    try out.writeAll("\"\n");
                },
                .decision => |s| {
                    try out.writeAll("    decision: \"");
                    try out.writeAll(s.value);
                    try out.writeAll("\"\n");
                },
                .tag => |s| {
                    try out.writeAll("    tag: \"");
                    try out.writeAll(s.value);
                    try out.writeAll("\"\n");
                },
                .hint => |s| {
                    try out.writeAll("    hint: \"");
                    try out.writeAll(s.value);
                    try out.writeAll("\"\n");
                },
                .deprecated => |s| {
                    try out.writeAll("    deprecated: \"");
                    try out.writeAll(s.value);
                    try out.writeAll("\"\n");
                },
                .pure => {
                    try out.writeAll("    pure\n");
                },
                .module_meta => |b| {
                    try metaOutputBlock(out, "module", b);
                },
                .guide => |b| {
                    try metaOutputBlock(out, "guide", b);
                },
                .related => |r| {
                    try out.writeAll("    related: ");
                    for (r.paths, 0..) |p, pidx| {
                        if (pidx > 0) try out.writeAll(", ");
                        try metaWritePath(out, p);
                    }
                    if (r.description) |desc| {
                        try out.writeAll(" \"");
                        try out.writeAll(desc);
                        try out.writeAll("\"");
                    }
                    try out.writeAll("\n");
                },
                .group_join => |gj| {
                    try out.writeAll("    in: \"");
                    try out.writeAll(gj.value);
                    try out.writeAll("\"\n");
                },
                .custom => |c| {
                    try out.writeAll("    @");
                    try out.writeAll(c.name);
                    if (c.args.len > 0) {
                        try out.writeAll("(");
                        for (c.args, 0..) |ca, cidx| {
                            if (cidx > 0) try out.writeAll(", ");
                            switch (ca) {
                                .string => |s| {
                                    try out.writeAll("\"");
                                    try out.writeAll(s);
                                    try out.writeAll("\"");
                                },
                                .path => |p| try metaWritePath(out, p),
                            }
                        }
                        try out.writeAll(")");
                    }
                    try out.writeAll("\n");
                },
                .group_def, .define => {},
            }
        }
    }

    try out.writeAll("\n");
    var count_buf: [32]u8 = undefined;
    const count_str = std.fmt.bufPrint(&count_buf, "{d} match{s} found.\n", .{
        matches.len,
        if (matches.len == 1) @as([]const u8, "") else "es",
    }) catch "matches found.\n";
    try out.writeAll(count_str);
}

fn metaOutputBlock(out: std.fs.File, label: []const u8, block: *const ast.MetaBlock) !void {
    try out.writeAll("    ");
    try out.writeAll(label);
    try out.writeAll(":\n");
    for (block.entries) |entry| {
        try out.writeAll("      ");
        try out.writeAll(entry.key);
        try out.writeAll(": ");
        switch (entry.value) {
            .string => |s| {
                try out.writeAll("\"");
                try out.writeAll(s);
                try out.writeAll("\"\n");
            },
            .string_list => |list| {
                try out.writeAll("[");
                for (list, 0..) |item, idx| {
                    if (idx > 0) try out.writeAll(", ");
                    try out.writeAll("\"");
                    try out.writeAll(item);
                    try out.writeAll("\"");
                }
                try out.writeAll("]\n");
            },
        }
    }
}

// ============================================================================
// Output: JSON
// ============================================================================

fn metaOutputJson(mode: MetaQueryMode, matches: []const MetaMatch) !void {
    const out = getStdOut();

    try out.writeAll("{");

    // Query type
    switch (mode) {
        .tag => |tag_name| {
            try out.writeAll("\"query\":\"tag\",\"value\":");
            try writeJsonEscaped(out, tag_name);
        },
        .module => try out.writeAll("\"query\":\"module\""),
        .deprecated => try out.writeAll("\"query\":\"deprecated\""),
        .hints => try out.writeAll("\"query\":\"hints\""),
        .related => |fn_name| {
            try out.writeAll("\"query\":\"related\",\"value\":");
            try writeJsonEscaped(out, fn_name);
        },
    }

    try out.writeAll(",\"matches\":[");

    for (matches, 0..) |m, midx| {
        if (midx > 0) try out.writeAll(",");
        try out.writeAll("{\"path\":");
        try writeJsonEscaped(out, m.path);
        try out.writeAll(",\"line\":");
        var line_buf: [16]u8 = undefined;
        const line_str = std.fmt.bufPrint(&line_buf, "{d}", .{m.line}) catch "0";
        try out.writeAll(line_str);
        try out.writeAll(",\"name\":");
        try writeJsonEscaped(out, m.name);
        try out.writeAll(",\"kind\":");
        try writeJsonEscaped(out, m.kind.toString());

        // Flatten meta into an object
        try out.writeAll(",\"meta\":{");
        try metaOutputJsonMeta(out, m.meta);
        try out.writeAll("}}");
    }

    try out.writeAll("]}\n");
}

fn metaOutputJsonMeta(out: std.fs.File, meta: []const ast.MetaAnnotation) !void {
    // Collect tags, then other fields
    var wrote_field = false;

    // Tags array
    var has_tags = false;
    for (meta) |ann| {
        if (ann == .tag) { has_tags = true; break; }
    }
    if (has_tags) {
        try out.writeAll("\"tag\":[");
        var first = true;
        for (meta) |ann| {
            switch (ann) {
                .tag => |t| {
                    if (!first) try out.writeAll(",");
                    try writeJsonEscaped(out, t.value);
                    first = false;
                },
                else => {},
            }
        }
        try out.writeAll("]");
        wrote_field = true;
    }

    // Scalar fields
    for (meta) |ann| {
        switch (ann) {
            .intent => |s| {
                if (wrote_field) try out.writeAll(",");
                try out.writeAll("\"intent\":");
                try writeJsonEscaped(out, s.value);
                wrote_field = true;
            },
            .decision => |s| {
                if (wrote_field) try out.writeAll(",");
                try out.writeAll("\"decision\":");
                try writeJsonEscaped(out, s.value);
                wrote_field = true;
            },
            .hint => |s| {
                if (wrote_field) try out.writeAll(",");
                try out.writeAll("\"hint\":");
                try writeJsonEscaped(out, s.value);
                wrote_field = true;
            },
            .deprecated => |s| {
                if (wrote_field) try out.writeAll(",");
                try out.writeAll("\"deprecated\":");
                try writeJsonEscaped(out, s.value);
                wrote_field = true;
            },
            .pure => {
                if (wrote_field) try out.writeAll(",");
                try out.writeAll("\"pure\":true");
                wrote_field = true;
            },
            .module_meta => |b| {
                if (wrote_field) try out.writeAll(",");
                try out.writeAll("\"module\":{");
                try metaOutputJsonBlock(out, b);
                try out.writeAll("}");
                wrote_field = true;
            },
            .guide => |b| {
                if (wrote_field) try out.writeAll(",");
                try out.writeAll("\"guide\":{");
                try metaOutputJsonBlock(out, b);
                try out.writeAll("}");
                wrote_field = true;
            },
            .related, .custom, .tag, .group_join, .group_def, .define => {},
        }
    }

    // Aggregate all .custom annotations into a single "custom" array
    var has_custom = false;
    for (meta) |ann| {
        if (ann == .custom) { has_custom = true; break; }
    }
    if (has_custom) {
        if (wrote_field) try out.writeAll(",");
        try out.writeAll("\"custom\":[");
        var first_custom = true;
        for (meta) |ann| {
            switch (ann) {
                .custom => |c| {
                    if (!first_custom) try out.writeAll(",");
                    try out.writeAll("{\"name\":");
                    try writeJsonEscaped(out, c.name);
                    if (c.args.len > 0) {
                        try out.writeAll(",\"args\":[");
                        for (c.args, 0..) |ca, cidx| {
                            if (cidx > 0) try out.writeAll(",");
                            switch (ca) {
                                .string => |s| try writeJsonEscaped(out, s),
                                .path => |p| try metaOutputJsonPath(out, p),
                            }
                        }
                        try out.writeAll("]");
                    }
                    try out.writeAll("}");
                    first_custom = false;
                },
                else => {},
            }
        }
        try out.writeAll("]");
        wrote_field = true;
    }

    // Aggregate all .related annotations into a single "related" array
    var has_related = false;
    for (meta) |ann| {
        if (ann == .related) { has_related = true; break; }
    }
    if (has_related) {
        if (wrote_field) try out.writeAll(",");
        try out.writeAll("\"related\":[");
        var first_path = true;
        for (meta) |ann| {
            switch (ann) {
                .related => |r| {
                    for (r.paths) |p| {
                        if (!first_path) try out.writeAll(",");
                        try out.writeAll("{\"path\":");
                        try metaOutputJsonPath(out, p);
                        if (r.description) |desc| {
                            try out.writeAll(",\"description\":");
                            try writeJsonEscaped(out, desc);
                        }
                        try out.writeAll("}");
                        first_path = false;
                    }
                },
                else => {},
            }
        }
        try out.writeAll("]");
    }
}

/// Write a MetaPath as a JSON-escaped string with :: separators.
/// Path segments are escaped for JSON safety.
fn metaOutputJsonPath(out: std.fs.File, p: ast.MetaPath) !void {
    try out.writeAll("\"");
    for (p.segments, 0..) |seg, sidx| {
        if (sidx > 0) try out.writeAll("::");
        try writeJsonEscapedRaw(out, seg);
    }
    try out.writeAll("\"");
}

fn metaOutputJsonBlock(out: std.fs.File, block: *const ast.MetaBlock) !void {
    for (block.entries, 0..) |entry, eidx| {
        if (eidx > 0) try out.writeAll(",");
        try writeJsonEscaped(out, entry.key);
        try out.writeAll(":");
        switch (entry.value) {
            .string => |s| try writeJsonEscaped(out, s),
            .string_list => |list| {
                try out.writeAll("[");
                for (list, 0..) |item, idx| {
                    if (idx > 0) try out.writeAll(",");
                    try writeJsonEscaped(out, item);
                }
                try out.writeAll("]");
            },
        }
    }
}
