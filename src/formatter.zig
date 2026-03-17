//! Klar source code formatter.
//!
//! Produces canonically formatted Klar source code from an AST.
//! Uses a two-pass approach:
//! 1. Pre-pass: Extract comments from raw source text
//! 2. Format pass: Walk AST, interleave comments by byte position

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const ast = @import("ast.zig");
const Span = ast.Span;

// ============================================================================
// Comment Extraction
// ============================================================================

pub const Comment = struct {
    kind: Kind,
    text: []const u8, // The comment text (including // or /* */)
    start: usize, // Byte offset in source
    end: usize, // Byte offset end
    line: u32,
    is_trailing: bool, // Same line as code before it

    pub const Kind = enum {
        line, // // comment
        block, // /* comment */
    };
};

/// Extract all comments from raw source text.
/// Returns a sorted list of Comment structs.
pub fn extractComments(allocator: Allocator, source: []const u8) ![]Comment {
    var comments = std.ArrayListUnmanaged(Comment){};
    errdefer comments.deinit(allocator);

    var i: usize = 0;
    var line: u32 = 1;
    var line_has_code: bool = false;

    while (i < source.len) {
        switch (source[i]) {
            '\n' => {
                line += 1;
                line_has_code = false;
                i += 1;
            },
            ' ', '\t', '\r' => {
                i += 1;
            },
            '/' => {
                if (i + 1 < source.len and source[i + 1] == '/') {
                    // Line comment
                    const start = i;
                    const comment_line = line;
                    while (i < source.len and source[i] != '\n') {
                        i += 1;
                    }
                    try comments.append(allocator, .{
                        .kind = .line,
                        .text = source[start..i],
                        .start = start,
                        .end = i,
                        .line = comment_line,
                        .is_trailing = line_has_code,
                    });
                } else if (i + 1 < source.len and source[i + 1] == '*') {
                    // Block comment
                    const start = i;
                    const comment_line = line;
                    i += 2;
                    var terminated = false;
                    while (i + 1 < source.len) {
                        if (source[i] == '*' and source[i + 1] == '/') {
                            i += 2;
                            terminated = true;
                            break;
                        }
                        if (source[i] == '\n') {
                            line += 1;
                        }
                        i += 1;
                    }
                    if (!terminated) {
                        // Consume remaining character if loop ended at last position
                        if (i < source.len) i += 1;
                    }
                    try comments.append(allocator, .{
                        .kind = .block,
                        .text = source[start..i],
                        .start = start,
                        .end = i,
                        .line = comment_line,
                        .is_trailing = line_has_code,
                    });
                } else {
                    line_has_code = true;
                    i += 1;
                }
            },
            '"' => {
                // Skip string literals (don't find // inside strings)
                line_has_code = true;
                i += 1;
                while (i < source.len and source[i] != '"') {
                    if (source[i] == '\\' and i + 1 < source.len) {
                        i += 2;
                    } else {
                        if (source[i] == '\n') line += 1;
                        i += 1;
                    }
                }
                if (i < source.len) i += 1; // skip closing quote
            },
            '\'' => {
                // Skip char literals
                line_has_code = true;
                i += 1;
                while (i < source.len and source[i] != '\'') {
                    if (source[i] == '\\' and i + 1 < source.len) {
                        i += 2;
                    } else {
                        i += 1;
                    }
                }
                if (i < source.len) i += 1;
            },
            else => {
                line_has_code = true;
                i += 1;
            },
        }
    }

    return comments.toOwnedSlice(allocator);
}

// ============================================================================
// Formatter
// ============================================================================

pub const Config = struct {
    indent_width: u32 = 4,
};

pub const Formatter = struct {
    allocator: Allocator,
    source: []const u8,
    comments: []const Comment,
    output: std.ArrayListUnmanaged(u8),
    indent_level: u32,
    comment_cursor: usize,
    config: Config,

    pub const Error = Allocator.Error;

    pub fn init(allocator: Allocator, source: []const u8, comments: []const Comment, config: Config) Formatter {
        return .{
            .allocator = allocator,
            .source = source,
            .comments = comments,
            .output = .{},
            .indent_level = 0,
            .comment_cursor = 0,
            .config = config,
        };
    }

    pub fn deinit(self: *Formatter) void {
        self.output.deinit(self.allocator);
    }

    pub fn getOutput(self: *Formatter) []const u8 {
        return self.output.items;
    }

    // --- Output helpers ---

    fn write(self: *Formatter, s: []const u8) Error!void {
        try self.output.appendSlice(self.allocator, s);
    }

    fn writeByte(self: *Formatter, b: u8) Error!void {
        try self.output.append(self.allocator, b);
    }

    fn newline(self: *Formatter) Error!void {
        try self.writeByte('\n');
    }

    fn writeIndent(self: *Formatter) Error!void {
        const spaces = self.indent_level * self.config.indent_width;
        var i: u32 = 0;
        while (i < spaces) : (i += 1) {
            try self.writeByte(' ');
        }
    }

    fn indent(self: *Formatter) void {
        self.indent_level += 1;
    }

    fn dedent(self: *Formatter) void {
        self.indent_level -= 1;
    }

    // --- Comment interleaving ---

    /// Flush all comments whose start position is before `before_pos`.
    /// Leading comments get their own line at current indent.
    fn flushCommentsBefore(self: *Formatter, before_pos: usize) Error!void {
        while (self.comment_cursor < self.comments.len) {
            const comment = self.comments[self.comment_cursor];
            if (comment.start >= before_pos) break;

            if (comment.is_trailing) {
                // Trailing comment: put on same line with two spaces
                try self.write("  ");
                try self.write(comment.text);
                try self.newline();
            } else {
                // Leading comment: own line at current indent
                try self.writeIndent();
                try self.write(comment.text);
                try self.newline();
            }
            self.comment_cursor += 1;
        }
    }

    /// Write a trailing comment if one exists for the current position.
    fn flushTrailingComment(self: *Formatter, after_pos: usize) Error!void {
        if (self.comment_cursor < self.comments.len) {
            const comment = self.comments[self.comment_cursor];
            if (comment.is_trailing and comment.start >= after_pos) {
                // Check it's on the same line: look for newline between after_pos and comment.start
                const search_end = @min(comment.start, self.source.len);
                if (after_pos > search_end) return;
                const between = self.source[after_pos..search_end];
                const has_newline = std.mem.indexOfScalar(u8, between, '\n') != null;
                if (!has_newline) {
                    try self.write("  ");
                    try self.write(comment.text);
                    self.comment_cursor += 1;
                }
            }
        }
    }

    // --- Source span helper ---

    fn sourceSlice(self: *Formatter, s: Span) []const u8 {
        std.debug.assert(s.start <= s.end and s.end <= self.source.len);
        return self.source[s.start..s.end];
    }

    // --- Meta Annotation Formatting ---

    fn formatMetaAnnotations(self: *Formatter, meta: []const ast.MetaAnnotation) Error!void {
        for (meta) |annotation| {
            try self.writeIndent();
            try self.formatOneMetaAnnotation(annotation);
            try self.newline();
        }
    }

    fn formatOneMetaAnnotation(self: *Formatter, ann: ast.MetaAnnotation) Error!void {
        switch (ann) {
            .intent => |s| {
                try self.write("meta intent(");
                try self.writeMetaString(s.value);
                try self.writeByte(')');
            },
            .decision => |s| {
                try self.write("meta decision(");
                try self.writeMetaString(s.value);
                try self.writeByte(')');
            },
            .tag => |s| {
                try self.write("meta tag(");
                try self.writeMetaString(s.value);
                try self.writeByte(')');
            },
            .hint => |s| {
                try self.write("meta hint(");
                try self.writeMetaString(s.value);
                try self.writeByte(')');
            },
            .deprecated => |s| {
                try self.write("meta deprecated(");
                try self.writeMetaString(s.value);
                try self.writeByte(')');
            },
            .pure => {
                try self.write("meta pure");
            },
            .module_meta => |block| {
                try self.write("meta module {");
                try self.newline();
                try self.formatMetaBlockEntries(block.entries);
                try self.writeIndent();
                try self.writeByte('}');
            },
            .guide => |block| {
                try self.write("meta guide {");
                try self.newline();
                try self.formatMetaBlockEntries(block.entries);
                try self.writeIndent();
                try self.writeByte('}');
            },
            .related => |rel| {
                try self.write("meta related(");
                for (rel.paths, 0..) |path, i| {
                    if (i > 0) try self.write(", ");
                    try self.formatMetaPath(path);
                }
                if (rel.description) |desc| {
                    if (rel.paths.len > 0) try self.write(", ");
                    try self.writeMetaString(desc);
                }
                try self.writeByte(')');
            },
            .group_def => |group| {
                try self.write("meta group ");
                try self.writeMetaString(group.name);
                try self.write(" {");
                try self.newline();
                self.indent();
                for (group.annotations) |nested| {
                    try self.flushCommentsBefore(nested.span().start);
                    try self.writeIndent();
                    try self.formatOneMetaAnnotation(nested);
                    try self.newline();
                }
                self.dedent();
                try self.writeIndent();
                try self.writeByte('}');
            },
            .group_join => |s| {
                try self.write("meta in(");
                try self.writeMetaString(s.value);
                try self.writeByte(')');
            },
            .define => |def| {
                try self.write("meta define ");
                try self.write(def.name);
                try self.writeByte('(');
                for (def.params, 0..) |param, i| {
                    if (i > 0) try self.write(", ");
                    try self.write(param.name);
                    try self.write(": ");
                    switch (param.type_constraint) {
                        .string_type => try self.write("string"),
                        .path_type => try self.write("path"),
                        .string_union => |values| {
                            for (values, 0..) |val, j| {
                                if (j > 0) try self.write(" | ");
                                try self.writeMetaString(val);
                            }
                        },
                    }
                }
                try self.writeByte(')');
                if (def.scope) |scope| {
                    try self.write(" for ");
                    try self.write(switch (scope) {
                        .fn_scope => "fn",
                        .module_scope => "module",
                        .struct_scope => "struct",
                        .enum_scope => "enum",
                        .trait_scope => "trait",
                        .field_scope => "field",
                        .variant_scope => "variant",
                        .test_scope => "test",
                    });
                }
            },
            .custom => |cust| {
                try self.write("meta ");
                try self.write(cust.name);
                try self.writeByte('(');
                for (cust.args, 0..) |arg, i| {
                    if (i > 0) try self.write(", ");
                    switch (arg) {
                        .string => |s| try self.writeMetaString(s),
                        .path => |p| try self.formatMetaPath(p),
                    }
                }
                try self.writeByte(')');
            },
            .require => |contract| {
                try self.write("meta require(");
                try self.formatExpr(contract.expr);
                try self.writeByte(')');
            },
            .ensure => |contract| {
                try self.write("meta ensure(");
                try self.formatExpr(contract.expr);
                try self.writeByte(')');
            },
        }
    }

    fn formatMetaBlockEntries(self: *Formatter, entries: []const ast.MetaKeyValue) Error!void {
        self.indent();
        for (entries, 0..) |entry, i| {
            try self.writeIndent();
            try self.write(entry.key);
            try self.write(": ");
            switch (entry.value) {
                .string => |s| try self.writeMetaString(s),
                .string_list => |items| {
                    try self.writeByte('[');
                    for (items, 0..) |item, j| {
                        if (j > 0) try self.write(", ");
                        try self.writeMetaString(item);
                    }
                    try self.writeByte(']');
                },
            }
            if (i + 1 < entries.len) {
                try self.writeByte(',');
            }
            try self.newline();
        }
        self.dedent();
    }

    fn formatMetaPath(self: *Formatter, path: ast.MetaPath) Error!void {
        for (path.segments, 0..) |seg, i| {
            if (i > 0) try self.write("::");
            try self.write(seg);
        }
    }

    fn writeMetaString(self: *Formatter, value: []const u8) Error!void {
        try self.writeByte('"');
        for (value) |c| {
            switch (c) {
                '"' => try self.write("\\\""),
                '\\' => try self.write("\\\\"),
                '\n' => try self.write("\\n"),
                '\r' => try self.write("\\r"),
                '\t' => try self.write("\\t"),
                0 => try self.write("\\0"),
                else => {
                    if (c < 0x20) {
                        // Escape remaining control characters
                        var esc_buf: [6]u8 = undefined;
                        const esc = std.fmt.bufPrint(&esc_buf, "\\u{x:0>4}", .{@as(u16, c)}) catch unreachable;
                        try self.write(esc);
                    } else {
                        try self.writeByte(c);
                    }
                },
            }
        }
        try self.writeByte('"');
    }

    // --- Format Module (entry point) ---

    pub fn formatModule(self: *Formatter, module: ast.Module) Error!void {
        // Module declaration
        if (module.module_decl) |mod_decl| {
            try self.flushCommentsBefore(mod_decl.span.start);
            try self.write("module ");
            for (mod_decl.path, 0..) |seg, i| {
                if (i > 0) try self.writeByte('.');
                try self.write(seg);
            }
            try self.newline();
            try self.newline();
        }

        // Imports (sorted alphabetically)
        if (module.imports.len > 0) {
            // Sort imports by path
            const sorted_imports = try self.allocator.alloc(ast.ImportDecl, module.imports.len);
            defer self.allocator.free(sorted_imports);
            @memcpy(sorted_imports, module.imports);

            std.mem.sort(ast.ImportDecl, sorted_imports, {}, struct {
                fn lessThan(_: void, a: ast.ImportDecl, b: ast.ImportDecl) bool {
                    // Compare by first path segment, then second, etc.
                    const min_len = @min(a.path.len, b.path.len);
                    for (a.path[0..min_len], b.path[0..min_len]) |as, bs| {
                        const order = std.mem.order(u8, as, bs);
                        if (order == .lt) return true;
                        if (order == .gt) return false;
                    }
                    return a.path.len < b.path.len;
                }
            }.lessThan);

            for (sorted_imports) |imp| {
                try self.flushCommentsBefore(imp.span.start);
                try self.formatImport(imp);
                try self.newline();
            }
            try self.newline();
        }

        // File-level meta annotations (meta module, meta group)
        if (module.file_meta.len > 0) {
            for (module.file_meta) |annotation| {
                try self.flushCommentsBefore(annotation.span().start);
                try self.formatOneMetaAnnotation(annotation);
                try self.newline();
            }
            if (module.declarations.len > 0) {
                try self.newline();
            }
        }

        // Declarations with blank lines between them
        for (module.declarations, 0..) |decl, i| {
            if (i > 0) {
                // Blank line between top-level declarations
                // Check if source had extra blank lines to preserve visual grouping
                const prev_end = module.declarations[i - 1].span().end;
                const curr_start = decl.span().start;
                const blank_lines = countBlankLinesBetween(self.source, prev_end, curr_start);
                if (blank_lines > 1) {
                    // Preserve one extra blank line for visual grouping
                    try self.newline();
                }
                try self.newline();
            }
            try self.flushCommentsBefore(decl.span().start);
            try self.formatDecl(decl);
            try self.newline();
        }

        // Flush any remaining comments (at end of file)
        while (self.comment_cursor < self.comments.len) {
            const comment = self.comments[self.comment_cursor];
            try self.writeIndent();
            try self.write(comment.text);
            try self.newline();
            self.comment_cursor += 1;
        }

        // Verify all comments were consumed
        std.debug.assert(self.comment_cursor == self.comments.len);
    }

    // --- Declarations ---

    fn formatDecl(self: *Formatter, decl: ast.Decl) Error!void {
        switch (decl) {
            .function => |f| try self.formatFunction(f, false),
            .test_decl => |t| try self.formatTestDecl(t),
            .struct_decl => |s| try self.formatStruct(s),
            .enum_decl => |e| try self.formatEnum(e),
            .trait_decl => |t| try self.formatTrait(t),
            .impl_decl => |imp| try self.formatImpl(imp),
            .type_alias => |ta| try self.formatTypeAlias(ta),
            .const_decl => |c| try self.formatConst(c),
            .import_decl => |i| try self.formatImportDecl(i),
            .module_decl => |m| try self.formatModuleDecl(m),
            .extern_type_decl => |e| try self.formatExternType(e),
            .extern_block => |b| try self.formatExternBlock(b),
        }
    }

    fn formatTestDecl(self: *Formatter, test_decl: *const ast.TestDecl) Error!void {
        try self.formatMetaAnnotations(test_decl.meta);
        try self.writeIndent();
        try self.write("test ");
        try self.write(test_decl.name);
        try self.write(" ");
        try self.formatBraceBlock(test_decl.body);
    }

    fn formatFunction(self: *Formatter, func: *const ast.FunctionDecl, in_trait: bool) Error!void {
        try self.formatMetaAnnotations(func.meta);
        try self.writeIndent();
        if (func.is_pub) try self.write("pub ");
        if (func.is_async) try self.write("async ");
        if (func.is_unsafe) try self.write("unsafe ");
        if (func.is_extern) try self.write("extern ");
        if (func.is_comptime) {
            try self.write("fn @");
            try self.write(func.name);
        } else {
            try self.write("fn ");
            try self.write(func.name);
        }

        // Type parameters
        if (func.type_params.len > 0) {
            try self.write("#[");
            for (func.type_params, 0..) |tp, i| {
                if (i > 0) try self.write(", ");
                try self.write(tp.name);
                if (tp.bounds.len > 0) {
                    try self.write(": ");
                    for (tp.bounds, 0..) |bound, j| {
                        if (j > 0) try self.write(" + ");
                        try self.formatTypeExpr(bound);
                    }
                }
            }
            try self.writeByte(']');
        }

        // Parameters
        try self.writeByte('(');
        for (func.params, 0..) |param, i| {
            if (i > 0) try self.write(", ");
            if (param.is_out) try self.write("out ");
            if (param.is_comptime) try self.writeByte('@');
            try self.write(param.name);
            try self.write(": ");
            try self.formatTypeExpr(param.type_);
            if (param.default_value) |def| {
                try self.write(" = ");
                try self.formatExpr(def);
            }
        }
        if (func.is_variadic) {
            if (func.params.len > 0) try self.write(", ");
            try self.write("...");
        }
        try self.writeByte(')');

        // Return type
        if (func.return_type) |ret| {
            try self.write(" -> ");
            try self.formatTypeExpr(ret);
        }

        // Where clause
        if (func.where_clause) |where| {
            try self.write(" where ");
            for (where, 0..) |constraint, i| {
                if (i > 0) try self.write(", ");
                try self.write(constraint.type_param);
                try self.write(": ");
                for (constraint.bounds, 0..) |bound, j| {
                    if (j > 0) try self.write(" + ");
                    try self.formatTypeExpr(bound);
                }
            }
        }

        // Body
        if (func.body) |body| {
            try self.write(" {");
            try self.newline();
            try self.formatBlockBody(body);
            try self.writeIndent();
            try self.writeByte('}');
        } else if (in_trait) {
            // Trait method declaration without body - no semicolon in Klar
            // nothing more to write
        }
    }

    fn formatStruct(self: *Formatter, s: *const ast.StructDecl) Error!void {
        try self.formatMetaAnnotations(s.meta);
        try self.writeIndent();
        if (s.is_pub) try self.write("pub ");
        if (s.is_extern) try self.write("extern ");
        if (s.is_packed) try self.write("packed ");
        try self.write("struct ");
        try self.write(s.name);

        // Type parameters
        if (s.type_params.len > 0) {
            try self.write("#[");
            for (s.type_params, 0..) |tp, i| {
                if (i > 0) try self.write(", ");
                try self.write(tp.name);
                if (tp.bounds.len > 0) {
                    try self.write(": ");
                    for (tp.bounds, 0..) |bound, j| {
                        if (j > 0) try self.write(" + ");
                        try self.formatTypeExpr(bound);
                    }
                }
            }
            try self.writeByte(']');
        }

        // Traits (struct S: Trait1 + Trait2)
        if (s.traits.len > 0) {
            try self.write(": ");
            for (s.traits, 0..) |t, i| {
                if (i > 0) try self.write(" + ");
                try self.formatTypeExpr(t);
            }
        }

        try self.write(" {");
        try self.newline();
        self.indent();

        for (s.fields) |field| {
            try self.flushCommentsBefore(field.span.start);
            try self.formatMetaAnnotations(field.meta);
            try self.writeIndent();
            if (field.is_pub) try self.write("pub ");
            try self.write(field.name);
            try self.write(": ");
            try self.formatTypeExpr(field.type_);
            try self.newline();
        }

        self.dedent();
        try self.writeIndent();
        try self.writeByte('}');
    }

    fn formatEnum(self: *Formatter, e: *const ast.EnumDecl) Error!void {
        try self.formatMetaAnnotations(e.meta);
        try self.writeIndent();
        if (e.is_pub) try self.write("pub ");
        if (e.is_extern) try self.write("extern ");
        try self.write("enum ");
        try self.write(e.name);

        // Type parameters
        if (e.type_params.len > 0) {
            try self.write("#[");
            for (e.type_params, 0..) |tp, i| {
                if (i > 0) try self.write(", ");
                try self.write(tp.name);
                if (tp.bounds.len > 0) {
                    try self.write(": ");
                    for (tp.bounds, 0..) |bound, j| {
                        if (j > 0) try self.write(" + ");
                        try self.formatTypeExpr(bound);
                    }
                }
            }
            try self.writeByte(']');
        }

        // Repr type for extern enums
        if (e.repr_type) |repr| {
            try self.write(": ");
            try self.formatTypeExpr(repr);
        }

        try self.write(" {");
        try self.newline();
        self.indent();

        for (e.variants) |variant| {
            try self.flushCommentsBefore(variant.span.start);
            try self.formatMetaAnnotations(variant.meta);
            try self.writeIndent();
            try self.write(variant.name);

            if (variant.payload) |payload| {
                switch (payload) {
                    .tuple => |types| {
                        try self.writeByte('(');
                        for (types, 0..) |t, j| {
                            if (j > 0) try self.write(", ");
                            try self.formatTypeExpr(t);
                        }
                        try self.writeByte(')');
                    },
                    .struct_ => |fields| {
                        try self.write(" {");
                        try self.newline();
                        self.indent();
                        for (fields) |field| {
                            try self.writeIndent();
                            if (field.is_pub) try self.write("pub ");
                            try self.write(field.name);
                            try self.write(": ");
                            try self.formatTypeExpr(field.type_);
                            try self.newline();
                        }
                        self.dedent();
                        try self.writeIndent();
                        try self.writeByte('}');
                    },
                }
            }

            if (variant.value) |val| {
                try self.write(" = ");
                // Format the discriminant value
                var buf: [32]u8 = undefined;
                const formatted = std.fmt.bufPrint(&buf, "{d}", .{val}) catch "?";
                try self.write(formatted);
            }

            try self.writeByte(',');
            try self.newline();
        }

        self.dedent();
        try self.writeIndent();
        try self.writeByte('}');
    }

    fn formatTrait(self: *Formatter, t: *const ast.TraitDecl) Error!void {
        try self.formatMetaAnnotations(t.meta);
        try self.writeIndent();
        if (t.is_pub) try self.write("pub ");
        if (t.is_unsafe) try self.write("unsafe ");
        try self.write("trait ");
        try self.write(t.name);

        // Type parameters
        if (t.type_params.len > 0) {
            try self.write("#[");
            for (t.type_params, 0..) |tp, i| {
                if (i > 0) try self.write(", ");
                try self.write(tp.name);
                if (tp.bounds.len > 0) {
                    try self.write(": ");
                    for (tp.bounds, 0..) |bound, j| {
                        if (j > 0) try self.write(" + ");
                        try self.formatTypeExpr(bound);
                    }
                }
            }
            try self.writeByte(']');
        }

        // Super traits
        if (t.super_traits.len > 0) {
            try self.write(": ");
            for (t.super_traits, 0..) |st, i| {
                if (i > 0) try self.write(" + ");
                try self.formatTypeExpr(st);
            }
        }

        try self.write(" {");
        try self.newline();
        self.indent();

        // Associated types
        for (t.associated_types) |assoc| {
            try self.flushCommentsBefore(assoc.span.start);
            try self.writeIndent();
            try self.write("type ");
            try self.write(assoc.name);
            if (assoc.bounds.len > 0) {
                try self.write(": ");
                for (assoc.bounds, 0..) |bound, j| {
                    if (j > 0) try self.write(" + ");
                    try self.formatTypeExpr(bound);
                }
            }
            if (assoc.default) |def| {
                try self.write(" = ");
                try self.formatTypeExpr(def);
            }
            try self.newline();
        }

        // Methods
        for (t.methods, 0..) |method, i| {
            if (i > 0 or t.associated_types.len > 0) {
                // Check source for blank line preservation
                if (i > 0) {
                    const prev_end = t.methods[i - 1].span.end;
                    const curr_start = method.span.start;
                    const between = self.source[prev_end..curr_start];
                    const newline_count = std.mem.count(u8, between, "\n");
                    if (newline_count > 1) try self.newline();
                } else {
                    try self.newline();
                }
            }
            try self.flushCommentsBefore(method.span.start);
            try self.formatFunction(&method, true);
            try self.newline();
        }

        self.dedent();
        try self.writeIndent();
        try self.writeByte('}');
    }

    fn formatImpl(self: *Formatter, imp: *const ast.ImplDecl) Error!void {
        try self.formatMetaAnnotations(imp.meta);
        try self.writeIndent();
        if (imp.is_unsafe) try self.write("unsafe ");
        try self.write("impl");

        // Type parameters
        if (imp.type_params.len > 0) {
            try self.write("#[");
            for (imp.type_params, 0..) |tp, i| {
                if (i > 0) try self.write(", ");
                try self.write(tp.name);
                if (tp.bounds.len > 0) {
                    try self.write(": ");
                    for (tp.bounds, 0..) |bound, j| {
                        if (j > 0) try self.write(" + ");
                        try self.formatTypeExpr(bound);
                    }
                }
            }
            try self.writeByte(']');
        }

        try self.writeByte(' ');
        try self.formatTypeExpr(imp.target_type);

        // Trait type
        if (imp.trait_type) |trait_t| {
            try self.write(": ");
            try self.formatTypeExpr(trait_t);
        }

        // Where clause
        if (imp.where_clause) |where| {
            try self.write(" where ");
            for (where, 0..) |constraint, i| {
                if (i > 0) try self.write(", ");
                try self.write(constraint.type_param);
                try self.write(": ");
                for (constraint.bounds, 0..) |bound, j| {
                    if (j > 0) try self.write(" + ");
                    try self.formatTypeExpr(bound);
                }
            }
        }

        try self.write(" {");
        try self.newline();
        self.indent();

        // Associated type bindings
        for (imp.associated_types) |assoc| {
            try self.flushCommentsBefore(assoc.span.start);
            try self.writeIndent();
            try self.write("type ");
            try self.write(assoc.name);
            try self.write(" = ");
            try self.formatTypeExpr(assoc.value);
            try self.newline();
        }

        // Methods
        for (imp.methods, 0..) |method, i| {
            if (i > 0 or imp.associated_types.len > 0) {
                if (i > 0) {
                    const prev_end = imp.methods[i - 1].span.end;
                    const curr_start = method.span.start;
                    const between = self.source[prev_end..curr_start];
                    const newline_count = std.mem.count(u8, between, "\n");
                    if (newline_count > 1) try self.newline();
                } else {
                    try self.newline();
                }
            }
            try self.flushCommentsBefore(method.span.start);
            try self.formatFunction(&method, false);
            try self.newline();
        }

        self.dedent();
        try self.writeIndent();
        try self.writeByte('}');
    }

    fn formatTypeAlias(self: *Formatter, ta: *const ast.TypeAlias) Error!void {
        try self.formatMetaAnnotations(ta.meta);
        try self.writeIndent();
        if (ta.is_pub) try self.write("pub ");
        try self.write("type ");
        try self.write(ta.name);

        if (ta.type_params.len > 0) {
            try self.write("#[");
            for (ta.type_params, 0..) |tp, i| {
                if (i > 0) try self.write(", ");
                try self.write(tp.name);
            }
            try self.writeByte(']');
        }

        try self.write(" = ");
        try self.formatTypeExpr(ta.target);
    }

    fn formatConst(self: *Formatter, c: *const ast.ConstDecl) Error!void {
        try self.formatMetaAnnotations(c.meta);
        try self.writeIndent();
        if (c.is_pub) try self.write("pub ");
        try self.write("const ");
        try self.write(c.name);
        if (c.type_) |t| {
            try self.write(": ");
            try self.formatTypeExpr(t);
        }
        try self.write(" = ");
        try self.formatExpr(c.value);
    }

    fn formatImport(self: *Formatter, imp: ast.ImportDecl) Error!void {
        try self.writeIndent();
        try self.write("import ");
        for (imp.path, 0..) |seg, i| {
            if (i > 0) try self.writeByte('.');
            try self.write(seg);
        }

        if (imp.items) |items| {
            switch (items) {
                .all => try self.write(".*"),
                .specific => |specs| {
                    try self.write(".{ ");
                    // Sort items alphabetically
                    const sorted = try self.allocator.alloc(ast.ImportItem, specs.len);
                    defer self.allocator.free(sorted);
                    @memcpy(sorted, specs);
                    std.mem.sort(ast.ImportItem, sorted, {}, struct {
                        fn lessThan(_: void, a: ast.ImportItem, b: ast.ImportItem) bool {
                            return std.mem.order(u8, a.name, b.name) == .lt;
                        }
                    }.lessThan);
                    for (sorted, 0..) |item, i| {
                        if (i > 0) try self.write(", ");
                        try self.write(item.name);
                        if (item.alias) |alias| {
                            try self.write(" as ");
                            try self.write(alias);
                        }
                    }
                    try self.write(" }");
                },
            }
        }

        if (imp.alias) |alias| {
            try self.write(" as ");
            try self.write(alias);
        }
    }

    fn formatImportDecl(self: *Formatter, imp: *const ast.ImportDecl) Error!void {
        try self.formatImport(imp.*);
    }

    fn formatModuleDecl(self: *Formatter, mod: *const ast.ModuleDecl) Error!void {
        try self.writeIndent();
        try self.write("module ");
        for (mod.path, 0..) |seg, i| {
            if (i > 0) try self.writeByte('.');
            try self.write(seg);
        }
    }

    fn formatExternType(self: *Formatter, e: *const ast.ExternTypeDecl) Error!void {
        try self.writeIndent();
        if (e.is_pub) try self.write("pub ");
        try self.write("extern type");
        if (e.size) |size| {
            try self.writeByte('(');
            var buf: [20]u8 = undefined;
            const formatted = std.fmt.bufPrint(&buf, "{d}", .{size}) catch "?";
            try self.write(formatted);
            try self.writeByte(')');
        }
        try self.writeByte(' ');
        try self.write(e.name);
    }

    fn formatExternBlock(self: *Formatter, b: *const ast.ExternBlock) Error!void {
        try self.writeIndent();
        try self.write("extern {");
        try self.newline();
        self.indent();

        for (b.functions) |func| {
            try self.flushCommentsBefore(func.span.start);
            try self.formatFunction(func, false);
            try self.newline();
        }

        self.dedent();
        try self.writeIndent();
        try self.writeByte('}');
    }

    // --- Statements ---

    fn formatStmt(self: *Formatter, stmt: ast.Stmt) Error!void {
        switch (stmt) {
            .let_decl => |l| try self.formatLetDecl(l),
            .var_decl => |v| try self.formatVarDecl(v),
            .assignment => |a| try self.formatAssignment(a),
            .expr_stmt => |e| try self.formatExprStmt(e),
            .return_stmt => |r| try self.formatReturnStmt(r),
            .break_stmt => |b| try self.formatBreakStmt(b),
            .continue_stmt => |_| try self.formatContinueStmt(),
            .for_loop => |f| try self.formatForLoop(f),
            .while_loop => |w| try self.formatWhileLoop(w),
            .loop_stmt => |l| try self.formatLoopStmt(l),
            .if_stmt => |ifs| try self.formatIfStmt(ifs),
            .match_stmt => |m| try self.formatMatchStmt(m),
        }
    }

    fn formatLetDecl(self: *Formatter, l: *const ast.LetDecl) Error!void {
        try self.writeIndent();
        if (l.is_shadow) try self.write("shadow ");
        try self.write("let ");
        try self.write(l.name);
        try self.write(": ");
        try self.formatTypeExpr(l.type_);
        try self.write(" = ");
        try self.formatExpr(l.value);
    }

    fn formatVarDecl(self: *Formatter, v: *const ast.VarDecl) Error!void {
        try self.writeIndent();
        if (v.is_shadow) try self.write("shadow ");
        try self.write("var ");
        try self.write(v.name);
        try self.write(": ");
        try self.formatTypeExpr(v.type_);
        try self.write(" = ");
        try self.formatExpr(v.value);
    }

    fn formatAssignment(self: *Formatter, a: *const ast.Assignment) Error!void {
        try self.writeIndent();
        try self.formatExpr(a.target);
        try self.writeByte(' ');
        try self.write(binaryOpStr(a.op));
        try self.writeByte(' ');
        try self.formatExpr(a.value);
    }

    fn formatExprStmt(self: *Formatter, e: *const ast.ExprStmt) Error!void {
        try self.writeIndent();
        try self.formatExpr(e.expr);
    }

    fn formatReturnStmt(self: *Formatter, r: *const ast.ReturnStmt) Error!void {
        try self.writeIndent();
        try self.write("return");
        if (r.value) |val| {
            try self.writeByte(' ');
            try self.formatExpr(val);
        }
    }

    fn formatBreakStmt(self: *Formatter, b: *const ast.BreakStmt) Error!void {
        try self.writeIndent();
        try self.write("break");
        if (b.value) |val| {
            try self.writeByte(' ');
            try self.formatExpr(val);
        }
    }

    fn formatContinueStmt(self: *Formatter) Error!void {
        try self.writeIndent();
        try self.write("continue");
    }

    fn formatForLoop(self: *Formatter, f: *const ast.ForLoop) Error!void {
        try self.writeIndent();
        try self.write("for ");
        try self.formatPattern(f.pattern);
        try self.write(" in ");
        try self.formatExpr(f.iterable);
        try self.write(" {");
        try self.newline();
        try self.formatBlockBody(f.body);
        try self.writeIndent();
        try self.writeByte('}');
    }

    fn formatWhileLoop(self: *Formatter, w: *const ast.WhileLoop) Error!void {
        try self.writeIndent();
        try self.write("while ");
        try self.formatExpr(w.condition);
        try self.write(" {");
        try self.newline();
        try self.formatBlockBody(w.body);
        try self.writeIndent();
        try self.writeByte('}');
    }

    fn formatLoopStmt(self: *Formatter, l: *const ast.LoopStmt) Error!void {
        try self.writeIndent();
        try self.write("loop {");
        try self.newline();
        try self.formatBlockBody(l.body);
        try self.writeIndent();
        try self.writeByte('}');
    }

    fn formatIfStmt(self: *Formatter, ifs: *const ast.IfStmt) Error!void {
        try self.writeIndent();
        try self.writeIfStmtInner(ifs);
    }

    fn writeIfStmtInner(self: *Formatter, ifs: *const ast.IfStmt) Error!void {
        try self.write("if ");
        try self.formatExpr(ifs.condition);
        try self.write(" {");
        try self.flushTrailingComment(ifs.condition.span().end);
        try self.newline();
        try self.formatBlockBody(ifs.then_branch);
        try self.writeIndent();
        try self.writeByte('}');

        if (ifs.else_branch) |else_branch| {
            try self.write(" else ");
            switch (else_branch.*) {
                .block => |block| {
                    try self.writeByte('{');
                    try self.newline();
                    try self.formatBlockBody(block);
                    try self.writeIndent();
                    try self.writeByte('}');
                },
                .if_stmt => |nested_if| {
                    try self.writeIfStmtInner(nested_if);
                },
            }
        }
    }

    fn formatMatchStmt(self: *Formatter, m: *const ast.MatchStmt) Error!void {
        try self.writeIndent();
        try self.write("match ");
        try self.formatExpr(m.subject);
        try self.write(" {");
        try self.newline();
        self.indent();

        for (m.arms) |arm| {
            try self.flushCommentsBefore(arm.span.start);
            try self.writeIndent();
            try self.formatPattern(arm.pattern);
            if (arm.guard) |guard| {
                try self.write(" if ");
                try self.formatExpr(guard);
            }
            try self.write(" => ");
            try self.formatBraceBlock(arm.body);
            try self.flushTrailingComment(arm.span.end);
            try self.newline();
        }

        self.dedent();
        try self.writeIndent();
        try self.writeByte('}');
    }

    // --- Block body helper ---

    /// Format a brace-delimited block. Empty blocks become `{ }`,
    /// simple single-statement blocks go inline `{ stmt }`,
    /// multi-statement or complex blocks get newlines.
    fn formatBraceBlock(self: *Formatter, block: *const ast.Block) Error!void {
        if (block.statements.len == 0 and block.final_expr == null) {
            try self.write("{ }");
            return;
        }
        if (block.statements.len == 1 and block.final_expr == null and isSimpleStmt(block.statements[0])) {
            try self.write("{ ");
            try self.formatStmtInline(block.statements[0]);
            try self.write(" }");
            return;
        }
        if (block.statements.len == 0 and block.final_expr != null) {
            // Block with only a final expression (e.g., match arm `{ r = 1 }`)
            try self.write("{ ");
            try self.formatExpr(block.final_expr.?);
            try self.write(" }");
            return;
        }
        try self.writeByte('{');
        try self.newline();
        try self.formatBlockBody(block);
        try self.writeIndent();
        try self.writeByte('}');
    }

    /// Check if a statement is simple enough for inline formatting.
    fn isSimpleStmt(stmt: ast.Stmt) bool {
        return switch (stmt) {
            .return_stmt, .break_stmt, .continue_stmt, .expr_stmt, .assignment => true,
            .let_decl, .var_decl => true,
            else => false,
        };
    }

    fn formatBlockBody(self: *Formatter, block: *const ast.Block) Error!void {
        self.indent();

        for (block.statements, 0..) |stmt, i| {
            // Preserve blank lines between statements
            if (i > 0) {
                const prev_end = block.statements[i - 1].span().end;
                const curr_start = stmt.span().start;
                const blank_lines = countBlankLinesBetween(self.source, prev_end, curr_start);
                if (blank_lines > 0) try self.newline();
            }
            try self.flushCommentsBefore(stmt.span().start);
            try self.formatStmt(stmt);
            try self.flushTrailingComment(stmt.span().end);
            try self.newline();
        }

        if (block.final_expr) |final_expr| {
            try self.flushCommentsBefore(final_expr.span().start);
            try self.writeIndent();
            try self.formatExpr(final_expr);
            try self.newline();
        }

        self.dedent();
    }

    // --- Expressions ---

    fn formatExpr(self: *Formatter, expr: ast.Expr) Error!void {
        switch (expr) {
            .literal => |l| try self.formatLiteral(l),
            .identifier => |id| try self.write(id.name),
            .binary => |b| try self.formatBinary(b),
            .unary => |u| try self.formatUnary(u),
            .postfix => |p| try self.formatPostfix(p),
            .call => |c| try self.formatCall(c),
            .index => |idx| try self.formatIndex(idx),
            .field => |f| try self.formatField(f),
            .method_call => |mc| try self.formatMethodCall(mc),
            .block => |b| try self.formatBlockExpr(b),
            .closure => |c| try self.formatClosure(c),
            .range => |r| try self.formatRange(r),
            .struct_literal => |s| try self.formatStructLiteral(s),
            .array_literal => |a| try self.formatArrayLiteral(a),
            .tuple_literal => |t| try self.formatTupleLiteral(t),
            .type_cast => |tc| try self.formatTypeCast(tc),
            .grouped => |g| try self.formatGrouped(g),
            .interpolated_string => |is| try self.formatInterpolatedString(is),
            .enum_literal => |el| try self.formatEnumLiteral(el),
            .comptime_block => |cb| try self.formatComptimeBlock(cb),
            .builtin_call => |bc| try self.formatBuiltinCall(bc),
            .unsafe_block => |ub| try self.formatUnsafeBlock(ub),
            .out_arg => |oa| try self.formatOutArg(oa),
        }
    }

    fn formatLiteral(self: *Formatter, lit: ast.Literal) Error!void {
        // Use source span to preserve original formatting (hex, binary, underscores)
        try self.write(self.sourceSlice(lit.span));
    }

    fn formatBinary(self: *Formatter, b: *const ast.Binary) Error!void {
        try self.formatExpr(b.left);
        try self.writeByte(' ');
        try self.write(binaryOpStr(b.op));
        try self.writeByte(' ');
        try self.formatExpr(b.right);
    }

    fn formatUnary(self: *Formatter, u: *const ast.Unary) Error!void {
        switch (u.op) {
            .negate => {
                try self.writeByte('-');
                try self.formatExpr(u.operand);
            },
            .not => {
                try self.write("not ");
                try self.formatExpr(u.operand);
            },
            .await_ => {
                try self.write("await ");
                try self.formatExpr(u.operand);
            },
            .ref => {
                try self.write("ref ");
                try self.formatExpr(u.operand);
            },
            .ref_mut => {
                try self.write("ref ");
                try self.formatExpr(u.operand);
            },
            .deref => {
                try self.writeByte('*');
                try self.formatExpr(u.operand);
            },
        }
    }

    fn formatPostfix(self: *Formatter, p: *const ast.Postfix) Error!void {
        try self.formatExpr(p.operand);
        switch (p.op) {
            .unwrap => try self.writeByte('?'),
            .force_unwrap => try self.writeByte('!'),
        }
    }

    fn formatCall(self: *Formatter, c: *const ast.Call) Error!void {
        try self.formatExpr(c.callee);
        if (c.type_args) |type_args| {
            try self.write("#[");
            for (type_args, 0..) |ta, i| {
                if (i > 0) try self.write(", ");
                try self.formatTypeExpr(ta);
            }
            try self.writeByte(']');
        }
        try self.writeByte('(');
        for (c.args, 0..) |arg, i| {
            if (i > 0) try self.write(", ");
            try self.formatExpr(arg);
        }
        try self.writeByte(')');
    }

    fn formatIndex(self: *Formatter, idx: *const ast.Index) Error!void {
        try self.formatExpr(idx.object);
        try self.writeByte('[');
        try self.formatExpr(idx.index);
        try self.writeByte(']');
    }

    fn formatField(self: *Formatter, f: *const ast.Field) Error!void {
        try self.formatExpr(f.object);
        try self.writeByte('.');
        try self.write(f.field_name);
    }

    fn formatMethodCall(self: *Formatter, mc: *const ast.MethodCall) Error!void {
        try self.formatExpr(mc.object);
        try self.writeByte('.');
        try self.write(mc.method_name);
        if (mc.type_args) |type_args| {
            try self.write("#[");
            for (type_args, 0..) |ta, i| {
                if (i > 0) try self.write(", ");
                try self.formatTypeExpr(ta);
            }
            try self.writeByte(']');
        }
        try self.writeByte('(');
        for (mc.args, 0..) |arg, i| {
            if (i > 0) try self.write(", ");
            try self.formatExpr(arg);
        }
        try self.writeByte(')');
    }

    fn formatBlockExpr(self: *Formatter, b: *const ast.Block) Error!void {
        try self.writeByte('{');
        try self.newline();
        try self.formatBlockBody(b);
        try self.writeIndent();
        try self.writeByte('}');
    }

    fn formatClosure(self: *Formatter, c: *const ast.Closure) Error!void {
        try self.writeByte('|');
        for (c.params, 0..) |param, i| {
            if (i > 0) try self.write(", ");
            try self.write(param.name);
            try self.write(": ");
            try self.formatTypeExpr(param.type_);
        }
        try self.write("| -> ");
        try self.formatTypeExpr(c.return_type);
        try self.write(" { ");

        // Format closure body inline if simple, block otherwise
        switch (c.body) {
            .block => |block| {
                if (block.statements.len <= 1 and block.final_expr == null) {
                    // Simple single-statement closure - inline
                    if (block.statements.len == 1) {
                        try self.formatStmtInline(block.statements[0]);
                    }
                    try self.write(" }");
                } else {
                    // Multi-statement closure
                    try self.newline();
                    try self.formatBlockBody(block);
                    try self.writeIndent();
                    try self.writeByte('}');
                }
            },
            else => {
                try self.formatExpr(c.body);
                try self.write(" }");
            },
        }
    }

    /// Format a statement without indent (for inline closures)
    fn formatStmtInline(self: *Formatter, stmt: ast.Stmt) Error!void {
        switch (stmt) {
            .return_stmt => |r| {
                try self.write("return");
                if (r.value) |val| {
                    try self.writeByte(' ');
                    try self.formatExpr(val);
                }
            },
            .expr_stmt => |e| try self.formatExpr(e.expr),
            else => try self.formatStmt(stmt),
        }
    }

    fn formatRange(self: *Formatter, r: *const ast.Range) Error!void {
        if (r.start) |start| try self.formatExpr(start);
        if (r.inclusive) {
            try self.write("..=");
        } else {
            try self.write("..");
        }
        if (r.end) |end| try self.formatExpr(end);
    }

    fn formatStructLiteral(self: *Formatter, s: *const ast.StructLiteral) Error!void {
        if (s.type_name) |tn| {
            try self.formatTypeExpr(tn);
            try self.writeByte(' ');
        }
        try self.write("{ ");
        for (s.fields, 0..) |field, i| {
            if (i > 0) try self.write(", ");
            try self.write(field.name);
            try self.write(": ");
            try self.formatExpr(field.value);
        }
        if (s.spread) |spread| {
            if (s.fields.len > 0) try self.write(", ");
            try self.write("..");
            try self.formatExpr(spread);
        }
        try self.write(" }");
    }

    fn formatArrayLiteral(self: *Formatter, a: *const ast.ArrayLiteral) Error!void {
        try self.writeByte('[');
        for (a.elements, 0..) |elem, i| {
            if (i > 0) try self.write(", ");
            try self.formatExpr(elem);
        }
        try self.writeByte(']');
    }

    fn formatTupleLiteral(self: *Formatter, t: *const ast.TupleLiteral) Error!void {
        try self.writeByte('(');
        for (t.elements, 0..) |elem, i| {
            if (i > 0) try self.write(", ");
            try self.formatExpr(elem);
        }
        try self.writeByte(')');
    }

    fn formatTypeCast(self: *Formatter, tc: *const ast.TypeCast) Error!void {
        try self.formatExpr(tc.expr);
        if (tc.truncating) {
            try self.write(".trunc#[");
        } else {
            try self.write(".as#[");
        }
        try self.formatTypeExpr(tc.target_type);
        try self.writeByte(']');
    }

    fn formatGrouped(self: *Formatter, g: *const ast.Grouped) Error!void {
        try self.writeByte('(');
        try self.formatExpr(g.expr);
        try self.writeByte(')');
    }

    fn formatInterpolatedString(self: *Formatter, is: *const ast.InterpolatedString) Error!void {
        // Preserve the original interpolated string from source
        try self.write(self.sourceSlice(is.span));
    }

    fn formatEnumLiteral(self: *Formatter, el: *const ast.EnumLiteral) Error!void {
        try self.formatTypeExpr(el.enum_type);
        try self.write("::");
        try self.write(el.variant_name);
        if (el.payload.len > 0) {
            try self.writeByte('(');
            for (el.payload, 0..) |p, i| {
                if (i > 0) try self.write(", ");
                try self.formatExpr(p);
            }
            try self.writeByte(')');
        }
    }

    fn formatComptimeBlock(self: *Formatter, cb: *const ast.ComptimeBlock) Error!void {
        try self.write("@{");
        // Check if it's a simple single-expression comptime block
        if (cb.body.statements.len == 0 and cb.body.final_expr != null) {
            try self.writeByte(' ');
            try self.formatExpr(cb.body.final_expr.?);
            try self.write(" }");
        } else {
            try self.newline();
            try self.formatBlockBody(cb.body);
            try self.writeIndent();
            try self.writeByte('}');
        }
    }

    fn formatBuiltinCall(self: *Formatter, bc: *const ast.BuiltinCall) Error!void {
        try self.writeByte('@');
        try self.write(bc.name);
        try self.writeByte('(');
        for (bc.args, 0..) |arg, i| {
            if (i > 0) try self.write(", ");
            switch (arg) {
                .type_arg => |ta| try self.formatTypeExpr(ta),
                .expr_arg => |ea| try self.formatExpr(ea),
            }
        }
        try self.writeByte(')');
    }

    fn formatUnsafeBlock(self: *Formatter, ub: *const ast.UnsafeBlock) Error!void {
        try self.write("unsafe {");
        try self.newline();
        try self.formatBlockBody(ub.body);
        try self.writeIndent();
        try self.writeByte('}');
    }

    fn formatOutArg(self: *Formatter, oa: *const ast.OutArg) Error!void {
        try self.write("out ");
        try self.write(oa.name);
    }

    // --- Patterns ---

    fn formatPattern(self: *Formatter, pat: ast.Pattern) Error!void {
        switch (pat) {
            .wildcard => try self.writeByte('_'),
            .literal => |l| try self.write(self.sourceSlice(l.span)),
            .binding => |b| {
                if (b.mutable) try self.write("var ");
                try self.write(b.name);
                if (b.type_annotation) |ta| {
                    try self.write(": ");
                    try self.formatTypeExpr(ta);
                }
            },
            .variant => |v| {
                if (v.type_expr) |te| {
                    try self.formatTypeExpr(te);
                    try self.writeByte('.');
                }
                try self.write(v.variant_name);
                if (v.payload) |payload| {
                    try self.writeByte('(');
                    try self.formatPattern(payload);
                    try self.writeByte(')');
                }
            },
            .struct_pattern => |sp| {
                if (sp.type_name) |tn| {
                    try self.write(tn);
                    try self.writeByte(' ');
                }
                try self.write("{ ");
                for (sp.fields, 0..) |field, i| {
                    if (i > 0) try self.write(", ");
                    try self.write(field.name);
                    if (field.pattern) |p| {
                        try self.write(": ");
                        try self.formatPattern(p);
                    }
                }
                try self.write(" }");
            },
            .tuple_pattern => |tp| {
                try self.writeByte('(');
                for (tp.elements, 0..) |elem, i| {
                    if (i > 0) try self.write(", ");
                    try self.formatPattern(elem);
                }
                try self.writeByte(')');
            },
            .or_pattern => |op| {
                for (op.alternatives, 0..) |alt, i| {
                    if (i > 0) try self.write(" | ");
                    try self.formatPattern(alt);
                }
            },
            .guarded => |gp| {
                try self.formatPattern(gp.pattern);
                try self.write(" if ");
                try self.formatExpr(gp.guard);
            },
        }
    }

    // --- Type Expressions ---

    fn formatTypeExpr(self: *Formatter, te: ast.TypeExpr) Error!void {
        switch (te) {
            .named => |n| try self.write(n.name),
            .array => |a| {
                try self.writeByte('[');
                try self.formatTypeExpr(a.element);
                try self.write("; ");
                try self.formatExpr(a.size);
                try self.writeByte(']');
            },
            .slice => |s| {
                try self.writeByte('[');
                try self.formatTypeExpr(s.element);
                try self.writeByte(']');
            },
            .tuple => |t| {
                try self.writeByte('(');
                for (t.elements, 0..) |elem, i| {
                    if (i > 0) try self.write(", ");
                    try self.formatTypeExpr(elem);
                }
                try self.writeByte(')');
            },
            .optional => |o| {
                try self.writeByte('?');
                try self.formatTypeExpr(o.inner);
            },
            .result => |r| {
                try self.write("Result#[");
                try self.formatTypeExpr(r.ok_type);
                try self.write(", ");
                try self.formatTypeExpr(r.err_type);
                try self.writeByte(']');
            },
            .function => |f| {
                try self.write("fn(");
                for (f.params, 0..) |param, i| {
                    if (i > 0) try self.write(", ");
                    try self.formatTypeExpr(param);
                }
                try self.write(") -> ");
                try self.formatTypeExpr(f.return_type);
            },
            .extern_function => |ef| {
                try self.write("extern fn(");
                for (ef.params, 0..) |param, i| {
                    if (i > 0) try self.write(", ");
                    try self.formatTypeExpr(param);
                }
                try self.write(") -> ");
                try self.formatTypeExpr(ef.return_type);
            },
            .reference => |r| {
                if (r.mutable) {
                    try self.write("inout ");
                } else {
                    try self.write("ref ");
                }
                try self.formatTypeExpr(r.inner);
            },
            .generic_apply => |g| {
                try self.formatTypeExpr(g.base);
                try self.write("#[");
                for (g.args, 0..) |arg, i| {
                    if (i > 0) try self.write(", ");
                    try self.formatTypeExpr(arg);
                }
                try self.writeByte(']');
            },
            .qualified => |q| {
                try self.formatTypeExpr(q.base);
                try self.writeByte('.');
                try self.write(q.member);
            },
        }
    }

    // --- Helper: binary op to string ---

    fn binaryOpStr(op: ast.BinaryOp) []const u8 {
        return switch (op) {
            .add => "+",
            .sub => "-",
            .mul => "*",
            .div => "/",
            .mod => "%",
            .add_wrap => "+%",
            .sub_wrap => "-%",
            .mul_wrap => "*%",
            .add_sat => "+|",
            .sub_sat => "-|",
            .mul_sat => "*|",
            .eq => "==",
            .not_eq => "!=",
            .lt => "<",
            .gt => ">",
            .lt_eq => "<=",
            .gt_eq => ">=",
            .is => "is",
            .and_ => "and",
            .or_ => "or",
            .bit_and => "&",
            .bit_or => "|",
            .bit_xor => "^",
            .shl => "<<",
            .shr => ">>",
            .null_coalesce => "??",
            .assign => "=",
            .add_assign => "+=",
            .sub_assign => "-=",
            .mul_assign => "*=",
            .div_assign => "/=",
            .mod_assign => "%=",
        };
    }
};

// ============================================================================
// Helpers
// ============================================================================

/// Count blank lines in source between two positions, ignoring comments.
/// A "blank line" is a line containing only whitespace.
fn countBlankLinesBetween(source: []const u8, start: usize, end: usize) u32 {
    if (start >= end or start >= source.len) return 0;
    const clamped_end = @min(end, source.len);
    const between = source[start..clamped_end];

    var blank_lines: u32 = 0;
    var consecutive_newlines: u32 = 0;

    for (between) |ch| {
        if (ch == '\n') {
            consecutive_newlines += 1;
            if (consecutive_newlines >= 2) {
                blank_lines += 1;
            }
        } else if (ch != ' ' and ch != '\t' and ch != '\r') {
            // Non-whitespace character resets the counter
            consecutive_newlines = 0;
        }
    }

    return blank_lines;
}

// ============================================================================
// Public API
// ============================================================================

/// Format a Klar source file. Returns the formatted source as a string.
/// Caller owns the returned memory.
/// On parse errors, writes details to stderr before returning error.ParseError.
pub fn format(allocator: Allocator, source: []const u8) ![]u8 {
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

    // Extract comments from raw source
    const comments = try extractComments(allocator, source);
    defer allocator.free(comments);

    // Parse the source
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var lexer = Lexer.init(source);
    var parser = Parser.init(arena.allocator(), &lexer, source);

    const module = parser.parseModule() catch {
        // Write parse error details to stderr before returning
        const stderr: std.fs.File = if (comptime builtin.os.tag == .windows)
            .{ .handle = std.os.windows.kernel32.GetStdHandle(std.os.windows.STD_ERROR_HANDLE) orelse
                @panic("failed to get stderr handle") }
        else
            .{ .handle = std.posix.STDERR_FILENO };
        var buf: [512]u8 = undefined;
        for (parser.errors.items) |parse_err| {
            const msg = std.fmt.bufPrint(&buf, "  {d}:{d}: {s}\n", .{
                parse_err.span.line,
                parse_err.span.column,
                parse_err.message,
            }) catch continue;
            stderr.writeAll(msg) catch {};
        }
        return error.ParseError;
    };

    // Format
    var formatter = Formatter.init(allocator, source, comments, .{});
    defer formatter.deinit();

    try formatter.formatModule(module);

    // Ensure file ends with exactly one newline
    const output = formatter.getOutput();
    var result_len = output.len;
    // Trim trailing newlines
    while (result_len > 0 and output[result_len - 1] == '\n') {
        result_len -= 1;
    }
    // Allocate result with exactly one trailing newline
    const result = try allocator.alloc(u8, result_len + 1);
    @memcpy(result[0..result_len], output[0..result_len]);
    result[result_len] = '\n';

    return result;
}

// ============================================================================
// Tests
// ============================================================================

test "extractComments: line comments" {
    const source = "let x: i32 = 42 // trailing\n// leading\nlet y: i32 = 5";
    const comments = try extractComments(std.testing.allocator, source);
    defer std.testing.allocator.free(comments);

    try std.testing.expectEqual(@as(usize, 2), comments.len);
    try std.testing.expectEqualStrings("// trailing", comments[0].text);
    try std.testing.expect(comments[0].is_trailing);
    try std.testing.expectEqualStrings("// leading", comments[1].text);
    try std.testing.expect(!comments[1].is_trailing);
}

test "extractComments: block comments" {
    const source = "let x: i32 = /* inline */ 42";
    const comments = try extractComments(std.testing.allocator, source);
    defer std.testing.allocator.free(comments);

    try std.testing.expectEqual(@as(usize, 1), comments.len);
    try std.testing.expectEqualStrings("/* inline */", comments[0].text);
}

test "extractComments: unterminated block comment" {
    const source = "let x: i32 = /* unterminated";
    const comments = try extractComments(std.testing.allocator, source);
    defer std.testing.allocator.free(comments);

    try std.testing.expectEqual(@as(usize, 1), comments.len);
    try std.testing.expectEqualStrings("/* unterminated", comments[0].text);
}

test "extractComments: block comment at end of source" {
    const source = "/**/";
    const comments = try extractComments(std.testing.allocator, source);
    defer std.testing.allocator.free(comments);

    try std.testing.expectEqual(@as(usize, 1), comments.len);
    try std.testing.expectEqualStrings("/**/", comments[0].text);
}

test "extractComments: empty source" {
    const source = "";
    const comments = try extractComments(std.testing.allocator, source);
    defer std.testing.allocator.free(comments);

    try std.testing.expectEqual(@as(usize, 0), comments.len);
}

test "extractComments: comments in strings are ignored" {
    const source =
        \\let s: string = "hello // not a comment"
        \\// real comment
    ;
    const comments = try extractComments(std.testing.allocator, source);
    defer std.testing.allocator.free(comments);

    try std.testing.expectEqual(@as(usize, 1), comments.len);
    try std.testing.expectEqualStrings("// real comment", comments[0].text);
}

test "format: simple function" {
    const source = "fn  main()  ->  i32  {\n  return   42\n}\n";
    const result = try format(std.testing.allocator, source);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings("fn main() -> i32 {\n    return 42\n}\n", result);
}

test "binaryOpStr covers all variants" {
    // Just ensure it doesn't crash for all variants
    const ops = [_]ast.BinaryOp{
        .add,           .sub,        .mul,        .div,        .mod,
        .add_wrap,      .sub_wrap,   .mul_wrap,   .add_sat,    .sub_sat,
        .mul_sat,       .eq,         .not_eq,     .lt,         .gt,
        .lt_eq,         .gt_eq,      .is,         .and_,       .or_,
        .bit_and,       .bit_or,     .bit_xor,    .shl,        .shr,
        .null_coalesce, .assign,     .add_assign, .sub_assign, .mul_assign,
        .div_assign,    .mod_assign,
    };
    for (ops) |op| {
        const s = Formatter.binaryOpStr(op);
        try std.testing.expect(s.len > 0);
    }
}

test "extractComments: multiple comments at same line" {
    const source = "let x: i32 = 42 // first\n// second\n// third\n";
    const comments = try extractComments(std.testing.allocator, source);
    defer std.testing.allocator.free(comments);

    try std.testing.expectEqual(@as(usize, 3), comments.len);
    try std.testing.expect(comments[0].is_trailing);
    try std.testing.expect(!comments[1].is_trailing);
    try std.testing.expect(!comments[2].is_trailing);
    // Comments should be in source order
    try std.testing.expect(comments[0].start < comments[1].start);
    try std.testing.expect(comments[1].start < comments[2].start);
}

test "extractComments: comment inside char literal ignored" {
    const source = "let c: char = '/' // real\n";
    const comments = try extractComments(std.testing.allocator, source);
    defer std.testing.allocator.free(comments);

    try std.testing.expectEqual(@as(usize, 1), comments.len);
    try std.testing.expectEqualStrings("// real", comments[0].text);
}

test "extractComments: consecutive block comments" {
    const source = "/* a *//* b */";
    const comments = try extractComments(std.testing.allocator, source);
    defer std.testing.allocator.free(comments);

    try std.testing.expectEqual(@as(usize, 2), comments.len);
    try std.testing.expectEqualStrings("/* a */", comments[0].text);
    try std.testing.expectEqualStrings("/* b */", comments[1].text);
}

test "extractComments: slash not followed by slash or star" {
    const source = "let x: i32 = a / b\n";
    const comments = try extractComments(std.testing.allocator, source);
    defer std.testing.allocator.free(comments);

    try std.testing.expectEqual(@as(usize, 0), comments.len);
}

test "extractComments: whitespace only source" {
    const source = "   \t\n\n  \t  \n";
    const comments = try extractComments(std.testing.allocator, source);
    defer std.testing.allocator.free(comments);

    try std.testing.expectEqual(@as(usize, 0), comments.len);
}

test "countBlankLinesBetween: no blank lines" {
    const source = "abc\ndef\n";
    try std.testing.expectEqual(@as(u32, 0), countBlankLinesBetween(source, 0, source.len));
}

test "countBlankLinesBetween: one blank line" {
    const source = "abc\n\ndef\n";
    try std.testing.expectEqual(@as(u32, 1), countBlankLinesBetween(source, 0, source.len));
}

test "countBlankLinesBetween: multiple blank lines" {
    const source = "abc\n\n\n\ndef\n";
    try std.testing.expectEqual(@as(u32, 3), countBlankLinesBetween(source, 0, source.len));
}

test "countBlankLinesBetween: start >= end returns 0" {
    const source = "abc\n\ndef\n";
    try std.testing.expectEqual(@as(u32, 0), countBlankLinesBetween(source, 5, 3));
    try std.testing.expectEqual(@as(u32, 0), countBlankLinesBetween(source, 5, 5));
}

test "countBlankLinesBetween: start >= source.len returns 0" {
    const source = "abc";
    try std.testing.expectEqual(@as(u32, 0), countBlankLinesBetween(source, 100, 200));
}

test "countBlankLinesBetween: blank lines with whitespace" {
    const source = "abc\n  \t  \ndef\n";
    // Line with only whitespace between two newlines counts as blank
    try std.testing.expectEqual(@as(u32, 1), countBlankLinesBetween(source, 0, source.len));
}

test "format: empty function body" {
    const source = "fn noop() {}\n";
    const result = try format(std.testing.allocator, source);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings("fn noop() {}\n", result);
}

test "format: idempotence" {
    const source = "fn add(a: i32, b: i32) -> i32 {\n    return a + b\n}\n";
    const first = try format(std.testing.allocator, source);
    defer std.testing.allocator.free(first);

    const second = try format(std.testing.allocator, first);
    defer std.testing.allocator.free(second);

    try std.testing.expectEqualStrings(first, second);
}

test "format: trailing newline normalization" {
    const source = "fn main() {}\n\n\n";
    const result = try format(std.testing.allocator, source);
    defer std.testing.allocator.free(result);

    // Should end with exactly one newline
    try std.testing.expect(result.len > 0);
    try std.testing.expectEqual(@as(u8, '\n'), result[result.len - 1]);
    if (result.len > 1) {
        try std.testing.expect(result[result.len - 2] != '\n');
    }
}
