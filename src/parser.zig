const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("token.zig").Token;
const Lexer = @import("lexer.zig").Lexer;
const ast = @import("ast.zig");

pub const ParseError = error{
    UnexpectedToken,
    ExpectedExpression,
    ExpectedIdentifier,
    ExpectedType,
    ExpectedPattern,
    ExpectedClosingParen,
    ExpectedClosingBracket,
    ExpectedClosingBrace,
    ExpectedColon,
    ExpectedArrow,
    ExpectedFatArrow,
    ExpectedEquals,
    UnterminatedString,
    InvalidNumber,
    InvalidCharacter,
    OutOfMemory,
};

/// Find the next unescaped '{' starting from pos.
/// Returns null if no unescaped '{' is found.
fn findUnescapedBrace(content: []const u8, start: usize) ?usize {
    var pos = start;
    while (pos < content.len) {
        if (content[pos] == '\\' and pos + 1 < content.len) {
            // Skip escaped character (including \{ and \})
            pos += 2;
            continue;
        }
        if (content[pos] == '{') {
            return pos;
        }
        pos += 1;
    }
    return null;
}

/// Process escape sequences in a string segment.
/// Converts \{ to {, \} to }, and other standard escapes.
fn processEscapes(allocator: Allocator, content: []const u8) ![]const u8 {
    // Quick check: if no backslashes, return as-is
    if (std.mem.indexOfScalar(u8, content, '\\') == null) {
        return content;
    }

    var result = std.ArrayListUnmanaged(u8){};
    var i: usize = 0;
    while (i < content.len) {
        if (content[i] == '\\' and i + 1 < content.len) {
            const next = content[i + 1];
            const escaped: u8 = switch (next) {
                '{' => '{',
                '}' => '}',
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                '\\' => '\\',
                '"' => '"',
                '0' => 0,
                else => next, // Unknown escape, keep as-is
            };
            try result.append(allocator, escaped);
            i += 2;
        } else {
            try result.append(allocator, content[i]);
            i += 1;
        }
    }
    return result.items;
}

pub const Parser = struct {
    allocator: Allocator,
    lexer: *Lexer,
    source: []const u8,
    current: Token,
    previous: Token,
    errors: std.ArrayListUnmanaged(Error),

    pub const Error = struct {
        message: []const u8,
        span: ast.Span,
    };

    pub fn init(allocator: Allocator, lexer: *Lexer, source: []const u8) Parser {
        var parser = Parser{
            .allocator = allocator,
            .lexer = lexer,
            .source = source,
            .current = undefined,
            .previous = undefined,
            .errors = .{},
        };
        // Prime the parser with the first token
        parser.advance();
        return parser;
    }

    pub fn deinit(self: *Parser) void {
        self.errors.deinit(self.allocator);
    }

    // ========================================================================
    // Core parsing utilities
    // ========================================================================

    fn advance(self: *Parser) void {
        self.previous = self.current;
        self.current = self.lexer.next();

        // Skip newlines in certain contexts
        while (self.current.kind == .newline) {
            self.current = self.lexer.next();
        }
    }

    fn check(self: *Parser, kind: Token.Kind) bool {
        return self.current.kind == kind;
    }

    fn peekNext(self: *Parser) Token {
        // Peek at the next token without consuming current
        // Save lexer state
        const saved_pos = self.lexer.pos;
        const saved_line = self.lexer.line;
        const saved_column = self.lexer.column;

        // Get next token
        var next = self.lexer.next();
        // Skip newlines like advance() does
        while (next.kind == .newline) {
            next = self.lexer.next();
        }

        // Restore lexer state
        self.lexer.pos = saved_pos;
        self.lexer.line = saved_line;
        self.lexer.column = saved_column;

        return next;
    }

    /// Check if a token kind can start a type expression.
    /// Used to distinguish between type arguments (foo.method[T]()) and
    /// array subscripts (foo.array[0]).
    fn canStartType(kind: Token.Kind) bool {
        return switch (kind) {
            .identifier, // Named types: i32, String, MyType
            .question, // Optional: ?T
            .ref, // Reference: ref T
            .inout, // Mutable reference: inout T
            .l_bracket, // Array/slice: [T] or [T; N]
            .l_paren, // Tuple: (T1, T2)
            .fn_, // Function: fn(Args) -> Ret
            .self_type, // Self type
            => true,
            else => false,
        };
    }

    fn match(self: *Parser, kind: Token.Kind) bool {
        if (!self.check(kind)) return false;
        self.advance();
        return true;
    }

    fn consume(self: *Parser, kind: Token.Kind, comptime message: []const u8) ParseError!void {
        if (self.check(kind)) {
            self.advance();
            return;
        }
        try self.reportError(message);
        return ParseError.UnexpectedToken;
    }

    fn consumeIdentifier(self: *Parser) ParseError![]const u8 {
        if (self.check(.identifier)) {
            const name = self.tokenText(self.current);
            self.advance();
            return name;
        }
        try self.reportError("expected identifier");
        return ParseError.ExpectedIdentifier;
    }

    fn tokenText(self: *Parser, token: Token) []const u8 {
        return self.source[token.loc.start..token.loc.end];
    }

    fn spanFromToken(self: *Parser, token: Token) ast.Span {
        _ = self;
        return ast.Span.from(token);
    }

    fn reportError(self: *Parser, message: []const u8) ParseError!void {
        try self.errors.append(self.allocator, .{
            .message = message,
            .span = ast.Span.from(self.current),
        });
    }

    // ========================================================================
    // Allocation helpers
    // ========================================================================

    fn create(self: *Parser, comptime T: type, value: T) ParseError!*T {
        const ptr = self.allocator.create(T) catch return ParseError.OutOfMemory;
        ptr.* = value;
        return ptr;
    }

    fn dupeSlice(self: *Parser, comptime T: type, items: []const T) ParseError![]const T {
        return self.allocator.dupe(T, items) catch return ParseError.OutOfMemory;
    }

    // ========================================================================
    // Expression Parsing (Pratt Parser)
    // ========================================================================

    /// Precedence levels from the Klar spec (higher = binds tighter)
    const Precedence = enum(u8) {
        none = 0,
        assignment = 1, // = += -= etc.
        null_coalesce = 2, // ??
        or_ = 3, // or
        and_ = 4, // and
        comparison = 5, // == != < > <= >= is
        range = 6, // .. ..=
        bit_or = 7, // | (bitwise)
        bit_xor = 8, // ^
        bit_and = 9, // & (bitwise)
        shift = 10, // << >>
        term = 11, // + - +% -% +| -|
        factor = 12, // * / % *% *|
        unary = 13, // - not & &mut *
        postfix = 14, // ? !
        call = 15, // . () []

        fn next(self: Precedence) Precedence {
            if (@intFromEnum(self) >= @intFromEnum(Precedence.call)) {
                return self;
            }
            return @enumFromInt(@intFromEnum(self) + 1);
        }
    };

    fn getPrecedence(kind: Token.Kind) Precedence {
        return switch (kind) {
            .eq, .plus_eq, .minus_eq, .star_eq, .slash_eq, .percent_eq => .assignment,
            .question_question => .null_coalesce,
            .or_ => .or_,
            .and_ => .and_,
            .eq_eq, .not_eq, .lt, .gt, .lt_eq, .gt_eq, .is => .comparison,
            .dot_dot, .dot_dot_eq => .range,
            .pipe => .bit_or,
            .caret => .bit_xor,
            .amp => .bit_and,
            .lt_lt, .gt_gt => .shift,
            .plus, .minus, .plus_wrap, .minus_wrap, .plus_sat, .minus_sat => .term,
            .star, .slash, .percent, .star_wrap, .star_sat => .factor,
            .question, .bang => .postfix,
            .dot, .l_paren, .l_bracket => .call,
            else => .none,
        };
    }

    /// Parse an expression with Pratt parsing
    pub fn parseExpression(self: *Parser) ParseError!ast.Expr {
        return self.parsePrecedence(.assignment);
    }

    fn parsePrecedence(self: *Parser, min_prec: Precedence) ParseError!ast.Expr {
        // Parse prefix expression
        var left = try self.parsePrefix();

        // Parse infix expressions while precedence is high enough
        while (true) {
            const prec = getPrecedence(self.current.kind);
            if (@intFromEnum(prec) < @intFromEnum(min_prec)) {
                break;
            }
            left = try self.parseInfix(left, prec);
        }

        return left;
    }

    fn parsePrefix(self: *Parser) ParseError!ast.Expr {
        return switch (self.current.kind) {
            // Literals
            .int_literal => self.parseIntLiteral(),
            .float_literal => self.parseFloatLiteral(),
            .string_literal => self.parseStringLiteral(),
            .char_literal => self.parseCharLiteral(),
            .true_, .false_ => self.parseBoolLiteral(),

            // Identifier (including 'self' keyword when used as expression)
            .identifier => self.parseIdentifierOrStructLiteral(),
            .self => self.parseSelfExpression(),

            // Grouping / tuple
            .l_paren => self.parseGroupedOrTuple(),

            // Array literal
            .l_bracket => self.parseArrayLiteral(),

            // Block expression
            .l_brace => self.parseBlockExpr(),

            // Closure (pipe syntax only: |x| expr, |x: T| expr, |x: T| -> R { expr })
            .pipe => self.parseClosure(),

            // Unary operators
            .minus => self.parseUnary(.negate),
            .not => self.parseUnary(.not),
            .ref => self.parseRefExpr(),
            .star => self.parseUnary(.deref),

            // @ can be: comptime block @{ ... } or builtin call @name(...)
            .at => self.parseAtExpr(),

            // unsafe { ... } block expression
            .unsafe_ => self.parseUnsafeBlock(),

            else => {
                try self.reportError("expected expression");
                return ParseError.ExpectedExpression;
            },
        };
    }

    fn parseInfix(self: *Parser, left: ast.Expr, prec: Precedence) ParseError!ast.Expr {
        return switch (self.current.kind) {
            // Binary operators
            .plus,
            .minus,
            .star,
            .slash,
            .percent,
            .plus_wrap,
            .minus_wrap,
            .star_wrap,
            .plus_sat,
            .minus_sat,
            .star_sat,
            .eq_eq,
            .not_eq,
            .lt,
            .gt,
            .lt_eq,
            .gt_eq,
            .is,
            .and_,
            .or_,
            .amp,
            .pipe,
            .caret,
            .lt_lt,
            .gt_gt,
            .question_question,
            .eq,
            .plus_eq,
            .minus_eq,
            .star_eq,
            .slash_eq,
            .percent_eq,
            => self.parseBinary(left, prec),

            // Range
            .dot_dot, .dot_dot_eq => self.parseRange(left),

            // Postfix operators
            .question => self.parsePostfix(left, .unwrap),
            .bang => self.parsePostfix(left, .force_unwrap),

            // Call/index/field
            .l_paren => self.parseCall(left),
            .l_bracket => self.parseIndex(left),
            .dot => self.parseFieldOrMethod(left),

            else => left,
        };
    }

    // ========================================================================
    // Literal parsing
    // ========================================================================

    fn parseIntLiteral(self: *Parser) ParseError!ast.Expr {
        const span = self.spanFromToken(self.current);
        const text = self.tokenText(self.current);
        self.advance();

        // Parse the integer value
        const value = parseIntValue(text) catch {
            try self.reportError("invalid integer literal");
            return ParseError.InvalidNumber;
        };

        return .{ .literal = .{
            .kind = .{ .int = value },
            .span = span,
        } };
    }

    fn parseIntValue(text: []const u8) !i128 {
        var s = text;
        var base: u8 = 10;

        // Check for base prefix
        if (s.len >= 2 and s[0] == '0') {
            switch (s[1]) {
                'x', 'X' => {
                    base = 16;
                    s = s[2..];
                },
                'b', 'B' => {
                    base = 2;
                    s = s[2..];
                },
                'o', 'O' => {
                    base = 8;
                    s = s[2..];
                },
                else => {},
            }
        }

        // Remove underscores and type suffix
        var clean: [128]u8 = undefined;
        var len: usize = 0;
        for (s) |c| {
            if (c == '_') continue;
            if (std.ascii.isAlphabetic(c) and base != 16) break; // type suffix
            if (base == 16 and !std.ascii.isHex(c)) break;
            clean[len] = c;
            len += 1;
        }

        return std.fmt.parseInt(i128, clean[0..len], base);
    }

    fn parseFloatLiteral(self: *Parser) ParseError!ast.Expr {
        const span = self.spanFromToken(self.current);
        const text = self.tokenText(self.current);
        self.advance();

        // Remove underscores and type suffix
        var clean: [128]u8 = undefined;
        var len: usize = 0;
        for (text) |c| {
            if (c == '_') continue;
            // Stop at type suffix (f32, f64)
            if (c == 'f' and len > 0) break;
            clean[len] = c;
            len += 1;
        }

        const value = std.fmt.parseFloat(f64, clean[0..len]) catch {
            try self.reportError("invalid float literal");
            return ParseError.InvalidNumber;
        };

        return .{ .literal = .{
            .kind = .{ .float = value },
            .span = span,
        } };
    }

    fn parseStringLiteral(self: *Parser) ParseError!ast.Expr {
        const span = self.spanFromToken(self.current);
        const text = self.tokenText(self.current);
        self.advance();

        // Strip quotes (simple approach - full impl would process escapes)
        const content = if (text.len >= 2) text[1 .. text.len - 1] else text;

        // Check for interpolation (contains unescaped {)
        if (findUnescapedBrace(content, 0)) |_| {
            return self.parseInterpolatedString(content, span);
        }

        // Process escape sequences for non-interpolated strings
        const processed = processEscapes(self.allocator, content) catch content;
        return .{ .literal = .{
            .kind = .{ .string = processed },
            .span = span,
        } };
    }

    fn parseInterpolatedString(self: *Parser, content: []const u8, span: ast.Span) ParseError!ast.Expr {
        var parts = std.ArrayListUnmanaged(ast.InterpolatedPart){};
        var pos: usize = 0;

        while (pos < content.len) {
            // Find next unescaped '{'
            if (findUnescapedBrace(content, pos)) |brace_start| {
                // Add string segment before the brace (if any)
                if (brace_start > pos) {
                    const segment = content[pos..brace_start];
                    const processed = processEscapes(self.allocator, segment) catch segment;
                    try parts.append(self.allocator, .{ .string = processed });
                }

                // Find matching '}' (skipping escaped braces and tracking depth)
                var brace_depth: usize = 1;
                var expr_end: usize = brace_start + 1;
                while (expr_end < content.len and brace_depth > 0) {
                    if (content[expr_end] == '\\' and expr_end + 1 < content.len) {
                        // Skip escaped characters inside expressions
                        expr_end += 2;
                        continue;
                    }
                    if (content[expr_end] == '{') {
                        brace_depth += 1;
                    } else if (content[expr_end] == '}') {
                        brace_depth -= 1;
                    }
                    if (brace_depth > 0) expr_end += 1;
                }

                if (brace_depth != 0) {
                    try self.reportError("unmatched '{' in string interpolation");
                    return ParseError.UnterminatedString;
                }

                // Parse the expression inside { }
                const expr_text = content[brace_start + 1 .. expr_end];
                const expr = try self.parseExpressionFromString(expr_text);
                try parts.append(self.allocator, .{ .expr = expr });

                pos = expr_end + 1;
            } else {
                // No more unescaped braces, add remaining string
                if (pos < content.len) {
                    const segment = content[pos..];
                    const processed = processEscapes(self.allocator, segment) catch segment;
                    try parts.append(self.allocator, .{ .string = processed });
                }
                break;
            }
        }

        const interp = try self.create(ast.InterpolatedString, .{
            .parts = try self.dupeSlice(ast.InterpolatedPart, parts.items),
            .span = span,
        });
        return .{ .interpolated_string = interp };
    }

    fn parseExpressionFromString(self: *Parser, expr_text: []const u8) ParseError!ast.Expr {
        // Create a sub-lexer and sub-parser for the expression
        var lexer = Lexer.init(expr_text);
        var sub_parser = Parser.init(self.allocator, &lexer, expr_text);
        defer sub_parser.deinit();

        return sub_parser.parseExpression();
    }

    fn parseCharLiteral(self: *Parser) ParseError!ast.Expr {
        const span = self.spanFromToken(self.current);
        const text = self.tokenText(self.current);
        self.advance();

        // Parse character value (simple - full impl would handle escapes)
        var value: u21 = 0;
        if (text.len >= 3) {
            if (text[1] == '\\' and text.len >= 4) {
                // Escape sequence
                value = switch (text[2]) {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    '\\' => '\\',
                    '\'' => '\'',
                    '0' => 0,
                    else => text[2],
                };
            } else {
                value = text[1];
            }
        }

        return .{ .literal = .{
            .kind = .{ .char = value },
            .span = span,
        } };
    }

    fn parseBoolLiteral(self: *Parser) ParseError!ast.Expr {
        const span = self.spanFromToken(self.current);
        const value = self.current.kind == .true_;
        self.advance();

        return .{ .literal = .{
            .kind = .{ .bool_ = value },
            .span = span,
        } };
    }

    /// Parse 'self' as an expression (used in method bodies)
    fn parseSelfExpression(self: *Parser) ParseError!ast.Expr {
        const span = self.spanFromToken(self.current);
        self.advance(); // consume 'self'
        return .{ .identifier = .{
            .name = "self",
            .span = span,
        } };
    }

    fn parseIdentifierOrStructLiteral(self: *Parser) ParseError!ast.Expr {
        const start_span = self.spanFromToken(self.current);
        const name = self.tokenText(self.current);
        self.advance();

        // Check if this is an uppercase identifier (type name)
        const is_type_name = name.len > 0 and std.ascii.isUpper(name[0]);

        // Check for generic type: TypeName[T, U] ...
        if (is_type_name and self.check(.l_bracket)) {
            // Parse type arguments
            self.advance(); // consume '['

            var type_args = std.ArrayListUnmanaged(ast.TypeExpr){};
            while (!self.check(.r_bracket) and !self.check(.eof)) {
                const type_arg = try self.parseType();
                try type_args.append(self.allocator, type_arg);
                if (!self.match(.comma)) break;
            }
            try self.consume(.r_bracket, "expected ']' after type arguments");

            // Create GenericApply type expression
            const base_type: ast.TypeExpr = .{ .named = .{ .name = name, .span = start_span } };
            const generic_type = try self.create(ast.GenericApply, .{
                .base = base_type,
                .args = try self.dupeSlice(ast.TypeExpr, type_args.items),
                .span = ast.Span.merge(start_span, self.spanFromToken(self.previous)),
            });
            const full_type: ast.TypeExpr = .{ .generic_apply = generic_type };

            // Check for enum literal: TypeName[T]::VariantName or TypeName[T]::VariantName(payload)
            if (self.check(.colon_colon)) {
                return self.parseEnumLiteralWithType(full_type, start_span);
            }

            // Check for struct literal with generic type: TypeName[T, U] { ... }
            if (self.check(.l_brace)) {
                return self.parseStructLiteralWithType(full_type, start_span);
            }

            // Otherwise, this is not a valid expression context for a generic type
            try self.reportError("generic type application requires struct literal or enum variant");
            return ParseError.UnexpectedToken;
        }

        // Check for enum literal: TypeName::VariantName or TypeName::VariantName(payload)
        if (is_type_name and self.check(.colon_colon)) {
            const base_type: ast.TypeExpr = .{ .named = .{ .name = name, .span = start_span } };
            return self.parseEnumLiteralWithType(base_type, start_span);
        }

        // Check for struct literal: Identifier { ... }
        // Only parse as struct literal if the identifier starts with uppercase (type convention)
        // This prevents `x { ... }` from being parsed as struct literal in contexts like `if x { }`
        if (self.check(.l_brace) and is_type_name) {
            const base_type: ast.TypeExpr = .{ .named = .{ .name = name, .span = start_span } };
            return self.parseStructLiteralWithType(base_type, start_span);
        }

        return .{ .identifier = .{
            .name = name,
            .span = start_span,
        } };
    }

    /// Parse enum literal: EnumType::VariantName or EnumType::VariantName(payload)
    fn parseEnumLiteralWithType(self: *Parser, enum_type: ast.TypeExpr, start_span: ast.Span) ParseError!ast.Expr {
        self.advance(); // consume '::'

        // Parse variant name
        const variant_name = try self.consumeIdentifier();

        // Check for payload: (expr, ...) or ()
        var payload = std.ArrayListUnmanaged(ast.Expr){};
        if (self.match(.l_paren)) {
            if (!self.check(.r_paren)) {
                while (true) {
                    try payload.append(self.allocator, try self.parseExpression());
                    if (!self.match(.comma)) break;
                }
            }
            try self.consume(.r_paren, "expected ')' after variant payload");
        }

        const end_span = self.spanFromToken(self.previous);

        const enum_lit = try self.create(ast.EnumLiteral, .{
            .enum_type = enum_type,
            .variant_name = variant_name,
            .payload = try self.dupeSlice(ast.Expr, payload.items),
            .span = ast.Span.merge(start_span, end_span),
        });

        return .{ .enum_literal = enum_lit };
    }

    fn parseStructLiteralWithType(self: *Parser, type_expr: ast.TypeExpr, start_span: ast.Span) ParseError!ast.Expr {
        self.advance(); // consume '{'

        var fields = std.ArrayListUnmanaged(ast.StructFieldInit){};
        var spread: ?ast.Expr = null;

        while (!self.check(.r_brace) and !self.check(.eof)) {
            // Check for spread: ..expr
            if (self.match(.dot_dot)) {
                spread = try self.parseExpression();
                _ = self.match(.comma);
                break;
            }

            const field_name = try self.consumeIdentifier();
            const field_span = self.spanFromToken(self.previous);

            try self.consume(.colon, "expected ':' after field name");
            const value = try self.parseExpression();

            try fields.append(self.allocator, .{
                .name = field_name,
                .value = value,
                .span = field_span,
            });

            if (!self.match(.comma)) break;
        }

        try self.consume(.r_brace, "expected '}' after struct fields");
        const end_span = self.spanFromToken(self.previous);

        const struct_lit = try self.create(ast.StructLiteral, .{
            .type_name = type_expr,
            .fields = try self.dupeSlice(ast.StructFieldInit, fields.items),
            .spread = spread,
            .span = ast.Span.merge(start_span, end_span),
        });
        return .{ .struct_literal = struct_lit };
    }

    // ========================================================================
    // Compound expression parsing
    // ========================================================================

    fn parseGroupedOrTuple(self: *Parser) ParseError!ast.Expr {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume '('

        // Empty tuple
        if (self.check(.r_paren)) {
            const end_span = self.spanFromToken(self.current);
            self.advance();
            const tuple = try self.create(ast.TupleLiteral, .{
                .elements = &[_]ast.Expr{},
                .span = ast.Span.merge(start_span, end_span),
            });
            return .{ .tuple_literal = tuple };
        }

        const first = try self.parseExpression();

        // Single expression in parens
        if (self.check(.r_paren)) {
            const end_span = self.spanFromToken(self.current);
            self.advance();
            const grouped = try self.create(ast.Grouped, .{
                .expr = first,
                .span = ast.Span.merge(start_span, end_span),
            });
            return .{ .grouped = grouped };
        }

        // Tuple with multiple elements
        if (self.match(.comma)) {
            var elements = std.ArrayListUnmanaged(ast.Expr){};
            try elements.append(self.allocator, first);

            while (!self.check(.r_paren) and !self.check(.eof)) {
                try elements.append(self.allocator, try self.parseExpression());
                if (!self.match(.comma)) break;
            }

            try self.consume(.r_paren, "expected ')' after tuple elements");
            const end_span = self.spanFromToken(self.previous);

            const tuple = try self.create(ast.TupleLiteral, .{
                .elements = try self.dupeSlice(ast.Expr, elements.items),
                .span = ast.Span.merge(start_span, end_span),
            });
            return .{ .tuple_literal = tuple };
        }

        try self.consume(.r_paren, "expected ')' after expression");
        const end_span = self.spanFromToken(self.previous);
        const grouped = try self.create(ast.Grouped, .{
            .expr = first,
            .span = ast.Span.merge(start_span, end_span),
        });
        return .{ .grouped = grouped };
    }

    fn parseArrayLiteral(self: *Parser) ParseError!ast.Expr {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume '['

        var elements = std.ArrayListUnmanaged(ast.Expr){};

        while (!self.check(.r_bracket) and !self.check(.eof)) {
            try elements.append(self.allocator, try self.parseExpression());
            if (!self.match(.comma)) break;
        }

        try self.consume(.r_bracket, "expected ']' after array elements");
        const end_span = self.spanFromToken(self.previous);

        const arr = try self.create(ast.ArrayLiteral, .{
            .elements = try self.dupeSlice(ast.Expr, elements.items),
            .span = ast.Span.merge(start_span, end_span),
        });
        return .{ .array_literal = arr };
    }

    fn parseBlockExpr(self: *Parser) ParseError!ast.Expr {
        const block = try self.parseBlock();
        return .{ .block = block };
    }

    /// Parse @ expression: either @{ ... } (comptime block) or @name(...) (builtin call)
    fn parseAtExpr(self: *Parser) ParseError!ast.Expr {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume '@'

        // Check if this is a comptime block: @{ ... }
        if (self.check(.l_brace)) {
            const block = try self.parseBlock();
            const end_span = block.span;

            const comptime_block = try self.create(ast.ComptimeBlock, .{
                .body = block,
                .span = ast.Span.merge(start_span, end_span),
            });

            return .{ .comptime_block = comptime_block };
        }

        // Otherwise it's a builtin call: @name(...)
        const name = try self.consumeIdentifier();

        // Parse argument list
        try self.consume(.l_paren, "expected '(' after builtin name");

        var args = std.ArrayListUnmanaged(ast.BuiltinArg){};

        if (!self.check(.r_paren)) {
            while (true) {
                // Try to parse as type first, then fall back to expression
                // This is a bit tricky - types start with identifiers or certain keywords
                const arg = try self.parseBuiltinArg();
                try args.append(self.allocator, arg);

                if (!self.match(.comma)) break;
            }
        }

        const end_span = self.spanFromToken(self.current);
        try self.consume(.r_paren, "expected ')' after builtin arguments");

        const builtin_call = try self.create(ast.BuiltinCall, .{
            .name = name,
            .args = try self.dupeSlice(ast.BuiltinArg, args.items),
            .span = ast.Span.merge(start_span, end_span),
        });

        return .{ .builtin_call = builtin_call };
    }

    /// Parse unsafe { ... } block expression
    fn parseUnsafeBlock(self: *Parser) ParseError!ast.Expr {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'unsafe'

        // Expect '{'
        if (!self.check(.l_brace)) {
            try self.reportError("expected '{' after 'unsafe'");
            return ParseError.UnexpectedToken;
        }

        const block = try self.parseBlock();
        const end_span = block.span;

        const unsafe_block = try self.create(ast.UnsafeBlock, .{
            .body = block,
            .span = ast.Span.merge(start_span, end_span),
        });

        return .{ .unsafe_block = unsafe_block };
    }

    /// Parse a builtin argument - could be a type or an expression
    fn parseBuiltinArg(self: *Parser) ParseError!ast.BuiltinArg {
        // Try to detect if this looks like a type
        // Types can be: identifiers, Self, primitives, ?T, [T], fn(...) -> T, etc.
        // For simplicity, we'll try parsing as type first if it looks like one

        // If the current token is just an identifier, check what follows to disambiguate.
        // If followed by an operator, it's definitely an expression.
        // This handles cases like `@foo(n - 1)` where `n` should be parsed as expression.
        if (self.current.kind == .identifier) {
            const name = self.source[self.current.loc.start..self.current.loc.end];

            // Check if this is a known primitive type name - always parse as type
            const is_primitive = std.mem.eql(u8, name, "i8") or
                std.mem.eql(u8, name, "i16") or
                std.mem.eql(u8, name, "i32") or
                std.mem.eql(u8, name, "i64") or
                std.mem.eql(u8, name, "i128") or
                std.mem.eql(u8, name, "isize") or
                std.mem.eql(u8, name, "u8") or
                std.mem.eql(u8, name, "u16") or
                std.mem.eql(u8, name, "u32") or
                std.mem.eql(u8, name, "u64") or
                std.mem.eql(u8, name, "u128") or
                std.mem.eql(u8, name, "usize") or
                std.mem.eql(u8, name, "f32") or
                std.mem.eql(u8, name, "f64") or
                std.mem.eql(u8, name, "bool") or
                std.mem.eql(u8, name, "char") or
                std.mem.eql(u8, name, "string") or
                std.mem.eql(u8, name, "void");

            if (!is_primitive) {
                // Peek ahead to check what follows this non-primitive identifier
                // An identifier followed by a binary operator is definitely an expression
                const next = self.peekNext();
                switch (next.kind) {
                    // Binary operators indicate this is an expression
                    .plus, .minus, .star, .slash, .percent,
                    .eq_eq, .not_eq, .lt, .lt_eq, .gt, .gt_eq,
                    .and_, .or_,
                    // These also indicate expression continuation
                    .dot, .l_paren,
                    => return .{ .expr_arg = try self.parseExpression() },
                    // For comma/rparen, default to trying as type first
                    // This handles @typeInfo(UserType) correctly
                    // For [ after identifier, try as type first for generic types
                    else => {},
                }
            }
        }

        // For non-identifier tokens that could start a type, try type first
        const could_be_type = switch (self.current.kind) {
            .identifier => true, // Identifiers (including primitives and user types)
            .self_type => true,
            .l_bracket, .question => true,
            .fn_ => true,
            else => false,
        };

        if (could_be_type) {
            // Try to parse as type
            const type_expr = self.parseType() catch {
                // If type parsing fails, try as expression
                return .{ .expr_arg = try self.parseExpression() };
            };
            return .{ .type_arg = type_expr };
        } else {
            return .{ .expr_arg = try self.parseExpression() };
        }
    }

    fn parseBlock(self: *Parser) ParseError!*ast.Block {
        const start_span = self.spanFromToken(self.current);
        try self.consume(.l_brace, "expected '{'");

        var statements = std.ArrayListUnmanaged(ast.Stmt){};
        var final_expr: ?ast.Expr = null;

        while (!self.check(.r_brace) and !self.check(.eof)) {
            // Skip newlines inside blocks
            while (self.match(.newline)) {}
            if (self.check(.r_brace)) break;

            // Check if this is a statement keyword
            if (self.isStatementKeyword()) {
                const stmt = try self.parseStatement();
                try statements.append(self.allocator, stmt);
                _ = self.match(.newline) or self.match(.semicolon);
                continue;
            }

            // Try to parse as expression
            const expr = try self.parseExpression();

            // If followed by closing brace (no newline/semicolon), it's the final expression
            if (self.check(.r_brace)) {
                final_expr = expr;
                break;
            }

            // Otherwise it's an expression statement
            const stmt = try self.create(ast.ExprStmt, .{
                .expr = expr,
                .span = expr.span(),
            });
            try statements.append(self.allocator, .{ .expr_stmt = stmt });

            // Consume optional newline/semicolon
            _ = self.match(.newline) or self.match(.semicolon);
        }

        try self.consume(.r_brace, "expected '}'");
        const end_span = self.spanFromToken(self.previous);

        return self.create(ast.Block, .{
            .statements = try self.dupeSlice(ast.Stmt, statements.items),
            .final_expr = final_expr,
            .span = ast.Span.merge(start_span, end_span),
        });
    }

    fn isStatementKeyword(self: *Parser) bool {
        return switch (self.current.kind) {
            .let, .var_, .return_, .break_, .continue_, .for_, .while_, .loop, .if_, .match => true,
            else => false,
        };
    }

    fn parseIfStmt(self: *Parser) ParseError!ast.Stmt {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'if'

        const condition = try self.parseExpression();
        const then_block = try self.parseBlock();

        var else_branch: ?*ast.ElseBranch = null;
        if (self.match(.else_)) {
            if (self.check(.if_)) {
                // Recursively parse else-if
                const nested_if = try self.parseIfStmt();
                const else_b = try self.create(ast.ElseBranch, .{ .if_stmt = nested_if.if_stmt });
                else_branch = else_b;
            } else {
                const else_block = try self.parseBlock();
                const else_b = try self.create(ast.ElseBranch, .{ .block = else_block });
                else_branch = else_b;
            }
        }

        const end_span = if (else_branch) |eb| switch (eb.*) {
            .block => |b| b.span,
            .if_stmt => |i| i.span,
        } else then_block.span;

        const if_stmt = try self.create(ast.IfStmt, .{
            .condition = condition,
            .then_branch = then_block,
            .else_branch = else_branch,
            .span = ast.Span.merge(start_span, end_span),
        });
        return .{ .if_stmt = if_stmt };
    }

    fn parseMatchStmt(self: *Parser) ParseError!ast.Stmt {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'match'

        // Klar uses "match value { ... }" syntax (Rust-style)
        const subject = try self.parseExpression();

        try self.consume(.l_brace, "expected '{' after match subject");

        var arms = std.ArrayListUnmanaged(ast.MatchArmStmt){};

        while (!self.check(.r_brace) and !self.check(.eof)) {
            while (self.match(.newline)) {}
            if (self.check(.r_brace)) break;

            const arm = try self.parseMatchArmStmt();
            try arms.append(self.allocator, arm);

            _ = self.match(.newline) or self.match(.comma);
        }

        try self.consume(.r_brace, "expected '}' after match arms");
        const end_span = self.spanFromToken(self.previous);

        const match_stmt = try self.create(ast.MatchStmt, .{
            .subject = subject,
            .arms = try self.dupeSlice(ast.MatchArmStmt, arms.items),
            .span = ast.Span.merge(start_span, end_span),
        });
        return .{ .match_stmt = match_stmt };
    }

    fn parseMatchArmStmt(self: *Parser) ParseError!ast.MatchArmStmt {
        const pattern = try self.parsePattern();

        var guard: ?ast.Expr = null;
        if (self.match(.if_)) {
            guard = try self.parseExpression();
        }

        try self.consume(.fat_arrow, "expected '=>' after pattern");

        // Match arm body must be a block
        const body = try self.parseBlock();

        return .{
            .pattern = pattern,
            .guard = guard,
            .body = body,
            .span = ast.Span.merge(pattern.span(), body.span),
        };
    }

    fn parseClosure(self: *Parser) ParseError!ast.Expr {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume '|'

        var params = std.ArrayListUnmanaged(ast.ClosureParam){};

        if (!self.check(.pipe)) {
            while (true) {
                const param_name = try self.consumeIdentifier();
                const param_span = self.spanFromToken(self.previous);

                try self.consume(.colon, "type annotation required for closure parameter");
                const param_type = try self.parseType();

                try params.append(self.allocator, .{
                    .name = param_name,
                    .type_ = param_type,
                    .span = param_span,
                });

                if (!self.match(.comma)) break;
            }
        }

        try self.consume(.pipe, "expected '|' after closure parameters");

        try self.consume(.arrow, "return type required for closure (use '-> Type')");
        const return_type = try self.parseType();

        const body = try self.parseExpression();
        const end_span = body.span();

        const closure = try self.create(ast.Closure, .{
            .params = try self.dupeSlice(ast.ClosureParam, params.items),
            .return_type = return_type,
            .body = body,
            .span = ast.Span.merge(start_span, end_span),
        });
        return .{ .closure = closure };
    }

    // ========================================================================
    // Operator parsing
    // ========================================================================

    fn parseUnary(self: *Parser, op: ast.UnaryOp) ParseError!ast.Expr {
        const start_span = self.spanFromToken(self.current);
        self.advance();

        const operand = try self.parsePrecedence(.unary);
        const end_span = operand.span();

        const unary = try self.create(ast.Unary, .{
            .op = op,
            .operand = operand,
            .span = ast.Span.merge(start_span, end_span),
        });
        return .{ .unary = unary };
    }

    fn parseRefExpr(self: *Parser) ParseError!ast.Expr {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'ref'

        // With new syntax, mutability is determined by the operand's type (var vs let)
        // The type checker will determine if this becomes ref or ref_mut
        const operand = try self.parsePrecedence(.unary);
        const end_span = operand.span();

        const unary = try self.create(ast.Unary, .{
            .op = .ref,
            .operand = operand,
            .span = ast.Span.merge(start_span, end_span),
        });
        return .{ .unary = unary };
    }

    fn parseBinary(self: *Parser, left: ast.Expr, prec: Precedence) ParseError!ast.Expr {
        const op_token = self.current;
        const op = ast.BinaryOp.fromToken(op_token.kind) orelse {
            try self.reportError("expected binary operator");
            return ParseError.UnexpectedToken;
        };
        self.advance();

        // Right associativity for assignment and null coalesce
        const next_prec = switch (prec) {
            .assignment, .null_coalesce => prec,
            else => prec.next(),
        };

        const right = try self.parsePrecedence(next_prec);
        const span = ast.Span.merge(left.span(), right.span());

        const binary = try self.create(ast.Binary, .{
            .left = left,
            .op = op,
            .right = right,
            .span = span,
        });
        return .{ .binary = binary };
    }

    fn parseRange(self: *Parser, left: ast.Expr) ParseError!ast.Expr {
        const inclusive = self.current.kind == .dot_dot_eq;
        self.advance();

        // Range end is optional (for x..)
        var end: ?ast.Expr = null;
        if (!self.check(.r_bracket) and !self.check(.r_paren) and !self.check(.r_brace) and !self.check(.comma) and !self.check(.eof)) {
            end = try self.parsePrecedence(Precedence.range.next());
        }

        const end_span = if (end) |e| e.span() else left.span();

        const range = try self.create(ast.Range, .{
            .start = left,
            .end = end,
            .inclusive = inclusive,
            .span = ast.Span.merge(left.span(), end_span),
        });
        return .{ .range = range };
    }

    fn parsePostfix(self: *Parser, left: ast.Expr, op: ast.PostfixOp) ParseError!ast.Expr {
        const end_span = self.spanFromToken(self.current);
        self.advance();

        const postfix = try self.create(ast.Postfix, .{
            .operand = left,
            .op = op,
            .span = ast.Span.merge(left.span(), end_span),
        });
        return .{ .postfix = postfix };
    }

    fn parseCall(self: *Parser, callee: ast.Expr) ParseError!ast.Expr {
        self.advance(); // consume '('

        var args = std.ArrayListUnmanaged(ast.Expr){};

        if (!self.check(.r_paren)) {
            while (true) {
                try args.append(self.allocator, try self.parseExpression());
                if (!self.match(.comma)) break;
            }
        }

        try self.consume(.r_paren, "expected ')' after arguments");
        const end_span = self.spanFromToken(self.previous);

        const call = try self.create(ast.Call, .{
            .callee = callee,
            .args = try self.dupeSlice(ast.Expr, args.items),
            .span = ast.Span.merge(callee.span(), end_span),
        });
        return .{ .call = call };
    }

    fn parseIndex(self: *Parser, object: ast.Expr) ParseError!ast.Expr {
        self.advance(); // consume '['

        const index = try self.parseExpression();

        try self.consume(.r_bracket, "expected ']' after index");
        const end_span = self.spanFromToken(self.previous);

        const idx = try self.create(ast.Index, .{
            .object = object,
            .index = index,
            .span = ast.Span.merge(object.span(), end_span),
        });
        return .{ .index = idx };
    }

    fn parseFieldOrMethod(self: *Parser, object: ast.Expr) ParseError!ast.Expr {
        self.advance(); // consume '.'

        // Check for type cast: .as[T] or .trunc[T]
        if (self.check(.as) or self.tokenText(self.current).len > 0) {
            const method_name = self.tokenText(self.current);
            if (std.mem.eql(u8, method_name, "as")) {
                return self.parseTypeCast(object, false);
            }
            if (std.mem.eql(u8, method_name, "trunc")) {
                return self.parseTypeCast(object, true);
            }
        }

        // Accept either identifier or integer literal for field name
        // Integer literals are used for tuple indexing: tuple.0, tuple.1
        const field_name = if (self.check(.identifier))
            blk: {
                const name = self.tokenText(self.current);
                self.advance();
                break :blk name;
            }
        else if (self.check(.int_literal))
            blk: {
                const name = self.tokenText(self.current);
                self.advance();
                break :blk name;
            }
        else
            return ParseError.ExpectedIdentifier;

        // Check for method call with optional type arguments: foo.method[T]() or foo.method()
        if (self.check(.l_paren)) {
            return self.parseMethodCall(object, field_name, null);
        }

        // Check for method call with type arguments: foo.method[T]()
        // Only consume '[' if the next token can start a type expression.
        // This distinguishes between:
        //   - foo.method[T]() - type arguments (T can start a type)
        //   - foo.array[0]    - array subscript (0 cannot start a type)
        if (self.check(.l_bracket) and canStartType(self.peekNext().kind)) {
            self.advance(); // consume '['
            var type_args = std.ArrayListUnmanaged(ast.TypeExpr){};
            if (!self.check(.r_bracket)) {
                while (true) {
                    try type_args.append(self.allocator, try self.parseType());
                    if (!self.match(.comma)) break;
                }
            }
            try self.consume(.r_bracket, "expected ']' after type arguments");

            // Now expect '(' for the method call
            if (!self.check(.l_paren)) {
                return ParseError.UnexpectedToken;
            }
            return self.parseMethodCall(object, field_name, try self.dupeSlice(ast.TypeExpr, type_args.items));
        }

        const end_span = self.spanFromToken(self.previous);
        const field = try self.create(ast.Field, .{
            .object = object,
            .field_name = field_name,
            .span = ast.Span.merge(object.span(), end_span),
        });
        return .{ .field = field };
    }

    fn parseTypeCast(self: *Parser, object: ast.Expr, truncating: bool) ParseError!ast.Expr {
        self.advance(); // consume 'as' or 'trunc'

        try self.consume(.l_bracket, "expected '[' after cast");
        const target_type = try self.parseType();
        try self.consume(.r_bracket, "expected ']' after type");

        const end_span = self.spanFromToken(self.previous);

        const type_cast = try self.create(ast.TypeCast, .{
            .expr = object,
            .target_type = target_type,
            .truncating = truncating,
            .span = ast.Span.merge(object.span(), end_span),
        });
        return .{ .type_cast = type_cast };
    }

    fn parseMethodCall(self: *Parser, object: ast.Expr, method_name: []const u8, type_args: ?[]const ast.TypeExpr) ParseError!ast.Expr {
        self.advance(); // consume '('

        var args = std.ArrayListUnmanaged(ast.Expr){};

        if (!self.check(.r_paren)) {
            while (true) {
                try args.append(self.allocator, try self.parseExpression());
                if (!self.match(.comma)) break;
            }
        }

        try self.consume(.r_paren, "expected ')' after method arguments");
        const end_span = self.spanFromToken(self.previous);

        const method_call = try self.create(ast.MethodCall, .{
            .object = object,
            .method_name = method_name,
            .type_args = type_args,
            .args = try self.dupeSlice(ast.Expr, args.items),
            .span = ast.Span.merge(object.span(), end_span),
        });
        return .{ .method_call = method_call };
    }

    // ========================================================================
    // REPL Input Parsing
    // ========================================================================

    /// Input type for REPL: can be expression, statement, or declaration
    pub const ReplInput = union(enum) {
        expr: ast.Expr,
        stmt: ast.Stmt,
        decl: ast.Decl,
        command: Command,
        empty,
    };

    /// REPL meta-commands
    pub const Command = struct {
        kind: Kind,
        arg: ?[]const u8,

        pub const Kind = enum {
            help,
            quit,
            reset,
            type_,
            list,
            load,
        };
    };

    /// Parse REPL input: either an expression, statement, or declaration.
    /// Tries to determine the intent from the first token(s).
    pub fn parseReplInput(self: *Parser) ParseError!ReplInput {
        // Empty input
        if (self.check(.eof)) {
            return .empty;
        }

        // Check for declaration keywords first (fn, struct, enum, trait, impl, type, const, pub)
        if (self.check(.fn_) or self.check(.struct_) or self.check(.enum_) or
            self.check(.trait) or self.check(.impl) or self.check(.type_) or
            self.check(.const_) or self.check(.pub_))
        {
            const decl = try self.parseDeclaration();
            return .{ .decl = decl };
        }

        // Check for statement keywords (let, var, return, break, continue, loops, if, match)
        if (self.check(.let) or self.check(.var_) or self.check(.return_) or
            self.check(.break_) or self.check(.continue_) or self.check(.for_) or
            self.check(.while_) or self.check(.loop) or self.check(.if_) or
            self.check(.match))
        {
            const stmt = try self.parseStatement();
            return .{ .stmt = stmt };
        }

        // Otherwise, try to parse as an expression
        const expr = try self.parseExpression();

        // If next token is '=' or an assignment operator, parse as assignment statement
        if (self.check(.eq) or self.check(.plus_eq) or self.check(.minus_eq) or
            self.check(.star_eq) or self.check(.slash_eq) or self.check(.percent_eq))
        {
            const op: ast.BinaryOp = switch (self.current.kind) {
                .eq => .assign,
                .plus_eq => .add_assign,
                .minus_eq => .sub_assign,
                .star_eq => .mul_assign,
                .slash_eq => .div_assign,
                .percent_eq => .mod_assign,
                else => unreachable,
            };
            self.advance();
            const value = try self.parseExpression();

            const assignment = try self.create(ast.Assignment, .{
                .target = expr,
                .op = op,
                .value = value,
                .span = ast.Span.merge(expr.span(), value.span()),
            });
            return .{ .stmt = .{ .assignment = assignment } };
        }

        return .{ .expr = expr };
    }

    // ========================================================================
    // Statement parsing
    // ========================================================================

    /// Parse a statement (let, var, return, break, continue, loops, or expression statement)
    pub fn parseStatement(self: *Parser) ParseError!ast.Stmt {
        return switch (self.current.kind) {
            .let => self.parseLetDecl(),
            .var_ => self.parseVarDecl(),
            .return_ => self.parseReturnStmt(),
            .break_ => self.parseBreakStmt(),
            .continue_ => self.parseContinueStmt(),
            .for_ => self.parseForLoop(),
            .while_ => self.parseWhileLoop(),
            .loop => self.parseLoopStmt(),
            .if_ => self.parseIfStmt(),
            .match => self.parseMatchStmt(),
            else => self.parseExpressionStatement(),
        };
    }

    fn parseLetDecl(self: *Parser) ParseError!ast.Stmt {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'let'

        const name = try self.consumeIdentifier();

        // Type annotation is required
        if (!self.match(.colon)) {
            try self.reportError("type annotation required for let declaration");
            return ParseError.ExpectedType;
        }
        const type_ = try self.parseType();

        try self.consume(.eq, "expected '=' in let declaration");
        const value = try self.parseExpression();

        const end_span = value.span();
        const let_decl = try self.create(ast.LetDecl, .{
            .name = name,
            .type_ = type_,
            .value = value,
            .span = ast.Span.merge(start_span, end_span),
        });
        return .{ .let_decl = let_decl };
    }

    fn parseVarDecl(self: *Parser) ParseError!ast.Stmt {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'var'

        const name = try self.consumeIdentifier();

        // Type annotation is required
        if (!self.match(.colon)) {
            try self.reportError("type annotation required for var declaration");
            return ParseError.ExpectedType;
        }
        const type_ = try self.parseType();

        try self.consume(.eq, "expected '=' in var declaration");
        const value = try self.parseExpression();

        const end_span = value.span();
        const var_decl = try self.create(ast.VarDecl, .{
            .name = name,
            .type_ = type_,
            .value = value,
            .span = ast.Span.merge(start_span, end_span),
        });
        return .{ .var_decl = var_decl };
    }

    fn parseReturnStmt(self: *Parser) ParseError!ast.Stmt {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'return'

        var value: ?ast.Expr = null;
        var end_span = start_span;

        // Return value is optional - check if we have an expression following
        if (!self.check(.newline) and !self.check(.r_brace) and !self.check(.semicolon) and !self.check(.eof)) {
            value = try self.parseExpression();
            end_span = value.?.span();
        }

        const return_stmt = try self.create(ast.ReturnStmt, .{
            .value = value,
            .span = ast.Span.merge(start_span, end_span),
        });
        return .{ .return_stmt = return_stmt };
    }

    fn parseBreakStmt(self: *Parser) ParseError!ast.Stmt {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'break'

        var value: ?ast.Expr = null;
        var end_span = start_span;

        // Break value is optional
        if (!self.check(.newline) and !self.check(.r_brace) and !self.check(.semicolon) and !self.check(.eof)) {
            value = try self.parseExpression();
            end_span = value.?.span();
        }

        const break_stmt = try self.create(ast.BreakStmt, .{
            .value = value,
            .span = ast.Span.merge(start_span, end_span),
        });
        return .{ .break_stmt = break_stmt };
    }

    fn parseContinueStmt(self: *Parser) ParseError!ast.Stmt {
        const span = self.spanFromToken(self.current);
        self.advance(); // consume 'continue'

        const continue_stmt = try self.create(ast.ContinueStmt, .{
            .span = span,
        });
        return .{ .continue_stmt = continue_stmt };
    }

    fn parseForLoop(self: *Parser) ParseError!ast.Stmt {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'for'

        const pattern = try self.parsePattern();

        // Require type annotation for simple binding patterns
        switch (pattern) {
            .binding => |bind| {
                if (bind.type_annotation == null) {
                    try self.reportError("type annotation required for for-loop iterator");
                    return ParseError.ExpectedType;
                }
            },
            else => {}, // Other patterns infer types from context
        }

        try self.consume(.in, "expected 'in' after for pattern");
        const iterable = try self.parseExpression();
        const body = try self.parseBlock();

        const for_loop = try self.create(ast.ForLoop, .{
            .pattern = pattern,
            .iterable = iterable,
            .body = body,
            .span = ast.Span.merge(start_span, body.span),
        });
        return .{ .for_loop = for_loop };
    }

    fn parseWhileLoop(self: *Parser) ParseError!ast.Stmt {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'while'

        const condition = try self.parseExpression();
        const body = try self.parseBlock();

        const while_loop = try self.create(ast.WhileLoop, .{
            .condition = condition,
            .body = body,
            .span = ast.Span.merge(start_span, body.span),
        });
        return .{ .while_loop = while_loop };
    }

    fn parseLoopStmt(self: *Parser) ParseError!ast.Stmt {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'loop'

        const body = try self.parseBlock();

        const loop_stmt = try self.create(ast.LoopStmt, .{
            .body = body,
            .span = ast.Span.merge(start_span, body.span),
        });
        return .{ .loop_stmt = loop_stmt };
    }

    fn parseExpressionStatement(self: *Parser) ParseError!ast.Stmt {
        const expr = try self.parseExpression();
        const expr_stmt = try self.create(ast.ExprStmt, .{
            .expr = expr,
            .span = expr.span(),
        });
        return .{ .expr_stmt = expr_stmt };
    }

    // ========================================================================
    // Pattern parsing
    // ========================================================================

    fn parsePattern(self: *Parser) ParseError!ast.Pattern {
        return switch (self.current.kind) {
            .identifier => self.parseBindingOrVariant(),
            .int_literal, .float_literal, .string_literal, .char_literal, .true_, .false_ => self.parsePatternLiteral(),
            .l_paren => self.parseTuplePattern(),
            else => {
                // Wildcard is just an identifier "_"
                if (self.check(.identifier)) {
                    const text = self.tokenText(self.current);
                    if (std.mem.eql(u8, text, "_")) {
                        const span = self.spanFromToken(self.current);
                        self.advance();
                        return .{ .wildcard = .{ .span = span } };
                    }
                }
                try self.reportError("expected pattern");
                return ParseError.ExpectedPattern;
            },
        };
    }

    fn parseBindingOrVariant(self: *Parser) ParseError!ast.Pattern {
        const start_span = self.spanFromToken(self.current);
        const name = self.tokenText(self.current);
        self.advance();

        // Check for wildcard
        if (std.mem.eql(u8, name, "_")) {
            return .{ .wildcard = .{ .span = start_span } };
        }

        // Check if this looks like a type name (starts with uppercase)
        const is_type_name = name.len > 0 and std.ascii.isUpper(name[0]);

        // Check for generic type pattern: Type[T]::Variant or Type[T].Variant
        if (is_type_name and self.check(.l_bracket)) {
            // Parse type arguments
            self.advance(); // consume '['

            var type_args = std.ArrayListUnmanaged(ast.TypeExpr){};
            while (!self.check(.r_bracket) and !self.check(.eof)) {
                const type_arg = try self.parseType();
                try type_args.append(self.allocator, type_arg);
                if (!self.match(.comma)) break;
            }
            try self.consume(.r_bracket, "expected ']' after type arguments");

            // Create GenericApply type expression
            const base_type: ast.TypeExpr = .{ .named = .{ .name = name, .span = start_span } };
            const generic_type = try self.create(ast.GenericApply, .{
                .base = base_type,
                .args = try self.dupeSlice(ast.TypeExpr, type_args.items),
                .span = ast.Span.merge(start_span, self.spanFromToken(self.previous)),
            });
            const full_type: ast.TypeExpr = .{ .generic_apply = generic_type };

            // Must be followed by :: or . for variant pattern
            if (self.match(.colon_colon) or self.match(.dot)) {
                return self.parseVariantPatternWithType(full_type, start_span);
            }

            try self.reportError("expected '::' or '.' after generic type in pattern");
            return ParseError.UnexpectedToken;
        }

        // Check for variant with :: syntax: Type::Variant
        if (is_type_name and self.match(.colon_colon)) {
            const base_type: ast.TypeExpr = .{ .named = .{ .name = name, .span = start_span } };
            return self.parseVariantPatternWithType(base_type, start_span);
        }

        // Check for variant with . syntax (legacy): Type.Variant
        if (self.match(.dot)) {
            const base_type: ast.TypeExpr = .{ .named = .{ .name = name, .span = start_span } };
            return self.parseVariantPatternWithType(base_type, start_span);
        }

        // Check for shorthand variant pattern: Ok(v), Err(e), Some(v), etc.
        // Uppercase identifier followed by '(' is a variant pattern without explicit type
        if (is_type_name and self.check(.l_paren)) {
            self.advance(); // consume '('
            var payload: ?ast.Pattern = null;
            if (!self.check(.r_paren)) {
                payload = try self.parsePattern();
            }
            try self.consume(.r_paren, "expected ')' after variant payload");

            const end_span = self.spanFromToken(self.previous);
            const variant = try self.create(ast.VariantPattern, .{
                .type_expr = null, // Infer type from match expression
                .variant_name = name,
                .payload = payload,
                .span = ast.Span.merge(start_span, end_span),
            });
            return .{ .variant = variant };
        }

        // Simple binding - check for type annotation
        var type_annotation: ?ast.TypeExpr = null;
        if (self.match(.colon)) {
            type_annotation = try self.parseType();
        }

        return .{ .binding = .{
            .name = name,
            .mutable = false,
            .type_annotation = type_annotation,
            .span = start_span,
        } };
    }

    /// Parse variant pattern after the type expression and :: or . have been consumed
    fn parseVariantPatternWithType(self: *Parser, type_expr: ast.TypeExpr, start_span: ast.Span) ParseError!ast.Pattern {
        const variant_name = try self.consumeIdentifier();
        var payload: ?ast.Pattern = null;

        if (self.match(.l_paren)) {
            payload = try self.parsePattern();
            try self.consume(.r_paren, "expected ')' after variant payload");
        }

        const end_span = self.spanFromToken(self.previous);
        const variant = try self.create(ast.VariantPattern, .{
            .type_expr = type_expr,
            .variant_name = variant_name,
            .payload = payload,
            .span = ast.Span.merge(start_span, end_span),
        });
        return .{ .variant = variant };
    }

    fn parsePatternLiteral(self: *Parser) ParseError!ast.Pattern {
        const span = self.spanFromToken(self.current);
        const text = self.tokenText(self.current);

        const kind: ast.Literal.Kind = switch (self.current.kind) {
            .int_literal => .{ .int = parseIntValue(text) catch 0 },
            .float_literal => .{ .float = std.fmt.parseFloat(f64, text) catch 0.0 },
            .string_literal => .{ .string = if (text.len >= 2) text[1 .. text.len - 1] else text },
            .char_literal => .{ .char = if (text.len >= 3) text[1] else 0 },
            .true_ => .{ .bool_ = true },
            .false_ => .{ .bool_ = false },
            else => .{ .bool_ = false },
        };

        self.advance();

        return .{ .literal = .{
            .kind = kind,
            .span = span,
        } };
    }

    fn parseTuplePattern(self: *Parser) ParseError!ast.Pattern {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume '('

        var elements = std.ArrayListUnmanaged(ast.Pattern){};

        if (!self.check(.r_paren)) {
            while (true) {
                try elements.append(self.allocator, try self.parsePattern());
                if (!self.match(.comma)) break;
            }
        }

        try self.consume(.r_paren, "expected ')' after tuple pattern");
        const end_span = self.spanFromToken(self.previous);

        const tuple = try self.create(ast.TuplePattern, .{
            .elements = try self.dupeSlice(ast.Pattern, elements.items),
            .span = ast.Span.merge(start_span, end_span),
        });
        return .{ .tuple_pattern = tuple };
    }

    // ========================================================================
    // Type parsing
    // ========================================================================

    fn parseType(self: *Parser) ParseError!ast.TypeExpr {
        // Optional type: ?T
        if (self.match(.question)) {
            const inner = try self.parseType();
            const opt = try self.create(ast.OptionalType, .{
                .inner = inner,
                .span = inner.span(),
            });
            return .{ .optional = opt };
        }

        // Reference type: ref T (read-only) or inout T (mutable)
        if (self.match(.ref)) {
            const inner = try self.parseType();
            const reference = try self.create(ast.ReferenceType, .{
                .inner = inner,
                .mutable = false,
                .span = inner.span(),
            });
            return .{ .reference = reference };
        }
        if (self.match(.inout)) {
            const inner = try self.parseType();
            const reference = try self.create(ast.ReferenceType, .{
                .inner = inner,
                .mutable = true,
                .span = inner.span(),
            });
            return .{ .reference = reference };
        }

        // Slice type: [T] or Array type: [T; size]
        if (self.match(.l_bracket)) {
            const start_span = ast.Span.from(self.previous);
            const element = try self.parseType();
            if (self.match(.semicolon)) {
                // Array type: [T; size]
                const size_expr = try self.parseExpression();
                try self.consume(.r_bracket, "expected ']' after array size");
                const end_span = ast.Span.from(self.previous);
                const arr = try self.create(ast.ArrayType, .{
                    .element = element,
                    .size = size_expr,
                    .span = ast.Span.merge(start_span, end_span),
                });
                return .{ .array = arr };
            } else {
                // Slice type: [T]
                try self.consume(.r_bracket, "expected ']' after slice type");
                const slice = try self.create(ast.SliceType, .{
                    .element = element,
                    .span = element.span(),
                });
                return .{ .slice = slice };
            }
        }

        // Tuple type: (T1, T2, ...)
        if (self.match(.l_paren)) {
            var elements = std.ArrayListUnmanaged(ast.TypeExpr){};

            if (!self.check(.r_paren)) {
                while (true) {
                    try elements.append(self.allocator, try self.parseType());
                    if (!self.match(.comma)) break;
                }
            }

            try self.consume(.r_paren, "expected ')' after tuple type");

            const tuple = try self.create(ast.TupleType, .{
                .elements = try self.dupeSlice(ast.TypeExpr, elements.items),
                .span = ast.Span.from(self.previous),
            });
            return .{ .tuple = tuple };
        }

        // Function type: fn(Args) -> Ret
        if (self.match(.fn_)) {
            try self.consume(.l_paren, "expected '(' after fn");

            var params = std.ArrayListUnmanaged(ast.TypeExpr){};
            if (!self.check(.r_paren)) {
                while (true) {
                    try params.append(self.allocator, try self.parseType());
                    if (!self.match(.comma)) break;
                }
            }

            try self.consume(.r_paren, "expected ')' after function parameters");
            try self.consume(.arrow, "expected '->' after function parameters");
            const return_type = try self.parseType();

            const func = try self.create(ast.FunctionType, .{
                .params = try self.dupeSlice(ast.TypeExpr, params.items),
                .return_type = return_type,
                .span = return_type.span(),
            });
            return .{ .function = func };
        }

        // Handle Self as a type (possibly with .Item access)
        if (self.match(.self_type)) {
            const start_span = self.spanFromToken(self.previous);
            var base: ast.TypeExpr = .{ .named = .{ .name = "Self", .span = start_span } };

            // Check for qualified access: Self.Item
            while (self.match(.dot)) {
                if (!self.check(.identifier)) {
                    try self.reportError("expected member name after '.'");
                    return ParseError.ExpectedType;
                }
                const member_name = self.tokenText(self.current);
                self.advance();
                const qualified = try self.create(ast.QualifiedType, .{
                    .base = base,
                    .member = member_name,
                    .span = start_span,
                });
                base = .{ .qualified = qualified };
            }

            return base;
        }

        // Named type (with possible generic args and qualified access)
        if (self.check(.identifier)) {
            const span = self.spanFromToken(self.current);
            const name = self.tokenText(self.current);
            self.advance();

            var base: ast.TypeExpr = .{ .named = .{ .name = name, .span = span } };

            // Check for generic arguments: Type[A, B]
            if (self.match(.l_bracket)) {
                var args = std.ArrayListUnmanaged(ast.TypeExpr){};

                while (true) {
                    try args.append(self.allocator, try self.parseType());
                    if (!self.match(.comma)) break;
                }

                try self.consume(.r_bracket, "expected ']' after type arguments");

                const generic = try self.create(ast.GenericApply, .{
                    .base = base,
                    .args = try self.dupeSlice(ast.TypeExpr, args.items),
                    .span = ast.Span.from(self.previous),
                });
                base = .{ .generic_apply = generic };
            }

            // Check for qualified access: T.Item or T[U].Item
            while (self.match(.dot)) {
                if (!self.check(.identifier)) {
                    try self.reportError("expected member name after '.'");
                    return ParseError.ExpectedType;
                }
                const member_name = self.tokenText(self.current);
                self.advance();
                const qualified = try self.create(ast.QualifiedType, .{
                    .base = base,
                    .member = member_name,
                    .span = span,
                });
                base = .{ .qualified = qualified };
            }

            return base;
        }

        try self.reportError("expected type");
        return ParseError.ExpectedType;
    }

    // ========================================================================
    // Declaration parsing
    // ========================================================================

    /// Parse a declaration at the top level
    pub fn parseDeclaration(self: *Parser) ParseError!ast.Decl {
        // Handle visibility modifier
        const is_pub = self.match(.pub_);

        // Handle unsafe modifier (for unsafe fn)
        const is_unsafe = self.match(.unsafe_);

        return switch (self.current.kind) {
            .fn_ => self.parseFunctionDecl(is_pub, is_unsafe),
            .struct_ => blk: {
                if (is_unsafe) {
                    try self.reportError("'unsafe' modifier is not allowed on struct declarations");
                    return ParseError.UnexpectedToken;
                }
                break :blk self.parseStructDecl(is_pub);
            },
            .enum_ => blk: {
                if (is_unsafe) {
                    try self.reportError("'unsafe' modifier is not allowed on enum declarations");
                    return ParseError.UnexpectedToken;
                }
                break :blk self.parseEnumDecl(is_pub);
            },
            .trait => blk: {
                if (is_unsafe) {
                    // Note: Future support for unsafe traits can be added here
                    try self.reportError("'unsafe' modifier is not yet supported on trait declarations");
                    return ParseError.UnexpectedToken;
                }
                break :blk self.parseTraitDecl(is_pub);
            },
            .impl => blk: {
                if (is_unsafe) {
                    // Note: Future support for unsafe impl can be added here
                    try self.reportError("'unsafe' modifier is not yet supported on impl declarations");
                    return ParseError.UnexpectedToken;
                }
                break :blk self.parseImplDecl();
            },
            .type_ => blk: {
                if (is_unsafe) {
                    try self.reportError("'unsafe' modifier is not allowed on type aliases");
                    return ParseError.UnexpectedToken;
                }
                break :blk self.parseTypeAlias(is_pub);
            },
            .const_ => blk: {
                if (is_unsafe) {
                    try self.reportError("'unsafe' modifier is not allowed on const declarations");
                    return ParseError.UnexpectedToken;
                }
                break :blk self.parseConstDecl(is_pub);
            },
            .import => blk: {
                if (is_unsafe) {
                    try self.reportError("'unsafe' modifier is not allowed on import declarations");
                    return ParseError.UnexpectedToken;
                }
                break :blk self.parseImportDecl();
            },
            .module => blk: {
                if (is_unsafe) {
                    try self.reportError("'unsafe' modifier is not allowed on module declarations");
                    return ParseError.UnexpectedToken;
                }
                break :blk self.parseModuleDecl();
            },
            else => {
                if (is_unsafe) {
                    try self.reportError("expected 'fn' after 'unsafe'");
                } else {
                    try self.reportError("expected declaration");
                }
                return ParseError.UnexpectedToken;
            },
        };
    }

    fn parseFunctionDecl(self: *Parser, is_pub: bool, is_unsafe: bool) ParseError!ast.Decl {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'fn'

        // Check for comptime function: fn @name(...)
        const is_comptime = self.match(.at);

        const name = try self.consumeIdentifier();

        // Parse optional type parameters: fn name[T, U]
        const type_params = try self.parseTypeParams();

        // Parse parameters
        try self.consume(.l_paren, "expected '(' after function name");
        const params = try self.parseFunctionParams();
        try self.consume(.r_paren, "expected ')' after parameters");

        // Parse optional return type
        var return_type: ?ast.TypeExpr = null;
        if (self.match(.arrow)) {
            return_type = try self.parseType();
        }

        // Parse optional where clause
        const where_clause = try self.parseWhereClause();

        // Parse body (either block or = expr for single-expression)
        var body: ?*ast.Block = null;
        if (self.check(.l_brace)) {
            body = try self.parseBlock();
        } else if (self.match(.eq)) {
            // Single-expression function: fn foo() -> i32 = 42
            const expr = try self.parseExpression();
            const stmts = std.ArrayListUnmanaged(ast.Stmt){};
            body = try self.create(ast.Block, .{
                .statements = try self.dupeSlice(ast.Stmt, stmts.items),
                .final_expr = expr,
                .span = expr.span(),
            });
        }

        const end_span = if (body) |b| b.span else if (return_type) |rt| rt.span() else start_span;

        const func = try self.create(ast.FunctionDecl, .{
            .name = name,
            .type_params = type_params,
            .params = params,
            .return_type = return_type,
            .where_clause = where_clause,
            .body = body,
            .is_pub = is_pub,
            .is_async = false,
            .is_comptime = is_comptime,
            .is_unsafe = is_unsafe,
            .span = ast.Span.merge(start_span, end_span),
        });
        return .{ .function = func };
    }

    fn parseTypeParams(self: *Parser) ParseError![]const ast.TypeParam {
        if (!self.match(.l_bracket)) {
            return &[_]ast.TypeParam{};
        }

        var params = std.ArrayListUnmanaged(ast.TypeParam){};

        while (!self.check(.r_bracket) and !self.check(.eof)) {
            const param_span = self.spanFromToken(self.current);
            const param_name = try self.consumeIdentifier();

            // Parse optional bounds: T: Trait + OtherTrait
            var bounds = std.ArrayListUnmanaged(ast.TypeExpr){};
            if (self.match(.colon)) {
                while (true) {
                    try bounds.append(self.allocator, try self.parseType());
                    if (!self.match(.plus)) break;
                }
            }

            try params.append(self.allocator, .{
                .name = param_name,
                .bounds = try self.dupeSlice(ast.TypeExpr, bounds.items),
                .span = param_span,
            });

            if (!self.match(.comma)) break;
        }

        try self.consume(.r_bracket, "expected ']' after type parameters");
        return self.dupeSlice(ast.TypeParam, params.items);
    }

    fn parseFunctionParams(self: *Parser) ParseError![]const ast.FunctionParam {
        var params = std.ArrayListUnmanaged(ast.FunctionParam){};

        if (self.check(.r_paren)) {
            return &[_]ast.FunctionParam{};
        }

        while (true) {
            const param_span = self.spanFromToken(self.current);

            // Check for comptime parameter: @param: Type
            const is_comptime = self.match(.at);

            // Handle 'self' and regular identifiers as parameter names
            const param_name = if (self.match(.self))
                "self"
            else
                try self.consumeIdentifier();

            try self.consume(.colon, "expected ':' after parameter name");
            const param_type = try self.parseType();

            // Parse optional default value
            var default_value: ?ast.Expr = null;
            if (self.match(.eq)) {
                default_value = try self.parseExpression();
            }

            try params.append(self.allocator, .{
                .name = param_name,
                .type_ = param_type,
                .default_value = default_value,
                .is_comptime = is_comptime,
                .span = param_span,
            });

            if (!self.match(.comma)) break;
        }

        return self.dupeSlice(ast.FunctionParam, params.items);
    }

    fn parseWhereClause(self: *Parser) ParseError!?[]const ast.WhereConstraint {
        if (!self.match(.where)) {
            return null;
        }

        var constraints = std.ArrayListUnmanaged(ast.WhereConstraint){};

        while (true) {
            const constraint_span = self.spanFromToken(self.current);
            const type_param = try self.consumeIdentifier();

            try self.consume(.colon, "expected ':' after type parameter in where clause");

            var bounds = std.ArrayListUnmanaged(ast.TypeExpr){};
            while (true) {
                try bounds.append(self.allocator, try self.parseType());
                if (!self.match(.plus)) break;
            }

            try constraints.append(self.allocator, .{
                .type_param = type_param,
                .bounds = try self.dupeSlice(ast.TypeExpr, bounds.items),
                .span = constraint_span,
            });

            if (!self.match(.comma)) break;
        }

        return try self.dupeSlice(ast.WhereConstraint, constraints.items);
    }

    fn parseStructDecl(self: *Parser, is_pub: bool) ParseError!ast.Decl {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'struct'

        const name = try self.consumeIdentifier();
        const type_params = try self.parseTypeParams();

        // Parse optional trait implementations: struct Foo: Trait1 + Trait2
        var traits = std.ArrayListUnmanaged(ast.TypeExpr){};
        if (self.match(.colon)) {
            while (true) {
                try traits.append(self.allocator, try self.parseType());
                if (!self.match(.plus)) break;
            }
        }

        try self.consume(.l_brace, "expected '{' after struct name");

        // Parse fields
        var fields = std.ArrayListUnmanaged(ast.StructField){};
        while (!self.check(.r_brace) and !self.check(.eof)) {
            // Skip newlines
            while (self.match(.newline)) {}
            if (self.check(.r_brace)) break;

            const field_is_pub = self.match(.pub_);
            const field_span = self.spanFromToken(self.current);
            const field_name = try self.consumeIdentifier();

            try self.consume(.colon, "expected ':' after field name");
            const field_type = try self.parseType();

            try fields.append(self.allocator, .{
                .name = field_name,
                .type_ = field_type,
                .is_pub = field_is_pub,
                .span = field_span,
            });

            _ = self.match(.comma) or self.match(.newline);
        }

        try self.consume(.r_brace, "expected '}' after struct fields");
        const end_span = self.spanFromToken(self.previous);

        const struct_decl = try self.create(ast.StructDecl, .{
            .name = name,
            .type_params = type_params,
            .fields = try self.dupeSlice(ast.StructField, fields.items),
            .traits = try self.dupeSlice(ast.TypeExpr, traits.items),
            .is_pub = is_pub,
            .span = ast.Span.merge(start_span, end_span),
        });
        return .{ .struct_decl = struct_decl };
    }

    fn parseEnumDecl(self: *Parser, is_pub: bool) ParseError!ast.Decl {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'enum'

        const name = try self.consumeIdentifier();
        const type_params = try self.parseTypeParams();

        try self.consume(.l_brace, "expected '{' after enum name");

        // Parse variants
        var variants = std.ArrayListUnmanaged(ast.EnumVariant){};
        while (!self.check(.r_brace) and !self.check(.eof)) {
            while (self.match(.newline)) {}
            if (self.check(.r_brace)) break;

            const variant_span = self.spanFromToken(self.current);
            const variant_name = try self.consumeIdentifier();

            // Parse optional payload
            var payload: ?ast.VariantPayload = null;

            if (self.match(.l_paren)) {
                // Tuple payload: Variant(T1, T2)
                var tuple_types = std.ArrayListUnmanaged(ast.TypeExpr){};
                if (!self.check(.r_paren)) {
                    while (true) {
                        try tuple_types.append(self.allocator, try self.parseType());
                        if (!self.match(.comma)) break;
                    }
                }
                try self.consume(.r_paren, "expected ')' after tuple payload");
                payload = .{ .tuple = try self.dupeSlice(ast.TypeExpr, tuple_types.items) };
            } else if (self.match(.l_brace)) {
                // Struct payload: Variant { field: T }
                var struct_fields = std.ArrayListUnmanaged(ast.StructField){};
                while (!self.check(.r_brace) and !self.check(.eof)) {
                    while (self.match(.newline)) {}
                    if (self.check(.r_brace)) break;

                    const field_span = self.spanFromToken(self.current);
                    const field_name = try self.consumeIdentifier();
                    try self.consume(.colon, "expected ':' after field name");
                    const field_type = try self.parseType();

                    try struct_fields.append(self.allocator, .{
                        .name = field_name,
                        .type_ = field_type,
                        .is_pub = false,
                        .span = field_span,
                    });

                    _ = self.match(.comma) or self.match(.newline);
                }
                try self.consume(.r_brace, "expected '}' after struct payload");
                payload = .{ .struct_ = try self.dupeSlice(ast.StructField, struct_fields.items) };
            }

            try variants.append(self.allocator, .{
                .name = variant_name,
                .payload = payload,
                .span = variant_span,
            });

            _ = self.match(.comma) or self.match(.newline);
        }

        try self.consume(.r_brace, "expected '}' after enum variants");
        const end_span = self.spanFromToken(self.previous);

        const enum_decl = try self.create(ast.EnumDecl, .{
            .name = name,
            .type_params = type_params,
            .variants = try self.dupeSlice(ast.EnumVariant, variants.items),
            .is_pub = is_pub,
            .span = ast.Span.merge(start_span, end_span),
        });
        return .{ .enum_decl = enum_decl };
    }

    fn parseTraitDecl(self: *Parser, is_pub: bool) ParseError!ast.Decl {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'trait'

        const name = try self.consumeIdentifier();
        const type_params = try self.parseTypeParams();

        // Parse optional super traits: trait A: B + C
        var super_traits = std.ArrayListUnmanaged(ast.TypeExpr){};
        if (self.match(.colon)) {
            // Parse first super trait
            const first_trait = try self.parseType();
            try super_traits.append(self.allocator, first_trait);

            // Parse additional super traits separated by '+'
            while (self.match(.plus)) {
                const next_trait = try self.parseType();
                try super_traits.append(self.allocator, next_trait);
            }
        }

        try self.consume(.l_brace, "expected '{' after trait name");

        // Parse associated types and method signatures
        var associated_types = std.ArrayListUnmanaged(ast.AssociatedTypeDecl){};
        var methods = std.ArrayListUnmanaged(ast.FunctionDecl){};
        while (!self.check(.r_brace) and !self.check(.eof)) {
            while (self.match(.newline)) {}
            if (self.check(.r_brace)) break;

            // Check for associated type declaration: type Item or type Item: Bounds
            if (self.check(.type_)) {
                const assoc_type = try self.parseAssociatedTypeDecl();
                try associated_types.append(self.allocator, assoc_type);
            } else {
                // Each method in trait is a function signature (potentially without body)
                // Note: unsafe methods in traits not yet supported
                const method_decl = try self.parseFunctionDecl(false, false);
                try methods.append(self.allocator, method_decl.function.*);
            }

            _ = self.match(.newline);
        }

        try self.consume(.r_brace, "expected '}' after trait members");
        const end_span = self.spanFromToken(self.previous);

        const trait_decl = try self.create(ast.TraitDecl, .{
            .name = name,
            .type_params = type_params,
            .super_traits = try self.dupeSlice(ast.TypeExpr, super_traits.items),
            .associated_types = try self.dupeSlice(ast.AssociatedTypeDecl, associated_types.items),
            .methods = try self.dupeSlice(ast.FunctionDecl, methods.items),
            .is_pub = is_pub,
            .span = ast.Span.merge(start_span, end_span),
        });
        return .{ .trait_decl = trait_decl };
    }

    /// Parse an associated type declaration: type Item or type Item: Clone + Hash or type Item = DefaultType
    fn parseAssociatedTypeDecl(self: *Parser) ParseError!ast.AssociatedTypeDecl {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'type'

        const assoc_name = try self.consumeIdentifier();

        // Parse optional bounds: type Item: Clone + Hash
        var bounds = std.ArrayListUnmanaged(ast.TypeExpr){};
        if (self.match(.colon)) {
            const first_bound = try self.parseType();
            try bounds.append(self.allocator, first_bound);

            while (self.match(.plus)) {
                const next_bound = try self.parseType();
                try bounds.append(self.allocator, next_bound);
            }
        }

        // Parse optional default: type Item = i32
        var default_type: ?ast.TypeExpr = null;
        if (self.match(.eq)) {
            default_type = try self.parseType();
        }

        const end_span = self.spanFromToken(self.previous);

        return ast.AssociatedTypeDecl{
            .name = assoc_name,
            .bounds = try self.dupeSlice(ast.TypeExpr, bounds.items),
            .default = default_type,
            .span = ast.Span.merge(start_span, end_span),
        };
    }

    fn parseImplDecl(self: *Parser) ParseError!ast.Decl {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'impl'

        // Parse optional type parameters: impl[T]
        const type_params = try self.parseTypeParams();

        // Parse target type
        const target_type = try self.parseType();

        // Parse optional trait: impl Type: Trait
        var trait_type: ?ast.TypeExpr = null;
        if (self.match(.colon)) {
            trait_type = try self.parseType();
        }

        // Parse optional where clause
        const where_clause = try self.parseWhereClause();

        try self.consume(.l_brace, "expected '{' after impl");

        // Parse associated type bindings and methods
        var associated_types = std.ArrayListUnmanaged(ast.AssociatedTypeBinding){};
        var methods = std.ArrayListUnmanaged(ast.FunctionDecl){};
        while (!self.check(.r_brace) and !self.check(.eof)) {
            while (self.match(.newline)) {}
            if (self.check(.r_brace)) break;

            // Check for associated type binding: type Item = ConcreteType
            if (self.check(.type_)) {
                const assoc_binding = try self.parseAssociatedTypeBinding();
                try associated_types.append(self.allocator, assoc_binding);
            } else {
                const is_pub = self.match(.pub_);
                const method_is_unsafe = self.match(.unsafe_);
                const method_decl = try self.parseFunctionDecl(is_pub, method_is_unsafe);
                try methods.append(self.allocator, method_decl.function.*);
            }

            _ = self.match(.newline);
        }

        try self.consume(.r_brace, "expected '}' after impl members");
        const end_span = self.spanFromToken(self.previous);

        const impl_decl = try self.create(ast.ImplDecl, .{
            .type_params = type_params,
            .target_type = target_type,
            .trait_type = trait_type,
            .associated_types = try self.dupeSlice(ast.AssociatedTypeBinding, associated_types.items),
            .where_clause = where_clause,
            .methods = try self.dupeSlice(ast.FunctionDecl, methods.items),
            .span = ast.Span.merge(start_span, end_span),
        });
        return .{ .impl_decl = impl_decl };
    }

    /// Parse an associated type binding: type Item = ConcreteType
    fn parseAssociatedTypeBinding(self: *Parser) ParseError!ast.AssociatedTypeBinding {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'type'

        const assoc_name = try self.consumeIdentifier();

        try self.consume(.eq, "expected '=' after associated type name");
        const value_type = try self.parseType();

        const end_span = self.spanFromToken(self.previous);

        return ast.AssociatedTypeBinding{
            .name = assoc_name,
            .value = value_type,
            .span = ast.Span.merge(start_span, end_span),
        };
    }

    fn parseTypeAlias(self: *Parser, is_pub: bool) ParseError!ast.Decl {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'type'

        const name = try self.consumeIdentifier();
        const type_params = try self.parseTypeParams();

        try self.consume(.eq, "expected '=' in type alias");
        const target = try self.parseType();

        const type_alias = try self.create(ast.TypeAlias, .{
            .name = name,
            .type_params = type_params,
            .target = target,
            .is_pub = is_pub,
            .span = ast.Span.merge(start_span, target.span()),
        });
        return .{ .type_alias = type_alias };
    }

    fn parseConstDecl(self: *Parser, is_pub: bool) ParseError!ast.Decl {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'const'

        const name = try self.consumeIdentifier();

        var type_: ?ast.TypeExpr = null;
        if (self.match(.colon)) {
            type_ = try self.parseType();
        }

        try self.consume(.eq, "expected '=' in const declaration");
        const value = try self.parseExpression();

        const const_decl = try self.create(ast.ConstDecl, .{
            .name = name,
            .type_ = type_,
            .value = value,
            .is_pub = is_pub,
            .span = ast.Span.merge(start_span, value.span()),
        });
        return .{ .const_decl = const_decl };
    }

    fn parseImportDecl(self: *Parser) ParseError!ast.Decl {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'import'

        // Parse module path: std.collections.List
        var path = std.ArrayListUnmanaged([]const u8){};
        try path.append(self.allocator, try self.consumeIdentifier());

        while (self.match(.dot)) {
            // Check for wildcard: import path.*
            if (self.match(.star)) {
                const import_decl = try self.create(ast.ImportDecl, .{
                    .path = try self.dupeSlice([]const u8, path.items),
                    .items = .all,
                    .alias = null,
                    .span = ast.Span.merge(start_span, self.spanFromToken(self.previous)),
                });
                return .{ .import_decl = import_decl };
            }

            // Check for specific items: import path.{ Item1, Item2 }
            if (self.match(.l_brace)) {
                var items = std.ArrayListUnmanaged(ast.ImportItem){};

                while (!self.check(.r_brace) and !self.check(.eof)) {
                    while (self.match(.newline)) {}
                    if (self.check(.r_brace)) break;

                    const item_span = self.spanFromToken(self.current);
                    const item_name = try self.consumeIdentifier();

                    var alias: ?[]const u8 = null;
                    if (self.match(.as)) {
                        alias = try self.consumeIdentifier();
                    }

                    try items.append(self.allocator, .{
                        .name = item_name,
                        .alias = alias,
                        .span = item_span,
                    });

                    _ = self.match(.comma);
                }

                try self.consume(.r_brace, "expected '}' after import items");
                const end_span = self.spanFromToken(self.previous);

                const import_decl = try self.create(ast.ImportDecl, .{
                    .path = try self.dupeSlice([]const u8, path.items),
                    .items = .{ .specific = try self.dupeSlice(ast.ImportItem, items.items) },
                    .alias = null,
                    .span = ast.Span.merge(start_span, end_span),
                });
                return .{ .import_decl = import_decl };
            }

            // Regular path segment
            try path.append(self.allocator, try self.consumeIdentifier());
        }

        // Check for alias: import path as alias
        var alias: ?[]const u8 = null;
        if (self.match(.as)) {
            alias = try self.consumeIdentifier();
        }

        const end_span = self.spanFromToken(self.previous);

        const import_decl = try self.create(ast.ImportDecl, .{
            .path = try self.dupeSlice([]const u8, path.items),
            .items = null,
            .alias = alias,
            .span = ast.Span.merge(start_span, end_span),
        });
        return .{ .import_decl = import_decl };
    }

    fn parseModuleDecl(self: *Parser) ParseError!ast.Decl {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'module'

        // Parse module path: module name.subname
        var path = std.ArrayListUnmanaged([]const u8){};
        try path.append(self.allocator, try self.consumeIdentifier());

        while (self.match(.dot)) {
            try path.append(self.allocator, try self.consumeIdentifier());
        }

        const end_span = self.spanFromToken(self.previous);

        const module_decl = try self.create(ast.ModuleDecl, .{
            .path = try self.dupeSlice([]const u8, path.items),
            .span = ast.Span.merge(start_span, end_span),
        });
        return .{ .module_decl = module_decl };
    }

    // ========================================================================
    // Top-level parsing
    // ========================================================================

    /// Parse a complete module (file)
    pub fn parseModule(self: *Parser) ParseError!ast.Module {
        var module_decl: ?ast.ModuleDecl = null;
        var imports = std.ArrayListUnmanaged(ast.ImportDecl){};
        var declarations = std.ArrayListUnmanaged(ast.Decl){};

        // Skip leading newlines
        while (self.match(.newline)) {}

        // Parse optional module declaration
        if (self.check(.module)) {
            const decl = try self.parseDeclaration();
            module_decl = decl.module_decl.*;
            _ = self.match(.newline);
        }

        // Parse imports and declarations
        while (!self.check(.eof)) {
            // Skip newlines
            while (self.match(.newline)) {}
            if (self.check(.eof)) break;

            const decl = try self.parseDeclaration();

            // Separate imports from other declarations
            if (decl == .import_decl) {
                try imports.append(self.allocator, decl.import_decl.*);
            } else {
                try declarations.append(self.allocator, decl);
            }

            _ = self.match(.newline);
        }

        return .{
            .module_decl = module_decl,
            .imports = try self.dupeSlice(ast.ImportDecl, imports.items),
            .declarations = try self.dupeSlice(ast.Decl, declarations.items),
        };
    }
};

// ============================================================================
// Tests
// ============================================================================

fn testParse(source: []const u8) !struct { expr: ast.Expr, arena: std.heap.ArenaAllocator } {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    errdefer arena.deinit();

    var lexer = Lexer.init(source);
    var parser = Parser.init(arena.allocator(), &lexer, source);

    const expr = try parser.parseExpression();
    return .{ .expr = expr, .arena = arena };
}

test "parse integer literal" {
    var result = try testParse("42");
    defer result.arena.deinit();

    try std.testing.expect(result.expr == .literal);
    try std.testing.expectEqual(@as(i128, 42), result.expr.literal.kind.int);
}

test "parse float literal" {
    var result = try testParse("3.14");
    defer result.arena.deinit();

    try std.testing.expect(result.expr == .literal);
    try std.testing.expectApproxEqAbs(@as(f64, 3.14), result.expr.literal.kind.float, 0.001);
}

test "parse binary expression" {
    var result = try testParse("1 + 2");
    defer result.arena.deinit();

    try std.testing.expect(result.expr == .binary);
    try std.testing.expectEqual(ast.BinaryOp.add, result.expr.binary.op);
}

test "parse precedence" {
    var result = try testParse("1 + 2 * 3");
    defer result.arena.deinit();

    try std.testing.expect(result.expr == .binary);
    try std.testing.expectEqual(ast.BinaryOp.add, result.expr.binary.op);
    // Right side should be 2 * 3
    try std.testing.expect(result.expr.binary.right == .binary);
    try std.testing.expectEqual(ast.BinaryOp.mul, result.expr.binary.right.binary.op);
}

test "parse call expression" {
    var result = try testParse("foo(1, 2)");
    defer result.arena.deinit();

    try std.testing.expect(result.expr == .call);
    try std.testing.expectEqual(@as(usize, 2), result.expr.call.args.len);
}

test "parse array literal" {
    var result = try testParse("[1, 2, 3]");
    defer result.arena.deinit();

    try std.testing.expect(result.expr == .array_literal);
    try std.testing.expectEqual(@as(usize, 3), result.expr.array_literal.elements.len);
}

test "parse closure" {
    var result = try testParse("|x: i32, y: i32| -> i32 { return x + y }");
    defer result.arena.deinit();

    try std.testing.expect(result.expr == .closure);
    try std.testing.expectEqual(@as(usize, 2), result.expr.closure.params.len);
}

test "parse field access" {
    var result = try testParse("point.x");
    defer result.arena.deinit();

    try std.testing.expect(result.expr == .field);
    try std.testing.expectEqualStrings("x", result.expr.field.field_name);
}

test "parse method call" {
    var result = try testParse("list.push(item)");
    defer result.arena.deinit();

    try std.testing.expect(result.expr == .method_call);
    try std.testing.expectEqualStrings("push", result.expr.method_call.method_name);
}

test "parse unary" {
    var result = try testParse("-x");
    defer result.arena.deinit();

    try std.testing.expect(result.expr == .unary);
    try std.testing.expectEqual(ast.UnaryOp.negate, result.expr.unary.op);
}

test "parse index" {
    var result = try testParse("arr[0]");
    defer result.arena.deinit();

    try std.testing.expect(result.expr == .index);
}

test "parse range" {
    var result = try testParse("0..10");
    defer result.arena.deinit();

    try std.testing.expect(result.expr == .range);
    try std.testing.expect(!result.expr.range.inclusive);
}

test "parse inclusive range" {
    var result = try testParse("0..=10");
    defer result.arena.deinit();

    try std.testing.expect(result.expr == .range);
    try std.testing.expect(result.expr.range.inclusive);
}

// ============================================================================
// Statement Tests
// ============================================================================

fn testParseStmt(source: []const u8) !struct { stmt: ast.Stmt, arena: std.heap.ArenaAllocator } {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    errdefer arena.deinit();

    var lexer = Lexer.init(source);
    var parser = Parser.init(arena.allocator(), &lexer, source);

    const stmt = try parser.parseStatement();
    return .{ .stmt = stmt, .arena = arena };
}

test "parse let declaration with type" {
    var result = try testParseStmt("let x: i32 = 42");
    defer result.arena.deinit();

    try std.testing.expect(result.stmt == .let_decl);
    try std.testing.expectEqualStrings("x", result.stmt.let_decl.name);
    // Type is required and should be a named type "i32"
    try std.testing.expect(result.stmt.let_decl.type_ == .named);
    try std.testing.expectEqualStrings("i32", result.stmt.let_decl.type_.named.name);
}

test "parse var declaration with type" {
    var result = try testParseStmt("var count: i32 = 0");
    defer result.arena.deinit();

    try std.testing.expect(result.stmt == .var_decl);
    try std.testing.expectEqualStrings("count", result.stmt.var_decl.name);
    // Type is required
    try std.testing.expect(result.stmt.var_decl.type_ == .named);
    try std.testing.expectEqualStrings("i32", result.stmt.var_decl.type_.named.name);
}

test "parse return statement" {
    var result = try testParseStmt("return 42");
    defer result.arena.deinit();

    try std.testing.expect(result.stmt == .return_stmt);
    try std.testing.expect(result.stmt.return_stmt.value != null);
}

test "parse break statement" {
    var result = try testParseStmt("break value");
    defer result.arena.deinit();

    try std.testing.expect(result.stmt == .break_stmt);
    try std.testing.expect(result.stmt.break_stmt.value != null);
}

test "parse continue statement" {
    var result = try testParseStmt("continue");
    defer result.arena.deinit();

    try std.testing.expect(result.stmt == .continue_stmt);
}

test "parse while loop" {
    var result = try testParseStmt("while x { y }");
    defer result.arena.deinit();

    try std.testing.expect(result.stmt == .while_loop);
}

test "parse for loop with type" {
    var result = try testParseStmt("for item: i32 in items { print(item) }");
    defer result.arena.deinit();

    try std.testing.expect(result.stmt == .for_loop);
    // Check that the pattern is a binding with a type annotation
    try std.testing.expect(result.stmt.for_loop.pattern == .binding);
    try std.testing.expect(result.stmt.for_loop.pattern.binding.type_annotation != null);
}

test "parse loop statement" {
    var result = try testParseStmt("loop { break }");
    defer result.arena.deinit();

    try std.testing.expect(result.stmt == .loop_stmt);
}

// ============================================================================
// Declaration Tests
// ============================================================================

fn testParseDecl(source: []const u8) !struct { decl: ast.Decl, arena: std.heap.ArenaAllocator } {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    errdefer arena.deinit();

    var lexer = Lexer.init(source);
    var parser = Parser.init(arena.allocator(), &lexer, source);

    const decl = try parser.parseDeclaration();
    return .{ .decl = decl, .arena = arena };
}

test "parse function declaration" {
    var result = try testParseDecl("fn add(x: i32, y: i32) -> i32 { x + y }");
    defer result.arena.deinit();

    try std.testing.expect(result.decl == .function);
    try std.testing.expectEqualStrings("add", result.decl.function.name);
    try std.testing.expectEqual(@as(usize, 2), result.decl.function.params.len);
    try std.testing.expect(result.decl.function.return_type != null);
}

test "parse generic function" {
    var result = try testParseDecl("fn identity[T](x: T) -> T { x }");
    defer result.arena.deinit();

    try std.testing.expect(result.decl == .function);
    try std.testing.expectEqual(@as(usize, 1), result.decl.function.type_params.len);
    try std.testing.expectEqualStrings("T", result.decl.function.type_params[0].name);
}

test "parse pub function" {
    var result = try testParseDecl("pub fn hello() { }");
    defer result.arena.deinit();

    try std.testing.expect(result.decl == .function);
    try std.testing.expect(result.decl.function.is_pub);
}

test "parse single-expression function" {
    var result = try testParseDecl("fn double(x: i32) -> i32 = x * 2");
    defer result.arena.deinit();

    try std.testing.expect(result.decl == .function);
    try std.testing.expect(result.decl.function.body != null);
    try std.testing.expect(result.decl.function.body.?.final_expr != null);
}

test "parse struct declaration" {
    var result = try testParseDecl("struct Point { x: i32, y: i32 }");
    defer result.arena.deinit();

    try std.testing.expect(result.decl == .struct_decl);
    try std.testing.expectEqualStrings("Point", result.decl.struct_decl.name);
    try std.testing.expectEqual(@as(usize, 2), result.decl.struct_decl.fields.len);
}

test "parse generic struct" {
    var result = try testParseDecl("struct Container[T] { value: T }");
    defer result.arena.deinit();

    try std.testing.expect(result.decl == .struct_decl);
    try std.testing.expectEqual(@as(usize, 1), result.decl.struct_decl.type_params.len);
}

test "parse enum declaration" {
    var result = try testParseDecl("enum Color { Red, Green, Blue }");
    defer result.arena.deinit();

    try std.testing.expect(result.decl == .enum_decl);
    try std.testing.expectEqualStrings("Color", result.decl.enum_decl.name);
    try std.testing.expectEqual(@as(usize, 3), result.decl.enum_decl.variants.len);
}

test "parse enum with tuple payload" {
    var result = try testParseDecl("enum Option[T] { Some(T), None }");
    defer result.arena.deinit();

    try std.testing.expect(result.decl == .enum_decl);
    try std.testing.expectEqual(@as(usize, 2), result.decl.enum_decl.variants.len);
    try std.testing.expect(result.decl.enum_decl.variants[0].payload != null);
}

test "parse trait declaration" {
    var result = try testParseDecl("trait Display { fn display(self: Self) -> string }");
    defer result.arena.deinit();

    try std.testing.expect(result.decl == .trait_decl);
    try std.testing.expectEqualStrings("Display", result.decl.trait_decl.name);
    try std.testing.expectEqual(@as(usize, 1), result.decl.trait_decl.methods.len);
}

test "parse impl declaration" {
    var result = try testParseDecl("impl Point { fn new() -> Point { } }");
    defer result.arena.deinit();

    try std.testing.expect(result.decl == .impl_decl);
    try std.testing.expect(result.decl.impl_decl.trait_type == null);
    try std.testing.expectEqual(@as(usize, 1), result.decl.impl_decl.methods.len);
}

test "parse impl trait for type" {
    var result = try testParseDecl("impl Point: Display { fn display(self: Self) -> string { } }");
    defer result.arena.deinit();

    try std.testing.expect(result.decl == .impl_decl);
    try std.testing.expect(result.decl.impl_decl.trait_type != null);
}

test "parse type alias" {
    var result = try testParseDecl("type Int = i32");
    defer result.arena.deinit();

    try std.testing.expect(result.decl == .type_alias);
    try std.testing.expectEqualStrings("Int", result.decl.type_alias.name);
}

test "parse const declaration" {
    var result = try testParseDecl("const PI = 3.14159");
    defer result.arena.deinit();

    try std.testing.expect(result.decl == .const_decl);
    try std.testing.expectEqualStrings("PI", result.decl.const_decl.name);
}

test "parse import declaration" {
    var result = try testParseDecl("import std.io");
    defer result.arena.deinit();

    try std.testing.expect(result.decl == .import_decl);
    try std.testing.expectEqual(@as(usize, 2), result.decl.import_decl.path.len);
}

test "parse import with items" {
    var result = try testParseDecl("import std.collections.{ List, Map }");
    defer result.arena.deinit();

    try std.testing.expect(result.decl == .import_decl);
    try std.testing.expect(result.decl.import_decl.items != null);
}

test "parse module declaration" {
    var result = try testParseDecl("module myapp.utils");
    defer result.arena.deinit();

    try std.testing.expect(result.decl == .module_decl);
    try std.testing.expectEqual(@as(usize, 2), result.decl.module_decl.path.len);
}

// ============================================================================
// Module Tests
// ============================================================================

fn testParseModule(source: []const u8) !struct { module: ast.Module, arena: std.heap.ArenaAllocator } {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    errdefer arena.deinit();

    var lexer = Lexer.init(source);
    var parser = Parser.init(arena.allocator(), &lexer, source);

    const module = try parser.parseModule();
    return .{ .module = module, .arena = arena };
}

test "parse complete module" {
    const source =
        \\module mymodule
        \\
        \\import std.io
        \\
        \\fn main() {
        \\    println("Hello")
        \\}
    ;

    var result = try testParseModule(source);
    defer result.arena.deinit();

    try std.testing.expect(result.module.module_decl != null);
    try std.testing.expectEqual(@as(usize, 1), result.module.imports.len);
    try std.testing.expectEqual(@as(usize, 1), result.module.declarations.len);
}
