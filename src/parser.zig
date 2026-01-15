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

            // Identifier
            .identifier => self.parseIdentifierOrStructLiteral(),

            // Grouping / tuple
            .l_paren => self.parseGroupedOrTuple(),

            // Array literal
            .l_bracket => self.parseArrayLiteral(),

            // Block expression
            .l_brace => self.parseBlockExpr(),

            // If expression
            .if_ => self.parseIfExpr(),

            // Match expression
            .match => self.parseMatchExpr(),

            // Closure
            .pipe => self.parseClosure(),
            .fn_ => self.parseFnClosure(),

            // Unary operators
            .minus => self.parseUnary(.negate),
            .not => self.parseUnary(.not),
            .amp => self.parseRefOrRefMut(),
            .star => self.parseUnary(.deref),

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

        return .{ .literal = .{
            .kind = .{ .string = content },
            .span = span,
        } };
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

    fn parseIdentifierOrStructLiteral(self: *Parser) ParseError!ast.Expr {
        const start_span = self.spanFromToken(self.current);
        const name = self.tokenText(self.current);
        self.advance();

        // Check for struct literal: Identifier { ... }
        // Only parse as struct literal if the identifier starts with uppercase (type convention)
        // This prevents `x { ... }` from being parsed as struct literal in contexts like `if x { }`
        if (self.check(.l_brace) and name.len > 0 and std.ascii.isUpper(name[0])) {
            return self.parseStructLiteralWithName(name, start_span);
        }

        return .{ .identifier = .{
            .name = name,
            .span = start_span,
        } };
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

    fn parseStructLiteralWithName(self: *Parser, name: []const u8, start_span: ast.Span) ParseError!ast.Expr {
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
            .type_name = .{ .named = .{ .name = name, .span = start_span } },
            .fields = try self.dupeSlice(ast.StructFieldInit, fields.items),
            .spread = spread,
            .span = ast.Span.merge(start_span, end_span),
        });
        return .{ .struct_literal = struct_lit };
    }

    fn parseBlockExpr(self: *Parser) ParseError!ast.Expr {
        const block = try self.parseBlock();
        return .{ .block = block };
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

            // Try to parse as expression first
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

    fn parseIfExpr(self: *Parser) ParseError!ast.Expr {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'if'

        const condition = try self.parseExpression();
        const then_block = try self.parseBlock();
        const then_expr = ast.Expr{ .block = then_block };

        var else_branch: ?ast.Expr = null;
        if (self.match(.else_)) {
            if (self.check(.if_)) {
                else_branch = try self.parseIfExpr();
            } else {
                const else_block = try self.parseBlock();
                else_branch = ast.Expr{ .block = else_block };
            }
        }

        const end_span = if (else_branch) |eb| eb.span() else then_expr.span();

        const if_expr = try self.create(ast.IfExpr, .{
            .condition = condition,
            .then_branch = then_expr,
            .else_branch = else_branch,
            .span = ast.Span.merge(start_span, end_span),
        });
        return .{ .if_expr = if_expr };
    }

    fn parseMatchExpr(self: *Parser) ParseError!ast.Expr {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'match'

        // Note: Klar uses "value match { ... }" syntax, but we could also support "match value { ... }"
        // For now, implement as if we're parsing: subject match { arms }
        // Actually looking at the spec, it's "value match { ... }"
        // But this function is called when we see 'match', so let's assume standalone match
        // We'll need to handle this in infix parsing instead for "expr match { }"

        try self.consume(.l_brace, "expected '{' after match");

        var arms = std.ArrayListUnmanaged(ast.MatchArm){};

        while (!self.check(.r_brace) and !self.check(.eof)) {
            while (self.match(.newline)) {}
            if (self.check(.r_brace)) break;

            const arm = try self.parseMatchArm();
            try arms.append(self.allocator, arm);

            _ = self.match(.newline) or self.match(.comma);
        }

        try self.consume(.r_brace, "expected '}' after match arms");
        const end_span = self.spanFromToken(self.previous);

        // This is incomplete - we need the subject. For standalone match, we'd need different syntax.
        // For now, create a placeholder
        const match_expr = try self.create(ast.MatchExpr, .{
            .subject = .{ .literal = .{ .kind = .{ .bool_ = true }, .span = start_span } },
            .arms = try self.dupeSlice(ast.MatchArm, arms.items),
            .span = ast.Span.merge(start_span, end_span),
        });
        return .{ .match_expr = match_expr };
    }

    fn parseMatchArm(self: *Parser) ParseError!ast.MatchArm {
        const pattern = try self.parsePattern();

        var guard: ?ast.Expr = null;
        if (self.match(.if_)) {
            guard = try self.parseExpression();
        }

        try self.consume(.fat_arrow, "expected '=>' after pattern");
        const body = try self.parseExpression();

        return .{
            .pattern = pattern,
            .guard = guard,
            .body = body,
            .span = ast.Span.merge(pattern.span(), body.span()),
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

                var param_type: ?ast.TypeExpr = null;
                if (self.match(.colon)) {
                    param_type = try self.parseType();
                }

                try params.append(self.allocator, .{
                    .name = param_name,
                    .type_ = param_type,
                    .span = param_span,
                });

                if (!self.match(.comma)) break;
            }
        }

        try self.consume(.pipe, "expected '|' after closure parameters");

        var return_type: ?ast.TypeExpr = null;
        if (self.match(.arrow)) {
            return_type = try self.parseType();
        }

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

    fn parseFnClosure(self: *Parser) ParseError!ast.Expr {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume 'fn'

        try self.consume(.l_paren, "expected '(' after fn");

        var params = std.ArrayListUnmanaged(ast.ClosureParam){};

        if (!self.check(.r_paren)) {
            while (true) {
                const param_name = try self.consumeIdentifier();
                const param_span = self.spanFromToken(self.previous);

                try self.consume(.colon, "expected ':' after parameter name");
                const param_type = try self.parseType();

                try params.append(self.allocator, .{
                    .name = param_name,
                    .type_ = param_type,
                    .span = param_span,
                });

                if (!self.match(.comma)) break;
            }
        }

        try self.consume(.r_paren, "expected ')' after parameters");

        var return_type: ?ast.TypeExpr = null;
        if (self.match(.arrow)) {
            return_type = try self.parseType();
        }

        const body = if (self.check(.l_brace))
            ast.Expr{ .block = try self.parseBlock() }
        else
            try self.parseExpression();

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

    fn parseRefOrRefMut(self: *Parser) ParseError!ast.Expr {
        const start_span = self.spanFromToken(self.current);
        self.advance(); // consume '&'

        const op: ast.UnaryOp = if (self.match(.mut)) .ref_mut else .ref;
        const operand = try self.parsePrecedence(.unary);
        const end_span = operand.span();

        const unary = try self.create(ast.Unary, .{
            .op = op,
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

        // Check for type cast methods: .as[T], .to[T], .trunc[T]
        if (self.check(.as) or self.tokenText(self.current).len > 0) {
            const method_name = self.tokenText(self.current);
            if (std.mem.eql(u8, method_name, "as") or
                std.mem.eql(u8, method_name, "to") or
                std.mem.eql(u8, method_name, "trunc"))
            {
                return self.parseTypeCast(object, method_name);
            }
        }

        const field_name = try self.consumeIdentifier();

        // Check for method call
        if (self.check(.l_paren)) {
            return self.parseMethodCall(object, field_name);
        }

        const end_span = self.spanFromToken(self.previous);
        const field = try self.create(ast.Field, .{
            .object = object,
            .field_name = field_name,
            .span = ast.Span.merge(object.span(), end_span),
        });
        return .{ .field = field };
    }

    fn parseTypeCast(self: *Parser, object: ast.Expr, method_name: []const u8) ParseError!ast.Expr {
        self.advance(); // consume 'as'/'to'/'trunc'

        try self.consume(.l_bracket, "expected '[' after cast method");
        const target_type = try self.parseType();
        try self.consume(.r_bracket, "expected ']' after type");

        const end_span = self.spanFromToken(self.previous);

        const cast_kind: ast.CastKind = if (std.mem.eql(u8, method_name, "as"))
            .as
        else if (std.mem.eql(u8, method_name, "to"))
            .to
        else
            .trunc;

        const type_cast = try self.create(ast.TypeCast, .{
            .expr = object,
            .cast_kind = cast_kind,
            .target_type = target_type,
            .span = ast.Span.merge(object.span(), end_span),
        });
        return .{ .type_cast = type_cast };
    }

    fn parseMethodCall(self: *Parser, object: ast.Expr, method_name: []const u8) ParseError!ast.Expr {
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
            .type_args = null,
            .args = try self.dupeSlice(ast.Expr, args.items),
            .span = ast.Span.merge(object.span(), end_span),
        });
        return .{ .method_call = method_call };
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
        const span = self.spanFromToken(self.current);
        const name = self.tokenText(self.current);
        self.advance();

        // Check for wildcard
        if (std.mem.eql(u8, name, "_")) {
            return .{ .wildcard = .{ .span = span } };
        }

        // Check for variant: Type.Variant or just Variant(payload)
        if (self.match(.dot)) {
            const variant_name = try self.consumeIdentifier();
            var payload: ?ast.Pattern = null;

            if (self.match(.l_paren)) {
                payload = try self.parsePattern();
                try self.consume(.r_paren, "expected ')' after variant payload");
            }

            const end_span = self.spanFromToken(self.previous);
            const variant = try self.create(ast.VariantPattern, .{
                .type_name = name,
                .variant_name = variant_name,
                .payload = payload,
                .span = ast.Span.merge(span, end_span),
            });
            return .{ .variant = variant };
        }

        // Simple binding
        return .{ .binding = .{
            .name = name,
            .mutable = false,
            .span = span,
        } };
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

        // Reference type: &T or &mut T
        if (self.match(.amp)) {
            const mutable = self.match(.mut);
            const inner = try self.parseType();
            const ref = try self.create(ast.ReferenceType, .{
                .inner = inner,
                .mutable = mutable,
                .span = inner.span(),
            });
            return .{ .reference = ref };
        }

        // Slice type: [T]
        if (self.match(.l_bracket)) {
            const element = try self.parseType();
            try self.consume(.r_bracket, "expected ']' after slice type");
            const slice = try self.create(ast.SliceType, .{
                .element = element,
                .span = element.span(),
            });
            return .{ .slice = slice };
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

        // Named type (with possible generic args)
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

            return base;
        }

        try self.reportError("expected type");
        return ParseError.ExpectedType;
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

test "parse if expression" {
    var result = try testParse("if x { 1 } else { 2 }");
    defer result.arena.deinit();

    try std.testing.expect(result.expr == .if_expr);
    try std.testing.expect(result.expr.if_expr.else_branch != null);
}

test "parse closure" {
    var result = try testParse("|x, y| x + y");
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
