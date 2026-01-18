const std = @import("std");
const Token = @import("token.zig").Token;

pub const Lexer = struct {
    source: []const u8,
    pos: usize,
    line: u32,
    column: u32,
    start_pos: usize,
    start_line: u32,
    start_column: u32,

    pub fn init(source: []const u8) Lexer {
        return .{
            .source = source,
            .pos = 0,
            .line = 1,
            .column = 1,
            .start_pos = 0,
            .start_line = 1,
            .start_column = 1,
        };
    }

    pub fn next(self: *Lexer) Token {
        self.skipWhitespace();
        self.start_pos = self.pos;
        self.start_line = self.line;
        self.start_column = self.column;

        if (self.isAtEnd()) {
            return self.makeToken(.eof);
        }

        const c = self.advance();

        if (isAlpha(c)) return self.identifier();
        if (isDigit(c)) return self.number();

        return switch (c) {
            '(' => self.makeToken(.l_paren),
            ')' => self.makeToken(.r_paren),
            '[' => self.makeToken(.l_bracket),
            ']' => self.makeToken(.r_bracket),
            '{' => self.makeToken(.l_brace),
            '}' => self.makeToken(.r_brace),
            ',' => self.makeToken(.comma),
            ';' => self.makeToken(.semicolon),
            '+' => self.handlePlus(),
            '-' => self.handleMinus(),
            '*' => self.handleStar(),
            '/' => self.handleSlash(),
            '%' => self.handlePercent(),
            '=' => self.matchChar('=', .eq_eq, '>', .fat_arrow, .eq),
            '!' => self.matchSingle('=', .not_eq, .bang),
            '<' => self.handleLt(),
            '>' => self.handleGt(),
            '&' => self.makeToken(.amp),
            '|' => self.makeToken(.pipe),
            '^' => self.makeToken(.caret),
            '~' => self.makeToken(.tilde),
            '?' => self.matchSingle('?', .question_question, .question),
            '.' => self.handleDot(),
            ':' => self.matchSingle(':', .colon_colon, .colon),
            '@' => self.makeToken(.at),
            '"' => self.string(),
            '\'' => self.char(),
            '\n' => self.handleNewline(),
            else => self.makeToken(.invalid),
        };
    }

    fn skipWhitespace(self: *Lexer) void {
        while (!self.isAtEnd()) {
            switch (self.peek()) {
                ' ', '\t', '\r' => _ = self.advance(),
                '/' => {
                    if (self.peekNext() == '/') {
                        // Line comment
                        while (!self.isAtEnd() and self.peek() != '\n') {
                            _ = self.advance();
                        }
                    } else if (self.peekNext() == '*') {
                        // Block comment
                        _ = self.advance();
                        _ = self.advance();
                        while (!self.isAtEnd()) {
                            if (self.peek() == '*' and self.peekNext() == '/') {
                                _ = self.advance();
                                _ = self.advance();
                                break;
                            }
                            if (self.peek() == '\n') {
                                self.line += 1;
                                self.column = 0;
                            }
                            _ = self.advance();
                        }
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    fn identifier(self: *Lexer) Token {
        while (!self.isAtEnd() and (isAlpha(self.peek()) or isDigit(self.peek()))) {
            _ = self.advance();
        }
        const text = self.source[self.start_pos..self.pos];
        const kind = keywords.get(text) orelse .identifier;
        return self.makeToken(kind);
    }

    fn number(self: *Lexer) Token {
        // Handle hex, binary, octal
        if (self.source[self.start_pos] == '0' and !self.isAtEnd()) {
            switch (self.peek()) {
                'x', 'X' => {
                    _ = self.advance();
                    while (!self.isAtEnd() and (isHexDigit(self.peek()) or self.peek() == '_')) {
                        _ = self.advance();
                    }
                    return self.makeToken(.int_literal);
                },
                'b', 'B' => {
                    _ = self.advance();
                    while (!self.isAtEnd() and (self.peek() == '0' or self.peek() == '1' or self.peek() == '_')) {
                        _ = self.advance();
                    }
                    return self.makeToken(.int_literal);
                },
                'o', 'O' => {
                    _ = self.advance();
                    while (!self.isAtEnd() and (self.peek() >= '0' and self.peek() <= '7' or self.peek() == '_')) {
                        _ = self.advance();
                    }
                    return self.makeToken(.int_literal);
                },
                else => {},
            }
        }

        while (!self.isAtEnd() and (isDigit(self.peek()) or self.peek() == '_')) {
            _ = self.advance();
        }

        var is_float = false;
        if (!self.isAtEnd() and self.peek() == '.' and isDigit(self.peekNext())) {
            is_float = true;
            _ = self.advance();
            while (!self.isAtEnd() and (isDigit(self.peek()) or self.peek() == '_')) {
                _ = self.advance();
            }
        }

        // Scientific notation
        if (!self.isAtEnd() and (self.peek() == 'e' or self.peek() == 'E')) {
            is_float = true;
            _ = self.advance();
            if (!self.isAtEnd() and (self.peek() == '+' or self.peek() == '-')) {
                _ = self.advance();
            }
            while (!self.isAtEnd() and isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        // Type suffix (i32, u64, f32, etc.)
        if (!self.isAtEnd() and isAlpha(self.peek())) {
            while (!self.isAtEnd() and (isAlpha(self.peek()) or isDigit(self.peek()))) {
                _ = self.advance();
            }
        }

        return self.makeToken(if (is_float) .float_literal else .int_literal);
    }

    fn string(self: *Lexer) Token {
        // Check for multi-line string """
        if (self.peek() == '"' and self.peekNext() == '"') {
            _ = self.advance(); // second "
            _ = self.advance(); // third "
            return self.multiLineString();
        }

        while (!self.isAtEnd() and self.peek() != '"') {
            if (self.peek() == '\n') {
                self.line += 1;
                self.column = 0;
            }
            if (self.peek() == '\\' and !self.isAtEnd()) {
                _ = self.advance(); // skip escape char
            }
            _ = self.advance();
        }
        if (self.isAtEnd()) return self.makeToken(.invalid);
        _ = self.advance(); // closing "
        return self.makeToken(.string_literal);
    }

    fn multiLineString(self: *Lexer) Token {
        while (!self.isAtEnd()) {
            if (self.peek() == '"' and self.peekNext() == '"' and self.peekN(2) == '"') {
                _ = self.advance();
                _ = self.advance();
                _ = self.advance();
                return self.makeToken(.string_literal);
            }
            if (self.peek() == '\n') {
                self.line += 1;
                self.column = 0;
            }
            _ = self.advance();
        }
        return self.makeToken(.invalid);
    }

    fn char(self: *Lexer) Token {
        if (!self.isAtEnd() and self.peek() == '\\') {
            _ = self.advance(); // backslash
            if (!self.isAtEnd()) {
                // Handle unicode escapes \u{...}
                if (self.peek() == 'u' and self.peekNext() == '{') {
                    _ = self.advance(); // u
                    _ = self.advance(); // {
                    while (!self.isAtEnd() and self.peek() != '}') {
                        _ = self.advance();
                    }
                    if (!self.isAtEnd()) _ = self.advance(); // }
                } else {
                    _ = self.advance(); // escape sequence char
                }
            }
        } else if (!self.isAtEnd()) {
            _ = self.advance(); // regular char
        }
        if (!self.isAtEnd() and self.peek() == '\'') {
            _ = self.advance();
            return self.makeToken(.char_literal);
        }
        return self.makeToken(.invalid);
    }

    fn handlePlus(self: *Lexer) Token {
        if (!self.isAtEnd()) {
            switch (self.peek()) {
                '%' => {
                    _ = self.advance();
                    return self.makeToken(.plus_wrap);
                },
                '|' => {
                    _ = self.advance();
                    return self.makeToken(.plus_sat);
                },
                '=' => {
                    _ = self.advance();
                    return self.makeToken(.plus_eq);
                },
                else => {},
            }
        }
        return self.makeToken(.plus);
    }

    fn handleMinus(self: *Lexer) Token {
        if (!self.isAtEnd()) {
            switch (self.peek()) {
                '>' => {
                    _ = self.advance();
                    return self.makeToken(.arrow);
                },
                '%' => {
                    _ = self.advance();
                    return self.makeToken(.minus_wrap);
                },
                '|' => {
                    _ = self.advance();
                    return self.makeToken(.minus_sat);
                },
                '=' => {
                    _ = self.advance();
                    return self.makeToken(.minus_eq);
                },
                else => {},
            }
        }
        return self.makeToken(.minus);
    }

    fn handleStar(self: *Lexer) Token {
        if (!self.isAtEnd()) {
            switch (self.peek()) {
                '%' => {
                    _ = self.advance();
                    return self.makeToken(.star_wrap);
                },
                '|' => {
                    _ = self.advance();
                    return self.makeToken(.star_sat);
                },
                '=' => {
                    _ = self.advance();
                    return self.makeToken(.star_eq);
                },
                else => {},
            }
        }
        return self.makeToken(.star);
    }

    fn handleSlash(self: *Lexer) Token {
        if (!self.isAtEnd() and self.peek() == '=') {
            _ = self.advance();
            return self.makeToken(.slash_eq);
        }
        return self.makeToken(.slash);
    }

    fn handlePercent(self: *Lexer) Token {
        if (!self.isAtEnd() and self.peek() == '=') {
            _ = self.advance();
            return self.makeToken(.percent_eq);
        }
        return self.makeToken(.percent);
    }

    fn handleLt(self: *Lexer) Token {
        if (!self.isAtEnd()) {
            if (self.peek() == '=') {
                _ = self.advance();
                return self.makeToken(.lt_eq);
            }
            if (self.peek() == '<') {
                _ = self.advance();
                return self.makeToken(.lt_lt);
            }
        }
        return self.makeToken(.lt);
    }

    fn handleGt(self: *Lexer) Token {
        if (!self.isAtEnd()) {
            if (self.peek() == '=') {
                _ = self.advance();
                return self.makeToken(.gt_eq);
            }
            if (self.peek() == '>') {
                _ = self.advance();
                return self.makeToken(.gt_gt);
            }
        }
        return self.makeToken(.gt);
    }

    fn handleDot(self: *Lexer) Token {
        if (!self.isAtEnd() and self.peek() == '.') {
            _ = self.advance();
            if (!self.isAtEnd() and self.peek() == '=') {
                _ = self.advance();
                return self.makeToken(.dot_dot_eq);
            }
            return self.makeToken(.dot_dot);
        }
        return self.makeToken(.dot);
    }

    fn handleNewline(self: *Lexer) Token {
        const tok = self.makeToken(.newline);
        self.line += 1;
        self.column = 1;
        return tok;
    }

    fn advance(self: *Lexer) u8 {
        const c = self.source[self.pos];
        self.pos += 1;
        self.column += 1;
        return c;
    }

    fn peek(self: *Lexer) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.pos];
    }

    fn peekNext(self: *Lexer) u8 {
        if (self.pos + 1 >= self.source.len) return 0;
        return self.source[self.pos + 1];
    }

    fn peekN(self: *Lexer, n: usize) u8 {
        if (self.pos + n >= self.source.len) return 0;
        return self.source[self.pos + n];
    }

    fn isAtEnd(self: *Lexer) bool {
        return self.pos >= self.source.len;
    }

    fn matchSingle(self: *Lexer, expected: u8, if_match: Token.Kind, otherwise: Token.Kind) Token {
        if (!self.isAtEnd() and self.peek() == expected) {
            _ = self.advance();
            return self.makeToken(if_match);
        }
        return self.makeToken(otherwise);
    }

    fn matchChar(self: *Lexer, c1: u8, k1: Token.Kind, c2: u8, k2: Token.Kind, default: Token.Kind) Token {
        if (!self.isAtEnd()) {
            if (self.peek() == c1) {
                _ = self.advance();
                return self.makeToken(k1);
            }
            if (self.peek() == c2) {
                _ = self.advance();
                return self.makeToken(k2);
            }
        }
        return self.makeToken(default);
    }

    fn makeToken(self: *Lexer, kind: Token.Kind) Token {
        return .{
            .kind = kind,
            .loc = .{
                .start = self.start_pos,
                .end = self.pos,
                .line = self.start_line,
                .column = self.start_column,
            },
        };
    }

    const keywords = std.StaticStringMap(Token.Kind).initComptime(.{
        .{ "fn", .fn_ },
        .{ "let", .let },
        .{ "var", .var_ },
        .{ "struct", .struct_ },
        .{ "enum", .enum_ },
        .{ "trait", .trait },
        .{ "impl", .impl },
        .{ "if", .if_ },
        .{ "else", .else_ },
        .{ "match", .match },
        .{ "for", .for_ },
        .{ "while", .while_ },
        .{ "loop", .loop },
        .{ "return", .return_ },
        .{ "break", .break_ },
        .{ "continue", .continue_ },
        .{ "pub", .pub_ },
        .{ "mut", .mut },
        .{ "async", .async_ },
        .{ "await", .await_ },
        .{ "unsafe", .unsafe_ },
        .{ "import", .import },
        .{ "module", .module },
        .{ "as", .as },
        .{ "in", .in },
        .{ "is", .is },
        .{ "and", .and_ },
        .{ "or", .or_ },
        .{ "not", .not },
        .{ "true", .true_ },
        .{ "false", .false_ },
        .{ "comptime", .comptime_ },
        .{ "where", .where },
        .{ "dyn", .dyn },
        .{ "type", .type_ },
        .{ "const", .const_ },
        .{ "static", .static },
        .{ "self", .self },
        .{ "Self", .self_type },
    });
};

fn isAlpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isHexDigit(c: u8) bool {
    return isDigit(c) or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F');
}

// ============================================================================
// Tests
// ============================================================================

test "lexer basics" {
    const source = "fn main() { let x = 42 }";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Kind.fn_, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.identifier, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.l_paren, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.r_paren, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.l_brace, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.let, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.identifier, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.eq, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.int_literal, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.r_brace, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.eof, lexer.next().kind);
}

test "lexer operators" {
    const source = "+% -| * / == != <= >= -> => ?? ..";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Kind.plus_wrap, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.minus_sat, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.star, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.slash, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.eq_eq, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.not_eq, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.lt_eq, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.gt_eq, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.arrow, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.fat_arrow, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.question_question, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.dot_dot, lexer.next().kind);
}

test "lexer strings" {
    const source =
        \\"hello world"
    ;
    var lexer = Lexer.init(source);

    const tok = lexer.next();
    try std.testing.expectEqual(Token.Kind.string_literal, tok.kind);
}

test "lexer numbers" {
    var lexer = Lexer.init("42 3.14 0xff 0b1010 0o755 42i64 1e10");

    try std.testing.expectEqual(Token.Kind.int_literal, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.float_literal, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.int_literal, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.int_literal, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.int_literal, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.int_literal, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.float_literal, lexer.next().kind);
}

test "lexer keywords" {
    var lexer = Lexer.init("fn let var struct enum if else match for while loop return");

    try std.testing.expectEqual(Token.Kind.fn_, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.let, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.var_, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.struct_, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.enum_, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.if_, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.else_, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.match, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.for_, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.while_, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.loop, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.return_, lexer.next().kind);
}

test "lexer comments" {
    var lexer = Lexer.init("a // comment\nb /* block */ c");

    try std.testing.expectEqual(Token.Kind.identifier, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.newline, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.identifier, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.identifier, lexer.next().kind);
}

test "lexer compound assignment" {
    var lexer = Lexer.init("+= -= *= /= %=");

    try std.testing.expectEqual(Token.Kind.plus_eq, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.minus_eq, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.star_eq, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.slash_eq, lexer.next().kind);
    try std.testing.expectEqual(Token.Kind.percent_eq, lexer.next().kind);
}

test "lexer location tracking" {
    var lexer = Lexer.init("fn\nmain");

    const fn_tok = lexer.next();
    try std.testing.expectEqual(@as(u32, 1), fn_tok.loc.line);
    try std.testing.expectEqual(@as(u32, 1), fn_tok.loc.column);

    _ = lexer.next(); // newline

    const main_tok = lexer.next();
    try std.testing.expectEqual(@as(u32, 2), main_tok.loc.line);
    try std.testing.expectEqual(@as(u32, 1), main_tok.loc.column);
}
