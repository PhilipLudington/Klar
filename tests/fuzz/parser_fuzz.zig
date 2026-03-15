const std = @import("std");
const Lexer = @import("lexer");
const Parser = @import("parser");
const Token = @import("token");

// Property: Lexer always terminates and produces EOF as last token.
// Property: All token spans are within source bounds.
fn fuzzLexer(_: void, source: []const u8) anyerror!void {
    var lexer = Lexer.Lexer.init(source);
    var token_count: usize = 0;
    const max_tokens = source.len * 4 + 1024; // generous upper bound

    while (true) {
        const tok = lexer.next();

        // Property: token positions within source bounds
        try std.testing.expect(tok.loc.start <= source.len);
        try std.testing.expect(tok.loc.end <= source.len);
        try std.testing.expect(tok.loc.start <= tok.loc.end);

        // Property: line/column are positive
        try std.testing.expect(tok.loc.line >= 1);
        try std.testing.expect(tok.loc.column >= 1);

        if (tok.kind == .eof) break;

        token_count += 1;
        // Property: lexer always terminates (no infinite loop)
        if (token_count > max_tokens) {
            return error.LexerInfiniteLoop;
        }
    }
}

// Property: Parser always terminates, never crashes on arbitrary input.
// Property: Parse errors are reported, not panics.
fn fuzzParser(_: void, source: []const u8) anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var lexer = Lexer.Lexer.init(source);
    var parser = Parser.Parser.init(alloc, &lexer, source);
    defer parser.deinit();

    // parseModuleRecovering should never crash — it recovers from errors
    const module = parser.parseModuleRecovering() catch |err| switch (err) {
        error.OutOfMemory => return, // OOM is acceptable under fuzzing
        else => return, // parse errors are expected
    };

    // Property: if parsing succeeded, module has valid structure
    if (parser.errors.items.len == 0) {
        // Valid parse — declarations should be accessible
        for (module.declarations) |decl| {
            _ = decl; // just verify no crash accessing declarations
        }
        for (module.imports) |imp| {
            _ = imp; // verify no crash accessing imports
        }
    }
}

test "fuzz lexer" {
    return std.testing.fuzz({}, fuzzLexer, .{
        .corpus = &.{
            // Empty input
            "",
            // Simple tokens
            "42",
            "3.14",
            "\"hello\"",
            "'c'",
            "true false",
            "let x: i32 = 5",
            // Keywords
            "fn struct enum trait impl import module",
            // Operators
            "+ - * / % == != < > <= >= and or not",
            // Delimiters
            "( ) [ ] { } , ; : . .. ...",
            // String with interpolation
            "\"hello {name}\"",
            // Comments
            "// comment\n42",
            "/// doc comment\nfn f() -> void {}",
            // Edge cases
            "\x00",
            "\xff\xfe\xfd",
            "0x1F 0b1010 0o777",
            "1_000_000",
            "3.14e10",
        },
    });
}

test "fuzz parser" {
    return std.testing.fuzz({}, fuzzParser, .{
        .corpus = &.{
            // Empty
            "",
            // Minimal valid programs
            "fn main() -> void {}",
            "let x: i32 = 42",
            "struct Point { x: f64, y: f64 }",
            "enum Color { Red, Green, Blue }",
            // Generics
            "fn max#[T: Ordered](a: T, b: T) -> T { return a }",
            "struct Pair#[A, B] { first: A, second: B }",
            // Traits
            "trait Drawable { fn draw(self) -> void }",
            "impl Point: Drawable { fn draw(self) -> void {} }",
            // Control flow
            "fn f() -> i32 { if true { return 1 } else { return 0 } }",
            "fn f() -> void { for i: i32 in 0..10 { println(i.to_string()) } }",
            "fn f(x: i32) -> string { match x { 0 => { return \"zero\" } _ => { return \"other\" } } }",
            // Imports
            "import std.io",
            "module mylib",
            // Error handling
            "fn f() -> ?i32 { return Some(42) }",
            "fn f() -> Result#[i32, string] { return Ok(1) }",
            // Complex expressions
            "fn f() -> i32 { let a: [i32; 3] = [1, 2, 3]\n return a[0] }",
            // Closures
            "fn f() -> void { let g: fn(i32) -> i32 = |x: i32| -> i32 { return x * 2 } }",
            // Malformed but shouldn't crash
            "fn {}",
            "let = ;",
            "struct { }",
            "if then else",
            "{{{{{{",
            "}}}}}}",
            "((((((",
            "))))))",
            "######",
            "fn fn fn fn",
            "123abc",
            "\"unterminated string",
            "'",
            "/* not a comment */",
        },
    });
}
