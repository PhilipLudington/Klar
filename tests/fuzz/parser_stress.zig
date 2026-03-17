const std = @import("std");
const Lexer = @import("lexer");
const Parser = @import("parser");

/// Standalone parser stress test — generates random inputs and verifies
/// the lexer and parser never crash. Run as: zig build fuzz-stress -- [iterations]
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var args_it = std.process.args();
    _ = args_it.next(); // skip program name

    var iterations: usize = 1_000_000;
    if (args_it.next()) |arg| {
        iterations = std.fmt.parseInt(usize, arg, 10) catch 1_000_000;
    }

    var prng = std.Random.DefaultPrng.init(0xDEADBEEF);
    const rand = prng.random();

    var crashes: usize = 0;
    var lexer_tested: usize = 0;
    var parser_tested: usize = 0;
    var structured_tested: usize = 0;

    const start = std.time.nanoTimestamp();

    var i: usize = 0;
    while (i < iterations) : (i += 1) {
        // Progress reporting every 100K
        if (i > 0 and i % 100_000 == 0) {
            const elapsed_ns: u64 = @intCast(std.time.nanoTimestamp() - start);
            const elapsed_s = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000_000.0;
            const rate = @as(f64, @floatFromInt(i)) / elapsed_s;
            std.debug.print("\r  {d}/{d} ({d:.0}/s)...", .{ i, iterations, rate });
        }

        const buf = try allocator.alloc(u8, rand.intRangeAtMost(usize, 0, 4096));
        defer allocator.free(buf);
        rand.bytes(buf);

        // Phase 1: Fuzz the lexer with random bytes
        {
            var lexer = Lexer.Lexer.init(buf);
            var token_count: usize = 0;
            const max_tokens = buf.len * 4 + 1024;
            while (true) {
                const tok = lexer.next();
                if (tok.kind == .eof) break;
                token_count += 1;
                if (token_count > max_tokens) {
                    std.debug.print("\nLEXER INFINITE LOOP at iteration {d}, input len {d}\n", .{ i, buf.len });
                    crashes += 1;
                    break;
                }
            }
            lexer_tested += 1;
        }

        // Phase 2: Fuzz the parser with random bytes
        {
            var arena = std.heap.ArenaAllocator.init(allocator);
            defer arena.deinit();

            var lexer = Lexer.Lexer.init(buf);
            var parser = Parser.Parser.init(arena.allocator(), &lexer, buf);
            defer parser.deinit();

            _ = parser.parseModuleRecovering() catch |err| switch (err) {
                error.OutOfMemory => {},
                else => {},
            };
            parser_tested += 1;
        }

        // Phase 3: Every 3rd iteration, generate structured Klar-like input
        if (i % 3 == 0) {
            const structured = try generateStructuredInput(allocator, rand);
            defer allocator.free(structured);

            var arena = std.heap.ArenaAllocator.init(allocator);
            defer arena.deinit();

            var lexer = Lexer.Lexer.init(structured);
            var parser = Parser.Parser.init(arena.allocator(), &lexer, structured);
            defer parser.deinit();

            _ = parser.parseModuleRecovering() catch |err| switch (err) {
                error.OutOfMemory => {},
                else => {},
            };
            structured_tested += 1;
        }
    }

    const elapsed_ns: u64 = @intCast(std.time.nanoTimestamp() - start);
    const elapsed_s = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000_000.0;

    std.debug.print("\r", .{});
    std.debug.print("=== Parser Stress Test Results ===\n", .{});
    std.debug.print("Iterations:       {d}\n", .{iterations});
    std.debug.print("Lexer tested:     {d}\n", .{lexer_tested});
    std.debug.print("Parser tested:    {d}\n", .{parser_tested});
    std.debug.print("Structured tests: {d}\n", .{structured_tested});
    std.debug.print("Crashes found:    {d}\n", .{crashes});
    std.debug.print("Time:             {d:.1}s\n", .{elapsed_s});
    std.debug.print("Rate:             {d:.0} inputs/s\n", .{@as(f64, @floatFromInt(iterations)) / elapsed_s});

    if (crashes > 0) {
        std.debug.print("\nFAILED: {d} crash(es) found!\n", .{crashes});
        std.process.exit(1);
    } else {
        std.debug.print("\nPASSED: No crashes in {d} inputs.\n", .{iterations});
    }
}

/// Generate semi-structured input that looks like Klar code.
/// This is more effective at finding parser bugs than pure random bytes
/// because it exercises deeper parser paths.
fn generateStructuredInput(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    var result: std.ArrayListUnmanaged(u8) = .{};

    const num_fragments = rand.intRangeAtMost(usize, 1, 20);
    var j: usize = 0;
    while (j < num_fragments) : (j += 1) {
        const fragment = fragments[rand.intRangeAtMost(usize, 0, fragments.len - 1)];
        try result.appendSlice(allocator, fragment);

        // Sometimes add random chars between fragments
        if (rand.boolean()) {
            const filler = fillers[rand.intRangeAtMost(usize, 0, fillers.len - 1)];
            try result.appendSlice(allocator, filler);
        }
    }

    return result.toOwnedSlice(allocator);
}

const fragments = [_][]const u8{
    // Keywords
    "fn ",       "let ",      "var ",       "struct ",    "enum ",
    "trait ",    "impl ",     "import ",    "module ",    "return ",
    "if ",       "else ",     "for ",       "while ",     "match ",
    "in ",       "pub ",      "async ",     "await ",     "unsafe ",
    "extern ",   "test ",     "type ",      "const ",     "meta ",
    "true",      "false",     "None",       "Some",       "Ok",
    "Err",       "self",      "ref ",       "mut ",       "and ",
    "or ",       "not ",      "break",      "continue",   "defer ",
    // Types
    "i32",       "i64",       "f64",        "bool",       "string",
    "void",      "u8",        "char",       "?",          "Result#[",
    "List#[",    "Map#[",     "Option#[",   "Rc#[",
    // Identifiers
    "x",         "y",         "foo",        "bar",        "main",
    "Point",     "Color",     "T",          "E",
    // Literals
    "42",        "3.14",      "0xFF",       "0b1010",     "0o777",
    "1_000",
    // Strings
    "\"hello\"",   "\"\"",      "\"hi {x}\"",  "'c'",
    // Operators
    "= ",        "== ",       "!= ",        "< ",         "> ",
    "<= ",       ">= ",       "+ ",         "- ",         "* ",
    "/ ",        "% ",        "-> ",        "=> ",        ".. ",
    "+% ",       "+| ",       ":: ",        ". ",         "? ",
    // Delimiters
    "(",         ")",         "[",          "]",          "{",
    "}",         ",",         ";",          ":",          "\n",
    // Type annotations
    ": i32",     ": f64",     ": string",   ": bool",     ": void",
    ": [i32; 3]", ": (i32, f64)",
    // Common patterns
    "fn f() -> void {}",
    "fn f(x: i32) -> i32 { return x }",
    "let x: i32 = 42\n",
    "struct S { x: i32 }\n",
    "enum E { A, B, C }\n",
    "trait T { fn f(self) -> void }\n",
    "#[",        "]",         "///",        "//!",        "//",
    ".as#[i32]", ".to#[f64]",
};

const fillers = [_][]const u8{
    " ",    "\n",    "\t",   "  ",   "\n\n",  "",
    "(",    ")",     "{",    "}",    "[",     "]",
    ",",    ";",     ":",    ".",    "42",    "x",
};
