const std = @import("std");

/// Property-based type checker test — generates random valid and invalid
/// Klar programs and verifies the type checker never crashes.
///
/// Usage: zig build property-test -- [count] [klar-binary]
///   count: number of programs to generate (default: 10000)
///   klar-binary: path to klar binary (default: ./zig-out/bin/klar)
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var args_it = std.process.args();
    _ = args_it.next(); // skip program name

    var count: usize = 10_000;
    if (args_it.next()) |arg| {
        count = std.fmt.parseInt(usize, arg, 10) catch 10_000;
    }

    var klar_binary: []const u8 = "./zig-out/bin/klar";
    if (args_it.next()) |arg| {
        klar_binary = arg;
    }

    var prng = std.Random.DefaultPrng.init(0xCAFEBABE);
    const rand = prng.random();

    var valid_pass: usize = 0;
    var valid_error: usize = 0;
    var invalid_error: usize = 0;
    var invalid_pass: usize = 0; // unexpected: invalid program passes check
    var crashes: usize = 0;
    var timeouts: usize = 0;

    const start = std.time.nanoTimestamp();

    // Write a temp file for the programs
    const tmp_path = "/tmp/klar_property_test.kl";

    var i: usize = 0;
    while (i < count) : (i += 1) {
        if (i > 0 and i % 1000 == 0) {
            const elapsed_ns: u64 = @intCast(std.time.nanoTimestamp() - start);
            const elapsed_s = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000_000.0;
            const rate = @as(f64, @floatFromInt(i)) / elapsed_s;
            std.debug.print("\r  {d}/{d} ({d:.0}/s) crashes={d}...", .{ i, count, rate, crashes });
        }

        // Alternate: even = generate valid program, odd = generate invalid
        const expect_valid = (i % 2 == 0);
        const program = if (expect_valid)
            try generateValidProgram(allocator, rand)
        else
            try generateInvalidProgram(allocator, rand);
        defer allocator.free(program);

        // Write to temp file
        {
            const file = try std.fs.createFileAbsolute(tmp_path, .{});
            defer file.close();
            try file.writeAll(program);
        }

        // Run klar check
        const result = runCheck(allocator, klar_binary, tmp_path) catch |err| {
            if (err == error.ProcessTimedOut) {
                timeouts += 1;
                continue;
            }
            // Treat other errors as crashes
            std.debug.print("\nSPAWN ERROR at iteration {d}: {}\n", .{ i, err });
            crashes += 1;
            continue;
        };

        switch (result) {
            .pass => {
                if (expect_valid) {
                    valid_pass += 1;
                } else {
                    invalid_pass += 1; // invalid program unexpectedly passed
                }
            },
            .check_error => {
                if (expect_valid) {
                    valid_error += 1; // valid program had a type error (generator imperfect)
                } else {
                    invalid_error += 1;
                }
            },
            .crash => {
                std.debug.print("\nCRASH at iteration {d}!\nProgram:\n{s}\n", .{ i, program });
                crashes += 1;
            },
        }
    }

    // Clean up temp file
    std.fs.deleteFileAbsolute(tmp_path) catch {};

    const elapsed_ns: u64 = @intCast(std.time.nanoTimestamp() - start);
    const elapsed_s = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000_000.0;

    std.debug.print("\r", .{});
    std.debug.print("=== Type Checker Property Test Results ===\n", .{});
    std.debug.print("Programs generated:  {d}\n", .{count});
    std.debug.print("Valid programs:      {d} passed, {d} had type errors\n", .{ valid_pass, valid_error });
    std.debug.print("Invalid programs:    {d} caught errors, {d} unexpectedly passed\n", .{ invalid_error, invalid_pass });
    std.debug.print("Crashes:             {d}\n", .{crashes});
    std.debug.print("Timeouts:            {d}\n", .{timeouts});
    std.debug.print("Time:                {d:.1}s\n", .{elapsed_s});
    std.debug.print("Rate:                {d:.0} programs/s\n", .{@as(f64, @floatFromInt(count)) / elapsed_s});

    if (crashes > 0) {
        std.debug.print("\nFAILED: {d} crash(es) found!\n", .{crashes});
        std.process.exit(1);
    } else {
        std.debug.print("\nPASSED: No crashes in {d} programs.\n", .{count});
    }
}

const CheckResult = enum { pass, check_error, crash };

fn runCheck(allocator: std.mem.Allocator, klar_binary: []const u8, file_path: []const u8) !CheckResult {
    var child = std.process.Child.init(&.{ klar_binary, "check", file_path }, allocator);
    child.stdout_behavior = .Ignore;
    child.stderr_behavior = .Pipe;

    try child.spawn();

    // Read stderr to check for "failed" in output
    var stderr_buf: [4096]u8 = undefined;
    const stderr_file = child.stderr.?;
    const stderr_len = stderr_file.readAll(&stderr_buf) catch 0;
    const stdout = stderr_buf[0..stderr_len];

    const term = try child.wait();

    return switch (term) {
        .Exited => {
            // klar check returns 0 even on type errors; detect via output
            if (std.mem.indexOf(u8, stdout, "failed") != null) {
                return .check_error;
            }
            return .pass;
        },
        .Signal => .crash,
        .Stopped => .crash,
        .Unknown => .crash,
    };
}

// ============================================================================
// Program generators
// ============================================================================

fn generateValidProgram(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    const kind = rand.intRangeAtMost(u8, 0, 14);
    return switch (kind) {
        0 => try generateArithmetic(allocator, rand),
        1 => try generateSimpleFunction(allocator, rand),
        2 => try generateStructDef(allocator, rand),
        3 => try generateEnumDef(allocator, rand),
        4 => try generateIfElse(allocator, rand),
        5 => try generateForLoop(allocator, rand),
        6 => try generateWhileLoop(allocator, rand),
        7 => try generateOptional(allocator, rand),
        8 => try generateResult(allocator, rand),
        9 => try generateClosure(allocator, rand),
        10 => try generateMatch(allocator, rand),
        11 => try generateArray(allocator, rand),
        12 => try generateMultiFunction(allocator, rand),
        13 => try generateTraitDef(allocator, rand),
        14 => try generateNestedControl(allocator, rand),
        else => unreachable,
    };
}

fn generateInvalidProgram(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    const kind = rand.intRangeAtMost(u8, 0, 9);
    return switch (kind) {
        0 => try generateTypeMismatch(allocator, rand),
        1 => try generateUndefinedVar(allocator, rand),
        2 => try generateMissingReturn(allocator, rand),
        3 => try generateWrongArgCount(allocator, rand),
        4 => try generateDuplicateDecl(allocator, rand),
        5 => try generateInvalidOp(allocator, rand),
        6 => try generateBadFieldAccess(allocator, rand),
        7 => try generateBadAssignment(allocator, rand),
        8 => try generateBadGeneric(allocator, rand),
        9 => try generateMixedTypes(allocator, rand),
        else => unreachable,
    };
}

// --- Valid program templates ---

const types = [_][]const u8{ "i32", "i64", "f64", "bool", "string" };
const i32_ops = [_][]const u8{ " + ", " - ", " * " };
const bool_ops = [_][]const u8{ " and ", " or " };
const cmp_ops = [_][]const u8{ " == ", " != ", " < ", " > " };
const var_names = [_][]const u8{ "a", "b", "c", "d", "e", "x", "y", "z", "n", "m" };
const struct_names = [_][]const u8{ "Point", "Vec", "Pair", "Data", "Item", "Node", "Cell" };

fn pick(comptime T: type, arr: []const T, rand: std.Random) T {
    return arr[rand.intRangeAtMost(usize, 0, arr.len - 1)];
}

fn generateArithmetic(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    const v1 = pick([]const u8, &var_names, rand);
    const v2 = pick([]const u8, &var_names, rand);
    const op = pick([]const u8, &i32_ops, rand);
    const n1 = rand.intRangeAtMost(i32, 0, 1000);
    const n2 = rand.intRangeAtMost(i32, 1, 1000); // avoid div by zero
    return std.fmt.allocPrint(allocator,
        \\fn main() -> i32 {{
        \\    let {s}: i32 = {d}
        \\    let {s}: i32 = {d}
        \\    let result: i32 = {s}{s}{s}
        \\    return 0
        \\}}
    , .{ v1, n1, v2, n2, v1, op, v2 });
}

fn generateSimpleFunction(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    const fname = pick([]const u8, &var_names, rand);
    const pname = pick([]const u8, &var_names, rand);
    const n = rand.intRangeAtMost(i32, 0, 100);
    _ = n;
    return std.fmt.allocPrint(allocator,
        \\fn {s}({s}: i32) -> i32 {{
        \\    return {s}
        \\}}
        \\
        \\fn main() -> i32 {{
        \\    let r: i32 = {s}(42)
        \\    return 0
        \\}}
    , .{ fname, pname, pname, fname });
}

fn generateStructDef(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    const sname = pick([]const u8, &struct_names, rand);
    const f1 = pick([]const u8, &var_names, rand);
    const f2 = pick([]const u8, &var_names, rand);
    return std.fmt.allocPrint(allocator,
        \\struct {s} {{
        \\    {s}: i32,
        \\    {s}: f64,
        \\}}
        \\
        \\fn main() -> i32 {{
        \\    let p: {s} = {s} {{ {s}: 1, {s}: 2.0 }}
        \\    return 0
        \\}}
    , .{ sname, f1, f2, sname, sname, f1, f2 });
}

fn generateEnumDef(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    _ = rand;
    return std.fmt.allocPrint(allocator,
        \\enum Color {{ Red, Green, Blue }}
        \\
        \\fn main() -> i32 {{
        \\    let c: Color = Color.Red
        \\    return 0
        \\}}
    , .{});
}

fn generateIfElse(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    const n = rand.intRangeAtMost(i32, 0, 100);
    return std.fmt.allocPrint(allocator,
        \\fn main() -> i32 {{
        \\    let x: i32 = {d}
        \\    if x > 50 {{
        \\        return 1
        \\    }} else {{
        \\        return 0
        \\    }}
        \\}}
    , .{n});
}

fn generateForLoop(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    const end = rand.intRangeAtMost(i32, 1, 20);
    return std.fmt.allocPrint(allocator,
        \\fn main() -> i32 {{
        \\    var sum: i32 = 0
        \\    for i: i32 in 0..{d} {{
        \\        sum = sum + i
        \\    }}
        \\    return 0
        \\}}
    , .{end});
}

fn generateWhileLoop(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    const limit = rand.intRangeAtMost(i32, 1, 20);
    return std.fmt.allocPrint(allocator,
        \\fn main() -> i32 {{
        \\    var i: i32 = 0
        \\    while i < {d} {{
        \\        i = i + 1
        \\    }}
        \\    return 0
        \\}}
    , .{limit});
}

fn generateOptional(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    const n = rand.intRangeAtMost(i32, 0, 100);
    return std.fmt.allocPrint(allocator,
        \\fn main() -> i32 {{
        \\    let x: ?i32 = Some({d})
        \\    let y: i32 = x ?? 0
        \\    return 0
        \\}}
    , .{n});
}

fn generateResult(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    _ = rand;
    return std.fmt.allocPrint(allocator,
        \\fn main() -> i32 {{
        \\    let r: Result#[i32, string] = Ok(42)
        \\    return 0
        \\}}
    , .{});
}

fn generateClosure(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    const n = rand.intRangeAtMost(i32, 1, 10);
    return std.fmt.allocPrint(allocator,
        \\fn main() -> i32 {{
        \\    let f: fn(i32) -> i32 = |x: i32| -> i32 {{ return x * {d} }}
        \\    let r: i32 = f(5)
        \\    return 0
        \\}}
    , .{n});
}

fn generateMatch(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    _ = rand;
    return std.fmt.allocPrint(allocator,
        \\enum Shape {{ Circle, Square, Triangle }}
        \\
        \\fn main() -> i32 {{
        \\    let s: Shape = Shape.Circle
        \\    var r: i32 = 0
        \\    match s {{
        \\        Shape.Circle => {{ r = 1 }}
        \\        Shape.Square => {{ r = 2 }}
        \\        Shape.Triangle => {{ r = 3 }}
        \\    }}
        \\    return 0
        \\}}
    , .{});
}

fn generateArray(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    const n1 = rand.intRangeAtMost(i32, 0, 100);
    const n2 = rand.intRangeAtMost(i32, 0, 100);
    const n3 = rand.intRangeAtMost(i32, 0, 100);
    return std.fmt.allocPrint(allocator,
        \\fn main() -> i32 {{
        \\    let arr: [i32; 3] = [{d}, {d}, {d}]
        \\    let first: i32 = arr[0]
        \\    return 0
        \\}}
    , .{ n1, n2, n3 });
}

fn generateMultiFunction(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    const n = rand.intRangeAtMost(i32, 1, 10);
    return std.fmt.allocPrint(allocator,
        \\fn add(a: i32, b: i32) -> i32 {{
        \\    return a + b
        \\}}
        \\
        \\fn double(x: i32) -> i32 {{
        \\    return x * 2
        \\}}
        \\
        \\fn main() -> i32 {{
        \\    let r: i32 = add(double({d}), 1)
        \\    return 0
        \\}}
    , .{n});
}

fn generateTraitDef(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    _ = rand;
    return std.fmt.allocPrint(allocator,
        \\trait Printable {{
        \\    fn to_str(self) -> string
        \\}}
        \\
        \\struct MyNum {{
        \\    val: i32,
        \\}}
        \\
        \\impl MyNum: Printable {{
        \\    fn to_str(self) -> string {{
        \\        return self.val.to_string()
        \\    }}
        \\}}
        \\
        \\fn main() -> i32 {{
        \\    let n: MyNum = MyNum {{ val: 42 }}
        \\    let s: string = n.to_str()
        \\    return 0
        \\}}
    , .{});
}

fn generateNestedControl(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    const n = rand.intRangeAtMost(i32, 1, 10);
    return std.fmt.allocPrint(allocator,
        \\fn main() -> i32 {{
        \\    var total: i32 = 0
        \\    for i: i32 in 0..{d} {{
        \\        if i > 3 {{
        \\            total = total + i
        \\        }} else {{
        \\            total = total + 1
        \\        }}
        \\    }}
        \\    return 0
        \\}}
    , .{n});
}

// --- Invalid program templates (should produce type errors, not crashes) ---

fn generateTypeMismatch(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    // Assign wrong type
    const declared = pick([]const u8, &types, rand);
    const actual_idx = rand.intRangeAtMost(usize, 0, types.len - 1);
    const actual = types[(actual_idx + 1) % types.len]; // pick a different type
    _ = actual;
    const val: []const u8 = if (std.mem.eql(u8, declared, "string"))
        "42"
    else if (std.mem.eql(u8, declared, "bool"))
        "42"
    else if (std.mem.eql(u8, declared, "f64"))
        "true"
    else
        "\"hello\"";
    return std.fmt.allocPrint(allocator,
        \\fn main() -> i32 {{
        \\    let x: {s} = {s}
        \\    return 0
        \\}}
    , .{ declared, val });
}

fn generateUndefinedVar(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    const v = pick([]const u8, &var_names, rand);
    return std.fmt.allocPrint(allocator,
        \\fn main() -> i32 {{
        \\    let r: i32 = {s} + 1
        \\    return 0
        \\}}
    , .{v});
}

fn generateMissingReturn(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    _ = rand;
    return std.fmt.allocPrint(allocator,
        \\fn compute() -> i32 {{
        \\    let x: i32 = 42
        \\}}
        \\
        \\fn main() -> i32 {{
        \\    return 0
        \\}}
    , .{});
}

fn generateWrongArgCount(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    const extra = rand.intRangeAtMost(i32, 1, 5);
    _ = extra;
    return std.fmt.allocPrint(allocator,
        \\fn add(a: i32, b: i32) -> i32 {{
        \\    return a + b
        \\}}
        \\
        \\fn main() -> i32 {{
        \\    let r: i32 = add(1, 2, 3)
        \\    return 0
        \\}}
    , .{});
}

fn generateDuplicateDecl(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    const v = pick([]const u8, &var_names, rand);
    return std.fmt.allocPrint(allocator,
        \\fn main() -> i32 {{
        \\    let {s}: i32 = 1
        \\    let {s}: i32 = 2
        \\    return 0
        \\}}
    , .{ v, v });
}

fn generateInvalidOp(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    _ = rand;
    return std.fmt.allocPrint(allocator,
        \\fn main() -> i32 {{
        \\    let x: string = "hello"
        \\    let y: i32 = x + 1
        \\    return 0
        \\}}
    , .{});
}

fn generateBadFieldAccess(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    _ = rand;
    return std.fmt.allocPrint(allocator,
        \\struct Point {{ x: i32, y: i32 }}
        \\
        \\fn main() -> i32 {{
        \\    let p: Point = Point {{ x: 1, y: 2 }}
        \\    let z: i32 = p.z
        \\    return 0
        \\}}
    , .{});
}

fn generateBadAssignment(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    _ = rand;
    return std.fmt.allocPrint(allocator,
        \\fn main() -> i32 {{
        \\    let x: i32 = 42
        \\    x = 100
        \\    return 0
        \\}}
    , .{});
}

fn generateBadGeneric(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    _ = rand;
    return std.fmt.allocPrint(allocator,
        \\fn identity#[T](x: T) -> T {{
        \\    return x
        \\}}
        \\
        \\fn main() -> i32 {{
        \\    let r: string = identity#[i32](42)
        \\    return 0
        \\}}
    , .{});
}

fn generateMixedTypes(allocator: std.mem.Allocator, rand: std.Random) ![]const u8 {
    _ = rand;
    return std.fmt.allocPrint(allocator,
        \\fn main() -> i32 {{
        \\    let x: i32 = 42
        \\    let y: f64 = 3.14
        \\    let z: i32 = x + y
        \\    return 0
        \\}}
    , .{});
}
