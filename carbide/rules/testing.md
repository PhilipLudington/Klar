---
globs: ["*.zig"]
---

# Testing Rules

## T1: Test Organization
- Place unit tests in same file as code
- Put integration tests in `tests/` directory

```zig
const MyStruct = struct {
    // implementation
};

test "MyStruct basic usage" {
    // test code
}
```

## T2: Test Naming
- Use pattern: "Subject behavior [condition]"
- Be descriptive about what's being tested

```zig
test "Parser parses empty object" { }
test "Parser returns error on invalid input" { }
test "Connection reconnects after timeout" { }
```

## T3: Testing Allocator
- ALWAYS use `std.testing.allocator` in tests
- Automatically detects memory leaks

```zig
test "no memory leaks" {
    var list = std.ArrayList(u8).init(std.testing.allocator);
    defer list.deinit();
    // ...
}
```

## T4: Failing Allocator
- Test allocation failure paths

```zig
test "handles allocation failure" {
    var failing = std.testing.FailingAllocator.init(
        std.testing.allocator,
        .{ .fail_index = 2 },
    );
    const result = MyStruct.init(failing.allocator());
    try std.testing.expectError(error.OutOfMemory, result);
}
```

## T5: Table-Driven Tests
- Use arrays of test cases for comprehensive coverage

```zig
test "parse integers" {
    const cases = [_]struct { input: []const u8, expected: ?i64 }{
        .{ .input = "42", .expected = 42 },
        .{ .input = "abc", .expected = null },
    };
    for (cases) |case| {
        // test each case
    }
}
```

## T6: Assertions
- Use `std.testing.expect*` functions
- Prefer specific assertions for better error messages

```zig
try std.testing.expectEqual(expected, actual);
try std.testing.expectEqualStrings("hello", result);
try std.testing.expectError(error.NotFound, result);
```
