---
globs: ["*.zig"]
---

# Error Message Decoder

Common Zig compiler errors and their fixes.

## Type Mismatches

### E001: Slice Type Mismatch
```
error: expected type '[]const u8', found '[*:0]const u8'
```
**Cause**: Mixing sentinel-terminated pointers with regular slices.
**Fix**: Use `std.mem.span()` to convert:
```zig
// BAD
const str: []const u8 = c_string;

// GOOD
const str: []const u8 = std.mem.span(c_string);
```

### E002: Many-Pointer vs Slice
```
error: expected type '[]u8', found '[*]u8'
```
**Cause**: Using many-pointer `[*]` where slice `[]` expected.
**Fix**: Create slice with known length:
```zig
// BAD
const slice: []u8 = ptr;

// GOOD
const slice: []u8 = ptr[0..len];
```

### E003: Const Mismatch
```
error: expected type '[]u8', found '[]const u8'
```
**Cause**: Passing const slice where mutable expected.
**Fix**: Use `@constCast` only if you own the memory:
```zig
// Only if you KNOW it's safe to mutate
var mutable = @constCast(const_slice);
```

## Integer/Float Errors

### E004: Integer Overflow
```
error: overflow of integer type 'u8'
```
**Cause**: Value exceeds type bounds at comptime.
**Fix**: Use larger type or explicit truncation:
```zig
// BAD
const x: u8 = 300;

// GOOD
const x: u16 = 300;
// OR
const x: u8 = @truncate(larger_value);
```

### E005: Signed/Unsigned Mismatch
```
error: expected type 'usize', found 'isize'
```
**Cause**: Mixing signed and unsigned integers.
**Fix**: Explicit cast with bounds checking:
```zig
// BAD
const index: usize = signed_value;

// GOOD
const index: usize = @intCast(signed_value);  // Panics if negative
// OR safer
const index: usize = if (signed_value >= 0) @intCast(signed_value) else return error.NegativeIndex;
```

### E006: Lossy Int-to-Float (Zig 0.15+)
```
error: integer value 9007199254740993 cannot be coerced to 'f64'
```
**Cause**: Zig 0.15 disallows implicit lossy int-to-float conversions.
**Fix**: Use explicit cast acknowledging potential precision loss:
```zig
// BAD (Zig 0.15+)
const f: f64 = large_int;

// GOOD
const f: f64 = @floatFromInt(large_int);
```

## Memory/Pointer Errors

### E007: Use After Free Pattern
```
error: pointer value '...' accessed outside bounds
```
**Cause**: Using memory after deallocation.
**Fix**: Check defer/errdefer ordering, ensure pointer lifetime:
```zig
// BAD - data freed before use
const data = try allocator.alloc(u8, 100);
defer allocator.free(data);
return data;  // Returns freed memory!

// GOOD - caller owns
const data = try allocator.alloc(u8, 100);
errdefer allocator.free(data);  // Only on error
return data;  // Caller must free
```

### E008: Alignment Error
```
error: pointer is misaligned
```
**Cause**: Pointer doesn't meet type's alignment requirement.
**Fix**: Use `@alignCast` or allocate with proper alignment:
```zig
// BAD
const ptr: *u32 = @ptrCast(byte_ptr);

// GOOD
const ptr: *u32 = @ptrCast(@alignCast(byte_ptr));
```

### E009: Undefined Arithmetic (Zig 0.15+)
```
error: use of undefined value
```
**Cause**: Zig 0.15 disallows arithmetic on `undefined` values.
**Fix**: Initialize before arithmetic:
```zig
// BAD (Zig 0.15+)
var x: u32 = undefined;
x += 1;

// GOOD
var x: u32 = 0;
x += 1;
```

## Comptime Errors

### E010: Comptime Requirement
```
error: unable to evaluate constant expression
```
**Cause**: Non-comptime value used where comptime required.
**Fix**: Ensure all dependencies are comptime-known:
```zig
// BAD
fn makeArray(size: usize) [size]u8  // size not comptime

// GOOD
fn makeArray(comptime size: usize) [size]u8
```

### E011: Comptime Scope Return
```
error: cannot return from within a comptime scope
```
**Cause**: Using `return` inside inline for/comptime block.
**Fix**: Use break or assign to variable:
```zig
// BAD
inline for (items) |item| {
    if (match(item)) return item;
}

// GOOD
const result = inline for (items) |item| {
    if (match(item)) break item;
} else null;
```

### E012: Type is not comptime-known
```
error: type 'T' is not comptime-known
```
**Cause**: Generic type parameter not marked comptime.
**Fix**: Add `comptime` keyword:
```zig
// BAD
fn Container(T: type) type

// GOOD
fn Container(comptime T: type) type
```

## Error Handling Errors

### E013: Error Not Handled
```
error: error is discarded
```
**Cause**: Ignoring error from function returning error union.
**Fix**: Handle with try, catch, or explicit discard:
```zig
// BAD
mayFail();

// GOOD
try mayFail();
// OR
mayFail() catch |err| { log.err("{}", .{err}); };
// OR (explicit, with justification comment)
mayFail() catch {};  // Best effort, failure is acceptable
```

### E014: Wrong Error Type
```
error: 'error.Foo' is not a member of error set 'ParseError'
```
**Cause**: Returning error not in declared error set.
**Fix**: Add error to set or use more general type:
```zig
// BAD
const MyError = error{Foo};
fn bar() MyError!void {
    return error.Bar;  // Not in MyError
}

// GOOD - add to set
const MyError = error{Foo, Bar};

// OR - infer error set
fn bar() !void
```

### E015: anyerror Conversion
```
error: cannot convert 'anyerror' to 'MyError'
```
**Cause**: Using generic error where specific set expected.
**Fix**: Use `catch` to convert or widen return type:
```zig
// BAD
fn wrapper() MyError!void {
    try genericFunction();  // Returns anyerror
}

// GOOD - handle and convert
fn wrapper() MyError!void {
    genericFunction() catch return error.GenericFailure;
}
```

## Optional Errors

### E016: Null Access
```
error: attempt to unwrap null optional
```
**Cause**: Using `.?` or `orelse unreachable` on null.
**Fix**: Use `if` or `orelse` with proper handling:
```zig
// BAD
const value = optional.?;  // Panics if null

// GOOD
const value = optional orelse return error.NotFound;
// OR
if (optional) |value| {
    // use value
}
```

## Struct/Union Errors

### E017: Missing Field
```
error: missing struct field 'name'
```
**Cause**: Required field not provided in struct literal.
**Fix**: Provide all required fields:
```zig
// BAD
const config = Config{};  // Missing required fields

// GOOD - provide all required, or add defaults to struct definition
const config = Config{
    .name = "app",
    .port = 8080,
};
```

### E018: Wrong Field Type
```
error: expected type '[]const u8', found 'usize'
```
**Cause**: Field value type doesn't match struct definition.
**Fix**: Provide correct type or convert:
```zig
// Check struct definition for expected types
```

## ArrayList Errors (Zig 0.15+)

### E021: ArrayList.init() Does Not Exist
```
error: no field or member function named 'init' in 'std.ArrayListUnmanaged(u8)'
```
**Cause**: Zig 0.15 removed `.init(allocator)` from ArrayListUnmanaged.
**Fix**: Use zero-initialization instead:
```zig
// BAD (Zig 0.15+)
var list = std.ArrayListUnmanaged(u8).init(allocator);

// GOOD - zero-initialize
var list = std.ArrayListUnmanaged(u8){};
defer list.deinit(allocator);
```

### E022: ArrayList Method Missing Allocator
```
error: expected 2 arguments, found 1
```
**Cause**: ArrayListUnmanaged methods require allocator parameter in 0.15+.
**Fix**: Pass allocator to each method call:
```zig
// BAD (Zig 0.15+)
try list.append(value);
list.deinit();
const slice = try list.toOwnedSlice();

// GOOD - pass allocator to each method
try list.append(allocator, value);
list.deinit(allocator);
const slice = try list.toOwnedSlice(allocator);
```

### E023: File.Writer Has No print()
```
error: no field or member function named 'print' in 'std.fs.File.Writer'
```
**Cause**: File.Writer doesn't have a `.print()` method in Zig 0.15+.
**Fix**: Use `std.fmt.bufPrint()` + `writeAll()`:
```zig
// BAD (Zig 0.15+)
const writer = file.writer();
try writer.print("Value: {d}\n", .{value});

// GOOD - format to buffer, then write
var buf: [256]u8 = undefined;
const formatted = std.fmt.bufPrint(&buf, "Value: {d}\n", .{value}) catch unreachable;
try file.writeAll(formatted);
```

## Build System Errors

### E019: root_source_file Deprecated (Zig 0.15+)
```
error: deprecated option 'root_source_file'
```
**Fix**: Use `root_module` pattern:
```zig
// BAD (deprecated)
const lib = b.addStaticLibrary(.{
    .root_source_file = b.path("src/lib.zig"),
});

// GOOD (Zig 0.15+)
const mod = b.addModule("mylib", .{
    .root_source_file = b.path("src/lib.zig"),
});
const lib = b.addStaticLibrary(.{
    .root_module = mod,
});
```

### E020: Module Not Found
```
error: no module named 'foo'
```
**Cause**: Import references undeclared module.
**Fix**: Add module to build.zig:
```zig
const mod = b.addModule("foo", .{
    .root_source_file = b.path("src/foo.zig"),
});
exe.root_module.addImport("foo", mod);
```

## Quick Reference Table

| Error Pattern | Likely Cause | Quick Fix |
|--------------|--------------|-----------|
| `'[]const u8'` vs `'[*:0]const u8'` | C string vs slice | `std.mem.span()` |
| `overflow of integer type` | Value too large | Use larger type or `@truncate` |
| `cannot coerce to 'f64'` (0.15+) | Lossy conversion | `@floatFromInt` |
| `use of undefined value` (0.15+) | Undefined arithmetic | Initialize first |
| `unable to evaluate constant` | Non-comptime in comptime context | Add `comptime` keyword |
| `error is discarded` | Unhandled error | `try`, `catch`, or explicit discard |
| `deprecated option` (0.15+) | Old build API | Use `root_module` pattern |
| `no member 'init'` in ArrayList (0.15+) | Old ArrayList API | Use `{}` zero-init |
| `expected 2 arguments, found 1` (ArrayList) | Missing allocator param | Pass allocator to methods |
| `no member 'print'` in File.Writer | Writer has no print | `std.fmt.bufPrint` + `writeAll` |
