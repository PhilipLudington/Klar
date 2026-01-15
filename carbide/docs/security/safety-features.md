# Zig Safety Features

This document describes Zig's built-in safety features and how to use them effectively.

## Build Modes

Zig provides four build modes with different safety/performance tradeoffs:

| Mode | Safety Checks | Optimizations | Use Case |
|------|---------------|---------------|----------|
| Debug | All | None | Development, debugging |
| ReleaseSafe | All | Full | **Production (recommended)** |
| ReleaseFast | None | Full + aggressive | Performance-critical |
| ReleaseSmall | None | Size-focused | Embedded, WASM |

### Recommendation

**Use ReleaseSafe for production** unless you have measured performance issues and can prove safety through testing.

```bash
# Build with ReleaseSafe (production)
zig build -Doptimize=ReleaseSafe

# Build with Debug (development)
zig build -Doptimize=Debug
```

---

## Runtime Safety Checks

These checks are enabled in Debug and ReleaseSafe modes:

### 1. Bounds Checking

```zig
var array = [_]u8{ 1, 2, 3 };
const idx: usize = 5;
_ = array[idx];  // Runtime panic: index out of bounds
```

**Protection**: Prevents buffer overflows and out-of-bounds reads.

### 2. Integer Overflow Detection

```zig
var x: u8 = 255;
x += 1;  // Runtime panic: integer overflow
```

**Alternative operators for intentional overflow**:

```zig
x +%= 1;  // Wrapping: 255 + 1 = 0
x +| 1;   // Saturating: 255 + 1 = 255
```

### 3. Null Pointer Detection

```zig
var ptr: ?*u32 = null;
_ = ptr.?;  // Runtime panic: attempt to unwrap null
```

**Safe alternatives**:

```zig
if (ptr) |p| {
    // p is guaranteed non-null here
}

const value = ptr orelse default;
```

### 4. Undefined Behavior Detection

```zig
var x: u32 = undefined;
_ = x + 1;  // Runtime panic: use of undefined value
```

---

## Compile-Time Safety

Zig catches many errors at compile time:

### Type Safety

```zig
var x: u32 = 42;
var y: i32 = x;  // Compile error: cannot convert u32 to i32
```

### Pointer Safety

```zig
fn getLocal() *u32 {
    var local: u32 = 42;
    return &local;  // Compile error: pointer to local variable
}
```

### Exhaustive Switch

```zig
const Status = enum { pending, active, complete };

fn handle(s: Status) void {
    switch (s) {
        .pending => {},
        .active => {},
        // Compile error: switch must handle all cases
    }
}
```

---

## Optional Types

Zig uses optional types instead of null pointers:

```zig
// Instead of nullable pointer
fn find(key: []const u8) ?*Value {
    // Returns null if not found
    return map.get(key);
}

// Safe usage
if (find("key")) |value| {
    // value is guaranteed non-null
    use(value);
}

// With default
const value = find("key") orelse &default_value;

// Assert non-null (panics if null)
const value = find("key").?;
```

---

## Error Handling

Zig's error unions prevent ignored errors:

```zig
// Must handle or propagate error
const file = try std.fs.cwd().openFile(path, .{});
// or
const file = std.fs.cwd().openFile(path, .{}) catch |err| {
    // Handle error
    return err;
};

// Compile error if error not handled
const file = std.fs.cwd().openFile(path, .{});  // Error: error ignored
```

---

## Sentinel-Terminated Types

For C interop, Zig provides sentinel-terminated types:

```zig
// Sentinel-terminated slice (null-terminated string)
const c_string: [:0]const u8 = "hello";

// Convert from regular slice
fn toCString(allocator: Allocator, s: []const u8) ![:0]u8 {
    return allocator.dupeZ(u8, s);
}

// Safe length calculation
const len = std.mem.len(c_string);  // Uses sentinel
```

---

## Slices vs Pointers

Zig slices carry length, preventing buffer overflows:

```zig
// Slice includes length - safe iteration
fn process(data: []const u8) void {
    for (data) |byte| {  // Cannot exceed bounds
        // ...
    }
}

// Many-pointer - unsafe, avoid when possible
fn unsafe_process(data: [*]const u8, len: usize) void {
    // Manual bounds checking required
}
```

---

## Debug Assertions

Use assertions for invariants that should never be violated:

```zig
// Debug-only assertion (removed in release)
std.debug.assert(index < array.len);

// Always-on assertion (use sparingly)
if (index >= array.len) unreachable;
```

---

## Memory Safety Patterns

### Allocator Tracking

```zig
// GeneralPurposeAllocator detects:
// - Memory leaks
// - Double frees
// - Use after free (in debug mode)
var gpa = std.heap.GeneralPurposeAllocator(.{
    .stack_trace_frames = 10,
}){};
```

### Testing Allocator

```zig
test "no leaks" {
    // std.testing.allocator automatically detects leaks
    var list = std.ArrayList(u8).init(std.testing.allocator);
    defer list.deinit();  // If we forget this, test fails
}
```

---

## When to Disable Safety

Only disable safety checks when:

1. **Proven necessary**: Profiling shows safety checks cause measurable slowdown
2. **Thoroughly tested**: Code has extensive test coverage
3. **Isolated scope**: Use `@setRuntimeSafety(false)` in specific functions, not globally

```zig
fn hotPath(data: []const u8) void {
    @setRuntimeSafety(false);  // Disable only in this function
    // Performance-critical code here
}
```

---

## Best Practices

1. **Default to ReleaseSafe** for production builds
2. **Use optional types** instead of sentinel values
3. **Leverage the type system** for compile-time safety
4. **Test with std.testing.allocator** to catch leaks
5. **Use defer/errdefer** for resource cleanup
6. **Validate external input** at system boundaries
7. **Avoid pointer arithmetic** - use slices instead
8. **Document safety assumptions** in comments
