# Avoiding Undefined Behavior in Zig

Zig is designed to minimize undefined behavior, but it still exists in certain cases, especially in release modes with safety checks disabled.

## What is Undefined Behavior?

Undefined behavior (UB) means the program can do anything: crash, produce wrong results, or appear to work correctly. Zig detects many forms of UB at runtime in Debug/ReleaseSafe modes, but they become truly undefined in ReleaseFast/ReleaseSmall.

---

## Common Sources of UB

### 1. Integer Overflow

```zig
// UB in ReleaseFast
var x: u8 = 255;
x += 1;  // What happens?

// SAFE: Use wrapping operators
x +%= 1;  // Explicitly wraps: result is 0

// SAFE: Use saturating operators
x +| 1;  // Saturates: result is 255

// SAFE: Use checked arithmetic
if (std.math.add(u8, x, 1)) |result| {
    x = result;
} else |_| {
    // Handle overflow
}
```

### 2. Out-of-Bounds Access

```zig
var array = [_]u8{ 1, 2, 3 };
const idx: usize = runtime_value;

// UB in ReleaseFast if idx >= 3
_ = array[idx];

// SAFE: Check bounds first
if (idx < array.len) {
    _ = array[idx];
}

// SAFE: Use optional access (no UB, returns null)
const ptr = if (idx < array.len) &array[idx] else null;
```

### 3. Use of Undefined Values

```zig
var x: u32 = undefined;
_ = x + 1;  // UB

// SAFE: Initialize before use
var x: u32 = 0;

// SAFE: Use optional for maybe-uninitialized
var x: ?u32 = null;
if (condition) x = computeValue();
if (x) |value| use(value);
```

### 4. Null Pointer Dereference

```zig
var ptr: ?*u32 = null;
_ = ptr.?;  // UB in ReleaseFast

// SAFE: Check before unwrap
if (ptr) |p| {
    _ = p.*;
}

// SAFE: Provide default
const value = (ptr orelse &default).*;
```

### 5. Invalid Enum Values

```zig
const Color = enum(u8) { red = 0, green = 1, blue = 2 };

// UB: Creating invalid enum from integer
const invalid: Color = @enumFromInt(99);

// SAFE: Validate before conversion
fn safeEnumFromInt(value: u8) ?Color {
    return switch (value) {
        0 => .red,
        1 => .green,
        2 => .blue,
        else => null,
    };
}
```

### 6. Unaligned Pointer Access

```zig
const bytes = [_]u8{ 1, 2, 3, 4, 5, 6, 7, 8 };
const ptr: *align(1) const u32 = @ptrCast(&bytes[1]);
_ = ptr.*;  // May be UB on some architectures

// SAFE: Use std.mem.readInt for unaligned reads
const value = std.mem.readInt(u32, bytes[1..5], .little);
```

### 7. Dangling Pointers

```zig
fn getDanglingPointer() *u32 {
    var local: u32 = 42;
    return &local;  // Compile error (Zig catches this!)
}

// But this can escape detection:
fn sneakyDangle(out: **u32) void {
    var local: u32 = 42;
    out.* = &local;  // UB after function returns
}
```

---

## Safe Arithmetic Operators

| Operator | Normal | Wrapping | Saturating |
|----------|--------|----------|------------|
| Add | `+` | `+%` | `+\|` |
| Subtract | `-` | `-%` | `-\|` |
| Multiply | `*` | `*%` | `*\|` |
| Negate | `-x` | `-%x` | `-\|x` |
| Shift left | `<<` | `<<%` | `<<\|` |

```zig
// Example: Wrapping addition
var x: u8 = 200;
x +%= 100;  // x is now 44 (200 + 100 = 300, wraps to 44)

// Example: Saturating addition
var y: u8 = 200;
y +| 100;  // y is now 255 (clamped at max)
```

---

## Checked Arithmetic

```zig
const std = @import("std");

fn safeAdd(a: u32, b: u32) ?u32 {
    return std.math.add(u32, a, b) catch null;
}

fn safeMul(a: u32, b: u32) ?u32 {
    return std.math.mul(u32, a, b) catch null;
}

fn safeCast(comptime T: type, value: anytype) ?T {
    return std.math.cast(T, value);
}
```

---

## Release Mode Considerations

In ReleaseFast and ReleaseSmall:

1. **All runtime safety checks are disabled**
2. **Undefined behavior is truly undefined**
3. **Code may be optimized based on UB assumptions**

### Testing Strategy

```zig
test "works in release mode" {
    // Run tests with different optimization levels
    // zig build test
    // zig build test -Doptimize=ReleaseSafe
    // zig build test -Doptimize=ReleaseFast
}
```

---

## Defensive Patterns

### 1. Validate at Boundaries

```zig
pub fn processUserInput(input: []const u8) !Result {
    // Validate early
    if (input.len == 0) return error.EmptyInput;
    if (input.len > MAX_INPUT_SIZE) return error.InputTooLarge;

    // Now safe to process
    return processValidated(input);
}
```

### 2. Use Assertions for Invariants

```zig
fn processIndex(array: []u8, idx: usize) void {
    // Debug assertion - catches bugs during development
    std.debug.assert(idx < array.len);

    // Safe to use idx
    array[idx] = 0;
}
```

### 3. Prefer Slices Over Pointers

```zig
// BAD: Easy to get wrong
fn process(ptr: [*]u8, len: usize) void { }

// GOOD: Length carried with data
fn process(data: []u8) void { }
```

### 4. Use Optional Types

```zig
// BAD: Magic values
fn find(key: Key) i32 {
    // Returns -1 if not found
}

// GOOD: Type-safe absence
fn find(key: Key) ?*Value {
    // Returns null if not found
}
```

---

## Tools for Finding UB

### 1. Debug Mode

Always develop with debug mode first - it catches most UB.

### 2. Testing Allocator

Detects memory-related UB (leaks, double-free, use-after-free).

### 3. Sanitizers (via Zig's LLVM backend)

```zig
// build.zig
const exe = b.addExecutable(.{ ... });
exe.sanitize_c = true;  // Enable C sanitizers
```

### 4. Valgrind

```bash
valgrind --leak-check=full ./zig-out/bin/myapp
```

---

## Checklist

Before switching to ReleaseFast:

- [ ] Extensive test coverage
- [ ] All tests pass in ReleaseSafe
- [ ] Profiling shows safety overhead is the bottleneck
- [ ] Critical sections reviewed for potential UB
- [ ] Input validation at all external boundaries
- [ ] Integer arithmetic reviewed for overflow potential
