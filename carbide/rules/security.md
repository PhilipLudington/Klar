---
globs: ["*.zig"]
---

# Security Rules

## S1: Input Validation
- Validate ALL external input at system boundaries
- Check lengths, formats, and ranges

```zig
pub fn parsePort(input: []const u8) !u16 {
    if (input.len == 0 or input.len > 5) {
        return error.InvalidPort;
    }
    const value = std.fmt.parseInt(u16, input, 10) catch {
        return error.InvalidPort;
    };
    if (value == 0) return error.PortZeroNotAllowed;
    return value;
}
```

## S2: Size Limits
- Define and enforce maximum sizes
- Prevent resource exhaustion

```zig
pub const max_request_size = 1024 * 1024;  // 1MB

pub fn readRequest(reader: anytype) !Request {
    const data = try reader.readAllAlloc(allocator, max_request_size);
    // ...
}
```

## S3: Integer Safety
- Use checked arithmetic or saturating ops
- Be aware of release mode behavior

```zig
// Checked (returns null on overflow)
const result = std.math.add(u32, a, b) catch null;

// Saturating (clamps at max/min)
const result = a +| b;
```

## S4: Pointer Safety
- Never return pointers to stack-local data
- Document lifetime of returned slices
- Use optional types instead of sentinel values

## S5: Build Modes
- Use `ReleaseSafe` for production (keeps safety checks)
- `ReleaseFast` only when proven necessary

## S6: Sensitive Data
- Zero sensitive data before freeing
- Use `@memset(password, 0)` or `std.crypto.utils.secureZero`
- Avoid logging sensitive information

## S7: C Interop Safety
- Use sentinel-terminated slices for C strings: `[:0]const u8`
- Validate C-provided pointers before use
