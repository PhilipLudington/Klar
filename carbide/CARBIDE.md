# CarbideZig Quick Reference

> **AI-Optimized Zig Development Standards**

## Core Principles

1. **Leverage the Type System** — Compile-time safety over runtime checks
2. **Explicit Resource Management** — Every allocation has an owner
3. **Fail Loudly** — Never silently ignore errors
4. **Comptime Over Runtime** — Move work to compile time
5. **Minimal Dependencies** — Standard library first

---

## Naming Conventions

| Element | Style | Example |
|---------|-------|---------|
| Types | PascalCase | `HttpClient` |
| Functions | camelCase | `readFile` |
| Variables | camelCase | `bufferSize` |
| Constants | snake_case | `max_size` |
| Files | snake_case | `http_client.zig` |

### Function Patterns

```zig
init() / deinit()         // Lifecycle
create() / destroy()      // Heap-allocated
name() / setName()        // Accessor / Mutator
isReady() / hasValue()    // Boolean queries
toSlice() / fromBytes()   // Conversions
```

---

## Memory Rules

```zig
// M1: Accept allocator parameter
pub fn init(allocator: Allocator) !Self

// M2: defer immediately after acquire
const buffer = try allocator.alloc(u8, size);
defer allocator.free(buffer);

// M3: errdefer for error-path cleanup
const a = try allocate();
errdefer free(a);

// M4: Poison after deinit
pub fn deinit(self: *Self) void {
    self.allocator.free(self.buffer);
    self.* = undefined;
}
```

---

## Error Handling

```zig
// E1: Specific error sets
pub const ParseError = error{ InvalidSyntax, UnexpectedToken };

// E2: try for propagation
const value = try readFile(path);

// E3: catch for handling
const value = readFile(path) catch |err| {
    log.err("Failed: {}", .{err});
    return err;
};

// E4: errdefer chain (runs in reverse)
const a = try alloc();
errdefer free(a);  // Runs 2nd
const b = try alloc();
errdefer free(b);  // Runs 1st
```

---

## API Design

```zig
// A1: Accept slices
pub fn process(data: []const u8) void

// A2: Use optional types
pub fn find(key: []const u8) ?*Value

// A3: Config structs with defaults
pub const Config = struct {
    port: u16 = 8080,
    timeout_ms: u32 = 30_000,
};

// A4: Return structs for multiple values
pub const Result = struct { value: i32, remainder: i32 };
```

---

## Testing

```zig
// T1: Use testing allocator (leak detection)
test "no leaks" {
    var list = ArrayList(u8).init(std.testing.allocator);
    defer list.deinit();
}

// T2: Descriptive test names
test "Parser returns error on invalid input" { }

// T3: Test error cases
try std.testing.expectError(error.NotFound, result);
```

---

## Security

```zig
// S1: Validate external input
if (input.len > max_size) return error.InputTooLarge;

// S2: Use checked/saturating arithmetic
const result = std.math.add(u32, a, b) catch return error.Overflow;
const result = a +| b;  // Saturating

// S3: Zero sensitive data
std.crypto.utils.secureZero(u8, password);
```

---

## Build Modes

| Mode | Safety | Use Case |
|------|--------|----------|
| Debug | All checks | Development |
| **ReleaseSafe** | All checks | **Production** |
| ReleaseFast | No checks | Performance-critical |
| ReleaseSmall | No checks | Embedded/WASM |

---

## Quick Commands

```bash
zig build              # Build project
zig build run          # Build and run
zig build test         # Run tests
zig fmt src/           # Format code
zig fmt --check src/   # Check formatting
```

---

## Slash Commands

| Command | Description |
|---------|-------------|
| `/carbide-init` | Create new project |
| `/carbide-review` | Code review |
| `/carbide-check` | Run validation |
| `/carbide-safety` | Security review |

---

## Common Patterns

```zig
// Struct lifecycle
var obj = try Struct.init(allocator);
defer obj.deinit();

// Optional handling
const value = optional orelse default;
if (optional) |v| { use(v); }

// Arena for batch allocs
var arena = std.heap.ArenaAllocator.init(allocator);
defer arena.deinit();
```

---

*Explicit over implicit. Simple over clever. Safe over fast.*
