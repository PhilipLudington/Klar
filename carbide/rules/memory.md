---
globs: ["*.zig"]
---

# Memory Management Rules

## M1: Allocator Injection
- ALWAYS accept `Allocator` as a parameter for functions that allocate
- NEVER use global allocators like `std.heap.page_allocator` directly
- Store allocator in struct if needed for deinit

```zig
// GOOD
pub fn init(allocator: Allocator) !Self

// BAD
pub fn init() !Self  // Hidden allocation
```

## M2: Ownership Documentation
- Document who owns returned pointers/slices
- Use "Caller owns" or "Valid until" patterns

```zig
/// Caller owns the returned slice and must free it.
pub fn duplicate(allocator: Allocator, data: []const u8) ![]u8
```

## M3: Immediate Cleanup
- Place `defer` immediately after resource acquisition
- Use `errdefer` for cleanup on error paths only

```zig
const buffer = try allocator.alloc(u8, size);
defer allocator.free(buffer);  // Immediately after

const file = try openFile(path);
errdefer file.close();  // Only on error
```

## M4: Struct Lifecycle
- Use `init/deinit` for stack-allocated structs
- Use `create/destroy` for heap-allocated structs
- Set `self.* = undefined` at end of deinit to poison

## M5: Arena for Batch Operations
- Use `ArenaAllocator` when many allocations share lifetime
- Frees everything at once, simpler code

## M6: Testing Allocator
- Use `std.testing.allocator` in tests (checks leaks)
- Use `std.heap.GeneralPurposeAllocator` for debug builds

## M7: ArrayList Patterns (Zig 0.15+)
- `std.ArrayListUnmanaged` is now the default/simpler pattern
- `std.ArrayList` no longer has `.init(allocator)` static method
- Prefer unmanaged when allocator is stored in parent struct

```zig
// Unmanaged (preferred in 0.15+): zero-initialize, pass allocator to methods
var list = std.ArrayListUnmanaged(u8){};  // Zero-init, NOT .init()
defer list.deinit(allocator);
try list.append(allocator, value);
const slice = try list.toOwnedSlice(allocator);

// In a struct: store allocator separately
const MyStruct = struct {
    allocator: Allocator,
    items: std.ArrayListUnmanaged(Item),

    pub fn init(allocator: Allocator) MyStruct {
        return .{
            .allocator = allocator,
            .items = .{},  // Zero-init
        };
    }

    pub fn deinit(self: *MyStruct) void {
        self.items.deinit(self.allocator);
    }

    pub fn add(self: *MyStruct, item: Item) !void {
        try self.items.append(self.allocator, item);
    }
};
```

**Common mistake**: Using `.init(allocator)` which doesn't exist in 0.15:
```zig
// BAD (compile error in 0.15)
var list = std.ArrayList(u8).init(allocator);
var list = std.ArrayListUnmanaged(u8).init(allocator);

// GOOD
var list = std.ArrayListUnmanaged(u8){};
```

## M8: Avoid undefined in Arithmetic
- Zig 0.15 disallows `undefined` in arithmetic operations
- Initialize variables explicitly before use

```zig
// BAD (Zig 0.15 compile error)
var x: u32 = undefined;
x += 1;

// GOOD
var x: u32 = 0;
x += 1;
```
