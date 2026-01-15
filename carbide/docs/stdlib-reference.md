# Zig Standard Library Quick Reference

Commonly used standard library modules and functions for CarbideZig projects.

## Memory Operations (std.mem)

### Allocation
```zig
const Allocator = std.mem.Allocator;

// Allocate array
const buf = try allocator.alloc(u8, 1024);
defer allocator.free(buf);

// Allocate single item
const node = try allocator.create(Node);
defer allocator.destroy(node);

// Duplicate slice
const copy = try allocator.dupe(u8, original);

// Resize (may relocate)
buf = try allocator.realloc(buf, new_size);
```

### Comparison and Search
```zig
// Compare slices
const equal = std.mem.eql(u8, slice1, slice2);

// Find substring
const pos = std.mem.indexOf(u8, haystack, needle);  // ?usize

// Check prefix/suffix
const has_prefix = std.mem.startsWith(u8, slice, prefix);
const has_suffix = std.mem.endsWith(u8, slice, suffix);

// Count occurrences
const count = std.mem.count(u8, slice, needle);
```

### Manipulation
```zig
// Copy (non-overlapping)
@memcpy(dest, src);

// Copy (overlapping OK)
std.mem.copyForwards(u8, dest, src);
std.mem.copyBackwards(u8, dest, src);

// Set all bytes
@memset(buffer, 0);

// Replace
const new = std.mem.replace(u8, allocator, input, needle, replacement);

// Split
var iter = std.mem.splitScalar(u8, input, ',');
while (iter.next()) |part| { ... }

// Tokenize (skip empty)
var iter = std.mem.tokenizeScalar(u8, input, ' ');
```

### Sentinel-Terminated Conversions
```zig
// C string to slice
const slice: []const u8 = std.mem.span(c_string);

// Add sentinel (allocates)
const z_str = try allocator.dupeZ(u8, slice);  // [:0]u8
```

## String Formatting (std.fmt)

### Print to Buffer
```zig
// Format to buffer
var buf: [256]u8 = undefined;
const str = try std.fmt.bufPrint(&buf, "Hello {s}, count: {d}", .{name, count});

// Format with allocation
const str = try std.fmt.allocPrint(allocator, "Value: {d}", .{value});
defer allocator.free(str);
```

### Format Specifiers
```zig
"{d}"       // Decimal integer
"{x}"       // Lowercase hex
"{X}"       // Uppercase hex
"{b}"       // Binary
"{s}"       // String ([]const u8)
"{c}"       // Single character
"{e}"       // Scientific notation float
"{}"        // Default format
"{any}"     // Debug format (any type)
"{*}"       // Pointer address

// Width and fill
"{d:5}"     // Min width 5, right-aligned
"{d:0>5}"   // Zero-padded, width 5
"{s:<10}"   // Left-aligned, width 10
"{d:_^10}"  // Center-aligned with underscores
```

### Parsing
```zig
const num = try std.fmt.parseInt(i32, "42", 10);
const unsigned = try std.fmt.parseUnsigned(u64, "FF", 16);
const float = try std.fmt.parseFloat(f64, "3.14");
```

## File System (std.fs)

### File Operations
```zig
const cwd = std.fs.cwd();

// Open file
const file = try cwd.openFile("path/to/file", .{ .mode = .read_only });
defer file.close();

// Create file
const file = try cwd.createFile("output.txt", .{});
defer file.close();

// Read entire file
const content = try cwd.readFileAlloc(allocator, "file.txt", max_size);
defer allocator.free(content);

// Write entire file
try cwd.writeFile(.{ .sub_path = "file.txt", .data = content });

// Delete
try cwd.deleteFile("file.txt");
try cwd.deleteDir("empty_dir");
try cwd.deleteTree("dir_with_contents");
```

### Directory Operations
```zig
// Create directory
try cwd.makeDir("new_dir");
try cwd.makePath("nested/dirs/path");

// Open directory
var dir = try cwd.openDir("src", .{ .iterate = true });
defer dir.close();

// Iterate directory
var iter = dir.iterate();
while (try iter.next()) |entry| {
    switch (entry.kind) {
        .file => { ... },
        .directory => { ... },
        else => {},
    }
}
```

### Path Operations
```zig
const joined = try std.fs.path.join(allocator, &.{"dir", "subdir", "file.txt"});
const dirname = std.fs.path.dirname("/path/to/file.txt");  // ?[]const u8
const basename = std.fs.path.basename("/path/to/file.txt");  // "file.txt"
const ext = std.fs.path.extension("file.tar.gz");  // ".gz"
```

## I/O (std.io) - Zig 0.15+

### Standard Streams
```zig
const stdout = std.io.getStdOut();
const stderr = std.io.getStdErr();
const stdin = std.io.getStdIn();
```

### Buffered Writing (Zig 0.15+)
```zig
var buffer: [4096]u8 = undefined;
var writer = file.writer(&buffer);

try writer.print("Hello {s}\n", .{name});
try writer.writeAll(data);
try writer.writeByte('\n');

try writer.flush();  // Don't forget!
```

### Buffered Reading (Zig 0.15+)
```zig
var buffer: [4096]u8 = undefined;
var reader = file.reader(&buffer);

// Read line
const line = try reader.readUntilDelimiter(line_buf, '\n');

// Read exact amount
try reader.readNoEof(exact_buf);

// Read all available
const bytes_read = try reader.read(buf);
```

## Data Structures

### ArrayList
```zig
// Prefer ArrayListUnmanaged in Zig 0.15+
var list = std.ArrayListUnmanaged(u32){};
defer list.deinit(allocator);

try list.append(allocator, 42);
try list.appendSlice(allocator, &.{1, 2, 3});
const item = list.pop();  // ?u32
const slice = list.items;

// With capacity hint
try list.ensureTotalCapacity(allocator, 100);
```

### HashMap
```zig
var map = std.StringHashMap(i32).init(allocator);
defer map.deinit();

try map.put("key", 42);
const value = map.get("key");  // ?i32

if (map.getPtr("key")) |ptr| {
    ptr.* += 1;  // Modify in place
}

var iter = map.iterator();
while (iter.next()) |entry| {
    const key = entry.key_ptr.*;
    const value = entry.value_ptr.*;
}
```

### AutoHashMap (for non-string keys)
```zig
var map = std.AutoHashMap(u64, Data).init(allocator);
defer map.deinit();
```

### BufSet (string set)
```zig
var set = std.BufSet.init(allocator);
defer set.deinit();

try set.insert("item");
const exists = set.contains("item");
set.remove("item");
```

## JSON (std.json)

### Parsing
```zig
const parsed = try std.json.parseFromSlice(
    MyStruct,
    allocator,
    json_string,
    .{},
);
defer parsed.deinit();

const data = parsed.value;
```

### Stringify
```zig
var buf = std.ArrayList(u8).init(allocator);
defer buf.deinit();

try std.json.stringify(value, .{}, buf.writer());
const json_string = buf.items;

// With formatting
try std.json.stringify(value, .{ .whitespace = .indent_2 }, writer);
```

### Dynamic JSON
```zig
const parsed = try std.json.parseFromSlice(
    std.json.Value,
    allocator,
    json_string,
    .{},
);
defer parsed.deinit();

const obj = parsed.value.object;
const name = obj.get("name").?.string;
```

## Time (std.time)

```zig
// Current timestamp
const now = std.time.timestamp();  // i64 seconds since epoch
const now_ns = std.time.nanoTimestamp();  // i128 nanoseconds

// Sleep
std.time.sleep(1_000_000_000);  // 1 second in nanoseconds

// Timer
var timer = try std.time.Timer.start();
// ... do work ...
const elapsed_ns = timer.read();
```

## Process (std.process)

### Command Line Arguments
```zig
var args = try std.process.argsWithAllocator(allocator);
defer args.deinit();

_ = args.skip();  // Skip program name
while (args.next()) |arg| {
    // Process arg
}
```

### Environment Variables
```zig
const home = std.process.getEnvVarOwned(allocator, "HOME") catch |err| switch (err) {
    error.EnvironmentVariableNotFound => "/tmp",
    else => return err,
};
defer allocator.free(home);
```

### Child Process
```zig
var child = std.process.Child.init(&.{"ls", "-la"}, allocator);
child.cwd = "/tmp";
child.stdout_behavior = .Pipe;

try child.spawn();
const stdout = child.stdout.?.reader();
// Read stdout...
const result = try child.wait();
```

## Hashing (std.hash)

```zig
// CRC32
const crc = std.hash.Crc32.hash(data);

// Wyhash (fast, good distribution)
const hash = std.hash.Wyhash.hash(0, data);

// For HashMap key hashing
const Context = std.hash_map.StringContext;
```

## Random (std.rand)

```zig
// Default PRNG
var prng = std.rand.DefaultPrng.init(seed);
var random = prng.random();

const value = random.int(u32);
const range = random.intRangeAtMost(u32, 1, 100);  // 1-100 inclusive
const float = random.float(f64);  // 0.0 to 1.0

// Shuffle slice
random.shuffle(ItemType, items);

// Cryptographically secure
var buf: [32]u8 = undefined;
std.crypto.random.bytes(&buf);
```

## Testing (std.testing)

```zig
// Assertions
try std.testing.expect(condition);
try std.testing.expectEqual(expected, actual);
try std.testing.expectEqualStrings("expected", actual);
try std.testing.expectEqualSlices(u8, expected, actual);
try std.testing.expectError(error.Foo, result);

// Leak-detecting allocator
const allocator = std.testing.allocator;

// Failing allocator (for testing error paths)
var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{
    .fail_index = 3,  // Fail on 4th allocation
});
```

## Debug (std.debug)

```zig
// Print to stderr (always available)
std.debug.print("Debug: {}\n", .{value});

// Panic with message
std.debug.panic("Something went wrong: {}", .{err});

// Assert (debug builds only)
std.debug.assert(condition);

// Stack trace
const trace = @returnAddress();
std.debug.dumpStackTrace(trace);
```

## Builtin Functions Reference

### Type Operations
```zig
@TypeOf(value)           // Get type of value
@typeInfo(T)             // Get type metadata
@typeName(T)             // Get type name as string
@sizeOf(T)               // Size in bytes
@alignOf(T)              // Alignment requirement
@bitSizeOf(T)            // Size in bits
```

### Casts
```zig
@intCast(value)          // Int to int (checked)
@floatCast(value)        // Float to float
@floatFromInt(value)     // Int to float
@intFromFloat(value)     // Float to int
@ptrCast(ptr)            // Pointer type change
@alignCast(ptr)          // Align pointer
@constCast(ptr)          // Remove const
@truncate(value)         // Truncate to smaller int
@bitCast(value)          // Reinterpret bits
```

### Memory
```zig
@memcpy(dest, src)       // Copy bytes
@memset(dest, value)     // Fill bytes
@embedFile(path)         // Embed file at compile time
```

### Comptime
```zig
@compileError(msg)       // Compile-time error
@compileLog(values...)   // Compile-time print
@This()                  // Current struct type
@hasDecl(T, name)        // Check if type has declaration
@hasField(T, name)       // Check if struct has field
@field(obj, name)        // Access field by name
```
