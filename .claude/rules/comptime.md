---
globs: ["*.zig"]
---

# Comptime Programming Rules

## CT1: Compile-Time Constants
- Use `const` at module level for compile-time values
- Use snake_case for comptime constants

```zig
pub const max_buffer_size = 4096;
pub const version = std.SemanticVersion{ .major = 1, .minor = 0, .patch = 0 };
```

## CT2: Comptime Functions
- Use comptime functions for static computation
- Generate lookup tables, validate configs

```zig
fn generateLookupTable() [256]bool {
    var table: [256]bool = undefined;
    for (0..256) |i| {
        table[i] = isValidChar(@intCast(i));
    }
    return table;
}

const lookup = generateLookupTable();  // Computed at compile time
```

## CT3: Generic Types
- Use `comptime T: type` for generic containers
- Return struct from function for generated types

```zig
pub fn ArrayList(comptime T: type) type {
    return struct {
        const Self = @This();
        items: []T,
        // ...
    };
}
```

## CT4: Type Reflection
- Use `@typeInfo` for introspection
- Use `@TypeOf` to get type of value
- Use `inline for` to iterate struct fields

```zig
inline for (@typeInfo(T).Struct.fields) |field| {
    // Process each field at compile time
}
```

## CT5: Comptime Validation
- Validate generic parameters at compile time
- Use `@compileError` for clear error messages

```zig
pub fn Buffer(comptime size: usize) type {
    if (size == 0) {
        @compileError("Buffer size must be > 0");
    }
    return struct {
        data: [size]u8 = undefined,
    };
}
```

## CT6: @embedFile
- Use for embedding static resources
- File contents available at compile time

```zig
const schema = @embedFile("schema.json");
```

---

# Comptime Cookbook

## Advanced Pattern: Type-Safe Builder

Generate builder pattern at compile time:

```zig
fn Builder(comptime T: type) type {
    const fields = @typeInfo(T).Struct.fields;

    return struct {
        const Self = @This();
        values: T = undefined,
        set_flags: [fields.len]bool = [_]bool{false} ** fields.len,

        pub fn build(self: Self) !T {
            inline for (fields, 0..) |field, i| {
                if (!self.set_flags[i] and field.default_value == null) {
                    return error.MissingRequiredField;
                }
            }
            return self.values;
        }

        // Generate setter for each field
        pub usingnamespace blk: {
            var decls = struct {};
            inline for (fields, 0..) |field, i| {
                const setter_name = "set_" ++ field.name;
                @field(decls, setter_name) = struct {
                    fn set(self: *Self, value: field.type) *Self {
                        @field(self.values, field.name) = value;
                        self.set_flags[i] = true;
                        return self;
                    }
                }.set;
            }
            break :blk decls;
        };
    };
}

// Note: usingnamespace removed in 0.15+
// Use explicit field generation instead
```

## Advanced Pattern: Compile-Time String Processing

```zig
fn parseFormat(comptime fmt: []const u8) []const FormatSpec {
    comptime {
        var specs: []const FormatSpec = &.{};
        var i: usize = 0;

        while (i < fmt.len) {
            if (fmt[i] == '{') {
                const end = std.mem.indexOfScalar(u8, fmt[i..], '}') orelse
                    @compileError("Unclosed format specifier");
                specs = specs ++ .{parseSpec(fmt[i + 1 .. i + end])};
                i += end + 1;
            } else {
                i += 1;
            }
        }
        return specs;
    }
}
```

## Advanced Pattern: Interface Verification

Verify type implements required interface at compile time:

```zig
fn Serializable(comptime T: type) type {
    // Verify required methods exist
    if (!@hasDecl(T, "serialize")) {
        @compileError(@typeName(T) ++ " must implement serialize()");
    }
    if (!@hasDecl(T, "deserialize")) {
        @compileError(@typeName(T) ++ " must implement deserialize()");
    }

    // Verify method signatures
    const serialize_info = @typeInfo(@TypeOf(T.serialize));
    if (serialize_info != .Fn) {
        @compileError("serialize must be a function");
    }

    return struct {
        pub fn toBytes(value: T) []const u8 {
            return value.serialize();
        }
    };
}
```

## Advanced Pattern: Enum from String Array

```zig
fn StringEnum(comptime strings: []const []const u8) type {
    var fields: [strings.len]std.builtin.Type.EnumField = undefined;
    for (strings, 0..) |str, i| {
        fields[i] = .{
            .name = str,
            .value = i,
        };
    }

    return @Type(.{
        .Enum = .{
            .tag_type = std.math.IntFittingRange(0, strings.len - 1),
            .fields = &fields,
            .decls = &.{},
            .is_exhaustive = true,
        },
    });
}

const HttpMethod = StringEnum(&.{ "GET", "POST", "PUT", "DELETE" });
```

## Advanced Pattern: Compile-Time Lookup Table

```zig
fn generateCrcTable() [256]u32 {
    var table: [256]u32 = undefined;
    for (0..256) |i| {
        var crc: u32 = @intCast(i);
        for (0..8) |_| {
            if (crc & 1 == 1) {
                crc = (crc >> 1) ^ 0xEDB88320;
            } else {
                crc >>= 1;
            }
        }
        table[i] = crc;
    }
    return table;
}

const crc_table = generateCrcTable();  // Computed at compile time

pub fn crc32(data: []const u8) u32 {
    var crc: u32 = 0xFFFFFFFF;
    for (data) |byte| {
        crc = crc_table[(crc ^ byte) & 0xFF] ^ (crc >> 8);
    }
    return ~crc;
}
```

## Advanced Pattern: Struct Field Iteration

```zig
fn printStruct(value: anytype) void {
    const T = @TypeOf(value);
    const info = @typeInfo(T).Struct;

    std.debug.print("{s} {{\n", .{@typeName(T)});
    inline for (info.fields) |field| {
        const field_value = @field(value, field.name);
        std.debug.print("  .{s} = {any},\n", .{ field.name, field_value });
    }
    std.debug.print("}}\n", .{});
}
```

## Advanced Pattern: Type-Safe Flags

```zig
fn Flags(comptime E: type) type {
    const info = @typeInfo(E).Enum;

    return packed struct {
        const Self = @This();
        bits: std.meta.Int(.unsigned, info.fields.len) = 0,

        pub fn init(flags: []const E) Self {
            var result = Self{};
            for (flags) |flag| {
                result.bits |= @as(@TypeOf(result.bits), 1) << @intFromEnum(flag);
            }
            return result;
        }

        pub fn contains(self: Self, flag: E) bool {
            return (self.bits >> @intFromEnum(flag)) & 1 == 1;
        }

        pub fn set(self: *Self, flag: E) void {
            self.bits |= @as(@TypeOf(self.bits), 1) << @intFromEnum(flag);
        }
    };
}

const Permission = enum { read, write, execute };
const Permissions = Flags(Permission);

// Usage
const perms = Permissions.init(&.{ .read, .execute });
if (perms.contains(.write)) { ... }
```

## Advanced Pattern: Compile-Time JSON Schema

```zig
fn JsonSchema(comptime T: type) []const u8 {
    comptime {
        var schema: []const u8 = "{";
        const info = @typeInfo(T).Struct;

        for (info.fields, 0..) |field, i| {
            if (i > 0) schema = schema ++ ",";
            schema = schema ++ "\"" ++ field.name ++ "\":{\"type\":\"";
            schema = schema ++ jsonTypeName(field.type) ++ "\"}";
        }

        return schema ++ "}";
    }
}

fn jsonTypeName(comptime T: type) []const u8 {
    return switch (@typeInfo(T)) {
        .Int => "integer",
        .Float => "number",
        .Bool => "boolean",
        .Pointer => |p| if (p.child == u8) "string" else "array",
        .Optional => |o| jsonTypeName(o.child),
        else => "object",
    };
}
```

## Pattern: Compile-Time Assertions

```zig
fn assertPowerOfTwo(comptime n: usize) void {
    if (n == 0 or (n & (n - 1)) != 0) {
        @compileError("Value must be a power of two");
    }
}

fn Buffer(comptime size: usize) type {
    assertPowerOfTwo(size);  // Compile-time check

    return struct {
        data: [size]u8 = undefined,
    };
}

const GoodBuffer = Buffer(1024);  // OK
// const BadBuffer = Buffer(1000);  // Compile error
```

## Pattern: Optional Field Access

```zig
fn getFieldOrDefault(comptime T: type, value: T, comptime field_name: []const u8, default: anytype) @TypeOf(default) {
    if (@hasField(T, field_name)) {
        return @field(value, field_name);
    } else {
        return default;
    }
}

// Usage
const name = getFieldOrDefault(Config, config, "name", "default");
```
