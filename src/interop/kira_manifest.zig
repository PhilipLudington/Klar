//! Kira manifest parser and Klar extern block generator.
//!
//! Reads a Kira type manifest JSON (produced by `kira build --manifest`)
//! and generates a `.kl` file containing extern declarations that Klar
//! can import to call Kira library functions.
//!
//! Usage:
//!   klar import-kira manifest.json           -> writes kira_<module>.kl
//!   klar import-kira manifest.json -o out.kl -> writes to out.kl

const std = @import("std");
const Allocator = std.mem.Allocator;

/// A parsed field (used in function params, struct fields, variant fields).
pub const Field = struct {
    name: []const u8,
    type_name: []const u8,
};

/// A variant of a sum type.
pub const Variant = struct {
    name: []const u8,
    tag: i64,
    fields: []const Field,
};

/// A type declaration from the manifest.
pub const TypeDecl = union(enum) {
    sum: struct {
        name: []const u8,
        variants: []const Variant,
    },
    product: struct {
        name: []const u8,
        fields: []const Field,
    },
};

/// A function declaration from the manifest.
pub const FunctionDecl = struct {
    name: []const u8,
    params: []const Field,
    return_type: []const u8,
};

/// A fully parsed Kira manifest.
pub const KiraManifest = struct {
    module_name: []const u8,
    functions: []const FunctionDecl,
    types: []const TypeDecl,

    pub fn deinit(self: *const KiraManifest, allocator: Allocator) void {
        allocator.free(self.module_name);

        // Free functions
        for (self.functions) |func| {
            allocator.free(func.name);
            allocator.free(func.return_type);
            for (func.params) |p| {
                allocator.free(p.name);
                allocator.free(p.type_name);
            }
            allocator.free(func.params);
        }
        allocator.free(self.functions);

        // Free types
        for (self.types) |td| {
            switch (td) {
                .sum => |s| {
                    allocator.free(s.name);
                    for (s.variants) |v| {
                        allocator.free(v.name);
                        for (v.fields) |f| {
                            allocator.free(f.name);
                            allocator.free(f.type_name);
                        }
                        allocator.free(v.fields);
                    }
                    allocator.free(s.variants);
                },
                .product => |p| {
                    allocator.free(p.name);
                    for (p.fields) |f| {
                        allocator.free(f.name);
                        allocator.free(f.type_name);
                    }
                    allocator.free(p.fields);
                },
            }
        }
        allocator.free(self.types);
    }
};

/// Parse a Kira type manifest from JSON content.
pub fn parseManifest(allocator: Allocator, content: []const u8) !KiraManifest {
    const parsed = std.json.parseFromSlice(std.json.Value, allocator, content, .{}) catch {
        return error.InvalidJson;
    };
    defer parsed.deinit();

    const root = parsed.value;
    if (root != .object) return error.InvalidJson;

    // Module name (required)
    const module_name = blk: {
        const val = root.object.get("module") orelse return error.MissingModule;
        break :blk switch (val) {
            .string => |s| try allocator.dupe(u8, s),
            else => return error.MissingModule,
        };
    };
    errdefer allocator.free(module_name);

    // Functions (required array)
    var functions = std.ArrayListUnmanaged(FunctionDecl){};
    errdefer {
        for (functions.items) |func| allocator.free(func.params);
        functions.deinit(allocator);
    }

    if (root.object.get("functions")) |funcs_val| {
        if (funcs_val == .array) {
            for (funcs_val.array.items) |func_val| {
                if (func_val != .object) continue;
                const func_obj = func_val.object;

                const name = switch (func_obj.get("name") orelse continue) {
                    .string => |s| try allocator.dupe(u8, s),
                    else => continue,
                };
                errdefer allocator.free(name);

                const return_type = switch (func_obj.get("return_type") orelse continue) {
                    .string => |s| try allocator.dupe(u8, s),
                    else => {
                        allocator.free(name);
                        continue;
                    },
                };
                errdefer allocator.free(return_type);

                // Parse params
                var params = std.ArrayListUnmanaged(Field){};
                errdefer {
                    for (params.items) |p| {
                        allocator.free(p.name);
                        allocator.free(p.type_name);
                    }
                    params.deinit(allocator);
                }

                if (func_obj.get("params")) |params_val| {
                    if (params_val == .array) {
                        for (params_val.array.items) |param_val| {
                            if (param_val != .object) continue;
                            const param_obj = param_val.object;
                            const pname = switch (param_obj.get("name") orelse continue) {
                                .string => |s| try allocator.dupe(u8, s),
                                else => continue,
                            };
                            errdefer allocator.free(pname);
                            const ptype = switch (param_obj.get("type") orelse continue) {
                                .string => |s| try allocator.dupe(u8, s),
                                else => {
                                    allocator.free(pname);
                                    continue;
                                },
                            };
                            try params.append(allocator, .{ .name = pname, .type_name = ptype });
                        }
                    }
                }

                try functions.append(allocator, .{
                    .name = name,
                    .params = try params.toOwnedSlice(allocator),
                    .return_type = return_type,
                });
            }
        }
    }

    // Types (required array)
    var types = std.ArrayListUnmanaged(TypeDecl){};
    errdefer {
        for (types.items) |td| {
            switch (td) {
                .sum => |s| {
                    for (s.variants) |v| allocator.free(v.fields);
                    allocator.free(s.variants);
                },
                .product => |p| allocator.free(p.fields),
            }
        }
        types.deinit(allocator);
    }

    if (root.object.get("types")) |types_val| {
        if (types_val == .array) {
            for (types_val.array.items) |type_val| {
                if (type_val != .object) continue;
                const type_obj = type_val.object;

                const name = switch (type_obj.get("name") orelse continue) {
                    .string => |s| try allocator.dupe(u8, s),
                    else => continue,
                };
                errdefer allocator.free(name);

                const kind = switch (type_obj.get("kind") orelse continue) {
                    .string => |s| s, // not duped — only used for comparison
                    else => {
                        allocator.free(name);
                        continue;
                    },
                };

                if (std.mem.eql(u8, kind, "sum")) {
                    var variants = std.ArrayListUnmanaged(Variant){};
                    errdefer {
                        for (variants.items) |v| {
                            allocator.free(v.name);
                            for (v.fields) |f| {
                                allocator.free(f.name);
                                allocator.free(f.type_name);
                            }
                            allocator.free(v.fields);
                        }
                        variants.deinit(allocator);
                    }

                    if (type_obj.get("variants")) |variants_val| {
                        if (variants_val == .array) {
                            for (variants_val.array.items) |v_val| {
                                if (v_val != .object) continue;
                                const v_obj = v_val.object;

                                const vname = switch (v_obj.get("name") orelse continue) {
                                    .string => |s| try allocator.dupe(u8, s),
                                    else => continue,
                                };
                                errdefer allocator.free(vname);

                                const tag: i64 = switch (v_obj.get("tag") orelse continue) {
                                    .integer => |i| i,
                                    else => {
                                        allocator.free(vname);
                                        continue;
                                    },
                                };

                                var fields = std.ArrayListUnmanaged(Field){};
                                errdefer {
                                    for (fields.items) |f| {
                                        allocator.free(f.name);
                                        allocator.free(f.type_name);
                                    }
                                    fields.deinit(allocator);
                                }

                                if (v_obj.get("fields")) |fields_val| {
                                    if (fields_val == .array) {
                                        for (fields_val.array.items) |f_val| {
                                            if (f_val != .object) continue;
                                            const f_obj = f_val.object;
                                            const fname = switch (f_obj.get("name") orelse continue) {
                                                .string => |s| try allocator.dupe(u8, s),
                                                else => continue,
                                            };
                                            errdefer allocator.free(fname);
                                            const ftype = switch (f_obj.get("type") orelse continue) {
                                                .string => |s| try allocator.dupe(u8, s),
                                                else => {
                                                    allocator.free(fname);
                                                    continue;
                                                },
                                            };
                                            try fields.append(allocator, .{ .name = fname, .type_name = ftype });
                                        }
                                    }
                                }

                                try variants.append(allocator, .{
                                    .name = vname,
                                    .tag = tag,
                                    .fields = try fields.toOwnedSlice(allocator),
                                });
                            }
                        }
                    }

                    try types.append(allocator, .{
                        .sum = .{
                            .name = name,
                            .variants = try variants.toOwnedSlice(allocator),
                        },
                    });
                } else if (std.mem.eql(u8, kind, "product")) {
                    var fields = std.ArrayListUnmanaged(Field){};
                    errdefer {
                        for (fields.items) |f| {
                            allocator.free(f.name);
                            allocator.free(f.type_name);
                        }
                        fields.deinit(allocator);
                    }

                    if (type_obj.get("fields")) |fields_val| {
                        if (fields_val == .array) {
                            for (fields_val.array.items) |f_val| {
                                if (f_val != .object) continue;
                                const f_obj = f_val.object;
                                const fname = switch (f_obj.get("name") orelse continue) {
                                    .string => |s| try allocator.dupe(u8, s),
                                    else => continue,
                                };
                                errdefer allocator.free(fname);
                                const ftype = switch (f_obj.get("type") orelse continue) {
                                    .string => |s| try allocator.dupe(u8, s),
                                    else => {
                                        allocator.free(fname);
                                        continue;
                                    },
                                };
                                try fields.append(allocator, .{ .name = fname, .type_name = ftype });
                            }
                        }
                    }

                    try types.append(allocator, .{
                        .product = .{
                            .name = name,
                            .fields = try fields.toOwnedSlice(allocator),
                        },
                    });
                } else {
                    allocator.free(name);
                }
            }
        }
    }

    return .{
        .module_name = module_name,
        .functions = try functions.toOwnedSlice(allocator),
        .types = try types.toOwnedSlice(allocator),
    };
}

/// Map a Kira type name to a Klar FFI type name.
pub fn kiraToKlarType(kira_type: []const u8) []const u8 {
    if (std.mem.eql(u8, kira_type, "i8")) return "i8";
    if (std.mem.eql(u8, kira_type, "i16")) return "i16";
    if (std.mem.eql(u8, kira_type, "i32") or std.mem.eql(u8, kira_type, "int")) return "i32";
    if (std.mem.eql(u8, kira_type, "i64")) return "i64";
    if (std.mem.eql(u8, kira_type, "i128")) return "i128";
    if (std.mem.eql(u8, kira_type, "u8")) return "u8";
    if (std.mem.eql(u8, kira_type, "u16")) return "u16";
    if (std.mem.eql(u8, kira_type, "u32")) return "u32";
    if (std.mem.eql(u8, kira_type, "u64")) return "u64";
    if (std.mem.eql(u8, kira_type, "u128")) return "u128";
    if (std.mem.eql(u8, kira_type, "f32")) return "f32";
    if (std.mem.eql(u8, kira_type, "f64") or std.mem.eql(u8, kira_type, "float")) return "f64";
    if (std.mem.eql(u8, kira_type, "bool")) return "Bool";
    if (std.mem.eql(u8, kira_type, "char")) return "Char";
    if (std.mem.eql(u8, kira_type, "string")) return "CStr";
    if (std.mem.eql(u8, kira_type, "void")) return "void";
    // User-defined types pass through unchanged
    return kira_type;
}

/// Generate a Klar `.kl` file from a parsed manifest.
/// The output contains extern struct/enum declarations and an extern function block.
pub fn generateKlarSource(allocator: Allocator, m: *const KiraManifest) ![]u8 {
    var output = std.ArrayListUnmanaged(u8){};
    errdefer output.deinit(allocator);

    // Header comment
    try appendSlice(allocator, &output, "// Generated by `klar import-kira` from Kira module: ");
    try appendSlice(allocator, &output, m.module_name);
    try appendSlice(allocator, &output, "\n// Do not edit — regenerate with `klar import-kira <manifest.json>`\n\n");

    // Emit type declarations
    for (m.types) |td| {
        switch (td) {
            .sum => |s| {
                // Emit extern enum for the tag (pub so it's exported for import)
                try appendSlice(allocator, &output, "pub extern enum ");
                try appendSlice(allocator, &output, s.name);
                try appendSlice(allocator, &output, "Tag: i32 {\n");
                for (s.variants) |v| {
                    try appendSlice(allocator, &output, "    ");
                    try appendSlice(allocator, &output, v.name);
                    try appendSlice(allocator, &output, " = ");
                    try appendInt(allocator, &output, v.tag);
                    try appendSlice(allocator, &output, ",\n");
                }
                try appendSlice(allocator, &output, "}\n\n");

                // Emit extern struct for each variant that has fields
                for (s.variants) |v| {
                    if (v.fields.len == 0) continue;
                    try appendSlice(allocator, &output, "pub extern struct ");
                    try appendSlice(allocator, &output, s.name);
                    try appendSlice(allocator, &output, v.name);
                    try appendSlice(allocator, &output, "Data {\n");
                    for (v.fields) |f| {
                        try appendSlice(allocator, &output, "    ");
                        try appendSlice(allocator, &output, f.name);
                        try appendSlice(allocator, &output, ": ");
                        try appendSlice(allocator, &output, kiraToKlarType(f.type_name));
                        try appendSlice(allocator, &output, ",\n");
                    }
                    try appendSlice(allocator, &output, "}\n\n");
                }
            },
            .product => |p| {
                try appendSlice(allocator, &output, "pub extern struct ");
                try appendSlice(allocator, &output, p.name);
                try appendSlice(allocator, &output, " {\n");
                for (p.fields) |f| {
                    try appendSlice(allocator, &output, "    ");
                    try appendSlice(allocator, &output, f.name);
                    try appendSlice(allocator, &output, ": ");
                    try appendSlice(allocator, &output, kiraToKlarType(f.type_name));
                    try appendSlice(allocator, &output, ",\n");
                }
                try appendSlice(allocator, &output, "}\n\n");
            },
        }
    }

    // Emit extern function block
    try appendSlice(allocator, &output, "extern {\n");
    for (m.functions) |func| {
        try appendSlice(allocator, &output, "    fn ");
        try appendSlice(allocator, &output, func.name);
        try appendSlice(allocator, &output, "(");
        for (func.params, 0..) |param, i| {
            if (i > 0) try appendSlice(allocator, &output, ", ");
            try appendSlice(allocator, &output, param.name);
            try appendSlice(allocator, &output, ": ");
            try appendSlice(allocator, &output, kiraToKlarType(param.type_name));
        }
        try appendSlice(allocator, &output, ") -> ");
        try appendSlice(allocator, &output, kiraToKlarType(func.return_type));
        try appendSlice(allocator, &output, "\n");
    }

    // Always include kira_free
    try appendSlice(allocator, &output, "    fn kira_free(ptr: CPtr#[void]) -> void\n");
    try appendSlice(allocator, &output, "}\n");

    return output.toOwnedSlice(allocator);
}

/// Run the import-kira command: read manifest JSON, generate .kl output.
pub fn importKiraCommand(allocator: Allocator, args: []const []const u8) !void {
    const stderr = std.fs.File{ .handle = if (comptime @import("builtin").os.tag == .windows) (std.os.windows.kernel32.GetStdHandle(std.os.windows.STD_ERROR_HANDLE) orelse @panic("failed to get stderr handle")) else std.posix.STDERR_FILENO };
    const stdout = std.fs.File{ .handle = if (comptime @import("builtin").os.tag == .windows) (std.os.windows.kernel32.GetStdHandle(std.os.windows.STD_OUTPUT_HANDLE) orelse @panic("failed to get stdout handle")) else std.posix.STDOUT_FILENO };

    var manifest_path: ?[]const u8 = null;
    var output_path: ?[]const u8 = null;

    var i: usize = 2;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "-o") or std.mem.eql(u8, arg, "--output")) {
            if (i + 1 >= args.len) {
                try stderr.writeAll("Error: missing value for -o\n");
                return;
            }
            output_path = args[i + 1];
            i += 1;
        } else if (std.mem.startsWith(u8, arg, "-o=")) {
            output_path = arg["-o=".len..];
        } else if (std.mem.startsWith(u8, arg, "--output=")) {
            output_path = arg["--output=".len..];
        } else if (!std.mem.startsWith(u8, arg, "-") and manifest_path == null) {
            manifest_path = arg;
        } else {
            var buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Error: unknown option '{s}'\n", .{arg}) catch "Error: unknown option\n";
            try stderr.writeAll(msg);
            return;
        }
    }

    const input_path = manifest_path orelse {
        try stderr.writeAll("Error: missing manifest path\nUsage: klar import-kira <manifest.json> [-o <output.kl>]\n");
        return;
    };

    // Read manifest file
    const content = std.fs.cwd().readFileAlloc(allocator, input_path, 10 * 1024 * 1024) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Error: could not read '{s}': {s}\n", .{ input_path, @errorName(err) }) catch "Error: could not read manifest file\n";
        try stderr.writeAll(msg);
        return;
    };
    defer allocator.free(content);

    // Parse manifest
    const m = parseManifest(allocator, content) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Error: could not parse manifest: {s}\n", .{@errorName(err)}) catch "Error: could not parse manifest\n";
        try stderr.writeAll(msg);
        return;
    };
    defer m.deinit(allocator);

    // Generate Klar source
    const klar_source = generateKlarSource(allocator, &m) catch |err| {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Error: code generation failed: {s}\n", .{@errorName(err)}) catch "Error: code generation failed\n";
        try stderr.writeAll(msg);
        return;
    };
    defer allocator.free(klar_source);

    // Write output
    if (output_path) |out| {
        const file = std.fs.cwd().createFile(out, .{}) catch |err| {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Error: could not create '{s}': {s}\n", .{ out, @errorName(err) }) catch "Error: could not create output file\n";
            try stderr.writeAll(msg);
            return;
        };
        defer file.close();
        file.writeAll(klar_source) catch |err| {
            var buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Error: write failed: {s}\n", .{@errorName(err)}) catch "Error: write failed\n";
            try stderr.writeAll(msg);
            return;
        };

        // Report success
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Generated {s} from Kira module '{s}' ({d} functions, {d} types)\n", .{
            out,
            m.module_name,
            m.functions.len,
            m.types.len,
        }) catch "Generated output file\n";
        try stdout.writeAll(msg);
    } else {
        // Default output: deps/kira_<module>.kl
        // Create deps/ directory if it doesn't exist
        std.fs.cwd().makeDir("deps") catch |err| {
            if (err != error.PathAlreadyExists) {
                var buf: [256]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Error: could not create deps/ directory: {s}\n", .{@errorName(err)}) catch "Error: could not create deps/ directory\n";
                try stderr.writeAll(msg);
                return;
            }
        };

        var name_buf: [256]u8 = undefined;
        const default_name = std.fmt.bufPrint(&name_buf, "deps/kira_{s}.kl", .{m.module_name}) catch {
            try stderr.writeAll("Error: module name too long for default output path\n");
            return;
        };

        const file = std.fs.cwd().createFile(default_name, .{}) catch |err| {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Error: could not create '{s}': {s}\n", .{ default_name, @errorName(err) }) catch "Error: could not create output file\n";
            try stderr.writeAll(msg);
            return;
        };
        defer file.close();
        file.writeAll(klar_source) catch |err| {
            var buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Error: write failed: {s}\n", .{@errorName(err)}) catch "Error: write failed\n";
            try stderr.writeAll(msg);
            return;
        };

        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Generated {s} from Kira module '{s}' ({d} functions, {d} types)\n", .{
            default_name,
            m.module_name,
            m.functions.len,
            m.types.len,
        }) catch "Generated output file\n";
        try stdout.writeAll(msg);
    }
}

fn appendSlice(allocator: Allocator, output: *std.ArrayListUnmanaged(u8), s: []const u8) !void {
    try output.appendSlice(allocator, s);
}

fn appendInt(allocator: Allocator, output: *std.ArrayListUnmanaged(u8), value: i64) !void {
    var buf: [20]u8 = undefined;
    const formatted = std.fmt.bufPrint(&buf, "{d}", .{value}) catch unreachable;
    try output.appendSlice(allocator, formatted);
}

// ── Tests ──────────────────────────────────────────────────────────────────

test "kiraToKlarType maps primitives" {
    try std.testing.expectEqualStrings("i32", kiraToKlarType("i32"));
    try std.testing.expectEqualStrings("i32", kiraToKlarType("int"));
    try std.testing.expectEqualStrings("i64", kiraToKlarType("i64"));
    try std.testing.expectEqualStrings("f64", kiraToKlarType("f64"));
    try std.testing.expectEqualStrings("f64", kiraToKlarType("float"));
    try std.testing.expectEqualStrings("Bool", kiraToKlarType("bool"));
    try std.testing.expectEqualStrings("CStr", kiraToKlarType("string"));
    try std.testing.expectEqualStrings("void", kiraToKlarType("void"));
    try std.testing.expectEqualStrings("u8", kiraToKlarType("u8"));
    try std.testing.expectEqualStrings("Char", kiraToKlarType("char"));
}

test "kiraToKlarType passes through user-defined types" {
    try std.testing.expectEqualStrings("Shape", kiraToKlarType("Shape"));
    try std.testing.expectEqualStrings("Point", kiraToKlarType("Point"));
    try std.testing.expectEqualStrings("MyCustomType", kiraToKlarType("MyCustomType"));
}

test "parseManifest with functions only" {
    const allocator = std.testing.allocator;
    const json =
        \\{
        \\  "module": "mathlib",
        \\  "functions": [
        \\    {
        \\      "name": "add",
        \\      "params": [{"name": "a", "type": "i32"}, {"name": "b", "type": "i32"}],
        \\      "return_type": "i32"
        \\    },
        \\    {
        \\      "name": "greet",
        \\      "params": [{"name": "name", "type": "string"}],
        \\      "return_type": "string"
        \\    }
        \\  ],
        \\  "types": []
        \\}
    ;

    const m = try parseManifest(allocator, json);
    defer m.deinit(allocator);

    try std.testing.expectEqualStrings("mathlib", m.module_name);
    try std.testing.expectEqual(@as(usize, 2), m.functions.len);
    try std.testing.expectEqualStrings("add", m.functions[0].name);
    try std.testing.expectEqual(@as(usize, 2), m.functions[0].params.len);
    try std.testing.expectEqualStrings("a", m.functions[0].params[0].name);
    try std.testing.expectEqualStrings("i32", m.functions[0].params[0].type_name);
    try std.testing.expectEqualStrings("i32", m.functions[0].return_type);
    try std.testing.expectEqualStrings("greet", m.functions[1].name);
    try std.testing.expectEqualStrings("string", m.functions[1].return_type);
}

test "parseManifest with sum and product types" {
    const allocator = std.testing.allocator;
    const json =
        \\{
        \\  "module": "shapes",
        \\  "functions": [],
        \\  "types": [
        \\    {
        \\      "name": "Shape",
        \\      "kind": "sum",
        \\      "variants": [
        \\        {"name": "Circle", "tag": 0, "fields": [{"name": "radius", "type": "f64"}]},
        \\        {"name": "Rectangle", "tag": 1, "fields": [{"name": "w", "type": "f64"}, {"name": "h", "type": "f64"}]}
        \\      ]
        \\    },
        \\    {
        \\      "name": "Point",
        \\      "kind": "product",
        \\      "fields": [{"name": "x", "type": "f64"}, {"name": "y", "type": "f64"}]
        \\    }
        \\  ]
        \\}
    ;

    const m = try parseManifest(allocator, json);
    defer m.deinit(allocator);

    try std.testing.expectEqualStrings("shapes", m.module_name);
    try std.testing.expectEqual(@as(usize, 2), m.types.len);

    // Sum type
    switch (m.types[0]) {
        .sum => |s| {
            try std.testing.expectEqualStrings("Shape", s.name);
            try std.testing.expectEqual(@as(usize, 2), s.variants.len);
            try std.testing.expectEqualStrings("Circle", s.variants[0].name);
            try std.testing.expectEqual(@as(i64, 0), s.variants[0].tag);
            try std.testing.expectEqual(@as(usize, 1), s.variants[0].fields.len);
            try std.testing.expectEqualStrings("radius", s.variants[0].fields[0].name);
            try std.testing.expectEqualStrings("Rectangle", s.variants[1].name);
            try std.testing.expectEqual(@as(i64, 1), s.variants[1].tag);
            try std.testing.expectEqual(@as(usize, 2), s.variants[1].fields.len);
        },
        else => return error.TestUnexpectedResult,
    }

    // Product type
    switch (m.types[1]) {
        .product => |p| {
            try std.testing.expectEqualStrings("Point", p.name);
            try std.testing.expectEqual(@as(usize, 2), p.fields.len);
            try std.testing.expectEqualStrings("x", p.fields[0].name);
            try std.testing.expectEqualStrings("f64", p.fields[0].type_name);
        },
        else => return error.TestUnexpectedResult,
    }
}

test "parseManifest rejects invalid JSON" {
    const allocator = std.testing.allocator;
    const result = parseManifest(allocator, "not json");
    try std.testing.expectError(error.InvalidJson, result);
}

test "parseManifest rejects missing module" {
    const allocator = std.testing.allocator;
    const result = parseManifest(allocator, "{\"functions\": [], \"types\": []}");
    try std.testing.expectError(error.MissingModule, result);
}

test "generateKlarSource with functions" {
    const allocator = std.testing.allocator;
    const json =
        \\{"module": "mathlib", "functions": [
        \\  {"name": "add", "params": [{"name": "a", "type": "i32"}, {"name": "b", "type": "i32"}], "return_type": "i32"}
        \\], "types": []}
    ;
    const m = try parseManifest(allocator, json);
    defer m.deinit(allocator);

    const source = try generateKlarSource(allocator, &m);
    defer allocator.free(source);

    try std.testing.expect(std.mem.indexOf(u8, source, "// Generated by `klar import-kira`") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "mathlib") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "extern {") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "fn add(a: i32, b: i32) -> i32") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "fn kira_free(ptr: CPtr#[void]) -> void") != null);
}

test "generateKlarSource with product type" {
    const allocator = std.testing.allocator;
    const json =
        \\{"module": "geo", "functions": [], "types": [
        \\  {"name": "Point", "kind": "product", "fields": [{"name": "x", "type": "f64"}, {"name": "y", "type": "f64"}]}
        \\]}
    ;
    const m = try parseManifest(allocator, json);
    defer m.deinit(allocator);

    const source = try generateKlarSource(allocator, &m);
    defer allocator.free(source);

    try std.testing.expect(std.mem.indexOf(u8, source, "pub extern struct Point {") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "    x: f64,") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "    y: f64,") != null);
}

test "generateKlarSource with sum type" {
    const allocator = std.testing.allocator;
    const json =
        \\{"module": "shapes", "functions": [], "types": [
        \\  {"name": "Shape", "kind": "sum", "variants": [
        \\    {"name": "Circle", "tag": 0, "fields": [{"name": "radius", "type": "f64"}]},
        \\    {"name": "Rectangle", "tag": 1, "fields": [{"name": "w", "type": "f64"}, {"name": "h", "type": "f64"}]}
        \\  ]}
        \\]}
    ;
    const m = try parseManifest(allocator, json);
    defer m.deinit(allocator);

    const source = try generateKlarSource(allocator, &m);
    defer allocator.free(source);

    // Tag enum
    try std.testing.expect(std.mem.indexOf(u8, source, "pub extern enum ShapeTag: i32 {") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "    Circle = 0,") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "    Rectangle = 1,") != null);

    // Variant data structs
    try std.testing.expect(std.mem.indexOf(u8, source, "pub extern struct ShapeCircleData {") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "    radius: f64,") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "pub extern struct ShapeRectangleData {") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "    w: f64,") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "    h: f64,") != null);
}

test "generateKlarSource maps string type to CStr" {
    const allocator = std.testing.allocator;
    const json =
        \\{"module": "hello", "functions": [
        \\  {"name": "greet", "params": [{"name": "name", "type": "string"}], "return_type": "string"}
        \\], "types": []}
    ;
    const m = try parseManifest(allocator, json);
    defer m.deinit(allocator);

    const source = try generateKlarSource(allocator, &m);
    defer allocator.free(source);

    try std.testing.expect(std.mem.indexOf(u8, source, "fn greet(name: CStr) -> CStr") != null);
}

test "round-trip parseManifest then generateKlarSource" {
    const allocator = std.testing.allocator;
    const json =
        \\{
        \\  "module": "mylib",
        \\  "functions": [
        \\    {
        \\      "name": "add",
        \\      "params": [{"name": "a", "type": "i32"}, {"name": "b", "type": "i32"}],
        \\      "return_type": "i32"
        \\    },
        \\    {
        \\      "name": "scale",
        \\      "params": [{"name": "x", "type": "f64"}, {"name": "factor", "type": "f64"}],
        \\      "return_type": "f64"
        \\    },
        \\    {
        \\      "name": "noop",
        \\      "params": [],
        \\      "return_type": "void"
        \\    }
        \\  ],
        \\  "types": [
        \\    {
        \\      "name": "Color",
        \\      "kind": "sum",
        \\      "variants": [
        \\        {"name": "Red", "tag": 0, "fields": []},
        \\        {"name": "Green", "tag": 1, "fields": []},
        \\        {"name": "Custom", "tag": 2, "fields": [{"name": "r", "type": "u8"}, {"name": "g", "type": "u8"}, {"name": "b", "type": "u8"}]}
        \\      ]
        \\    },
        \\    {
        \\      "name": "Vec2",
        \\      "kind": "product",
        \\      "fields": [{"name": "x", "type": "f64"}, {"name": "y", "type": "f64"}]
        \\    }
        \\  ]
        \\}
    ;

    const m = try parseManifest(allocator, json);
    defer m.deinit(allocator);

    const source = try generateKlarSource(allocator, &m);
    defer allocator.free(source);

    // All functions present
    try std.testing.expect(std.mem.indexOf(u8, source, "fn add(a: i32, b: i32) -> i32") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "fn scale(x: f64, factor: f64) -> f64") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "fn noop() -> void") != null);

    // Tag enum with all variants
    try std.testing.expect(std.mem.indexOf(u8, source, "pub extern enum ColorTag: i32 {") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "    Red = 0,") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "    Green = 1,") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "    Custom = 2,") != null);

    // Only Custom has a data struct (Red and Green have no fields)
    try std.testing.expect(std.mem.indexOf(u8, source, "pub extern struct ColorCustomData {") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "pub extern struct ColorRedData {") == null);

    // Product type
    try std.testing.expect(std.mem.indexOf(u8, source, "pub extern struct Vec2 {") != null);
}
