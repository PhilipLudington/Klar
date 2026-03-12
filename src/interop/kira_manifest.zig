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

/// Returns true if any parameter or the return type is `string` (needs CStr wrapper).
fn needsStringWrapper(func: FunctionDecl) bool {
    if (std.mem.eql(u8, func.return_type, "string")) return true;
    for (func.params) |param| {
        if (std.mem.eql(u8, param.type_name, "string")) return true;
    }
    return false;
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

                // Determine if any variant has fields (for the data field)
                var has_data_variants = false;
                for (s.variants) |v| {
                    if (v.fields.len > 0) {
                        has_data_variants = true;
                        break;
                    }
                }

                const largest_idx = findLargestVariantIndex(s.variants);
                const data_offset = computeDataOffset(s.variants);

                // Emit unified extern struct (tag + data matching C tagged union layout)
                try appendSlice(allocator, &output, "pub extern struct ");
                try appendSlice(allocator, &output, s.name);
                try appendSlice(allocator, &output, " {\n    tag: ");
                try appendSlice(allocator, &output, s.name);
                try appendSlice(allocator, &output, "Tag,\n");
                if (has_data_variants) {
                    try appendSlice(allocator, &output, "    data: ");
                    try appendSlice(allocator, &output, s.name);
                    try appendSlice(allocator, &output, s.variants[largest_idx].name);
                    try appendSlice(allocator, &output, "Data,\n");
                }
                try appendSlice(allocator, &output, "}\n\n");

                // Emit tag getter for match-based dispatch
                try appendSlice(allocator, &output, "pub fn ");
                try appendLower(allocator, &output, s.name);
                try appendSlice(allocator, &output, "_tag(s: ref ");
                try appendSlice(allocator, &output, s.name);
                try appendSlice(allocator, &output, ") -> ");
                try appendSlice(allocator, &output, s.name);
                try appendSlice(allocator, &output, "Tag {\n    return s.tag\n}\n\n");

                // Emit safe accessor functions for each variant with fields
                for (s.variants, 0..) |v, vi| {
                    if (v.fields.len == 0) continue;

                    const is_single = v.fields.len == 1;

                    // Function signature: pub fn as_<variant>(s: ref Type) -> ?ReturnType
                    try appendSlice(allocator, &output, "pub fn as_");
                    try appendLower(allocator, &output, v.name);
                    try appendSlice(allocator, &output, "(s: ref ");
                    try appendSlice(allocator, &output, s.name);
                    try appendSlice(allocator, &output, ") -> ?");
                    if (is_single) {
                        try appendSlice(allocator, &output, kiraToKlarType(v.fields[0].type_name));
                    } else {
                        try appendSlice(allocator, &output, s.name);
                        try appendSlice(allocator, &output, v.name);
                        try appendSlice(allocator, &output, "Data");
                    }
                    try appendSlice(allocator, &output, " {\n");

                    // Tag check
                    try appendSlice(allocator, &output, "    if s.tag == ");
                    try appendSlice(allocator, &output, s.name);
                    try appendSlice(allocator, &output, "Tag.");
                    try appendSlice(allocator, &output, v.name);
                    try appendSlice(allocator, &output, " {\n");

                    if (vi == largest_idx) {
                        // Largest variant: data field is already the right type
                        if (is_single) {
                            try appendSlice(allocator, &output, "        return Some(s.data.");
                            try appendSlice(allocator, &output, v.fields[0].name);
                            try appendSlice(allocator, &output, ")\n");
                        } else {
                            try appendSlice(allocator, &output, "        return Some(s.data)\n");
                        }
                    } else {
                        // Non-largest variant: ptr_cast to correct type
                        try appendSlice(allocator, &output, "        unsafe {\n");
                        try appendSlice(allocator, &output, "            let p: CPtr#[");
                        try appendSlice(allocator, &output, s.name);
                        try appendSlice(allocator, &output, "] = ref_to_ptr(s)\n");
                        try appendSlice(allocator, &output, "            let bp: CPtr#[u8] = ptr_cast#[u8](p)\n");
                        try appendSlice(allocator, &output, "            let dp: CPtr#[");
                        try appendSlice(allocator, &output, s.name);
                        try appendSlice(allocator, &output, v.name);
                        try appendSlice(allocator, &output, "Data] = ptr_cast#[");
                        try appendSlice(allocator, &output, s.name);
                        try appendSlice(allocator, &output, v.name);
                        try appendSlice(allocator, &output, "Data](offset(bp, ");
                        {
                            var buf: [20]u8 = undefined;
                            const formatted = std.fmt.bufPrint(&buf, "{d}", .{data_offset}) catch unreachable;
                            try appendSlice(allocator, &output, formatted);
                        }
                        try appendSlice(allocator, &output, ".as#[isize]))\n");
                        try appendSlice(allocator, &output, "            let d: ");
                        try appendSlice(allocator, &output, s.name);
                        try appendSlice(allocator, &output, v.name);
                        try appendSlice(allocator, &output, "Data = read(dp)\n");
                        if (is_single) {
                            try appendSlice(allocator, &output, "            return Some(d.");
                            try appendSlice(allocator, &output, v.fields[0].name);
                            try appendSlice(allocator, &output, ")\n");
                        } else {
                            try appendSlice(allocator, &output, "            return Some(d)\n");
                        }
                        try appendSlice(allocator, &output, "        }\n");
                    }

                    try appendSlice(allocator, &output, "    }\n");
                    try appendSlice(allocator, &output, "    return None\n");
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
        if (needsStringWrapper(func)) {
            try appendSlice(allocator, &output, "__raw_");
        }
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

    // Emit string wrapper functions
    for (m.functions) |func| {
        if (!needsStringWrapper(func)) continue;
        const returns_string = std.mem.eql(u8, func.return_type, "string");
        const returns_void = std.mem.eql(u8, func.return_type, "void");

        // pub fn <name>(<params with string→string>) -> <return with string→string>
        try appendSlice(allocator, &output, "\npub fn ");
        try appendSlice(allocator, &output, func.name);
        try appendSlice(allocator, &output, "(");
        for (func.params, 0..) |param, i| {
            if (i > 0) try appendSlice(allocator, &output, ", ");
            try appendSlice(allocator, &output, param.name);
            try appendSlice(allocator, &output, ": ");
            if (std.mem.eql(u8, param.type_name, "string")) {
                try appendSlice(allocator, &output, "string");
            } else {
                try appendSlice(allocator, &output, kiraToKlarType(param.type_name));
            }
        }
        try appendSlice(allocator, &output, ") -> ");
        if (returns_string) {
            try appendSlice(allocator, &output, "string");
        } else {
            try appendSlice(allocator, &output, kiraToKlarType(func.return_type));
        }
        try appendSlice(allocator, &output, " {\n");

        // Body: unsafe { __raw_<name>(args...) }
        try appendSlice(allocator, &output, "    unsafe {\n");
        if (returns_void) {
            try appendSlice(allocator, &output, "        __raw_");
        } else {
            try appendSlice(allocator, &output, "        return __raw_");
        }
        try appendSlice(allocator, &output, func.name);
        try appendSlice(allocator, &output, "(");
        for (func.params, 0..) |param, i| {
            if (i > 0) try appendSlice(allocator, &output, ", ");
            try appendSlice(allocator, &output, param.name);
            if (std.mem.eql(u8, param.type_name, "string")) {
                try appendSlice(allocator, &output, ".as_cstr()");
            }
        }
        try appendSlice(allocator, &output, ")");
        if (returns_string) {
            try appendSlice(allocator, &output, ".to_string()");
        }
        try appendSlice(allocator, &output, "\n");
        try appendSlice(allocator, &output, "    }\n");
        try appendSlice(allocator, &output, "}\n");
    }

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

fn appendLower(allocator: Allocator, output: *std.ArrayListUnmanaged(u8), name: []const u8) !void {
    for (name) |c| {
        try output.append(allocator, if (c >= 'A' and c <= 'Z') c + 32 else c);
    }
}

/// Estimate the C sizeof for a Kira type name (used for finding the largest variant).
fn estimateFieldSize(type_name: []const u8) usize {
    if (std.mem.eql(u8, type_name, "i8") or std.mem.eql(u8, type_name, "u8") or
        std.mem.eql(u8, type_name, "bool") or std.mem.eql(u8, type_name, "Bool"))
        return 1;
    if (std.mem.eql(u8, type_name, "i16") or std.mem.eql(u8, type_name, "u16"))
        return 2;
    if (std.mem.eql(u8, type_name, "i32") or std.mem.eql(u8, type_name, "u32") or
        std.mem.eql(u8, type_name, "f32") or std.mem.eql(u8, type_name, "int"))
        return 4;
    // i64, u64, f64, pointers, string, char — all 8 bytes
    return 8;
}

/// Max alignment across all variant fields (determines union alignment in C).
fn maxFieldAlignment(variants: []const Variant) usize {
    var max_align: usize = 1;
    for (variants) |v| {
        for (v.fields) |f| {
            const field_align = estimateFieldSize(f.type_name);
            if (field_align > max_align) max_align = field_align;
        }
    }
    return max_align;
}

/// Compute the byte offset from struct start to the data (union) portion.
/// In C: tag is i32 (4 bytes), data starts at roundUp(4, union_alignment).
fn computeDataOffset(variants: []const Variant) usize {
    const tag_size: usize = 4; // extern enum: i32
    const data_align = maxFieldAlignment(variants);
    return ((tag_size + data_align - 1) / data_align) * data_align;
}

/// Find the index of the variant with the largest estimated size.
fn findLargestVariantIndex(variants: []const Variant) usize {
    var largest_idx: usize = 0;
    var largest_size: usize = 0;
    for (variants, 0..) |v, i| {
        var size: usize = 0;
        for (v.fields) |f| {
            size += estimateFieldSize(f.type_name);
        }
        if (size > largest_size) {
            largest_size = size;
            largest_idx = i;
        }
    }
    return largest_idx;
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

    // Unified struct (Phase 11: ADT Consumption)
    try std.testing.expect(std.mem.indexOf(u8, source, "pub extern struct Shape {") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "    tag: ShapeTag,") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "    data: ShapeRectangleData,") != null);

    // Tag getter
    try std.testing.expect(std.mem.indexOf(u8, source, "pub fn shape_tag(s: ref Shape) -> ShapeTag {") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "    return s.tag") != null);

    // Accessor for Circle (non-largest, single-field -> ?f64)
    try std.testing.expect(std.mem.indexOf(u8, source, "pub fn as_circle(s: ref Shape) -> ?f64 {") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "    if s.tag == ShapeTag.Circle {") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "ptr_cast#[ShapeCircleData]") != null);

    // Accessor for Rectangle (largest variant, multi-field -> ?ShapeRectangleData)
    try std.testing.expect(std.mem.indexOf(u8, source, "pub fn as_rectangle(s: ref Shape) -> ?ShapeRectangleData {") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "        return Some(s.data)") != null);
}

test "generateKlarSource ADT with unit variants" {
    const allocator = std.testing.allocator;
    const json =
        \\{"module": "traffic", "functions": [], "types": [
        \\  {"name": "Light", "kind": "sum", "variants": [
        \\    {"name": "Red", "tag": 0, "fields": []},
        \\    {"name": "Yellow", "tag": 1, "fields": []},
        \\    {"name": "Green", "tag": 2, "fields": []}
        \\  ]}
        \\]}
    ;
    const m = try parseManifest(allocator, json);
    defer m.deinit(allocator);

    const source = try generateKlarSource(allocator, &m);
    defer allocator.free(source);

    // Tag enum exists
    try std.testing.expect(std.mem.indexOf(u8, source, "pub extern enum LightTag: i32 {") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "    Red = 0,") != null);

    // Unified struct with tag only (no data field since all variants are unit)
    try std.testing.expect(std.mem.indexOf(u8, source, "pub extern struct Light {") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "    tag: LightTag,") != null);
    // No data field — verify there's no "data:" between struct opening and closing
    const struct_start = std.mem.indexOf(u8, source, "pub extern struct Light {") orelse unreachable;
    const struct_region = source[struct_start..@min(struct_start + 100, source.len)];
    try std.testing.expect(std.mem.indexOf(u8, source[struct_start..], "    data:") == null or
        std.mem.indexOf(u8, struct_region, "    data:") == null);

    // Tag getter exists
    try std.testing.expect(std.mem.indexOf(u8, source, "pub fn light_tag(s: ref Light) -> LightTag {") != null);

    // No accessor functions (all variants are unit)
    try std.testing.expect(std.mem.indexOf(u8, source, "pub fn as_red") == null);
    try std.testing.expect(std.mem.indexOf(u8, source, "pub fn as_yellow") == null);
    try std.testing.expect(std.mem.indexOf(u8, source, "pub fn as_green") == null);
}

test "generateKlarSource ADT mixed unit and data variants" {
    const allocator = std.testing.allocator;
    const json =
        \\{"module": "shapes", "functions": [], "types": [
        \\  {"name": "Shape", "kind": "sum", "variants": [
        \\    {"name": "Circle", "tag": 0, "fields": [{"name": "radius", "type": "f64"}]},
        \\    {"name": "Rectangle", "tag": 1, "fields": [{"name": "w", "type": "f64"}, {"name": "h", "type": "f64"}]},
        \\    {"name": "Point", "tag": 2, "fields": []}
        \\  ]}
        \\]}
    ;
    const m = try parseManifest(allocator, json);
    defer m.deinit(allocator);

    const source = try generateKlarSource(allocator, &m);
    defer allocator.free(source);

    // Unified struct has data (because some variants have fields)
    try std.testing.expect(std.mem.indexOf(u8, source, "pub extern struct Shape {") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "    data: ShapeRectangleData,") != null);

    // Accessors for data variants only
    try std.testing.expect(std.mem.indexOf(u8, source, "pub fn as_circle(s: ref Shape) -> ?f64 {") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "pub fn as_rectangle(s: ref Shape) -> ?ShapeRectangleData {") != null);

    // No accessor for unit variant Point
    try std.testing.expect(std.mem.indexOf(u8, source, "pub fn as_point") == null);

    // Tag getter still works for all variants (including Point)
    try std.testing.expect(std.mem.indexOf(u8, source, "pub fn shape_tag(s: ref Shape) -> ShapeTag {") != null);
}

test "generateKlarSource maps string type to CStr with wrapper" {
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

    // Raw extern uses __raw_ prefix with CStr types
    try std.testing.expect(std.mem.indexOf(u8, source, "fn __raw_greet(name: CStr) -> CStr") != null);

    // Wrapper accepts/returns string
    try std.testing.expect(std.mem.indexOf(u8, source, "pub fn greet(name: string) -> string {") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "return __raw_greet(name.as_cstr()).to_string()") != null);
}

test "generateKlarSource string wrapper: params only" {
    const allocator = std.testing.allocator;
    const json =
        \\{"module": "util", "functions": [
        \\  {"name": "count_chars", "params": [{"name": "text", "type": "string"}], "return_type": "i32"}
        \\], "types": []}
    ;
    const m = try parseManifest(allocator, json);
    defer m.deinit(allocator);

    const source = try generateKlarSource(allocator, &m);
    defer allocator.free(source);

    // Extern: __raw_ prefix
    try std.testing.expect(std.mem.indexOf(u8, source, "fn __raw_count_chars(text: CStr) -> i32") != null);

    // Wrapper: string param, i32 return (no .to_string())
    try std.testing.expect(std.mem.indexOf(u8, source, "pub fn count_chars(text: string) -> i32 {") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "return __raw_count_chars(text.as_cstr())") != null);
    // Should NOT have .to_string() on non-string return
    try std.testing.expect(std.mem.indexOf(u8, source, "count_chars(text.as_cstr()).to_string()") == null);
}

test "generateKlarSource string wrapper: return only" {
    const allocator = std.testing.allocator;
    const json =
        \\{"module": "info", "functions": [
        \\  {"name": "get_version", "params": [], "return_type": "string"}
        \\], "types": []}
    ;
    const m = try parseManifest(allocator, json);
    defer m.deinit(allocator);

    const source = try generateKlarSource(allocator, &m);
    defer allocator.free(source);

    // Extern: __raw_ prefix
    try std.testing.expect(std.mem.indexOf(u8, source, "fn __raw_get_version() -> CStr") != null);

    // Wrapper: no params, string return
    try std.testing.expect(std.mem.indexOf(u8, source, "pub fn get_version() -> string {") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "return __raw_get_version().to_string()") != null);
}

test "generateKlarSource string wrapper: void return with string param" {
    const allocator = std.testing.allocator;
    const json =
        \\{"module": "logger", "functions": [
        \\  {"name": "log_msg", "params": [{"name": "msg", "type": "string"}], "return_type": "void"}
        \\], "types": []}
    ;
    const m = try parseManifest(allocator, json);
    defer m.deinit(allocator);

    const source = try generateKlarSource(allocator, &m);
    defer allocator.free(source);

    // Extern: __raw_ prefix
    try std.testing.expect(std.mem.indexOf(u8, source, "fn __raw_log_msg(msg: CStr) -> void") != null);

    // Wrapper: string param, void return (no return keyword)
    try std.testing.expect(std.mem.indexOf(u8, source, "pub fn log_msg(msg: string) -> void {") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "        __raw_log_msg(msg.as_cstr())") != null);
    // No "return" for void
    try std.testing.expect(std.mem.indexOf(u8, source, "return __raw_log_msg") == null);
}

test "generateKlarSource string wrapper: mixed params" {
    const allocator = std.testing.allocator;
    const json =
        \\{"module": "db", "functions": [
        \\  {"name": "query", "params": [{"name": "sql", "type": "string"}, {"name": "limit", "type": "i32"}], "return_type": "string"}
        \\], "types": []}
    ;
    const m = try parseManifest(allocator, json);
    defer m.deinit(allocator);

    const source = try generateKlarSource(allocator, &m);
    defer allocator.free(source);

    // Wrapper: mixed params — only string param gets .as_cstr()
    try std.testing.expect(std.mem.indexOf(u8, source, "pub fn query(sql: string, limit: i32) -> string {") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "__raw_query(sql.as_cstr(), limit).to_string()") != null);
}

test "generateKlarSource no wrapper for non-string functions" {
    const allocator = std.testing.allocator;
    const json =
        \\{"module": "math", "functions": [
        \\  {"name": "add", "params": [{"name": "a", "type": "i32"}, {"name": "b", "type": "i32"}], "return_type": "i32"}
        \\], "types": []}
    ;
    const m = try parseManifest(allocator, json);
    defer m.deinit(allocator);

    const source = try generateKlarSource(allocator, &m);
    defer allocator.free(source);

    // No __raw_ prefix for non-string functions
    try std.testing.expect(std.mem.indexOf(u8, source, "fn add(a: i32, b: i32) -> i32") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "__raw_add") == null);
    // No wrapper generated
    try std.testing.expect(std.mem.indexOf(u8, source, "pub fn add") == null);
}

test "needsStringWrapper detects string params and return" {
    // String return
    try std.testing.expect(needsStringWrapper(.{
        .name = "f",
        .params = &[_]Field{},
        .return_type = "string",
    }));
    // String param
    try std.testing.expect(needsStringWrapper(.{
        .name = "f",
        .params = &[_]Field{.{ .name = "x", .type_name = "string" }},
        .return_type = "void",
    }));
    // No strings
    try std.testing.expect(!needsStringWrapper(.{
        .name = "f",
        .params = &[_]Field{.{ .name = "x", .type_name = "i32" }},
        .return_type = "i32",
    }));
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

    // Unified struct for Color (Custom is the only variant with data)
    try std.testing.expect(std.mem.indexOf(u8, source, "pub extern struct Color {") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "    data: ColorCustomData,") != null);

    // Tag getter
    try std.testing.expect(std.mem.indexOf(u8, source, "pub fn color_tag(s: ref Color) -> ColorTag {") != null);

    // Accessor only for Custom (Red/Green have no fields)
    try std.testing.expect(std.mem.indexOf(u8, source, "pub fn as_custom(s: ref Color) -> ?ColorCustomData {") != null);
    try std.testing.expect(std.mem.indexOf(u8, source, "pub fn as_red") == null);

    // Product type
    try std.testing.expect(std.mem.indexOf(u8, source, "pub extern struct Vec2 {") != null);
}

test "estimateFieldSize returns correct sizes" {
    try std.testing.expectEqual(@as(usize, 1), estimateFieldSize("u8"));
    try std.testing.expectEqual(@as(usize, 1), estimateFieldSize("i8"));
    try std.testing.expectEqual(@as(usize, 1), estimateFieldSize("bool"));
    try std.testing.expectEqual(@as(usize, 2), estimateFieldSize("i16"));
    try std.testing.expectEqual(@as(usize, 4), estimateFieldSize("i32"));
    try std.testing.expectEqual(@as(usize, 4), estimateFieldSize("f32"));
    try std.testing.expectEqual(@as(usize, 8), estimateFieldSize("f64"));
    try std.testing.expectEqual(@as(usize, 8), estimateFieldSize("i64"));
    try std.testing.expectEqual(@as(usize, 8), estimateFieldSize("string"));
    try std.testing.expectEqual(@as(usize, 8), estimateFieldSize("UserType"));
}

test "computeDataOffset aligns correctly" {
    // With f64 fields: tag(4) rounds up to 8
    const f64_variants = [_]Variant{
        .{ .name = "A", .tag = 0, .fields = &[_]Field{.{ .name = "x", .type_name = "f64" }} },
    };
    try std.testing.expectEqual(@as(usize, 8), computeDataOffset(&f64_variants));

    // With i32 fields: tag(4) rounds up to 4 (already aligned)
    const i32_variants = [_]Variant{
        .{ .name = "A", .tag = 0, .fields = &[_]Field{.{ .name = "x", .type_name = "i32" }} },
    };
    try std.testing.expectEqual(@as(usize, 4), computeDataOffset(&i32_variants));

    // With u8 fields: tag(4) rounds up to 4 (alignment is 1, but tag is 4)
    const u8_variants = [_]Variant{
        .{ .name = "A", .tag = 0, .fields = &[_]Field{.{ .name = "x", .type_name = "u8" }} },
    };
    try std.testing.expectEqual(@as(usize, 4), computeDataOffset(&u8_variants));
}

test "findLargestVariantIndex picks correct variant" {
    const variants = [_]Variant{
        .{ .name = "Small", .tag = 0, .fields = &[_]Field{.{ .name = "x", .type_name = "f64" }} },
        .{ .name = "Large", .tag = 1, .fields = &[_]Field{ .{ .name = "w", .type_name = "f64" }, .{ .name = "h", .type_name = "f64" } } },
        .{ .name = "Empty", .tag = 2, .fields = &[_]Field{} },
    };
    try std.testing.expectEqual(@as(usize, 1), findLargestVariantIndex(&variants));
}
