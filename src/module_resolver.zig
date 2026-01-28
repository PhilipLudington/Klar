//! Module resolution for multi-file compilation.
//!
//! Discovers modules from imports, builds dependency graph, and provides
//! topological ordering for compilation.

const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("ast.zig");
const types = @import("types.zig");

/// State of a module in the compilation pipeline.
pub const ModuleState = enum {
    /// Module path discovered but not yet loaded.
    discovered,
    /// Source file is being parsed.
    parsing,
    /// AST is available.
    parsed,
    /// Type checking in progress.
    checking,
    /// Type checking complete.
    checked,
};

/// Information about a module in the compilation.
pub const ModuleInfo = struct {
    /// Module path segments (e.g., ["std", "io"]).
    path: []const []const u8,
    /// Absolute file path to the source.
    file_path: []const u8,
    /// Source code (null until loaded).
    source: ?[]const u8,
    /// Parsed AST (null until parsed).
    module_ast: ?ast.Module,
    /// Current state in the pipeline.
    state: ModuleState,
    /// Modules this module imports.
    dependencies: std.ArrayListUnmanaged(*ModuleInfo),
    /// Exported symbols (populated during type checking).
    exports: std.StringHashMapUnmanaged(ExportedSymbol),
    /// Whether this is the entry module.
    is_entry: bool,

    pub fn init(allocator: Allocator, path: []const []const u8, file_path: []const u8) !*ModuleInfo {
        const info = try allocator.create(ModuleInfo);
        info.* = .{
            .path = path,
            .file_path = file_path,
            .source = null,
            .module_ast = null,
            .state = .discovered,
            .dependencies = .{},
            .exports = .{},
            .is_entry = false,
        };
        return info;
    }

    pub fn deinit(self: *ModuleInfo, allocator: Allocator) void {
        self.dependencies.deinit(allocator);
        self.exports.deinit(allocator);
        // Note: source is NOT freed here - the caller is responsible for
        // keeping source alive until after codegen (AST contains slices into it)
    }

    /// Get the canonical module name (dot-separated path).
    pub fn canonicalName(self: *const ModuleInfo, allocator: Allocator) ![]const u8 {
        if (self.path.len == 0) return try allocator.dupe(u8, "");

        var total_len: usize = 0;
        for (self.path) |segment| {
            total_len += segment.len + 1; // +1 for dot
        }
        total_len -= 1; // Remove trailing dot

        const result = try allocator.alloc(u8, total_len);
        var pos: usize = 0;
        for (self.path, 0..) |segment, i| {
            @memcpy(result[pos..][0..segment.len], segment);
            pos += segment.len;
            if (i < self.path.len - 1) {
                result[pos] = '.';
                pos += 1;
            }
        }
        return result;
    }
};

/// An exported symbol from a module.
pub const ExportedSymbol = struct {
    name: []const u8,
    kind: Kind,
    type_: ?types.Type,
    /// The module that originally defines this symbol.
    source_module: *ModuleInfo,

    pub const Kind = enum {
        function,
        struct_type,
        enum_type,
        trait_type,
        type_alias,
        constant,
    };
};

/// Error information for module resolution.
pub const ResolveError = struct {
    kind: Kind,
    message: []const u8,
    span: ?ast.Span,
    module_path: ?[]const []const u8,

    pub const Kind = enum {
        module_not_found,
        circular_import,
        file_not_found,
        parse_error,
        visibility_error,
        ambiguous_import,
    };
};

/// Module resolver - discovers and manages modules for compilation.
pub const ModuleResolver = struct {
    allocator: Allocator,
    /// All discovered modules by canonical name.
    modules: std.StringHashMapUnmanaged(*ModuleInfo),
    /// Search paths for module resolution.
    search_paths: std.ArrayListUnmanaged([]const u8),
    /// Standard library path (null if not found).
    std_lib_path: ?[]const u8,
    /// Entry module (the main file being compiled).
    entry_module: ?*ModuleInfo,
    /// Resolution errors.
    errors: std.ArrayListUnmanaged(ResolveError),
    /// Arena for path allocations.
    arena: std.heap.ArenaAllocator,

    pub fn init(allocator: Allocator) ModuleResolver {
        return .{
            .allocator = allocator,
            .modules = .{},
            .search_paths = .{},
            .std_lib_path = null,
            .entry_module = null,
            .errors = .{},
            .arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    pub fn deinit(self: *ModuleResolver) void {
        var it = self.modules.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.*.deinit(self.allocator);
            self.allocator.destroy(entry.value_ptr.*);
        }
        self.modules.deinit(self.allocator);
        self.search_paths.deinit(self.allocator);
        self.errors.deinit(self.allocator);
        self.arena.deinit();
    }

    /// Add a search path for module resolution.
    pub fn addSearchPath(self: *ModuleResolver, path: []const u8) !void {
        try self.search_paths.append(self.allocator, path);
    }

    /// Set the standard library path.
    pub fn setStdLibPath(self: *ModuleResolver, path: []const u8) void {
        self.std_lib_path = path;
    }

    /// Resolve the entry module and discover all dependencies.
    pub fn resolveEntry(self: *ModuleResolver, entry_path: []const u8) !*ModuleInfo {
        // Create the entry module
        const abs_path = try self.makeAbsolutePath(entry_path);
        const entry = try self.createModule(&.{}, abs_path);
        entry.is_entry = true;
        self.entry_module = entry;

        // Add the entry file's directory as a search path
        if (std.fs.path.dirname(abs_path)) |dir| {
            try self.addSearchPath(dir);
        }

        return entry;
    }

    /// Resolve a module import path.
    /// Returns the ModuleInfo for the imported module.
    pub fn resolve(
        self: *ModuleResolver,
        path: []const []const u8,
        from: ?*ModuleInfo,
    ) !*ModuleInfo {
        // Build canonical name for lookup
        const canonical = try self.buildCanonicalName(path);

        // Check if already resolved
        if (self.modules.get(canonical)) |existing| {
            // Still add dependency even for already-resolved modules
            // This is needed for cycle detection in topological sort
            if (from) |f| {
                try f.dependencies.append(self.allocator, existing);
            }
            return existing;
        }

        // Try to find the file
        const file_path = try self.findModuleFile(path, from) orelse {
            try self.errors.append(self.allocator, .{
                .kind = .module_not_found,
                .message = try std.fmt.allocPrint(
                    self.arena.allocator(),
                    "module '{s}' not found",
                    .{canonical},
                ),
                .span = null,
                .module_path = path,
            });
            return error.ModuleNotFound;
        };

        // Create the module
        const module = try self.createModule(path, file_path);

        // Add dependency from importing module
        if (from) |f| {
            try f.dependencies.append(self.allocator, module);
        }

        return module;
    }

    /// Create a new module entry.
    fn createModule(self: *ModuleResolver, path: []const []const u8, file_path: []const u8) !*ModuleInfo {
        const canonical = try self.buildCanonicalName(path);

        // Check if already exists
        if (self.modules.get(canonical)) |existing| {
            return existing;
        }

        // Duplicate path segments
        const path_copy = try self.arena.allocator().alloc([]const u8, path.len);
        for (path, 0..) |segment, i| {
            path_copy[i] = try self.arena.allocator().dupe(u8, segment);
        }

        const module = try ModuleInfo.init(self.allocator, path_copy, file_path);
        try self.modules.put(self.allocator, canonical, module);

        return module;
    }

    /// Build a canonical name from path segments.
    fn buildCanonicalName(self: *ModuleResolver, path: []const []const u8) ![]const u8 {
        if (path.len == 0) return "";

        var total_len: usize = 0;
        for (path) |segment| {
            total_len += segment.len + 1;
        }
        total_len -= 1;

        const result = try self.arena.allocator().alloc(u8, total_len);
        var pos: usize = 0;
        for (path, 0..) |segment, i| {
            @memcpy(result[pos..][0..segment.len], segment);
            pos += segment.len;
            if (i < path.len - 1) {
                result[pos] = '.';
                pos += 1;
            }
        }
        return result;
    }

    /// Find the file for a module path.
    fn findModuleFile(
        self: *ModuleResolver,
        path: []const []const u8,
        from: ?*ModuleInfo,
    ) !?[]const u8 {
        if (path.len == 0) return null;

        const arena = self.arena.allocator();

        // Check if it's a relative import (starts with '.')
        // For now, treat imports starting with known prefixes specially
        const is_std_import = path.len > 0 and std.mem.eql(u8, path[0], "std");

        if (is_std_import) {
            // Look in standard library path
            if (self.std_lib_path) |std_path| {
                if (try self.tryFindFile(arena, std_path, path[1..])) |found| {
                    return found;
                }
            }
        }

        // Look relative to importing module first, then walk up directory tree
        if (from) |f| {
            var current_dir_buf: [std.fs.max_path_bytes]u8 = undefined;
            var current_dir_buf2: [std.fs.max_path_bytes]u8 = undefined;
            var use_buf1 = true;
            var current_dir: ?[]const u8 = std.fs.path.dirname(f.file_path);

            while (current_dir) |dir| {
                if (try self.tryFindFile(arena, dir, path)) |found| {
                    return found;
                }

                // Walk up to parent directory
                // Use alternating buffers to avoid aliasing issues with @memcpy
                if (dir.len < std.fs.max_path_bytes) {
                    if (use_buf1) {
                        @memcpy(current_dir_buf[0..dir.len], dir);
                        current_dir = std.fs.path.dirname(current_dir_buf[0..dir.len]);
                    } else {
                        @memcpy(current_dir_buf2[0..dir.len], dir);
                        current_dir = std.fs.path.dirname(current_dir_buf2[0..dir.len]);
                    }
                    use_buf1 = !use_buf1;
                } else {
                    break;
                }
            }
        }

        // Look in search paths
        for (self.search_paths.items) |search_path| {
            if (try self.tryFindFile(arena, search_path, path)) |found| {
                return found;
            }
        }

        return null;
    }

    /// Try to find a file at base_path + module_path.
    fn tryFindFile(
        self: *ModuleResolver,
        arena: Allocator,
        base_path: []const u8,
        path: []const []const u8,
    ) !?[]const u8 {
        _ = self;

        if (path.len == 0) return null;

        // Build the file path
        // First, build the directory portion (all but last segment)
        var components = std.ArrayListUnmanaged([]const u8){};
        defer components.deinit(arena);

        try components.append(arena, base_path);
        for (path[0 .. path.len - 1]) |segment| {
            try components.append(arena, segment);
        }

        const last_segment = path[path.len - 1];

        // Try: base/path/to/module.kl
        {
            var file_components = try components.clone(arena);
            defer file_components.deinit(arena);
            const filename = try std.fmt.allocPrint(arena, "{s}.kl", .{last_segment});
            try file_components.append(arena, filename);

            const full_path = try std.fs.path.join(arena, file_components.items);
            if (fileExists(full_path)) {
                return full_path;
            }
        }

        // Try: base/path/to/module/mod.kl
        {
            var dir_components = try components.clone(arena);
            defer dir_components.deinit(arena);
            try dir_components.append(arena, last_segment);
            try dir_components.append(arena, "mod.kl");

            const full_path = try std.fs.path.join(arena, dir_components.items);
            if (fileExists(full_path)) {
                return full_path;
            }
        }

        return null;
    }

    /// Make an absolute path from a potentially relative path.
    fn makeAbsolutePath(self: *ModuleResolver, path: []const u8) ![]const u8 {
        if (std.fs.path.isAbsolute(path)) {
            return try self.arena.allocator().dupe(u8, path);
        }

        const cwd = std.fs.cwd();
        const abs = try cwd.realpathAlloc(self.arena.allocator(), path);
        return abs;
    }

    /// Get modules in topological order for compilation.
    /// Earlier modules have no dependencies on later ones.
    pub fn getCompilationOrder(self: *ModuleResolver) ![]const *ModuleInfo {
        var order = std.ArrayListUnmanaged(*ModuleInfo){};
        var visited = std.StringHashMapUnmanaged(void){};
        var in_stack = std.StringHashMapUnmanaged(void){};

        defer visited.deinit(self.allocator);
        defer in_stack.deinit(self.allocator);

        // Start from entry module
        if (self.entry_module) |entry| {
            const canonical = try entry.canonicalName(self.arena.allocator());
            try self.topoSort(entry, canonical, &order, &visited, &in_stack);
        }

        return try order.toOwnedSlice(self.allocator);
    }

    /// Topological sort helper using DFS.
    fn topoSort(
        self: *ModuleResolver,
        module: *ModuleInfo,
        canonical: []const u8,
        order: *std.ArrayListUnmanaged(*ModuleInfo),
        visited: *std.StringHashMapUnmanaged(void),
        in_stack: *std.StringHashMapUnmanaged(void),
    ) !void {
        // Check for cycle
        if (in_stack.contains(canonical)) {
            try self.errors.append(self.allocator, .{
                .kind = .circular_import,
                .message = try std.fmt.allocPrint(
                    self.arena.allocator(),
                    "circular import detected involving '{s}'",
                    .{canonical},
                ),
                .span = null,
                .module_path = module.path,
            });
            return error.CircularImport;
        }

        // Already processed
        if (visited.contains(canonical)) {
            return;
        }

        try in_stack.put(self.allocator, canonical, {});

        // Visit dependencies first
        for (module.dependencies.items) |dep| {
            const dep_canonical = try dep.canonicalName(self.arena.allocator());
            try self.topoSort(dep, dep_canonical, order, visited, in_stack);
        }

        _ = in_stack.remove(canonical);
        try visited.put(self.allocator, canonical, {});
        try order.append(self.allocator, module);
    }

    /// Detect if there are any circular imports.
    /// Returns the cycle path if found, null otherwise.
    pub fn detectCycle(self: *ModuleResolver) !?[]*ModuleInfo {
        var visited = std.StringHashMapUnmanaged(void){};
        var in_stack = std.StringHashMapUnmanaged(void){};
        var stack = std.ArrayListUnmanaged(*ModuleInfo){};

        defer visited.deinit(self.allocator);
        defer in_stack.deinit(self.allocator);
        defer stack.deinit(self.allocator);

        if (self.entry_module) |entry| {
            const canonical = try entry.canonicalName(self.arena.allocator());
            if (try self.detectCycleDFS(entry, canonical, &visited, &in_stack, &stack)) {
                return try stack.toOwnedSlice(self.allocator);
            }
        }

        return null;
    }

    fn detectCycleDFS(
        self: *ModuleResolver,
        module: *ModuleInfo,
        canonical: []const u8,
        visited: *std.StringHashMapUnmanaged(void),
        in_stack: *std.StringHashMapUnmanaged(void),
        stack: *std.ArrayListUnmanaged(*ModuleInfo),
    ) !bool {
        if (in_stack.contains(canonical)) {
            return true; // Cycle found
        }

        if (visited.contains(canonical)) {
            return false;
        }

        try visited.put(self.allocator, canonical, {});
        try in_stack.put(self.allocator, canonical, {});
        try stack.append(self.allocator, module);

        for (module.dependencies.items) |dep| {
            const dep_canonical = try dep.canonicalName(self.arena.allocator());
            if (try self.detectCycleDFS(dep, dep_canonical, visited, in_stack, stack)) {
                return true;
            }
        }

        _ = in_stack.remove(canonical);
        _ = stack.pop();
        return false;
    }

    /// Check if there are any resolution errors.
    pub fn hasErrors(self: *const ModuleResolver) bool {
        return self.errors.items.len > 0;
    }
};

/// Check if a file exists.
fn fileExists(path: []const u8) bool {
    std.fs.cwd().access(path, .{}) catch return false;
    return true;
}

// ============================================================================
// Tests
// ============================================================================

test "ModuleInfo canonical name" {
    const allocator = std.testing.allocator;

    const path = [_][]const u8{ "std", "io" };
    const info = try ModuleInfo.init(allocator, &path, "/test/std/io.kl");
    defer {
        var m = info;
        m.deinit(allocator);
        allocator.destroy(m);
    }

    const name = try info.canonicalName(allocator);
    defer allocator.free(name);

    try std.testing.expectEqualStrings("std.io", name);
}

test "ModuleResolver initialization" {
    const allocator = std.testing.allocator;

    var resolver = ModuleResolver.init(allocator);
    defer resolver.deinit();

    try resolver.addSearchPath("/test/lib");
    try std.testing.expectEqual(@as(usize, 1), resolver.search_paths.items.len);
}

test "ModuleResolver build canonical name" {
    const allocator = std.testing.allocator;

    var resolver = ModuleResolver.init(allocator);
    defer resolver.deinit();

    const path = [_][]const u8{ "utils", "math" };
    const canonical = try resolver.buildCanonicalName(&path);

    try std.testing.expectEqualStrings("utils.math", canonical);
}
