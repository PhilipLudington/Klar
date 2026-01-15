const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("../ast.zig");
const Span = ast.Span;
const types = @import("../types.zig");
const Type = types.Type;

/// Represents the ownership state of a variable at a given point in the program.
pub const OwnershipState = enum {
    /// Variable owns the value and can use it freely.
    owned,
    /// Ownership has been transferred to another variable.
    /// The variable can no longer be used.
    moved,
    /// Value is temporarily borrowed immutably.
    /// Multiple immutable borrows can coexist.
    borrowed,
    /// Value is temporarily borrowed mutably (exclusive access).
    /// No other borrows can coexist with a mutable borrow.
    borrowed_mut,
    /// Some fields of a struct have been moved out.
    /// The struct as a whole cannot be used, but unmoved fields can.
    partially_moved,
};

/// The kind of borrow: immutable or mutable.
pub const BorrowKind = enum {
    immutable,
    mutable,

    pub fn format(
        self: BorrowKind,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .immutable => try writer.writeAll("&"),
            .mutable => try writer.writeAll("&mut"),
        }
    }
};

/// Information about an active borrow.
pub const BorrowInfo = struct {
    /// Whether this is an immutable or mutable borrow.
    kind: BorrowKind,
    /// Source location where the borrow was created.
    span: Span,
    /// Scope depth when the borrow was created.
    /// Used to invalidate borrows when their scope ends.
    scope_depth: u32,
    /// The expression that created this borrow (for error messages).
    borrow_expr: ?*const ast.Expr,

    pub fn format(
        self: BorrowInfo,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{} at {}:{} (scope {})", .{
            self.kind,
            self.span.line,
            self.span.column,
            self.scope_depth,
        });
    }
};

/// Tracks the complete ownership state of a variable.
pub const VariableState = struct {
    /// Current ownership state.
    state: OwnershipState,
    /// Source location where the variable was declared.
    decl_span: Span,
    /// Source location where the value was moved (if state is .moved).
    move_span: ?Span,
    /// Active borrows of this variable.
    borrows: std.ArrayListUnmanaged(BorrowInfo),
    /// Whether this type implements Copy (cached for performance).
    is_copy: bool,
    /// The type of this variable.
    ty: Type,
    /// Name of the variable (for error messages).
    name: []const u8,

    pub fn init(name: []const u8, ty: Type, decl_span: Span, is_copy: bool) VariableState {
        return .{
            .state = .owned,
            .decl_span = decl_span,
            .move_span = null,
            .borrows = .{},
            .is_copy = is_copy,
            .ty = ty,
            .name = name,
        };
    }

    pub fn deinit(self: *VariableState, allocator: Allocator) void {
        self.borrows.deinit(allocator);
    }

    /// Check if the variable can be read (used in an expression).
    pub fn canRead(self: *const VariableState) bool {
        return switch (self.state) {
            .owned, .borrowed, .borrowed_mut => true,
            .moved, .partially_moved => false,
        };
    }

    /// Check if the variable can be written to.
    pub fn canWrite(self: *const VariableState) bool {
        return switch (self.state) {
            .owned => true,
            .borrowed, .borrowed_mut, .moved, .partially_moved => false,
        };
    }

    /// Check if an immutable borrow can be created.
    pub fn canBorrowImmutable(self: *const VariableState) bool {
        if (self.state == .moved or self.state == .partially_moved) {
            return false;
        }
        // Cannot create immutable borrow while mutable borrow exists
        for (self.borrows.items) |borrow| {
            if (borrow.kind == .mutable) {
                return false;
            }
        }
        return true;
    }

    /// Check if a mutable borrow can be created.
    pub fn canBorrowMutable(self: *const VariableState) bool {
        if (self.state == .moved or self.state == .partially_moved) {
            return false;
        }
        // Cannot create mutable borrow while any borrow exists
        return self.borrows.items.len == 0;
    }

    /// Get the first conflicting borrow (for error messages).
    pub fn getConflictingBorrow(self: *const VariableState, requested: BorrowKind) ?BorrowInfo {
        for (self.borrows.items) |borrow| {
            if (requested == .mutable) {
                // Any existing borrow conflicts with mutable
                return borrow;
            } else {
                // Only mutable borrows conflict with immutable
                if (borrow.kind == .mutable) {
                    return borrow;
                }
            }
        }
        return null;
    }

    /// Add a new borrow.
    pub fn addBorrow(self: *VariableState, allocator: Allocator, info: BorrowInfo) !void {
        try self.borrows.append(allocator, info);
        // Update state based on borrow kind
        if (info.kind == .mutable) {
            self.state = .borrowed_mut;
        } else if (self.state == .owned) {
            self.state = .borrowed;
        }
    }

    /// Remove borrows that are no longer valid (scope ended).
    pub fn invalidateBorrowsAtDepth(self: *VariableState, scope_depth: u32) void {
        // Remove borrows created at or below the given scope depth
        var i: usize = 0;
        while (i < self.borrows.items.len) {
            if (self.borrows.items[i].scope_depth >= scope_depth) {
                _ = self.borrows.swapRemove(i);
            } else {
                i += 1;
            }
        }
        // Update state if no borrows remain
        if (self.borrows.items.len == 0 and
            (self.state == .borrowed or self.state == .borrowed_mut))
        {
            self.state = .owned;
        }
    }

    /// Mark the variable as moved.
    pub fn markMoved(self: *VariableState, move_span: Span) void {
        self.state = .moved;
        self.move_span = move_span;
    }

    /// Format for debugging.
    pub fn format(
        self: *const VariableState,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{s}: {} ({})", .{
            self.name,
            self.state,
            if (self.is_copy) "Copy" else "Move",
        });
        if (self.state == .moved) {
            if (self.move_span) |span| {
                try writer.print(" [moved at {}:{}]", .{ span.line, span.column });
            }
        }
        if (self.borrows.items.len > 0) {
            try writer.writeAll(" borrows: [");
            for (self.borrows.items, 0..) |borrow, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.print("{}", .{borrow});
            }
            try writer.writeAll("]");
        }
    }
};

/// Scope for tracking variable ownership within a lexical scope.
pub const OwnershipScope = struct {
    /// Variables declared in this scope.
    variables: std.StringHashMapUnmanaged(VariableState),
    /// Parent scope (if any).
    parent: ?*OwnershipScope,
    /// Depth of this scope (0 = global).
    depth: u32,
    /// Kind of scope (for special handling of loops, functions).
    kind: Kind,
    /// Allocator for this scope.
    allocator: Allocator,

    pub const Kind = enum {
        global,
        function,
        block,
        loop,
        conditional,
    };

    pub fn init(allocator: Allocator, parent: ?*OwnershipScope, kind: Kind) OwnershipScope {
        const depth = if (parent) |p| p.depth + 1 else 0;
        return .{
            .variables = .{},
            .parent = parent,
            .depth = depth,
            .kind = kind,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *OwnershipScope) void {
        var it = self.variables.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
        }
        self.variables.deinit(self.allocator);
    }

    /// Define a new variable in this scope.
    pub fn define(self: *OwnershipScope, name: []const u8, state: VariableState) !void {
        try self.variables.put(self.allocator, name, state);
    }

    /// Look up a variable, searching parent scopes if not found locally.
    pub fn lookup(self: *OwnershipScope, name: []const u8) ?*VariableState {
        if (self.variables.getPtr(name)) |state| {
            return state;
        }
        if (self.parent) |parent| {
            return parent.lookup(name);
        }
        return null;
    }

    /// Look up a variable only in this scope (not parent scopes).
    pub fn lookupLocal(self: *OwnershipScope, name: []const u8) ?*VariableState {
        return self.variables.getPtr(name);
    }

    /// Get all variables that need to be dropped when this scope exits.
    /// Returns variables that are still owned (not moved) and not Copy types.
    pub fn getDroppableVariables(self: *OwnershipScope, allocator: Allocator) ![]const []const u8 {
        var result = std.ArrayListUnmanaged([]const u8){};
        var it = self.variables.iterator();
        while (it.next()) |entry| {
            const state = entry.value_ptr;
            if (state.state == .owned and !state.is_copy) {
                try result.append(allocator, entry.key_ptr.*);
            }
        }
        return result.toOwnedSlice(allocator);
    }

    /// Check if this scope or any parent is a loop scope.
    pub fn isInLoop(self: *const OwnershipScope) bool {
        if (self.kind == .loop) return true;
        if (self.parent) |parent| {
            return parent.isInLoop();
        }
        return false;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "VariableState basic operations" {
    const allocator = std.testing.allocator;
    var state = VariableState.init("x", .{ .primitive = .i32 }, .{ .line = 1, .column = 1, .offset = 0 }, false);
    defer state.deinit(allocator);

    // Initially owned
    try std.testing.expect(state.canRead());
    try std.testing.expect(state.canWrite());
    try std.testing.expect(state.canBorrowImmutable());
    try std.testing.expect(state.canBorrowMutable());

    // Add immutable borrow
    try state.addBorrow(allocator, .{
        .kind = .immutable,
        .span = .{ .line = 2, .column = 1, .offset = 0 },
        .scope_depth = 1,
        .borrow_expr = null,
    });

    try std.testing.expect(state.canRead());
    try std.testing.expect(!state.canWrite());
    try std.testing.expect(state.canBorrowImmutable()); // Multiple immutable OK
    try std.testing.expect(!state.canBorrowMutable()); // Mutable conflicts

    // Invalidate borrow
    state.invalidateBorrowsAtDepth(1);
    try std.testing.expect(state.canBorrowMutable()); // Now mutable OK
}

test "VariableState move tracking" {
    const allocator = std.testing.allocator;
    var state = VariableState.init("buf", .{ .primitive = .i32 }, .{ .line = 1, .column = 1, .offset = 0 }, false);
    defer state.deinit(allocator);

    try std.testing.expect(state.canRead());

    state.markMoved(.{ .line = 5, .column = 10, .offset = 0 });

    try std.testing.expect(!state.canRead());
    try std.testing.expect(!state.canBorrowImmutable());
    try std.testing.expect(!state.canBorrowMutable());
    try std.testing.expectEqual(state.move_span.?.line, 5);
}

test "OwnershipScope hierarchy" {
    const allocator = std.testing.allocator;

    var global = OwnershipScope.init(allocator, null, .global);
    defer global.deinit();

    try global.define("x", VariableState.init("x", .{ .primitive = .i32 }, .{ .line = 1, .column = 1, .offset = 0 }, true));

    var block = OwnershipScope.init(allocator, &global, .block);
    defer block.deinit();

    try block.define("y", VariableState.init("y", .{ .primitive = .i32 }, .{ .line = 2, .column = 1, .offset = 0 }, false));

    // Can find y in block
    try std.testing.expect(block.lookupLocal("y") != null);
    // Can find x in parent
    try std.testing.expect(block.lookup("x") != null);
    // Cannot find y in global
    try std.testing.expect(global.lookupLocal("y") == null);

    // Check scope depth
    try std.testing.expectEqual(global.depth, 0);
    try std.testing.expectEqual(block.depth, 1);
}

test "OwnershipScope droppable variables" {
    const allocator = std.testing.allocator;

    var scope = OwnershipScope.init(allocator, null, .block);
    defer scope.deinit();

    // Copy type - not droppable
    try scope.define("a", VariableState.init("a", .{ .primitive = .i32 }, .{ .line = 1, .column = 1, .offset = 0 }, true));

    // Non-copy type - droppable
    try scope.define("b", VariableState.init("b", .{ .primitive = .i32 }, .{ .line = 2, .column = 1, .offset = 0 }, false));

    // Moved - not droppable
    var moved_state = VariableState.init("c", .{ .primitive = .i32 }, .{ .line = 3, .column = 1, .offset = 0 }, false);
    moved_state.markMoved(.{ .line = 4, .column = 1, .offset = 0 });
    try scope.define("c", moved_state);

    const droppable = try scope.getDroppableVariables(allocator);
    defer allocator.free(droppable);

    try std.testing.expectEqual(droppable.len, 1);
    try std.testing.expectEqualStrings(droppable[0], "b");
}
