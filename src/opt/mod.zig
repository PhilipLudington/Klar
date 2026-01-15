//! Klar IR Optimization Passes
//!
//! This module provides optimization passes that operate on the Klar IR
//! before it is lowered to LLVM IR. The passes are designed to be composable
//! and can be configured via optimization levels.
//!
//! ## Available Passes
//!
//! - `constfold` - Constant folding and propagation
//! - `dce` - Dead code elimination
//! - `simplify` - Instruction simplification
//!
//! ## Optimization Levels
//!
//! - `-O0` - No optimizations (default)
//! - `-O1` - Basic optimizations (constant folding, dead code elimination)
//! - `-O2` - Standard optimizations (all -O1 + simplification)
//! - `-O3` - Aggressive optimizations (all -O2 + LLVM aggressive opts)
//!
//! ## Usage
//!
//! ```zig
//! const opt = @import("opt/mod.zig");
//!
//! var pm = opt.PassManager.init(allocator);
//! defer pm.deinit();
//!
//! // Run optimization passes on a module
//! try pm.run(&module, .O2);
//! ```

const std = @import("std");
const Allocator = std.mem.Allocator;
const ir = @import("../ir/mod.zig");

// Re-export pass implementations
pub const constfold = @import("constfold.zig");
pub const dce = @import("dce.zig");
pub const simplify = @import("simplify.zig");

/// Optimization level.
pub const OptLevel = enum {
    /// No optimizations.
    O0,
    /// Basic optimizations: constant folding, dead code elimination.
    O1,
    /// Standard optimizations: all O1 + instruction simplification.
    O2,
    /// Aggressive optimizations: all O2 + LLVM aggressive optimizations.
    O3,

    /// Parse optimization level from string.
    pub fn fromString(s: []const u8) ?OptLevel {
        if (std.mem.eql(u8, s, "-O0") or std.mem.eql(u8, s, "0")) return .O0;
        if (std.mem.eql(u8, s, "-O1") or std.mem.eql(u8, s, "1")) return .O1;
        if (std.mem.eql(u8, s, "-O2") or std.mem.eql(u8, s, "2")) return .O2;
        if (std.mem.eql(u8, s, "-O3") or std.mem.eql(u8, s, "3")) return .O3;
        return null;
    }

    /// Convert to string for display.
    pub fn toString(self: OptLevel) []const u8 {
        return switch (self) {
            .O0 => "-O0",
            .O1 => "-O1",
            .O2 => "-O2",
            .O3 => "-O3",
        };
    }
};

/// Pass execution statistics.
pub const PassStats = struct {
    /// Number of instructions removed.
    instructions_removed: usize = 0,
    /// Number of instructions simplified.
    instructions_simplified: usize = 0,
    /// Number of constants folded.
    constants_folded: usize = 0,
    /// Number of dead blocks removed.
    blocks_removed: usize = 0,

    /// Merge stats from another PassStats.
    pub fn merge(self: *PassStats, other: PassStats) void {
        self.instructions_removed += other.instructions_removed;
        self.instructions_simplified += other.instructions_simplified;
        self.constants_folded += other.constants_folded;
        self.blocks_removed += other.blocks_removed;
    }

    /// Check if any optimizations were applied.
    pub fn hasChanges(self: PassStats) bool {
        return self.instructions_removed > 0 or
            self.instructions_simplified > 0 or
            self.constants_folded > 0 or
            self.blocks_removed > 0;
    }
};

/// Pass manager for running optimization passes on IR modules.
pub const PassManager = struct {
    allocator: Allocator,
    /// Enable verbose output.
    verbose: bool,
    /// Total statistics across all passes.
    total_stats: PassStats,

    pub fn init(allocator: Allocator) PassManager {
        return .{
            .allocator = allocator,
            .verbose = false,
            .total_stats = .{},
        };
    }

    pub fn deinit(self: *PassManager) void {
        _ = self;
        // No cleanup needed currently
    }

    /// Run optimization passes on a module based on optimization level.
    pub fn run(self: *PassManager, module: *ir.Module, level: OptLevel) !void {
        if (level == .O0) {
            // No optimizations
            return;
        }

        // Reset stats
        self.total_stats = .{};

        // Run passes in order based on optimization level
        // We iterate multiple times until no more changes are made (fixed point)
        var changed = true;
        var iterations: usize = 0;
        const max_iterations = 10;

        while (changed and iterations < max_iterations) {
            changed = false;
            iterations += 1;

            // -O1 and above: constant folding
            if (@intFromEnum(level) >= @intFromEnum(OptLevel.O1)) {
                const stats = try constfold.run(self.allocator, module);
                self.total_stats.merge(stats);
                if (stats.hasChanges()) changed = true;
            }

            // -O1 and above: dead code elimination
            if (@intFromEnum(level) >= @intFromEnum(OptLevel.O1)) {
                const stats = try dce.run(self.allocator, module);
                self.total_stats.merge(stats);
                if (stats.hasChanges()) changed = true;
            }

            // -O2 and above: instruction simplification
            if (@intFromEnum(level) >= @intFromEnum(OptLevel.O2)) {
                const stats = try simplify.run(self.allocator, module);
                self.total_stats.merge(stats);
                if (stats.hasChanges()) changed = true;
            }
        }

        if (self.verbose and self.total_stats.hasChanges()) {
            std.debug.print("Optimization stats (after {d} iterations):\n", .{iterations});
            std.debug.print("  Constants folded: {d}\n", .{self.total_stats.constants_folded});
            std.debug.print("  Instructions simplified: {d}\n", .{self.total_stats.instructions_simplified});
            std.debug.print("  Instructions removed: {d}\n", .{self.total_stats.instructions_removed});
            std.debug.print("  Blocks removed: {d}\n", .{self.total_stats.blocks_removed});
        }
    }

    /// Run a single pass on a module.
    pub fn runPass(self: *PassManager, module: *ir.Module, comptime pass: type) !PassStats {
        const stats = try pass.run(self.allocator, module);
        self.total_stats.merge(stats);
        return stats;
    }

    /// Get total statistics.
    pub fn getStats(self: *const PassManager) PassStats {
        return self.total_stats;
    }

    /// Set verbose mode.
    pub fn setVerbose(self: *PassManager, verbose: bool) void {
        self.verbose = verbose;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "OptLevel parsing" {
    const testing = std.testing;

    try testing.expectEqual(OptLevel.O0, OptLevel.fromString("-O0").?);
    try testing.expectEqual(OptLevel.O1, OptLevel.fromString("-O1").?);
    try testing.expectEqual(OptLevel.O2, OptLevel.fromString("-O2").?);
    try testing.expectEqual(OptLevel.O3, OptLevel.fromString("-O3").?);
    try testing.expectEqual(OptLevel.O0, OptLevel.fromString("0").?);
    try testing.expect(OptLevel.fromString("-O4") == null);
    try testing.expect(OptLevel.fromString("invalid") == null);
}

test "PassStats merging" {
    var stats1 = PassStats{
        .instructions_removed = 5,
        .constants_folded = 3,
    };

    const stats2 = PassStats{
        .instructions_removed = 2,
        .instructions_simplified = 4,
    };

    stats1.merge(stats2);

    try std.testing.expectEqual(@as(usize, 7), stats1.instructions_removed);
    try std.testing.expectEqual(@as(usize, 3), stats1.constants_folded);
    try std.testing.expectEqual(@as(usize, 4), stats1.instructions_simplified);
}

test "PassManager initialization" {
    const testing = std.testing;
    var pm = PassManager.init(testing.allocator);
    defer pm.deinit();

    try testing.expect(!pm.verbose);
    try testing.expect(!pm.total_stats.hasChanges());
}

test {
    _ = constfold;
    _ = dce;
    _ = simplify;
}
