//! Statement helper utilities for codegen.
//!
//! Provides structs and enums for statement and control-flow emission.
//! The emission implementation (emitStmt, emitIfStmt, emitForLoop, etc.)
//! remains in emit.zig.
//!
//! ## Provided by this module
//!
//! - `LoopContext`: Break/continue target block pair for nested loops
//! - `PatternKind`: Enum for match-statement pattern dispatch
//! - `getPatternKind`: Map AST pattern tag to PatternKind

const std = @import("std");
const llvm = @import("llvm.zig");

/// Loop context for break/continue targets.
pub const LoopContext = struct {
    continue_block: llvm.BasicBlockRef,
    break_block: llvm.BasicBlockRef,
};

/// Pattern kind for match statement dispatch.
pub const PatternKind = enum {
    wildcard, // _
    literal, // 42, "hello", true
    identifier, // x (binding)
    variant, // Some(x), None
    tuple, // (a, b)
    struct_, // { x, y }
};

/// Determine pattern kind from AST pattern.
pub fn getPatternKind(tag: u8) PatternKind {
    // This maps AST pattern tags to our enum
    return switch (tag) {
        0 => .wildcard,
        1 => .literal,
        2 => .identifier,
        3 => .variant,
        4 => .tuple,
        5 => .struct_,
        else => .wildcard,
    };
}
