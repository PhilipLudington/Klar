// ============================================================================
// Klar Runtime Library
// ============================================================================
//
// This module provides runtime support for Klar native executables.
// It includes reference counting (Rc/Arc), memory allocation, and panic handling.

const std = @import("std");

pub const rc = @import("rc.zig");
pub const arc = @import("arc.zig");
pub const alloc = @import("alloc.zig");
pub const string = @import("string.zig");

// Re-export commonly used functions for C-compatible FFI

// Rc (non-thread-safe reference counting)
pub const klar_rc_alloc = rc.klar_rc_alloc;
pub const klar_rc_clone = rc.klar_rc_clone;
pub const klar_rc_drop = rc.klar_rc_drop;
pub const klar_weak_clone = rc.klar_weak_clone;
pub const klar_weak_drop = rc.klar_weak_drop;
pub const klar_weak_upgrade = rc.klar_weak_upgrade;
pub const klar_rc_downgrade = rc.klar_rc_downgrade;

// Arc (thread-safe atomic reference counting)
pub const klar_arc_alloc = arc.klar_arc_alloc;
pub const klar_arc_clone = arc.klar_arc_clone;
pub const klar_arc_drop = arc.klar_arc_drop;
pub const klar_weak_arc_clone = arc.klar_weak_arc_clone;
pub const klar_weak_arc_drop = arc.klar_weak_arc_drop;
pub const klar_weak_arc_upgrade = arc.klar_weak_arc_upgrade;
pub const klar_arc_downgrade = arc.klar_arc_downgrade;

// String operations
pub const klar_string_trim = string.klar_string_trim;
pub const klar_string_to_uppercase = string.klar_string_to_uppercase;
pub const klar_string_to_lowercase = string.klar_string_to_lowercase;

test {
    _ = rc;
    _ = arc;
    _ = alloc;
    _ = string;
}
