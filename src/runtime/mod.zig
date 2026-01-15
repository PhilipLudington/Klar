// ============================================================================
// Klar Runtime Library
// ============================================================================
//
// This module provides runtime support for Klar native executables.
// It includes reference counting (Rc/Arc), memory allocation, and panic handling.

const std = @import("std");

pub const rc = @import("rc.zig");
pub const alloc = @import("alloc.zig");

// Re-export commonly used functions for C-compatible FFI
pub const klar_rc_alloc = rc.klar_rc_alloc;
pub const klar_rc_clone = rc.klar_rc_clone;
pub const klar_rc_drop = rc.klar_rc_drop;
pub const klar_weak_clone = rc.klar_weak_clone;
pub const klar_weak_drop = rc.klar_weak_drop;
pub const klar_weak_upgrade = rc.klar_weak_upgrade;
pub const klar_rc_downgrade = rc.klar_rc_downgrade;

test {
    _ = rc;
    _ = alloc;
}
