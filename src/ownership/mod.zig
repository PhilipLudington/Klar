const std = @import("std");

pub const state = @import("state.zig");
pub const checker = @import("checker.zig");
pub const drop = @import("drop.zig");

// Re-export main types for convenience
pub const OwnershipState = state.OwnershipState;
pub const BorrowKind = state.BorrowKind;
pub const BorrowInfo = state.BorrowInfo;
pub const VariableState = state.VariableState;
pub const OwnershipChecker = checker.OwnershipChecker;
pub const OwnershipError = checker.OwnershipError;
pub const DropInserter = drop.DropInserter;
pub const DropPoint = drop.DropPoint;

test {
    std.testing.refAllDecls(@This());
}
