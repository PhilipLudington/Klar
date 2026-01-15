//! # MyLib
//!
//! Brief description of what this library does.
//!
//! ## Features
//! - Feature 1
//! - Feature 2
//!
//! ## Example
//! ```zig
//! const mylib = @import("mylib");
//! var widget = try mylib.Widget.init(allocator);
//! defer widget.deinit();
//! ```

const std = @import("std");
const Allocator = std.mem.Allocator;

// Re-export public API
pub const Widget = @import("lib/widget.zig").Widget;

/// Library version.
pub const version = std.SemanticVersion{
    .major = 0,
    .minor = 1,
    .patch = 0,
};

/// Initialize the library with default configuration.
pub fn init() void {
    // Library initialization if needed
}

test {
    // Run all tests in imported modules
    std.testing.refAllDecls(@This());
}
