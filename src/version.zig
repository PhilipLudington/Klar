/// Single source of truth for Klar version information.
/// Update this file when bumping versions.

pub const major = 0;
pub const minor = 4;
pub const patch = 0;
pub const pre_release = "";

/// Full version string: "0.4.0-dev" or "0.4.0" if no pre-release
pub const string = if (pre_release.len > 0)
    std.fmt.comptimePrint("{d}.{d}.{d}-{s}", .{ major, minor, patch, pre_release })
else
    std.fmt.comptimePrint("{d}.{d}.{d}", .{ major, minor, patch });

/// For display: "Klar 0.4.0"
pub const display = "Klar " ++ string;

/// For REPL banner: "Klar REPL v0.4.0"
pub const repl_banner = "Klar REPL v" ++ string;

/// For LLVM/debug info producer: "Klar Compiler 0.4.0"
/// Note: Excludes pre-release suffix for stable debug info
pub const producer = std.fmt.comptimePrint("Klar Compiler {d}.{d}.{d}", .{ major, minor, patch });

const std = @import("std");
