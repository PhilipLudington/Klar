/// Single source of truth for Klar version information.
/// Update this file when bumping versions.

pub const major = 0;
pub const minor = 3;
pub const patch = 1;
pub const pre_release = "dev";

/// Full version string: "0.3.1-dev" or "0.3.1" if no pre-release
pub const string = if (pre_release.len > 0)
    std.fmt.comptimePrint("{d}.{d}.{d}-{s}", .{ major, minor, patch, pre_release })
else
    std.fmt.comptimePrint("{d}.{d}.{d}", .{ major, minor, patch });

/// For display: "Klar 0.3.1-dev"
pub const display = "Klar " ++ string;

/// For REPL banner: "Klar REPL v0.3.1-dev"
pub const repl_banner = "Klar REPL v" ++ string;

/// For LLVM/debug info producer: "Klar Compiler 0.3.1"
/// Note: Excludes pre-release suffix for stable debug info
pub const producer = std.fmt.comptimePrint("Klar Compiler {d}.{d}.{d}", .{ major, minor, patch });

const std = @import("std");
