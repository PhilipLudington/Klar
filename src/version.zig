/// Klar version information.
/// The version is written once, in `build.zig.zon`; `build.zig` passes it in as
/// `build_options.version`. Bump it there.
pub const major = semver.major;
pub const minor = semver.minor;
pub const patch = semver.patch;
pub const pre_release = semver.pre orelse "";

const semver = std.SemanticVersion.parse(build_options.version) catch
    @compileError("build.zig.zon .version is not a semantic version: " ++ build_options.version);

/// Full version string: "0.6.0-dev" or "0.6.0" if no pre-release
pub const string = if (pre_release.len > 0)
    std.fmt.comptimePrint("{d}.{d}.{d}-{s}", .{ major, minor, patch, pre_release })
else
    std.fmt.comptimePrint("{d}.{d}.{d}", .{ major, minor, patch });

/// For display: "Klar 0.6.0"
pub const display = "Klar " ++ string;

/// For REPL banner: "Klar REPL v0.6.0"
pub const repl_banner = "Klar REPL v" ++ string;

/// For LLVM/debug info producer: "Klar Compiler 0.6.0"
/// Note: Excludes pre-release suffix for stable debug info
pub const producer = std.fmt.comptimePrint("Klar Compiler {d}.{d}.{d}", .{ major, minor, patch });

const std = @import("std");
const build_options = @import("build_options");
