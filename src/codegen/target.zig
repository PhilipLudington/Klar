//! Target platform configuration.
//!
//! Provides target triple generation and platform-specific settings.

const std = @import("std");
const builtin = @import("builtin");
const llvm = @import("llvm.zig");

/// Get the default target triple for the host platform.
pub fn getDefaultTriple() [:0]const u8 {
    return switch (builtin.os.tag) {
        .macos => switch (builtin.cpu.arch) {
            .aarch64 => "arm64-apple-macosx",
            .x86_64 => "x86_64-apple-macosx",
            else => "unknown-apple-macosx",
        },
        .linux => switch (builtin.cpu.arch) {
            .x86_64 => "x86_64-unknown-linux-gnu",
            .aarch64 => "aarch64-unknown-linux-gnu",
            else => "unknown-unknown-linux-gnu",
        },
        .windows => switch (builtin.cpu.arch) {
            .x86_64 => "x86_64-pc-windows-msvc",
            .aarch64 => "aarch64-pc-windows-msvc",
            else => "unknown-pc-windows-msvc",
        },
        else => "unknown-unknown-unknown",
    };
}

/// Get default CPU name for optimization.
pub fn getDefaultCPU() [:0]const u8 {
    return switch (builtin.cpu.arch) {
        .aarch64 => switch (builtin.os.tag) {
            .macos => "apple-m1",
            else => "generic",
        },
        .x86_64 => "x86-64",
        else => "generic",
    };
}

/// Get default CPU features.
pub fn getDefaultFeatures() [:0]const u8 {
    return "";
}

/// Platform information for linker and code generation.
pub const Platform = struct {
    os: std.Target.Os.Tag,
    arch: std.Target.Cpu.Arch,

    /// Get the current host platform.
    pub fn current() Platform {
        return .{
            .os = builtin.os.tag,
            .arch = builtin.cpu.arch,
        };
    }

    /// Get the path to the system linker.
    pub fn getLinkerPath(self: Platform) []const u8 {
        return switch (self.os) {
            .macos => "/usr/bin/ld",
            .linux => "/usr/bin/ld",
            .windows => "link.exe",
            else => "ld",
        };
    }

    /// Get the object file extension for this platform.
    pub fn getObjectExtension(self: Platform) []const u8 {
        return switch (self.os) {
            .windows => ".obj",
            else => ".o",
        };
    }

    /// Get the executable file extension for this platform.
    pub fn getExecutableExtension(self: Platform) []const u8 {
        return switch (self.os) {
            .windows => ".exe",
            else => "",
        };
    }

    /// Get the architecture name for the linker.
    pub fn getLinkerArch(self: Platform) []const u8 {
        return switch (self.os) {
            .macos => switch (self.arch) {
                .aarch64 => "arm64",
                .x86_64 => "x86_64",
                else => "unknown",
            },
            else => switch (self.arch) {
                .aarch64 => "aarch64",
                .x86_64 => "x86_64",
                else => "unknown",
            },
        };
    }

    /// Check if this platform requires position-independent code.
    pub fn requiresPIC(self: Platform) bool {
        return switch (self.os) {
            .macos => true,
            .linux => false, // PIE is common but not strictly required for executables
            else => false,
        };
    }
};

/// LLVM optimization level mapping.
pub const OptLevel = enum {
    none, // -O0
    less, // -O1
    default_, // -O2
    aggressive, // -O3

    pub fn toLLVM(self: OptLevel) llvm.c.LLVMCodeGenOptLevel {
        return switch (self) {
            .none => llvm.c.LLVMCodeGenLevelNone,
            .less => llvm.c.LLVMCodeGenLevelLess,
            .default_ => llvm.c.LLVMCodeGenLevelDefault,
            .aggressive => llvm.c.LLVMCodeGenLevelAggressive,
        };
    }
};

/// LLVM relocation mode.
pub const RelocMode = enum {
    default_,
    static,
    pic,
    dynamic_no_pic,

    pub fn toLLVM(self: RelocMode) llvm.c.LLVMRelocMode {
        return switch (self) {
            .default_ => llvm.c.LLVMRelocDefault,
            .static => llvm.c.LLVMRelocStatic,
            .pic => llvm.c.LLVMRelocPIC,
            .dynamic_no_pic => llvm.c.LLVMRelocDynamicNoPic,
        };
    }
};

/// LLVM code model.
pub const CodeModel = enum {
    default_,
    small,
    medium,
    large,

    pub fn toLLVM(self: CodeModel) llvm.c.LLVMCodeModel {
        return switch (self) {
            .default_ => llvm.c.LLVMCodeModelDefault,
            .small => llvm.c.LLVMCodeModelSmall,
            .medium => llvm.c.LLVMCodeModelMedium,
            .large => llvm.c.LLVMCodeModelLarge,
        };
    }
};
