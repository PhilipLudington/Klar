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

    /// Get the pointer size in bytes for this platform.
    pub fn getPointerSize(self: Platform) usize {
        return switch (self.arch) {
            .x86_64, .aarch64 => 8, // 64-bit architectures
            .x86, .arm => 4, // 32-bit architectures
            else => 8, // Default to 64-bit
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

/// Calling convention for the platform.
/// LLVM calling conventions are defined in llvm-c/Core.h
pub const CallingConvention = enum(c_uint) {
    /// Default C calling convention (platform default)
    c = 0,
    /// Fast calling convention (LLVM specific, fewer registers saved)
    fast = 8,
    /// Cold calling convention (optimize for infrequent calls)
    cold = 9,
    /// WebKit JS calling convention
    webkit_js = 12,
    /// Any register calling convention
    any_reg = 13,
    /// x86 stdcall (Windows)
    x86_stdcall = 64,
    /// x86 fastcall (Windows)
    x86_fastcall = 65,
    /// ARM AAPCS calling convention
    arm_aapcs = 67,
    /// ARM AAPCS-VFP (use VFP registers for floats)
    arm_aapcs_vfp = 68,
    /// x86-64 System V AMD64 ABI
    x86_64_sysv = 78,
    /// Win64 calling convention
    win64 = 79,

    /// Get the appropriate calling convention for the current platform.
    pub fn forPlatform(platform: Platform) CallingConvention {
        return switch (platform.os) {
            .macos => switch (platform.arch) {
                // macOS ARM64 uses AAPCS64-like but LLVM uses C convention
                .aarch64 => .c,
                // macOS x86_64 uses System V AMD64 ABI (same as Linux)
                .x86_64 => .c, // LLVM's C convention handles SysV on Unix
                else => .c,
            },
            .linux => switch (platform.arch) {
                .aarch64 => .c,
                .x86_64 => .c, // System V AMD64 is the default on Linux
                else => .c,
            },
            .windows => switch (platform.arch) {
                .x86_64 => .win64,
                .aarch64 => .c,
                else => .c,
            },
            else => .c,
        };
    }

    /// Convert to LLVM calling convention constant.
    pub fn toLLVM(self: CallingConvention) c_uint {
        return @intFromEnum(self);
    }
};

/// ABI information for struct passing/returning.
/// Different platforms have different rules for how structs are passed:
/// - System V AMD64: structs <= 16 bytes in registers, > 16 bytes via hidden pointer
/// - Win64: structs <= 8 bytes in register, > 8 bytes via hidden pointer
/// - AAPCS64: structs <= 16 bytes in registers, > 16 bytes via hidden pointer
pub const ABI = struct {
    platform: Platform,

    pub fn init(platform: Platform) ABI {
        return .{ .platform = platform };
    }

    /// Maximum size of struct that can be returned in registers.
    pub fn maxRegisterReturnSize(self: ABI) usize {
        return switch (self.platform.os) {
            .windows => switch (self.platform.arch) {
                .x86_64 => 8, // Win64: only 64-bit or smaller in RAX
                else => 8,
            },
            else => switch (self.platform.arch) {
                // System V AMD64 and AAPCS64: 16 bytes (in RAX:RDX or X0:X1)
                .x86_64, .aarch64 => 16,
                else => 8,
            },
        };
    }

    /// Number of integer argument registers available.
    pub fn integerArgRegs(self: ABI) usize {
        return switch (self.platform.os) {
            .windows => switch (self.platform.arch) {
                .x86_64 => 4, // RCX, RDX, R8, R9
                else => 8,
            },
            else => switch (self.platform.arch) {
                .x86_64 => 6, // RDI, RSI, RDX, RCX, R8, R9
                .aarch64 => 8, // X0-X7
                else => 6,
            },
        };
    }

    /// Number of floating-point argument registers available.
    pub fn floatArgRegs(self: ABI) usize {
        return switch (self.platform.os) {
            .windows => 4, // XMM0-XMM3 (shared with int on Win64)
            else => 8, // XMM0-XMM7 on x86_64, V0-V7 on ARM64
        };
    }

    /// Check if a struct of given size should be passed by pointer.
    pub fn passByPointer(self: ABI, size: usize) bool {
        return size > self.maxRegisterReturnSize();
    }
};
