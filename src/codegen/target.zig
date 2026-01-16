//! Target platform configuration.
//!
//! Provides target triple generation, parsing, and platform-specific settings.
//! Supports cross-compilation via the --target flag.

const std = @import("std");
const builtin = @import("builtin");
const llvm = @import("llvm.zig");

/// Parsed target information for cross-compilation.
pub const TargetInfo = struct {
    arch: Arch,
    os: Os,
    abi: Abi,
    /// The LLVM target triple string (null-terminated).
    triple: [:0]const u8,

    pub const Arch = enum {
        x86_64,
        aarch64,
        unknown,

        pub fn fromString(s: []const u8) Arch {
            if (std.mem.eql(u8, s, "x86_64") or std.mem.eql(u8, s, "x86-64")) return .x86_64;
            if (std.mem.eql(u8, s, "aarch64") or std.mem.eql(u8, s, "arm64")) return .aarch64;
            return .unknown;
        }

        pub fn toLinkerArch(self: Arch, os: Os) []const u8 {
            return switch (os) {
                .macos => switch (self) {
                    .aarch64 => "arm64",
                    .x86_64 => "x86_64",
                    .unknown => "unknown",
                },
                else => switch (self) {
                    .aarch64 => "aarch64",
                    .x86_64 => "x86_64",
                    .unknown => "unknown",
                },
            };
        }
    };

    pub const Os = enum {
        macos,
        linux,
        windows,
        unknown,

        pub fn fromString(s: []const u8) Os {
            if (std.mem.eql(u8, s, "macos") or std.mem.eql(u8, s, "darwin") or
                std.mem.eql(u8, s, "apple") or std.mem.startsWith(u8, s, "macosx"))
            {
                return .macos;
            }
            if (std.mem.eql(u8, s, "linux") or std.mem.startsWith(u8, s, "linux")) return .linux;
            if (std.mem.eql(u8, s, "windows") or std.mem.eql(u8, s, "win32") or
                std.mem.startsWith(u8, s, "windows"))
            {
                return .windows;
            }
            return .unknown;
        }
    };

    pub const Abi = enum {
        gnu,
        msvc,
        none,
        unknown,

        pub fn fromString(s: []const u8) Abi {
            if (std.mem.eql(u8, s, "gnu")) return .gnu;
            if (std.mem.eql(u8, s, "msvc")) return .msvc;
            if (s.len == 0) return .none;
            return .unknown;
        }
    };

    /// Parse a target triple string like "x86_64-linux-gnu" or "aarch64-apple-darwin".
    pub fn parse(allocator: std.mem.Allocator, triple_str: []const u8) !TargetInfo {
        // Handle common shorthand forms
        const normalized = normalizeTriple(triple_str);

        // Split by '-'
        var parts = std.mem.splitScalar(u8, normalized, '-');

        const arch_str = parts.next() orelse return error.InvalidTarget;
        const arch = Arch.fromString(arch_str);

        // Second part could be vendor (apple, pc, unknown) or OS
        const second = parts.next() orelse "";
        var os: Os = .unknown;
        var abi: Abi = .unknown;

        // Check if second part is a vendor or OS
        if (std.mem.eql(u8, second, "apple") or std.mem.eql(u8, second, "pc") or
            std.mem.eql(u8, second, "unknown"))
        {
            // It's a vendor, next is OS
            const os_str = parts.next() orelse "";
            os = Os.fromString(os_str);

            // Last part might be ABI
            if (parts.next()) |abi_str| {
                abi = Abi.fromString(abi_str);
            } else {
                abi = defaultAbi(os);
            }
        } else {
            // Second part is the OS
            os = Os.fromString(second);

            // Third part might be ABI
            if (parts.next()) |abi_str| {
                abi = Abi.fromString(abi_str);
            } else {
                abi = defaultAbi(os);
            }
        }

        // Generate the canonical LLVM triple
        const triple = try generateTriple(allocator, arch, os, abi);

        return .{
            .arch = arch,
            .os = os,
            .abi = abi,
            .triple = triple,
        };
    }

    /// Get the host target info.
    pub fn host(allocator: std.mem.Allocator) !TargetInfo {
        const arch: Arch = switch (builtin.cpu.arch) {
            .x86_64 => .x86_64,
            .aarch64 => .aarch64,
            else => .unknown,
        };
        const os: Os = switch (builtin.os.tag) {
            .macos => .macos,
            .linux => .linux,
            .windows => .windows,
            else => .unknown,
        };
        const abi = defaultAbi(os);
        const triple = try generateTriple(allocator, arch, os, abi);

        return .{
            .arch = arch,
            .os = os,
            .abi = abi,
            .triple = triple,
        };
    }

    fn normalizeTriple(s: []const u8) []const u8 {
        // Handle common shortcuts
        // e.g., "linux" -> use host arch + linux
        // For now, just return as-is
        return s;
    }

    fn defaultAbi(os: Os) Abi {
        return switch (os) {
            .linux => .gnu,
            .windows => .msvc,
            .macos => .none,
            .unknown => .unknown,
        };
    }

    fn generateTriple(allocator: std.mem.Allocator, arch: Arch, os: Os, abi: Abi) ![:0]const u8 {
        const arch_str = switch (arch) {
            .x86_64 => "x86_64",
            .aarch64 => switch (os) {
                .macos => "arm64",
                else => "aarch64",
            },
            .unknown => "unknown",
        };

        const os_str = switch (os) {
            .macos => "apple-macosx",
            .linux => "unknown-linux",
            .windows => "pc-windows",
            .unknown => "unknown-unknown",
        };

        const abi_str = switch (abi) {
            .gnu => "-gnu",
            .msvc => "-msvc",
            .none => "",
            .unknown => "",
        };

        // Format the triple string and add null terminator
        const triple = try std.fmt.allocPrint(allocator, "{s}-{s}{s}", .{ arch_str, os_str, abi_str });

        // Allocate with null terminator
        const result = try allocator.allocSentinel(u8, triple.len, 0);
        @memcpy(result, triple);
        allocator.free(triple);

        return result;
    }

    /// Convert to Platform struct for compatibility.
    pub fn toPlatform(self: TargetInfo) Platform {
        return .{
            .os = switch (self.os) {
                .macos => .macos,
                .linux => .linux,
                .windows => .windows,
                .unknown => builtin.os.tag, // Fallback to host
            },
            .arch = switch (self.arch) {
                .x86_64 => .x86_64,
                .aarch64 => .aarch64,
                .unknown => builtin.cpu.arch, // Fallback to host
            },
        };
    }

    /// Get the linker architecture string.
    pub fn getLinkerArch(self: TargetInfo) []const u8 {
        return self.arch.toLinkerArch(self.os);
    }

    /// Check if this is a cross-compilation (target != host).
    pub fn isCrossCompile(self: TargetInfo) bool {
        const host_arch: Arch = switch (builtin.cpu.arch) {
            .x86_64 => .x86_64,
            .aarch64 => .aarch64,
            else => .unknown,
        };
        const host_os: Os = switch (builtin.os.tag) {
            .macos => .macos,
            .linux => .linux,
            .windows => .windows,
            else => .unknown,
        };

        return self.arch != host_arch or self.os != host_os;
    }

    /// Free the allocated triple string.
    pub fn deinit(self: TargetInfo, allocator: std.mem.Allocator) void {
        allocator.free(self.triple);
    }
};

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
