//! Native code generation module.
//!
//! Provides LLVM-based code generation for Klar programs.

pub const llvm = @import("llvm.zig");
pub const target = @import("target.zig");
pub const emit = @import("emit.zig");
pub const linker = @import("linker.zig");
pub const layout = @import("layout.zig");

/// Errors that can occur during code generation.
pub const CodegenError = error{
    OutOfMemory,
    UnsupportedFeature,
    InvalidAST,
    LLVMError,
    ModuleVerificationFailed,
    TargetNotFound,
    TargetMachineCreationFailed,
    EmitFailed,
    LinkerFailed,
    LinkerNotFound,
    OutputWriteFailed,
};

/// Klar IR optimization level (before LLVM lowering).
pub const KlarOptLevel = @import("../opt/mod.zig").OptLevel;

/// Compile options for native code generation.
pub const CompileOptions = struct {
    output_path: ?[]const u8 = null,
    emit_llvm_ir: bool = false,
    emit_assembly: bool = false,
    optimization_level: target.OptLevel = .none,
    /// Klar IR optimization level (constant folding, DCE, simplification).
    opt_level: KlarOptLevel = .O0,
    /// Verbose output for optimization passes.
    verbose_opt: bool = false,
    /// Generate debug information (DWARF).
    debug_info: bool = false,
    /// Source file path (for debug info).
    source_path: ?[]const u8 = null,
    /// Target triple for cross-compilation (e.g., "x86_64-linux-gnu").
    /// If null, compiles for the host platform.
    target_triple: ?[]const u8 = null,
};

/// Convenience re-exports
pub const Emitter = emit.Emitter;
pub const EmitError = emit.EmitError;
pub const Platform = target.Platform;
pub const OptLevel = target.OptLevel;
pub const TargetInfo = target.TargetInfo;
pub const LayoutCalculator = layout.LayoutCalculator;
pub const StructLayout = layout.StructLayout;
