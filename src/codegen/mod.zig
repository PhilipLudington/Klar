//! Native code generation module.
//!
//! Provides LLVM-based code generation for Klar programs.
//!
//! ## Module Organization
//!
//! | Module          | Purpose                                          |
//! |-----------------|--------------------------------------------------|
//! | emit.zig        | Main Emitter struct (AST -> LLVM IR)             |
//! | llvm.zig        | LLVM C API bindings                              |
//! | target.zig      | Target platform/ABI information                  |
//! | layout.zig      | Struct layout calculation                        |
//! | linker.zig      | Native linker invocation                         |
//! | runtime.zig     | C library declarations, Rc/Arc runtime           |
//! | generics.zig    | Monomorphization utilities                       |
//! | types_emit.zig  | Type conversion documentation                    |
//! | strings_emit.zig| String type utilities                            |
//! | list.zig        | List[T] utilities                                |
//! | map.zig         | Map[K,V] utilities                               |
//! | set.zig         | Set[T] utilities                                 |
//! | io.zig          | I/O type utilities                               |
//! | optionals.zig   | Optional/Result utilities                        |
//! | builtins.zig    | Built-in function utilities                      |
//! | expressions.zig | Expression emission utilities                    |
//! | statements.zig  | Statement emission utilities                     |
//! | functions.zig   | Function emission utilities                      |

pub const llvm = @import("llvm.zig");
pub const target = @import("target.zig");
pub const emit = @import("emit.zig");
pub const linker = @import("linker.zig");
pub const layout = @import("layout.zig");
pub const runtime = @import("runtime.zig");

// Supporting modules with utilities and documentation
// These are re-exported from emit.zig for convenience
pub const generics = emit.generics;
pub const types_emit = emit.types_emit;
pub const strings_emit = emit.strings_emit;
pub const list = emit.list;
pub const map = emit.map;
pub const set = emit.set;
pub const io = emit.io;
pub const optionals = emit.optionals;
pub const builtins = emit.builtins;
pub const expressions = emit.expressions;
pub const statements = emit.statements;
pub const functions = emit.functions;

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
    emit_klar_ir: bool = false,
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
    /// Suppress "Built" message (for klar run).
    quiet: bool = false,
    /// Libraries to link (e.g., "m" for -lm, "curl" for -lcurl).
    link_libs: []const []const u8 = &.{},
    /// Library search paths (e.g., "/usr/local/lib").
    link_paths: []const []const u8 = &.{},
    /// Freestanding mode: compile without standard library (no libc).
    freestanding: bool = false,
    /// Entry point symbol name (default: main). Used with --freestanding.
    entry_point: ?[]const u8 = null,
    /// Custom linker script path. Used with --freestanding.
    linker_script: ?[]const u8 = null,
    /// Compile only - produce object file, don't link.
    compile_only: bool = false,
    /// Additional search paths for module resolution (e.g., from package dependencies).
    search_paths: []const []const u8 = &.{},
};

/// Convenience re-exports
pub const Emitter = emit.Emitter;
pub const EmitError = emit.EmitError;
pub const Platform = target.Platform;
pub const OptLevel = target.OptLevel;
pub const TargetInfo = target.TargetInfo;
pub const LayoutCalculator = layout.LayoutCalculator;
pub const StructLayout = layout.StructLayout;
