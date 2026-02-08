//! Type checking module.
//!
//! Provides semantic analysis and type checking for Klar programs.
//!
//! ## Module Organization
//!
//! | Module            | Purpose                                          |
//! |-------------------|--------------------------------------------------|
//! | checker.zig       | Main TypeChecker struct (AST -> Type)            |
//! | statements.zig    | Statement type checking implementation           |
//! | patterns.zig      | Pattern matching type checking implementation    |
//! | builtins_check.zig| Builtin function checking implementation         |
//! | declarations.zig  | Declaration checking implementation              |
//! | comptime_eval.zig | Compile-time evaluation implementation           |
//! | expressions.zig   | Expression type checking implementation          |
//! | type_utils.zig    | Type compatibility and utility functions         |
//! | methods.zig       | Method resolution and signature verification     |
//! | type_resolution.zig | Type substitution, unification, AST resolution |
//! | trait_checking.zig| Trait implementation and bounds checking         |
//! | method_calls.zig  | Builtin method call type checking                |
//! | generics.zig      | Monomorphization type definitions                |
//! | traits.zig        | Trait type definitions                           |
//! | comptime.zig      | Compile-time value type definitions              |
//! | builtins.zig      | Built-in function documentation                  |
//! | modules.zig       | Module system type definitions                   |

pub const checker = @import("checker.zig");
pub const generics = @import("generics.zig");
pub const traits = @import("traits.zig");
pub const comptime_ = @import("comptime.zig");
pub const builtins = @import("builtins.zig");
pub const modules = @import("modules.zig");

// Implementation modules are imported by checker.zig directly.
// They are not re-exported here as they are internal implementation details.

// Re-export main checker types from checker.zig
pub const TypeChecker = checker.TypeChecker;
pub const CheckError = checker.CheckError;
pub const Symbol = checker.Symbol;
pub const Scope = checker.Scope;
pub const TypeEnv = checker.TypeEnv;

// Re-export types from submodules (via checker.zig which imports them)
pub const MonomorphizedFunction = checker.MonomorphizedFunction;
pub const MonomorphizedStruct = checker.MonomorphizedStruct;
pub const MonomorphizedEnum = checker.MonomorphizedEnum;
pub const MonomorphizedMethod = checker.MonomorphizedMethod;
pub const TraitMethodCall = checker.TraitMethodCall;
pub const TraitInfo = checker.TraitInfo;
pub const AssociatedTypeBinding = checker.AssociatedTypeBinding;
pub const TraitImplInfo = checker.TraitImplInfo;
pub const StructMethod = checker.StructMethod;
pub const ErrorConversionInfo = checker.ErrorConversionInfo;
pub const ComptimeValue = checker.ComptimeValue;
pub const ComptimeStruct = checker.ComptimeStruct;
pub const ComptimeArray = checker.ComptimeArray;
pub const RepeatInfo = checker.RepeatInfo;
pub const ModuleSymbols = checker.ModuleSymbols;
pub const ModuleSymbol = checker.ModuleSymbol;
