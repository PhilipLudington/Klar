//! Klar Intermediate Representation (IR)
//!
//! The Klar IR is a typed, SSA-form intermediate representation that sits
//! between the typed AST and LLVM IR. It provides:
//!
//! - Platform-neutral code representation
//! - Explicit ownership operations (move, copy, drop, borrow)
//! - Reference counting operations (rc.inc, rc.dec)
//! - Optional type operations (some, none, unwrap)
//! - Typed arithmetic with overflow checking variants
//!
//! ## Module Structure
//!
//! - `inst.zig` - IR instruction definitions and types
//! - `builder.zig` - Fluent API for constructing IR
//! - `printer.zig` - Text output for debugging
//! - `lower.zig` - AST to IR lowering (TODO)
//! - `validate.zig` - IR validation (TODO)
//!
//! ## Example
//!
//! ```zig
//! const ir = @import("ir/mod.zig");
//!
//! var module = ir.Module.init(allocator, "example");
//! defer module.deinit();
//!
//! var builder = ir.Builder.init(allocator, &module);
//!
//! _ = try builder.beginFunction("add", &.{"a", "b"}, &.{.i32_, .i32_}, .i32_);
//!
//! const func = builder.getCurrentFunction().?;
//! const a = func.params[0].value;
//! const b = func.params[1].value;
//!
//! const sum = try builder.buildAdd(a, b);
//! try builder.buildRet(sum);
//!
//! builder.endFunction();
//!
//! const output = try ir.printer.moduleToString(allocator, &module);
//! ```

// Re-export instruction types
pub const inst = @import("inst.zig");
pub const IrType = inst.IrType;
pub const Value = inst.Value;
pub const Constant = inst.Constant;
pub const Inst = inst.Inst;
pub const BasicBlock = inst.BasicBlock;
pub const BlockId = inst.BlockId;
pub const Function = inst.Function;
pub const Param = inst.Param;
pub const Module = inst.Module;

// Re-export builder
pub const builder = @import("builder.zig");
pub const Builder = builder.Builder;

// Re-export printer
pub const printer = @import("printer.zig");
pub const printModule = printer.printModule;
pub const printFunction = printer.printFunction;
pub const printType = printer.printType;
pub const moduleToString = printer.moduleToString;
pub const functionToString = printer.functionToString;

// Re-export lowerer
pub const lower = @import("lower.zig");
pub const Lowerer = lower.Lowerer;
pub const LowerError = lower.LowerError;

// Tests
test {
    _ = @import("inst.zig");
    _ = @import("builder.zig");
    _ = @import("printer.zig");
    _ = @import("lower.zig");
    _ = @import("tests.zig");
}
