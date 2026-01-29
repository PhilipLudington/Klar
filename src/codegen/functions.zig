//! Function emission utilities for codegen.
//!
//! This module documents function and method emission for code generation.
//!
//! ## Function Declaration
//!
//! Klar functions are emitted in two passes:
//! 1. Declaration: Create LLVM function with signature
//! 2. Emission: Generate function body
//!
//! This allows mutual recursion and forward references.
//!
//! ## Function Types
//!
//! | Type          | Example                    | Notes                    |
//! |---------------|----------------------------|--------------------------|
//! | Regular       | `fn foo(x: i32) -> i32`    | Standard function        |
//! | Method        | `impl Foo { fn bar(self) }`| Receiver-based           |
//! | Generic       | `fn max[T](a: T, b: T)`    | Type-parameterized       |
//! | Extern        | `extern fn malloc(n: u64)` | C FFI declaration        |
//! | Closure       | `\|x\| { x + 1 }`            | Lambda with captures     |
//!
//! ## Calling Conventions
//!
//! - Default: Platform standard (System V AMD64 on Linux/macOS)
//! - Sret: Large struct returns via hidden first parameter
//!
//! ## Parameter Passing
//!
//! - Primitives: By value
//! - Structs (small): By value
//! - Structs (large): By pointer (sret for return)
//! - References (&T): As pointer
//! - Slices: As {ptr, len} struct
//!
//! ## Closure Implementation
//!
//! Closures are represented as a fat pointer:
//!
//! ```
//! struct Closure {
//!     fn_ptr: *fn(env, args...) -> ret,  // Function pointer
//!     env_ptr: *Environment,              // Captured values
//! }
//! ```
//!
//! The environment struct is heap-allocated and contains copies of
//! all captured variables.
//!
//! ## Entry Point
//!
//! For `fn main(args: [String]) -> i32`:
//! 1. User's main is renamed to `_klar_user_main`
//! 2. A C-style `main(argc, argv)` wrapper is generated
//! 3. Wrapper converts argv to [String] and calls user main
//!
//! ## Key Functions in emit.zig
//!
//! - `declareFunction`: Create function signature
//! - `emitFunction`: Generate function body
//! - `declareExternFunction`: Declare C FFI function
//! - `declareImplMethods`: Declare all methods in impl block
//! - `emitImplMethods`: Generate method bodies
//! - `emitClosure`: Generate closure struct and lifted function
//! - `createClosureEnvironment`: Allocate and populate env struct
//! - `emitMainArgsWrapper`: Generate main() wrapper for args

const std = @import("std");
const llvm = @import("llvm.zig");

/// Function attribute types.
pub const FnAttr = struct {
    pub const noreturn_attr = "noreturn";
    pub const nounwind_attr = "nounwind";
    pub const readonly_attr = "readonly";
    pub const sret_attr = "sret";
    pub const noalias_attr = "noalias";
};

/// Get LLVM attribute kind for a named attribute.
pub fn getAttributeKind(name: []const u8) c_uint {
    return llvm.c.LLVMGetEnumAttributeKindForName(name.ptr, name.len);
}

/// Platform threshold for sret (struct return via pointer).
/// Structs larger than this are returned via hidden first parameter.
pub const sret_threshold_bytes: usize = 16;

/// Check if a struct size requires sret calling convention.
pub fn requiresSretForSize(size: usize) bool {
    return size > sret_threshold_bytes;
}

/// Closure struct field indices.
pub const ClosureField = struct {
    pub const fn_ptr = 0;
    pub const env_ptr = 1;
};

/// Create the LLVM type for a closure struct.
pub fn createClosureType(ctx: llvm.Context) llvm.TypeRef {
    var fields = [_]llvm.TypeRef{
        llvm.Types.pointer(ctx), // function pointer
        llvm.Types.pointer(ctx), // environment pointer
    };
    return llvm.Types.struct_(ctx, &fields, false);
}

test "requiresSretForSize" {
    try std.testing.expect(!requiresSretForSize(8));
    try std.testing.expect(!requiresSretForSize(16));
    try std.testing.expect(requiresSretForSize(17));
    try std.testing.expect(requiresSretForSize(32));
}
