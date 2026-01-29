//! Statement emission utilities for codegen.
//!
//! This module documents the statement emission logic in code generation.
//!
//! ## Statement Types
//!
//! | Statement    | Example                     | Notes                    |
//! |--------------|-----------------------------|--------------------------|
//! | VarDecl      | `let x: i32 = 5`            | Variable declaration     |
//! | Assignment   | `x = 10`                    | Variable assignment      |
//! | Expression   | `foo()`                     | Expression statement     |
//! | If           | `if cond { } else { }`      | Conditional              |
//! | Match        | `match x { ... }`           | Pattern matching         |
//! | While        | `while cond { }`            | While loop               |
//! | For          | `for i in 0..10 { }`        | For loop                 |
//! | Loop         | `loop { }`                  | Infinite loop            |
//! | Break        | `break`                     | Exit loop                |
//! | Continue     | `continue`                  | Next iteration           |
//! | Return       | `return value`              | Function return          |
//!
//! ## Control Flow Generation
//!
//! Control flow uses LLVM basic blocks:
//!
//! ```
//! if cond {
//!     then_body
//! } else {
//!     else_body
//! }
//! ```
//!
//! Generates:
//! ```
//! entry:
//!   %cond = ...
//!   br %cond, then_bb, else_bb
//! then_bb:
//!   ; then_body
//!   br merge_bb
//! else_bb:
//!   ; else_body
//!   br merge_bb
//! merge_bb:
//!   ; continue
//! ```
//!
//! ## Loop Generation
//!
//! Loops maintain a stack of (continue_block, break_block) for nested loops.
//!
//! ```
//! while cond {
//!     body
//! }
//! ```
//!
//! Generates:
//! ```
//! entry:
//!   br loop_bb
//! loop_bb:
//!   %cond = ...
//!   br %cond, body_bb, exit_bb
//! body_bb:
//!   ; body
//!   br loop_bb
//! exit_bb:
//!   ; continue
//! ```
//!
//! ## For Loop Variants
//!
//! - Range iteration: `for i in 0..n`
//! - Array iteration: `for x in arr`
//! - Slice iteration: `for x in slice`
//! - List iteration: `for x in list`
//! - Set iteration: `for x in set`
//! - Map iteration: `for (k, v) in map`
//!
//! ## Pattern Matching
//!
//! Match statements use discriminated dispatch:
//!
//! - Literal patterns: Direct comparison
//! - Variant patterns: Tag comparison + payload extraction
//! - Wildcard patterns: Unconditional match
//! - Binding patterns: Variable capture
//!
//! ## Key Functions in emit.zig
//!
//! - `emitStmt`: Main dispatch for all statement types
//! - `emitBlock`: Emit block of statements
//! - `emitIfStmt`: If/else generation
//! - `emitMatchStmt`: Match statement generation
//! - `emitWhileLoop`: While loop generation
//! - `emitForLoop`: For loop dispatch by iterator type
//! - `emitInfiniteLoop`: Loop {} generation
//! - `emitBreak`: Break with scope cleanup
//! - `emitContinue`: Continue with scope cleanup
//! - `emitPatternMatch`: Pattern matching predicate
//! - `bindPatternVariables`: Extract pattern bindings

const std = @import("std");
const llvm = @import("llvm.zig");

/// Loop context for break/continue targets.
pub const LoopContext = struct {
    continue_block: llvm.BasicBlockRef,
    break_block: llvm.BasicBlockRef,
};

/// Pattern kind for match statement dispatch.
pub const PatternKind = enum {
    wildcard, // _
    literal, // 42, "hello", true
    identifier, // x (binding)
    variant, // Some(x), None
    tuple, // (a, b)
    struct_, // { x, y }
};

/// Determine pattern kind from AST pattern.
pub fn getPatternKind(tag: u8) PatternKind {
    // This maps AST pattern tags to our enum
    return switch (tag) {
        0 => .wildcard,
        1 => .literal,
        2 => .identifier,
        3 => .variant,
        4 => .tuple,
        5 => .struct_,
        else => .wildcard,
    };
}
