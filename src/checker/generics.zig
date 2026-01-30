//! Monomorphization support for the type checker.
//!
//! This module provides types and utilities for handling generic types and functions
//! during type checking. The main implementation is in checker.zig, but this
//! module documents the interface for working with monomorphized entities.
//!
//! ## Key Concepts
//!
//! - **Monomorphization**: The process of creating concrete type instantiations
//!   from generic templates (e.g., `List[i32]` from `List[T]`)
//! - **Mangled names**: Unique names for each concrete instantiation
//!   (e.g., `List$i32` for `List[i32]`)
//! - **Type parameter scopes**: Track type parameters (like `T`, `U`) during
//!   generic function/struct checking
//! - **Type substitution**: Replace type variables with concrete types during
//!   monomorphization
//!
//! ## Type Checking Flow for Generics
//!
//! 1. When checking a generic function definition:
//!    - `pushTypeParams()` creates scope for type parameters
//!    - Body is checked with type variables representing the parameters
//!    - `popTypeParams()` removes the scope
//!
//! 2. When a generic function is called:
//!    - Infer concrete types from arguments
//!    - `recordMonomorphization()` records the instantiation
//!    - Return substituted return type
//!
//! 3. When a generic struct/enum is used:
//!    - `recordStructMonomorphization()` / `recordEnumMonomorphization()`
//!    - Creates concrete type with substituted field types
//!
//! ## Name Mangling
//!
//! Generic instantiations are named using the pattern:
//! `BaseName$TypeArg1$TypeArg2...`
//!
//! Examples:
//! - `identity[i32]` -> `identity$i32`
//! - `Pair[i32, string]` -> `Pair$i32$string`
//! - `Option[List[i32]]` -> `Option$List$i32`

const std = @import("std");
const types = @import("../types.zig");
const Type = types.Type;
const ast = @import("../ast.zig");

/// Information about a monomorphized function instance.
pub const MonomorphizedFunction = struct {
    /// Original generic function name
    original_name: []const u8,
    /// Mangled name for this instantiation (e.g., "identity$i32")
    mangled_name: []const u8,
    /// The concrete type arguments used for this instantiation
    type_args: []const Type,
    /// The concrete function type after substitution
    concrete_type: Type,
    /// Reference to the original generic function declaration (for codegen)
    original_decl: *ast.FunctionDecl,
};

/// Information about a monomorphized struct instance.
pub const MonomorphizedStruct = struct {
    /// Original generic struct name (e.g., "Pair")
    original_name: []const u8,
    /// Mangled name for this instantiation (e.g., "Pair$i32$string")
    mangled_name: []const u8,
    /// The concrete type arguments used for this instantiation
    type_args: []const Type,
    /// The concrete struct type with substituted field types
    concrete_type: *types.StructType,
};

/// Information about a monomorphized enum instance.
pub const MonomorphizedEnum = struct {
    /// Original generic enum name (e.g., "Option")
    original_name: []const u8,
    /// Mangled name for this instantiation (e.g., "Option$i32")
    mangled_name: []const u8,
    /// The concrete type arguments used for this instantiation
    type_args: []const Type,
    /// The concrete enum type with substituted variant payloads
    concrete_type: *types.EnumType,
};

/// Information about a monomorphized method instance.
pub const MonomorphizedMethod = struct {
    /// Original struct name (e.g., "Pair")
    struct_name: []const u8,
    /// Original method name (e.g., "get_first")
    method_name: []const u8,
    /// Mangled name for this instantiation (e.g., "Pair$i32$string_get_first")
    mangled_name: []const u8,
    /// The concrete type arguments used for this instantiation
    type_args: []const Type,
    /// The concrete function type after substitution
    concrete_type: Type,
    /// Reference to the original method declaration (for codegen)
    original_decl: *ast.FunctionDecl,
};

/// Information about a trait method call through a generic bound.
/// Used to track which trait methods are called on type variables,
/// so codegen can emit the correct monomorphized method calls.
pub const TraitMethodCall = struct {
    /// Name of the type variable (e.g., "T")
    type_var_name: []const u8,
    /// Name of the trait providing the method
    trait_name: []const u8,
    /// Name of the method being called
    method_name: []const u8,
};

// ========================================================================
// Utility Functions
// ========================================================================

/// Append a primitive type name suitable for mangling.
pub fn primitiveNameForMangling(prim: types.Primitive) []const u8 {
    return switch (prim) {
        .i8_ => "i8",
        .i16_ => "i16",
        .i32_ => "i32",
        .i64_ => "i64",
        .i128_ => "i128",
        .isize_ => "isize",
        .u8_ => "u8",
        .u16_ => "u16",
        .u32_ => "u32",
        .u64_ => "u64",
        .u128_ => "u128",
        .usize_ => "usize",
        .f32_ => "f32",
        .f64_ => "f64",
        .bool_ => "bool",
        .char_ => "char",
        .string_ => "string",
    };
}
