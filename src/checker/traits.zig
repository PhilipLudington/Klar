//! Trait system support for the type checker.
//!
//! This module provides types and documentation for the trait system implementation.
//! The main implementation methods are in checker.zig.
//!
//! ## Key Concepts
//!
//! - **Traits**: Interface definitions specifying required methods
//! - **Implementations**: Concrete implementations of traits for types
//! - **Trait bounds**: Constraints on type parameters (e.g., `T: Eq`)
//! - **Associated types**: Type members declared in traits (e.g., `type Item`)
//!
//! ## Builtin Traits
//!
//! | Trait | Methods | Purpose |
//! |-------|---------|---------|
//! | Eq | `eq(&self, other: &Self) -> bool` | Equality comparison |
//! | Ordered | `lt`, `le`, `gt`, `ge` | Ordering comparison |
//! | Clone | `clone(&self) -> Self` | Explicit copying |
//! | Drop | `drop(self)` | Custom destructor |
//! | Default | `default() -> Self` | Default values |
//! | Hash | `hash(&self) -> i64` | Hashing for maps/sets |
//! | Iterator | `next(&mut self) -> ?Self.Item` | Iteration protocol |
//! | IntoIterator | `into_iter(self) -> Self.IntoIter` | Convert to iterator |
//! | From[E] | `from(err: E) -> Self` | Error type conversion |
//! | Into[T] | `into(self) -> T` | Type conversion |
//! | Read | `read(&mut self, buf: &mut [u8]) -> Result[i32, IoError]` | Read I/O |
//! | Write | `write(&mut self, buf: &[u8]) -> Result[i32, IoError]` | Write I/O |
//!
//! ## Type Checking Flow for Traits
//!
//! 1. `checkTrait()`: Register trait definition
//!    - Parse method signatures
//!    - Register associated types
//!    - Handle super traits (inheritance)
//!
//! 2. `checkImpl()`: Check trait implementation
//!    - Verify all required methods are implemented
//!    - Check method signatures match trait
//!    - Validate associated type bindings
//!    - Register in trait_impls registry
//!
//! 3. During generic function checking:
//!    - `typeSatisfiesBounds()` verifies trait bounds
//!    - Track trait method calls for monomorphization
//!
//! ## Trait Registry Keys
//!
//! Trait implementations are stored with keys like:
//! - `TypeName:TraitName` (e.g., `Point:Eq`)
//! - `TypeName:TraitName[TypeArg]` for generic traits (e.g., `AppError:From[IoError]`)

const std = @import("std");
const types = @import("../types.zig");
const Type = types.Type;
const ast = @import("../ast.zig");

/// Information about a trait definition.
pub const TraitInfo = struct {
    /// The trait type containing method signatures
    trait_type: *types.TraitType,
    /// The original AST declaration (null for builtin traits)
    decl: ?*ast.TraitDecl,
};

/// Information about an associated type binding in an impl block.
pub const AssociatedTypeBinding = struct {
    /// Name of the associated type
    name: []const u8,
    /// The concrete type provided
    concrete_type: Type,
};

/// Information about a trait implementation for a type.
pub const TraitImplInfo = struct {
    /// Name of the trait being implemented
    trait_name: []const u8,
    /// Name of the implementing type
    impl_type_name: []const u8,
    /// Type parameters from the impl block (for generic impls)
    impl_type_params: []const types.TypeVar,
    /// Associated type bindings (type Item = ConcreteType)
    associated_type_bindings: []const AssociatedTypeBinding,
    /// The methods implementing the trait
    methods: []const StructMethod,
};

/// Information about a method defined in an impl block.
pub const StructMethod = struct {
    /// Name of the method
    name: []const u8,
    /// The function declaration from the AST
    decl: *ast.FunctionDecl,
    /// Type parameters from the impl block (for generic impls)
    impl_type_params: []const types.TypeVar,
    /// The resolved function type (with type vars if generic)
    func_type: Type,
    /// Whether the first parameter is 'self'
    has_self: bool,
    /// Whether self is mutable (&mut self vs &self)
    self_is_mutable: bool,
};

/// Information about an error conversion needed for the ? operator.
/// When a function returns Result[T, TargetError] and uses ? on Result[U, SourceError],
/// this records the From[SourceError] conversion to be generated in codegen.
pub const ErrorConversionInfo = struct {
    /// The source error type (from the operand's Result type)
    source_type: Type,
    /// The target error type (from the function's return type)
    target_type: Type,
    /// Mangled name of the from method (e.g., "AppError_from_IoError")
    from_method_name: []const u8,
};
