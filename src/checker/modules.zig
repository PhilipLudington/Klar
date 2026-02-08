//! Module system type definitions for the type checker.
//!
//! This module provides types and documentation for the module system implementation.
//! The main implementation methods are in checker.zig.
//!
//! ## Key Concepts
//!
//! - **Modules**: Compilation units with their own scope
//! - **Imports**: Bringing symbols from other modules into scope
//! - **Exports**: Making symbols available to other modules
//!
//! ## Import Styles
//!
//! | Syntax | Effect |
//! |--------|--------|
//! | `import module` | Module name as namespace |
//! | `import module.*` | All public symbols |
//! | `import module.{A, B}` | Specific symbols |
//! | `import module.{A as X}` | Symbol with alias |
//!
//! ## Module System Flow
//!
//! 1. `prepareForNewModule()`: Create fresh scope inheriting builtins
//! 2. `checkModule()`: Type-check the module
//! 3. `registerModuleExports()`: Collect public symbols
//! 4. `processImports()`: Resolve imports in dependent modules

const std = @import("std");
const types = @import("../types.zig");
const Type = types.Type;
const module_resolver = @import("../module_resolver.zig");
const ModuleInfo = module_resolver.ModuleInfo;

/// Symbols exported from a module.
pub const ModuleSymbols = struct {
    /// All exported symbols from this module.
    symbols: std.StringHashMapUnmanaged(ModuleSymbol),
    /// The module info (for re-export tracking).
    module_info: *ModuleInfo,
};

/// A symbol imported from another module.
pub const ModuleSymbol = struct {
    /// Name of the symbol.
    name: []const u8,
    /// Kind of symbol.
    kind: Kind,
    /// Resolved type (null for types/traits).
    type_: ?Type,
    /// Whether the symbol is public.
    is_pub: bool,

    pub const Kind = enum {
        function,
        struct_type,
        enum_type,
        trait_type,
        type_alias,
        constant,
    };
};
