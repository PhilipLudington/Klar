//! Runtime function declarations for codegen.
//!
//! Implements C library declarations (malloc, free, string ops, file I/O)
//! and Rc/Arc reference counting runtime via inline LLVM IR generation.
//! Functions take LLVM primitives directly to avoid circular imports.
//!
//! ## Provided by this module
//!
//! - `RuntimeContext`: LLVM state wrapper with save/restore for nested generation
//! - `getOrDeclare*`: Cached C library function declarations
//! - `getOrCreateRc*` / `getOrCreateArc*`: Reference counting implementations

const std = @import("std");
const llvm = @import("llvm.zig");
const target = @import("target.zig");

/// Context for runtime function generation.
/// This holds the LLVM components needed to declare and define runtime functions.
pub const RuntimeContext = struct {
    ctx: llvm.Context,
    module: llvm.Module,
    builder: llvm.Builder,
    platform: target.Platform,
    /// Saved builder state for nested function generation.
    saved_bb: ?llvm.BasicBlockRef = null,
    saved_func: ?llvm.ValueRef = null,

    pub fn savePosition(self: *RuntimeContext) void {
        self.saved_bb = llvm.c.LLVMGetInsertBlock(self.builder.ref);
    }

    pub fn restorePosition(self: *RuntimeContext) void {
        if (self.saved_bb) |bb| {
            self.builder.positionAtEnd(bb);
        }
    }
};

// ============================================================================
// C Standard Library Declarations
// ============================================================================

/// Get or declare malloc.
pub fn getOrDeclareMalloc(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "malloc";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    var param_types = [_]llvm.TypeRef{i64_type};
    const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 1, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare realloc.
pub fn getOrDeclareRealloc(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "realloc";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    var param_types = [_]llvm.TypeRef{ ptr_type, i64_type };
    const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 2, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare free.
pub fn getOrDeclareFree(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "free";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    var param_types = [_]llvm.TypeRef{ptr_type};
    const fn_type = llvm.c.LLVMFunctionType(llvm.Types.void_(rc.ctx), &param_types, 1, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare strlen.
pub fn getOrDeclareStrlen(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "strlen";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    var param_types = [_]llvm.TypeRef{ptr_type};
    const fn_type = llvm.c.LLVMFunctionType(i64_type, &param_types, 1, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare strdup.
pub fn getOrDeclareStrdup(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "strdup";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    var param_types = [_]llvm.TypeRef{ptr_type};
    const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 1, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare strcmp.
pub fn getOrDeclareStrcmp(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "strcmp";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i32_type = llvm.Types.int32(rc.ctx);
    var param_types = [_]llvm.TypeRef{ ptr_type, ptr_type };
    const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 2, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare strstr.
pub fn getOrDeclareStrstr(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "strstr";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    var param_types = [_]llvm.TypeRef{ ptr_type, ptr_type };
    const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 2, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare strncmp.
pub fn getOrDeclareStrncmp(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "strncmp";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i32_type = llvm.Types.int32(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    var param_types = [_]llvm.TypeRef{ ptr_type, ptr_type, i64_type };
    const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 3, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare memcpy.
pub fn getOrDeclareMemcpy(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "memcpy";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    var param_types = [_]llvm.TypeRef{ ptr_type, ptr_type, i64_type };
    const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 3, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare memcmp.
pub fn getOrDeclareMemcmp(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "memcmp";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    const i32_type = llvm.Types.int32(rc.ctx);
    var param_types = [_]llvm.TypeRef{ ptr_type, ptr_type, i64_type };
    const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 3, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare memset.
pub fn getOrDeclareMemset(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "memset";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i32_type = llvm.Types.int32(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    var param_types = [_]llvm.TypeRef{ ptr_type, i32_type, i64_type };
    const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 3, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare memchr.
pub fn getOrDeclareMemchr(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "memchr";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i32_type = llvm.Types.int32(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    var param_types = [_]llvm.TypeRef{ ptr_type, i32_type, i64_type };
    const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 3, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare snprintf.
pub fn getOrDeclareSnprintf(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "snprintf";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    const i32_type = llvm.Types.int32(rc.ctx);
    var param_types = [_]llvm.TypeRef{ ptr_type, i64_type, ptr_type };
    const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 3, 1); // variadic
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare printf.
pub fn getOrDeclarePrintf(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "printf";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i32_type = llvm.Types.int32(rc.ctx);
    var param_types = [_]llvm.TypeRef{ptr_type};
    const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 1, 1); // variadic
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare fprintf.
pub fn getOrDeclareFprintf(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "fprintf";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i32_type = llvm.Types.int32(rc.ctx);
    var param_types = [_]llvm.TypeRef{ ptr_type, ptr_type };
    const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 2, 1); // variadic
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare puts.
pub fn getOrDeclarePuts(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "puts";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i32_type = llvm.Types.int32(rc.ctx);
    var param_types = [_]llvm.TypeRef{ptr_type};
    const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 1, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare fgets.
pub fn getOrDeclareFgets(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "fgets";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i32_type = llvm.Types.int32(rc.ctx);
    var param_types = [_]llvm.TypeRef{ ptr_type, i32_type, ptr_type };
    const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 3, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare strtol.
pub fn getOrDeclareStrtol(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "strtol";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    const i32_type = llvm.Types.int32(rc.ctx);
    var param_types = [_]llvm.TypeRef{ ptr_type, ptr_type, i32_type };
    const fn_type = llvm.c.LLVMFunctionType(i64_type, &param_types, 3, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare strtod.
pub fn getOrDeclareStrtod(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "strtod";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const f64_type = llvm.Types.double_(rc.ctx);
    var param_types = [_]llvm.TypeRef{ ptr_type, ptr_type };
    const fn_type = llvm.c.LLVMFunctionType(f64_type, &param_types, 2, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare abort.
pub fn getOrDeclareAbort(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "abort";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    var param_types = [_]llvm.TypeRef{};
    const fn_type = llvm.c.LLVMFunctionType(llvm.Types.void_(rc.ctx), &param_types, 0, 0);
    const func = llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);

    // Mark as noreturn
    llvm.c.LLVMAddAttributeAtIndex(func, @as(c_uint, @bitCast(@as(c_int, -1))), llvm.c.LLVMCreateEnumAttribute(rc.ctx.ref, llvm.c.LLVMGetEnumAttributeKindForName("noreturn", 8), 0));

    return func;
}

// ============================================================================
// File I/O C Library Declarations
// ============================================================================

/// Get or declare fopen.
pub fn getOrDeclareFopen(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "fopen";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    var param_types = [_]llvm.TypeRef{ ptr_type, ptr_type };
    const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 2, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare fclose.
pub fn getOrDeclareFclose(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "fclose";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i32_type = llvm.Types.int32(rc.ctx);
    var param_types = [_]llvm.TypeRef{ptr_type};
    const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 1, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare fread.
pub fn getOrDeclareFread(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "fread";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    var param_types = [_]llvm.TypeRef{ ptr_type, i64_type, i64_type, ptr_type };
    const fn_type = llvm.c.LLVMFunctionType(i64_type, &param_types, 4, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare fwrite.
pub fn getOrDeclareFwrite(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "fwrite";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    var param_types = [_]llvm.TypeRef{ ptr_type, i64_type, i64_type, ptr_type };
    const fn_type = llvm.c.LLVMFunctionType(i64_type, &param_types, 4, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare fflush.
pub fn getOrDeclareFflush(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "fflush";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i32_type = llvm.Types.int32(rc.ctx);
    var param_types = [_]llvm.TypeRef{ptr_type};
    const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 1, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare fseek.
pub fn getOrDeclareFseek(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "fseek";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    const i32_type = llvm.Types.int32(rc.ctx);
    var param_types = [_]llvm.TypeRef{ ptr_type, i64_type, i32_type };
    const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 3, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare ftell.
pub fn getOrDeclareFtell(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "ftell";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    var param_types = [_]llvm.TypeRef{ptr_type};
    const fn_type = llvm.c.LLVMFunctionType(i64_type, &param_types, 1, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare ferror.
pub fn getOrDeclareFerror(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "ferror";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i32_type = llvm.Types.int32(rc.ctx);
    var param_types = [_]llvm.TypeRef{ptr_type};
    const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 1, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

// ============================================================================
// Filesystem C Library Declarations
// ============================================================================

/// Get or declare access.
pub fn getOrDeclareAccess(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "access";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i32_type = llvm.Types.int32(rc.ctx);
    var param_types = [_]llvm.TypeRef{ ptr_type, i32_type };
    const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 2, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare stat.
pub fn getOrDeclareStat(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "stat";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i32_type = llvm.Types.int32(rc.ctx);
    var param_types = [_]llvm.TypeRef{ ptr_type, ptr_type };
    const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 2, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare mkdir.
pub fn getOrDeclareMkdir(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "mkdir";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i32_type = llvm.Types.int32(rc.ctx);
    var param_types = [_]llvm.TypeRef{ ptr_type, i32_type };
    const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 2, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare rmdir.
pub fn getOrDeclareRmdir(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "rmdir";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i32_type = llvm.Types.int32(rc.ctx);
    var param_types = [_]llvm.TypeRef{ptr_type};
    const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 1, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare unlink.
pub fn getOrDeclareUnlink(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "unlink";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i32_type = llvm.Types.int32(rc.ctx);
    var param_types = [_]llvm.TypeRef{ptr_type};
    const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 1, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare opendir.
pub fn getOrDeclareOpendir(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "opendir";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    var param_types = [_]llvm.TypeRef{ptr_type};
    const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 1, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare readdir.
pub fn getOrDeclareReaddir(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "readdir";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    var param_types = [_]llvm.TypeRef{ptr_type};
    const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 1, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare closedir.
pub fn getOrDeclareClosedir(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "closedir";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i32_type = llvm.Types.int32(rc.ctx);
    var param_types = [_]llvm.TypeRef{ptr_type};
    const fn_type = llvm.c.LLVMFunctionType(i32_type, &param_types, 1, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

/// Get or declare __errno_location (Linux) or __error (macOS).
pub fn getOrDeclareErrno(rc: *RuntimeContext) llvm.ValueRef {
    // Platform-specific errno accessor
    const fn_name = if (rc.platform.os == .macos) "__error" else "__errno_location";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    var param_types = [_]llvm.TypeRef{};
    const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 0, 0);
    return llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);
}

// ============================================================================
// Rc (Reference Counting) Runtime Functions
// ============================================================================

/// Get or declare klar_rc_alloc.
/// Allocates memory with Rc header: [strong_count:i64][weak_count:i64][value]
pub fn getOrDeclareRcAlloc(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "klar_rc_alloc";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    var param_types = [_]llvm.TypeRef{ i64_type, i64_type };
    const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 2, 0);
    const func = llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);

    // Create function body
    const entry_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "entry");
    rc.savePosition();

    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, entry_bb);

    const value_size = llvm.c.LLVMGetParam(func, 0);
    _ = llvm.c.LLVMGetParam(func, 1); // value_align (unused)

    // Calculate total size: 16 (header) + value_size
    const header_size = llvm.Const.int64(rc.ctx, 16);
    const total_size = llvm.c.LLVMBuildAdd(rc.builder.ref, header_size, value_size, "total_size");

    // Call malloc
    const malloc_fn = getOrDeclareMalloc(rc);
    var malloc_args = [_]llvm.ValueRef{total_size};
    const raw_ptr = llvm.c.LLVMBuildCall2(rc.builder.ref, llvm.c.LLVMGlobalGetValueType(malloc_fn), malloc_fn, &malloc_args, 1, "malloc_result");

    // Store strong_count = 1 at offset 0
    const strong_ptr = llvm.c.LLVMBuildBitCast(rc.builder.ref, raw_ptr, llvm.c.LLVMPointerType(i64_type, 0), "strong_ptr");
    _ = llvm.c.LLVMBuildStore(rc.builder.ref, llvm.Const.int64(rc.ctx, 1), strong_ptr);

    // Store weak_count = 1 at offset 8
    const eight = llvm.Const.int64(rc.ctx, 8);
    const weak_ptr_int = llvm.c.LLVMBuildAdd(rc.builder.ref, llvm.c.LLVMBuildPtrToInt(rc.builder.ref, raw_ptr, i64_type, "ptr_int"), eight, "weak_off");
    const weak_ptr = llvm.c.LLVMBuildIntToPtr(rc.builder.ref, weak_ptr_int, llvm.c.LLVMPointerType(i64_type, 0), "weak_ptr");
    _ = llvm.c.LLVMBuildStore(rc.builder.ref, llvm.Const.int64(rc.ctx, 1), weak_ptr);

    // Return pointer to value (offset 16)
    const sixteen = llvm.Const.int64(rc.ctx, 16);
    const val_ptr_int = llvm.c.LLVMBuildAdd(rc.builder.ref, llvm.c.LLVMBuildPtrToInt(rc.builder.ref, raw_ptr, i64_type, "ptr_int2"), sixteen, "val_off");
    const val_ptr = llvm.c.LLVMBuildIntToPtr(rc.builder.ref, val_ptr_int, ptr_type, "val_ptr");

    _ = llvm.c.LLVMBuildRet(rc.builder.ref, val_ptr);

    rc.restorePosition();

    return func;
}

/// Get or declare klar_rc_clone.
/// Increments strong_count and returns the same pointer.
pub fn getOrDeclareRcClone(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "klar_rc_clone";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    var param_types = [_]llvm.TypeRef{ptr_type};
    const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 1, 0);
    const func = llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);

    const entry_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "entry");
    rc.savePosition();

    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, entry_bb);

    const value_ptr = llvm.c.LLVMGetParam(func, 0);

    // Calculate header address: value_ptr - 16
    const ptr_int = llvm.c.LLVMBuildPtrToInt(rc.builder.ref, value_ptr, i64_type, "ptr_int");
    const header_off = llvm.c.LLVMBuildSub(rc.builder.ref, ptr_int, llvm.Const.int64(rc.ctx, 16), "header_off");
    const strong_ptr = llvm.c.LLVMBuildIntToPtr(rc.builder.ref, header_off, llvm.c.LLVMPointerType(i64_type, 0), "strong_ptr");

    // Load, increment, store strong_count
    const old_count = llvm.c.LLVMBuildLoad2(rc.builder.ref, i64_type, strong_ptr, "old_count");
    const new_count = llvm.c.LLVMBuildAdd(rc.builder.ref, old_count, llvm.Const.int64(rc.ctx, 1), "new_count");
    _ = llvm.c.LLVMBuildStore(rc.builder.ref, new_count, strong_ptr);

    _ = llvm.c.LLVMBuildRet(rc.builder.ref, value_ptr);

    rc.restorePosition();

    return func;
}

/// Get or declare klar_rc_drop.
/// Decrements strong_count; if it reaches 0, decrements weak_count and potentially frees.
pub fn getOrDeclareRcDrop(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "klar_rc_drop";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    var param_types = [_]llvm.TypeRef{ptr_type};
    const fn_type = llvm.c.LLVMFunctionType(llvm.Types.void_(rc.ctx), &param_types, 1, 0);
    const func = llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);

    const entry_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "entry");
    const dec_weak_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "dec_weak");
    const free_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "free");
    const done_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "done");

    rc.savePosition();

    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, entry_bb);

    const value_ptr = llvm.c.LLVMGetParam(func, 0);

    // Calculate header address: value_ptr - 16
    const ptr_int = llvm.c.LLVMBuildPtrToInt(rc.builder.ref, value_ptr, i64_type, "ptr_int");
    const header_off = llvm.c.LLVMBuildSub(rc.builder.ref, ptr_int, llvm.Const.int64(rc.ctx, 16), "header_off");
    const strong_ptr = llvm.c.LLVMBuildIntToPtr(rc.builder.ref, header_off, llvm.c.LLVMPointerType(i64_type, 0), "strong_ptr");

    // Load, decrement strong_count
    const old_strong = llvm.c.LLVMBuildLoad2(rc.builder.ref, i64_type, strong_ptr, "old_strong");
    const new_strong = llvm.c.LLVMBuildSub(rc.builder.ref, old_strong, llvm.Const.int64(rc.ctx, 1), "new_strong");
    _ = llvm.c.LLVMBuildStore(rc.builder.ref, new_strong, strong_ptr);

    // If strong_count == 0, decrement weak_count
    const strong_zero = llvm.c.LLVMBuildICmp(rc.builder.ref, llvm.c.LLVMIntEQ, new_strong, llvm.Const.int64(rc.ctx, 0), "strong_zero");
    _ = llvm.c.LLVMBuildCondBr(rc.builder.ref, strong_zero, dec_weak_bb, done_bb);

    // dec_weak block
    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, dec_weak_bb);
    const weak_off = llvm.c.LLVMBuildSub(rc.builder.ref, ptr_int, llvm.Const.int64(rc.ctx, 8), "weak_off");
    const weak_ptr = llvm.c.LLVMBuildIntToPtr(rc.builder.ref, weak_off, llvm.c.LLVMPointerType(i64_type, 0), "weak_ptr");

    const old_weak = llvm.c.LLVMBuildLoad2(rc.builder.ref, i64_type, weak_ptr, "old_weak");
    const new_weak = llvm.c.LLVMBuildSub(rc.builder.ref, old_weak, llvm.Const.int64(rc.ctx, 1), "new_weak");
    _ = llvm.c.LLVMBuildStore(rc.builder.ref, new_weak, weak_ptr);

    // If weak_count == 0, free the memory
    const weak_zero = llvm.c.LLVMBuildICmp(rc.builder.ref, llvm.c.LLVMIntEQ, new_weak, llvm.Const.int64(rc.ctx, 0), "weak_zero");
    _ = llvm.c.LLVMBuildCondBr(rc.builder.ref, weak_zero, free_bb, done_bb);

    // free block
    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, free_bb);
    const raw_ptr = llvm.c.LLVMBuildIntToPtr(rc.builder.ref, header_off, ptr_type, "raw_ptr");
    const free_fn = getOrDeclareFree(rc);
    var free_args = [_]llvm.ValueRef{raw_ptr};
    _ = llvm.c.LLVMBuildCall2(rc.builder.ref, llvm.c.LLVMGlobalGetValueType(free_fn), free_fn, &free_args, 1, "");
    _ = llvm.c.LLVMBuildBr(rc.builder.ref, done_bb);

    // done block
    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, done_bb);
    _ = llvm.c.LLVMBuildRetVoid(rc.builder.ref);

    rc.restorePosition();

    return func;
}

/// Get or declare klar_rc_downgrade.
/// Creates a Weak pointer from an Rc pointer.
pub fn getOrDeclareRcDowngrade(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "klar_rc_downgrade";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    var param_types = [_]llvm.TypeRef{ptr_type};
    const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 1, 0);
    const func = llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);

    const entry_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "entry");
    rc.savePosition();

    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, entry_bb);

    const value_ptr = llvm.c.LLVMGetParam(func, 0);

    // Calculate weak_count address: value_ptr - 8
    const ptr_int = llvm.c.LLVMBuildPtrToInt(rc.builder.ref, value_ptr, i64_type, "ptr_int");
    const weak_off = llvm.c.LLVMBuildSub(rc.builder.ref, ptr_int, llvm.Const.int64(rc.ctx, 8), "weak_off");
    const weak_ptr = llvm.c.LLVMBuildIntToPtr(rc.builder.ref, weak_off, llvm.c.LLVMPointerType(i64_type, 0), "weak_ptr");

    // Increment weak_count
    const old_weak = llvm.c.LLVMBuildLoad2(rc.builder.ref, i64_type, weak_ptr, "old_weak");
    const new_weak = llvm.c.LLVMBuildAdd(rc.builder.ref, old_weak, llvm.Const.int64(rc.ctx, 1), "new_weak");
    _ = llvm.c.LLVMBuildStore(rc.builder.ref, new_weak, weak_ptr);

    _ = llvm.c.LLVMBuildRet(rc.builder.ref, value_ptr);

    rc.restorePosition();

    return func;
}

/// Get or declare klar_weak_upgrade.
/// Attempts to upgrade a Weak to an Rc; returns null if strong_count is 0.
pub fn getOrDeclareWeakUpgrade(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "klar_weak_upgrade";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    var param_types = [_]llvm.TypeRef{ptr_type};
    const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 1, 0);
    const func = llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);

    const entry_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "entry");
    const upgrade_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "upgrade");
    const fail_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "fail");

    rc.savePosition();

    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, entry_bb);

    const weak_value_ptr = llvm.c.LLVMGetParam(func, 0);

    // Calculate strong_count address: value_ptr - 16
    const ptr_int = llvm.c.LLVMBuildPtrToInt(rc.builder.ref, weak_value_ptr, i64_type, "ptr_int");
    const strong_off = llvm.c.LLVMBuildSub(rc.builder.ref, ptr_int, llvm.Const.int64(rc.ctx, 16), "strong_off");
    const strong_ptr = llvm.c.LLVMBuildIntToPtr(rc.builder.ref, strong_off, llvm.c.LLVMPointerType(i64_type, 0), "strong_ptr");

    // Check if strong_count > 0
    const strong_count = llvm.c.LLVMBuildLoad2(rc.builder.ref, i64_type, strong_ptr, "strong_count");
    const is_alive = llvm.c.LLVMBuildICmp(rc.builder.ref, llvm.c.LLVMIntSGT, strong_count, llvm.Const.int64(rc.ctx, 0), "is_alive");
    _ = llvm.c.LLVMBuildCondBr(rc.builder.ref, is_alive, upgrade_bb, fail_bb);

    // upgrade block: increment strong_count and return ptr
    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, upgrade_bb);
    const new_strong = llvm.c.LLVMBuildAdd(rc.builder.ref, strong_count, llvm.Const.int64(rc.ctx, 1), "new_strong");
    _ = llvm.c.LLVMBuildStore(rc.builder.ref, new_strong, strong_ptr);
    _ = llvm.c.LLVMBuildRet(rc.builder.ref, weak_value_ptr);

    // fail block: return null
    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, fail_bb);
    _ = llvm.c.LLVMBuildRet(rc.builder.ref, llvm.c.LLVMConstNull(ptr_type));

    rc.restorePosition();

    return func;
}

// ============================================================================
// Arc (Atomic Reference Counting) Runtime Functions
// ============================================================================

/// Get or declare klar_arc_alloc.
/// Same as rc_alloc but for Arc types.
pub fn getOrDeclareArcAlloc(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "klar_arc_alloc";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    var param_types = [_]llvm.TypeRef{ i64_type, i64_type };
    const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 2, 0);
    const func = llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);

    const entry_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "entry");
    rc.savePosition();

    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, entry_bb);

    const value_size = llvm.c.LLVMGetParam(func, 0);
    _ = llvm.c.LLVMGetParam(func, 1);

    const header_size = llvm.Const.int64(rc.ctx, 16);
    const total_size = llvm.c.LLVMBuildAdd(rc.builder.ref, header_size, value_size, "total_size");

    const malloc_fn = getOrDeclareMalloc(rc);
    var malloc_args = [_]llvm.ValueRef{total_size};
    const raw_ptr = llvm.c.LLVMBuildCall2(rc.builder.ref, llvm.c.LLVMGlobalGetValueType(malloc_fn), malloc_fn, &malloc_args, 1, "malloc_result");

    const strong_ptr = llvm.c.LLVMBuildBitCast(rc.builder.ref, raw_ptr, llvm.c.LLVMPointerType(i64_type, 0), "strong_ptr");
    _ = llvm.c.LLVMBuildStore(rc.builder.ref, llvm.Const.int64(rc.ctx, 1), strong_ptr);

    const eight = llvm.Const.int64(rc.ctx, 8);
    const weak_ptr_int = llvm.c.LLVMBuildAdd(rc.builder.ref, llvm.c.LLVMBuildPtrToInt(rc.builder.ref, raw_ptr, i64_type, "ptr_int"), eight, "weak_off");
    const weak_ptr = llvm.c.LLVMBuildIntToPtr(rc.builder.ref, weak_ptr_int, llvm.c.LLVMPointerType(i64_type, 0), "weak_ptr");
    _ = llvm.c.LLVMBuildStore(rc.builder.ref, llvm.Const.int64(rc.ctx, 1), weak_ptr);

    const sixteen = llvm.Const.int64(rc.ctx, 16);
    const val_ptr_int = llvm.c.LLVMBuildAdd(rc.builder.ref, llvm.c.LLVMBuildPtrToInt(rc.builder.ref, raw_ptr, i64_type, "ptr_int2"), sixteen, "val_off");
    const val_ptr = llvm.c.LLVMBuildIntToPtr(rc.builder.ref, val_ptr_int, ptr_type, "val_ptr");

    _ = llvm.c.LLVMBuildRet(rc.builder.ref, val_ptr);

    rc.restorePosition();

    return func;
}

/// Get or declare klar_arc_clone.
/// Uses atomic increment for thread safety.
pub fn getOrDeclareArcClone(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "klar_arc_clone";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    var param_types = [_]llvm.TypeRef{ptr_type};
    const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 1, 0);
    const func = llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);

    const entry_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "entry");
    rc.savePosition();

    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, entry_bb);

    const value_ptr = llvm.c.LLVMGetParam(func, 0);

    // Calculate strong_count address
    const ptr_int = llvm.c.LLVMBuildPtrToInt(rc.builder.ref, value_ptr, i64_type, "ptr_int");
    const strong_off = llvm.c.LLVMBuildSub(rc.builder.ref, ptr_int, llvm.Const.int64(rc.ctx, 16), "strong_off");
    const strong_ptr = llvm.c.LLVMBuildIntToPtr(rc.builder.ref, strong_off, ptr_type, "strong_ptr");

    // Atomic fetch_add for thread safety
    _ = llvm.c.LLVMBuildAtomicRMW(
        rc.builder.ref,
        llvm.c.LLVMAtomicRMWBinOpAdd,
        strong_ptr,
        llvm.Const.int64(rc.ctx, 1),
        llvm.c.LLVMAtomicOrderingSequentiallyConsistent,
        0,
    );

    _ = llvm.c.LLVMBuildRet(rc.builder.ref, value_ptr);

    rc.restorePosition();

    return func;
}

/// Get or declare klar_arc_drop.
/// Uses atomic decrement for thread safety.
pub fn getOrDeclareArcDrop(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "klar_arc_drop";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    var param_types = [_]llvm.TypeRef{ptr_type};
    const fn_type = llvm.c.LLVMFunctionType(llvm.Types.void_(rc.ctx), &param_types, 1, 0);
    const func = llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);

    const entry_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "entry");
    const dec_weak_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "dec_weak");
    const free_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "free");
    const done_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "done");

    rc.savePosition();

    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, entry_bb);

    const value_ptr = llvm.c.LLVMGetParam(func, 0);

    // Calculate header addresses
    const ptr_int = llvm.c.LLVMBuildPtrToInt(rc.builder.ref, value_ptr, i64_type, "ptr_int");
    const strong_off = llvm.c.LLVMBuildSub(rc.builder.ref, ptr_int, llvm.Const.int64(rc.ctx, 16), "strong_off");
    const strong_ptr = llvm.c.LLVMBuildIntToPtr(rc.builder.ref, strong_off, ptr_type, "strong_ptr");

    // Atomic fetch_sub
    const old_strong = llvm.c.LLVMBuildAtomicRMW(
        rc.builder.ref,
        llvm.c.LLVMAtomicRMWBinOpSub,
        strong_ptr,
        llvm.Const.int64(rc.ctx, 1),
        llvm.c.LLVMAtomicOrderingSequentiallyConsistent,
        0,
    );

    // If old strong_count was 1 (now 0), decrement weak_count
    const was_one = llvm.c.LLVMBuildICmp(rc.builder.ref, llvm.c.LLVMIntEQ, old_strong, llvm.Const.int64(rc.ctx, 1), "was_one");
    _ = llvm.c.LLVMBuildCondBr(rc.builder.ref, was_one, dec_weak_bb, done_bb);

    // dec_weak block
    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, dec_weak_bb);
    const weak_off = llvm.c.LLVMBuildSub(rc.builder.ref, ptr_int, llvm.Const.int64(rc.ctx, 8), "weak_off");
    const weak_ptr = llvm.c.LLVMBuildIntToPtr(rc.builder.ref, weak_off, ptr_type, "weak_ptr");

    const old_weak = llvm.c.LLVMBuildAtomicRMW(
        rc.builder.ref,
        llvm.c.LLVMAtomicRMWBinOpSub,
        weak_ptr,
        llvm.Const.int64(rc.ctx, 1),
        llvm.c.LLVMAtomicOrderingSequentiallyConsistent,
        0,
    );

    const weak_was_one = llvm.c.LLVMBuildICmp(rc.builder.ref, llvm.c.LLVMIntEQ, old_weak, llvm.Const.int64(rc.ctx, 1), "weak_was_one");
    _ = llvm.c.LLVMBuildCondBr(rc.builder.ref, weak_was_one, free_bb, done_bb);

    // free block
    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, free_bb);
    const raw_ptr = llvm.c.LLVMBuildIntToPtr(rc.builder.ref, strong_off, ptr_type, "raw_ptr");
    const free_fn = getOrDeclareFree(rc);
    var free_args = [_]llvm.ValueRef{raw_ptr};
    _ = llvm.c.LLVMBuildCall2(rc.builder.ref, llvm.c.LLVMGlobalGetValueType(free_fn), free_fn, &free_args, 1, "");
    _ = llvm.c.LLVMBuildBr(rc.builder.ref, done_bb);

    // done block
    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, done_bb);
    _ = llvm.c.LLVMBuildRetVoid(rc.builder.ref);

    rc.restorePosition();

    return func;
}

/// Get or declare klar_arc_downgrade.
/// Creates a WeakArc pointer from an Arc pointer.
pub fn getOrDeclareArcDowngrade(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "klar_arc_downgrade";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    var param_types = [_]llvm.TypeRef{ptr_type};
    const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 1, 0);
    const func = llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);

    const entry_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "entry");
    rc.savePosition();

    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, entry_bb);

    const value_ptr = llvm.c.LLVMGetParam(func, 0);

    // Calculate weak_count address
    const ptr_int = llvm.c.LLVMBuildPtrToInt(rc.builder.ref, value_ptr, i64_type, "ptr_int");
    const weak_off = llvm.c.LLVMBuildSub(rc.builder.ref, ptr_int, llvm.Const.int64(rc.ctx, 8), "weak_off");
    const weak_ptr = llvm.c.LLVMBuildIntToPtr(rc.builder.ref, weak_off, ptr_type, "weak_ptr");

    // Atomic increment weak_count
    _ = llvm.c.LLVMBuildAtomicRMW(
        rc.builder.ref,
        llvm.c.LLVMAtomicRMWBinOpAdd,
        weak_ptr,
        llvm.Const.int64(rc.ctx, 1),
        llvm.c.LLVMAtomicOrderingSequentiallyConsistent,
        0,
    );

    _ = llvm.c.LLVMBuildRet(rc.builder.ref, value_ptr);

    rc.restorePosition();

    return func;
}

/// Get or declare klar_weak_arc_upgrade.
/// Attempts to upgrade a WeakArc to an Arc.
pub fn getOrDeclareWeakArcUpgrade(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "klar_weak_arc_upgrade";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    var param_types = [_]llvm.TypeRef{ptr_type};
    const fn_type = llvm.c.LLVMFunctionType(ptr_type, &param_types, 1, 0);
    const func = llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);

    const entry_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "entry");
    const loop_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "loop");
    const success_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "success");
    const fail_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "fail");

    rc.savePosition();

    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, entry_bb);

    const weak_value_ptr = llvm.c.LLVMGetParam(func, 0);

    // Calculate strong_count address
    const ptr_int = llvm.c.LLVMBuildPtrToInt(rc.builder.ref, weak_value_ptr, i64_type, "ptr_int");
    const strong_off = llvm.c.LLVMBuildSub(rc.builder.ref, ptr_int, llvm.Const.int64(rc.ctx, 16), "strong_off");
    const strong_ptr = llvm.c.LLVMBuildIntToPtr(rc.builder.ref, strong_off, ptr_type, "strong_ptr");

    _ = llvm.c.LLVMBuildBr(rc.builder.ref, loop_bb);

    // loop block: try CAS
    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, loop_bb);

    // Load current strong_count
    const current = llvm.c.LLVMBuildLoad2(rc.builder.ref, i64_type, strong_ptr, "current");

    // If current == 0, fail
    const is_zero = llvm.c.LLVMBuildICmp(rc.builder.ref, llvm.c.LLVMIntEQ, current, llvm.Const.int64(rc.ctx, 0), "is_zero");
    const try_cas_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "try_cas");
    _ = llvm.c.LLVMBuildCondBr(rc.builder.ref, is_zero, fail_bb, try_cas_bb);

    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, try_cas_bb);
    const new_count = llvm.c.LLVMBuildAdd(rc.builder.ref, current, llvm.Const.int64(rc.ctx, 1), "new_count");

    // Compare and swap
    const cmpxchg = llvm.c.LLVMBuildAtomicCmpXchg(
        rc.builder.ref,
        strong_ptr,
        current,
        new_count,
        llvm.c.LLVMAtomicOrderingSequentiallyConsistent,
        llvm.c.LLVMAtomicOrderingSequentiallyConsistent,
        0,
    );
    const success = llvm.c.LLVMBuildExtractValue(rc.builder.ref, cmpxchg, 1, "success");
    _ = llvm.c.LLVMBuildCondBr(rc.builder.ref, success, success_bb, loop_bb);

    // success block
    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, success_bb);
    _ = llvm.c.LLVMBuildRet(rc.builder.ref, weak_value_ptr);

    // fail block
    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, fail_bb);
    _ = llvm.c.LLVMBuildRet(rc.builder.ref, llvm.c.LLVMConstNull(ptr_type));

    rc.restorePosition();

    return func;
}

/// Get or declare klar_weak_arc_drop.
/// Decrements weak_count for WeakArc.
pub fn getOrDeclareWeakArcDrop(rc: *RuntimeContext) llvm.ValueRef {
    const fn_name = "klar_weak_arc_drop";
    if (llvm.c.LLVMGetNamedFunction(rc.module.ref, fn_name)) |func| {
        return func;
    }

    const ptr_type = llvm.Types.pointer(rc.ctx);
    const i64_type = llvm.Types.int64(rc.ctx);
    var param_types = [_]llvm.TypeRef{ptr_type};
    const fn_type = llvm.c.LLVMFunctionType(llvm.Types.void_(rc.ctx), &param_types, 1, 0);
    const func = llvm.c.LLVMAddFunction(rc.module.ref, fn_name, fn_type);

    const entry_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "entry");
    const free_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "free");
    const done_bb = llvm.c.LLVMAppendBasicBlockInContext(rc.ctx.ref, func, "done");

    rc.savePosition();

    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, entry_bb);

    const value_ptr = llvm.c.LLVMGetParam(func, 0);

    // Calculate addresses
    const ptr_int = llvm.c.LLVMBuildPtrToInt(rc.builder.ref, value_ptr, i64_type, "ptr_int");
    const weak_off = llvm.c.LLVMBuildSub(rc.builder.ref, ptr_int, llvm.Const.int64(rc.ctx, 8), "weak_off");
    const weak_ptr = llvm.c.LLVMBuildIntToPtr(rc.builder.ref, weak_off, ptr_type, "weak_ptr");

    // Atomic decrement weak_count
    const old_weak = llvm.c.LLVMBuildAtomicRMW(
        rc.builder.ref,
        llvm.c.LLVMAtomicRMWBinOpSub,
        weak_ptr,
        llvm.Const.int64(rc.ctx, 1),
        llvm.c.LLVMAtomicOrderingSequentiallyConsistent,
        0,
    );

    // If old weak_count was 1, free the memory
    const was_one = llvm.c.LLVMBuildICmp(rc.builder.ref, llvm.c.LLVMIntEQ, old_weak, llvm.Const.int64(rc.ctx, 1), "was_one");
    _ = llvm.c.LLVMBuildCondBr(rc.builder.ref, was_one, free_bb, done_bb);

    // free block
    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, free_bb);
    const strong_off = llvm.c.LLVMBuildSub(rc.builder.ref, ptr_int, llvm.Const.int64(rc.ctx, 16), "strong_off");
    const raw_ptr = llvm.c.LLVMBuildIntToPtr(rc.builder.ref, strong_off, ptr_type, "raw_ptr");
    const free_fn = getOrDeclareFree(rc);
    var free_args = [_]llvm.ValueRef{raw_ptr};
    _ = llvm.c.LLVMBuildCall2(rc.builder.ref, llvm.c.LLVMGlobalGetValueType(free_fn), free_fn, &free_args, 1, "");
    _ = llvm.c.LLVMBuildBr(rc.builder.ref, done_bb);

    // done block
    llvm.c.LLVMPositionBuilderAtEnd(rc.builder.ref, done_bb);
    _ = llvm.c.LLVMBuildRetVoid(rc.builder.ref);

    rc.restorePosition();

    return func;
}
