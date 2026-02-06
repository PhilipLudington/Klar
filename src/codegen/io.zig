//! I/O helper utilities for codegen.
//!
//! Provides constants, type constructors, and POSIX constants for File, Path,
//! BufReader, and BufWriter code generation. The emission implementation
//! (emitFileOpen, emitFileRead, emitFsExists, etc.) remains in emit.zig.
//!
//! ## Provided by this module
//!
//! - `buffer_size`: Buffer size constant for BufReader/BufWriter (8KB)
//! - `BufReaderField` / `BufWriterField`: Struct field index constants
//! - `IoErrorVariant`: Error variant index constants
//! - `AccessMode` / `SeekWhence`: POSIX flag constants
//! - `createBufReaderStructType` / `createBufWriterStructType`: LLVM type builders
//! - `createPathStructType`: LLVM type builder for Path
//!
//! ## Type Layouts (reference)
//!
//! - File: `ptr` (C FILE*)
//! - Path: `{ String }` (wrapper around String)
//! - BufReader: `{ ptr inner, [8192 x i8] buffer, i32 pos, i32 filled }`
//! - BufWriter: `{ ptr inner, [8192 x i8] buffer, i32 pos }`

const std = @import("std");
const llvm = @import("llvm.zig");

/// Buffer size for BufReader/BufWriter (8KB).
pub const buffer_size = 8192;

/// BufReader struct field indices.
pub const BufReaderField = struct {
    pub const inner = 0;
    pub const buffer = 1;
    pub const pos = 2;
    pub const filled = 3;
};

/// BufWriter struct field indices.
pub const BufWriterField = struct {
    pub const inner = 0;
    pub const buffer = 1;
    pub const pos = 2;
};

/// IoError variant indices.
pub const IoErrorVariant = struct {
    pub const not_found = 0;
    pub const permission_denied = 1;
    pub const already_exists = 2;
    pub const invalid_input = 3;
    pub const other = 4;
};

/// POSIX file access mode flags.
pub const AccessMode = struct {
    pub const F_OK = 0; // File exists
    pub const R_OK = 4; // Read permission
    pub const W_OK = 2; // Write permission
    pub const X_OK = 1; // Execute permission
};

/// POSIX fseek whence values.
pub const SeekWhence = struct {
    pub const SEEK_SET = 0;
    pub const SEEK_CUR = 1;
    pub const SEEK_END = 2;
};

/// Create the LLVM type for BufReader struct.
pub fn createBufReaderStructType(ctx: llvm.Context) llvm.TypeRef {
    const buffer_type = llvm.Types.array(llvm.Types.int8(ctx), buffer_size);
    var fields = [_]llvm.TypeRef{
        llvm.Types.pointer(ctx), // inner (FILE*)
        buffer_type, // buffer
        llvm.Types.int32(ctx), // pos
        llvm.Types.int32(ctx), // filled
    };
    return llvm.Types.struct_(ctx, &fields, false);
}

/// Create the LLVM type for BufWriter struct.
pub fn createBufWriterStructType(ctx: llvm.Context) llvm.TypeRef {
    const buffer_type = llvm.Types.array(llvm.Types.int8(ctx), buffer_size);
    var fields = [_]llvm.TypeRef{
        llvm.Types.pointer(ctx), // inner (FILE*)
        buffer_type, // buffer
        llvm.Types.int32(ctx), // pos
    };
    return llvm.Types.struct_(ctx, &fields, false);
}

/// Create the LLVM type for Path struct (wrapper around String).
pub fn createPathStructType(ctx: llvm.Context) llvm.TypeRef {
    var string_fields = [_]llvm.TypeRef{
        llvm.Types.pointer(ctx), // data
        llvm.Types.int32(ctx), // len
        llvm.Types.int32(ctx), // capacity
    };
    const string_type = llvm.Types.struct_(ctx, &string_fields, false);

    var fields = [_]llvm.TypeRef{string_type};
    return llvm.Types.struct_(ctx, &fields, false);
}
