//! I/O emission utilities for codegen.
//!
//! This module documents the File, Path, BufReader, and BufWriter type
//! implementations for code generation.
//!
//! ## File Type
//!
//! Files are represented as C FILE* pointers:
//!
//! ```
//! type File = ptr  // FILE* from libc
//! ```
//!
//! File operations wrap standard C library functions:
//! - `fopen`, `fclose`, `fread`, `fwrite`, `fflush`, `fseek`, `ftell`
//!
//! ## Path Type
//!
//! Paths are String wrappers for filesystem operations:
//!
//! ```
//! struct Path {
//!     inner: String,  // Path string data
//! }
//! ```
//!
//! ## BufReader Type
//!
//! Buffered reader for efficient I/O:
//!
//! ```
//! struct BufReader[R] {
//!     inner: R,           // Underlying reader (File, etc.)
//!     buffer: [u8; 8192], // Internal buffer
//!     pos: i32,           // Current position in buffer
//!     filled: i32,        // Bytes filled in buffer
//! }
//! ```
//!
//! ## BufWriter Type
//!
//! Buffered writer for efficient I/O:
//!
//! ```
//! struct BufWriter[W] {
//!     inner: W,           // Underlying writer (File, etc.)
//!     buffer: [u8; 8192], // Internal buffer
//!     pos: i32,           // Current position in buffer
//! }
//! ```
//!
//! ## IoError Enum
//!
//! I/O operations return Result[T, IoError]:
//!
//! ```
//! enum IoError {
//!     NotFound,        // ENOENT
//!     PermissionDenied, // EACCES
//!     AlreadyExists,   // EEXIST
//!     InvalidInput,    // EINVAL
//!     Other,           // Catch-all
//! }
//! ```
//!
//! ## Filesystem Operations
//!
//! Key functions in emit.zig:
//!
//! - `emitFileOpen`: Open file for reading/writing
//! - `emitFileRead`: Read bytes from file
//! - `emitFileWrite`: Write bytes to file
//! - `emitFileClose`: Close file handle
//! - `emitFileFlush`: Flush file buffer
//! - `emitFsExists`: Check if path exists
//! - `emitFsIsFile`: Check if path is a file
//! - `emitFsIsDir`: Check if path is a directory
//! - `emitFsCreateDir`: Create a directory
//! - `emitFsRemoveFile`: Remove a file
//! - `emitFsRemoveDir`: Remove a directory
//! - `emitFsReadString`: Read entire file to string
//! - `emitFsWriteString`: Write string to file
//! - `emitFsReadDir`: List directory contents
//!
//! ## Standard Streams
//!
//! - `emitStdoutWrite`: Write to stdout
//! - `emitStderrWrite`: Write to stderr
//! - `emitStdinRead`: Read from stdin

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
