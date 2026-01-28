const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("ast.zig");
const Span = ast.Span;

// ============================================================================
// Type Representation
// ============================================================================

/// Core type representation for the Klar type system.
/// All types are represented as variants of this union.
pub const Type = union(enum) {
    // Primitive types
    primitive: Primitive,

    // Composite types
    array: *ArrayType,
    slice: *SliceType,
    tuple: *TupleType,
    optional: *Type,
    result: *ResultType,
    function: *FunctionType,
    reference: *ReferenceType,

    // User-defined types
    struct_: *StructType,
    enum_: *EnumType,
    trait_: *TraitType,

    // Generic types
    type_var: TypeVar,
    applied: *AppliedType,
    /// Associated type reference (e.g., T.Item where T is a type variable)
    /// This is resolved to a concrete type during monomorphization.
    associated_type_ref: *AssociatedTypeRef,

    // Reference-counted types
    rc: *RcType,
    weak_rc: *WeakRcType,
    arc: *ArcType,
    weak_arc: *WeakArcType,

    // Interior mutability types
    cell: *CellType,

    // Range type
    range: *RangeType,

    // Error context type
    context_error: *ContextErrorType,

    // Collection types
    list: *ListType,
    map: *MapType,
    set: *SetType,
    string_data: *StringDataType,

    // I/O types
    file: void, // Opaque FILE* handle
    io_error: void, // IoError enum type
    stdout_handle: void, // Stdout marker type
    stderr_handle: void, // Stderr marker type
    stdin_handle: void, // Stdin marker type
    path: void, // Path type (wrapper around String for filesystem paths)

    // Buffered I/O types
    buf_reader: *BufReaderType, // BufReader[R: Read] buffered reader wrapper
    buf_writer: *BufWriterType, // BufWriter[W: Write] buffered writer wrapper

    // FFI types
    extern_type: *ExternType, // External type for C interop
    cptr: *CptrType, // Non-null raw pointer (CPtr[T])
    copt_ptr: *CoptPtrType, // Nullable raw pointer (COptPtr[T])
    cstr: void, // Null-terminated C string (borrowed)
    cstr_owned: void, // Null-terminated C string (owned, must be freed)

    // Special types
    void_,
    never,
    unknown,
    error_type,

    pub fn eql(self: Type, other: Type) bool {
        if (@intFromEnum(self) != @intFromEnum(other)) return false;

        return switch (self) {
            .primitive => |p| other.primitive == p,
            .array => |a| a.element.eql(other.array.element) and a.size == other.array.size,
            .slice => |s| s.element.eql(other.slice.element),
            .tuple => |t| blk: {
                const ot = other.tuple;
                if (t.elements.len != ot.elements.len) break :blk false;
                for (t.elements, ot.elements) |e1, e2| {
                    if (!e1.eql(e2)) break :blk false;
                }
                break :blk true;
            },
            .optional => |o| o.eql(other.optional.*),
            .result => |r| r.ok_type.eql(other.result.ok_type) and r.err_type.eql(other.result.err_type),
            .function => |f| blk: {
                const of = other.function;
                if (f.params.len != of.params.len) break :blk false;
                for (f.params, of.params) |p1, p2| {
                    if (!p1.eql(p2)) break :blk false;
                }
                break :blk f.return_type.eql(of.return_type);
            },
            .reference => |r| r.inner.eql(other.reference.inner) and r.mutable == other.reference.mutable,
            .struct_ => |s| std.mem.eql(u8, s.name, other.struct_.name),
            .enum_ => |e| std.mem.eql(u8, e.name, other.enum_.name),
            .trait_ => |t| std.mem.eql(u8, t.name, other.trait_.name),
            .type_var => |tv| tv.id == other.type_var.id,
            .applied => |a| blk: {
                const oa = other.applied;
                if (!a.base.eql(oa.base)) break :blk false;
                if (a.args.len != oa.args.len) break :blk false;
                for (a.args, oa.args) |arg1, arg2| {
                    if (!arg1.eql(arg2)) break :blk false;
                }
                break :blk true;
            },
            .associated_type_ref => |a| blk: {
                const oa = other.associated_type_ref;
                break :blk a.type_var.id == oa.type_var.id and
                    std.mem.eql(u8, a.assoc_name, oa.assoc_name);
            },
            .rc => |r| r.inner.eql(other.rc.inner),
            .weak_rc => |w| w.inner.eql(other.weak_rc.inner),
            .arc => |a| a.inner.eql(other.arc.inner),
            .weak_arc => |w| w.inner.eql(other.weak_arc.inner),
            .cell => |c| c.inner.eql(other.cell.inner),
            // Context error equality depends on inner error type
            .context_error => |ce| ce.inner_type.eql(other.context_error.inner_type),
            // Range type equality only depends on element type; inclusive is a runtime property
            .range => |r| r.element_type.eql(other.range.element_type),
            // List type equality depends on element type
            .list => |l| l.element.eql(other.list.element),
            // Map type equality depends on key and value types
            .map => |m| m.key.eql(other.map.key) and m.value.eql(other.map.value),
            // Set type equality depends on element type
            .set => |s| s.element.eql(other.set.element),
            // String data is a singleton type (no type parameters)
            .string_data => true,
            // Buffered I/O type equality depends on inner reader/writer type
            .buf_reader => |br| br.inner.eql(other.buf_reader.inner),
            .buf_writer => |bw| bw.inner.eql(other.buf_writer.inner),
            // I/O types are singleton types
            .file, .io_error, .stdout_handle, .stderr_handle, .stdin_handle, .path => true,
            // Extern types are equal if they have the same name
            .extern_type => |e| std.mem.eql(u8, e.name, other.extern_type.name),
            // FFI pointer types depend on inner type
            .cptr => |c| c.inner.eql(other.cptr.inner),
            .copt_ptr => |c| c.inner.eql(other.copt_ptr.inner),
            // CStr and CStrOwned are singleton types (no type parameters)
            .cstr, .cstr_owned => true,
            .void_, .never, .unknown, .error_type => true,
        };
    }

    pub fn isNumeric(self: Type) bool {
        return switch (self) {
            .primitive => |p| p.isNumeric(),
            else => false,
        };
    }

    pub fn isInteger(self: Type) bool {
        return switch (self) {
            .primitive => |p| p.isInteger(),
            else => false,
        };
    }

    pub fn isFloat(self: Type) bool {
        return switch (self) {
            .primitive => |p| p.isFloat(),
            else => false,
        };
    }

    pub fn isSigned(self: Type) bool {
        return switch (self) {
            .primitive => |p| p.isSigned(),
            else => false,
        };
    }

    /// Returns true if this type is a Copy type (values are copied, not moved).
    /// Copy types include:
    /// - All primitive types (integers, floats, bool, char)
    /// - References (both immutable and mutable)
    /// - Arrays of Copy types
    /// - Tuples where all elements are Copy
    /// - Structs explicitly marked as Copy
    pub fn isCopyType(self: Type) bool {
        return switch (self) {
            // All primitives are Copy (integers, floats, bool, char, string literals)
            .primitive => true,

            // References are always Copy (they're just pointers)
            .reference => true,

            // Arrays are Copy if their element type is Copy
            .array => |a| a.element.isCopyType(),

            // Tuples are Copy if all elements are Copy
            .tuple => |t| {
                for (t.elements) |elem| {
                    if (!elem.isCopyType()) return false;
                }
                return true;
            },

            // Structs may be explicitly marked as Copy
            .struct_ => |s| s.is_copy,

            // Optionals are Copy if the inner type is Copy
            .optional => |o| o.isCopyType(),

            // Type variables might be Copy depending on bounds, but conservatively say no
            .type_var => false,

            // Applied types need to check the base type
            .applied => |a| a.base.isCopyType(),

            // void and never are trivially Copy (no data to move)
            .void_, .never => true,

            // Range is Copy (contains integers and bool)
            .range => |r| r.element_type.isCopyType(),

            // Raw pointers are Copy (just the address, like in Rust)
            // CStr is borrowed so it's Copy; CStrOwned is NOT Copy (owns memory)
            .cptr, .copt_ptr, .cstr => true,

            // These types are NOT Copy:
            // - Slices (contain a pointer + length, may own data)
            // - Enums (may contain non-Copy payloads)
            // - Traits (dyn trait objects have ownership semantics)
            // - Result (may contain non-Copy ok/err types)
            // - Function types (closures may capture non-Copy data)
            // - Rc/Weak (must use .clone() explicitly to get new reference)
            // - Arc/WeakArc (thread-safe reference counting, must use .clone())
            // - Cell (provides interior mutability, has move semantics)
            // - List (owns heap memory, must be moved or cloned)
            // - Map (owns heap memory, must be moved or cloned)
            // - Set (owns heap memory, must be moved or cloned)
            // - String (heap-allocated string, must be moved or cloned)
            // - CStrOwned (owns null-terminated C string memory, must be moved)
            // - ContextError (contains a string message)
            // - File (owns file handle resource)
            // - IoError (may contain string payload)
            // - Stdout/Stderr/Stdin (unique handles)
            // - BufReader/BufWriter (wrap I/O types, have internal state)
            // - Path (owns string data for filesystem path)
            // - Unknown/error types (conservative)
            // - Associated type refs (will be resolved during monomorphization)
            // - Extern types (opaque or sized, treat as non-Copy for safety)
            // - CStrOwned (owns null-terminated C string memory)
            .slice, .enum_, .trait_, .result, .function, .rc, .weak_rc, .arc, .weak_arc, .cell, .list, .map, .set, .string_data, .context_error, .file, .io_error, .stdout_handle, .stderr_handle, .stdin_handle, .buf_reader, .buf_writer, .path, .unknown, .error_type, .associated_type_ref, .extern_type, .cstr_owned => false,
        };
    }
};

// ============================================================================
// Primitive Types
// ============================================================================

pub const Primitive = enum {
    // Signed integers
    i8_,
    i16_,
    i32_,
    i64_,
    i128_,
    isize_,

    // Unsigned integers
    u8_,
    u16_,
    u32_,
    u64_,
    u128_,
    usize_,

    // Floating point
    f32_,
    f64_,

    // Other primitives
    bool_,
    char_,
    string_,

    pub fn isNumeric(self: Primitive) bool {
        return self.isInteger() or self.isFloat();
    }

    pub fn isInteger(self: Primitive) bool {
        return switch (self) {
            .i8_, .i16_, .i32_, .i64_, .i128_, .isize_ => true,
            .u8_, .u16_, .u32_, .u64_, .u128_, .usize_ => true,
            else => false,
        };
    }

    pub fn isFloat(self: Primitive) bool {
        return switch (self) {
            .f32_, .f64_ => true,
            else => false,
        };
    }

    pub fn isSigned(self: Primitive) bool {
        return switch (self) {
            .i8_, .i16_, .i32_, .i64_, .i128_, .isize_ => true,
            .f32_, .f64_ => true,
            else => false,
        };
    }

    pub fn bitSize(self: Primitive) ?u16 {
        return switch (self) {
            .i8_, .u8_ => 8,
            .i16_, .u16_ => 16,
            .i32_, .u32_, .f32_ => 32,
            .i64_, .u64_, .f64_ => 64,
            .i128_, .u128_ => 128,
            .isize_, .usize_ => null, // platform dependent
            .bool_ => 1,
            .char_ => 21, // unicode scalar (up to 21 bits)
            .string_ => null,
        };
    }

    pub fn name(self: Primitive) []const u8 {
        return switch (self) {
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

    pub fn fromName(name_str: []const u8) ?Primitive {
        const primitives = std.StaticStringMap(Primitive).initComptime(.{
            .{ "i8", .i8_ },
            .{ "i16", .i16_ },
            .{ "i32", .i32_ },
            .{ "i64", .i64_ },
            .{ "i128", .i128_ },
            .{ "isize", .isize_ },
            .{ "u8", .u8_ },
            .{ "u16", .u16_ },
            .{ "u32", .u32_ },
            .{ "u64", .u64_ },
            .{ "u128", .u128_ },
            .{ "usize", .usize_ },
            .{ "f32", .f32_ },
            .{ "f64", .f64_ },
            .{ "bool", .bool_ },
            .{ "char", .char_ },
            .{ "string", .string_ },
        });
        return primitives.get(name_str);
    }

    /// Check if widening conversion from self to other is safe
    pub fn canWidenTo(self: Primitive, other: Primitive) bool {
        if (self == other) return true;

        // Get sizes
        const self_size = self.bitSize() orelse return false;
        const other_size = other.bitSize() orelse return false;

        // Float widening
        if (self.isFloat() and other.isFloat()) {
            return self_size <= other_size;
        }

        // Integer widening
        if (self.isInteger() and other.isInteger()) {
            // Signed can widen to larger signed
            if (self.isSigned() and other.isSigned()) {
                return self_size <= other_size;
            }
            // Unsigned can widen to larger unsigned
            if (!self.isSigned() and !other.isSigned()) {
                return self_size <= other_size;
            }
            // Unsigned can widen to larger signed (needs extra bit)
            if (!self.isSigned() and other.isSigned()) {
                return self_size < other_size;
            }
            // Signed cannot safely widen to unsigned
            return false;
        }

        return false;
    }
};

// ============================================================================
// Composite Types
// ============================================================================

pub const ArrayType = struct {
    element: Type,
    size: usize,
};

pub const SliceType = struct {
    element: Type,
};

pub const TupleType = struct {
    elements: []const Type,
};

pub const ResultType = struct {
    ok_type: Type,
    err_type: Type,
};

pub const FunctionType = struct {
    params: []const Type,
    return_type: Type,
    is_async: bool = false,
    /// Bitmask indicating which parameters are comptime (index i is comptime if bit i is set)
    comptime_params: u64 = 0,
    /// True if this function is unsafe and can only be called from an unsafe context
    is_unsafe: bool = false,
    /// Bitmask indicating which parameters are out parameters (index i is out if bit i is set)
    /// Out parameters are for FFI - the caller passes a pointer to uninitialized memory
    out_params: u64 = 0,
};

pub const ReferenceType = struct {
    inner: Type,
    mutable: bool,
};

// ============================================================================
// User-Defined Types
// ============================================================================

pub const StructType = struct {
    name: []const u8,
    type_params: []const TypeVar,
    fields: []const StructField,
    traits: []const *TraitType,
    is_copy: bool = false,
    is_extern: bool = false,
    is_packed: bool = false,
};

pub const StructField = struct {
    name: []const u8,
    type_: Type,
    is_pub: bool,
};

pub const EnumType = struct {
    name: []const u8,
    type_params: []const TypeVar,
    variants: []const EnumVariant,
    is_extern: bool = false, // extern enum for C-compatible layout
    repr_type: ?Type = null, // Resolved repr type for extern enums
};

pub const EnumVariant = struct {
    name: []const u8,
    payload: ?VariantPayload,
    value: ?i128 = null, // Explicit discriminant value for extern enums
};

pub const VariantPayload = union(enum) {
    tuple: []const Type,
    struct_: []const StructField,
};

/// Associated type declaration in a trait (e.g., `type Item` or `type Item: Clone`)
pub const AssociatedType = struct {
    name: []const u8,
    bounds: []const *TraitType, // Required trait bounds
    default: ?Type, // Optional default type
};

pub const TraitType = struct {
    name: []const u8,
    type_params: []const TypeVar,
    associated_types: []const AssociatedType, // Associated type declarations
    methods: []const TraitMethod,
    super_traits: []const *TraitType,
    is_unsafe: bool = false, // Unsafe trait - implementing requires unsafe impl
};

pub const TraitMethod = struct {
    name: []const u8,
    signature: FunctionType,
    has_default: bool,
};

// ============================================================================
// FFI Types
// ============================================================================

/// External type for C interop
/// Opaque types (size == null) can only be used behind pointers
/// Sized types (size != null) can be passed by value
pub const ExternType = struct {
    name: []const u8,
    size: ?u64, // null for opaque types, byte size for sized types
};

/// Non-null raw pointer type (CPtr[T])
/// Equivalent to T* in C, assumed non-null.
/// Dereferencing requires `unsafe` block.
pub const CptrType = struct {
    inner: Type, // The pointed-to type
};

/// Nullable raw pointer type (COptPtr[T])
/// Equivalent to T* in C, may be null.
/// Must check for null before dereferencing.
pub const CoptPtrType = struct {
    inner: Type, // The pointed-to type
};

// Note: CStr is represented as a void variant in Type union
// (no type parameters, like File/IoError)

// ============================================================================
// Generic Types
// ============================================================================

/// Type variable for generics (e.g., T in List[T])
pub const TypeVar = struct {
    id: u32,
    name: []const u8,
    bounds: []const *TraitType,
};

/// Applied generic type (e.g., List[i32])
pub const AppliedType = struct {
    base: Type,
    args: []const Type,
};

/// Associated type reference (e.g., T.Item where T: Container)
/// Used to represent associated types that depend on type variables,
/// which are resolved to concrete types during monomorphization.
pub const AssociatedTypeRef = struct {
    /// The type variable that has the associated type bound (e.g., T in T.Item)
    type_var: TypeVar,
    /// The name of the associated type (e.g., "Item")
    assoc_name: []const u8,
    /// The trait that declares this associated type
    trait: *TraitType,
};

// ============================================================================
// Reference-Counted Types
// ============================================================================

/// Reference-counted wrapper type (Rc[T])
/// Provides shared ownership with automatic memory management.
/// The value is freed when the last Rc reference is dropped.
/// NOT thread-safe - use Arc for thread-safe reference counting.
pub const RcType = struct {
    inner: Type, // The wrapped type
};

/// Weak reference type (Weak[T])
/// Non-owning reference that doesn't prevent deallocation.
/// Must be upgraded to Rc before accessing the value.
pub const WeakRcType = struct {
    inner: Type, // The wrapped type
};

/// Atomic reference-counted wrapper type (Arc[T])
/// Thread-safe shared ownership with automatic memory management.
/// Uses atomic operations for reference count updates.
/// The value is freed when the last Arc reference is dropped.
pub const ArcType = struct {
    inner: Type, // The wrapped type
};

/// Weak atomic reference type (WeakArc[T])
/// Thread-safe non-owning reference that doesn't prevent deallocation.
/// Must be upgraded to Arc before accessing the value.
pub const WeakArcType = struct {
    inner: Type, // The wrapped type
};

/// Cell type for interior mutability (Cell[T])
/// Allows mutation of the inner value even through an immutable reference.
/// Only safe for Copy types (no aliasing issues).
pub const CellType = struct {
    inner: Type, // The wrapped type (must be Copy)
};

/// ContextError type for wrapping errors with context messages (ContextError[E])
/// Wraps an error with a context message for better error reporting.
/// Layout: { message: string, cause: E, file: ?string, line: i32, column: i32 }
/// In debug builds, file/line/column track the location of the .context() call.
/// In release builds, file=null, line=0, column=0.
pub const ContextErrorType = struct {
    inner_type: Type, // The wrapped error type
};

/// Range type for iteration (Range[T])
/// Represents an iterator over a sequence of values from start to end.
/// Layout: { start: T, end: T, current: T, inclusive: bool }
pub const RangeType = struct {
    element_type: Type,
    inclusive: bool,
};

/// List type for growable collections (List[T])
/// A heap-allocated, dynamically-sized array.
/// Layout: { ptr: *T, len: i32, capacity: i32 }
pub const ListType = struct {
    element: Type,
};

/// Map type for key-value collections (Map[K, V])
/// A heap-allocated hash map using open addressing with linear probing.
/// Layout: { entries: *Entry, len: i32, capacity: i32, tombstone_count: i32 }
/// Entry layout: { state: i8, cached_hash: i32, key: K, value: V }
/// State: 0=EMPTY, 1=OCCUPIED, 2=TOMBSTONE
pub const MapType = struct {
    key: Type,
    value: Type,
};

/// Set type for unique element collections (Set[T])
/// A heap-allocated hash set using open addressing with linear probing.
/// Layout: { entries: *Entry, len: i32, capacity: i32, tombstone_count: i32 }
/// Entry layout: { state: i8, cached_hash: i32, element: T }
/// State: 0=EMPTY, 1=OCCUPIED, 2=TOMBSTONE
pub const SetType = struct {
    element: Type,
};

/// String type for heap-allocated UTF-8 strings (String)
/// A heap-allocated, dynamically-sized string.
/// Layout: { ptr: *u8, len: i32, capacity: i32 }
/// The data is null-terminated for C interop compatibility.
pub const StringDataType = struct {
    // No type parameters - String is not generic
    // This is a marker struct to distinguish from primitive string_ type
};

/// BufReader type for buffered reading (BufReader[R: Read])
/// Wraps a Read implementation with an internal buffer for efficient I/O.
/// Layout: { inner: R, buffer: [u8; 8192], pos: i32, cap: i32 }
pub const BufReaderType = struct {
    inner: Type, // The wrapped reader type (must implement Read)
};

/// BufWriter type for buffered writing (BufWriter[W: Write])
/// Wraps a Write implementation with an internal buffer for efficient I/O.
/// Layout: { inner: W, buffer: [u8; 8192], len: i32 }
pub const BufWriterType = struct {
    inner: Type, // The wrapped writer type (must implement Write)
};

// ============================================================================
// Type Builder - Arena-based type allocation
// ============================================================================

pub const TypeBuilder = struct {
    arena: std.heap.ArenaAllocator,
    next_type_var_id: u32 = 0,

    pub fn init(alloc: Allocator) TypeBuilder {
        return .{
            .arena = std.heap.ArenaAllocator.init(alloc),
        };
    }

    pub fn deinit(self: *TypeBuilder) void {
        self.arena.deinit();
    }

    pub fn getAllocator(self: *TypeBuilder) Allocator {
        return self.arena.allocator();
    }

    // Primitive type constructors
    pub fn i32Type(self: *TypeBuilder) Type {
        _ = self;
        return .{ .primitive = .i32_ };
    }

    pub fn i64Type(self: *TypeBuilder) Type {
        _ = self;
        return .{ .primitive = .i64_ };
    }

    pub fn f64Type(self: *TypeBuilder) Type {
        _ = self;
        return .{ .primitive = .f64_ };
    }

    pub fn boolType(self: *TypeBuilder) Type {
        _ = self;
        return .{ .primitive = .bool_ };
    }

    pub fn charType(self: *TypeBuilder) Type {
        _ = self;
        return .{ .primitive = .char_ };
    }

    pub fn stringType(self: *TypeBuilder) Type {
        _ = self;
        return .{ .primitive = .string_ };
    }

    pub fn voidType(self: *TypeBuilder) Type {
        _ = self;
        return .void_;
    }

    pub fn neverType(self: *TypeBuilder) Type {
        _ = self;
        return .never;
    }

    pub fn unknownType(self: *TypeBuilder) Type {
        _ = self;
        return .unknown;
    }

    // Composite type constructors
    pub fn arrayType(self: *TypeBuilder, element: Type, size: usize) !Type {
        const arr = try self.arena.allocator().create(ArrayType);
        arr.* = .{ .element = element, .size = size };
        return .{ .array = arr };
    }

    pub fn sliceType(self: *TypeBuilder, element: Type) !Type {
        const slice = try self.arena.allocator().create(SliceType);
        slice.* = .{ .element = element };
        return .{ .slice = slice };
    }

    pub fn tupleType(self: *TypeBuilder, elements: []const Type) !Type {
        const tuple = try self.arena.allocator().create(TupleType);
        const elems = try self.arena.allocator().dupe(Type, elements);
        tuple.* = .{ .elements = elems };
        return .{ .tuple = tuple };
    }

    pub fn optionalType(self: *TypeBuilder, inner: Type) !Type {
        const opt = try self.arena.allocator().create(Type);
        opt.* = inner;
        return .{ .optional = opt };
    }

    pub fn resultType(self: *TypeBuilder, ok_type: Type, err_type: Type) !Type {
        const result = try self.arena.allocator().create(ResultType);
        result.* = .{ .ok_type = ok_type, .err_type = err_type };
        return .{ .result = result };
    }

    pub fn functionType(self: *TypeBuilder, params: []const Type, return_type: Type) !Type {
        return self.functionTypeWithFlags(params, return_type, 0, false);
    }

    pub fn functionTypeWithComptime(self: *TypeBuilder, params: []const Type, return_type: Type, comptime_params: u64) !Type {
        return self.functionTypeWithFlags(params, return_type, comptime_params, false);
    }

    pub fn functionTypeWithFlags(self: *TypeBuilder, params: []const Type, return_type: Type, comptime_params: u64, is_unsafe: bool) !Type {
        return self.functionTypeWithAllFlags(params, return_type, comptime_params, is_unsafe, 0);
    }

    pub fn functionTypeWithAllFlags(self: *TypeBuilder, params: []const Type, return_type: Type, comptime_params: u64, is_unsafe: bool, out_params: u64) !Type {
        const func = try self.arena.allocator().create(FunctionType);
        const params_copy = try self.arena.allocator().dupe(Type, params);
        func.* = .{ .params = params_copy, .return_type = return_type, .comptime_params = comptime_params, .is_unsafe = is_unsafe, .out_params = out_params };
        return .{ .function = func };
    }

    pub fn referenceType(self: *TypeBuilder, inner: Type, mutable: bool) !Type {
        const ref = try self.arena.allocator().create(ReferenceType);
        ref.* = .{ .inner = inner, .mutable = mutable };
        return .{ .reference = ref };
    }

    // Type variable creation
    pub fn newTypeVar(self: *TypeBuilder, name_str: []const u8) !TypeVar {
        const id = self.next_type_var_id;
        self.next_type_var_id += 1;
        const name_copy = try self.arena.allocator().dupe(u8, name_str);
        return .{
            .id = id,
            .name = name_copy,
            .bounds = &.{},
        };
    }

    pub fn newTypeVarWithBounds(self: *TypeBuilder, name_str: []const u8, bounds: []const *TraitType) !TypeVar {
        const id = self.next_type_var_id;
        self.next_type_var_id += 1;
        const name_copy = try self.arena.allocator().dupe(u8, name_str);
        const bounds_copy = try self.arena.allocator().dupe(*TraitType, bounds);
        return .{
            .id = id,
            .name = name_copy,
            .bounds = bounds_copy,
        };
    }

    pub fn typeVarType(self: *TypeBuilder, name_str: []const u8) !Type {
        return .{ .type_var = try self.newTypeVar(name_str) };
    }

    // Applied type constructor
    pub fn appliedType(self: *TypeBuilder, base: Type, args: []const Type) !Type {
        const applied = try self.arena.allocator().create(AppliedType);
        const args_copy = try self.arena.allocator().dupe(Type, args);
        applied.* = .{ .base = base, .args = args_copy };
        return .{ .applied = applied };
    }

    // Associated type reference constructor
    pub fn associatedTypeRefType(self: *TypeBuilder, type_var: TypeVar, assoc_name: []const u8, trait: *TraitType) !Type {
        const ref = try self.arena.allocator().create(AssociatedTypeRef);
        const name_copy = try self.arena.allocator().dupe(u8, assoc_name);
        ref.* = .{ .type_var = type_var, .assoc_name = name_copy, .trait = trait };
        return .{ .associated_type_ref = ref };
    }

    // Reference-counted type constructors
    pub fn rcType(self: *TypeBuilder, inner: Type) !Type {
        const rc = try self.arena.allocator().create(RcType);
        rc.* = .{ .inner = inner };
        return .{ .rc = rc };
    }

    pub fn weakRcType(self: *TypeBuilder, inner: Type) !Type {
        const weak = try self.arena.allocator().create(WeakRcType);
        weak.* = .{ .inner = inner };
        return .{ .weak_rc = weak };
    }

    // Atomic reference-counted type constructors
    pub fn arcType(self: *TypeBuilder, inner: Type) !Type {
        const arc = try self.arena.allocator().create(ArcType);
        arc.* = .{ .inner = inner };
        return .{ .arc = arc };
    }

    pub fn weakArcType(self: *TypeBuilder, inner: Type) !Type {
        const weak = try self.arena.allocator().create(WeakArcType);
        weak.* = .{ .inner = inner };
        return .{ .weak_arc = weak };
    }

    // Interior mutability type constructors
    pub fn cellType(self: *TypeBuilder, inner: Type) !Type {
        const cell = try self.arena.allocator().create(CellType);
        cell.* = .{ .inner = inner };
        return .{ .cell = cell };
    }

    // Context error type constructor
    pub fn contextErrorType(self: *TypeBuilder, inner_type: Type) !Type {
        const ctx_err = try self.arena.allocator().create(ContextErrorType);
        ctx_err.* = .{ .inner_type = inner_type };
        return .{ .context_error = ctx_err };
    }

    // Range type constructor
    pub fn rangeType(self: *TypeBuilder, element_type: Type, inclusive: bool) !Type {
        const range = try self.arena.allocator().create(RangeType);
        range.* = .{ .element_type = element_type, .inclusive = inclusive };
        return .{ .range = range };
    }

    // List type constructor
    pub fn listType(self: *TypeBuilder, element: Type) !Type {
        const list = try self.arena.allocator().create(ListType);
        list.* = .{ .element = element };
        return .{ .list = list };
    }

    // Map type constructor
    pub fn mapType(self: *TypeBuilder, key: Type, value: Type) !Type {
        const map = try self.arena.allocator().create(MapType);
        map.* = .{ .key = key, .value = value };
        return .{ .map = map };
    }

    // Set type constructor
    pub fn setType(self: *TypeBuilder, element: Type) !Type {
        const set = try self.arena.allocator().create(SetType);
        set.* = .{ .element = element };
        return .{ .set = set };
    }

    // String data type constructor
    pub fn stringDataType(self: *TypeBuilder) !Type {
        const str = try self.arena.allocator().create(StringDataType);
        str.* = .{};
        return .{ .string_data = str };
    }

    // I/O type constructors
    pub fn fileType(self: *TypeBuilder) Type {
        _ = self;
        return .{ .file = {} };
    }

    pub fn ioErrorType(self: *TypeBuilder) Type {
        _ = self;
        return .{ .io_error = {} };
    }

    pub fn stdoutType(self: *TypeBuilder) Type {
        _ = self;
        return .{ .stdout_handle = {} };
    }

    pub fn stderrType(self: *TypeBuilder) Type {
        _ = self;
        return .{ .stderr_handle = {} };
    }

    pub fn stdinType(self: *TypeBuilder) Type {
        _ = self;
        return .{ .stdin_handle = {} };
    }

    pub fn pathType(self: *TypeBuilder) Type {
        _ = self;
        return .{ .path = {} };
    }

    // Buffered I/O type constructors
    pub fn bufReaderType(self: *TypeBuilder, inner: Type) !Type {
        const br = try self.arena.allocator().create(BufReaderType);
        br.* = .{ .inner = inner };
        return .{ .buf_reader = br };
    }

    pub fn bufWriterType(self: *TypeBuilder, inner: Type) !Type {
        const bw = try self.arena.allocator().create(BufWriterType);
        bw.* = .{ .inner = inner };
        return .{ .buf_writer = bw };
    }

    // FFI pointer type constructors
    pub fn cptrType(self: *TypeBuilder, inner: Type) !Type {
        const cptr = try self.arena.allocator().create(CptrType);
        cptr.* = .{ .inner = inner };
        return .{ .cptr = cptr };
    }

    pub fn coptPtrType(self: *TypeBuilder, inner: Type) !Type {
        const copt_ptr = try self.arena.allocator().create(CoptPtrType);
        copt_ptr.* = .{ .inner = inner };
        return .{ .copt_ptr = copt_ptr };
    }

    pub fn cstrType(self: *TypeBuilder) Type {
        _ = self;
        return .{ .cstr = {} };
    }

    pub fn cstrOwnedType(self: *TypeBuilder) Type {
        _ = self;
        return .{ .cstr_owned = {} };
    }
};

// ============================================================================
// Type Formatting
// ============================================================================

pub fn formatType(writer: anytype, t: Type) !void {
    switch (t) {
        .primitive => |p| try writer.writeAll(p.name()),
        .array => |a| {
            try writer.writeAll("[");
            try formatType(writer, a.element);
            try writer.print("; {d}]", .{a.size});
        },
        .slice => |s| {
            try writer.writeAll("[");
            try formatType(writer, s.element);
            try writer.writeAll("]");
        },
        .tuple => |tup| {
            try writer.writeAll("(");
            for (tup.elements, 0..) |elem, i| {
                if (i > 0) try writer.writeAll(", ");
                try formatType(writer, elem);
            }
            try writer.writeAll(")");
        },
        .optional => |o| {
            try writer.writeAll("?");
            try formatType(writer, o.*);
        },
        .result => |r| {
            try writer.writeAll("Result[");
            try formatType(writer, r.ok_type);
            try writer.writeAll(", ");
            try formatType(writer, r.err_type);
            try writer.writeAll("]");
        },
        .function => |f| {
            try writer.writeAll("fn(");
            for (f.params, 0..) |param, i| {
                if (i > 0) try writer.writeAll(", ");
                try formatType(writer, param);
            }
            try writer.writeAll(") -> ");
            try formatType(writer, f.return_type);
        },
        .reference => |r| {
            if (r.mutable) {
                try writer.writeAll("&mut ");
            } else {
                try writer.writeAll("&");
            }
            try formatType(writer, r.inner);
        },
        .struct_ => |s| try writer.writeAll(s.name),
        .enum_ => |e| try writer.writeAll(e.name),
        .trait_ => |tr| try writer.writeAll(tr.name),
        .type_var => |tv| try writer.writeAll(tv.name),
        .applied => |a| {
            try formatType(writer, a.base);
            try writer.writeAll("[");
            for (a.args, 0..) |arg, i| {
                if (i > 0) try writer.writeAll(", ");
                try formatType(writer, arg);
            }
            try writer.writeAll("]");
        },
        .rc => |r| {
            try writer.writeAll("Rc[");
            try formatType(writer, r.inner);
            try writer.writeAll("]");
        },
        .weak_rc => |w| {
            try writer.writeAll("Weak[");
            try formatType(writer, w.inner);
            try writer.writeAll("]");
        },
        .arc => |a| {
            try writer.writeAll("Arc[");
            try formatType(writer, a.inner);
            try writer.writeAll("]");
        },
        .weak_arc => |w| {
            try writer.writeAll("WeakArc[");
            try formatType(writer, w.inner);
            try writer.writeAll("]");
        },
        .cell => |c| {
            try writer.writeAll("Cell[");
            try formatType(writer, c.inner);
            try writer.writeAll("]");
        },
        .context_error => |ce| {
            try writer.writeAll("ContextError[");
            try formatType(writer, ce.inner_type);
            try writer.writeAll("]");
        },
        .range => |r| {
            try writer.writeAll("Range[");
            try formatType(writer, r.element_type);
            try writer.writeAll("]");
        },
        .list => |l| {
            try writer.writeAll("List[");
            try formatType(writer, l.element);
            try writer.writeAll("]");
        },
        .map => |m| {
            try writer.writeAll("Map[");
            try formatType(writer, m.key);
            try writer.writeAll(", ");
            try formatType(writer, m.value);
            try writer.writeAll("]");
        },
        .set => |s| {
            try writer.writeAll("Set[");
            try formatType(writer, s.element);
            try writer.writeAll("]");
        },
        .string_data => {
            try writer.writeAll("String");
        },
        .file => try writer.writeAll("File"),
        .io_error => try writer.writeAll("IoError"),
        .stdout_handle => try writer.writeAll("Stdout"),
        .stderr_handle => try writer.writeAll("Stderr"),
        .stdin_handle => try writer.writeAll("Stdin"),
        .path => try writer.writeAll("Path"),
        .buf_reader => |br| {
            try writer.writeAll("BufReader[");
            try formatType(writer, br.inner);
            try writer.writeAll("]");
        },
        .buf_writer => |bw| {
            try writer.writeAll("BufWriter[");
            try formatType(writer, bw.inner);
            try writer.writeAll("]");
        },
        .associated_type_ref => |a| {
            try writer.writeAll(a.type_var.name);
            try writer.writeAll(".");
            try writer.writeAll(a.assoc_name);
        },
        .extern_type => |ext| {
            try writer.writeAll(ext.name);
        },
        .cptr => |c| {
            try writer.writeAll("CPtr[");
            try formatType(writer, c.inner);
            try writer.writeAll("]");
        },
        .copt_ptr => |c| {
            try writer.writeAll("COptPtr[");
            try formatType(writer, c.inner);
            try writer.writeAll("]");
        },
        .cstr => try writer.writeAll("CStr"),
        .cstr_owned => try writer.writeAll("CStrOwned"),
        .void_ => try writer.writeAll("void"),
        .never => try writer.writeAll("!"),
        .unknown => try writer.writeAll("?unknown"),
        .error_type => try writer.writeAll("?error"),
    }
}

pub fn typeToString(allocator: Allocator, t: Type) ![]u8 {
    var list: std.ArrayListUnmanaged(u8) = .{};
    errdefer list.deinit(allocator);
    try formatType(list.writer(allocator), t);
    return list.toOwnedSlice(allocator);
}

// ============================================================================
// Tests
// ============================================================================

test "Primitive type properties" {
    const testing = std.testing;

    try testing.expect(Primitive.i32_.isInteger());
    try testing.expect(Primitive.i32_.isSigned());
    try testing.expect(!Primitive.u32_.isSigned());
    try testing.expect(Primitive.f64_.isFloat());
    try testing.expect(!Primitive.bool_.isNumeric());
    try testing.expectEqual(@as(u16, 32), Primitive.i32_.bitSize().?);
    try testing.expectEqualStrings("i32", Primitive.i32_.name());
}

test "Primitive widening" {
    const testing = std.testing;

    // Same type
    try testing.expect(Primitive.i32_.canWidenTo(.i32_));

    // Valid widening
    try testing.expect(Primitive.i32_.canWidenTo(.i64_));
    try testing.expect(Primitive.u8_.canWidenTo(.u16_));
    try testing.expect(Primitive.f32_.canWidenTo(.f64_));

    // Unsigned to larger signed
    try testing.expect(Primitive.u8_.canWidenTo(.i16_));
    try testing.expect(!Primitive.u8_.canWidenTo(.i8_)); // Not enough room

    // Invalid: signed to unsigned
    try testing.expect(!Primitive.i32_.canWidenTo(.u64_));

    // Invalid: narrowing
    try testing.expect(!Primitive.i64_.canWidenTo(.i32_));
}

test "Primitive fromName" {
    const testing = std.testing;

    try testing.expectEqual(Primitive.i32_, Primitive.fromName("i32").?);
    try testing.expectEqual(Primitive.string_, Primitive.fromName("string").?);
    try testing.expect(Primitive.fromName("invalid") == null);
}

test "TypeBuilder basic types" {
    const testing = std.testing;

    var builder = TypeBuilder.init(testing.allocator);
    defer builder.deinit();

    const i32_t = builder.i32Type();
    try testing.expect(i32_t.isInteger());
    try testing.expect(!i32_t.isFloat());

    const bool_t = builder.boolType();
    try testing.expect(!bool_t.isNumeric());
}

test "TypeBuilder composite types" {
    const testing = std.testing;

    var builder = TypeBuilder.init(testing.allocator);
    defer builder.deinit();

    // Array type
    const arr_t = try builder.arrayType(builder.i32Type(), 10);
    try testing.expect(arr_t == .array);
    try testing.expectEqual(@as(usize, 10), arr_t.array.size);

    // Optional type
    const opt_t = try builder.optionalType(builder.stringType());
    try testing.expect(opt_t == .optional);

    // Function type
    const func_t = try builder.functionType(&.{ builder.i32Type(), builder.i32Type() }, builder.i32Type());
    try testing.expect(func_t == .function);
    try testing.expectEqual(@as(usize, 2), func_t.function.params.len);
}

test "Type equality" {
    const testing = std.testing;

    var builder = TypeBuilder.init(testing.allocator);
    defer builder.deinit();

    const i32_1 = builder.i32Type();
    const i32_2 = builder.i32Type();
    const i64_t = builder.i64Type();

    try testing.expect(i32_1.eql(i32_2));
    try testing.expect(!i32_1.eql(i64_t));

    // Composite equality
    const arr1 = try builder.arrayType(builder.i32Type(), 10);
    const arr2 = try builder.arrayType(builder.i32Type(), 10);
    const arr3 = try builder.arrayType(builder.i32Type(), 20);

    try testing.expect(arr1.eql(arr2));
    try testing.expect(!arr1.eql(arr3));
}

test "Type formatting" {
    const testing = std.testing;

    var builder = TypeBuilder.init(testing.allocator);
    defer builder.deinit();

    // Primitive
    const i32_str = try typeToString(testing.allocator, builder.i32Type());
    defer testing.allocator.free(i32_str);
    try testing.expectEqualStrings("i32", i32_str);

    // Optional
    const opt_t = try builder.optionalType(builder.stringType());
    const opt_str = try typeToString(testing.allocator, opt_t);
    defer testing.allocator.free(opt_str);
    try testing.expectEqualStrings("?string", opt_str);

    // Function
    const func_t = try builder.functionType(&.{ builder.i32Type(), builder.i32Type() }, builder.boolType());
    const func_str = try typeToString(testing.allocator, func_t);
    defer testing.allocator.free(func_str);
    try testing.expectEqualStrings("fn(i32, i32) -> bool", func_str);
}

test "isCopyType" {
    const testing = std.testing;

    var builder = TypeBuilder.init(testing.allocator);
    defer builder.deinit();

    // Primitives are Copy
    try testing.expect(builder.i32Type().isCopyType());
    try testing.expect(builder.boolType().isCopyType());
    try testing.expect(builder.f64Type().isCopyType());
    try testing.expect(builder.charType().isCopyType());
    try testing.expect(builder.stringType().isCopyType());

    // References are Copy
    const ref_t = try builder.referenceType(builder.i32Type(), false);
    try testing.expect(ref_t.isCopyType());
    const mut_ref_t = try builder.referenceType(builder.i32Type(), true);
    try testing.expect(mut_ref_t.isCopyType());

    // Arrays of Copy types are Copy
    const arr_copy = try builder.arrayType(builder.i32Type(), 10);
    try testing.expect(arr_copy.isCopyType());

    // Tuples of Copy types are Copy
    const tuple_copy = try builder.tupleType(&.{ builder.i32Type(), builder.boolType() });
    try testing.expect(tuple_copy.isCopyType());

    // Optionals of Copy types are Copy
    const opt_copy = try builder.optionalType(builder.i32Type());
    try testing.expect(opt_copy.isCopyType());

    // void and never are Copy
    try testing.expect(builder.voidType().isCopyType());
    try testing.expect(builder.neverType().isCopyType());

    // Slices are NOT Copy (they may own data)
    const slice_t = try builder.sliceType(builder.i32Type());
    try testing.expect(!slice_t.isCopyType());

    // Function types are NOT Copy
    const func_t = try builder.functionType(&.{builder.i32Type()}, builder.i32Type());
    try testing.expect(!func_t.isCopyType());

    // Rc types are NOT Copy (must use .clone())
    const rc_t = try builder.rcType(builder.i32Type());
    try testing.expect(!rc_t.isCopyType());

    // Weak types are NOT Copy
    const weak_t = try builder.weakRcType(builder.i32Type());
    try testing.expect(!weak_t.isCopyType());
}

test "Rc type creation and equality" {
    const testing = std.testing;

    var builder = TypeBuilder.init(testing.allocator);
    defer builder.deinit();

    // Create Rc types
    const rc_i32 = try builder.rcType(builder.i32Type());
    const rc_i32_2 = try builder.rcType(builder.i32Type());
    const rc_bool = try builder.rcType(builder.boolType());

    try testing.expect(rc_i32.eql(rc_i32_2));
    try testing.expect(!rc_i32.eql(rc_bool));

    // Weak types
    const weak_i32 = try builder.weakRcType(builder.i32Type());
    const weak_i32_2 = try builder.weakRcType(builder.i32Type());
    try testing.expect(weak_i32.eql(weak_i32_2));

    // Rc and Weak are different
    try testing.expect(!rc_i32.eql(weak_i32));
}

test "Rc type formatting" {
    const testing = std.testing;

    var builder = TypeBuilder.init(testing.allocator);
    defer builder.deinit();

    // Rc[i32]
    const rc_t = try builder.rcType(builder.i32Type());
    const rc_str = try typeToString(testing.allocator, rc_t);
    defer testing.allocator.free(rc_str);
    try testing.expectEqualStrings("Rc[i32]", rc_str);

    // Weak[string]
    const weak_t = try builder.weakRcType(builder.stringType());
    const weak_str = try typeToString(testing.allocator, weak_t);
    defer testing.allocator.free(weak_str);
    try testing.expectEqualStrings("Weak[string]", weak_str);

    // Nested: Rc[?i32]
    const opt_i32 = try builder.optionalType(builder.i32Type());
    const rc_opt = try builder.rcType(opt_i32);
    const rc_opt_str = try typeToString(testing.allocator, rc_opt);
    defer testing.allocator.free(rc_opt_str);
    try testing.expectEqualStrings("Rc[?i32]", rc_opt_str);
}

test "Arc type creation and equality" {
    const testing = std.testing;

    var builder = TypeBuilder.init(testing.allocator);
    defer builder.deinit();

    // Create Arc types
    const arc_i32 = try builder.arcType(builder.i32Type());
    const arc_i32_2 = try builder.arcType(builder.i32Type());
    const arc_bool = try builder.arcType(builder.boolType());

    try testing.expect(arc_i32.eql(arc_i32_2));
    try testing.expect(!arc_i32.eql(arc_bool));

    // WeakArc types
    const weak_i32 = try builder.weakArcType(builder.i32Type());
    const weak_i32_2 = try builder.weakArcType(builder.i32Type());
    try testing.expect(weak_i32.eql(weak_i32_2));

    // Arc and WeakArc are different
    try testing.expect(!arc_i32.eql(weak_i32));

    // Arc and Rc are different
    const rc_i32 = try builder.rcType(builder.i32Type());
    try testing.expect(!arc_i32.eql(rc_i32));
}

test "Arc type formatting" {
    const testing = std.testing;

    var builder = TypeBuilder.init(testing.allocator);
    defer builder.deinit();

    // Arc[i32]
    const arc_t = try builder.arcType(builder.i32Type());
    const arc_str = try typeToString(testing.allocator, arc_t);
    defer testing.allocator.free(arc_str);
    try testing.expectEqualStrings("Arc[i32]", arc_str);

    // WeakArc[string]
    const weak_t = try builder.weakArcType(builder.stringType());
    const weak_str = try typeToString(testing.allocator, weak_t);
    defer testing.allocator.free(weak_str);
    try testing.expectEqualStrings("WeakArc[string]", weak_str);

    // Nested: Arc[?i32]
    const opt_i32 = try builder.optionalType(builder.i32Type());
    const arc_opt = try builder.arcType(opt_i32);
    const arc_opt_str = try typeToString(testing.allocator, arc_opt);
    defer testing.allocator.free(arc_opt_str);
    try testing.expectEqualStrings("Arc[?i32]", arc_opt_str);
}

test "Arc is not Copy" {
    const testing = std.testing;

    var builder = TypeBuilder.init(testing.allocator);
    defer builder.deinit();

    // Arc types are NOT Copy (must use .clone())
    const arc_t = try builder.arcType(builder.i32Type());
    try testing.expect(!arc_t.isCopyType());

    // WeakArc types are NOT Copy
    const weak_t = try builder.weakArcType(builder.i32Type());
    try testing.expect(!weak_t.isCopyType());
}

test "CPtr type creation and equality" {
    const testing = std.testing;

    var builder = TypeBuilder.init(testing.allocator);
    defer builder.deinit();

    // Create CPtr types
    const cptr_i32 = try builder.cptrType(builder.i32Type());
    const cptr_i32_2 = try builder.cptrType(builder.i32Type());
    const cptr_bool = try builder.cptrType(builder.boolType());

    try testing.expect(cptr_i32.eql(cptr_i32_2));
    try testing.expect(!cptr_i32.eql(cptr_bool));

    // COptPtr types
    const coptptr_i32 = try builder.coptPtrType(builder.i32Type());
    const coptptr_i32_2 = try builder.coptPtrType(builder.i32Type());
    try testing.expect(coptptr_i32.eql(coptptr_i32_2));

    // CPtr and COptPtr are different types
    try testing.expect(!cptr_i32.eql(coptptr_i32));

    // CStr singleton type
    const cstr1 = builder.cstrType();
    const cstr2 = builder.cstrType();
    try testing.expect(cstr1.eql(cstr2));

    // CStr is different from CPtr[i8]
    const cptr_i8 = try builder.cptrType(.{ .primitive = .i8_ });
    try testing.expect(!cstr1.eql(cptr_i8));
}

test "CPtr type formatting" {
    const testing = std.testing;

    var builder = TypeBuilder.init(testing.allocator);
    defer builder.deinit();

    // CPtr[i32]
    const cptr_t = try builder.cptrType(builder.i32Type());
    const cptr_str = try typeToString(testing.allocator, cptr_t);
    defer testing.allocator.free(cptr_str);
    try testing.expectEqualStrings("CPtr[i32]", cptr_str);

    // COptPtr[string]
    const coptptr_t = try builder.coptPtrType(builder.stringType());
    const coptptr_str = try typeToString(testing.allocator, coptptr_t);
    defer testing.allocator.free(coptptr_str);
    try testing.expectEqualStrings("COptPtr[string]", coptptr_str);

    // CStr
    const cstr_t = builder.cstrType();
    const cstr_str = try typeToString(testing.allocator, cstr_t);
    defer testing.allocator.free(cstr_str);
    try testing.expectEqualStrings("CStr", cstr_str);

    // Nested: CPtr[COptPtr[i32]]
    const inner = try builder.coptPtrType(builder.i32Type());
    const nested = try builder.cptrType(inner);
    const nested_str = try typeToString(testing.allocator, nested);
    defer testing.allocator.free(nested_str);
    try testing.expectEqualStrings("CPtr[COptPtr[i32]]", nested_str);
}

test "CPtr is Copy" {
    const testing = std.testing;

    var builder = TypeBuilder.init(testing.allocator);
    defer builder.deinit();

    // CPtr types ARE Copy (just the address)
    const cptr_t = try builder.cptrType(builder.i32Type());
    try testing.expect(cptr_t.isCopyType());

    // COptPtr types ARE Copy
    const coptptr_t = try builder.coptPtrType(builder.i32Type());
    try testing.expect(coptptr_t.isCopyType());

    // CStr is Copy
    const cstr_t = builder.cstrType();
    try testing.expect(cstr_t.isCopyType());
}
