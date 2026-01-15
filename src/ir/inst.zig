//! Klar IR instruction definitions.
//!
//! The Klar IR is a typed, SSA-form intermediate representation between
//! the typed AST and LLVM IR. It provides a platform-neutral representation
//! that makes ownership tracking and drop insertion straightforward.
//!
//! Design goals:
//! - SSA form: Every value is defined exactly once
//! - Typed: All operations carry type information
//! - Ownership-aware: First-class move, copy, drop, and borrow operations
//! - Platform-neutral: No target-specific details

const std = @import("std");
const Allocator = std.mem.Allocator;
const types = @import("../types.zig");
const ast = @import("../ast.zig");

// ============================================================================
// IR Types
// ============================================================================

/// IR type representation (mirrors types.zig but simplified for codegen).
pub const IrType = union(enum) {
    // Primitive types
    void_,
    bool_,
    i8_,
    i16_,
    i32_,
    i64_,
    i128_,
    u8_,
    u16_,
    u32_,
    u64_,
    u128_,
    isize_,
    usize_,
    f32_,
    f64_,
    char_,

    // Composite types
    ptr: *const IrType, // Pointer to type
    array: ArrayTypeInfo,
    slice: *const IrType, // Slice of element type
    tuple: []const IrType,
    struct_: StructTypeInfo,
    func: FuncTypeInfo,
    optional: *const IrType,

    // Reference types (for borrow tracking)
    ref: RefTypeInfo,
    ref_mut: RefTypeInfo,

    pub const ArrayTypeInfo = struct {
        element: *const IrType,
        size: usize,
    };

    pub const StructTypeInfo = struct {
        name: []const u8,
        fields: []const FieldInfo,
        is_copy: bool,
    };

    pub const FieldInfo = struct {
        name: []const u8,
        ty: IrType,
        offset: usize,
    };

    pub const FuncTypeInfo = struct {
        params: []const IrType,
        return_type: *const IrType,
    };

    pub const RefTypeInfo = struct {
        inner: *const IrType,
    };

    /// Get size in bits (for primitives).
    pub fn bitSize(self: IrType) ?u16 {
        return switch (self) {
            .bool_ => 1,
            .i8_, .u8_ => 8,
            .i16_, .u16_ => 16,
            .i32_, .u32_, .f32_, .char_ => 32,
            .i64_, .u64_, .f64_ => 64,
            .i128_, .u128_ => 128,
            .ptr, .ref, .ref_mut, .slice => 64, // Pointer size (assuming 64-bit)
            else => null,
        };
    }

    /// Check if this type implements Copy (bitwise copyable).
    pub fn isCopy(self: IrType) bool {
        return switch (self) {
            .void_, .bool_, .i8_, .i16_, .i32_, .i64_, .i128_ => true,
            .u8_, .u16_, .u32_, .u64_, .u128_ => true,
            .isize_, .usize_, .f32_, .f64_, .char_ => true,
            .ref, .ref_mut => true, // References are Copy
            .ptr => false, // Raw pointers need special handling
            .array => |arr| arr.element.isCopy(),
            .tuple => |elems| {
                for (elems) |elem| {
                    if (!elem.isCopy()) return false;
                }
                return true;
            },
            .struct_ => |s| s.is_copy,
            else => false,
        };
    }

    /// Check if this is a numeric type.
    pub fn isNumeric(self: IrType) bool {
        return self.isInteger() or self.isFloat();
    }

    /// Check if this is an integer type.
    pub fn isInteger(self: IrType) bool {
        return switch (self) {
            .i8_, .i16_, .i32_, .i64_, .i128_ => true,
            .u8_, .u16_, .u32_, .u64_, .u128_ => true,
            .isize_, .usize_ => true,
            else => false,
        };
    }

    /// Check if this is a floating-point type.
    pub fn isFloat(self: IrType) bool {
        return switch (self) {
            .f32_, .f64_ => true,
            else => false,
        };
    }

    /// Check if this is a signed type.
    pub fn isSigned(self: IrType) bool {
        return switch (self) {
            .i8_, .i16_, .i32_, .i64_, .i128_, .isize_ => true,
            .f32_, .f64_ => true,
            else => false,
        };
    }
};

// ============================================================================
// Values (SSA references)
// ============================================================================

/// A reference to an SSA value.
pub const Value = struct {
    /// Unique ID within the function.
    id: u32,
    /// Type of the value.
    ty: IrType,

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("%{d}", .{self.id});
    }
};

/// A constant value.
pub const Constant = union(enum) {
    int: IntConst,
    float: FloatConst,
    bool_: bool,
    void_,
    null_,
    undef,
    string: []const u8,

    pub const IntConst = struct {
        value: i128,
        ty: IrType,
    };

    pub const FloatConst = struct {
        value: f64,
        ty: IrType,
    };
};

// ============================================================================
// Instructions
// ============================================================================

/// An IR instruction.
pub const Inst = struct {
    /// The operation being performed.
    op: Op,
    /// Result value (null for terminators and void ops).
    result: ?Value,
    /// Source location for debugging.
    span: ?ast.Span,

    /// Instruction opcodes.
    pub const Op = union(enum) {
        // ====================================================================
        // Constants
        // ====================================================================
        /// Load a constant value.
        constant: Constant,

        // ====================================================================
        // Arithmetic (checked - trap on overflow)
        // ====================================================================
        /// Integer addition (checked).
        add: BinaryOp,
        /// Integer subtraction (checked).
        sub: BinaryOp,
        /// Integer multiplication (checked).
        mul: BinaryOp,
        /// Signed division.
        sdiv: BinaryOp,
        /// Unsigned division.
        udiv: BinaryOp,
        /// Signed remainder.
        srem: BinaryOp,
        /// Unsigned remainder.
        urem: BinaryOp,

        // ====================================================================
        // Arithmetic (wrapping - no trap)
        // ====================================================================
        /// Integer addition (wrapping).
        add_wrap: BinaryOp,
        /// Integer subtraction (wrapping).
        sub_wrap: BinaryOp,
        /// Integer multiplication (wrapping).
        mul_wrap: BinaryOp,

        // ====================================================================
        // Arithmetic (saturating)
        // ====================================================================
        /// Integer addition (saturating).
        add_sat: BinaryOp,
        /// Integer subtraction (saturating).
        sub_sat: BinaryOp,
        /// Integer multiplication (saturating).
        mul_sat: BinaryOp,

        // ====================================================================
        // Floating-point arithmetic
        // ====================================================================
        /// Float addition.
        fadd: BinaryOp,
        /// Float subtraction.
        fsub: BinaryOp,
        /// Float multiplication.
        fmul: BinaryOp,
        /// Float division.
        fdiv: BinaryOp,
        /// Float remainder.
        frem: BinaryOp,

        // ====================================================================
        // Unary operations
        // ====================================================================
        /// Integer negation.
        neg: UnaryOp,
        /// Floating-point negation.
        fneg: UnaryOp,
        /// Logical not (bool).
        not: UnaryOp,

        // ====================================================================
        // Comparison
        // ====================================================================
        /// Integer comparison.
        icmp: CmpOp,
        /// Float comparison.
        fcmp: CmpOp,

        // ====================================================================
        // Bitwise operations
        // ====================================================================
        /// Bitwise AND.
        bit_and: BinaryOp,
        /// Bitwise OR.
        bit_or: BinaryOp,
        /// Bitwise XOR.
        bit_xor: BinaryOp,
        /// Shift left.
        shl: BinaryOp,
        /// Arithmetic shift right.
        ashr: BinaryOp,
        /// Logical shift right.
        lshr: BinaryOp,

        // ====================================================================
        // Type conversions
        // ====================================================================
        /// Sign-extend integer.
        sext: CastOp,
        /// Zero-extend integer.
        zext: CastOp,
        /// Truncate integer.
        trunc: CastOp,
        /// Float to larger float.
        fpext: CastOp,
        /// Float to smaller float.
        fptrunc: CastOp,
        /// Signed int to float.
        sitofp: CastOp,
        /// Unsigned int to float.
        uitofp: CastOp,
        /// Float to signed int.
        fptosi: CastOp,
        /// Float to unsigned int.
        fptoui: CastOp,
        /// Pointer to int.
        ptrtoint: CastOp,
        /// Int to pointer.
        inttoptr: CastOp,
        /// Bitcast (same size, different type).
        bitcast: CastOp,

        // ====================================================================
        // Memory operations (stack)
        // ====================================================================
        /// Stack allocation.
        alloca: AllocaOp,
        /// Load from memory.
        load: LoadOp,
        /// Store to memory.
        store: StoreOp,

        // ====================================================================
        // Memory operations (heap)
        // ====================================================================
        /// Heap allocation.
        heap_alloc: HeapAllocOp,
        /// Heap deallocation.
        heap_free: UnaryOp,

        // ====================================================================
        // Aggregate operations
        // ====================================================================
        /// Get element pointer (for arrays, structs).
        gep: GepOp,
        /// Extract value from aggregate.
        extract_value: ExtractOp,
        /// Insert value into aggregate.
        insert_value: InsertOp,

        // ====================================================================
        // Function calls
        // ====================================================================
        /// Direct function call.
        call: CallOp,
        /// Indirect function call (through pointer).
        call_indirect: CallIndirectOp,

        // ====================================================================
        // Ownership operations
        // ====================================================================
        /// Move ownership (source becomes invalid).
        move: MoveOp,
        /// Copy value (for Copy types).
        copy: UnaryOp,
        /// Drop value (call destructor).
        drop: DropOp,
        /// Create immutable borrow.
        borrow: UnaryOp,
        /// Create mutable borrow.
        borrow_mut: UnaryOp,

        // ====================================================================
        // Reference counting (Rc/Arc)
        // ====================================================================
        /// Increment reference count.
        rc_inc: UnaryOp,
        /// Decrement reference count (may free).
        rc_dec: UnaryOp,

        // ====================================================================
        // Optional operations
        // ====================================================================
        /// Wrap value in Some.
        some: UnaryOp,
        /// Create None value.
        none: NoneOp,
        /// Unwrap optional (trap if None).
        unwrap: UnaryOp,
        /// Check if optional is Some.
        is_some: UnaryOp,
        /// Check if optional is None.
        is_none: UnaryOp,

        // ====================================================================
        // Control flow (terminators)
        // ====================================================================
        /// Unconditional branch.
        br: BranchOp,
        /// Conditional branch.
        cond_br: CondBranchOp,
        /// Return from function.
        ret: RetOp,
        /// Return void from function.
        ret_void,
        /// Unreachable (trap).
        unreachable_,

        // ====================================================================
        // Phi nodes (SSA merge)
        // ====================================================================
        /// Phi node for SSA merge at control flow join points.
        phi: PhiOp,
    };

    /// Binary operation operands.
    pub const BinaryOp = struct {
        lhs: Value,
        rhs: Value,
    };

    /// Unary operation operand.
    pub const UnaryOp = struct {
        operand: Value,
    };

    /// Comparison operation.
    pub const CmpOp = struct {
        pred: CmpPred,
        lhs: Value,
        rhs: Value,
    };

    /// Comparison predicate.
    pub const CmpPred = enum {
        eq,
        ne,
        slt,
        sle,
        sgt,
        sge,
        ult,
        ule,
        ugt,
        uge,
        // Float predicates
        oeq, // Ordered equal
        one, // Ordered not equal
        olt, // Ordered less than
        ole, // Ordered less or equal
        ogt, // Ordered greater than
        oge, // Ordered greater or equal
        ord, // Ordered (neither is NaN)
        uno, // Unordered (either is NaN)
    };

    /// Type cast operation.
    pub const CastOp = struct {
        operand: Value,
        target_ty: IrType,
    };

    /// Stack allocation.
    pub const AllocaOp = struct {
        ty: IrType,
        count: ?Value, // For array alloca
        name: ?[]const u8,
    };

    /// Load operation.
    pub const LoadOp = struct {
        ptr: Value,
        ty: IrType,
    };

    /// Store operation.
    pub const StoreOp = struct {
        value: Value,
        ptr: Value,
    };

    /// Heap allocation.
    pub const HeapAllocOp = struct {
        size: Value,
        align_: u32,
    };

    /// Get element pointer operation.
    pub const GepOp = struct {
        base: Value,
        indices: []const Value,
        result_ty: IrType,
    };

    /// Extract value from aggregate.
    pub const ExtractOp = struct {
        aggregate: Value,
        index: u32,
    };

    /// Insert value into aggregate.
    pub const InsertOp = struct {
        aggregate: Value,
        value: Value,
        index: u32,
    };

    /// Function call.
    pub const CallOp = struct {
        func_name: []const u8,
        args: []const Value,
        return_ty: IrType,
    };

    /// Indirect function call.
    pub const CallIndirectOp = struct {
        callee: Value,
        args: []const Value,
        return_ty: IrType,
    };

    /// Move operation.
    pub const MoveOp = struct {
        src: Value,
        dst_name: ?[]const u8,
    };

    /// Drop operation.
    pub const DropOp = struct {
        value: Value,
        ty: IrType,
    };

    /// None (empty optional) operation.
    pub const NoneOp = struct {
        ty: IrType, // The optional type
    };

    /// Unconditional branch.
    pub const BranchOp = struct {
        target: BlockId,
    };

    /// Conditional branch.
    pub const CondBranchOp = struct {
        cond: Value,
        then_target: BlockId,
        else_target: BlockId,
    };

    /// Return operation.
    pub const RetOp = struct {
        value: ?Value,
    };

    /// Phi node.
    pub const PhiOp = struct {
        incoming: []const PhiIncoming,
    };

    pub const PhiIncoming = struct {
        value: Value,
        block: BlockId,
    };
};

// ============================================================================
// Basic Blocks
// ============================================================================

/// Block identifier.
pub const BlockId = u32;

/// A basic block in the IR.
pub const BasicBlock = struct {
    /// Block identifier.
    id: BlockId,
    /// Optional name for debugging.
    name: ?[]const u8,
    /// Instructions in the block.
    instructions: std.ArrayListUnmanaged(Inst),
    /// Predecessor blocks.
    predecessors: std.ArrayListUnmanaged(BlockId),
    /// Successor blocks (determined by terminator).
    successors: std.ArrayListUnmanaged(BlockId),

    pub fn init(id: BlockId, name: ?[]const u8) BasicBlock {
        return .{
            .id = id,
            .name = name,
            .instructions = .{},
            .predecessors = .{},
            .successors = .{},
        };
    }

    pub fn deinit(self: *BasicBlock, allocator: Allocator) void {
        self.instructions.deinit(allocator);
        self.predecessors.deinit(allocator);
        self.successors.deinit(allocator);
    }

    /// Check if block is terminated.
    pub fn isTerminated(self: BasicBlock) bool {
        if (self.instructions.items.len == 0) return false;
        const last = self.instructions.items[self.instructions.items.len - 1];
        return switch (last.op) {
            .br, .cond_br, .ret, .ret_void, .unreachable_ => true,
            else => false,
        };
    }
};

// ============================================================================
// Functions
// ============================================================================

/// A function parameter.
pub const Param = struct {
    name: []const u8,
    ty: IrType,
    /// SSA value for this parameter.
    value: Value,
};

/// An IR function.
pub const Function = struct {
    /// Function name.
    name: []const u8,
    /// Parameters.
    params: []const Param,
    /// Return type.
    return_ty: IrType,
    /// Basic blocks (first is entry).
    blocks: std.ArrayListUnmanaged(BasicBlock),
    /// Next value ID for SSA.
    next_value_id: u32,
    /// Next block ID.
    next_block_id: BlockId,
    /// Is this function extern (no body)?
    is_extern: bool,

    pub fn init(allocator: Allocator, name: []const u8, params: []const Param, return_ty: IrType) !Function {
        var func = Function{
            .name = name,
            .params = params,
            .return_ty = return_ty,
            .blocks = .{},
            .next_value_id = @intCast(params.len),
            .next_block_id = 0,
            .is_extern = false,
        };

        // Create entry block
        _ = try func.addBlock(allocator, "entry");

        return func;
    }

    pub fn deinit(self: *Function, allocator: Allocator) void {
        for (self.blocks.items) |*block| {
            block.deinit(allocator);
        }
        self.blocks.deinit(allocator);
    }

    /// Add a new basic block.
    pub fn addBlock(self: *Function, allocator: Allocator, name: ?[]const u8) !BlockId {
        const id = self.next_block_id;
        self.next_block_id += 1;
        try self.blocks.append(allocator, BasicBlock.init(id, name));
        return id;
    }

    /// Get block by ID.
    pub fn getBlock(self: *Function, id: BlockId) ?*BasicBlock {
        for (self.blocks.items) |*block| {
            if (block.id == id) return block;
        }
        return null;
    }

    /// Get entry block.
    pub fn entryBlock(self: *Function) ?*BasicBlock {
        if (self.blocks.items.len == 0) return null;
        return &self.blocks.items[0];
    }

    /// Generate a new SSA value.
    pub fn newValue(self: *Function, ty: IrType) Value {
        const id = self.next_value_id;
        self.next_value_id += 1;
        return .{ .id = id, .ty = ty };
    }
};

// ============================================================================
// Module
// ============================================================================

/// An IR module (compilation unit).
pub const Module = struct {
    /// Module name.
    name: []const u8,
    /// Functions in the module.
    functions: std.ArrayListUnmanaged(Function),
    /// Global constants (strings, etc.).
    globals: std.ArrayListUnmanaged(Global),
    /// Struct type definitions.
    struct_types: std.StringHashMapUnmanaged(IrType.StructTypeInfo),
    /// Allocator for IR construction.
    allocator: Allocator,

    pub const Global = struct {
        name: []const u8,
        ty: IrType,
        init: ?Constant,
        is_const: bool,
    };

    pub fn init(allocator: Allocator, name: []const u8) Module {
        return .{
            .name = name,
            .functions = .{},
            .globals = .{},
            .struct_types = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Module) void {
        for (self.functions.items) |*func| {
            func.deinit(self.allocator);
        }
        self.functions.deinit(self.allocator);
        self.globals.deinit(self.allocator);
        self.struct_types.deinit(self.allocator);
    }

    /// Add a function to the module.
    pub fn addFunction(self: *Module, func: Function) !void {
        try self.functions.append(self.allocator, func);
    }

    /// Get a function by name.
    pub fn getFunction(self: *Module, name: []const u8) ?*Function {
        for (self.functions.items) |*func| {
            if (std.mem.eql(u8, func.name, name)) return func;
        }
        return null;
    }

    /// Add a global variable.
    pub fn addGlobal(self: *Module, global: Global) !void {
        try self.globals.append(self.allocator, global);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "IrType properties" {
    const testing = std.testing;

    const i32_ty: IrType = .i32_;
    const u32_ty: IrType = .u32_;
    const f64_ty: IrType = .f64_;
    const bool_ty: IrType = .bool_;

    try testing.expect(i32_ty.isInteger());
    try testing.expect(i32_ty.isSigned());
    try testing.expect(!u32_ty.isSigned());
    try testing.expect(f64_ty.isFloat());
    try testing.expect(!bool_ty.isNumeric());
    try testing.expect(i32_ty.isCopy());
    try testing.expect(bool_ty.isCopy());
    try testing.expectEqual(@as(u16, 32), i32_ty.bitSize().?);
}

test "Value formatting" {
    const testing = std.testing;

    const val = Value{ .id = 42, .ty = .i32_ };
    var buf: [256]u8 = undefined;
    const formatted = std.fmt.bufPrint(&buf, "{any}", .{val}) catch unreachable;
    // Check that the formatted output contains the ID
    try testing.expect(std.mem.indexOf(u8, formatted, "42") != null);
}

test "BasicBlock termination" {
    const testing = std.testing;

    var block = BasicBlock.init(0, "test");
    defer block.deinit(testing.allocator);

    try testing.expect(!block.isTerminated());

    try block.instructions.append(testing.allocator, .{
        .op = .{ .br = .{ .target = 1 } },
        .result = null,
        .span = null,
    });

    try testing.expect(block.isTerminated());
}

test "Function value generation" {
    const testing = std.testing;

    var func = try Function.init(testing.allocator, "test", &.{}, .void_);
    defer func.deinit(testing.allocator);

    const v1 = func.newValue(.i32_);
    const v2 = func.newValue(.i64_);

    try testing.expectEqual(@as(u32, 0), v1.id);
    try testing.expectEqual(@as(u32, 1), v2.id);
}

test "Module function lookup" {
    const testing = std.testing;

    var module = Module.init(testing.allocator, "test");
    defer module.deinit();

    const func = try Function.init(testing.allocator, "my_func", &.{}, .void_);
    try module.addFunction(func);

    const found = module.getFunction("my_func");
    try testing.expect(found != null);
    try testing.expectEqualStrings("my_func", found.?.name);

    const not_found = module.getFunction("other");
    try testing.expect(not_found == null);
}
