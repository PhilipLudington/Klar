const std = @import("std");
const Allocator = std.mem.Allocator;
const values = @import("values.zig");

// ============================================================================
// Opcode Definitions
// ============================================================================

/// Bytecode opcodes for the Klar VM.
/// Single-byte opcodes with variable-length operands.
pub const OpCode = enum(u8) {
    // ------------------------------------------------------------------------
    // Stack Manipulation (1.2)
    // ------------------------------------------------------------------------

    /// Push constant from pool onto stack.
    /// Operand: u16 constant pool index
    op_const,

    /// Push constant from pool with extended index.
    /// Operand: u24 constant pool index (for large constant pools)
    op_const_long,

    /// Pop and discard top of stack
    op_pop,

    /// Duplicate top of stack
    op_dup,

    /// Swap top two values on stack
    op_swap,

    // ------------------------------------------------------------------------
    // Immediate Values (optimized short forms)
    // ------------------------------------------------------------------------

    /// Push true onto stack
    op_true,

    /// Push false onto stack
    op_false,

    /// Push void onto stack
    op_void,

    /// Push small integer (-1 to 5) onto stack
    /// These are common enough to warrant dedicated opcodes
    op_int_neg1,
    op_int_0,
    op_int_1,
    op_int_2,
    op_int_3,
    op_int_4,
    op_int_5,

    // ------------------------------------------------------------------------
    // Arithmetic Operations (1.3)
    // ------------------------------------------------------------------------

    /// Pop two values, push sum (checked - trap on overflow)
    op_add,
    /// Pop two values, push difference (checked)
    op_sub,
    /// Pop two values, push product (checked)
    op_mul,
    /// Pop two values, push quotient (checked)
    op_div,
    /// Pop two values, push remainder (checked)
    op_mod,

    /// Wrapping arithmetic (wrap on overflow)
    op_add_wrap,
    op_sub_wrap,
    op_mul_wrap,

    /// Saturating arithmetic (clamp to min/max on overflow)
    op_add_sat,
    op_sub_sat,
    op_mul_sat,

    /// Unary negation (pop value, push negated)
    op_neg,

    // ------------------------------------------------------------------------
    // Comparison Operations (1.4)
    // ------------------------------------------------------------------------

    /// Pop two values, push true if equal
    op_eq,
    /// Pop two values, push true if not equal
    op_ne,
    /// Pop two values, push true if left < right
    op_lt,
    /// Pop two values, push true if left > right
    op_gt,
    /// Pop two values, push true if left <= right
    op_le,
    /// Pop two values, push true if left >= right
    op_ge,

    // ------------------------------------------------------------------------
    // Logical Operations (1.4)
    // ------------------------------------------------------------------------

    /// Pop value, push logical negation
    op_not,

    /// Short-circuit AND: if TOS is false, jump forward
    /// Operand: u16 jump offset
    op_and,

    /// Short-circuit OR: if TOS is true, jump forward
    /// Operand: u16 jump offset
    op_or,

    // ------------------------------------------------------------------------
    // Bitwise Operations (1.5)
    // ------------------------------------------------------------------------

    /// Pop two values, push bitwise AND
    op_bit_and,
    /// Pop two values, push bitwise OR
    op_bit_or,
    /// Pop two values, push bitwise XOR
    op_bit_xor,
    /// Pop value, push bitwise NOT
    op_bit_not,
    /// Pop two values, push left shifted by right
    op_shl,
    /// Pop two values, push left shifted right by right
    op_shr,

    // ------------------------------------------------------------------------
    // Control Flow (1.6)
    // ------------------------------------------------------------------------

    /// Unconditional jump forward.
    /// Operand: u16 offset
    op_jump,

    /// Unconditional jump backward (for loops).
    /// Operand: u16 offset
    op_loop,

    /// Jump if TOS is false, pop the value.
    /// Operand: u16 offset
    op_jump_if_false,

    /// Jump if TOS is false, DON'T pop (for match guards).
    /// Operand: u16 offset
    op_jump_if_false_no_pop,

    /// Jump if TOS is true, pop the value.
    /// Operand: u16 offset
    op_jump_if_true,

    /// Jump if TOS is true, DON'T pop (for short-circuit `or`).
    /// Operand: u16 offset
    op_jump_if_true_no_pop,

    // ------------------------------------------------------------------------
    // Variable Access (1.7)
    // ------------------------------------------------------------------------

    /// Read local variable by slot index.
    /// Operand: u8 slot index
    op_get_local,

    /// Write local variable by slot index.
    /// Operand: u8 slot index
    op_set_local,

    /// Read local variable with extended index.
    /// Operand: u16 slot index
    op_get_local_long,

    /// Write local variable with extended index.
    /// Operand: u16 slot index
    op_set_local_long,

    /// Read global variable by name.
    /// Operand: u16 constant pool index (name string)
    op_get_global,

    /// Write global variable by name.
    /// Operand: u16 constant pool index (name string)
    op_set_global,

    /// Define global variable.
    /// Operand: u16 constant pool index (name string)
    op_define_global,

    /// Read upvalue (for closures).
    /// Operand: u8 upvalue index
    op_get_upvalue,

    /// Write upvalue (for closures).
    /// Operand: u8 upvalue index
    op_set_upvalue,

    /// Close upvalue, moving it from stack to heap.
    op_close_upvalue,

    // ------------------------------------------------------------------------
    // Function Operations (1.8)
    // ------------------------------------------------------------------------

    /// Call function with N arguments.
    /// Operand: u8 argument count
    op_call,

    /// Tail call optimization.
    /// Operand: u8 argument count
    op_tail_call,

    /// Return from function (return value on TOS).
    op_return,

    /// Return void from function.
    op_return_void,

    /// Create closure capturing upvalues.
    /// Operand: u16 constant pool index (function)
    /// Followed by upvalue descriptors: (is_local: u8, index: u8) pairs
    op_closure,

    // ------------------------------------------------------------------------
    // Composite Types (1.9)
    // ------------------------------------------------------------------------

    /// Create array from N stack values.
    /// Operand: u16 element count
    op_array,

    /// Create tuple from N stack values.
    /// Operand: u8 element count
    op_tuple,

    /// Create struct with field names.
    /// Operand: u16 constant pool index (struct info)
    op_struct,

    /// Array/tuple index access (object and index on stack).
    op_get_index,

    /// Array/tuple index assignment (object, index, value on stack).
    op_set_index,

    /// Struct field access.
    /// Operand: u16 constant pool index (field name)
    op_get_field,

    /// Struct field assignment.
    /// Operand: u16 constant pool index (field name)
    op_set_field,

    /// Create empty array with capacity.
    /// Operand: u16 capacity
    op_array_empty,

    /// Append to array (array and value on stack).
    op_array_push,

    // ------------------------------------------------------------------------
    // Optional/Result Types (1.10)
    // ------------------------------------------------------------------------

    /// Wrap TOS in Some
    op_some,

    /// Push None onto stack
    op_none,

    /// Unwrap optional (panic on None)
    op_unwrap,

    /// Unwrap optional with default (optional, default on stack)
    op_unwrap_or,

    /// Check if TOS is None (push bool)
    op_is_none,

    /// Check if TOS is Some (push bool)
    op_is_some,

    /// Null coalescing: if TOS is None, pop and use next value
    op_null_coalesce,

    // ------------------------------------------------------------------------
    // Type Operations
    // ------------------------------------------------------------------------

    /// Type cast (checked) - converts value to target type, errors on overflow.
    /// Operand: u8 target type tag
    op_cast,

    /// Type cast (truncating) - converts value to target type, wraps on overflow.
    /// Operand: u8 target type tag
    op_cast_trunc,

    /// Get type of TOS as string
    op_type_of,

    /// Check if TOS is of type.
    /// Operand: u16 constant pool index (type descriptor)
    op_is_type,

    // ------------------------------------------------------------------------
    // String Operations
    // ------------------------------------------------------------------------

    /// Concatenate two strings
    op_concat,

    /// Build string from N parts (for interpolation).
    /// Operand: u8 part count
    op_string_build,

    // ------------------------------------------------------------------------
    // Method Calls
    // ------------------------------------------------------------------------

    /// Call method on object.
    /// Operand: u16 constant pool index (method name), u8 arg count
    op_invoke,

    /// Call super method.
    /// Operand: u16 constant pool index (method name), u8 arg count
    op_invoke_super,

    // ------------------------------------------------------------------------
    // Pattern Matching
    // ------------------------------------------------------------------------

    /// Test if TOS matches a literal value.
    /// Operand: u16 constant pool index
    op_match_literal,

    /// Test if TOS is an enum variant.
    /// Operand: u16 constant pool index (variant name)
    op_match_variant,

    /// Bind TOS to a pattern variable (duplicate).
    op_match_bind,

    // ------------------------------------------------------------------------
    // Range Operations
    // ------------------------------------------------------------------------

    /// Create inclusive range from two values
    op_range_inclusive,

    /// Create exclusive range from two values
    op_range_exclusive,

    /// Check if value is in range
    op_in_range,

    // ------------------------------------------------------------------------
    // Iteration
    // ------------------------------------------------------------------------

    /// Get iterator from iterable
    op_get_iter,

    /// Get next value from iterator (pushes value and bool)
    op_iter_next,

    // ------------------------------------------------------------------------
    // Debug/Introspection
    // ------------------------------------------------------------------------

    /// Print TOS without newline
    op_print,

    /// Print TOS with newline
    op_println,

    /// Assert TOS is true
    op_assert,

    /// Panic with message on TOS
    op_panic,

    /// No-op (for padding/debugging)
    op_nop,

    /// Returns the number of bytes for this opcode's operands
    pub fn operandBytes(self: OpCode) u8 {
        return switch (self) {
            // No operands
            .op_pop,
            .op_dup,
            .op_swap,
            .op_true,
            .op_false,
            .op_void,
            .op_int_neg1,
            .op_int_0,
            .op_int_1,
            .op_int_2,
            .op_int_3,
            .op_int_4,
            .op_int_5,
            .op_add,
            .op_sub,
            .op_mul,
            .op_div,
            .op_mod,
            .op_add_wrap,
            .op_sub_wrap,
            .op_mul_wrap,
            .op_add_sat,
            .op_sub_sat,
            .op_mul_sat,
            .op_neg,
            .op_eq,
            .op_ne,
            .op_lt,
            .op_gt,
            .op_le,
            .op_ge,
            .op_not,
            .op_bit_and,
            .op_bit_or,
            .op_bit_xor,
            .op_bit_not,
            .op_shl,
            .op_shr,
            .op_get_index,
            .op_set_index,
            .op_some,
            .op_none,
            .op_unwrap,
            .op_unwrap_or,
            .op_is_none,
            .op_is_some,
            .op_null_coalesce,
            .op_type_of,
            .op_concat,
            .op_match_bind,
            .op_range_inclusive,
            .op_range_exclusive,
            .op_in_range,
            .op_get_iter,
            .op_iter_next,
            .op_print,
            .op_println,
            .op_assert,
            .op_panic,
            .op_return,
            .op_return_void,
            .op_close_upvalue,
            .op_array_push,
            .op_nop,
            => 0,

            // 1-byte operand
            .op_get_local,
            .op_set_local,
            .op_get_upvalue,
            .op_set_upvalue,
            .op_call,
            .op_tail_call,
            .op_tuple,
            .op_cast,
            .op_cast_trunc,
            .op_string_build,
            => 1,

            // 2-byte operand
            .op_const,
            .op_and,
            .op_or,
            .op_jump,
            .op_loop,
            .op_jump_if_false,
            .op_jump_if_false_no_pop,
            .op_jump_if_true,
            .op_jump_if_true_no_pop,
            .op_get_local_long,
            .op_set_local_long,
            .op_get_global,
            .op_set_global,
            .op_define_global,
            .op_closure,
            .op_array,
            .op_struct,
            .op_get_field,
            .op_set_field,
            .op_array_empty,
            .op_is_type,
            .op_match_literal,
            .op_match_variant,
            => 2,

            // 3-byte operand
            .op_const_long,
            .op_invoke,
            .op_invoke_super,
            => 3,
        };
    }
};

// ============================================================================
// Type Tags for Casts
// ============================================================================

/// Type tags used as operands for cast operations.
pub const TypeTag = enum(u8) {
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

    // Other
    bool_,
    char_,
    string_,

    pub fn fromIntegerType(int_type: values.Integer.IntegerType) TypeTag {
        return switch (int_type) {
            .i8_ => .i8_,
            .i16_ => .i16_,
            .i32_ => .i32_,
            .i64_ => .i64_,
            .i128_ => .i128_,
            .isize_ => .isize_,
            .u8_ => .u8_,
            .u16_ => .u16_,
            .u32_ => .u32_,
            .u64_ => .u64_,
            .u128_ => .u128_,
            .usize_ => .usize_,
        };
    }

    pub fn toIntegerType(self: TypeTag) ?values.Integer.IntegerType {
        return switch (self) {
            .i8_ => .i8_,
            .i16_ => .i16_,
            .i32_ => .i32_,
            .i64_ => .i64_,
            .i128_ => .i128_,
            .isize_ => .isize_,
            .u8_ => .u8_,
            .u16_ => .u16_,
            .u32_ => .u32_,
            .u64_ => .u64_,
            .u128_ => .u128_,
            .usize_ => .usize_,
            else => null,
        };
    }

    pub fn toFloatType(self: TypeTag) ?values.Float.FloatType {
        return switch (self) {
            .f32_ => .f32_,
            .f64_ => .f64_,
            else => null,
        };
    }
};

// ============================================================================
// Upvalue Descriptor
// ============================================================================

/// Describes how to capture an upvalue when creating a closure.
pub const UpvalueDescriptor = struct {
    /// If true, capture from enclosing function's locals.
    /// If false, capture from enclosing function's upvalues.
    is_local: bool,

    /// Index into locals or upvalues array.
    index: u8,
};

// ============================================================================
// Tests
// ============================================================================

test "OpCode operand bytes" {
    const testing = std.testing;

    // No operands
    try testing.expectEqual(@as(u8, 0), OpCode.op_add.operandBytes());
    try testing.expectEqual(@as(u8, 0), OpCode.op_pop.operandBytes());
    try testing.expectEqual(@as(u8, 0), OpCode.op_return.operandBytes());

    // 1-byte operands
    try testing.expectEqual(@as(u8, 1), OpCode.op_get_local.operandBytes());
    try testing.expectEqual(@as(u8, 1), OpCode.op_call.operandBytes());

    // 2-byte operands
    try testing.expectEqual(@as(u8, 2), OpCode.op_const.operandBytes());
    try testing.expectEqual(@as(u8, 2), OpCode.op_jump.operandBytes());
    try testing.expectEqual(@as(u8, 2), OpCode.op_get_global.operandBytes());

    // 3-byte operands
    try testing.expectEqual(@as(u8, 3), OpCode.op_const_long.operandBytes());
    try testing.expectEqual(@as(u8, 3), OpCode.op_invoke.operandBytes());
}

test "TypeTag conversions" {
    const testing = std.testing;

    // Integer type round-trip
    const int_type = values.Integer.IntegerType.i32_;
    const tag = TypeTag.fromIntegerType(int_type);
    try testing.expectEqual(TypeTag.i32_, tag);
    try testing.expectEqual(int_type, tag.toIntegerType().?);

    // Float type
    const f64_tag = TypeTag.f64_;
    try testing.expectEqual(values.Float.FloatType.f64_, f64_tag.toFloatType().?);
    try testing.expect(f64_tag.toIntegerType() == null);
}
