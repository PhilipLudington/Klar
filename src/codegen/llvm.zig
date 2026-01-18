//! LLVM C API wrappers.
//!
//! Provides type-safe Zig wrappers for LLVM-C types and functions.

const std = @import("std");

pub const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/Target.h");
    @cInclude("llvm-c/TargetMachine.h");
    @cInclude("llvm-c/Analysis.h");
    @cInclude("llvm-c/DebugInfo.h");
});

// Re-export opaque pointer types
pub const ContextRef = c.LLVMContextRef;
pub const ModuleRef = c.LLVMModuleRef;
pub const BuilderRef = c.LLVMBuilderRef;
pub const TypeRef = c.LLVMTypeRef;
pub const ValueRef = c.LLVMValueRef;
pub const BasicBlockRef = c.LLVMBasicBlockRef;
pub const TargetRef = c.LLVMTargetRef;
pub const TargetMachineRef = c.LLVMTargetMachineRef;
pub const TargetDataRef = c.LLVMTargetDataRef;
pub const DIBuilderRef = c.LLVMDIBuilderRef;
pub const MetadataRef = c.LLVMMetadataRef;

/// Errors that can occur during LLVM operations.
pub const Error = error{
    ModuleVerificationFailed,
    TargetNotFound,
    TargetMachineCreationFailed,
    EmitFailed,
};

/// Initialize LLVM targets for the host platform.
pub fn initializeNativeTarget() void {
    _ = c.LLVMInitializeNativeTarget();
    _ = c.LLVMInitializeNativeAsmPrinter();
    _ = c.LLVMInitializeNativeAsmParser();
}

/// Initialize all supported targets for cross-compilation.
/// Note: LLVM_InitializeAllTargets, etc. are macros that expand to individual
/// target initialization calls. We explicitly initialize the targets we care about.
pub fn initializeAllTargets() void {
    // Initialize X86 target
    c.LLVMInitializeX86TargetInfo();
    c.LLVMInitializeX86Target();
    c.LLVMInitializeX86TargetMC();
    c.LLVMInitializeX86AsmPrinter();
    c.LLVMInitializeX86AsmParser();

    // Initialize AArch64 (ARM64) target
    c.LLVMInitializeAArch64TargetInfo();
    c.LLVMInitializeAArch64Target();
    c.LLVMInitializeAArch64TargetMC();
    c.LLVMInitializeAArch64AsmPrinter();
    c.LLVMInitializeAArch64AsmParser();
}

/// LLVM Context wrapper.
pub const Context = struct {
    ref: ContextRef,

    pub fn create() Context {
        return .{ .ref = c.LLVMContextCreate() };
    }

    pub fn dispose(self: Context) void {
        c.LLVMContextDispose(self.ref);
    }
};

/// LLVM Module wrapper.
pub const Module = struct {
    ref: ModuleRef,

    pub fn create(name: [:0]const u8, ctx: Context) Module {
        return .{ .ref = c.LLVMModuleCreateWithNameInContext(name.ptr, ctx.ref) };
    }

    pub fn dispose(self: Module) void {
        c.LLVMDisposeModule(self.ref);
    }

    pub fn setTarget(self: Module, triple: [:0]const u8) void {
        c.LLVMSetTarget(self.ref, triple.ptr);
    }

    pub fn setDataLayout(self: Module, layout: [:0]const u8) void {
        c.LLVMSetDataLayout(self.ref, layout.ptr);
    }

    pub fn verify(self: Module) Error!void {
        var error_msg: [*c]u8 = null;
        if (c.LLVMVerifyModule(self.ref, c.LLVMReturnStatusAction, &error_msg) != 0) {
            if (error_msg != null) {
                std.debug.print("LLVM Module verification failed: {s}\n", .{error_msg});
                c.LLVMDisposeMessage(error_msg);
            }
            return Error.ModuleVerificationFailed;
        }
    }

    pub fn printToString(self: Module, allocator: std.mem.Allocator) ![]u8 {
        const str = c.LLVMPrintModuleToString(self.ref);
        defer c.LLVMDisposeMessage(str);
        const len = std.mem.len(str);
        const result = try allocator.alloc(u8, len);
        @memcpy(result, str[0..len]);
        return result;
    }

    pub fn printToFile(self: Module, filename: [:0]const u8) Error!void {
        var error_msg: [*c]u8 = null;
        if (c.LLVMPrintModuleToFile(self.ref, filename.ptr, &error_msg) != 0) {
            if (error_msg != null) {
                std.debug.print("LLVM print to file failed: {s}\n", .{error_msg});
                c.LLVMDisposeMessage(error_msg);
            }
            return Error.EmitFailed;
        }
    }

    pub fn getNamedFunction(self: Module, name: [:0]const u8) ?ValueRef {
        const func = c.LLVMGetNamedFunction(self.ref, name.ptr);
        if (func == null) return null;
        return func;
    }
};

/// LLVM IR Builder wrapper.
pub const Builder = struct {
    ref: BuilderRef,

    pub fn create(ctx: Context) Builder {
        return .{ .ref = c.LLVMCreateBuilderInContext(ctx.ref) };
    }

    pub fn dispose(self: Builder) void {
        c.LLVMDisposeBuilder(self.ref);
    }

    pub fn positionAtEnd(self: Builder, block: BasicBlockRef) void {
        c.LLVMPositionBuilderAtEnd(self.ref, block);
    }

    // Return instructions
    pub fn buildRet(self: Builder, value: ValueRef) ValueRef {
        return c.LLVMBuildRet(self.ref, value);
    }

    pub fn buildRetVoid(self: Builder) ValueRef {
        return c.LLVMBuildRetVoid(self.ref);
    }

    // Arithmetic instructions
    pub fn buildAdd(self: Builder, lhs: ValueRef, rhs: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildAdd(self.ref, lhs, rhs, name.ptr);
    }

    pub fn buildNSWAdd(self: Builder, lhs: ValueRef, rhs: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildNSWAdd(self.ref, lhs, rhs, name.ptr);
    }

    pub fn buildSub(self: Builder, lhs: ValueRef, rhs: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildSub(self.ref, lhs, rhs, name.ptr);
    }

    pub fn buildNSWSub(self: Builder, lhs: ValueRef, rhs: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildNSWSub(self.ref, lhs, rhs, name.ptr);
    }

    pub fn buildMul(self: Builder, lhs: ValueRef, rhs: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildMul(self.ref, lhs, rhs, name.ptr);
    }

    pub fn buildNSWMul(self: Builder, lhs: ValueRef, rhs: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildNSWMul(self.ref, lhs, rhs, name.ptr);
    }

    pub fn buildSDiv(self: Builder, lhs: ValueRef, rhs: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildSDiv(self.ref, lhs, rhs, name.ptr);
    }

    pub fn buildSRem(self: Builder, lhs: ValueRef, rhs: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildSRem(self.ref, lhs, rhs, name.ptr);
    }

    pub fn buildNeg(self: Builder, value: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildNeg(self.ref, value, name.ptr);
    }

    // Comparison instructions
    pub fn buildICmp(self: Builder, op: c.LLVMIntPredicate, lhs: ValueRef, rhs: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildICmp(self.ref, op, lhs, rhs, name.ptr);
    }

    pub fn buildFCmp(self: Builder, op: c.LLVMRealPredicate, lhs: ValueRef, rhs: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildFCmp(self.ref, op, lhs, rhs, name.ptr);
    }

    // Floating-point arithmetic
    pub fn buildFAdd(self: Builder, lhs: ValueRef, rhs: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildFAdd(self.ref, lhs, rhs, name.ptr);
    }

    pub fn buildFSub(self: Builder, lhs: ValueRef, rhs: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildFSub(self.ref, lhs, rhs, name.ptr);
    }

    pub fn buildFMul(self: Builder, lhs: ValueRef, rhs: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildFMul(self.ref, lhs, rhs, name.ptr);
    }

    pub fn buildFDiv(self: Builder, lhs: ValueRef, rhs: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildFDiv(self.ref, lhs, rhs, name.ptr);
    }

    pub fn buildFNeg(self: Builder, value: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildFNeg(self.ref, value, name.ptr);
    }

    // Control flow
    pub fn buildCondBr(self: Builder, cond: ValueRef, then_block: BasicBlockRef, else_block: BasicBlockRef) ValueRef {
        return c.LLVMBuildCondBr(self.ref, cond, then_block, else_block);
    }

    pub fn buildBr(self: Builder, block: BasicBlockRef) ValueRef {
        return c.LLVMBuildBr(self.ref, block);
    }

    pub fn buildPhi(self: Builder, ty: TypeRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildPhi(self.ref, ty, name.ptr);
    }

    // Function calls
    pub fn buildCall(self: Builder, fn_type: TypeRef, func: ValueRef, args: []const ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildCall2(
            self.ref,
            fn_type,
            func,
            @ptrCast(@constCast(args.ptr)),
            @intCast(args.len),
            name.ptr,
        );
    }

    // Memory operations
    pub fn buildAlloca(self: Builder, ty: TypeRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildAlloca(self.ref, ty, name.ptr);
    }

    pub fn buildLoad(self: Builder, ty: TypeRef, ptr: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildLoad2(self.ref, ty, ptr, name.ptr);
    }

    pub fn buildStore(self: Builder, value: ValueRef, ptr: ValueRef) ValueRef {
        return c.LLVMBuildStore(self.ref, value, ptr);
    }

    // Bitwise operations
    pub fn buildAnd(self: Builder, lhs: ValueRef, rhs: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildAnd(self.ref, lhs, rhs, name.ptr);
    }

    pub fn buildOr(self: Builder, lhs: ValueRef, rhs: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildOr(self.ref, lhs, rhs, name.ptr);
    }

    pub fn buildXor(self: Builder, lhs: ValueRef, rhs: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildXor(self.ref, lhs, rhs, name.ptr);
    }

    pub fn buildNot(self: Builder, value: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildNot(self.ref, value, name.ptr);
    }

    pub fn buildShl(self: Builder, lhs: ValueRef, rhs: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildShl(self.ref, lhs, rhs, name.ptr);
    }

    pub fn buildAShr(self: Builder, lhs: ValueRef, rhs: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildAShr(self.ref, lhs, rhs, name.ptr);
    }

    pub fn buildLShr(self: Builder, lhs: ValueRef, rhs: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildLShr(self.ref, lhs, rhs, name.ptr);
    }

    // Type conversions
    pub fn buildSExt(self: Builder, value: ValueRef, dest_ty: TypeRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildSExt(self.ref, value, dest_ty, name.ptr);
    }

    pub fn buildZExt(self: Builder, value: ValueRef, dest_ty: TypeRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildZExt(self.ref, value, dest_ty, name.ptr);
    }

    pub fn buildTrunc(self: Builder, value: ValueRef, dest_ty: TypeRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildTrunc(self.ref, value, dest_ty, name.ptr);
    }

    pub fn buildSIToFP(self: Builder, value: ValueRef, dest_ty: TypeRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildSIToFP(self.ref, value, dest_ty, name.ptr);
    }

    pub fn buildFPToSI(self: Builder, value: ValueRef, dest_ty: TypeRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildFPToSI(self.ref, value, dest_ty, name.ptr);
    }

    pub fn buildUIToFP(self: Builder, value: ValueRef, dest_ty: TypeRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildUIToFP(self.ref, value, dest_ty, name.ptr);
    }

    pub fn buildFPToUI(self: Builder, value: ValueRef, dest_ty: TypeRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildFPToUI(self.ref, value, dest_ty, name.ptr);
    }

    pub fn buildFPExt(self: Builder, value: ValueRef, dest_ty: TypeRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildFPExt(self.ref, value, dest_ty, name.ptr);
    }

    pub fn buildFPTrunc(self: Builder, value: ValueRef, dest_ty: TypeRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildFPTrunc(self.ref, value, dest_ty, name.ptr);
    }

    // Unsigned arithmetic
    pub fn buildUDiv(self: Builder, lhs: ValueRef, rhs: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildUDiv(self.ref, lhs, rhs, name.ptr);
    }

    pub fn buildURem(self: Builder, lhs: ValueRef, rhs: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildURem(self.ref, lhs, rhs, name.ptr);
    }

    // Floating-point remainder
    pub fn buildFRem(self: Builder, lhs: ValueRef, rhs: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildFRem(self.ref, lhs, rhs, name.ptr);
    }

    // Unreachable (for traps)
    pub fn buildUnreachable(self: Builder) ValueRef {
        return c.LLVMBuildUnreachable(self.ref);
    }

    // Extract value from aggregate (used for overflow intrinsic results)
    pub fn buildExtractValue(self: Builder, agg: ValueRef, index: c_uint, name: [:0]const u8) ValueRef {
        return c.LLVMBuildExtractValue(self.ref, agg, index, name.ptr);
    }

    // GEP (get element pointer)
    pub fn buildGEP(self: Builder, ty: TypeRef, ptr: ValueRef, indices: []const ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildGEP2(
            self.ref,
            ty,
            ptr,
            @ptrCast(@constCast(indices.ptr)),
            @intCast(indices.len),
            name.ptr,
        );
    }

    // Select instruction (ternary)
    pub fn buildSelect(self: Builder, cond: ValueRef, then_val: ValueRef, else_val: ValueRef, name: [:0]const u8) ValueRef {
        return c.LLVMBuildSelect(self.ref, cond, then_val, else_val, name.ptr);
    }

    // Global string pointer (creates a global string constant and returns a pointer to it)
    pub fn buildGlobalStringPtr(self: Builder, str: [:0]const u8, name: [:0]const u8) ValueRef {
        return c.LLVMBuildGlobalStringPtr(self.ref, str.ptr, name.ptr);
    }
};

/// Type constructors.
pub const Types = struct {
    pub fn int1(ctx: Context) TypeRef {
        return c.LLVMInt1TypeInContext(ctx.ref);
    }

    pub fn int8(ctx: Context) TypeRef {
        return c.LLVMInt8TypeInContext(ctx.ref);
    }

    pub fn int16(ctx: Context) TypeRef {
        return c.LLVMInt16TypeInContext(ctx.ref);
    }

    pub fn int32(ctx: Context) TypeRef {
        return c.LLVMInt32TypeInContext(ctx.ref);
    }

    pub fn int64(ctx: Context) TypeRef {
        return c.LLVMInt64TypeInContext(ctx.ref);
    }

    pub fn int128(ctx: Context) TypeRef {
        return c.LLVMInt128TypeInContext(ctx.ref);
    }

    pub fn intN(ctx: Context, bits: c_uint) TypeRef {
        return c.LLVMIntTypeInContext(ctx.ref, bits);
    }

    pub fn float32(ctx: Context) TypeRef {
        return c.LLVMFloatTypeInContext(ctx.ref);
    }

    pub fn float64(ctx: Context) TypeRef {
        return c.LLVMDoubleTypeInContext(ctx.ref);
    }

    pub fn void_(ctx: Context) TypeRef {
        return c.LLVMVoidTypeInContext(ctx.ref);
    }

    pub fn pointer(ctx: Context) TypeRef {
        // LLVM 15+ uses opaque pointers
        return c.LLVMPointerTypeInContext(ctx.ref, 0);
    }

    pub fn function(return_type: TypeRef, param_types: []const TypeRef, is_var_arg: bool) TypeRef {
        return c.LLVMFunctionType(
            return_type,
            @ptrCast(@constCast(param_types.ptr)),
            @intCast(param_types.len),
            @intFromBool(is_var_arg),
        );
    }

    pub fn array(elem_type: TypeRef, count: u64) TypeRef {
        return c.LLVMArrayType2(elem_type, count);
    }

    /// Get the length of an array type.
    pub fn getArrayLength(array_type: TypeRef) u64 {
        return c.LLVMGetArrayLength2(array_type);
    }

    pub fn struct_(ctx: Context, elem_types: []const TypeRef, is_packed: bool) TypeRef {
        return c.LLVMStructTypeInContext(
            ctx.ref,
            @ptrCast(@constCast(elem_types.ptr)),
            @intCast(elem_types.len),
            @intFromBool(is_packed),
        );
    }
};

/// Constant value constructors.
pub const Const = struct {
    pub fn int(ty: TypeRef, value: u64, sign_extend: bool) ValueRef {
        return c.LLVMConstInt(ty, value, @intFromBool(sign_extend));
    }

    pub fn int1(ctx: Context, value: bool) ValueRef {
        return c.LLVMConstInt(Types.int1(ctx), @intFromBool(value), 0);
    }

    pub fn int8(ctx: Context, value: i8) ValueRef {
        const val64: i64 = value;
        return c.LLVMConstInt(Types.int8(ctx), @bitCast(@as(u64, @bitCast(val64))), 1);
    }

    pub fn int16(ctx: Context, value: i16) ValueRef {
        const val64: i64 = value;
        return c.LLVMConstInt(Types.int16(ctx), @bitCast(@as(u64, @bitCast(val64))), 1);
    }

    pub fn int32(ctx: Context, value: i32) ValueRef {
        // Sign-extend to 64 bits for LLVM
        const val64: i64 = value;
        return c.LLVMConstInt(Types.int32(ctx), @bitCast(@as(u64, @bitCast(val64))), 1);
    }

    pub fn int64(ctx: Context, value: i64) ValueRef {
        return c.LLVMConstInt(Types.int64(ctx), @bitCast(@as(u64, @bitCast(value))), 1);
    }

    pub fn float64(ctx: Context, value: f64) ValueRef {
        return c.LLVMConstReal(Types.float64(ctx), value);
    }

    pub fn float32(ctx: Context, value: f32) ValueRef {
        return c.LLVMConstReal(Types.float32(ctx), value);
    }

    pub fn null_(ty: TypeRef) ValueRef {
        return c.LLVMConstNull(ty);
    }

    pub fn undef(ty: TypeRef) ValueRef {
        return c.LLVMGetUndef(ty);
    }

    pub fn string(ctx: Context, str: []const u8, null_terminate: bool) ValueRef {
        return c.LLVMConstStringInContext(
            ctx.ref,
            str.ptr,
            @intCast(str.len),
            @intFromBool(!null_terminate),
        );
    }

    /// Create a constant named struct value.
    pub fn namedStruct(struct_type: TypeRef, vals: []const ValueRef) ValueRef {
        return c.LLVMConstNamedStruct(struct_type, @ptrCast(@constCast(vals.ptr)), @intCast(vals.len));
    }

    /// Create a constant array value.
    pub fn array(element_type: TypeRef, vals: []const ValueRef) ValueRef {
        return c.LLVMConstArray2(element_type, @ptrCast(@constCast(vals.ptr)), @intCast(vals.len));
    }
};

/// Function operations.
pub fn addFunction(module: Module, name: [:0]const u8, fn_type: TypeRef) ValueRef {
    return c.LLVMAddFunction(module.ref, name.ptr, fn_type);
}

pub fn appendBasicBlock(ctx: Context, func: ValueRef, name: [:0]const u8) BasicBlockRef {
    return c.LLVMAppendBasicBlockInContext(ctx.ref, func, name.ptr);
}

pub fn getParam(func: ValueRef, index: c_uint) ValueRef {
    return c.LLVMGetParam(func, index);
}

pub fn setFunctionCallConv(func: ValueRef, cc: c_uint) void {
    c.LLVMSetFunctionCallConv(func, cc);
}

pub fn getGlobalValueType(func: ValueRef) TypeRef {
    return c.LLVMGlobalGetValueType(func);
}

/// PHI node operations.
pub fn addIncoming(phi: ValueRef, incoming_values: []const ValueRef, incoming_blocks: []const BasicBlockRef) void {
    c.LLVMAddIncoming(
        phi,
        @ptrCast(@constCast(incoming_values.ptr)),
        @ptrCast(@constCast(incoming_blocks.ptr)),
        @intCast(incoming_values.len),
    );
}

/// Target machine operations.
pub fn getDefaultTargetTriple(allocator: std.mem.Allocator) ![:0]u8 {
    const triple = c.LLVMGetDefaultTargetTriple();
    defer c.LLVMDisposeMessage(triple);
    const len = std.mem.len(triple);
    const result = try allocator.allocSentinel(u8, len, 0);
    @memcpy(result, triple[0..len]);
    return result;
}

pub fn getTargetFromTriple(triple: [:0]const u8) Error!TargetRef {
    var target: TargetRef = undefined;
    var error_msg: [*c]u8 = null;
    if (c.LLVMGetTargetFromTriple(triple.ptr, &target, &error_msg) != 0) {
        if (error_msg != null) {
            std.debug.print("LLVM target lookup failed: {s}\n", .{error_msg});
            c.LLVMDisposeMessage(error_msg);
        }
        return Error.TargetNotFound;
    }
    return target;
}

pub fn createTargetMachine(
    target: TargetRef,
    triple: [:0]const u8,
    cpu: [:0]const u8,
    features: [:0]const u8,
    opt_level: c.LLVMCodeGenOptLevel,
    reloc_mode: c.LLVMRelocMode,
    code_model: c.LLVMCodeModel,
) ?TargetMachineRef {
    const tm = c.LLVMCreateTargetMachine(
        target,
        triple.ptr,
        cpu.ptr,
        features.ptr,
        opt_level,
        reloc_mode,
        code_model,
    );
    if (tm == null) return null;
    return tm;
}

pub fn disposeTargetMachine(tm: TargetMachineRef) void {
    c.LLVMDisposeTargetMachine(tm);
}

pub fn targetMachineEmitToFile(
    tm: TargetMachineRef,
    module: Module,
    filename: [:0]const u8,
    codegen: c.LLVMCodeGenFileType,
) Error!void {
    var error_msg: [*c]u8 = null;
    if (c.LLVMTargetMachineEmitToFile(
        tm,
        module.ref,
        @ptrCast(@constCast(filename.ptr)),
        codegen,
        &error_msg,
    ) != 0) {
        if (error_msg != null) {
            std.debug.print("LLVM emit to file failed: {s}\n", .{error_msg});
            c.LLVMDisposeMessage(error_msg);
        }
        return Error.EmitFailed;
    }
}

pub fn getTargetMachineTriple(tm: TargetMachineRef, allocator: std.mem.Allocator) ![:0]u8 {
    const triple = c.LLVMGetTargetMachineTriple(tm);
    defer c.LLVMDisposeMessage(triple);
    const len = std.mem.len(triple);
    const result = try allocator.allocSentinel(u8, len, 0);
    @memcpy(result, triple[0..len]);
    return result;
}

pub fn createTargetDataLayout(tm: TargetMachineRef) TargetDataRef {
    return c.LLVMCreateTargetDataLayout(tm);
}

pub fn disposeTargetData(td: TargetDataRef) void {
    c.LLVMDisposeTargetData(td);
}

pub fn copyStringRepOfTargetData(td: TargetDataRef, allocator: std.mem.Allocator) ![:0]u8 {
    const str = c.LLVMCopyStringRepOfTargetData(td);
    defer c.LLVMDisposeMessage(str);
    const len = std.mem.len(str);
    const result = try allocator.allocSentinel(u8, len, 0);
    @memcpy(result, str[0..len]);
    return result;
}

/// Get the intrinsic ID for a given name.
pub fn lookupIntrinsicID(name: []const u8) c_uint {
    return c.LLVMLookupIntrinsicID(name.ptr, name.len);
}

/// Get the intrinsic declaration for a module.
pub fn getIntrinsicDeclaration(module: Module, id: c_uint, param_types: []const TypeRef) ValueRef {
    return c.LLVMGetIntrinsicDeclaration(
        module.ref,
        id,
        @ptrCast(@constCast(param_types.ptr)),
        param_types.len,
    );
}

/// Get the type of an intrinsic.
pub fn intrinsicGetType(ctx: Context, id: c_uint, param_types: []const TypeRef) TypeRef {
    return c.LLVMIntrinsicGetType(
        ctx.ref,
        id,
        @ptrCast(@constCast(param_types.ptr)),
        param_types.len,
    );
}

/// Get the type kind of a type (for checking if float vs int).
pub fn getTypeKind(ty: TypeRef) c.LLVMTypeKind {
    return c.LLVMGetTypeKind(ty);
}

/// Get type of a value.
pub fn typeOf(value: ValueRef) TypeRef {
    return c.LLVMTypeOf(value);
}

/// Get the return type of a function type.
pub fn getReturnType(fn_type: TypeRef) TypeRef {
    return c.LLVMGetReturnType(fn_type);
}

/// Check if a type is void.
pub fn isVoidType(ty: TypeRef) bool {
    return c.LLVMGetTypeKind(ty) == c.LLVMVoidTypeKind;
}

/// Get the bit width of an integer type.
pub fn getIntTypeWidth(ty: TypeRef) u32 {
    return c.LLVMGetIntTypeWidth(ty);
}

/// Get the size of a type in bytes using target data.
pub fn sizeOfType(target_data: TargetDataRef, ty: TypeRef) u64 {
    return c.LLVMABISizeOfType(target_data, ty);
}

/// Get the ABI alignment of a type in bytes using target data.
pub fn abiAlignOfType(target_data: TargetDataRef, ty: TypeRef) u32 {
    return c.LLVMABIAlignmentOfType(target_data, ty);
}

// ============================================================================
// Debug Information API
// ============================================================================

/// Debug info builder wrapper for generating DWARF debug information.
pub const DIBuilder = struct {
    ref: DIBuilderRef,

    /// Create a new DIBuilder for the given module.
    pub fn create(module: Module) DIBuilder {
        return .{ .ref = c.LLVMCreateDIBuilder(module.ref) };
    }

    /// Dispose of the DIBuilder.
    pub fn dispose(self: DIBuilder) void {
        c.LLVMDisposeDIBuilder(self.ref);
    }

    /// Finalize the debug info. Must be called before the module is verified.
    pub fn finalize(self: DIBuilder) void {
        c.LLVMDIBuilderFinalize(self.ref);
    }

    /// Create a compile unit (the root of debug info).
    pub fn createCompileUnit(
        self: DIBuilder,
        lang: c.LLVMDWARFSourceLanguage,
        file: MetadataRef,
        producer: []const u8,
        is_optimized: bool,
        flags: []const u8,
        runtime_version: c_uint,
        split_name: []const u8,
        kind: c.LLVMDWARFEmissionKind,
        dwo_id: c_uint,
        split_debug_inlining: bool,
        debug_info_for_profiling: bool,
        sysroot: []const u8,
        sdk: []const u8,
    ) MetadataRef {
        return c.LLVMDIBuilderCreateCompileUnit(
            self.ref,
            lang,
            file,
            producer.ptr,
            producer.len,
            @intFromBool(is_optimized),
            flags.ptr,
            flags.len,
            runtime_version,
            split_name.ptr,
            split_name.len,
            kind,
            dwo_id,
            @intFromBool(split_debug_inlining),
            @intFromBool(debug_info_for_profiling),
            sysroot.ptr,
            sysroot.len,
            sdk.ptr,
            sdk.len,
        );
    }

    /// Create a file descriptor.
    pub fn createFile(self: DIBuilder, filename: []const u8, directory: []const u8) MetadataRef {
        return c.LLVMDIBuilderCreateFile(
            self.ref,
            filename.ptr,
            filename.len,
            directory.ptr,
            directory.len,
        );
    }

    /// Create a subroutine type (function signature for debug info).
    pub fn createSubroutineType(
        self: DIBuilder,
        file: ?MetadataRef,
        param_types: []const MetadataRef,
        flags: c.LLVMDIFlags,
    ) MetadataRef {
        return c.LLVMDIBuilderCreateSubroutineType(
            self.ref,
            file orelse null,
            @ptrCast(@constCast(param_types.ptr)),
            @intCast(param_types.len),
            flags,
        );
    }

    /// Create a function definition for debug info.
    pub fn createFunction(
        self: DIBuilder,
        scope: MetadataRef,
        name: []const u8,
        linkage_name: []const u8,
        file: MetadataRef,
        line_no: c_uint,
        ty: MetadataRef,
        is_local_to_unit: bool,
        is_definition: bool,
        scope_line: c_uint,
        flags: c.LLVMDIFlags,
        is_optimized: bool,
    ) MetadataRef {
        return c.LLVMDIBuilderCreateFunction(
            self.ref,
            scope,
            name.ptr,
            name.len,
            linkage_name.ptr,
            linkage_name.len,
            file,
            line_no,
            ty,
            @intFromBool(is_local_to_unit),
            @intFromBool(is_definition),
            scope_line,
            flags,
            @intFromBool(is_optimized),
        );
    }

    /// Create a basic type (e.g., int, float).
    pub fn createBasicType(
        self: DIBuilder,
        name: []const u8,
        size_in_bits: u64,
        encoding: c_uint,
        flags: c.LLVMDIFlags,
    ) MetadataRef {
        return c.LLVMDIBuilderCreateBasicType(
            self.ref,
            name.ptr,
            name.len,
            size_in_bits,
            encoding,
            flags,
        );
    }

    /// Create a lexical block scope.
    pub fn createLexicalBlock(
        self: DIBuilder,
        scope: MetadataRef,
        file: MetadataRef,
        line: c_uint,
        column: c_uint,
    ) MetadataRef {
        return c.LLVMDIBuilderCreateLexicalBlock(
            self.ref,
            scope,
            file,
            line,
            column,
        );
    }
};

/// Create a debug location.
pub fn createDebugLocation(
    ctx: Context,
    line: c_uint,
    column: c_uint,
    scope: MetadataRef,
    inlined_at: ?MetadataRef,
) MetadataRef {
    return c.LLVMDIBuilderCreateDebugLocation(
        ctx.ref,
        line,
        column,
        scope,
        inlined_at orelse null,
    );
}

/// Set the current debug location for the builder.
pub fn setCurrentDebugLocation(builder: Builder, loc: ?MetadataRef) void {
    c.LLVMSetCurrentDebugLocation2(builder.ref, loc orelse null);
}

/// Attach debug info to a function.
pub fn setSubprogram(func: ValueRef, subprogram: MetadataRef) void {
    c.LLVMSetSubprogram(func, subprogram);
}

/// Get the subprogram attached to a function.
pub fn getSubprogram(func: ValueRef) ?MetadataRef {
    const sp = c.LLVMGetSubprogram(func);
    if (sp == null) return null;
    return sp;
}
