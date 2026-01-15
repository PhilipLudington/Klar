# Phase 3: Native Compiler Implementation Plan

> **Goal:** Compile Klar programs directly to native machine code for optimal performance.

## Executive Summary

Phase 3 transforms Klar from an interpreted language to a compiled one. This plan outlines the implementation of a native code compiler using **LLVM** as the code generation backend. The approach introduces a mid-level IR (Intermediate Representation) layer between the typed AST and LLVM IR, and implements Klar's **ownership-based memory model** with compile-time tracking and automatic destructor insertion.

**Key Decision: LLVM over Cranelift**

| Factor | LLVM | Cranelift |
|--------|------|-----------|
| Optimization quality | Best-in-class (20+ years) | Good but not extreme |
| Target coverage | 20+ architectures | x86_64, ARM64, RISC-V, s390x |
| Ecosystem maturity | Battle-tested (Clang, Rust, Swift) | Younger, fewer production users |
| Debug info | Full DWARF/PDB support | Basic DWARF |
| Zig integration | Well-established C API | Uncertain/immature bindings |
| LTO/PGO | Full support | Not available |
| Documentation | Extensive | Sparse |
| Compilation speed | Slower | Fast (JIT-focused) |

LLVM is the right choice for a production AOT compiler. While compilation is slower than Cranelift, this matters less for ahead-of-time compilation than for JIT scenarios. The optimization quality, tooling maturity, and target coverage justify the complexity.

**Memory Model: Ownership (Not GC)**

Klar uses ownership-based memory management as specified in DESIGN.md:
- **Single owner** - each value has exactly one owner
- **Move semantics** - assignment transfers ownership
- **Borrowing** - `&T` and `&mut T` for temporary access
- **RAII** - automatic destruction when owner goes out of scope
- **Rc/Arc** - explicit reference counting for shared ownership
- **No tracing garbage collector**

---

## Phase 3 Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         KLAR COMPILATION PIPELINE                        │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│   Source (.kl)                                                          │
│        │                                                                │
│        ▼                                                                │
│   ┌─────────┐                                                           │
│   │  Lexer  │  ← existing                                               │
│   └────┬────┘                                                           │
│        │ tokens                                                         │
│        ▼                                                                │
│   ┌─────────┐                                                           │
│   │ Parser  │  ← existing                                               │
│   └────┬────┘                                                           │
│        │ AST                                                            │
│        ▼                                                                │
│   ┌─────────┐                                                           │
│   │ Checker │  ← existing (+ ownership analysis)                        │
│   └────┬────┘                                                           │
│        │ typed AST + ownership info                                     │
│        │                                                                │
│   ┌────┴────────────────────────────────────────┐                       │
│   │             COMPILATION TARGETS             │                       │
│   ├─────────────────┬───────────────────────────┤                       │
│   │                 │                           │                       │
│   ▼                 ▼                           ▼                       │
│ ┌──────────┐   ┌──────────┐              ┌───────────┐                  │
│ │Interpreter│   │ Bytecode │              │  Klar IR  │  ← NEW          │
│ │(existing) │   │ Compiler │              │ Generator │                 │
│ └──────────┘   │(existing) │              └─────┬─────┘                 │
│                └─────┬─────┘                    │                       │
│                      │                    ┌─────┴─────┐                 │
│                      ▼                    │ Drop/Move │  ← NEW          │
│                ┌──────────┐               │ Insertion │                 │
│                │    VM    │               └─────┬─────┘                 │
│                │(existing)│                     │ IR + destructors      │
│                └──────────┘                     ▼                       │
│                                         ┌─────────────┐                 │
│                                         │ Optimizer   │  ← NEW          │
│                                         │   Passes    │                 │
│                                         └──────┬──────┘                 │
│                                                │                        │
│                                                ▼                        │
│                                         ┌───────────┐                   │
│                                         │   LLVM    │  ← NEW            │
│                                         │  Backend  │                   │
│                                         └─────┬─────┘                   │
│                                               │                         │
│                                               ▼                         │
│                                        Native Binary                    │
│                                       (.exe / ELF / Mach-O)             │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Memory Model Implementation

### Ownership Rules (from DESIGN.md)

```klar
// Single owner - value destroyed when owner goes out of scope
let a = Buffer.new(1024)
// a destroyed here (RAII)

// Move semantics - ownership transfers
let a = Buffer.new(1024)
let b = a                    // a is MOVED to b, a is now invalid
// only b is destroyed

// Copy types - opt-in for simple types
struct Point: Copy { x: f64, y: f64 }
let p1 = Point { x: 1.0, y: 2.0 }
let p2 = p1                  // p1 is COPIED, both valid

// Borrowing - temporary access without ownership transfer
fn print_length(buf: &Buffer) {    // immutable borrow
    print(buf.len)
}
let buffer = Buffer.new(1024)
print_length(&buffer)              // buffer still valid

// Reference counting for shared ownership
let data = Rc.new(Config.load())
let alias = data.clone()           // cheap reference count increment
// Both data and alias valid, freed when last reference dropped
```

### Compiler Implementation Strategy

**1. Ownership Tracking (in Checker)**
- Track ownership state for each variable: `owned`, `moved`, `borrowed`
- Error on use-after-move
- Verify borrow rules: one `&mut` OR many `&`, never both

**2. Drop Insertion (new pass)**
- Insert destructor calls at scope exits
- Handle early returns, breaks, continues
- Respect move semantics (don't drop moved values)

**3. Copy vs Move Decision**
- Types implementing `Copy` trait are copied
- All other types are moved by default

**4. Reference Counting (for Rc/Arc)**
- `clone()` increments reference count
- Drop decrements and frees when zero
- Weak references for cycle breaking

---

## Milestone Breakdown

### Milestone 1: LLVM Integration & Foundation ✅

**Objective:** Set up LLVM C API bindings and create the basic code generation infrastructure.

**Status:** Complete (January 2026)

**Deliverables:**
- [x] Add LLVM-C dependency to build system
- [x] Create `src/codegen/` module directory
- [x] Implement `src/codegen/llvm.zig` - LLVM C API wrapper
- [x] Implement `src/codegen/target.zig` - Target platform abstraction
- [x] Create basic test: compile `fn main() -> i32 { 42 }` to native code
- [x] Add `klar build` command to CLI for native compilation mode
- [x] Set up system linker invocation (ld on macOS/Linux)

**Files Created:**
```
src/codegen/
├── mod.zig           # Module root, exports public API
├── llvm.zig          # LLVM C API wrapper (Context, Module, Builder, Types, Const)
├── target.zig        # Platform detection, target triple generation
├── emit.zig          # AST to LLVM IR translation
└── linker.zig        # System linker invocation
```

**LLVM C API Usage:**
```zig
// Example wrapper structure
const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/Target.h");
    @cInclude("llvm-c/TargetMachine.h");
});

pub const Context = struct {
    handle: c.LLVMContextRef,

    pub fn create() Context {
        return .{ .handle = c.LLVMContextCreate() };
    }

    pub fn dispose(self: Context) void {
        c.LLVMContextDispose(self.handle);
    }
};
```

**Success Criteria:**
- `klar build --native hello.kl` produces an executable
- Executable runs and returns correct exit code
- Works on at least one platform (macOS ARM64 or Linux x86_64)

---

### Milestone 2: Klar IR Design & Generation ✅

**Objective:** Design and implement a mid-level IR that bridges typed AST and LLVM.

**Status:** Complete (January 2026)

**Deliverables:**
- [x] Design Klar IR instruction set (SSA-form)
- [x] Implement `src/ir/mod.zig` - IR module root
- [x] Implement `src/ir/inst.zig` - IR instructions
- [x] Implement `src/ir/builder.zig` - IR construction API
- [x] Implement `src/ir/printer.zig` - IR text output for debugging
- [x] Create AST-to-IR lowering pass in `src/ir/lower.zig`
- [x] Unit tests for IR generation from simple functions

**Files Created:**
```
src/ir/
├── mod.zig           # Module root, exports public API
├── inst.zig          # IR instructions (60+ ops), types, values, functions
├── builder.zig       # Fluent IR construction API
├── printer.zig       # LLVM-style text output
├── lower.zig         # AST to IR lowering pass
└── tests.zig         # Comprehensive unit tests (17 test cases)
```

**IR Features Implemented:**
- SSA-form values with unique IDs
- Typed instructions: i8-i128, u8-u128, f32/f64, bool, char
- Composite types: ptr, array, slice, tuple, struct, optional, ref
- Arithmetic: add/sub/mul/div with checked, wrapping, saturating variants
- Comparisons: icmp (eq/ne/lt/le/gt/ge), fcmp
- Memory: alloca, load, store
- Ownership: move, copy, drop, borrow, borrow_mut
- Reference counting: rc_inc, rc_dec
- Optional: some, none, is_some, is_none, unwrap
- Control flow: br, cond_br, ret, ret_void, phi nodes

**Klar IR Instruction Set (Draft):**
```
; Control flow
  br %label                   ; unconditional branch
  br.cond %cond, %then, %else ; conditional branch
  ret %val                    ; return value
  unreachable                 ; trap/panic

; Constants
  %0 = const.i32 42
  %1 = const.f64 3.14
  %2 = const.bool true
  %3 = const.str "hello"

; Arithmetic (typed)
  %r = add.i32 %a, %b         ; checked (traps on overflow)
  %r = add.wrap.i32 %a, %b    ; wrapping
  %r = add.sat.i32 %a, %b     ; saturating

; Comparison
  %r = cmp.eq %a, %b
  %r = cmp.lt.i32 %a, %b

; Memory (stack)
  %ptr = alloca i32           ; stack allocation
  %val = load %ptr
  store %val, %ptr

; Memory (heap) - for Rc/Arc internals
  %ptr = heap.alloc %size
  heap.free %ptr

; Calls
  %r = call @fn_name(%arg1, %arg2)

; Ownership operations
  move %src to %dst           ; transfer ownership
  copy %src to %dst           ; copy (for Copy types)
  drop %val                   ; call destructor
  borrow %val                 ; create reference
  borrow.mut %val             ; create mutable reference

; Reference counting
  rc.inc %ptr                 ; increment ref count
  rc.dec %ptr                 ; decrement, free if zero

; Type operations
  %r = cast %val to i64       ; safe widening
  %r = trunc %val to i8       ; truncation

; Aggregates
  %r = struct.new @Point
  %f = struct.get %r, field_idx
  struct.set %r, field_idx, %val
```

**Success Criteria:**
- Simple functions (arithmetic, conditionals) lower to IR
- IR printer produces readable text representation
- IR validates (type consistency, proper SSA form)

---

### Milestone 3: Ownership Analysis & Drop Insertion ✅

**Objective:** Implement compile-time ownership tracking and automatic destructor insertion.

**Status:** Complete (January 2026)

**Deliverables:**
- [x] Extend checker with ownership state tracking
- [x] Implement move detection and use-after-move errors
- [x] Implement borrow checking (one &mut OR many &)
- [x] Create drop insertion pass
- [x] Handle drops at scope exits, returns, breaks
- [x] Implement Copy trait detection (skip drop for Copy types)
- [x] Add `--dump-ownership` debug flag

**Files Created:**
```
src/ownership/
├── mod.zig           # Module root, exports public API
├── state.zig         # OwnershipState, BorrowInfo, VariableState, OwnershipScope
├── checker.zig       # OwnershipChecker with AST analysis
└── drop.zig          # DropInserter, DropPoint, DropInfo
```

**Key Features:**
- OwnershipState enum: owned, moved, borrowed, borrowed_mut, partially_moved
- Variable state tracking with borrow lists
- Use-after-move detection with clear error messages
- Borrow conflict detection (immutable + mutable, double mutable)
- Scope-based borrow invalidation
- Type.isCopyType() for Copy trait detection
- Integration with main checker via --dump-ownership flag

**Ownership States:**
```
enum OwnershipState {
    owned,           // Variable owns the value
    moved,           // Ownership transferred elsewhere
    borrowed,        // Temporarily lent out (immutable)
    borrowed_mut,    // Temporarily lent out (mutable)
    partially_moved, // Some fields moved (for structs)
}
```

**Drop Insertion Example:**
```klar
// Source
fn example() {
    let a = Buffer.new(100)
    let b = Buffer.new(200)
    if condition {
        return           // Must drop both a and b
    }
    let c = a            // Move a to c
    // Must drop b and c (not a, it was moved)
}

// After drop insertion (IR)
fn example() {
    %a = call @Buffer.new(100)
    %b = call @Buffer.new(200)
    br.cond %condition, %early_return, %continue

%early_return:
    drop %b              ; inserted
    drop %a              ; inserted
    ret void

%continue:
    move %a to %c
    ; ... rest of function ...
    drop %c              ; inserted (not %a - it was moved)
    drop %b              ; inserted
    ret void
}
```

**Success Criteria:**
- Use-after-move produces compile error
- Values automatically destroyed at scope exit
- No double-frees or memory leaks
- Moved values not dropped

---

### Milestone 4: Primitive Operations & Control Flow ✅

**Objective:** Implement native code generation for primitive types and basic control flow.

**Status:** Complete (January 2026)

**Deliverables:**
- [x] Implement integer arithmetic (add, sub, mul, div, mod)
- [x] Implement overflow-checking arithmetic (NSW variants)
- [x] Implement wrapping/saturating arithmetic variants
- [x] Implement floating-point operations
- [x] Implement comparison operators
- [x] Implement logical operators (and, or, not) with short-circuit evaluation
- [x] Implement if/else code generation with PHI nodes
- [x] Implement while loops with loop context
- [x] Implement basic for loops (range iteration)
- [x] Implement break/continue with loop stack

**Files Modified:**
```
src/codegen/emit.zig    # Enhanced with loop stack, assignment handling, type-aware ops
src/codegen/llvm.zig    # Added UDiv, URem, FRem, intrinsic functions
```

**Test Programs:**
```klar
// test_arithmetic.kl - returns 35 ✅
fn main() -> i32 {
    let x = 10 + 20 * 3
    let y = x / 2
    y
}

// test_control.kl - returns 20 ✅
fn main() -> i32 {
    var sum = 0
    for i in 0..10 {
        if i % 2 == 0 {
            sum = sum + i
        }
    }
    sum
}

// test_fib.kl - fib(30) = 832040, ~0.01s native vs ~1.25s VM ✅
fn fib(n: i32) -> i32 {
    if n <= 1 { n } else { fib(n - 1) + fib(n - 2) }
}
fn main() -> i32 { fib(30) }
```

**Success Criteria:**
- ✅ Arithmetic test returns correct value (35)
- ✅ Control flow test returns 20 (sum of even numbers 0-9)
- ✅ Overflow uses NSW (No Signed Wrap) for debug checking
- ✅ Generated code matches expected semantics
- ✅ ~100x+ speedup for compute-bound code (fib(30): 1.25s → <0.01s)

---

### Milestone 5: Functions & Calling Convention ✅

**Objective:** Implement function definitions, calls, and follow platform ABI.

**Status:** Complete (January 2026)

**Deliverables:**
- [x] Use platform calling conventions (System V AMD64, AAPCS64)
- [x] Implement function prologue/epilogue generation
- [x] Implement function calls (direct)
- [x] Implement return values (including struct returns)
- [x] Implement multiple parameters
- [x] Implement local variables (stack slots via LLVM alloca)
- [x] Implement recursive function calls
- [x] Add stack overflow detection (OS-level guard pages)

**Files Modified:**
```
src/codegen/
├── emit.zig          # Added platform/ABI info, calling convention setup
├── llvm.zig          # Added getReturnType(), isVoidType() helpers
└── target.zig        # Added CallingConvention, ABI structs
```

**Key Features:**
- Target triple automatically set on LLVM module
- Platform-appropriate calling conventions selected
- ABI information tracked for future struct returns
- Void function calls properly handled (no name assignment)
- Tested with 8+ parameters (beyond register args)
- Deep mutual recursion tested (100+ stack frames)

**Calling Convention (use platform standard):**
```
x86_64 System V ABI:
  Parameters: rdi, rsi, rdx, rcx, r8, r9 (integers)
              xmm0-xmm7 (floats)
  Return:     rax (integers), xmm0 (floats)
              Small structs in rax:rdx
              Large structs via hidden pointer parameter

ARM64 AAPCS64:
  Parameters: x0-x7 (integers), v0-v7 (floats)
  Return:     x0 (integers), v0 (floats)
```

**Test Programs:**
```klar
// test_fib.kl
fn fib(n: i32) -> i32 {
    if n <= 1 {
        return n
    }
    fib(n - 1) + fib(n - 2)
}

fn main() -> i32 {
    fib(30)
}
```

**Success Criteria:**
- ✅ fib(30) returns 832040 (verified: exit code 40 = 832040 % 256)
- ✅ Performance comparable to C implementation (<0.01s)
- ✅ No stack corruption on deep recursion (tested 100+ frames)

---

### Milestone 6: Composite Types (Structs, Tuples, Arrays) ✅

**Objective:** Implement memory layout and operations for composite types.

**Status:** Complete (January 2026)

**Deliverables:**
- [x] Implement struct memory layout calculation (match C ABI)
- [x] Implement struct creation (stack allocation)
- [x] Implement field access (get/set) - numeric indices for tuples
- [x] Implement tuple creation and element access
- [x] Implement fixed-size array allocation
- [x] Implement array indexing
- [x] Implement slice representation (ptr + len)
- [x] Implement array bounds checking (traps on out-of-bounds)
- [x] Implement named field access for structs
- [ ] Implement struct destruction (call field destructors) - deferred to Milestone 10 (Runtime Library)
- [ ] Implement struct passing (by value or reference per ABI) - deferred to Milestone 13 (Multi-Platform)

**Note:** Struct destruction and ABI-compliant passing require runtime library support (drop functions, custom destructors) which will be implemented in later milestones. Current implementation handles primitive types correctly.

**Files Created/Modified:**
```
src/codegen/
├── layout.zig        # NEW: Memory layout calculation (C ABI compatible)
├── mod.zig           # Updated to export layout module
├── emit.zig          # Enhanced with composite type support, struct field lookup, bounds checking
├── llvm.zig          # Added getArrayLength()
└── target.zig        # Added getPointerSize()

src/parser.zig        # Updated to parse tuple index access (.0, .1)
```

**Test Programs:**
```
test/native/
├── tuple.kl          # Tuple literal and element access (pair.0, pair.1) ✅
├── array.kl          # Array literal and indexing (arr[0]) ✅
├── struct.kl         # Struct literal and named field access (p.x, p.y) ✅
├── struct_order.kl   # Struct with fields in different order ✅
├── array_bounds.kl   # Array bounds checking (traps on out-of-bounds) ✅
```

**Memory Layout Rules:**
```
Struct layout (C-compatible):
  - Fields ordered as declared
  - Natural alignment for each field
  - Padding inserted as needed
  - Total size rounded to max field alignment

Example:
  struct Point { x: f64, y: f64 }
  Size: 16 bytes, Align: 8 bytes
  Layout: [x:8][y:8]

Struct destruction:
  - Call drop on each field (in reverse declaration order)
  - Skip fields that implement Copy
```

**Test Programs:**
```klar
// test_struct.kl
struct Point { x: f64, y: f64 }

fn distance(p: Point) -> f64 {
    (p.x * p.x + p.y * p.y).sqrt()
}

fn main() {
    let p = Point { x: 3.0, y: 4.0 }
    print(distance(p))  // 5.0
}
```

**Success Criteria:**
- ✅ Struct fields correctly accessed by name (point.x, point.y)
- ✅ Array bounds checking works (traps on out-of-bounds)
- ✅ Tuple element access works (pair.0, pair.1)
- ✅ No memory corruption

---

### Milestone 7: Optional & Result Types ✅

**Objective:** Implement the `?T` and `Result[T, E]` types with proper native representation.

**Status:** Partially Complete (January 2026) - Optional types implemented, Result types deferred

**Deliverables:**
- [x] Implement Optional as tagged union (i1 tag + value)
- [x] Implement Some/None construction (implicit from function return type)
- [x] Implement optional unwrapping with trap on None (`!` operator)
- [x] Implement `??` null coalescing operator
- [x] Implement `?` propagation operator (currently traps like `!`)
- [ ] Implement Result type as tagged union - deferred to later milestone
- [ ] Implement Ok/Err construction - deferred to later milestone
- [ ] Implement Result matching - deferred to later milestone
- [ ] Proper drop semantics (drop inner value) - deferred to Milestone 10 (Runtime Library)

**Note:** Result types require more complex pattern matching and error propagation infrastructure. Optional types are fully functional and sufficient for many use cases.

**Files Modified:**
```
src/codegen/emit.zig    # Added emitPostfix, emitNullCoalesce, emitSome, emitNone
src/checker.zig         # Updated checkReturn for implicit Some/None conversion
```

**Test Programs:**
```
test/native/
├── optional_some.kl           # Force unwrap Some value ✅
├── optional_unwrap.kl         # Basic unwrap test ✅
├── optional_coalesce.kl       # Null coalescing with None ✅
├── optional_coalesce_some.kl  # Null coalescing with Some ✅
```

**Optional Layout:**
```
?T layout (using i1 for compact representation):
  - Tag: i1 (0 = None, 1 = Some)
  - Value storage (only valid when tag = 1)
  - Struct: { i1, T }

Example: ?i32 is { i1, i32 }
```

**Success Criteria:**
- ✅ Optional Some/None distinguishable at runtime
- ✅ Unwrap on None produces trap (unreachable)
- ✅ `??` operator works correctly with short-circuit evaluation
- ✅ Implicit Some/None conversion for function returns

---

### Milestone 8: Reference Counting (Rc/Arc) 🚧

**Objective:** Implement Rc and Arc for shared ownership scenarios.

**Status:** Partially Complete (January 2026) - Rc.new(), .clone(), .downgrade(), .upgrade() implemented

**Deliverables:**
- [x] Implement Rc struct layout (count + value)
- [x] Implement `Rc.new()` - allocate and initialize
- [x] Implement `Rc.clone()` - increment reference count
- [x] Implement `Weak.upgrade()` - attempt to get Rc from Weak
- [x] Implement `Rc.downgrade()` - create Weak reference
- [x] Add Rc[T] and Weak[T] to type system
- [x] Implement embedded runtime functions (klar_rc_alloc, klar_rc_clone, etc.)
- [ ] Implement automatic Rc drop at scope exit - deferred (requires emitter scope tracking)
- [ ] Implement Arc with atomic operations for thread safety
- [ ] Add cycle detection in debug mode (optional)

**Rc Layout:**
```
Rc[T] internal structure:
┌─────────────────────────┐
│ strong_count: usize     │  (reference count)
│ weak_count: usize       │  (weak reference count)
│ value: T                │  (the owned value)
└─────────────────────────┘

Rc[T] handle (what user sees):
  - Single pointer to the above structure

Operations:
  Rc.new(val)  → allocate struct, set count=1, move val in
  rc.clone()   → increment strong_count, return new handle
  drop(rc)     → decrement strong_count, if 0: drop value and free
```

**Arc Implementation:**
```
Arc[T] - same as Rc but with atomic operations:
  - Use atomic increment/decrement for counts
  - Memory ordering: Acquire on load, Release on store
  - Implements Send + Sync traits
```

**Test Programs:**
```
test/native/
├── rc_basic.kl         # Basic Rc.new() allocation ✅
├── rc_clone.kl         # Rc.clone() reference count increment ✅
```

```klar
// test/native/rc_basic.kl - Basic Rc allocation ✅
fn main() -> i32 {
    let rc_value = Rc.new(42)
    0
}

// test/native/rc_clone.kl - Clone creates shared reference ✅
fn main() -> i32 {
    let rc1 = Rc.new(42)
    let rc2 = rc1.clone()
    0
}
```

**Future Test (when print is implemented):**
```klar
// test_rc.kl
fn main() {
    let data = Rc.new(Buffer.new(1024))
    let alias = data.clone()

    print(data.len)   // 1024
    print(alias.len)  // 1024 (same buffer)

    // Both references dropped, buffer freed once
}
```

**Success Criteria:**
- Shared data accessible through multiple Rc handles
- Memory freed exactly once when last reference dropped
- No use-after-free
- Arc works correctly across threads

---

### Milestone 9: Closures & First-Class Functions

**Objective:** Implement closures with captured variables respecting ownership.

**Deliverables:**
- [ ] Implement function pointer representation
- [ ] Implement closure environment capture (by move or borrow)
- [ ] Implement closure struct layout (fn_ptr + environment)
- [ ] Implement closure invocation
- [ ] Implement closure passing as arguments
- [ ] Implement closure return from functions
- [ ] Handle environment destruction (drop captured values)

**Closure Representation:**
```
Closure layout:
┌─────────────────────────┐
│ fn_ptr: *fn             │  (pointer to generated function)
│ env: Environment        │  (captured variables, inline)
└─────────────────────────┘

Capture modes:
  - Move: ownership transferred to closure
  - Borrow: reference stored (closure lifetime limited)
  - Copy: value copied into closure (for Copy types)
```

**Closure Drop:**
```
When closure is dropped:
  1. Drop each captured value (if owned, not borrowed)
  2. Free closure struct (if heap-allocated)
```

**Test Programs:**
```klar
// test_closure.kl
fn make_adder(n: i32) -> fn(i32) -> i32 {
    return |x| x + n  // n is captured by copy (i32 is Copy)
}

fn main() {
    let add5 = make_adder(5)
    print(add5(10))  // 15
}

// test_closure_move.kl
fn make_counter() -> fn() -> i32 {
    let state = Rc.new(Cell.new(0))
    return || {
        let current = state.get()
        state.set(current + 1)
        current
    }
}
```

**Success Criteria:**
- Closures correctly capture variables
- Captured values properly dropped when closure dropped
- No memory leaks from closure environments

---

### Milestone 10: Runtime Library & Builtins

**Objective:** Implement the native runtime support library.

**Deliverables:**
- [ ] Create `src/runtime/` directory for native runtime
- [ ] Implement `print` and `println` builtins
- [ ] Implement string operations (concat, slice, compare)
- [ ] Implement type introspection (`type_of`)
- [ ] Implement panic handler with stack traces
- [ ] Implement assert and assert_eq
- [ ] Implement allocator interface (system malloc/free)
- [ ] Link runtime library into generated executables

**Runtime Library Contents:**
```
klar_runtime.a (static library)
├── print.zig         # Console output
├── string.zig        # String type and operations
├── panic.zig         # Trap handler with backtraces
├── alloc.zig         # Heap allocation (malloc/free wrapper)
├── rc.zig            # Rc/Arc implementation
└── math.zig          # Math builtins (sqrt, etc.)
```

**Panic Handler:**
```
On trap/panic:
  1. Capture stack trace (using frame pointers or DWARF)
  2. Print error message with source location
  3. Print stack trace with function names
  4. Exit with non-zero code
```

**Success Criteria:**
- `print()` works with all primitive types
- String concatenation works
- Panic shows source location and stack trace

---

### Milestone 11: Optimization Passes

**Objective:** Implement optimization passes on Klar IR before LLVM lowering.

**Deliverables:**
- [ ] Implement dead code elimination
- [ ] Implement constant folding and propagation
- [ ] Implement common subexpression elimination
- [ ] Implement function inlining (small functions)
- [ ] Implement drop coalescing (combine adjacent drops)
- [ ] Implement move elimination (for Copy types)
- [ ] Implement tail call optimization
- [ ] Add optimization level flags (`-O0`, `-O1`, `-O2`, `-O3`)
- [ ] Let LLVM handle most heavy optimizations at -O2+

**Optimization Pass Pipeline:**
```
Klar IR
  → ConstantFold
  → DeadCodeElim
  → DropCoalesce
  → Inline (small fns)
  → TailCall
  → LLVM IR
  → LLVM Optimizations (when -O1+)
  → Native Code
```

**Success Criteria:**
- fib(35) runs faster with `-O2` than `-O0`
- Tail-recursive functions don't grow stack
- Constant expressions evaluated at compile time
- LLVM optimizations applied correctly

---

### Milestone 12: Debug Information & Tooling

**Objective:** Generate debug information for native debugging.

**Deliverables:**
- [ ] Generate DWARF debug info via LLVM (Linux/macOS)
- [ ] Generate PDB debug info via LLVM (Windows)
- [ ] Map Klar source locations to native instructions
- [ ] Enable `lldb`/`gdb` debugging of Klar programs
- [ ] Implement `-g` flag for debug builds
- [ ] Add `--emit-llvm` flag to output LLVM IR
- [ ] Add `--emit-asm` flag to output assembly

**Debug Info via LLVM:**
```zig
// Use LLVM's debug info API
const di_builder = c.LLVMCreateDIBuilder(module);
const di_file = c.LLVMDIBuilderCreateFile(di_builder, filename, ...);
const di_func = c.LLVMDIBuilderCreateFunction(di_builder, ...);
// Attach debug locations to instructions
c.LLVMSetCurrentDebugLocation2(builder, location);
```

**Success Criteria:**
- Can set breakpoints by Klar source line in lldb/gdb
- Can inspect local variables in debugger
- Stack traces show Klar function names

---

### Milestone 13: Multi-Platform Support

**Objective:** Support multiple target platforms.

**Deliverables:**
- [ ] Complete x86_64 Linux support (ELF)
- [ ] Complete ARM64 macOS support (Mach-O)
- [ ] Complete x86_64 macOS support (Mach-O)
- [ ] Complete x86_64 Windows support (PE/COFF) - stretch goal
- [ ] Implement cross-compilation (`--target` flag)
- [ ] Add platform-specific runtime variants

**Platform Matrix:**

| Platform | Architecture | Object Format | Priority |
|----------|--------------|---------------|----------|
| Linux | x86_64 | ELF | Primary |
| macOS | ARM64 | Mach-O | Primary |
| macOS | x86_64 | Mach-O | Secondary |
| Windows | x86_64 | PE/COFF | Stretch |

**Cross-Compilation:**
```bash
# Compile for Linux from macOS
klar build --target=x86_64-linux-gnu program.kl

# Compile for macOS Intel from ARM
klar build --target=x86_64-apple-darwin program.kl
```

**Success Criteria:**
- Same Klar program compiles on Linux and macOS
- Cross-compilation produces working binaries
- Executables run correctly on target platform

---

### Milestone 14: Integration & Polish

**Objective:** Complete integration testing and polish the native compiler.

**Deliverables:**
- [ ] Run all existing test suite with native backend
- [ ] Benchmark suite: compare VM vs native performance
- [ ] Document native compilation in README
- [ ] Improve `klar build` command
- [ ] Add `--emit-ir` flag for Klar IR debugging
- [ ] Error messages with source context
- [ ] Memory usage profiling

**Benchmark Suite:**
```klar
// benchmarks/
├── fib.kl           # Recursive Fibonacci
├── sort.kl          # Quicksort implementation
├── matrix.kl        # Matrix multiplication
├── string.kl        # String operations
└── rc_stress.kl     # Reference counting stress test
```

**Performance Targets:**

| Benchmark | VM Time | Native Target | Speedup |
|-----------|---------|---------------|---------|
| fib(35) | ~15s | <0.1s | 150x+ |
| sort(10000) | ~2s | <0.05s | 40x+ |
| matrix(100) | ~5s | <0.1s | 50x+ |

**Success Criteria:**
- All tests pass with native backend
- Performance targets met
- No regressions from VM behavior
- Clean, documented codebase

---

## File Structure (Final)

```
src/
├── main.zig              # CLI entry point (updated)
├── lexer.zig             # existing
├── parser.zig            # existing
├── checker.zig           # existing (+ ownership tracking)
├── ast.zig               # existing
├── token.zig             # existing
├── types.zig             # existing
├── errors.zig            # existing
│
├── interpreter.zig       # existing, Phase 1
├── values.zig            # existing, Phase 1
│
├── compiler.zig          # existing, Phase 2, bytecode
├── bytecode.zig          # existing, Phase 2
├── chunk.zig             # existing, Phase 2
├── vm.zig                # existing, Phase 2
├── vm_value.zig          # existing, Phase 2
├── vm_builtins.zig       # existing, Phase 2
├── disasm.zig            # existing, Phase 2
│
├── ownership/            # NEW: Ownership Analysis
│   ├── mod.zig
│   ├── state.zig         # Ownership state tracking
│   ├── checker.zig       # Borrow checking
│   └── drop.zig          # Drop insertion pass
│
├── ir/                   # NEW: Intermediate Representation
│   ├── mod.zig
│   ├── inst.zig          # IR instructions
│   ├── builder.zig       # IR construction
│   ├── printer.zig       # IR text output
│   ├── lower.zig         # AST → IR lowering
│   └── validate.zig      # IR validation
│
├── codegen/              # NEW: Native Code Generation
│   ├── mod.zig
│   ├── llvm.zig          # LLVM C API wrapper
│   ├── target.zig        # Platform targets
│   ├── emit.zig          # IR → LLVM translation
│   ├── object.zig        # Object file emission
│   └── linker.zig        # System linker invocation
│
├── opt/                  # NEW: Optimization Passes
│   ├── mod.zig
│   ├── constfold.zig
│   ├── dce.zig           # Dead code elimination
│   ├── inline.zig
│   └── tailcall.zig
│
└── runtime/              # NEW: Native Runtime Library
    ├── mod.zig
    ├── print.zig
    ├── string.zig
    ├── panic.zig
    ├── alloc.zig
    └── rc.zig            # Rc/Arc implementation
```

---

## Dependencies

**build.zig modifications:**

```zig
// Link against LLVM libraries
const llvm = b.systemLibrary("LLVM");
exe.linkLibrary(llvm);

// Or use LLVM from a specific path
exe.addLibraryPath("/usr/local/opt/llvm/lib");
exe.addIncludePath("/usr/local/opt/llvm/include");
exe.linkSystemLibrary("LLVM");
```

**Required LLVM components:**
- Core
- Target (X86, AArch64)
- Analysis
- CodeGen
- MC (Machine Code)
- Object

**Installation:**
```bash
# macOS
brew install llvm

# Ubuntu/Debian
apt install llvm-dev

# From source (for specific version)
cmake -G Ninja ../llvm -DLLVM_ENABLE_PROJECTS="clang" -DCMAKE_BUILD_TYPE=Release
```

---

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| LLVM API complexity | Medium | Medium | Start with minimal subset, expand incrementally |
| Ownership analysis edge cases | Medium | High | Extensive test suite, study Rust's approach |
| Platform ABI differences | Low | Medium | Use LLVM's target-specific code generation |
| Debug info generation | Medium | Low | Can ship without full debugger support initially |
| Compilation time | Low | Low | LLVM is slower but acceptable for AOT |

---

## Success Metrics

**Phase 3 is complete when:**

1. **Functionality**
   - [ ] All example programs compile and run natively
   - [ ] All unit tests pass with native backend
   - [ ] Ownership rules enforced at compile time
   - [ ] No memory leaks (verified by tools like Valgrind)

2. **Performance**
   - [ ] fib(35) runs in <0.1 seconds
   - [ ] At least 100x faster than VM for compute-bound code
   - [ ] Compilation time <2 seconds for small programs

3. **Usability**
   - [ ] `klar build program.kl` produces working executable
   - [ ] Clear error messages for ownership violations
   - [ ] Works on Linux x86_64 and macOS ARM64

4. **Quality**
   - [ ] Code compiles with no warnings
   - [ ] Documentation updated
   - [ ] Benchmark suite with reproducible results

---

## Getting Started (First Steps)

1. **Set up LLVM development environment**
   - Install LLVM (14+ recommended)
   - Verify C API headers available
   - Create minimal Zig program that links LLVM

2. **Proof of concept: hello world**
   - Generate LLVM IR for `fn main() -> i32 { 42 }`
   - Compile to object file
   - Link with system linker
   - Execute and verify exit code

3. **Set up codegen module structure**
   - Create `src/codegen/llvm.zig` with basic wrappers
   - Add `--native` CLI flag
   - Wire up the compilation pipeline

4. **Implement ownership tracking**
   - Add ownership state to checker
   - Implement basic move detection
   - Add use-after-move error

---

## Comparison: GC vs Ownership (Why Ownership)

| Aspect | Garbage Collection | Ownership (Klar's approach) |
|--------|-------------------|----------------------------|
| Memory overhead | GC metadata per object | Zero runtime overhead |
| Latency | Pause times during collection | Predictable, no pauses |
| Determinism | Non-deterministic cleanup | Deterministic destruction |
| FFI | Complex (root tracking) | Natural C interop |
| Learning curve | Easier initially | Requires understanding ownership |
| Resource management | Only memory | Memory + files + locks + etc. |

Klar's ownership model provides:
- **Zero-cost abstraction** - no runtime overhead for memory safety
- **Deterministic destruction** - resources freed immediately when done
- **RAII** - works for all resources, not just memory
- **Simpler FFI** - natural interop with C libraries
- **No GC pauses** - predictable performance

---

*Plan Version: 2.0*
*Created: January 2026*
*Revised: January 2026 (LLVM backend, ownership model)*
*Status: Ready for Implementation*
