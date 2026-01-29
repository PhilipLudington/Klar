# Klar FFI Specification

**Status:** Draft
**Purpose:** Enable Klar to interface with C code, specifically for seL4 integration in the Kove operating system.

## Overview

This document specifies Foreign Function Interface (FFI) extensions to the Klar programming language. These extensions allow Klar code to:

1. Call functions written in C
2. Define types with C-compatible memory layout
3. Work with raw pointers when necessary
4. Clearly mark unsafe operations

All FFI features require explicit `unsafe` blocks, maintaining Klar's philosophy of "no surprises."

---

## 1. External Function Declarations

### Syntax

```klar
extern {
    fn function_name(param: Type, ...) -> ReturnType
}
```

### Semantics

- Functions declared in `extern` blocks use the C calling convention
- The compiler emits LLVM IR with `ccc` (C calling convention)
- These functions are resolved at link time
- Calling any `extern` function requires an `unsafe` block

### Examples

```klar
extern {
    // Basic function
    fn puts(s: CStr) -> i32

    // Function with out parameter
    fn seL4_Recv(src: seL4_CPtr, sender: out seL4_CPtr) -> seL4_MessageInfo

    // Variadic function
    fn printf(fmt: CStr, ...) -> i32

    // No return value
    fn seL4_Send(dest: seL4_CPtr, msg: seL4_MessageInfo) -> void
}
```

### Out Parameters

The `out` keyword indicates a pointer parameter that the callee writes to:

```klar
extern {
    fn seL4_Recv(src: seL4_CPtr, sender: out seL4_CPtr) -> seL4_MessageInfo
}

// Usage
fn example() {
    let sender: seL4_CPtr    // Uninitialized, will be written by callee
    unsafe {
        let info = seL4_Recv(endpoint, out sender)
        // sender is now initialized
    }
}
```

The compiler:
1. Allocates stack space for `sender`
2. Passes a pointer to that space to the C function
3. After the call, `sender` is considered initialized

---

## 2. External Types

### Opaque Types

For C types whose internal structure Klar doesn't need to know:

```klar
extern type seL4_CPtr
extern type FILE
```

Opaque types:
- Have unknown size (cannot be stack-allocated directly)
- Can only be used behind pointers: `CPtr[FILE]`, `COptPtr[seL4_CPtr]`
- Cannot be constructed in Klar code

### Sized External Types

For opaque types with known size (handles, capabilities):

```klar
extern type(8) seL4_CPtr    // 8 bytes, can be passed by value
extern type(16) Uuid        // 16 bytes
```

Sized external types:
- Can be passed by value
- Can be stored in structs
- Cannot have their internals accessed

---

## 3. C-Compatible Layout

### Structs

Use `extern struct` to guarantee C-compatible memory layout:

```klar
extern struct seL4_MessageInfo {
    label: u64,
    caps_unwrapped: u64,
    extra_caps: u64,
    length: u64,
}
```

Without `extern`:
- Klar may reorder fields for efficiency
- Klar may add padding differently than C

With `extern struct`:
- Fields are laid out in declaration order
- Padding follows C ABI rules for the target platform
- The struct can be passed to/from C functions

### Enums

Use `extern enum` with explicit integer type for C-compatible enums:

```klar
extern enum seL4_Error: u32 {
    NoError = 0,
    InvalidArgument = 1,
    InvalidCapability = 2,
    IllegalOperation = 3,
    RangeError = 4,
    AlignmentError = 5,
    FailedLookup = 6,
    TruncatedMessage = 7,
    DeleteFirst = 8,
    RevokeFirst = 9,
    NotEnoughMemory = 10,
}
```

The `: u32` specifies the underlying integer representation.

### Packed Structs

For structs with no padding:

```klar
extern struct packed PackedData {
    a: u8,
    b: u32,    // No padding before this field
    c: u8,
}
```

Warning: Packed structs may cause unaligned access on some architectures.

---

## 4. Pointer Types

FFI requires raw pointer types that don't exist in safe Klar:

### CPtr[T] — Non-null Pointer

```klar
type CPtr[T]    // Equivalent to T* in C, assumed non-null
```

- Represents a non-null pointer to T
- Dereferencing requires `unsafe`
- Can be obtained from references using `ref_to_ptr`

### COptPtr[T] — Nullable Pointer

```klar
type COptPtr[T]    // Equivalent to T* in C, may be null
```

- Represents a potentially null pointer to T
- Must check for null before dereferencing
- Common return type for allocation functions

### CStr — C String

```klar
type CStr    // *const char, null-terminated, borrowed
```

- Pointer to null-terminated UTF-8 (or ASCII) string
- Does not own the memory
- Lifetime tied to source

### Pointer Operations

```klar
// Check for null
fn is_null[T](ptr: COptPtr[T]) -> bool

// Convert nullable to non-null (unsafe, caller must verify)
fn unwrap_ptr[T](ptr: COptPtr[T]) -> CPtr[T]

// Pointer arithmetic
fn offset[T](ptr: CPtr[T], count: isize) -> CPtr[T]

// Dereference (read)
fn read[T](ptr: CPtr[T]) -> T

// Dereference (write)
fn write[T](ptr: CPtr[T], value: T) -> void

// Cast between pointer types
fn cast[T, U](ptr: CPtr[T]) -> CPtr[U]
```

All pointer operations except `is_null` require `unsafe`.

---

## 5. Unsafe Blocks

### Syntax

```klar
unsafe {
    // Unsafe operations allowed here
}
```

### What Requires Unsafe

1. Calling `extern` functions
2. Dereferencing raw pointers
3. Converting between pointers and references
4. Accessing mutable global state (FFI globals)
5. Inline assembly

### Unsafe Functions

Functions that are inherently unsafe can be marked:

```klar
unsafe fn dangerous_operation(ptr: CPtr[u8]) -> void {
    // Entire function body is implicitly unsafe
    write(ptr, 0)
}

// Must be called from unsafe context
fn example() {
    unsafe {
        dangerous_operation(some_ptr)
    }
}
```

### Unsafe Traits

For traits that have safety invariants the compiler can't check:

```klar
unsafe trait RawHandle {
    fn as_raw(self: ref Self) -> CPtr[void]
}

// Implementing an unsafe trait requires unsafe impl
unsafe impl RawHandle for FileDescriptor {
    fn as_raw(self: ref Self) -> CPtr[void] {
        // ...
    }
}
```

---

## 6. String Conversions

### Klar String to C String

```klar
impl string {
    // Borrow as C string (adds null terminator, valid while string lives)
    fn as_cstr(self: ref Self) -> CStr

    // Copy to owned C string
    fn to_cstr(self: ref Self) -> CStrOwned
}
```

### C String to Klar String

```klar
impl CStr {
    // Copy C string to Klar string (requires unsafe, reads from pointer)
    unsafe fn to_string(self: Self) -> string

    // Get length without copying
    unsafe fn len(self: Self) -> usize
}
```

### Example

```klar
extern {
    fn getenv(name: CStr) -> COptPtr[i8]
}

fn get_env_var(name: string) -> ?string {
    unsafe {
        let result = getenv(name.as_cstr())

        if result.is_null() {
            return none
        }

        // Cast to CStr and convert
        let cstr = CStr.from_ptr(result.unwrap_ptr().cast[i8]())
        return some(cstr.to_string())
    }
}
```

---

## 7. Memory Management Across FFI Boundary

### Allocating in C, Freeing in C

```klar
extern {
    fn malloc(size: usize) -> COptPtr[void]
    fn free(ptr: CPtr[void]) -> void
}

fn allocate_buffer(size: usize) -> ?CPtr[u8] {
    unsafe {
        let ptr = malloc(size)
        if ptr.is_null() {
            return none
        }
        return some(ptr.unwrap_ptr().cast[u8]())
    }
}
```

### Passing Klar Data to C

When C needs to hold a pointer to Klar data:

```klar
// Pin data in place and get raw pointer
fn with_pinned[T, R](value: ref T, f: fn(CPtr[T]) -> R) -> R {
    unsafe {
        let ptr = ref_to_ptr(value)
        f(ptr)
        // value must remain valid until f returns
    }
}
```

### Transferring Ownership to C

```klar
// Convert Klar-owned value to C-owned
fn into_raw[T](value: T) -> CPtr[T] {
    unsafe {
        let ptr = allocate[T]()
        write(ptr, value)
        // Klar forgets about value, C now owns it
        return ptr
    }
}

// Take ownership back from C
fn from_raw[T](ptr: CPtr[T]) -> T {
    unsafe {
        let value = read(ptr)
        free(ptr.cast[void]())
        return value
    }
}
```

---

## 8. Complete Example: seL4 Bindings

```klar
// ============================================
// src/platform/seL4_raw.kl
// Low-level seL4 bindings
// ============================================

module seL4_raw

// Capability pointer - word-sized handle
extern type(8) seL4_CPtr

// Message info - passed by value
pub extern struct seL4_MessageInfo {
    words: u64,
}

// Error codes
pub extern enum seL4_Error: u32 {
    NoError = 0,
    InvalidArgument = 1,
    InvalidCapability = 2,
    IllegalOperation = 3,
    RangeError = 4,
    AlignmentError = 5,
    FailedLookup = 6,
    TruncatedMessage = 7,
    DeleteFirst = 8,
    RevokeFirst = 9,
    NotEnoughMemory = 10,
}

// IPC Buffer - accessed via thread-local pointer
extern type seL4_IPCBuffer

extern {
    // IPC primitives
    fn seL4_Send(dest: seL4_CPtr, msg_info: seL4_MessageInfo) -> void
    fn seL4_Recv(src: seL4_CPtr, sender: out seL4_CPtr) -> seL4_MessageInfo
    fn seL4_Call(dest: seL4_CPtr, msg_info: seL4_MessageInfo) -> seL4_MessageInfo
    fn seL4_Reply(msg_info: seL4_MessageInfo) -> void
    fn seL4_NBSend(dest: seL4_CPtr, msg_info: seL4_MessageInfo) -> void
    fn seL4_Wait(src: seL4_CPtr, sender: out seL4_CPtr) -> seL4_MessageInfo
    fn seL4_Poll(src: seL4_CPtr, sender: out seL4_CPtr) -> seL4_MessageInfo

    // Message registers
    fn seL4_GetMR(i: i32) -> u64
    fn seL4_SetMR(i: i32, value: u64) -> void

    // IPC buffer
    fn seL4_GetIPCBuffer() -> CPtr[seL4_IPCBuffer]

    // Debug (if configured)
    fn seL4_DebugPutChar(c: u8) -> void
}

// Helper to build message info
pub fn make_message_info(label: u64, num_mrs: u64) -> seL4_MessageInfo {
    // seL4 message info format: label | capsUnwrapped | extraCaps | length
    return seL4_MessageInfo {
        words: (label << 12) | (num_mrs & 0x7f),
    }
}

pub fn get_label(info: seL4_MessageInfo) -> u64 {
    return info.words >> 12
}

pub fn get_length(info: seL4_MessageInfo) -> u64 {
    return info.words & 0x7f
}
```

```klar
// ============================================
// src/platform/ipc.kl
// Safe Klar wrapper over seL4 IPC
// ============================================

module ipc

import seL4_raw

/// An endpoint capability for IPC
pub struct Endpoint {
    cap: seL4_raw::seL4_CPtr,
}

/// A message that can be sent/received
pub struct Message {
    label: u64,
    data: [u64; 4],
    len: usize,
}

impl Message {
    pub fn new(label: u64) -> Self {
        return Message {
            label: label,
            data: [0, 0, 0, 0],
            len: 0,
        }
    }

    pub fn with_data(label: u64, data: [u64; 4], len: usize) -> Self {
        return Message {
            label: label,
            data: data,
            len: len,
        }
    }

    pub fn push(self: inout Self, value: u64) -> Result[void, IpcError] {
        if self.len >= 4 {
            return Err(IpcError::MessageFull)
        }
        self.data[self.len] = value
        self.len = self.len + 1
        return Ok(())
    }
}

pub enum IpcError {
    MessageFull,
    InvalidCapability,
    SendFailed,
}

impl Endpoint {
    /// Send a message (blocks until received)
    pub fn send(self: ref Self, msg: ref Message) -> void {
        unsafe {
            for i in 0..msg.len {
                seL4_raw::seL4_SetMR(i.as[i32], msg.data[i])
            }

            let info = seL4_raw::make_message_info(msg.label, msg.len.as[u64])
            seL4_raw::seL4_Send(self.cap, info)
        }
    }

    /// Receive a message (blocks until available)
    pub fn recv(self: ref Self) -> (Message, Endpoint) {
        unsafe {
            let sender: seL4_raw::seL4_CPtr
            let info = seL4_raw::seL4_Recv(self.cap, out sender)

            let label = seL4_raw::get_label(info)
            let len = seL4_raw::get_length(info).as[usize]

            let mut data: [u64; 4] = [0, 0, 0, 0]
            for i in 0..len.min(4) {
                data[i] = seL4_raw::seL4_GetMR(i.as[i32])
            }

            let msg = Message {
                label: label,
                data: data,
                len: len,
            }

            return (msg, Endpoint { cap: sender })
        }
    }

    /// Call: send a message and wait for reply
    pub fn call(self: ref Self, msg: ref Message) -> Message {
        unsafe {
            for i in 0..msg.len {
                seL4_raw::seL4_SetMR(i.as[i32], msg.data[i])
            }

            let info = seL4_raw::make_message_info(msg.label, msg.len.as[u64])
            let reply_info = seL4_raw::seL4_Call(self.cap, info)

            let label = seL4_raw::get_label(reply_info)
            let len = seL4_raw::get_length(reply_info).as[usize]

            let mut data: [u64; 4] = [0, 0, 0, 0]
            for i in 0..len.min(4) {
                data[i] = seL4_raw::seL4_GetMR(i.as[i32])
            }

            return Message {
                label: label,
                data: data,
                len: len,
            }
        }
    }
}
```

---

## 9. Compiler Implementation Notes

### LLVM IR Generation

For `extern` functions:

```llvm
; Declaration for seL4_Send
declare void @seL4_Send(i64 %dest, i64 %msg_info)

; Call site in Klar function
define void @klar_send_wrapper(i64 %dest, i64 %info) {
    call void @seL4_Send(i64 %dest, i64 %info)
    ret void
}
```

Ensure:
- Use `ccc` calling convention (default for LLVM, matches C)
- Match platform ABI for struct passing (by value vs. by pointer)
- Handle `out` parameters by allocating stack space and passing pointer

### Type Layout

For `extern struct` types, use LLVM's `DataLayout` to match the target C ABI:
- Respect natural alignment of each field
- Add padding between fields as C would
- Final struct size is padded to alignment

### Linking

Klar object files (`.o` from LLVM) link directly with C object files. No special runtime is needed for FFI—it's just function calls at the ABI level.

---

## 10. Function Pointers

Function pointers enable bidirectional FFI: passing Klar functions to C as callbacks and receiving/calling function pointers from C APIs.

### Function Pointer Type Syntax

The `extern fn` type represents a raw C function pointer:

```klar
// Basic function pointer types
extern fn(i32) -> i32           // Takes i32, returns i32
extern fn(i32, i32) -> void     // Takes two i32, returns void
extern fn(CPtr[void], CPtr[void]) -> i32  // qsort comparator signature
```

Unlike Klar closures (which are 16-byte structs containing function pointer + environment), `extern fn` is an 8-byte raw pointer compatible with C.

### Creating Function Pointers with `@fn_ptr`

The `@fn_ptr` builtin converts Klar functions to C function pointers:

```klar
// Named function
fn my_callback(x: i32) -> i32 {
    return x * 2
}

// Get function pointer
let fp: extern fn(i32) -> i32 = @fn_ptr(my_callback)
```

Stateless closures (closures without captures) can also be converted:

```klar
// Stateless closure - works
let add_one: extern fn(i32) -> i32 = @fn_ptr(|x: i32| -> i32 { return x + 1 })

// Closure with captures - COMPILE ERROR
let offset: i32 = 10
let bad: extern fn(i32) -> i32 = @fn_ptr(|x: i32| -> i32 { return x + offset })
// Error: Cannot create C function pointer from closure with captures
```

### Calling Function Pointers

Calling a function pointer requires `unsafe`:

```klar
let fp: extern fn(i32) -> i32 = @fn_ptr(my_func)
let result: i32 = unsafe { fp(42) }
```

### Function Pointer Parameters in Extern Functions

Extern functions can accept and return function pointers:

```klar
extern {
    // qsort - accepts a comparator callback
    fn qsort(
        base: CPtr[void],
        nmemb: usize,
        size: usize,
        compar: extern fn(CPtr[void], CPtr[void]) -> i32
    )

    // signal - accepts handler, returns old handler
    fn signal(
        signum: i32,
        handler: extern fn(i32) -> void
    ) -> extern fn(i32) -> void
}
```

### Optional Function Pointers

Use `?extern fn` for nullable function pointers:

```klar
fn get_callback() -> ?extern fn(i32) -> i32 {
    // Returns None implicitly
}

fn use_callback() {
    let cb: ?extern fn(i32) -> i32 = get_callback()
    match cb {
        Some(fp) => {
            let result: i32 = unsafe { fp(42) }
        }
        None => { }
    }
}
```

### Example: qsort

```klar
extern {
    fn qsort(
        base: CPtr[void],
        nmemb: usize,
        size: usize,
        compar: extern fn(CPtr[void], CPtr[void]) -> i32
    )
}

fn compare_i32(a: CPtr[void], b: CPtr[void]) -> i32 {
    let a_ptr: CPtr[i32] = unsafe { ptr_cast[i32](a) }
    let b_ptr: CPtr[i32] = unsafe { ptr_cast[i32](b) }
    let a_val: i32 = unsafe { read(a_ptr) }
    let b_val: i32 = unsafe { read(b_ptr) }
    return a_val - b_val
}

fn sort_array() {
    var arr: [i32; 5] = [5, 2, 8, 1, 9]
    let base: CPtr[void] = unsafe { ptr_cast[void](ref_to_ptr(ref arr)) }
    let cmp: extern fn(CPtr[void], CPtr[void]) -> i32 = @fn_ptr(compare_i32)
    unsafe { qsort(base, 5.as[usize], 4.as[usize], cmp) }
    // arr is now [1, 2, 5, 8, 9]
}
```

---

## 11. Future Extensions

### Inline Assembly

For truly low-level operations:

```klar
unsafe fn read_timestamp() -> u64 {
    let result: u64
    asm {
        "rdtsc",
        out("rax") result,
    }
    return result
}
```

### Global Variables

Accessing C global variables:

```klar
extern {
    static errno: i32
    static mut environ: CPtr[CPtr[i8]]
}
```

### Variadic Function Pointers

Function pointers to variadic C functions (deferred):

```klar
// Not yet implemented
extern fn(CStr, ...) -> i32  // printf-style callback
```

---

## Appendix: seL4 API Reference

Key seL4 functions to bind:

| Function | Purpose |
|----------|---------|
| `seL4_Send` | Send message, block until delivered |
| `seL4_Recv` | Block until message received |
| `seL4_Call` | Send + wait for reply (RPC) |
| `seL4_Reply` | Reply to last received message |
| `seL4_NBSend` | Non-blocking send |
| `seL4_Wait` | Wait for message (like Recv) |
| `seL4_Poll` | Non-blocking receive |
| `seL4_Yield` | Yield timeslice |
| `seL4_GetMR` | Get message register |
| `seL4_SetMR` | Set message register |
| `seL4_DebugPutChar` | Debug output (if enabled) |
