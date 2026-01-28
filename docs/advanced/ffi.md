# Foreign Function Interface (FFI)

Klar provides a Foreign Function Interface (FFI) for calling C libraries and interoperating with existing native code. All FFI operations are inherently unsafe and require explicit `unsafe` blocks.

## Table of Contents

- [Quick Start](#quick-start)
- [Unsafe Blocks](#unsafe-blocks)
- [External Functions](#external-functions)
- [Pointer Types](#pointer-types)
- [External Types](#external-types)
- [External Structs](#external-structs)
- [External Enums](#external-enums)
- [String Conversions](#string-conversions)
- [Function Pointers](#function-pointers)
- [Linking Libraries](#linking-libraries)

## Quick Start

Here's a complete example calling the C standard library:

```klar
// Declare external C functions
extern {
    fn puts(s: CStr) -> i32
    fn strlen(s: CStr) -> usize
}

fn main() -> i32 {
    let msg: string = "Hello from Klar FFI!"

    unsafe {
        // Convert Klar string to C string and call puts
        puts(msg.as_cstr())
    }

    return 0
}
```

## Unsafe Blocks

All FFI operations are unsafe because the compiler cannot verify memory safety across the C boundary. Use `unsafe { ... }` blocks to mark these operations:

```klar
// Calling C functions requires unsafe
unsafe {
    let result: i32 = c_function()
}

// Functions can be marked unsafe
unsafe fn dangerous_operation() -> void {
    // Body is implicitly unsafe context
}

// Calling an unsafe function also requires unsafe
unsafe {
    dangerous_operation()
}
```

### Unsafe Traits

For traits that have safety invariants the compiler can't verify, use `unsafe trait`:

```klar
// An unsafe trait - implementors must uphold certain invariants
unsafe trait RawHandle {
    fn as_raw(self: Self) -> i32
}

struct FileDescriptor {
    fd: i32,
}

// Implementing an unsafe trait requires unsafe impl
unsafe impl FileDescriptor: RawHandle {
    fn as_raw(self: FileDescriptor) -> i32 {
        return self.fd
    }
}
```

Unsafe traits:
- Have safety invariants that the compiler cannot verify
- Require `unsafe impl` when implementing
- Methods on unsafe traits can be called safely (unsafety is in the implementation contract)

## External Functions

Declare C functions using `extern` blocks:

```klar
extern {
    // Basic function
    fn exit(code: i32) -> void

    // Function returning a value
    fn getpid() -> i32

    // Function with pointer parameters
    fn malloc(size: usize) -> COptPtr[void]
    fn free(ptr: CPtr[void]) -> void

    // Variadic function (like printf)
    fn printf(format: CStr, ...) -> i32
}
```

### Parameter Modifiers

The `out` modifier indicates a parameter that will be written by the C function:

```klar
extern {
    // The result parameter is written by the function
    fn get_result(out result: CPtr[i32]) -> i32
}
```

## Pointer Types

Klar provides three pointer types for FFI:

### CPtr[T] - Non-null Pointer

A pointer that is guaranteed to be non-null:

```klar
let ptr: CPtr[i32] = ...

unsafe {
    // Read value at pointer
    let value: i32 = read(ptr)

    // Write value to pointer
    write(ptr, 42)

    // Pointer arithmetic
    let next: CPtr[i32] = offset(ptr, 1)
}
```

### COptPtr[T] - Nullable Pointer

A pointer that may be null (like C pointers):

```klar
let ptr: COptPtr[i32] = ...

// Safe null check (no unsafe needed)
if is_null(ptr) {
    println("Pointer is null")
} else {
    unsafe {
        // Unwrap to non-null pointer
        let non_null: CPtr[i32] = unwrap_ptr(ptr)
    }
}
```

### CStr - C String

A borrowed null-terminated string (pointer to `i8`):

```klar
extern {
    fn puts(s: CStr) -> i32
}

fn main() -> i32 {
    let msg: string = "Hello!"

    unsafe {
        // Convert Klar string to CStr
        puts(msg.as_cstr())
    }

    return 0
}
```

### Pointer Built-in Functions

| Function | Signature | Unsafe | Description |
|----------|-----------|--------|-------------|
| `is_null` | `fn[T](COptPtr[T]) -> bool` | No | Check if pointer is null |
| `unwrap_ptr` | `fn[T](COptPtr[T]) -> CPtr[T]` | Yes | Convert nullable to non-null |
| `offset` | `fn[T](CPtr[T], isize) -> CPtr[T]` | Yes | Pointer arithmetic |
| `read` | `fn[T](CPtr[T]) -> T` | Yes | Dereference pointer |
| `write` | `fn[T](CPtr[T], T) -> void` | Yes | Write through pointer |
| `ref_to_ptr` | `fn[T](ref T) -> CPtr[T]` | Yes | Get pointer to reference |

## External Types

Declare opaque C types that can only be used through pointers:

```klar
// Opaque type (unknown size, use behind pointers)
extern type FILE

// Sized external type (known size, can be embedded in structs)
extern type(8) pthread_t
```

Usage:

```klar
extern type FILE

extern {
    fn fopen(path: CStr, mode: CStr) -> COptPtr[FILE]
    fn fclose(file: CPtr[FILE]) -> i32
}

fn main() -> i32 {
    unsafe {
        let file: COptPtr[FILE] = fopen("/tmp/test.txt".as_cstr(), "w".as_cstr())
        if not is_null(file) {
            fclose(unwrap_ptr(file))
        }
    }
    return 0
}
```

## External Structs

Define C-compatible structs with `extern struct`:

```klar
// C-compatible struct layout
extern struct Point {
    x: f64,
    y: f64,
}

// Packed struct (no padding)
extern struct packed Header {
    magic: u16,
    version: u8,
    flags: u8,
}
```

External structs:
- Use C ABI-compatible memory layout
- Fields are laid out in declaration order
- Cannot have generic parameters
- Cannot have methods that take ownership

## External Enums

Define C-compatible enums with explicit integer representation:

```klar
extern enum Status: i32 {
    Ok = 0,
    Error = -1,
    Pending = 1,
}

extern enum Flags: u8 {
    None = 0,
    Read = 1,
    Write = 2,
    Execute = 4,
}
```

External enums:
- Require explicit integer representation type (`: i32`, `: u8`, etc.)
- Require explicit values for all variants
- Cannot have payload variants (no `Variant(T)`)

## String Conversions

### Klar String to C String

```klar
let klar_str: string = "Hello"

unsafe {
    // Borrow as CStr (no allocation)
    let c_str: CStr = klar_str.as_cstr()
    puts(c_str)
}
```

### C String to Klar String

```klar
extern {
    fn getenv(name: CStr) -> COptPtr[i8]
}

fn get_home() -> ?String {
    unsafe {
        let ptr: COptPtr[i8] = getenv("HOME".as_cstr())
        if is_null(ptr) {
            return None
        }
        // Convert C string to Klar String (copies data)
        let c_str: CStr = CStr::from_ptr(unwrap_ptr(ptr))
        return Some(c_str.to_string())
    }
}
```

### String Methods

| Method | Signature | Unsafe | Description |
|--------|-----------|--------|-------------|
| `as_cstr` | `fn(ref self) -> CStr` | No | Borrow string as C string |
| `to_cstr` | `fn(ref self) -> CStrOwned` | No | Copy to owned C string |
| `to_string` | `fn(self: CStr) -> String` | Yes | Copy C string to Klar String |
| `len` | `fn(self: CStr) -> usize` | Yes | Get C string length |
| `from_ptr` | `fn(CPtr[i8]) -> CStr` | No | Construct from pointer |

### CStrOwned - Owned C String

`CStrOwned` is an owned, null-terminated C string. Unlike `CStr` (which borrows), `CStrOwned` owns its memory and **automatically frees it when the variable goes out of scope**.

```klar
fn use_c_api() -> void {
    // to_cstr() allocates and copies the string data
    let owned: CStrOwned = "Hello, C!".to_cstr()

    unsafe {
        // Use the owned string with C APIs
        some_c_function(owned.as_cstr())

        // Get length
        let length: usize = owned.len()

        // Convert back to Klar String
        let klar_string: String = owned.to_string()
    }

    // owned is automatically freed here (calls free())
}
```

**Important:** `CStrOwned` calls `free()` when it goes out of scope. This means:
- Do **not** pass `CStrOwned` to C functions that take ownership of the string
- If a C function will free the string, use `CStr` with manual memory management instead
- The automatic cleanup prevents memory leaks in normal usage

| Method | Signature | Unsafe | Description |
|--------|-----------|--------|-------------|
| `as_cstr` | `fn(ref self) -> CStr` | No | Borrow as CStr |
| `to_string` | `fn(self) -> String` | Yes | Copy to Klar String |
| `len` | `fn(self) -> usize` | Yes | Get string length |

## Function Pointers

Klar supports C function pointers for passing callbacks to C code and receiving/calling function pointers from C APIs.

### The `extern fn` Type

The `extern fn` type represents a raw C function pointer (8 bytes), distinct from Klar closures (which are 16-byte structs containing both a function pointer and an environment pointer).

```klar
// Type syntax: extern fn(ParamTypes) -> ReturnType
let callback: extern fn(i32) -> i32
let handler: extern fn(i32) -> void
let comparator: extern fn(CPtr[void], CPtr[void]) -> i32
```

### Creating Function Pointers with `@fn_ptr`

Use the `@fn_ptr` builtin to convert a Klar function to a C-compatible function pointer:

```klar
// Named function
fn my_callback(x: i32) -> i32 {
    return x * 2
}

fn main() -> i32 {
    // Get function pointer from named function
    let fp: extern fn(i32) -> i32 = @fn_ptr(my_callback)

    // Call the function pointer (requires unsafe)
    let result: i32 = unsafe { fp(21) }
    println(result.to_string())  // prints "42"

    return 0
}
```

### Stateless Closures

Closures without captures can also be converted to function pointers:

```klar
// Stateless closure (no captures) - works
let add_one: extern fn(i32) -> i32 = @fn_ptr(|x: i32| -> i32 { return x + 1 })

// Closure with captures - COMPILE ERROR
let offset: i32 = 10
let bad: extern fn(i32) -> i32 = @fn_ptr(|x: i32| -> i32 { return x + offset })
// Error: Cannot create C function pointer from closure with captures
```

Closures with captures cannot become C function pointers because C function pointers have no way to store the captured environment.

### Calling Function Pointers

Calling a function pointer received from C requires `unsafe`:

```klar
extern {
    fn get_callback() -> extern fn(i32) -> i32
}

fn main() -> i32 {
    let cb: extern fn(i32) -> i32 = unsafe { get_callback() }

    // Call requires unsafe (raw pointer dereference)
    let result: i32 = unsafe { cb(42) }
    return result
}
```

### Function Pointer Parameters in Extern Functions

Declare extern functions that accept or return function pointers:

```klar
extern {
    // qsort callback
    fn qsort(
        base: CPtr[void],
        nmemb: usize,
        size: usize,
        compar: extern fn(CPtr[void], CPtr[void]) -> i32
    )

    // Signal handler
    fn signal(
        signum: i32,
        handler: extern fn(i32) -> void
    ) -> extern fn(i32) -> void
}
```

### Optional Function Pointers

Use `?extern fn` for nullable function pointers:

```klar
fn get_optional_callback() -> ?extern fn(i32) -> i32 {
    // Return None implicitly by not returning a value
}

fn get_some_callback() -> ?extern fn(i32) -> i32 {
    return @fn_ptr(my_func)  // Automatically wrapped in Some
}

fn use_callback() {
    let cb: ?extern fn(i32) -> i32 = get_optional_callback()

    match cb {
        Some(fp) => {
            let result: i32 = unsafe { fp(42) }
            println(result.to_string())
        }
        None => {
            println("No callback")
        }
    }
}
```

### Example: qsort Callback

A complete example using C's `qsort` function:

```klar
extern {
    fn qsort(
        base: CPtr[void],
        nmemb: usize,
        size: usize,
        compar: extern fn(CPtr[void], CPtr[void]) -> i32
    )
}

// Compare function for ascending order
fn compare_ascending(a: CPtr[void], b: CPtr[void]) -> i32 {
    let a_ptr: CPtr[i32] = unsafe { ptr_cast[i32](a) }
    let b_ptr: CPtr[i32] = unsafe { ptr_cast[i32](b) }
    let a_val: i32 = unsafe { read(a_ptr) }
    let b_val: i32 = unsafe { read(b_ptr) }
    return a_val - b_val
}

fn main() -> i32 {
    var arr: [i32; 5] = [5, 2, 8, 1, 9]

    let arr_ptr: CPtr[[i32; 5]] = unsafe { ref_to_ptr(ref arr) }
    let base: CPtr[void] = unsafe { ptr_cast[void](arr_ptr) }

    let cmp: extern fn(CPtr[void], CPtr[void]) -> i32 = @fn_ptr(compare_ascending)

    unsafe {
        qsort(base, 5.as[usize], 4.as[usize], cmp)
    }

    // arr is now [1, 2, 5, 8, 9]
    return 0
}
```

### Example: Signal Handler

Register a signal handler:

```klar
extern {
    fn signal(signum: i32, handler: extern fn(i32) -> void) -> extern fn(i32) -> void
}

fn my_handler(sig: i32) -> void {
    println("Received signal")
    return
}

fn main() -> i32 {
    let SIGTERM: i32 = 15
    let handler: extern fn(i32) -> void = @fn_ptr(my_handler)

    let old_handler: extern fn(i32) -> void = unsafe { signal(SIGTERM, handler) }

    // Later, restore the old handler
    unsafe { signal(SIGTERM, old_handler) }

    return 0
}
```

## Linking Libraries

When building Klar programs that use FFI, you may need to link additional libraries.

### Basic Usage

```bash
# Link with the math library
klar build program.kl -lm

# Link with multiple libraries
klar build program.kl -lm -lcurl -lssl

# Specify library search path
klar build program.kl -L/usr/local/lib -lmylib

# Combined flags
klar build program.kl -L/opt/homebrew/lib -lm -lcurl
```

### Flag Reference

| Flag | Description | Example |
|------|-------------|---------|
| `-l<lib>` | Link with library | `-lm` links `libm` |
| `-l <lib>` | Link with library (space separated) | `-l curl` links `libcurl` |
| `-L<path>` | Add library search path | `-L/usr/local/lib` |
| `-L <path>` | Add library search path (space separated) | `-L /opt/lib` |

### Common Libraries

| Library | Flag | Description |
|---------|------|-------------|
| libc | (implicit) | C standard library (always linked) |
| libm | `-lm` | Math functions (sin, cos, sqrt, etc.) |
| libpthread | `-lpthread` | POSIX threads |
| libcurl | `-lcurl` | HTTP/HTTPS client |
| libssl | `-lssl` | OpenSSL |

### Example: Using libm

```klar
extern {
    fn sqrt(x: f64) -> f64
    fn sin(x: f64) -> f64
    fn cos(x: f64) -> f64
    fn pow(base: f64, exp: f64) -> f64
}

fn main() -> i32 {
    unsafe {
        let x: f64 = sqrt(16.0)    // 4.0
        let y: f64 = pow(2.0, 10.0) // 1024.0
        println(x.to_string())
        println(y.to_string())
    }
    return 0
}
```

Build with:
```bash
klar build math_example.kl -lm
```

### Platform Notes

**macOS:**
- System libraries are automatically found
- Homebrew libraries typically in `/opt/homebrew/lib` (Apple Silicon) or `/usr/local/lib` (Intel)

**Linux:**
- Standard paths like `/usr/lib` and `/lib` are searched by default
- Use `-L` for non-standard locations

**Windows:**
- Uses MinGW for cross-compilation
- Native builds use MSVC linker

## Complete Example: seL4 Bindings

Here's a more complete example showing FFI patterns for system programming:

```klar
// External types
extern type(8) SeL4CPtr
extern type(8) SeL4Word

// C-compatible structs
extern struct SeL4MessageInfo {
    words: [SeL4Word; 2],
}

// External enums
extern enum SeL4Error: i32 {
    NoError = 0,
    InvalidArgument = 1,
    InvalidCapability = 2,
    IllegalOperation = 3,
}

// External functions
extern {
    fn seL4_Send(dest: SeL4CPtr, info: SeL4MessageInfo) -> void
    fn seL4_Recv(src: SeL4CPtr, out sender: SeL4Word) -> SeL4MessageInfo
    fn seL4_Call(dest: SeL4CPtr, info: SeL4MessageInfo) -> SeL4MessageInfo
}

// Wrapper struct with methods
pub struct Endpoint {
    cap: SeL4CPtr,
}

impl Endpoint {
    pub fn send(self: ref Self, info: SeL4MessageInfo) -> void {
        unsafe {
            seL4_Send(self.cap, info)
        }
    }

    pub fn call(self: ref Self, info: SeL4MessageInfo) -> SeL4MessageInfo {
        return unsafe {
            seL4_Call(self.cap, info)
        }
    }
}
```

## Safety Guidelines

1. **Minimize unsafe blocks**: Keep `unsafe` blocks as small as possible
2. **Wrap unsafe operations**: Create safe wrapper functions/methods
3. **Validate pointers**: Always check for null before dereferencing
4. **Manage lifetimes**: Ensure C strings and pointers outlive their use
5. **Document assumptions**: Note what invariants C code expects
