# Bug: SIGSEGV with large stack arrays + file-read strings [FIXED]

## Summary

Programs crash with SIGSEGV when combining:
1. A large stack-allocated array (`[u8; 30000]`)
2. A string read from a file (`File.read_to_string`)
3. Complex loop operations on the array

The crash does NOT occur with:
- Smaller arrays (1000 elements works fine)
- String literals instead of file content
- Simple programs without loops

## Minimal Reproduction

```klar
fn main() -> i32 {
    let result: Result[String, IoError] = File.read_to_string("test.txt")
    if result.is_err() {
        return 1
    }
    let program: String = result.unwrap()
    run(program.as_str())
    return 0
}

fn run(program: string) {
    var tape: [u8; 30000] = @repeat(0.as[u8], 30000)
    var ptr: i32 = 0

    let code: [u8] = program.bytes()
    let len: i32 = code.len()

    var i: i32 = 0
    while i < len {
        let cmd: u8 = code.get(i).unwrap()

        // Any moderate loop causes crash
        if cmd == 43.as[u8] {  // '+'
            tape[ptr] = tape[ptr] +% 1.as[u8]
        }

        i = i + 1
    }
}
```

With `test.txt` containing: `++++++++++[>+++++++>++++++++++>+++>+<<<<-]`

## Error

```
Exit code: 139 (SIGSEGV)
```

## Analysis

### LLVM IR Pattern

The generated LLVM IR shows an inefficient pattern:

```llvm
; @repeat creates a temporary array and calls memset
%repeat_arr = alloca [30000 x i8], align 1
%1 = call ptr @memset(ptr %repeat_arr, i32 0, i64 30000)

; Then loads the ENTIRE 30000-byte array into a "register"
%repeat_val = load [30000 x i8], ptr %repeat_arr, align 1

; Allocates the actual variable
%tape = alloca [30000 x i8], align 1

; Stores the entire 30000-byte value
store [30000 x i8] %repeat_val, ptr %tape, align 1
```

This pattern:
1. Allocates 30KB on stack for temp array
2. Calls memset (good)
3. Loads 30KB into an LLVM "register" (problematic)
4. Allocates another 30KB for the final variable
5. Stores 30KB from register to variable

### Suspected Causes

1. **Double stack allocation**: 60KB total (temp + final) may exceed limits in certain configurations
2. **Large aggregate load/store**: Loading 30KB into an LLVM value may cause issues with certain backends or optimizations
3. **Interaction with file I/O**: The `String` from `File.read_to_string` may be using stack space that conflicts with the large array

### Why it only fails with file content

String literals are stored in the data section and accessed via pointer. File content is heap-allocated but the `String` structure itself and the slice from `.as_str()` use stack space. Combined with the 60KB array allocation, this may be pushing stack limits.

## Proposed Fix

Modify `emitRepeat` to avoid the load/store pattern for large arrays. Instead of:

```zig
// Current (problematic for large arrays):
const array_alloca = self.builder.buildAlloca(array_type, "repeat_arr");
// ... memset ...
return self.builder.buildLoad(array_type, array_alloca, "repeat_val");
```

The fix should integrate with variable declaration so that:
1. The variable's alloca is used directly (no temp array)
2. memset is called on the final location
3. No intermediate load/store of the entire array

This requires changes to how `@repeat` interacts with variable declarations.

### Alternative Fix

For now, a simpler fix could add a threshold (e.g., 4096 bytes) above which `emitRepeat` returns the alloca pointer directly instead of loading the value. The caller would need to handle this case.

## Workaround

Use smaller arrays:
```klar
var tape: [u8; 1000] = @repeat(0.as[u8], 1000)  // Works fine
```

## Test Case

Save this as `test.txt`:
```
++++++++++[>+++++++>++++++++++>+++>+<<<<-]
```

The program should run without crashing. Currently crashes with exit code 139.

## Environment

- Klar compiler with memset optimization for @repeat
- macOS Darwin 24.6.0
- LLVM via Homebrew

## Related

The `@repeat` function was previously generating N individual store instructions (30000 stores for 30000 elements). This was fixed by adding memset optimization, but the underlying issue with the load/store pattern for large arrays remains.

## Date

2026-01-23

## Resolution

**Fixed** on 2026-01-23.

The fix adds two new functions to `src/codegen/emit.zig`:

1. `tryGetLargeRepeatInfo` - Detects `@repeat` expressions with large arrays (> 4096 bytes)
2. `emitRepeatInto` - Directly initializes an alloca with memset, avoiding intermediate load/store

Both `let_decl` and `var_decl` handling now check for large `@repeat` and use the optimized path:
- Creates the variable's alloca first
- Calls `emitRepeatInto` which memsets directly into that alloca
- No temporary array allocation
- No loading the entire array as a value

Generated LLVM IR now produces:
```llvm
%tape = alloca [30000 x i8], align 1
%1 = call ptr @memset(ptr %tape, i32 0, i64 30000)
```

Instead of the problematic pattern:
```llvm
%repeat_arr = alloca [30000 x i8], align 1
%1 = call ptr @memset(ptr %repeat_arr, i32 0, i64 30000)
%repeat_val = load [30000 x i8], ptr %repeat_arr, align 1  ; Loading 30KB!
%tape = alloca [30000 x i8], align 1
store [30000 x i8] %repeat_val, ptr %tape, align 1
```
