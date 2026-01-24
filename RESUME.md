# Command-Line Arguments Implementation Progress

## Goal
Enable Klar programs to receive command-line arguments via `fn main(args: [String]) -> i32`.

## Completed

### Phase 1: Type Checker Validation ✅
**File**: `src/checker.zig`

Added main signature validation in two places:
1. `checkFunction` (~line 8219-8243) - validates when checking individual functions
2. `checkModule` second pass (~line 9878-9902) - validates during module-level function signature checking

Validation rules:
- `main()` accepts at most one parameter
- If one parameter, it must be `[String]` (slice of String/string_data type)
- Return type must be `i32` or `void`

**Test files created**:
- `scratch/test_main_valid.kl` - valid main() with no args
- `scratch/test_main_with_args.kl` - valid main(args: [String])
- `scratch/test_main_invalid_param.kl` - invalid: main(x: i32)
- `scratch/test_main_too_many_params.kl` - invalid: main with 2+ params

### Phase 2: Native Codegen ✅
**File**: `src/codegen/emit.zig`

1. **Added `main_takes_args` flag** (~line 109) - tracks if main takes [String] args

2. **Modified `declareFunction`** (~line 425-497):
   - Detects `main(args: [String])`
   - Sets `main_takes_args = true`
   - Declares function as `_klar_user_main` instead of `main`

3. **Added `isStringSliceTypeExpr`** (~line 487-497):
   - Helper to check if a TypeExpr is `[String]`

4. **Modified `emitFunction`** (~line 728-744):
   - Looks up `_klar_user_main` when emitting main with args

5. **Added `emitMainArgsWrapper`** (~line 407-471):
   - Generates C-style `main(argc, argv)` wrapper
   - Calls `_klar_args_from_argv` to convert to [String]
   - Calls `_klar_user_main` with the args
   - Calls `_klar_args_free` to cleanup

6. **Added `emitArgsFromArgvFn`** (~line 512-647):
   - Generates inline LLVM IR for argc/argv to [String] conversion
   - **Skips argv[0]** (the executable path)
   - Allocates array of String structs for argv[1..argc]
   - For each argv[i+1]: strlen, malloc, memcpy, store to String struct
   - Returns slice { ptr, len } with length = argc - 1

7. **Added `emitArgsFreeeFn`** (~line 650-733):
   - Generates inline LLVM IR to free [String] slice
   - Frees each string's data buffer
   - Frees the array

8. **Added helper functions**:
   - `getOrCreateMallocFn` (~line 736-745)
   - `getOrCreateStrlenFn` (~line 748-757)
   - `getOrCreateMemcpyFn` (~line 760-769)
   - `getOrCreateFreeFn` (~line 772-780)
   - `getOrCreateSnprintfFn` (~line 783-796)

### Phase 2.5: Bugfixes ✅
**Files**: `src/checker.zig`, `src/codegen/emit.zig`

Fixed issues discovered while debugging `args.len()`:

1. **`to_string()` return type** (`checker.zig:4205`):
   - Changed `to_string()` to return `String` (stringDataType) instead of `string` (primitive stringType)

2. **`print`/`println` type checking** (`checker.zig:3529-3543`):
   - Added special handling to accept both `string` literals and `String` objects

3. **`print`/`println` codegen** (`emit.zig:10130-10162`):
   - Updated `emitPrint` to extract data pointer from `String` structs before passing to `puts`/`printf`

4. **`to_string()` for integers** (`emit.zig:7758-7764`, `emit.zig:11496-11557`):
   - Added `emitIntToString` function to convert integers to String
   - Uses snprintf to format, allocates heap memory, returns String struct

5. **Relaxed main return type** (`checker.zig:8237-8243`, `checker.zig:9896-9902`):
   - Allow `void` return type in addition to `i32` (for backward compatibility)

### Phase 3: Pass Args Through CLI ✅
**File**: `src/main.zig`

1. **Modified run command handling** (~line 46-91):
   - Parses klar flags (--vm, --debug, --interpret)
   - Collects program args (non-flag args or everything after `--`)
   - Passes program_args to runNativeFile

2. **Modified `runNativeFile`** (~line 921-983):
   - Accepts `program_args: []const []const u8` parameter
   - Builds argv as [temp_path, path, arg1, arg2, ...]
   - Uses `std.ArrayListUnmanaged` (Zig 0.15 pattern)

3. **Modified codegen `emitArgsFromArgvFn`** (`emit.zig:534-647`):
   - Now skips argv[0] (the executable path)
   - Returns argv[1..argc] as the args slice
   - So `klar run file.kl arg1 arg2` gives args = ["file.kl", "arg1", "arg2"]

### Phase 4: Interpreter Support ✅
**File**: `src/main.zig`

1. **Modified `runInterpreterFile`** (~line 197):
   - Added `program_args: []const []const u8` parameter

2. **Updated call site** (~line 82):
   - Pass `program_args` to `runInterpreterFile`

3. **Added args handling in main call** (~line 267-314):
   - Check if main takes args (`func.params.len > 0`)
   - Build `Value.array` of `Value.string` from program_args
   - Uses arena allocator for args array (no memory leak)
   - Pass args to `callFunction` when calling main

## Current Status
- All 446 tests pass
- `args.len()` works correctly
- `println(count.to_string())` works for integers
- Native binaries receive args correctly when run directly
- **`klar run` passes args to programs correctly (native)**
- **`klar run --interpret` passes args to programs correctly (interpreter)**

## Remaining Work

### Phase 5: VM Support
**File**: `src/vm.zig`

Need to:
1. Modify `callMain` to optionally accept args
2. Convert args to ObjArray of ObjString
3. Push onto stack before calling

## Test Commands

```bash
# Build compiler
./build.sh

# Check type validation
./zig-out/bin/klar check scratch/test_main_invalid_param.kl  # Should show error

# Run with args via CLI - native (Phase 3)
./zig-out/bin/klar run scratch/test_args_print.kl hello world  # Prints "3"
./zig-out/bin/klar run scratch/test_args_print.kl -- --help    # Passes "--help" to program

# Run with args via CLI - interpreter (Phase 4)
./zig-out/bin/klar run scratch/test_args_print.kl --interpret hello world  # Prints "3"

# Build and run directly
./zig-out/bin/klar build scratch/test_args_print.kl -o /tmp/test
/tmp/test file.kl hello world  # Prints "3" (skips argv[0], sees [file.kl, hello, world])

# View generated LLVM IR
./zig-out/bin/klar build scratch/test_args_print.kl --emit-llvm
cat test_args_print.ll
```

## Files Modified

| File | Changes |
|------|---------|
| `src/checker.zig` | Main signature validation, `to_string()` type fix, `print`/`println` type handling (~80 lines) |
| `src/codegen/emit.zig` | Main wrapper generation, args conversion IR (skips argv[0]), `emitPrint` fix, `emitIntToString` (~450 lines) |
| `src/main.zig` | Run command arg parsing, `runNativeFile` accepts program_args, `runInterpreterFile` accepts program_args and passes to main (~90 lines) |

## Generated LLVM IR Structure

For `fn main(args: [String]) -> i32`:

```llvm
; User's main renamed
define i32 @_klar_user_main({ ptr, i64 } %args) { ... }

; Args conversion (skips argv[0], returns argv[1..argc])
define { ptr, i64 } @_klar_args_from_argv(i32 %argc, ptr %argv) {
  ; user_argc = argc - 1
  ; allocates array for user_argc elements
  ; loops i from 0 to user_argc, reads argv[i+1]
  ; returns slice { ptr, user_argc }
}

; Args cleanup (inline)
define void @_klar_args_free({ ptr, i64 } %args) { ... }

; C-style wrapper
define i32 @main(i32 %argc, ptr %argv) {
  %args = call { ptr, i64 } @_klar_args_from_argv(i32 %argc, ptr %argv)
  %result = call i32 @_klar_user_main({ ptr, i64 } %args)
  call void @_klar_args_free({ ptr, i64 } %args)
  ret i32 %result
}
```
