# Bug Report: Klar `readline()` Function Not Implemented

## Status: RESOLVED

**Resolved**: 2026-01-22

## Summary

The `readline()` function is documented in the Klar Language Reference (v0.4.1) and is now fully implemented across all three backends (interpreter, VM, and native compilation).

## Implementation Details

The `readline()` function:
- Reads a line from stdin
- Strips trailing newlines
- Returns empty string on EOF
- Signature: `fn readline() -> string`

### Files Modified

1. `src/checker.zig` - Type signature registration
2. `src/values.zig` - Added IOError to RuntimeError enum
3. `src/vm_value.zig` - Added IOError to RuntimeError enum
4. `src/interpreter.zig` - Interpreter implementation
5. `src/vm_builtins.zig` - VM native function
6. `src/codegen/emit.zig` - LLVM native codegen

## Verification

```klar
fn main() {
    println("Enter your name:")
    let name: string = readline()
    println("Hello, {name}!")
}
```

Run with:
```bash
# Native compilation (default)
echo "World" | klar run test.kl
# Output: Enter your name:
#         Hello, World!

# VM backend
echo "World" | klar run test.kl --vm

# Interpreter backend
echo "World" | klar run test.kl --interpret
```

## Original Report

### Environment

- **Klar Version**: 0.4.1 (Phase 4 - Language Completion)
- **Reference Location**: `Klar-Toolkit/deps/Klar-Reference/REFERENCE.md`, line 882
- **Platform**: macOS (Darwin 24.6.0)

### Original Issue

The `readline()` function was documented but returned:
```
Type error(s):
  1:57: undefined variable 'readline'
  1:57: cannot call non-function type
```

### Date Reported

2026-01-22

### Date Resolved

2026-01-22
