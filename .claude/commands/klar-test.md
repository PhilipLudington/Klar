# Klar Test Runner

**USE THIS SKILL** when running Klar test files. Always use this instead of manual `./zig-out/bin/klar` bash commands when:
- Testing any `.kl` file to verify it works correctly
- Running files in `test/` or `test/native/` directories
- Wanting to quickly check if code compiles and runs

Run Klar source files for testing.

## Arguments

- `$ARGUMENTS` - Required: path to .kl file(s) to test

## Instructions

**IMPORTANT**: Always use `klar run` for testing. It compiles natively and executes in one step, avoiding unnecessary permission prompts.

### Single File Test

```bash
./zig-out/bin/klar run <file.kl>
```

### Multiple Files

If multiple files are provided, run each one:

```bash
./zig-out/bin/klar run <file1.kl>
./zig-out/bin/klar run <file2.kl>
```

### Test Directory Pattern

If a glob pattern is provided (e.g., `test/native/typeinfo*.kl`), use Glob tool first to find files, then run each.

### Execution Modes

- `klar run file.kl` - Native compilation (default, preferred)
- `klar run file.kl --vm` - Bytecode VM
- `klar run file.kl --interpret` - Tree-walking interpreter

### When to Use Build Instead

Only use `klar build` when you need:
- A persistent executable (`-o output`)
- Cross-compilation (`--target`)
- LLVM IR output (`--emit-llvm`)
- Debug symbols (`-g`)

### Output Interpretation

- Exit code 0 = test passed
- Non-zero exit code = test failed (report the exit code)
- Show any printed output to the user

## Examples

```
/klar-test test/native/hello.kl
/klar-test test/native/typeinfo_basic.kl
```
