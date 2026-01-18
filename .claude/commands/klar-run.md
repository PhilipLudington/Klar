# Klar Run

**USE THIS SKILL** when running Klar programs. Always use this instead of manual `./zig-out/bin/klar run` bash commands when:
- Executing any `.kl` file to see its output
- Running examples or demos
- Quick iteration during development

Execute a Klar program directly without creating a persistent binary.

## Arguments

- `$ARGUMENTS` - Required: path to .kl file and optional flags

## Instructions

Use `klar run` to compile and execute in one step:

```bash
./zig-out/bin/klar run <file.kl> [flags]
```

### Available Flags

- `--vm` - Use bytecode VM instead of native compilation
- `--interpret` - Use tree-walking interpreter
- (default) - Native compilation via LLVM

### Examples

```bash
# Run with native compilation (default, fastest)
./zig-out/bin/klar run program.kl

# Run with bytecode VM
./zig-out/bin/klar run program.kl --vm

# Run with interpreter (useful for debugging)
./zig-out/bin/klar run program.kl --interpret
```

### Output

- Show any printed output from the program
- Report the exit code if non-zero
- Show any compilation errors that occur

## Examples

```
/klar-run examples/hello.kl
/klar-run test/native/generics_basic.kl --vm
```
