# Klar Check

Type-check a Klar file without generating code.

## Arguments

- `$ARGUMENTS` - Required: path to .kl file(s) to check

## Instructions

Use `klar check` for fast type checking without compilation:

```bash
./zig-out/bin/klar check <file.kl>
```

### Single File

```bash
./zig-out/bin/klar check file.kl
```

### Multiple Files

If multiple files are provided, check each one:

```bash
./zig-out/bin/klar check file1.kl
./zig-out/bin/klar check file2.kl
```

### Glob Pattern

If a glob pattern is provided, use Glob tool first to find files, then check each.

### Output Interpretation

- No output = file is valid (types check correctly)
- Type errors are reported with file:line:column format
- Parse errors are reported if syntax is invalid

### When to Use

Use `klar check` when you want to:
- Quickly validate code without waiting for LLVM compilation
- Check for type errors during development
- Verify syntax is correct

For actually running the code, use `/klar-run` or `/klar-test` instead.

## Examples

```
/klar-check src/main.kl
/klar-check test/native/generics_basic.kl
```
