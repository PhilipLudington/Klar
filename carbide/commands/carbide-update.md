# CarbideZig Update

Update CarbideZig to the latest version.

## Arguments

- `$ARGUMENTS` - Not used

## Instructions

Update the CarbideZig installation to the latest version:

### 1. Remove existing CarbideZig and clone latest

```bash
rm -rf carbide
git clone https://github.com/PhilipLudington/CarbideZig.git carbide
rm -rf carbide/.git
```

### 2. Update Claude Code integration

```bash
cp carbide/commands/*.md .claude/commands/
cp carbide/rules/*.md .claude/rules/
```

### 3. Optionally update build templates

If you want the latest build templates (will overwrite existing):

```bash
cp carbide/templates/build.zig .
cp carbide/templates/build.zig.zon .
```

### 4. Verify update

Confirm these directories contain updated files:
- `.claude/commands/` (should have carbide-*.md files)
- `.claude/rules/` (should have api-design.md, memory.md, etc.)

Check the carbide directory for updated STANDARDS.md and CARBIDE.md.

## Notes

- This preserves your project's source code and configuration
- Only CarbideZig framework files are updated
- Review STANDARDS.md for any new or changed guidelines
