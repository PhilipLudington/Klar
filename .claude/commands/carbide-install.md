# CarbideZig Install

Install CarbideZig into an existing Zig project.

## Arguments

- `$ARGUMENTS` - Not used

## Instructions

Integrate CarbideZig into the current project by following these steps:

### 1. Clone CarbideZig into the project

```bash
git clone https://github.com/PhilipLudington/CarbideZig.git carbide
rm -rf carbide/.git
```

### 2. Copy Claude Code integration

```bash
mkdir -p .claude/commands .claude/rules
cp carbide/commands/*.md .claude/commands/
cp carbide/rules/*.md .claude/rules/
```

### 3. Optionally copy build templates

If starting fresh or want CarbideZig build templates:

```bash
cp carbide/templates/build.zig .
cp carbide/templates/build.zig.zon .
```

### 4. Add reference to CLAUDE.md

Add this line to your project's `.claude/CLAUDE.md` (create if needed):

```markdown
CarbideZig is used in this project for Zig development standards. See `carbide/CARBIDE.md` and `carbide/STANDARDS.md`.
```

### 5. Verify installation

Confirm these directories contain markdown files:
- `.claude/commands/` (should have carbide-*.md files)
- `.claude/rules/` (should have api-design.md, memory.md, etc.)

## Available Commands After Installation

- `/carbide-review` - Review code against CarbideZig standards
- `/carbide-safety` - Security-focused review
- `/carbide-check` - Run validation (zig build test, fmt check)
- `/carbide-init` - Create a new CarbideZig project
- `/carbide-update` - Update to latest CarbideZig version
