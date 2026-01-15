# CarbideZig

**Hardened Zig Development Standards for AI-Assisted Programming**

CarbideZig is a comprehensive coding standards framework designed for Claude Code, enabling developers to write safe, maintainable, and trustworthy Zig code with AI assistance.

## Quick Commands (No Setup)

For immediate use on any Zig codebase with Claude Code:

```
/carbide-review src/main.zig    # Review code against CarbideZig standards
/carbide-safety src/            # Security-focused review
```

## Creating a New Project

```
/carbide-init my-project
cd my-project
zig build          # Build
zig build test     # Run tests
zig build run      # Run executable
```

## Integrating Into Existing Projects

### Step 1: Download the installer

```bash
mkdir -p .claude/commands
curl -o .claude/commands/carbide-install.md \
  https://raw.githubusercontent.com/PhilipLudington/CarbideZig/main/commands/carbide-install.md
```

### Step 2: Run the installation

In Claude Code:

```
/carbide-install
```

### Step 3: Use CarbideZig commands

```
/carbide-review src/main.zig
/carbide-check
/carbide-update
```

## What Gets Installed

When you run `/carbide-install`, CarbideZig is cloned into a `carbide/` directory in your project:

- `carbide/` — CarbideZig framework (cloned from GitHub)
  - `STANDARDS.md` — Complete coding standards reference
  - `CARBIDE.md` — Quick reference card
  - `commands/` — Slash command source files
  - `rules/` — Rule source files
  - `templates/` — Build configuration templates
- `.claude/commands/` — 6 slash commands (copied from carbide/)
- `.claude/rules/` — 10 auto-loaded rules (copied from carbide/)

## Quick Start

After installation, build and run your project:

```bash
cd my-project
zig build          # Build
zig build run      # Run
zig build test     # Test
zig fmt src/       # Format
```

## Using AI-Assisted Development

Once `.claude/rules/` is in your project, Claude Code automatically loads the CarbideZig standards. You'll get:

- **Automatic rule enforcement** — Claude follows Zig best practices for memory, errors, naming, etc.
- **Slash commands** — Use `/carbide-review`, `/carbide-check`, and `/carbide-safety`
- **Context-aware suggestions** — Claude understands your project follows CarbideZig patterns

## Features

### Comprehensive Standards

- **[STANDARDS.md](STANDARDS.md)** — Complete coding standards covering naming, memory, errors, API design, testing, security, and more

### AI-Optimized Rules

10 rule files in `.claude/rules/` for automatic Claude Code integration:

| Rule | Description |
|------|-------------|
| `memory.md` | Allocator patterns, ownership, defer |
| `errors.md` | Error unions, try/catch, errdefer |
| `naming.md` | Zig naming conventions |
| `api-design.md` | Slices, optional types, config structs |
| `security.md` | Input validation, safety modes |
| `testing.md` | Test patterns, testing allocator |
| `concurrency.md` | Threads, atomics, mutexes |
| `logging.md` | std.log patterns |
| `comptime.md` | Compile-time programming |
| `build.md` | build.zig patterns |

### Slash Commands

| Command | Description |
|---------|-------------|
| `/carbide-install` | Install CarbideZig into existing project |
| `/carbide-update` | Update to latest CarbideZig version |
| `/carbide-init` | Create new CarbideZig project |
| `/carbide-review` | Code review against standards |
| `/carbide-check` | Run build, test, format checks |
| `/carbide-safety` | Security-focused review |

### Pattern Documentation

Detailed implementation guides in `docs/patterns/`:

- Memory management patterns
- Error handling patterns
- API design patterns
- Resource lifecycle patterns

### Security Documentation

Security-focused guides in `docs/security/`:

- Zig safety features
- Avoiding undefined behavior
- Memory safety practices

## Project Structure

```
CarbideZig/
├── STANDARDS.md               # Comprehensive coding standards
├── CARBIDE.md                 # Quick reference card
├── commands/                  # Slash command sources
│   ├── carbide-install.md
│   ├── carbide-update.md
│   ├── carbide-init.md
│   ├── carbide-review.md
│   ├── carbide-check.md
│   └── carbide-safety.md
├── rules/                     # Rule file sources
├── docs/
│   ├── patterns/              # Implementation patterns
│   └── security/              # Security guides
├── templates/
│   ├── build.zig              # Build template
│   └── project/               # Project scaffold
└── examples/
    └── hello/                 # Example project
```

## Core Principles

1. **Leverage the Type System** — Let the compiler catch errors at compile time
2. **Explicit Resource Management** — Every allocation has an owner and a cleanup path
3. **Fail Loudly** — Errors should be visible and handled, never silently ignored
4. **Comptime Over Runtime** — Prefer compile-time computation when possible
5. **Minimal Dependencies** — Standard library first, external dependencies sparingly

## Example

```zig
const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Config = struct {
    max_size: usize = 1024,
    timeout_ms: u32 = 5000,
};

pub const Client = struct {
    allocator: Allocator,
    buffer: []u8,

    pub fn init(allocator: Allocator, config: Config) !Client {
        const buffer = try allocator.alloc(u8, config.max_size);
        errdefer allocator.free(buffer);

        return Client{
            .allocator = allocator,
            .buffer = buffer,
        };
    }

    pub fn deinit(self: *Client) void {
        self.allocator.free(self.buffer);
        self.* = undefined;
    }
};
```

## Naming Conventions

| Element | Style | Example |
|---------|-------|---------|
| Types | PascalCase | `HttpClient` |
| Functions | camelCase | `readFile` |
| Variables | camelCase | `bufferSize` |
| Constants | snake_case | `max_size` |
| Files | snake_case | `http_client.zig` |

## Verifying Installation

After adding CarbideZig to your project, verify it's working:

1. Start Claude Code in your project directory: `claude`
2. Ask Claude about Zig memory management — it should reference CarbideZig rules
3. Run `/carbide-check` to validate your build configuration
4. Run `/carbide-review` to review existing code against standards

## Requirements

- Zig 0.15.0 or later
- Claude Code (for AI-assisted features)

## License

MIT License — See [LICENSE](LICENSE) for details.

---

*CarbideZig — Explicit over implicit. Simple over clever. Safe over fast.*
