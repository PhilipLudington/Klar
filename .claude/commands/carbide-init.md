# CarbideZig Project Initialization

Create a new CarbideZig project with standard structure and configuration.

## Arguments

- `$ARGUMENTS` - Project name (required)

## Instructions

1. Create the following directory structure:

```
$ARGUMENTS/
├── build.zig           # Build configuration
├── build.zig.zon       # Package manifest
├── src/
│   ├── main.zig        # Application entry point
│   ├── root.zig        # Library root (public API)
│   └── lib/            # Internal modules
└── tests/              # Integration tests
```

2. Create `build.zig` with standard configuration:
   - Library and executable targets
   - Test step
   - Run step with argument passthrough
   - Documentation generation step

3. Create `build.zig.zon` with:
   - Project name matching `$ARGUMENTS`
   - Version 0.1.0
   - Minimum Zig version 0.13.0

4. Create `src/main.zig` with:
   - Basic main function
   - GeneralPurposeAllocator setup with leak detection
   - Example command-line argument handling

5. Create `src/root.zig` with:
   - Module-level documentation template
   - Example public function
   - Test block

6. Copy CARBIDE.md and STANDARDS.md from the CarbideZig project root to the new project

7. Provide next steps:
   - Build: `cd $ARGUMENTS && zig build`
   - Run: `zig build run`
   - Test: `zig build test`
   - Format: `zig fmt src/`

## Example Usage

```
/carbide-init my-project
```

Creates `my-project/` with full CarbideZig structure.
