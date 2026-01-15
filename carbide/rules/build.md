---
globs: ["build.zig", "build.zig.zon"]
---

# Build System Rules

## B1: Standard Structure
- Use `standardTargetOptions` and `standardOptimizeOption`
- Define clear build steps: build, run, test, docs

```zig
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    // ...
}
```

## B2: Artifact Installation
- Use `b.installArtifact()` for libraries and executables
- Use named steps for discoverability

```zig
const lib = b.addStaticLibrary(.{ ... });
b.installArtifact(lib);

const run_step = b.step("run", "Run the application");
run_step.dependOn(&run_cmd.step);
```

## B3: Dependencies (build.zig.zon)
- Declare dependencies in build.zig.zon
- Use specific versions or commits

```zig
.dependencies = .{
    .zap = .{
        .url = "https://github.com/zigzap/zap/archive/v0.0.1.tar.gz",
        .hash = "...",
    },
},
```

## B4: Build Options
- Expose configurable options with `b.option`
- Use `addOptions` to pass to modules

```zig
const enable_logging = b.option(bool, "log", "Enable logging") orelse false;

const options = b.addOptions();
options.addOption(bool, "enable_logging", enable_logging);
lib.root_module.addOptions("config", options);
```

## B5: Test Configuration (Zig 0.15+)
- Use `root_module` instead of deprecated `root_source_file`
- Configure test step with same target/optimize

```zig
// Zig 0.15+: Use root_module pattern
const lib_mod = b.addModule("mylib", .{
    .root_source_file = b.path("src/root.zig"),
    .target = target,
    .optimize = optimize,
});

const tests = b.addTest(.{
    .root_module = lib_mod,
});
const test_step = b.step("test", "Run unit tests");
test_step.dependOn(&b.addRunArtifact(tests).step);
```

## B6: Cross-Compilation
- Target options enable cross-compilation out of box
- Test on multiple targets in CI

```bash
zig build -Dtarget=x86_64-linux-gnu
zig build -Dtarget=aarch64-macos
```

## B7: New Build Features (Zig 0.15+)
- `zig init --minimal` generates stub templates
- `zig test-obj` compiles tests to object files
- `--watch` for file system watching (macOS)
- `--webui` exposes build interface with timing reports

---

# Build.zig Cookbook

## Recipe: Multi-Target Build

Build for multiple targets in one command:

```zig
pub fn build(b: *std.Build) void {
    const targets = [_]std.Target.Query{
        .{ .cpu_arch = .x86_64, .os_tag = .linux },
        .{ .cpu_arch = .aarch64, .os_tag = .macos },
        .{ .cpu_arch = .x86_64, .os_tag = .windows },
    };

    for (targets) |t| {
        const target = b.resolveTargetQuery(t);
        const exe = b.addExecutable(.{
            .name = "myapp",
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = .ReleaseSafe,
        });

        const target_output = b.addInstallArtifact(exe, .{
            .dest_dir = .{ .custom = @tagName(t.cpu_arch.?) ++ "-" ++ @tagName(t.os_tag.?) },
        });
        b.getInstallStep().dependOn(&target_output.step);
    }
}
```

## Recipe: Embed Files at Compile Time

```zig
// In build.zig
const exe = b.addExecutable(.{ ... });

// Embed entire directory
exe.root_module.addAnonymousImport("assets", .{
    .root_source_file = b.path("assets/manifest.zig"),
});

// In source code
const assets = @import("assets");

// Or use @embedFile directly
const config_template = @embedFile("templates/config.json");
const logo_png = @embedFile("assets/logo.png");
```

## Recipe: Conditional Compilation

```zig
pub fn build(b: *std.Build) void {
    const enable_logging = b.option(bool, "log", "Enable debug logging") orelse false;
    const use_sse = b.option(bool, "sse", "Enable SSE optimizations") orelse true;

    const exe = b.addExecutable(.{ ... });

    // Pass options to code
    const options = b.addOptions();
    options.addOption(bool, "enable_logging", enable_logging);
    options.addOption(bool, "use_sse", use_sse);
    exe.root_module.addOptions("config", options);
}

// In source code:
const config = @import("config");
if (config.enable_logging) {
    std.log.debug("...", .{});
}
```

## Recipe: Custom Build Step

```zig
pub fn build(b: *std.Build) void {
    const generate_step = b.addSystemCommand(&.{
        "python3", "scripts/generate_code.py",
    });

    const exe = b.addExecutable(.{ ... });
    exe.step.dependOn(&generate_step.step);

    // Or add generated file
    exe.root_module.addAnonymousImport("generated", .{
        .root_source_file = generate_step.addOutputFileArg("generated.zig"),
    });
}
```

## Recipe: C Library Integration

```zig
pub fn build(b: *std.Build) void {
    const exe = b.addExecutable(.{ ... });

    // Add C source files
    exe.addCSourceFiles(.{
        .files = &.{ "src/helper.c", "src/util.c" },
        .flags = &.{ "-std=c11", "-Wall", "-O2" },
    });

    // Include paths
    exe.addIncludePath(b.path("include"));
    exe.addSystemIncludePath(.{ .path = "/usr/local/include" });

    // Link libraries
    exe.linkLibC();
    exe.linkSystemLibrary("ssl");
    exe.linkSystemLibrary("crypto");

    // Static library
    exe.addObjectFile(b.path("vendor/libfoo.a"));
}
```

## Recipe: Library + Executable + Tests

```zig
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Library module
    const lib_mod = b.addModule("mylib", .{
        .root_source_file = b.path("src/lib.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Static library artifact
    const lib = b.addStaticLibrary(.{
        .name = "mylib",
        .root_module = lib_mod,
    });
    b.installArtifact(lib);

    // Executable using the library
    const exe = b.addExecutable(.{
        .name = "myapp",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addImport("mylib", lib_mod);
    b.installArtifact(exe);

    // Tests for library
    const lib_tests = b.addTest(.{
        .root_module = lib_mod,
    });
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&b.addRunArtifact(lib_tests).step);

    // Run step
    const run_cmd = b.addRunArtifact(exe);
    const run_step = b.step("run", "Run the application");
    run_step.dependOn(&run_cmd.step);
}
```

## Recipe: Documentation Generation

```zig
pub fn build(b: *std.Build) void {
    const lib = b.addStaticLibrary(.{ ... });

    const docs = b.addInstallDirectory(.{
        .source_dir = lib.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "docs",
    });

    const docs_step = b.step("docs", "Generate documentation");
    docs_step.dependOn(&docs.step);
}
```

## Recipe: Release Build Profile

```zig
pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "myapp",
        .root_source_file = b.path("src/main.zig"),
        .optimize = optimize,
    });

    // Strip debug symbols in release
    if (optimize != .Debug) {
        exe.root_module.strip = true;
    }

    // Link-time optimization for release
    if (optimize == .ReleaseFast or optimize == .ReleaseSmall) {
        exe.want_lto = true;
    }
}
```

## Recipe: Version Information

```zig
pub fn build(b: *std.Build) void {
    const exe = b.addExecutable(.{ ... });

    // Get git hash
    const git_hash = b.run(&.{ "git", "rev-parse", "--short", "HEAD" });

    const options = b.addOptions();
    options.addOption([]const u8, "git_hash", git_hash);
    options.addOption([]const u8, "version", "1.0.0");
    exe.root_module.addOptions("build_info", options);
}

// In source:
const build_info = @import("build_info");
pub fn getVersion() []const u8 {
    return build_info.version ++ "-" ++ build_info.git_hash;
}
```
