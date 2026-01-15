# Klar Project

CarbideZig is used in this project for Zig development standards. See `carbide/CARBIDE.md` and `carbide/STANDARDS.md`.

## Building

Always use the GitStat wrapper script to build:
```bash
./build.sh
```
Do NOT run `zig build` directly - use the wrapper script to preserve GitStat integration and result tracking.

## Running Tests

Always use the GitStat wrapper scripts to run tests:

**Unit tests (Zig):**
```bash
./run-tests.sh
```

**Native compilation tests:**
```bash
./run-native-tests.sh
```

Do NOT run `zig build test` directly - use the wrapper scripts to preserve GitStat integration and result tracking.

## GitStat Integration

This project uses GitStat for build/test status tracking. The wrapper scripts write results to:
- `.build-results.json` - Build status
- `.test-results.json` - Unit test results
- `.native-test-results.json` - Native compilation test results

These JSON files are read by GitStat to display status badges.
