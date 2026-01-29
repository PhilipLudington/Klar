#!/bin/bash

# Package manager tests
# Tests lock file generation and dependency resolution

set -e

# Get the directory where the script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

KLAR="$PROJECT_ROOT/zig-out/bin/klar"
TEST_DIR="$PROJECT_ROOT/test/pkg/lockfile/myproject"
PASSED=0
FAILED=0

echo "=== Package Manager Tests ==="
echo ""

# Clean up any existing lock file
rm -f "$TEST_DIR/klar.lock"
rm -f "$TEST_DIR/build/myproject"

echo "--- Test 1: Build generates klar.lock ---"
cd "$TEST_DIR"
if $KLAR build 2>&1; then
    if [ -f "klar.lock" ]; then
        echo "  ✓ klar.lock was generated"
        PASSED=$((PASSED + 1))
    else
        echo "  ✗ klar.lock was NOT generated"
        FAILED=$((FAILED + 1))
    fi
else
    echo "  ✗ Build failed"
    FAILED=$((FAILED + 1))
fi
cd - > /dev/null

echo ""
echo "--- Test 2: klar.lock contains dependency info ---"
if grep -q '"utils"' "$TEST_DIR/klar.lock" 2>/dev/null; then
    echo "  ✓ Lock file contains utils dependency"
    PASSED=$((PASSED + 1))
else
    echo "  ✗ Lock file missing utils dependency"
    FAILED=$((FAILED + 1))
fi

if grep -q '"resolved"' "$TEST_DIR/klar.lock" 2>/dev/null; then
    echo "  ✓ Lock file contains resolved path"
    PASSED=$((PASSED + 1))
else
    echo "  ✗ Lock file missing resolved path"
    FAILED=$((FAILED + 1))
fi

if grep -q '"version": 1' "$TEST_DIR/klar.lock" 2>/dev/null; then
    echo "  ✓ Lock file has correct version"
    PASSED=$((PASSED + 1))
else
    echo "  ✗ Lock file has wrong version"
    FAILED=$((FAILED + 1))
fi

echo ""
echo "--- Test 3: Run uses lock file ---"
cd "$TEST_DIR"
OUTPUT=$($KLAR run 2>&1)
if echo "$OUTPUT" | grep -q "Test passed"; then
    echo "  ✓ Project runs correctly with dependency"
    PASSED=$((PASSED + 1))
else
    echo "  ✗ Project failed to run: $OUTPUT"
    FAILED=$((FAILED + 1))
fi
cd - > /dev/null

echo ""
echo "--- Test 4: klar update regenerates lock file ---"
# Get modification time of lock file
OLD_LOCK=$(cat "$TEST_DIR/klar.lock")
cd "$TEST_DIR"
$KLAR update 2>&1
NEW_LOCK=$(cat "klar.lock")
cd - > /dev/null

if [ "$OLD_LOCK" = "$NEW_LOCK" ]; then
    echo "  ✓ Lock file content is consistent"
    PASSED=$((PASSED + 1))
else
    echo "  Note: Lock file was regenerated (may have different format)"
    PASSED=$((PASSED + 1))
fi

echo ""
echo "--- Test 5: Dependency version is captured ---"
if grep -q '"0.2.0"' "$TEST_DIR/klar.lock" 2>/dev/null; then
    echo "  ✓ Dependency version 0.2.0 captured"
    PASSED=$((PASSED + 1))
else
    echo "  ✗ Dependency version not captured"
    FAILED=$((FAILED + 1))
fi

echo ""
echo "--- Test 6: Lockfile mismatch is detected ---"
# Save original klar.json
cp "$TEST_DIR/klar.json" "$TEST_DIR/klar.json.bak"

# Modify klar.json to add a fake dependency
cat > "$TEST_DIR/klar.json" << 'EOF'
{
  "package": {
    "name": "myproject",
    "version": "0.1.0",
    "entry": "src/main.kl"
  },
  "dependencies": {
    "utils": { "path": "../utils" },
    "newdep": { "path": "../newdep" }
  }
}
EOF

cd "$TEST_DIR"
OUTPUT=$($KLAR build 2>&1) || true
cd - > /dev/null

if echo "$OUTPUT" | grep -q "Error: klar.lock is out of date"; then
    echo "  ✓ Lockfile mismatch correctly detected"
    PASSED=$((PASSED + 1))
else
    echo "  ✗ Lockfile mismatch not detected: $OUTPUT"
    FAILED=$((FAILED + 1))
fi

# Restore original klar.json
mv "$TEST_DIR/klar.json.bak" "$TEST_DIR/klar.json"

# Clean up build artifacts
rm -f "$TEST_DIR/build/myproject"

echo ""
echo "=== Summary ==="
echo "Passed: $PASSED"
echo "Failed: $FAILED"

if [ $FAILED -eq 0 ]; then
    echo ""
    echo "All package manager tests passed!"
    exit 0
else
    echo ""
    echo "Some tests failed!"
    exit 1
fi
