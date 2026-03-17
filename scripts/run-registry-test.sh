#!/bin/bash
# scripts/run-registry-test.sh — Integration test for the package registry
# Tests: publish, metadata fetch, download, klar add, duplicate add, version conflict, 404

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
KLAR="$SCRIPT_DIR/zig-out/bin/klar"
REGISTRY_BIN="$SCRIPT_DIR/build/registry"
PORT=19891
TEST_DIR="/tmp/klar_registry_integration_$$"
PASSED=0
FAILED=0
TOTAL=8

cleanup() {
    if [ -n "$REGISTRY_PID" ]; then
        kill "$REGISTRY_PID" 2>/dev/null
        wait "$REGISTRY_PID" 2>/dev/null
    fi
    rm -rf "$TEST_DIR"
}
trap cleanup EXIT

# Build registry if needed
if [ ! -f "$REGISTRY_BIN" ]; then
    echo "Building registry server..."
    "$KLAR" build "$SCRIPT_DIR/tools/registry/main.kl" -o "$REGISTRY_BIN" 2>/dev/null
fi

# Create test directory
rm -rf "$TEST_DIR"
mkdir -p "$TEST_DIR/registry_data"

# Start registry server (cwd = registry_data so storage/ goes there)
cd "$TEST_DIR/registry_data"
"$REGISTRY_BIN" "$PORT" &
REGISTRY_PID=$!
sleep 1

export KLAR_REGISTRY="http://127.0.0.1:$PORT"

pass() { echo "  PASS: $1"; PASSED=$((PASSED + 1)); }
fail() { echo "  FAIL: $1"; FAILED=$((FAILED + 1)); }

echo "--- Registry Integration Tests ---"

# Create a test package
mkdir -p "$TEST_DIR/test-lib/src"
cat > "$TEST_DIR/test-lib/klar.json" << 'EOF'
{
  "package": {
    "name": "test-lib",
    "version": "1.0.0",
    "entry": "src/lib.kl"
  },
  "dependencies": {}
}
EOF
cat > "$TEST_DIR/test-lib/src/lib.kl" << 'EOF'
pub fn greet() -> string {
    return "hello from test-lib"
}
EOF

# Test 1: Publish a package
cd "$TEST_DIR/test-lib"
OUTPUT=$("$KLAR" publish 2>&1)
if echo "$OUTPUT" | grep -q "Published test-lib v1.0.0"; then
    pass "publish package"
else
    fail "publish package: $OUTPUT"
fi

# Test 2: Fetch metadata
METADATA=$(curl -s "http://127.0.0.1:$PORT/packages/test-lib")
if echo "$METADATA" | grep -q '"test-lib"' && echo "$METADATA" | grep -q '"1.0.0"'; then
    pass "fetch metadata"
else
    fail "fetch metadata: $METADATA"
fi

# Test 3: Download archive
ARCHIVE=$(curl -s "http://127.0.0.1:$PORT/packages/test-lib/1.0.0")
if echo "$ARCHIVE" | grep -q '"src/lib.kl"' && echo "$ARCHIVE" | grep -q '"klar.json"'; then
    pass "download archive"
else
    fail "download archive: $ARCHIVE"
fi

# Test 4: 404 for unknown package
STATUS=$(curl -s -o /dev/null -w "%{http_code}" "http://127.0.0.1:$PORT/packages/nonexistent")
if [ "$STATUS" = "404" ]; then
    pass "404 for unknown package"
else
    fail "404 for unknown package (got $STATUS)"
fi

# Test 5: Version conflict (duplicate publish)
cd "$TEST_DIR/test-lib"
OUTPUT=$("$KLAR" publish 2>&1)
if echo "$OUTPUT" | grep -q "publish failed"; then
    pass "version conflict detection"
else
    fail "version conflict detection: $OUTPUT"
fi

# Test 6: klar add downloads package
mkdir -p "$TEST_DIR/consumer"
cd "$TEST_DIR/consumer"
"$KLAR" init consumer-app >/dev/null 2>&1
OUTPUT=$("$KLAR" add test-lib 2>&1)
if echo "$OUTPUT" | grep -q "Added test-lib v1.0.0"; then
    pass "klar add downloads package"
else
    fail "klar add downloads package: $OUTPUT"
fi

# Test 7: deps/ directory contains files
if [ -f "$TEST_DIR/consumer/deps/test-lib/src/lib.kl" ] && [ -f "$TEST_DIR/consumer/deps/test-lib/klar.json" ]; then
    pass "deps/ contains extracted files"
else
    fail "deps/ missing files: $(find "$TEST_DIR/consumer/deps" -type f 2>/dev/null || echo 'no deps dir')"
fi

# Test 8: Duplicate add is idempotent
OUTPUT=$("$KLAR" add test-lib 2>&1)
if echo "$OUTPUT" | grep -q "already in dependencies"; then
    pass "duplicate add is idempotent"
else
    fail "duplicate add: $OUTPUT"
fi

echo ""
echo "Registry tests: $PASSED passed, $FAILED failed (of $TOTAL)"
if [ "$FAILED" -gt 0 ]; then
    exit 1
fi
