#!/bin/bash
# scripts/run-kira-interop-test.sh — Integration tests for Kira interop module resolution
# Tests: import-kira generates to deps/, import resolves, extern functions type-check

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
KLAR="$SCRIPT_DIR/zig-out/bin/klar"
TEST_DIR="/tmp/klar_kira_interop_$$"
PASSED=0
FAILED=0
TOTAL=5

cleanup() {
    rm -rf "$TEST_DIR"
}
trap cleanup EXIT

# Create test directory
rm -rf "$TEST_DIR"
mkdir -p "$TEST_DIR"

pass() { echo "  PASS: $1"; PASSED=$((PASSED + 1)); }
fail() { echo "  FAIL: $1"; FAILED=$((FAILED + 1)); }

echo "--- Kira Interop Module Resolution Tests ---"

# Create a test Kira manifest
cat > "$TEST_DIR/mathlib.json" << 'MANIFEST'
{
  "module": "mathlib",
  "functions": [
    {
      "name": "kira_add",
      "params": [{"name": "a", "type": "i32"}, {"name": "b", "type": "i32"}],
      "return_type": "i32"
    },
    {
      "name": "kira_multiply",
      "params": [{"name": "x", "type": "f64"}, {"name": "y", "type": "f64"}],
      "return_type": "f64"
    },
    {
      "name": "kira_noop",
      "params": [],
      "return_type": "void"
    }
  ],
  "types": [
    {
      "name": "Point",
      "kind": "product",
      "fields": [{"name": "x", "type": "f64"}, {"name": "y", "type": "f64"}]
    }
  ]
}
MANIFEST

# Test 1: import-kira generates file in deps/
cd "$TEST_DIR"
OUTPUT=$("$KLAR" import-kira mathlib.json 2>&1)
if [ -f "$TEST_DIR/deps/kira_mathlib.kl" ]; then
    pass "import-kira generates deps/kira_mathlib.kl"
else
    fail "import-kira should generate deps/kira_mathlib.kl: $OUTPUT"
fi

# Test 2: Generated file contains expected extern declarations
if grep -q "fn kira_add" "$TEST_DIR/deps/kira_mathlib.kl" && \
   grep -q "fn kira_multiply" "$TEST_DIR/deps/kira_mathlib.kl" && \
   grep -q "extern struct Point" "$TEST_DIR/deps/kira_mathlib.kl"; then
    pass "generated file contains expected declarations"
else
    fail "generated file missing expected declarations"
fi

# Test 3: import resolves and type-checks (import with glob)
cat > "$TEST_DIR/test_import.kl" << 'PROGRAM'
import kira_mathlib.*

fn main() -> i32 {
    let result: i32 = unsafe { kira_add(1, 2) }
    return result
}
PROGRAM

OUTPUT=$("$KLAR" check "$TEST_DIR/test_import.kl" 2>&1)
EXIT_CODE=$?
if [ $EXIT_CODE -eq 0 ] && echo "$OUTPUT" | grep -q "checks passed"; then
    pass "import kira_mathlib.* resolves and type-checks"
else
    fail "import should resolve: exit=$EXIT_CODE output=$OUTPUT"
fi

# Test 4: extern functions have correct types (type mismatch detected)
cat > "$TEST_DIR/test_type_error.kl" << 'PROGRAM'
import kira_mathlib.*

fn main() -> i32 {
    let result: f64 = unsafe { kira_add(1, 2) }
    return 0
}
PROGRAM

OUTPUT=$("$KLAR" check "$TEST_DIR/test_type_error.kl" 2>&1)
EXIT_CODE=$?
if [ $EXIT_CODE -ne 0 ] || echo "$OUTPUT" | grep -qi "mismatch\|error"; then
    pass "type mismatch in extern function detected"
else
    fail "should detect type mismatch: exit=$EXIT_CODE output=$OUTPUT"
fi

# Test 5: import-kira with -o flag generates at custom path
"$KLAR" import-kira "$TEST_DIR/mathlib.json" -o "$TEST_DIR/custom_output.kl" 2>&1
if [ -f "$TEST_DIR/custom_output.kl" ]; then
    pass "import-kira -o generates at custom path"
else
    fail "import-kira -o should generate at custom path"
fi

echo ""
echo "Kira interop tests: $PASSED passed, $FAILED failed (of $TOTAL)"
if [ "$FAILED" -gt 0 ]; then
    exit 1
fi
