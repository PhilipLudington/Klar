#!/bin/bash
# Test command-line arguments across all three backends (native, VM, interpreter)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RESULTS_FILE="$SCRIPT_DIR/.args-test-results.json"
KLAR="$SCRIPT_DIR/zig-out/bin/klar"
TEST_DIR="$SCRIPT_DIR/test/args"

# Ensure compiler is built
if [ ! -f "$KLAR" ]; then
    echo "Building Klar compiler first..."
    cd "$SCRIPT_DIR" && ./build.sh || exit 1
fi

PASSED=0
FAILED=0
FAILURES=""

pass() {
    echo "✓ $1"
    PASSED=$((PASSED + 1))
}

fail() {
    echo "✗ $1"
    FAILED=$((FAILED + 1))
    if [ -n "$FAILURES" ]; then
        FAILURES="$FAILURES,"
    fi
    FAILURES="$FAILURES\"$1\""
}

echo "=== Command-Line Arguments Tests ==="
echo ""

# Test 1: args_count - check that arg count is correct across backends
echo "--- Test: args_count (3 backends × 2 scenarios) ---"

# Native: no args (just path)
output=$($KLAR run "$TEST_DIR/args_count.kl" 2>&1)
if echo "$output" | grep -q "^1$"; then
    pass "native: no user args -> prints 1 (path only)"
else
    fail "native: no user args -> expected '1' in output"
fi

# Native: with args (path + 2 user args)
output=$($KLAR run "$TEST_DIR/args_count.kl" foo bar 2>&1)
if echo "$output" | grep -q "^3$"; then
    pass "native: 2 user args -> prints 3"
else
    fail "native: 2 user args -> expected '3' in output"
fi

# VM: no args
output=$($KLAR run "$TEST_DIR/args_count.kl" --vm 2>&1)
if echo "$output" | grep -q "^1$"; then
    pass "vm: no user args -> prints 1 (path only)"
else
    fail "vm: no user args -> expected '1' in output"
fi

# VM: with args
output=$($KLAR run "$TEST_DIR/args_count.kl" --vm foo bar 2>&1)
if echo "$output" | grep -q "^3$"; then
    pass "vm: 2 user args -> prints 3"
else
    fail "vm: 2 user args -> expected '3' in output"
fi

# Interpreter: no args
output=$($KLAR run "$TEST_DIR/args_count.kl" --interpret 2>&1)
if echo "$output" | grep -q "^1$"; then
    pass "interpreter: no user args -> prints 1 (path only)"
else
    fail "interpreter: no user args -> expected '1' in output"
fi

# Interpreter: with args
output=$($KLAR run "$TEST_DIR/args_count.kl" --interpret foo bar 2>&1)
if echo "$output" | grep -q "^3$"; then
    pass "interpreter: 2 user args -> prints 3"
else
    fail "interpreter: 2 user args -> expected '3' in output"
fi

echo ""
echo "--- Test: args_access (verify arg values via VM/interpreter) ---"

# VM: check output contains the args
output=$($KLAR run "$TEST_DIR/args_access.kl" --vm hello world 2>&1)
if echo "$output" | grep -q "hello" && echo "$output" | grep -q "world"; then
    pass "vm: args accessible and correct"
else
    fail "vm: args not accessible"
fi

# Interpreter: check output
output=$($KLAR run "$TEST_DIR/args_access.kl" --interpret hello world 2>&1)
if echo "$output" | grep -q "hello" && echo "$output" | grep -q "world"; then
    pass "interpreter: args accessible and correct"
else
    fail "interpreter: args not accessible"
fi

echo ""
echo "--- Test: no_args (backwards compatibility) ---"

# VM: main() without args still works
output=$($KLAR run "$TEST_DIR/no_args.kl" --vm 2>&1)
if [ $? -eq 0 ] && ! echo "$output" | grep -qi "error"; then
    pass "vm: main() without args works"
else
    fail "vm: main() without args failed"
fi

# Interpreter: main() without args still works
output=$($KLAR run "$TEST_DIR/no_args.kl" --interpret 2>&1)
if [ $? -eq 0 ] && ! echo "$output" | grep -qi "error"; then
    pass "interpreter: main() without args works"
else
    fail "interpreter: main() without args failed"
fi

# Native: build and run to check exit code
temp_bin="/tmp/klar_args_test_no_args"
if $KLAR build "$TEST_DIR/no_args.kl" -o "$temp_bin" 2>/dev/null; then
    "$temp_bin" 2>/dev/null
    if [ $? -eq 42 ]; then
        pass "native: main() without args returns 42"
    else
        fail "native: main() without args -> expected exit 42"
    fi
    rm -f "$temp_bin"
else
    fail "native: main() without args -> build failed"
fi

echo ""
echo "--- Test: -- separator passes flags as args ---"

output=$($KLAR run "$TEST_DIR/args_access.kl" --vm -- --help -v 2>&1)
if echo "$output" | grep -q "\-\-help" && echo "$output" | grep -q "\-v"; then
    pass "vm: -- separator works"
else
    fail "vm: -- separator failed"
fi

output=$($KLAR run "$TEST_DIR/args_access.kl" --interpret -- --help -v 2>&1)
if echo "$output" | grep -q "\-\-help" && echo "$output" | grep -q "\-v"; then
    pass "interpreter: -- separator works"
else
    fail "interpreter: -- separator failed"
fi

# Write results JSON
TOTAL=$((PASSED + FAILED))
cat > "$RESULTS_FILE" << EOF
{
  "passed": $PASSED,
  "failed": $FAILED,
  "total": $TOTAL,
  "failures": [$FAILURES]
}
EOF

echo ""
echo "All $PASSED args tests passed"

if [ $FAILED -gt 0 ]; then
    exit 1
fi
