#!/bin/bash
# GitStat test wrapper for Klar module/multi-file compilation tests
# Tests import resolution, visibility, circular detection, etc.

RESULTS_FILE=".module-test-results.json"
KLAR="./zig-out/bin/klar"

# Ensure compiler is built
if [ ! -f "$KLAR" ]; then
    echo "Building Klar compiler first..."
    zig build || exit 1
fi

PASSED=0
FAILED=0
FAILURES=""

# Helper to record a failure
record_failure() {
    local name="$1"
    local msg="$2"
    FAILED=$((FAILED + 1))
    if [ -n "$FAILURES" ]; then
        FAILURES="$FAILURES,"
    fi
    FAILURES="$FAILURES\"$name: $msg\""
}

# Helper to record success
record_success() {
    local name="$1"
    PASSED=$((PASSED + 1))
}

echo "=== Module Tests ==="
echo ""

# Test 1: Basic multi-file import
echo "--- basic: Multi-file import ---"
temp_bin="/tmp/klar_module_basic"
if $KLAR build test/module/basic/main.kl -o "$temp_bin" 2>/dev/null; then
    result=$("$temp_bin" 2>/dev/null; echo $?)
    # greet() returns 42, add(10, 20) returns 30, total = 72
    if [ "$result" = "72" ]; then
        echo "✓ basic (exit: $result)"
        record_success "basic"
    else
        echo "✗ basic (expected: 72, got: $result)"
        record_failure "basic" "expected 72, got $result"
    fi
    rm -f "$temp_bin"
else
    echo "✗ basic (build failed)"
    record_failure "basic" "build failed"
fi

# Test 2: Nested module paths
echo "--- nested: Nested module paths ---"
temp_bin="/tmp/klar_module_nested"
if $KLAR build test/module/nested/main.kl -o "$temp_bin" 2>/dev/null; then
    result=$("$temp_bin" 2>/dev/null; echo $?)
    # add(5, 3) = 8, multiply(4, 2) = 8, total = 16
    if [ "$result" = "16" ]; then
        echo "✓ nested (exit: $result)"
        record_success "nested"
    else
        echo "✗ nested (expected: 16, got: $result)"
        record_failure "nested" "expected 16, got $result"
    fi
    rm -f "$temp_bin"
else
    echo "✗ nested (build failed)"
    record_failure "nested" "build failed"
fi

# Test 3: Visibility enforcement (should FAIL to compile)
echo "--- visibility: Private symbol import rejected ---"
temp_bin="/tmp/klar_module_visibility"
error_output=$($KLAR build test/module/visibility/main.kl -o "$temp_bin" 2>&1)
# Check if output contains error (compiler may return 0 even with errors)
if echo "$error_output" | grep -q "not found\|not exported\|private\|error"; then
    if echo "$error_output" | grep -q "private_fn.*not found\|symbol.*not found"; then
        echo "✓ visibility (correctly rejected private import)"
        record_success "visibility"
    else
        echo "✗ visibility (failed but wrong error)"
        record_failure "visibility" "wrong error message"
    fi
    rm -f "$temp_bin" 2>/dev/null
else
    echo "✗ visibility (should have failed but succeeded)"
    record_failure "visibility" "should have rejected private import"
    rm -f "$temp_bin"
fi

# Test 4: Circular import detection (should FAIL to compile)
echo "--- circular: Circular import detected ---"
temp_bin="/tmp/klar_module_circular"
if [ -d "test/module/circular" ]; then
    error_output=$($KLAR build test/module/circular/main.kl -o "$temp_bin" 2>&1)
    # Check output for circular detection
    if echo "$error_output" | grep -qi "circular"; then
        echo "✓ circular (correctly detected circular import)"
        record_success "circular"
    else
        echo "✗ circular (should have detected circular import)"
        echo "    output: $error_output"
        record_failure "circular" "should have detected circular import"
    fi
    rm -f "$temp_bin" 2>/dev/null
else
    echo "⊘ circular (test not found, skipping)"
fi

# Test 5: Glob imports
echo "--- glob: Glob import (.*) ---"
temp_bin="/tmp/klar_module_glob"
if [ -d "test/module/glob" ]; then
    if $KLAR build test/module/glob/main.kl -o "$temp_bin" 2>/dev/null; then
        result=$("$temp_bin" 2>/dev/null; echo $?)
        if [ "$result" = "42" ]; then
            echo "✓ glob (exit: $result)"
            record_success "glob"
        else
            echo "✗ glob (expected: 42, got: $result)"
            record_failure "glob" "expected 42, got $result"
        fi
        rm -f "$temp_bin"
    else
        echo "✗ glob (build failed)"
        record_failure "glob" "build failed"
    fi
else
    echo "⊘ glob (test not found, skipping)"
fi

# Test 6: Aliased imports
echo "--- alias: Aliased import (as) ---"
temp_bin="/tmp/klar_module_alias"
if [ -d "test/module/alias" ]; then
    if $KLAR build test/module/alias/main.kl -o "$temp_bin" 2>/dev/null; then
        "$temp_bin" >/dev/null 2>&1
        result=$?
        if [ "$result" = "42" ]; then
            echo "✓ alias (exit: $result)"
            record_success "alias"
        else
            echo "✗ alias (expected: 42, got: $result)"
            record_failure "alias" "expected 42, got $result"
        fi
        rm -f "$temp_bin"
    else
        echo "✗ alias (build failed)"
        record_failure "alias" "build failed"
    fi
else
    echo "⊘ alias (test not found, skipping)"
fi

TOTAL=$((PASSED + FAILED))

# Write results JSON
cat > "$RESULTS_FILE" << EOF
{
  "passed": $PASSED,
  "failed": $FAILED,
  "total": $TOTAL,
  "failures": [$FAILURES]
}
EOF

# Print summary
echo ""
if [ $FAILED -eq 0 ]; then
    echo "All $PASSED module tests passed"
else
    echo "$FAILED/$TOTAL module tests failed"
    exit 1
fi
