#!/bin/bash
# GitStat test wrapper for Klar native compilation tests
# Compiles and runs test/native/*.kl files

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RESULTS_FILE="$SCRIPT_DIR/.native-test-results.json"
KLAR="$SCRIPT_DIR/zig-out/bin/klar"
TEST_DIR="$SCRIPT_DIR/test/native"

# Ensure compiler is built
if [ ! -f "$KLAR" ]; then
    echo "Building Klar compiler first..."
    cd "$SCRIPT_DIR" && zig build || exit 1
fi

PASSED=0
FAILED=0
FAILURES=""

# Get expected result for a test
get_expected() {
    case "$1" in
        arith) echo 50 ;;
        call) echo 42 ;;
        early_return) echo 162 ;;
        hello) echo 42 ;;
        local_vars) echo 19 ;;
        many_params) echo 36 ;;
        nested_calls) echo 15 ;;
        recursive_deep) echo 42 ;;
        return_types) echo 42 ;;
        tuple) echo 42 ;;  # 10 + 32 = 42
        array) echo 42 ;;  # 10 + 20 + 12 = 42
        optional_some) echo 42 ;;  # Force unwrap Some(42)
        optional_unwrap) echo 42 ;;  # Force unwrap Some(42)
        optional_coalesce) echo 99 ;;  # None ?? 99 = 99
        optional_coalesce_some) echo 42 ;;  # Some(42) ?? 99 = 42
        optional_propagate) echo 52 ;;  # ? on Optional: 42 + 10 = 52
        result_propagate) echo 52 ;;  # ? on Result: 42 + 10 = 52
        result_propagate_simple) echo 42 ;;  # Simpler ? on Result test
        *) echo -1 ;;  # -1 means accept any result
    esac
}

# Run each test (including subdirectories)
for f in $(find "$TEST_DIR" -name "*.kl" | sort); do
    [ -f "$f" ] || continue

    name=$(basename "$f" .kl)
    temp_bin="/tmp/klar_test_$name"

    # Compile
    if $KLAR build "$f" -o "$temp_bin" 2>/dev/null | grep -q "^Built"; then
        # Run and get exit code
        "$temp_bin" 2>/dev/null
        result=$?

        # Check against expected (if defined)
        expected=$(get_expected "$name")

        if [ "$expected" = "-1" ] || [ $result -eq $expected ]; then
            echo "✓ $name (exit: $result)"
            PASSED=$((PASSED + 1))
        else
            echo "✗ $name (expected: $expected, got: $result)"
            FAILED=$((FAILED + 1))
            if [ -n "$FAILURES" ]; then
                FAILURES="$FAILURES,"
            fi
            FAILURES="$FAILURES\"$name: expected $expected, got $result\""
        fi

        rm -f "$temp_bin"
    else
        echo "✗ $name (build failed)"
        FAILED=$((FAILED + 1))
        if [ -n "$FAILURES" ]; then
            FAILURES="$FAILURES,"
        fi
        FAILURES="$FAILURES\"$name: build failed\""
    fi
done

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
    echo "All $PASSED native tests passed"
else
    echo "$FAILED/$TOTAL native tests failed"
    exit 1
fi
