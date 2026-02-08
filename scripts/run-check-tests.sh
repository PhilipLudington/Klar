#!/bin/bash
# Run type-checking and ownership tests using `klar check`
#
# Test conventions (in first 5 lines):
#   - "// Expected: check-pass" = klar check should succeed with no errors
#   - "// Expected: check-error <kind>" = klar check should report error of given kind

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RESULTS_FILE="$SCRIPT_DIR/.check-test-results.json"
KLAR="$SCRIPT_DIR/zig-out/bin/klar"
TEST_DIR="$SCRIPT_DIR/test/check"

# Ensure compiler is built
if [ ! -f "$KLAR" ]; then
    echo "Building Klar compiler first..."
    cd "$SCRIPT_DIR" && zig build || exit 1
fi

PASSED=0
FAILED=0
FAILURES=""

for f in $(find "$TEST_DIR" -name "*.kl" | sort); do
    [ -f "$f" ] || continue
    name=$(basename "$f" .kl)

    # Read expected outcome from first 5 lines
    expected_line=$(head -5 "$f" | grep "// Expected:")
    if [ -z "$expected_line" ]; then
        echo "? $name (no Expected: directive, skipping)"
        continue
    fi

    # Run klar check
    check_output=$($KLAR check "$f" 2>&1)
    check_exit=$?

    if echo "$expected_line" | grep -q "check-pass"; then
        # Should pass with no errors
        if echo "$check_output" | grep -q "All checks passed"; then
            echo "✓ $name (check passed)"
            PASSED=$((PASSED + 1))
        else
            echo "✗ $name (expected check-pass, got errors)"
            echo "  $check_output" | head -3
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: expected check-pass\""
        fi
    elif echo "$expected_line" | grep -q "check-error"; then
        # Should fail with specific error kind
        error_kind=$(echo "$expected_line" | sed 's/.*check-error //')
        if echo "$check_output" | grep -q "$error_kind"; then
            echo "✓ $name (correctly caught: $error_kind)"
            PASSED=$((PASSED + 1))
        elif echo "$check_output" | grep -q "All checks passed"; then
            echo "✗ $name (expected $error_kind, but check passed)"
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: expected $error_kind, but passed\""
        else
            echo "✗ $name (expected $error_kind, got different error)"
            echo "  $check_output" | head -3
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: wrong error kind\""
        fi
    fi
done

TOTAL=$((PASSED + FAILED))
echo ""
echo "Check tests: $PASSED/$TOTAL passed"

# Write results JSON
cat > "$RESULTS_FILE" <<ENDJSON
{
  "test_suite": "check",
  "total": $TOTAL,
  "passed": $PASSED,
  "failed": $FAILED,
  "failures": [$FAILURES]
}
ENDJSON

if [ $FAILED -gt 0 ]; then
    exit 1
fi
