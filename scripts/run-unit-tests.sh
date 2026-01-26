#!/bin/bash
# GitStat test wrapper for Klar unit tests
# Runs zig build test and writes results to .test-results.json

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RESULTS_FILE="$SCRIPT_DIR/.test-results.json"

# Run tests from project root
cd "$SCRIPT_DIR"

# Run tests with summary to get accurate counts
TEST_OUTPUT=$(zig build test --summary all 2>&1) || TEST_EXIT=$?
TEST_EXIT=${TEST_EXIT:-0}

# Parse test results from Zig summary output
# Format: "X/Y tests passed" or "X passed"
if echo "$TEST_OUTPUT" | grep -qE '[0-9]+/[0-9]+ tests passed'; then
    # Format: "181/181 tests passed"
    PASSED=$(echo "$TEST_OUTPUT" | grep -oE '[0-9]+/[0-9]+ tests passed' | grep -oE '^[0-9]+')
    TOTAL=$(echo "$TEST_OUTPUT" | grep -oE '[0-9]+/[0-9]+ tests passed' | grep -oE '/[0-9]+' | tr -d '/')
    FAILED=$((TOTAL - PASSED))
elif echo "$TEST_OUTPUT" | grep -qE '[0-9]+ passed'; then
    # Format: "X passed"
    PASSED=$(echo "$TEST_OUTPUT" | grep -oE '[0-9]+ passed' | grep -oE '[0-9]+' || echo "0")
    FAILED=$(echo "$TEST_OUTPUT" | grep -oE '[0-9]+ failed' | grep -oE '[0-9]+' || echo "0")
else
    # Fallback if parsing fails
    if [ $TEST_EXIT -eq 0 ]; then
        PASSED=1
        FAILED=0
    else
        PASSED=0
        FAILED=1
    fi
fi

TOTAL=$((PASSED + FAILED))

# Extract failure messages
FAILURES=""
if [ $FAILED -gt 0 ]; then
    FAILURES=$(echo "$TEST_OUTPUT" | grep -A2 "FAIL" | head -20 | \
        sed 's/"/\\"/g' | \
        awk '{printf "%s\"%s\"", (NR>1?",":""), $0}')
fi

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
if [ $FAILED -eq 0 ]; then
    echo "All $PASSED tests passed"
else
    echo "$FAILED/$TOTAL tests failed"
    echo "$TEST_OUTPUT"
    exit 1
fi
