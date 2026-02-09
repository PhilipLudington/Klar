#!/bin/bash
# Run formatter tests
# Each test has a .kl input and .expected output in test/fmt/
# Also verifies idempotence on all test files

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR/.."

KLAR="./zig-out/bin/klar"
TEST_DIR="test/fmt"
PASS=0
FAIL=0
TOTAL=0

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

echo "Running formatter tests..."

# Test 1: Input/expected pairs
for input in "$TEST_DIR"/*.kl; do
    name=$(basename "$input" .kl)
    expected="$TEST_DIR/$name.expected"

    if [ ! -f "$expected" ]; then
        echo -e "${RED}SKIP${NC} $name (no .expected file)"
        continue
    fi

    TOTAL=$((TOTAL + 1))
    actual=$("$KLAR" fmt "$input" 2>&1)

    if [ $? -ne 0 ]; then
        echo -e "${RED}FAIL${NC} $name (formatter error)"
        FAIL=$((FAIL + 1))
        continue
    fi

    # Compare with expected (use printf to avoid echo adding extra newline)
    if diff -q <(printf '%s\n' "$actual") "$expected" > /dev/null 2>&1; then
        echo -e "${GREEN}PASS${NC} $name"
        PASS=$((PASS + 1))
    else
        echo -e "${RED}FAIL${NC} $name (output differs)"
        diff <(printf '%s\n' "$actual") "$expected" | head -10
        FAIL=$((FAIL + 1))
    fi
done

# Test 2: Idempotence on all input files
echo ""
echo "Running idempotence tests..."
for input in "$TEST_DIR"/*.kl; do
    name=$(basename "$input" .kl)
    TOTAL=$((TOTAL + 1))

    first=$("$KLAR" fmt "$input" 2>/dev/null)
    if [ $? -ne 0 ]; then
        echo -e "${RED}FAIL${NC} $name (idempotent: first pass error)"
        FAIL=$((FAIL + 1))
        continue
    fi

    second=$(echo "$first" | "$KLAR" fmt /dev/stdin 2>/dev/null)
    if [ $? -ne 0 ]; then
        # Try via temp file if stdin doesn't work
        echo "$first" > /tmp/klar_fmt_idem.kl
        second=$("$KLAR" fmt /tmp/klar_fmt_idem.kl 2>/dev/null)
        if [ $? -ne 0 ]; then
            echo -e "${RED}FAIL${NC} $name (idempotent: second pass error)"
            FAIL=$((FAIL + 1))
            continue
        fi
    fi

    if [ "$first" = "$second" ]; then
        echo -e "${GREEN}PASS${NC} $name (idempotent)"
        PASS=$((PASS + 1))
    else
        echo -e "${RED}FAIL${NC} $name (idempotent: output differs on second pass)"
        diff <(echo "$first") <(echo "$second") | head -10
        FAIL=$((FAIL + 1))
    fi
done

# Summary
echo ""
echo "Formatter tests: $PASS/$TOTAL passed, $FAIL failed"

# Write results JSON for AirTower
cat > .fmt-test-results.json << ENDJSON
{
    "passed": $PASS,
    "failed": $FAIL,
    "total": $TOTAL
}
ENDJSON

if [ $FAIL -eq 0 ]; then
    echo -e "${GREEN}All formatter tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some formatter tests failed${NC}"
    exit 1
fi
