#!/bin/bash
# Windows test runner - runs test suites that work without LLVM
# Used by CI for Windows matrix jobs where LLVM dev headers are unavailable

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$SCRIPT_DIR"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

TOTAL_FAILED=0
FAILED_SUITES=""

run_suite() {
    local name="$1"
    local script="$2"

    echo ""
    echo "═══════════════════════════════════════════════════════════"
    echo -e "${YELLOW}Running: $name${NC}"
    echo "═══════════════════════════════════════════════════════════"

    if "$script"; then
        echo -e "${GREEN}✓ $name passed${NC}"
        return 0
    else
        echo -e "${RED}✗ $name failed${NC}"
        FAILED_SUITES="$FAILED_SUITES $name"
        return 1
    fi
}

# Build first
echo "═══════════════════════════════════════════════════════════"
echo -e "${YELLOW}Building compiler (VM-only mode)...${NC}"
echo "═══════════════════════════════════════════════════════════"
if ! ./run-build.sh; then
    echo -e "${RED}Build failed - aborting tests${NC}"
    exit 1
fi

# Run test suites that work without LLVM
run_suite "Unit Tests" "./scripts/run-unit-tests.sh" || TOTAL_FAILED=$((TOTAL_FAILED + 1))
run_suite "Check Tests" "./scripts/run-check-tests.sh" || TOTAL_FAILED=$((TOTAL_FAILED + 1))
run_suite "Fmt Tests" "./scripts/run-fmt-tests.sh" || TOTAL_FAILED=$((TOTAL_FAILED + 1))
run_suite "Meta Tests" "./scripts/run-meta-tests.sh" || TOTAL_FAILED=$((TOTAL_FAILED + 1))

# Summary
echo ""
echo "═══════════════════════════════════════════════════════════"
echo "WINDOWS TEST SUMMARY"
echo "═══════════════════════════════════════════════════════════"

read_json_field() {
    local file="$1"
    local field="$2"
    grep -o "\"$field\": *[0-9]*" "$file" 2>/dev/null | grep -o '[0-9]*' || echo "0"
}

UNIT_PASSED=$(read_json_field .test-results.json passed)
UNIT_FAILED=$(read_json_field .test-results.json failed)
CHECK_PASSED=$(read_json_field .check-test-results.json passed)
CHECK_FAILED=$(read_json_field .check-test-results.json failed)
FMT_PASSED=$(read_json_field .fmt-test-results.json passed)
FMT_FAILED=$(read_json_field .fmt-test-results.json failed)
META_PASSED=$(read_json_field .meta-test-results.json passed)
META_FAILED=$(read_json_field .meta-test-results.json failed)

TOTAL_PASSED=$((UNIT_PASSED + CHECK_PASSED + FMT_PASSED + META_PASSED))
TOTAL_FAIL=$((UNIT_FAILED + CHECK_FAILED + FMT_FAILED + META_FAILED))

printf "  %-15s %3d passed, %d failed\n" "Unit Tests:" "$UNIT_PASSED" "$UNIT_FAILED"
printf "  %-15s %3d passed, %d failed\n" "Check Tests:" "$CHECK_PASSED" "$CHECK_FAILED"
printf "  %-15s %3d passed, %d failed\n" "Fmt Tests:" "$FMT_PASSED" "$FMT_FAILED"
printf "  %-15s %3d passed, %d failed\n" "Meta Tests:" "$META_PASSED" "$META_FAILED"
echo "───────────────────────────────────────────────────────────"
printf "  %-15s %3d passed, %d failed\n" "TOTAL:" "$TOTAL_PASSED" "$TOTAL_FAIL"
echo ""
echo "Note: Native, App, Module, Args, Freestanding, and Selfhost tests"
echo "      require LLVM and are skipped on Windows (VM-only builds)."
echo ""

if [ $TOTAL_FAILED -eq 0 ]; then
    echo -e "${GREEN}All Windows tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed:$FAILED_SUITES${NC}"
    exit 1
fi
