#!/bin/bash
# Master test runner - runs all test suites
# Individual test scripts are in scripts/

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Track overall results
TOTAL_PASSED=0
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
echo -e "${YELLOW}Building compiler...${NC}"
echo "═══════════════════════════════════════════════════════════"
if ! ./build.sh; then
    echo -e "${RED}Build failed - aborting tests${NC}"
    exit 1
fi

# Run all test suites
run_suite "Unit Tests" "./scripts/run-unit-tests.sh" || TOTAL_FAILED=$((TOTAL_FAILED + 1))
run_suite "Native Tests" "./scripts/run-native-tests.sh" || TOTAL_FAILED=$((TOTAL_FAILED + 1))
run_suite "App Tests" "./scripts/run-app-tests.sh" || TOTAL_FAILED=$((TOTAL_FAILED + 1))
run_suite "Module Tests" "./scripts/run-module-tests.sh" || TOTAL_FAILED=$((TOTAL_FAILED + 1))
run_suite "Args Tests" "./scripts/run-args-tests.sh" || TOTAL_FAILED=$((TOTAL_FAILED + 1))

# Summary
echo ""
echo "═══════════════════════════════════════════════════════════"
echo "SUMMARY"
echo "═══════════════════════════════════════════════════════════"

# Read results from JSON files
read_json_field() {
    local file="$1"
    local field="$2"
    grep -o "\"$field\": *[0-9]*" "$file" 2>/dev/null | grep -o '[0-9]*' || echo "0"
}

UNIT_PASSED=$(read_json_field .test-results.json passed)
UNIT_FAILED=$(read_json_field .test-results.json failed)
NATIVE_PASSED=$(read_json_field .native-test-results.json passed)
NATIVE_FAILED=$(read_json_field .native-test-results.json failed)
APP_PASSED=$(read_json_field .app-test-results.json passed)
APP_FAILED=$(read_json_field .app-test-results.json failed)
MODULE_PASSED=$(read_json_field .module-test-results.json passed)
MODULE_FAILED=$(read_json_field .module-test-results.json failed)
ARGS_PASSED=$(read_json_field .args-test-results.json passed)
ARGS_FAILED=$(read_json_field .args-test-results.json failed)

TOTAL_PASSED=$((UNIT_PASSED + NATIVE_PASSED + APP_PASSED + MODULE_PASSED + ARGS_PASSED))
TOTAL_FAILED=$((UNIT_FAILED + NATIVE_FAILED + APP_FAILED + MODULE_FAILED + ARGS_FAILED))

printf "  %-15s %3d passed, %d failed\n" "Unit Tests:" "$UNIT_PASSED" "$UNIT_FAILED"
printf "  %-15s %3d passed, %d failed\n" "Native Tests:" "$NATIVE_PASSED" "$NATIVE_FAILED"
printf "  %-15s %3d passed, %d failed\n" "App Tests:" "$APP_PASSED" "$APP_FAILED"
printf "  %-15s %3d passed, %d failed\n" "Module Tests:" "$MODULE_PASSED" "$MODULE_FAILED"
printf "  %-15s %3d passed, %d failed\n" "Args Tests:" "$ARGS_PASSED" "$ARGS_FAILED"
echo "───────────────────────────────────────────────────────────"
printf "  %-15s %3d passed, %d failed\n" "TOTAL:" "$TOTAL_PASSED" "$TOTAL_FAILED"
echo ""

if [ $TOTAL_FAILED -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed:$FAILED_SUITES${NC}"
    exit 1
fi
