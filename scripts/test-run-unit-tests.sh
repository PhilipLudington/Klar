#!/bin/bash
# Regression test for scripts/run-unit-tests.sh's summary parsing (Bug 78).
# Runs a copy of the wrapper in a scratch directory, with a stub `zig` first on
# PATH printing a canned Zig 0.16 build summary, and checks the counts it writes.
# The copy keeps the real .test-results.json untouched.

set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

mkdir -p "$WORK/scripts" "$WORK/bin"
cp "$SCRIPT_DIR/run-unit-tests.sh" "$WORK/scripts/run-unit-tests.sh"

FAILURES=0

# check <name> <stub exit> <summary line> <expected wrapper exit> <expected failed> <expected passed>
check() {
    local name="$1" stub_exit="$2" line="$3" want_exit="$4" want_failed="$5" want_passed="$6"
    cat > "$WORK/bin/zig" << EOF
#!/bin/bash
echo "Build Summary: $line"
exit $stub_exit
EOF
    chmod +x "$WORK/bin/zig"
    PATH="$WORK/bin:$PATH" "$WORK/scripts/run-unit-tests.sh" > "$WORK/out.txt" 2>&1
    local got_exit=$?
    local json="$WORK/.test-results.json"
    local got_failed got_passed
    got_failed=$(grep -oE '"failed": [0-9]+' "$json" | grep -oE '[0-9]+$')
    got_passed=$(grep -oE '"passed": [0-9]+' "$json" | grep -oE '[0-9]+$')
    if [ "$got_exit" = "$want_exit" ] && [ "$got_failed" = "$want_failed" ] && [ "$got_passed" = "$want_passed" ]; then
        echo "PASS: $name"
    else
        echo "FAIL: $name — exit $got_exit (want $want_exit), failed $got_failed (want $want_failed), passed $got_passed (want $want_passed)"
        FAILURES=$((FAILURES + 1))
    fi
}

check "no skips" 0 "3/3 steps succeeded; 294/294 tests passed" 0 0 294
check "a skip is not a failure" 0 "3/3 steps succeeded; 293/294 tests passed (1 skipped)" 0 0 293
check "a failure beside a skip counts once" 1 "2/3 steps succeeded (1 failed); 292/294 tests passed (1 skipped, 1 failed)" 1 1 292
check "a skipped step does not count as a skipped test" 0 "3/4 steps succeeded (1 skipped); 290/294 tests passed (4 skipped)" 0 0 290

if [ "$FAILURES" -gt 0 ]; then
    echo "$FAILURES wrapper case(s) failed"
    exit 1
fi
echo "All wrapper cases passed"
