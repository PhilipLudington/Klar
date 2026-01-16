#!/bin/bash
# Test reference apps compile and run correctly

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
KLAR="$SCRIPT_DIR/zig-out/bin/klar"
APPS_DIR="$SCRIPT_DIR/examples/apps"

# Use gtimeout on macOS if available, otherwise try timeout
if command -v gtimeout &> /dev/null; then
    TIMEOUT_CMD="gtimeout"
elif command -v timeout &> /dev/null; then
    TIMEOUT_CMD="timeout"
else
    # No timeout command, just run directly
    TIMEOUT_CMD=""
fi

run_with_timeout() {
    if [ -n "$TIMEOUT_CMD" ]; then
        $TIMEOUT_CMD "$@"
    else
        shift  # Remove timeout argument
        "$@"
    fi
}

# Apps that work with native compilation
NATIVE_APPS=(
    "mandelbrot"
    "collatz"
    "fibonacci"
    "array_demo"
    "struct_demo"
    "closure_demo"
    "optional_demo"
    "result_demo"
    "rc_demo"
)

# Apps that only work with VM (need features not yet in native)
VM_ONLY_APPS=(
    "sort_viz"
)

passed=0
failed=0

echo "Testing reference apps..."
echo ""

# Test native compilation
for app in "${NATIVE_APPS[@]}"; do
    echo -n "Testing $app (native)... "
    temp_bin=$(mktemp)

    if $KLAR build "$APPS_DIR/$app.kl" -o "$temp_bin" 2>/dev/null; then
        # Run and capture output (with timeout, ignore exit code since apps don't return 0)
        output=$(run_with_timeout 5 "$temp_bin" 2>&1) || true
        if [ -n "$output" ]; then
            echo "OK"
            ((passed++))
        else
            echo "FAILED (no output)"
            ((failed++))
        fi
    else
        echo "FAILED (compile error)"
        ((failed++))
    fi

    rm -f "$temp_bin"
done

# Test VM-only apps
for app in "${VM_ONLY_APPS[@]}"; do
    echo -n "Testing $app (VM)... "

    if run_with_timeout 10 $KLAR run "$APPS_DIR/$app.kl" >/dev/null 2>&1; then
        echo "OK"
        ((passed++))
    else
        echo "FAILED"
        ((failed++))
    fi
done

echo ""
echo "Results: $passed passed, $failed failed"

if [ $failed -gt 0 ]; then
    exit 1
fi
