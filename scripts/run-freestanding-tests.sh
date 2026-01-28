#!/bin/bash
# Freestanding/bare-metal compilation tests
# These test cross-compilation for targets like aarch64-none-elf

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RESULTS_FILE="$SCRIPT_DIR/.freestanding-test-results.json"
KLAR="$SCRIPT_DIR/zig-out/bin/klar"
TEST_DIR="$SCRIPT_DIR/test/native/freestanding"

# Ensure compiler is built
if [ ! -f "$KLAR" ]; then
    echo "Building Klar compiler first..."
    cd "$SCRIPT_DIR" && zig build || exit 1
fi

PASSED=0
FAILED=0
SKIPPED=0

# Check if cross-linker is available
has_cross_linker() {
    command -v aarch64-elf-ld >/dev/null 2>&1 || \
    command -v aarch64-none-elf-ld >/dev/null 2>&1
}

echo "=== Freestanding Tests ==="
echo ""

# Test 1: Bare-metal target compilation (object file only)
echo "--- Test: bare_metal_target.kl ---"
OUTPUT=$($KLAR build "$TEST_DIR/bare_metal_target.kl" \
    --target aarch64-none-elf --freestanding -c \
    -o /tmp/bare_metal_target.o 2>&1)
if [ $? -eq 0 ] && [ -f /tmp/bare_metal_target.o ]; then
    # Verify it's an ELF object file
    if file /tmp/bare_metal_target.o | grep -q "ELF.*aarch64"; then
        echo "  Compiles to aarch64 ELF object"
        PASSED=$((PASSED + 1))
    else
        echo "  FAIL: Not an aarch64 ELF object"
        FAILED=$((FAILED + 1))
    fi
    rm -f /tmp/bare_metal_target.o
else
    echo "  FAIL: Compilation failed"
    echo "  $OUTPUT"
    FAILED=$((FAILED + 1))
fi

# Test 2: Custom entry point
echo "--- Test: custom_entry.kl ---"
OUTPUT=$($KLAR build "$TEST_DIR/custom_entry.kl" \
    --target aarch64-none-elf --freestanding --entry kernel_main -c \
    -o /tmp/custom_entry.o 2>&1)
if [ $? -eq 0 ] && [ -f /tmp/custom_entry.o ]; then
    # Verify the entry point symbol exists (kernel_main, not main)
    if command -v aarch64-elf-nm >/dev/null 2>&1; then
        if aarch64-elf-nm /tmp/custom_entry.o 2>/dev/null | grep -q "kernel_main"; then
            echo "  Entry point renamed to kernel_main"
            PASSED=$((PASSED + 1))
        else
            echo "  FAIL: kernel_main symbol not found"
            FAILED=$((FAILED + 1))
        fi
    elif command -v llvm-nm >/dev/null 2>&1; then
        if llvm-nm /tmp/custom_entry.o 2>/dev/null | grep -q "kernel_main"; then
            echo "  Entry point renamed to kernel_main"
            PASSED=$((PASSED + 1))
        else
            echo "  FAIL: kernel_main symbol not found"
            FAILED=$((FAILED + 1))
        fi
    else
        # No nm available, just check compilation succeeded
        echo "  Compiles with --entry flag (nm not available to verify symbol)"
        PASSED=$((PASSED + 1))
    fi
    rm -f /tmp/custom_entry.o
else
    echo "  FAIL: Compilation failed"
    echo "  $OUTPUT"
    FAILED=$((FAILED + 1))
fi

# Test 3: Linker script (requires cross-linker)
echo "--- Test: linker_script.kl ---"
if has_cross_linker; then
    OUTPUT=$($KLAR build "$TEST_DIR/linker_script.kl" \
        --target aarch64-none-elf --freestanding \
        -T "$TEST_DIR/test.ld" \
        -o /tmp/linker_script_test 2>&1)
    if [ $? -eq 0 ] && [ -f /tmp/linker_script_test ]; then
        echo "  Links with custom linker script"
        PASSED=$((PASSED + 1))
        rm -f /tmp/linker_script_test
    else
        echo "  FAIL: Linking failed"
        echo "  $OUTPUT"
        FAILED=$((FAILED + 1))
    fi
else
    echo "  SKIP: Cross-linker not available (aarch64-elf-ld)"
    SKIPPED=$((SKIPPED + 1))
fi

echo ""
TOTAL=$((PASSED + FAILED))

# Write results JSON
cat > "$RESULTS_FILE" << EOF
{
  "passed": $PASSED,
  "failed": $FAILED,
  "skipped": $SKIPPED,
  "total": $TOTAL
}
EOF

if [ $FAILED -eq 0 ]; then
    if [ $SKIPPED -gt 0 ]; then
        echo "All $PASSED freestanding tests passed ($SKIPPED skipped)"
    else
        echo "All $PASSED freestanding tests passed"
    fi
    exit 0
else
    echo "$FAILED/$TOTAL freestanding tests failed"
    exit 1
fi
