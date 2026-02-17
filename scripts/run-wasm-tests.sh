#!/bin/bash
# WebAssembly target test runner for Klar
# Compiles test/wasm/*.kl to .wasm and verifies build success
#
# Test conventions:
#   - "// Expected: build-success" = test should compile to .wasm successfully
#   - "// Expected: build-error"   = test should fail to compile (e.g., unsupported features)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
KLAR="$SCRIPT_DIR/zig-out/bin/klar"
TEST_DIR="$SCRIPT_DIR/test/wasm"
BUILD_DIR="$SCRIPT_DIR/build/wasm-tests"

# Ensure compiler is built
if [ ! -f "$KLAR" ]; then
    echo "Building Klar compiler first..."
    cd "$SCRIPT_DIR" && zig build || exit 1
fi

# Create build directory
mkdir -p "$BUILD_DIR"

PASSED=0
FAILED=0
TOTAL=0
FAILURES=""

echo "=== Klar WebAssembly Target Tests ==="
echo ""

for test_file in "$TEST_DIR"/*.kl; do
    [ -f "$test_file" ] || continue
    base=$(basename "$test_file" .kl)
    TOTAL=$((TOTAL + 1))

    # Check expected result
    expects_error=false
    if head -5 "$test_file" | grep -q "// Expected: build-error"; then
        expects_error=true
    fi

    # Compile to .wasm
    output="$BUILD_DIR/$base.wasm"
    compile_output=$("$KLAR" build "$test_file" --target wasm -o "$output" 2>&1)
    compile_status=$?

    if [ "$expects_error" = true ]; then
        # Expect failure
        if [ $compile_status -ne 0 ]; then
            echo "  PASS: $base (expected build error)"
            PASSED=$((PASSED + 1))
        else
            echo "  FAIL: $base (expected build error but succeeded)"
            FAILED=$((FAILED + 1))
            FAILURES="$FAILURES\n  $base: expected build error but succeeded"
        fi
    else
        # Expect success
        if [ $compile_status -eq 0 ] && [ -f "$output" ]; then
            # Verify it's actually a wasm file
            if file "$output" | grep -q "WebAssembly"; then
                echo "  PASS: $base"
                PASSED=$((PASSED + 1))
            else
                echo "  FAIL: $base (output is not WebAssembly)"
                FAILED=$((FAILED + 1))
                FAILURES="$FAILURES\n  $base: output is not WebAssembly"
            fi
        else
            echo "  FAIL: $base (compilation failed)"
            FAILED=$((FAILED + 1))
            FAILURES="$FAILURES\n  $base: $compile_output"
        fi
    fi
done

echo ""
echo "=== Results: $PASSED/$TOTAL passed, $FAILED failed ==="

if [ -n "$FAILURES" ]; then
    echo ""
    echo "Failures:"
    echo -e "$FAILURES"
fi

# Clean up build artifacts
rm -rf "$BUILD_DIR"

if [ $FAILED -gt 0 ]; then
    exit 1
fi
exit 0
