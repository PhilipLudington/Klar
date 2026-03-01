#!/bin/bash
# AirTower test wrapper for Klar native compilation tests
# Compiles and runs test/native/*.kl files
#
# Test conventions:
#   - "// Expected: build-error" in first 5 lines = test should fail to compile
#   - "// Requires: c-helper" in first 5 lines = test needs external C library
#   - "// Skip: native-tests" in first 5 lines = skip (handled by different runner)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RESULTS_FILE="$SCRIPT_DIR/.native-test-results.json"
KLAR="$SCRIPT_DIR/zig-out/bin/klar"
TEST_DIR="$SCRIPT_DIR/test/native"
HELPER_C="$TEST_DIR/ffi/helper.c"

# Cross-platform temp directory for test artifacts
BUILD_DIR="$SCRIPT_DIR/build"
mkdir -p "$BUILD_DIR"

# Ensure compiler is built
if [ ! -f "$KLAR" ]; then
    echo "Building Klar compiler first..."
    cd "$SCRIPT_DIR" && zig build || exit 1
fi

# Build C helper library if helper.c exists
if [ -f "$HELPER_C" ]; then
    if [[ "$OS" == "Windows_NT" ]]; then
        # MSVC toolchain: cl.exe + lib.exe
        # Use MSYS_NO_PATHCONV to prevent Git Bash from mangling /Fo and /OUT: paths
        HELPER_C_WIN="$(cygpath -w "$HELPER_C")"
        OBJ_WIN="$(cygpath -w "$BUILD_DIR/klarhelper.obj")"
        LIB_WIN="$(cygpath -w "$BUILD_DIR/klarhelper.lib")"
        MSYS_NO_PATHCONV=1 cl.exe /nologo /c "$HELPER_C_WIN" /Fo"$OBJ_WIN" || echo "Warning: cl.exe compile failed"
        MSYS_NO_PATHCONV=1 lib.exe /NOLOGO /OUT:"$LIB_WIN" "$OBJ_WIN" || echo "Warning: lib.exe failed"
        rm -f "$BUILD_DIR/klarhelper.obj"
        if [ -f "$BUILD_DIR/klarhelper.lib" ]; then
            echo "Built C helper library: $BUILD_DIR/klarhelper.lib"
        else
            echo "Warning: C helper library was not built"
        fi
    else
        cc -c "$HELPER_C" -o "$BUILD_DIR/klarhelper.o" 2>/dev/null
        ar rcs "$BUILD_DIR/libklarhelper.a" "$BUILD_DIR/klarhelper.o" 2>/dev/null
        rm -f "$BUILD_DIR/klarhelper.o"
    fi
fi

PASSED=0
FAILED=0
FAILURES=""

# Check if test expects a build error (looks in first 5 lines)
expects_build_error() {
    head -5 "$1" | grep -q "// Expected: build-error"
}

# Check if test requires external C helper library
requires_c_helper() {
    head -5 "$1" | grep -q "// Requires: c-helper"
}

# Extract -l flags from "// Requires: -lXXX" comments in first 5 lines
get_link_flags() {
    head -5 "$1" | grep '// Requires:' | grep -o -- '-l[^ ]*' | tr '\n' ' '
}

# Check if test should be skipped (handled by different runner)
should_skip() {
    head -5 "$1" | grep -q "// Skip: native-tests"
}

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
        async_await_basic) echo 0 ;;
        async_await_failed_error) echo 1 ;;
        async_await_pending_error) echo 1 ;;
        array) echo 42 ;;  # 10 + 20 + 12 = 42
        optional_some) echo 42 ;;  # Force unwrap Some(42)
        optional_unwrap) echo 42 ;;  # Force unwrap Some(42)
        optional_coalesce) echo 99 ;;  # None ?? 99 = 99
        optional_coalesce_some) echo 42 ;;  # Some(42) ?? 99 = 42
        optional_propagate) echo 52 ;;  # ? on Optional: 42 + 10 = 52
        result_propagate) echo 52 ;;  # ? on Result: 42 + 10 = 52
        result_propagate_simple) echo 42 ;;  # Simpler ? on Result test
        result_propagate_string) echo 43 ;;  # ? on Result#[i32, string] (sret)
        result_propagate_string_err) echo 0 ;;  # ? on Result#[String, String] (droppable sret)
        return_none) echo 0 ;;  # return None in fn -> ?T
        string_as_str_safe) echo 0 ;;  # as_str() returns safe copy
        saturating_add) echo 0 ;;  # +| clamps at INT_MAX/INT_MIN
        saturating_sub) echo 0 ;;  # -| clamps at INT_MAX/INT_MIN
        saturating_mul) echo 0 ;;  # *| clamps at INT_MAX/INT_MIN
        wrapping_add) echo 0 ;;  # +% wraps on overflow
        wrapping_sub) echo 0 ;;  # -% wraps on overflow
        wrapping_mul) echo 0 ;;  # *% wraps on overflow
        test_blocks_ignore_type_errors) echo 77 ;;
        test_blocks_ignore_runtime_failures) echo 78 ;;
        list_last) echo 42 ;;
        list_pop) echo 42 ;;
        list_string_drop) echo 42 ;;
        list_index_assign) echo 42 ;;
        list_nested_basic) echo 42 ;;
        env_get_set) echo 42 ;;
        fs_stat) echo 42 ;;
        process_run) echo 42 ;;
        timestamp_now) echo 42 ;;
        *) echo -1 ;;  # -1 means accept any result
    esac
}

# Run each test (including subdirectories)
for f in $(find "$TEST_DIR" -name "*.kl" | sort); do
    [ -f "$f" ] || continue

    name=$(basename "$f" .kl)
    temp_bin="$BUILD_DIR/klar_test_$name"

    # Check if test should be skipped
    if should_skip "$f"; then
        continue
    fi

    # Check if test expects a build error
    if expects_build_error "$f"; then
        # This test SHOULD fail to compile
        if $KLAR build "$f" -o "$temp_bin" 2>/dev/null | grep -q "^Built"; then
            # Compiled successfully - that's a failure for this test
            echo "✗ $name (expected build error, but compiled)"
            FAILED=$((FAILED + 1))
            if [ -n "$FAILURES" ]; then
                FAILURES="$FAILURES,"
            fi
            FAILURES="$FAILURES\"$name: expected build error, but compiled\""
            rm -f "$temp_bin"
        else
            # Build failed as expected
            echo "✓ $name (correctly rejected)"
            PASSED=$((PASSED + 1))
        fi
        continue
    fi

    # Normal test - compile and run
    # Add linker flags for tests requiring C helper or external libraries
    LINK_FLAGS=$(get_link_flags "$f")
    if requires_c_helper "$f"; then
        BUILD_CMD="$KLAR build $f -o $temp_bin -L$BUILD_DIR -lklarhelper $LINK_FLAGS"
    elif [ -n "$LINK_FLAGS" ]; then
        BUILD_CMD="$KLAR build $f -o $temp_bin $LINK_FLAGS"
    else
        BUILD_CMD="$KLAR build $f -o $temp_bin"
    fi

    build_stdout=$($BUILD_CMD 2>"$BUILD_DIR/klar_build_stderr_$$" || true)
    build_stderr=$(cat "$BUILD_DIR/klar_build_stderr_$$" 2>/dev/null || true)
    rm -f "$BUILD_DIR/klar_build_stderr_$$"

    if echo "$build_stdout" | grep -q "^Built"; then
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
        # Show build error for diagnosis (first 3 lines)
        build_err_line=$(echo "$build_stderr" | head -3 | tr '\n' ' ')
        if [ -n "$build_err_line" ]; then
            echo "✗ $name (build failed: $build_err_line)"
        else
            # Check stdout for linker messages (MSVC link.exe outputs some errors to stdout)
            build_out_err=$(echo "$build_stdout" | head -3 | tr '\n' ' ')
            if [ -n "$build_out_err" ]; then
                echo "✗ $name (build failed: $build_out_err)"
            else
                echo "✗ $name (build failed)"
            fi
        fi
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
