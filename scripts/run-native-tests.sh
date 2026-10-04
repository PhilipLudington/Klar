#!/bin/bash
# AirTower test wrapper for Klar native compilation tests
# Compiles and runs test/native/*.kl files
#
# Test conventions:
#   - "// Expected: build-error" in first 5 lines = test should fail to compile
#   - "// Requires: c-helper" in first 5 lines = test needs external C library
#   - "// Skip: native-tests" in first 5 lines = skip (handled by different runner)
#   - "// Expected: trap" in first 5 lines = a runtime check must stop the program
#     through llvm.trap: SIGILL (exit 132, x86) or SIGTRAP (133, arm64) on POSIX, so
#     a segfault or a SIGFPE from a bare `sdiv` does not pass; any non-zero exit on
#     Windows, where Git Bash reports the exception code's low byte.

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

# Per-test run timeout, so one hanging binary is one failure instead of a hung
# job (the Linux ARM64 CI job sat 98 minutes on array_bounds). GNU timeout is
# `timeout` on Linux and Git Bash and `gtimeout` from Homebrew coreutils on
# macOS; `--version` tells it apart from Windows' own timeout.exe. Without
# either, tests run untimed as before.
NATIVE_TEST_TIMEOUT="${NATIVE_TEST_TIMEOUT:-60}"
RUN_TIMEOUT=""
if timeout --version >/dev/null 2>&1; then
    TIMEOUT_CMD="timeout"
elif gtimeout --version >/dev/null 2>&1; then
    TIMEOUT_CMD="gtimeout"
fi
# -k: a binary that ignores SIGTERM is killed 5s later instead of hanging the job.
if [ -n "${TIMEOUT_CMD:-}" ]; then
    RUN_TIMEOUT="$TIMEOUT_CMD -k 5 $NATIVE_TEST_TIMEOUT"
fi

# A run timed out: 124 when SIGTERM ended it, 137 when the -k SIGKILL did.
is_timeout_exit() {
    [ -n "$RUN_TIMEOUT" ] && { [ "$1" -eq 124 ] || [ "$1" -eq 137 ]; }
}

# Check if test expects a runtime trap (looks in first 5 lines)
expects_trap() {
    head -5 "$1" | grep -q "// Expected: trap"
}

# Did the exit code come from a trap? See "// Expected: trap" above.
is_trap_exit() {
    if [[ "$OS" == "Windows_NT" ]]; then
        [ "$1" -ne 0 ]
    else
        [ "$1" -eq 132 ] || [ "$1" -eq 133 ]
    fi
}

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

# Check if test should be skipped (handled by different runner or platform-specific)
should_skip() {
    head -5 "$1" | grep -q "// Skip: native-tests" && return 0
    if [[ "$OS" == "Windows_NT" ]]; then
        head -5 "$1" | grep -q "// Skip: windows" && return 0
    else
        head -5 "$1" | grep -q "// Requires: windows" && return 0
    fi
    return 1
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
        string_enum_cross_module) echo 0 ;;  # string from enum payload (Bug 5)
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
        list_push_literal_width) echo 0 ;;  # push stores a literal at the element's width (Bug 79)
        channel_send_literal_width) echo 0 ;;  # send stores a literal at the element's width (Bug 79)
        hint_reach_literal_width) echo 0 ;;  # a hint never reaches a nested literal (Bug 79)
        channel_param_endpoints) echo 0 ;;  # send/recv/close through a Sender/Receiver parameter (Bug 83)
        channel_field_endpoints) echo 0 ;;  # send/recv through a struct or tuple field (Bug 86)
        channel_alias_endpoints) echo 0 ;;  # send/recv through an aliased endpoint type (Bug 87)
        channel_index_endpoints) echo 0 ;;  # send/recv through an index or a for binding (Bug 90)
        type_alias_lowering) echo 0 ;;  # an alias lowers as the type it names (Bug 89)
        type_alias_readers) echo 0 ;;  # struct and unsigned aliases read as their targets (Bug 89)
        type_alias_declarations) echo 0 ;;  # var, method, ref and string alias declarations (Bug 89)
        none_hint_width) echo 0 ;;  # a bare None under a tuple or push hint takes the optional's layout (Bug 84)
        unsigned_index_and_division) echo 0 ;;  # unsigned index and division keep their own semantics (Bugs 74, 75)
        unsigned_operand_sources) echo 0 ;;
        array_to_slice_coercion) echo 0 ;;  # any array stored into a slice becomes { ptr, len } (Bug 97)  # a field or element operand keeps its unsigned semantics (Bugs 74, 75 qa-fix)
        list_string_drop) echo 42 ;;
        list_index_assign) echo 42 ;;
        list_nested_basic) echo 42 ;;
        env_get_set) echo 42 ;;
        fs_stat) echo 42 ;;
        process_run) echo 42 ;;
        timestamp_now) echo 42 ;;
        result_tuple_string_helper) echo 42 ;;
        int_literal_bases) echo 0 ;;
        string_escape_hex_unicode) echo 0 ;;
        process_spawn_windows) echo 0 ;;
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
        # On Windows, MSVC link.exe adds .exe; use that path if it exists
        run_bin="$temp_bin"
        if [[ "$OS" == "Windows_NT" ]] && [ -f "$temp_bin.exe" ]; then
            run_bin="$temp_bin.exe"
        fi

        # Run and get exit code (capture stderr for diagnostics on failure)
        run_stderr_file="$BUILD_DIR/klar_run_stderr_$$"
        $RUN_TIMEOUT "$run_bin" 2>"$run_stderr_file"
        result=$?
        run_stderr=$(cat "$run_stderr_file" 2>/dev/null | head -3 || true)
        rm -f "$run_stderr_file"

        # Check against expected (if defined)
        expected=$(get_expected "$name")

        if is_timeout_exit $result; then
            echo "✗ $name (timed out after ${NATIVE_TEST_TIMEOUT}s)"
            FAILED=$((FAILED + 1))
            if [ -n "$FAILURES" ]; then
                FAILURES="$FAILURES,"
            fi
            FAILURES="$FAILURES\"$name: timed out after ${NATIVE_TEST_TIMEOUT}s\""
        elif expects_trap "$f"; then
            if is_trap_exit $result; then
                echo "✓ $name (trapped, exit: $result)"
                PASSED=$((PASSED + 1))
            else
                echo "✗ $name (expected a trap, got exit: $result)"
                FAILED=$((FAILED + 1))
                if [ -n "$FAILURES" ]; then
                    FAILURES="$FAILURES,"
                fi
                FAILURES="$FAILURES\"$name: expected a trap, got exit $result\""
            fi
        elif [ "$expected" = "-1" ] || [ $result -eq $expected ]; then
            echo "✓ $name (exit: $result)"
            PASSED=$((PASSED + 1))
        else
            echo "✗ $name (expected: $expected, got: $result)"
            if [ -n "$run_stderr" ]; then
                echo "  stderr: $run_stderr"
            fi
            FAILED=$((FAILED + 1))
            if [ -n "$FAILURES" ]; then
                FAILURES="$FAILURES,"
            fi
            FAILURES="$FAILURES\"$name: expected $expected, got $result\""
        fi

        rm -f "$temp_bin" "$temp_bin.exe"
    else
        # Show build error for diagnosis (first 3 lines)
        # Strip leading/trailing whitespace to avoid false positives from empty stderr
        build_err_line=$(echo "$build_stderr" | head -3 | tr '\n' ' ' | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
        if [ -n "$build_err_line" ]; then
            echo "✗ $name (build failed: $build_err_line)"
        else
            # Check stdout for linker/error messages (MSVC link.exe outputs some errors to stdout)
            build_out_err=$(echo "$build_stdout" | head -3 | tr '\n' ' ' | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
            if [ -n "$build_out_err" ]; then
                echo "✗ $name (build failed: $build_out_err)"
            else
                echo "✗ $name (build failed, no output)"
            fi
        fi
        FAILED=$((FAILED + 1))
        if [ -n "$FAILURES" ]; then
            FAILURES="$FAILURES,"
        fi
        FAILURES="$FAILURES\"$name: build failed\""
    fi
done

# Trap lowering (Bug 73): every runtime check's failure block must call
# llvm.trap before its `unreachable`. A bare `unreachable` is undefined
# behavior: aarch64 Linux emits no instruction for it, so a failed check falls
# through into whatever code follows (array_bounds hung there), and the
# optimizer may delete the check outright. test/native/runtime_traps.kl reaches
# each kind of check; this compiles it to LLVM IR (no link) and reads the IR.
trap_name="runtime_trap_lowering"
trap_dir="$BUILD_DIR/klar_trap_ir_$$"
mkdir -p "$trap_dir"
trap_ir="$trap_dir/runtime_traps.ll"
( cd "$trap_dir" && "$KLAR" build "$TEST_DIR/runtime_traps.kl" -c -o "$trap_dir/runtime_traps.o" --emit-llvm >/dev/null 2>&1 )
if [ -f "$trap_ir" ]; then
    # Pair each failure-block label with its first instruction.
    trap_blocks=$(awk '
        /^[A-Za-z_.0-9]*(fail|trap|failed)[0-9]*:/ { label = $1; next }
        label != "" && NF > 0 { print label " " $0; label = "" }
    ' "$trap_ir")
    trap_total=$(printf '%s\n' "$trap_blocks" | grep -c . || true)
    trap_bare=$(printf '%s\n' "$trap_blocks" | grep -v "call void @llvm.trap()" | grep -c . || true)
    # Every kind of check must still reach the IR, and the fixture's block count
    # must not drop: a failure block that disappears is a check that was lost
    # (Debt 6). Raise the floor when runtime_traps.kl gains a site.
    TRAP_BLOCK_FLOOR=33
    trap_missing=""
    for kind in bounds.fail list.bounds.fail set.fail overflow_trap div.fail unwrap.fail unwrap_err.fail match.failed; do
        if ! printf '%s\n' "$trap_blocks" | awk -v k="$kind" '{ l = $1; sub(/[0-9]*:$/, "", l); if (l == k) f = 1 } END { exit !f }'; then
            trap_missing="$trap_missing $kind"
        fi
    done
    if [ "$trap_bare" -eq 0 ] && [ "$trap_total" -ge "$TRAP_BLOCK_FLOOR" ] && [ -z "$trap_missing" ]; then
        echo "✓ $trap_name ($trap_total failure blocks trap)"
        PASSED=$((PASSED + 1))
    else
        if [ "$trap_bare" -ne 0 ]; then
            trap_why="$trap_bare of $trap_total failure blocks do not call llvm.trap"
        elif [ -n "$trap_missing" ]; then
            trap_why="no failure block for:$trap_missing"
        else
            trap_why="$trap_total failure blocks, fewer than $TRAP_BLOCK_FLOOR"
        fi
        echo "✗ $trap_name ($trap_why)"
        printf '%s\n' "$trap_blocks" | grep -v "call void @llvm.trap()" | head -5 | sed 's/^/  /'
        FAILED=$((FAILED + 1))
        if [ -n "$FAILURES" ]; then
            FAILURES="$FAILURES,"
        fi
        FAILURES="$FAILURES\"$trap_name: $trap_why\""
    fi
else
    echo "✗ $trap_name (runtime_traps.kl did not compile to LLVM IR)"
    FAILED=$((FAILED + 1))
    if [ -n "$FAILURES" ]; then
        FAILURES="$FAILURES,"
    fi
    FAILURES="$FAILURES\"$trap_name: runtime_traps.kl did not compile to LLVM IR\""
fi
rm -rf "$trap_dir"

# Timeout branch (Debt 7): a binary that never returns must be reported as timed
# out, not hang the job or pass. test/native/timeout_hang.kl loops forever; run it
# under a 1-second limit through the same timeout command and exit test as above.
if [ -n "$RUN_TIMEOUT" ]; then
    hang_name="native_timeout_branch"
    hang_bin="$BUILD_DIR/klar_test_timeout_hang"
    if $KLAR build "$TEST_DIR/timeout_hang.kl" -o "$hang_bin" 2>/dev/null | grep -q "^Built"; then
        hang_run="$hang_bin"
        if [[ "$OS" == "Windows_NT" ]] && [ -f "$hang_bin.exe" ]; then
            hang_run="$hang_bin.exe"
        fi
        $TIMEOUT_CMD -k 5 1 "$hang_run" >/dev/null 2>&1
        hang_result=$?
        if is_timeout_exit $hang_result; then
            echo "✓ $hang_name (hanging binary timed out, exit: $hang_result)"
            PASSED=$((PASSED + 1))
        else
            echo "✗ $hang_name (hanging binary was not reported as timed out, exit: $hang_result)"
            FAILED=$((FAILED + 1))
            if [ -n "$FAILURES" ]; then
                FAILURES="$FAILURES,"
            fi
            FAILURES="$FAILURES\"$hang_name: exit $hang_result, not a timeout\""
        fi
        rm -f "$hang_bin" "$hang_bin.exe"
    else
        echo "✗ $hang_name (timeout_hang.kl did not build)"
        FAILED=$((FAILED + 1))
        if [ -n "$FAILURES" ]; then
            FAILURES="$FAILURES,"
        fi
        FAILURES="$FAILURES\"$hang_name: timeout_hang.kl did not build\""
    fi
fi

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
