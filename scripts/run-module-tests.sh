#!/bin/bash
# AirTower test wrapper for Klar module/multi-file compilation tests
# Tests import resolution, visibility, circular detection, etc.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RESULTS_FILE="$SCRIPT_DIR/.module-test-results.json"
KLAR="$SCRIPT_DIR/zig-out/bin/klar"
TEST_DIR="$SCRIPT_DIR/test/module"

# Ensure compiler is built
if [ ! -f "$KLAR" ]; then
    echo "Building Klar compiler first..."
    cd "$SCRIPT_DIR" && zig build || exit 1
fi

# Portable timeout: use 'timeout' if available, else 'gtimeout', else a shell fallback
run_with_timeout() {
    local secs="$1"; shift
    if command -v timeout >/dev/null 2>&1; then
        timeout "$secs" "$@"
    elif command -v gtimeout >/dev/null 2>&1; then
        gtimeout "$secs" "$@"
    else
        # Shell-based fallback: run in background, kill after timeout
        "$@" &
        local pid=$!
        ( sleep "$secs"; kill "$pid" 2>/dev/null ) &
        local watchdog=$!
        wait "$pid" 2>/dev/null
        local ret=$?
        kill "$watchdog" 2>/dev/null
        wait "$watchdog" 2>/dev/null
        return $ret
    fi
}

PASSED=0
FAILED=0
FAILURES=""

# Helper to record a failure
record_failure() {
    local name="$1"
    local msg="$2"
    FAILED=$((FAILED + 1))
    if [ -n "$FAILURES" ]; then
        FAILURES="$FAILURES,"
    fi
    FAILURES="$FAILURES\"$name: $msg\""
}

# Helper to record success
record_success() {
    local name="$1"
    PASSED=$((PASSED + 1))
}

echo "=== Module Tests ==="
echo ""

# Test 1: Basic multi-file import
echo "--- basic: Multi-file import ---"
temp_bin="/tmp/klar_module_basic"
if $KLAR build $TEST_DIR/basic/main.kl -o "$temp_bin" 2>/dev/null; then
    result=$("$temp_bin" 2>/dev/null; echo $?)
    # greet() returns 42, add(10, 20) returns 30, total = 72
    if [ "$result" = "72" ]; then
        echo "✓ basic (exit: $result)"
        record_success "basic"
    else
        echo "✗ basic (expected: 72, got: $result)"
        record_failure "basic" "expected 72, got $result"
    fi
    rm -f "$temp_bin"
else
    echo "✗ basic (build failed)"
    record_failure "basic" "build failed"
fi

# Test 2: Nested module paths
echo "--- nested: Nested module paths ---"
temp_bin="/tmp/klar_module_nested"
if $KLAR build $TEST_DIR/nested/main.kl -o "$temp_bin" 2>/dev/null; then
    result=$("$temp_bin" 2>/dev/null; echo $?)
    # add(5, 3) = 8, multiply(4, 2) = 8, total = 16
    if [ "$result" = "16" ]; then
        echo "✓ nested (exit: $result)"
        record_success "nested"
    else
        echo "✗ nested (expected: 16, got: $result)"
        record_failure "nested" "expected 16, got $result"
    fi
    rm -f "$temp_bin"
else
    echo "✗ nested (build failed)"
    record_failure "nested" "build failed"
fi

# Test 3: Visibility enforcement (should FAIL to compile)
echo "--- visibility: Private symbol import rejected ---"
temp_bin="/tmp/klar_module_visibility"
error_output=$($KLAR build $TEST_DIR/visibility/main.kl -o "$temp_bin" 2>&1)
# Check if output contains error (compiler may return 0 even with errors)
if echo "$error_output" | grep -q "not found\|not exported\|private\|error"; then
    if echo "$error_output" | grep -q "private_fn.*not found\|symbol.*not found"; then
        echo "✓ visibility (correctly rejected private import)"
        record_success "visibility"
    else
        echo "✗ visibility (failed but wrong error)"
        record_failure "visibility" "wrong error message"
    fi
    rm -f "$temp_bin" 2>/dev/null
else
    echo "✗ visibility (should have failed but succeeded)"
    record_failure "visibility" "should have rejected private import"
    rm -f "$temp_bin"
fi

# Test 4: Circular import detection (should FAIL to compile)
echo "--- circular: Circular import detected ---"
temp_bin="/tmp/klar_module_circular"
if [ -d "$TEST_DIR/circular" ]; then
    error_output=$($KLAR build $TEST_DIR/circular/main.kl -o "$temp_bin" 2>&1)
    # Check output for circular detection
    if echo "$error_output" | grep -qi "circular"; then
        echo "✓ circular (correctly detected circular import)"
        record_success "circular"
    else
        echo "✗ circular (should have detected circular import)"
        echo "    output: $error_output"
        record_failure "circular" "should have detected circular import"
    fi
    rm -f "$temp_bin" 2>/dev/null
else
    echo "⊘ circular (test not found, skipping)"
fi

# Test 5: Glob imports
echo "--- glob: Glob import (.*) ---"
temp_bin="/tmp/klar_module_glob"
if [ -d "$TEST_DIR/glob" ]; then
    if $KLAR build $TEST_DIR/glob/main.kl -o "$temp_bin" 2>/dev/null; then
        result=$("$temp_bin" 2>/dev/null; echo $?)
        if [ "$result" = "42" ]; then
            echo "✓ glob (exit: $result)"
            record_success "glob"
        else
            echo "✗ glob (expected: 42, got: $result)"
            record_failure "glob" "expected 42, got $result"
        fi
        rm -f "$temp_bin"
    else
        echo "✗ glob (build failed)"
        record_failure "glob" "build failed"
    fi
else
    echo "⊘ glob (test not found, skipping)"
fi

# Test 6: Aliased imports
echo "--- alias: Aliased import (as) ---"
temp_bin="/tmp/klar_module_alias"
if [ -d "$TEST_DIR/alias" ]; then
    if $KLAR build $TEST_DIR/alias/main.kl -o "$temp_bin" 2>/dev/null; then
        "$temp_bin" >/dev/null 2>&1
        result=$?
        if [ "$result" = "42" ]; then
            echo "✓ alias (exit: $result)"
            record_success "alias"
        else
            echo "✗ alias (expected: 42, got: $result)"
            record_failure "alias" "expected 42, got $result"
        fi
        rm -f "$temp_bin"
    else
        echo "✗ alias (build failed)"
        record_failure "alias" "build failed"
    fi
else
    echo "⊘ alias (test not found, skipping)"
fi

# Test 7: Sibling directory imports (test file imports from sibling lib/)
# This tests that imports resolve relative to cwd, not just relative to entry file
echo "--- sibling: Import from sibling directory ---"
temp_bin="/tmp/klar_module_sibling"
if [ -d "$TEST_DIR/sibling" ]; then
    # Run from the sibling test directory (simulating running tests from project root)
    pushd "$TEST_DIR/sibling" > /dev/null
    if $KLAR build tests/main.kl -o "$temp_bin" 2>/dev/null; then
        "$temp_bin" >/dev/null 2>&1
        result=$?
        if [ "$result" = "42" ]; then
            echo "✓ sibling (exit: $result)"
            record_success "sibling"
        else
            echo "✗ sibling (expected: 42, got: $result)"
            record_failure "sibling" "expected 42, got $result"
        fi
        rm -f "$temp_bin"
    else
        echo "✗ sibling (build failed)"
        record_failure "sibling" "build failed"
    fi
    popd > /dev/null
else
    echo "⊘ sibling (test not found, skipping)"
fi

# Test 8: Meta annotation import
echo "--- meta_import: Import custom meta annotations ---"
temp_bin="/tmp/klar_module_meta_import"
if [ -d "$TEST_DIR/meta_import" ]; then
    if $KLAR build $TEST_DIR/meta_import/main.kl -o "$temp_bin" 2>/dev/null; then
        "$temp_bin" >/dev/null 2>&1
        result=$?
        if [ "$result" = "0" ]; then
            echo "✓ meta_import (exit: $result)"
            record_success "meta_import"
        else
            echo "✗ meta_import (expected: 0, got: $result)"
            record_failure "meta_import" "expected 0, got $result"
        fi
        rm -f "$temp_bin"
    else
        echo "✗ meta_import (build failed)"
        record_failure "meta_import" "build failed"
    fi
else
    echo "⊘ meta_import (test not found, skipping)"
fi

# Test 9: JSON library (stdlib/json.kl)
echo "--- json: JSON library (stdlib/json.kl) ---"
temp_bin="/tmp/klar_module_json"
if [ -d "$TEST_DIR/json" ]; then
    if $KLAR build $TEST_DIR/json/main.kl -o "$temp_bin" 2>/dev/null; then
        "$temp_bin" >/dev/null 2>&1
        result=$?
        if [ "$result" = "0" ]; then
            echo "✓ json (exit: $result)"
            record_success "json"
        else
            echo "✗ json (expected: 0, got: $result)"
            record_failure "json" "expected 0, got $result"
        fi
        rm -f "$temp_bin"
    else
        echo "✗ json (build failed)"
        record_failure "json" "build failed"
    fi
else
    echo "⊘ json (test not found, skipping)"
fi

# Test 10: String.as_str() use-after-free fix
echo "--- string_as_str: String.as_str() returns safe copy ---"
temp_bin="/tmp/klar_module_string_as_str"
if [ -d "$TEST_DIR/string_as_str" ]; then
    if $KLAR build $TEST_DIR/string_as_str/main.kl -o "$temp_bin" 2>/dev/null; then
        "$temp_bin" >/dev/null 2>&1
        result=$?
        if [ "$result" = "0" ]; then
            echo "✓ string_as_str (exit: $result)"
            record_success "string_as_str"
        else
            echo "✗ string_as_str (expected: 0, got: $result)"
            record_failure "string_as_str" "expected 0, got $result"
        fi
        rm -f "$temp_bin"
    else
        echo "✗ string_as_str (build failed)"
        record_failure "string_as_str" "build failed"
    fi
else
    echo "⊘ string_as_str (test not found, skipping)"
fi

# Test 11: SHA-256 library (stdlib/sha256.kl)
echo "--- sha256: SHA-256 library (stdlib/sha256.kl) ---"
temp_bin="/tmp/klar_module_sha256"
if [ -d "$TEST_DIR/sha256" ]; then
    if $KLAR build $TEST_DIR/sha256/main.kl -o "$temp_bin" 2>/dev/null; then
        "$temp_bin" >/dev/null 2>&1
        result=$?
        if [ "$result" = "0" ]; then
            echo "✓ sha256 (exit: $result)"
            record_success "sha256"
        else
            echo "✗ sha256 (expected: 0, got: $result)"
            record_failure "sha256" "expected 0, got $result"
        fi
        rm -f "$temp_bin"
    else
        echo "✗ sha256 (build failed)"
        record_failure "sha256" "build failed"
    fi
else
    echo "⊘ sha256 (test not found, skipping)"
fi

# Test 12: CLI library (stdlib/cli.kl)
echo "--- cli: CLI argument parsing (stdlib/cli.kl) ---"
temp_bin="/tmp/klar_module_cli"
if [ -d "$TEST_DIR/cli" ]; then
    if $KLAR build $TEST_DIR/cli/main.kl -o "$temp_bin" 2>/dev/null; then
        output=$("$temp_bin" 2>/dev/null)
        result=$?
        if [ "$result" = "0" ]; then
            echo "✓ cli (exit: $result)"
            record_success "cli"
        else
            echo "✗ cli (expected: 0, got: $result)"
            echo "    output: $output"
            record_failure "cli" "expected 0, got $result"
        fi
        rm -f "$temp_bin"
    else
        echo "✗ cli (build failed)"
        record_failure "cli" "build failed"
    fi
else
    echo "⊘ cli (test not found, skipping)"
fi

# Test 13: Cross-module string enum payload
echo "--- string_enum: Cross-module string enum .len() ---"
temp_bin="/tmp/klar_module_string_enum"
if [ -d "$TEST_DIR/string_enum" ]; then
    if $KLAR build $TEST_DIR/string_enum/main.kl -o "$temp_bin" 2>/dev/null; then
        "$temp_bin" >/dev/null 2>&1
        result=$?
        if [ "$result" = "0" ]; then
            echo "✓ string_enum (exit: $result)"
            record_success "string_enum"
        else
            echo "✗ string_enum (expected: 0, got: $result)"
            record_failure "string_enum" "expected 0, got $result"
        fi
        rm -f "$temp_bin"
    else
        echo "✗ string_enum (build failed)"
        record_failure "string_enum" "build failed"
    fi
else
    echo "⊘ string_enum (test not found, skipping)"
fi

# Test 14: Cross-module string enum complex (index_of, starts_with, contains)
echo "--- string_enum_complex: Cross-module string enum methods ---"
temp_bin="/tmp/klar_module_string_enum_complex"
if [ -d "$TEST_DIR/string_enum_complex" ]; then
    if $KLAR build $TEST_DIR/string_enum_complex/main.kl -o "$temp_bin" 2>/dev/null; then
        "$temp_bin" >/dev/null 2>&1
        result=$?
        if [ "$result" = "0" ]; then
            echo "✓ string_enum_complex (exit: $result)"
            record_success "string_enum_complex"
        else
            echo "✗ string_enum_complex (expected: 0, got: $result)"
            record_failure "string_enum_complex" "expected 0, got $result"
        fi
        rm -f "$temp_bin"
    else
        echo "✗ string_enum_complex (build failed)"
        record_failure "string_enum_complex" "build failed"
    fi
else
    echo "⊘ string_enum_complex (test not found, skipping)"
fi

# Test 15: TOML library (stdlib/toml.kl)
echo "--- toml: TOML library (stdlib/toml.kl) ---"
temp_bin="/tmp/klar_module_toml"
if [ -d "$TEST_DIR/toml" ]; then
    if $KLAR build $TEST_DIR/toml/main.kl -o "$temp_bin" 2>/dev/null; then
        "$temp_bin" >/dev/null 2>&1
        result=$?
        if [ "$result" = "0" ]; then
            echo "✓ toml (exit: $result)"
            record_success "toml"
        else
            echo "✗ toml (expected: 0, got: $result)"
            record_failure "toml" "expected 0, got $result"
        fi
        rm -f "$temp_bin"
    else
        echo "✗ toml (build failed)"
        record_failure "toml" "build failed"
    fi
else
    echo "⊘ toml (test not found, skipping)"
fi

# Test 16: Integration test (all stdlib libraries together)
echo "--- integration: All stdlib libraries composed ---"
temp_bin="/tmp/klar_module_integration"
if [ -d "$TEST_DIR/integration" ]; then
    if $KLAR build $TEST_DIR/integration/main.kl -o "$temp_bin" 2>/dev/null; then
        output=$("$temp_bin" 2>/dev/null)
        result=$?
        if [ "$result" = "0" ]; then
            echo "✓ integration (exit: $result)"
            record_success "integration"
        else
            echo "✗ integration (expected: 0, got: $result)"
            echo "    output: $output"
            record_failure "integration" "expected 0, got $result"
        fi
        rm -f "$temp_bin"
    else
        echo "✗ integration (build failed)"
        record_failure "integration" "build failed"
    fi
else
    echo "⊘ integration (test not found, skipping)"
fi

# Test 17 & 18: HTTP server/client tests require POSIX process APIs (fork/pipe/execvp)
# Skip on Windows where these APIs are not available
echo "--- http_server: HTTP server library ---"
temp_bin="/tmp/klar_module_http_server"
if [[ "$OS" == "Windows_NT" ]]; then
    echo "⊘ http_server (skipped on Windows — requires POSIX process APIs)"
elif [ -d "$TEST_DIR/http_server" ]; then
    if $KLAR build $TEST_DIR/http_server/main.kl -o "$temp_bin" 2>/dev/null; then
        run_with_timeout 30 "$temp_bin" >/dev/null 2>&1
        result=$?
        if [ "$result" = "0" ]; then
            echo "✓ http_server (exit: $result)"
            record_success "http_server"
        else
            echo "✗ http_server (expected: 0, got: $result)"
            record_failure "http_server" "expected 0, got $result"
        fi
        rm -f "$temp_bin"
    else
        echo "✗ http_server (build failed)"
        record_failure "http_server" "build failed"
    fi
else
    echo "⊘ http_server (test not found, skipping)"
fi

echo "--- http_client: HTTP client library ---"
temp_bin="/tmp/klar_module_http_client"
if [[ "$OS" == "Windows_NT" ]]; then
    echo "⊘ http_client (skipped on Windows — requires POSIX process APIs)"
elif [ -d "$TEST_DIR/http_client" ]; then
    if $KLAR build $TEST_DIR/http_client/main.kl -o "$temp_bin" 2>/dev/null; then
        run_with_timeout 30 "$temp_bin" >/dev/null 2>&1
        result=$?
        if [ "$result" = "0" ]; then
            echo "✓ http_client (exit: $result)"
            record_success "http_client"
        else
            echo "✗ http_client (expected: 0, got: $result)"
            record_failure "http_client" "expected 0, got $result"
        fi
        rm -f "$temp_bin"
    else
        echo "✗ http_client (build failed)"
        record_failure "http_client" "build failed"
    fi
else
    echo "⊘ http_client (test not found, skipping)"
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
    echo "All $PASSED module tests passed"
else
    echo "$FAILED/$TOTAL module tests failed"
    exit 1
fi
