#!/bin/bash
# Test command-line arguments across all three backends (native, VM, interpreter)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RESULTS_FILE="$SCRIPT_DIR/.args-test-results.json"
KLAR="$SCRIPT_DIR/zig-out/bin/klar"
TEST_DIR="$SCRIPT_DIR/test/args"

# Ensure compiler is built
if [ ! -f "$KLAR" ]; then
    echo "Building Klar compiler first..."
    cd "$SCRIPT_DIR" && ./run-build.sh || exit 1
fi

PASSED=0
FAILED=0
FAILURES=""

pass() {
    echo "✓ $1"
    PASSED=$((PASSED + 1))
}

fail() {
    echo "✗ $1"
    FAILED=$((FAILED + 1))
    if [ -n "$FAILURES" ]; then
        FAILURES="$FAILURES,"
    fi
    FAILURES="$FAILURES\"$1\""
}

echo "=== Command-Line Arguments Tests ==="
echo ""

# Test 1: args_count - check that arg count is correct across backends
echo "--- Test: args_count (3 backends × 2 scenarios) ---"

# Native: no args (just path)
output=$($KLAR run "$TEST_DIR/args_count.kl" 2>&1)
if echo "$output" | grep -q "^1$"; then
    pass "native: no user args -> prints 1 (path only)"
else
    fail "native: no user args -> expected '1' in output"
fi

# Native: with args (path + 2 user args)
output=$($KLAR run "$TEST_DIR/args_count.kl" foo bar 2>&1)
if echo "$output" | grep -q "^3$"; then
    pass "native: 2 user args -> prints 3"
else
    fail "native: 2 user args -> expected '3' in output"
fi

# VM: no args
output=$($KLAR run "$TEST_DIR/args_count.kl" --vm 2>&1)
if echo "$output" | grep -q "^1$"; then
    pass "vm: no user args -> prints 1 (path only)"
else
    fail "vm: no user args -> expected '1' in output"
fi

# VM: with args
output=$($KLAR run "$TEST_DIR/args_count.kl" --vm foo bar 2>&1)
if echo "$output" | grep -q "^3$"; then
    pass "vm: 2 user args -> prints 3"
else
    fail "vm: 2 user args -> expected '3' in output"
fi

# Interpreter: no args
output=$($KLAR run "$TEST_DIR/args_count.kl" --interpret 2>&1)
if echo "$output" | grep -q "^1$"; then
    pass "interpreter: no user args -> prints 1 (path only)"
else
    fail "interpreter: no user args -> expected '1' in output"
fi

# Interpreter: with args
output=$($KLAR run "$TEST_DIR/args_count.kl" --interpret foo bar 2>&1)
if echo "$output" | grep -q "^3$"; then
    pass "interpreter: 2 user args -> prints 3"
else
    fail "interpreter: 2 user args -> expected '3' in output"
fi

echo ""
echo "--- Test: args_access (verify arg values via VM/interpreter) ---"

# VM: check output contains the args
output=$($KLAR run "$TEST_DIR/args_access.kl" --vm hello world 2>&1)
if echo "$output" | grep -q "hello" && echo "$output" | grep -q "world"; then
    pass "vm: args accessible and correct"
else
    fail "vm: args not accessible"
fi

# Interpreter: check output
output=$($KLAR run "$TEST_DIR/args_access.kl" --interpret hello world 2>&1)
if echo "$output" | grep -q "hello" && echo "$output" | grep -q "world"; then
    pass "interpreter: args accessible and correct"
else
    fail "interpreter: args not accessible"
fi

echo ""
echo "--- Test: no_args (backwards compatibility) ---"

# VM: main() without args still works
output=$($KLAR run "$TEST_DIR/no_args.kl" --vm 2>&1)
if [ $? -eq 0 ] && ! echo "$output" | grep -qi "error"; then
    pass "vm: main() without args works"
else
    fail "vm: main() without args failed"
fi

# Interpreter: main() without args still works
output=$($KLAR run "$TEST_DIR/no_args.kl" --interpret 2>&1)
if [ $? -eq 0 ] && ! echo "$output" | grep -qi "error"; then
    pass "interpreter: main() without args works"
else
    fail "interpreter: main() without args failed"
fi

# Native: build and run to check exit code
temp_bin="/tmp/klar_args_test_no_args"
if $KLAR build "$TEST_DIR/no_args.kl" -o "$temp_bin" 2>/dev/null; then
    "$temp_bin" 2>/dev/null
    if [ $? -eq 42 ]; then
        pass "native: main() without args returns 42"
    else
        fail "native: main() without args -> expected exit 42"
    fi
    rm -f "$temp_bin"
else
    fail "native: main() without args -> build failed"
fi

echo ""
echo "--- Test: -- separator passes flags as args ---"

output=$($KLAR run "$TEST_DIR/args_access.kl" --vm -- --help -v 2>&1)
if echo "$output" | grep -q "\-\-help" && echo "$output" | grep -q "\-v"; then
    pass "vm: -- separator works"
else
    fail "vm: -- separator failed"
fi

output=$($KLAR run "$TEST_DIR/args_access.kl" --interpret -- --help -v 2>&1)
if echo "$output" | grep -q "\-\-help" && echo "$output" | grep -q "\-v"; then
    pass "interpreter: -- separator works"
else
    fail "interpreter: -- separator failed"
fi

echo ""
echo "--- Test: assertion helpers across all backends ---"

output=$($KLAR run "$TEST_DIR/assert_helpers_backends.kl" 2>&1)
if [ $? -eq 0 ] && ! echo "$output" | grep -qi "error"; then
    pass "assert helpers: native backend"
else
    fail "assert helpers: native backend failed"
fi

output=$($KLAR run "$TEST_DIR/assert_helpers_backends.kl" --vm 2>&1)
if [ $? -eq 0 ] && ! echo "$output" | grep -qi "error"; then
    pass "assert helpers: vm backend"
else
    fail "assert helpers: vm backend failed"
fi

output=$($KLAR run "$TEST_DIR/assert_helpers_backends.kl" --interpret 2>&1)
if [ $? -eq 0 ] && ! echo "$output" | grep -qi "error"; then
    pass "assert helpers: interpreter backend"
else
    fail "assert helpers: interpreter backend failed"
fi

echo ""
echo "--- Test: klar test command ---"

output=$($KLAR test "$TEST_DIR/test_command_pass.kl" 2>&1)
if echo "$output" | grep -q "PASS target" && echo "$output" | grep -q "1 passed, 0 failed"; then
    pass "test command: passing test block"
else
    fail "test command: expected passing output"
fi

output=$($KLAR test "$TEST_DIR/test_command_alias_import.kl" 2>&1)
if echo "$output" | grep -q "PASS target" && echo "$output" | grep -q "1 passed, 0 failed"; then
    pass "test command: aliased import in test runtime"
else
    fail "test command: aliased import failed"
fi

output=$($KLAR test "$TEST_DIR/test_command_namespace_import.kl" 2>&1)
if echo "$output" | grep -q "PASS target" && echo "$output" | grep -q "1 passed, 0 failed"; then
    pass "test command: namespace import in test runtime"
else
    fail "test command: namespace import failed"
fi

output=$($KLAR test "$TEST_DIR/test_command_type_only_import.kl" 2>&1)
if echo "$output" | grep -q "PASS target" && echo "$output" | grep -q "1 passed, 0 failed"; then
    pass "test command: type-only specific import does not fail runtime setup"
else
    fail "test command: type-only specific import failed runtime setup"
fi

output=$($KLAR test "$TEST_DIR/test_command_filter.kl" --fn target_two 2>&1)
if echo "$output" | grep -q "PASS target_two" && ! echo "$output" | grep -q "PASS target_one" && echo "$output" | grep -q "1 passed, 0 failed"; then
    pass "test command: --fn filters tests by name"
else
    fail "test command: --fn filtering failed"
fi

output=$($KLAR test "$TEST_DIR/test_command_dir" 2>&1)
if echo "$output" | grep -q "PASS dir_one" && echo "$output" | grep -q "PASS dir_two" && echo "$output" | grep -q "Directory test result: 2 file(s), 0 failed"; then
    pass "test command: directory mode runs .kl files recursively"
else
    fail "test command: directory mode failed"
fi

output=$($KLAR test "$TEST_DIR/test_command_pass.kl" --json 2>&1)
if echo "$output" | grep -q '"file"' && \
   echo "$output" | grep -q '"total":1' && \
   echo "$output" | grep -q '"passed":1' && \
   echo "$output" | grep -q '"failed":0' && \
   echo "$output" | grep -q '"tests"' && \
   echo "$output" | grep -q '"assertions"' && \
   echo "$output" | grep -q '"type":"assert_eq"' && \
   echo "$output" | grep -q '"expected":"7"' && \
   echo "$output" | grep -q '"actual":"7"'; then
    pass "test command: --json emits per-test assertion details"
else
    fail "test command: --json file summary failed"
fi

output=$($KLAR test "$TEST_DIR/test_command_pass.kl" --json --include-source 2>&1)
if echo "$output" | grep -q '"source"' && \
   echo "$output" | grep -q '"source":"fn target() -> i32 {' && \
   echo "$output" | grep -q '"assertions"'; then
    pass "test command: --include-source embeds function source in JSON results"
else
    fail "test command: --include-source JSON source embedding failed"
fi

output=$($KLAR test "$TEST_DIR/test_command_dir" --json 2>&1)
if echo "$output" | grep -q '"path"' && echo "$output" | grep -q '"files"' && echo "$output" | grep -q '"summary"' && echo "$output" | grep -q '"tests"'; then
    pass "test command: --json emits machine-readable directory summary"
else
    fail "test command: --json directory summary failed"
fi

if $KLAR test "$TEST_DIR/test_command_fail.kl" --json >/tmp/klar_test_command_json_fail.out 2>&1; then
    fail "test command: --json should fail when assertion fails"
else
    output=$(cat /tmp/klar_test_command_json_fail.out)
    if echo "$output" | grep -q '"status":"FAIL"' && echo "$output" | grep -q '"type":"assert_eq"' && echo "$output" | grep -q '"expected":"8"' && echo "$output" | grep -q '"actual":"7"'; then
        pass "test command: --json includes expected vs actual for failing assertions"
    else
        fail "test command: --json missing expected/actual on assertion failure"
    fi
fi
rm -f /tmp/klar_test_command_json_fail.out

output=$($KLAR test "$TEST_DIR/test_command_empty_dir" --json 2>&1)
if echo "$output" | grep -q '"path"' && echo "$output" | grep -q '"files":\[\]' && echo "$output" | grep -q '"summary"'; then
    pass "test command: --json empty directory remains machine-readable"
else
    fail "test command: --json empty directory summary failed"
fi

if $KLAR test "$TEST_DIR/test_command_dir_error/file_parse_error.kl" --json >/tmp/klar_test_command_json_file_error.out 2>&1; then
    fail "test command: --json should fail for parse-error file"
else
    output=$(cat /tmp/klar_test_command_json_file_error.out)
    if echo "$output" | grep -q '"file"' && \
       echo "$output" | grep -q 'file_parse_error.kl' && \
       echo "$output" | grep -q '"failed":0' && \
       echo "$output" | grep -q '"errors"' && \
       echo "$output" | grep -q '"stage":"parse"'; then
        pass "test command: --json file parse errors still emit machine-readable summary"
    else
        fail "test command: --json file parse error summary missing"
    fi
fi
rm -f /tmp/klar_test_command_json_file_error.out

if $KLAR test "$TEST_DIR/test_command_dir_error" --json >/tmp/klar_test_command_json_dir_error.out 2>&1; then
    fail "test command: --json should still fail when a directory file errors"
else
    output=$(cat /tmp/klar_test_command_json_dir_error.out)
    if echo "$output" | grep -q '"failed_files":1' && echo "$output" | grep -q '"failed":0' && echo "$output" | grep -q 'file_parse_error.kl'; then
        pass "test command: --json directory summary includes errored files without failed test inflation"
    else
        fail "test command: --json directory error summary accounting mismatch"
    fi
fi
rm -f /tmp/klar_test_command_json_dir_error.out

output=$($KLAR test "$TEST_DIR/test_command_no_tests.kl" --strict-tests 2>&1)
if [ $? -eq 0 ] && echo "$output" | grep -q "No tests found" && echo "$output" | grep -q "Warning: no tests found"; then
    pass "test command: --strict-tests warns when no tests found"
else
    fail "test command: --strict-tests warning behavior failed"
fi

if $KLAR test "$TEST_DIR/test_command_no_tests.kl" --require-tests >/tmp/klar_test_command_require.out 2>&1; then
    fail "test command: --require-tests should fail when no tests found"
else
    output=$(cat /tmp/klar_test_command_require.out)
    if echo "$output" | grep -q "No tests found" && echo "$output" | grep -q "Error: --require-tests enabled"; then
        pass "test command: --require-tests fails when no tests found"
    else
        fail "test command: --require-tests output mismatch"
    fi
fi
rm -f /tmp/klar_test_command_require.out

if $KLAR test "$TEST_DIR/test_command_empty_dir" --require-tests >/tmp/klar_test_command_require_empty_dir.out 2>&1; then
    fail "test command: --require-tests should fail on empty directory"
else
    output=$(cat /tmp/klar_test_command_require_empty_dir.out)
    if echo "$output" | grep -q "No .kl files found" && echo "$output" | grep -q "Error: --require-tests enabled"; then
        pass "test command: --require-tests fails on empty directory"
    else
        fail "test command: --require-tests empty directory output mismatch"
    fi
fi
rm -f /tmp/klar_test_command_require_empty_dir.out

if $KLAR test "$TEST_DIR/test_command_dir_fail" --require-tests >/tmp/klar_test_command_require_fail_dir.out 2>&1; then
    fail "test command: failing test dir should return non-zero with --require-tests"
else
    output=$(cat /tmp/klar_test_command_require_fail_dir.out)
    if echo "$output" | grep -q "FAIL target" && ! echo "$output" | grep -qi "no tests found"; then
        pass "test command: --require-tests does not misreport no-tests for failing test dir"
    else
        fail "test command: --require-tests failing dir output mismatch"
    fi
fi
rm -f /tmp/klar_test_command_require_fail_dir.out

if $KLAR test "$TEST_DIR/test_command_fail.kl" >/tmp/klar_test_command_fail.out 2>&1; then
    fail "test command: failing test should return non-zero"
else
    output=$(cat /tmp/klar_test_command_fail.out)
    if echo "$output" | grep -q "FAIL target" && echo "$output" | grep -q "0 passed, 1 failed"; then
        pass "test command: failing test block returns non-zero"
    else
        fail "test command: failing output mismatch"
    fi
fi
rm -f /tmp/klar_test_command_fail.out

echo ""
echo "--- Test: klar check partial mode ---"

partial_fixture="/tmp/klar_check_partial_fixture.kl"
cat > "$partial_fixture" << 'EOF'
fn ok() -> i32 {
    return 1
}

fn broken( -> i32 {
    return 2
}

fn mismatch() -> i32 {
    let s: string = "oops"
    return s
}
EOF

output=$($KLAR check "$partial_fixture" --partial 2>&1)
if echo "$output" | grep -q "Parse diagnostics" && echo "$output" | grep -q "Type check failed"; then
    pass "check command: --partial reports parse and type diagnostics in one run"
else
    fail "check command: --partial diagnostics output mismatch"
fi
rm -f "$partial_fixture"

output=$($KLAR check "$TEST_DIR/no_args.kl" --scope-json 2>&1)
if echo "$output" | grep -q "Error: --scope-json requires --scope-at"; then
    pass "check command: --scope-json requires --scope-at"
else
    fail "check command: --scope-json dependency validation failed"
fi

expected_fixture="/tmp/klar_check_expected_type_fixture.kl"
cat > "$expected_fixture" << 'EOF'
fn main() -> void {
    let x: i32 = 1 + 2
}
EOF

output=$($KLAR check "$expected_fixture" --expected-type-at 2:18 2>&1)
if echo "$output" | grep -q "Expected type at 2:18: i32"; then
    pass "check command: --expected-type-at infers let initializer type"
else
    fail "check command: --expected-type-at inference output mismatch"
fi
rm -f "$expected_fixture"

expected_assign_fixture="/tmp/klar_check_expected_type_assign.kl"
cat > "$expected_assign_fixture" << 'EOF'
fn main() -> void {
    var x: i32 = 0
    x = 1 + 2
}
EOF

assign_line=$(grep -n "x = 1 + 2" "$expected_assign_fixture" | cut -d: -f1)
assign_col=$(awk '/x = 1 \+ 2/ { print index($0, "1 + 2") }' "$expected_assign_fixture")
output=$($KLAR check "$expected_assign_fixture" --expected-type-at "${assign_line}:${assign_col}" 2>&1)
if echo "$output" | grep -q "Expected type at ${assign_line}:${assign_col}: i32"; then
    pass "check command: --expected-type-at infers assignment RHS type from local binding"
else
    fail "check command: --expected-type-at assignment inference output mismatch"
fi
rm -f "$expected_assign_fixture"

expected_return_fixture="/tmp/klar_check_expected_type_return.kl"
cat > "$expected_return_fixture" << 'EOF'
fn f() -> i32 {
    return 1 + 2
}
EOF

ret_line=$(grep -n "return 1 + 2" "$expected_return_fixture" | cut -d: -f1)
ret_col=$(awk '/return 1 \+ 2/ { print index($0, "1 + 2") }' "$expected_return_fixture")
output=$($KLAR check "$expected_return_fixture" --expected-type-at "${ret_line}:${ret_col}" 2>&1)
if echo "$output" | grep -q "Expected type at ${ret_line}:${ret_col}: i32"; then
    pass "check command: --expected-type-at infers return expression type"
else
    fail "check command: --expected-type-at return inference output mismatch"
fi
rm -f "$expected_return_fixture"

expected_method_fixture="/tmp/klar_check_expected_type_method.kl"
cat > "$expected_method_fixture" << 'EOF'
struct Counter {
    value: i32
}

impl Counter {
    fn add(self: inout Counter, amount: i32) -> void {
        self.value = self.value + amount
    }
}

fn main() -> void {
    var c: Counter = Counter { value: 0 }
    c.add(1 + 2)
}
EOF

method_line=$(grep -n "c.add(1 + 2)" "$expected_method_fixture" | cut -d: -f1)
method_col=$(awk '/c.add\(1 \+ 2\)/ { print index($0, "1 + 2") }' "$expected_method_fixture")
output=$($KLAR check "$expected_method_fixture" --expected-type-at "${method_line}:${method_col}" 2>&1)
if echo "$output" | grep -q "Expected type at ${method_line}:${method_col}: i32"; then
    pass "check command: --expected-type-at infers method argument type"
else
    fail "check command: --expected-type-at method argument inference output mismatch"
fi
rm -f "$expected_method_fixture"

echo ""
echo "--- Test: klar lsp transport ---"

init_payload='{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}'
shutdown_payload='{"jsonrpc":"2.0","id":2,"method":"shutdown","params":null}'
exit_payload='{"jsonrpc":"2.0","method":"exit","params":null}'

lsp_output=$(
    {
        printf 'Content-Length: %d\r\n\r\n%s' "${#init_payload}" "$init_payload"
        printf 'Content-Length: %d\r\n\r\n%s' "${#shutdown_payload}" "$shutdown_payload"
        printf 'Content-Length: %d\r\n\r\n%s' "${#exit_payload}" "$exit_payload"
    } | $KLAR lsp 2>&1
)

if echo "$lsp_output" | grep -q "Content-Length:" && \
   echo "$lsp_output" | grep -q '"id":1' && \
   echo "$lsp_output" | grep -q '"diagnosticProvider"' && \
   echo "$lsp_output" | grep -q '"completionProvider"' && \
   echo "$lsp_output" | grep -q '"capabilities"' && \
   echo "$lsp_output" | grep -q '"id":2' && \
   echo "$lsp_output" | grep -q '"result":null'; then
    pass "lsp command: stdio JSON-RPC initialize/shutdown/exit handshake"
else
    fail "lsp command: JSON-RPC transport handshake failed"
fi

invalid_json_payload='{not json'
invalid_json_output=$(
    {
        printf 'Content-Length: %d\r\n\r\n%s' "${#invalid_json_payload}" "$invalid_json_payload"
    } | $KLAR lsp 2>&1
)
if echo "$invalid_json_output" | grep -q '"code":-32700' && \
   echo "$invalid_json_output" | grep -q '"id":null'; then
    pass "lsp command: malformed JSON returns parse error"
else
    fail "lsp command: malformed JSON parse error handling failed"
fi

invalid_request_payload='{"jsonrpc":"2.0","id":7,"method":123}'
invalid_request_output=$(
    {
        printf 'Content-Length: %d\r\n\r\n%s' "${#invalid_request_payload}" "$invalid_request_payload"
    } | $KLAR lsp 2>&1
)
if echo "$invalid_request_output" | grep -q '"code":-32600' && \
   echo "$invalid_request_output" | grep -q '"id":7'; then
    pass "lsp command: invalid request shape returns -32600"
else
    fail "lsp command: invalid request shape handling failed"
fi

unknown_method_payload='{"jsonrpc":"2.0","id":8,"method":"klar/unknown"}'
unknown_method_output=$(
    {
        printf 'Content-Length: %d\r\n\r\n%s' "${#unknown_method_payload}" "$unknown_method_payload"
    } | $KLAR lsp 2>&1
)
if echo "$unknown_method_output" | grep -q '"code":-32601' && \
   echo "$unknown_method_output" | grep -q '"id":8'; then
    pass "lsp command: unknown method returns -32601"
else
    fail "lsp command: unknown method handling failed"
fi

if printf 'Content-Length: 99999999\r\n\r\n{}' | $KLAR lsp >/tmp/klar_lsp_oversize.out 2>&1; then
    fail "lsp command: oversized content-length should fail"
else
    pass "lsp command: oversized content-length is rejected"
fi
rm -f /tmp/klar_lsp_oversize.out

exit_only_payload='{"jsonrpc":"2.0","method":"exit","params":null}'
if {
    printf 'Content-Length: %d\r\n\r\n%s' "${#exit_only_payload}" "$exit_only_payload"
} | $KLAR lsp >/tmp/klar_lsp_exit_only.out 2>&1; then
    fail "lsp command: exit without shutdown should return non-zero"
else
    pass "lsp command: exit without shutdown returns non-zero"
fi
rm -f /tmp/klar_lsp_exit_only.out

diag_fixture="/tmp/klar_lsp_diagnostic_fixture.kl"
cat > "$diag_fixture" << 'EOF'
fn main() -> i32 {
    let value: i32 = "oops"
    return value
}
EOF

diag_uri="file://${diag_fixture}"
diag_payload=$(printf '{"jsonrpc":"2.0","id":11,"method":"textDocument/diagnostic","params":{"textDocument":{"uri":"%s"}}}' "$diag_uri")
diag_output=$(
    {
        printf 'Content-Length: %d\r\n\r\n%s' "${#diag_payload}" "$diag_payload"
    } | $KLAR lsp 2>&1
)
if echo "$diag_output" | grep -q '"id":11' && \
   echo "$diag_output" | grep -q '"kind":"full"' && \
   echo "$diag_output" | grep -q '"source":"checker"' && \
   echo "$diag_output" | grep -q '"severity":1'; then
    pass "lsp command: textDocument/diagnostic returns checker diagnostics"
else
    fail "lsp command: textDocument/diagnostic handling failed"
fi
rm -f "$diag_fixture"

diag_mem_uri='file:///tmp/klar%20lsp%20memory.kl'
did_open_payload=$(printf '{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"%s","languageId":"klar","version":1,"text":"fn main() -> i32 {\\n    let value: i32 = \\"bad\\"\\n    return value\\n}\\n"}}}' "$diag_mem_uri")
diag_mem_bad_payload=$(printf '{"jsonrpc":"2.0","id":12,"method":"textDocument/diagnostic","params":{"textDocument":{"uri":"%s"}}}' "$diag_mem_uri")
did_change_payload=$(printf '{"jsonrpc":"2.0","method":"textDocument/didChange","params":{"textDocument":{"uri":"%s","version":2},"contentChanges":[{"text":"fn main() -> i32 {\\n    let value: i32 = 1\\n    return value\\n}\\n"}]}}' "$diag_mem_uri")
diag_mem_good_payload=$(printf '{"jsonrpc":"2.0","id":13,"method":"textDocument/diagnostic","params":{"textDocument":{"uri":"%s"}}}' "$diag_mem_uri")
did_close_payload=$(printf '{"jsonrpc":"2.0","method":"textDocument/didClose","params":{"textDocument":{"uri":"%s"}}}' "$diag_mem_uri")
diag_mem_closed_payload=$(printf '{"jsonrpc":"2.0","id":14,"method":"textDocument/diagnostic","params":{"textDocument":{"uri":"%s"}}}' "$diag_mem_uri")

diag_mem_output=$(
    {
        printf 'Content-Length: %d\r\n\r\n%s' "${#did_open_payload}" "$did_open_payload"
        printf 'Content-Length: %d\r\n\r\n%s' "${#diag_mem_bad_payload}" "$diag_mem_bad_payload"
        printf 'Content-Length: %d\r\n\r\n%s' "${#did_change_payload}" "$did_change_payload"
        printf 'Content-Length: %d\r\n\r\n%s' "${#diag_mem_good_payload}" "$diag_mem_good_payload"
        printf 'Content-Length: %d\r\n\r\n%s' "${#did_close_payload}" "$did_close_payload"
        printf 'Content-Length: %d\r\n\r\n%s' "${#diag_mem_closed_payload}" "$diag_mem_closed_payload"
    } | $KLAR lsp 2>&1
)
if echo "$diag_mem_output" | grep -q '"id":12' && \
   echo "$diag_mem_output" | grep -q '"source":"checker"' && \
   echo "$diag_mem_output" | grep -q '"id":13' && \
   echo "$diag_mem_output" | grep -F -q '"items":[]' && \
   echo "$diag_mem_output" | grep -q '"id":14' && \
   echo "$diag_mem_output" | grep -q '"code":-32603'; then
    pass "lsp command: didOpen/didChange/didClose drive in-memory diagnostics"
else
    fail "lsp command: in-memory document diagnostics lifecycle failed"
fi

diag_uri_fixture="/tmp/klar lsp uri fixture.kl"
cat > "$diag_uri_fixture" << 'EOF'
fn main() -> i32 {
    let value: i32 = "oops"
    return value
}
EOF
diag_uri_encoded='file:///tmp/klar%20lsp%20uri%20fixture.kl'
diag_uri_payload=$(printf '{"jsonrpc":"2.0","id":15,"method":"textDocument/diagnostic","params":{"textDocument":{"uri":"%s"}}}' "$diag_uri_encoded")
diag_uri_output=$(
    {
        printf 'Content-Length: %d\r\n\r\n%s' "${#diag_uri_payload}" "$diag_uri_payload"
    } | $KLAR lsp 2>&1
)
if echo "$diag_uri_output" | grep -q '"id":15' && \
   echo "$diag_uri_output" | grep -q '"source":"checker"'; then
    pass "lsp command: percent-encoded file URI is decoded"
else
    fail "lsp command: percent-encoded URI decoding failed"
fi
rm -f "$diag_uri_fixture"

completion_fixture="/tmp/klar_lsp_completion_fixture.kl"
cat > "$completion_fixture" << 'EOF'
fn main() -> i32 {
    let number: i32 = 1
    let text: string = "x"
    return nu
}
EOF
completion_uri="file://${completion_fixture}"
completion_payload=$(printf '{"jsonrpc":"2.0","id":16,"method":"textDocument/completion","params":{"textDocument":{"uri":"%s"},"position":{"line":3,"character":11}}}' "$completion_uri")
completion_output=$(
    {
        printf 'Content-Length: %d\r\n\r\n%s' "${#completion_payload}" "$completion_payload"
    } | $KLAR lsp 2>&1
)
if echo "$completion_output" | grep -q '"id":16' && \
   echo "$completion_output" | grep -q '"isIncomplete":false' && \
   echo "$completion_output" | grep -q '"label":"number"' && \
   echo "$completion_output" | grep -q '"detail":"i32"' && \
   echo "$completion_output" | grep -q '"sortText":"0_number"' && \
   echo "$completion_output" | grep -q '"label":"text"' && \
   echo "$completion_output" | grep -q '"sortText":"1_text"'; then
    pass "lsp command: textDocument/completion returns scope/type-aware items"
else
    fail "lsp command: textDocument/completion handling failed"
fi
rm -f "$completion_fixture"

# Write results JSON
TOTAL=$((PASSED + FAILED))
cat > "$RESULTS_FILE" << EOF
{
  "passed": $PASSED,
  "failed": $FAILED,
  "total": $TOTAL,
  "failures": [$FAILURES]
}
EOF

echo ""
echo "All $PASSED args tests passed"

if [ $FAILED -gt 0 ]; then
    exit 1
fi
