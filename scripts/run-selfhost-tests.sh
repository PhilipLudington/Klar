#!/bin/bash
# Run self-hosted compiler stub tests
#
# Phase 1: Verify each selfhost/*.kl file passes klar check and inline tests
# Phase 2: Lexer parity testing — compares Zig vs selfhost dump-tokens output

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RESULTS_FILE="$SCRIPT_DIR/.selfhost-test-results.json"
KLAR="$SCRIPT_DIR/zig-out/bin/klar"
SELFHOST_DIR="$SCRIPT_DIR/selfhost"

# Ensure compiler is built
if [ ! -f "$KLAR" ]; then
    echo "Building Klar compiler first..."
    cd "$SCRIPT_DIR" && zig build || exit 1
fi

PASSED=0
FAILED=0
FAILURES=""

# Phase 1: Type-check all selfhost files
echo "Phase 1: Type checking selfhost/ stubs"
echo "────────────────────────────────────────"

for f in "$SELFHOST_DIR"/*.kl; do
    [ -f "$f" ] || continue
    name=$(basename "$f" .kl)

    check_output=$("$KLAR" check "$f" 2>&1)
    check_exit=$?

    if [ $check_exit -eq 0 ]; then
        echo "  ✓ $name (check passed)"
        PASSED=$((PASSED + 1))
    else
        echo "  ✗ $name (check failed)"
        echo "    $check_output" | head -3
        FAILED=$((FAILED + 1))
        [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
        FAILURES="$FAILURES\"$name: check failed\""
    fi
done

# Phase 1b: Run inline tests for standalone files (no imports)
echo ""
echo "Phase 1b: Running inline tests"
echo "────────────────────────────────────────"

for f in "$SELFHOST_DIR"/*.kl; do
    [ -f "$f" ] || continue
    # Skip files with imports — they need multi-file klar test support
    grep -q "^import " "$f" && continue
    name=$(basename "$f" .kl)

    test_output=$("$KLAR" test "$f" 2>&1)
    test_exit=$?

    if [ $test_exit -eq 0 ]; then
        echo "  ✓ $name (tests passed)"
        PASSED=$((PASSED + 1))
    else
        echo "  ✗ $name (tests failed)"
        echo "    $test_output" | head -3
        FAILED=$((FAILED + 1))
        [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
        FAILURES="$FAILURES\"$name: tests failed\""
    fi
done

# Phase 2: Lexer parity testing (dump-tokens)
# Compares Zig lexer JSON output with selfhost lexer JSON output.
# Pre-builds the selfhost lexer binary once, then reuses it for all test files.
echo ""
echo "Phase 2: Lexer parity testing (dump-tokens)"
echo "────────────────────────────────────────"

SELFHOST_LEXER="$SCRIPT_DIR/build/selfhost_lexer"
build_output=$("$KLAR" build "$SELFHOST_DIR/lexer.kl" -o "$SELFHOST_LEXER" 2>&1)
build_exit=$?
if [ $build_exit -ne 0 ]; then
    echo "  ✗ selfhost lexer build failed"
    echo "    $build_output" | head -3
    FAILED=$((FAILED + 1))
    [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
    FAILURES="$FAILURES\"selfhost lexer: build failed\""
else
    PARITY_FILES="test/native/arith.kl test/native/struct.kl test/native/trait_basic.kl test/native/generics_basic.kl test/native/string_primitives.kl test/native/closure_simple.kl test/native/enum_struct_payload.kl test/native/for_range.kl"
    for f in $PARITY_FILES; do
        [ -f "$SCRIPT_DIR/$f" ] || continue
        name=$(basename "$f" .kl)

        # Get Zig lexer output
        zig_tokens=$("$KLAR" dump-tokens "$SCRIPT_DIR/$f" 2>/dev/null)
        zig_exit=$?
        if [ $zig_exit -ne 0 ]; then
            echo "  ✗ $name (zig lexer failed)"
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: zig lexer failed\""
            continue
        fi

        # Get selfhost lexer output (using pre-built binary)
        selfhost_tokens=$("$SELFHOST_LEXER" "$SCRIPT_DIR/$f" 2>/dev/null)
        selfhost_exit=$?
        if [ $selfhost_exit -ne 0 ]; then
            echo "  ✗ $name (selfhost lexer failed)"
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: selfhost lexer failed\""
            continue
        fi

        # Compare token streams
        if [ "$zig_tokens" = "$selfhost_tokens" ]; then
            echo "  ✓ $name (token parity)"
            PASSED=$((PASSED + 1))
        else
            echo "  ✗ $name (token mismatch)"
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: token mismatch\""
            # Show first difference for debugging
            diff <(echo "$zig_tokens" | python3 -m json.tool 2>/dev/null || echo "$zig_tokens") \
                 <(echo "$selfhost_tokens" | python3 -m json.tool 2>/dev/null || echo "$selfhost_tokens") 2>/dev/null | head -10
        fi
    done
fi

TOTAL=$((PASSED + FAILED))
echo ""
echo "Selfhost tests: $PASSED/$TOTAL passed"

# Write results JSON
cat > "$RESULTS_FILE" <<ENDJSON
{
  "test_suite": "selfhost",
  "total": $TOTAL,
  "passed": $PASSED,
  "failed": $FAILED,
  "failures": [$FAILURES]
}
ENDJSON

if [ $FAILED -gt 0 ]; then
    exit 1
fi
