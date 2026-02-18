#!/bin/bash
# Run self-hosted compiler stub tests
#
# Phase 1: Verify each selfhost/*.kl file passes klar check and inline tests
# Phase 2 (future): Parity testing with dump-tokens/dump-ast

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

# Phase 2 (future): Parity testing
# When the selfhost lexer/parser are functional (milestones 9.4+),
# uncomment and extend this section.
#
# echo ""
# echo "Phase 2: Parity testing (dump-tokens / dump-ast)"
# echo "────────────────────────────────────────"
#
# PARITY_FILES="test/native/arith.kl test/native/struct_basic.kl test/native/trait_basic.kl"
# for f in $PARITY_FILES; do
#     [ -f "$SCRIPT_DIR/$f" ] || continue
#     name=$(basename "$f" .kl)
#
#     # Compare Zig lexer output vs selfhost lexer output
#     zig_tokens=$("$KLAR" dump-tokens "$SCRIPT_DIR/$f" 2>/dev/null)
#     # selfhost_tokens=$("$KLAR" run "$SELFHOST_DIR/main.kl" -- dump-tokens "$SCRIPT_DIR/$f" 2>/dev/null)
#     # if [ "$zig_tokens" = "$selfhost_tokens" ]; then
#     #     echo "  ✓ $name (token parity)"
#     #     PASSED=$((PASSED + 1))
#     # else
#     #     echo "  ✗ $name (token mismatch)"
#     #     FAILED=$((FAILED + 1))
#     # fi
# done

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
