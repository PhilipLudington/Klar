#!/bin/bash
# Run self-hosted compiler stub tests
#
# Phase 1: Verify each selfhost/*.kl file passes klar check and inline tests
# Phase 2: Lexer parity testing — compares Zig vs selfhost dump-tokens output
# Phase 3a: Parser parse check — verify selfhost parser can parse test corpus
# Phase 3b: Parser AST parity — compare normalized AST output vs Zig dump-ast

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
PARITY_FILES="test/native/arith.kl test/native/struct.kl test/native/trait_basic.kl test/native/generics_basic.kl test/native/string_primitives.kl test/native/closure_simple.kl test/native/enum_struct_payload.kl test/native/for_range.kl test/native/optional_coalesce.kl test/native/ref_self_method.kl test/native/nested_struct_field.kl test/native/result_propagate.kl test/native/array_methods.kl test/native/for_array.kl test/native/return_types.kl test/native/array_sized.kl test/native/recursive_deep.kl test/native/comptime_const_access.kl"

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

# Phase 3: Parser parity testing (dump-ast)
# Phase 3a: Verify selfhost parser can parse each test corpus file (exit 0)
# Phase 3b: Compare normalized AST output with Zig dump-ast
echo ""
echo "Phase 3a: Parser parse check"
echo "────────────────────────────────────────"

SELFHOST_PARSER="$SCRIPT_DIR/build/selfhost_parser"
NORMALIZE_SCRIPT="$SCRIPT_DIR/scripts/normalize-ast.py"
build_output=$("$KLAR" build "$SELFHOST_DIR/parser_main.kl" -o "$SELFHOST_PARSER" 2>&1)
build_exit=$?
PARSED_FILES=""
if [ $build_exit -ne 0 ]; then
    echo "  ✗ selfhost parser build failed"
    echo "    $build_output" | head -3
    FAILED=$((FAILED + 1))
    [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
    FAILURES="$FAILURES\"selfhost parser: build failed\""
else
    for f in $PARITY_FILES; do
        [ -f "$SCRIPT_DIR/$f" ] || continue
        name=$(basename "$f" .kl)

        selfhost_ast=$("$SELFHOST_PARSER" "$SCRIPT_DIR/$f" 2>/dev/null)
        selfhost_exit=$?
        if [ $selfhost_exit -eq 0 ]; then
            echo "  ✓ $name (parse ok)"
            PASSED=$((PASSED + 1))
            PARSED_FILES="$PARSED_FILES $f"
        else
            echo "  ✗ $name (parse failed, exit $selfhost_exit)"
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: parser failed (exit $selfhost_exit)\""
        fi
    done
fi

echo ""
echo "Phase 3b: Parser AST parity (normalized)"
echo "────────────────────────────────────────"

if [ $build_exit -ne 0 ]; then
    echo "  (skipped — selfhost parser build failed)"
elif [ -z "$PARSED_FILES" ]; then
    echo "  (skipped — no files parsed successfully)"
else
    for f in $PARSED_FILES; do
        name=$(basename "$f" .kl)

        # Get Zig dump-ast output
        zig_ast=$("$KLAR" dump-ast "$SCRIPT_DIR/$f" 2>/dev/null)
        zig_exit=$?
        if [ $zig_exit -ne 0 ]; then
            echo "  ✗ $name (zig dump-ast failed)"
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: zig dump-ast failed\""
            continue
        fi

        # Get selfhost parser output
        selfhost_ast=$("$SELFHOST_PARSER" "$SCRIPT_DIR/$f" 2>/dev/null)
        selfhost_exit=$?
        if [ $selfhost_exit -ne 0 ]; then
            echo "  ✗ $name (selfhost parser crashed, exit $selfhost_exit)"
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: parser crashed (exit $selfhost_exit)\""
            continue
        fi

        # Compare with normalization
        norm_output=$(python3 "$NORMALIZE_SCRIPT" --str "$zig_ast" "$selfhost_ast" 2>&1)
        norm_exit=$?
        if [ $norm_exit -eq 0 ]; then
            echo "  ✓ $name (AST parity)"
            PASSED=$((PASSED + 1))
        elif [ $norm_exit -eq 2 ]; then
            echo "  ✗ $name (normalization error)"
            echo "    $norm_output" | head -5
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: normalization error\""
        else
            echo "  ✗ $name (AST mismatch)"
            echo "    $norm_output" | head -5
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: AST mismatch\""
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
