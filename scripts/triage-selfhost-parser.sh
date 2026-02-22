#!/bin/bash
# Triage all test/native/*.kl files against the selfhost parser.
#
# Categorizes each file into:
#   A = parse + AST parity pass (ready to add to PARITY_FILES)
#   B = parses OK but AST differs (needs normalization or parser fix)
#   C = parse error/crash (needs parser feature work)
#
# Usage: ./scripts/triage-selfhost-parser.sh [--verbose]

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
KLAR="$SCRIPT_DIR/zig-out/bin/klar"
SELFHOST_DIR="$SCRIPT_DIR/selfhost"
SELFHOST_PARSER="$SCRIPT_DIR/build/selfhost_parser"
NORMALIZE_SCRIPT="$SCRIPT_DIR/scripts/normalize-ast.py"
VERBOSE=false

if [ "$1" = "--verbose" ]; then
    VERBOSE=true
fi

# Already-passing files (extracted from PARITY_FILES in run-selfhost-tests.sh)
ALREADY_PASSING=$(grep 'test/native/.*\.kl' "$SCRIPT_DIR/scripts/run-selfhost-tests.sh" | sed 's|.*test/native/||' | sed 's|\.kl.*||' | tr '\n' ' ')

is_already_passing() {
    local name="$1"
    for p in $ALREADY_PASSING; do
        if [ "$name" = "$p" ]; then
            return 0
        fi
    done
    return 1
}

# Ensure compiler is built
if [ ! -f "$KLAR" ]; then
    echo "Building Klar compiler first..."
    cd "$SCRIPT_DIR" && zig build || exit 1
fi

# Build selfhost parser once
echo "Building selfhost parser..."
build_output=$("$KLAR" build "$SELFHOST_DIR/parser_main.kl" -o "$SELFHOST_PARSER" 2>&1)
build_exit=$?
if [ $build_exit -ne 0 ]; then
    echo "ERROR: selfhost parser build failed"
    echo "$build_output"
    exit 1
fi
echo "Selfhost parser built OK"
echo ""

# Collect results
BUCKET_A=""
BUCKET_B=""
BUCKET_C=""
COUNT_A=0
COUNT_B=0
COUNT_C=0
COUNT_SKIP=0

for f in "$SCRIPT_DIR"/test/native/*.kl; do
    [ -f "$f" ] || continue
    name=$(basename "$f" .kl)

    # Skip already-passing files
    if is_already_passing "$name"; then
        COUNT_SKIP=$((COUNT_SKIP + 1))
        continue
    fi

    # Try selfhost parser
    selfhost_ast=$("$SELFHOST_PARSER" "$f" 2>/dev/null)
    selfhost_exit=$?

    if [ $selfhost_exit -ne 0 ]; then
        # Bucket C: parse error/crash
        COUNT_C=$((COUNT_C + 1))
        BUCKET_C="$BUCKET_C $name"
        if $VERBOSE; then
            # Get first error line
            error_line=$("$SELFHOST_PARSER" "$f" 2>&1 | head -1)
            echo "  C $name — $error_line"
        fi
        continue
    fi

    # Parser succeeded — check AST parity
    zig_ast=$("$KLAR" dump-ast "$f" 2>/dev/null)
    zig_exit=$?
    if [ $zig_exit -ne 0 ]; then
        # Zig dump-ast failed — skip
        COUNT_C=$((COUNT_C + 1))
        BUCKET_C="$BUCKET_C $name"
        if $VERBOSE; then
            echo "  C $name — zig dump-ast failed"
        fi
        continue
    fi

    # Compare with normalization
    norm_output=$(python3 "$NORMALIZE_SCRIPT" --str "$zig_ast" "$selfhost_ast" 2>&1)
    norm_exit=$?

    if [ $norm_exit -eq 0 ]; then
        # Bucket A: full parity
        COUNT_A=$((COUNT_A + 1))
        BUCKET_A="$BUCKET_A $name"
        if $VERBOSE; then
            echo "  A $name"
        fi
    else
        # Bucket B: parses but AST differs
        COUNT_B=$((COUNT_B + 1))
        BUCKET_B="$BUCKET_B $name"
        if $VERBOSE; then
            echo "  B $name — $norm_output"
        fi
    fi
done

# Summary
echo "════════════════════════════════════════"
echo "Triage Summary"
echo "════════════════════════════════════════"
echo "Already passing: $COUNT_SKIP"
echo "Bucket A (ready to add):    $COUNT_A"
echo "Bucket B (AST differs):     $COUNT_B"
echo "Bucket C (parse error):     $COUNT_C"
echo ""

if [ $COUNT_A -gt 0 ]; then
    echo "── Bucket A (ready to add) ──"
    for name in $BUCKET_A; do
        echo "  test/native/$name.kl"
    done
    echo ""
fi

if [ $COUNT_B -gt 0 ]; then
    echo "── Bucket B (AST differs) ──"
    for name in $BUCKET_B; do
        echo "  test/native/$name.kl"
    done
    echo ""
fi

if [ $COUNT_C -gt 0 ]; then
    echo "── Bucket C (parse error) ──"
    for name in $BUCKET_C; do
        echo "  test/native/$name.kl"
    done
    echo ""
fi

# If --verbose, show sample diffs for Bucket B
if $VERBOSE && [ $COUNT_B -gt 0 ]; then
    echo "── Sample B diffs (first 5) ──"
    count=0
    for name in $BUCKET_B; do
        if [ $count -ge 5 ]; then break; fi
        f="$SCRIPT_DIR/test/native/$name.kl"
        echo ""
        echo "--- $name ---"
        zig_ast=$("$KLAR" dump-ast "$f" 2>/dev/null)
        selfhost_ast=$("$SELFHOST_PARSER" "$f" 2>/dev/null)
        python3 "$NORMALIZE_SCRIPT" --str "$zig_ast" "$selfhost_ast" 2>&1 | head -10
        count=$((count + 1))
    done
fi
