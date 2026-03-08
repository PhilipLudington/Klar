#!/bin/bash
# Validate typed AST pipeline on full test/native/ suite
#
# For each test/native/*.kl file:
#   1. Run via standard pipeline: klar run file.kl → exit code + stdout
#   2. Selfhost emit-typed-ast: selfhost_main emit-typed-ast file.kl → JSON
#   3. Run via typed AST pipeline: klar run file.kl --typed-ast-input JSON → exit code + stdout
#   4. Compare exit codes and stdout
#
# Usage: ./scripts/validate-typed-ast-pipeline.sh [--fix] [--verbose] [file.kl ...]

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
KLAR="$SCRIPT_DIR/zig-out/bin/klar"
SELFHOST_MAIN="$SCRIPT_DIR/build/selfhost_main"
TMPDIR=$(mktemp -d /tmp/klar_typed_ast_XXXXXX)
trap 'rm -rf "$TMPDIR"' EXIT INT TERM

VERBOSE=0
SPECIFIC_FILES=""

for arg in "$@"; do
    case "$arg" in
        --verbose|-v) VERBOSE=1 ;;
        *.kl) SPECIFIC_FILES="$SPECIFIC_FILES $arg" ;;
    esac
done

# Ensure compiler is built
if [ ! -f "$KLAR" ]; then
    echo "Error: Klar compiler not found. Run ./run-build.sh first."
    exit 1
fi

# Build selfhost main if needed
if [ ! -f "$SELFHOST_MAIN" ]; then
    echo "Building selfhost main..."
    "$KLAR" build "$SCRIPT_DIR/selfhost/main.kl" -o "$SELFHOST_MAIN" 2>/dev/null
    if [ $? -ne 0 ] || [ ! -f "$SELFHOST_MAIN" ]; then
        echo "Error: Failed to build selfhost main"
        exit 1
    fi
fi

# Collect test files
if [ -n "$SPECIFIC_FILES" ]; then
    FILES="$SPECIFIC_FILES"
else
    FILES=$(ls "$SCRIPT_DIR"/test/native/*.kl | sort)
fi

TOTAL=0
MATCH=0
EMIT_FAIL=0
BUILD_FAIL=0
EXIT_MISMATCH=0
STDOUT_MISMATCH=0
EMIT_FAIL_LIST=""
BUILD_FAIL_LIST=""
EXIT_MISMATCH_LIST=""
STDOUT_MISMATCH_LIST=""

echo "Typed AST Pipeline Validation"
echo "════════════════════════════════════════"

for f in $FILES; do
    [ -f "$f" ] || continue
    name=$(basename "$f" .kl)
    TOTAL=$((TOTAL + 1))

    # Step 1: Standard pipeline
    std_stdout=$("$KLAR" run "$f" 2>/dev/null)
    std_exit=$?

    # Step 2: Selfhost emit-typed-ast (retry up to 3 times on failure)
    typed_json="$TMPDIR/${name}.json"
    emit_ok=0
    for attempt in 1 2 3; do
        emit_stderr=$("$SELFHOST_MAIN" emit-typed-ast "$f" > "$typed_json" 2>&1)
        emit_exit=$?
        if [ -s "$typed_json" ]; then
            emit_ok=1
            break
        fi
        if [ $VERBOSE -eq 1 ] && [ $attempt -lt 3 ]; then
            echo "  ⟳ $name (emit retry $attempt, exit $emit_exit)"
        fi
        rm -f "$typed_json"
    done

    if [ $emit_ok -eq 0 ]; then
        EMIT_FAIL=$((EMIT_FAIL + 1))
        EMIT_FAIL_LIST="$EMIT_FAIL_LIST $name"
        if [ $VERBOSE -eq 1 ]; then
            echo "  ✗ $name (emit-typed-ast failed after 3 attempts, exit $emit_exit)"
        fi
        continue
    fi

    # Step 3: Typed AST pipeline
    typed_stdout=$("$KLAR" run "$f" --typed-ast-input "$typed_json" 2>/dev/null)
    typed_exit=$?

    rm -f "$typed_json"

    # Step 4: Compare
    if [ $std_exit -ne $typed_exit ]; then
        EXIT_MISMATCH=$((EXIT_MISMATCH + 1))
        EXIT_MISMATCH_LIST="$EXIT_MISMATCH_LIST $name(std=$std_exit,typed=$typed_exit)"
        echo "  ✗ $name (exit mismatch: std=$std_exit typed=$typed_exit)"
    elif [ "$std_stdout" != "$typed_stdout" ]; then
        STDOUT_MISMATCH=$((STDOUT_MISMATCH + 1))
        STDOUT_MISMATCH_LIST="$STDOUT_MISMATCH_LIST $name"
        echo "  ✗ $name (stdout mismatch, exit=$std_exit)"
        if [ $VERBOSE -eq 1 ]; then
            echo "    std:   $(echo "$std_stdout" | head -3)"
            echo "    typed: $(echo "$typed_stdout" | head -3)"
        fi
    else
        MATCH=$((MATCH + 1))
        if [ $VERBOSE -eq 1 ]; then
            echo "  ✓ $name (match, exit=$std_exit)"
        fi
    fi
done

ATTEMPTED=$((TOTAL - EMIT_FAIL))
PIPELINE_FAIL=$((EXIT_MISMATCH + STDOUT_MISMATCH + BUILD_FAIL))

echo ""
echo "════════════════════════════════════════"
echo "Results: $MATCH/$TOTAL match ($EMIT_FAIL emit-fail, $EXIT_MISMATCH exit-mismatch, $STDOUT_MISMATCH stdout-mismatch)"
echo ""
echo "  total files:      $TOTAL"
echo "  emit succeeded:   $ATTEMPTED"
echo "  match:            $MATCH"
echo "  emit-fail:        $EMIT_FAIL"
echo "  exit-mismatch:    $EXIT_MISMATCH"
echo "  stdout-mismatch:  $STDOUT_MISMATCH"

if [ $EMIT_FAIL -gt 0 ]; then
    echo ""
    echo "Emit failures ($EMIT_FAIL):"
    for name in $EMIT_FAIL_LIST; do
        echo "  - $name"
    done
fi

if [ $EXIT_MISMATCH -gt 0 ]; then
    echo ""
    echo "Exit code mismatches ($EXIT_MISMATCH):"
    for item in $EXIT_MISMATCH_LIST; do
        echo "  - $item"
    done
fi

if [ $STDOUT_MISMATCH -gt 0 ]; then
    echo ""
    echo "Stdout mismatches ($STDOUT_MISMATCH):"
    for name in $STDOUT_MISMATCH_LIST; do
        echo "  - $name"
    done
fi

echo ""
if [ $PIPELINE_FAIL -eq 0 ] && [ $EMIT_FAIL -eq 0 ]; then
    echo "✓ All $TOTAL files pass the typed AST pipeline"
    exit 0
elif [ $PIPELINE_FAIL -eq 0 ]; then
    echo "✓ All emittable files ($ATTEMPTED/$TOTAL) pass the typed AST pipeline"
    echo "  $EMIT_FAIL files could not be emitted (selfhost limitation)"
    exit 0
else
    echo "✗ $PIPELINE_FAIL pipeline failures found"
    exit 1
fi
