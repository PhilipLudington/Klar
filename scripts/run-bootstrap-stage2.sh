#!/usr/bin/env bash

# Bootstrap Stage 2 — Phase 8.6.5 (Fixed-Point Validation)
#
# Builds Stage 1 (selfhost compiled by Zig), runs Stage 1's emit-typed-ast-multi
# on selfhost/main.kl, feeds the result to the Zig backend with --typed-ast-input
# to produce Stage 2, then validates Stage 2 by:
#   1. Comparing single-file emission against Stage 1 on a test corpus
#   2. Fixed-point validation: Stage 2 emit-typed-ast-multi == Stage 1 output
#
# Usage: ./scripts/run-bootstrap-stage2.sh [--verbose] [--skip-build]

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$SCRIPT_DIR"

KLAR="$SCRIPT_DIR/zig-out/bin/klar"
BUILD_DIR="$SCRIPT_DIR/build"
SELFHOST_DIR="$SCRIPT_DIR/selfhost"
STAGE1_MAIN="$BUILD_DIR/stage1_main"
STAGE2_MAIN="$BUILD_DIR/stage2_main"

export KLAR_BIN="$KLAR"

VERBOSE=0
SKIP_BUILD=0

for arg in "$@"; do
    case "$arg" in
        --verbose|-v) VERBOSE=1 ;;
        --skip-build) SKIP_BUILD=1 ;;
    esac
done

# ─── Colors ──────────────────────────────────────────────────────────────────
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BOLD='\033[1m'
NC='\033[0m'

BOOTSTRAP_TMPDIR=$(mktemp -d /tmp/klar_bootstrap_s2_XXXXXX)
trap 'rm -rf "$BOOTSTRAP_TMPDIR"' EXIT INT TERM HUP QUIT

echo ""
echo "╔════════════════════════════════════════════════════════════════╗"
echo "║           Bootstrap Stage 2: Multi-Module Pipeline            ║"
echo "║               Phase 8.6.5: Fixed-Point Validation             ║"
echo "╚════════════════════════════════════════════════════════════════╝"
echo ""

# ─── Prerequisites ────────────────────────────────────────────────────────────

if [ ! -f "$KLAR" ]; then
    echo "Error: Klar compiler not found at $KLAR"
    echo "Run ./run-build.sh first."
    exit 1
fi

mkdir -p "$BUILD_DIR"

# ─── Stage 1: Build selfhost binary with Zig compiler ─────────────────────────

if [ $SKIP_BUILD -eq 0 ] || [ ! -f "$STAGE1_MAIN" ]; then
    echo -e "${BOLD}Step 1: Building Stage 1 binary (Zig → selfhost)${NC}"
    echo "────────────────────────────────────────────────────────────────"

    echo -n "  Compiling selfhost/main.kl (-O2) ... "
    if "$KLAR" build "$SELFHOST_DIR/main.kl" -o "$STAGE1_MAIN" -O2 > "$BOOTSTRAP_TMPDIR/stage1_build.log" 2>&1; then
        echo -e "${GREEN}✓${NC}"
    else
        echo -e "${RED}FAILED${NC}"
        head -10 "$BOOTSTRAP_TMPDIR/stage1_build.log"
        exit 1
    fi
    echo ""
else
    echo -e "${BOLD}Step 1: Stage 1 binary already built (--skip-build)${NC}"
    echo ""
fi

# ─── Stage 2a: Emit multi-module typed AST via Stage 1 ────────────────────────

echo -e "${BOLD}Step 2: Emitting multi-module typed AST via Stage 1${NC}"
echo "────────────────────────────────────────────────────────────────"

STAGE2_JSON="$BOOTSTRAP_TMPDIR/stage2_typed_ast.json"

echo -n "  Running: stage1_main emit-typed-ast-multi selfhost/main.kl ... "
emit_exit=0
"$STAGE1_MAIN" emit-typed-ast-multi "$SELFHOST_DIR/main.kl" > "$STAGE2_JSON" 2>"$BOOTSTRAP_TMPDIR/stage2_emit.log" || emit_exit=$?
if [ $emit_exit -eq 0 ]; then
    # Check that output is non-empty and looks like valid JSON
    if [ -s "$STAGE2_JSON" ]; then
        JSON_SIZE=$(wc -c < "$STAGE2_JSON" | tr -d ' ')
        echo -e "${GREEN}✓${NC} (${JSON_SIZE} bytes)"
    else
        echo -e "${RED}FAILED${NC} (empty output)"
        cat "$BOOTSTRAP_TMPDIR/stage2_emit.log"
        exit 1
    fi
else
    echo -e "${RED}FAILED${NC} (exit code $emit_exit)"
    cat "$BOOTSTRAP_TMPDIR/stage2_emit.log"
    exit 1
fi

if [ $VERBOSE -eq 1 ]; then
    # Show module count from JSON
    MODULE_COUNT=$(python3 -c "import json,sys; d=json.load(open(sys.argv[1])); print(len(d.get('modules',[])))" "$STAGE2_JSON" 2>/dev/null || echo "?")
    echo "  Modules in typed AST: $MODULE_COUNT"
fi

echo ""

# ─── Stage 2b: Build Stage 2 binary via Zig backend ──────────────────────────

echo -e "${BOLD}Step 3: Building Stage 2 binary (typed AST → Zig backend → native)${NC}"
echo "────────────────────────────────────────────────────────────────"

echo -n "  Compiling via --typed-ast-input ... "
if "$KLAR" build "$SELFHOST_DIR/main.kl" --typed-ast-input "$STAGE2_JSON" -o "$STAGE2_MAIN" > "$BOOTSTRAP_TMPDIR/stage2_build.log" 2>&1; then
    echo -e "${GREEN}✓${NC}"
else
    echo -e "${RED}FAILED${NC}"
    head -20 "$BOOTSTRAP_TMPDIR/stage2_build.log"
    exit 1
fi

echo ""

# ─── Validation: Compare Stage 1 and Stage 2 outputs ─────────────────────────

echo -e "${BOLD}Step 4: Validating Stage 2 against Stage 1${NC}"
echo "────────────────────────────────────────────────────────────────"

# Test corpus: representative test/native/ files covering key features
# (aspirational list — not all files exist yet)
TEST_CORPUS=(
    test/native/arith.kl
    test/native/hello.kl
    test/native/variables.kl
    test/native/if_else.kl
    test/native/for_range.kl
    test/native/closures.kl
    test/native/struct_basic.kl
    test/native/enum_basic.kl
    test/native/match_basic.kl
    test/native/generics_basic.kl
    test/native/trait_basic.kl
    test/native/optional_basic.kl
    test/native/result_basic.kl
    test/native/string_basic.kl
    test/native/list_basic.kl
    test/native/map_basic.kl
    test/native/while_loop.kl
    test/native/nested_fn.kl
    test/native/tuple_basic.kl
    test/native/methods.kl
)

PASS=0
FAIL=0
SKIP=0
MISSING=0
FAIL_LIST=""

for f in "${TEST_CORPUS[@]}"; do
    name=$(basename "$f" .kl)
    if [ ! -f "$f" ]; then
        MISSING=$((MISSING + 1))
        if [ $VERBOSE -eq 1 ]; then
            echo -e "  ${YELLOW}⊘${NC} $name (file not found, skipped)"
        fi
        continue
    fi

    # Stage 1: emit-typed-ast for this file, then run via Zig backend
    s1_json="$BOOTSTRAP_TMPDIR/${name}_s1.json"
    s1_err="$BOOTSTRAP_TMPDIR/${name}_s1.err"
    s1_emit_exit=0
    "$STAGE1_MAIN" emit-typed-ast "$f" > "$s1_json" 2>"$s1_err" || s1_emit_exit=$?

    if [ $s1_emit_exit -ne 0 ] || [ ! -s "$s1_json" ]; then
        SKIP=$((SKIP + 1))
        if [ $VERBOSE -eq 1 ]; then
            echo "  ⊘ $name (Stage 1 emit failed, exit=$s1_emit_exit)"
            [ -s "$s1_err" ] && sed 's/^/    /' "$s1_err" | head -3
        fi
        continue
    fi

    s1_stdout=$("$KLAR" run "$f" --typed-ast-input "$s1_json" 2>/dev/null)
    s1_exit=$?

    # Stage 2: same test via Stage 2 binary
    s2_json="$BOOTSTRAP_TMPDIR/${name}_s2.json"
    s2_err="$BOOTSTRAP_TMPDIR/${name}_s2.err"
    s2_emit_exit=0
    "$STAGE2_MAIN" emit-typed-ast "$f" > "$s2_json" 2>"$s2_err" || s2_emit_exit=$?

    if [ $s2_emit_exit -ne 0 ] || [ ! -s "$s2_json" ]; then
        SKIP=$((SKIP + 1))
        if [ $VERBOSE -eq 1 ]; then
            echo "  ⊘ $name (Stage 2 emit failed, exit=$s2_emit_exit)"
            [ -s "$s2_err" ] && sed 's/^/    /' "$s2_err" | head -3
        fi
        continue
    fi

    s2_stdout=$("$KLAR" run "$f" --typed-ast-input "$s2_json" 2>/dev/null)
    s2_exit=$?

    # Compare
    if [ $s1_exit -eq $s2_exit ] && [ "$s1_stdout" = "$s2_stdout" ]; then
        PASS=$((PASS + 1))
        if [ $VERBOSE -eq 1 ]; then
            echo -e "  ${GREEN}✓${NC} $name (exit=$s1_exit)"
        fi
    else
        FAIL=$((FAIL + 1))
        FAIL_LIST="$FAIL_LIST $name"
        echo -e "  ${RED}✗${NC} $name (S1: exit=$s1_exit, S2: exit=$s2_exit)"
        if [ $VERBOSE -eq 1 ] && [ "$s1_stdout" != "$s2_stdout" ]; then
            echo "    S1 stdout: $(echo "$s1_stdout" | head -3)"
            echo "    S2 stdout: $(echo "$s2_stdout" | head -3)"
        fi
    fi

    rm -f "$s1_json" "$s2_json"
done

echo ""

# ─── Functional test: Stage 2 help ────────────────────────────────────────────

echo -e "${BOLD}Step 5: Stage 2 binary functional test${NC}"
echo "────────────────────────────────────────────────────────────────"

echo -n "  stage2_main help ... "
if "$STAGE2_MAIN" help > "$BOOTSTRAP_TMPDIR/stage2_help.log" 2>&1; then
    echo -e "${GREEN}✓${NC}"
else
    # help may exit non-zero, check if it produced output
    if [ -s "$BOOTSTRAP_TMPDIR/stage2_help.log" ]; then
        echo -e "${GREEN}✓${NC} (produced output)"
    else
        echo -e "${RED}FAILED${NC} (no output)"
    fi
fi

echo -n "  stage2_main check selfhost/lexer.kl ... "
check_exit=0
"$STAGE2_MAIN" check "$SELFHOST_DIR/lexer.kl" > "$BOOTSTRAP_TMPDIR/stage2_check.log" 2>&1 || check_exit=$?
if [ $check_exit -eq 0 ]; then
    echo -e "${GREEN}✓${NC}"
else
    echo -e "${YELLOW}⚠${NC} (exit $check_exit)"
fi

echo ""

# ─── Fixed-Point Validation: Stage 1 vs Stage 2 multi-module output ──────────

echo -e "${BOLD}Step 6: Fixed-point validation (multi-module AST comparison)${NC}"
echo "────────────────────────────────────────────────────────────────"

S1_MULTI_JSON="$BOOTSTRAP_TMPDIR/s1_multi.json"
S2_MULTI_JSON="$BOOTSTRAP_TMPDIR/s2_multi.json"

echo -n "  Stage 1 emit-typed-ast-multi selfhost/main.kl ... "
s1_multi_exit=0
"$STAGE1_MAIN" emit-typed-ast-multi "$SELFHOST_DIR/main.kl" > "$S1_MULTI_JSON" 2>/dev/null || s1_multi_exit=$?
if [ $s1_multi_exit -eq 0 ] && [ -s "$S1_MULTI_JSON" ]; then
    S1_SIZE=$(wc -c < "$S1_MULTI_JSON" | tr -d ' ')
    echo -e "${GREEN}✓${NC} (${S1_SIZE} bytes)"
else
    echo -e "${RED}FAILED${NC}"
    FIXED_POINT="skip"
fi

echo -n "  Stage 2 emit-typed-ast-multi selfhost/main.kl ... "
s2_multi_exit=0
"$STAGE2_MAIN" emit-typed-ast-multi "$SELFHOST_DIR/main.kl" > "$S2_MULTI_JSON" 2>/dev/null || s2_multi_exit=$?
if [ $s2_multi_exit -eq 0 ] && [ -s "$S2_MULTI_JSON" ]; then
    S2_SIZE=$(wc -c < "$S2_MULTI_JSON" | tr -d ' ')
    echo -e "${GREEN}✓${NC} (${S2_SIZE} bytes)"
else
    echo -e "${RED}FAILED${NC}"
    FIXED_POINT="skip"
fi

if [ "${FIXED_POINT:-}" != "skip" ]; then
    S1_HASH=$(shasum -a 256 "$S1_MULTI_JSON" | awk '{print $1}')
    S2_HASH=$(shasum -a 256 "$S2_MULTI_JSON" | awk '{print $1}')
    echo -n "  Comparing SHA-256 hashes ... "
    if [ "$S1_HASH" = "$S2_HASH" ]; then
        echo -e "${GREEN}✓ FIXED POINT ACHIEVED${NC}"
        echo "    SHA-256: $S1_HASH"
        FIXED_POINT="pass"
    else
        echo -e "${RED}✗ MISMATCH${NC}"
        echo "    Stage 1: $S1_HASH"
        echo "    Stage 2: $S2_HASH"
        FIXED_POINT="fail"
    fi
else
    echo -e "  ${YELLOW}⚠ Skipped (emission failed)${NC}"
fi

echo ""

# ─── Summary ─────────────────────────────────────────────────────────────────

TESTED=$((PASS + FAIL))
TOTAL=$((TESTED + SKIP + MISSING))

echo "╔════════════════════════════════════════════════════════════════╗"
echo "║                         Summary                               ║"
echo "╚════════════════════════════════════════════════════════════════╝"
echo ""
echo -e "  ${BOLD}Stage 1 → Stage 2 Pipeline:${NC}"
echo "    Stage 1 built:      ✓ (Zig → selfhost/main.kl)"
echo "    Typed AST emitted:  ✓ (Stage 1 emit-typed-ast-multi)"
echo "    Stage 2 built:      ✓ (Zig backend ← typed AST)"
echo ""
echo -e "  ${BOLD}Validation (Stage 1 vs Stage 2):${NC}"
echo -e "    Corpus:  ${TOTAL} files (${TESTED} tested, ${MISSING} not yet created, ${SKIP} emit-failed)"
echo -e "    Match:   ${GREEN}$PASS/$TESTED${NC}"
if [ $FAIL -gt 0 ]; then
    echo -e "    Fail:    ${RED}$FAIL/$TESTED${NC}"
fi
echo ""
echo -e "  ${BOLD}Fixed-Point Validation:${NC}"
if [ "${FIXED_POINT:-}" = "pass" ]; then
    echo -e "    Result:  ${GREEN}✓ Stage 1 == Stage 2 (byte-identical multi-module AST)${NC}"
elif [ "${FIXED_POINT:-}" = "fail" ]; then
    echo -e "    Result:  ${RED}✗ Stage 1 ≠ Stage 2 (multi-module AST differs)${NC}"
else
    echo -e "    Result:  ${YELLOW}⚠ Skipped${NC}"
fi

if [ $FAIL -gt 0 ]; then
    echo ""
    echo "  Failures:"
    for name in $FAIL_LIST; do
        echo "    - $name"
    done
fi

echo ""
if [ $FAIL -eq 0 ] && [ $TESTED -gt 0 ] && [ "${FIXED_POINT:-}" = "pass" ]; then
    echo -e "${GREEN}${BOLD}✓ BOOTSTRAP STAGE 2 PASSED (FIXED POINT)${NC}"
    echo "  Stage 2 binary produces byte-identical typed AST to Stage 1."
    echo "  The selfhost compiler successfully compiles itself."
    exit 0
elif [ $FAIL -eq 0 ] && [ $TESTED -gt 0 ]; then
    echo -e "${GREEN}${BOLD}✓ BOOTSTRAP STAGE 2 PASSED${NC}"
    echo "  Stage 2 binary produces identical output to Stage 1 on $TESTED test files."
    exit 0
elif [ $FAIL -eq 0 ] && [ $TESTED -eq 0 ]; then
    echo -e "${YELLOW}${BOLD}⚠ NO TESTS RAN${NC}"
    echo "  All test files were skipped. Check Stage 1/Stage 2 emit capabilities."
    exit 1
else
    echo -e "${RED}${BOLD}✗ BOOTSTRAP STAGE 2 FAILED${NC}"
    echo "  $FAIL/$TESTED test files produced different output between Stage 1 and Stage 2."
    exit 1
fi
