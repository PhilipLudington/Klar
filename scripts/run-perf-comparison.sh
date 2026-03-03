#!/usr/bin/env bash

# Performance comparison: Klar self-hosted frontend vs Zig frontend
# Milestone 9.11.5
#
# Measures three pipeline stages:
#   1. Lexer   (dump-tokens)
#   2. Parser  (dump-ast)
#   3. Checker (check)
#
# Each stage is timed on a batch of representative test files.
# Multiple iterations; reports best time for each.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$SCRIPT_DIR"

KLAR="$SCRIPT_DIR/zig-out/bin/klar"
BUILD_DIR="$SCRIPT_DIR/build"
SELFHOST_DIR="$SCRIPT_DIR/selfhost"

# Detect python
if command -v python3 &>/dev/null; then
    PYTHON=python3
else
    PYTHON=python
fi

# ─── Colors ──────────────────────────────────────────────────────────────────
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

# ─── Configuration ───────────────────────────────────────────────────────────
ITERATIONS=5           # Iterations per stage; reports best
WARMUP_ITERATIONS=2    # Warmup iterations (not counted)

# Representative test files — mix of small, medium, and large
PERF_FILES="
test/native/arith.kl
test/native/hello.kl
test/native/variables.kl
test/native/if_else.kl
test/native/while_loop.kl
test/native/for_range.kl
test/native/closures.kl
test/native/struct_basic.kl
test/native/enum_basic.kl
test/native/match_basic.kl
test/native/generics_basic.kl
test/native/generics_multiple.kl
test/native/generics_where_clause.kl
test/native/trait_basic.kl
test/native/trait_default_method.kl
test/native/trait_inheritance.kl
test/native/optional_basic.kl
test/native/result_basic.kl
test/native/result_context.kl
test/native/list_basic.kl
test/native/map_basic.kl
test/native/set_basic.kl
test/native/set_operations.kl
test/native/string_primitives.kl
test/native/string_slice.kl
test/native/array_slice_param.kl
test/native/unsigned_arithmetic.kl
test/native/saturating_arithmetic.kl
test/native/wrapping_arithmetic.kl
test/native/tuple.kl
test/native/rc_basic.kl
test/native/arc_basic.kl
test/native/import_basic.kl
test/native/fn_generic.kl
test/native/fn_generic_multi.kl
test/native/fn_generic_bounds.kl
test/native/enum_generic.kl
test/native/for_slice.kl
test/native/list_zip.kl
test/native/meta_pure_pass.kl
"

# ─── Ensure compiler is built ────────────────────────────────────────────────
if [ ! -f "$KLAR" ]; then
    echo "Building Klar compiler..."
    ./run-build.sh > /dev/null 2>&1
fi

mkdir -p "$BUILD_DIR"

# ─── Filter to files that exist ──────────────────────────────────────────────
VALID_FILES=""
TOTAL_BYTES=0
FILE_COUNT=0
for f in $PERF_FILES; do
    if [ -f "$SCRIPT_DIR/$f" ]; then
        VALID_FILES="$VALID_FILES $SCRIPT_DIR/$f"
        sz=$(wc -c < "$SCRIPT_DIR/$f")
        TOTAL_BYTES=$((TOTAL_BYTES + sz))
        FILE_COUNT=$((FILE_COUNT + 1))
    fi
done

if [ $FILE_COUNT -eq 0 ]; then
    echo "Error: no valid test files found"
    exit 1
fi

TOTAL_KB=$($PYTHON -c "print(f'{$TOTAL_BYTES / 1024:.1f}')")

echo ""
echo "╔════════════════════════════════════════════════════════════════╗"
echo "║     Performance Comparison: Klar Frontend vs Zig Frontend     ║"
echo "╚════════════════════════════════════════════════════════════════╝"
echo ""
echo -e "  ${CYAN}Files:${NC}       $FILE_COUNT test files ($TOTAL_KB KB total)"
echo -e "  ${CYAN}Iterations:${NC}  $ITERATIONS (+ $WARMUP_ITERATIONS warmup)"
echo ""

# ─── Build selfhost binaries ─────────────────────────────────────────────────
echo -e "${BOLD}Building selfhost binaries...${NC}"

SELFHOST_MAIN="$BUILD_DIR/selfhost_main"
echo -n "  selfhost/main.kl → $SELFHOST_MAIN ... "
build_out=$("$KLAR" build "$SELFHOST_DIR/main.kl" -o "$SELFHOST_MAIN" 2>&1)
if [ $? -ne 0 ]; then
    echo -e "${RED}FAILED${NC}"
    echo "    $build_out"
    exit 1
fi
echo -e "${GREEN}OK${NC}"

echo ""

# ─── Timing helpers ──────────────────────────────────────────────────────────
# time_batch CMD [ARGS_PREFIX]
# Runs CMD ARGS_PREFIX <file> for each file, times the whole batch.
# Returns elapsed time in seconds (float).
time_batch() {
    local cmd="$1"
    shift
    local start end
    start=$($PYTHON -c 'import time; print(time.time())')
    for f in $VALID_FILES; do
        "$cmd" "$@" "$f" > /dev/null 2>&1 || true
    done
    end=$($PYTHON -c 'import time; print(time.time())')
    $PYTHON -c "print(f'{$end - $start:.4f}')"
}

# run_stage LABEL ZIG_CMD ZIG_ARGS SELFHOST_CMD SELFHOST_ARGS
run_stage() {
    local label="$1"
    local zig_cmd="$2"
    local zig_args="$3"
    local sh_cmd="$4"
    local sh_args="$5"

    echo -e "${BOLD}Stage: $label${NC}"
    echo "────────────────────────────────────────────────────────────────"

    # Warmup
    echo -n "  Warming up..."
    for i in $(seq 1 $WARMUP_ITERATIONS); do
        time_batch "$zig_cmd" $zig_args > /dev/null
        time_batch "$sh_cmd" $sh_args > /dev/null
    done
    echo " done"

    # Timed runs
    local best_zig=999999
    local best_sh=999999

    for i in $(seq 1 $ITERATIONS); do
        local zig_t=$(time_batch "$zig_cmd" $zig_args)
        local sh_t=$(time_batch "$sh_cmd" $sh_args)

        best_zig=$($PYTHON -c "print(min($best_zig, $zig_t))")
        best_sh=$($PYTHON -c "print(min($best_sh, $sh_t))")

        printf "  Run %d/%d: Zig %ss  Klar %ss\n" "$i" "$ITERATIONS" "$zig_t" "$sh_t"
    done

    # Results
    local ratio=$($PYTHON -c "print(f'{$best_sh / $best_zig:.2f}')")
    local zig_per_file=$($PYTHON -c "print(f'{$best_zig / $FILE_COUNT * 1000:.2f}')")
    local sh_per_file=$($PYTHON -c "print(f'{$best_sh / $FILE_COUNT * 1000:.2f}')")
    local zig_throughput=$($PYTHON -c "print(f'{$TOTAL_BYTES / $best_zig / 1024:.0f}')")
    local sh_throughput=$($PYTHON -c "print(f'{$TOTAL_BYTES / $best_sh / 1024:.0f}')")

    echo ""
    printf "  ${CYAN}%-18s${NC} │ %12s │ %12s\n" "" "Zig" "Klar"
    echo "  ──────────────────┼──────────────┼──────────────"
    printf "  %-18s │ %10ss │ %10ss\n" "Best batch time" "$best_zig" "$best_sh"
    printf "  %-18s │ %9sms │ %9sms\n" "Per file" "$zig_per_file" "$sh_per_file"
    printf "  %-18s │ %8s KB/s │ %8s KB/s\n" "Throughput" "$zig_throughput" "$sh_throughput"

    if $PYTHON -c "exit(0 if $best_sh <= $best_zig else 1)" 2>/dev/null; then
        echo -e "  Ratio:             ${GREEN}${ratio}x (Klar is faster or equal)${NC}"
    else
        echo -e "  Ratio:             ${YELLOW}${ratio}x (Klar is slower)${NC}"
    fi
    echo ""

    # Return values for JSON
    echo "$best_zig $best_sh $ratio" > /tmp/_perf_stage_result
}

# ─── Stage 1: Lexer ─────────────────────────────────────────────────────────
run_stage "Lexer (dump-tokens)" \
    "$KLAR" "dump-tokens" \
    "$SELFHOST_MAIN" "dump-tokens"
LEXER_RESULT=$(cat /tmp/_perf_stage_result)

# ─── Stage 2: Parser ─────────────────────────────────────────────────────────
run_stage "Parser (dump-ast)" \
    "$KLAR" "dump-ast" \
    "$SELFHOST_MAIN" "dump-ast"
PARSER_RESULT=$(cat /tmp/_perf_stage_result)

# ─── Stage 3: Full Frontend (check) ──────────────────────────────────────────
run_stage "Full Frontend (check)" \
    "$KLAR" "check" \
    "$SELFHOST_MAIN" "check"
CHECKER_RESULT=$(cat /tmp/_perf_stage_result)

# ─── Summary ─────────────────────────────────────────────────────────────────
echo "╔════════════════════════════════════════════════════════════════╗"
echo "║                         Summary                               ║"
echo "╚════════════════════════════════════════════════════════════════╝"
echo ""
printf "  ${CYAN}%-22s${NC} │ %10s │ %10s │ %8s\n" "Stage" "Zig (s)" "Klar (s)" "Ratio"
echo "  ────────────────────────┼────────────┼────────────┼──────────"

for stage_data in "Lexer:$LEXER_RESULT" "Parser:$PARSER_RESULT" "Full Frontend:$CHECKER_RESULT"; do
    stage_name="${stage_data%%:*}"
    stage_vals="${stage_data#*:}"
    zig_t=$(echo "$stage_vals" | cut -d' ' -f1)
    sh_t=$(echo "$stage_vals" | cut -d' ' -f2)
    ratio=$(echo "$stage_vals" | cut -d' ' -f3)
    printf "  %-22s │ %10s │ %10s │ %7sx\n" "$stage_name" "$zig_t" "$sh_t" "$ratio"
done

echo "  ────────────────────────┴────────────┴────────────┴──────────"
echo ""
echo -e "  ${CYAN}Ratio = Klar time / Zig time${NC}"
echo "  < 1.0 = Klar faster, > 1.0 = Zig faster"
echo ""

# ─── JSON output ─────────────────────────────────────────────────────────────
LEXER_ZIG=$(echo "$LEXER_RESULT" | cut -d' ' -f1)
LEXER_KLAR=$(echo "$LEXER_RESULT" | cut -d' ' -f2)
LEXER_RATIO=$(echo "$LEXER_RESULT" | cut -d' ' -f3)
PARSER_ZIG=$(echo "$PARSER_RESULT" | cut -d' ' -f1)
PARSER_KLAR=$(echo "$PARSER_RESULT" | cut -d' ' -f2)
PARSER_RATIO=$(echo "$PARSER_RESULT" | cut -d' ' -f3)
CHECKER_ZIG=$(echo "$CHECKER_RESULT" | cut -d' ' -f1)
CHECKER_KLAR=$(echo "$CHECKER_RESULT" | cut -d' ' -f2)
CHECKER_RATIO=$(echo "$CHECKER_RESULT" | cut -d' ' -f3)

RESULTS_FILE="$SCRIPT_DIR/.perf-comparison-results.json"
$PYTHON -c "
import json, sys
data = {
    'file_count': $FILE_COUNT,
    'total_bytes': $TOTAL_BYTES,
    'iterations': $ITERATIONS,
    'stages': {
        'lexer': {'zig_time': $LEXER_ZIG, 'klar_time': $LEXER_KLAR, 'ratio': $LEXER_RATIO},
        'parser': {'zig_time': $PARSER_ZIG, 'klar_time': $PARSER_KLAR, 'ratio': $PARSER_RATIO},
        'checker': {'zig_time': $CHECKER_ZIG, 'klar_time': $CHECKER_KLAR, 'ratio': $CHECKER_RATIO}
    }
}
json.dump(data, sys.stdout, indent=2)
print()
" > "$RESULTS_FILE"

echo "Results saved to $RESULTS_FILE"
echo ""

# Cleanup
rm -f /tmp/_perf_stage_result

echo "Done!"
