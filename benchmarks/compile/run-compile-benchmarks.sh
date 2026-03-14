#!/bin/bash
# Compilation performance benchmark suite.
# Measures wall-clock time for compiling different workloads.
# Outputs results to benchmarks/compile/results.json
set -e

KLAR="./zig-out/bin/klar"
RESULTS_FILE="benchmarks/compile/results.json"
RUNS=3

if [ ! -f "$KLAR" ]; then
    echo "Error: compiler not built. Run ./run-build.sh first."
    exit 1
fi

# Generate synthetic program if not present
if [ ! -f "benchmarks/compile/synthetic_10k.kl" ]; then
    ./benchmarks/compile/generate_synthetic.sh
fi

echo "═══════════════════════════════════════════════════════"
echo "Compilation Performance Benchmarks ($RUNS runs each)"
echo "═══════════════════════════════════════════════════════"
echo ""

# Helper: run a benchmark N times, report min/max/avg in milliseconds
run_bench() {
    local name="$1"
    shift
    local times=()

    for run in $(seq 1 $RUNS); do
        # Use bash TIMEFORMAT to get wall-clock seconds
        local start_ns=$(python3 -c "import time; print(int(time.time_ns()))")
        eval "$@" > /dev/null 2>&1
        local end_ns=$(python3 -c "import time; print(int(time.time_ns()))")
        local elapsed_ms=$(python3 -c "print(($end_ns - $start_ns) / 1_000_000)")
        times+=("$elapsed_ms")
    done

    # Compute min, max, avg
    local stats=$(python3 -c "
times = [${times[0]}, ${times[1]}, ${times[2]}]
mn = min(times)
mx = max(times)
avg = sum(times) / len(times)
variance_pct = ((mx - mn) / avg * 100) if avg > 0 else 0
print(f'{mn:.1f} {mx:.1f} {avg:.1f} {variance_pct:.1f}')
")
    local min_ms=$(echo "$stats" | awk '{print $1}')
    local max_ms=$(echo "$stats" | awk '{print $2}')
    local avg_ms=$(echo "$stats" | awk '{print $3}')
    local var_pct=$(echo "$stats" | awk '{print $4}')

    printf "  %-30s avg: %7s ms  (min: %s, max: %s, var: %s%%)\n" \
        "$name" "$avg_ms" "$min_ms" "$max_ms" "$var_pct"

    # Store for JSON output
    echo "$name|$avg_ms|$min_ms|$max_ms|$var_pct|${times[0]}|${times[1]}|${times[2]}" >> "$RESULTS_FILE.tmp"
}

rm -f "$RESULTS_FILE.tmp"

# Benchmark 1: Synthetic 10K-line program (klar check)
echo "Benchmark: Synthetic 10K-line program"
SYNTHETIC="benchmarks/compile/synthetic_10k.kl"
LINES=$(wc -l < "$SYNTHETIC" | tr -d ' ')
run_bench "check ($LINES lines)" "$KLAR check $SYNTHETIC"
echo ""

# Benchmark 2: Single large selfhost file (checker_main.kl is usually the biggest)
echo "Benchmark: Selfhost single file (checker_main.kl)"
if [ -f "selfhost/checker_main.kl" ]; then
    SELFHOST_LINES=$(wc -l < "selfhost/checker_main.kl" | tr -d ' ')
    run_bench "check checker_main ($SELFHOST_LINES lines)" "$KLAR check selfhost/checker_main.kl"
fi
echo ""

# Benchmark 3: Native test suite compilation (check all files)
echo "Benchmark: Native test suite (check all)"
NATIVE_COUNT=$(ls test/native/*.kl 2>/dev/null | wc -l | tr -d ' ')
bench_native_all() {
    for f in test/native/*.kl; do
        $KLAR check "$f" 2>/dev/null
    done
}
run_bench "check $NATIVE_COUNT native tests" "bench_native_all"
echo ""

# Benchmark 4: Synthetic 10K-line build (native compilation via LLVM)
echo "Benchmark: Synthetic 10K-line build (native)"
run_bench "build ($LINES lines)" "$KLAR build $SYNTHETIC -o /tmp/klar_bench_synthetic"
rm -f /tmp/klar_bench_synthetic /tmp/klar_bench_synthetic.dSYM 2>/dev/null
echo ""

# Write JSON results
echo "{" > "$RESULTS_FILE"
echo '  "timestamp": "'$(date -u +%Y-%m-%dT%H:%M:%SZ)'",' >> "$RESULTS_FILE"
echo '  "runs_per_benchmark": '$RUNS',' >> "$RESULTS_FILE"
echo '  "benchmarks": [' >> "$RESULTS_FILE"

first=true
while IFS='|' read -r name avg_ms min_ms max_ms var_pct t1 t2 t3; do
    if [ "$first" = true ]; then
        first=false
    else
        echo '    ,' >> "$RESULTS_FILE"
    fi
    cat >> "$RESULTS_FILE" << BENCH_JSON
    {
      "name": "$name",
      "avg_ms": $avg_ms,
      "min_ms": $min_ms,
      "max_ms": $max_ms,
      "variance_pct": $var_pct,
      "runs_ms": [$t1, $t2, $t3]
    }
BENCH_JSON
done < "$RESULTS_FILE.tmp"

echo '  ]' >> "$RESULTS_FILE"
echo "}" >> "$RESULTS_FILE"
rm -f "$RESULTS_FILE.tmp"

echo "═══════════════════════════════════════════════════════"
echo "Results written to $RESULTS_FILE"
echo "═══════════════════════════════════════════════════════"
