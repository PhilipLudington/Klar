#!/bin/bash
# Runtime performance benchmark suite.
# Compares native (LLVM -O2) vs VM (bytecode) execution times.
# Outputs results to benchmarks/runtime/results.json
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$SCRIPT_DIR"

KLAR="./zig-out/bin/klar"
RESULTS_FILE="benchmarks/runtime/results.json"
TMP_DIR="/tmp/klar_runtime_benchmarks"
NATIVE_RUNS=3
VM_RUNS=1

if [ ! -f "$KLAR" ]; then
    echo "Error: compiler not built. Run ./run-build.sh first."
    exit 1
fi

mkdir -p "$TMP_DIR"

echo "═══════════════════════════════════════════════════════════════"
echo "Runtime Performance Benchmarks (native: ${NATIVE_RUNS} runs, VM: ${VM_RUNS} run)"
echo "═══════════════════════════════════════════════════════════════"
echo ""

# Benchmarks: name|file|vm_supported
BENCHMARKS=(
    "fib40|benchmarks/runtime/fib.kl|yes"
    "quicksort_100k|benchmarks/runtime/quicksort.kl|no"
    "string_10k|benchmarks/runtime/string_processing.kl|no"
)

# Helper: time a command, return seconds and exit code
time_command() {
    local start_ns end_ns elapsed_ms
    start_ns=$(python3 -c "import time; print(int(time.time_ns()))")
    "$@" > /dev/null 2>&1
    local exit_code=$?
    end_ns=$(python3 -c "import time; print(int(time.time_ns()))")
    elapsed_ms=$(python3 -c "print(f'{($end_ns - $start_ns) / 1_000_000:.1f}')")
    echo "$elapsed_ms $exit_code"
}

# Helper: compute stats from an array of values
compute_stats() {
    local csv
    csv=$(IFS=,; echo "$*")
    python3 -c "
vals = [$csv]
mn = min(vals)
mx = max(vals)
avg = sum(vals) / len(vals)
var_pct = ((mx - mn) / avg * 100) if avg > 0 else 0
print(f'{mn:.1f} {mx:.1f} {avg:.1f} {var_pct:.1f}')
"
}

printf "%-20s │ %12s │ %12s │ %10s │ %s\n" \
    "Benchmark" "Native (ms)" "VM (ms)" "Speedup" "Status"
echo "─────────────────────┼──────────────┼──────────────┼────────────┼──────────"

# Collect JSON entries
json_entries=()

for bench_entry in "${BENCHMARKS[@]}"; do
    IFS='|' read -r name file vm_supported <<< "$bench_entry"

    # Compile to native with -O2
    native_binary="$TMP_DIR/$name"
    if ! $KLAR build "$file" -o "$native_binary" -O2 > /dev/null 2>&1; then
        printf "%-20s │ %12s │ %12s │ %10s │ COMPILE FAIL\n" "$name" "-" "-" "-"
        continue
    fi

    # Run native benchmark (multiple runs)
    native_times=()
    native_exit=0
    for run in $(seq 1 $NATIVE_RUNS); do
        result=$(time_command "$native_binary")
        t=$(echo "$result" | cut -d' ' -f1)
        e=$(echo "$result" | cut -d' ' -f2)
        native_times+=("$t")
        native_exit=$e
    done

    native_stats=$(compute_stats "${native_times[@]}")
    native_min=$(echo "$native_stats" | awk '{print $1}')
    native_max=$(echo "$native_stats" | awk '{print $2}')
    native_avg=$(echo "$native_stats" | awk '{print $3}')

    # Run VM benchmark (if supported)
    vm_avg="-"
    vm_min="-"
    vm_max="-"
    speedup="-"
    vm_times_json="null"
    if [ "$vm_supported" = "yes" ]; then
        vm_times=()
        for run in $(seq 1 $VM_RUNS); do
            result=$(time_command $KLAR run "$file")
            t=$(echo "$result" | cut -d' ' -f1)
            vm_times+=("$t")
        done

        if [ ${#vm_times[@]} -eq 1 ]; then
            vm_avg="${vm_times[0]}"
            vm_min="${vm_times[0]}"
            vm_max="${vm_times[0]}"
        else
            vm_stats=$(compute_stats "${vm_times[@]}")
            vm_min=$(echo "$vm_stats" | awk '{print $1}')
            vm_max=$(echo "$vm_stats" | awk '{print $2}')
            vm_avg=$(echo "$vm_stats" | awk '{print $3}')
        fi

        speedup=$(python3 -c "print(f'{$vm_avg / $native_avg:.1f}x')" 2>/dev/null || echo "N/A")
        vm_times_json="[$(IFS=,; echo "${vm_times[*]}")]"
    fi

    # Check correctness
    if [ "$native_exit" -eq 0 ]; then
        status="PASS"
    else
        status="FAIL (exit $native_exit)"
    fi

    printf "%-20s │ %10s ms │ %10s ms │ %10s │ %s\n" \
        "$name" "$native_avg" "$vm_avg" "$speedup" "$status"

    # Build JSON entry
    native_times_json="[$(IFS=,; echo "${native_times[*]}")]"
    json_entries+=("{
      \"name\": \"$name\",
      \"file\": \"$file\",
      \"vm_supported\": $([ "$vm_supported" = "yes" ] && echo "true" || echo "false"),
      \"native_avg_ms\": $native_avg,
      \"native_min_ms\": $native_min,
      \"native_max_ms\": $native_max,
      \"native_runs_ms\": $native_times_json,
      \"vm_avg_ms\": $([ "$vm_avg" = "-" ] && echo "null" || echo "$vm_avg"),
      \"vm_min_ms\": $([ "$vm_min" = "-" ] && echo "null" || echo "$vm_min"),
      \"vm_max_ms\": $([ "$vm_max" = "-" ] && echo "null" || echo "$vm_max"),
      \"vm_runs_ms\": $vm_times_json,
      \"speedup\": $([ "$speedup" = "-" ] && echo "null" || echo "\"$speedup\""),
      \"pass\": $([ "$native_exit" -eq 0 ] && echo "true" || echo "false")
    }")
done

echo "─────────────────────┴──────────────┴──────────────┴────────────┴──────────"
echo ""

# Write JSON results
{
    echo "{"
    echo "  \"timestamp\": \"$(date -u +%Y-%m-%dT%H:%M:%SZ)\","
    echo "  \"native_runs\": $NATIVE_RUNS,"
    echo "  \"vm_runs\": $VM_RUNS,"
    echo "  \"benchmarks\": ["
    for i in "${!json_entries[@]}"; do
        if [ $i -gt 0 ]; then
            echo "    ,"
        fi
        echo "    ${json_entries[$i]}"
    done
    echo "  ]"
    echo "}"
} > "$RESULTS_FILE"

echo "Results written to $RESULTS_FILE"

# Cleanup
rm -rf "$TMP_DIR"

echo "Done!"
