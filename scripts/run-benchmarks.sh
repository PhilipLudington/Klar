#!/usr/bin/env bash

# Benchmark runner script for Klar
# Compares VM (bytecode) vs Native (LLVM) performance

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$SCRIPT_DIR"

# Ensure compiler is built
echo "Building Klar compiler..."
./build.sh > /dev/null 2>&1

KLAR="$SCRIPT_DIR/zig-out/bin/klar"
BENCHMARK_DIR="$SCRIPT_DIR/benchmarks"
RESULTS_FILE="$SCRIPT_DIR/.benchmark-results.json"
TMP_DIR="/tmp/klar_benchmarks"

mkdir -p "$TMP_DIR"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo ""
echo "╔════════════════════════════════════════════════════════════════╗"
echo "║           Klar Benchmark Suite: VM vs Native                  ║"
echo "╚════════════════════════════════════════════════════════════════╝"
echo ""

# Get expected exit code for a benchmark
get_expected_exit() {
    case "$1" in
        fib) echo 201 ;;        # fib(35) mod 256
        matrix) echo 184 ;;     # 3000 mod 256
        rc_stress) echo 100 ;;
        sort) echo 136 ;;       # bubble sort result mod 256
        string) echo 248 ;;     # 11000 mod 256
        *) echo 0 ;;
    esac
}

# Number of iterations for timing
VM_ITERATIONS=1
NATIVE_ITERATIONS=3

# Function to time command execution
time_command() {
    local start end elapsed
    start=$(python3 -c 'import time; print(time.time())')
    "$@" > /dev/null 2>&1
    local exit_code=$?
    end=$(python3 -c 'import time; print(time.time())')
    elapsed=$(python3 -c "print(f'{$end - $start:.3f}')")
    echo "$elapsed $exit_code"
}

# Initialize results array
results_json="["
first_result=true

echo "Running benchmarks..."
echo ""
printf "%-15s │ %12s │ %12s │ %10s │ %s\n" "Benchmark" "VM Time" "Native Time" "Speedup" "Status"
echo "────────────────┼──────────────┼──────────────┼────────────┼──────────"

for bench_file in "$BENCHMARK_DIR"/*.kl; do
    bench_name=$(basename "$bench_file" .kl)
    expected_exit=$(get_expected_exit "$bench_name")

    native_binary="$TMP_DIR/$bench_name"

    # Compile to native
    compile_output=$("$KLAR" build "$bench_file" -o "$native_binary" -O2 2>&1) || {
        printf "%-15s │ %12s │ %12s │ %10s │ ${RED}COMPILE FAIL${NC}\n" "$bench_name" "-" "-" "-"
        continue
    }

    # Run VM benchmark (single run - VM is slower)
    vm_result=$(time_command "$KLAR" run "$bench_file")
    vm_time=$(echo "$vm_result" | cut -d' ' -f1)
    vm_exit=$(echo "$vm_result" | cut -d' ' -f2)

    # Run Native benchmark (multiple runs, take best)
    best_native_time=999999
    native_exit=0
    for i in $(seq 1 $NATIVE_ITERATIONS); do
        native_result=$(time_command "$native_binary")
        native_time=$(echo "$native_result" | cut -d' ' -f1)
        native_exit=$(echo "$native_result" | cut -d' ' -f2)
        if (( $(echo "$native_time < $best_native_time" | bc -l) )); then
            best_native_time=$native_time
        fi
    done

    # Calculate speedup
    if (( $(echo "$best_native_time > 0" | bc -l) )); then
        speedup=$(python3 -c "print(f'{$vm_time / $best_native_time:.1f}x')")
    else
        speedup="∞"
    fi

    # Check exit codes
    if [[ "$native_exit" == "$expected_exit" ]]; then
        status="${GREEN}✓ PASS${NC}"
        pass_json="true"
    else
        status="${RED}✗ FAIL (expected $expected_exit, got $native_exit)${NC}"
        pass_json="false"
    fi

    printf "%-15s │ %10ss │ %10ss │ %10s │ %b\n" \
        "$bench_name" "$vm_time" "$best_native_time" "$speedup" "$status"

    # Add to JSON results
    if [ "$first_result" = true ]; then
        first_result=false
    else
        results_json="$results_json,"
    fi
    results_json="$results_json
  {\"name\":\"$bench_name\",\"vm_time\":$vm_time,\"native_time\":$best_native_time,\"speedup\":\"$speedup\",\"pass\":$pass_json}"
done

echo "────────────────┴──────────────┴──────────────┴────────────┴──────────"
echo ""

# Write JSON results
results_json="$results_json
]"
echo "$results_json" > "$RESULTS_FILE"

echo "Results saved to $RESULTS_FILE"

# Cleanup
rm -rf "$TMP_DIR"

echo ""
echo "Done!"
