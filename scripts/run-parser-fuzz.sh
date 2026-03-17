#!/bin/bash
# Parser fuzzer — tests the Klar lexer and parser against random inputs.
# Usage:
#   ./scripts/run-parser-fuzz.sh              # Run 1M inputs (default)
#   ./scripts/run-parser-fuzz.sh 100000       # Run 100K inputs
#   ./scripts/run-parser-fuzz.sh --seed-only  # Run seed corpus only (fast)

set -euo pipefail
cd "$(dirname "$0")/.."

COUNT="${1:-1000000}"
SEED_ONLY=false

if [[ "${1:-}" == "--seed-only" ]]; then
    SEED_ONLY=true
fi

echo "=== Klar Parser Fuzzer ==="
echo ""

# Always run seed corpus first (quick sanity check)
echo "Running seed corpus..."
if zig build fuzz 2>&1; then
    echo "Seed corpus: PASS"
else
    echo "Seed corpus: FAIL"
    exit 1
fi
echo ""

if [ "$SEED_ONLY" = true ]; then
    echo "Done (seed corpus only)."
    exit 0
fi

# Run stress test with random inputs
echo "Running stress test with ${COUNT} random inputs..."
zig build fuzz-stress -- "$COUNT"
