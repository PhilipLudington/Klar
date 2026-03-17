#!/bin/bash
# Property-based type checker tests — generates random Klar programs and
# verifies the type checker never crashes.
# Usage:
#   ./scripts/run-property-tests.sh              # Run 10K programs (default)
#   ./scripts/run-property-tests.sh 1000          # Run 1K programs

set -euo pipefail
cd "$(dirname "$0")/.."

COUNT="${1:-10000}"

echo "=== Klar Property-Based Type Checker Tests ==="
echo "Generating and checking ${COUNT} random programs..."
echo ""

zig build property-test -- "$COUNT"
