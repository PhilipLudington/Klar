#!/bin/bash
# GitStat build wrapper for Klar compiler
# Runs zig build and writes results to .build-results.json

set -e

RESULTS_FILE=".build-results.json"

# Capture build output
BUILD_OUTPUT=$(zig build 2>&1) || BUILD_EXIT=$?
BUILD_EXIT=${BUILD_EXIT:-0}

# Count errors and warnings
ERRORS=$(echo "$BUILD_OUTPUT" | grep -c "error:" || true)
WARNINGS=$(echo "$BUILD_OUTPUT" | grep -c "warning:" || true)

# Determine success
if [ $BUILD_EXIT -eq 0 ]; then
    SUCCESS="true"
else
    SUCCESS="false"
fi

# Extract error/warning messages (up to 10)
MESSAGES=$(echo "$BUILD_OUTPUT" | grep -E "(error:|warning:)" | head -10 | \
    sed 's/"/\\"/g' | \
    awk '{printf "%s\"%s\"", (NR>1?",":""), $0}')

# Write results JSON
cat > "$RESULTS_FILE" << EOF
{
  "success": $SUCCESS,
  "errors": $ERRORS,
  "warnings": $WARNINGS,
  "messages": [$MESSAGES]
}
EOF

# Print summary
if [ "$SUCCESS" = "true" ]; then
    if [ $WARNINGS -gt 0 ]; then
        echo "Build succeeded with $WARNINGS warning(s)"
    else
        echo "Build succeeded"
    fi
else
    echo "Build failed with $ERRORS error(s)"
    echo "$BUILD_OUTPUT"
    exit 1
fi
