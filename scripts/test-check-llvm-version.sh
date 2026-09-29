#!/bin/bash
# Tests scripts/check-llvm-version.sh against fake LLVM prefixes: each case writes a
# prefix whose include/llvm/Config/llvm-config.h defines a given LLVM_VERSION_MAJOR and
# checks the script's exit code.

set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CHECK="$SCRIPT_DIR/check-llvm-version.sh"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

FAILURES=0

# fake_prefix <dir> <header line or empty for no header>
fake_prefix() {
    local dir="$1" line="$2"
    mkdir -p "$dir"
    if [ -n "$line" ]; then
        mkdir -p "$dir/include/llvm/Config"
        printf '/* fake */\n#define LLVM_VERSION_MINOR 1\n%s\n#define LLVM_VERSION_PATCH 8\n' "$line" \
            > "$dir/include/llvm/Config/llvm-config.h"
    fi
}

# check <name> <want exit> <prefix or "UNSET"> <args...>
check() {
    local name="$1" want_exit="$2" prefix="$3"
    shift 3
    local got_exit
    if [ "$prefix" = UNSET ]; then
        (unset LLVM_PREFIX; "$CHECK" "$@") > "$WORK/out.txt" 2>&1
    else
        LLVM_PREFIX="$prefix" "$CHECK" "$@" > "$WORK/out.txt" 2>&1
    fi
    got_exit=$?
    if [ "$got_exit" = "$want_exit" ]; then
        echo "✓ $name (exit $got_exit)"
    else
        echo "✗ $name: exit $got_exit, want $want_exit"
        sed 's/^/    /' "$WORK/out.txt"
        FAILURES=$((FAILURES + 1))
    fi
}

fake_prefix "$WORK/llvm21" "#define LLVM_VERSION_MAJOR 21"
fake_prefix "$WORK/llvm20" "#define LLVM_VERSION_MAJOR 20"
fake_prefix "$WORK/llvm210" "#define LLVM_VERSION_MAJOR 210"
fake_prefix "$WORK/llvm2" "#define LLVM_VERSION_MAJOR 2"
fake_prefix "$WORK/nodefine" "#define LLVM_VERSION_STRING \"21.1.8\""
fake_prefix "$WORK/noheader" ""

check "matching major passes" 0 "$WORK/llvm21" 21
check "older major fails" 1 "$WORK/llvm20" 21
check "major with the expected digits as a prefix fails" 1 "$WORK/llvm210" 21
check "major that is a prefix of the expected fails" 1 "$WORK/llvm2" 21
check "header without LLVM_VERSION_MAJOR fails" 1 "$WORK/nodefine" 21
check "prefix without headers fails" 1 "$WORK/noheader" 21
check "unset LLVM_PREFIX fails" 1 UNSET 21
check "missing argument is a usage error" 2 "$WORK/llvm21"

# Every CI job that runs ./run-build.sh must run `check-llvm-version.sh 21` on an earlier
# line of the same job. A job is a two-space-indented key under `jobs:`.
CI_YML="$SCRIPT_DIR/../.github/workflows/ci.yml"
unchecked=$(awk '
    /^jobs:/ { in_jobs = 1; next }
    in_jobs && /^  [A-Za-z0-9_-]+:/ { job = $1; checked = 0; next }
    in_jobs && /check-llvm-version\.sh 21([^0-9]|$)/ { checked = 1 }
    in_jobs && /\.\/run-build\.sh/ { builds++; if (!checked) print job }
    END { if (builds == 0) print "NO-BUILD-JOBS" }
' "$CI_YML")
if [ -z "$unchecked" ]; then
    echo "✓ every ci.yml job that builds checks LLVM 21 first"
else
    echo "✗ ci.yml jobs that build without checking LLVM 21 first: $unchecked"
    FAILURES=$((FAILURES + 1))
fi

if [ "$FAILURES" -gt 0 ]; then
    echo "$FAILURES check-llvm-version case(s) failed"
    exit 1
fi
echo "All check-llvm-version cases passed"
