#!/bin/bash
# Fails unless the LLVM that build.zig will link is the expected major version.
#
#   scripts/check-llvm-version.sh <major>
#
# Reads LLVM_PREFIX (the variable build.zig's detectLLVMPrefix honors first) and the
# LLVM_VERSION_MAJOR define in <prefix>/include/llvm/Config/llvm-config.h. The header
# ships with every dev install (apt llvm-N-dev, brew llvm@N, the vovkos Windows
# packages), so no llvm-config binary is needed. CI runs this before the build so a
# job cannot quietly build against a different LLVM.

set -u

want="${1:-}"
if ! [[ "$want" =~ ^[0-9]+$ ]]; then
    echo "usage: $0 <major>" >&2
    exit 2
fi

if [ -z "${LLVM_PREFIX:-}" ]; then
    echo "LLVM_PREFIX is not set, so build.zig would guess the LLVM; set it to the LLVM $want prefix" >&2
    exit 1
fi

header="$LLVM_PREFIX/include/llvm/Config/llvm-config.h"
if [ ! -f "$header" ]; then
    echo "no LLVM headers at $header (LLVM_PREFIX=$LLVM_PREFIX)" >&2
    exit 1
fi

got=$(sed -n 's/^#define LLVM_VERSION_MAJOR \([0-9][0-9]*\).*$/\1/p' "$header" | head -1)
if [ -z "$got" ]; then
    echo "no LLVM_VERSION_MAJOR in $header" >&2
    exit 1
fi

if [ "$got" != "$want" ]; then
    echo "LLVM $got at $LLVM_PREFIX, expected LLVM $want" >&2
    exit 1
fi

echo "LLVM $got at $LLVM_PREFIX"
