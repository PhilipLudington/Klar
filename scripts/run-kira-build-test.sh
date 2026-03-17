#!/bin/bash
# scripts/run-kira-build-test.sh — Integration tests for Kira build orchestration
# Tests: klar build auto-detects kira-dependencies, builds them, links, and caches

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
KLAR="$SCRIPT_DIR/zig-out/bin/klar"
TEST_DIR="/tmp/klar_kira_build_$$"
PASSED=0
FAILED=0
TOTAL=10

cleanup() {
    rm -rf "$TEST_DIR"
}
trap cleanup EXIT

rm -rf "$TEST_DIR"
mkdir -p "$TEST_DIR"

pass() { echo "  PASS: $1"; PASSED=$((PASSED + 1)); }
fail() { echo "  FAIL: $1"; FAILED=$((FAILED + 1)); }

echo "--- Kira Build Orchestration Tests ---"

# Check that kira is available
if ! command -v kira &> /dev/null; then
    echo "SKIP: 'kira' command not found — skipping Kira build tests"
    echo ""
    echo "Kira build tests: 0 passed, 0 failed (of $TOTAL) — SKIPPED"
    exit 0
fi

# --- Setup: Create a Kira library project ---
mkdir -p "$TEST_DIR/kira-mathlib"
cat > "$TEST_DIR/kira-mathlib/mathlib.ki" << 'KIRA_SRC'
fn add(x: i32, y: i32) -> i32 {
  x + y
}

fn multiply(x: i32, y: i32) -> i32 {
  x * y
}
KIRA_SRC

# --- Setup: Create a Klar project that depends on it ---
mkdir -p "$TEST_DIR/klar-project"
cat > "$TEST_DIR/klar-project/klar.json" << 'MANIFEST'
{
  "package": {
    "name": "testapp",
    "version": "0.1.0",
    "entry": "main.kl"
  },
  "dependencies": {},
  "kira-dependencies": {
    "mathlib": { "path": "../kira-mathlib" }
  }
}
MANIFEST

cat > "$TEST_DIR/klar-project/main.kl" << 'KLAR_SRC'
import kira_mathlib.*

fn main() -> i32 {
    let a: i32 = unsafe { add(3, 4) }
    let b: i32 = unsafe { multiply(5, 6) }
    println(a.to_string())
    println(b.to_string())
    return 0
}
KLAR_SRC

# --- Test 1: No deps/ directory before first build (auto-import creates it) ---
if [ ! -d "$TEST_DIR/klar-project/deps" ]; then
    pass "no deps/ directory before first build"
else
    fail "deps/ should not exist before first build"
fi

# --- Test 2: klar build succeeds with Kira dependency ---
cd "$TEST_DIR/klar-project"
OUTPUT=$("$KLAR" build 2>&1)
EXIT_CODE=$?
if [ $EXIT_CODE -eq 0 ]; then
    pass "klar build succeeds with Kira dependency"
else
    fail "klar build should succeed: exit=$EXIT_CODE output=$OUTPUT"
fi

# --- Test 3: Built executable runs and Kira functions are callable ---
if [ -f "$TEST_DIR/klar-project/build/testapp" ]; then
    OUTPUT=$("$TEST_DIR/klar-project/build/testapp" 2>&1)
    EXIT_CODE=$?
    if [ $EXIT_CODE -eq 0 ]; then
        pass "executable runs successfully — Kira functions callable (exit 0)"
    else
        fail "executable failed: exit=$EXIT_CODE output=$OUTPUT"
    fi
else
    fail "executable not found at build/testapp"
fi

# --- Test 4: Generated .kl file exists in deps/ ---
if [ -f "$TEST_DIR/klar-project/deps/kira_mathlib.kl" ]; then
    pass "generated kira_mathlib.kl exists in deps/"
else
    fail "deps/kira_mathlib.kl not found"
fi

# --- Test 5: Object file exists in build/ ---
if [ -f "$TEST_DIR/klar-project/build/kira_mathlib.o" ]; then
    pass "compiled object file exists in build/"
else
    fail "build/kira_mathlib.o not found"
fi

# --- Test 6: Cached rebuild (unchanged source uses cached .o) ---
# Record .o modification time
OBJ_MTIME_BEFORE=$(stat -f '%m' "$TEST_DIR/klar-project/build/kira_mathlib.o" 2>/dev/null || stat -c '%Y' "$TEST_DIR/klar-project/build/kira_mathlib.o" 2>/dev/null)
sleep 1
OUTPUT=$("$KLAR" build 2>&1)
OBJ_MTIME_AFTER=$(stat -f '%m' "$TEST_DIR/klar-project/build/kira_mathlib.o" 2>/dev/null || stat -c '%Y' "$TEST_DIR/klar-project/build/kira_mathlib.o" 2>/dev/null)
if [ "$OBJ_MTIME_BEFORE" = "$OBJ_MTIME_AFTER" ]; then
    pass "unchanged source uses cached .o (mtime unchanged)"
else
    fail "rebuild should use cache: before=$OBJ_MTIME_BEFORE after=$OBJ_MTIME_AFTER"
fi

# --- Test 7: Changed source triggers rebuild ---
sleep 1
cat > "$TEST_DIR/kira-mathlib/mathlib.ki" << 'KIRA_SRC2'
fn add(x: i32, y: i32) -> i32 {
  x + y
}

fn multiply(x: i32, y: i32) -> i32 {
  x * y
}

fn subtract(x: i32, y: i32) -> i32 {
  x - y
}
KIRA_SRC2
OUTPUT=$("$KLAR" build 2>&1)
OBJ_MTIME_REBUILD=$(stat -f '%m' "$TEST_DIR/klar-project/build/kira_mathlib.o" 2>/dev/null || stat -c '%Y' "$TEST_DIR/klar-project/build/kira_mathlib.o" 2>/dev/null)
if [ "$OBJ_MTIME_REBUILD" != "$OBJ_MTIME_AFTER" ]; then
    pass "changed Kira source triggers rebuild (mtime changed)"
else
    fail "rebuild should trigger: after=$OBJ_MTIME_AFTER rebuild=$OBJ_MTIME_REBUILD"
fi

# --- Test 8: New function from changed source is auto-imported and callable ---
cat > "$TEST_DIR/klar-project/main.kl" << 'KLAR_SRC2'
import kira_mathlib.*

fn main() -> i32 {
    let a: i32 = unsafe { subtract(10, 3) }
    println(a.to_string())
    return 0
}
KLAR_SRC2
OUTPUT=$("$KLAR" build 2>&1)
EXIT_CODE=$?
if [ $EXIT_CODE -eq 0 ]; then
    EXEC_OUTPUT=$("$TEST_DIR/klar-project/build/testapp" 2>&1)
    EXEC_EXIT=$?
    if [ $EXEC_EXIT -eq 0 ]; then
        pass "newly added Kira function auto-imported and callable"
    else
        fail "executable with new function failed: exit=$EXEC_EXIT output=$EXEC_OUTPUT"
    fi
else
    fail "build failed after Kira source change: exit=$EXIT_CODE output=$OUTPUT"
fi

# --- Test 9: klar clean removes build artifacts and generated interop files ---
OUTPUT=$("$KLAR" clean 2>&1)
EXIT_CODE=$?
CLEAN_OK=true
if [ -d "$TEST_DIR/klar-project/build" ]; then
    CLEAN_OK=false
fi
if [ -f "$TEST_DIR/klar-project/deps/kira_mathlib.kl" ]; then
    CLEAN_OK=false
fi
if [ $EXIT_CODE -eq 0 ] && [ "$CLEAN_OK" = "true" ]; then
    pass "klar clean removes build/ and deps/kira_*.kl"
else
    fail "klar clean should remove artifacts: exit=$EXIT_CODE build_exists=$([ -d build ] && echo yes || echo no) kl_exists=$([ -f deps/kira_mathlib.kl ] && echo yes || echo no)"
fi

# --- Test 10: Rebuild after clean regenerates everything ---
# Restore the original main.kl for a clean rebuild
cat > "$TEST_DIR/klar-project/main.kl" << 'KLAR_SRC3'
import kira_mathlib.*

fn main() -> i32 {
    let a: i32 = unsafe { subtract(10, 3) }
    println(a.to_string())
    return 0
}
KLAR_SRC3
OUTPUT=$("$KLAR" build 2>&1)
EXIT_CODE=$?
REBUILD_OK=true
if [ ! -f "$TEST_DIR/klar-project/build/testapp" ]; then
    REBUILD_OK=false
fi
if [ ! -f "$TEST_DIR/klar-project/deps/kira_mathlib.kl" ]; then
    REBUILD_OK=false
fi
if [ ! -f "$TEST_DIR/klar-project/build/kira_mathlib.o" ]; then
    REBUILD_OK=false
fi
if [ $EXIT_CODE -eq 0 ] && [ "$REBUILD_OK" = "true" ]; then
    pass "rebuild after clean regenerates all artifacts"
else
    fail "rebuild after clean failed: exit=$EXIT_CODE output=$OUTPUT"
fi

echo ""
echo "Kira build tests: $PASSED passed, $FAILED failed (of $TOTAL)"
if [ "$FAILED" -gt 0 ]; then
    exit 1
fi
