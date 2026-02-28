#!/bin/bash
# Run meta command tests
# Tests the `klar meta` CLI command against fixture files in test/meta/

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR/.."

KLAR="./zig-out/bin/klar"
PASS=0
FAIL=0
TOTAL=0

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

echo "Running meta command tests..."

check_output() {
    local desc="$1"
    local expected="$2"
    local actual="$3"
    TOTAL=$((TOTAL + 1))
    if echo "$actual" | grep -qF "$expected"; then
        echo -e "${GREEN}PASS${NC} $desc"
        PASS=$((PASS + 1))
    else
        echo -e "${RED}FAIL${NC} $desc"
        echo "  Expected to contain: $expected"
        echo "  Got: $(echo "$actual" | head -5)"
        FAIL=$((FAIL + 1))
    fi
}

check_exact_count() {
    local desc="$1"
    local expected_count="$2"
    local actual="$3"
    TOTAL=$((TOTAL + 1))
    if echo "$actual" | grep -qE "^${expected_count} match(es)? found"; then
        echo -e "${GREEN}PASS${NC} $desc"
        PASS=$((PASS + 1))
    else
        echo -e "${RED}FAIL${NC} $desc"
        echo "  Expected: $expected_count match(es)"
        echo "  Got: $(echo "$actual" | tail -1)"
        FAIL=$((FAIL + 1))
    fi
}

# --- Test 1: --tag on directory ---
output=$("$KLAR" meta --tag "parsing" test/meta/project/ 2>&1)
check_output "--tag parsing: finds next_token" "next_token" "$output"
check_output "--tag parsing: finds parse_expr" "parse_expr" "$output"
check_output "--tag parsing: finds parse_stmt" "parse_stmt" "$output"
check_exact_count "--tag parsing: 3 matches" "3" "$output"

# --- Test 2: --tag with group expansion ---
output=$("$KLAR" meta --tag "utility" test/meta/project/utils.kl 2>&1)
check_output "--tag utility: group expansion finds clamp" "clamp" "$output"
check_output "--tag utility: group expansion finds abs_val" "abs_val" "$output"
check_exact_count "--tag utility: 2 matches" "2" "$output"

# --- Test 3: --module on directory ---
output=$("$KLAR" meta --module test/meta/project/ 2>&1)
check_output "--module: finds lexer purpose" "Tokenizer for Klar source" "$output"
check_output "--module: finds parser purpose" "Recursive descent parser" "$output"
check_exact_count "--module: 2 matches" "2" "$output"

# --- Test 4: --deprecated on directory ---
output=$("$KLAR" meta --deprecated test/meta/project/ 2>&1)
check_output "--deprecated: finds next_token" "next_token" "$output"
check_output "--deprecated: shows message" "Use new_tokenizer instead" "$output"
check_exact_count "--deprecated: 1 match" "1" "$output"

# --- Test 5: --hints on directory (includes group-expanded hints) ---
output=$("$KLAR" meta --hints test/meta/project/ 2>&1)
check_output "--hints: finds skip_whitespace" "skip_whitespace" "$output"
check_exact_count "--hints: 3 matches (group expansion)" "3" "$output"

# --- Test 6: --related on single file ---
output=$("$KLAR" meta --related process_v2 test/meta/single_file.kl 2>&1)
check_output "--related: finds function itself" "process_v2" "$output"
check_output "--related: finds referencing test" "process_works" "$output"
check_exact_count "--related process_v2: 2 matches" "2" "$output"

# --- Test 7: --json tag output ---
output=$("$KLAR" meta --tag "parsing" test/meta/project/ --json 2>&1)
check_output "--json tag: has query field" '"query":"tag"' "$output"
check_output "--json tag: has matches array" '"matches":[' "$output"
check_output "--json tag: has parse_expr" '"parse_expr"' "$output"

# --- Test 8: Empty results ---
output=$("$KLAR" meta --tag "nonexistent" test/meta/project/ 2>&1)
check_output "--tag nonexistent: 0 matches" "0 matches found" "$output"

# --- Test 9: Empty file ---
output=$("$KLAR" meta --tag "anything" test/meta/empty.kl 2>&1)
check_output "empty file: 0 matches" "0 matches found" "$output"

# --- Test 10: --deprecated on single file ---
output=$("$KLAR" meta --deprecated test/meta/single_file.kl 2>&1)
check_output "--deprecated single file: old_process" "old_process" "$output"
check_output "--deprecated single file: message" "Use process_v2 instead" "$output"

# --- Test 11: --tag on struct ---
output=$("$KLAR" meta --tag "data-model" test/meta/single_file.kl 2>&1)
check_output "--tag data-model: finds Point struct" "Point" "$output"
check_output "--tag data-model: shows struct kind" "struct" "$output"

# --- Test 12: --help flag ---
output=$("$KLAR" meta --help 2>&1)
check_output "--help: shows usage" "Usage:" "$output"

# --- Test 13: No query mode error ---
output=$("$KLAR" meta test/meta/single_file.kl 2>&1)
check_output "no query mode: error message" "no query mode specified" "$output"

# --- Test 14: --json module output ---
output=$("$KLAR" meta --module test/meta/project/ --json 2>&1)
check_output "--json module: has query field" '"query":"module"' "$output"
check_output "--json module: has module block" '"module":{' "$output"

# --- Test 15: --tag on existing native test file ---
output=$("$KLAR" meta --tag "data-model" test/native/meta_tag.kl 2>&1)
check_output "--tag on native test: finds Point" "Point" "$output"
check_exact_count "--tag on native test: 1 match" "1" "$output"

# --- Test 16: --related with :: qualified path ---
output=$("$KLAR" meta --related "lexer::next_token" test/meta/project/ 2>&1)
check_output "--related qualified: finds parse_stmt" "parse_stmt" "$output"
check_exact_count "--related qualified: 1 match" "1" "$output"

# --- Test 17: --related with bare name across directory ---
output=$("$KLAR" meta --related next_token test/meta/project/ 2>&1)
check_output "--related bare: finds next_token decl" "next_token" "$output"
check_output "--related bare: finds referencing parse_stmt" "parse_stmt" "$output"
check_exact_count "--related bare: 2 matches" "2" "$output"

# --- Test 18: --deprecated with struct fields ---
output=$("$KLAR" meta --deprecated test/meta/deprecated_detail.kl 2>&1)
check_output "--deprecated detail: finds struct Config" "Config" "$output"
check_output "--deprecated detail: finds field timeout" "timeout" "$output"
check_output "--deprecated detail: shows field kind" "field" "$output"
check_output "--deprecated detail: finds function old_init" "old_init" "$output"
check_exact_count "--deprecated detail: 3 matches" "3" "$output"

# --- Test 19: --json deprecated output ---
output=$("$KLAR" meta --deprecated test/meta/deprecated_detail.kl --json 2>&1)
check_output "--json deprecated: has query field" '"query":"deprecated"' "$output"
check_output "--json deprecated: has struct match" '"kind":"struct"' "$output"
check_output "--json deprecated: has field match" '"kind":"field"' "$output"

# --- Test 20: --json hints output ---
output=$("$KLAR" meta --hints test/meta/project/ --json 2>&1)
check_output "--json hints: has query field" '"query":"hints"' "$output"
check_output "--json hints: has skip_whitespace" '"skip_whitespace"' "$output"

# --- Test 21: --json related output (no duplicate keys) ---
output=$("$KLAR" meta --related process test/meta/multi_related.kl --json 2>&1)
check_output "--json related: has query field" '"query":"related"' "$output"
check_output "--json related: has related array" '"related":[' "$output"
check_output "--json related: has validate path" '"validate"' "$output"
check_output "--json related: has format_output path" '"format_output"' "$output"

# --- Test 22: path separator uses :: not . ---
output=$("$KLAR" meta --tag "parsing" test/meta/project/parser.kl 2>&1)
check_output "path separator: uses ::" "lexer::next_token" "$output"

# --- Test 23: --json path separator uses :: ---
output=$("$KLAR" meta --tag "parsing" test/meta/project/parser.kl --json 2>&1)
check_output "json path separator: uses ::" "lexer::next_token" "$output"

# --- Test 24: --hints on struct fields ---
output=$("$KLAR" meta --hints test/meta/hints_on_fields.kl 2>&1)
check_output "--hints fields: finds host" "host" "$output"
check_output "--hints fields: finds port" "port" "$output"
check_output "--hints fields: shows field kind" "field" "$output"
check_exact_count "--hints fields: 4 matches (2 struct fields + 2 enum variants)" "4" "$output"

# --- Test 25: --hints on enum variants ---
output=$("$KLAR" meta --hints test/meta/hints_on_fields.kl --json 2>&1)
check_output "--json hints fields: has host" '"host"' "$output"
check_output "--json hints fields: has Debug variant" '"Debug"' "$output"

# --- Test 26: parse error warning on stderr ---
output=$("$KLAR" meta --tag "anything" test/meta/parse_error.kl 2>&1)
check_output "parse error: warning on stderr" "warning: skipping" "$output"
check_output "parse error: 0 matches" "0 matches found" "$output"

# Summary
echo ""
echo "Meta command tests: $PASS/$TOTAL passed, $FAIL failed"

# Write results JSON for AirTower
cat > .meta-test-results.json << ENDJSON
{
    "passed": $PASS,
    "failed": $FAIL,
    "total": $TOTAL
}
ENDJSON

if [ $FAIL -eq 0 ]; then
    echo -e "${GREEN}All meta command tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some meta command tests failed${NC}"
    exit 1
fi
