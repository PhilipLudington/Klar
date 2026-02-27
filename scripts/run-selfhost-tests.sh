#!/bin/bash
# Run self-hosted compiler stub tests
#
# Phase 1: Verify each selfhost/*.kl file passes klar check and inline tests
# Phase 2: Lexer parity testing — compares Zig vs selfhost dump-tokens output
# Phase 3a: Parser parse check — verify selfhost parser can parse test corpus
# Phase 3b: Parser AST parity — compare normalized AST output vs Zig dump-ast

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RESULTS_FILE="$SCRIPT_DIR/.selfhost-test-results.json"
KLAR="$SCRIPT_DIR/zig-out/bin/klar"
SELFHOST_DIR="$SCRIPT_DIR/selfhost"

# Ensure compiler is built
if [ ! -f "$KLAR" ]; then
    echo "Building Klar compiler first..."
    cd "$SCRIPT_DIR" && zig build || exit 1
fi

PASSED=0
FAILED=0
FAILURES=""
PARITY_FILES="
test/native/arc_basic.kl
test/native/arc_clone.kl
test/native/arc_drop.kl
test/native/arith.kl
test/native/array_assign.kl
test/native/assoc_type_basic.kl
test/native/assoc_type_self_item.kl
test/native/assoc_type_with_bounds.kl
test/native/assoc_type_generic.kl
test/native/async_await_basic.kl
test/native/async_await_failed_error.kl
test/native/async_await_in_control_flow.kl
test/native/async_await_pending_error.kl
test/native/async_await_void.kl
test/native/async_calls_sync.kl
test/native/async_closure_capture.kl
test/native/async_deep_chain.kl
test/native/async_future_string.kl
test/native/async_generic_future.kl
test/native/async_method_chaining.kl
test/native/async_multiple_awaits.kl
test/native/async_result_propagation.kl
test/native/array_bounds.kl
test/native/array_contains_string.kl
test/native/array_element_field.kl
test/native/array_methods.kl
test/native/array_of_structs.kl
test/native/array_repeat.kl
test/native/array_return.kl
test/native/array_sized.kl
test/native/array_slice_param.kl
test/native/array.kl
test/native/bufreader_basic.kl
test/native/bufreader_readline.kl
test/native/bufwriter_basic.kl
test/native/bufwriter_drop.kl
test/native/call.kl
test/native/cell_basic.kl
test/native/clone_trait_bool.kl
test/native/clone_trait_float.kl
test/native/clone_trait_int.kl
test/native/clone_trait_string.kl
test/native/closure_arg_capture.kl
test/native/closure_arg.kl
test/native/closure_capture.kl
test/native/closure_map.kl
test/native/closure_multi_capture.kl
test/native/closure_return_simple.kl
test/native/closure_return.kl
test/native/closure_simple.kl
test/native/closure_typed_capture.kl
test/native/comptime_array.kl
test/native/comptime_assert.kl
test/native/comptime_basic.kl
test/native/comptime_block.kl
test/native/comptime_const_access.kl
test/native/comptime_fn.kl
test/native/comptime_fn_simple.kl
test/native/comptime_param.kl
test/native/comptime_recursive.kl
test/native/comptime_recursive_simple.kl
test/native/comptime_string.kl
test/native/comptime_typename.kl
test/native/comptime_struct.kl
test/native/debug_builtin.kl
test/native/default_trait_bool.kl
test/native/default_trait_float.kl
test/native/default_trait_int.kl
test/native/default_trait_string.kl
test/native/drop_trait.kl
test/native/early_return.kl
test/native/enum_struct_payload_mismatch.kl
test/native/enum_struct_payload.kl
test/native/eq_trait_bool.kl
test/native/eq_trait_string.kl
test/native/eq_trait.kl
test/native/error_from_conversion.kl
test/native/file_error.kl
test/native/file_read_all.kl
test/native/file_read_buffer.kl
test/native/file_read_to_string.kl
test/native/file_write.kl
test/native/for_array.kl
test/native/for_list.kl
test/native/for_range.kl
test/native/for_slice.kl
test/native/generic_enum.kl
test/native/generic_enum_match.kl
test/native/generic_struct.kl
test/native/generic_struct_method.kl
test/native/generics_basic.kl
test/native/hash_trait_bool.kl
test/native/hash_trait_float.kl
test/native/hash_trait_int.kl
test/native/hash_trait_string.kl
test/native/hello.kl
test/native/io_generic.kl
test/native/int_abs.kl
test/native/int_max.kl
test/native/int_min.kl
test/native/into_trait.kl
test/native/iter_trait_basic.kl
test/native/list_basic.kl
test/native/list_clone.kl
test/native/list_drop.kl
test/native/list_enumerate.kl
test/native/list_filter.kl
test/native/list_for.kl
test/native/list_index_assign.kl
test/native/list_last.kl
test/native/list_map.kl
test/native/list_nested_basic.kl
test/native/list_pop.kl
test/native/list_skip.kl
test/native/list_string_drop.kl
test/native/list_take.kl
test/native/list_with_capacity.kl
test/native/list_zip.kl
test/native/local_vars.kl
test/native/many_params.kl
test/native/map_basic.kl
test/native/map_filter.kl
test/native/map_for.kl
test/native/map_resize.kl
test/native/map_skip.kl
test/native/map_take.kl
test/native/map_values.kl
test/native/match_tuple_element.kl
test/native/nested_calls.kl
test/native/nested_struct_field.kl
test/native/optional_and_then.kl
test/native/optional_clone.kl
test/native/optional_coalesce_some.kl
test/native/optional_coalesce.kl
test/native/optional_eq.kl
test/native/optional_expect.kl
test/native/optional_map.kl
test/native/optional_propagate.kl
test/native/optional_some.kl
test/native/optional_unwrap_or.kl
test/native/optional_unwrap.kl
test/native/ordered_trait_float.kl
test/native/ordered_trait_string.kl
test/native/ordered_trait.kl
test/native/overflow_add.kl
test/native/overflow_mul.kl
test/native/overflow_sub.kl
test/native/print_hello.kl
test/native/print_no_newline.kl
test/native/range_basic.kl
test/native/range_inclusive.kl
test/native/rc_basic.kl
test/native/rc_clone.kl
test/native/rc_drop.kl
test/native/read_trait_basic.kl
test/native/recursive_deep.kl
test/native/ref_addr.kl
test/native/ref_inout.kl
test/native/ref_self_generic.kl
test/native/ref_self_method.kl
test/native/result_and_then.kl
test/native/result_clone.kl
test/native/result_closure.kl
test/native/result_context_display.kl
test/native/result_context.kl
test/native/result_eq.kl
test/native/result_err.kl
test/native/result_expect.kl
test/native/result_is_ok.kl
test/native/result_map_err.kl
test/native/result_map.kl
test/native/result_ok_err.kl
test/native/result_ok.kl
test/native/result_propagate_simple.kl
test/native/result_propagate.kl
test/native/result_unwrap_err.kl
test/native/result_unwrap_ok.kl
test/native/result_unwrap_or.kl
test/native/return_types.kl
test/native/saturating_add.kl
test/native/saturating_arithmetic.kl
test/native/saturating_mul.kl
test/native/saturating_sub.kl
test/native/self_type_impl.kl
test/native/set_basic.kl
test/native/set_enumerate.kl
test/native/set_filter.kl
test/native/set_for.kl
test/native/set_map.kl
test/native/set_operations.kl
test/native/set_skip.kl
test/native/set_take.kl
test/native/set_zip.kl
test/native/slice_in_struct.kl
test/native/stdin_basic.kl
test/native/stdout_basic.kl
test/native/string_append.kl
test/native/string_as_str.kl
test/native/string_basic.kl
test/native/string_bytes.kl
test/native/string_chars.kl
test/native/string_clear.kl
test/native/string_clone.kl
test/native/string_concat.kl
test/native/string_contains.kl
test/native/string_drop.kl
test/native/string_ends_with.kl
test/native/string_eq.kl
test/native/string_escape_braces.kl
test/native/string_hash.kl
test/native/string_interp.kl
test/native/string_is_empty.kl
test/native/string_len.kl
test/native/string_primitives.kl
test/native/string_push.kl
test/native/string_simple.kl
test/native/string_slice.kl
test/native/string_starts_with.kl
test/native/string_to_lowercase.kl
test/native/string_to_uppercase.kl
test/native/string_trim.kl
test/native/struct_field_assign.kl
test/native/struct_method.kl
test/native/struct_order.kl
test/native/struct_param.kl
test/native/struct_slice_field.kl
test/native/struct_static_method.kl
test/native/struct.kl
test/native/test_assert_eq_fail.kl
test/native/test_assert_eq_pass.kl
test/native/test_assert_fail.kl
test/native/test_assert_helpers_pass.kl
test/native/test_assert_pass.kl
test/native/test_block_skipped_by_build_and_run.kl
test/native/test_dbg.kl
test/native/test_len.kl
test/native/test_panic.kl
test/native/test_type_name.kl
test/native/trait_basic.kl
test/native/trait_bounds.kl
test/native/trait_default.kl
test/native/trait_inheritance.kl
test/native/trait_method_inout_bounds.kl
test/native/trait_method_ref_bounds.kl
test/native/trait_method_through_bounds.kl
test/native/trait_method_with_args.kl
test/native/trait_multi_inherit.kl
test/native/tuple.kl
test/native/type_resolve_alloc.kl
test/native/typeinfo_basic.kl
test/native/typeinfo_enum.kl
test/native/typeinfo_struct.kl
test/native/unsigned_arithmetic.kl
test/native/wrapping_add.kl
test/native/wrapping_arithmetic.kl
test/native/wrapping_mul.kl
test/native/wrapping_sub.kl
test/native/write_trait_basic.kl
"

# Phase 1: Type-check all selfhost files
echo "Phase 1: Type checking selfhost/ stubs"
echo "────────────────────────────────────────"

for f in "$SELFHOST_DIR"/*.kl; do
    [ -f "$f" ] || continue
    name=$(basename "$f" .kl)

    check_output=$("$KLAR" check "$f" 2>&1)
    check_exit=$?

    if [ $check_exit -eq 0 ]; then
        echo "  ✓ $name (check passed)"
        PASSED=$((PASSED + 1))
    else
        echo "  ✗ $name (check failed)"
        echo "    $check_output" | head -3
        FAILED=$((FAILED + 1))
        [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
        FAILURES="$FAILURES\"$name: check failed\""
    fi
done

# Phase 1b: Run inline tests for standalone files (no imports)
echo ""
echo "Phase 1b: Running inline tests"
echo "────────────────────────────────────────"

for f in "$SELFHOST_DIR"/*.kl; do
    [ -f "$f" ] || continue
    # Skip files with imports — they need multi-file klar test support
    grep -q "^import " "$f" && continue
    name=$(basename "$f" .kl)

    test_output=$("$KLAR" test "$f" 2>&1)
    test_exit=$?

    if [ $test_exit -eq 0 ]; then
        echo "  ✓ $name (tests passed)"
        PASSED=$((PASSED + 1))
    else
        echo "  ✗ $name (tests failed)"
        echo "    $test_output" | head -3
        FAILED=$((FAILED + 1))
        [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
        FAILURES="$FAILURES\"$name: tests failed\""
    fi
done

# Phase 2: Lexer parity testing (dump-tokens)
# Compares Zig lexer JSON output with selfhost lexer JSON output.
# Pre-builds the selfhost lexer binary once, then reuses it for all test files.
echo ""
echo "Phase 2: Lexer parity testing (dump-tokens)"
echo "────────────────────────────────────────"

SELFHOST_LEXER="$SCRIPT_DIR/build/selfhost_lexer"
build_output=$("$KLAR" build "$SELFHOST_DIR/lexer.kl" -o "$SELFHOST_LEXER" 2>&1)
build_exit=$?
if [ $build_exit -ne 0 ]; then
    echo "  ✗ selfhost lexer build failed"
    echo "    $build_output" | head -3
    FAILED=$((FAILED + 1))
    [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
    FAILURES="$FAILURES\"selfhost lexer: build failed\""
else
    for f in $PARITY_FILES; do
        [ -f "$SCRIPT_DIR/$f" ] || continue
        name=$(basename "$f" .kl)

        # Get Zig lexer output
        zig_tokens=$("$KLAR" dump-tokens "$SCRIPT_DIR/$f" 2>/dev/null)
        zig_exit=$?
        if [ $zig_exit -ne 0 ]; then
            echo "  ✗ $name (zig lexer failed)"
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: zig lexer failed\""
            continue
        fi

        # Get selfhost lexer output (using pre-built binary)
        selfhost_tokens=$("$SELFHOST_LEXER" "$SCRIPT_DIR/$f" 2>/dev/null)
        selfhost_exit=$?
        if [ $selfhost_exit -ne 0 ]; then
            echo "  ✗ $name (selfhost lexer failed)"
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: selfhost lexer failed\""
            continue
        fi

        # Compare token streams
        if [ "$zig_tokens" = "$selfhost_tokens" ]; then
            echo "  ✓ $name (token parity)"
            PASSED=$((PASSED + 1))
        else
            echo "  ✗ $name (token mismatch)"
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: token mismatch\""
            # Show first difference for debugging
            diff <(echo "$zig_tokens" | python3 -m json.tool 2>/dev/null || echo "$zig_tokens") \
                 <(echo "$selfhost_tokens" | python3 -m json.tool 2>/dev/null || echo "$selfhost_tokens") 2>/dev/null | head -10
        fi
    done
fi

# Phase 3: Parser parity testing (dump-ast)
# Phase 3a: Verify selfhost parser can parse each test corpus file (exit 0)
# Phase 3b: Compare normalized AST output with Zig dump-ast
echo ""
echo "Phase 3a: Parser parse check"
echo "────────────────────────────────────────"

SELFHOST_PARSER="$SCRIPT_DIR/build/selfhost_parser"
NORMALIZE_SCRIPT="$SCRIPT_DIR/scripts/normalize-ast.py"
build_output=$("$KLAR" build "$SELFHOST_DIR/parser_main.kl" -o "$SELFHOST_PARSER" 2>&1)
build_exit=$?
PARSED_FILES=""
if [ $build_exit -ne 0 ]; then
    echo "  ✗ selfhost parser build failed"
    echo "    $build_output" | head -3
    FAILED=$((FAILED + 1))
    [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
    FAILURES="$FAILURES\"selfhost parser: build failed\""
else
    for f in $PARITY_FILES; do
        [ -f "$SCRIPT_DIR/$f" ] || continue
        name=$(basename "$f" .kl)

        selfhost_ast=$("$SELFHOST_PARSER" "$SCRIPT_DIR/$f" 2>/dev/null)
        selfhost_exit=$?
        if [ $selfhost_exit -eq 0 ]; then
            echo "  ✓ $name (parse ok)"
            PASSED=$((PASSED + 1))
            PARSED_FILES="$PARSED_FILES $f"
        else
            echo "  ✗ $name (parse failed, exit $selfhost_exit)"
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: parser failed (exit $selfhost_exit)\""
        fi
    done
fi

echo ""
echo "Phase 3b: Parser AST parity (normalized)"
echo "────────────────────────────────────────"

if [ $build_exit -ne 0 ]; then
    echo "  (skipped — selfhost parser build failed)"
elif [ -z "$PARSED_FILES" ]; then
    echo "  (skipped — no files parsed successfully)"
else
    for f in $PARSED_FILES; do
        name=$(basename "$f" .kl)

        # Get Zig dump-ast output
        zig_ast=$("$KLAR" dump-ast "$SCRIPT_DIR/$f" 2>/dev/null)
        zig_exit=$?
        if [ $zig_exit -ne 0 ]; then
            echo "  ✗ $name (zig dump-ast failed)"
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: zig dump-ast failed\""
            continue
        fi

        # Get selfhost parser output
        selfhost_ast=$("$SELFHOST_PARSER" "$SCRIPT_DIR/$f" 2>/dev/null)
        selfhost_exit=$?
        if [ $selfhost_exit -ne 0 ]; then
            echo "  ✗ $name (selfhost parser crashed, exit $selfhost_exit)"
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: parser crashed (exit $selfhost_exit)\""
            continue
        fi

        # Compare with normalization
        norm_output=$(python3 "$NORMALIZE_SCRIPT" --str "$zig_ast" "$selfhost_ast" 2>&1)
        norm_exit=$?
        if [ $norm_exit -eq 0 ]; then
            echo "  ✓ $name (AST parity)"
            PASSED=$((PASSED + 1))
        elif [ $norm_exit -eq 2 ]; then
            echo "  ✗ $name (normalization error)"
            echo "    $norm_output" | head -5
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: normalization error\""
        else
            echo "  ✗ $name (AST mismatch)"
            echo "    $norm_output" | head -5
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: AST mismatch\""
        fi
    done
fi

# Phase 3c: Error parity (both parsers must reject these files)
echo ""
echo "Phase 3c: Parser error parity"
echo "────────────────────────────────────────"

ERROR_PARITY_FILES="
test/native/missing_return_type.kl
"

if [ $build_exit -ne 0 ]; then
    echo "  (skipped — selfhost parser build failed)"
else
    for f in $ERROR_PARITY_FILES; do
        name=$(basename "$f" .kl)
        if [ ! -f "$SCRIPT_DIR/$f" ]; then
            echo "  ✗ $name (file not found: $f)"
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: file not found\""
            continue
        fi

        # Zig parser should reject
        "$KLAR" dump-ast "$SCRIPT_DIR/$f" >/dev/null 2>&1
        zig_exit=$?

        # Selfhost parser should reject
        "$SELFHOST_PARSER" "$SCRIPT_DIR/$f" >/dev/null 2>&1
        selfhost_exit=$?

        if [ $zig_exit -ne 0 ] && [ $selfhost_exit -ne 0 ]; then
            echo "  ✓ $name (both reject)"
            PASSED=$((PASSED + 1))
        elif [ $zig_exit -eq 0 ] && [ $selfhost_exit -eq 0 ]; then
            echo "  ✗ $name (both accept — expected rejection)"
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: expected parse error from both\""
        elif [ $selfhost_exit -eq 0 ]; then
            echo "  ✗ $name (selfhost accepts, zig rejects)"
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: selfhost should reject\""
        else
            echo "  ✗ $name (selfhost rejects, zig accepts)"
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: zig should reject\""
        fi
    done
fi

# Phase 4: Checker parity testing
# Runs both Zig klar check and selfhost checker on test files.
# Requires pre-built selfhost parser (from Phase 3) and checker_parity binary.
echo ""
echo "Phase 4: Checker parity testing"
echo "────────────────────────────────────────"

# Temp file cleanup on interrupt
_CHECKER_TMPJSON=""
trap 'rm -f "$_CHECKER_TMPJSON"' INT TERM

SELFHOST_CHECKER="$SCRIPT_DIR/build/checker_parity"
checker_build_output=$("$KLAR" build "$SELFHOST_DIR/checker_parity.kl" -o "$SELFHOST_CHECKER" 2>&1)
checker_build_exit=$?
# Check if binary was actually created (build may exit 0 without producing binary)
if [ ! -f "$SELFHOST_CHECKER" ]; then
    echo "  ✗ selfhost checker build failed (no binary produced)"
    echo "    $checker_build_output" | head -3
    FAILED=$((FAILED + 1))
    [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
    FAILURES="$FAILURES\"selfhost checker: build failed\""
else
    # Test tiers — start with simple programs
    CHECKER_TIER1="
test/native/hello.kl
test/native/arith.kl
test/native/local_vars.kl
test/native/call.kl
test/native/early_return.kl
test/native/return_types.kl
test/native/nested_calls.kl
test/native/many_params.kl
test/native/struct.kl
test/native/struct_method.kl
test/native/struct_param.kl
test/native/struct_order.kl
test/native/struct_field_assign.kl
test/native/struct_static_method.kl
test/native/nested_struct_field.kl
test/native/for_range.kl
test/native/for_array.kl
test/native/range_basic.kl
test/native/range_inclusive.kl
test/native/string_simple.kl
test/native/string_basic.kl
test/native/string_concat.kl
test/native/string_eq.kl
test/native/string_len.kl
test/native/eq_trait.kl
test/native/eq_trait_bool.kl
test/native/eq_trait_string.kl
test/native/clone_trait_int.kl
test/native/clone_trait_bool.kl
test/native/clone_trait_float.kl
test/native/clone_trait_string.kl
"

    # Known gaps — features not yet in selfhost checker
    # string_simple/basic/concat/eq need String (capital-S) type support
    CHECKER_KNOWN_GAPS="async_ ffi/ ownership_ shadow_ import string_simple string_basic string_concat string_eq"

    MATCH_ACCEPT=0
    MATCH_REJECT=0
    KNOWN_GAP=0
    SELFHOST_TOO_PERMISSIVE=0
    SELFHOST_TOO_STRICT=0
    CRASH=0

    for f in $CHECKER_TIER1; do
        [ -f "$SCRIPT_DIR/$f" ] || continue
        name=$(basename "$f" .kl)

        # Check known gaps
        skip=0
        for gap in $CHECKER_KNOWN_GAPS; do
            case "$name" in
                ${gap}*) skip=1 ;;
            esac
        done
        if [ $skip -eq 1 ]; then
            KNOWN_GAP=$((KNOWN_GAP + 1))
            continue
        fi

        # Run Zig klar check
        "$KLAR" check "$SCRIPT_DIR/$f" >/dev/null 2>&1
        zig_exit=$?

        # Run selfhost: dump-ast directly to temp file, then checker_parity
        tmpjson=$(mktemp /tmp/checker_parity_XXXXXX.json)
        _CHECKER_TMPJSON="$tmpjson"
        "$KLAR" dump-ast "$SCRIPT_DIR/$f" > "$tmpjson" 2>/dev/null
        dump_exit=$?
        if [ $dump_exit -ne 0 ]; then
            rm -f "$tmpjson"
            KNOWN_GAP=$((KNOWN_GAP + 1))
            continue
        fi

        selfhost_output=$("$SELFHOST_CHECKER" "$tmpjson" 2>&1)
        selfhost_exit=$?
        rm -f "$tmpjson"

        if [ $selfhost_exit -ge 128 ]; then
            echo "  ✗ $name (CRASH, exit $selfhost_exit)"
            CRASH=$((CRASH + 1))
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: checker crash (exit $selfhost_exit)\""
        elif [ $zig_exit -eq 0 ] && [ $selfhost_exit -eq 0 ]; then
            echo "  ✓ $name (match-accept)"
            MATCH_ACCEPT=$((MATCH_ACCEPT + 1))
            PASSED=$((PASSED + 1))
        elif [ $zig_exit -ne 0 ] && [ $selfhost_exit -ne 0 ]; then
            echo "  ✓ $name (match-reject)"
            MATCH_REJECT=$((MATCH_REJECT + 1))
            PASSED=$((PASSED + 1))
        elif [ $zig_exit -eq 0 ] && [ $selfhost_exit -ne 0 ]; then
            echo "  ✗ $name (selfhost-too-strict)"
            echo "    selfhost: $selfhost_output" | head -3
            SELFHOST_TOO_STRICT=$((SELFHOST_TOO_STRICT + 1))
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: selfhost too strict\""
        else
            echo "  ✗ $name (selfhost-too-permissive)"
            SELFHOST_TOO_PERMISSIVE=$((SELFHOST_TOO_PERMISSIVE + 1))
            FAILED=$((FAILED + 1))
            [ -n "$FAILURES" ] && FAILURES="$FAILURES,"
            FAILURES="$FAILURES\"$name: selfhost too permissive\""
        fi
    done

    echo ""
    echo "  Checker parity summary:"
    echo "    match-accept: $MATCH_ACCEPT"
    echo "    match-reject: $MATCH_REJECT"
    echo "    known-gap: $KNOWN_GAP"
    echo "    selfhost-too-strict: $SELFHOST_TOO_STRICT"
    echo "    selfhost-too-permissive: $SELFHOST_TOO_PERMISSIVE"
    echo "    crash: $CRASH"
fi

TOTAL=$((PASSED + FAILED))
echo ""
echo "Selfhost tests: $PASSED/$TOTAL passed"

# Write results JSON
cat > "$RESULTS_FILE" <<ENDJSON
{
  "test_suite": "selfhost",
  "total": $TOTAL,
  "passed": $PASSED,
  "failed": $FAILED,
  "failures": [$FAILURES]
}
ENDJSON

if [ $FAILED -gt 0 ]; then
    exit 1
fi
