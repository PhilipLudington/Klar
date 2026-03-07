#!/usr/bin/env python3
"""
Validation tests for docs/design/typed-ast-format.md

Validates:
1. Schema covers all AST node types present in test/native/ files
2. Format is unambiguous and parseable (sample JSON validates)
3. All expression/statement/declaration/pattern/type_expr kinds are documented
"""

import json
import os
import re
import subprocess
import sys
from pathlib import Path

# Project root (relative to this script's location)
SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parent.parent
SPEC_PATH = PROJECT_ROOT / "docs" / "design" / "typed-ast-format.md"
AST_ZIG = PROJECT_ROOT / "src" / "ast.zig"
TYPES_ZIG = PROJECT_ROOT / "src" / "types.zig"
TEST_NATIVE_DIR = PROJECT_ROOT / "test" / "native"
KLAR_BIN = PROJECT_ROOT / "zig-out" / "bin" / "klar"

passed = 0
failed = 0
errors = []


def test(name, condition, detail=""):
    global passed, failed
    if condition:
        passed += 1
        print(f"  PASS: {name}")
    else:
        failed += 1
        msg = f"  FAIL: {name}"
        if detail:
            msg += f" -- {detail}"
        print(msg)
        errors.append(msg)


def read_spec():
    """Read the typed AST format spec."""
    with open(SPEC_PATH) as f:
        return f.read()


def read_source(path):
    """Read a source file."""
    with open(path) as f:
        return f.read()


# ============================================================================
# Test 1: Spec file exists and has required sections
# ============================================================================
def test_spec_structure():
    print("\n--- Test 1: Spec structure ---")
    test("Spec file exists", SPEC_PATH.exists())
    spec = read_spec()

    # Required sections
    required_sections = [
        "Top-Level Structure",
        "Resolved Type Representation",
        "Typed Expressions",
        "Typed Statements",
        "Typed Declarations",
        "Patterns",
        "Type Expressions",
        "Monomorphizations Section",
        "Trait Implementations Section",
        "Node Type Coverage",
        "Validation Rules",
    ]
    for section in required_sections:
        test(f"Section '{section}' exists", section in spec)

    # Required format identifiers
    test("Format version defined", '"format": "typed-ast"' in spec)
    test("Version number defined", '"version": 1' in spec)


# ============================================================================
# Test 2: All expression kinds from ast.zig are documented
# ============================================================================
def test_expression_coverage():
    print("\n--- Test 2: Expression kind coverage ---")
    spec = read_spec()
    ast_src = read_source(AST_ZIG)

    # Extract expression kinds from Expr union in ast.zig
    expr_match = re.search(r'pub const Expr = union\(enum\) \{(.*?)\n\s*pub fn span',
                           ast_src, re.DOTALL)
    assert expr_match, "Could not find Expr union in ast.zig"

    expr_kinds = re.findall(r'^\s+(\w+):', expr_match.group(1), re.MULTILINE)
    # Filter out the span function
    expr_kinds = [k for k in expr_kinds if k not in ('span',)]

    # Map ast.zig field names to JSON kind names
    kind_map = {
        'literal': 'literal',
        'identifier': 'identifier',
        'binary': 'binary',
        'unary': 'unary',
        'postfix': 'postfix',
        'call': 'call',
        'index': 'index',
        'field': 'field',
        'method_call': 'method_call',
        'block': 'block',
        'closure': 'closure',
        'range': 'range',
        'struct_literal': 'struct_literal',
        'array_literal': 'array_literal',
        'tuple_literal': 'tuple_literal',
        'type_cast': 'type_cast',
        'grouped': 'grouped',
        'interpolated_string': 'interpolated_string',
        'enum_literal': 'enum_literal',
        'comptime_block': 'comptime_block',
        'builtin_call': 'builtin_call',
        'unsafe_block': 'unsafe_block',
        'out_arg': 'out_arg',
    }

    for field_name in expr_kinds:
        json_kind = kind_map.get(field_name, field_name)
        # Check the spec mentions this kind in the expressions section or coverage table
        test(f"Expression '{json_kind}' documented",
             f'`{json_kind}`' in spec or f'"{json_kind}"' in spec,
             f"ast.zig field: {field_name}")


# ============================================================================
# Test 3: All statement kinds from ast.zig are documented
# ============================================================================
def test_statement_coverage():
    print("\n--- Test 3: Statement kind coverage ---")
    spec = read_spec()
    ast_src = read_source(AST_ZIG)

    stmt_match = re.search(r'pub const Stmt = union\(enum\) \{(.*?)\n\s*pub fn span',
                           ast_src, re.DOTALL)
    assert stmt_match, "Could not find Stmt union in ast.zig"

    stmt_kinds = re.findall(r'^\s+(\w+):', stmt_match.group(1), re.MULTILINE)

    for kind in stmt_kinds:
        test(f"Statement '{kind}' documented",
             f'`{kind}`' in spec or f'"{kind}"' in spec)


# ============================================================================
# Test 4: All declaration kinds from ast.zig are documented
# ============================================================================
def test_declaration_coverage():
    print("\n--- Test 4: Declaration kind coverage ---")
    spec = read_spec()
    ast_src = read_source(AST_ZIG)

    decl_match = re.search(r'pub const Decl = union\(enum\) \{(.*?)\n\s*pub fn span',
                           ast_src, re.DOTALL)
    assert decl_match, "Could not find Decl union in ast.zig"

    decl_fields = re.findall(r'^\s+(\w+):', decl_match.group(1), re.MULTILINE)

    # Map field names to JSON kind names (from existing dump-ast)
    kind_map = {
        'function': 'function',
        'test_decl': 'test_decl',
        'struct_decl': 'struct_decl',
        'enum_decl': 'enum_decl',
        'trait_decl': 'trait_decl',
        'impl_decl': 'impl_decl',
        'type_alias': 'type_alias',
        'const_decl': 'const_decl',
        'import_decl': 'import_decl',
        'module_decl': 'module_decl',
        'extern_type_decl': 'extern_type_decl',
        'extern_block': 'extern_block',
    }

    for field_name in decl_fields:
        json_kind = kind_map.get(field_name, field_name)
        test(f"Declaration '{json_kind}' documented",
             f'`{json_kind}`' in spec or f'"{json_kind}"' in spec)


# ============================================================================
# Test 5: All pattern kinds from ast.zig are documented
# ============================================================================
def test_pattern_coverage():
    print("\n--- Test 5: Pattern kind coverage ---")
    spec = read_spec()
    ast_src = read_source(AST_ZIG)

    pat_match = re.search(r'pub const Pattern = union\(enum\) \{(.*?)\n\s*pub fn span',
                          ast_src, re.DOTALL)
    assert pat_match, "Could not find Pattern union in ast.zig"

    pat_kinds = re.findall(r'^\s+(\w+):', pat_match.group(1), re.MULTILINE)

    for kind in pat_kinds:
        test(f"Pattern '{kind}' documented",
             f'`{kind}`' in spec or f'"{kind}"' in spec)


# ============================================================================
# Test 6: All type expression kinds from ast.zig are documented
# ============================================================================
def test_type_expr_coverage():
    print("\n--- Test 6: TypeExpr kind coverage ---")
    spec = read_spec()
    ast_src = read_source(AST_ZIG)

    te_match = re.search(r'pub const TypeExpr = union\(enum\) \{(.*?)\n\s*pub fn span',
                         ast_src, re.DOTALL)
    assert te_match, "Could not find TypeExpr union in ast.zig"

    te_kinds = re.findall(r'^\s+(\w+):', te_match.group(1), re.MULTILINE)

    for kind in te_kinds:
        test(f"TypeExpr '{kind}' documented",
             f'`{kind}`' in spec or f'"{kind}"' in spec)


# ============================================================================
# Test 7: All resolved type kinds from types.zig are documented
# ============================================================================
def test_resolved_type_coverage():
    print("\n--- Test 7: Resolved Type kind coverage ---")
    spec = read_spec()
    types_src = read_source(TYPES_ZIG)

    type_match = re.search(r'pub const Type = union\(enum\) \{(.*?)\n\s*pub fn eql',
                           types_src, re.DOTALL)
    assert type_match, "Could not find Type union in types.zig"

    type_fields = re.findall(r'^\s+(\w+):', type_match.group(1), re.MULTILINE)

    # Map types.zig field names to JSON kind names in the spec
    kind_map = {
        'primitive': 'primitive',
        'array': 'array',
        'slice': 'slice',
        'tuple': 'tuple',
        'optional': 'optional',
        'result': 'result',
        'function': 'function',
        'reference': 'reference',
        'struct_': 'struct',
        'enum_': 'enum',
        'trait_': 'trait',
        'type_var': 'type_var',
        'applied': 'applied',
        'associated_type_ref': 'associated_type_ref',
        'rc': 'rc',
        'weak_rc': 'weak_rc',
        'arc': 'arc',
        'weak_arc': 'weak_arc',
        'cell': 'cell',
        'range': 'range',
        'context_error': 'context_error',
        'list': 'list',
        'map': 'map',
        'set': 'set',
        'string_data': 'string_data',
        'file': 'file',
        'io_error': 'io_error',
        'stdout_handle': 'stdout_handle',
        'stderr_handle': 'stderr_handle',
        'stdin_handle': 'stdin_handle',
        'path': 'path',
        'buf_reader': 'buf_reader',
        'buf_writer': 'buf_writer',
        'extern_type': 'extern_type',
        'cptr': 'cptr',
        'copt_ptr': 'copt_ptr',
        'cstr': 'cstr',
        'cstr_owned': 'cstr_owned',
        'extern_fn': 'extern_fn',
        'void_': 'void',
        'never': 'never',
        'unknown': 'unknown',
        'error_type': 'error',
    }

    for field_name in type_fields:
        json_kind = kind_map.get(field_name, field_name)
        test(f"Resolved type '{json_kind}' documented",
             f'"kind": "{json_kind}"' in spec or f'`{json_kind}`' in spec
             or f'"kind":"{json_kind}"' in spec
             or f'"{json_kind}"' in spec,
             f"types.zig field: {field_name}")


# ============================================================================
# Test 8: Sample JSON in spec is valid JSON
# ============================================================================
def test_sample_json_parseable():
    print("\n--- Test 8: Sample JSON parseability ---")
    spec = read_spec()

    # Extract JSON blocks from the spec
    json_blocks = re.findall(r'```json\n(.*?)```', spec, re.DOTALL)
    test("Spec contains JSON examples", len(json_blocks) > 0,
         f"Found {len(json_blocks)} JSON blocks")

    parseable_count = 0
    # Try to parse each JSON block - some are fragments with ... or <TypedExpr>
    for i, block in enumerate(json_blocks):
        block = block.strip()
        # Skip blocks with placeholders
        if '<' in block or '...' in block or '"..."' in block:
            continue
        # Skip comment-only blocks
        if block.startswith('//'):
            continue
        try:
            json.loads(block)
            parseable_count += 1
        except json.JSONDecodeError as e:
            # Only report if it looks like it should be valid (no placeholders)
            pass

    test("At least 5 valid JSON examples", parseable_count >= 5,
         f"Found {parseable_count} parseable JSON blocks")


# ============================================================================
# Test 9: dump-ast JSON kinds match spec-documented kinds
# ============================================================================
def test_dump_ast_kinds_covered():
    print("\n--- Test 9: dump-ast output kinds covered by spec ---")
    spec = read_spec()

    if not KLAR_BIN.exists():
        print("  SKIP: klar binary not found, skipping dump-ast validation")
        return

    # Pick a diverse set of test files that exercise many node types
    test_files = [
        "arith.kl",                  # binary, literal, return
        "array.kl",                  # array_literal, index
        "struct_method.kl",          # struct_decl, struct_literal, field, method_call
        "enum_struct_payload.kl",    # enum_decl, enum_literal, match
        "closure_simple.kl",         # closure
        "generic_struct.kl",         # type_params, generic_apply
        "trait_default.kl",          # trait_decl, impl_decl
        "optional_some.kl",          # optional, postfix
        "for_range.kl",              # for_loop, range
        "tuple.kl",                  # tuple_literal, tuple type
        "unsigned_widen_cast.kl",    # type_cast
        "string_interp.kl",         # interpolated_string
        "match_tuple_element.kl",   # match_stmt
    ]

    all_kinds = set()
    files_processed = 0

    for test_file in test_files:
        filepath = TEST_NATIVE_DIR / test_file
        if not filepath.exists():
            continue

        try:
            result = subprocess.run(
                [str(KLAR_BIN), "dump-ast", str(filepath)],
                capture_output=True, text=True, timeout=10
            )
            if result.returncode != 0:
                continue

            ast_json = json.loads(result.stdout)
            files_processed += 1

            # Recursively collect all "kind" values
            def collect_kinds(obj):
                if isinstance(obj, dict):
                    if "kind" in obj:
                        all_kinds.add(obj["kind"])
                    for v in obj.values():
                        collect_kinds(v)
                elif isinstance(obj, list):
                    for item in obj:
                        collect_kinds(item)

            collect_kinds(ast_json)
        except (subprocess.TimeoutExpired, json.JSONDecodeError, OSError):
            continue

    test("Processed at least 5 test files", files_processed >= 5,
         f"Processed {files_processed}")
    test("Found diverse AST node kinds", len(all_kinds) >= 10,
         f"Found {len(all_kinds)} kinds: {sorted(all_kinds)}")

    # Verify each kind from dump-ast is documented in the spec
    for kind in sorted(all_kinds):
        test(f"dump-ast kind '{kind}' in spec",
             f'`{kind}`' in spec or f'"{kind}"' in spec)


# ============================================================================
# Test 10: Typed AST additions are unambiguous
# ============================================================================
def test_unambiguous_format():
    print("\n--- Test 10: Format unambiguity ---")
    spec = read_spec()

    # Every expression should document resolved_type
    test("resolved_type documented for expressions",
         "resolved_type" in spec and spec.count("resolved_type") >= 10)

    # Key typed fields should be documented
    typed_fields = [
        "resolved_type",
        "resolved_callee",
        "resolved_dispatch",
        "resolved_method",
        "resolved_var_type",
        "resolved_element_type",
        "resolved_bindings",
        "resolved_signature",
        "resolved_fields",
        "resolved_variants",
        "resolved_methods",
        "resolved_target",
        "resolved_trait",
        "resolved_associated_types",
        "comptime_value",
    ]
    for field in typed_fields:
        test(f"Typed field '{field}' documented",
             f'"{field}"' in spec or f'`{field}`' in spec)

    # ResolvedType always has a "kind" discriminator
    test("ResolvedType uses 'kind' discriminator",
         '"kind"' in spec and "discriminator" in spec.lower())

    # Monomorphizations section documented
    test("monomorphizations section documented",
         "monomorphizations" in spec)

    # Trait impls section documented
    test("trait_impls section documented",
         "trait_impls" in spec)


# ============================================================================
# Test 11: Complete example is valid JSON
# ============================================================================
def test_complete_example():
    print("\n--- Test 11: Complete example validity ---")
    spec = read_spec()

    # Find the complete example section
    example_match = re.search(
        r'## Complete Example.*?```json\n(.*?)```',
        spec, re.DOTALL
    )
    test("Complete example section exists", example_match is not None)

    if example_match:
        example_json = example_match.group(1).strip()
        # Replace the "..." placeholder
        example_json = example_json.replace('"..."', '""')
        example_json = example_json.replace('"statements": ["..."]',
                                           '"statements": []')
        try:
            parsed = json.loads(example_json)
            test("Complete example is valid JSON", True)

            # Verify structure
            test("Has format field",
                 parsed.get("format") == "typed-ast")
            test("Has version field",
                 parsed.get("version") == 1)
            test("Has declarations array",
                 isinstance(parsed.get("declarations"), list))
            test("Has monomorphizations",
                 isinstance(parsed.get("monomorphizations"), dict))
            test("Monomorphizations has functions",
                 isinstance(parsed.get("monomorphizations", {}).get("functions"), list))
            test("Monomorphizations has structs",
                 isinstance(parsed.get("monomorphizations", {}).get("structs"), list))
            test("Monomorphizations has enums",
                 isinstance(parsed.get("monomorphizations", {}).get("enums"), list))
            test("Monomorphizations has methods",
                 isinstance(parsed.get("monomorphizations", {}).get("methods"), list))

            # Verify at least one declaration has resolved_type info
            has_resolved = False
            def check_resolved(obj):
                nonlocal has_resolved
                if isinstance(obj, dict):
                    if "resolved_type" in obj or "resolved_signature" in obj:
                        has_resolved = True
                    for v in obj.values():
                        check_resolved(v)
                elif isinstance(obj, list):
                    for item in obj:
                        check_resolved(item)
            check_resolved(parsed)
            test("Example contains resolved type info", has_resolved)

        except json.JSONDecodeError as e:
            test("Complete example is valid JSON", False, str(e))


# ============================================================================
# Test 12: Node type coverage table is complete
# ============================================================================
def test_coverage_table():
    print("\n--- Test 12: Coverage table completeness ---")
    spec = read_spec()

    # Check the coverage table mentions all expression counts
    # The spec claims 18+ expression kinds; ast.zig has ~23 (including some aliases)
    ast_src = read_source(AST_ZIG)

    expr_match = re.search(r'pub const Expr = union\(enum\) \{(.*?)\n\s*pub fn span',
                           ast_src, re.DOTALL)
    expr_kinds = re.findall(r'^\s+(\w+):', expr_match.group(1), re.MULTILINE)

    stmt_match = re.search(r'pub const Stmt = union\(enum\) \{(.*?)\n\s*pub fn span',
                           ast_src, re.DOTALL)
    stmt_kinds = re.findall(r'^\s+(\w+):', stmt_match.group(1), re.MULTILINE)

    decl_match = re.search(r'pub const Decl = union\(enum\) \{(.*?)\n\s*pub fn span',
                           ast_src, re.DOTALL)
    decl_kinds = re.findall(r'^\s+(\w+):', decl_match.group(1), re.MULTILINE)

    pat_match = re.search(r'pub const Pattern = union\(enum\) \{(.*?)\n\s*pub fn span',
                          ast_src, re.DOTALL)
    pat_kinds = re.findall(r'^\s+(\w+):', pat_match.group(1), re.MULTILINE)

    te_match = re.search(r'pub const TypeExpr = union\(enum\) \{(.*?)\n\s*pub fn span',
                         ast_src, re.DOTALL)
    te_kinds = re.findall(r'^\s+(\w+):', te_match.group(1), re.MULTILINE)

    # The coverage table should mention each kind
    test(f"Coverage table has all {len(expr_kinds)} expression kinds",
         all(k in spec or k.rstrip('_') in spec for k in expr_kinds))
    test(f"Coverage table has all {len(stmt_kinds)} statement kinds",
         all(k in spec for k in stmt_kinds))
    test(f"Coverage table has all {len(decl_kinds)} declaration kinds",
         all(k in spec or k.replace('_decl', '') in spec for k in decl_kinds))
    test(f"Coverage table has all {len(pat_kinds)} pattern kinds",
         all(k in spec for k in pat_kinds))
    test(f"Coverage table has all {len(te_kinds)} type expr kinds",
         all(k in spec or k.replace('_', '') in spec for k in te_kinds))


# ============================================================================
# Main
# ============================================================================
def main():
    print("=== Typed AST Format Validation ===")
    print(f"Spec: {SPEC_PATH}")
    print(f"AST source: {AST_ZIG}")
    print(f"Types source: {TYPES_ZIG}")

    test_spec_structure()
    test_expression_coverage()
    test_statement_coverage()
    test_declaration_coverage()
    test_pattern_coverage()
    test_type_expr_coverage()
    test_resolved_type_coverage()
    test_sample_json_parseable()
    test_dump_ast_kinds_covered()
    test_unambiguous_format()
    test_complete_example()
    test_coverage_table()

    print(f"\n=== Results: {passed} passed, {failed} failed ===")
    if errors:
        print("\nFailures:")
        for e in errors:
            print(e)

    return 0 if failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
