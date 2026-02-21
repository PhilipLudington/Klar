#!/usr/bin/env python3
"""Normalize AST JSON from Zig dump-ast and selfhost parser for parity comparison.

Known structural differences handled:
  1. Operator naming: Zig "not_eq" vs selfhost "neq" (and similar)
  2. Extra fields: selfhost adds "fallible" on type_cast nodes
  3. Trait methods: selfhost emits empty methods:[] for trait decls
  4. Impl method kind: selfhost may include/omit "kind":"function" on impl methods

Usage:
  python3 normalize-ast.py <zig_json_file> <selfhost_json_file>
  python3 normalize-ast.py --str <zig_json_string> <selfhost_json_string>

Exit 0 if normalized forms match, exit 1 with diff if different.
"""

import json
import sys


# ── Operator name mapping (Zig → canonical) ────────────────────────────
# The canonical form uses the Zig names; selfhost names are mapped to them.
SELFHOST_OP_MAP = {
    "neq": "not_eq",
}


def normalize(obj):
    """Recursively normalize an AST JSON object to canonical form."""
    if isinstance(obj, list):
        return [normalize(item) for item in obj]

    if not isinstance(obj, dict):
        return obj

    result = {}
    for key, value in obj.items():
        # ── 1. Normalize operator names ──────────────────────────
        if key == "op" and isinstance(value, str):
            result[key] = SELFHOST_OP_MAP.get(value, value)
        # ── 2. Strip extra "fallible" field from type_cast nodes ─
        elif key == "fallible" and obj.get("kind") == "type_cast":
            continue  # skip this field entirely
        else:
            result[key] = normalize(value)

    # ── 3. Trait methods: selfhost emits stub methods:[] ────────
    # The selfhost parser intentionally stubs trait method signatures,
    # emitting an empty methods array. Zig emits the full bodyless
    # signatures. Normalize by clearing methods on trait_decl nodes.
    if result.get("kind") == "trait_decl" and "methods" in result:
        result["methods"] = []

    # ── 4. Impl method declarations: ensure "kind":"function" present ──
    # Zig impl methods may omit "kind" field; selfhost includes it.
    # Normalize: if a dict has "name", "params", "return_type", "body" but
    # no "kind", add "kind":"function".
    if (
        "name" in result
        and "params" in result
        and "return_type" in result
        and "body" in result
        and "kind" not in result
    ):
        result["kind"] = "function"

    return result


def json_dumps_sorted(obj):
    """Deterministic JSON serialization for comparison."""
    return json.dumps(obj, sort_keys=True, indent=2)


def main():
    if len(sys.argv) == 4 and sys.argv[1] == "--str":
        zig_json = sys.argv[2]
        self_json = sys.argv[3]
    elif len(sys.argv) == 3:
        with open(sys.argv[1]) as f:
            zig_json = f.read()
        with open(sys.argv[2]) as f:
            self_json = f.read()
    else:
        print(f"Usage: {sys.argv[0]} <zig_file> <selfhost_file>", file=sys.stderr)
        print(f"       {sys.argv[0]} --str <zig_json> <selfhost_json>", file=sys.stderr)
        sys.exit(2)

    try:
        zig_ast = json.loads(zig_json)
    except json.JSONDecodeError as e:
        print(f"Error parsing Zig JSON: {e}", file=sys.stderr)
        sys.exit(2)

    try:
        self_ast = json.loads(self_json)
    except json.JSONDecodeError as e:
        print(f"Error parsing selfhost JSON: {e}", file=sys.stderr)
        sys.exit(2)

    zig_norm = normalize(zig_ast)
    self_norm = normalize(self_ast)

    zig_str = json_dumps_sorted(zig_norm)
    self_str = json_dumps_sorted(self_norm)

    if zig_str == self_str:
        sys.exit(0)
    else:
        # Print a useful diff
        zig_lines = zig_str.splitlines()
        self_lines = self_str.splitlines()

        # Find first difference
        for i, (a, b) in enumerate(zip(zig_lines, self_lines)):
            if a != b:
                print(f"First difference at line {i + 1}:", file=sys.stderr)
                print(f"  zig:     {a.strip()}", file=sys.stderr)
                print(f"  selfhost:{b.strip()}", file=sys.stderr)
                break
        else:
            if len(zig_lines) != len(self_lines):
                shorter = min(len(zig_lines), len(self_lines))
                print(
                    f"Outputs differ in length: zig={len(zig_lines)} selfhost={len(self_lines)}",
                    file=sys.stderr,
                )
                print(f"  Extra content starts at line {shorter + 1}", file=sys.stderr)

        sys.exit(1)


if __name__ == "__main__":
    main()
