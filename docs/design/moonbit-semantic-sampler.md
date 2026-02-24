# MoonBit Semantic Sampler: Opportunities for Klar

> **Source:** "MoonBit: Explore the Design of an AI-Friendly Programming Language" (LLM4Code 2024, ICSE).
> By Haoxiang Fei, Yu Zhang, Hongbo Zhang, Yanlin Wang, Qing Liu (IDEA / Sun Yat-sen University).
> Both MoonBit and Klar are AI-native languages. These are the opportunities where MoonBit's research can strengthen Klar.

---

## What MoonBit's Paper Proposes

### The Problem

LLMs generating code face three challenges:
1. Cannot understand the full project context and dependencies
2. Generated code requires human verification and correction
3. No assurance of basic requirements like syntactic correctness or type safety

### The Solution: Semantic Sampler

A real-time, semantics-based sampler that guides LLM token generation using two layers:

**Local Sampling (Syntax):**
- Maintains a speculation buffer of recently generated tokens
- As each token is produced, checks syntactic validity against the parser
- On invalid token: backtracks and communicates valid continuations to the model
- Cost: nearly zero — parser state machines are fast

**Global Sampling (Semantics):**
- After syntactically valid tokens accumulate, runs the type checker
- Catches type errors, undefined variables, wrong argument counts
- Reports valid alternatives when a token fails type checking
- Cost: ~3% performance overhead on inference latency (~0.86s per suggestion)

**Result:** Significant improvement in compilation rates with minimal latency penalty.

### Language Design Decisions That Enable This

MoonBit made specific language design choices to make the sampler effective:

| Decision | Why It Helps AI |
|----------|----------------|
| **Mandatory top-level type signatures** | Sampler knows expected types at every call site |
| **Flattened design** (no deep nesting) | Reduces KV-cache misses in transformer models |
| **Structural interfaces** (top-level methods) | Linear code generation without back-and-forth |
| **Simple, regular grammar** | Local sampling state machine is small and fast |

---

## Klar Audit: Where We Stand

| Aspect | Klar Today | MoonBit Ideal | Gap |
|--------|-----------|---------------|-----|
| Variable type annotations | **Mandatory** | Mandatory | None |
| Function return types | **Optional** | Mandatory | **High** |
| impl block nesting | **2 levels** (impl { fn { } }) | Flat (top-level) | Medium |
| Incremental type checking | **None** | Real-time partial checking | **High** |
| Grammar machine-readability | No spec file | Parseable grammar spec | Medium |
| Keyword-based syntax | **Yes** (`and`/`or`/`not`) | Yes | None |
| Explicit type conversions | **Yes** (`.as#[T]`, `.to#[T]`) | Yes | None |

Klar already has strong foundations: mandatory variable types, keyword-based operators, explicit conversions, and no implicit behavior. The gaps are in **function signatures**, **incremental checking**, and **tooling for AI integration**.

---

## Opportunity 1: Mandatory Function Return Types

**Inspiration:** MoonBit requires all top-level definitions to have explicit type signatures. This gives the semantic sampler deterministic type information at every function boundary.

**Current Klar behavior:** Return types are optional (`parser.zig:2446-2449`). A function without `-> T` is treated as returning `void`.

```klar
// Current: both are valid
fn greet(name: string) -> void { println("Hello " + name) }
fn greet(name: string) -> void { println("Hello " + name) }
```

**Proposed change:** Require `-> T` on all function declarations (including `-> void`).

```klar
// After: explicit return type always required
fn greet(name: string) -> void { println("Hello " + name) }
fn add(a: i32, b: i32) -> i32 { return a + b }
```

### Why This Matters

- Aligns with Klar's "no ambiguity" philosophy — every function self-documents its contract
- AI tools can determine the expected return type without running the type checker
- Enables the semantic sampler to constrain return expressions during generation
- Already the convention in all Klar examples and tests — this just enforces it

### Implementation

- **Parser** (`parser.zig:2446`): Change optional `-> Type` to required. Error if missing.
- **Checker**: No change (already handles explicit return types)
- **Formatter**: Already formats return types when present
- **Migration**: Grep for functions without `->` in test suite and fix

### Scope

Low effort. The parser change is ~5 lines. The test suite already uses explicit return types everywhere. This is a convention-to-rule upgrade.

---

## Opportunity 2: Semantic Sampler Server (`klar assist`)

**Inspiration:** MoonBit's local + global sampling architecture that constrains LLM output in real time.

**Klar Design:** Expose Klar's parser and type checker as a JSON-RPC server that AI tools can query during code generation.

```bash
# Start the semantic sampler server
klar assist --port 9100

# Or use stdio for tool integration (like LSP)
klar assist --stdio
```

### Architecture

```
┌─────────────┐   JSON-RPC    ┌──────────────────────────────┐
│ LLM / AI    │ ◄────────────►│  klar assist                 │
│ Tool        │               │  ┌────────────────────────┐  │
│ (Copilot,   │  valid_tokens │  │ Local: Parser State    │  │
│  Cursor,    │ ◄─────────────│  │ (syntax validation)    │  │
│  Claude)    │               │  └────────────────────────┘  │
│             │  type_check   │  ┌────────────────────────┐  │
│             │ ◄─────────────│  │ Global: Type Checker   │  │
│             │               │  │ (semantic validation)  │  │
└─────────────┘               │  └────────────────────────┘  │
                              └──────────────────────────────┘
```

### Protocol

**Request: `validate_token`** (Local Sampling)

Given partial source code and a candidate next token, return whether it's syntactically valid and suggest alternatives if not.

```json
{
  "method": "validate_token",
  "params": {
    "source": "fn add(a: i32, b: i32) -> i32 {\n    return a ",
    "candidate": "+"
  }
}
```

```json
{
  "result": {
    "valid": true,
    "alternatives": ["+", "-", "*", "/", "+%", "+|", "+?", "%", "==", "!=", ">", "<"]
  }
}
```

**Request: `check_partial`** (Global Sampling)

Type-check a partial code snippet up to a cursor position.

```json
{
  "method": "check_partial",
  "params": {
    "source": "fn add(a: i32, b: string) -> i32 {\n    return a + b\n}",
    "cursor_line": 2
  }
}
```

```json
{
  "result": {
    "errors": [
      {
        "line": 2,
        "message": "binary operator '+' not defined for types 'i32' and 'string'",
        "expected_type": "i32",
        "actual_type": "string"
      }
    ],
    "scope": {
      "a": "i32",
      "b": "string"
    }
  }
}
```

**Request: `completions_at`** (Token Suggestion)

Given a cursor position, return valid completions with their types.

```json
{
  "method": "completions_at",
  "params": {
    "source": "let p: Point = Point { x: 1.0, y: 2.0 }\np.",
    "cursor_line": 2,
    "cursor_col": 2
  }
}
```

```json
{
  "result": {
    "completions": [
      { "name": "x", "type": "f64", "kind": "field" },
      { "name": "y", "type": "f64", "kind": "field" },
      { "name": "distance", "type": "fn() -> f64", "kind": "method" }
    ]
  }
}
```

### Implementation Phases

**Phase 1: Local Sampling (Parser-based)**
- Expose parser state after consuming partial input
- Return set of valid next token kinds
- Fast — no type checking needed

**Phase 2: Global Sampling (Type-checker-based)**
- Add incremental/partial mode to type checker (see Opportunity 3)
- Return type errors and scope information
- Return expected type at cursor position

**Phase 3: AI Tool Integration**
- VS Code extension uses `klar assist` for enhanced completions
- Claude Code / Cursor integration via stdio protocol
- Constrained decoding plugin for local LLM inference

### Dependencies

- Opportunity 3 (Incremental Type Checking) for global sampling
- LSP work from Phase 4 Milestone 9 can share infrastructure

---

## Opportunity 3: Incremental Type Checking

**Inspiration:** MoonBit's global sampler runs the type checker on partial code in real time with ~0.86s latency per check.

**Current state:** Klar's `checkModule()` (`checker.zig:3543`) requires a complete, parsed AST. It cannot check partial or incomplete code.

**Proposed:** Add an incremental checking mode that can:
1. Accept incomplete source code (missing function bodies, partial expressions)
2. Report errors found so far
3. Report the expected type at a given cursor position
4. Track scope (what variables/functions are in scope and their types)

### Design

```zig
// New API surface in checker
pub fn checkPartial(self: *TypeChecker, source: []const u8, cursor: Position) PartialCheckResult {
    // 1. Parse with error recovery (continue past errors)
    // 2. Type-check all complete declarations
    // 3. For the declaration containing cursor, check up to cursor
    // 4. Return errors + scope + expected type at cursor
}

pub const PartialCheckResult = struct {
    errors: []const TypeError,
    scope: []const ScopeEntry,         // variables in scope at cursor
    expected_type: ?Type,              // what type is expected at cursor
    valid_identifiers: []const []const u8,  // names that would be valid here
};
```

### Implementation

- **Error-recovering parser**: Parser continues after errors instead of aborting. Produces a partial AST with holes.
- **Partial type checker**: Walks the partial AST, skipping error nodes, collecting type information up to the cursor.
- **Scope extraction**: At the cursor position, report all in-scope bindings with their types.

### Scope

High effort. This is a significant extension to both the parser and checker. It shares infrastructure with the LSP (which also needs error recovery and incremental checking).

### Phased Approach

1. **Phase 1**: Error-recovering parser (continue past first error, produce partial AST)
2. **Phase 2**: Scope extraction at cursor position (no partial type checking yet)
3. **Phase 3**: Incremental type checking of partial declarations
4. **Phase 4**: Expected-type inference at cursor (what type does the context demand?)

---

## Opportunity 4: Grammar Specification File (`grammar.json`)

**Inspiration:** MoonBit's local sampling works because the parser is simple and regular enough to run as a fast state machine during token generation. A machine-readable grammar lets AI tools do local sampling without a running Klar process.

**Klar Design:** Ship a `grammar.json` file that describes Klar's syntax in a format AI tools can load.

```json
{
  "version": "0.5.0",
  "keywords": ["fn", "let", "var", "struct", "enum", "trait", "impl", "if", "else", ...],
  "operators": {
    "binary": ["+", "-", "*", "/", "%", "+%", "+|", "+?", "==", "!=", ...],
    "unary": ["-", "not"],
    "assignment": ["="]
  },
  "precedence": [
    { "level": 1, "operators": ["or"], "assoc": "left" },
    { "level": 2, "operators": ["and"], "assoc": "left" },
    ...
  ],
  "type_syntax": {
    "primitives": ["i32", "i64", "f64", "bool", "string", "void"],
    "optional": "?T",
    "result": "Result#[T, E]",
    "array": "[T; N]",
    "function": "fn(Args) -> Ret"
  },
  "declaration_patterns": {
    "function": "fn NAME(PARAMS) -> TYPE { BODY }",
    "variable": "let NAME: TYPE = EXPR",
    "mutable": "var NAME: TYPE = EXPR",
    "struct": "struct NAME { FIELDS }",
    "enum": "enum NAME { VARIANTS }",
    "trait": "trait NAME { METHODS }",
    "impl": "impl TYPE { METHODS }",
    "impl_trait": "impl TYPE: TRAIT { METHODS }"
  }
}
```

### Why This Matters

- AI tools can do syntactic validation locally (no server needed)
- Grammar-constrained decoding libraries (like `outlines`, `guidance`) can load this directly
- Complements `spec.json` from the MEMORY.md milestone (Nanolang plan)
- Low effort, high utility

### Implementation

- Extract grammar rules from `parser.zig` into JSON
- Include operator precedence table from `ast.zig`
- Include all keyword tokens from `lexer.zig`
- Ship in repo root alongside `MEMORY.md`

---

## Opportunity 5: KV-Cache Friendly Syntax Audit

**Inspiration:** MoonBit's "flattened design" minimizes nesting to reduce KV-cache misses in transformer models. Structural interfaces put methods at the top level instead of nesting inside `impl` blocks.

**Klar's current nesting:**

```klar
// Current: methods nested inside impl blocks (2 levels deep)
impl Point {
    fn distance(self: Point) -> f64 {       // level 1: impl
        return sqrt(self.x * self.x)        // level 2: fn body
    }
}

impl Point: Display {
    fn to_string(self: Point) -> string {   // level 1: impl
        return "(" + self.x.to_string() + ")"  // level 2: fn body
    }
}
```

**MoonBit's approach (flat):**

```moonbit
// MoonBit: methods at top level, associated by type signature
fn Point::distance(self : Point) -> Double { ... }
fn Point::to_string(self : Point) -> String { ... }
```

### Assessment

Klar's `impl` blocks are already relatively flat — they're a single level of braces around method definitions. This is comparable to Rust, Go (`func (p Point) Distance()` is flat), and most languages.

**Recommendation: Do NOT flatten impl syntax.** The current syntax is:
- Familiar to developers coming from Rust/Swift/C#
- Groups related methods visually (readability benefit)
- Only 1 extra nesting level vs. flat
- Changing it would break every existing Klar program

Instead, focus on the **high-impact** opportunities (mandatory return types, semantic sampler) that don't require syntax changes.

### Minor Improvements Worth Considering

- **Allow `impl` blocks to be split**: Multiple `impl Point { ... }` blocks in the same file (already supported? needs verification)
- **Module-level doc comments**: Ensure each file can start with `///` doc comments for LLM context

---

## Priority and Dependencies

| # | Opportunity | Effort | Impact | Dependencies |
|---|-----------|--------|--------|-------------|
| 1 | Mandatory return types | **Low** | **High** | None (parser-only change) |
| 4 | Grammar spec (`grammar.json`) | **Low** | Medium | None (documentation) |
| 3 | Incremental type checking | **High** | **High** | Shares LSP infrastructure |
| 2 | Semantic sampler (`klar assist`) | **High** | **High** | Opportunity 3 for global sampling |
| 5 | Syntax audit | **None** | Low | N/A (recommendation: no change) |

### Suggested Order

1. **Mandatory return types** — Low effort, immediate AI benefit, aligns with Klar philosophy
2. **Grammar spec file** — Low effort, enables grammar-constrained decoding tools
3. **Incremental type checking** — Foundation for semantic sampler and LSP
4. **Semantic sampler server** — The marquee feature, builds on all previous work
5. ~~Syntax flattening~~ — Not recommended

---

## How This Fits With the Nanolang Plan

The Nanolang-inspired plan (Phase 5) and MoonBit-inspired improvements are complementary:

| Nanolang Plan | MoonBit Plan | Overlap |
|--------------|-------------|---------|
| MEMORY.md (LLM reference) | grammar.json (machine-readable spec) | Both improve AI generation context |
| Checked arithmetic (`+?`) | — | Unique to Nanolang |
| Inline test blocks | — | Unique to Nanolang |
| FFI sandbox | — | Unique to Nanolang |
| — | Mandatory return types | Unique to MoonBit |
| — | Semantic sampler server | Unique to MoonBit |
| — | Incremental type checking | Shared with LSP (Phase 4 M9) |

**Recommended combined order:**
1. MEMORY.md + grammar.json (documentation, no code changes)
2. Mandatory return types (tiny parser change)
3. Checked arithmetic (small feature)
4. Inline test blocks (medium feature)
5. Incremental type checking + semantic sampler (large, shared with LSP)
6. FFI sandbox (large, independent)

---

## References

- [MoonBit Paper (IEEE)](https://ieeexplore.ieee.org/document/10734654/)
- [MoonBit Paper (ACM)](https://dl.acm.org/doi/10.1145/3643795.3648376)
- [MoonBit Paper (Semantic Scholar)](https://www.semanticscholar.org/paper/MoonBit:-Explore-the-Design-of-an-AI-Friendly-Fei-Zhang/a282b93b7c57b24e268350a8b81bf70c529a590d)
- [MoonBit AI Blog Post](https://www.moonbitlang.com/blog/moonbit-ai)
- [MoonBit AI-Native Toolchain Blog](https://www.moonbitlang.com/blog/ai-coding)
