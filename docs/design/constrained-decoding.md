# Constrained Decoding Evaluation

> **Status:** Evaluated — **No-go** for grammar-constrained decoding. Klar's REPL verification loop + `klar check` already covers the practical need.
> **Decision date:** 2026-03-15

---

## Motivation

Constrained decoding forces an LLM to produce only tokens that conform to a formal grammar specification. Instead of generating free-form text and hoping it parses, the inference engine masks out invalid next tokens at each step, guaranteeing syntactically valid output.

This is relevant to Klar because Klar is designed as an "AI-native" language — optimized for AI code generation. The question is: should we provide a formal grammar spec so LLM inference engines can constrain their output to valid Klar syntax?

---

## Current State in Klar

Klar already has two mechanisms for AI-assisted code validation:

### 1. REPL Verification Loop

```
AI generates code → AI tests in REPL → AI fixes internally → User gets working code
```

The REPL enables AI assistants to self-verify generated code before presenting it to users. This catches not just syntax errors but also type errors, scope errors, and semantic issues that no grammar spec can detect.

### 2. `klar check` — Fast Type Checking

```bash
klar check file.kl    # Full type check in ~85ms for 10K lines
```

`klar check` validates syntax, types, scoping, trait bounds, and ownership — far beyond what grammar-constrained decoding provides. With the Phase 15 optimizations, it runs in under 100ms for typical files.

---

## Grammar Spec Formats Evaluated

### Format A: GBNF (GGML BNF)

Used by llama.cpp and derivatives (Ollama, LM Studio, text-generation-webui).

```gbnf
root          ::= declaration*
declaration   ::= fn-decl | struct-decl | enum-decl | import-decl | meta-decl
fn-decl       ::= "pub"? "async"? "fn" ident generic-params? "(" params ")" "->" type block
ident         ::= [a-zA-Z_] [a-zA-Z0-9_]*
generic-params ::= "#[" type-param ("," type-param)* "]"
block         ::= "{" statement* "}"
...
```

| Dimension | Assessment |
|-----------|------------|
| **Ecosystem** | llama.cpp, Ollama, LM Studio, koboldcpp |
| **Expressiveness** | Context-free grammar only. Cannot express Klar's struct literal context sensitivity or string interpolation validation. |
| **Maintenance** | Must stay in sync with parser.zig (~4200 lines). No automated extraction possible — grammar is procedurally encoded. |
| **Target users** | Local LLM users running small models that need syntax guardrails |

### Format B: JSON Schema (Structured Outputs)

Used by OpenAI, Anthropic (tool use), and cloud inference APIs.

```json
{
  "type": "object",
  "properties": {
    "code": { "type": "string", "description": "Valid Klar source code" }
  }
}
```

| Dimension | Assessment |
|-----------|------------|
| **Ecosystem** | OpenAI structured outputs, Anthropic tool use, Google Gemini |
| **Expressiveness** | Cannot express programming language grammars. JSON Schema constrains JSON structure, not arbitrary text syntax. |
| **Maintenance** | N/A — fundamentally wrong tool for this job |
| **Target users** | API users wanting structured responses (not code grammar) |

**Verdict:** JSON Schema is categorically unsuitable for constraining code generation. It structures JSON output, not programming language syntax.

### Format C: Regex / CFG (Outlines, vLLM)

Used by Outlines (Hugging Face), vLLM, and SGLang.

```python
# Outlines CFG format
klar_grammar = r"""
start: declaration+
declaration: fn_decl | struct_decl | enum_decl
fn_decl: "fn" IDENT "(" params ")" "->" type block
...
"""
```

| Dimension | Assessment |
|-----------|------------|
| **Ecosystem** | Outlines, vLLM, SGLang, TensorRT-LLM |
| **Expressiveness** | Full CFG (Lark grammar). Better than GBNF for some constructs, but same context-free limitations. |
| **Maintenance** | Same burden as GBNF — manual sync with parser.zig |
| **Target users** | Python ML engineers running local/self-hosted models |

### Format D: Custom Klar Grammar Spec

A bespoke format designed for Klar, potentially with semantic predicates.

| Dimension | Assessment |
|-----------|------------|
| **Ecosystem** | None — requires every tool to add Klar-specific support |
| **Expressiveness** | Could handle all of Klar's grammar including context-sensitive parts |
| **Maintenance** | Highest — both grammar spec and tool integrations to maintain |
| **Target users** | Nobody — no existing tool would consume it |

---

## Analysis Against Klar's Principles

### Principle 1: "No ambiguity. No surprises."

Klar's grammar is intentionally designed to be unambiguous for *humans and AI reading code*. The question is whether a formal grammar spec helps AI *write* code.

**Finding:** Klar's grammar has one significant context-sensitive construct — struct literal parsing depends on whether the parser is inside a condition context (`if`, `while`, `match`). This cannot be expressed in a context-free grammar:

```klar
// Context A: struct literal
let p: Point = Point { x: 1.0, y: 2.0 }

// Context B: block (no struct literals allowed in conditions)
if some_bool { println("yes") }
```

A grammar spec that allows struct literals everywhere would accept programs the real parser rejects, and vice versa. The spec would be a *lie* about what Klar accepts.

### Principle 2: "The code explains itself."

Grammar constraints ensure *syntactic* validity but say nothing about:
- Type correctness (`let x: i32 = "hello"` is grammatically valid)
- Scope validity (`return x` where `x` is undefined)
- Trait bounds (`fn max#[T: Ordered](a: T, b: T)` — grammar can't check that `T` implements `Ordered`)
- Ownership violations (use-after-move)

For an AI generating Klar code, syntactic validity is the *easiest* problem. The hard problems are all semantic — and `klar check` catches those in 85ms.

### Principle 3: "Explicitness earns its characters."

A grammar spec must earn its maintenance cost. Klar's parser is ~4200 lines of recursive descent with:
- 15 operator precedence levels
- String interpolation validation
- Context-sensitive struct literal handling
- Modifier ordering enforcement
- Generic type parameter parsing

Manually maintaining a parallel grammar spec (estimated ~800–1200 lines of GBNF) that stays in sync with parser.zig would be a significant ongoing burden.

---

## Who Would Use This?

| User segment | Grammar spec useful? | Why / why not |
|---|---|---|
| Claude Code users | No | Claude generates Klar, runs `klar check`, iterates. REPL loop is faster and catches more. |
| Cloud API users (OpenAI, Anthropic) | No | Cloud APIs don't support custom grammar constraints. Structured outputs use JSON Schema, not language grammars. |
| Local LLM users (llama.cpp) | Marginally | Small local models benefit most from grammar constraints, but these models are also least likely to generate *semantically* valid Klar code even with correct syntax. |
| IDE autocomplete | No | LSP-style completion uses the actual parser/checker, not a grammar spec. |
| Klar language documentation | Marginally | A formal grammar is useful as reference, but `docs/` and the parser source serve this role. |

The primary beneficiaries would be users running small local LLMs (7B–13B parameters) via llama.cpp. But these models typically struggle with the *semantic* aspects of any programming language. Guaranteeing syntax while getting wrong types, scoping, and logic is not a meaningful improvement.

---

## Comparison: Grammar Constraint vs. Verification Loop

| Dimension | Grammar Constraint | REPL + `klar check` |
|---|---|---|
| **Syntax errors caught** | 100% (by construction) | 100% (post-hoc) |
| **Type errors caught** | 0% | 100% |
| **Scope errors caught** | 0% | 100% |
| **Ownership errors caught** | 0% | 100% |
| **Latency** | 0ms (inline with generation) | ~85ms per check |
| **Works with cloud LLMs** | No (Claude, GPT-4 don't support custom grammars) | Yes |
| **Works with local LLMs** | Yes (llama.cpp, vLLM) | Yes |
| **Maintenance cost** | High (~1000 LOC grammar spec, manual sync) | Zero (uses the real compiler) |
| **Context-sensitive constructs** | Cannot express | Handled correctly |

The verification loop is strictly superior in every dimension except generation-time latency (0ms vs 85ms). For interactive use, 85ms is imperceptible. For batch generation, the semantic guarantees of `klar check` far outweigh the latency.

---

## Decision

**No-go.** Klar will not provide a grammar spec for constrained decoding.

### Rationale

1. **Wrong level of abstraction.** Syntax is the easiest part of code generation. Constraining syntax while allowing semantic errors gives a false sense of correctness. The REPL verification loop catches *all* error categories.

2. **Ecosystem mismatch.** The major LLM providers (Claude, GPT-4, Gemini) that are most capable of generating correct Klar code don't support custom grammar constraints. The local LLM tools that do support grammar constraints run models too small to reliably generate semantically valid Klar.

3. **Context-sensitive grammar.** Klar's struct literal parsing is context-sensitive. A context-free grammar spec would either over-accept or under-accept programs, making it an inaccurate specification of the language.

4. **Maintenance burden.** Keeping a ~1000-line grammar spec in sync with a ~4200-line procedural parser is error-prone and provides little value. Every parser change would require a corresponding grammar update.

5. **Existing solution is better.** `klar check` runs in 85ms and validates syntax, types, scoping, trait bounds, and ownership. It uses the *real* parser, so it's always correct by definition.

### What Could Change This Decision

- **Cloud LLM APIs adopt custom grammar support.** If Claude or GPT-4 added GBNF-style grammar constraints, the ecosystem argument weakens significantly.
- **Grammar-guided *semantic* checking.** If constrained decoding evolved beyond CFGs to include type-level constraints (e.g., dependent types in the grammar), the value proposition changes.
- **Klar adoption in local LLM ecosystem.** If a significant user base runs Klar code generation with llama.cpp and requests grammar support, we'd reconsider.

### Instead

Continue investing in the REPL verification loop and `klar check` performance. These provide strictly more value than grammar constraints at lower maintenance cost. Consider these focused improvements instead:

1. **`klar check --json`** — machine-readable error output for AI parsing (already partially done via structured errors)
2. **`klar check --fix`** — auto-fix common errors (like adding missing type annotations)
3. **REPL batch mode** — accept multiple statements from stdin for faster AI iteration
