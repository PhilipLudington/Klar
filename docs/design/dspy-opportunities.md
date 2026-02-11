# DSPy: Opportunities for Klar

> **Source:** "DSPy: Compiling Declarative Language Model Calls into Self-Improving Pipelines" (ICLR 2024).
> By Omar Khattab et al. (Stanford NLP).
> DSPy's core insight — separate *what* an LM should do from *how* it does it, then compile — maps to concrete language features for an AI-native compiled language like Klar.

---

## What DSPy Proposes

### The Problem

LLM-powered applications are built with fragile, hand-crafted prompt templates. When the model, data, or pipeline changes, prompts break. There's no systematic way to compose, verify, or optimize LM calls.

### The Solution: Programming, Not Prompting

DSPy introduces three abstractions:

**1. Signatures** — Declarative input/output contracts for LM calls:
```python
# DSPy: what the LM should do, not how
"question -> answer"
"context: list[str], question: str -> answer: str, confidence: float"
```

**2. Modules** — Composable strategies that implement signatures:
```python
# DSPy: wraps a signature with a reasoning strategy
predict = dspy.ChainOfThought("question -> answer")
result = predict(question="What is 2+2?")
```

**3. Optimizers (Teleprompters)** — Compilers that automatically tune prompts, demonstrations, and weights to maximize a metric:
```python
# DSPy: compile the program against a metric + training data
optimizer = MIPROv2(metric=accuracy, auto="light")
optimized = optimizer.compile(program, trainset=data)
```

### Key Results

- Small models (Llama2-13b) become competitive with GPT-3.5 after compilation
- GPT-3.5 pipelines improve 25%+ over standard few-shot prompting
- 5-10 training examples are often sufficient

### Additional Concepts

**Assertions/Refinement** — Programmatic constraints on LM output with automatic retry:
```python
# DSPy: if constraint fails, backtrack and retry with feedback
dspy.Suggest(len(query) <= 100, "Query should be under 100 chars")
```

**Typed Predictors** — Enforce type constraints on LM output using Pydantic models, with automatic retry when types don't match.

**BestOfN / Refine** — Run a module N times, select the best output by reward function, or iteratively refine with LM-generated feedback.

---

## How This Maps to Klar

Klar is a compiled language, not an LLM orchestration framework. The opportunities are about embedding DSPy's *principles* into language features that make AI-generated Klar code better — not about making Klar a DSPy replacement.

The key principles that transfer:

| DSPy Principle | Klar Opportunity |
|---------------|-----------------|
| Signatures (declarative I/O contracts) | Type system as output schema; function contracts |
| Compilation (optimize against metrics) | `klar test` as a quality gate for AI-generated code |
| Assertions with retry | Test blocks with structured failure feedback for AI |
| Typed output enforcement | Export Klar types as JSON Schema for LLM structured output |
| Composable modules | Type-checked function pipelines |

---

## Opportunity 1: Type Schema Export (`klar schema`)

**Inspiration:** DSPy's Typed Predictors use Pydantic models to constrain LLM output. When the LLM produces output that doesn't match the type, it retries with the type error as feedback.

**Klar Design:** Add a `klar schema` command that exports Klar type definitions as JSON Schema, so LLM tools can use Klar types as structured output constraints.

```bash
# Export all public types from a file as JSON Schema
klar schema types.kl

# Export a specific type
klar schema types.kl --type WeatherReport
```

Given this Klar source:

```klar
enum Sentiment { Positive, Negative, Neutral }

struct WeatherReport {
    city: string,
    temperature: f64,
    condition: string,
    humidity: i32,
}

struct MovieReview {
    title: string,
    rating: i32,
    sentiment: Sentiment,
    summary: string,
}
```

`klar schema types.kl` would produce:

```json
{
  "Sentiment": {
    "type": "string",
    "enum": ["Positive", "Negative", "Neutral"]
  },
  "WeatherReport": {
    "type": "object",
    "properties": {
      "city": { "type": "string" },
      "temperature": { "type": "number" },
      "condition": { "type": "string" },
      "humidity": { "type": "integer" }
    },
    "required": ["city", "temperature", "condition", "humidity"]
  },
  "MovieReview": {
    "type": "object",
    "properties": {
      "title": { "type": "string" },
      "rating": { "type": "integer" },
      "sentiment": { "$ref": "#/Sentiment" },
      "summary": { "type": "string" }
    },
    "required": ["title", "rating", "sentiment", "summary"]
  }
}
```

### Why This Matters

- LLMs with structured output (OpenAI, Anthropic) can use these schemas directly
- Klar's type system becomes the source of truth for data shapes
- Eliminates manual schema maintenance — types and schemas stay in sync
- Enables DSPy-style typed prediction: generate structured data, validate against Klar types

### Implementation

- **New CLI command** in `src/main.zig`: `klar schema <file> [--type Name]`
- **Type walker**: traverse AST, extract public structs/enums, emit JSON Schema
- **Type mapping**: `i32`/`i64` → `integer`, `f64` → `number`, `bool` → `boolean`, `string` → `string`, `?T` → nullable, `List[T]` → array, `enum` → string enum or tagged union

### Effort: Low-Medium | Impact: Medium

---

## Opportunity 2: Structured Test Feedback for AI (`klar test --ai-feedback`)

**Inspiration:** DSPy's assertion mechanism provides structured feedback when constraints fail — the failing output, the error message, and instructions for correction are injected back into the LLM's prompt. The Refine module generates LM feedback on its own failures for iterative improvement.

**Klar Design:** When `klar test` finds failures, output structured JSON feedback designed for LLM consumption — enabling an assert → feedback → regenerate loop.

```bash
# Standard test output (human-readable)
klar test math.kl
# FAIL: test gcd - assert_eq failed: gcd(12, 8) = 6, expected 4

# AI feedback mode (structured for LLM consumption)
klar test math.kl --ai-feedback
```

AI feedback output:

```json
{
  "file": "math.kl",
  "tests": [
    {
      "name": "gcd",
      "status": "FAIL",
      "assertions": [
        {
          "type": "assert_eq",
          "passed": false,
          "call": "gcd(12, 8)",
          "expected": 4,
          "actual": 6,
          "location": "math.kl:10"
        },
        {
          "type": "assert_eq",
          "passed": true,
          "call": "gcd(7, 0)",
          "expected": 7,
          "actual": 7,
          "location": "math.kl:11"
        }
      ],
      "function_source": "fn gcd(a: i32, b: i32) -> i32 {\n    return a % b\n}",
      "feedback": "Function gcd(12, 8) returned 6 but expected 4. The function computes a % b directly instead of recursively computing GCD."
    }
  ]
}
```

### The AI Regeneration Loop

This enables a DSPy-style compile loop for AI-generated Klar code:

```
1. AI generates Klar code (function + test block)
2. `klar test --ai-feedback` runs tests
3. If failures: feed structured JSON back to AI
4. AI regenerates with failure context (like DSPy's backtracking)
5. Repeat until all tests pass (like DSPy's Refine with N attempts)
```

This is exactly DSPy's assertion → backtrack → retry pattern, but at the language level:
- **DSPy Signature** → Klar function signature (typed contract)
- **DSPy Assert** → Klar `test` block assertions
- **DSPy Backtrack** → AI re-generation with test feedback
- **DSPy Metric** → test pass rate

### Implementation

- Extend `klar test` (from Nanolang Milestone 3) with `--ai-feedback` flag
- Output JSON with: test name, assertion results, expected vs actual, source of failing function
- Optionally include compiler error messages in same format

### Dependencies

- Milestone 3 from Nanolang plan (inline test blocks)

### Effort: Low (once test blocks exist) | Impact: High

---

## Opportunity 3: Function Contracts (`@doc` annotations)

**Inspiration:** DSPy Signatures carry semantic descriptions — `dspy.InputField(desc="facts assumed to be true")` — that guide LLM behavior. Field names like `question` vs `sql_query` carry meaning. The description is part of the interface, not a comment.

**Klar Design:** Add structured `@doc` annotations to functions that describe behavior in a machine-readable way. These go beyond doc comments — they're part of the function's contract that tools can inspect.

```klar
/// Compute the greatest common divisor of two non-negative integers.
/// Returns the largest integer that divides both a and b.
@doc(
    precondition: "a >= 0 and b >= 0",
    postcondition: "result divides a and result divides b",
    examples: [
        { input: (12, 8), output: 4 },
        { input: (7, 0), output: 7 },
    ]
)
fn gcd(a: i32, b: i32) -> i32 {
    if b == 0 { return a }
    return gcd(b, a % b)
}
```

### What This Enables

- **AI code generation**: LLM reads `@doc` to understand what to generate
- **Test generation**: Preconditions, postconditions, and examples can be auto-converted to test assertions
- **Contract checking**: Preconditions can be checked at runtime in debug mode
- **Documentation**: `klar doc` extracts `@doc` into API docs
- **Schema export**: `klar schema` includes descriptions in JSON Schema

### Design: Lightweight Version

If full `@doc` annotations are too heavy, a simpler approach uses typed doc comments:

```klar
/// Compute the greatest common divisor.
/// @pre a >= 0 and b >= 0
/// @post result divides a and result divides b
/// @example gcd(12, 8) == 4
/// @example gcd(7, 0) == 7
fn gcd(a: i32, b: i32) -> i32 { ... }
```

The doc generator and `klar schema` can parse these structured comments.

### Effort: Medium | Impact: Medium

---

## Opportunity 4: Pipeline Operator for Function Composition

**Inspiration:** DSPy modules compose into pipelines where the output of one module feeds into the next. The type system ensures compatibility. DSPy's `forward()` method chains modules sequentially.

**Klar Design:** Add a pipeline operator `|>` for readable left-to-right function composition with compile-time type checking.

```klar
// Without pipeline: nested calls, read inside-out
let result: string = format(validate(parse(input)))

// With pipeline: linear, read left-to-right
let result: string = input |> parse |> validate |> format
```

### Semantics

`x |> f` is sugar for `f(x)`. The compiler type-checks that the left side's type matches the function's parameter type:

```klar
fn parse(s: string) -> Result[Data, ParseError] { ... }
fn validate(d: Data) -> Result[Data, ValidationError] { ... }
fn format(d: Data) -> string { ... }

// Type-checked pipeline: string → Result[Data, ParseError] → ...
// Compiler verifies each connection
let output: string = raw_input
    |> parse       // string → Result[Data, ParseError]
    |> validate    // Data → Result[Data, ValidationError]  (auto-unwrap with ?)
    |> format      // Data → string
```

### Integration with Error Handling

The pipeline operator composes naturally with `?` for error propagation:

```klar
fn process(input: string) -> Result[string, Error] {
    let result: string = input
        |> parse?       // propagate ParseError
        |> validate?    // propagate ValidationError
        |> format
    return Ok(result)
}
```

### Why This Matters for AI

- LLMs generate cleaner code — no deeply nested function calls
- Pipeline structure is linear and unambiguous (like DSPy's module chains)
- Type errors in pipelines have clear location (which step fails)
- Aligns with Klar's "parseable at a glance" philosophy

### Implementation

- **Parser**: `|>` as new binary operator with lowest precedence
- **Checker**: desugar `x |> f` to `f(x)`, type-check normally
- **All backends**: no special codegen needed (desugared before backends)

### Effort: Low | Impact: Medium

---

## Opportunity 5: Compile-Time Example Validation

**Inspiration:** DSPy's optimizers use training examples to improve pipeline quality. The compilation step validates that examples produce correct outputs. Demonstrations that don't meet the metric are rejected.

**Klar Design:** Allow `@example` annotations on functions that the compiler validates at compile time (via comptime) or test time.

```klar
fn celsius_to_fahrenheit(c: f64) -> f64 {
    return c * 1.8 + 32.0
}

test celsius_to_fahrenheit {
    // Standard assertions
    assert_eq(celsius_to_fahrenheit(0.0), 32.0)
    assert_eq(celsius_to_fahrenheit(100.0), 212.0)
}
```

This already exists with test blocks (Nanolang plan). The DSPy-inspired addition is **comptime example validation**:

```klar
// Comptime-checked examples: validated during compilation, not just test time
@example(celsius_to_fahrenheit(0.0) == 32.0)
@example(celsius_to_fahrenheit(100.0) == 212.0)
@example(celsius_to_fahrenheit(-40.0) == -40.0)
fn celsius_to_fahrenheit(c: f64) -> f64 {
    return c * 1.8 + 32.0
}
```

### Semantics

- `@example` expressions are evaluated at compile time (using Klar's existing comptime infrastructure)
- If any example fails, compilation fails with the failing case
- Examples serve as documentation, tests, AND compilation checks simultaneously
- Cheaper than test blocks (no separate test run needed)

### Relationship to Test Blocks

| Feature | `@example` | `test` block |
|---------|-----------|-------------|
| When checked | Compile time | Test time (`klar test`) |
| Complexity | Single expression | Arbitrary code |
| Use case | Simple I/O examples | Complex scenarios |
| Runtime cost | Zero | Test execution time |

They're complementary: `@example` for quick checks, `test` blocks for thorough testing.

### Effort: Low (comptime exists) | Impact: Medium

---

## Opportunity 6: Quality Metrics in Test Blocks

**Inspiration:** DSPy's optimizers maximize a user-defined metric function. Tests aren't just pass/fail — they return scores that drive optimization. The Refine module uses reward functions to iteratively improve output.

**Klar Design:** Allow test blocks to report quality scores, not just pass/fail. This enables AI tools to optimize code quality, not just correctness.

```klar
fn sort(arr: List[i32]) -> List[i32] {
    // ... sorting implementation
}

test sort {
    // Correctness (pass/fail)
    assert_eq(sort(List.from([3, 1, 2])), List.from([1, 2, 3]))
    assert_eq(sort(List.from([])), List.from([]))

    // Performance metric (scored, 0.0 to 1.0)
    let large: List[i32] = List.range(0, 10000).reverse()
    let start: i64 = time_ns()
    let _sorted: List[i32] = sort(large)
    let elapsed: i64 = time_ns() - start

    // Report metric: lower time = higher score
    metric("sort_10k_time", 1.0 - (elapsed.to_f64() / 1000000000.0))
}
```

```bash
# Standard test output
klar test sort.kl
# PASS: test sort (2/2 assertions passed)
# METRIC: sort_10k_time = 0.85

# AI feedback includes metrics
klar test sort.kl --ai-feedback
# { "metrics": { "sort_10k_time": 0.85 }, ... }
```

### How AI Uses This

1. AI generates a sorting function
2. `klar test --ai-feedback` reports correctness + metrics
3. If correctness passes but `sort_10k_time` < 0.9, AI tries a faster algorithm
4. Repeat until metrics meet thresholds

This is exactly DSPy's compile loop: generate → evaluate metric → optimize → repeat.

### Implementation

- Add `metric(name: string, score: f64)` builtin to test block scope
- `klar test` collects and reports metrics alongside pass/fail
- `--ai-feedback` includes metrics in structured JSON output
- Metrics are informational — they don't cause test failure by themselves

### Effort: Low | Impact: Medium

---

## Priority and Dependencies

| # | Opportunity | Effort | Impact | Dependencies |
|---|-----------|--------|--------|-------------|
| 1 | Type schema export (`klar schema`) | Low-Med | Medium | None |
| 2 | AI test feedback (`--ai-feedback`) | Low | **High** | Nanolang M3 (test blocks) |
| 4 | Pipeline operator (`\|>`) | Low | Medium | None |
| 5 | Comptime examples (`@example`) | Low | Medium | Comptime (exists) |
| 6 | Quality metrics in tests | Low | Medium | Nanolang M3 (test blocks) |
| 3 | Function contracts (`@doc`) | Medium | Medium | Doc generator (exists) |

### Suggested Order

1. **Pipeline operator** — Small parser addition, immediate ergonomic benefit
2. **Type schema export** — New CLI command, enables structured LLM output
3. **AI test feedback** — Builds on test blocks, enables DSPy-style compile loop
4. **Comptime examples** — Leverages existing comptime, catches errors earlier
5. **Quality metrics** — Extends test blocks with scored evaluation
6. **Function contracts** — Larger feature, depends on how `@doc` is designed

---

## How This Fits With Existing Plans

### Complementary to Nanolang Plan

| Nanolang Feature | DSPy Feature That Enhances It |
|-----------------|------------------------------|
| Inline test blocks (M3) | AI test feedback, quality metrics |
| MEMORY.md (M1) | Function contracts feed into it |
| Checked arithmetic (M2) | Pipeline operator for clean composition |

### Complementary to MoonBit Plan

| MoonBit Feature | DSPy Feature That Enhances It |
|----------------|------------------------------|
| Semantic sampler | Type schema export (structured output) |
| Mandatory return types | Pipeline operator (type-checked chains) |
| Incremental type checking | AI test feedback (structured errors) |

### Combined Priority (All Three Plans)

```
Phase 5A: Foundation (no code changes)
  1. MEMORY.md + grammar.json + spec.json

Phase 5B: Small Language Features
  2. Mandatory return types (MoonBit)
  3. Checked arithmetic +? (Nanolang)
  4. Pipeline operator |> (DSPy)

Phase 5C: Testing Story
  5. Inline test blocks (Nanolang)
  6. Comptime examples @example (DSPy)
  7. AI test feedback --ai-feedback (DSPy)
  8. Quality metrics (DSPy)

Phase 5D: Tooling
  9. Type schema export klar schema (DSPy)
  10. Semantic sampler klar assist (MoonBit)
  11. Incremental type checking (MoonBit)

Phase 5E: Advanced
  12. Function contracts @doc (DSPy)
  13. FFI sandbox (Nanolang)
  14. Formal verification (Nanolang)
```

---

## References

- [DSPy Paper (arXiv)](https://arxiv.org/abs/2310.03714)
- [DSPy Paper (ICLR 2024)](https://openreview.net/pdf?id=sY5N0zY5Od)
- [DSPy Framework](https://dspy.ai/)
- [DSPy GitHub](https://github.com/stanfordnlp/dspy)
- [DSPy Signatures](https://dspy.ai/learn/programming/signatures/)
- [DSPy Optimizers](https://dspy.ai/learn/optimization/optimizers/)
- [DSPy Output Refinement](https://dspy.ai/tutorials/output_refinement/best-of-n-and-refine/)
- [DSPy Assertions (deprecated)](https://dspy.ai/learn/programming/7-assertions/)
