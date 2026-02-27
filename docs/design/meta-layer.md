# Meta Layer: Self-Describing Code

> **Status:** Design spec (not yet implemented)
> Klar's meta layer embeds intent, architecture, and conventions directly in the language — where the compiler can validate them and tooling can query them.

---

## Motivation

### The Problem

AI agents working on Klar codebases waste tokens and make mistakes because context lives outside the code:

| Context source | Problem |
|---------------|---------|
| CLAUDE.md / README | Drifts from code, duplicates information |
| Code comments | Unstructured, no validation, inconsistent |
| External docs | AI must discover, fetch, and correlate |
| "Read 5 files to understand 1 function" | Explosive token cost |

When an AI generates or modifies code, it needs to know *why* things are the way they are — not just *what* the code does. Types tell you what. The meta layer tells you why.

### Design Philosophy

Klar's design philosophy has three principles (see [PHILOSOPHY.md](../PHILOSOPHY.md)):

1. **"No ambiguity. No surprises."** — Syntax is self-describing. Every construct is parseable at a glance without surrounding context.
2. **"The code explains itself."** — Code carries its own intent, architecture, and decisions. No external document needed to understand why.
3. **"Explicitness earns its characters."** — Every explicit annotation must justify its verbosity by preventing the reader from needing to look elsewhere.

Principle 3 is the **verbosity test** that governs the meta layer:

> **Does this annotation prevent the reader (human or AI) from needing to read other code to understand this code?**
>
> If yes, it earns its place. If not, it's noise.

**Passes the test (earns its verbosity):**

```klar
// Without this, you'd have to read 3 other files to understand why len() returns i32
meta decision("i32 not usize -- app-level ergonomics over systems safety")
fn len(self) -> i32 { ... }
```

```klar
// The type signature says "what", this says "when and why"
meta intent("System boundary: validates untrusted input. Internal callers use parse_int_unchecked")
fn parse_int(s: string) -> Result#[i32, ParseError] { ... }
```

**Fails the test (noise):**

```klar
// The type signature already says this -- meta adds nothing
meta intent("Takes a string and returns an integer")
fn parse_int(s: string) -> ?i32 { ... }
```

**Corollary:** Meta density should scale with code complexity. Simple code explains itself through its implementation. Complex code needs the meta layer.

```klar
// Simple helper -- no meta needed, the code IS the documentation
fn add(a: i32, b: i32) -> i32 {
    return a + b
}
```

---

## Syntax: The `meta` Keyword

### Why `meta`

Klar prefers keywords over symbols: `and`/`or`/`not`, `let`/`var`, `fn`/`struct`/`enum`/`trait`. The language is word-oriented. `meta` fits naturally.

| Alternative | Why rejected |
|------------|-------------|
| `@` prefix | Already used for comptime (`@assert(...)`, `@typeName(...)`) |
| `#` prefix | Already used for generics (`#[T]`, `Option#[T]`) |
| `#!` prefix | Too symbolic for Klar's word-oriented philosophy |
| `///` doc comments | Blurs the line between comments and semantics — violates "no ambiguity" |

You see `meta`, you know it's metadata. Zero ambiguity. No overlap with any existing syntax.

**Note on `in`:** The `in` keyword is already used in `for` loops (`for x: i32 in list`). In the meta layer, `in` only appears immediately after `meta` as `meta in("group")`. The parser disambiguates by context — `in` after `meta` is always a group join, `in` after a loop variable is always iteration.

### Grammar

```
meta_annotation = "meta" meta_kind

meta_kind = module_meta
          | intent_meta
          | decision_meta
          | tag_meta
          | related_meta
          | guide_meta
          | hint_meta
          | pure_meta
          | deprecated_meta
          | define_meta
          | group_def
          | group_join
          | custom_meta

module_meta     = "module" "{" key_value_list "}"
intent_meta     = "intent" "(" string_literal ")"
decision_meta   = "decision" "(" string_literal ")"
tag_meta        = "tag" "(" string_literal ")"
related_meta    = "related" "(" path ("," path)* ("," string_literal)? ")"
guide_meta      = "guide" "{" key_value_list "}"
hint_meta       = "hint" "(" string_literal ")"
pure_meta       = "pure"
deprecated_meta = "deprecated" "(" string_literal ")"
define_meta     = "define" identifier "(" typed_params ")" ("for" scope_target)?
group_def       = "group" string_literal "{" meta_annotation* "}"
group_join      = "in" "(" string_literal ")"
custom_meta     = identifier "(" expression_list ")"

typed_params    = typed_param ("," typed_param)*
typed_param     = identifier ":" (param_type | string_union)
param_type      = "string" | "path"
string_union    = string_literal ("|" string_literal)*
scope_target    = "fn" | "module" | "struct" | "enum" | "trait" | "field"
expression_list = expression ("," expression)*
path            = identifier ("::" identifier)*
key_value_list  = (identifier ":" expression ("," identifier ":" expression)* ","?)?
```

### Where `meta` Can Appear

| Position | Example annotations |
|----------|-------------------|
| File-level (top of file) | `meta module { ... }`, `meta guide { ... }` |
| Before functions | `meta intent(...)`, `meta decision(...)`, `meta tag(...)`, `meta pure` |
| Before structs, enums, traits | `meta tag(...)`, `meta intent(...)`, `meta deprecated(...)` |
| Before impl blocks | `meta tag(...)`, `meta guide { ... }` |
| Before fields | `meta tag(...)`, `meta deprecated(...)` |

Multiple meta annotations can be stacked (one per line) or chained inline for compact Tier 1 annotations:

```klar
// Stacked (Tier 2 style)
meta intent("Entry point for expression parsing")
meta decision("Recursive descent over Pratt parsing -- simpler error recovery")
fn parse_expr(self) -> Expr { ... }

// Inline chaining (Tier 1 style)
meta tag("parsing") meta pure
fn token_length(tok: Token) -> i32 { ... }
```

---

## Built-in Annotations (Core Vocabulary)

The meta layer has a small built-in vocabulary. Every Klar project speaks the same meta language, which is itself a token-saver for AI moving between projects.

| Annotation | Level | Tier | Purpose |
|-----------|-------|------|---------|
| `meta module { ... }` | File | 3 | Architecture role, purpose, dependencies |
| `meta guide { ... }` | File/impl | 3 | Conventions and patterns |
| `meta intent("...")` | Function/type | 2 | Why it exists, when to use it |
| `meta decision("...")` | Any | 2 | Why a choice was made |
| `meta related(...)` | Any | 2 | Explicit cross-references |
| `meta hint("...")` | Any | 2 | AI code generation guidance |
| `meta tag("...")` | Any | 1 | Cross-file grouping |
| `meta pure` | Function | 1 | No side effects (compiler-verifiable) |
| `meta deprecated("...")` | Any | 1 | Migration guidance |

### `meta module { ... }` — File-level purpose and architecture

Describes a file's role in the system. Read once for orientation.

```klar
meta module {
    purpose: "Recursive descent parser - transforms token stream into AST",
    role: "frontend",
    depends: ["lexer", "ast", "token"],
    owns: "All parsing logic and error recovery"
}
```

Simpler form for smaller files:

```klar
meta module {
    purpose: "Recursive descent parser",
    role: "frontend"
}
```

**Fields:**

| Field | Type | Description |
|-------|------|-------------|
| `purpose` | string | What this file/module does |
| `role` | string | Architecture layer (e.g., "frontend", "codegen", "runtime") |
| `depends` | list of strings | Module names this depends on |
| `owns` | string | What this module is responsible for |

### `meta intent("...")` — Why something exists

The most important annotation. Explains *when to use this* and *why it exists* — things the type signature doesn't convey.

```klar
meta intent("System boundary: validates untrusted input. Internal callers use parse_int_unchecked")
fn parse_int(s: string) -> Result#[i32, ParseError] { ... }
```

```klar
meta intent("Entry point for expression parsing -- delegates to parse_binary for operator precedence")
fn parse_expr(self) -> Expr { ... }
```

```klar
meta intent("Resolves generic type T to its concrete monomorphized form. Called during codegen, not during type checking.")
fn monomorphize(self, ty: GenericType, args: [Type]) -> Type { ... }
```

### `meta decision("...")` — Why a choice was made

Captures design rationale. Prevents future developers (human or AI) from "fixing" intentional decisions.

```klar
meta decision("i32 not usize -- app-level ergonomics over systems safety")
fn len(self) -> i32 { ... }
```

```klar
meta decision("Recursive descent over Pratt parsing -- simpler error recovery")
fn parse_expr(self) -> Expr { ... }
```

### `meta tag("...")` — Cross-file grouping

Tags create virtual groups across the codebase. Use `klar meta --tag "name"` to find everything with a tag.

```klar
meta tag("parsing")
fn parse_expr(self) -> Expr { ... }

// In another file entirely:
meta tag("error_handling")
enum Error { ParseError(string), TypeError(string) }
```

Tag names are strings — unambiguous, compiler-validated, and consistent with the rest of the meta layer.

### `meta related(...)` — Explicit cross-references

Makes dependencies between distant code explicit. The compiler validates that all path targets (functions, types, or any named declaration) actually exist.

Arguments are one or more paths, with an optional description string **always last**:

```klar
// Paths + description (string is always the last argument)
meta related(checker::resolve_type, "This emits the LLVM for types that checker resolves")
fn emit_type(self, ty: Type) -> LLVMTypeRef { ... }
```

```klar
// Multiple paths, no description
meta related(checker::register_generic, codegen::emit_type)
fn monomorphize(self, ty: GenericType, args: [Type]) -> Type { ... }
```

```klar
// Paths can reference functions or types
meta related(lexer::next_token, ast::Expr)
fn parse_expr(self) -> Expr { ... }
```

### `meta guide { ... }` — Conventions and patterns

File-level or impl-level conventions. Tells AI "how things are done here."

```klar
meta guide {
    style: "All public functions must have meta intent annotations",
    errors: "Use KlarError enum, never raw strings",
    testing: "Each function needs a test in test/native/",
    patterns: ["Visitor pattern for AST traversal",
               "Builder pattern for IR construction"]
}
```

**Fields:** All fields are optional. Common conventions:

| Field | Type | Description |
|-------|------|-------------|
| `style` | string | Coding style conventions |
| `errors` | string | Error handling conventions |
| `testing` | string | Testing requirements |
| `patterns` | list of strings | Design patterns used in this scope |
| `strictness` | string | Opt-in compiler strictness level (see [Compiler Validation](#optional-strictness-levels)) |

The field set is open — projects can add any key-value pairs. The fields above are conventions, not a closed schema.

### `meta hint("...")` — AI code generation guidance

Direct instructions for AI generating code in this context. These are things a human might tell an AI collaborator.

```klar
meta hint("Always check .is_generic() before calling monomorphize()")
meta hint("Error messages should include source location via .span()")
meta hint("Prefer returning Result over panicking")
```

### `meta pure` — No side effects

Marks a function as pure (no side effects, no mutation). The compiler can verify this.

```klar
meta pure
fn add(a: i32, b: i32) -> i32 {
    return a + b
}
```

**Compiler-verifiable:** The compiler can check that a `meta pure` function:
- Does not mutate any state
- Does not call impure functions
- Does not perform I/O

### `meta deprecated("...")` — Migration guidance

Marks something as deprecated with a migration path.

```klar
meta deprecated("Use parse_expr_v2 which handles operator precedence correctly")
fn parse_expr(self) -> Expr { ... }
```

---

## Custom Annotations (`meta define`)

Projects can define their own meta keys beyond the built-in vocabulary.

### Basic definition

```klar
meta define priority(level: string)
```

Usage:

```klar
meta priority("high")
fn handle_payment(amount: f64) -> Result#[Receipt, PaymentError] { ... }

meta priority("low")
fn format_log_line(msg: string) -> string { ... }
```

### With string union constraints

The compiler validates that values match the allowed set:

```klar
meta define priority(level: "high" | "medium" | "low")
```

Now `meta priority("hihg")` is a **compiler error** — typo caught at compile time.

### Scope restrictions

Limit where a custom annotation can appear:

```klar
meta define priority(level: string) for fn
meta define owner(team: string) for module
```

Using `meta priority(...)` on a struct when it's defined `for fn` is a compiler error.

### Importing custom definitions from modules

Custom meta definitions are just code — they live in modules and are imported with Klar's standard import syntax:

```klar
import project_meta.{ priority, owner }
```

A team defines their meta vocabulary once in a shared module, imports it everywhere. The compiler recognizes imported `meta define` declarations and makes them available as valid meta annotations in the importing file.

---

## Meta Groups (Explicit Opt-in Inheritance)

### The Problem

Without groups, related functions repeat the same meta annotations:

```klar
meta guide { errors: "Use KlarError enum" }
meta tag("ast_traversal")
fn visit_expr(self, expr: Expr) -> Result#[Value, KlarError] { ... }

meta guide { errors: "Use KlarError enum" }
meta tag("ast_traversal")
fn visit_stmt(self, stmt: Stmt) -> Result#[Value, KlarError] { ... }
```

### The Solution: `meta group` / `meta in`

Three inheritance models were considered:

| Model | Description | Tradeoff |
|-------|-------------|----------|
| A: No inheritance | Always explicit | Maximum Klar-ness, but repetitive |
| B: Scoped cascading | Meta applies to its block | Less repetition, but violates "parseable at a glance" |
| **C: Explicit grouping** | **Opt-in via `meta group` / `meta in`** | **Reduces repetition while staying explicit** |

**Model C was chosen.** The `meta in("name")` line is like a type annotation: it costs a few characters but tells you exactly what's going on.

### Defining a group

```klar
meta group "parser_visitor" {
    meta guide { errors: "Use KlarError enum" },
    meta tag("ast_traversal")
}
```

### Joining a group

```klar
meta in("parser_visitor")
fn visit_expr(self, expr: Expr) -> Result#[Value, KlarError] { ... }

meta in("parser_visitor")
fn visit_stmt(self, stmt: Stmt) -> Result#[Value, KlarError] { ... }
```

### Override semantics

Function-level meta adds to (or overrides) group-level meta:

```klar
meta in("parser_visitor")
meta intent("Special case: handles operator precedence via Pratt parsing")
fn visit_binary(self, expr: BinaryExpr) -> Result#[Value, KlarError] { ... }
```

Here `visit_binary` gets everything from the `parser_visitor` group, plus its own `intent` annotation.

---

## Compiler Validation

The meta layer is a first-class language feature. The compiler validates annotations, not just stores them.

### What the compiler checks

| Rule | Severity | Description |
|------|----------|-------------|
| `meta related(target)` — target exists | Error | Referenced function/type must exist |
| `meta in("group")` — group exists | Error | Group must be defined before use |
| Custom meta matches `meta define` shape | Error | Arguments must match the definition |
| String union constraints | Error | Value must be one of the allowed strings |
| Scope restrictions (`for fn`, `for module`) | Error | Annotation used in wrong position |
| `meta pure` — function is actually pure | Error | Compiler verifies no side effects |
| `meta deprecated` — used item triggers warning | Warning | Caller warned when using deprecated item |

### Warning vs. error distinction

- **Errors** halt compilation: structural violations (missing group, wrong type, invalid target)
- **Warnings** inform but don't block: usage of deprecated items, missing recommended annotations

### Optional strictness levels

Projects can opt into stricter meta requirements:

```klar
// In a project config or meta guide:
meta guide {
    strictness: "all public functions require meta intent"
}
```

The compiler emits warnings for public functions missing `meta intent` in strict mode. This is opt-in — not all projects need this level of documentation.

---

## CLI: `klar meta`

The `klar meta` command queries the meta layer across a codebase.

### Commands

| Command | Description |
|---------|-------------|
| `klar meta --tag "name"` | Find everything with a specific tag |
| `klar meta --module` | List all module descriptions |
| `klar meta --related fn_name` | Follow cross-references for a function |
| `klar meta --deprecated` | List all deprecated items and their replacements |
| `klar meta --hints` | List all AI hints |

### JSON output mode

All commands support `--json` for machine-readable output, designed for AI tooling:

```bash
klar meta --tag "parsing" --json
```

```json
{
  "tag": "parsing",
  "matches": [
    {
      "path": "src/parser.kl",
      "name": "parse_expr",
      "kind": "function",
      "meta": {
        "tag": ["parsing"],
        "intent": "Entry point for expression parsing"
      }
    },
    {
      "path": "src/lexer.kl",
      "name": "next_token",
      "kind": "function",
      "meta": {
        "tag": ["parsing"],
        "intent": "Produces the next token from the source stream"
      }
    }
  ]
}
```

This enables AI agents to query project structure programmatically instead of reading entire files.

---

## Tiered Density Guidelines

Meta annotations are organized into three tiers by when they're read:

### Tier 1: Scan-level (inline, compact)

Read when scanning a file. Compact markers that categorize code.

- `meta tag("...")` — cross-file grouping
- `meta pure` — side-effect marker
- `meta deprecated("...")` — migration notice

```klar
meta tag("parsing") meta pure
fn token_length(tok: Token) -> i32 { ... }
```

### Tier 2: Understanding-level (function-attached, detailed)

Read when needing to understand a specific function. Answers "why does this exist?"

- `meta intent("...")` — why it exists, when to use it
- `meta decision("...")` — why a choice was made
- `meta related(...)` — cross-references

```klar
meta intent("Entry point for expression parsing")
meta decision("Recursive descent over Pratt parsing -- simpler error recovery")
meta related(lexer::next_token, ast::Expr)
fn parse_expr(self) -> Expr { ... }
```

### Tier 3: Orientation-level (file-level, read once)

Read once when first encountering a file. Provides architectural context.

- `meta module { ... }` — architecture role and purpose
- `meta guide { ... }` — conventions and patterns

```klar
meta module {
    purpose: "Recursive descent parser",
    role: "frontend"
}
```

### Reading order for AI

An AI agent reads: **Tier 3 first** (file-level context) → **Tier 1** (scanning function markers) → **Tier 2 only when needed** (understanding a specific function).

This is token-efficient — the AI can stop early. It reads the module description, scans the tags to find what it needs, and only reads intent/decision annotations for the specific functions it's modifying.

### When meta earns its verbosity

| Code complexity | Recommended density |
|----------------|-------------------|
| Simple helpers (`add`, `max`) | None — the code is the documentation |
| Internal utilities | Tier 1 at most — tags for discoverability |
| Public API functions | Tier 1 + Tier 2 — intent and decisions matter |
| Architecture boundaries | All tiers — full context for AI and humans |

---

## Full Examples

### A small module fully annotated

```klar
// ============================================================
// parser.kl — Recursive descent parser for Klar
// ============================================================

meta module {
    purpose: "Recursive descent parser - transforms token stream into AST",
    role: "frontend",
    depends: ["lexer", "ast", "token"],
    owns: "All parsing logic and error recovery"
}

meta guide {
    style: "All public functions must have meta intent annotations",
    errors: "Use KlarError enum, never raw strings",
    testing: "Each function needs a test in test/native/",
    patterns: ["Visitor pattern for AST traversal",
               "Builder pattern for IR construction"]
}

meta group "parser_entry" {
    meta tag("parsing"),
    meta hint("These functions are the public API of the parser")
}

struct Parser {
    tokens: [Token],
    pos: i32,
}

impl Parser {
    meta intent("Creates a parser from a token stream. Call parse() to produce an AST.")
    fn new(tokens: [Token]) -> Parser {
        return Parser { tokens: tokens, pos: 0 }
    }

    meta in("parser_entry")
    meta intent("Main entry point: parses a complete program into a list of declarations")
    meta related(parse_fn_decl, parse_struct_decl)
    fn parse(self) -> Result#[Program, KlarError] {
        var decls: List#[Decl] = List.new#[Decl]()
        while not self.at_end() {
            let decl: Decl = self.parse_decl()?
            decls.push(decl)
        }
        return Ok(Program { decls: decls })
    }

    meta in("parser_entry")
    meta intent("Parses a single expression. Used by REPL and expression statements.")
    meta decision("Recursive descent over Pratt parsing -- simpler error recovery")
    meta related(lexer::next_token, ast::Expr)
    fn parse_expr(self) -> Result#[Expr, KlarError] {
        return self.parse_binary(0)
    }

    meta tag("parsing") meta pure
    fn peek(self) -> Token {
        return self.tokens[self.pos]
    }

    meta tag("parsing")
    fn advance(self) -> Token {
        let tok: Token = self.tokens[self.pos]
        self.pos = self.pos + 1
        return tok
    }

    meta deprecated("Use parse_expr which handles all expression forms")
    fn parse_simple_expr(self) -> Result#[Expr, KlarError] {
        return self.parse_expr()
    }
}
```

### Before/after: External docs vs. meta-annotated code

**Before** (context lives in CLAUDE.md):

```markdown
## Parser Architecture

The parser uses recursive descent (not Pratt parsing) because it gives
simpler error recovery. The main entry point is `parse()` which produces
a list of declarations. For expressions, use `parse_expr()` which delegates
to `parse_binary` for operator precedence.

All parsing functions should use the KlarError enum for errors, never raw
strings. Each public function needs a corresponding test in test/native/.

The parser depends on the lexer (for tokens), ast (for node types), and
token (for token types). It owns all parsing logic and error recovery.

Note: `parse_simple_expr` is deprecated — use `parse_expr` instead.
```

**After** (context lives in the code):

All of the above information is embedded in the meta annotations shown in the previous example. The CLAUDE.md paragraph is replaced by `meta module`, `meta guide`, `meta intent`, `meta decision`, `meta related`, and `meta deprecated` — all compiler-validated, queryable via `klar meta`, and co-located with the code they describe.

| Before (external docs) | After (meta layer) |
|------------------------|-------------------|
| 20-line paragraph explaining architecture | `meta module { purpose: "...", role: "frontend" }` |
| Conventions buried in CLAUDE.md | `meta guide { errors: "Use KlarError enum" }` |
| Grep-and-hope to find related code | `meta related(checker::resolve_type)` |
| Reading 5 files to understand "why" | `meta decision("...")` on the function |
| AI reading 500 lines to infer patterns | `meta guide { patterns: [...] }` |

### Token efficiency estimate

The meta layer costs ~10-15% more characters in source files, but saves ~50-80% of the tokens an AI spends understanding code — because the AI reads targeted annotations instead of entire files and external documents.

---

## Design Decisions

### Why strings for tag/group names

Four naming options were considered:

| Option | Example | Decision |
|--------|---------|----------|
| Bare identifiers | `meta tag(parsing)` | Rejected — ambiguous with variable/type names |
| **Strings** | **`meta tag("parsing")`** | **Chosen — zero ambiguity, compiler validates** |
| Dotted names | `meta tag(.parsing)` | Rejected — new syntax for little benefit |
| Colon prefix | `meta tag(:parsing)` | Rejected — too symbolic |

Strings are consistent with the rest of the meta layer and the compiler can validate them.

### Why not doc comments

Structured doc comments (`/// @intent ...`) were considered but rejected:

- Comments are invisible to the compiler — no validation possible
- Blurs the line between documentation and semantics
- Violates Klar's "no ambiguity" principle (is it a comment or a declaration?)
- Cannot be queried by tooling without a separate doc parser

### Relationship to `@` comptime

`@` is for compile-time computation: `@assert(...)`, `@typeName(...)`, `@sizeOf(...)`. These affect code generation.

`meta` is for metadata about code: intent, decisions, tags. These don't affect code generation (except `meta pure` which enables optimizations and `meta deprecated` which emits warnings).

The two are complementary and non-overlapping.

### Relationship to `#[T]` generics

`#[T]` is for generic type parameters. `meta` is for metadata annotations. No syntactic overlap.

---

## Implementation Notes

### Parsing

The `meta` keyword introduces a new statement/annotation form in the parser. `meta` is reserved for future use (see `docs/appendix/keywords.md`) but not yet implemented in the lexer — it must be added as a recognized keyword when this feature is built.

### Type checking

The checker validates:
- `meta related` targets resolve to real declarations
- `meta in` groups are defined
- Custom meta usage matches `meta define` shapes
- `meta pure` functions are actually pure

### Backends

The meta layer is erased during compilation — it produces no runtime code. Backends can ignore meta annotations entirely. The exception is `meta pure`, which may enable backend optimizations.

### Storage

Meta annotations are stored in the AST during parsing and available to all compiler phases. The `klar meta` CLI reads the AST directly.

---

## Future Considerations

- **LSP integration**: IDE tooltips showing meta annotations on hover
- **Meta inheritance across modules**: Importing a module inherits its guide conventions
- **Meta coverage reporting**: What percentage of public API has intent annotations
- **AI-optimized output** (not in initial CLI): `klar meta --context fn_name` would dump everything an AI needs to modify a function (its meta, related functions' meta, module context)
