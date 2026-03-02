# Klar Design Philosophy

Klar is an AI-native application programming language. Three principles govern every design decision.

## 1. No ambiguity. No surprises.

Syntax is self-describing. Every construct is parseable at a glance without surrounding context.

```klar
[i32; 3]            // Array type is self-contained — no lookahead needed
and, or, not        // Keywords over cryptic symbols
let x: i32 = 5     // Explicit type annotations — always
return a + b        // Explicit returns — no implicit last-expression
x.as#[i64]          // Explicit conversions — no silent coercion
a +% b              // Explicit overflow — wrapping, not undefined
```

This benefits both humans reading code and AI generating it. You never need to read surrounding code to understand one line.

**What this means in practice:**
- One way to do things (no syntax aliases)
- Keywords over symbols where possible
- No implicit behavior (returns, conversions, coercions)
- Every operation has defined semantics (no undefined behavior)

## 2. The code explains itself.

Code carries its own intent, architecture, and decisions. No external document needed to understand why.

```klar
meta module {
    purpose: "Recursive descent parser - transforms token stream into AST",
    role: "frontend"
}

meta decision("i32 not usize -- app-level ergonomics over systems safety")
fn len(self) -> i32 { ... }

meta intent("System boundary: validates untrusted input. Internal callers use parse_int_unchecked")
fn parse_int(s: string) -> Result#[i32, ParseError] { ... }
```

Types tell you *what*. The meta layer tells you *why*. Together, they make external documentation redundant for understanding code.

**What this means in practice:**
- The meta layer embeds intent, decisions, and architecture in the source
- The compiler validates meta annotations (not just stores them)
- `klar meta` CLI queries metadata across the codebase
- AI agents read annotations instead of entire files and external docs

See [Meta Layer Design Spec](design/meta-layer.md) for the full design.

## 3. Explicitness earns its characters.

Every explicit annotation must justify its verbosity by preventing the reader from needing to look elsewhere.

`let x: i32 = 5` is more verbose than `let x = 5`, but it earns those characters — you never need to trace through inference to know the type. The meta layer applies this same principle to intent, architecture, and decisions.

**The verbosity test:**

> Does this annotation prevent the reader (human or AI) from needing to read other code to understand this code?
>
> If yes, it earns its place. If not, it's noise.

**Passes:**
```klar
// Without this, you'd read 3 files to understand why len() returns i32
meta decision("i32 not usize -- app-level ergonomics over systems safety")
fn len(self) -> i32 { ... }
```

**Fails:**
```klar
// The type signature already says this
meta intent("Takes a string and returns an integer")
fn parse_int(s: string) -> ?i32 { ... }
```

**What this means in practice:**
- Simple code needs no annotations — the implementation is the documentation
- Complex code earns more meta annotations
- Meta density scales with code complexity
- Every character of explicitness must reduce ambiguity or save reading time

## How the Principles Interact

| Principle | Governs | Example |
|-----------|---------|---------|
| No ambiguity. No surprises. | Syntax and semantics | `and` not `&&`, explicit types, defined overflow |
| The code explains itself. | Intent and architecture | `meta intent(...)`, `meta decision(...)`, `meta module { ... }` |
| Explicitness earns its characters. | When to be verbose | Type annotations: yes. Restating the obvious: no. |

Principle 1 is the foundation — syntax must be unambiguous. Principle 2 extends this to meaning — code must be self-explaining. Principle 3 is the filter — only add verbosity that prevents reading other code.

## Target Domain

Klar targets **application-level programming** (like C#/Go), not bare-metal systems (like C/Rust/Zig). This is why:

- `.len()` returns `i32` for ergonomic loop counter usage
- Ownership is simpler than Rust (no lifetime annotations)
- Reference counting (`Rc#[T]`, `Arc#[T]`) where needed
- Bounds checking on array access (safety over performance)

The language is designed for the 95% of code that is application logic, not the 5% that is systems plumbing.
