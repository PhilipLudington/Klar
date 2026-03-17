# Effect System Evaluation

> **Status:** Evaluated — **No-go** for algebraic effects. Existing `meta pure` + `async fn`/`await` covers Klar's needs.
> **Decision date:** 2026-03-14

---

## Motivation

Algebraic effects are a mechanism for separating *what* a computation does (declaring an effect) from *how* it's handled (providing a handler). Languages like Eff, Koka, and OCaml 5 use them to unify async, exceptions, state, and I/O under a single abstraction.

This document evaluates whether algebraic effects align with Klar's philosophy and whether they would improve the language for its target audience (application developers and AI code generators).

---

## Current State in Klar

Klar already has two mechanisms that cover effect-related concerns:

### 1. `meta pure` — Compiler-Verified Purity

```klar
meta pure
fn add(a: i32, b: i32) -> i32 {
    return a + b
}
```

The compiler verifies that `meta pure` functions:
- Don't call impure builtins (print, file I/O, etc.)
- Don't call other impure functions (transitive checking)
- Don't perform side effects

This is a binary pure/impure distinction — sufficient for optimization hints, memoization safety, and AI agent guidance.

### 2. `async fn` / `await` — Asynchronous Control Flow

```klar
async fn fetch_data(url: string) -> string {
    let response: string = await http_get(url)
    return response
}
```

Async functions return `Future#[T]`, and `await` suspends until the future completes. This covers the most common "effect" in application programming: asynchronous I/O.

### 3. Kira Interop Effect Awareness

The Kira FFI already distinguishes pure vs effectful functions via the `"effect"` field in type manifests. Generated Klar wrappers get `meta pure` for pure Kira functions and `// effect` comments for effectful ones.

---

## Syntax Proposals Evaluated

### Proposal A: Full Algebraic Effects (Koka/Eff Style)

```klar
// Declare an effect
effect Console {
    fn read_line() -> string
    fn write(s: string) -> void
}

// Use the effect (declared in function signature)
fn greet() -> void with Console {
    let name: string = Console.read_line()
    Console.write("Hello, " + name)
}

// Handle the effect
handle greet() {
    Console.read_line => { resume("World") }
    Console.write(s) => { println(s); resume(()) }
}
```

**Tradeoffs:**

| Advantage | Disadvantage |
|-----------|--------------|
| Unifies async, I/O, exceptions, state | Significant type checker complexity (~2000+ LOC) |
| Testable: swap handlers for mocks | `with` clauses propagate through call chains |
| Theoretically elegant | `resume` is a delimited continuation — hard to reason about |
| Composable effects | Requires effect polymorphism for generic code |
| | Unfamiliar to target audience (C#/Go developers) |
| | Conflicts with ownership — resumed continuations may alias moved values |

**Interaction with ownership model:**
When a handler calls `resume`, execution returns to the effect site. If the function has moved a value before the effect call, `resume` would re-enter a scope where that value is no longer valid. This requires either:
- Copying all live values at effect boundaries (expensive)
- Restricting what can be in scope at effect sites (confusing)
- Linear typing of continuations (adds complexity)

**Type checker impact:** Effect rows, effect polymorphism, handler type checking, and effect subtyping would roughly double the complexity of generic type inference. Estimated 2000–3000 lines of new checker code.

### Proposal B: Lightweight Effect Annotations (Checked-Effects)

```klar
// Mark functions with their effects
effect IO
effect Async

fn read_file(path: string) -> string with IO {
    return fs_read(path)
}

async fn fetch(url: string) -> string with IO, Async {
    return await http_get(url)
}

// Pure functions have no 'with' clause (equivalent to meta pure)
fn add(a: i32, b: i32) -> i32 {
    return a + b
}
```

**Tradeoffs:**

| Advantage | Disadvantage |
|-----------|--------------|
| Makes side effects visible in signatures | Annotation burden on every function |
| Compatible with existing `meta pure` | Doesn't enable new capabilities (no handlers) |
| Simpler than full algebraic effects | Effect sets propagate virally through callers |
| Familiar to Rust/Java developers | Refactoring moves between "worlds" are painful |
| | `with IO` on 80%+ of application functions is noise |

**Key problem:** In application-level programming, *most* functions perform I/O or call something that does. Marking everything `with IO` provides little signal — it's the *absence* of IO that's interesting, which is exactly what `meta pure` already captures.

### Proposal C: Effect Capabilities (Object-Capability Style)

```klar
// Effects are capabilities passed as parameters
struct Console {
    read_line: fn() -> string,
    write: fn(string) -> void,
}

fn greet(console: Console) -> void {
    let name: string = console.read_line()
    console.write("Hello, " + name)
}

// Caller provides the capability
let real_console: Console = Console {
    read_line: || -> string { return readline() },
    write: |s: string| -> void { println(s) },
}
greet(real_console)
```

**Tradeoffs:**

| Advantage | Disadvantage |
|-----------|--------------|
| No language changes needed — works today | Verbose: threading capabilities through call chains |
| Explicit and visible | No compiler enforcement of effect discipline |
| Testable: pass mock capabilities | Capabilities are just structs — no special optimization |
| Familiar pattern (dependency injection) | Doesn't compose as cleanly as algebraic effects |

**Key insight:** This is already expressible in Klar. It's a design pattern, not a language feature. If users want capability-based effects, they can adopt this pattern without language changes.

---

## Analysis Against Klar's Principles

### "No ambiguity. No surprises."

Algebraic effects (Proposal A) introduce `resume` — a non-local control flow mechanism that returns to a different stack frame. This is inherently surprising. The handler at the call site determines what `resume` does, meaning the function body alone doesn't tell you what happens next. This violates "parseable at a glance."

### "The code explains itself."

Effect annotations (Proposal B) add information to signatures, which aligns with self-describing code. However, the `with IO` annotation on most functions becomes noise rather than signal. `meta pure` already captures the interesting case (the function is side-effect-free) without annotating the common case.

### "Explicitness earns its characters."

The verbosity test: *does this annotation prevent the reader from needing to read other code?*

- `meta pure` passes — you know the function has no side effects without reading its body.
- `with IO, Async` fails for most functions — in an application, I/O is the default expectation. The annotation is noise on 80%+ of functions and only adds value on the 20% that are pure — which `meta pure` already handles.

---

## Comparison with Existing Approach

| Concern | Current Klar | Full Algebraic Effects |
|---------|-------------|----------------------|
| Purity | `meta pure` (compiler-verified) | Effect rows (more granular but noisier) |
| Async | `async fn` / `await` (explicit) | Async as an effect (more uniform but unfamiliar) |
| Testing | Dependency injection / closures | Handler swapping (elegant but requires learning effects) |
| I/O tracking | Impure by default, opt-in purity | Effect annotations on all functions |
| Complexity | Low — binary pure/impure | High — effect polymorphism, handlers, rows |
| AI-friendliness | High — simple rules | Medium — effect propagation rules are non-trivial |

---

## Decision: No-Go

**Algebraic effects are not being pursued for Klar.** The rationale:

1. **Klar's existing approach covers the practical cases.** `meta pure` handles purity verification. `async fn`/`await` handles asynchronous control flow. These are the two effects that matter most in application programming.

2. **Effect systems add disproportionate complexity.** Full algebraic effects would roughly double type checker complexity for a feature that application developers rarely need. Klar's target audience writes web services, CLI tools, and data processing — not programming language research.

3. **Ownership interaction is unsolved.** Resumed continuations create aliasing problems with moved values. No production language has cleanly combined algebraic effects with ownership-based memory safety. Solving this for Klar would require significant research investment with uncertain payoff.

4. **The annotation burden violates Klar's philosophy.** In application code, most functions are effectful. Annotating the common case is noise. `meta pure` correctly annotates the *uncommon* case — pure functions — where the annotation provides real signal.

5. **Capability-based patterns work today.** For users who want explicit effect tracking, Proposal C (passing effect capabilities as parameters) is already expressible in Klar without language changes. This follows Go's philosophy: use interfaces and dependency injection rather than effect types.

### What Could Change This Decision

- If Klar's target shifts toward systems/embedded where fine-grained effect tracking matters
- If a production language demonstrates algebraic effects + ownership working well together
- If users consistently report that `meta pure` is insufficient for their purity-tracking needs

---

## Implemented Improvements (No New Syntax)

Instead of algebraic effects, these incremental improvements to the existing `meta pure` system were implemented:

1. **Builtin method purity checking** — mutating collection methods (`push`, `pop`, `insert`, `remove`, `clear`, `set`, `sort`, `reverse`, `push_front`, `push_back`, `pop_front`, `pop_back`, `push_str`, `push_char`), I/O methods (`write_all`, `flush`, `close`), and concurrency methods (`send`, `recv`) are now flagged as errors inside `meta pure` functions. Read-only methods (`len`, `is_empty`, `get`, `first`, `last`, `contains`, `to_string`) remain allowed.

2. **User-defined method purity checking** — struct and enum method calls inside `meta pure` functions must target methods that are themselves marked `meta pure`. Non-pure method calls produce a clear error.

3. **`inout` parameter rejection** — `meta pure` functions cannot have `inout` parameters, since mutable references allow mutation of external state. The compiler emits an error at the parameter declaration site.

### Remaining Known Gap

- **Function-typed parameters (callbacks)** — could hold impure function references. Statically verifying this would require a "pure callback" annotation on function types, which is not yet implemented.

### Future Possibilities

- **Add `meta io` annotation** — optional, for functions that directly perform I/O (not transitively). Useful for documentation and AI guidance, not for type system enforcement.

- **Formalize the capability pattern** — document the dependency-injection approach (Proposal C) in the language guide as the recommended way to make effects testable.
