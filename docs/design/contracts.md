# Lightweight Contracts Evaluation

> **Status:** Evaluated — **Go** for `meta require` / `meta ensure` as runtime-checked contracts within the existing meta system. **No-go** for static verification and struct invariants.
> **Decision date:** 2026-03-15

---

## Motivation

Function contracts — preconditions, postconditions, and invariants — make requirements and guarantees explicit at function boundaries. Languages like Eiffel (Design by Contract), D (`in`/`out` blocks), Ada/SPARK (`Pre`/`Post` aspects), and Kotlin (`require`/`check`) provide varying levels of support.

The question: should Klar add contract syntax, and if so, at what level — runtime assertions, compiler-verified annotations, or full static analysis?

---

## Current State in Klar

### 1. Runtime Assertions

```klar
fn binary_search(arr: [i32], target: i32) -> ?i32 {
    assert(arr.len() > 0)  // precondition (crashes if violated)
    // ...body...
}
```

Klar provides `assert`, `assert_eq`, `assert_ne`, `assert_ok`, `assert_err`, `assert_some`, `assert_none`. These are runtime-checked and panic with diagnostic output on failure.

**Limitation:** Postconditions require manually inserting assertions before every `return` statement. For functions with multiple return paths, this is error-prone:

```klar
fn clamp(x: i32, lo: i32, hi: i32) -> i32 {
    if x < lo {
        // Must remember to assert here
        assert(lo >= lo and lo <= hi)
        return lo
    }
    if x > hi {
        // And here
        assert(hi >= lo and hi <= hi)
        return hi
    }
    // And here
    assert(x >= lo and x <= hi)
    return x
}
```

### 2. Compile-Time Assertions

```klar
@assert(size_of#[i32]() == 4)
@assert(size_of#[i32]() == 4, "i32 must be 4 bytes")
```

`@assert` validates conditions during compilation. Useful for type-level guarantees, not function contracts.

### 3. Meta Annotations

```klar
meta pure
fn add(a: i32, b: i32) -> i32 {
    return a + b
}

meta intent("System boundary: validates untrusted input")
fn parse_port(s: string) -> Result#[i32, string] { ... }
```

The `meta` system provides compiler-verified annotations (`meta pure`) and documentation annotations (`meta intent`, `meta decision`). Contracts could extend this system naturally.

---

## Syntax Proposals Evaluated

### Proposal A: New Keywords (`require` / `ensure` / `invariant`)

```klar
fn binary_search(arr: [i32], target: i32) -> ?i32
    require arr.len() > 0
    ensure result is None or arr.contains(target)
{
    // body
}

struct SortedList {
    items: List#[i32]
    invariant is_sorted(items)
}
```

| Advantage | Disadvantage |
|-----------|--------------|
| Clean, dedicated syntax | 3 new reserved words added to the language |
| Familiar to D/Eiffel/Ada users | `invariant` requires hooking every mutation site |
| Postconditions automatically wrap all return paths | `ensure` needs special `result` binding |
| Visible in function signature | Parsing complexity for clause-between-signature-and-body |

**Complexity estimate:** ~500 LOC parser, ~400 LOC checker, ~300 LOC codegen = ~1200 LOC total.

### Proposal B: Meta-Based Contracts (`meta require` / `meta ensure`)

```klar
meta require(arr.len() > 0)
meta ensure(result is None or arr.contains(target))
fn binary_search(arr: [i32], target: i32) -> ?i32 {
    // body
}

meta require(lo <= hi)
meta ensure(result >= lo and result <= hi)
fn clamp(x: i32, lo: i32, hi: i32) -> i32 {
    if x < lo { return lo }
    if x > hi { return hi }
    return x
}
```

| Advantage | Disadvantage |
|-----------|--------------|
| No new keywords — extends existing meta system | `meta require(expr)` is novel (meta usually takes strings or identifiers) |
| Natural for Klar — meta already decorates functions | Postcondition `result` binding still needed |
| Visible in `klar doc` output automatically | Two systems for assertions (assert + meta require) |
| Checker already processes meta annotations | Meta expressions must be type-checked in function's parameter scope |
| Can be toggled (debug vs release builds) | |

**Complexity estimate:** ~150 LOC meta_validation.zig, ~200 LOC codegen, ~50 LOC parser = ~400 LOC total.

### Proposal C: Doc-Comment Contracts

```klar
/// @pre arr.len() > 0
/// @post result is None or arr.contains(target)
fn binary_search(arr: [i32], target: i32) -> ?i32 {
    // body
}
```

| Advantage | Disadvantage |
|-----------|--------------|
| No syntax changes at all | Comments aren't parsed — no runtime checking |
| Familiar from Java/Kotlin | Just documentation, not verification |
| Zero implementation cost | No compiler support for validation |
| Already possible today | Easy to become stale |

**Complexity estimate:** 0 LOC (already possible).

### Proposal D: Assert Wrapper (Library Pattern)

```klar
fn binary_search(arr: [i32], target: i32) -> ?i32 {
    pre(arr.len() > 0)
    // body
    return post(result, |r: ?i32| -> bool {
        return r is None or arr.contains(target)
    })
}
```

| Advantage | Disadvantage |
|-----------|--------------|
| No language changes | Postconditions are verbose and error-prone |
| Uses existing assert infrastructure | Must wrap every return statement |
| Clear runtime semantics | `post()` doesn't exist and would need a builtin |
| | Not visible in function signature or docs |

**Complexity estimate:** ~100 LOC for `pre`/`post` builtins.

---

## Analysis Against Klar's Principles

### Principle 1: "No ambiguity. No surprises."

Contracts make function requirements unambiguous. A caller seeing `meta require(arr.len() > 0)` knows immediately that passing an empty array is a bug — without reading the function body.

**Proposal B (meta-based) wins here.** The contract is at the declaration site, visible at a glance. Proposal C (doc comments) is ambiguous — is it enforced or just a wish? Proposal A (keywords) is unambiguous but adds syntactic weight.

### Principle 2: "The code explains itself."

Contracts are self-documenting. `meta ensure(result >= lo and result <= hi)` tells the reader what the function guarantees without reading the implementation. This aligns perfectly with the meta system's purpose — explaining *why* and *what*, not just *how*.

**Proposal B wins again.** Meta annotations already serve this explanatory role. Adding `require`/`ensure` as meta variants is a natural extension.

### Principle 3: "Explicitness earns its characters."

The verbosity test: does `meta require(arr.len() > 0)` earn its characters compared to `assert(arr.len() > 0)` on the first line of the body?

**Yes, for two reasons:**
1. **Postconditions.** `meta ensure(result >= 0)` checks all return paths automatically. The equivalent with `assert` requires duplicating the check before every `return` — violating DRY and being error-prone.
2. **Documentation.** Meta annotations appear in `klar doc` output. Assert statements in the body do not. A caller shouldn't have to read the implementation to know the contract.

For preconditions alone, the value over `assert` is marginal. For postconditions, the value is significant.

---

## Runtime vs. Static Analysis

| Dimension | Runtime Checking | Static Analysis |
|-----------|-----------------|-----------------|
| **Implementation complexity** | ~400 LOC | ~3000+ LOC (abstract interpretation or SMT) |
| **Expressiveness** | Any boolean expression | Limited to decidable expressions |
| **False positives** | None (checks real values) | Common (over-approximation) |
| **Performance cost** | Check on every call | Zero runtime cost |
| **Coverage** | Only tested paths | All paths (in theory) |
| **Dependencies** | None | SMT solver (Z3) or custom prover |
| **Maintenance** | Low | High (solver integration, abstraction domain) |

**Decision: Runtime-only.** Static verification of arbitrary contracts requires either an SMT solver integration or a custom abstract interpretation engine. Both are 5000+ LOC, add an external dependency, and produce false positives that frustrate users. Klar's target audience (application developers) benefits more from clear runtime failures than from static proofs.

This matches Klar's existing philosophy — `assert` is runtime-checked, `meta pure` is structurally verified (not semantically proved), and type checking catches the common errors statically.

---

## Struct Invariants: Separate Analysis

```klar
struct SortedList {
    items: List#[i32]
    invariant is_sorted(items)
}
```

Struct invariants require checking the invariant expression after every method that mutates the struct. This means:

1. The checker must identify all `impl` methods that take `inout self`
2. Codegen must insert invariant checks after every mutation method body
3. The invariant expression must be evaluated in the struct's scope
4. Performance cost on every mutation (not just construction)

**Decision: No-go for invariants.** The mutation-tracking requirement is invasive (~800 LOC), the performance cost is unpredictable (invariant runs after every method), and the use cases are narrow (mostly data structure libraries, not application code). Users can call an `assert_invariant()` method explicitly where needed.

---

## Comparison: Proposals vs. Current Approach

| Feature | Assert (current) | Proposal A (keywords) | Proposal B (meta) | Proposal C (docs) |
|---------|-----------------|----------------------|-------------------|-------------------|
| Preconditions | Body-level assert | Signature-level `require` | Signature-level `meta require` | Comment only |
| Postconditions | Manual per-return | Automatic `ensure` | Automatic `meta ensure` | Comment only |
| Invariants | Manual method calls | `invariant` clause | Not supported | Comment only |
| Visible in docs | No | Yes | Yes (via `klar doc`) | Yes |
| Runtime cost | Per-call | Per-call | Per-call (togglable) | None |
| New keywords | None | 3 (`require`, `ensure`, `invariant`) | None | None |
| Implementation | Exists | ~1200 LOC | ~400 LOC | 0 LOC |
| Compiler-verified | No | Expression type-checked | Expression type-checked | No |

---

## Decision

**Go for Proposal B: `meta require` / `meta ensure` (runtime-checked contracts via meta system).**

**No-go for struct invariants, static analysis, and new keywords.**

### Rationale

1. **Natural fit.** Meta annotations already decorate functions with compiler-verified properties (`meta pure`). Adding `require`/`ensure` as meta variants extends a proven system — no new syntactic concepts.

2. **Postconditions are the killer feature.** `meta ensure(result >= 0)` automatically applies to all return paths. This can't be replicated with `assert` without duplicating checks at every return statement — a real source of bugs.

3. **Low implementation cost.** ~400 LOC total, compared to ~1200 LOC for new keywords or ~5000+ LOC for static analysis. The infrastructure (meta parsing, meta_validation.zig, codegen hooks) already exists.

4. **Documentation for free.** `klar doc` already extracts meta annotations. Contracts appear in generated documentation automatically.

5. **Togglable.** Runtime contract checks can be compiled out in release builds (like assertions in other languages) — a future optimization, not required for initial implementation.

### Specification

```klar
// Precondition: checked at function entry
meta require(arr.len() > 0)
meta require(target >= 0)
fn binary_search(arr: [i32], target: i32) -> ?i32 {
    // body
}

// Postcondition: checked before every return
// `result` is a special binding referring to the return value
meta ensure(result >= lo and result <= hi)
fn clamp(x: i32, lo: i32, hi: i32) -> i32 {
    if x < lo { return lo }    // ensure checked here
    if x > hi { return hi }    // and here
    return x                   // and here
}

// Combined
meta require(n >= 0)
meta ensure(result >= 1)
fn factorial(n: i32) -> i32 {
    if n == 0 { return 1 }
    return n * factorial(n - 1)
}
```

**Checker rules:**
- `meta require` expressions are type-checked in the function's parameter scope (can reference parameters but not local variables)
- `meta ensure` expressions are type-checked with an additional `result` binding of the function's return type
- Expressions must be of type `bool`
- Multiple `meta require` / `meta ensure` annotations are allowed (all must hold)
- Violations panic with a diagnostic message including the contract expression, file, and line

**Codegen:**
- `meta require`: insert assertion at function entry, before any user code
- `meta ensure`: wrap every `return expr` to evaluate `expr` into a temporary, check the contract with that temporary bound to `result`, then return the temporary

### What Could Change This Decision

- **Demand for struct invariants.** If Klar develops a rich data structure library ecosystem that needs invariant checking, we'd reconsider.
- **SMT integration becomes trivial.** If a lightweight, embeddable theorem prover emerges that can verify Klar expressions without Z3, static analysis becomes more attractive.
- **Performance sensitivity.** If contract checking overhead is measurable in production workloads, we'd add a `--release` flag to compile them out.
