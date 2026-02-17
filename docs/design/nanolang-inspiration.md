# Nanolang-Inspired Improvements for Klar

> **Source:** Analysis of [Nanolang](https://github.com/jordanhubbard/nanolang) by Jordan Hubbard (FreeBSD co-founder, ex-Apple, NVIDIA).
> Both languages share the "AI-native, no ambiguity" philosophy. These are the opportunities where Nanolang's ideas can strengthen Klar.

---

## Opportunity 1: Inline Test Blocks (`test` keyword)

**Inspiration:** Nanolang's `shadow` blocks — mandatory inline tests for every function, enforced by the compiler.

```nano
fn double(x: int) -> int { return (* x 2) }
shadow double {
    assert (== (double 5) 10)
    assert (== (double 0) 0)
}
```

**Klar Design:**

```klar
fn gcd(a: i32, b: i32) -> i32 {
    if b == 0 { return a }
    return gcd(b, a % b)
}

test gcd {
    assert_eq(gcd(12, 8), 4)
    assert_eq(gcd(7, 0), 7)
    assert_eq(gcd(0, 5), 5)
    assert_eq(gcd(17, 17), 17)
}
```

### Semantics

- `test <name> { ... }` block declares inline tests for the preceding function
- Test blocks are compiled into the binary but only execute when:
  - `klar test file.kl` runs all tests in a file
  - `klar test file.kl --fn gcd` runs a specific function's tests
  - `klar run file.kl` does NOT run tests (production mode)
- Test blocks have access to the function and all types in scope
- Test blocks do NOT have access to the function's private state

### Built-in Test Assertions

```klar
assert(condition: bool)                    // panics with location on failure
assert_eq(left: T, right: T)              // requires T: Eq, shows both values
assert_ne(left: T, right: T)              // requires T: Eq
assert_err(result: Result[T, E])          // asserts Err variant
assert_ok(result: Result[T, E])           // asserts Ok variant
assert_some(opt: ?T)                      // asserts Some variant
assert_none(opt: ?T)                      // asserts None variant
```

### Enforcement Modes

| Mode | Behavior |
|------|----------|
| Default | `test` blocks are optional, compiler accepts either way |
| `--strict-tests` | Warning for `pub fn` without a `test` block |
| `--require-tests` | Error for any `pub fn` without a `test` block |

### Implementation Notes

- Parser: new `test` keyword, parsed as top-level declaration tied to a function name
- Checker: validate test block references an existing function, type-check assertions
- All three backends: conditionally compile test blocks based on build mode
- `klar test` command: discover and run all test blocks, report pass/fail with location
- Integration with `run-tests.sh` and AirTower

### Why This Matters for AI

LLMs generating Klar code can include inline tests as proof of correctness. When `--require-tests` is enabled, the compiler rejects code without tests — making "works but untested" impossible.

---

## Opportunity 2: LLM Reference File (MEMORY.md)

**Inspiration:** Nanolang ships a single `MEMORY.md` file (~1400 lines) designed specifically for LLM context windows — a complete, condensed language reference with canonical patterns, common errors, and debugging workflows.

**Klar Design:**

Create `MEMORY.md` at the repo root — a single-file, token-efficient language reference optimized for LLM consumption.

### Structure

```
MEMORY.md
├── Quick Reference (types, operators, keywords)
├── Canonical Patterns (one way to do each thing)
│   ├── Variables and functions
│   ├── Structs, enums, traits
│   ├── Generics
│   ├── Error handling (Result, ?, ??)
│   ├── Collections (List, Map, Set)
│   ├── I/O and filesystem
│   ├── FFI
│   └── Modules and packages
├── Common Errors & Fixes (table format)
├── Type Conversion Cheat Sheet
├── Standard Library Quick Reference
└── Anti-Patterns (what NOT to generate)
```

### Design Principles

- **Token-efficient:** No prose where a table or code example suffices
- **One canonical form:** For every construct, show THE way to write it
- **Error-driven:** Include common mistakes and their fixes (LLMs learn from negative examples)
- **Self-contained:** An LLM reading only this file can generate valid Klar code
- **Versioned:** Include language version at top so LLMs know which features exist

### Companion: spec.json

A machine-parseable language specification:
- All keywords and their roles
- Type system rules
- Operator precedence table
- Grammar in a simplified BNF
- Built-in functions with signatures

### Why This Matters for AI

Currently Klar's docs are spread across `docs/`, `CLAUDE.md`, and code comments. A purpose-built LLM reference reduces context window waste and eliminates ambiguity about "which way is correct."

---

## Opportunity 3: Process-Isolated FFI Sandbox

**Inspiration:** Nanolang's `nano_cop` runs FFI calls in a separate co-process via RPC. A crashing C library cannot take down the runtime.

**Klar Design:**

Add a `--sandbox` mode to `klar run` and the REPL that isolates FFI calls:

```bash
# Normal mode: FFI calls execute in-process (fast, unsafe)
klar run program.kl

# Sandbox mode: FFI calls execute in child process via IPC
klar run program.kl --sandbox

# REPL with sandboxed FFI
klar repl --sandbox
```

### Architecture

```
┌──────────────┐     IPC (pipes/sockets)     ┌──────────────┐
│  Klar VM     │ ◄──────────────────────────► │  FFI Worker  │
│  (safe code) │   serialize args/results     │  (C calls)   │
└──────────────┘                              └──────────────┘
       │                                             │
  Continues on                                  Crashes here
  worker crash                                  are contained
```

### Semantics

- Pure Klar code runs in the main process as normal
- When an `extern fn` is called, arguments are serialized and sent to the worker
- Worker executes the C function and returns the result
- If the worker crashes (segfault, abort), the main process receives an error:
  ```klar
  let result: Result[i32, FfiError] = sandbox { c_function(42) }
  ```
- Worker is automatically restarted for the next call

### Scope

- **Phase 1:** REPL sandbox (highest value — interactive FFI experimentation)
- **Phase 2:** `klar run --sandbox` for bytecode VM
- **Phase 3:** Native compilation with optional sandbox wrapper

### Why This Matters for AI

LLMs experimenting with FFI code in the REPL won't crash the session. Bad C bindings become recoverable errors instead of fatal crashes.

---

## Opportunity 4: Formal Verification of Core Type System

**Inspiration:** Nanolang's core semantics are mechanically verified in the Rocq Prover (Coq) with zero axioms, covering type soundness, determinism, and semantic equivalence.

**Klar Design:**

Formalize Klar's core type system in Lean 4 (more modern tooling than Coq) and prove key properties.

### Verified Subset (KlarCore)

Start with the core language subset:
- Primitive types: `i32`, `i64`, `f64`, `bool`, `string`
- Variables: `let`, `var`, assignment
- Functions: definition, calls, return
- Control flow: `if`/`else`, `while`, `for`
- Structs: definition, field access
- Enums: definition, `match` exhaustiveness
- Optionals: `?T`, `Some`, `None`, `??`
- Results: `Result[T, E]`, `Ok`, `Err`, `?` operator

### Properties to Prove

| Property | Meaning |
|----------|---------|
| Type soundness (preservation) | Well-typed programs remain well-typed after evaluation |
| Type soundness (progress) | Well-typed programs either produce a value or step forward |
| Determinism | Same program + same input = same result, always |
| Exhaustiveness correctness | Match expressions cover all variants |
| Ownership soundness | No use-after-move in the formal model |

### Implementation Plan

1. Define KlarCore syntax as an inductive type in Lean 4
2. Define typing rules as an inductive relation
3. Define operational semantics (small-step)
4. Prove preservation and progress theorems
5. Extract test oracle from formal model for cross-checking against real compiler

### Deliverables

- `proofs/` directory with Lean 4 source
- `proofs/README.md` explaining what is proved and what assumptions exist
- CI step that verifies proofs still hold after language changes

### Why This Matters for AI

A formally verified type system means LLMs can trust the compiler's error messages completely. If the compiler accepts code, it IS type-safe — no edge cases, no "works in practice but theoretically unsound."

---

## Opportunity 5: Checked Arithmetic Returning Result

**Inspiration:** Nanolang's `checked_add`, `checked_mul` etc. return `Result<T, string>` for overflow-safe arithmetic.

**Klar Design:**

Klar already has wrapping (`+%`) and saturating (`+|`) operators. Add checked operators that return `Result[T, OverflowError]`:

```klar
// Existing overflow operators
let a: i32 = x +% y    // wrapping: silently wraps on overflow
let b: i32 = x +| y    // saturating: clamps to min/max

// New: checked operators
let c: Result[i32, OverflowError] = x +? y    // returns Err on overflow
```

### Operator Table (Complete)

| Operator | Behavior | Return Type |
|----------|----------|-------------|
| `+` | Panic on overflow (debug), wrap (release) | `T` |
| `+%` | Wrapping (modular arithmetic) | `T` |
| `+|` | Saturating (clamp to bounds) | `T` |
| `+?` | Checked (return Result) | `Result[T, OverflowError]` |

Same pattern for `-`, `*`, `/`:
- `-?`, `*?`, `/?` all return `Result[T, OverflowError]`
- `/?` also catches division by zero

### OverflowError Enum

```klar
enum OverflowError {
    Overflow,
    Underflow,
    DivisionByZero,
}
```

### Usage Patterns

```klar
// With ? operator for propagation
fn safe_area(w: i32, h: i32) -> Result[i32, OverflowError] {
    let area: i32 = (w *? h)?
    return Ok(area)
}

// With ?? for default
let size: i32 = (count *? element_size) ?? i32.max()

// In validation
match x +? y {
    Ok(sum) => { println(sum.to_string()) }
    Err(e) => { println("arithmetic error") }
}
```

### Implementation Notes

- Parser: `+?`, `-?`, `*?`, `/?` as new binary operators
- Checker: operands must be integer types, result type is `Result[T, OverflowError]`
- LLVM codegen: use LLVM overflow intrinsics (`llvm.sadd.with.overflow.*`)
- VM/Interpreter: runtime overflow check, construct Result value

### Why This Matters for AI

Completes Klar's overflow-handling story. LLMs can choose the appropriate overflow behavior for each context: panic (default), wrap (crypto/hash), saturate (audio/graphics), or checked (financial/validation).

---

## Priority and Dependencies

| # | Opportunity | Effort | Impact | Dependencies |
|---|-----------|--------|--------|-------------|
| 1 | Inline test blocks | Medium | **High** | Parser, checker, all backends, `klar test` command |
| 2 | LLM reference (MEMORY.md) | Low | **High** | None (documentation only) |
| 3 | Process-isolated FFI | High | Medium | IPC mechanism, serialization, REPL changes |
| 4 | Formal verification | Very High | Medium | Lean 4 expertise, ongoing maintenance |
| 5 | Checked arithmetic (`+?`) | Low-Medium | Medium | Parser, checker, LLVM intrinsics |

### Suggested Order

1. **MEMORY.md** — Immediate value, no code changes, improves AI generation today
2. **Checked arithmetic** — Small scope, fills a gap in existing operator design
3. **Inline test blocks** — Major feature, defines Klar's testing story
4. **Process-isolated FFI** — Nice-to-have for REPL safety
5. **Formal verification** — Long-term investment, start after language stabilizes

---

## References

- [Nanolang GitHub](https://github.com/jordanhubbard/nanolang)
- [Nanolang MEMORY.md](https://github.com/jordanhubbard/nanolang/blob/main/MEMORY.md)
- [Hacker News Discussion](https://news.ycombinator.com/item?id=46684958)
- [Simon Willison's coverage](https://simonwillison.net/2026/Jan/19/nanolang/)
