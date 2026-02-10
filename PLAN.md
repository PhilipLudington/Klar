# Klar Phase 5: Nanolang-Inspired Improvements

> **Goal:** Strengthen Klar's AI-native story with inline tests, checked arithmetic, an LLM reference file, FFI sandboxing, and formal verification.
>
> **Inspiration:** [Nanolang](https://github.com/jordanhubbard/nanolang) by Jordan Hubbard. Both languages share the "AI-native, no ambiguity" philosophy. See [docs/design/nanolang-inspiration.md](docs/design/nanolang-inspiration.md) for full analysis.

## Previous Phases

**Phase 4 (Language Completion):** All 13 milestones complete. Generics, traits, modules, stdlib (List, Map, Set, String, Option, Result), iterators, error handling (`?` operator), REPL, comptime, FFI (including function pointers), package manager, formatter, and doc generator all working.

> **Phase 4 archive:** [docs/history/phase4-language-completion.md](docs/history/phase4-language-completion.md)

---

## Milestone 1: LLM Reference File (MEMORY.md)

**Objective:** Create a single-file, token-efficient language reference optimized for LLM context windows.

**Status:** Not started

**Effort:** Low | **Impact:** High | **Dependencies:** None

### Why First

Immediate value with zero code changes. Improves AI code generation for Klar today. Consolidates docs currently spread across `docs/`, `CLAUDE.md`, and code comments.

### Tasks

- [ ] **1.1** Create `MEMORY.md` at repo root
  - Language version and feature list at top
  - Quick reference: all types, operators, keywords (table format)
  - Canonical patterns: one way to write each construct
    - Variables and functions
    - Structs, enums, traits
    - Generics
    - Error handling (Result, `?`, `??`)
    - Collections (List, Map, Set)
    - I/O and filesystem
    - FFI
    - Modules and packages
  - Common errors and fixes (table format)
  - Type conversion cheat sheet (`.as[T]`, `.to[T]`, `.trunc[T]`)
  - Standard library quick reference (all builtins with signatures)
  - Anti-patterns: what NOT to generate

- [ ] **1.2** Create `spec.json` companion file
  - All keywords and their roles
  - Type system rules
  - Operator precedence table
  - Grammar in simplified BNF
  - Built-in functions with signatures

### Design Principles

- Token-efficient: tables and code over prose
- One canonical form per construct
- Error-driven: include common mistakes (LLMs learn from negative examples)
- Self-contained: an LLM reading only this file can generate valid Klar
- Versioned: language version at top

### Success Criteria

- [ ] An LLM given only MEMORY.md can generate syntactically valid Klar code
- [ ] All language features are documented with exactly one canonical example
- [ ] Common error patterns have explicit fixes
- [ ] File fits comfortably in a single LLM context window (~1400 lines target)

---

## Milestone 2: Checked Arithmetic (`+?`, `-?`, `*?`, `/?`)

**Objective:** Add checked arithmetic operators that return `Result[T, OverflowError]`, completing Klar's four-mode overflow story.

**Status:** Not started

**Effort:** Low-Medium | **Impact:** Medium | **Dependencies:** None (Result type exists)

### Current Overflow Operators

| Operator | Behavior | Return Type | Status |
|----------|----------|-------------|--------|
| `+` | Panic (debug), wrap (release) | `T` | Exists |
| `+%` | Wrapping (modular) | `T` | Exists |
| `+|` | Saturating (clamp) | `T` | Exists |
| `+?` | Checked (return Result) | `Result[T, OverflowError]` | **NEW** |

Same pattern for `-`, `*`, `/`. `/?` also catches division by zero.

### Implementation Tasks

#### Phase 2A: Tokens and AST

- [ ] **2A.1** Add tokens to `src/token.zig`
  - `.plus_checked`, `.minus_checked`, `.star_checked`, `.slash_checked`
  - Add display strings: `"+?"`, `"-?"`, `"*?"`, `"/?"`
- [ ] **2A.2** Add lexer rules in `src/lexer.zig`
  - In `handlePlus()` (~line 247): check for `?` after `+`, emit `.plus_checked`
  - Same for `handleMinus()`, `handleStar()`, `handleSlash()`
- [ ] **2A.3** Add AST binary op variants in `src/ast.zig`
  - `add_checked`, `sub_checked`, `mul_checked`, `div_checked` in `BinaryOp` enum

#### Phase 2B: Type Checker

- [ ] **2B.1** Add `OverflowError` as a builtin enum type
  - Variants: `Overflow`, `Underflow`, `DivisionByZero`
  - Register in builtin type setup (similar to how `IoError` is registered)
- [ ] **2B.2** Type-check checked operators in `src/checker.zig`
  - Operands must be integer types (`i32`, `i64`, `u8`, etc.)
  - Result type is `Result[T, OverflowError]` where `T` matches operand type
  - `/?` also valid for integer division (catches division by zero)

#### Phase 2C: LLVM Codegen

- [ ] **2C.1** Emit checked arithmetic in `src/codegen/emit.zig`
  - Use existing `llvm.sadd.with.overflow.*` intrinsics (already cached at ~line 114)
  - Extract overflow bit from intrinsic result
  - Branch: overflow → construct `Err(OverflowError.Overflow)`, no overflow → construct `Ok(value)`
  - For `/?`: also check divisor == 0 → `Err(OverflowError.DivisionByZero)`
  - Distinguish `Overflow` vs `Underflow` by checking operand signs

#### Phase 2D: VM and Interpreter

- [ ] **2D.1** Add checked opcodes to bytecode compiler (`src/compiler.zig`)
- [ ] **2D.2** Add VM execution for checked opcodes (`src/vm.zig`)
  - Runtime overflow check, construct Result value
- [ ] **2D.3** Add interpreter handling (`src/interpreter.zig`)

#### Phase 2E: Tests and Formatter

- [ ] **2E.1** Create `test/native/checked_arithmetic/` directory
  - `checked_add.kl` — basic `+?` with Ok and Err cases
  - `checked_sub.kl` — `-?` with underflow
  - `checked_mul.kl` — `*?` with overflow
  - `checked_div.kl` — `/?` with division by zero
  - `checked_propagation.kl` — `(x +? y)?` with `?` operator
  - `checked_default.kl` — `(x +? y) ?? 0` with `??` operator
  - `checked_match.kl` — match on checked result
- [ ] **2E.2** Update formatter (`src/formatter.zig`) to handle `+?` operators
- [ ] **2E.3** Update doc generator if needed

### Success Criteria

- [ ] `+?`, `-?`, `*?`, `/?` parse and type-check correctly
- [ ] Overflow returns `Err(OverflowError.Overflow)`
- [ ] Underflow returns `Err(OverflowError.Underflow)`
- [ ] Division by zero returns `Err(OverflowError.DivisionByZero)`
- [ ] Composes with `?` operator for propagation
- [ ] Composes with `??` operator for default values
- [ ] All three backends produce correct results
- [ ] All tests pass

---

## Milestone 3: Inline Test Blocks (`test` keyword)

**Objective:** Add `test <name> { ... }` blocks for inline testing, with a `klar test` command to discover and run them.

**Status:** Not started (`klar test` command exists as stub in `src/main.zig` line ~365)

**Effort:** Medium | **Impact:** High | **Dependencies:** Milestone 2 (for `OverflowError` test examples, but not blocking)

### Design

```klar
fn gcd(a: i32, b: i32) -> i32 {
    if b == 0 { return a }
    return gcd(b, a % b)
}

test gcd {
    assert_eq(gcd(12, 8), 4)
    assert_eq(gcd(7, 0), 7)
    assert_eq(gcd(0, 5), 5)
}
```

### Implementation Tasks

#### Phase 3A: Lexer and Parser

- [ ] **3A.1** Add `test` keyword to lexer (`src/lexer.zig` ~line 441)
  - Add to keyword table alongside `trait`, `impl`, etc.
- [ ] **3A.2** Add `TestDecl` AST node (`src/ast.zig`)
  - Fields: `name: []const u8`, `body: []const *Statement`, `location: Location`
  - Add `test_decl: *TestDecl` variant to `Decl` union (~line 590)
- [ ] **3A.3** Parse `test` blocks in `src/parser.zig`
  - In `parseDeclaration()`: when token is `test`, parse name + block
  - Test block body uses same statement parsing as function bodies
  - Test blocks are top-level declarations, not nested inside functions

#### Phase 3B: Additional Test Assertions

- [ ] **3B.1** Add assertion builtins in `src/checker/builtins.zig`
  - `assert_ne(left: T, right: T)` — requires T: Eq, shows values on failure
  - `assert_err(result: Result[T, E])` — asserts Err variant
  - `assert_ok(result: Result[T, E])` — asserts Ok variant
  - `assert_some(opt: ?T)` — asserts Some variant
  - `assert_none(opt: ?T)` — asserts None variant
  - Note: `assert(bool)` and `assert_eq(T, T)` already exist

- [ ] **3B.2** Implement assertion builtins in all three backends
  - Interpreter: `src/interpreter.zig`
  - VM: `src/compiler.zig` + `src/vm.zig`
  - Native: `src/codegen/emit.zig`
  - On failure: print assertion type, location, and values (for `assert_eq`/`assert_ne`)

#### Phase 3C: Type Checker

- [ ] **3C.1** Validate test blocks in `src/checker.zig`
  - Test block name must reference an existing function in the same scope
  - Type-check all statements in the test body
  - Test blocks have access to all types and functions in scope
  - Test blocks do NOT have access to function-local state
- [ ] **3C.2** Add enforcement mode flags
  - `--strict-tests`: warn for `pub fn` without a `test` block
  - `--require-tests`: error for any `pub fn` without a `test` block
  - Default: test blocks optional

#### Phase 3D: Backend Support

- [ ] **3D.1** Conditional compilation in all backends
  - `klar run` / `klar build`: skip test blocks entirely (production mode)
  - `klar test`: compile and execute test blocks
- [ ] **3D.2** Native backend (`src/codegen/emit.zig`)
  - Generate test runner entry point when in test mode
  - Each test block becomes a callable function
- [ ] **3D.3** VM backend (`src/compiler.zig` + `src/vm.zig`)
  - Compile test blocks as separate callable units
- [ ] **3D.4** Interpreter backend (`src/interpreter.zig`)
  - Execute test blocks directly when in test mode

#### Phase 3E: `klar test` Command

- [ ] **3E.1** Implement `klar test` in `src/main.zig`
  - `klar test file.kl` — run all test blocks in file
  - `klar test file.kl --fn gcd` — run specific function's test block
  - `klar test dir/` — discover and run all `.kl` files with test blocks
- [ ] **3E.2** Test runner output format
  - Report pass/fail per test block with source location
  - Summary: N passed, M failed, K skipped
  - Exit code: 0 if all pass, 1 if any fail
- [ ] **3E.3** Integration with `run-tests.sh` and AirTower
  - Write results to `.test-results.json`

#### Phase 3F: Formatter and Tests

- [ ] **3F.1** Update formatter (`src/formatter.zig`)
  - Format `test` blocks with same indentation rules as function bodies
  - Preserve blank line between function and its test block
- [ ] **3F.2** Create test files
  - `test/native/test_blocks/basic_test.kl` — simple test block
  - `test/native/test_blocks/assertions.kl` — all assertion functions
  - `test/native/test_blocks/multiple_tests.kl` — multiple test blocks in one file
  - `test/native/test_blocks/test_failure.kl` — expected failure reporting
  - `test/native/test_blocks/strict_mode.kl` — `--strict-tests` behavior
  - `test/check/test_blocks/` — negative type-checking tests

### Success Criteria

- [ ] `test` keyword parses as top-level declaration
- [ ] Test blocks type-check and validate against referenced function
- [ ] `klar test file.kl` discovers and runs all test blocks
- [ ] `klar test file.kl --fn name` runs specific test
- [ ] `klar run` / `klar build` skip test blocks entirely
- [ ] All assertion builtins work: `assert`, `assert_eq`, `assert_ne`, `assert_err`, `assert_ok`, `assert_some`, `assert_none`
- [ ] `--strict-tests` warns, `--require-tests` errors on missing tests
- [ ] Formatter handles test blocks correctly
- [ ] All three backends support test execution

---

## Milestone 4: Process-Isolated FFI Sandbox

**Objective:** Add `--sandbox` mode that isolates FFI calls in a child process via IPC, so crashing C code cannot take down the Klar runtime.

**Status:** Not started

**Effort:** High | **Impact:** Medium | **Dependencies:** Milestone 12-13 (FFI, complete)

### Architecture

```
┌──────────────┐     IPC (pipes)          ┌──────────────┐
│  Klar VM     │ ◄──────────────────────► │  FFI Worker  │
│  (safe code) │   serialize args/results │  (C calls)   │
└──────────────┘                          └──────────────┘
       │                                         │
  Continues on                              Crashes here
  worker crash                              are contained
```

### Implementation Tasks

#### Phase 4A: REPL Sandbox (Highest Value)

- [ ] **4A.1** Design IPC protocol
  - Serialize: function name, argument types and values
  - Deserialize: return value or error (including crash signal)
  - Use POSIX pipes for macOS/Linux
- [ ] **4A.2** Implement FFI worker process
  - Child process loads shared libraries
  - Receives function call requests via pipe
  - Executes C function, sends result back
  - Parent detects SIGCHLD on crash, returns `FfiError`
- [ ] **4A.3** Add `--sandbox` flag to REPL
  - `klar repl --sandbox`
  - Fork worker on startup, reconnect on crash
- [ ] **4A.4** Add `FfiError` type
  - `WorkerCrashed`, `SerializationError`, `Timeout`
- [ ] **4A.5** Add `sandbox { expr }` syntax (optional sugar)
  - Returns `Result[T, FfiError]` instead of `T`

#### Phase 4B: Runtime Sandbox

- [ ] **4B.1** Add `--sandbox` flag to `klar run`
  - `klar run program.kl --sandbox`
  - All `extern fn` calls routed through worker
- [ ] **4B.2** Worker lifecycle management
  - Automatic restart after crash
  - Configurable timeout for FFI calls
  - Graceful shutdown on program exit

#### Phase 4C: Native Compilation (Stretch)

- [ ] **4C.1** Sandbox wrapper for native builds
  - Optional sandbox mode in compiled binaries
  - Runtime flag or compile-time selection

### Tests

- [ ] `test/native/sandbox/basic_sandbox.kl` — sandbox FFI call that succeeds
- [ ] `test/native/sandbox/crash_recovery.kl` — C function that crashes, recovered as error
- [ ] `test/native/sandbox/timeout.kl` — FFI call that hangs, timeout triggered

### Success Criteria

- [ ] `klar repl --sandbox` isolates FFI crashes
- [ ] Worker crash returns `FfiError` instead of killing session
- [ ] Worker auto-restarts for next call
- [ ] `klar run --sandbox` works for bytecode VM
- [ ] Serialization handles all FFI-compatible types

---

## Milestone 5: Formal Verification (Long-Term)

**Objective:** Formalize Klar's core type system in Lean 4 and prove type soundness, determinism, and exhaustiveness.

**Status:** Not started

**Effort:** Very High | **Impact:** Medium | **Dependencies:** Language stabilization

### Verified Subset (KlarCore)

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
| Determinism | Same program + same input = same result |
| Exhaustiveness correctness | Match expressions cover all variants |
| Ownership soundness | No use-after-move in the formal model |

### Implementation Tasks

- [ ] **5.1** Define KlarCore syntax as inductive type in Lean 4
- [ ] **5.2** Define typing rules as inductive relation
- [ ] **5.3** Define operational semantics (small-step)
- [ ] **5.4** Prove preservation theorem
- [ ] **5.5** Prove progress theorem
- [ ] **5.6** Prove determinism theorem
- [ ] **5.7** Prove match exhaustiveness correctness
- [ ] **5.8** Extract test oracle from formal model
- [ ] **5.9** CI step to verify proofs on language changes

### Deliverables

- `proofs/` directory with Lean 4 source
- `proofs/README.md` explaining what is proved and assumptions
- CI integration

### Success Criteria

- [ ] Core type system formalized in Lean 4
- [ ] Preservation and progress theorems proved
- [ ] Test oracle generates test cases that pass against real compiler
- [ ] Proofs checked in CI

---

## Implementation Order

Based on effort, impact, and dependencies:

```
1. MEMORY.md            Low effort,  High impact,  No deps       ← START HERE
2. Checked arithmetic   Low-Med,     Medium,       No deps
3. Inline test blocks   Medium,      High,         No deps
4. FFI sandbox          High,        Medium,       FFI complete
5. Formal verification  Very High,   Medium,       Language stable
```

Milestones 1 and 2 can proceed in parallel. Milestone 3 is the largest code change and defines Klar's testing story. Milestones 4 and 5 are longer-term investments.

---

## Remaining Phase 4 Items

These items from Phase 4 are still open and may be addressed alongside Phase 5:

| Item | Status |
|------|--------|
| LSP (Language Server Protocol) | Not started |
| VS Code extension | Not started |
| Async/Await | Stretch goal |
| Self-hosting | Stretch goal |
| WebAssembly target | Stretch goal |
| Windows support | Stretch goal |

---

## References

- [Nanolang GitHub](https://github.com/jordanhubbard/nanolang)
- [Nanolang MEMORY.md](https://github.com/jordanhubbard/nanolang/blob/main/MEMORY.md)
- [Design Analysis](docs/design/nanolang-inspiration.md)
