# Incremental Compilation Evaluation

> **Status:** Evaluated — **Conditional go** for object file caching only. Frontend caching not worth the complexity.
> **Decision date:** 2026-03-15

---

## Motivation

As the Klar codebase grows, rebuild times become a friction point. The selfhost compiler (23K lines across 23 modules) takes ~3.7s for a debug build and ~29s with -O2 optimization. Incremental compilation would skip recompilation for unchanged modules, reducing rebuild times for typical edit-compile-test cycles.

---

## Current Build Profile

### Selfhost Build (23,887 lines, 23 modules)

| Phase | Time | % of Total |
|-------|------|------------|
| Parse + Type Check | 265ms | 7% |
| LLVM Codegen (debug) | ~3.4s | 92% |
| Linker | ~100ms | 1% |
| **Total (debug)** | **~3.7s** | **100%** |
| **Total (-O2)** | **~29s** | — |

### Per-Module Type Check Times

| Module | Lines | Parse | Check | Total |
|--------|-------|-------|-------|-------|
| checker.kl | 2,901 | 5.2ms | 46.9ms | 52.0ms |
| checker_expr.kl | 1,674 | 3.1ms | 76.0ms | 79.1ms |
| checker_stmt.kl | 506 | 1.1ms | 82.8ms | 83.9ms |
| emitter.kl | 1,863 | 4.9ms | 70.4ms | 75.3ms |
| parser_expr.kl | 989 | 2.6ms | 54.7ms | 57.3ms |
| lexer.kl | 977 | 2.3ms | 2.6ms | 4.9ms |
| types.kl | 923 | 2.0ms | 2.3ms | 4.3ms |

### Other Workloads

| Workload | Check Time | Build Time |
|----------|------------|------------|
| Synthetic 10K lines (single file) | 59ms | 1.6s |
| 69 check test files | — | 2.6s total |
| Selfhost multi-module | 265ms | 3.7s |

### Key Insight

**LLVM codegen is 92% of build time.** The frontend (parse + check) is already fast — 265ms for 24K lines. Caching frontend artifacts (AST, typed AST) would save at most ~250ms. Caching LLVM object files would save ~3.4s (debug) or ~28s (-O2).

---

## Caching Opportunities

### Level 1: Object File Cache (Module → .o)

Cache the LLVM-generated `.o` file for each module. On rebuild, if a module's source hash hasn't changed and no dependency has changed, reuse the cached `.o` and skip directly to linking.

| Dimension | Assessment |
|-----------|------------|
| **Speedup** | ~90% for unchanged modules (~3.4s → ~350ms for selfhost debug) |
| **Cache key** | SHA-256 of module source + all transitive dependency hashes + compiler flags |
| **Invalidation** | Content-hash based; any change to module or its dependencies triggers recompilation |
| **Storage** | `.klar-cache/` directory with `{hash}.o` files |
| **Implementation** | ~300 LOC: hash computation, cache directory management, hit/miss logic |

### Level 2: Typed AST Cache (Module → typed AST JSON)

Cache the typed AST JSON output per module. On rebuild, skip parse + check for unchanged modules.

| Dimension | Assessment |
|-----------|------------|
| **Speedup** | ~7% at best (~250ms for selfhost) |
| **Cache key** | Same as Level 1 |
| **Complexity** | Higher — must ensure cached typed AST is compatible with current checker state |
| **Risk** | Stale type information if checker semantics change between compiler versions |
| **Implementation** | ~500 LOC: serialization, deserialization, version checking |

### Level 3: LLVM IR Cache (Module → .ll)

Cache the LLVM IR text per module, then only run LLVM optimization + codegen on cache miss.

| Dimension | Assessment |
|-----------|------------|
| **Speedup** | Marginal over Level 1 — LLVM codegen and optimization are tightly coupled |
| **Complexity** | LLVM IR serialization is not a stable format between LLVM versions |
| **Risk** | IR format changes across LLVM upgrades; subtle ABI issues |
| **Implementation** | Not recommended |

---

## Analysis Against Klar's Principles

### Principle 1: "No ambiguity. No surprises."

Incremental compilation must be invisible to the developer — the output must be byte-identical whether built from cache or from scratch. If cache invalidation has bugs, developers get "it works on my machine" problems.

**Level 1 (object cache) is safe here.** Content-hash invalidation is deterministic. If the hash matches, the object file is identical. If it doesn't, we recompile. No ambiguity.

### Principle 2: "The code explains itself."

No impact — this is a build system optimization, not a language feature.

### Principle 3: "Explicitness earns its characters."

The cache should be transparent (no user-facing syntax) but have clear CLI visibility:

```bash
klar build main.kl           # uses cache automatically
klar build main.kl --no-cache # force full rebuild
klar clean                   # already clears build/ directory
```

---

## Invalidation Strategy

### Content-Hash Approach

For each module, compute:

```
module_hash = SHA-256(
    source_content +
    transitive_dependency_hashes +
    compiler_version +
    target_triple +
    optimization_level
)
```

Cache hit: `module_hash` matches a cached `.o` file → skip codegen.
Cache miss: recompile module, store new `.o` with `module_hash` filename.

### Dependency Graph

The module resolver already computes transitive imports (topological sort). This gives us the dependency graph for free. When module A imports module B:
- If B's hash changes, A must be recompiled (its dependency hash changed)
- If only A's source changes, only A is recompiled

### Typical Edit Patterns

| Edit | Modules Recompiled | Estimated Time |
|------|-------------------|---------------|
| Change a function body in checker_expr.kl | 1 (checker_expr.kl) | ~200ms |
| Add a field to TypeChecker in checker.kl | ~5 (checker.kl + dependents) | ~800ms |
| Change a type in types.kl | ~15 (most modules import types) | ~2.5s |
| Add a new stdlib module | 1 (new module only) | ~200ms |
| Full clean build | 23 (all modules) | ~3.7s |

For the common case (editing one function body), the speedup is **~95%** (3.7s → ~200ms).

---

## Decision

**Conditional go for Level 1 (object file caching) only.**

**No-go for Level 2 (typed AST cache) and Level 3 (LLVM IR cache).**

### Rationale

1. **92% of build time is LLVM codegen.** Caching object files captures almost all the potential speedup. Frontend caching (Level 2) would save ~250ms at higher complexity and risk.

2. **Low implementation cost.** ~300 LOC for hashing, cache directory management, and hit/miss logic. The module resolver already provides the dependency graph.

3. **Safe invalidation.** Content-hash based invalidation is deterministic and immune to timestamp-based races. If any doubt, the user can `klar clean` or `--no-cache`.

4. **Common case is fast.** Editing one function body in the selfhost rebuilds ~200ms instead of ~3.7s. The -O2 case drops from ~29s to ~2s for single-module changes.

### Not Implementing Now

Despite the go decision, implementation is **deferred** — the current 3.7s debug build time is acceptable for the project's current scale. The design is documented here for when build times become a bottleneck (likely when the selfhost exceeds ~50K lines or when -O2 builds are needed in the edit-compile-test loop).

### Implementation Sketch (When Needed)

1. Add `--no-cache` flag to `klar build`
2. Create `.klar-cache/` directory (gitignored)
3. After module resolution, compute content hashes for all modules
4. For each module: check `.klar-cache/{hash}.o` → if exists, skip codegen
5. Link all `.o` files (cached and fresh) as usual
6. `klar clean` removes `.klar-cache/`

### What Could Change This Decision

- **Build times exceed 10s in debug mode.** At current growth rates (~2K lines/month), this would happen around 60K+ lines. Object caching would bring it back under 1s for typical edits.
- **CI/CD integration.** If Klar projects use CI pipelines with clean builds, caching across builds (via shared cache directories) would significantly reduce CI times.
- **-O2 in edit loop.** If developers routinely build with -O2 (29s), caching becomes essential for productivity.
