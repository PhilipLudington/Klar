# Klar Implementation Plan

## Current Status

**Phase 1: Tree-Walking Interpreter** — COMPLETE ✓
- Lexer with all token types
- Parser with full expression/statement support
- Type checker with inference
- Interpreter with all core features
- Built-in functions and methods
- String interpolation
- Closures with lexical scoping

---

## Phase 2: Bytecode Virtual Machine

Transform the tree-walking interpreter into a bytecode compiler and stack-based VM for improved performance.

### Milestone 1: Bytecode Instruction Set Design ✓

Design a compact, efficient instruction set for the Klar VM.

#### Tasks

- [x] **1.1** Define core bytecode format and encoding
  - Single-byte opcodes with variable-length operands
  - Constant pool for literals (strings, numbers, functions)
  - Consider alignment and cache-friendliness

- [x] **1.2** Define stack manipulation instructions
  - `OP_CONST` - Push constant from pool
  - `OP_POP` - Pop and discard top of stack
  - `OP_DUP` - Duplicate top of stack

- [x] **1.3** Define arithmetic instructions
  - `OP_ADD`, `OP_SUB`, `OP_MUL`, `OP_DIV`, `OP_MOD`
  - `OP_ADD_WRAP`, `OP_SUB_WRAP`, `OP_MUL_WRAP` (wrapping)
  - `OP_ADD_SAT`, `OP_SUB_SAT`, `OP_MUL_SAT` (saturating)
  - `OP_NEG` (unary negation)

- [x] **1.4** Define comparison and logical instructions
  - `OP_EQ`, `OP_NE`, `OP_LT`, `OP_GT`, `OP_LE`, `OP_GE`
  - `OP_NOT`, `OP_AND`, `OP_OR`

- [x] **1.5** Define bitwise instructions
  - `OP_BIT_AND`, `OP_BIT_OR`, `OP_BIT_XOR`, `OP_BIT_NOT`
  - `OP_SHL`, `OP_SHR`

- [x] **1.6** Define control flow instructions
  - `OP_JUMP` - Unconditional jump
  - `OP_JUMP_IF_FALSE` - Conditional jump
  - `OP_LOOP` - Backward jump (for loops)

- [x] **1.7** Define variable access instructions
  - `OP_GET_LOCAL` - Read local variable by slot index
  - `OP_SET_LOCAL` - Write local variable by slot index
  - `OP_GET_GLOBAL` - Read global by name (constant pool index)
  - `OP_SET_GLOBAL` - Write global by name

- [x] **1.8** Define function instructions
  - `OP_CALL` - Call function with N arguments
  - `OP_RETURN` - Return from function
  - `OP_CLOSURE` - Create closure capturing upvalues

- [x] **1.9** Define composite type instructions
  - `OP_ARRAY` - Create array from N stack values
  - `OP_TUPLE` - Create tuple from N stack values
  - `OP_STRUCT` - Create struct with field names
  - `OP_GET_INDEX` - Array/tuple index access
  - `OP_SET_INDEX` - Array/tuple index assignment
  - `OP_GET_FIELD` - Struct field access
  - `OP_SET_FIELD` - Struct field assignment

- [x] **1.10** Define optional/result instructions
  - `OP_SOME` - Wrap value in Some
  - `OP_NONE` - Push None
  - `OP_UNWRAP` - Unwrap optional (trap on None)
  - `OP_UNWRAP_OR` - Unwrap with default

#### Deliverables
- `src/bytecode.zig` - Opcode enum and bytecode chunk structure ✓
- `src/chunk.zig` - Bytecode container with constant pool ✓

---

### Milestone 2: Compiler (AST to Bytecode)

Implement a single-pass compiler that transforms AST nodes into bytecode.

#### Tasks

- [x] **2.1** Create compiler infrastructure
  - Compiler struct with bytecode output
  - Scope tracking for local variable slots
  - Constant pool management

- [x] **2.2** Compile literal expressions
  - Numbers → constant pool + OP_CONST
  - Strings → constant pool + OP_CONST
  - Booleans → OP_TRUE / OP_FALSE
  - None → OP_NONE

- [x] **2.3** Compile binary expressions
  - Arithmetic: emit left, emit right, emit op
  - Comparison: same pattern
  - Short-circuit `and`/`or` with jumps

- [x] **2.4** Compile unary expressions
  - Negation, not, references

- [x] **2.5** Compile variable declarations and access
  - Track local variable slots in scope
  - Resolve at compile-time where possible
  - Handle shadowing correctly

- [x] **2.6** Compile control flow
  - If/else with jump patching
  - While loops with backward jumps
  - For loops (desugar to while)
  - Match expressions with jump tables

- [x] **2.7** Compile functions
  - Compile function body to separate chunk
  - Parameter binding as locals
  - Return statement handling

- [x] **2.8** Compile closures
  - Upvalue resolution
  - Closure creation with captured variables

- [x] **2.9** Compile pattern matching
  - Match arm compilation
  - Guard condition handling
  - Variable binding in patterns

- [x] **2.10** Compile method calls and type conversions
  - Built-in method dispatch
  - Type cast operations

#### Deliverables
- `src/compiler.zig` - AST to bytecode compiler
- Unit tests for each compilation phase

---

### Milestone 3: Virtual Machine Core ✓

Implement the stack-based bytecode interpreter.

#### Tasks

- [x] **3.1** Create VM infrastructure
  - Value stack (fixed size or growable)
  - Call stack for function frames
  - Global variable table
  - Instruction pointer management

- [x] **3.2** Implement value representation
  - Tagged union for runtime values
  - NaN-boxing consideration for performance
  - Object heap for strings, arrays, closures

- [x] **3.3** Implement stack operations
  - Push, pop, peek
  - Stack underflow/overflow checking

- [x] **3.4** Implement arithmetic execution
  - All numeric operations
  - Overflow behavior (trap/wrap/saturate)
  - Type checking at runtime

- [x] **3.5** Implement comparison and logical ops
  - Equality, ordering
  - Short-circuit evaluation

- [x] **3.6** Implement control flow execution
  - Jump instructions
  - Loop back-edges

- [x] **3.7** Implement variable access
  - Local variable slots in call frame
  - Global variable hash table

- [x] **3.8** Implement function calls
  - Call frame creation
  - Argument passing
  - Return value handling

- [x] **3.9** Implement closures
  - Upvalue objects
  - Closure invocation

- [x] **3.10** Implement composite types
  - Array/tuple creation and indexing
  - Struct field access

#### Deliverables
- `src/vm.zig` - Virtual machine implementation ✓
- `src/vm_value.zig` - Runtime value representation ✓
- Integration tests running example programs

---

### Milestone 4: Built-in Functions and Methods ✓

Re-implement all builtins for the VM.

#### Tasks

- [x] **4.1** Core output functions
  - `print`, `println` for VM

- [x] **4.2** Assertion functions
  - `assert`, `assert_eq`, `panic`

- [x] **4.3** Utility functions
  - `len`, `type_of`

- [x] **4.4** String methods
  - `len`, `is_empty`, `contains`
  - `starts_with`, `ends_with`, `trim`
  - `to_uppercase`, `to_lowercase`
  - `chars`, `bytes`

- [x] **4.5** Array methods
  - `len`, `is_empty`, `first`, `last`, `get`
  - `contains`

- [x] **4.6** Integer methods
  - `abs`, `min`, `max`

- [x] **4.7** Optional methods
  - `is_some`, `is_none`, `unwrap`, `unwrap_or`, `expect`

#### Deliverables
- `src/vm_builtins.zig` - Built-in native function implementations ✓
- Built-in function dispatch in VM ✓
- Method call resolution ✓

---

### Milestone 5: Memory Management

Implement proper memory management for the VM.

#### Tasks

- [ ] **5.1** Object allocation
  - Heap allocation for objects
  - Object headers for GC

- [ ] **5.2** String interning
  - Deduplicate identical strings
  - Intern table

- [ ] **5.3** Mark-sweep garbage collection
  - Root marking (stack, globals)
  - Object graph traversal
  - Sweep and reclaim

- [ ] **5.4** GC triggering
  - Allocation threshold
  - Stress test mode for debugging

#### Deliverables
- `src/gc.zig` - Garbage collector
- Memory leak tests

---

### Milestone 6: Debugging and Tooling

Add debugging support and developer tools.

#### Tasks

- [ ] **6.1** Disassembler
  - Pretty-print bytecode
  - Show constant pool

- [ ] **6.2** Source maps
  - Line number tracking
  - Error messages with source locations

- [ ] **6.3** Stack traces
  - Function name tracking
  - Call stack on errors

- [ ] **6.4** Debug mode
  - Instruction tracing
  - Stack inspection

#### Deliverables
- `zig build run -- disasm file.kl` command
- Rich error messages with line numbers

---

### Milestone 7: Integration and Testing

Ensure the VM passes all existing tests and runs example programs.

#### Tasks

- [ ] **7.1** Port interpreter tests to VM
  - Arithmetic tests
  - Control flow tests
  - Function tests
  - Pattern matching tests

- [ ] **7.2** Run example programs
  - `hello.kl`, `fibonacci.kl`, `fizzbuzz.kl`
  - Verify output matches interpreter

- [ ] **7.3** Performance comparison
  - Benchmark vs tree-walking interpreter
  - Identify optimization opportunities

- [ ] **7.4** CLI integration
  - `klar run` uses VM by default
  - `klar run --interpret` for tree-walker (debug)

#### Deliverables
- All tests passing on VM
- Performance benchmark results

---

## Phase 3: Native Compilation (Future)

Compile bytecode to native code using Cranelift or LLVM.

### Planned Features
- JIT compilation for hot paths
- AOT compilation for deployment
- Platform-specific optimizations

---

## Phase 4: Self-Hosting (Future)

Rewrite the Klar compiler in Klar itself.

### Planned Steps
1. Implement Klar lexer in Klar
2. Implement Klar parser in Klar
3. Implement Klar type checker in Klar
4. Implement Klar bytecode compiler in Klar
5. Bootstrap: compile Klar compiler with itself

---

## Architecture Overview

```
Source Code (.kl)
       │
       ▼
┌─────────────┐
│   Lexer     │  src/lexer.zig
└─────────────┘
       │ Tokens
       ▼
┌─────────────┐
│   Parser    │  src/parser.zig
└─────────────┘
       │ AST
       ▼
┌─────────────┐
│   Checker   │  src/checker.zig
└─────────────┘
       │ Typed AST
       ▼
┌─────────────┐
│  Compiler   │  src/compiler.zig  ← Phase 2
└─────────────┘
       │ Bytecode
       ▼
┌─────────────┐
│     VM      │  src/vm.zig        ← Phase 2
└─────────────┘
       │
       ▼
    Output
```

---

## File Structure (After Phase 2)

```
src/
├── main.zig           # CLI entry point
├── lexer.zig          # Tokenizer
├── token.zig          # Token types
├── parser.zig         # AST parser
├── ast.zig            # AST node types
├── types.zig          # Type system
├── checker.zig        # Type checker
├── interpreter.zig    # Tree-walking interpreter (Phase 1)
├── values.zig         # Interpreter values
├── bytecode.zig       # Opcode definitions        ← NEW
├── chunk.zig          # Bytecode container        ← NEW
├── compiler.zig       # AST → Bytecode            ← NEW
├── vm.zig             # Virtual machine           ← NEW
├── vm_value.zig       # VM runtime values         ← NEW
├── vm_builtins.zig    # VM built-in functions     ← NEW
├── gc.zig             # Garbage collector         ← NEW
└── disasm.zig         # Disassembler              ← NEW
```

---

## Success Criteria for Phase 2

1. **Correctness**: All Phase 1 tests pass on the VM
2. **Performance**: At least 5x faster than tree-walking interpreter
3. **Memory**: No memory leaks under stress testing
4. **Debugging**: Clear error messages with source locations
5. **Examples**: All example programs run correctly

---

*Last updated: January 14, 2026*
