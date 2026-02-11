# Repository Guidelines

## Project Structure & Module Organization
Klar is a Zig-based language implementation.
- `src/`: compiler and runtime internals (lexer, parser, checker, VM, LLVM codegen, CLI in `src/main.zig`).
- `std/`: Klar standard library sources.
- `test/`: test suites (`native/`, `module/`, `args/`, `fmt/`, `check/`, `pkg/`).
- `scripts/`: suite-specific test runners.
- `examples/` and `benchmarks/`: sample programs and performance cases.
- `docs/`: language and design documentation.

## Build, Test, and Development Commands
- `./run-build.sh`: builds with `zig build` and writes `.build-results.json`.
- `./run-tests.sh`: full CI-style local run (build + all suites).
- `./scripts/run-unit-tests.sh`: Zig unit tests.
- `./scripts/run-native-tests.sh`: compile/run native Klar programs in `test/native/`.
- `./scripts/run-module-tests.sh`, `./scripts/run-fmt-tests.sh`, `./scripts/run-check-tests.sh`: focused suites.
- `./scripts/run-benchmarks.sh`: benchmark runs.

Use wrapper scripts for normal development (`./run-build.sh`, `./run-tests.sh`); avoid raw `zig build`/`zig build test` so JSON result files remain consistent.
For any testing task, always run `./run-tests.sh` (not individual suite scripts) so AirTower captures full repository health visibility.

Run `./run-tests.sh` before opening a PR.

## Coding Style & Naming Conventions
- Follow CarbideZig standards (`carbide/CARBIDE.md`): 4-space indentation, max ~120 columns, prefer `const`, explicit error handling.
- Keep compiler code split by concern (e.g., checker logic in `src/checker/*`, codegen in `src/codegen/*`).
- Klar examples/tests should be explicit and readable; function declarations require explicit return types (use `-> void` when applicable).
- Test file names use descriptive snake_case, e.g., `result_map_err.kl`, `missing_return_type.kl`.

## Testing Guidelines
- Add tests alongside feature areas (e.g., parser/checker/codegen changes should include `test/native/` coverage).
- For expected output tests, include `// EXPECTED:` comments; for negative tests, include `// ERROR:` comments.
- Prefer small, single-purpose test programs.
- For language feature changes, update parser, AST, checker, interpreter, VM, and native codegen together, then add regression tests.

## Debugging & Scratch Workflow
- Prefer `./zig-out/bin/klar run file.kl` for fast iteration instead of compiling temporary binaries.
- Useful debug flags: `--interpret` (tree-walking backend), `--emit-ir` (internal IR), `--emit-llvm` (LLVM IR).
- Use `scratch/` for throwaway experiments, temporary fixtures, and investigation notes; keep production changes out of it.

## Design Guardrails
- Keep changes aligned with Klar principles: unambiguous syntax, explicit types/conversions, and no undefined behavior.

## Commit & Pull Request Guidelines
- Use focused branches: `feature/...`, `fix/...`, `docs/...`, `refactor/...`.
- Commit messages should be imperative and specific; conventional commit style is preferred (e.g., `fix(parser): require explicit return type`).
- PRs should include: concise summary, rationale, test coverage updates, and linked issue(s) when applicable.
- Do not push directly to `main`; use PRs and keep scope tight.
