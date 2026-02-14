# Async Runtime Internals

This document defines internal async runtime conventions used by current Klar backends.

## Scope

- Internal implementation detail only.
- Not a stable public ABI.
- `Future[T]` layout and state tags may change without compatibility guarantees.

## Future State Tags

Current numeric mapping used by runtime enums and native codegen:

- `0` = `pending`
- `1` = `completed`
- `2` = `failed`
- `3` = `cancelled`

Interpreter and VM use enum-based state representations with equivalent semantics.

## Await Semantics (Current)

- `await` on `Future[T]` in `completed` state yields `T` (or `void` payload for `Future[void]`).
- `await` on `pending`/`failed`/`cancelled` is a runtime error with message:
  - `runtime error: await on non-completed Future`
- CLI backend behavior is standardized to exit non-zero (`exit code 1`) on these runtime errors.

## Consistency Requirement

When changing async runtime behavior, keep these in sync:

- `src/interpreter.zig`
- `src/vm.zig`
- `src/codegen/emit.zig`
- runtime unit tests in `src/interpreter.zig` and `src/vm.zig`
- native raw-layout smoke test in `test/native/async_await_pending_error.kl`
