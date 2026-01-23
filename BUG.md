# Klar Compiler Bug Reports

## Environment

- **Klar Version**: 0.3.1-dev
- **Platform**: macOS (Darwin 24.6.0)
- **Date**: 2026-01-23

## Previously Fixed Issues

- Arrays of structs - FIXED
- Enum variant construction (`Status::Active`) - FIXED
- Constants passed to functions as wrong type (f64 constants emitted as `i32 0`) - FIXED
- Array element boolean field access (Bug 1) - FIXED (commit eccc066)
- Array element string field access / get_policy_name pattern (Bug 2) - FIXED (commit eccc066)

---

All known codegen bugs have been fixed. The Narrow Window game now runs successfully.
