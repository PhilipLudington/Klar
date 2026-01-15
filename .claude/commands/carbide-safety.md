# CarbideZig Security Review

Perform a security-focused code review identifying potential vulnerabilities.

## Arguments

- `$ARGUMENTS` - File path or directory to review (defaults to `src/`)

## Instructions

Review code for security issues in these categories:

### 1. Input Validation (CWE-20)
- [ ] All external input validated before use
- [ ] Length checks on strings and buffers
- [ ] Range checks on numeric values
- [ ] Format validation for structured data (URLs, emails, etc.)

### 2. Buffer Safety (CWE-119, CWE-120)
- [ ] No manual pointer arithmetic without bounds checks
- [ ] Slice operations within bounds
- [ ] No unchecked array indexing from external input
- [ ] Sentinel-terminated strings properly handled

### 3. Integer Safety (CWE-190, CWE-191)
- [ ] Overflow-prone operations use checked arithmetic
- [ ] Size calculations validated before allocation
- [ ] Cast operations checked for truncation
- [ ] Saturating arithmetic used where appropriate

### 4. Memory Safety (CWE-416, CWE-415)
- [ ] No use-after-free patterns
- [ ] No double-free patterns
- [ ] Proper `defer`/`errdefer` ordering
- [ ] Lifetime of returned slices documented

### 5. Error Handling (CWE-754)
- [ ] No silently ignored errors
- [ ] Fallible operations use error unions
- [ ] Error paths don't leak resources
- [ ] Panic only for truly unrecoverable states

### 6. Sensitive Data (CWE-312, CWE-532)
- [ ] Credentials not logged
- [ ] Sensitive data zeroed before free
- [ ] No hardcoded secrets
- [ ] Proper handling of authentication data

### 7. Concurrency (CWE-362, CWE-367)
- [ ] Shared mutable state protected by mutex
- [ ] Atomic operations used correctly
- [ ] No TOCTOU vulnerabilities
- [ ] Thread safety documented

### 8. Build Configuration
- [ ] ReleaseSafe used for production (not ReleaseFast)
- [ ] Safety checks not disabled without justification
- [ ] Debug assertions for invariants

## Output Format

```
## Security Review Results

### Critical Vulnerabilities
1. [CWE-XXX] [File:Line] Description
   Risk: High/Medium/Low
   Remediation: How to fix

### Warnings
1. [CWE-XXX] [File:Line] Description
   Risk: Medium/Low
   Remediation: How to fix

### Informational
1. [File:Line] Description
   Suggestion: Improvement

### Summary
- Critical: X
- Warnings: Y
- Informational: Z
- Overall Risk: HIGH/MEDIUM/LOW
```

## References

- OWASP Top 10
- CWE Top 25
- Zig Safety Documentation
