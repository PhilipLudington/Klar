# CarbideZig Code Review

Perform a comprehensive code review against CarbideZig standards.

## Arguments

- `$ARGUMENTS` - File path or directory to review (defaults to `src/`)

## Instructions

Review the specified code against these CarbideZig standard categories:

### 1. Naming Conventions
- [ ] Types use PascalCase
- [ ] Functions and methods use camelCase
- [ ] Variables and fields use camelCase
- [ ] Compile-time constants use snake_case
- [ ] Function names follow patterns (init/deinit, is*/has*, etc.)

### 2. Memory Management
- [ ] Functions that allocate accept Allocator parameter
- [ ] Ownership is documented for returned pointers/slices
- [ ] `defer` placed immediately after resource acquisition
- [ ] `errdefer` used for error-path cleanup
- [ ] Structs have proper init/deinit lifecycle

### 3. Error Handling
- [ ] Domain-specific error sets defined
- [ ] Error sets suffixed with "Error"
- [ ] `try` used for propagation, `catch` for handling
- [ ] No silently ignored errors
- [ ] `errdefer` chains ordered correctly

### 4. API Design
- [ ] Functions accept slices, not pointer+length
- [ ] Optional types used instead of magic values
- [ ] Configuration structs have defaults
- [ ] Multiple return values use structs
- [ ] Const correctness maintained

### 5. Code Organization
- [ ] Imports ordered: std, external, local
- [ ] Public declarations before private
- [ ] Tests at bottom of file
- [ ] Module-level documentation present

### 6. Documentation
- [ ] Public functions have doc comments
- [ ] Ownership documented for pointer returns
- [ ] Thread safety documented
- [ ] Examples provided for complex APIs

### 7. Testing
- [ ] Tests use std.testing.allocator
- [ ] Test names follow "Subject behavior" pattern
- [ ] Error paths tested
- [ ] Edge cases covered

### 8. Security
- [ ] External input validated
- [ ] Size limits defined and enforced
- [ ] No unchecked arithmetic in release paths
- [ ] Sensitive data handling appropriate

## Output Format

Provide findings as:

```
## Summary
- X issues found (Y critical, Z warnings)

## Critical Issues
1. [File:Line] Description

## Warnings
1. [File:Line] Description

## Suggestions
1. [File:Line] Description
```
