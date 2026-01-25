# Modules

Klar uses a module system for organizing code across multiple files.

## Basic Imports

### Selective Import

Import specific items from a module:

```klar
import utils.{ greet, add }

fn main() -> i32 {
    greet()
    return add(10, 20)
}
```

### Glob Import

Import all public items from a module:

```klar
import utils.*

fn main() -> i32 {
    let sum: i32 = add(10, 20)
    let product: i32 = multiply(3, 4)
    return sum + product
}
```

### Aliased Import

Rename imports to avoid conflicts or for clarity:

```klar
import math.{ add as sum, multiply as product }

fn main() -> i32 {
    let a: i32 = sum(10, 20)       // Uses math.add
    let b: i32 = product(3, 4)    // Uses math.multiply
    return a + b
}
```

## Visibility

### Public Items (pub)

Use `pub` to make items visible to other modules:

```klar
// utils.kl
pub fn greet() -> i32 {
    return 42
}

pub fn add(a: i32, b: i32) -> i32 {
    return a + b
}

// Private - not importable
fn private_helper() -> i32 {
    return 100
}
```

### Private by Default

Items without `pub` are private to their module:

```klar
// mymodule.kl
fn internal_function() -> i32 {
    return helper()
}

fn helper() -> i32 {
    return 42
}

pub fn public_api() -> i32 {
    return internal_function()
}
```

Only `public_api` can be imported from other modules.

## File Organization

### One File = One Module

Each `.kl` file is a module. The module name is the filename without extension:

```
project/
├── main.kl         # Main module
├── utils.kl        # utils module
└── math.kl         # math module
```

### Nested Modules

Use directories for nested modules:

```
project/
├── main.kl
└── lib/
    └── math.kl     # lib/math module
```

Import nested modules with path:

```klar
import lib/math.{ add }
```

## Module Resolution

When you write `import utils.{ add }`, Klar looks for:

1. `utils.kl` in the same directory as the current file
2. `utils/mod.kl` if `utils` is a directory

## Public Structs and Enums

Make types public with `pub`:

```klar
// types.kl
pub struct Point {
    pub x: i32,
    pub y: i32,
}

pub enum Color {
    Red,
    Green,
    Blue,
}
```

Fields can also have visibility modifiers:

```klar
pub struct User {
    pub name: string,      // Public field
    password: string,      // Private field (same-module only)
}
```

## Public Traits

```klar
// traits.kl
pub trait Drawable {
    fn draw(self: Self)
}
```

## Example: Multi-File Project

### Project Structure

```
project/
├── main.kl
├── shapes.kl
└── utils.kl
```

### shapes.kl

```klar
pub struct Point {
    pub x: i32,
    pub y: i32,
}

pub struct Rectangle {
    pub top_left: Point,
    pub width: i32,
    pub height: i32,
}

impl Rectangle {
    pub fn area(self: Rectangle) -> i32 {
        return self.width * self.height
    }
}
```

### utils.kl

```klar
pub fn max(a: i32, b: i32) -> i32 {
    if a > b {
        return a
    }
    return b
}

pub fn min(a: i32, b: i32) -> i32 {
    if a < b {
        return a
    }
    return b
}
```

### main.kl

```klar
import shapes.{ Point, Rectangle }
import utils.{ max }

fn main() -> i32 {
    let rect: Rectangle = Rectangle {
        top_left: Point { x: 0, y: 0 },
        width: 10,
        height: 20,
    }

    let area: i32 = rect.area()
    let bigger: i32 = max(area, 100)

    return bigger
}
```

## Circular Imports

Klar supports circular imports between modules:

```klar
// module_a.kl
import module_b.{ func_b }

pub fn func_a() -> i32 {
    return func_b() + 1
}

// module_b.kl
import module_a.{ func_a }

pub fn func_b() -> i32 {
    return 42
}
```

The compiler handles circular dependencies during type checking.

## Best Practices

### Organize by Feature

Group related functionality in modules:

```
project/
├── main.kl
├── user/
│   ├── mod.kl        # User types and functions
│   └── auth.kl       # Authentication logic
├── database/
│   ├── mod.kl        # Database connection
│   └── queries.kl    # Query helpers
└── utils.kl          # General utilities
```

### Minimize Public API

Only expose what's needed:

```klar
// Good - minimal public API
pub fn connect() -> Connection { ... }
fn internal_setup() { ... }
fn validate_config() { ... }

// Bad - too much exposed
pub fn connect() -> Connection { ... }
pub fn internal_setup() { ... }  // Should be private
pub fn validate_config() { ... } // Should be private
```

### Use Descriptive Module Names

```klar
// Good
import authentication.{ login, logout }
import database.{ query, execute }

// Less clear
import auth.{ do_login }
import db.{ run }
```

## Next Steps

- [Structs](structs.md) - Public struct definitions
- [Traits](traits.md) - Public trait definitions
- [Functions](functions.md) - Function visibility
