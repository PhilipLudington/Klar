# Smart Pointers

Klar provides smart pointer types for shared ownership and interior mutability: `Rc[T]`, `Arc[T]`, and `Cell[T]`.

## Rc[T] - Reference Counted

`Rc[T]` provides shared ownership through reference counting. Multiple owners can share the same data, and the data is freed when the last owner drops.

### Creating Rc

```klar
let rc: Rc[i32] = Rc.new(42)
```

### Accessing the Value

Use `.get()` to access the wrapped value:

```klar
let rc: Rc[i32] = Rc.new(42)
let value: i32 = rc.get()  // 42
```

### Cloning (Shared Ownership)

Clone creates another owner of the same data:

```klar
let rc1: Rc[i32] = Rc.new(42)
let rc2: Rc[i32] = rc1.clone()  // Both point to same data

// rc1.get() == rc2.get() == 42
```

### Reference Count

Check how many owners exist:

```klar
let rc: Rc[i32] = Rc.new(42)
let count1: i32 = rc.ref_count()  // 1

let clone: Rc[i32] = rc.clone()
let count2: i32 = rc.ref_count()  // 2
```

### Dropping

When an `Rc` is dropped, the reference count decreases. The data is freed when the count reaches zero:

```klar
fn example() {
    let rc1: Rc[i32] = Rc.new(42)
    {
        let rc2: Rc[i32] = rc1.clone()
        // ref_count is 2
    }
    // rc2 dropped, ref_count is 1
}
// rc1 dropped, ref_count is 0, data freed
```

### Example: Shared Data Structure

```klar
struct Node {
    value: i32,
    next: ?Rc[Node],
}

fn create_list() -> Rc[Node] {
    let node3: Rc[Node] = Rc.new(Node { value: 3, next: None })
    let node2: Rc[Node] = Rc.new(Node { value: 2, next: Some(node3) })
    let node1: Rc[Node] = Rc.new(Node { value: 1, next: Some(node2) })
    return node1
}
```

## Arc[T] - Atomic Reference Counted

`Arc[T]` is like `Rc[T]` but safe for use across threads. Use `Arc` when data needs to be shared between threads.

### Creating Arc

```klar
let arc: Arc[i32] = Arc.new(42)
```

### Arc Methods

| Method | Description |
|--------|-------------|
| `Arc.new(value)` | Create new Arc |
| `.get()` | Access the value |
| `.clone()` | Create another owner |
| `.ref_count()` | Get reference count |

### Example: Thread-Safe Counter

```klar
let counter: Arc[i32] = Arc.new(0)

// Can be safely shared across threads
let thread1_counter: Arc[i32] = counter.clone()
let thread2_counter: Arc[i32] = counter.clone()
```

## Rc vs Arc

| Feature | Rc[T] | Arc[T] |
|---------|-------|--------|
| Thread-safe | No | Yes |
| Performance | Faster | Slower (atomic ops) |
| Use case | Single-threaded | Multi-threaded |

Use `Rc` when:
- All access is from a single thread
- Performance is critical

Use `Arc` when:
- Data is shared across threads
- Thread safety is required

## Cell[T] - Interior Mutability

`Cell[T]` provides interior mutability - the ability to mutate data even through immutable references.

### Creating Cell

```klar
let cell: Cell[i32] = Cell.new(42)
```

### Getting and Setting

```klar
let cell: Cell[i32] = Cell.new(42)

let value: i32 = cell.get()  // 42
cell.set(100)
let new_value: i32 = cell.get()  // 100
```

### Cell Methods

| Method | Description |
|--------|-------------|
| `Cell.new(value)` | Create new Cell |
| `.get()` | Get the current value |
| `.set(value)` | Set a new value |
| `.swap(value) -> T` | Set new value, return old |

### Example: Counter in Struct

```klar
struct Counter {
    count: Cell[i32],
}

impl Counter {
    fn new() -> Counter {
        return Counter { count: Cell.new(0) }
    }

    // Note: takes `ref self` (immutable) but can still modify count
    fn increment(ref self: Counter) {
        let current: i32 = self.count.get()
        self.count.set(current + 1)
    }

    fn get(ref self: Counter) -> i32 {
        return self.count.get()
    }
}
```

### When to Use Cell

Use `Cell` when you need to:
- Mutate a value inside an immutable struct
- Have mutable state without mutable references
- Implement patterns like lazy initialization

```klar
struct Config {
    settings: Map[string, string],
    cache: Cell[?ComputedValue],  // Lazy cached value
}
```

## Combining Smart Pointers

### Rc[Cell[T]] - Shared Mutable State

Combine `Rc` and `Cell` for shared mutable state:

```klar
let shared: Rc[Cell[i32]] = Rc.new(Cell.new(0))

let copy1: Rc[Cell[i32]] = shared.clone()
let copy2: Rc[Cell[i32]] = shared.clone()

// Both can modify the shared value
copy1.get().set(10)
let value: i32 = copy2.get().get()  // 10
```

### Arc[Cell[T]] - Thread-Safe Shared Mutable State

```klar
let shared: Arc[Cell[i32]] = Arc.new(Cell.new(0))

// Can be safely shared and mutated across threads
let thread1: Arc[Cell[i32]] = shared.clone()
let thread2: Arc[Cell[i32]] = shared.clone()
```

## Example: Shared Configuration

```klar
struct AppConfig {
    settings: Rc[Map[string, string]],
}

fn create_shared_config() -> (AppConfig, AppConfig) {
    let settings: Rc[Map[string, string]] = Rc.new(Map.new[string, string]())

    settings.get().insert("theme", "dark")
    settings.get().insert("language", "en")

    let config1: AppConfig = AppConfig { settings: settings.clone() }
    let config2: AppConfig = AppConfig { settings: settings }

    return (config1, config2)
}
```

## Example: Tree with Shared Nodes

```klar
struct TreeNode {
    value: i32,
    left: ?Rc[TreeNode],
    right: ?Rc[TreeNode],
}

fn create_shared_subtree() -> (Rc[TreeNode], Rc[TreeNode]) {
    // Shared subtree
    let shared: Rc[TreeNode] = Rc.new(TreeNode {
        value: 100,
        left: None,
        right: None,
    })

    let tree1: Rc[TreeNode] = Rc.new(TreeNode {
        value: 1,
        left: Some(shared.clone()),
        right: None,
    })

    let tree2: Rc[TreeNode] = Rc.new(TreeNode {
        value: 2,
        left: None,
        right: Some(shared),
    })

    // Both trees share the same subtree
    return (tree1, tree2)
}
```

## Best Practices

### Avoid Cycles with Rc

Reference cycles cause memory leaks because the count never reaches zero:

```klar
// BAD - creates a cycle
struct Node {
    value: i32,
    next: ?Rc[Node],
}

// To avoid cycles, consider:
// - Using weak references (when available)
// - Breaking the cycle manually before dropping
// - Restructuring to avoid shared ownership
```

### Prefer Ownership When Possible

Only use smart pointers when shared ownership is needed:

```klar
// If one owner is enough, use regular values
let data: MyStruct = MyStruct { ... }

// Only use Rc when sharing is necessary
let shared: Rc[MyStruct] = Rc.new(MyStruct { ... })
```

## Next Steps

- [Ownership](../memory/ownership.md) - Ownership model
- [References](../memory/references.md) - ref and inout
- [Reference Counting](../memory/reference-counting.md) - Detailed patterns
