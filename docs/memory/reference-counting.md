# Reference Counting

When values need to be shared between multiple owners, Klar provides reference-counted smart pointers: `Rc[T]` for single-threaded use and `Arc[T]` for multi-threaded use.

## When to Use Reference Counting

Use reference counting when:

- Multiple parts of code need to own the same data
- The ownership graph is complex (not a simple tree)
- You need shared data structures (graphs, caches, etc.)
- The exact lifetime of data isn't known at compile time

## Rc[T] - Single-Threaded Shared Ownership

### Creating Rc

```klar
let data: Rc[i32] = Rc.new(42)
```

### Sharing Ownership

Use `.clone()` to create another owner:

```klar
let original: Rc[string] = Rc.new("shared data")
let copy1: Rc[string] = original.clone()
let copy2: Rc[string] = original.clone()

// All three point to the same string
println(original.get())  // "shared data"
println(copy1.get())     // "shared data"
println(copy2.get())     // "shared data"
```

### Automatic Cleanup

The data is freed when the last owner is dropped:

```klar
fn example() {
    let rc1: Rc[i32] = Rc.new(42)
    let rc2: Rc[i32] = rc1.clone()

    println(rc1.ref_count())  // 2

    {
        let rc3: Rc[i32] = rc1.clone()
        println(rc1.ref_count())  // 3
    }  // rc3 dropped, count = 2

    println(rc1.ref_count())  // 2
}  // rc1 and rc2 dropped, count = 0, data freed
```

### Example: Shared Configuration

```klar
struct Config {
    theme: string,
    language: string,
}

struct Component {
    config: Rc[Config],
    name: string,
}

fn main() -> i32 {
    let config: Rc[Config] = Rc.new(Config {
        theme: "dark",
        language: "en",
    })

    let comp1: Component = Component {
        config: config.clone(),
        name: "header",
    }

    let comp2: Component = Component {
        config: config.clone(),
        name: "footer",
    }

    // Both components share the same config
    println(comp1.config.get().theme)  // "dark"
    println(comp2.config.get().theme)  // "dark"

    return 0
}
```

## Arc[T] - Thread-Safe Shared Ownership

`Arc` is the atomic version of `Rc`, safe for sharing across threads.

### Creating Arc

```klar
let data: Arc[i32] = Arc.new(42)
```

### Thread-Safe Sharing

```klar
let shared: Arc[Counter] = Arc.new(Counter { value: 0 })

let thread1_data: Arc[Counter] = shared.clone()
let thread2_data: Arc[Counter] = shared.clone()

// Can be safely sent to different threads
```

### When to Use Arc vs Rc

| Scenario | Use |
|----------|-----|
| Single-threaded program | `Rc[T]` |
| Sharing within one thread | `Rc[T]` |
| Sharing across threads | `Arc[T]` |
| Performance critical, single-threaded | `Rc[T]` |

## Combining with Cell for Mutability

Since `Rc` and `Arc` provide shared ownership, the data they contain is immutable. Use `Cell[T]` for interior mutability.

### Rc[Cell[T]]

```klar
let counter: Rc[Cell[i32]] = Rc.new(Cell.new(0))

let ref1: Rc[Cell[i32]] = counter.clone()
let ref2: Rc[Cell[i32]] = counter.clone()

// Both can modify the shared value
ref1.get().set(10)
println(ref2.get().get())  // 10

ref2.get().set(20)
println(ref1.get().get())  // 20
```

### Example: Shared Mutable State

```klar
struct GameState {
    score: Cell[i32],
    level: Cell[i32],
}

struct Player {
    state: Rc[GameState],
    name: string,
}

impl Player {
    fn add_points(ref self: Player, points: i32) {
        let current: i32 = self.state.get().score.get()
        self.state.get().score.set(current + points)
    }
}

fn main() -> i32 {
    let state: Rc[GameState] = Rc.new(GameState {
        score: Cell.new(0),
        level: Cell.new(1),
    })

    let player1: Player = Player { state: state.clone(), name: "Alice" }
    let player2: Player = Player { state: state, name: "Bob" }

    player1.add_points(100)
    player2.add_points(50)

    // Both see the same score
    println(player1.state.get().score.get())  // 150
    println(player2.state.get().score.get())  // 150

    return 0
}
```

## Common Patterns

### Shared Cache

```klar
struct Cache {
    data: Rc[Map[string, string]],
}

impl Cache {
    fn new() -> Cache {
        return Cache { data: Rc.new(Map.new[string, string]()) }
    }

    fn share(ref self: Cache) -> Cache {
        return Cache { data: self.data.clone() }
    }
}
```

### Tree with Shared Subtrees

```klar
struct TreeNode {
    value: i32,
    children: List[Rc[TreeNode]],
}

fn share_subtree() {
    let shared: Rc[TreeNode] = Rc.new(TreeNode {
        value: 100,
        children: List.new[Rc[TreeNode]](),
    })

    var root1_children: List[Rc[TreeNode]] = List.new[Rc[TreeNode]]()
    root1_children.push(shared.clone())

    var root2_children: List[Rc[TreeNode]] = List.new[Rc[TreeNode]]()
    root2_children.push(shared.clone())

    let root1: TreeNode = TreeNode { value: 1, children: root1_children }
    let root2: TreeNode = TreeNode { value: 2, children: root2_children }

    // Both trees share the same subtree
}
```

### Observer Pattern

```klar
struct Observable {
    observers: List[Rc[Observer]],
}

impl Observable {
    fn subscribe(inout self: Observable, observer: Rc[Observer]) {
        self.observers.push(observer)
    }

    fn notify(ref self: Observable, event: string) {
        for obs: Rc[Observer] in self.observers {
            obs.get().on_event(event)
        }
    }
}
```

## Avoiding Reference Cycles

Reference cycles cause memory leaks because the count never reaches zero.

### Problem: Cycle

```klar
// BAD: Creates a cycle
struct Node {
    value: i32,
    next: ?Rc[Node],
    prev: ?Rc[Node],  // Cycle!
}
```

### Solutions

1. **Weak References** (when available)
2. **Break cycles manually**
3. **Restructure to avoid cycles**

```klar
// Option: Use indices instead of references
struct NodeList {
    nodes: List[Node],
}

struct Node {
    value: i32,
    next_index: ?i32,
    prev_index: ?i32,
}
```

## Performance Considerations

### Overhead

- Each `clone()` and `drop` updates the reference count
- Small overhead per operation
- `Arc` is slower than `Rc` due to atomic operations

### When to Avoid

- Hot loops where cloning is frequent
- Performance-critical paths
- When simple ownership suffices

```klar
// Consider: Do you really need shared ownership?

// Maybe ownership transfer is enough:
fn process(data: Data) { ... }

// Or borrowing:
fn process(ref data: Data) { ... }
```

## Best Practices

### Minimize Cloning

```klar
// Good - clone only when needed
let shared: Rc[Data] = Rc.new(data)
let reference: Rc[Data] = shared.clone()  // Only when sharing

// Bad - unnecessary clones
for i: i32 in 0..100 {
    process(shared.clone())  // Do you need a new owner each time?
}

// Better - borrow if possible
for i: i32 in 0..100 {
    process(ref shared.get())  // Borrow instead
}
```

### Document Shared Ownership

```klar
/// Configuration shared across all components.
/// Modifications visible to all holders.
struct AppConfig {
    config: Rc[Cell[ConfigData]],
}
```

## Next Steps

- [Smart Pointers](../types/smart-pointers.md) - Full Rc, Arc, Cell reference
- [Ownership](ownership.md) - Basic ownership model
- [References](references.md) - Borrowing without ownership
