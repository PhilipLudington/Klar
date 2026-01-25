# Collections

Klar provides dynamic collection types: `List[T]`, `Map[K, V]`, `Set[T]`, and `Range[T]`.

## List[T]

A dynamic, growable array.

### Creating Lists

```klar
var list: List[i32] = List.new[i32]()
```

### Adding Elements

```klar
var list: List[i32] = List.new[i32]()
list.push(10)
list.push(20)
list.push(30)
// list: [10, 20, 30]
```

### Accessing Elements

```klar
let first: i32 = list[0]     // 10
let second: i32 = list[1]    // 20
```

### List Methods

| Method | Description |
|--------|-------------|
| `push(value)` | Add element to end |
| `pop() -> T` | Remove and return last element |
| `len() -> i32` | Number of elements |
| `is_empty() -> bool` | Check if empty |
| `clear()` | Remove all elements |
| `get(index) -> ?T` | Safe access by index |
| `drop()` | Free the list's memory |

### Iterating Lists

```klar
var numbers: List[i32] = List.new[i32]()
numbers.push(1)
numbers.push(2)
numbers.push(3)

for n: i32 in numbers {
    println("{n}")
}

numbers.drop()  // Clean up
```

### Example: Building a List

```klar
fn range_list(start: i32, end: i32) -> List[i32] {
    var result: List[i32] = List.new[i32]()
    for i: i32 in start..end {
        result.push(i)
    }
    return result
}
```

## Map[K, V]

A hash map storing key-value pairs.

### Creating Maps

```klar
var scores: Map[string, i32] = Map.new[string, i32]()
```

### Adding and Updating

```klar
var map: Map[string, i32] = Map.new[string, i32]()
map.insert("alice", 100)
map.insert("bob", 85)
map.insert("alice", 95)  // Updates existing key
```

### Accessing Values

```klar
let score: i32 = map.get("alice")  // 95

// Check if key exists
if map.contains_key("bob") {
    println("Bob's score: {map.get(\"bob\")}")
}
```

### Map Methods

| Method | Description |
|--------|-------------|
| `insert(key, value)` | Add or update key-value pair |
| `get(key) -> V` | Get value for key |
| `contains_key(key) -> bool` | Check if key exists |
| `remove(key)` | Remove key-value pair |
| `len() -> i32` | Number of entries |
| `is_empty() -> bool` | Check if empty |
| `clear()` | Remove all entries |
| `keys() -> List[K]` | Get all keys |
| `values() -> List[V]` | Get all values |
| `drop()` | Free the map's memory |

### Iterating Maps

Maps iterate as `(key, value)` tuples:

```klar
var scores: Map[string, i32] = Map.new[string, i32]()
scores.insert("alice", 100)
scores.insert("bob", 85)

for (name, score) in scores {
    println("{name}: {score}")
}

scores.drop()
```

### Example: Word Counter

```klar
fn count_words(words: List[string]) -> Map[string, i32] {
    var counts: Map[string, i32] = Map.new[string, i32]()

    for word: string in words {
        if counts.contains_key(word) {
            let current: i32 = counts.get(word)
            counts.insert(word, current + 1)
        } else {
            counts.insert(word, 1)
        }
    }

    return counts
}
```

## Set[T]

A collection of unique values.

### Creating Sets

```klar
var set: Set[i32] = Set.new[i32]()
```

### Adding Elements

```klar
var set: Set[i32] = Set.new[i32]()
set.insert(10)
set.insert(20)
set.insert(10)  // Duplicate - no effect
// set: {10, 20}
```

### Set Methods

| Method | Description |
|--------|-------------|
| `insert(value)` | Add element (no-op if exists) |
| `contains(value) -> bool` | Check if element exists |
| `remove(value)` | Remove element |
| `len() -> i32` | Number of elements |
| `is_empty() -> bool` | Check if empty |
| `clear()` | Remove all elements |
| `drop()` | Free the set's memory |

### Iterating Sets

```klar
var numbers: Set[i32] = Set.new[i32]()
numbers.insert(3)
numbers.insert(1)
numbers.insert(4)

for n: i32 in numbers {
    println("{n}")
}
// Note: Order is not guaranteed

numbers.drop()
```

### Example: Finding Unique Values

```klar
fn unique(list: List[i32]) -> List[i32] {
    var seen: Set[i32] = Set.new[i32]()
    var result: List[i32] = List.new[i32]()

    for n: i32 in list {
        if not seen.contains(n) {
            seen.insert(n)
            result.push(n)
        }
    }

    seen.drop()
    return result
}
```

## Range[T]

A range of values, typically used for iteration.

### Creating Ranges

```klar
// Exclusive range: 0, 1, 2, 3, 4
let exclusive: Range[i32] = 0..5

// Inclusive range: 0, 1, 2, 3, 4, 5
let inclusive: Range[i32] = 0..=5
```

### Range Properties

```klar
let r: Range[i32] = 0..10

let start: i32 = r.start    // 0
let end: i32 = r.end        // 10
let is_inc: bool = r.inclusive  // false
```

### Iterating Ranges

```klar
// Most common: directly in for loop
for i: i32 in 0..5 {
    println("{i}")  // 0, 1, 2, 3, 4
}

// As a variable
var r: Range[i32] = 1..=3
for x: i32 in r {
    println("{x}")  // 1, 2, 3
}
```

### Range with Different Types

```klar
// Ranges work with any numeric type
for i: i64 in 0.as[i64]..100.as[i64] {
    // ...
}
```

## Memory Management

Collections allocate memory on the heap. Call `drop()` when done:

```klar
fn process() {
    var list: List[i32] = List.new[i32]()
    list.push(1)
    list.push(2)

    // Use the list...

    list.drop()  // Free memory
}
```

### Nested Collections

Drop inner collections before outer:

```klar
var matrix: List[List[i32]] = List.new[List[i32]]()

// Create rows
var row1: List[i32] = List.new[i32]()
row1.push(1)
row1.push(2)
matrix.push(row1)

// When cleaning up, drop rows first (they're moved into matrix)
// matrix.drop() will handle it
matrix.drop()
```

## Example: Graph Adjacency List

```klar
struct Graph {
    edges: Map[i32, List[i32]],
}

impl Graph {
    fn new() -> Graph {
        return Graph { edges: Map.new[i32, List[i32]]() }
    }

    fn add_edge(inout self: Graph, from: i32, to: i32) {
        if not self.edges.contains_key(from) {
            self.edges.insert(from, List.new[i32]())
        }
        var neighbors: List[i32] = self.edges.get(from)
        neighbors.push(to)
    }

    fn drop(inout self: Graph) {
        // Drop all neighbor lists, then the map
        for (_, neighbors) in self.edges {
            neighbors.drop()
        }
        self.edges.drop()
    }
}
```

## Next Steps

- [Smart Pointers](smart-pointers.md) - Rc, Arc for shared ownership
- [Memory](../memory/ownership.md) - Ownership model
- [Control Flow](../language/control-flow.md) - Iteration patterns
