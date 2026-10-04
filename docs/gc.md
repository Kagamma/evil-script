# Garbage Collector

Evil Script uses a **concurrent, incremental, tri-color mark-and-sweep garbage collector with two generations**.

The collector is designed for games and interactive applications. The main goal is to keep stop-the-world pauses short by performing the expensive object graph traversal concurrently with the VM.

A single-threaded fallback is also available for environments where threading is disabled.

## Overview

The collector has two generations:

```text
Young
Old
```

New objects are always allocated into the young generation. Objects that survive enough young collections are promoted to the old generation. The default promotion threshold is **2 collections**. A normal collection is therefore a **minor collection** of the young generation, while every `FOldObjectCheckCycle` collections a **full collection** is performed over both generations. The default full-collection interval is 10 collections.

The collection phases are:

```text
Initial → Mark → Mark Remaining → Sweep
```

`Initial` and `Sweep` are performed with VM threads suspended. `Mark` can run concurrently on a dedicated helper thread. If the mutator creates references to objects that were not yet marked, the write barrier records them for `Mark Remaining`.

---

## Tri-color marking

GC objects use the usual tri-color abstraction:

```text
White   Not yet found reachable.
Gray    Reachable, but not completely scanned.
Black   Reachable and completely scanned.
```

The normal transition is:

```text
White → Gray → Black
```

The gray queue is represented by `FGrayValueQueue`.

Newly allocated objects are initially black. This makes objects created by the mutator during an active collection immediately live from the collector's point of view.

Pinned objects are also protected from being reset to white; `ResetColor` makes a locked object gray instead.

---

## Generations

### Young generation

New objects are appended to the young-generation list.

Each time a young object survives a minor collection, its `Visit` counter is incremented. Once it reaches `Promotion`, the object is removed from the young list and appended to the old-generation list.

```text
new object
    │
    ▼
  Young
    │
    │ survives minor collections
    ▼
   Old
```

The current default is:

```text
Promotion = 2
```

### Minor collection

A normal collection only resets and sweeps the young generation.

Old objects are not reset to white and are therefore not traversed as part of the normal full-heap marking process.

This is what makes the collector genuinely generational rather than merely using generations to organize its object lists.

---

## Remembered set

Because minor collections do not scan the entire old generation, references from old objects to young objects must be tracked.

Evil Script uses a remembered set for this:

```text
Old object ─────→ Young object
      │
      └── remembered set
```

When an old object receives a reference to a young GC object, `WriteBarrier` adds the old object's GC node index to `FRememberedNodeList`.

Each object also contains a `Remembered` flag so that the same old object is not repeatedly inserted into the remembered set.

During a minor collection, remembered objects are reset to gray and therefore become additional marking roots.

This allows the collector to trace:

```text
VM roots
   +
remembered old objects
   │
   ▼
young generation
```

without scanning every old object.

After sweeping, the remembered set is rebuilt by checking whether each remembered object still contains a reference to a young object. Entries that no longer contain young references are removed and their `Remembered` flag is cleared.

---

## Full collection

Every `FOldObjectCheckCycle` collections, the collector performs a full collection.

The default is:

```text
FOldObjectCheckCycle = 10
```

During a full collection, both the young and old generations are reset to white and participate in marking.

The sweep phase also sweeps both generations:

```text
minor collection:
    Sweep(Young)

full collection:
    Sweep(Young + Old)
```

This periodically discovers garbage that survived previous minor collections or is otherwise no longer reachable from the root set.

Thus the normal pattern is approximately:

```text
Minor
Minor
Minor
...
Minor
Full
```

rather than scanning the entire heap on every collection.

---

## Concurrent marking

When threading is enabled, the marking phase can be executed by `TSEGarbageCollectorMarkJob`.

The VM is suspended briefly to establish the initial root set. The collector then resumes the VM and lets the marking thread traverse the reachable graph concurrently.

The implementation intentionally does not hold the global GC lock while performing the expensive part of marking. `Mark()` only acquires the global lock around the short operations that access shared GC-node state; container contents are then scanned outside the global lock.

Individual maps have their own locks while their contents are being inspected.

This avoids turning concurrent marking into a globally serialized operation.

```text
             GC thread
                 │
                 ▼
        ┌─────────────────┐
        │ acquire GC lock │
        │ get node state  │
        └────────┬────────┘
                 │
            release lock
                 │
                 ▼
          scan object/map
                 │
                 ▼
        lock container only
        when its contents
        need protection
```

`EnableParallel` is somewhat historically named: it enables this **single-helper-thread concurrent marking**, not multiple GC workers performing parallel stop-the-world marking.

---

## Write barrier

The write barrier has two responsibilities.

### Generational barrier

When an old object receives a young object:

```text
Old ───→ Young
```

the old object is added to `FRememberedNodeList`.

This keeps minor collections correct without scanning the old generation.

### Concurrent marking barrier

During `segcpMark`, if a black object receives a reference to a white object:

```text
Black A ───→ White B
```

the barrier changes `B` to gray and adds it to `FRemainingGrayValueList`.

```text
Black A ───→ Gray B
```

This preserves the tri-color invariant while the VM modifies the object graph concurrently with the marker.

The two responsibilities are independent:

```text
write barrier
    │
    ├── Old → Young
    │      └── remembered set
    │
    └── Black → White during marking
           └── remaining gray list
```

---

## Incremental marking

Large array maps are scanned incrementally.

`IncrementalScanLimit` controls how many array elements are processed during one visit. The default is:

```text
IncrementalScanLimit = 4096
```

The current position is stored in `TSEValueMark.CurrentIndex`. If the array is not finished, the same object is placed back into the gray queue and scanning continues later.

For example:

```text
Mark(array, 0)
Mark(array, 4096)
Mark(array, 8192)
...
```

This prevents a very large array from turning into one long uninterrupted marking operation.

Shape-based maps are handled differently because maps with thousands of named properties are considered uncommon. Their properties are scanned as a normal map traversal rather than using the incremental array path.

---

## GC roots

The initial root scan examines the VM stacks, globals, constants, and `ScriptVarMap`.

Each reachable GC value is added to `FReachableValueList` and its corresponding object is turned gray. The marker then follows references from those objects.

Conceptually:

```text
VM stacks
VM globals
constants
ScriptVarMap
    │
    ▼
  GC roots
    │
    ▼
   Gray
    │
    ▼
   Black
```

Remembered old objects provide additional roots during minor collections.

---

## Mark Remaining

Concurrent marking cannot simply stop at the moment the marking worker becomes idle because the VM may have modified the object graph while marking was running.

Objects captured by the concurrent write barrier are stored in:

```text
FRemainingGrayValueList
```

The VM is suspended and these values are marked before sweeping begins.

The complete concurrent cycle is therefore:

```text
Initial
   │
   ▼
Mark roots
   │
   ▼
Concurrent Mark
   │
   ├── mutator changes graph
   │       │
   │       └── write barrier
   │               │
   │               ▼
   │        Remaining Gray List
   │
   ▼
Mark Remaining
   │
   ▼
Sweep
```

---

## Sweep

After marking has completed, white objects in the collection's target generation are unreachable.

The sweep phase destroys them and returns their GC-node slots to `FNodeAvailStack`.

Objects currently collected include:

* maps;
* strings;
* buffers;
* managed Pascal objects.

For managed Pascal objects, the wrapped `TObject` is also freed.

Minor collection:

```text
Sweep(1)
```

Full collection:

```text
Sweep(2)
```

The generation-specific linked lists allow sweep to operate directly on the selected generation rather than walking the entire node list.

---

## Shapes

Shapes are metadata used by shape-based maps and have their own collector in `TSEShapeManager`.

They are deliberately not part of the normal object-generation system.

Shape collection uses a simple mark-and-sweep process:

```text
BeginMark
    ↓
Mark reachable shapes
    ↓
Sweep unreachable shapes
```

The normal GC marks shapes when running in single-threaded mode. Concurrent marking currently does not mark shapes through the same path. If shape creation exceeds its configured ceiling, the collector temporarily disables concurrent marking and performs the shape marking/sweep synchronously.

This is acceptable because shapes are metadata and are normally much fewer than ordinary runtime objects.

---

## Allocation

GC-managed objects are registered in `TSEGCNodeList`.

Each node contains:

```text
Value
Prev
Next
```

The collector maintains separate linked-list tails for young and old objects:

```text
FNodeLastYoung
FNodeLastOld
```

When an object is collected, its node index is pushed into `FNodeAvailStack`. Future allocations reuse these slots instead of continuously growing the node list.

---

## Collection scheduling

Automatic collection is primarily time and heap-growth driven.

`CheckForGCFast` checks the configured interval, while `ObjectThreshold` prevents collections when the heap has not grown sufficiently since the previous collection.

The current defaults are:

```text
Interval              = 1000 ms
ObjectThreshold       = 700
Promotion             = 2
OldObjectCheckCycle   = 10
IncrementalScanLimit  = 4096
```

These are configuration parameters rather than requirements of the collector design.

---

## Design

The collector can therefore be summarized as:

```text
                 Evil Script GC
                       │
          ┌────────────┼────────────┐
          │            │            │
          ▼            ▼            ▼
      Tri-color    Generational  Concurrent
      mark/sweep       GC           Mark
          │            │            │
          │       ┌────┴────┐       │
          │       │         │       │
          │     Young      Old      │
          │       │         │       │
          │       └────┬────┘       │
          │            │            │
          │      Remembered Set     │
          │            │            │
          └────────────┼────────────┘
                       │
                 Write Barrier
                       │
              ┌────────┴────────┐
              │                 │
          Old → Young       Black → White
          remembered set    gray queue
```

The main design trade-off is now a conventional generational one: **minor collections avoid scanning the old generation by maintaining a remembered set**, while full collections periodically reset and scan the entire heap. Concurrent and incremental marking then reduce the frame-time cost of the marking work itself.

In short:

> **Evil Script uses a two-generation, concurrent, incremental, tri-color mark-and-sweep collector. Young objects are collected frequently, survivors are promoted to the old generation, and an old-to-young remembered set makes minor collections possible without scanning the old generation. Concurrent marking and incremental array scanning keep the remaining GC work from becoming a large frame-time pause.**
