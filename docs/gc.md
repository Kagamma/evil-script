# Garbage Collector

Evil Script uses a **concurrent, incremental, tri-color mark-and-sweep garbage collector** with a lightweight generational optimization.

The collector is designed primarily for games and interactive applications, where avoiding long stop-the-world pauses is more important than minimizing the total CPU time spent on garbage collection.

## Overview

The collector consists of the following phases:

```text
Initial -> Mark -> Mark Remaining -> Sweep
```

`Initial`, `Mark Remaining`, and `Sweep` contain stop-the-world portions, while the main marking phase runs concurrently with the VM.

The expensive part of collection - the traversal of the reachable object graph - is therefore performed concurrently whenever possible.

The collector maintains two generations:

```text
Young
Old
```

New objects start in the young generation. Objects surviving enough collections are promoted to the old generation.

Unlike a traditional generational collector, Evil Script does **not** maintain a remembered set for old->young references. Old objects are still traversed during marking. The generational system therefore primarily reduces the amount of work performed during the stop-the-world phases rather than eliminating old-generation scanning completely.

---

## Tri-color marking

Each GC object is conceptually in one of three states:

```text
White   Object has not been found reachable.
Gray    Object is reachable but has not been completely scanned.
Black   Object and its references have been scanned.
```

Objects progress through:

```text
White -> Gray -> Black
```

The gray objects form the marking work queue.

GC roots are made gray during the initial phase. The concurrent marking thread then repeatedly processes gray objects and scans their references.

Newly allocated objects are initially black. This avoids requiring objects created during an active collection to go through the normal marking process.

---

## Concurrent marking

The marking phase runs concurrently with the VM on a dedicated GC thread.

An important part of the implementation is that **fine-grained locking is used to avoid serializing the marking operation**.

The GC has shared data structures that require synchronization, such as the GC node list and marking state. However, the collector does **not** keep the global GC lock while traversing an object's contents.

Instead, the lock is held only long enough to perform the necessary bookkeeping, after which it is released before the potentially expensive object traversal begins.

Conceptually:

```text
Acquire GC lock
    │
    ├─ obtain/update GC node state
    └─ obtain information needed for scanning
Release GC lock
    │
    ├─ scan map/array/object
    ├─ inspect referenced values
    └─ mark referenced objects
    │
    ▼
Acquire GC lock when shared GC state must be modified
```

---

## Write barrier

Because the VM continues modifying objects while marking is running, a write barrier is required.

When a black object receives a reference to a white object, the referenced object is turned gray and added to the remaining-gray list.

Conceptually:

```text
Black A ───> White B

        write barrier

Black A ───> Gray B
```

The barrier is used for references stored in GC-managed objects as well as relevant VM values such as locals, globals, and stack values.

At the end of concurrent marking, the VM is stopped and the remaining gray values are processed. This closes the marking phase while preserving the tri-color invariant.

---

## Incremental marking

Large array maps are scanned incrementally.

`Mark()` processes at most 8192 array entries at a time. If the complete array has not yet been scanned, the object remains gray and is returned to the marking queue with its current scan position.

Thus a large array does not become one long indivisible GC operation:

```text
Mark(array, 0)
Mark(array, 8192)
Mark(array, 16384)
...
```

This is particularly useful for games, where a single large container should not create an unexpectedly long GC operation.

Shape-based maps are normally much smaller and are not currently split into the same incremental chunks.

---

## Generational collection

Objects are initially placed in the young generation.

Objects which survive a configurable number of collections are promoted to the old generation. The default promotion threshold is 10 collections.

Normal collections primarily operate on the young generation during the stop-the-world phases. The old generation is periodically included in collection, rather than being swept on every cycle.

The important difference from a conventional generational collector is that there is **no remembered set**:

```text
Old ───> Young
```

is not recorded in a remembered-set structure.

Instead, reachable old objects are still scanned by the concurrent marker.

This deliberately trades additional concurrent marking work for a simpler implementation and simpler write barriers.

The generational system therefore mainly reduces:

* repeated old-generation sweeping;
* repeated stop-the-world initialization work;
* the amount of heap bookkeeping performed for short-lived objects.

It does not attempt to make young-generation collection completely independent from the old generation.

---

## Sweep

After marking has completed, white objects are unreachable and can be destroyed.

The sweep phase runs with the VM stopped.

Normally the young generation is swept. The old generation is swept periodically according to the old-generation collection cycle.

Collected GC nodes are returned to the node allocator's free-node stack so their bookkeeping slots can be reused by later allocations.

The collector also supports locking/pinning of objects. Locked objects are kept alive during collection and are not treated as ordinary white objects during the generation reset.

---

## GC roots

The collector obtains roots from the VM, including values held by:

* VM stacks;
* global variables;
* constants;
* `ScriptVarMap`;
* other VM-managed root structures.

From these roots the marker follows references through the GC object graph.

---

## Shapes

Shapes are metadata describing the layout of shape-based maps. They are managed by a separate, simpler collector in `TSEShapeManager`.

Shapes are not ordinary script objects and are normally present in relatively small numbers, so using the full concurrent GC machinery for them would add complexity for little benefit.

Shape collection is a simple mark-and-sweep process over the shape graph.

Unreachable shapes and their transitions are removed when shape collection is triggered.

This separation also reflects their different lifetime characteristics:

```text
Script objects
    -> many
    -> frequently allocated
    -> frequently collected

Shapes
    -> comparatively few
    -> metadata
    -> usually long-lived
```

---

## Design trade-offs

The collector intentionally favors **low pause times and implementation simplicity** over minimum total GC work.

```text
                ┌────────────────────┐
                │ Concurrent marking │
                │ scans old objects  │
                └─────────┬──────────┘
                          │
              reduces frame-time cost
                          │
                          ▼
          ┌────────────────────────────┐
          │ Lightweight generations    │
          │ reduce STW work and sweep  │
          └────────────────────────────┘
```

The resulting collector is therefore best described as:

> **A concurrent, incremental tri-color mark-and-sweep collector with a lightweight generational optimization, designed to keep stop-the-world pauses short rather than minimize total heap traversal work.**
