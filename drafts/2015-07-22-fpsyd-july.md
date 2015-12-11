---
title: FP-Syd July
tags: events, fp-syd, purescript, caching, compilers
location: Sydney, New South Wales
---

# Tim Docker on Purescript

Build a new language like Haskell but for the browser ecosystem.

- Clean, consise syntax
- Purity
- Types (repr effects, etc.)
- Std libraries informed by reality

Maybe not:

- laziness (complicated runtime system, etc.)
- all the historical baggage (library design, etc.)

Purescript is a small, simple, pure functional programming language
which compiles down to JavaScript. The generated JavaScript is not too
dissimilar (in many cases) to hand written code. Purescript modules
are compiled to normal JavaScript "modules", which can be used without
any additional infrastructure in standard JavaScript programs.

Functions in the Purescript standard library tend toward totality
(`head`, etc. are `Maybe`).

A good, working record system. Field overloading, structural typing,
works with the underlying JavaScript object representation. Slightly
different syntax (access with `.`; composition uses `>>>`
instead). Record types are real types themselves, row polymorphism:

````
area :: forall l. { width :: Number, height :: Number | l } -> Number
````

Uses row typing for effects too. `Eff` is morally equivalent to the
`IO` monad but it carries a row type of the effects being performed.

````{.purescript}
print :: forall a eff . (Show a) =>
  a -> Eff (console :: CONSOLE | eff) Unit

readTextFile :: forall eff .
  Encoding -> FilePath -> Eff (fs::FS, err::EXCEPTION | eff) String

printFile :: forall eff.
  Encoding -> String -> Eff (console::CONSOLE, fs::FS, err::EXCEPTION | eff) Unit
printFile e p = readTextFile e p >>= print
````

Handlers:

````
catchException :: forall a eff.
  (Error -> Eff eff a) -> Eff (err :: Exception | eff) a -> Eff eff a

runPure :: forall a. (forall e. Eff e a) -> a
````

Does the usual things in JavaScript land (bower, etc.)

Good documentation: [*Purescript by example* book][ps-by-eg], API
documentation, code search (pursuit?).

[ps-by-eg]: https://leanpub.com/purescript/read

FFI has a bunch of adaptors structures, passing conventions. Libraries
with integration to JS land (React bindings, FRP, etc.)

# Mark Hibberd - ARC

Caching. von Neumann, hierarchy of memories, slower. Orders of
magnitude.

Distributed systems, but in a non-traditional way. Reliable storage,
data locality; lazy replication of data to ephemeral compute nodes.

Simple example: HTTP server responding to requests: look in memory,
fallback to disk, fallback to remote.

LRU was the default cache choice for a long time; simple, constant
time operations; extremely bad in some operations (scans).

LFU has better average cases, logarithmic complexity, and more
resilient to some of the bad operations.

Hybrids which mostly failed (log complexity), suitable only for a very
few specifically tuned workloads. Created cottage industry of tuning
papers.

ARC combines frequency and recency (to give money to IBM); used in
FreeBSD, Solaris (VM, file systems, etc.), and was in PostgreSQL, but
they got scared and took it out again. Constant time and space
complexity, self tuning, and good for general purpose.

LRU keeps list of things recently seen. ARC keeps two lists:

- LRU-ish list to keep track of things seen recently; called L1.

- LFU-ish list to keep track of things seen frequently; called L2.

Stored back to back, with more recently, frequently in the middle.

Inside segments T1, T2 are things that are on disk; ouside segments
B1, B2 are things we know about but don't have.

````
   L1 | L2
B1 T1 | T2 B2
````

Gradually slide the `T1 \union T2` window depending on misses. LRU
with LRU on top.

Hierarchy:

- ARC - memory
- L2ARC - SSD
- Network

This architecture can make the L2ARC a bottleneck of the entire
system; need to make writes into L2ARC synchronous, dropping cached
objects on the ground. Repeated reads is better than slowing down the
whole system.

## Results

There are no correct LRU caches in Haskell (all logarithmic) so no benchmarks.

# Jacob - SSA vs ANF

Single static assignment form is functional programming (SSA is
Functional Programming, Appel 1998); invented by imperative compiler
writers. Arguments to fns must be atomic (sub-expressions named), each
variable assigned only once. Used in many optimising compilers. Proc,
label, var, etc.

ANF (Administrative normal form) is restricted lambda calculus;
arguments are also atomic, but no diff between label or proc (GHC,
DDC, SML/NJ, MLton).

Dead code elimination in SSA very complex (multiple passes, etc.)
compared to ANF.

Inter-procedural operations (e.g. inlining) quite difficult due to the
two diff jumps (call and goto). Need to introduce new labels, new
variables, fix up phi nodes, etc. ANF, in constract, just substitutes
the code with minor rewriting.

See jystic/ssa-anf

Generally after every pass on ANF you want to do copy propagation,
renaming. Then substitution becomes fairly trivial anyway.
