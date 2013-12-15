---
title: FP-Syd, October 2013
tags: event, fp-syd, meetup, functional programming, icfp
location: Sydney, New South Wales
excerpt: 
  The October 2013 meeting of the FP-Syd functional programming group in Sydney
  heard talks about data flow fusion, a constrained domain specific language
  for building filesystems and a round-up of ICFP.
---

Little mention of [linux.conf.au 2014][lca2014] and how we should all take a
look at the available programme and see if we want to go.

[lca2014]: http://linux.conf.au/

# Erik's ICFP roundup

The [International Conference on Functional Programming][2] (ICFP for short) is
a three-day core conference and is collocated with a number of related events.
[ICFP 2013][3] was in Boston and a number of FP-Syd regulars presented and/or
attended.

[2]: http://www.icfpconference.org
[3]: http://www.icfpconference.org/icfp2013/

Erik was a long time LCA attendee but ICFP has supplanted it as his "must go"
conference. I hope to make the same switch in [2014][4]!

[4]: http://www.icfpconference.org/icfp2014/

The **Haskell Implementers Workshop** covers the internals of Haskell
implementations which, these days, means GHC to a very large extent. Covers a
lot of interesting techniques, with a particular focus on compilers. Erik
mentioned work on the non-safety of generalised `newtype` deriving; using
Hermit (a dynamic/guided optimisation framework) to optimise
scrap-your-boilerplate code; and Habit (a strict Haskell dialect for OS
programming).

The **Commercial Users of Functional Programming** was, reportedly, a bit
boring, but I've liked the few [CUFP 2013 YouTube videos][5] I've watched so
far. YMMV.

[5]: http://www.youtube.com/channel/UCfSUv7I_aHgzcnXMcd8obsw

The **Haskell Symposium** was a main draw (for Erik). Highlights which Erik
found worth mentioning and I found worth noting down include:

- Oleg asked difficult questions of a lot of speakers. I wonder what would
  happen if he asked an easy one?

- Effects seemed something of a hot topic.

- Demonstrations of [Liquid Haskell][6] (which sounds pretty great), and a
  Javascript backend for GHC.

[6]: http://hackage.haskell.org/package/liquidhaskell

- Intel are developing a research compiler which uses GHC's front-end to
  compile to Core and then uses their own backend. Does loop vectorisation,
  currently only better performance on a few benchmarks.

- The third iteration of the I/O manager for GHC. Multithreaded, influenced by
  Kazu Yamamoto's work on Warp and mighttpd. Benchmarks against Nginx seem very
  good; Warp with multiple cores sees extremely good speedups (contra Nginx).

The main event -- **ICFP** -- is an academic conference and a lot of the
content will fly straight over the head of many a "working programmer". Some of
the highlights included:

- A few talks on vectorisation (w/ SIMD from Intel, stream fusion, etc.) and
  optimisation (for GPUs, etc.)

- A few talks on dependent types.

- Tactics in Coq are untyped; one talk discussed an approach to typed tactic
  programming in Coq. Sounds especially interesting now that there is a "Coq
  fight" in the FP-Syd calendar for next year!

- People who didn't attend are encourage to watch the video of the "fun with
  semi-rings" talk. I haven't been able to find it, though.

- One talk described a useful-sounding approach to parsing context free
  grammars with a divide-and-conquer approach, allowing partial and parallel
  parsing.

- Simon Peyton-Jones discussed the new curriculum for secondary computer
  science education in the United Kingdom.

- An extension or two to System F: System Fc (explicitly kind equality) and
  System Fi (type indices). Everyone who can understand System F shouldn't have
  a problem reading the System Fi paper.)

- Constrained monad problem (which, apparently, Oleg said was crap?). Paper on
  solving a problem which occurs when using `Monad` but they should have used
  `Applicative`. Seems as though they mostly wanted the `do` syntactic sugar;
  see also idiom brackets and the attempt to generalise the Monad sugar.

- "Querying ordered graphs." Three words which sound interesting, but I've no
  idea why I wrote them down.

- Also: experience reports! Someone took a Scheme compiler from 4-5 to 25
  passes ("nanoparsing"?) and, at the same time, also added a good colouring
  register allocator. Apparently one of these changes made it better.

Other events:

- Talking about a benchmark/framework to compare approaches to generic
  programming at the Workshop on Generic Programming.

- Brent Yorgey doing animations with `diagrams` at the Workshop on Functional
  Art, Music, Modeling and Design.

- Chordify is a system (written in Haskell) to analyse recordings and generate
  chord transcripts. It's not perfect but gives pretty good approximations.

# Ben talking about Data Flow Fusion

[Ben Lippmeier][7] -- an FP-Syd regular -- presented a paper at ICFP and
reprised that presentation back in Sydney for those of use who weren't in
Boston. He described an approach using data flow to guide the compilation of
programs using stream fusion.

[7]: http://www.cse.unsw.edu.au/~benl/

Wants to process a list of points, adding 1 to each, filtering those about 0
and also finding the maximum.

Doing stream fusion

````{.haskell}
map f = unstream . mapsS f . stream
filter f = unstream . filterS f . stream

-- RULE to remove (stream . unstream)
````

Example computes `(vec3, n)` can't float `vec3` because it's being used in the
result *and* in the computation of `n`. So we get two loops.

    **1** -> 2 -> **3** -> 4
    	              |      |
                  (    ,    )

`zipWithX` tends to use X+1 loop counters for stream fusion. There're only 8
registeres to use on some platforms.

## Data Flow Fusion

### Slight manual refactor

Split `filter` into two combinators `flag` -- which contains `True` or `False`
for each member -- and `pack` -- which does the filtering.

### Extract the data flow graph

This code generates the data flow graph.

````
fun vec1 (\s1 -> 
  let s2    = map (+ 1) s1
      flags = map (> 0) s2
  in mkSel flags (\sel ->
  let s3   = pack sel s2
      vec3 = create s3
      n    = fold max 0 s3
  in (vec3, n)))

vec1 :: Vector Int
s1 :: Series k1 Int
s2 :: Series k1 Int
flags :: Sel k1 k2
s3 :: Series k2 Int
````

Series has a phantom type variable which helps keep track of the code which can
be fused into a single loop.

We learn that `k1 >= k2`

With the flow graph (annotated with operations, etc.), throw away the source.

### Schedule the grapch into an abstract loop nest

Abstract loop nest:

````
loop k1 {

  start: ....
  
  body: ....
  
  inner: ...
  
  end: ...

} yields ...
````

Start at the front of the data flow graph and add elements of the graph to the
nested abstract loop.

Operations go into different places in the nested abstract loop. A `fold`, for
example, allocates and accumulator in `start`, increments somewhere within
`body` and reads it in `end`.

### Extract implementation from abstract loop nest.

Translate the various bits and pieces of the abstract loop nest data structure
into different Haskell combinators.

## Implementation

GHC plugin which grabs Core, does data flow compilation and generates Core to
give back to GHC.

Some issues in current implementation where LLVM doesn't realise that writing
to the output doesn't *need* to reload the start and length numbers.

> **If** your program is first order (argument functions take scalars,
> not series), non-recursive, synchronous, finite data flow program
> using out combinators.
>
> **Then** by construction your program will be compiled correctly by
> this system.

# Liam on CDSL

Liam O'Connor works for NICTA. Instead of talking about something he recently
learned, he's talking about work: CDSL - a restricted functional language for
file system verification.

Trying to establish a formal proof of the correctness of a file system driver
in an operating system.

Already have an architecture for this sort of problem (from seL4):

1. Abstract spec - high-level, nondeterministic (followed by an "interesting"
proof of relation to ~ 15%)

2. Low level spec - purely functional (followed by a "largely boring" proof of
relation to ~ 30%)

3. C implementation - efficient.

~ 55% is showing that the other proofs don't do something stupid; proving
invariants all hold.

Ignoring the kernel proper, architecture support, and drivers (another NICTA
project), the largest part of the Linux kernel is the `fs/` directory; 31
different file systems were supported by the kernel running on some random
NICTA server.

There are lots of file systems with, one assumes, quite a lot of common
functionality and infrastructure. The goal of the project is not to make a
cathedral of a single verified file system, more a factory for churning out
numerous file systems. The approach is to use a DSL to generate the low-level
spec, proof and implementation. High-level spec and proof are done by hand, so
generated outputs need to be readable.

Should

- establish key verification properties

- compete with efficient C code (imperative, destructive updates, etc.)

- be expressive enough to write a file system

But:

- doesn't need to express *everything* in a file system. Hand-written components
  could be plugged in to the DSL (and, hopefully, re-used).

## Simply-typed lambda calculus

Simple-typed lambda calculus is strongly normalising (you can't write general
recursion, e.g. the Y combinator).

First-order language: lambdas go away, use `let` binding and restrict to
defining top-level functions. Added structural rules for mixing, weakening, ?

Need to do memory management which is safe, expressive (no pass by value, we
need the heap), no GC (you'd have to verify it, introduce latency, etc.)

Automatic member management (GC) is too big a burden. Many static automatic
memory management is inefficient or unsafe.

What about manual memory management?

````{.haskell}
let x = allocateData ()
    x' = updateData x
    _ = free x
in x'
````

But this is terrible! Unsafe, inefficient, etc.

So have a linear type system, throwing away weakening, etc. Forces use of
things exactly matching (can't alloc and not use, doesn't discharge the new
fact). The typing rules require that introduction and elemination be paired.

Linear types means that the elimination operations (e.g. `updateDate`) are the
*last* to access terms, so they can do destructive updates.

Two interpresations of these semantics:

- value semantics: pass by value, no heap, immutability, reasoning.

- update semantics: heap, updates, deallocates, implementation.

Linear types allow for both.

But sometimes you want non-linear, pass-by-value (arithmetic operations, etc.):

- Unboxed types, ints, small structs
- Functions themselves

Allow structural rules (dereliction and contraction) for certain types only. So
now we have `T_{.}` and `T_{#}` (unboxed and value types).


## Buffer interface

````
make : () -> .Buf
free : .Buf -> ()
length : .Buf -> (#U32, .Buf)

serialise : (.Obj, .Buf) -> (.Obj, .Buf)
deserialise : .Buf -> (.Obj, .Buf)
````

Non-linear "look but don't touch" references with `*`:

````
make : () -> .Buf
free : .Buf -> ()

length : *Buf -> #U32
serialise : (*Obj, .Buf) -> .Buf
deserialise : *Buf -> .Obj
````

Use `let!` construct which is like `let` but we mark specific variables as
read-only within the `let` clauses and back to linear in the `in`.

But this is unsafe (read-only can escape the let). Could use regions, but
choose not to unless it's required.

Linear typing breaks some control flow:

````{.haskell}
let x = alloc ()
in if cond
   then update(x)
   else x
````

## Loops

Hardest, most annoying part of the formalisation of the language.

Built-in loop combinators, map, fold, with, for.

````
let sum = for (x,y) in fold(arr) with 0
              do (x + y)

let arr', sum = for (x,y) in map(arr) with 0
                    do (x * 2, x + y)
````

Alas, this is unsafe. Double free, etc. But you can restrict linear types in
the loop expression. Then have to make any required linear types into
accumulator parms.

## Error handling

The return-code convention using in languages like C is pretty bad. Instead,
separate statements and expressions.

Statements have three types:

- s : {\bar T_{s}}
- s : fails {\bar T_{f}}
- s : {\bar T_{?}} fails {\bar T_{?}}

Type of `if then else` is `T_{t} \leastupperbound T_{e}`. Lattice join,
subtype, etc.

Make `let` and `let!` only handle success cases. Force sub-expressions to
handle potential errors. Type system *forces* you to handle your errors and the
*linear* type system forces you to free your resources.

## Types

Product and sum types (implemented as structs and tagged unions).

Accessing members of linear records is problematic as you use the record
multiple times:

````
let sum = operation(x.field1, x.field2)
````

Instead use an open/close structure.
