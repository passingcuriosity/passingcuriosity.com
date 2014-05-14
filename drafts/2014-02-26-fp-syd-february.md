---
title: FP-Syd, February 2014
tags: event, fp-syd, meetup, functional programming, fusion, big integers
location: Sydney, New South Wales
excerpt: 
  The February 2014 meeting of the FP-Syd group.
---

# News and announcements

Next month is re-scheduled due to Yaron Minsky's visit in Sydney. He'll be
telling us about Jane Stree Capital and their use of OCaml.

Let's not have stupid, exclusionary problems like other communities do.

Welcome to the few new people.

If people want to talk but are a bit nervous, ask and people will provide
advice, etc.

Proposals for YOW! Lambda Jam talks are due before the 12th of March.

# Amos Robinson - Faster Fusion

> I want my programs to be fast! As fast as I can be bothered making them.

Writing low-level code in C is tedious and bug-prone, but writing high-level
code in languages like Haskell will likely be slow. Merging, e.g., three loops
is *fusion*; has restrictions: only loops of the same size can be fused, folds
can't be fused with their outputs (the fold processes the whole input).

> Can we fuse operations on filtered data with the original?

````{.haskell}
sum2 (as : Vector Int) =
  let filt = filter (>0) as
      s1   = fold  (+) 0 filt
	  s2   = fold  (+) 0 as
  in (s1, s2)
````

This would naïvely be implemented as three loops: filter, fold, and fold. But
clearly this can be fused into a single loop.

````{.haskell}
normalise2 (as : Vector Int) =
  let filt = filter (>0) as
      s1   = fold  (+) 0 filt
	  s2   = fold  (+) 0 as
	  s1'  = ... filt
	  s2'  = ... as
  in (s1', s2')
````

How to fuse *this* program? Integer linear programming (using an external
library): convert a Haskell (-ish) program into an integer linear program
(using a data dependency graph), then solve the program to find fusable "bits"
of the Haskell (ish) program.

````{dot}
digraph "eg" {
	as -> filter;
	as -> fold;
	filter -> fold2;
	fold -> out [slash];
	fold2 -> out [slash];

	as [shape=rect];
}
````

Minimise generated programme which use these:

`C(i,j)` - whether the pair of nodes is or is not reduced (0, 1).

`W(i,j)` - benefit of fusing the nodes (100 iff j mentions i, 1 otherwise).

Add constraints for folds (they can't fuse: `C(i,j) = 1`), etc.

Add constraints to prove the clustering is acyclic. `C(i,j) = 0` then
`pi(i) = pi(j)`; `C(i,j) = 1` then `pi(i) < pi(j)`(?)

Feed this programme to the solver and it'll give back an assignment of the
variables which will indicate the clusters to be fused. This information can
be fed into Ben's fusion work (described in his Haskell Symposium paper).

# Erik on big integers

Lots of free time around xmas, pick a new project. `Integer` is Haskell's
arbitrary size integer arithmetic; it's a mainstay of FP examples like `fib`,
crypto code, the compiler (dealing with constants, compile-time eval, etc.)
GHC comes with support for two: integer-fmp is fast but uses libgmp (LGPL),
integer-simple is pure (GHC) Haskell but very, very slow. So a new library!

Tools:

- Hspec for testing and validating (using QuickCheck for property-based
  testing).

- Criterion for benchmarking.

- Standard development tools.

## Env

First of all: a way to compare simple and GMP. Can use criterion to run two
programs and merge the results, but instead hacked Simple so it can be included
in the same program as GMP.

## Special requirement

The Integer library is compiled into GHC itself, this means it can't use much
at all in the way of dependancies (i.e. `base`).

## Operations

Needs to support a wide range of operations:

add, sub, mult, div (quotRem, divMod, quot, rem)

Conversion functions (from/to Int, Word, Double, etc.)

Comparison (all of Ord)

Bitwise operators (and, or, xor, not, shiftL, shiftR, etc.)

And more.

## Repr

Simple uses a linked list of `Word#`. 

GMP has a case for `Int` and another for "bit" values: (Int, ByteArray) -- the
length and array of bytes.

Decided to follow GMP's example: have a special case for "fits in a machine
word".

```
= SmallPos {-# UNPACK #-} !Word
= SmallPos {-# UNPACK #-} !Word
| Positive !Nat
| Negative !Nat

Nat = Nat
{-# UNPACK #-} !Int
{-# UNPACK #-} !WordArray
```

Word and WordArray are used in Text and ByteString, come from data primitive
library.

Basically: arithmetic at base $2^64$!

Use primitive operations:

```
plusWord2# :: Word# -> Word# -> (# Word#, Word# #)
timesWord2# :: Word# -> Word# -> (# Word#, Word# #)
quotRemWord2# :: Word# -> Word# -> Word# -> (# Word#, Word# #)
```

## Make it fast

Arrays of unboxed values is a start.

Lazy evaluation is the enemy.

Add bang patterns to everything.

Hope that GHC keeps boxing and unboxing a minimum. Will need to check this
later.

Write really low-level code (loops, highly imperative).


## StrictPrim

merge Carter's StrictIdentity monad with ST into StrictPrim. Use it to do
mutable array of words.

# Mark - Lens from the ground up

> I'm going to assume that people know Haskell. And I'm going to assume that
> people know nothing about lens.

1. Motivations - why it is cute?

2. Demos

## What?

A lens is two functions, a get and a set.

```{haskell}
data Lens a b = Lens {
get :: a -> b
set :: a -> b -> a
}
```

Pierce's laws (bi-directional functional programming) "well-behaved lenses":

- get-set
- set-get
- set-set

Using records for them is horrible when they nest, etc.

Can't do polymorphic updates (set an Int into something that's currently got an
String).

Can't be read-only or write-only; must have set *and* get.

Composition matters (compose two lenses on same field: one name is set and will
win).

Partiality!

## Applied category theory

flip set:

```{haskell}
set :: b -> a -> a
```
?

But problems; didn't work.

## CPS-style approach

