---
title: Sequence edit distance
strapline: Edit distance generalised to other data types.
tags: haskell, functional programming, algorithms, abstraction
location: Sydney, New South Wales
excerpt: 
  I've been working on a project that needs to compute the difference between
  two sequential data structures. This post describes a solution based on the
  well-known algorithm for string edit distances by Wagner-Fischer (and
  others).
---

I've been working on a problem which requires computing the difference between
two sequences of values and there are a number of good reasons to prefer
smaller acceptable solutions to larger ones. I seem to recall university
classes about a wealth of material for solving this problem for sequences of
characters (i.e. for strings) but can't, off the top of my head remember one
for more complete data types. So I'll generalise the algorithm I have to make
it work in the case I care about.

String edit distance
====================

The *edit distance* problem is fairly easy to define: what is the "distance"
between two strings, i.e., how much work is involved in editing one string so
that it is identical to another.

A *change script* is a sequence of changes to be made to a string.

Each operation has a *cost* and the cost function can be extended to apply to
change scripts by summing the cost of each component operation.

The *edit distance* between two strings is the cost of the optimal change
script between them.

The [Wagner-Fischer algorithm][1] is a dynamic programming algorithm to compute 

[1]: https://en.wikipedia.org/wiki/Wagner%E2%80%93Fischer_algorithm

````
int EditDistance(char s[1..m], char t[1..n])
  let d be a 2-d array of int with dimensions [0..m, 0..n]
  
  for i in [0..m]
    d[i, 0] ← i // the distance of any first string to an empty second string
  for j in [0..n]
    d[0, j] ← j // the distance of any second string to an empty first string
  
  for j in [1..n]
    for i in [1..m]
      if s[i] = t[j] then  
        d[i, j] ← d[i-1, j-1]         // no operation
      else
        d[i, j] ← minimum of
                   (
                     d[i-1, j  ] + 1, // deletion
                     d[i  , j-1] + 1, // insertion
                     d[i-1, j-1] + 1  // substitution
                   )
  
  return d[m,n]
````

Abstracting the details
=======================

There are a number of concrete details embedded in the algorithm as described
above:

1. Sequences are indexed by integers;
2. There are exactly three operations;
3. All three operations are always valid; and
4. All three operations have unit costs.

Ideally I'd remove all three of these constraints but I'm lazy, so I'll only
deal with (3) and (4) first. Furthermore my primary concern is getting an
optimal edit script and not the particular distance number. So while I'm at it
I'll be modifying the algorithm to retain the particular operations at each
step and return an edit script.

The algorithm doesn't care about the data type of items in the sequences being
compared so I'll use the type variable `v` to represent a value.

The algorithm doesn't care about the data type of operations in the edit script so
I'll use the type variable `o` to represent an operation.

The algorithm *also*, strictly speaking, doesn't care about the data type
representing the costs. There's not much to be gained by abstracting this but
I might as well, so I'll use the type variable `c` to represent a cost. There
are a few things we require of a cost and, to avoid spelling all of them out,
I'll just insist that they be a number.

With these in mind the type of a function implementing the algorithm will be
something like:

````{haskell}
-- | Compare two sequences and compute an optimal cost and edit script between
-- them.
minimalEdit :: (Num c) => [v] -> [v] -> (c, [o])
````

Operations
----------

Given the above, we have two notions of *operations*:

- A operation value in an edit script; and

- A function which, given an index and two values, computes such a operation
value if the operation is valid on the two values.

I'll represent these two different notions of operations as:

- a value of type `o`; and

- a function of type `Int -> v -> v -> Maybe o`.

Indexing sequences
------------------

The goal is to compute an edit script so that I can actually *apply* the script
later. This means that operation values of type `o` should include the sequence
index which should be modified. Slightly complicating the matter is the fact
that some operations advance the index of the current item (i.e. insert and
substitute) but delete operations do *not*.

I'll model this by adding an `advance` function of type `o -> Int` which
calculates the advance of the sequence index due to this operation. In most
cases this will be either 0 or 1.

Costs
-----

Each operation value of type `o` has a cost value of type `c` associated with
it. I don't much care what they are, just that they behave more or less like
numbers. Calculating the cost of an operation will add yet another function to
my interface:

````{haskell}
cost :: o -> c
````

Equality
--------

The only piece of code in the original algorithm which I *haven't* replaced yet
is the comparison which checks whether a change needs to be made at all. I'll
fix that by adding another function:

````{.haskell}
equivalent :: v -> v -> Bool
````

Parameters
----------

Packaging all these parameters up in a record yields something like this:

````{haskell}
data Params v o c = Params
    { delete :: Int -> v -> v -> Maybe o
    , insert :: Int -> v -> v -> Maybe o
    , substitute :: Int -> v -> v -> Maybe o
    , cost :: o -> c
    , advance :: o -> Int
    }
````

Passing this into the algorithm makes its type look like this:

````{haskell}
minimalEdit :: (Num c) => Params v o c -> [v] -> [v] -> (c, [o])
````

Building edit scripts
=====================

Final Code
==========

