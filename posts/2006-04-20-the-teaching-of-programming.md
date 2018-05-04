---
title: The Teaching of Programming
tags: haskell, programming
---

Having decided that computer science isn't really my thing (I prefer it
as a hobby, rather than a career), I'm <a
href="http://interestingexperience.blogspot.com/2006/02/change-in-focus.html">now
training to be a teacher</a> and, as such, spend a certain amount of
time thinking about ways to present material. One of the topics that is
of no small interest to me is the teaching of programming, as distinct
from teaching programming languages.

I spent some of the 2.5 hour bus trip back from classes in Hobart today
thinking about ways to present <a
href="http://en.wikipedia.org/wiki/Monads_in_functional_programming">monads</a>.
As a result, I've decided to write my very own version of that mainstay
of the Haskell community -- Yet Another Monads Tutorial. The goals of
this project are twofold:

1. to try to solidify my understanding of monads, their uses and theory;
   and

2. to present programming in a way that very few or, more likely, no
   students will experience before advanced undergraduate CS classes.

My current thinking is to introduce functional programming in terms of
expression reduction (using <a
href="http://www.haskell.org/hugs/">Hugs</a> or, maybe, <a
href="http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html">GHCi</a>).
This will segue into a discussion of evaluation semantics (specifically,
lazy evaluation) which will raise the problem of sequenced computations.
`Monad`s (as we already know) provide a solution to this problem which
will be demonstrated with some work in the `IO` monad. To really
understand how `Monad`s work in Haskell, we'll have to take a brief
detour through type classes before we look at defining our own instances
of `Monad`.

The main example (and the only one that I've thought most about) will be
a `Monad` that allows us to compose transformation matrices for 3D work
in monadic style. In this `Monad`, we'll be able to create a
transformation matrix by doing something like:

````{.haskell}
myTransform = do
    identityMatrix;
    rotateAbout Z 90;
    scale X 1.5;
    translate Y 20
````

rather than explicitly creating the matrix for each transformation and
multiplying them together or, worse, poking at the individual members of
the transformation matrix. Furthermore, this example lends itself quite
well to extension. Instead of calculating a matrix which performs the
appropriate transformations, we might instead create an data-structure
encoding the calculation which will, when evaluated, yield such a
matrix. This data-structure could then be evaluated (like, if I
understand correctly [runST][] does for the `ST` monad) or it could be
subjected to symbolic manipulation (perhaps allowing the students to
implement a simple optimiser).

[runST]: http://www.haskell.org/ghc/docs/6.4.1/html/libraries/base/Control-Monad-ST.html#v%3ArunST

This will probably turn into my major project for next semester
(developing plans and materials for an entire unit). If it does, I'll
probably make it available online somewhere.
