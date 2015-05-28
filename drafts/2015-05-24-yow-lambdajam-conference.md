---
title: Yow! Lambda Jam 2015
tags: events, conferences, functional programming, presentations
location: Brisbane, Queensland
excerpt:
  I attended and gave a talk at the Yow! Lambda Jam functional programming
  conference last week. Here's a quick trip report.
---

Yow! Lambda Jam is a functional programming conference held annually in
Brisbane, Queensland. I attended in 2013, found my current role actually
*working* as a functional programmer there in 2014, and gave a talk this year.
Each day begins with a keynote talk from some FP luminary, ends with workshops,
and has a bunch of talks in the middle. It's a great couple of days and
I recommend attending if you can.

Talks
=====

The opening keynote was another category-theory laden tour de force by Ed
Kmett. He discussed $O(n)$ solutions to problems we've convinced ourselves are
$O(n log n)$, $O(n^2)$ and, in particular, sorting, partitioning, and joining. 

Charles O'Farrell's talk described a number of common patterns in property
based testing including symmetry (invertible functions), models (comparing an
optimised implementation with an obviously correct one), invariants (check
idempotence, etc.), configuration (fast testing in development, more exhaustive
in CI), polymorphism (properties polymorphic over typeclasses, instantiated at
instances to check laws), newtypes (generate even standard types with correct
properties), customer generators (check edge cases across whole suite, not just
single unit or regression test).

Matthew Brecknell described the techniques for pattern matching dependent types
in Coq. The talk was well presented, but it made me like Coq less than I did
before seeing it.

Next I gave my talk about [retcon][5], a system we built at work. It's hard to
tell how well it went but the audience laughed at the right places, clapped at
the end, and a few comments and discussions afterward suggest it was well
received. At the same time Christian Marie was talking about
[roundtrip-aeson][5b].

After morning team I saw Sidney Shek talk about Atlassian's use of the [event
sourcing][6] pattern in Scala. Event sourcing is a data storage pattern which
should be rather comfortable to functional programmers: rather than putting
data in a database and mutating it from time to time, instead leave existing
data unchanged and record any changes ("events").

My last talk of the day was by Rowan Davies and Stephan Hoermann from the
Commonwealth Bank talking about a pattern they noticed while refactoring their
code. I also did the related workshop, working through the implementation of

Day two opened with a keynote from Mark Hibberd about the ways in which
functional programming impacts on the engineering aspects of programming.

Eric Torreborre's discussion of data types reiterated some some points that
were mentioned in Charles' talk and hinted at in my own.

Tim McGilchrist's introduction to OCaml's module system was interesting and
enlightening.

Where monads build-up, comonads tear-down. David Laing introduced free monads,
comonads, and cofree comonads; showing that while free monads give languages,
cofree comonads give interpreters for them.

The afternoon was interrupted by a fire alarm (caused by burned food in the
first-floor kitchen) and after I got back into the venue I decided to give the
next session a miss; I've seen Ben speak about related topics before and I'm
not terribly interested in Clojure.

Dylan Just's introduction to record systems used the examples of Haskell, Elm,
and Purescript to ground discussions of record systems, and then introduced
a Haskell library called Vinyl.

I spent the rest of the afternoon taking another crack at learning Coq in the
Software Foundations workshop presented by a crowd from BFPG.

Take Aways
==========

A strand that I saw running through several of the talks I saw was a focus on
the very basics of functional programming: algebraic data types, pattern
matching, and functions do a *lot* to rule out huge classes of errors. These
basics, used well, help to design, structure, and testing of programs in
functional languages.

Some more specific things:

- A better handle on property-based testing, including some patterns I'd been
groping toward in the few days before the conference (synchronicity for the win!)

- Event sourcing, which looks interesting enough that I'm now reading
a Microsoft book to learn more about it.

- Relative monads and cofree comonads both like interesting approaches. I'll
need to find some time to look into both and see if and how I can use them in
my own projects.

- Using Coq is fun, and I should get back to learning it and implementing some
ideas I had.
