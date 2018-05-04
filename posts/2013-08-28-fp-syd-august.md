---
title: FP-Syd, August 2013
tags: event, functional programming, fp-syd, simulation, testing, compiling, SAT, logic
location: Sydney, New South Wales
excerpt: 
  The August 2013 meeting of the FP-Syd functional programming group in Sydney
  heard talks about simulation testing, functional languages targetting
  Javascript, and SAT solving.
---

Here are some nodes from the August 2013 meeting of the [FP-Syd][fp-syd]
functional programming group.

[fp-syd]: http://fp-syd.ouroborus.net/

# Julian Gamble on Simulation Testing in Datomic

[Julian Gamble](http://juliangamble.com) ([@juliansgamble][] on Twitter) gave
his first FP-Syd talk with an introduction to simulation testing using [Datomic][].

[@juliansgamble]: http://twitter.com/juliansgamble
[Datomic]: http://www.datomic.com

> Plug: He's writing a book called [Clojure Recipes](http://clojurerecipes.net/) which is due out in January 2014.

[Simulant][] -- the subject of the talk -- is a framework for Datomic database.
It's for *simulation testing*.

[Simulant]: https://github.com/Datomic/simulant

Many types of testing (in something resembling order of popularity):

- Unit testing
- User acceptance testing
- Performance testing
- Simulation testing

Simulation testing uses modeling and simulation to "test" systems which are too
complex for linear models like unit testing. Generations of simulations:

- High school solving maths problems

- Stock analysts modelling and analysing companies

- Analytics driven audits simulating systems for comparison.

- Business scenarios predicting responses to, e.g., market crashes.

Most of these can be done on a piece of paper or on a single machine, but
systems which aren't amenable to such approaches are becoming more common.

[Chris Okasaki's book Purely Functional Data Structures][pfds] popularised the
use of purely functional approaches to data structures through sharing.

[pfds]: http://www.amazon.com/Purely-Functional-Structures-Chris-Okasaki/dp/0521663504

Datomic is "a database as a value". Or, put another way, a database as a
persistent data structure. This makes state management easier for, e.g.,
reproducing problems for bug fixing.

Built on pluggable storage system. Uses a Java-native store locally, can use
Amazon Dynamo DB. Writing is done through a single transactor process with
querying done directly from the data store.

Simulant is a framework which uses Datomic to help to distribute and scale
simulation testing. Assumes that you'll be modelling *agents* and *actions* --
which are stored in the Simulant schema -- and additonal model details stored
in your own schema. Uses git too, to keep track of version of the simulation
changing over time.

## Process

1. Develop a Datomic schema for your model. This will be used to record the
generic details of the simulation -- the actions performed by the agents -- and
the domain specific details.

2. Set the model parameters (stocks/prices, etc. or ants/food/world size)

3. Make statistical assertions about the system. These will be verified against
the data recorded during the simulation.

There are more details to this, but they flew past and I couldn't get them down.

## Why Datomic?

Being persistent (in the "persistent data structures" sense), Datomic makes it
far easier to review old data from older simulations, add additional
statistical assertions, etc. without having to jump through the many and varied
hoops you'd need for, e.g., a relational database.

I'm not sure how true a comparison this is, given that Datomic forces all
writes to the database through the single transactor. A similar architecture
with a relational database could quite easily use a single transactor to
enforce timestamp consistency on data being recorded. I must be missing
something.

## Applicability

- Non-trivial system with multiple agents.

- Datomic's database as value, thing.

- Where you have statistical assertions to be evaluated.

# Shane Stephens on Web Animations

Works on the [web animations specification][was] for the W3C. Unifies SVG and
CSS animations on the web.

[was]: http://w3.org/TR/web-animations/

The web animations specification defines a Javascript API which looks something
like this:

````javascript
new Animation(
	document.getElementById('hello'),
	[ {"left" : "200px"},
	  {"left" : "400px", "height" : "100px"}],
	1
);
````

This talk isn't about "generating a functional API for web animations" but he
thought it was two weeks ago. He tried to generate bindings, but failed.
Instead, it's a discussion about the attempt and the result.

I think there might be animations of yak shaving involved.

## Haskell to JS compilers

There are quite a few functional languages which target Javascript and they
all, in Shane's opinion, hate the web.

### Utrecht Haskell Compiler JavaScript backend

The [UHC][] Javascript backend has little documentation, claims to "compile
most of Hackage" and provides an FFI to interact with "native" Javascript code.

[UHC]: https://github.com/UU-ComputerScience/uhc

The barrier between Haskell and Javascript is the problem. Everything on the
web "platform" is exposed with APIs in Javascript. Having a UHC-JS generate a
blob of HTML and CSS and Javascript stuff is pretty hard to compose with other
web-ish things.

There's a big impedance mismatch between Haskell and Javascript.

### Elm

[Elm][] is a functional reactive programming language which compiles to
Javascript. Lots of documentation, an online editor, and it already has
animations.

[Elm]: http://elm-lang.org/

But Elm is another "replace the world" abstraction.

### Roy

[Roy][] has a much saner approach, largely just syntactice sugar around
Javascript:

[Roy]: http://roy.brianmckenna.org/

- Javascript functions are available
- Roy types are almost Javascript "types"

But no ADTs, etc. Because JS is pretty shitty with no recursion, etc.

### krazy

So with no "good" existing languages he started his own language called krazy.

- The current implementation is a PEG parser and interpreter in Javascript.

- Functional types are Javascript types (lists, for example, really are Javascript arrays).

- Supports ADTs, HOFs, pattern matching, etc.

- JS interop "constrained" by type assertions.

- Will probably add record with optional, structural typing.

## Animations

Back to the web animations API. 

The web animations specification has side-effect free constructors for
animations, effects, timing groups, etc.

This could be exposed to library authors and used as an interface or to
generate an interface automatically? I'm not sure.

# [Thomas Sewell][ts] on learnings about SAT

[Thomas Sewell](http://ssrg.nicta.com.au/people/?cn=Thomas+Sewell)

> Survey: who can name an NP-complete problem?

NP-complete problems can be solved by a non-deterministic machine but the
solutions can be checked by a deterministic machine. In essence, they are very
hard to solve but easy to check.

Circuit satisfiability can be encoded in SAT.

The SAT problem attempts to assign values to logical variables in a formula in
conjunctive normal form and produces either a set of assignments (if the
formula is satisfiable) or "no" (if there is no assignment).

The DPLL algorithm is pretty naive and does lots of backtracking.

The CDCL algorithm -- discovered in the 90s -- increased the size of viable
problems to millions of variables. Instead of having to "re-learn" the same
pieces of information repeatedly when backtracking, the Conflict Driven Clause
Learning algorithm tracks the "cause" of a clause you learn and, when a
contradiction is derived, it learns the inverse of it's parent.

E.g.

> If we reach contratiction, and the parents are $x_{1}$, $\neg x_{2}$, $x_{12}$.
> Then we need to learn $\neg x_{1} \vee x_{2} \vee \neg x_{12}$ as at least one
> of the assumptions are false, so the negation of their disjunction must hold.

## Learnings

- Competitions - progress

- Fast propagation - a modern SAT solver needs a very efficient implementation
  of the propagation algorithm.

- Locality - solvers make decisions "near" previous decisions. Need a heuristic
  to find "nearby" variables for choice.

- Phases - alternate between phases focussed on SAT and un-SAT phases.

- Pruning - prune the database of clauses periodically to speed propagation.

- Glue - Not sure what this means?

- Rewriting - preprocessing the problem into an equisatisfiable problem. Make
  the problem "better", works well as a first step. Useful on problems like
  CPUs problems.

Lots of problems have nice and/or useful SAT encodings. 

NP-complete problems were, in the not too distant past, primarily useful as a
polite "no" for managers. (You can't have your cake and eat it too.)

## SAT with Proofs

Some solvers produce a resolution proof.

Reverse Unit Propagation of a proof is a services of clauses that can be
learned by unit propagation only. The conflict clauses of a CDCL solver in the
order they are learned form a RUP proof.

DRUP adds clause deletion, to speed up unit propagation.

Having useful proofs with rewriting is complex. Checking that a SAT proof for a
rewritten problem is tricky; dealing with the rewriting (incorporating it into
the proof and validating the rewriting is often as complex as the SAT problem
itself, etc.)

## Motivations

Have some SMT proofs and would love to check them in HOL4 or Isabelle/HOL.
Satisfiability Modulo Theories (SMT) incorporates SAT as part of it. HOL4 and
Isabelle/HOL are highly trusted but very slow. Using SMT/SAT to solve a problem
quickly and Isabelle/HOL to replay and verify the result should result in a
fast, trusted proof.

There are SAT replay tools that do this sort of thing, but they were all pretty
or extremely slow. Turns out millions of variables are hard in more traditional
tools.
