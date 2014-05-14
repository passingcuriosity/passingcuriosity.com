---
title: ScalaSyd April 2014
location: Sydney, New South Wales
tags: event, meetup, scalasyd, scala, functional programming, jvm, reactive, numerics, types
description:
  The April 2014 meeting of the ScalaSyd user group.
---

New people from places like Tyro Payments, Fairfax, CBA, Typesafe (New
Zealanders!), etc.

The ScalaSyd community has grown to nearly 500 members in two and a
half year. Yay!

[Yow! Lambda Jam](http://lambdajam.yowconference.com.au/) is coming
soon and everyone who's interested in functional programming should
go. Dean Wampler (well-known Scala programmer) and Ed Kmett
(well-known Haskell hacker) are the keynote speakers. It'll be great!

Jobs:

- Atlassian: SaaS engineering, mostly Scala.

# Greg Baran on generic numerics

"high performance generic numerics for Scala: the spire library"

Saw a talk by the developers of Spire on infoq. (Only 20 minutes, go
look it up)

Work at Atlassian; lots of data (HTTP logs, etc.) and drive to get
more value from it. Tools like R, SASS, Pandas, etc. aren't terribly
useful for building large-scale systems.

Devs worked on bioinformatics & geospatial applications. Lots and lots
of numbers. Project started as an attempt to improve the built-in
numerics; grew into a library.

Spire is quite low level: above JVM and Scala, below "stats library".

Goals:

- generic - less code, fewer tests

- fast - specialisation

- precise - maintained, not sacrificed for speed

Allow application developers to write efficient numerical algorithms
without depending on specific representations.

Overloading functions for types means duplication of code.

Primitive types are great, using boxing increases memory usage, GC
load, etc.

````{.scala}
def gcd[@specialized(Long) A: Integral](x:A,y:A):A = {
  import spire.implicits._
  ...
}
````

> Scala uses `int` for the `Int` type if it can.
> Has the standard Java numeric types.
> Also has a few type classes.

## Types provided by spire

Natural

Rational

Algebraic: lazily-computer, arbitrary precision algebraic numbers

Real

Complex[A] - complex numbers on A

Quaternion[A] - 4D numbers on A

UByte

ULong

SafeLong - essentially a long, but overflow proof (automatic promotion
on overflow)

FixedPoint - fractions with unboxed Long numerator and implicit denominator.

Interval[A] - arithmetic on open, closed and unbound intervals.

Polynomial[A] - univariate polynomials

## Demo

### `mean(1e20, 1, -1e20)`

on Double is completely inaccurate (1e20, 1, -1e20) = 0

on BigDecimal is OK, within it's limitations (0.33333...3333).

on Rational is `1/3`

### `sqrt(363) - sqrt(300) - sqrt(3)`

on Double is `1`

on BigDecimal is `8e-33`

on Real is `0`

## Types

parametic polymorphism (same code over diff types -- list of foos)
versus ad hoc polymorphism (different implementation for different
types).

Type classes for lots of algebraic structures.

## Specialisation

generates versions of code specialised for selected concrete types.

## Conclusion

Accomplishes goals

Powerful pattern for generic libraries to follow.

Only has a minimal ecosystem

Going to give it a try for performance analysis at Atlassian.

# Alexey Raga on Reactive Extensions

Alexey Raga from Arbor Networks (/Packetloop).

> According to Jed: a blend of functional reactive programming and
> "more pragmatic" approaches.

Two main paradigms of communication: pull ("interactive"; proactive,
poke people to get things) and push ("reactive"; passive, wait for
someone to poke you, then react).

Rx -- reactive extensions -- started by Eric Meyer about 5 years
ago. Interactive and reactive are duals;

````
Iterable[A]
iterator : Iterator[A]

Iterator[A]
hasNext: Boolean
next: A
````

The dual of the interactive iterator interface is the reactive
observable interface:

````
Observable[A]
subscribe(o:Observer[A])

Observer[A]
onCompleted(f: Unit => Unit)
onNext(f: A => Unit)
onError(f: Throwable => Unit)
````

## What is Rx

Provides a large number of combinators for dealing with
observables. Lots of standard things (flatMap, etc) but also things
like buffering, timestamps, etc.

- unified interface for asynch data streams

- compose and query asynch data streams

- manage and parameterise concurrency

# Mark Hopkins on higher kinded and higher rank types

@antiselfdual

Arbor Networks bought Packetloop (startup); does security analytics,
real-time event processing using Scala and JS.

> SPJ calls hk and hr types "sexy types"

Java programmers use things like:

- Magic that's baked into the language (serialisation, enums,
  iterators).

- Crazy hacks like bytecode manipulation

- Unsafe features like down casting

In Scala the "higher" features replace many of these things.

## Types

A type system is a machine-checked formal system for proving the
absence of certain types of programming errors.

- lightweight

- machine-checked

- ubiquitous - you can't forget it!

> `3 + 4` is ok
>
> `3 + true` not so much.

Safe languages prevent un-trapped errors at runtime. That is, unsafe
languages allow nonsense programs to be written.

## Abstraction

abstraction, polymorphism,

## polymorphism

On:

- value (functions)

- functions (HO functions)

- type (parametric [java generics], inclusion [Java poly], ad hoc)

- structure (type classes)

- effects (monads)

- shape (shape : type = function : value)

