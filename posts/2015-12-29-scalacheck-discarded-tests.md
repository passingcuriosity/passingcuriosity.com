---
title: Some ScalaCheck generators fail too much
tags: scala, functional programming, work, testing, properties
location: Sydney, New South Wales
excerpt: 
  ScalaCheck comes with generators which will happily try and, with
  high probability, fail to construct sets.
---

Most of the code I'll be working with in my new job (BTW blog: I have
a new job) is written in [Scala][1] and uses property based testing
with [ScalaCheck][2]. Yesterday I ran into a problem with an existing
test suite that suddenly began failing with too many discarded tests:

[1]: http://www.scala-lang.org/
[2]: https://scalacheck.org/

````
[info] FormattersSpec
[info]   Formatters are invertible for:
[info]     + Mapping
[info]     + Identifier
[info]
[error]     x Metadata
[error]  Gave up after only 39 passed tests. 197 tests were discarded. (FormattersSpec.scala:11)
````

This test generates random `Metadata` values and makes sure that they
can be serialised and deserialised correctly (i.e. values can be
round-tripped). The property being test here is identical, only the
`Arbitrary`, `Serialise`, and `Deserialise` instances vary in each
case. The truly odd thing is that the pertinent code looks like this:

````{.scala}
case class Identifier(name: String)
case class Metadata (id: Identifier, maps: Set[Identifier])

implicit val ArbIdentifier = Arbitrary(
  for {
    name  <- arbitrary[String]
  } yield Identifier(name)
)

implicit val ArbMetadata = Arbitrary(
  for {
    identifier <- arbitrary[Identifier]
    mappings   <- arbitrary[Set[Identifier]]
  } yield Metadata(identifier, mappings)
)
````

My first step was redefining a few related `Arbitrary` instances to
avoid using `suchThat` (which discards invalid values) but this didn't
fix the problem. Eventually I tried redefining `ArbMetadata` like
this:

````{.scala}
implicit val ArbMetadata = Arbitrary(
  for {
    identifier <- arbitrary[Identifier]
    mappings   <- Gen.const(Set.empty[Set[Identifier]])
  } yield Metadata(identifier, mappings)
)
````

and the problem went away. Trying to use `arbitrary[Set[Identifier]]`
in various ways in the Scala REPL confirmed that it is the problem; we
can easily generate as large a `List[Identifier]` as we like, but a
`Set[Identifier]` fails fairly frequently:

````{.scala}
// This always generates a Some[List[Identifier]] value.
Gen.listOfN(100, arbitrary[Identifier]).map(*.length).sample
// Sometimes we get a Some[List[Set[Identifier]]] and others None.
Gen.listOfN(100, arbitrary[Set[Identifier]]).map(*.length).sample
````

It appears as though whatever mechanism is used by `arbitrary[Set[_]]`
to construct the sets, it doesn't fails when the generator for the
value type returns duplicate elements. You can confirm this easily by
trying `arbitrary[Set[Unit]]`; any `Gen[Unit]` has no choice but to
return a the single value of type `Unit` (or to fail) and, as
expected, this almost never succeeds. Replacing the problematic
`arbitrary[Set[Identifier]]` in the original code with
`arbitrary[Seq[Identifier]].map(_.toSet)` resolves the issue:
constructing a set from a list of possibly duplicate `Identifier`s
always works.

After a bit of reading in the ScalaCheck source code it *seems* as
though the root cause of this problem is some instance of
`CanBuildFrom[Set[_], A, Set[A]]` but I've no idea how to go about
figure out which one or why it's broken. In any case, I now know a bit
more about working with Scala.

For more information, see the [ScalaCheck issue #89][3].

[3]: https://github.com/rickynils/scalacheck/issues/89
