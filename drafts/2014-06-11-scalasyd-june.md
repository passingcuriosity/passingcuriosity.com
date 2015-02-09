---
title: ScalaSyd Episode 26
tags: event, meetup, scalasyd, scala, functional programming, json
location: Sydney, New South Wales
excerpt:
  The June ScalaSyd meeting: argonaut, shapes, and codensity.
---

# Mark on Argonaut

JSON library in Scala. The only excuse for using JSON is
Javscript. Why a new JSON library? Had 4 days, 4 JSON libraries and 4
critical bugs. Wrote Argonaut in a few hours (then made it better over
six more versions).

Argonaut: no exceptions, no runtime. Functions return either
(`scalaz.\/`) rather than raise exceptions.

`json.spaces2` is a pretty printer.

Uses cursors (zippers) to navigate JSON document and support efficient
local update. Zipper navigation must handle possible failure, an
`ACursor` is a cursor which can fail; lets you handle failed
navigation (try `.fred` and if that doesn't work, look in `.barney`
instead). Makes it much easier to compose a path and leave all the
error handling to the end (rather than spread it through the whole
navigation).

History cursor: navigation can fail (as with ACursor) but also
maintains a log of operations. Allows you to produce nice error
messages when an input can't be processed.

    (branches.hcursor.downArray.downField("name") :=
    "was-codecs".asJson).undo

Navigate the document and set a field; then undo the navigation to get
the whole document with change.

Zippers are highly efficient; but Argonaut also provides a lens API:

    (jArrayPL >=> PLenss.listNthPLens(0) >=> ...).set("was-codecs")

An Argonaut codec provides support for converting from JSON to some
type and back again. This is like the FromJSON and ToJSON classes from
aeson in Haskell.

Has three data types which provide encoding, decoding and codecing to
and from JSON. They mutually derive.

Depends on scalaz; will be optionally dependent on shapeless soon.

# Hugh on Codensity

Hugh Giddens using codensity to improve all the things.

Mark gave a talk about steam processing at LambdaJam (used pipes,
conduit, or scalaz streams). Went home to learn more about pipes and
found note about quadratic space complexity of a few functions you
should never use and using "codensity" to make it linear.

There's a codensity type in Scalaz, but little to no guidance on using
it. Janis Voigtlander's paper Asymptotic Improvement of Computations
over Free Monads; the technique is non-intrusive, no need to change
the code to be improved.

This presentation is largely a translation and presentation of that
paper in Scala (rather than Haskell).

Example of a binary tree data structure. Function to generate a
complete binary tree; another to traverse the tree. Composing them
yields polynomial time; part of this is due to the construction of
sub-trees which we destroy in creating the next level.

Codensity makes bind right associative; makes use of the monad laws
and is completely safe. This (and something else) is what makes the
tree example work; essentially fusing repeated maps. Applications:
traversing data structures, free monads, pipes, scalaz-stream.

See also: Ed Kmett's blog posts about Yoneda Lemma.

Maybe the way free avoids stack overflows is codensity? Probably not
all of it.

# Max talking about Shapes

All data types have a "shape".

The "barn yard problem": we have a bunch of animals on our farm and we
need to encode them in JSON.

