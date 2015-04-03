---
title: Released some Haskell packages
tags: haskell, functional programming, algorithms, software, work
location: Sydney, New South Wales
excerpt: 
  I finished and released a couple of packages written in Haskell today. Here
  are a few details about them.
---

# edit-distance-vector

The [edit-distance-vector][1] package is a small library for calculating the
optimal edit script and cost to transform one sequence of values into another.
The implementation uses the [Wagner-Fischer][2] algorithm and the rather fun 
[`constructN`][3] function.

I have a draft blog post on the way about the details of this package but until
that's done you'll have to make do with the [documentation][4].

[1]: https://hackage.haskell.org/package/edit-distance-vector
[2]: https://en.wikipedia.org/wiki/Wagner–Fischer_algorithm
[3]: https://hackage.haskell.org/package/vector/docs/Data-Vector.html#v:constructN
[4]: https://hackage.haskell.org/package/edit-distance-vector-1.0/docs/Data-Vector-Distance.html

# aeson-diff

The [aeson-diff][5] package includes a library and two command-line programs
for extracting the differences between two JSON documents and for applying
these changes. The commands are:

- `aeson-diff` which compares two JSON documents and generates a patch
  describing the differences between them; and

- `aeson-patch` which takes a JSON document and updates it according to patch.

I find the `aeson-diff` command quite useful for comparing different versions
of the  JSON documents spewed out by several systems I have to deal with at
work.

[5]: https://hackage.haskell.org/package/aeson-diff
