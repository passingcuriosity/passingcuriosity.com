---
title: Testing multiple GHC versions on Travis CI
tags: haskell, functional programming, software, work, testing
location: Sydney, New South Wales
excerpt: 
  We're currently in a transition period between GHC releases so it's important
  to be able to test packages with multiple versions of GHC. Here's the
  standard approach and what I learned doing it to a package of mine.
---

GHC 7.10.1 was recently released and, if nothing else, that means there's a new
version of the base library so a lot of developers will need to bump the upper
bound specified in their cabal files. Having multiple versions of the compiler
installed isn't all that difficult, but actually building an testing a cabal
package with multiple compiler versions *is* pretty tedious. It's easy enough
to do on Travis CI though and with just a little bit of cargo-culting, you too
can be extending your set of supported GHC versions.

# Multiple versions of GHC on Travis CI

[Travis CI][1], for those who aren't familiar with it, is a continuous
integration service with pretty tight integration with [GitHub][2]. Using it is
pretty straightforward: you add a YAML file to your repository describing how
to test your project and then you turn it on. Using [matrix variables][3] in
your YAML file you can specify multiple values for various aspects of your
build process and Travis CI will run your job multiple times - once for each
combination of values. The standard approach to testing with [multiple GHC
versions][4] on Travis CI uses this to specify which versions of GHC and Cabal
to install and use in the build; specify four versions and get run four times.
Magic!

The `.travis.yml` file you use to do this is a little more complex than the
usual one saying "make my Haskell go!", but you can generally just copy it
around from project to project. I edited [my `.travis.yml` file][5] slightly to
tweak the way Travis CI sends me email and to select the versions of GHC that I
care about and now [every build for that project][6] automatically covers all
the cases I care about.

# So what?

My `edit-distance-vector` package is very simple: it's one module with 166
lines (including comments and white space) and 100 lines of tests (again,
including comments and white space). Here are the issues picked up by testing
with four versions of GHC:

1. Obviously, the version bounds on the base library need to be broadened. I've
   used `base >=4.5 && <4.9` now.

2. Next I learn that the [Sum][7] type didn't have a [Num][8] instance in
   earlier versions. This means that constants like `1` can't have types like
   `Sum Int` so I've just applied the `Sum` constructor manually: `Sum 1`.

3. Then I learned that importing a module `hiding` something that it doesn't
   export used to be an error (it is now a warning).

The [commit][9] fixing these issues is pretty trivial but made the library
usable in a wider range of environments. Yay!

I think I'll be using this by default in new Haskell repositories; when my code
doesn't work with some version of GHC I'd like it to be because I *decided* to
do it, not just that I didn't know.

[1]: https://travis-ci.org/
[2]: https://github.com/
[3]: http://docs.travis-ci.com/user/environment-variables/#Matrix-Variables
[4]: https://github.com/hvr/multi-ghc-travis
[5]: https://github.com/thsutton/edit-distance-vector/blob/master/.travis.yml
[6]: https://travis-ci.org/thsutton/edit-distance-vector
[7]: http://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Monoid.html#t:Sum
[8]: http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#t:Num
[9]: https://github.com/thsutton/edit-distance-vector/commit/d287d8b97deee5cb3b3e2fe74e155226c40b96a4
